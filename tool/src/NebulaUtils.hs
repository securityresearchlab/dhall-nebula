{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module NebulaUtils (writeYamlFile, generateCertKey, prepareDhallDirString, signKey, autoSignKeys, verifyCert) where

import Control.Monad
import Control.Monad.Parallel
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Maybe
import Data.String as S
import qualified Data.Text as T
import Dhall
import qualified Dhall.JSON
import Dhall.Marshal.Decode
import Dhall.TH
import Dhall.Yaml as DY
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath (replaceExtension)
import System.IO
import System.Process
import System.Process (cleanupProcess)
import qualified TH

data Certificate = Certificate
  { details :: CertificateDetails,
    fingerprint :: String,
    signature :: String
  }
  deriving (Show, Generic)

instance FromJSON Certificate

instance ToJSON Certificate

data CertificateDetails = CertificateDetails
  { groups :: [String],
    ips :: [String],
    isCa :: Bool,
    issuer :: String,
    name :: String,
    notAfter :: String,
    notBefore :: String,
    publicKey :: String,
    subnets :: [String]
  }
  deriving (Show, Generic)

instance FromJSON CertificateDetails

instance ToJSON CertificateDetails

genericConfigContent :: String -> TH.IPv4 -> String
genericConfigContent configFileName ip =
  let ipString :: String
      ipString = foldl (<>) "" (intersperse " " (Prelude.map show [TH.i1 ip, TH.i2 ip, TH.i3 ip, TH.i4 ip]))
  in "let network = ./" <> configFileName <> ".dhall \
      \let nebula = ./package.dhall \
      \let Optional/null \
      \  = https://prelude.dhall-lang.org/v21.1.0/Optional/null \
      \    sha256:3871180b87ecaba8b53fffb2a8b52d3fce98098fab09a6f759358b9e8042eedc \
      \let yaml = nebula.configFromIP network (nebula.mkIPv4 " <> ipString <> ") \
      \let isNone = Optional/null nebula.HostConfig yaml \
      \let _ = assert : nebula.validate network \
      \let _ = assert : False === isNone \
      \in  yaml"


hostConfigFileName :: String -> String
hostConfigFileName h = h <> "-config.dhall"

generateYamlExpression :: String -> TH.IPv4 -> String
generateYamlExpression = genericConfigContent

generateNodeDirectory :: String -> String -> String
generateNodeDirectory baseDir name = baseDir <> if last baseDir == '/' then "" else "/" <> name <> "/"

generateFilePathNoExt :: String -> String -> String
generateFilePathNoExt baseDir name = generateNodeDirectory baseDir name <> name

generateYamlFilePath :: String -> String -> String
generateYamlFilePath baseDir name = generateFilePathNoExt baseDir name <> ".yaml"

prepareDhallDirString :: String -> String
prepareDhallDirString dir = if last dir == '/' then dir else dir <> "/"

isHostInGroup :: TH.Host -> TH.Group -> Bool
isHostInGroup host = elem host . TH.group_hosts

writeYamlFile :: String -> String -> String -> TH.Host -> IO Bool
writeYamlFile dhallBaseDir configFileName configDir host = do
  let host_name = T.unpack $ TH.name host
  let host_ip = TH.ip host
  let dhallDir = prepareDhallDirString dhallBaseDir
  putStrLn ("Generating yaml configuration for " <> host_name)
  let dhallExpression = T.pack (generateYamlExpression configFileName host_ip)
  let filePath = generateYamlFilePath configDir host_name
  createDirectoryIfMissing True (generateNodeDirectory configDir host_name)
  let options = DY.defaultOptions {omission = Dhall.JSON.omitNull . Dhall.JSON.omitEmpty}
  yamlContent <- DY.dhallToYaml options (Just dhallDir) dhallExpression
  -- print yamlContent
  if yamlContent == "null\n"
    then pure False
    else do
      _ <- B.writeFile filePath yamlContent
      pure True

executeShellCommand :: String -> IO Bool
executeShellCommand command = fst <$> executeShellCommandWithOutput command

executeShellCommandWithOutput :: String -> IO (Bool, String)
executeShellCommandWithOutput command = do
  let process = (shell command) {std_out = CreatePipe}
  (result, str, _) <- readCreateProcessWithExitCode process ""
  let res = case result of
        ExitSuccess -> True
        ExitFailure _ -> False
  pure (res, str)

generateSignOptions :: TH.Host -> TH.Network -> String
generateSignOptions host network =
  "-name \""
    <> host_name
    <> "\" -ip \""
    <> host_ip
    <> "\" "
    <> groupsOption
    <> " "
    <> subnetOptions
  where
    host_name = T.unpack $ TH.name host
    groups_names = Prelude.map (T.unpack . TH.group_name) $ Prelude.filter (isHostInGroup host) (TH.groups network)
    host_ip = (show . TH.ip) host <> "/" <> show (TH.ip_mask network)
    groupsOption = if null groups_names then "" else foldl (<>) "-groups \"" (intersperse "," groups_names) <> "\""
    subnets = Prelude.map show (getHostSubnets host network)
    subnetOptions = if null subnets then "" else foldl (<>) "-subnets \"" (intersperse "," subnets) <> "\""

signKey :: String -> String -> String -> TH.Host -> TH.Network -> String -> IO Bool
signKey nebulaCertPath caCrtPath caKeyPath host network keyPath = do
  let command =
        nebulaCertPath
          <> " sign -ca-key \""
          <> caKeyPath
          <> "\" -ca-crt \""
          <> caCrtPath
          <> "\" -in-pub \""
          <> keyPath
          <> "\" -out-crt \""
          <> replaceExtension keyPath "crt"
          <> "\" "
          <> generateSignOptions host network
  executeShellCommand command

autoSignKeys :: String -> String -> String -> TH.Network -> String -> String -> IO Bool
autoSignKeys nebulaCertPath caCrtPath caKeyPath network keyPath keysExt = do
  let pairs = Prelude.map (\h -> (h, prepareCrtName h)) (TH.hosts network)
  results <- Control.Monad.Parallel.mapM (\(h, path) -> signKey nebulaCertPath caCrtPath caKeyPath h network path) pairs
  pure (and results)
  where
    ext :: String
    ext
      | keysExt == "" = keysExt
      | head keysExt == '.' = keysExt
      | otherwise = "." <> keysExt
    prepareCrtName :: TH.Host -> String
    prepareCrtName host = generateFilePathNoExt keyPath ((T.unpack . TH.name) host) <> ext

generateCertKey :: String -> String -> String -> TH.Host -> TH.Network -> String -> IO Bool
generateCertKey nebulaCertPath caCrtPath caKeyPath host network baseDir = do
  let host_name = T.unpack $ TH.name host
  let dir = generateNodeDirectory baseDir host_name
  createDirectoryIfMissing True dir
  let command =
        nebulaCertPath
          <> " sign -ca-key "
          <> caKeyPath
          <> " -ca-crt "
          <> caCrtPath
          <> " "
          <> generateSignOptions host network
          <> " -out-key "
          <> dir
          <> host_name
          <> ".key"
          <> " -out-crt "
          <> dir
          <> host_name
          <> ".crt"
  executeShellCommand command

verifyCert :: String -> String -> String -> TH.Network -> IO (Either String ())
verifyCert nebulaCertPath caCrtPath crtPath network = do
  let verifyCommand =
        nebulaCertPath
          <> " verify -ca "
          <> caCrtPath
          <> " -crt "
          <> crtPath
  verified <- executeShellCommand verifyCommand
  if verified
    then do
      let command =
            nebulaCertPath
              <> " print -json -path "
              <> crtPath
      (res, out) <- executeShellCommandWithOutput command
      if res
        then do
          let certificate = eitherDecode (LB.fromStrict (C8.pack out)) :: Either String Certificate
          pure $ certificate >>= checkCertificate
        else pure $ Left "Could not check certificate content"
    else pure $ Left "Invalid certificate"
  where
    groupFromName :: String -> Maybe TH.Group
    groupFromName n = fst <$> uncons (Prelude.filter (\g -> (T.unpack . TH.group_name) g == n) (TH.groups network))
    checkCertificate :: Certificate -> Either String ()
    checkCertificate cert =
      let certDetails = details cert
          uc = uncons $ Prelude.filter (\h -> (T.unpack . TH.name) h == name certDetails) (TH.hosts network)
       in case uc of
            Nothing -> Left "No corresponding host found"
            Just (host, _) -> do
              let hostGroups = Prelude.map groupFromName (groups certDetails)
              if Nothing `elem` hostGroups
                then Left "Invalid groups"
                else do
                  let groupCheck = catMaybes hostGroups == Prelude.filter (isHostInGroup host) (catMaybes hostGroups)
                  let ipCheck = (show (TH.ip host) <> "/" <> show (TH.ip_mask network)) `elem` ips certDetails
                  let configSubnets = Prelude.map show (getHostSubnets host network)
                  let subnetCheck = all (\x -> x `elem` subnets certDetails) configSubnets && all (`elem` configSubnets) (subnets certDetails)
                  -- TODO: add subnet check
                  if groupCheck && ipCheck && subnetCheck
                    then Right ()
                    else Left $ "Groups ok: " <> show groupCheck <> "; IPs ok: " <> show ipCheck <> "; subnets ok: " <> show subnetCheck

getHostSubnets :: TH.Host -> TH.Network -> [TH.IPv4Network]
getHostSubnets host network =
  let unsafeRoutes = concatMap (TH.unsafe_routes . TH.tun) (TH.hosts network)
      relevant_unsafe_routes = filter (\ur -> TH.via ur == TH.ip host) unsafeRoutes
   in Prelude.map TH.u_route relevant_unsafe_routes
