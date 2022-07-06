{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module GenerateYaml (writeYamlFile, generateCertKey, prepareDhallDirString, signKey, autoSignKeys) where

import Control.Monad
import Control.Monad.Parallel
import qualified Data.ByteString as B
import Data.List
import Data.String as S
import qualified Data.Text as T
import Dhall
import qualified Dhall.JSON
import Dhall.Marshal.Decode
import Dhall.TH
import Dhall.Yaml as DY
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Exit
import System.IO
import System.Process
import TH

genericConfigContent :: String
genericConfigContent = "let config = ./network-description.dhall let nebula = ./package.dhall in  nebula.generateHostConfig config.network config."

hostConfigFileName :: String -> String
hostConfigFileName h = h <> "-config.dhall"

generateYamlExpression :: String -> String
generateYamlExpression = (<>) genericConfigContent

generateNodeDirectory :: String -> String -> String
generateNodeDirectory baseDir name = baseDir <> "/" <> name <> "/"

generateFilePathNoExt :: String -> String -> String
generateFilePathNoExt baseDir name = generateNodeDirectory baseDir name <> name

generateYamlFilePath :: String -> String -> String
generateYamlFilePath baseDir name = generateFilePathNoExt baseDir name <> ".yaml"

prepareDhallDirString :: String -> String
prepareDhallDirString dir = if last dir == '/' || last dir == '\\' then dir else dir <> "/"

isHostInGroup :: Host -> Group -> Bool
isHostInGroup host = elem host . group_hosts

writeYamlFile :: String -> String -> String -> IO ()
writeYamlFile dhallBaseDir configDir node_name = do
  let dhallDir = prepareDhallDirString dhallBaseDir
  putStrLn ("Generating yaml configuration for " <> node_name)
  let dhallExpression = T.pack (generateYamlExpression node_name)
  let filePath = generateYamlFilePath configDir node_name
  createDirectoryIfMissing True (generateNodeDirectory configDir node_name)
  let options = DY.defaultOptions {omission = Dhall.JSON.omitNull . Dhall.JSON.omitEmpty}
  yamlContent <- DY.dhallToYaml options (Just dhallDir) dhallExpression
  B.writeFile filePath yamlContent

executeShellCommand :: String -> IO Bool
executeShellCommand command = do
  let process = (shell command) {std_out = CreatePipe}
  (_, Just hout, _, ph) <- createProcess_ "execute shell command" process
  result <- waitForProcess ph
  hClose hout
  pure $ case result of
    ExitSuccess -> True
    ExitFailure _ -> False

signKey :: String -> String -> String -> Host -> Network -> String -> IO Bool
signKey nebulaCertPath caCrtPath caKeyPath host network keyPath = do
  let host_name = T.unpack $ name host
  let groups_names = Prelude.map (T.unpack . group_name) $ Prelude.filter (isHostInGroup host) (groups network)
  let host_ip = (show . ip) host <> "/" <> show (ip_mask network)
  let groupsOption = if null groups_names then "" else foldl (<>) "-groups \"" (intersperse "," groups_names) <> "\""
  let command = nebulaCertPath
          <> " sign -ca-key "
          <> caKeyPath
          <> " -ca-crt "
          <> caCrtPath
          <> " -in-pub "
          <> keyPath
          <> " -name \""
          <> host_name
          <> "\" -ip \""
          <> host_ip
          <> "\" "
          <> groupsOption
  executeShellCommand command

autoSignKeys :: String -> String -> String -> Network -> String -> IO Bool
autoSignKeys nebulaCertPath caCrtPath caKeyPath network keyPath = do
  let pairs = Prelude.map (\h -> (h, prepareCrtName h)) (hosts network)
  results <- Control.Monad.Parallel.mapM (\(h, path) -> signKey nebulaCertPath caCrtPath caKeyPath h network path) pairs
  pure (and results)
  where prepareCrtName :: Host -> String
        prepareCrtName host = generateFilePathNoExt keyPath ((T.unpack . name) host) <> ".crt"

generateCertKey :: String -> String -> String -> Host -> Network -> String -> IO Bool
generateCertKey nebulaCertPath caCrtPath caKeyPath host network baseDir = do
  let host_name = T.unpack $ name host
  let dir = generateNodeDirectory baseDir host_name
  createDirectoryIfMissing True dir
  let groups_names = Prelude.map (T.unpack . group_name) $ Prelude.filter (isHostInGroup host) (groups network)
  let host_ip = (show . ip) host <> "/" <> show (ip_mask network)
  let groupsOption = if null groups_names then "" else foldl (<>) "-groups \"" (intersperse "," groups_names) <> "\""
  let command =
        nebulaCertPath
          <> " sign -ca-key "
          <> caKeyPath
          <> " -ca-crt "
          <> caCrtPath
          <> " -name \""
          <> host_name
          <> "\" -ip \""
          <> host_ip
          <> "\" "
          <> groupsOption
          <> " -out-key "
          <> dir
          <> host_name
          <> ".key"
          <> " -out-crt "
          <> dir
          <> host_name
          <> ".crt"
  executeShellCommand command
