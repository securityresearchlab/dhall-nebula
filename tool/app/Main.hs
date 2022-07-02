{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Exit
import System.IO
import System.Process
import Control.Monad
import Control.Monad.Parallel
import Dhall
import qualified Dhall.JSON
import Dhall.TH
import Dhall.Yaml as DY
import Dhall.Marshal.Decode
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.String as S
import Data.List
import System.Directory (createDirectoryIfMissing)
import System.Environment

-- dhallBaseDir :: String
-- dhallBaseDir = "../dhall/" -- the final slash is essential for the dhallToYaml method

-- nebulaPath :: String
-- nebulaPath = "..\\..\\nebula-windows-amd64\\nebula.exe"

-- nebulaCertPath :: String
-- nebulaCertPath = "..\\..\\nebula-windows-amd64\\nebula-cert.exe"

-- caKeyPath :: String
-- caKeyPath = "..\\..\\nebula-windows-amd64\\ca.key"

-- caCrtPath :: String
-- caCrtPath = "..\\..\\nebula-windows-amd64\\ca.crt"

genericConfigContent :: String
genericConfigContent = "let config = ./network-description.dhall let nebula = ./package.dhall in  nebula.generateHostConfig config.network config."

hostConfigFileName :: String -> String
hostConfigFileName h = h <> "-config.dhall"

generateYamlExpression :: String -> String
generateYamlExpression = (<>) genericConfigContent

generateNodeDirectory :: String -> String
generateNodeDirectory name = "./nebula_configs/" <> name <> "/"

generateYamlFilePath :: String -> String
generateYamlFilePath name = (generateNodeDirectory name) <> name <> ".yaml"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dhallBaseDir, nebulaPath, nebulaCertPath, caKeyPath, caCrtPath] -> do
      putStrLn "Reading network configuration"
      network <- input auto (T.pack ("(" <> dhallBaseDir <> "network-description.dhall).network")) :: IO Network
      putStrLn "Configuration read, writing yaml files"
      let generateYamlFile = setupYamlGeneration dhallBaseDir nebulaPath nebulaCertPath caKeyPath caCrtPath
      results <- Control.Monad.Parallel.mapM (generateYamlFile network) (hosts network)
      let prettyResults = Prelude.map (\(h, v) -> (T.unpack (name h)) <> " is " <> (if v then "" else "not ") <> "valid on this machine") results
      Control.Monad.mapM putStrLn prettyResults
      putStrLn "Finished"
    _ -> putStrLn "Wrong number of arguments"

setupYamlGeneration :: String -> String -> String -> String -> String -> (Network -> Host -> IO (Host, Bool))
setupYamlGeneration dhallBaseDir nebulaPath nebulaCertPath caKeyPath caCrtPath =
  \network -> \host -> do
    let node_name = T.unpack (name host)
    writeYamlFile dhallBaseDir node_name
    validity <- verifyYamlFile nebulaPath $ generateNodeDirectory node_name
    if validity
      then do
        _ <- generateCertKey nebulaCertPath caCrtPath caKeyPath host network $ generateNodeDirectory node_name
        pure (host, validity)
      else pure (host, validity)

writeYamlFile :: String -> String -> IO ()
writeYamlFile dhallBaseDir node_name = do
  putStrLn ("Generating yaml configuration for " <> node_name)
  let dhallExpression = T.pack (generateYamlExpression node_name)
  let filePath = generateYamlFilePath node_name
  createDirectoryIfMissing True (generateNodeDirectory node_name)
  let options = DY.defaultOptions { omission = (Dhall.JSON.omitNull . Dhall.JSON.omitEmpty) }
  yamlContent <- DY.dhallToYaml options (Just dhallBaseDir) dhallExpression
  B.writeFile filePath yamlContent

executeShellCommand :: String -> IO Bool
executeShellCommand command = do
  let process = (shell command) { std_out = CreatePipe }
  (_, Just hout, _, ph) <- createProcess_ "execute shell command" process
  result <- waitForProcess ph
  hClose hout
  pure $ case result of
          ExitSuccess   -> True
          ExitFailure _ -> False

verifyYamlFile :: String -> FilePath -> IO Bool
verifyYamlFile nebulaPath path = do
  let command = nebulaPath <> " -test -config " <> path
  executeShellCommand command


generateCertKey :: String -> String -> String -> Host -> Network -> String -> IO Bool
generateCertKey nebulaCertPath caCrtPath caKeyPath host network dir = do
  let isHostInGroup = (\g -> elem host (group_hosts g)) :: Group -> Bool
  let host_name = T.unpack $ name host
  let groups_names = Prelude.map (T.unpack . group_name) $ Prelude.filter isHostInGroup (groups network)
  let host_ip = ((show . ip) host) <> "/" <> show (ip_mask network)
  let groupsOption = if null groups_names then "" else (foldl (<>) "-groups \"" (intersperse "," groups_names)) <> "\""
  let command = nebulaCertPath
                  <> " sign -ca-key " <> caKeyPath
                  <> " -ca-crt " <> caCrtPath
                  <> " -name \"" <> host_name
                  <> "\" -ip \"" <> host_ip
                  <> "\" " <> groupsOption
                  <> " -out-key " <> dir <> host_name <> ".key"
                  <> " -out-crt "  <> dir <> host_name <> ".crt"
  executeShellCommand command
