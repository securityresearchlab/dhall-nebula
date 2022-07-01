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
import Dhall
import Dhall.TH
import Dhall.Yaml as DY
import Dhall.Marshal.Decode
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.String as S
import Data.List
import System.Directory (createDirectoryIfMissing)

dhallBaseDir :: String
dhallBaseDir = "../dhall/" -- the final slash is essential for the dhallToYaml method

nebulaPath :: String
nebulaPath = "..\\..\\nebula-windows-amd64\\nebula.exe"

nebulaCertPath :: String
nebulaCertPath = "..\\..\\nebula-windows-amd64\\nebula-cert.exe"

caKeyPath :: String
caKeyPath = "..\\..\\nebula-windows-amd64\\ca.key"

caCrtPath :: String
caCrtPath = "..\\..\\nebula-windows-amd64\\ca.crt"

genericConfigContent :: String
genericConfigContent = "let config = ./network-description.dhall let nebula = ./package.dhall in  nebula.generateHostConfig config.network config."

hostConfigFileName :: String -> String
hostConfigFileName h = h <> "-config.dhall"

configFilePath :: String -> String
configFilePath = (<>) dhallBaseDir

generateYamlExpression :: String -> String
generateYamlExpression = (<>) genericConfigContent

generateNodeDirectory :: String -> String
generateNodeDirectory name = "./nebula_configs/" <> name <> "/"

generateYamlFilePath :: String -> String
generateYamlFilePath name = (generateNodeDirectory name) <> name <> ".yaml"

main :: IO ()
main = do
  putStrLn "Reading network configuration"
  network <- input auto (T.pack ("(" <> dhallBaseDir <> "network-description.dhall).network")) :: IO Network
  putStrLn "Configuration read, writing yaml files"
  results <- Control.Monad.mapM (generateYamlFile network) (hosts network)
  let prettyResults = Prelude.map (\(h, v) -> (T.unpack (name h)) <> " is " <> (if v then "" else "not ") <> "valid") results
  Control.Monad.mapM putStrLn prettyResults
  putStrLn "Finished"

generateYamlFile :: Network -> Host -> IO (Host, Bool)
generateYamlFile network host = do
  let node_name = T.unpack (name host)
  writeYamlFile node_name
  validity <- verifyYamlFile $ generateNodeDirectory node_name
  if validity
    then do
      _ <- generateCertKey host network $ generateNodeDirectory node_name
      pure (host, validity)
    else pure (host, validity)

writeYamlFile :: String -> IO ()
writeYamlFile node_name = do
  putStrLn ("Generating yaml configuration for " <> node_name)
  let dhallExpression = T.pack (generateYamlExpression node_name)
  yamlContent <- DY.dhallToYaml DY.defaultOptions (Just dhallBaseDir) dhallExpression
  let filePath = generateYamlFilePath node_name
  createDirectoryIfMissing True (generateNodeDirectory node_name)
  B.writeFile filePath yamlContent

executeShellCommand :: String -> IO Bool
executeShellCommand command = do
  let process = (shell command) { std_out = CreatePipe }
  (_, _, _, ph) <- createProcess process
  result <- waitForProcess ph
  pure $ case result of
          ExitSuccess   -> True
          ExitFailure _ -> False

verifyYamlFile :: FilePath -> IO Bool
verifyYamlFile path = do
  let command = nebulaPath <> " -test -config " <> path
  executeShellCommand command


generateCertKey :: Host -> Network -> String -> IO Bool
generateCertKey host network dir = do
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
