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

dhallBaseDir :: String
dhallBaseDir = "../dhall"

hostConfigFileName :: String -> String
hostConfigFileName h = h <> "-config.dhall"

configFilePath :: String -> String
configFilePath = (<>) dhallBaseDir

genericConfigContent :: String
genericConfigContent = "let config = ./network-description.dhall let nebula = ./package.dhall in  nebula.generateHostConfig config.network config."

generateYamlExpression :: String -> String
generateYamlExpression = (<>) genericConfigContent

generateNodeDirectory :: String -> String
generateNodeDirectory name = "./" <> name <> "/"

generateYamlFilePath :: String -> String
generateYamlFilePath name = (generateNodeDirectory name) <> name <> ".yaml"

main :: IO ()
main = do
  putStrLn "Reading network configuration"
  network <- input auto (T.pack ("(" <> dhallBaseDir <> "/network-description.dhall).network")) :: IO Network
  putStrLn "Configuration read, writing yaml files"
  let names = Prelude.map (T.unpack . name) (hosts network)
  Control.Monad.mapM writeYamlFile names
  putStrLn (show names)

writeYamlFile :: String -> IO ()
writeYamlFile node_name = do
  putStrLn ("Generating yaml configuration for " <> node_name)
  let dhallExpression = T.pack (generateYamlExpression node_name)
  yamlContent <- DY.dhallToYaml DY.defaultOptions (Just "../dhall/") dhallExpression
  let filePath = generateYamlFilePath node_name
  B.writeFile filePath yamlContent


verifyYamlFile :: FilePath -> IO Bool
verifyYamlFile name = undefined

generateCertKey :: Text -> IO ()
generateCertKey name = undefined
