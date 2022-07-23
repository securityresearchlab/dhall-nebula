{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ArgParser
import Control.Monad
import Control.Monad.Parallel
import Data.List.Unique
import qualified Data.Text as T
import qualified Dhall
import System.FilePath (dropExtension)
import NebulaUtils
import Options.Applicative
import System.Environment
import TH

main :: IO ()
main = do
  runOptions <- customExecParser (prefs showHelpOnEmpty) opts :: IO Options
  let dir = uniformDirDelimiters $ dhallDir runOptions
  let configFile = dropExtension $ configFileName runOptions
  let c = optCommand runOptions
  network <- readConfig dir configFile
  let networkHosts = hosts network
  if allUnique networkHosts
    then case c of
      GenerateConfig configsPath -> do
        results <- Control.Monad.Parallel.mapM (writeYamlFile dir configFile (uniformDirDelimiters configsPath)) networkHosts
        putStrLn $ "Everything done: " <> show (and results)
      GenerateCertificates configsPath caCrtPath caKeyPath nebulaCertPath -> do
        results <- Control.Monad.Parallel.mapM (\h -> generateCertKey nebulaCertPath caCrtPath caKeyPath h network (uniformDirDelimiters configsPath)) networkHosts
        putStrLn $ "Done without errors: " <> show (and results)
      SignKey keyPath hostName caCrtPath caKeyPath nebulaCertPath -> do
        let matches = Prelude.filter (\h -> (T.unpack . name) h == hostName) networkHosts
        result <- signKey nebulaCertPath caCrtPath caKeyPath (head matches) network keyPath
        putStrLn $ "Signed: " <> show result
      AutoSignKey keysDir keysExt caCrtPath caKeyPath nebulaCertPath -> do
        result <- autoSignKeys nebulaCertPath caCrtPath caKeyPath network (uniformDirDelimiters keysDir) keysExt
        putStrLn $ "Done without errors: " <> show result
      VerifyCert crtPath caCrtPath nebulaCertPath -> do
        result <- verifyCert nebulaCertPath caCrtPath crtPath network
        putStrLn $ "Valid: " <> show result
    else putStrLn "Illegal configuration: there are hosts with the same name"

uniformDirDelimiters :: FilePath -> FilePath
uniformDirDelimiters = map (\c -> if c == '\\' then '/' else c)

readConfig :: String -> String -> IO Network
readConfig dhallBaseDir configFile = do
  let dir = prepareDhallDirString (uniformDirDelimiters dhallBaseDir)
  putStrLn "Reading network configuration"
  network <- Dhall.input Dhall.auto (T.pack ("(" <> dir <> configFile <> ".dhall).network")) :: IO Network
  putStrLn "Configuration read"
  pure network
