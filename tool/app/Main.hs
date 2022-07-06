{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ArgParser
import Control.Monad
import Control.Monad.Parallel
import qualified Data.Text as T
import qualified Dhall
import GenerateYaml
import Options.Applicative
import System.Environment
import TH

main :: IO ()
main = do
  runOptions <- customExecParser (prefs showHelpOnEmpty) opts :: IO Options
  let dir = dhallDir runOptions
  let c = optCommand runOptions
  print c
  case c of
    GenerateConfig configsPath -> do
      network <- readConfig dir
      let hostNames = Prelude.map (T.unpack . name) (hosts network)
      Control.Monad.Parallel.mapM_ (writeYamlFile dir configsPath) hostNames
      putStrLn "Done"
    GenerateCertificates configsPath caCrtPath caKeyPath nebulaCertPath -> do
      network <- readConfig dir
      results <- Control.Monad.Parallel.mapM (\h -> generateCertKey nebulaCertPath caCrtPath caKeyPath h network configsPath) (hosts network)
      putStrLn $ "Done without errors: " <> show (and results)
    SignKey keyPath hostName caCrtPath caKeyPath nebulaCertPath -> do
      network <- readConfig dir
      let matches = Prelude.filter (\h -> (T.unpack . name) h == hostName) (hosts network)
      result <- signKey nebulaCertPath caCrtPath caKeyPath (head matches) network keyPath
      putStrLn $ "signed: " <> show result
    AutoSignKey keysPath caCrtPath caKeyPath nebulaCertPath -> do
      network <- readConfig dir
      result <- autoSignKeys nebulaCertPath caCrtPath caKeyPath network keysPath
      putStrLn $ "Done without errors: " <> show result

readConfig :: String -> IO Network
readConfig dhallBaseDir = do
  let dir = prepareDhallDirString (map (\c -> if c == '\\' then '/' else c) dhallBaseDir)
  putStrLn "Reading network configuration"
  Dhall.input Dhall.auto (T.pack ("(" <> dir <> "network-description.dhall).network")) :: IO Network
