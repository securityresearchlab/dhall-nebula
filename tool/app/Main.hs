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
import NebulaUtils
import Options.Applicative
import System.Environment
import TH
import Data.List.Unique

main :: IO ()
main = do
  runOptions <- customExecParser (prefs showHelpOnEmpty) opts :: IO Options
  let dir = dhallDir runOptions
  let c = optCommand runOptions
  network <- readConfig dir
  let networkHosts = hosts network
  if allUnique networkHosts
    then case c of
      GenerateConfig configsPath -> do
        let hostNames = Prelude.map (T.unpack . name) networkHosts
        Control.Monad.Parallel.mapM_ (writeYamlFile dir configsPath) hostNames
        putStrLn "Done"
      GenerateCertificates configsPath caCrtPath caKeyPath nebulaCertPath -> do
        results <- Control.Monad.Parallel.mapM (\h -> generateCertKey nebulaCertPath caCrtPath caKeyPath h network configsPath) networkHosts
        putStrLn $ "Done without errors: " <> show (and results)
      SignKey keyPath hostName caCrtPath caKeyPath nebulaCertPath -> do
        let matches = Prelude.filter (\h -> (T.unpack . name) h == hostName) networkHosts
        result <- signKey nebulaCertPath caCrtPath caKeyPath (head matches) network keyPath
        putStrLn $ "Signed: " <> show result
      AutoSignKey keysDir keysExt caCrtPath caKeyPath nebulaCertPath -> do
        result <- autoSignKeys nebulaCertPath caCrtPath caKeyPath network keysDir keysExt
        putStrLn $ "Done without errors: " <> show result
    else putStrLn "Illegal configuration: there are hosts with the same name"

readConfig :: String -> IO Network
readConfig dhallBaseDir = do
  let dir = prepareDhallDirString (map (\c -> if c == '\\' then '/' else c) dhallBaseDir)
  putStrLn "Reading network configuration"
  Dhall.input Dhall.auto (T.pack ("(" <> dir <> "network-description.dhall).network")) :: IO Network
