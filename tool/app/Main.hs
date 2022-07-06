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
    SignKey keyPath caCrtPath caKeyPath nebulaCertPath -> do
      network <- readConfig dir
      -- signKey :: String -> String -> String -> Host -> Network -> String -> String -> IO Bool
      let unixKeyPath = Prelude.map (\c -> if c == '\\' then '/' else c)
      let pathPieces = splitOn "/" unixKeyPath
      -- let name =
      putStrLn "sign"
    SignKey keysPath caCrtPath caKeyPath nebulaCertPath -> do
      network <- readConfig dir
      putStrLn "sign auto"
    _ -> return ()

readConfig :: String -> IO Network
readConfig dhallBaseDir = do
  let dir = prepareDhallDirString (map (\c -> if c == '\\' then '/' else c) dhallBaseDir)
  putStrLn "Reading network configuration"
  Dhall.input Dhall.auto (T.pack ("(" <> dir <> "network-description.dhall).network")) :: IO Network
