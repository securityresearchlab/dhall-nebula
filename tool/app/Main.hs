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

import Control.Monad
import Control.Monad.Parallel
import qualified Data.Text as T
import System.Environment
import Dhall
import TH
import GenerateYaml

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
