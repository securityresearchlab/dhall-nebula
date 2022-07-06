{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Parallel
import qualified Data.Text as T
import qualified Dhall
import GenerateYaml
import Options.Applicative
import System.Environment
import TH

data Mode
  = GenerateConfig
      { config :: Bool,
        dhallDir :: String,
        configsPath :: String
      }
  | GenerateCertificates
      { certificates :: Bool,
        dhallDir :: String,
        configsPath :: String,
        caCrtPath :: String,
        caKeyPath :: String,
        nebulaCertPath :: String
      }
  | SignCertificate
      { sign :: Bool,
        auto :: Bool,
        dhallDir :: String,
        crtPath :: String,
        caCrtPath :: String,
        caKeyPath :: String,
        nebulaCertPath :: String
      }

signInput :: Parser Mode
signInput =
  SignCertificate
    <$> switch
      ( long "sign"
          <> help "If you wish to sign a certificate"
      )
    <*> switch
      (long "auto" <> help "Assume certificates to sign are in the subdirectories of crtPath as if they were generated by this tool")
    <*> strOption
      ( long "dhallDir"
          <> short 'd'
          <> help "The directory of the Dhall configuration"
      )
    <*> strOption
      ( long "crtPath"
          <> help "The path of the certificate to sign"
      )
    <*> strOption
      ( long "caCrtPath"
          <> help "The path of the certificate of the CA to be used to sign"
      )
    <*> strOption
      ( long "caKeyPath"
          <> help "The path of the key of the CA to be used to sign"
      )
    <*> strOption
      ( long "nebulaCertPath"
          <> help "The path of nebula-cert executable"
      )

generateCertificates :: Parser Mode
generateCertificates =
  GenerateCertificates
    <$> switch (long "certificates" <> help "If you wish to generate certificates and key for each host")
    <*> strOption
      ( long "dhallDir"
          <> short 'd'
          <> help "The directory of the Dhall configuration"
      )
    <*> strOption
      ( long "configsPath"
          <> help "The base directory where to put the generated keys and certificates"
      )
    <*> strOption
      ( long "caCrtPath"
          <> help "The path of the certificate of the CA to be used to sign"
      )
    <*> strOption
      ( long "caKeyPath"
          <> help "The path of the key of the CA to be used to sign"
      )
    <*> strOption
      ( long "nebulaCertPath"
          <> help "The path of nebula-cert executable"
      )

generateConfig :: Parser Mode
generateConfig =
  GenerateConfig
    <$> switch (long "config" <> help "If you wish to generate .yaml configuration for each host")
    <*> strOption
      ( long "dhallDir"
          <> short 'd'
          <> help "The directory of the Dhall configuration"
      )
    <*> strOption
      ( long "configsPath"
          <> help "The base directory where to put the generated yaml files"
      )

opts :: ParserInfo Mode
opts = info (signInput <|> generateConfig <|> generateCertificates) (fullDesc <> progDesc "Generate configuration for Nebula")

main :: IO ()
main = do
  options <- execParser opts
  case options of
    GenerateConfig True dhallDir configsPath -> putStrLn "config"
    GenerateCertificates True dhallDir configsPath caCrtPath caKeyPath nebulaCertPath -> putStrLn "g certificates"
    SignCertificate True False dhallDir crtPath caCrtPath caKeyPath nebulaCertPath -> putStrLn "sign"
    SignCertificate True True dhallDir crtPath caCrtPath caKeyPath nebulaCertPath -> putStrLn "sign auto"
    _ -> return ()
-- case args of
--   [dhallBaseDir, nebulaPath, nebulaCertPath, caKeyPath, caCrtPath] -> do
--     putStrLn "Reading network configuration"
--     network <- Dhall.input Dhall.auto (T.pack ("(" <> dhallBaseDir <> "network-description.dhall).network")) :: IO Network
--     putStrLn "Configuration read, writing yaml files"
--     let generateYamlFile = setupYamlGeneration dhallBaseDir nebulaPath nebulaCertPath caKeyPath caCrtPath
--     results <- Control.Monad.Parallel.mapM (generateYamlFile network) (hosts network)
--     putStrLn $ if and results then "Done" else "Errors arised, please check the results"
--   _ -> putStrLn "Wrong number of arguments"
