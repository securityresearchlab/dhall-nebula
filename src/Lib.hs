{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( test
    ) where

import Data.Either.Validation (Validation(..))
import Data.List
import Data.Map
import Data.Text (Text)
import Dhall (FromDhall, Vector)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Core

data PkiInfo = PkiInfo { ca :: Text, cert :: Text, key :: Text }
  deriving (Generic, Show, FromDhall)

data LighthouseInfo = LighthouseInfo { interval :: Natural, hosts :: Vector Text }
  deriving (Generic, Show, FromDhall)

data InterfaceInfo = InterfaceInfo { host :: Text, port :: Natural }
  deriving (Generic, Show, FromDhall)

-- data TunInfo = TunInfo {}

data LogInfo = LogInfo { level :: Text, format :: Text}
  deriving (Generic, Show, FromDhall)

data Host = Host { is_lighthouse :: Bool, pki :: PkiInfo, lighthouse :: Maybe LighthouseInfo, static_hosts :: Map Text (Vector Text), listen_interface :: InterfaceInfo, punchy :: Bool, logging :: LogInfo}
  deriving (Generic, Show, FromDhall)

data Group = Group { name :: Text, group_hosts :: Vector Host}
  deriving (Generic, Show, FromDhall)

data Port = PortNumber Natural | Any
  deriving (Generic, Show, FromDhall)

data Connection = Connection { connection_port :: Port, proto :: Text, group :: Group }
  deriving (Generic, Show, FromDhall)

data Network = Network { lighthouses :: Vector Host, connections :: Vector Connection }
  deriving (Generic, Show, FromDhall)

generateNetworkDhallType :: IO ()
generateNetworkDhallType = do
  -- x <- input auto "./dhall/network.dhall"
  -- print (x :: Network)
  case Dhall.expected (Dhall.auto @Network) of
        Success result -> Text.IO.putStrLn (Dhall.Core.pretty result)
        Failure errors -> print errors
