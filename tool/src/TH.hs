{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Data.List
import HaskellTypes

$makeTypes

instance Show IPv4 where
  show (MakeIPv4 { i1 = i, i2 = j, i3 = k, i4 = l}) = foldl (<>) "" $ intersperse "." $ Prelude.map show [i, j, k, l]

deriving instance Eq IPv4
deriving instance Show IPv4WithPort
deriving instance Eq IPv4WithPort
deriving instance Show IPv4Network
deriving instance Eq IPv4Network
deriving instance Show IPv4NetworkBoolMapEntry
deriving instance Eq IPv4NetworkBoolMapEntry
deriving instance Show TextBoolMapEntry
deriving instance Eq TextBoolMapEntry
deriving instance Show CAName
deriving instance Eq CAName
deriving instance Show Directory
deriving instance Eq Directory
deriving instance Show HostName
deriving instance Eq HostName
deriving instance Show PkiInfo
deriving instance Eq PkiInfo
deriving instance Show DNSConfig
deriving instance Eq DNSConfig
deriving instance Show IsLighthouseConfig
deriving instance Eq IsLighthouseConfig
deriving instance Show PunchyInfo
deriving instance Eq PunchyInfo
deriving instance Show LocalAllowListInfo
deriving instance Eq LocalAllowListInfo
deriving instance Show LighthouseInfo
deriving instance Eq LighthouseInfo
deriving instance Show SSHDUsers
deriving instance Eq SSHDUsers
deriving instance Show SSHDInfo
deriving instance Eq SSHDInfo
deriving instance Show InterfaceInfo
deriving instance Eq InterfaceInfo
deriving instance Show ListenInfo
deriving instance Eq ListenInfo
deriving instance Show TunInfo
deriving instance Eq TunInfo
deriving instance Show TunRoute
deriving instance Eq TunRoute
deriving instance Show TunUnsafeRoute
deriving instance Eq TunUnsafeRoute
deriving instance Show LogInfo
deriving instance Eq LogInfo
deriving instance Show Host
deriving instance Eq Host

deriving instance Show GroupName
deriving instance Show Group
deriving instance Show RuleDirection
deriving instance Show ConnectionTarget
deriving instance Show TrafficTarget
deriving instance Show PortRange
deriving instance Show Port
deriving instance Show Proto
deriving instance Show UnidirectionalConnection
deriving instance Show Connection
deriving instance Show Cipher
deriving instance Show FirewallRule
deriving instance Show AdHocFirewallRule
deriving instance Show Network
