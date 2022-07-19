{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Data.List
import HaskellTypes

$makeTypes

instance Show IPv4 where
  show MakeIPv4 {i1 = i, i2 = j, i3 = k, i4 = l} = foldl (<>) "" (intersperse "." (Prelude.map show [i, j, k, l]))

deriving instance Eq IPv4

deriving instance Ord IPv4

instance Show IPv4WithPort where
  show MakeIPv4WithPort {ip1 = i, ip2 = j, ip3 = k, ip4 = l, i_port = p} = foldl (<>) "" (intersperse "." (Prelude.map show [i, j, k, l])) <> ":" <> show p

deriving instance Eq IPv4WithPort

deriving instance Ord IPv4WithPort

instance Show IPv4Network where
  show MakeIPv4Network {in1 = i, in2 = j, in3 = k, in4 = l, mask = m} = foldl (<>) "" (intersperse "." (Prelude.map show [i, j, k, l])) <> "/" <> show m

deriving instance Eq IPv4Network

deriving instance Ord IPv4Network

deriving instance Show IPv4NetworkBoolMapEntry

deriving instance Eq IPv4NetworkBoolMapEntry

deriving instance Ord IPv4NetworkBoolMapEntry

deriving instance Show TextBoolMapEntry

deriving instance Eq TextBoolMapEntry

deriving instance Ord TextBoolMapEntry

deriving instance Show CAName

deriving instance Eq CAName

deriving instance Ord CAName

deriving instance Show Directory

deriving instance Eq Directory

deriving instance Ord Directory

deriving instance Show HostName

deriving instance Eq HostName

deriving instance Ord HostName

deriving instance Show PkiInfo

deriving instance Eq PkiInfo

deriving instance Ord PkiInfo

deriving instance Show DNSConfig

deriving instance Eq DNSConfig

deriving instance Ord DNSConfig

deriving instance Show IsLighthouseConfig

deriving instance Eq IsLighthouseConfig

deriving instance Ord IsLighthouseConfig

deriving instance Show PunchyInfo

deriving instance Eq PunchyInfo

deriving instance Ord PunchyInfo

deriving instance Show LocalAllowListInfo

deriving instance Eq LocalAllowListInfo

deriving instance Ord LocalAllowListInfo

deriving instance Show LighthouseInfo

deriving instance Eq LighthouseInfo

deriving instance Ord LighthouseInfo

deriving instance Show SSHDUsers

deriving instance Eq SSHDUsers

deriving instance Ord SSHDUsers

deriving instance Show SSHDInfo

deriving instance Eq SSHDInfo

deriving instance Ord SSHDInfo

deriving instance Show InterfaceInfo

deriving instance Eq InterfaceInfo

deriving instance Ord InterfaceInfo

deriving instance Show ListenInfo

deriving instance Eq ListenInfo

deriving instance Ord ListenInfo

deriving instance Show TunInfo

deriving instance Eq TunInfo

deriving instance Ord TunInfo

deriving instance Show TunRoute

deriving instance Eq TunRoute

deriving instance Ord TunRoute

deriving instance Show TunUnsafeRoute

deriving instance Eq TunUnsafeRoute

deriving instance Ord TunUnsafeRoute

deriving instance Show LogInfo

deriving instance Eq LogInfo

deriving instance Ord LogInfo

deriving instance Show Host

instance Eq Host where
  (==) host host' = name host == name host' && ip host == ip host'

deriving instance Ord Host

deriving instance Show GroupName

deriving instance Eq GroupName

deriving instance Show Group

deriving instance Eq Group

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
