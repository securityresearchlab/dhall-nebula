{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import Lib
import HaskellTypes
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO
import Control.Monad

$makeTypes

deriving instance Show IPv4
deriving instance Show IPv4WithPort
deriving instance Show IPv4Network
deriving instance Show IPv4NetworkBoolMapEntry
deriving instance Show TextBoolMapEntry
deriving instance Show CAName
deriving instance Show Directory
deriving instance Show HostName
deriving instance Show PkiInfo
deriving instance Show DNSConfig
deriving instance Show IsLighthouseConfig
deriving instance Show PunchyInfo
deriving instance Show LocalAllowListInfo
deriving instance Show LighthouseInfo
deriving instance Show SSHDUsers
deriving instance Show SSHDInfo
deriving instance Show InterfaceInfo
deriving instance Show ListenInfo
deriving instance Show TunInfo
deriving instance Show TunRoute
deriving instance Show TunUnsafeRoute
deriving instance Show LogInfo
deriving instance Show Host

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
