{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellTypes (makeTypes) where

import Dhall.Marshal.Encode
import Dhall.TH as TH
import Language.Haskell.TH

typesFile :: Text
typesFile = "../dhall/types.dhall"

data TypeOfType = Union | Record

data TypeInfo = TypeInfo {t :: TypeOfType, name :: Text}

mkCode :: Text -> Text -> Text
mkCode file name = ("(" :: Text) <> file <> (")." :: Text) <> name

fromInfoToHaskellType :: Text -> TypeInfo -> HaskellType Text
fromInfoToHaskellType file (TypeInfo {name = n, t = t'}) =
  let mkName :: Text
      mkName = ("Make" :: Text) <> n
   in case t' of
        Union -> TH.MultipleConstructors n (mkCode file n)
        Record -> TH.SingleConstructor n mkName (mkCode file n)

hostTypes :: [TypeInfo]
hostTypes =
  [ TypeInfo {name = "IPv4", t = Record},
    TypeInfo {name = "IPv4WithPort", t = Record},
    TypeInfo {name = "IPv4Network", t = Record},
    TypeInfo {name = "IPv4NetworkBoolMapEntry", t = Record},
    TypeInfo {name = "TextBoolMapEntry", t = Record},
    TypeInfo {name = "CAName", t = Record},
    TypeInfo {name = "Directory", t = Record},
    TypeInfo {name = "HostName", t = Record},
    TypeInfo {name = "PkiInfo", t = Record},
    TypeInfo {name = "DNSConfig", t = Record},
    TypeInfo {name = "IsLighthouseConfig", t = Record},
    TypeInfo {name = "PunchyInfo", t = Record},
    TypeInfo {name = "LocalAllowListInfo", t = Record},
    TypeInfo {name = "LighthouseInfo", t = Record},
    TypeInfo {name = "SSHDUsers", t = Record},
    TypeInfo {name = "SSHDInfo", t = Record},
    TypeInfo {name = "InterfaceInfo", t = Record},
    TypeInfo {name = "ListenInfo", t = Record},
    TypeInfo {name = "TunInfo", t = Record},
    TypeInfo {name = "TunRoute", t = Record},
    TypeInfo {name = "TunUnsafeRoute", t = Record},
    TypeInfo {name = "LogInfo", t = Record},
    TypeInfo {name = "Host", t = Record}
  ]

connectionTypes :: [TypeInfo]
connectionTypes =
  [ TypeInfo {name = "GroupName", t = Record},
    TypeInfo {name = "Group", t = Record},
    TypeInfo {name = "RuleDirection", t = Union},
    TypeInfo {name = "ConnectionTarget", t = Union},
    TypeInfo {name = "TrafficTarget", t = Union},
    TypeInfo {name = "PortRange", t = Record},
    TypeInfo {name = "Port", t = Union},
    TypeInfo {name = "Proto", t = Union},
    TypeInfo {name = "UnidirectionalConnection", t = Record},
    TypeInfo {name = "Connection", t = Record},
    TypeInfo {name = "Cipher", t = Union},
    TypeInfo {name = "FirewallRule", t = Record},
    TypeInfo {name = "AdHocFirewallRule", t = Record},
    TypeInfo {name = "Network", t = Record}
  ]

extraTypesList :: [HaskellType Text]
extraTypesList =
  []

-- TH.SingleConstructor "IPv4" "MakeIPv4" "../dhall/types/host/IPv4.dhall"
-- , TH.SingleConstructor "IPv4WithPort" "MakeIPv4WithPort" "../dhall/types/host/IPv4WithPort.dhall"

hostTypesList :: [HaskellType Text]
hostTypesList = map (fromInfoToHaskellType typesFile) hostTypes

connectionTypesList :: [HaskellType Text]
connectionTypesList = map (fromInfoToHaskellType typesFile) connectionTypes

typesList :: [HaskellType Text]
typesList = extraTypesList <> hostTypesList <> connectionTypesList

makeTypes :: Q [Dec]
makeTypes = TH.makeHaskellTypes typesList
