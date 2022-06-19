let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let PkiInfo
    : Type
    = { ca : Text, cert : Text, key : Text }

let LighthouseInfo
    : Type
    = { interval : Natural, hosts : List Text }

let InterfaceInfo
    : Type
    = { host : Text, port : Natural }

let TunInfo
    : Type
    = {}

let LogInfo
    : Type
    = { level : Text, format : Text }

let Host
    : Type
    = { id : Natural
      , name : Text
      , is_lighthouse : Bool
      , pki : PkiInfo
      , lighthouse : Optional LighthouseInfo
      , static_hosts : Map Text (List Text)
      , listen_interface : InterfaceInfo
      , punchy : Bool
      , logging : LogInfo
      }

let GroupName
    : Type
    = Text

let Group
    : Type
    = { name : GroupName, hosts : List Host }

let ConnectionTarget
    : Type
    = < Group : Group | Host : Host >

let UnidirectionalConnection
    : Type
    = { from : ConnectionTarget, to : ConnectionTarget }

let ConnectionType
    : Type
    = < BidirectionalConnection : Group
      | UnidirectionalConnection : UnidirectionalConnection
      >

let Port
    : Type
    = < Port : Natural | Any >

let Connection
    : Type
    = { port : Port, proto : Text, type : ConnectionType }

let Network
    : Type
    = { hosts : List Host, connections : List Connection }

let ApplyTarget
    : Type
    = < Host : Host | Hosts : List Host | Group : Group | Groups : List Group >

let RuleDirection
    : Type
    = < In | Out >

let FirewallRule
    : Type
    = { port : Port
      , proto : Text
      , applies_to : ApplyTarget
      , direction : RuleDirection
      }

in  { PkiInfo
    , LighthouseInfo
    , InterfaceInfo
    , TunInfo
    , LogInfo
    , Host
    , GroupName
    , Group
    , ConnectionType
    , UnidirectionalConnection
    , ConnectionTarget
    , Port
    , Connection
    , Network
    , ApplyTarget
    , RuleDirection
    , FirewallRule
    }
