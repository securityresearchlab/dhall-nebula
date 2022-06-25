let host = ./host.dhall

let GroupName
    : Type
    = Text

let Group
    : Type
    = { name : GroupName, hosts : List host.Host }

let ConnectionTarget
    : Type
    = < Group : Group | Host : host.Host >

let UnidirectionalConnection
    : Type
    = { from : ConnectionTarget, to : ConnectionTarget }

let ConnectionType
    : Type
    = < GroupConnection : Group
      | UnidirectionalConnection : UnidirectionalConnection
      | AllConnection
      >

let Port
    : Type
    = < Port : Natural | Any >

let Proto
    : Type
    = < Proto : Text | Any >

let Connection
    : Type
    = { port : Port, proto : Proto, type : ConnectionType }

let Network
    : Type
    = { hosts : List host.Host, connections : List Connection }

let ApplyTarget
    : Type
    = < AnyHost
      | Host : host.Host
      | Hosts : List host.Host
      | Group : Group
      | Groups : List Group
      >

let RuleDirection
    : Type
    = < In | Out >

let FirewallRule
    : Type
    = { port : Port
      , proto : Proto
      , applies_to : ApplyTarget
      , direction : RuleDirection
      }

in  { GroupName
    , Group
    , ConnectionType
    , UnidirectionalConnection
    , ConnectionTarget
    , Port
    , Proto
    , Connection
    , Network
    , ApplyTarget
    , RuleDirection
    , FirewallRule
    }
