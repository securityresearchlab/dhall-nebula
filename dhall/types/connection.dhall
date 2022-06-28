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
      >

let PortRange
    : Type
    = { from : Natural, to : Natural }

let Port
    : Type
    = < Port : Natural | Range : PortRange | Any >

let Proto
    : Type
    = < any | tcp | udp | icmp >

let Connection
    : Type
    = { port : Port, proto : Proto, type : ConnectionType }

let TrafficTarget
    : Type
    = < AnyHost | Host : host.Host | Group : Group | Groups : List Group | CIDR : Text >

let RuleDirection
    : Type
    = < In | Out >

let FirewallRule
    : Type
    = { port : Port
      , proto : Proto
      , traffic_target : TrafficTarget
      , direction : RuleDirection
      , ca_name : Optional Text
      , ca_sha : Optional Text
      }

let AdHocFirewallRule
    : Type
    = { targets : List host.Host } //\\ FirewallRule

let Cipher
    : Type
    = < aes | chachapoly >

let Network
    : Type
    = { hosts : List host.Host
      , connections : List Connection
      , ad_hoc_rules : List AdHocFirewallRule
      , cipher : Cipher
      }

in  { GroupName
    , Group
    , ConnectionType
    , UnidirectionalConnection
    , ConnectionTarget
    , Port
    , PortRange
    , Proto
    , Connection
    , Cipher
    , Network
    , TrafficTarget
    , RuleDirection
    , FirewallRule
    , AdHocFirewallRule
    }
