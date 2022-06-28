let host = ./host.dhall

let GroupName
    : Type
    = Text

let Group
    : Type
    = { name : GroupName, hosts : List host.Host }

let RuleDirection
    : Type
    = < In | Out >

let IPv4Network =
      { mask : Natural, _1 : Natural, _2 : Natural, _3 : Natural, _4 : Natural }

let ConnectionTarget
    : Type
    = < Group : Group | Host : host.Host | CIDR : IPv4Network | AnyNebulaHost | AnyExternalHost >

let TrafficTarget
    : Type
    = < AnyHost
      | Host : host.Host
      | Group : Group
      | Groups : List Group
      | CIDR : IPv4Network
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

let UnidirectionalConnection
    : Type
    = { port : Port
      , proto : Proto
      , from : ConnectionTarget
      , to : ConnectionTarget
      }

let Connection
    : Type
    = List UnidirectionalConnection

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
    = { target : host.Host } //\\ FirewallRule

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
    , IPv4Network
    , UnidirectionalConnection
    , Connection
    , ConnectionTarget
    , Port
    , PortRange
    , Proto
    , Cipher
    , Network
    , TrafficTarget
    , RuleDirection
    , FirewallRule
    , AdHocFirewallRule
    }
