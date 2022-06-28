let host = ./host.dhall

let GroupName
    : Type
    = Text

let Group
    : Type
    = { name : GroupName, hosts : List host.Host }

let Direction
    : Type
    = < In | Out >

let ConnectionTarget
    : Type
    = < Group : Group | Host : host.Host >

let TrafficTarget
    : Type
    = < AnyHost
      | Host : host.Host
      | Group : Group
      | Groups : List Group
      | CIDR : Text
      >

let UnidirectionalConnection
    : Type
    = { from : ConnectionTarget, to : ConnectionTarget }

let FreeConnection
    : Type
    = { target : TrafficTarget, direction : Direction }

let ConnectionType
    : Type
    = < GroupConnection : Group -- allows connectivity between all group hosts, inbound and outbound
      | UnidirectionalConnection : UnidirectionalConnection -- allows connectivity from a ConnectionTarget to another
      | FreeConnection : FreeConnection -- allows connectivity, in the specified direction, for all hosts, from/to the given traffic target
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

let FirewallRule
    : Type
    = { port : Port
      , proto : Proto
      , traffic_target : TrafficTarget
      , direction : Direction
      , ca_name : Optional Text
      , ca_sha : Optional Text
      }

let Cipher
    : Type
    = < aes | chachapoly >

let Network
    : Type
    = { hosts : List host.Host, connections : List Connection, cipher : Cipher }

in  { GroupName
    , Group
    , ConnectionType
    , UnidirectionalConnection
    , FreeConnection
    , ConnectionTarget
    , Port
    , PortRange
    , Proto
    , Connection
    , Cipher
    , Network
    , TrafficTarget
    , Direction
    , FirewallRule
    }
