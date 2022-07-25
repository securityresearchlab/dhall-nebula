let host = ./host.dhall

let GroupName
    : Type
    = Text

let Group
    : Type
    = { group_name : GroupName, group_hosts : List host.Host }

let RuleDirection
    : Type
    = < In | Out >

let ConnectionTarget
    : Type
    = < CTGroup : Group
      | CTHost : host.Host
      | CTCidr : host.IPv4Network
      | AnyNebulaHost
      | AnyExternalHost
      >

let TrafficTarget
    : Type
    = < AnyHost
      | TTHost : host.Host
      | TTGroup : Group
      | Groups : List Group
      | TTCidr : host.IPv4Network
      >

let PortRange
    : Type
    = { r_from : Natural, r_to : Natural }

let Port
    : Type
    = < Port : Natural | Range : PortRange | AnyPort | Fragment >

let Proto
    : Type
    = < AnyProto | TCP | UDP | ICMP >

let UnidirectionalConnection
    : Type
    = { uc_port : Port
      , uc_proto : Proto
      , from : ConnectionTarget
      , to : ConnectionTarget
      , ca_name : Optional Text
      , ca_sha : Optional Text
      }

let Connection
    : Type
    = List UnidirectionalConnection

let FirewallRule
    : Type
    = { fr_port : Port
      , fr_proto : Proto
      , traffic_target : TrafficTarget
      , direction : RuleDirection
      , fr_ca_name : Optional Text
      , fr_ca_sha : Optional Text
      }

let AdHocFirewallRule
    : Type
    = { target : host.Host
      , ah_port : Port
      , ah_proto : Proto
      , ah_traffic_target : TrafficTarget
      , ah_direction : RuleDirection
      , ah_ca_name : Optional Text
      , ah_ca_sha : Optional Text
      }

let Cipher
    : Type
    = < AES | Chachapoly >

let Network
    : Type
    = { hosts : List host.Host
      , groups : List Group
      , connections : List Connection
      , ad_hoc_rules : List AdHocFirewallRule
      , blocklist : List Text
      , cipher : Cipher
      , ip_mask : Natural
      }

in  { GroupName
    , Group
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
