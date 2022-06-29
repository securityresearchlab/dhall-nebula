let types = ./pure.dhall

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let LocalAllowListElement
    : Type
    = < InterfacesInfo : Map Text Bool | CIDR : Bool >

let LocalAllowListConfig
    : Type
    = Map Text LocalAllowListElement

let ListenConfig
    : Type
    = { host : Text, port : Natural }

let LighthouseConfig
    : Type
    = { am_lighthouse : Bool
      , interval : Natural
      , hosts : List Text
      , serve_dns : Optional Bool
      , dns : Optional ListenConfig
      , remote_allow_list : Optional (Map Text Bool)
      , local_allow_list : Optional LocalAllowListConfig
      }

let FirewallConnectionConfig
    : Type
    = { tcp_timeout : Text
      , udp_timeout : Text
      , default_timeout : Text
      , max_connections : Natural
      }

let PortConfig
    : Type
    = < Port : Natural | Description : Text >

let BaseRule
    : Type
    = { port : PortConfig
      , proto : types.Proto
      , ca_name : Optional Text
      , ca_sha : Optional Text
      }

let Rule
    : Type
    = < HostRule : BaseRule //\\ { host : Text }
      | GroupRule : BaseRule //\\ { group : Text }
      | GroupsRule : BaseRule //\\ { groups : List Text }
      | CIDRRule : BaseRule //\\ { cidr : Text }
      >

let FirewallConfig
    : Type
    = { conntrack : FirewallConnectionConfig
      , outbound : List Rule
      , inbound : List Rule
      }

let SSHDConfig
    : Type
    = { enabled : Bool
      , listen : Text
      , host_key : Text
      , authorized_users : List types.SSHDUsers
      }

let HostConfig
    : Type
    = { pki : types.PkiInfo
      , static_host_map : Map Text (List Text)
      , lighthouse : LighthouseConfig
      , listen : ListenConfig
      , punchy : types.PunchyInfo
      , tun : types.TunInfo
      , logging : types.LogInfo
      , firewall : FirewallConfig
      , cipher : types.Cipher
      , local_range : Optional Text
      , sshd : Optional SSHDConfig
      }

in  { LighthouseConfig
    , SSHDConfig
    , ListenConfig
    , HostConfig
    , FirewallConnectionConfig
    , FirewallConfig
    , Rule
    , PortConfig
    , LocalAllowListConfig
    , LocalAllowListElement
    }
