let types = ./pure.dhall

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let LighthouseConfig
    : Type
    = { am_lighthouse : Bool
      , interval : Natural
      , hosts : List Text
      , serve_dns : Optional Bool
      , dns : Optional types.InterfaceInfo
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

let ProtoConfig
    : Type
    = < Proto : Text | Description : Text >

let Rule
    : Type
    = < HostRule : { port : PortConfig, proto : ProtoConfig, host : Text }
      | HostsRule :
          { port : PortConfig, proto : ProtoConfig, hosts : List Text }
      | GroupRule : { port : PortConfig, proto : ProtoConfig, group : Text }
      | GroupsRule :
          { port : PortConfig, proto : ProtoConfig, groups : List Text }
      >

let FirewallConfig
    : Type
    = { conntrack : FirewallConnectionConfig
      , outbound : List Rule
      , inbound : List Rule
      }

let HostConfig
    : Type
    = { pki : types.PkiInfo
      , static_host_map : Map Text (List Text)
      , lighthouse : LighthouseConfig
      , listen : types.ListenInfo
      , punchy : types.PunchyInfo
      , tun : types.TunInfo
      , logging : types.LogInfo
      , firewall : FirewallConfig
      , cipher : types.Cipher
      , local_range : Optional Text
      }

in  { LighthouseConfig
    , HostConfig
    , FirewallConnectionConfig
    , FirewallConfig
    , Rule
    , PortConfig
    , ProtoConfig
    }
