let types = ./pure.dhall

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let LighthouseConfig
    : Type
    = { am_lighthouse : Bool, interval : Natural, hosts : List Text }

let PunchyConfig
    : Type
    = { punch : Bool }

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

let Rule
    : Type
    = < HostRule : { port : PortConfig, proto : Text, host : Text }
      | HostsRule : { port : PortConfig, proto : Text, hosts : List Text }
      | GroupRule : { port : PortConfig, proto : Text, group : Text }
      | GroupsRule : { port : PortConfig, proto : Text, groups : List Text }
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
      , ip : Text
      , static_host_map : Map Text (List Text)
      , lighthouse : LighthouseConfig
      , listen : types.InterfaceInfo
      , punchy : PunchyConfig
      , tun : types.TunInfo
      , logging : types.LogInfo
      , firewall : FirewallConfig
      }

in  { LighthouseConfig
    , HostConfig
    , PunchyConfig
    , FirewallConnectionConfig
    , FirewallConfig
    , Rule
    , PortConfig
    }
