let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let IPv4
    : Type
    = { _1 : Natural, _2 : Natural, _3 : Natural, _4 : Natural }

let IPv4WithPort
    : Type
    = IPv4 //\\ { port : Natural }

let IPv4Network =
      { mask : Natural, _1 : Natural, _2 : Natural, _3 : Natural, _4 : Natural }

let CAName
    : Type
    = Text

let Directory
    : Type
    = Text

let HostName
    : Type
    = Text

let PkiInfo
    : Type
    = { ca : Text, cert : Text, key : Text, blocklist : Optional (List Text) }

let InterfaceInfo
    : Type
    = { host : IPv4, port : Natural }

let ListenInfo
    : Type
    =     InterfaceInfo
      //\\  { batch : Optional Natural
            , read_buffer : Optional Natural
            , write_buffer : Optional Natural
            }

let DNSConfig
    : Type
    = { dns_interface : InterfaceInfo }

let IsLighthouseConfig
    : Type
    = { dns : Optional DNSConfig }

let PunchyInfo
    : Type
    = { punch : Bool, respond : Optional Bool, delay : Optional Text }

let LocalAllowListInfo
    : Type
    = { interfaces : Optional (Map Text Bool)
      , cidrs : Optional (Map IPv4Network Bool)
      }

let LighthouseInfo
    : Type
    = { interval : Natural
      , remote_allow_list : Optional (Map IPv4Network Bool)
      , local_allow_list : Optional LocalAllowListInfo
      }

let SSHDUsers
    : Type
    = { user : Text, keys : List Text }

let SSHDInfo
    : Type
    = { listen : InterfaceInfo
      , host_key : Text
      , authorized_users : List SSHDUsers
      }

let TunRoute
    : Type
    = { mtu : Natural, route : Text }

let TunUnsafeRoute
    : Type
    = { mtu : Natural, route : Text, via : Text }

let TunInfo
    : Type
    = { disabled : Bool
      , dev : Text
      , drop_local_broadcast : Bool
      , drop_multicast : Bool
      , tx_queue : Natural
      , mtu : Natural
      , routes : List TunRoute
      , unsafe_routes : List TunUnsafeRoute
      }

let LogInfo
    : Type
    = { level : Text
      , format : Text
      , disable_timestamp : Optional Bool
      , timestamp_format : Optional Text
      }

let Host
    : Type
    = { name : HostName
      , ip : IPv4
      , lighthouse_config : Optional IsLighthouseConfig
      , pki : PkiInfo
      , lighthouse : LighthouseInfo
      , static_ips : List IPv4WithPort
      , listen_interface : ListenInfo
      , punchy : PunchyInfo
      , logging : LogInfo
      , tun : TunInfo
      , local_range : Optional Text
      , sshd : Optional SSHDInfo
      }

in  { IPv4
    , IPv4WithPort
    , IPv4Network
    , CAName
    , Directory
    , HostName
    , PkiInfo
    , DNSConfig
    , IsLighthouseConfig
    , PunchyInfo
    , LocalAllowListInfo
    , LighthouseInfo
    , SSHDUsers
    , SSHDInfo
    , InterfaceInfo
    , ListenInfo
    , TunInfo
    , TunRoute
    , TunUnsafeRoute
    , LogInfo
    , Host
    }
