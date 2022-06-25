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
    = { host : Text, port : Natural }

let DNSConfig
    : Type
    = { dns_interface : InterfaceInfo }

let IsLighthouseConfig
    : Type
    = { dns : Optional DNSConfig }

let LighthouseInfo
    : Type
    = { interval : Natural, hosts : List Text }

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
    = { id : Natural
      , name : HostName
      , ip : Text
      , lighthouse_config : Optional IsLighthouseConfig
      , pki : PkiInfo
      , lighthouse : LighthouseInfo
      , static_ips : List Text
      , listen_interface : InterfaceInfo
      , punchy : Bool
      , logging : LogInfo
      , tun : TunInfo
      }

in  { CAName
    , Directory
    , HostName
    , PkiInfo
    , DNSConfig
    , IsLighthouseConfig
    , LighthouseInfo
    , InterfaceInfo
    , TunInfo
    , TunRoute
    , TunUnsafeRoute
    , LogInfo
    , Host
    }
