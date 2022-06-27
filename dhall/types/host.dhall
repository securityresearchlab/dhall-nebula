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

let ListenInfo
    : Type
    = { host : Text
      , port : Natural
      , batch : Optional Natural
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

let LighthouseInfo
    : Type
    = { interval : Natural }

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
    = { id : Natural
      , name : HostName
      , ip : Text
      , lighthouse_config : Optional IsLighthouseConfig
      , pki : PkiInfo
      , lighthouse : LighthouseInfo
      , static_ips : List Text
      , listen_interface : ListenInfo
      , punchy : PunchyInfo
      , logging : LogInfo
      , tun : TunInfo
      , local_range : Optional Text
      , sshd : Optional SSHDInfo
      }

in  { CAName
    , Directory
    , HostName
    , PkiInfo
    , DNSConfig
    , IsLighthouseConfig
    , PunchyInfo
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
