let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let Map/empty = https://prelude.dhall-lang.org/v21.1.0/Map/empty

let PkiInfo =
      { Type = { ca : Text, cert : Text, key : Text }
      , default =
        { ca = "/etc/nebula/ca.crt"
        , cert = "/etc/nebula/host.crt"
        , key = "/etc/nebula/host.key"
        }
      }

let LighthouseInfo =
      { Type = { interval : Integer, hosts : List Text }
      , default = { interval = 60, hosts = [ "192.168.100.1" ] }
      }

let InterfaceInfo =
      { Type = { host : Text, port : Natural }
      , default = { host = "0.0.0.0", port = 4242 }
      }

let TunInfo = { Type = {}, default = {=} }

let LogInfo =
      { Type = { level : Text, format : Text }
      , default = { level = "info", format = "text" }
      }

let Host =
      { Type =
          { is_lighthouse : Bool
          , pki : PkiInfo.Type
          , lighthouse : Optional LighthouseInfo.Type
          , static_hosts : Map Text (List Text)
          , listen_interface : InterfaceInfo.Type
          , punchy : Bool
          , tun_info : TunInfo.Type
          , logging : LogInfo.Type
          }
      , default =
        { is_lighthouse = False
        , pki = PkiInfo.default
        , lighthouse = None LighthouseInfo.Type
        , static_hosts = Map/empty
        , listen_interface = InterfaceInfo.default
        , punchy = True
        , tun_info = TunInfo.default
        , logging = LogInfo.default
        }
      }

let GroupName
    : Type
    = Text

let Group
    = {name : GroupName, hosts : List Host.Type }

let Connection
    : Type
    = { port : Text, proto : Text, group : GroupName }

let lighthouse
    : Host.Type
    = { is_lighthouse = False
      , pki = PkiInfo.default
      , lighthouse = None LighthouseInfo.Type
      , static_hosts = Map/empty Text (List Text)
      , listen_interface = InterfaceInfo.default
      , punchy = True
      , tun_info = TunInfo.default
      , logging = LogInfo.default
      }

in  lighthouse
