let types = ./types.dhall

let Map/empty = https://prelude.dhall-lang.org/v21.1.0/Map/empty

let PkiInfo =
      { Type = types.PkiInfo
      , default =
        { ca = "/etc/nebula/ca.crt"
        , cert = "/etc/nebula/host.crt"
        , key = "/etc/nebula/host.key"
        }
      }

let LighthouseInfo =
      { Type = types.LighthouseInfo
      , default = { interval = 60, hosts = [ "192.168.100.1" ] }
      }

let InterfaceInfo =
      { Type = types.InterfaceInfo
      , default = { host = "0.0.0.0", port = 4242 }
      }

let TunInfo = { Type = types.TunInfo, default = {=} }

let LogInfo =
      { Type = types.LogInfo, default = { level = "info", format = "text" } }

let Host =
      { Type = types.Host
      , default =
        { id = 0
        , name = ""
        , is_lighthouse = False
        , pki = PkiInfo.default
        , lighthouse = None LighthouseInfo.Type
        , static_hosts = Map/empty
        , listen_interface = InterfaceInfo.default
        , punchy = True
        , logging = LogInfo.default
        }
      }

in  { PkiInfo, LighthouseInfo, InterfaceInfo, TunInfo, LogInfo, Host }
