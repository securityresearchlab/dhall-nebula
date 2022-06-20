let types = ../types.dhall

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
        , ip = ""
        , is_lighthouse = False
        , pki = PkiInfo.default
        , lighthouse = { interval = 60, hosts = [] : List Text}
        , static_ips = [] : List Text
        , listen_interface = InterfaceInfo.default
        , punchy = True
        , logging = LogInfo.default
        }
      }

in  { PkiInfo, LighthouseInfo, InterfaceInfo, TunInfo, LogInfo, Host }
