let types = ../types.dhall

let PkiInfo =
      { Type = types.PkiInfo
      , default =
        { ca = "/etc/nebula/ca.crt"
        , cert = "/etc/nebula/host.crt"
        , key = "/etc/nebula/host.key"
        , blocklist = None (List Text)
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

let TunInfo =
      { Type = types.TunInfo
      , default =
        { disabled = False
        , dev = "nebula"
        , drop_local_broadcast = False
        , drop_multicast = False
        , tx_queue = 500
        , mtu = 1300
        , routes = [] : List types.TunRoute
        , unsafe_routes = [] : List types.TunUnsafeRoute
        }
      }

let LogInfo =
      { Type = types.LogInfo
      , default =
        { level = "info"
        , format = "text"
        , disable_timestamp = None Bool
        , timestamp_format = None Text
        }
      }

let Host =
      { Type = types.Host
      , default =
        { id = 0
        , name = ""
        , ip = ""
        , lighthouse_config = None types.IsLighthouseConfig
        , pki = PkiInfo.default
        , lighthouse = { interval = 60, hosts = [] : List Text }
        , static_ips = [] : List Text
        , listen_interface = InterfaceInfo.default
        , punchy = True
        , logging = LogInfo.default
        , tun = TunInfo.default
        }
      }

in  { PkiInfo, LighthouseInfo, InterfaceInfo, TunInfo, LogInfo, Host }
