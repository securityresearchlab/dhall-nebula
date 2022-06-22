let nebula = ./package.dhall

let inputs = ./config-input.dhall

let Map/empty = https://prelude.dhall-lang.org/v21.1.0/Map/empty

let lighthouse_ip = inputs.lighthouse_ip
let lighthouse_name = "lighthouse"

let lighthouse
    : nebula.Host.Type
    = { id = 0
      , name = lighthouse_name
      , ip = lighthouse_ip
      , lighthouse_config = Some { dns = Some { dns_interface = { host = "0.0.0.0", port = 53 }} }
      , pki = nebula.mkPkiInfoWithoutBlocklist inputs.lighthouse_dir "ca" lighthouse_name
      , lighthouse = { interval = 60, hosts = [] : List Text }
      , static_ips = [] : List Text
      , listen_interface = nebula.InterfaceInfo.default
      , punchy = True
      , logging = nebula.LogInfo.default
      , tun = nebula.TunInfo.default
      }

let laptop1
    : nebula.Host.Type
    = { id = 1
      , name = "laptop1"
      , ip = "192.168.100.2"
      , lighthouse_config = None nebula.IsLighthouseConfig
      , pki = nebula.PkiInfo.default
      , lighthouse = { interval = 60, hosts = [ lighthouse_ip ] }
      , static_ips = [] : List Text
      , listen_interface = nebula.InterfaceInfo.default
      , punchy = True
      , logging = nebula.LogInfo.default
      , tun = nebula.TunInfo.default
      }

let laptop2
    : nebula.Host.Type
    = { id = 2
      , name = "laptop2"
      , ip = "192.168.100.3"
      , lighthouse_config = None nebula.IsLighthouseConfig
      , pki = nebula.PkiInfo.default
      , lighthouse = { interval = 60, hosts = [ lighthouse_ip ] }
      , static_ips = [] : List Text
      , listen_interface = nebula.InterfaceInfo.default
      , punchy = True
      , logging = nebula.LogInfo.default
      , tun = nebula.TunInfo.default
      }

let home_group
    : nebula.Group
    = { name = "home", hosts = [ laptop1, laptop2 ] }

let home_connection
    : nebula.Connection
    = { port = nebula.Port.Any
      , proto = "tcp"
      , type = nebula.ConnectionType.BidirectionalConnection home_group
      }

let network
    : nebula.Network
    = { hosts = [ lighthouse, laptop1, laptop2 ]
      , connections = [ home_connection ]
      }

in  nebula.generateHostConfig network lighthouse
