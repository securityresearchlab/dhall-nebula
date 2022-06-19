let nebula = ./package.dhall

let Map/empty = https://prelude.dhall-lang.org/v21.1.0/Map/empty

let lighthouse
    : nebula.Host.Type
    = { id = 0
      , name = "lighthouse"
      , is_lighthouse = True
      , pki = nebula.PkiInfo.default
      , lighthouse = Some nebula.LighthouseInfo.default
      , static_hosts = Map/empty Text (List Text)
      , listen_interface = nebula.InterfaceInfo.default
      , punchy = True
      , logging = nebula.LogInfo.default
      }

let laptop1
    : nebula.Host.Type
    = { id = 1
      , name = "laptop1"
      , is_lighthouse = False
      , pki = nebula.PkiInfo.default
      , lighthouse = None nebula.LighthouseInfo.Type
      , static_hosts = Map/empty Text (List Text)
      , listen_interface = nebula.InterfaceInfo.default
      , punchy = True
      , logging = nebula.LogInfo.default
      }

let laptop2
    : nebula.Host.Type
    = { id = 2
      , name = "laptop2"
      , is_lighthouse = False
      , pki = nebula.PkiInfo.default
      , lighthouse = None nebula.LighthouseInfo.Type
      , static_hosts = Map/empty Text (List Text)
      , listen_interface = nebula.InterfaceInfo.default
      , punchy = True
      , logging = nebula.LogInfo.default
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

in  network
