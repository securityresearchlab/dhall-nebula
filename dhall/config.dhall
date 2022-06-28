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
      , lighthouse_config = Some
        { dns = Some { dns_interface = { host = "0.0.0.0", port = 53 } } }
      , pki =
          nebula.mkPkiInfoWithoutBlocklist
            inputs.lighthouse_dir
            "ca"
            lighthouse_name
      , lighthouse.interval = 60
      , static_ips = [ "20.20.20.20:4242" ]
      , listen_interface = nebula.ListenInfo.default
      , punchy = nebula.PunchyInfo.default
      , logging = nebula.LogInfo.default
      , tun = nebula.TunInfo.default
      , local_range = None Text
      , sshd = None nebula.SSHDInfo
      }

let laptop1
    : nebula.Host.Type
    = { id = 1
      , name = "laptop1"
      , ip = "192.168.100.2"
      , lighthouse_config = None nebula.IsLighthouseConfig
      , pki = nebula.PkiInfo.default
      , lighthouse.interval = 60
      , static_ips = [] : List Text
      , listen_interface = nebula.ListenInfo.default
      , punchy = nebula.PunchyInfo.default
      , logging = nebula.LogInfo.default
      , tun = nebula.TunInfo.default
      , local_range = None Text
      , sshd = None nebula.SSHDInfo
      }

let laptop2
    : nebula.Host.Type
    = { id = 2
      , name = "laptop2"
      , ip = "192.168.100.3"
      , lighthouse_config = None nebula.IsLighthouseConfig
      , pki = nebula.PkiInfo.default
      , lighthouse.interval = 60
      , static_ips = [] : List Text
      , listen_interface = nebula.ListenInfo.default
      , punchy = nebula.PunchyInfo.default
      , logging = nebula.LogInfo.default
      , tun = nebula.TunInfo.default
      , local_range = None Text
      , sshd = None nebula.SSHDInfo
      }

let hosts_list
    : List nebula.Host.Type
    = [ lighthouse, laptop1, laptop2 ]

let all_group
    : nebula.Group
    = { name = "all", hosts = hosts_list }

let home_group
    : nebula.Group
    = { name = "home", hosts = [ laptop1, laptop2 ] }

let home_connection
    : nebula.Connection
    = { port = nebula.Port.Any
      , proto = nebula.Proto.tcp
      , type = nebula.ConnectionType.GroupConnection home_group
      }

let outbound_connection
    : nebula.Connection
    = { port = nebula.Port.Any
      , proto = nebula.Proto.any
      , type =
          nebula.ConnectionType.FreeConnection
            { target = nebula.TrafficTarget.AnyHost
            , direction = nebula.Direction.Out
            }
      }

let outbound_connection
    : nebula.Connection
    = { port = nebula.Port.Any
      , proto = nebula.Proto.any
      , type =
          nebula.ConnectionType.FreeConnection
            { target = nebula.TrafficTarget.AnyHost
            , direction = nebula.Direction.Out
            }
      }

let icmp_connection
    : nebula.Connection
    = { port = nebula.Port.Port 22
      , proto = nebula.Proto.icmp
      , type =
          nebula.ConnectionType.FreeConnection
            { target = nebula.TrafficTarget.AnyHost
            , direction = nebula.Direction.In
            }
      }

let network
    : nebula.Network
    = { hosts = hosts_list
      , connections = [ home_connection, outbound_connection, icmp_connection ]
      , cipher = nebula.Cipher.aes
      }

let _ = assert : nebula.validate network

in  nebula.generateHostConfig network lighthouse
