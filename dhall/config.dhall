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

let home_group
    : nebula.Group
    = { name = "home", hosts = [ laptop1, laptop2 ] }

let home_connection
    : nebula.Connection
    = { port = nebula.Port.Any
      , proto = nebula.Proto.tcp
      , type = nebula.ConnectionType.GroupConnection home_group
      }

let allow_all_outbound_rule
    : nebula.AdHocFirewallRule
    = { port = nebula.Port.Any
      , proto = nebula.Proto.any
      , traffic_target = nebula.TrafficTarget.AnyHost
      , direction = nebula.RuleDirection.Out
      , targets = hosts_list
      , ca_name = None Text
      , ca_sha = None Text
      }

let allow_icmp_rule
    : nebula.AdHocFirewallRule
    = { port = nebula.Port.Port 22
      , proto = nebula.Proto.icmp
      , traffic_target = nebula.TrafficTarget.AnyHost
      , direction = nebula.RuleDirection.In
      , targets = hosts_list
      , ca_name = None Text
      , ca_sha = None Text
      }

let network
    : nebula.Network
    = { hosts = hosts_list
      , connections = [ home_connection ]
      , ad_hoc_rules = [ allow_all_outbound_rule, allow_icmp_rule ]
      , cipher = nebula.Cipher.aes
      }

let _ = assert : nebula.validate network

in  nebula.generateHostConfig network lighthouse
