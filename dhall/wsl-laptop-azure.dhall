let nebula = ./package.dhall

let Map/empty =
      https://prelude.dhall-lang.org/v21.1.0/Map/empty
        sha256:4c612558b8bbe8f955550ed3fb295d57b1b864c85cd52615b52d0ee0e9682e52

let Map =
      https://prelude.dhall-lang.org/v21.1.0/Map/Type

let lighthouse
    : nebula.Host.Type
    = nebula.Host::{
      , name = "lighthouse"
      , ip = nebula.mkIPv4 192 168 100 1
      , lighthouse_config = Some { dns = None nebula.DNSConfig }
      , pki =
          nebula.mkPkiInfoWithoutBlocklist "/etc/nebula" "ca" "lighthouse"
      , static_ips = [ nebula.mkIPv4WithPort 20 63 142 142 4242 ]
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      , am_relay = True
      }

let laptop1
    : nebula.Host.Type
    = nebula.Host::{
      , name = "laptop1"
      , ip = nebula.mkIPv4 192 168 100 2
      , pki = nebula.mkPkiInfoWithoutBlocklist "C:\\Users\\Giorgia\\Documents\\Universita\\Magistrale-Ingegneria_informatica\\Tesi\\nebula-windows-amd64" "ca" "laptop1"
      , lighthouse = nebula.LighthouseInfo.default
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      , relays = [ lighthouse.ip ]
      }

let wsl
    : nebula.Host.Type
    = nebula.Host::{
      , name = "wsl"
      , ip = nebula.mkIPv4 192 168 100 3
      , pki = nebula.mkPkiInfoWithoutBlocklist "/home/gio/tesi" "ca" "wsl"
      , lighthouse = nebula.LighthouseInfo.default
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      , relays = [ lighthouse.ip ]
      }

let hosts_list
    : List nebula.Host.Type
    = [ lighthouse, laptop1, wsl ]

let all_group
    : nebula.Group
    = { group_name = "all", group_hosts = hosts_list }

let home_group
    : nebula.Group
    = { group_name = "home", group_hosts = [ laptop1, wsl ] }

let home_connection
    : nebula.Connection
    = nebula.mkIntraGroupConnection
        nebula.Port.AnyPort
        nebula.Proto.TCP
        home_group
        (None Text)
        (None Text)

let outbound_connection
    : nebula.Connection
    = nebula.mkUnidirectionalConnection
        nebula.Port.AnyPort
        nebula.Proto.AnyProto
        nebula.ConnectionTarget.AnyNebulaHost
        nebula.ConnectionTarget.AnyExternalHost
        (None Text)
        (None Text)

let icmp_connection
    : nebula.Connection
    = nebula.mkUnidirectionalConnection
        nebula.Port.AnyPort
        nebula.Proto.ICMP
        nebula.ConnectionTarget.AnyExternalHost
        nebula.ConnectionTarget.AnyNebulaHost
        (None Text)
        (None Text)

let network
    : nebula.Network
    = { hosts = hosts_list
      , groups = [ all_group, home_group ]
      , connections = [ home_connection, outbound_connection, icmp_connection ]
      , ad_hoc_rules = [] : List nebula.AdHocFirewallRule
      , cipher = nebula.Cipher.AES
      , ip_mask = 24
      }

let _ = assert : nebula.validate network

in  { network, lighthouse, laptop1, wsl }
