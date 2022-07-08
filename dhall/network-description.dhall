let nebula = ./package.dhall

let inputs = ./inputs.dhall

let Map/empty = https://prelude.dhall-lang.org/v21.1.0/Map/empty

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let lighthouse_ip = inputs.lighthouse_ip

let ca = inputs.ca_name

let lighthouse_name = "lighthouse"

let lighthouse
    : nebula.Host.Type
    = nebula.Host::{
      , name = lighthouse_name
      , ip = lighthouse_ip
      , lighthouse_config = Some { dns = None nebula.DNSConfig }
      , pki = nebula.mkPkiInfoWithoutBlocklist "/etc/nebula" ca lighthouse_name
      , static_ips = [ nebula.mkIPv4WithPort 10 0 2 4 4242 ]
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      }

let laptop1
    : nebula.Host.Type
    = nebula.Host::{
      , name = "laptop1"
      , ip = nebula.mkIPv4 192 168 100 2
      , pki = nebula.mkPkiInfoWithoutBlocklist "/etc/nebula" ca "laptop1"
      , lighthouse = nebula.LighthouseInfo.default
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      }

let laptop2
    : nebula.Host.Type
    = nebula.Host::{
      , name = "laptop2"
      , ip = nebula.mkIPv4 192 168 100 3
      , pki = nebula.mkPkiInfoWithoutBlocklist "/etc/nebula" ca "laptop2"
      , lighthouse = nebula.LighthouseInfo.default
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      , tun = nebula.TunInfo::{
        , unsafe_routes =
          [ { u_mtu = 8880
            , u_route = nebula.mkIPv4Network 10 0 3 0 24
            , via = nebula.mkIPv4 192 168 100 2
            }
            , { u_mtu = 8880
            , u_route = nebula.mkIPv4Network 10 0 4 0 24
            , via = nebula.mkIPv4 192 168 100 3
            }
          ]
        }
      }

let hosts_list
    : List nebula.Host.Type
    = [ lighthouse, laptop1, laptop2 ]

let all_group
    : nebula.Group
    = { group_name = "all", group_hosts = hosts_list }

let home_group
    : nebula.Group
    = { group_name = "home", group_hosts = [ laptop1, laptop2 ] }

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

in  { network, lighthouse, laptop1, laptop2 }
