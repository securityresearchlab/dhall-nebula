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
      , pki =
          nebula.mkPkiInfoWithoutBlocklist inputs.config_dir ca lighthouse_name
      , static_ips = [ nebula.mkIPv4WithPort 20 63 142 142 4242 ]
      }

let laptop1
    : nebula.Host.Type
    = nebula.Host::{
      , name = "laptop1"
      , ip = nebula.mkIPv4 192 168 100 2
      , pki = nebula.mkPkiInfoWithoutBlocklist inputs.config_dir ca "laptop1"
      , lighthouse = nebula.LighthouseInfo::{
        , remote_allow_list = Some
          [ { mapKeyIB = nebula.mkIPv4Network 192 168 1 18 24, mapValueIB = False }
          , { mapKeyIB = nebula.mkIPv4Network 192 168 1 18 26, mapValueIB = True }
          , { mapKeyIB = nebula.mkIPv4Network 0 0 0 0 0, mapValueIB = True }
          ]
        }
      }

let laptop2
    : nebula.Host.Type
    = nebula.Host::{
      , name = "laptop2"
      , ip = nebula.mkIPv4 192 168 100 3
      , pki = nebula.mkPkiInfoWithoutBlocklist inputs.config_dir ca "laptop2"
      , lighthouse = nebula.LighthouseInfo::{
        , local_allow_list = Some
          { interfaces = Some [ { mapKeyTB = "tun0", mapValueTB = True } ]
          , cidrs = Some
            [ { mapKeyIB = nebula.mkIPv4Network 192 168 1 18 24
              , mapValueIB = False
              }
            ]
          }
        }
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
    = nebula.mkIntraGroupConnection
        nebula.Port.Any
        nebula.Proto.tcp
        home_group
        (None Text)
        (None Text)

let outbound_connection
    : nebula.Connection
    = nebula.mkUnidirectionalConnection
        nebula.Port.Any
        nebula.Proto.any
        nebula.ConnectionTarget.AnyNebulaHost
        nebula.ConnectionTarget.AnyExternalHost
        (None Text)
        (None Text)

let icmp_connection
    : nebula.Connection
    = nebula.mkUnidirectionalConnection
        nebula.Port.Any
        nebula.Proto.icmp
        nebula.ConnectionTarget.AnyExternalHost
        nebula.ConnectionTarget.AnyNebulaHost
        (None Text)
        (None Text)

let network
    : nebula.Network
    = { hosts = hosts_list
      , connections = [ home_connection, outbound_connection, icmp_connection ]
      , ad_hoc_rules = [] : List nebula.AdHocFirewallRule
      , cipher = nebula.Cipher.aes
      }

let _ = assert : nebula.validate network

in  { network, lighthouse, laptop1, laptop2 }