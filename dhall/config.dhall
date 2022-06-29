let nebula = ./package.dhall

let inputs = ./config-input.dhall

let Map/empty = https://prelude.dhall-lang.org/v21.1.0/Map/empty

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let lighthouse_ip = inputs.lighthouse_ip

let lighthouse_name = "lighthouse"

let lighthouse
    : nebula.Host.Type
    = nebula.Host::{
      , name = lighthouse_name
      , ip = lighthouse_ip
      , lighthouse_config = Some
        { dns = Some
          { dns_interface = { host = nebula.mkIPv4 0 0 0 0, port = 53 } }
        }
      , pki =
          nebula.mkPkiInfoWithoutBlocklist
            inputs.lighthouse_dir
            "ca"
            lighthouse_name
      , static_ips = [ nebula.mkIPv4WithPort 20 20 20 20 4242 ]
      }

let laptop1
    : nebula.Host.Type
    = nebula.Host::{
      , name = "laptop1"
      , ip = nebula.mkIPv4 192 168 100 2
      , lighthouse = nebula.LighthouseInfo::{
        , remote_allow_list = Some
          [ { mapKey = nebula.mkIPv4Network 192 168 1 18 24, mapValue = False }
          , { mapKey = nebula.mkIPv4Network 192 168 1 18 26, mapValue = True }
          , { mapKey = nebula.mkIPv4Network 0 0 0 0 0, mapValue = True }
          ]
        }
      }

let laptop2
    : nebula.Host.Type
    = nebula.Host::{
      , name = "laptop2"
      , ip = nebula.mkIPv4 192 168 100 3
      , lighthouse = nebula.LighthouseInfo::{
        , local_allow_list = Some
          { interfaces = Some [ { mapKey = "tun0", mapValue = True } ]
          , cidrs = Some
            [ { mapKey = nebula.mkIPv4Network 192 168 1 18 24
              , mapValue = False
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
        (nebula.Port.Port 22)
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

in  nebula.generateHostConfig network laptop2
