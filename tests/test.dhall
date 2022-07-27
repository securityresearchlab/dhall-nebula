let nebula = ../dhall/package.dhall

let List/map = https://prelude.dhall-lang.org/v21.1.0/List/map

let List/iterate = https://prelude.dhall-lang.org/v21.1.0/List/iterate

let List/unpackOptionals =
      https://prelude.dhall-lang.org/v21.1.0/List/unpackOptionals

let Natural/lessThan = https://prelude.dhall-lang.org/v21.1.0/Natural/lessThan

let nClients = 10

let lighthouse
    : nebula.Host.Type
    = nebula.Host::{
      , name = "lighthouse"
      , ip = nebula.mkIPv4 192 168 0 1
      , lighthouse_config = Some { dns = None nebula.DNSConfig }
      , pki = nebula.mkPkiInfo "/etc/nebula" "ca" "lighthouse"
      , static_ips = [ nebula.mkIPv4WithPort 20 63 142 142 4242 ]
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      , am_relay = True
      }

let server
    : nebula.Host.Type
    = nebula.Host::{
      , name = "server"
      , ip = nebula.mkIPv4 192 168 0 2
      , pki = nebula.mkPkiInfo "/etc/nebula" "ca" "server"
      , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
      }

let nextIPv4
    : Optional nebula.IPv4 -> Optional nebula.IPv4
    = \(mip : Optional nebula.IPv4) ->
        let f =
              \(ip : nebula.IPv4) ->
                let newI4 = ip.i4 + 1

                let newI3 = ip.i3 + 1

                let newI2 = ip.i2 + 1

                let newI1 = ip.i1 + 1

                in  if    Natural/lessThan newI4 255
                    then  Some (nebula.mkIPv4 ip.i1 ip.i2 ip.i3 newI4)
                    else  if Natural/lessThan newI3 255
                    then  Some (nebula.mkIPv4 ip.i1 ip.i2 newI3 0)
                    else  if Natural/lessThan newI2 255
                    then  Some (nebula.mkIPv4 ip.i1 newI2 0 0)
                    else  if Natural/lessThan newI1 255
                    then  Some (nebula.mkIPv4 newI1 0 0 0)
                    else  None nebula.IPv4

        in  merge { Some = f, None = None nebula.IPv4 } mip

let ipToHost
    : nebula.IPv4 -> nebula.Host.Type
    = \(ip : nebula.IPv4) ->
        let n = Natural/show ip.i3 ++ Natural/show ip.i4

        let name = "client" ++ n

        in  nebula.Host::{
            , name
            , ip
            , pki = nebula.mkPkiInfo "/etc/nebula" "ca" name
            , lighthouse = nebula.LighthouseInfo.default
            , punchy = nebula.PunchyInfo::{ punch = True, respond = Some True }
            , relays = [ lighthouse.ip ]
            }

let clients =
      List/map
        nebula.IPv4
        nebula.Host.Type
        ipToHost
        ( List/unpackOptionals
            nebula.IPv4
            ( List/iterate
                nClients
                (Optional nebula.IPv4)
                nextIPv4
                (Some (nebula.mkIPv4 192 168 0 3))
            )
        )

let hosts = [ lighthouse, server ] # clients

let all_connection =
      nebula.mkBidirectionalConnection
        nebula.Port.AnyPort
        nebula.Proto.AnyProto
        nebula.ConnectionTarget.AnyNebulaHost
        nebula.ConnectionTarget.AnyNebulaHost
        (None Text)
        (None Text)

let network
    : nebula.Network
    = { hosts
      , groups = [] : List nebula.Group
      , connections = [ all_connection ]
      , blocklist = [] : List Text
      , cipher = nebula.Cipher.AES
      , ip_mask = 24
      }

let _ = assert : nebula.validate network

in  network
