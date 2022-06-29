let connection = ./connection.dhall

let generics = ./generics.dhall

let types = ../types.dhall

let Bool/and = https://prelude.dhall-lang.org/v21.1.0/Bool/and

let Bool/not = https://prelude.dhall-lang.org/v21.1.0/Bool/not

let List/all = https://prelude.dhall-lang.org/v21.1.0/List/all

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

let List/concat = https://prelude.dhall-lang.org/v21.1.0/List/concat

let List/filter = https://prelude.dhall-lang.org/v21.1.0/List/filter

let List/map = https://prelude.dhall-lang.org/v21.1.0/List/map

let List/null = https://prelude.dhall-lang.org/v21.1.0/List/null

let List/partition = https://prelude.dhall-lang.org/v21.1.0/List/partition

let Map/values = https://prelude.dhall-lang.org/v21.1.0/Map/values

let Natural/equal = https://prelude.dhall-lang.org/v21.1.0/Natural/equal

let Natural/lessThan = https://prelude.dhall-lang.org/v21.1.0/Natural/lessThan

let Natural/lessThanEqual =
      https://prelude.dhall-lang.org/v21.1.0/Natural/lessThanEqual

let isLighthouse
    : types.Host -> Bool
    = \(host : types.Host) ->
        merge
          { Some = \(config : types.IsLighthouseConfig) -> True, None = False }
          host.lighthouse_config

let areIPsUnique
    : List types.IPv4 -> Bool
    = \(ns : List types.IPv4) ->
        let check_ip
            : types.IPv4 -> Bool
            = \(n : types.IPv4) ->
                Natural/equal
                  ( List/length
                      types.IPv4
                      ( List/partition
                          types.IPv4
                          (\(x : types.IPv4) -> generics.areIPv4Equal n x)
                          ns
                      ).true
                  )
                  1

        let checks = List/map types.IPv4 Bool check_ip ns

        in  Bool/and checks

let validateIPv4
    : types.IPv4 -> Bool
    = \(ip : types.IPv4) ->
        let isNAcceptable
            : Natural -> Bool
            = \(n : Natural) -> Natural/lessThanEqual n 255

        in      isNAcceptable ip._1
            &&  isNAcceptable ip._2
            &&  isNAcceptable ip._3
            &&  isNAcceptable ip._4

let validateIPv4WithPort
    : types.IPv4WithPort -> Bool
    = \(ip : types.IPv4WithPort) -> validateIPv4 ip.{ _1, _2, _3, _4 }

let validateHostInterfaces
    : types.Host -> Bool
    = \(host : types.Host) ->
        let listen_validity = validateIPv4 host.listen_interface.host

        let dns_interface_validity =
              merge
                { Some =
                    \(c : types.IsLighthouseConfig) ->
                      merge
                        { Some =
                            \(d : types.DNSConfig) ->
                              validateIPv4 d.dns_interface.host
                        , None
                        }
                        c.dns
                , None = True
                }
                host.lighthouse_config

        in  listen_validity && dns_interface_validity

let validateHost
    : types.Host -> Bool
    = \(host : types.Host) ->
        let sshd_validity =
              merge
                { Some =
                    \(c : types.SSHDInfo) ->
                      Bool/not (Natural/equal c.listen.port 22)
                , None = True
                }
                host.sshd

        let ip_validity = validateIPv4 host.ip

        let static_ips_validity =
              Bool/and
                ( List/map
                    types.IPv4WithPort
                    Bool
                    validateIPv4WithPort
                    host.static_ips
                )

        let interfaces_validity = validateHostInterfaces host

        in      sshd_validity
            &&  ip_validity
            &&  static_ips_validity
            &&  interfaces_validity

let validateHosts
    : List types.Host -> Bool
    = \(hs : List types.Host) ->
        let single_hosts_validity =
              Bool/and (List/map types.Host Bool validateHost hs)

        let hosts_validity =
              areIPsUnique
                (List/map types.Host types.IPv4 (\(h : types.Host) -> h.ip) hs)

        in  single_hosts_validity && hosts_validity

let validateRules
    : List types.FirewallRule -> Bool
    = \(rules : List types.FirewallRule) ->
        List/all
          Bool
          (\(b : Bool) -> b)
          ( List/map
              types.FirewallRule
              Bool
              ( \(rule : types.FirewallRule) ->
                  merge
                    { Port = \(n : Natural) -> True
                    , Range =
                        \(r : types.PortRange) -> Natural/lessThan r.from r.to
                    , Any = True
                    }
                    rule.port
              )
              rules
          )

let validate
    : types.Network -> Type
    = \(network : types.Network) ->
        let expected =
              { hosts_check = True
              , lighthouse_present_check = True
              , rules_check = True
              }

        let rules_lists
            : List (List types.FirewallRule)
            = Map/values
                types.Host
                (List types.FirewallRule)
                (connection.getRules network)

        let rules = List/concat types.FirewallRule rules_lists

        let actual =
              { hosts_check = validateHosts network.hosts
              , lighthouse_present_check =
                  List/any types.Host isLighthouse network.hosts
              , rules_check = validateRules rules
              }

        in  expected === actual

in  { validate }
