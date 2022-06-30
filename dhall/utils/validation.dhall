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

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let Map/values = https://prelude.dhall-lang.org/v21.1.0/Map/values

let Map/keys = https://prelude.dhall-lang.org/v21.1.0/Map/keys

let Natural/equal = https://prelude.dhall-lang.org/v21.1.0/Natural/equal

let Natural/lessThan = https://prelude.dhall-lang.org/v21.1.0/Natural/lessThan

let Natural/lessThanEqual =
      https://prelude.dhall-lang.org/v21.1.0/Natural/lessThanEqual

let LocalAllowListChecks
    : Type
    = { cidrs_validites : Bool, interfaces_validities : Bool }

let LighthouseAllowListsChecks
    : Type
    = { remote_list_validities : Bool
      , local_list_validities : LocalAllowListChecks
      }

let HostChecks
    : Type
    = { sshd_validities : Bool
      , ip_validities : Bool
      , static_ips_validity : Bool
      , interfaces_validities : Bool
      , lighthouse_allow_lists_validities : LighthouseAllowListsChecks
      }

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

        in      isNAcceptable ip.i1
            &&  isNAcceptable ip.i2
            &&  isNAcceptable ip.i3
            &&  isNAcceptable ip.i4

let validateIPv4WithPort
    : types.IPv4WithPort -> Bool
    = \(ip : types.IPv4WithPort) -> validateIPv4 { i1 = ip.ip1, i2 = ip.ip2, i3 = ip.ip3, i4 = ip.ip4 }

let validateIPv4Network
    : types.IPv4Network -> Bool
    = \(n : types.IPv4Network) ->
        validateIPv4 { i1 = n.in1, i2 = n.in2, i3 = n.in3, i4 = n.in4 } && Natural/lessThan n.mask 32

let validateHostInterfaces
    : types.Host -> Bool
    = \(host : types.Host) ->
        let listen_validity = validateIPv4 host.listen_interface.l_host

        let dns_interface_validity =
              merge
                { Some =
                    \(c : types.IsLighthouseConfig) ->
                      merge
                        { Some =
                            \(d : types.DNSConfig) ->
                              validateIPv4 d.dns_interface.host
                        , None = True
                        }
                        c.dns
                , None = True
                }
                host.lighthouse_config

        in  listen_validity && dns_interface_validity

let listValidation
    : Map types.IPv4Network Bool -> Bool
    = \(m : Map types.IPv4Network Bool) ->
        let isDefaultNetwork
            : types.IPv4Network -> Bool
            = \(n : types.IPv4Network) ->
                    Natural/equal n.in1 0
                &&  Natural/equal n.in2 0
                &&  Natural/equal n.in3 0
                &&  Natural/equal n.in4 0
                &&  Natural/equal n.mask 0

        let values = Map/values types.IPv4Network Bool m

        let keys = Map/keys types.IPv4Network Bool m

        let cidrs_validities =
              Bool/and
                (List/map types.IPv4Network Bool validateIPv4Network keys)

        in  if        Bool/and values
                  ||  Bool/and (List/map Bool Bool Bool/not values)
            then  cidrs_validities
            else      cidrs_validities
                  &&  Bool/not
                        ( List/null
                            types.IPv4Network
                            ( List/filter
                                types.IPv4Network
                                isDefaultNetwork
                                keys
                            )
                        )

let validateRemoteAllowList
    : types.Host -> Bool
    = \(host : types.Host) ->
        merge
          { Some = \(list : List types.IPv4NetworkBoolMapEntry) -> listValidation (generics.fromIPv4NetworkBoolMapToMap list)
          , None = True
          }
          host.lighthouse.remote_allow_list

let validateLocalCIDRAllowLists
    : types.Host -> Bool
    = \(host : types.Host) ->
        let cidrs =
              merge
                { Some =
                    \(info : types.LocalAllowListInfo) ->
                      merge
                        { Some = \(c : List types.IPv4NetworkBoolMapEntry) -> Some c
                        , None = None (List types.IPv4NetworkBoolMapEntry)
                        }
                        info.cidrs
                , None = None (List types.IPv4NetworkBoolMapEntry)
                }
                host.lighthouse.local_allow_list

        let cidrs_validity =
              merge
                { Some =
                    \(list : List types.IPv4NetworkBoolMapEntry) -> listValidation (generics.fromIPv4NetworkBoolMapToMap list)
                , None = True
                }
                cidrs

        in  cidrs_validity

let validateLocalInterfacesAllowLists
    : types.Host -> Bool
    = \(host : types.Host) ->
        let interfaces =
              merge
                { Some =
                    \(info : types.LocalAllowListInfo) ->
                      merge
                        { Some = \(i : List types.TextBoolMapEntry) -> Some i
                        , None = None (List types.TextBoolMapEntry)
                        }
                        info.interfaces
                , None = None (List types.TextBoolMapEntry)
                }
                host.lighthouse.local_allow_list

        in  merge
              { Some =
                  \(list : List types.TextBoolMapEntry) ->
                    let values = List/map types.TextBoolMapEntry Bool (\(e : types.TextBoolMapEntry) -> e.mapValueTB) list

                    in      Bool/and values
                        ||  Bool/and (List/map Bool Bool Bool/not values)
              , None = True
              }
              interfaces

let validateHostSSHD
    : types.Host -> Bool
    = \(host : types.Host) ->
        merge
          { Some =
              \(c : types.SSHDInfo) -> Bool/not (Natural/equal c.listen.port 22)
          , None = True
          }
          host.sshd

let validateHostIPv4
    : types.Host -> Bool
    = \(host : types.Host) -> validateIPv4 host.ip

let validateHostStaticIPv4s
    : types.Host -> Bool
    = \(host : types.Host) ->
        Bool/and
          ( List/map
              types.IPv4WithPort
              Bool
              validateIPv4WithPort
              host.static_ips
          )

let validateHostsAsNetwork
    : List types.Host -> Bool
    = \(hs : List types.Host) ->
        areIPsUnique
          (List/map types.Host types.IPv4 (\(h : types.Host) -> h.ip) hs)

let validateSingleHosts
    : List types.Host -> HostChecks
    = \(hs : List types.Host) ->
        { sshd_validities =
            Bool/and (List/map types.Host Bool validateHostSSHD hs)
        , ip_validities =
            Bool/and (List/map types.Host Bool validateHostIPv4 hs)
        , static_ips_validity =
            Bool/and (List/map types.Host Bool validateHostStaticIPv4s hs)
        , interfaces_validities =
            Bool/and (List/map types.Host Bool validateHostInterfaces hs)
        , lighthouse_allow_lists_validities =
          { remote_list_validities =
              Bool/and (List/map types.Host Bool validateRemoteAllowList hs)
          , local_list_validities =
            { cidrs_validites =
                Bool/and
                  (List/map types.Host Bool validateLocalCIDRAllowLists hs)
            , interfaces_validities =
                Bool/and
                  ( List/map
                      types.Host
                      Bool
                      validateLocalInterfacesAllowLists
                      hs
                  )
            }
          }
        }

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
        let rules_lists
            : List (List types.FirewallRule)
            = Map/values
                types.Host
                (List types.FirewallRule)
                (connection.getRules network)

        let rules = List/concat types.FirewallRule rules_lists

        let expected =
              { hosts_checks =
                { single_hosts_validity =
                  { sshd_validities = True
                  , ip_validities = True
                  , static_ips_validity = True
                  , interfaces_validities = True
                  , lighthouse_allow_lists_validities =
                    { remote_list_validities = True
                    , local_list_validities =
                      { cidrs_validites = True, interfaces_validities = True }
                    }
                  }
                , network_hosts_validity = True
                , lighthouse_present_check = True
                }
              , rules_checks.rules_check = True
              }

        let actual =
              { hosts_checks =
                { single_hosts_validity = validateSingleHosts network.hosts
                , network_hosts_validity = validateHostsAsNetwork network.hosts
                , lighthouse_present_check =
                    List/any types.Host isLighthouse network.hosts
                }
              , rules_checks.rules_check = validateRules rules
              }

        in  expected === actual

in  { validate }
