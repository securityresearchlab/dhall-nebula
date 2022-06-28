let makes = ./makes.dhall

let types = ../types.dhall

let schemas = ../schemas.dhall

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

let List/concat = https://prelude.dhall-lang.org/v21.1.0/List/concat

let List/filter = https://prelude.dhall-lang.org/v21.1.0/List/filter

let List/map =
      https://prelude.dhall-lang.org/v21.1.0/List/map
        sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let Map/Entry = https://prelude.dhall-lang.org/v21.1.0/Map/Entry

let Natural/equal = https://prelude.dhall-lang.org/v21.1.0/Natural/equal

let showIPv4Network
    : types.IPv4Network -> Text
    = \(n : types.IPv4Network) ->
        "${Natural/show n._1}.${Natural/show
                                  n._2}.${Natural/show
                                            n._3}.${Natural/show
                                                      n._4}/${Natural/show
                                                                n.mask}"

let showIPv4
    : types.IPv4 -> Text
    = \(ip : types.IPv4) ->
        "${Natural/show ip._1}.${Natural/show ip._2}.${Natural/show
                                                         ip._3}.${Natural/show
                                                                    ip._4}"

let areIPv4Equal
    : types.IPv4 -> types.IPv4 -> Bool
    = \(ip1 : types.IPv4) ->
      \(ip2 : types.IPv4) ->
            Natural/equal ip1._1 ip2._1
        &&  Natural/equal ip1._2 ip2._2
        &&  Natural/equal ip1._3 ip2._3
        &&  Natural/equal ip1._4 ip2._4

let isHostInList
    : types.Host -> List types.Host -> Bool
    = \(host : types.Host) ->
      \(list : List types.Host) ->
        List/any
          types.Host
          (\(h : types.Host) -> areIPv4Equal h.ip host.ip)
          list

let isHostInGroup
    : types.Host -> types.Group -> Bool
    = \(host : types.Host) ->
      \(group : types.Group) ->
        isHostInList host group.hosts

let isIPInNetwork
    : types.IPv4 -> types.IPv4Network -> Bool
    = \(ip : types.IPv4) -> \(network : types.IPv4Network) -> True

let isTarget
    : types.Host -> types.ConnectionTarget -> Bool
    = \(host : types.Host) ->
      \(target : types.ConnectionTarget) ->
        merge
          { Group = \(g : types.Group) -> isHostInGroup host g
          , Host = \(h : types.Host) -> areIPv4Equal host.ip h.ip
          , CIDR =
              \(n : types.IPv4Network) ->
                isIPInNetwork { _1 = 1, _2 = 2, _3 = 3, _4 = 4 } n
          , AnyNebulaHost = True
          , AnyExternalHost = False
          }
          target

let generateOutboundRule
    : types.UnidirectionalConnection -> types.FirewallRule
    = \(connection : types.UnidirectionalConnection) ->
        let target =
              merge
                { Group = \(g : types.Group) -> types.TrafficTarget.Group g
                , Host = \(h : types.Host) -> types.TrafficTarget.Host h
                , CIDR = \(n : types.IPv4Network) -> types.TrafficTarget.CIDR n
                , AnyNebulaHost = types.TrafficTarget.AnyHost
                , AnyExternalHost = types.TrafficTarget.AnyHost
                }
                connection.from

        in  { port = connection.port
            , proto = connection.proto
            , traffic_target = target
            , direction = types.RuleDirection.Out
            , ca_name = None Text
            , ca_sha = None Text
            }

let generateInboundRule
    : types.UnidirectionalConnection -> types.FirewallRule
    = \(connection : types.UnidirectionalConnection) ->
        let target =
              merge
                { Group = \(g : types.Group) -> types.TrafficTarget.Group g
                , Host = \(h : types.Host) -> types.TrafficTarget.Host h
                , CIDR = \(n : types.IPv4Network) -> types.TrafficTarget.CIDR n
                , AnyNebulaHost = types.TrafficTarget.AnyHost
                , AnyExternalHost = types.TrafficTarget.AnyHost
                }
                connection.to

        in  { port = connection.port
            , proto = connection.proto
            , traffic_target = target
            , direction = types.RuleDirection.In
            , ca_name = None Text
            , ca_sha = None Text
            }

let generateRulesForUnidirectionalConnection
    : types.UnidirectionalConnection -> types.Host -> List types.FirewallRule
    = \(connection : types.UnidirectionalConnection) ->
      \(host : types.Host) ->
        let inbound_rule =
              if    isTarget host connection.to
              then  [ generateInboundRule connection ]
              else  [] : List types.FirewallRule

        let outbound_rule =
              if    isTarget host connection.from
              then  [ generateOutboundRule connection ]
              else  [] : List types.FirewallRule

        in  inbound_rule # outbound_rule

let generateRulesForConnection
    : types.Connection -> types.Host -> List types.FirewallRule
    = \(connection : types.Connection) ->
      \(host : types.Host) ->
        List/concat
          types.FirewallRule
          ( List/map
              types.UnidirectionalConnection
              (List types.FirewallRule)
              ( \(uc : types.UnidirectionalConnection) ->
                  generateRulesForUnidirectionalConnection uc host
              )
              connection
          )

let getHostRules
    : types.Network -> types.Host -> List types.FirewallRule
    = \(network : types.Network) ->
      \(host : types.Host) ->
        let rules_lists
            : List (List types.FirewallRule)
            =
              -- TODO: does the lighthouse need all the possible firewall rules?
              List/map
                types.Connection
                (List types.FirewallRule)
                (\(c : types.Connection) -> generateRulesForConnection c host)
                network.connections

        let connection_rules
            : List types.FirewallRule
            = List/concat types.FirewallRule rules_lists

        let ad_hoc_rules
            : List types.FirewallRule
            = List/map
                types.AdHocFirewallRule
                types.FirewallRule
                ( \(rule : types.AdHocFirewallRule) ->
                    rule.{ port
                         , proto
                         , traffic_target
                         , direction
                         , ca_name
                         , ca_sha
                         }
                )
                ( List/filter
                    types.AdHocFirewallRule
                    ( \(rule : types.AdHocFirewallRule) ->
                        areIPv4Equal rule.target.ip host.ip
                    )
                    network.ad_hoc_rules
                )

        let dns_rules
            : List types.FirewallRule
            = merge
                { Some =
                    \(c : types.IsLighthouseConfig) ->
                      merge
                        { Some =
                            \(d : types.DNSConfig) ->
                              [ { port = types.Port.Port d.dns_interface.port
                                , proto = types.Proto.any
                                , traffic_target = types.TrafficTarget.AnyHost
                                , direction = types.RuleDirection.In
                                , ca_name = None Text
                                , ca_sha = None Text
                                }
                              ]
                        , None = [] : List types.FirewallRule
                        }
                        c.dns
                , None = [] : List types.FirewallRule
                }
                host.lighthouse_config

        in  connection_rules # ad_hoc_rules # dns_rules

let getRules
    : types.Network -> Map types.Host (List types.FirewallRule)
    = \(network : types.Network) ->
        List/map
          types.Host
          (Map/Entry types.Host (List types.FirewallRule))
          ( \(h : types.Host) ->
              { mapKey = h, mapValue = getHostRules network h }
          )
          network.hosts

in  { getHostRules, getRules, showIPv4Network, showIPv4, areIPv4Equal }
