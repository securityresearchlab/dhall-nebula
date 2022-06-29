let generics = ./generics.dhall

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

let isTarget
    : types.Host -> types.ConnectionTarget -> Bool
    = \(host : types.Host) ->
      \(target : types.ConnectionTarget) ->
        merge
          { Group = \(g : types.Group) -> generics.isHostInGroup host g
          , Host = \(h : types.Host) -> generics.areIPv4Equal host.ip h.ip
          , CIDR =
              \(n : types.IPv4Network) ->
                generics.isIPInNetwork { _1 = 1, _2 = 2, _3 = 3, _4 = 4 } n
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
            , ca_name = connection.ca_name
            , ca_sha = connection.ca_sha
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
            , ca_name = connection.ca_name
            , ca_sha = connection.ca_sha
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
                        generics.areIPv4Equal rule.target.ip host.ip
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

in  { getHostRules, getRules }