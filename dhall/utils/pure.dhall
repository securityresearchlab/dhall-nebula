let types = ../types.dhall

let schemas = ../schemas.dhall

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

let List/filter = https://prelude.dhall-lang.org/v21.1.0/List/filter

let List/map =
      https://prelude.dhall-lang.org/v21.1.0/List/map
        sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let Map/Entry = https://prelude.dhall-lang.org/v21.1.0/Map/Entry

let Natural/equal = https://prelude.dhall-lang.org/v21.1.0/Natural/equal

let mkPkiInfoWithBlocklist
    : types.Directory ->
      types.CAName ->
      types.HostName ->
      List Text ->
        types.PkiInfo
    = \(dir : types.Directory) ->
      \(ca : types.CAName) ->
      \(name : types.HostName) ->
      \(blocklist : List Text) ->
        { ca = dir ++ "/" ++ ca ++ ".crt"
        , cert = dir ++ "/" ++ name ++ ".crt"
        , key = dir ++ "/" ++ name ++ ".key"
        , blocklist = Some blocklist
        }

let mkPkiInfoWithoutBlocklist
    : types.Directory -> types.CAName -> types.HostName -> types.PkiInfo
    = \(dir : types.Directory) ->
      \(ca : types.CAName) ->
      \(name : types.HostName) ->
        { ca = dir ++ "/" ++ ca ++ ".crt"
        , cert = dir ++ "/" ++ name ++ ".crt"
        , key = dir ++ "/" ++ name ++ ".key"
        , blocklist = None (List Text)
        }

let isHostInList
    : types.Host -> List types.Host -> Bool
    = \(host : types.Host) ->
      \(list : List types.Host) ->
        List/any
          types.Host
          (\(h : types.Host) -> Natural/equal host.id h.id)
          list

let isHostInGroup
    : types.Host -> types.Group -> Bool
    = \(host : types.Host) ->
      \(group : types.Group) ->
        isHostInList host group.hosts

let isTarget
    : types.Host -> types.ConnectionTarget -> Bool
    = \(host : types.Host) ->
      \(target : types.ConnectionTarget) ->
        merge
          { Group = \(g : types.Group) -> isHostInGroup host g
          , Host = \(h : types.Host) -> Natural/equal host.id h.id
          }
          target

let getTrafficTarget
    : types.ConnectionType -> types.RuleDirection -> types.TrafficTarget
    = \(type : types.ConnectionType) ->
      \(dir : types.RuleDirection) ->
        merge
          { GroupConnection = \(g : types.Group) -> types.TrafficTarget.Group g
          , UnidirectionalConnection =
              \(c : types.UnidirectionalConnection) ->
                merge
                  { Group = \(g : types.Group) -> types.TrafficTarget.Group g
                  , Host = \(h : types.Host) -> types.TrafficTarget.Host h
                  }
                  (merge { In = c.from, Out = c.to } dir)
          }
          type

let generateRulesForConnection
    : types.Connection -> types.Host -> List types.FirewallRule
    = \(connection : types.Connection) ->
      \(host : types.Host) ->
        merge
          { GroupConnection =
              \(group : types.Group) ->
                if    isHostInGroup host group
                then  [ { port = connection.port
                        , proto = connection.proto
                        , traffic_target = types.TrafficTarget.Group group
                        , direction = types.RuleDirection.In
                        , ca_name = None Text
                        , ca_sha = None Text
                        }
                      , { port = connection.port
                        , proto = connection.proto
                        , traffic_target = types.TrafficTarget.Group group
                        , direction = types.RuleDirection.Out
                        , ca_name = None Text
                        , ca_sha = None Text
                        }
                      ]
                else  [] : List types.FirewallRule
          , UnidirectionalConnection =
              -- TODO: there is the possibility of an host being both the from target and the to target, add a case for this
              \(c : types.UnidirectionalConnection) ->
                if    isTarget host c.from
                then  [ { port = connection.port
                        , proto = connection.proto
                        , traffic_target =
                            getTrafficTarget
                              connection.type
                              types.RuleDirection.Out
                        , direction = types.RuleDirection.Out
                        , ca_name = None Text
                        , ca_sha = None Text
                        }
                      ]
                else  if isTarget host c.to
                then  [ { port = connection.port
                        , proto = connection.proto
                        , traffic_target =
                            getTrafficTarget
                              connection.type
                              types.RuleDirection.In
                        , direction = types.RuleDirection.In
                        , ca_name = None Text
                        , ca_sha = None Text
                        }
                      ]
                else  [] : List types.FirewallRule
          }
          connection.type

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

        let connection_rules =
              List/fold
                (List types.FirewallRule)
                rules_lists
                (List types.FirewallRule)
                ( \(l : List types.FirewallRule) ->
                  \(a : List types.FirewallRule) ->
                    l # a
                )
                ([] : List types.FirewallRule)

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
                        isHostInList host rule.targets
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

in  { getHostRules
    , getRules
    , mkPkiInfoWithBlocklist
    , mkPkiInfoWithoutBlocklist
    }
