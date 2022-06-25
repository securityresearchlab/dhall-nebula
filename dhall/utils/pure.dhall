let types = ../types.dhall

let schemas = ../schemas.dhall

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

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

let isHostInGroup
    : types.Host -> types.Group -> Bool
    = \(host : types.Host) ->
      \(group : types.Group) ->
        List/any
          types.Host
          (\(h : types.Host) -> Natural/equal host.id h.id)
          group.hosts

let isTarget
    : types.Host -> types.ConnectionTarget -> Bool
    = \(host : types.Host) ->
      \(target : types.ConnectionTarget) ->
        merge
          { Group = \(g : types.Group) -> isHostInGroup host g
          , Host = \(h : types.Host) -> Natural/equal host.id h.id
          }
          target

let getApplyTarget
    : types.ConnectionType -> types.RuleDirection -> types.ApplyTarget
    = \(type : types.ConnectionType) ->
      \(dir : types.RuleDirection) ->
        merge
          { GroupConnection = \(g : types.Group) -> types.ApplyTarget.Group g
          , UnidirectionalConnection =
              \(c : types.UnidirectionalConnection) ->
                merge
                  { Group = \(g : types.Group) -> types.ApplyTarget.Group g
                  , Host = \(h : types.Host) -> types.ApplyTarget.Host h
                  }
                  (merge { In = c.from, Out = c.to } dir)
          , AllConnection = types.ApplyTarget.AnyHost
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
                        , applies_to = types.ApplyTarget.Group group
                        , direction = types.RuleDirection.In
                        }
                      , { port = connection.port
                        , proto = connection.proto
                        , applies_to = types.ApplyTarget.Group group
                        , direction = types.RuleDirection.Out
                        }
                      ]
                else  [] : List types.FirewallRule
          , UnidirectionalConnection =
              -- TODO: there is the possibility of an host being both the from target and the to target, add a case for this
              \(c : types.UnidirectionalConnection) ->
                if    isTarget host c.from
                then  [ { port = connection.port
                        , proto = connection.proto
                        , applies_to =
                            getApplyTarget
                              connection.type
                              types.RuleDirection.Out
                        , direction = types.RuleDirection.Out
                        }
                      ]
                else  if isTarget host c.to
                then  [ { port = connection.port
                        , proto = connection.proto
                        , applies_to =
                            getApplyTarget
                              connection.type
                              types.RuleDirection.In
                        , direction = types.RuleDirection.In
                        }
                      ]
                else  [] : List types.FirewallRule
          , AllConnection =
            [ { port = connection.port
              , proto = connection.proto
              , applies_to =
                  getApplyTarget connection.type types.RuleDirection.In
              , direction = types.RuleDirection.In
              }
            , { port = connection.port
              , proto = connection.proto
              , applies_to =
                  getApplyTarget connection.type types.RuleDirection.Out
              , direction = types.RuleDirection.Out
              }
            ]
          }
          connection.type

let getHostRules
    : types.Network -> types.Host -> List types.FirewallRule
    = \(network : types.Network) ->
      \(host : types.Host) ->
        let rules
            : List (List types.FirewallRule)
            =
              -- TODO: does the lighthouse need all the possible firewall rules?
              List/map
                types.Connection
                (List types.FirewallRule)
                (\(c : types.Connection) -> generateRulesForConnection c host)
                network.connections

        in  List/fold
              (List types.FirewallRule)
              rules
              (List types.FirewallRule)
              ( \(l : List types.FirewallRule) ->
                \(a : List types.FirewallRule) ->
                  l # a
              )
              ([] : List types.FirewallRule)

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
