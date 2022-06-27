let types = ../types.dhall

let pure_utils = ./pure.dhall

let Bool/not = https://prelude.dhall-lang.org/v21.1.0/Bool/not

let List/all = https://prelude.dhall-lang.org/v21.1.0/List/all

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

let List/filter = https://prelude.dhall-lang.org/v21.1.0/List/filter

let List/map = https://prelude.dhall-lang.org/v21.1.0/List/map

let List/null = https://prelude.dhall-lang.org/v21.1.0/List/null

let List/partition = https://prelude.dhall-lang.org/v21.1.0/List/partition

let Map/values = https://prelude.dhall-lang.org/v21.1.0/Map/values

let Natural/equal = https://prelude.dhall-lang.org/v21.1.0/Natural/equal

let Natural/lessThan = https://prelude.dhall-lang.org/v21.1.0/Natural/lessThan

let isLighthouse
    : types.Host -> Bool
    = \(host : types.Host) ->
        merge
          { Some = \(config : types.IsLighthouseConfig) -> True, None = False }
          host.lighthouse_config

let areIdsUnique
    : List Natural -> Bool
    = \(ns : List Natural) ->
        let check_id
            : Natural -> Bool
            = \(n : Natural) ->
                Natural/equal
                  ( List/length
                      Natural
                      ( List/partition
                          Natural
                          (\(x : Natural) -> Natural/equal n x)
                          ns
                      ).true
                  )
                  1

        let checks = List/map Natural Bool check_id ns

        in  List/fold
              Bool
              checks
              Bool
              (\(b : Bool) -> \(a : Bool) -> b && a)
              True

let validateHost
    : types.Host -> Bool
    = \(host : types.Host) ->
        merge
          { Some =
              \(c : types.SSHDInfo) -> Bool/not (Natural/equal c.listen.port 22)
          , None = True
          }
          host.sshd

let validateHosts
    : List types.Host -> Bool
    = \(hs : List types.Host) ->
        let single_hosts_validity =
              List/fold
                Bool
                (List/map types.Host Bool validateHost hs)
                Bool
                (\(b : Bool) -> \(a : Bool) -> b && a)
                True

        let hosts_validity =
              areIdsUnique
                (List/map types.Host Natural (\(h : types.Host) -> h.id) hs)

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

        let rules_lists : List (List types.FirewallRule) = Map/values types.Host (List types.FirewallRule) (pure_utils.getRules network)

        let rules =
              List/fold
                (List types.FirewallRule)
                rules_lists
                (List types.FirewallRule)
                ( \(l : List types.FirewallRule) ->
                  \(a : List types.FirewallRule) ->
                    l # a
                )
                ([] : List types.FirewallRule)

        let actual =
              { hosts_check = validateHosts network.hosts
              , lighthouse_present_check =
                  List/any types.Host isLighthouse network.hosts
              , rules_check = validateRules rules
              }

        in  expected === actual

in  { validate }
