let types = ../types.dhall

let pure_utils = ./pure.dhall

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
                          (\(x : types.IPv4) -> pure_utils.areIPv4Equal n x)
                          ns
                      ).true
                  )
                  1

        let checks = List/map types.IPv4 Bool check_ip ns

        in  Bool/and checks

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
                (pure_utils.getRules network)

        let rules = List/concat types.FirewallRule rules_lists

        let actual =
              { hosts_check = validateHosts network.hosts
              , lighthouse_present_check =
                  List/any types.Host isLighthouse network.hosts
              , rules_check = validateRules rules
              }

        in  expected === actual

in  { validate }
