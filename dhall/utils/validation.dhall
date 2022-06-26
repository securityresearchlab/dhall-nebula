let types = ../types.dhall

let Bool/not = https://prelude.dhall-lang.org/v21.1.0/Bool/not

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

let List/map = https://prelude.dhall-lang.org/v21.1.0/List/map

let List/null = https://prelude.dhall-lang.org/v21.1.0/List/null

let isLighthouse
    : types.Host -> Bool
    = \(host : types.Host) ->
        merge
          { Some = \(config : types.IsLighthouseConfig) -> True, None = False }
          host.lighthouse_config

let validateHost
    : types.Host -> Bool
    = \(host : types.Host) ->
        let is_lighthouse = isLighthouse host

        let hosts_check = List/null Text host.lighthouse.hosts

        in  is_lighthouse && hosts_check || Bool/not is_lighthouse

let validateHosts
    : List types.Host -> Bool
    = \(hs : List types.Host) ->
        let hosts_validity = List/map types.Host Bool validateHost hs

        in  List/fold
              Bool
              hosts_validity
              Bool
              (\(b : Bool) -> \(a : Bool) -> b && a)
              True

let validate
    : types.Network -> Type
    = \(network : types.Network) ->
        let expected =
              { ciphers_check = True
              , hosts_check = True
              , lighthouse_present_check = True
              }

        let actual =
              { ciphers_check = True -- TODO:
              , hosts_check = validateHosts network.hosts
              , lighthouse_present_check = List/any types.Host isLighthouse network.hosts
              }

        in  expected === actual

in  { validate }
