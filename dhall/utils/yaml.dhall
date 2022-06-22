let pure = ./pure.dhall

let types = ../types.dhall

let schemas = ../schemas.dhall

let List/map =
      https://prelude.dhall-lang.org/v21.1.0/List/map
        sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let List/filter = https://prelude.dhall-lang.org/v21.1.0/List/filter

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let Map/Entry = https://prelude.dhall-lang.org/v21.1.0/Map/Entry

let rule_map
    : types.FirewallRule -> types.Rule
    = \(rule : types.FirewallRule) ->
        let port
            : types.PortConfig
            = merge
                { Port = \(n : Natural) -> types.PortConfig.Port n
                , Any = types.PortConfig.Description "any"
                }
                rule.port

        let general_info = { port, proto = rule.proto }

        let result_rule
            : types.Rule
            = merge
                { AnyHost =
                    types.Rule.HostRule (general_info // { host = "any" })
                , Host =
                    \(t : types.Host) ->
                      types.Rule.HostRule (general_info // { host = t.ip })
                , Hosts =
                    \(l : List types.Host) ->
                      let hosts =
                            List/map
                              types.Host
                              Text
                              (\(h : types.Host) -> h.ip)
                              l

                      in  types.Rule.HostsRule (general_info // { hosts })
                , Group =
                    \(g : types.Group) ->
                      types.Rule.GroupRule (general_info // { group = g.name })
                , Groups =
                    \(l : List types.Group) ->
                      let groups =
                            List/map
                              types.Group
                              Text
                              (\(g : types.Group) -> g.name)
                              l

                      in  types.Rule.GroupsRule (general_info // { groups })
                }
                rule.applies_to

        in  result_rule

let generateHostConfig
    : types.Network -> types.Host -> types.HostConfig
    = \(network : types.Network) ->
      \(host : types.Host) ->
        let firewall_rules
            : List types.FirewallRule
            = pure.getHostRules network host

        let inbound_rules
            : List types.Rule
            = List/map
                types.FirewallRule
                types.Rule
                rule_map
                ( List/filter
                    types.FirewallRule
                    ( \(rule : types.FirewallRule) ->
                        merge { In = True, Out = False } rule.direction
                    )
                    firewall_rules
                )

        let outbound_rules
            : List types.Rule
            = List/map
                types.FirewallRule
                types.Rule
                rule_map
                ( List/filter
                    types.FirewallRule
                    ( \(rule : types.FirewallRule) ->
                        merge { In = False, Out = True } rule.direction
                    )
                    firewall_rules
                )

        let static_hosts
            : Map Text (List Text)
            = List/map
                types.Host
                (Map/Entry Text (List Text))
                ( \(h : types.Host) ->
                    { mapKey = h.ip, mapValue = h.static_ips }
                )
                network.hosts

        let lighthouse_config
            : types.LighthouseConfig
            = let lighthouse_base_config =
                    { am_lighthouse = True
                    , interval = host.lighthouse.interval
                    , hosts = [] : List Text
                    }

              in  merge
                    { Some =
                        \(c : types.IsLighthouseConfig) ->
                              lighthouse_base_config
                          //  merge
                                { Some =
                                    \(d : types.DNSConfig) ->
                                      { serve_dns = Some True
                                      , dns = Some d.dns_interface
                                      }
                                , None = {
                                    serve_dns = None Bool
                                    , dns = None types.InterfaceInfo
                                }
                                }
                                c.dns
                    , None =
                      { am_lighthouse = False
                      , interval = host.lighthouse.interval
                      , hosts = host.lighthouse.hosts
                      , serve_dns = None Bool
                      , dns = None types.InterfaceInfo
                      }
                    }
                    host.lighthouse_config

        in  { pki = host.pki
            , ip = host.ip
            , static_host_map = static_hosts
            , lighthouse = lighthouse_config
            , listen = host.listen_interface
            , punchy.punch = host.punchy
            , tun = {=}
            , logging = host.logging
            , firewall =
              { conntrack = schemas.FirewallConnectionConfig.default
              , outbound = outbound_rules
              , inbound = inbound_rules
              }
            }

in  { generateHostConfig }
