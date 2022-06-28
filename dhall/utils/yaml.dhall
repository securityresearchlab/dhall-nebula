let connection = ./connection.dhall

let generics = ./generics.dhall

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
                , Range =
                    \(r : types.PortRange) ->
                      types.PortConfig.Description
                        "${Natural/show r.from}-${Natural/show r.to}"
                }
                rule.port

        let general_info =
              { port
              , proto = rule.proto
              , ca_sha = None Text
              , ca_name = None Text
              }

        let result_rule
            : types.Rule
            = merge
                { AnyHost =
                    types.Rule.HostRule (general_info // { host = "any" })
                , Host =
                    \(t : types.Host) ->
                      types.Rule.HostRule
                        (general_info // { host = generics.showIPv4 t.ip })
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
                , CIDR =
                    \(cidr : types.IPv4Network) ->
                      types.Rule.CIDRRule
                        (     general_info
                          //  { cidr = generics.showIPv4Network cidr }
                        )
                }
                rule.traffic_target

        in  result_rule

let generateHostConfig
    : types.Network -> types.Host -> types.HostConfig
    = \(network : types.Network) ->
      \(host : types.Host) ->
        let firewall_rules
            : List types.FirewallRule
            = connection.getHostRules network host

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
            = let getStaticIPsAsText
                  : types.Host -> List Text
                  = \(h : types.Host) ->
                      List/map
                        types.IPv4WithPort
                        Text
                        generics.showIPv4WithPort
                        h.static_ips

              in  List/map
                    types.Host
                    (Map/Entry Text (List Text))
                    ( \(h : types.Host) ->
                        { mapKey = generics.showIPv4 h.ip
                        , mapValue = getStaticIPsAsText h
                        }
                    )
                    network.hosts

        let lighthouses_ips
            : List Text
            = List/map
                types.Host
                Text
                (\(h : types.Host) -> generics.showIPv4 h.ip)
                ( List/filter
                    types.Host
                    ( \(host : types.Host) ->
                        merge
                          { Some = \(l : types.IsLighthouseConfig) -> True
                          , None = False
                          }
                          host.lighthouse_config
                    )
                    network.hosts
                )

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
                                , None =
                                  { serve_dns = None Bool
                                  , dns = None types.InterfaceInfo
                                  }
                                }
                                c.dns
                    , None =
                      { am_lighthouse = False
                      , interval = host.lighthouse.interval
                      , hosts = lighthouses_ips
                      , serve_dns = None Bool
                      , dns = None types.InterfaceInfo
                      }
                    }
                    host.lighthouse_config

        let sshd_config
            : Optional types.SSHDConfig
            = merge
                { Some =
                    \(c : types.SSHDInfo) ->
                      Some
                        { enabled = True
                        , listen =
                            "${c.listen.host}:${Natural/show c.listen.port}"
                        , host_key = c.host_key
                        , authorized_users = c.authorized_users
                        }
                , None = None types.SSHDConfig
                }
                host.sshd

        in  { pki = host.pki
            , static_host_map = static_hosts
            , lighthouse = lighthouse_config
            , listen = host.listen_interface
            , punchy = host.punchy
            , tun = host.tun
            , logging = host.logging
            , firewall =
              { conntrack = schemas.FirewallConnectionConfig.default
              , outbound = outbound_rules
              , inbound = inbound_rules
              }
            , cipher = network.cipher
            , local_range = host.local_range
            , sshd = sshd_config
            }

in  { generateHostConfig }
