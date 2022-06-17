-- the :: may be useful
-- imports
let List/map =
      https://prelude.dhall-lang.org/v21.1.0/List/map
        sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

-- types
let ApplyTarget =
      < Host : Text | Hosts : List Text | Group : Text | Groups : List Text >

let FirewallRule : Type = { port : Text, proto : Text, applies_to : ApplyTarget }

let ExtendedFirewallRule =
      { Type =
          { port : Text
          , proto : Text
          , host : Optional Text
          , hosts : Optional (List Text)
          , group : Optional Text
          , groups : Optional (List Text)
          }
      , default =
        { port = "any"
        , proto = "any"
        , host = Some "any"
        , hosts = None (List Text)
        , group = None Text
        , groups = None (List Text)
        }
      }

-- merge handlers
let apply_target_handler =
      { Host =
          \(h : Text) ->
            { host = Some h
            , hosts = None (List Text)
            , group = None Text
            , groups = None (List Text)
            }
      , Hosts =
          \(hs : List Text) ->
            { host = None Text
            , hosts = Some hs
            , group = None Text
            , groups = None (List Text)
            }
      , Group =
          \(g : Text) ->
            { host = None Text
            , hosts = None (List Text)
            , group = Some g
            , groups = None (List Text)
            }
      , Groups =
          \(gs : List Text) ->
            { host = None Text
            , hosts = None (List Text)
            , group = None Text
            , groups = Some gs
            }
      }

let firewall_rule_extender =
      \(rule : FirewallRule) ->
            ({ port = rule.port, proto = rule.proto }
        //  merge apply_target_handler rule.applies_to)

let firewall_rule_mapper = List/map FirewallRule ExtendedFirewallRule.Type firewall_rule_extender

let outbound_rules
    : List ExtendedFirewallRule.Type
    = firewall_rule_mapper
        [ { port = "any", proto = "any", applies_to = ApplyTarget.Host "any" } ]

let inbound_rules
    : List ExtendedFirewallRule.Type
    = firewall_rule_mapper
        [ { port = "any", proto = "ICMP", applies_to = ApplyTarget.Host "any" }
        , { port = "any"
          , proto = "TCP"
          , applies_to = ApplyTarget.Groups [ "home" ]
          }
        ]

let lighthouse =
      { lighthouse = True
      , nebula_ip = "192.168.100.1"
      , outbound = outbound_rules
      , inbound = inbound_rules
      }

let laptop1 =
      { lighthouse = False
      , nebula_ip = "192.168.100.5"
      , outbound = outbound_rules
      , inbound = inbound_rules
      }

let laptop2 =
      { lighthouse = False
      , nebula_ip = "192.168.100.9"
      , outbound = outbound_rules
      , inbound = inbound_rules
      }

let hosts = [ lighthouse ]

in  { hosts }
