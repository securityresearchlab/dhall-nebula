let types = ../types.dhall

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

let List/map = https://prelude.dhall-lang.org/v21.1.0/List/map

let Map = https://prelude.dhall-lang.org/v21.1.0/Map/Type

let Map/Entry = https://prelude.dhall-lang.org/v21.1.0/Map/Entry

let Natural/equal = https://prelude.dhall-lang.org/v21.1.0/Natural/equal

let showIPv4Network
    : types.IPv4Network -> Text
    = \(n : types.IPv4Network) ->
        "${Natural/show n.in1}.${Natural/show
                                  n.in2}.${Natural/show
                                            n.in3}.${Natural/show
                                                      n.in4}/${Natural/show
                                                                n.mask}"

let showIPv4
    : types.IPv4 -> Text
    = \(ip : types.IPv4) ->
        "${Natural/show ip.i1}.${Natural/show ip.i2}.${Natural/show
                                                         ip.i3}.${Natural/show
                                                                    ip.i4}"

let showIPv4WithPort
    : types.IPv4WithPort -> Text
    = \(ip : types.IPv4WithPort) ->
        let show_ip
            : Text
            = showIPv4 { i1 = ip.ip1, i2 = ip.ip2, i3 = ip.ip3, i4 = ip.ip4 }

        in  "${show_ip}:${Natural/show ip.i_port}"

let areIPv4Equal
    : types.IPv4 -> types.IPv4 -> Bool
    = \(ip1 : types.IPv4) ->
      \(ip2 : types.IPv4) ->
            Natural/equal ip1.i1 ip2.i1
        &&  Natural/equal ip1.i2 ip2.i2
        &&  Natural/equal ip1.i3 ip2.i3
        &&  Natural/equal ip1.i4 ip2.i4

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
        isHostInList host group.group_hosts

let isIPInNetwork
    : types.IPv4 -> types.IPv4Network -> Bool
    = \(ip : types.IPv4) -> \(network : types.IPv4Network) -> True -- TODO

let fromIPv4NetworkBoolMapToMap : List types.IPv4NetworkBoolMapEntry -> Map types.IPv4Network Bool =
    \(entries : List types.IPv4NetworkBoolMapEntry) ->
        List/map
        types.IPv4NetworkBoolMapEntry
        (Map/Entry types.IPv4Network Bool)
        (\(e : types.IPv4NetworkBoolMapEntry) -> { mapKey = e.mapKeyIB, mapValue = e.mapValueIB })
        entries

let fromTextBoolMapToMap : List types.TextBoolMapEntry -> Map Text Bool =
    \(entries : List types.TextBoolMapEntry) ->
        List/map
        types.TextBoolMapEntry
        (Map/Entry Text Bool)
        (\(e : types.TextBoolMapEntry) -> { mapKey = e.mapKeyTB, mapValue = e.mapValueTB })
        entries

in  { showIPv4Network
    , showIPv4
    , showIPv4WithPort
    , areIPv4Equal
    , isHostInGroup
    , isIPInNetwork
    , fromIPv4NetworkBoolMapToMap
    , fromTextBoolMapToMap
    }
