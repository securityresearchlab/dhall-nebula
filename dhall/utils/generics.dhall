let types = ../types.dhall

let List/any = https://prelude.dhall-lang.org/v21.1.0/List/any

let Natural/equal = https://prelude.dhall-lang.org/v21.1.0/Natural/equal

let showIPv4Network
    : types.IPv4Network -> Text
    = \(n : types.IPv4Network) ->
        "${Natural/show n._1}.${Natural/show
                                  n._2}.${Natural/show
                                            n._3}.${Natural/show
                                                      n._4}/${Natural/show
                                                                n.mask}"

let showIPv4
    : types.IPv4 -> Text
    = \(ip : types.IPv4) ->
        "${Natural/show ip._1}.${Natural/show ip._2}.${Natural/show
                                                         ip._3}.${Natural/show
                                                                    ip._4}"

let showIPv4WithPort
    : types.IPv4WithPort -> Text
    = \(ip : types.IPv4WithPort) ->
        let show_ip
            : Text
            = showIPv4 ip.{ _1, _2, _3, _4 }

        in  "${show_ip}:${Natural/show ip.port}"

let areIPv4Equal
    : types.IPv4 -> types.IPv4 -> Bool
    = \(ip1 : types.IPv4) ->
      \(ip2 : types.IPv4) ->
            Natural/equal ip1._1 ip2._1
        &&  Natural/equal ip1._2 ip2._2
        &&  Natural/equal ip1._3 ip2._3
        &&  Natural/equal ip1._4 ip2._4

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
        isHostInList host group.hosts

let isIPInNetwork
    : types.IPv4 -> types.IPv4Network -> Bool
    = \(ip : types.IPv4) -> \(network : types.IPv4Network) -> True

in  { showIPv4Network
    , showIPv4
    , showIPv4WithPort
    , areIPv4Equal
    , isHostInGroup
    , isIPInNetwork
    }
