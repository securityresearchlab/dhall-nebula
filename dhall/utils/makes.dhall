let types = ../types.dhall

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


let mkBidirectionalConnection
    : types.Port ->
      types.Proto ->
      types.ConnectionTarget ->
      types.ConnectionTarget ->
        types.Connection
    = \(port : types.Port) ->
      \(proto : types.Proto) ->
      \(c1 : types.ConnectionTarget) ->
      \(c2 : types.ConnectionTarget) ->
        [ { port, proto, from = c1, to = c2 }
        , { port, proto, from = c2, to = c1 }
        ]

let mkIntraGroupConnection
    : types.Port -> types.Proto -> types.Group -> types.Connection
    = \(port : types.Port) ->
      \(proto : types.Proto) ->
      \(g : types.Group) ->
        [ { port
          , proto
          , from = types.ConnectionTarget.Group g
          , to = types.ConnectionTarget.Group g
          }
        ]

let mkUnidirectionalConnection
    : types.Port ->
      types.Proto ->
      types.ConnectionTarget ->
      types.ConnectionTarget ->
        types.Connection
    = \(port : types.Port) ->
      \(proto : types.Proto) ->
      \(from : types.ConnectionTarget) ->
      \(to : types.ConnectionTarget) ->
        [ { port, proto, from, to } ]

in {
    mkPkiInfoWithBlocklist
    , mkPkiInfoWithoutBlocklist
    , mkBidirectionalConnection
    , mkUnidirectionalConnection
    , mkIntraGroupConnection
}
