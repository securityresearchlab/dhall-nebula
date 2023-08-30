let network = ./test.dhall

let nebula = ./package.dhall

let Optional/null =
      https://prelude.dhall-lang.org/v21.1.0/Optional/null
        sha256:3871180b87ecaba8b53fffb2a8b52d3fce98098fab09a6f759358b9e8042eedc

let yaml = nebula.configFromIP network (nebula.mkIPv4 192 168 N_IP3 N_IP4)

let isNone = Optional/null nebula.HostConfig yaml

let _ = assert : nebula.validate network

let _ = assert : False === isNone

in  yaml
