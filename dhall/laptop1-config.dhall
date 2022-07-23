let config = ./vm-laptop-azure.dhall

let nebula = ./package.dhall

in  nebula.configFromIP config.network (nebula.mkIPv4 192 168 100 2)
