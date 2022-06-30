let config = ./network-description.dhall

let nebula = ./package.dhall

in  nebula.generateHostConfig config.network config.laptop1
