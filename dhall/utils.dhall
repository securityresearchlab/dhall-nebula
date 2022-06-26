let pure = ./utils/pure.dhall

let yaml = ./utils/yaml.dhall

let validation = ./utils/validation.dhall

in  pure // yaml // validation
