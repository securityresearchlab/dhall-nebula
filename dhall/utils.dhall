let pure = ./utils/pure.dhall

let yaml = ./utils/yaml.dhall

let validation = ./utils/validation.dhall

let makes = ./utils/makes.dhall

in  pure // yaml // makes // validation
