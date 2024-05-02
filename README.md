# Centralized Configuration Manager for the Nebula SDN solution

With this tool it is possible to define in a centralized configuration the whole topology of a Nebula network through the dhall programming language.

The project is composed of two main parts:

* Dhall package to define the configuration for a nebula network (nebula subdirectory)
* cli tool to generate the actual .yaml configurations (and optionally the certificates) for each node of the network (tool subdirectory).

The Dhall package defines the types for the main nebula configuration entities. With this typing information it is possible to type-check the whole configuration to avoid a large number of errors. It also defines many validation funtions to check the values defined in the configuration.

## Configuration example

The top level configuration entity is the Network object:

```
let network
    : nebula.Network
    = { hosts = []
      , groups = []
      , connections = []
      , blocklist = [] : List Text
      , cipher = nebula.Cipher.AES
      , ip_mask = 24
      }
```

It requires:

* The list of the **hosts** in the network
* The list of the **group** definitions that are used to partition the nodes according to their security profile
* The list of **connections** that represent the security policies enforced through the network.

The Dhall package provides all the necessary helper function to construct these entities as the following example:

```

let hostExample
    : nebula.host.type
    = nebula.host::{
      , name = "hostExample"
      , ip = nebula.mkipv4 192 168 100 3
      , pki = nebula.mkpkiinfo "/etc/nebula" ca "hostExample"
      , lighthouse = nebula.lighthouseinfo.default
      , punchy = nebula.punchyinfo::{ punch = true, respond = some true }
      , relays = []
      }


let groupExample
    : nebula.Group
    = { group_name = "groupExample", group_hosts = [ hostExample ] }


let connectionExample
    : nebula.Connection
    = nebula.mkIntraGroupConnection
        nebula.Port.AnyPort
        nebula.Proto.TCP
        groupExample
        (None Text)
        (None Text)
```

# CLI

The cli tool used to generate the final yaml configuration files from the Dhall specification is written in the Haskell programming language and it can be compiled and installed using the [stack](https://docs.haskellstack.org/en/stable/) build tool.

## Available operations for the cli tool

Generate configuration:
```
.\.stack-work\install\62d3440a\bin\tool-exe.exe --dhallDir "..\dhall" --configFileName "wsl-laptop-azure" config --configsPath "./nebula_configs"
```

Generate certificates and private key for each host of the configuration:
```
tool-exe.exe --dhallDir "..\dhall" --configFileName "wsl-laptop-azure" certificates --configsPath "./nebula_configs"  --caCrtPath "..\..\nebula-windows-amd64\ca.crt" --caKeyPath "..\..\nebula-windows-amd64\ca.key" --nebulaCertPath "..\..\nebula-windows-amd64\nebula-cert.exe"
```

Generate a pair of private and public key:
```
..\..\nebula-windows-amd64\nebula-cert.exe keygen --out-key .\nebula_configs\laptop1\laptop1.key --out-pub .\nebula_configs\laptop1\laptop1.kpub
```

Sign a specific public key:
```
.\.stack-work\install\62d3440a\bin\tool-exe.exe --dhallDir "..\dhall" --configFileName "wsl-laptop-azure" sign --caCrtPath "..\..\nebula-windows-amd64\ca.crt" --caKeyPath "..\..\nebula-windows-amd64\ca.key" --nebulaCertPath "..\..\nebula-windows-amd64\nebula-cert.exe" --keyPath ".\nebula_configs\laptop1\laptop1.kpub" --hostName "laptop1"
```

Sign public keys for all hosts:
```
.\.stack-work\install\62d3440a\bin\tool-exe.exe --dhallDir "..\dhall" --configFileName "wsl-laptop-azure" autosign --caCrtPath "..\..\nebula-windows-amd64\ca.crt" --caKeyPath "..\..\nebula-windows-amd64\ca.key" --nebulaCertPath "..\..\nebula-windows-amd64\nebula-cert.exe" --keysDir ".\nebula_configs" --keysExt ".kpub"
```

Verify cert:
```
.\.stack-work\install\62d3440a\bin\tool-exe.exe --dhallDir "..\dhall\" --configFileName "wsl-laptop-azure" verify --crtPath .\nebula_configs\laptop1\laptop1.crt --caCrtPath ..\..\nebula-windows-amd64\ca.crt --nebulaCertPath ..\..\nebula-windows-amd64\nebula-cert.exe
```
