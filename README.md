# Multihost Dhall configuration for Nebula

Generate configuration:
```
tool-exe.exe --dhallDir "../dhall" config --configsPath "./nebula_config"
```

Generate certificates and private key for each host of the configuration:
```
tool-exe.exe --dhallDir "..\dhall" certificates --configsPath "./nebula_config"  --caCrtPath "..\..\nebula-windows-amd64\ca.crt" --caKeyPath "..\..\nebula-windows-amd64\ca.key" --nebulaCertPath "..\..\nebula-windows-amd64\nebula-cert.exe"
```

Generate a pair of private and public key:
```
..\..\nebula-windows-amd64\nebula-cert.exe keygen --out-key .\nebula_config\laptop1\laptop1.key --out-pub .\nebula_config\laptop1\laptop1.kpub
```

Sign a specific public key:
```
.\.stack-work\install\62d3440a\bin\tool-exe.exe --dhallDir "..\dhall" sign --caCrtPath "..\..\nebula-windows-amd64\ca.crt" --caKeyPath "..\..\nebula-windows-amd64\ca.key" --nebulaCertPath "..\..\nebula-windows-amd64\nebula-cert.exe" --keyPath ".\nebula_config\laptop1\laptop1.kpub" --hostName "laptop1"
```

Sign public keys for all hosts:
```
.\.stack-work\install\62d3440a\bin\tool-exe.exe --dhallDir "..\dhall" autosign --caCrtPath "..\..\nebula-windows-amd64\ca.crt" --caKeyPath "..\..\nebula-windows-amd64\ca.key" --nebulaCertPath "..\..\nebula-windows-amd64\nebula-cert.exe" --keysDir ".\nebula_config" --keysExt ".kpub"

```
