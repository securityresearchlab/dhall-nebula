# Multihost Dhall configuration for Nebula

Generate configuration:
```
tool-exe.exe --dhallDir "..\dhall" --configFileName "wsl-laptop-azure" config --configsPath "./nebula_configs"
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
tool-exe.exe --dhallDir "..\dhall\" verify --crtPath .\nebula_configs\laptop1\laptop1.crt --caCrtPath ..\..\nebula-windows-amd64\ca.crt --nebulaCertPath ..\..\nebula-windows-amd64\nebula-cert.exe
```
