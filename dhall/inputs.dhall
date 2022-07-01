let nebula = ./package.dhall

let lighthouse_ip
    : nebula.IPv4
    = nebula.mkIPv4 192 168 100 1

let config_dir
    : Text
    = "C:\\Users\\Giorgia\\Documents\\Universita\\Magistrale-Ingegneria_informatica\\Tesi\\nebula-windows-amd64"

let ca_name
    : Text
    = "ca"

in  { lighthouse_ip, config_dir, ca_name }
