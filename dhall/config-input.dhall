let nebula = ./package.dhall

let lighthouse_ip
    : nebula.IPv4
    = nebula.mkIPv4 192 168 100 1

let lighthouse_dir
    : Text
    = "/etc/nebula"

in  { lighthouse_ip, lighthouse_dir }
