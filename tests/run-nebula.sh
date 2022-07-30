echo client $1
BINS=`echo "obase=2;$1" | bc`
PADDED=`printf %16s $BINS | tr ' ' 0`
N_IP3=$((2#${PADDED:0:8}))
N_IP4=$((2#${PADDED:8:16}))
echo nebula ip _._.$N_IP3.$N_IP4

NAME=${N_IP3}${N_IP4}
# fix config file
sed -i "s/N_IP3/$N_IP3/g" /home/app/dhall/client-config.dhall
sed -i "s/N_IP4/$N_IP4/g" /home/app/dhall/client-config.dhall
# generate config
dhall-to-yaml-ng --omit-empty --file /home/app/dhall/client-config.dhall --output /etc/nebula/config.yaml
# generate keys
nebula-cert keygen --out-key /etc/nebula/client$NAME.key --out-pub /etc/nebula/client$NAME.pub
echo client$NAME
# sign key
nebula-cert sign \
     -ca-crt /etc/nebula/ca.crt \
     -ca-key /etc/nebula/ca.key \
     -in-pub /etc/nebula/client$NAME.pub \
     -ip "192.168.$N_IP3.$N_IP4/16" \
     -name client$NAME \
     -out-crt /etc/nebula/client$NAME.crt
# tool --dhallDir /home/app/dhall \
#      --configFileName test \
#      sign \
#      --caCrtPath /etc/nebula/ca.crt \
#      --caKeyPath /etc/nebula/ca.key \
#      --nebulaCertPath "nebula-cert" \
#      --keyPath /etc/nebula/client$NAME.pub --hostName client$NAME

echo "creating tun device"
mkdir -p /dev/net
mknod /dev/net/tun c 10 200
chmod 600 /dev/net/tun

touch /tmp/healthy

# run nebula
nebula -config /etc/nebula/config.yaml
