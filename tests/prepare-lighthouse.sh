curl -L https://github.com/slackhq/nebula/releases/download/v1.6.0/nebula-linux-amd64.tar.gz --output nebula.tar.gz
tar -xvf nebula.tar.gz
mkdir /etc/nebula
mv ./lighthouse.* /etc/nebula
./nebula -config /etc/nebula/lighthouse.yaml
