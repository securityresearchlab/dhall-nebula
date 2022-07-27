FROM tool

RUN apt-get update && \
    apt-get install libnuma-dev ca-certificates bsdmainutils bc -y

WORKDIR /etc/nebula
ADD ./ca.crt /etc/nebula/
ADD ./ca.key /etc/nebula/

WORKDIR /home/app/dhall
ADD ./test.dhall /home/app/dhall
ADD ./client-config.dhall /home/app/dhall

WORKDIR /home/app
ADD ./run-client.sh /home/app/

# ENTRYPOINT [ "/bin/bash", "-c", "/home/app/run-client.sh" ]
