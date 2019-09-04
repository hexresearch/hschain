FROM debian:9

RUN  apt update \
     && apt install --yes libgmp10 ca-certificates netbase \
     && rm -rf /var/lib/apt/lists

COPY bin/hschain-coin-node /usr/local/bin/

ENTRYPOINT ["hschain-coin-node"]
