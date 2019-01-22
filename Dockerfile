FROM debian:9

RUN  apt update \
     && apt install --yes libgmp10 ca-certificates netbase \
     && rm -rf /var/lib/apt/lists

COPY bin/thundermint-coin-node /usr/local/bin/

ENTRYPOINT ["thundermint-coin-node"]
