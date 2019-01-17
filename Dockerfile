FROM debian:9

RUN apt install --yes libgmp10 ca-certificates

COPY bin/thundermint-coin-node /usr/local/bin/

CMD ["thundermint-coin-node"]
