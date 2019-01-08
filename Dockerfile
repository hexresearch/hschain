FROM debian:9

COPY bin/thundermint-coin-node /usr/local/bin/

CMD ["thundermint-coin-node"]
