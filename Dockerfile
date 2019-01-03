FROM debian:9

COPY bin/thundermint-node /usr/local/bin/

CMD ["thundermint-node"]
