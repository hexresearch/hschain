#!/bin/bash

# A demo script to create genesis for a some kind of DEX using SQL.
#
# !!! XXX PLEASE NOTE THAT THERE ARE PRIVATE KEYS EXPOSED HERE XXX !!!
# !!! XXX THIS IS DONE SO THAT USER WILL GET SAME RESULTS AND XXX !!!
# !!! XXX TO SLIGHTLY RELIEVE BURDEN OF KEY/SIGNATURES MANAGEMENT XXX !!!
#
# All questionable things here are marked with XXX somewhere in the name.
# Strict bash mode ensure that removal of them won't go unnoticed.


set -euo pipefail
IFS=$'\n\t '

export XXX_privkey_main="DDVdAh2uvFhxDSKkaccZqAgjvcZncKH773Ep7MAdiUrY" # XXX private key!
pubkey_main="Hp1jkgqzvpbKDFVPiewHM9167tdYznjq2q9Z3Wsg33xy"

XXX_privkey_val1="FZq3j1d7Xfb9aknrJArBaFi41pevtQE8ko6nPR9vXKXN" # XXX private key!
pubkey_val1="A9bm2Yv8tBq5HEyog7yabkKASFdxfw7tevrujFwF5Snv"

XXX_privkey_val2="9HnyZaEJx4ctvp8rebMuhXKBx5y5Hz2rLyHsYTPodVf4" # XXX private key!
pubkey_val2="2fpEJt7hf4jEG7cKCANiwkDToRFMv6srH88pKUySm6Bo"

XXX_privkey_val3="9YUUcKxk6eMADmH2JuRpXFmFpry7eQaWuEjjeGVuSc18" # XXX private key!
pubkey_val3="kUJgAbAwNyVRw21nwveTZgdk2daEUN1iUYaEjz5DhZo"

XXX_privkey_val4="BmGxrLVzU4xeRrjWp4ij3CgY5Kt2u2p12S46NwnUZq48" # XXX private key!
pubkey_val4="2saS5ng5xX7B9KQgsyqwwmkhgEnFSYfTaQ4YnigaHpoA"

walletDemoTableName=funds

create_populate_funds="-- funds available
CREATE TABLE $walletDemoTableName
  ( wallet_id TEXT PRIMARY KEY
  , amount    INTEGER
  , CONSTRAINT no_overdraft CHECK (amount >= 0)
  );
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('$pubkey_main', 1000000000000000);
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('$pubkey_val1', 1000000000000000);
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('$pubkey_val2', 1000000000000000);
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('$pubkey_val3', 1000000000000000);
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('$pubkey_val4', 1000000000000000);
"

request="\
UPDATE $walletDemoTableName \
SET amount = CASE \
WHEN wallet_id = :user_id      THEN amount - :transfer_amount \
WHEN wallet_id = :dest_user_id THEN amount + :transfer_amount \
END \
WHERE     (wallet_id = :user_id OR wallet_id = :dest_user_id) \
AND :transfer_amount > 0;"

add_transfer_request=$(cabal new-exec -- hschain-sql-utils add-request-code \
  --request "$request" --id "transfer" --positive transfer_amount \
  --string user_id --string dest_user_id
  )


cabal new-exec -- hschain-sql-utils mandatory-system-tables \
	--table "$create_populate_funds" \
	--request "$add_transfer_request" \
	--key-role "$pubkey_main:MAIN" \
	--key-role "$pubkey_val1:VALIDATOR" \
	--key-role "$pubkey_val2:VALIDATOR" \
	--key-role "$pubkey_val3:VALIDATOR" \
	--key-role "$pubkey_val4:VALIDATOR" \
	>qex-genesis-unsigned.txt

cabal new-exec -- hschain-sql-utils normalize <qex-genesis-unsigned.txt \
	| tee qex-genesis-unsigned-normalized.txt \
	| cabal new-exec -- hschain-sql-utils sign --secret-key-env-var XXX_privkey_main >qex-genesis.txt

echo ""
echo "qex-genesis.txt is written"
echo ""
echo "Also check qex-genesis-unsigned.txt and qex-genesis-unsigned-normalized.txt"

mkdir -p config

for i in 1 2 3 4; do
	pubkname=pubkey_val$i
	XXX_privkname=XXX_privkey_val$i
	config_fname="config/local$i.yaml"
	echo "nodeSeeds:"               >$config_fname
	echo "  - \"127.0.0.1:40001\"" >>$config_fname
	echo "  - \"127.0.0.1:40002\"" >>$config_fname
	echo "  - \"127.0.0.1:40003\"" >>$config_fname
	echo "  - \"127.0.0.1:40004\"" >>$config_fname
	echo "nodePort: 4000$i"        >>$config_fname
	echo "nodeAPIPort: 5000$i"     >>$config_fname
	echo "nspecPrivKey: \"${!XXX_privkname}\""     >>$config_fname
	echo "nspecLogFile:"                      >>$config_fname
	echo "  - type      : "ScribeJSON""       >>$config_fname
	echo "    path      : "logs/node-$i.log"" >>$config_fname
	echo "    severity  : "Debug""            >>$config_fname
	echo "    verbosity : "V2""               >>$config_fname
	echo "nspecDbName: "db/node-$i.db""       >>$config_fname
done

mkdir -p db

echo ""
echo "wrote configs:"
ls config/*
echo ""
echo "Validator keys are in the genesis generated above."
