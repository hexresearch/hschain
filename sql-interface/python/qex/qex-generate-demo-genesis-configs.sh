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

XXX_privkey5="AHZpvuG4CrMASFzVivwdmnLYho3hVQX4CNuk4PNc5iF4" # XXX private key!
pubkey5="2RLRHpLEKtKcCYoHK1xDTEgWV6H5pG3zHmNaneJEHqto"

XXX_privkey6="78iz4djwWm9TH68HL4aZL21pZWRYKHEQmUXDJBJ3bWKP" # XXX private key!
pubkey6="7XC8T8EHjT5LYUabdrJ2iyZZrpc795gadcgnuqTTLGA"

walletDemoTableName=funds

create_populate_funds="-- funds available
CREATE TABLE allowed_assets
  ( asset_name  TEXT PRIMARY KEY NOT NULL
  );
INSERT INTO allowed_assets
  ( asset_name) VALUES ('bollar');
INSERT INTO allowed_assets
  ( asset_name) VALUES ('buble');

CREATE TABLE $walletDemoTableName
  ( wallet_id   TEXT NOT NULL PRIMARY KEY
  , amount      INTEGER NOT NULL
  , asset_name  TEXT NOT NULL
  , CONSTRAINT no_overdraft CHECK (amount >= 0)
  , CONSTRAINT only_allowed_assets
               FOREIGN KEY (asset_name)
	       REFERENCES allowed_assets(asset_name)
  );
INSERT INTO $walletDemoTableName
            (wallet_id, amount, asset_name)
            VALUES ('$pubkey_main', 1000000000000000, 'bollar');
INSERT INTO $walletDemoTableName
            (wallet_id, amount, asset_name)
            VALUES ('$pubkey_val1', 1000000000000000, 'bollar');
INSERT INTO $walletDemoTableName
            (wallet_id, amount, asset_name)
            VALUES ('$pubkey_val2', 1000000000000000, 'bollar');
INSERT INTO $walletDemoTableName
            (wallet_id, amount, asset_name)
            VALUES ('$pubkey_val3', 1000000000000000, 'bollar');
INSERT INTO $walletDemoTableName
            (wallet_id, amount, asset_name)
            VALUES ('$pubkey_val4', 1000000000000000, 'bollar');
INSERT INTO $walletDemoTableName
            (wallet_id, amount, asset_name)
            VALUES ('$pubkey5', 1000000000000000, 'buble');
INSERT INTO $walletDemoTableName
            (wallet_id, amount, asset_name)
            VALUES ('$pubkey6', 1000000000000000, 'buble');

CREATE TABLE sell_bids
  ( height        INTEGER NOT NULL
  , seq_index     INTEGER NOT NULL -- order of bids placed at same height.
  , wallet_id     TEXT NOT NULL
  , asset_name    TEXT NOT NULL -- selling this
  , amount        INTEGER NOT NULL
  , purchase_asset_name TEXT NOT NULL -- purchasing that
  , purchase_amount INTEGER NOT NULL
  , CONSTRAINT height_seq_index_ordering PRIMARY KEY (height, seq_index)
  , CONSTRAINT selling_valid_asset FOREIGN KEY (asset_name) REFERENCES allowed_assets (asset_name)
  , CONSTRAINT purchasing_valid_asset FOREIGN KEY (purchase_asset_name) REFERENCES allowed_assets (asset_name)
  , CONSTRAINT selling_valid_amount CHECK (amount > 0)
  , CONSTRAINT purchasing_valid_amount CHECK (purchase_amount > 0)
  );
"

transfer_request="\
UPDATE $walletDemoTableName \
SET amount = CASE \
WHEN wallet_id = :user_id      THEN amount - :transfer_amount \
WHEN wallet_id = :dest_user_id THEN amount + :transfer_amount \
END \
WHERE     (wallet_id = :user_id OR wallet_id = :dest_user_id) \
AND :transfer_amount > 0
AND :asset_name == asset_name;"

add_transfer_request=$(cabal new-exec -- hschain-sql-utils add-request-code \
  --request "$transfer_request" --id "transfer" --positive transfer_amount \
  --string user_id --string dest_user_id --string asset_name
  )


cabal new-exec -- hschain-sql-utils mandatory-system-tables \
	--table "$create_populate_funds" \
	--request "$add_transfer_request" \
	--key-role "$pubkey_main:MAIN" \
	--key-role "$pubkey_val1:VALIDATOR" \
	--key-role "$pubkey_val2:VALIDATOR" \
	--key-role "$pubkey_val3:VALIDATOR" \
	--key-role "$pubkey_val4:VALIDATOR" \
	--key-role "$pubkey_val1:VOTER" \
	--key-role "$pubkey_val2:VOTER" \
	--key-role "$pubkey_val3:VOTER" \
	--key-role "$pubkey_val4:VOTER" \
	--key-role "$pubkey5:VOTER" \
	--key-role "$pubkey6:VOTER" \
	>qex-genesis-unsigned.txt

cabal new-exec -- hschain-sql-utils normalize <qex-genesis-unsigned.txt \
	| tee qex-genesis-unsigned-normalized.txt \
	| cabal new-exec -- hschain-sql-utils sign --secret-key-env-var XXX_privkey_main >qex-genesis.txt

echo ""
echo "qex-genesis.txt is written"
echo ""
echo "Also check qex-genesis-unsigned.txt and qex-genesis-unsigned-normalized.txt"

