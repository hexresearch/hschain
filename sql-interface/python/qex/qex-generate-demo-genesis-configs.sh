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
INSERT INTO allowed_assets
  ( asset_name) VALUES ('brank');

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
            VALUES ('$pubkey_main', 1000000000000000, 'brank');
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

-- sell one thing purchasing other.
-- for example, sell 107 bollars for 103 bubles.
CREATE TABLE orders
  ( height              INTEGER NOT NULL
  , seq_index           INTEGER NOT NULL -- order of bids placed at same height.
  , salt                TEXT NOT NULL -- identifier
  , wallet_id           TEXT NOT NULL
  , sell_asset_name     TEXT NOT NULL -- purchasing this
  , sell_amount         INTEGER NOT NULL
  , purchase_asset_name TEXT NOT NULL -- selling that
  , purchase_amount     INTEGER NOT NULL
  , CONSTRAINT salt_unique UNIQUE (salt)
  , CONSTRAINT purchasing_valid_asset FOREIGN KEY (purchase_asset_name) REFERENCES allowed_assets (asset_name)
  , CONSTRAINT selling_valid_asset FOREIGN KEY (sell_asset_name) REFERENCES allowed_assets (asset_name)
  , CONSTRAINT selling_valid_amount CHECK (sell_amount > 0)
  , CONSTRAINT purchasing_valid_amount CHECK (purchase_amount > 0)
  );
CREATE UNIQUE INDEX index_orders_by_height_seq ON orders (height, seq_index);
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

add_transfer_request_code=$(cabal new-exec -- hschain-sql-utils add-request-code \
  --request "$transfer_request" --id "transfer" --positive transfer_amount \
  --string user_id --string dest_user_id --string asset_name
  )

post_order_request="\
INSERT INTO orders \
  ( height, seq_index, salt, wallet_id, sell_asset_name \
  , sell_amount, purchase_asset_name, purchase_amount) \
  SELECT current_height.height, seq_index = seq_index + 1, :salt, :user_id \
       , :sell_asset, :sell_amount, :purchase_asset, :purchase_amount \
  FROM height as current_height, allowed_assets, $walletDemoTableName \
  WHERE \
        allowed_assets.asset_name = :sell_asset \
    AND allowed_assets.asset_name = :purchase_asset \
    AND :sell_amount > 0 \
    AND :purchase_amount > 0; \
"
add_post_order_request_code=$(cabal new-exec -- hschain-sql-utils add-request-code \
  --request "$post_order_request" --id "post_order" --positive sell_amount \
  --positive purchase_amount --string salt \
  --string user_id --string sell_asset_name --string purchase_asset_name
  )

remove_order_request="\
DELETE FROM orders \
  WHERE \
        wallet_id = :user_id \
    AND salt = :salt; \
"
add_remove_order_request_code=$(cabal new-exec -- hschain-sql-utils add-request-code \
  --request "$remove_order_request" --id "remove_order" \
  --string user_id --string salt
  )


cabal new-exec -- hschain-sql-utils mandatory-system-tables \
	--table "$create_populate_funds" \
	--request "$add_transfer_request_code" \
	--request "$add_post_order_request_code" \
	--request "$add_remove_order_request_code" \
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

