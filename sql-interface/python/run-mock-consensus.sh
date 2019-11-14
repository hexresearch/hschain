#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

walletDemoTableName=funds

create_populate_funds="-- funds available
CREATE TABLE $walletDemoTableName
  ( wallet_id STRING PRIMARY KEY
  , amount    INTEGER
  , CONSTRAINT no_overdraft CHECK (amount >= 0)
  );
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('u1', 1000);
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('u2', 1000);
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('u3', 1000);
INSERT INTO $walletDemoTableName
            (wallet_id, amount)
            VALUES ('u4', 1000);
"

request="\
UPDATE $walletDemoTableName \
SET amount = CASE \
WHEN wallet_id = :user_id      THEN amount - :transfer_amount \
WHEN address = :dest_user_id THEN amount + :transfer_amount \
END \
WHERE     (address = :user_id OR address = :dest_user_id) \
AND :transfer_amount > 0;"

add_transfer_request=$(cabal new-exec -- hschain-sql-utils add-request-code \
  --request "$request" --id "transfer" --positive transfer_amount \
  --string user_id --string dest_user_id
  )

tclsh8.6 mock-consensus-node.tcl 22222                            \
        "cabal new-exec -- hschain-sql-utils mandatory-system-tables --table \"$create_populate_funds\" --request \"$add_transfer_request\""
