# wallets with funds.

import sys
import pyodbc

def _allowed_request_sql_code(id, sql, params):
    
def print_genesis():
    """Print out the genesis database schema: tables and their population with data"""

    genesis = """
CREATE TABLE wallets
    ( pubkey_id STRING
    , amount INTEGER
    , CONSTRAINT [unique wallet id] UNIQUE (pubkey_id)
    , CONSTRAINT [no overdraft] CHECK (amount >= 0)
    );

INSERT INTO wallets (pubkey_id, amount) VALUES ("root", 1000000);

-- these two tables below are special.

-- special table - allowed requests.
CREATE TABLE allowed_requests
    ( request_id STRING PRIMARY KEY -- we expect hash here, for now any string would do.
    , request_text STRING
    );

-- special table - parameters for requests.
CREATE TABLE allowed_requests_params
    ( request_id STRING
    , request_param_name STRING
    , request_param_type STRING
    , CONSTRAINT UNIQUE (request_id, request_param_name)
    , CONSTRAINT FOREIGN KEY (request_id) REFERENCES allowed_requests(request_id)
    , CONSTRAINT CHECK (request_param_type = "S" OR request_param_type = "I" OR request_param_type = "P")
    );

-- registering a wallet.
INSERT INTO allowed_requests (request_id, request_text) VALUES
 ("add_wallet", "INSERT INTO WALLETS (pubkey_id, amount) VALUES (:user_id, 0);");
INSERT INTO allowed_requests_params (request_id, request_param_name, request_param_type) VALUES
 ("add_wallet", "user_id", "S");

-- moving funds between wallets.
INSERT INTO allowed_requests (request_id, request_text) VALUES
 ("funds_transfer",
  "UPDATE wallet
    SET   amount = CASE
             WHEN address = :user_id      THEN amount - :transfer_amount
             WHEN address = :dest_user_id THEN amount - :transfer_amount
          END
    WHERE     (address = :user_id OR address = :dest_user_id)
          AND :transfer_amount > 0;");
INSERT INTO allowed_requests_params (request_id, request_param_name, request_param_type) VALUES
 ("funds_transfer", "user_id", "S");
INSERT INTO allowed_requests_params (request_id, request_param_name, request_param_type) VALUES
 ("funds_transfer", "dest_user_id", "S");
INSERT INTO allowed_requests_params (request_id, request_param_name, request_param_type) VALUES
 ("funds_transfer", "transfer_amount", "P");
"""

  print (genesis)

# main wallet function.
def wallet_main(args):
  assert len(args) > 0
  command = args[0]
  args = args[1:]
  (args, node_info, driver_info) = find_driver_node_info(args)
  if command == 'help':
    print ('usage: python funds-wallet.py COMMAND COMMAND-ARGS')
    exit(1)
  elif command == 'genesis':
    print_genesis()
  else:
    connection = pyodbc.connect("Driver="+driver_info+";LocalDatabase=mirror.db;Node="+node_info()+";")
    if   command == 'info':
      show_info(args, connection)
    elif command == 'funds':
      show_funds(args, connection)
    elif command == 'transfer':
      transfer(args, connection)
    elif command == 'operations':
      list_operations(args, connection)
    else:
      print ('invalid command, try python funds-wallet.py help')
      exit(1)
    connection.close()

if __name__ == 'main':
  args = sys.argv[1:]

  if len(args) < 1:
    args = ['help']

  wallet_main(args)
