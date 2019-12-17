# QEX client.

import os
import sys
import pyodbc

def exit_failure(msg):
  print (msg)
  exit(1)

def arg_value(args, arg_name, env_variable, default_value, conversion):
  """look for some named argument value in the command line arguments,
environment variables (if env_variable is not None) and then assigning the default value.

Report failure to obtain any value and exit."""
  result_args = list()
  arg_prefix = "--"+arg_name+"="
  value = None
  for x in args:
    if value == None and x.startswith(arg_prefix):
      value = conversion(x[len(arg_prefix):])
    else:
      result_args.append(x)
  if value == None:
    value = conversion(os.environ.get(env_variable, default_value)) if env_variable != None else default_value
  if value == None:
    exit_failure("unable to find value for "+arg_name+" in command line arguments and, possible, environment variables ("+(env_variable if env_variable != None else "None specified")+")")
  return (result_args, value)

def find_driver_node_info(args):
  """Look into command line arguments and envorinment variables for ODBC driver and consensus node(s) information"""
  (args, node_info) = arg_value(args, "node-info", "HSCHAIN_NODE_INFO", None, lambda x: x)
  (args, driver_info) = arg_value(args, "driver-info", "HSCHAIN_DRIVER_INFO", None, lambda x: x)
  (args, pub_key) = arg_value(args, "public-key", "HSCHAIN_PUBLIC_KEY", None, lambda x: x)
  return (args, node_info, pub_key, driver_info)

def hschain_q(sql):
  """append prefix that signals hschain query to driver"""
  return "PRAGMA HSCHAIN QUERY;"+sql

def show_info(pub_key, args, connection):
  """show info about current state"""
  cursor = connection.cursor()
  cursor.execute(hschain_q(""))
  cursor = connection.cursor()
  for x in cursor.execute("SELECT height FROM height;"):
    print("height: "+str(x[0]))
  for x in cursor.execute("SELECT amount, asset FROM funds WHERE wallet_id = :user_id;", [pub_key]):
    print("amount "+str(x[0])+" of "+str(x[1]))

def transfer(pub_key, args, connection):
  """trasfer funds from one wallet to another"""
  (args, amount) = arg_value(args, "amount", None, None, lambda x: int(x))
  (args, asset) = arg_value(args, "asset", None, None, lambda x: int(x))
  (args, dest) = arg_value(args, "to", None, None, lambda x: x)
  cursor = connection.cursor()
  cursor.execute(hschain_q("UPDATE funds SET amount = CASE WHEN wallet_id = :user_id      THEN amount - :transfer_amount WHEN wallet_id = :dest_user_id THEN amount + :transfer_amount END WHERE     (wallet_id = :user_id OR wallet_id = :dest_user_id) AND :transfer_amount > 0
AND :asset_name == asset_name;")
                , [pub_key, amount, dest, asset])


def operations(pub_key, args, connection):
  """show operations related to our public key"""
  cursor = connection.cursor()
  query = """SELECT op.height, op_dest.request_param_value, op_amount.request_param_value
  FROM serialized_requests AS op, serialized_requests_params as op_dest
     , serialized_requests_params as op_amount
     , serialized_requests_params as op_user
  WHERE
        op.request_id = 'transfer' AND op_dest.height = op.height
    AND op_amount.height = op.height AND op_dest.seq_index = op.seq_index
    AND op_amount.seq_index = op.seq_index
    AND op_user.height = op.height AND op_user.seq_index = op.seq_index
    AND op_user.request_param_value = :user_id
    AND op_user.request_param_name = 'user_id'
    AND op_amount.request_param_name = 'transfer_amount'
    AND op_dest.request_param_name = 'dest_user_id'
  ORDER BY op.height, op.seq_index
  """
  for x in cursor.execute(query, [pub_key]):
    print("height: "+str(x[0]))
    print("        transfer "+str(x[2])+" to "+str(x[1]))

def internal_validate_asset_name(connection, asset_name):
  cursor = connection.cursor()
  r = cursor.execute("SELECT COUNT(*) from allowed_assets WHERE asset_name = :asset_name;", [asset_name])
  for x in r:
    if x[0]<1:
      exit_failure("invalid asset name "+asset_name)
    return
  exit_failure("internal error in determining validity of asset "+asset_name);

def post_order(pub_key, args, connection):
  (args, sell_amount) = arg_value(args, "sell-amount", None, None, lambda x: int(x))
  if sell_amount <= 0:
    exit_failure("invalid sell amount: "+str(sell_amount))
  (args, sell_asset) = arg_value(args, "sell-asset", None, None, lambda x: x)
  internal_validate_asset_name(connection, sell_asset)
  (args, purchase_amount) = arg_value(args, "purchase-amount", None, None, lambda x: int(x))
  if purchase_amount <= 0:
    exit_failure("invalid purchase amount: "+str(purchase_amount));
  (args, purchase_asset) = arg_value(args, "purchase-asset", None, None, lambda x: x)
  internal_validate_asset_name(connection, purchase_asset)
  salt = datetime.now()..strftime("%d-%b-%Y (%H:%M:%S.%f)")+"/"+pub_key
  cursor = connection.cursor()
  cursor.execute(hschain_q("INSERT INTO orders ( height, seq_index, salt, wallet_id, sell_asset_name , sell_amount, purchase_asset_name, purchase_amount) SELECT current_height.height, seq_index = seq_index + 1, :salt, :user_id , :sell_asset, :sell_amount, :purchase_asset, :purchase_amount FROM height as current_height, allowed_assets, funds WHERE allowed_assets.asset_name = :sell_asset AND allowed_assets.asset_name = :purchase_asset AND :sell_amount > 0 AND :purchase_amount > 0;"), [salt, user_id, sell_asset, sell_amount, purchase_asset, purchase_amount])
  print ("order posted. order ID: "+salt)

# main wallet function.
def wallet_main(args):
  assert len(args) > 0
  command = args[0]
  args = args[1:]
  (args, node_info, pub_key, driver_info) = find_driver_node_info(args)
  if command == 'help':
    print ('usage: python funds-wallet.py COMMAND COMMAND-ARGS')
    exit(1)
  else:
    connection_string = ";".join(
      [ "Driver="+driver_info
      , "consensus_nodes="+node_info
      , "publickey="+pub_key
      , "Database=mirror.db"
      , "tracefile=trace.log"
      ])
    connection = pyodbc.connect(connection_string)
    print("connected")
    if   command == 'info':
      show_info(pub_key, args, connection)
    elif command == 'funds':
      show_funds(args, connection)
    elif command == 'transfer':
      transfer(pub_key, args, connection)
    elif command == 'post-order':
      transfer(pub_key, args, connection)
    elif command == 'remove-order':
      transfer(pub_key, args, connection)
    elif command == 'operations':
      operations(pub_key, args, connection)
    else:
      print ('invalid command, try python funds-wallet.py help')
      exit(1)
    connection.close()

if __name__ == '__main__':
  args = sys.argv[1:]

  if len(args) < 1:
    args = ['help']

  wallet_main(args)
