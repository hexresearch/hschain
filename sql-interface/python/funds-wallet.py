# wallets with funds.

import os
import sys
import pyodbc

def exit_failure(msg):
  print (msg)
  exit(1)

def arg_value(args, arg_name, env_variable, default_value):
  """look for some named argument value in the command line arguments,
environment variables (if env_variable is not None) and then assigning the default value.

Report failure to obtain any value and exit."""
  result_args = list()
  arg_prefix = "--"+arg_name+"="
  value = None
  for x in args:
    if value == None and x.startswith(arg_prefix):
      value = x[len(arg_prefix):]
    else:
      result_args.append(x)
  if value == None:
    value = os.environ.get(env_variable, default_value) if env_variable != None else default_value
  if value == None:
    exit_failure("unable to find value for "+arg_name+" in command line arguments and, possible, environment variables ("+(env_variable if env_variable != None else "None specified")+")")
  return (result_args, value)

def find_driver_node_info(args):
  """Look into command line arguments and envorinment variables for ODBC driver and consensus node(s) information"""
  (args, node_info) = arg_value(args, "node-info", "HSCHAIN_NODE_INFO", None)
  (args, driver_info) = arg_value(args, "driver-info", "HSCHAIN_DRIVER_INFO", None)
  (args, pub_key) = arg_value(args, "public-key", "HSCHAIN_PUBLIC_KEY", None)
  return (args, node_info, pub_key, driver_info)

def hschain_q(sql):
  """append prefix that signals hschain query to driver"""
  return "PRAGMA HSCHAIN QUERY;\n"+sql

def show_info(pub_key, args, connection):
  """show info about current state"""
  cursor = connection.cursor()
  cursor.execute(hschain_q(""))
  print("synchronization is done")
  cursor = connection.cursor()
  for x in cursor.execute("SELECT height FROM height;"):
    print("height: "+str(x[0]))
  for x in cursor.execute("SELECT amount FROM funds WHERE wallet_id = :user_id;", [pub_key]):
    print("amount: "+str(x[0]))

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
    print (connection_string)
    connection = pyodbc.connect(connection_string)
    print("connected")
    if   command == 'info':
      show_info(pub_key, args, connection)
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

if __name__ == '__main__':
  args = sys.argv[1:]

  if len(args) < 1:
    args = ['help']

  wallet_main(args)
