# wallets with funds.

import sys
import pyodbc

# main wallet function.
def wallet_main(args):
  assert len(args) > 0
  command = args[0]
  args = args[1:]
  (args, node_info, driver_info) = find_driver_node_info(args)
  if command == 'help':
    print ('usage: python funds-wallet.py COMMAND COMMAND-ARGS')
    exit(1)
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
