#!/bin/python

# testing the intended operation.
# this is script

import sys
import pyodbc

import genesis

if __name__ != 'main':
    print (sys.args[0] + " is intended to be run as a script")
    exit(1)



connection = pyodbc.connect(
     ";".join([ "Driver=/home/sz/usr/lib/libsqlite3odbc-hschain.so"
              , "height_increment=10"
              , "publickey=user1pubkey"
              , "privatekey=user1privkey"
              , "Database=testio.db"
              ])
   )

cursor = connection.cursor()

cursor.close()

connection.close()
