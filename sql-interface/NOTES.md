notes on driver etc
===================

Connection string handling
--------------------------

getdsnattr() - get attribute from connection string.

    static int
    getdsnattr(char *dsn, char *attr, char *out, int outLen)

You may try to find where it is called to find a place
where connection string is handled.

We add the following parameters:

 - "height_increment" - we require transactions to be included
   to blocks not higher than current_height + height_increment.
   This allows for some guarantee about deciding on the
   transaction non-executable state for client which is outside
   of the blockchain.
 - "publickey" - public key identifier, ASCII string, base58 encoded.
 - "privatekey" - private key, base58 encoded.
