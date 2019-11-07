notes on driver etc
===================

Connection string handling
--------------------------

getdsnattr() - get attribute from connection string.

    static int
    getdsnattr(char *dsn, char *attr, char *out, int outLen)

You may try to find where it is called to find a place
where connection string is handled.

As ODBC driver is built with driver manager (unixODBC), we should
use SQLGetPrivateProfileString function and appropriate branch in
`#ifdef ...WITHOUT_DRIVERMGR`.

We add the following parameters:

 - "height_increment" - we require transactions to be included
   to blocks not higher than current_height + height_increment.
   This allows for some guarantee about deciding on the
   transaction non-executable state for client which is outside
   of the blockchain.
 - "publickey" - public key identifier, ASCII string, base58 encoded.
 - "privatekey" - private key, base58 encoded.
 - "consensus_nodes" - an IPv4/v6 address of a comma-separated consensus
   node list (space separated pairs addr and port) to work with - post
   transaction and get updates.

