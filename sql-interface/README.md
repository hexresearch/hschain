SQL interface for blockchain
============================


Purpose
-------

SQL is known to many developers and can be conveniently and efficiently used
to perform various user-defined functions.

As blockchain can be seen as a some kind of key-value store (with specific
keys and values) it is only logical to have good query language for wallet
functionality, audits and much more. And SQL seem to be reasonable choice.

Also SQL is a programming-language agnostic and it is possible to have single
interface accessible from different programming languages.


Limitations
-----------

Most blockchains have limitations of some kind. Most database engines also have
ones. We are no exception here and below is the list of limitations we impose.

=== Availability restrictions

Blockchain is a realtime system. Blocks must be validated quickly enough so
that system can support high throughput required for contemporary problems.

Thus, transactions issued to blockchain must not:

  - require substantial amount of data read or written for completion.
    This means that SELECT statements executed in blockchain must not read
    much data - copying whole history of transactions is prohibited.
  - assume specific state of data outside of constraints imposed.
    For example, funds transfer must not have updates that assign specific
    amounts of money, i.e. "UPDATE funds (amount = 1234) WHERE user=4321".
    But assumption that expressed as database schema constraints (e.g.
    funds.amount is non-negative) is alright.
  - assume immediacy of execution.
    Blockchain is a distributed system and transactions may take some time to
    be included into the block. With the system with 100 verifiers and one
    second round time it may take up to 100 seconds (and in some cases even more)
    for transaction to be included in the block and executed by all verifiers.
  - return any results.

=== Blockchain and non-blockchain transactions

By default transactions are posted into blockchain. The system verifies that
transaction posted does respect limitations listed above.

But sometimes one need to perform more complex quieries and it is possible to
use locally-mirrored state of blockchain. This state can be a little stale
and may not include transactions from several lates blocks but it allows
to perform more complex quieries.

These quieries must start with the statement "PRAGMA LOCAL QUERY;" and may contain
anything base SQL engine allows.
