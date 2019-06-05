\* Implementation of toy gossip. It very simplified and inteneded to
\* serve as model to study how to work with toy models.
\*
\* Model is oversimplified:
\*  - Each node votes for "block"
\*  - Once it has votes from all nodes it can proceed
---- MODULE toygossip ----
EXTENDS Naturals, TLC, Sequences

CONSTANT N
\* ----------------------------------------------------------------
(* --algorithm toygossip {

variables
  queue = [i \in 1 .. N |-> << >>];

process (Proc \in 1 .. N)
variables
  \* Current height of process
  H     = 0;
  \* Votes at current height
  votes = {};
  \* Our knowledge about other peers
  peers = [ i \in 1 .. N |-> {} ];
{
  procstart:
  while( TRUE ) {
    \* We start with empty set of votes and vote themselves
    start:
      votes := {self};
      peers := [ i \in 1 .. N |-> {} ];
    \* Main loop
    loop:
\*    while( TRUE ) {
    while( votes /= 1 .. N ) {
      step:
      either {
 	\* Take message from queue and process it
	await (Len(queue[self]) > 0);
        if( Head(queue[self]).h = H ) {
          votes := votes \union { Head(queue[self]).id };
	  peers[Head(queue[self]).sender] := peers[Head(queue[self]).sender] \union { Head(queue[self]).id };
        };
	queue[self] := Tail(queue[self]);
      } or {
	\* Send message to random peer
	with( peer \in (1 .. N \ {self})) {
	  with( v \in (votes \ peers[peer]) ) {
            queue[peer] := Append(queue[peer], [h |-> H, id |-> v, sender |-> self]);
            peers[peer] := peers[peer] \union {v};
	  }
        }
      }
    };
    \* Next height
    nextH: H := H + 1;
  }
}

} *)

\* BEGIN TRANSLATION
\* END TRANSLATION

====
