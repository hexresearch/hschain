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
variable
  \* Current height of process
  H     = 0;
  \* Votes at current height
  votes = {};
  \* Our knowledge about other peers  
  peers = [ i \in 1 .. N |-> FALSE ]; 
{
  procstart:
  while( TRUE ) {
    \* We start with empty set of votes
    start:
      votes := {self};
    \* Main loop
    loop:
\*    while( TRUE ) {      
    while( votes /= 1 .. N ) {
      either {
 	\* Take message from queue
        recv:	
	await (Len(queue[self]) > 0);
	  \* /\ (Head(queue[self])["h"] == H);
        print(queue);	  
        votes       := votes \union { Head(queue).id };
	queue[self] := Tail(queue[self]);
      } or {
 	\* Drop message from queue
        recvDrop:	
	  await Len(queue[self]) > 0;
  \* /\ Head(queue[self])["h"] /= H;
	queue[self] := Tail(queue[self]);
      } or {
	\* Send message to random peer
	send:
	with( peer \in (1 .. N \ {self}); v \in votes) {
          print(v);	  
	  queue[peer] := Append(queue[peer], [h |-> H, id |-> v]);
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

