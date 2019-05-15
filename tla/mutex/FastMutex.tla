/* Algorithm copied from PlusCal manual. It's here to serve as
/* an example and demonstration of build process

---- MODULE FastMutex ----
EXTENDS Naturals, TLC

CONSTANT N
(* --algorithm FastMutex {

variables
  x; y = 0; b = [i \in 1 .. N |-> FALSE ];

process (Proc \in 1 .. N)
variable
  j;
{
  ncs: while( TRUE ) {
    skip;
    start: b[self] := TRUE;
    l1:    x       := self;
    l2:    if( y /= 0 ) {
       l3: b[self] := FALSE;
       l4: await y = 0;
           goto start
    };
    l5:    y := self;
    l6:    if( x /= self ) {
      l7: b[self] := FALSE;
          j := 1;
      l8: while( j <= N ) {
        await ~b[j];
	j := j + 1
      };
      l9: if (y /= self) {
        l10: await y = 0;
	goto start
      };
    };
    cs: assert \A i \in 1..N : (i /= self) => (pc[i] /= "cs");
    l11: y := 0;
    l12: b[self] := FALSE;
  }
}

} *)

\* BEGIN TRANSLATION
\* END TRANSLATION

====
