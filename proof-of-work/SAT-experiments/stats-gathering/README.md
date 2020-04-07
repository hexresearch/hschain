Proof-of-work with random kSAT
============================

We can use random kSAT near phase transition point as a proof of work. The steps are:

1. generate random kSAT problem from combined data (including hash of some data, actual nonce, timestamp, etc) using current hardness parameters (number of variables N, clause count C - see below about transition point)
2. search and obtain either satisfying assignment which is discarded or unsatisfiability proof trace
3. use "shortest" (to be defined during research) proof as a PoW evidence.

We use proofs of unsatisfiability as proof of work because to check unsatisfiability one must be thorough (search complete assignment space somehow), not lucky (one must find satisfiability assignmentby sheer luck).

SAT solution is a memory intensive task (each clause is kept on 2-literal-watch list) and also branch-heavy - there are loops, recursive calls, memory management, proof store and pruning, etc.

It looks like it is [quantum-proof](https://uwspace.uwaterloo.ca/bitstream/handle/10012/8330/Arunachalam_Srinivasan_2014.pdf?sequence=8&isAllowed=y) - this thesis reports at most quadratic speedups for hard SAT instances (3SAT, to be specific). Most of hard SAT instances are already of polynomial size of N - O(N^2) for pigeonhole problems, O(N^2) for factoring problems, etc. This effectively means that quantum computer will not help here much.

Also note that compression techniques (based on ROBDD and/or ZDD - Zres, Cassatt as examples) do not apply well to random kSAT - there are no speedups and no compression gains. Moreover, these solvers do not produce resolution proofs.


kSAT introduction
----------------

SAT is a satisfiability problem, usually encoded in Conjunctive Normal Form (CNF) - as a conjunction of disjuncts (clauses).

kSAT is a satisfiability problem encoded in CNF with all disjuncts having length of k literals (literal is either variable or negation of variable, there must not be a variable and its negation in any of disjuncts).

Randomly generated kSAT instance have interesting property of phase transition: the bigger ratio A=C/N (C - number of disjuncts, N - number of variables), the less is the probability of such problem being satisfiable. For 3SAT probability of 0.5 is reached at A=4.17. The effort to solve the kSAT problem with modern solvers grows with a peak at probability of about 0.65.

References for further reading:
  - [Paper about phase transition in random kSAT](http://guava.physics.uiuc.edu/~nigel/courses/563/Essays_2017/PDF/chertkov.pdf)
  - [Thresholds for different k](https://pdfs.semanticscholar.org/dc70/906533144e71025f4d050bd8520d362301ac.pdf)

Effort to solve is also proportional to 2^(O(N)), current estimates are at about 1.3^N ([Wikipedia](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem#Algorithms_for_solving_SAT)).

