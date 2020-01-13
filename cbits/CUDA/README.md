Content and purpose
===================

These two directories contain source code of original Floydberry's donna ( efficient implementation of Ed25519) and our modified version for CUDA.

The original source code is in donna-orig directory and our code in donna-GPU.

The purpose of the experiment was to investigate feasibility and speed of GPU implementation of Ed25519 signature verification.

Results
=======

The donna-GPU directory contains Makefile which can be used to test speed of the CUDA version. It is as easy as running ``make`` command.

We are able to achieve speed of ~89000 CPU ticks per message on the 2.8GHz i7 and 1050 TI CUDA card. That translates to ~32us per message. About two times faster than optimized CPU version.

CUDA version is 9.1.

CUDA 10 and 1080 card allowed for **12x speedup over unoptimzied** CPU implementation. It translates into **about five times speed up over optimized** CPU implementation.


Probably venue of investigation
-------------------------------

All ed25519 implementations use variable time code in a non-splittable way.

But there is a Curve25519 implementation that looks like it does not use variable time code: https://github.com/jedisct1/libhydrogen

It may be worth considering it for crypto for GPU.


Main bottleneck
==============

Divergent execution
-------------------

The main problem with GPU code is divergent execution. When GPU code encounters conditional execution, it splits current block execution in two parts, lets name them "thens" and "elses" and executes them sequentially (because there may be threads that execute "then" part and threads that execute "else" part).

This effectively makes things longer than expected.

There are two parts in code that have this problem.

SHA512
------

SHA512 employed here is written for clarity and not for speed. Also, it can sport adivergent execution if there are messages that span different number of blocks. For example, if there are 255 150 bytes messages and one 161 byte message, then these 255 threads will wait for single message to finish.

As SHA512 is computed at the very beginning of the verification, the divergence may continue into other parts of signing process.

I tried to mitigate it with ``__syncthreads()`` after SHA512 computation, but I haven't checkedactual gains from these.

The recommendations:

- GPU: either move into separate kernel or make sure divergent execution does not continue into verification code;
- general: optimize C code so that it does not use array (tried to no avail).

Contraction of polynomials
-------------------------

There is a function ``contract256_slidingwindow_modm`` which contracts polynomial represented as big integer (five 64-bit words - 4 * 56 bits and one with 32 bits) into an array of bit-per-byte values.

It is highly divergent because it has a for loop with condition with "else" part that has for loop with condition with one more for loop inside it.

All it does is bit manipulation. Basically, it extracts progressively bigger integer values (2 bits, 3 bits, etc) from expanded 256-bit value and sees whether it needs to increment or decrement this bigger 256 bit value.

The sums of bits values computed then used in the variable time scalar multiplication and are used as indices into some tables.

Recommendation:

- GPU: probably, fixed time scalar multiplicatin can be better here; I cannot assess changes right now.

