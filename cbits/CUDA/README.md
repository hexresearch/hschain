Content and purpose
===================

These two directories contain source code of original Floydberry's donna ( efficient implementation of Ed25519) and our modified version for CUDA.

The original source code is in donna-orig directory and our code in donna-GPU.

The purpose of the experiment was to investigate feasibility and speed of GPU implementation of Ed25519 signature verification.


Results
=======

The donna-GPU directory contains Makefile which can be used to test speed of the CUDA version. It is as easy as running ``make`` command.

We are able to achieve speed of ~89000 CPU ticks per message on the 2.8GHz i7 and 1050 TI CUDA card. That translates to ~32us per message.

CUDA version is 9.1.


Main bottleneck
==============

GPU does not like divergent execution and there are plenty of them in the
