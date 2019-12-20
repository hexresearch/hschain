SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

all: test-cuda
	./test-cuda

HEADERS = curve25519-donna-64bit.h         ed25519-donna-portable-identify.h \
curve25519-donna-helpers.h       ed25519.h \
ed25519-donna-64bit-tables.h     ed25519-hash.h \
ed25519-donna-64bit-x86.h        ed25519-randombytes-custom.h \
ed25519-donna-basepoint-table.h  ed25519-randombytes.h \
ed25519-donna-batchverify.h      modm-donna-64bit.h \
ed25519-donna.h                  regression.h \
ed25519-donna-impl-base.h        test-ticks.h \
ed25519-donna-portable.h         ed-cuda.h


test: test.c ed25519.c $(HEADERS)
	gcc -o test -O3 -DED25519_NO_INLINE_ASM -DED25519_CUSTOMRANDOM -DED25519_REFHASH test.c ed25519.c

test-cuda: test-cuda.cu ed25519.cu $(HEADERS)
	nvcc -o test-cuda -O3 -DED25519_FORCE_NON_NATIVE_UINT128 -DED25519_NO_INLINE_ASM -DED25519_CUSTOMRANDOM -DED25519_REFHASH test-cuda.cu

