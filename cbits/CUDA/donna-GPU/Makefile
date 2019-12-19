all: test
	./test

test: test.c ed25519.h ed25519.c test-ticks.h ed25519-donna.h ed25519-donna-portable.h ed25519-donna-portable-identify.h curve25519-donna-helpers.h modm-donna-64bit.h ed25519-donna-basepoint-table.h ed25519-donna-64bit-tables.h
	gcc -o test -O3 -DED25519_NO_INLINE_ASM -DED25519_CUSTOMRANDOM -DED25519_REFHASH test.c ed25519.c randombytes-custom-stupid.c

