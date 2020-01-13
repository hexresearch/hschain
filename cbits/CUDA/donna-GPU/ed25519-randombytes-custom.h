/*
	a custom randombytes must implement:

	void ED25519_FN(ed25519_randombytes_unsafe) (void *p, size_t len);

	ed25519_randombytes_unsafe is used by the batch verification function
	to create random scalars
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

static uint64_t crnd_counter = 12345678;
void
ED25519_FN(ed25519_randombytes_unsafe) (void *data_, size_t len) {
        uint8_t* data = (uint8_t*) data_;
        while (len > 0) {
                uint64_t x = crnd_counter * 0x8088405;
                *data = x ^ (x>>8) ^ (x >> 32);
                crnd_counter++;
                data ++;
                len --;
        }
} /* ed25519_randombytes_unsafe */

