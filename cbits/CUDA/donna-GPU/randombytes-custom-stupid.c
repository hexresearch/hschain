#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

static uint64_t counter = 12345678;
void
ED25519_FN(ed25519_randombytes_unsafe) (void *data_, size_t len) {
	uint8_t* data = (uint8_t*) data_;
	while (len > 0) {
printf("%s %d [%d]\n", __FILE__, __LINE__, len); fflush(stdout);
		uint64_t x = counter * 0x8088405;
		*data = x ^ (x>>8) ^ (x >> 32);
		counter++;
		data ++;
		len --;
	}
} /* ed25519_randombytes_unsafe */
