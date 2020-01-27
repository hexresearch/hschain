/* test for EVPOW algorithm.
 *
 * We test here for:
 *  - probability of success for different complexities
 *  - time to find a refutation and/or solution
 */

#include <time.h>
#include <stdio.h>
#include <stdarg.h>

#include "evpow.h"

static void
failure(char*fmt, ...) {
	va_list vl;
	va_start(vl, fmt);
	vprintf(fmt, vl);
	printf("\n");
	exit(1);
} /* failure */

#define SOME_BYTES_COUNT (64)
typedef struct test_rec_s {
	uint8_t		some_bytes[SOME_BYTES_COUNT];
	evpow_answer	answer;
	evpow_hash	hash;
} test_rec_t;

static void
fill_some_bytes(uint8_t* some_bytes) {
	int i;
	unsigned char *text = "Z0MePuDeZ";
	unsigned char*p = text;
	uint8_t mask = 0x88;
	for (i=9;i<SOME_BYTES_COUNT;i++) {
		some_bytes[i] = *p ^ mask;
		mask = ~mask;
		p++;
		if (!*p) {
			p = text; // start anew.
		}
	}
} /* fill_some_bytes */

static int
timed_solve( uint8_t* prefix
           , size_t prefix_size
           , uint8_t* answer
           , uint8_t* solution_hash
           , int complexity_shift
           , uint16_t complexity_mantissa
	   , uint32_t milliseconds_allowance
	   , uint32_t attempts_allowed
           ) {
	clock_t start = clock();
	int32_t attempts_count;
	int r = evpow_solve(prefix, prefix_size, answer, solution_hash, complexity_shift, complexity_mantissa, milliseconds_allowance, attempts_allowed, & attempts_count);
	clock_t end = clock();
	int64_t diff = end - start;
	int64_t milliseconds = (diff * 1000 + CLOCKS_PER_SEC - 1) / CLOCKS_PER_SEC;
	printf("answer found %d, complexity shift %3d, mantissa %04x, time %ld (ms), attempts %d\n", r, complexity_shift, complexity_mantissa, milliseconds, attempts_allowed);
	fflush(stdout);
	return 0;
} /* timed_solve */

static void
test_evpow_logic(void) {
	test_rec_t testrec;
	fill_some_bytes(testrec.some_bytes);
	if (!timed_solve(testrec.some_bytes, sizeof(testrec.some_bytes), testrec.answer, testrec.hash, 1,0xffff, 30000, 300000)) {
		failure("unable to find answer for initial puzzle");
	}
} /* test_evpow_logic */

int main(void) {
	test_evpow_logic();
	return 0;
} /* main */

