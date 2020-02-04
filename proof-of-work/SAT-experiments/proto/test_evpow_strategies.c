/**
 * Testing different strategies on random kSAT
 */

#include <time.h>
#include <stdio.h>
#include <string.h>

#include "evpow.h"

#define SOME_BYTES_COUNT (64)
typedef struct test_rec_s {
        uint8_t         some_bytes[SOME_BYTES_COUNT];
        evpow_answer    answer;
        evpow_hash      hash;
} test_rec_t;

#define MAX_FIXED_BITS (16)
#define MAX_NUMBER_OF_SEARCHES (1 << MAX_FIXED_BITS)

/**
 * Here we run search for same problem with different number of bits fixed (up to 16).
 *
 * The parameters here are advised ones - EVPOW_K and EVPOW_ANSWER_BITS are fixed in header file
 * and clauses count is advised one.
 *
 * We run S=2^(bits fixed) searches and record fastest time to find an answer to a puzzle. This time
 * is multiplied by S (slightly more complex - see below) and give us, let's say, total time T used
 * to find an answer. This found quantity T is proportional to energy expenditure during parallel search.
 */
void
energy_expenditure_performance_for_advised_parameters(FILE* report) {
	int64_t search_time[MAX_NUMBER_OF_SEARCHES];
	int found_answer[MAX_NUMBER_OF_SEARCHES];
	int fixed_bits_count;
	test_rec_t testrec;
	memset(&testrec, 0, sizeof(testrec));
	testrec.some_bytes[0] = 123;
	testrec.some_bytes[1] = 222; // for something fancier than just zeroes.
	for (fixed_bits_count = 0; fixed_bits_count < MAX_FIXED_BITS; fixed_bits_count ++) {
		uint32_t fixed_bits;
		int64_t min_answer_time = 1000000000; // 1e6 seconds - much more that 100 seconds of time allowed.
		int found_count = 0;
		printf("fixed bits count %d\n", fixed_bits_count);
		for (fixed_bits = 0; fixed_bits < (1 << fixed_bits_count); fixed_bits ++) {
			char fn[100];
			clock_t start = clock();
			clock_t end, delta;
			int64_t milliseconds;
			sprintf(fn, "count-%02d-bits-%08x-dump.cnf", fixed_bits_count, fixed_bits);
			int found = evpow_solve
				   ( testrec.some_bytes
			           , sizeof(testrec.some_bytes)
			           , testrec.answer
			           , testrec.hash
			           , EVPOW_ADVISED_CLAUSES_COUNT
			           , 4
			           , 0xffff
			           , 100000
			           , -1
			           , NULL
			           , fixed_bits_count     // you may spawn several parallel attempts to solve the puzzle with some bits fixed (up to 32 bits)
			           , fixed_bits      // to what value these bits are fixed
				   , fn
           			   );
			end = clock();
			found_answer[fixed_bits] = found;
			milliseconds = ((end - start) * 1000 + CLOCKS_PER_SEC - 1) / CLOCKS_PER_SEC;
			search_time[fixed_bits] = milliseconds;
			found_count += found ? 1 : 0;
			if (found && milliseconds < min_answer_time) {
				min_answer_time = milliseconds;
			}
			printf("bits %04x: %s in %ld milliseconds\n", fixed_bits, found ? "found" : "not found", milliseconds);
		}
		int64_t sum_time = 0;
		for (fixed_bits = 0; fixed_bits < (1 << fixed_bits_count); fixed_bits ++) {
			int64_t ms = search_time[fixed_bits];
			if (ms > min_answer_time) { // we think we can stop stale searches immediately. good enough.
				ms = min_answer_time;
			}
			sum_time += ms;
		}
		fprintf(report, "fixed bits count %d, summary search time %ld (ms), number of branches with answer %d, minimum time to answer %ld\n", fixed_bits_count, sum_time, found_count, min_answer_time);
		fflush(report);
	}
} /* energy_expenditure_performance_for_advised_parameters */

int
main(void) {
	FILE* report = fopen("report.txt", "w");
	if (!report) {
		printf("unable to open a report file\n");
		return 1;
	}
	energy_expenditure_performance_for_advised_parameters(report);
	return 0;
} /* main */
