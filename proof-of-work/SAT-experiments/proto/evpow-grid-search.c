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

void
next_sample(test_rec_t* sample) {
	int i;
	for (i = 0; i < sizeof(sample->some_bytes); i ++) {
		sample->some_bytes[i] += i * 2 + 1;
	}
} /* next_sample */

double
measure_average_solution_time
	( int complexity_shift, uint16_t complexity_mantissa, test_rec_t* sample
	, int64_t search_time_ms
	, int64_t flips
	) {
	int found = 0;
	int i, num_samples = 100;
	clock_t start = clock(), end;
	for (i = 0; i < num_samples; i ++) {
		while(!found) {
			next_sample(sample);
			found = evpow_solve
				( testrec.some_bytes
        	                , sizeof(testrec.some_bytes)
                	        , testrec.answer
	                        , testrec.hash
        	                , EVPOW_ADVISED_CLAUSES_COUNT
                	        , complexity_shift
	                        , complexity_mantissa
        	                , search_time_ms
                	        , attempts_allowed
	                        , 0
        	                , 0
                	        , NULL
	                        );
		}

	}
	end = clock();
	return ((double)start - end)*1000.0/CLOCKS_PER_SEC/num_samples;
} /* measure_average_solution_time */

void
grid_search(FILE* report) {
	test_rec_t testrec;
	int complexity_shift = 9;
	uint16_t complexity_mantissa = 0xc000; // shift and this mantissa would give us approx 1 min of solution search time.
	int64_t mantissa_multiplier = 0x0eac0c6e7;
	int64_t search_time_low = 500;
	int64_t search_time_high = 
	int64_t attempts_allowed = 2*250000; // about 250000 flips per one tenths of a second - 0.2 seconds to look for solution.
	memset(&testrec, 0, sizeof(testrec));
	testrec.some_bytes[0] = 123;
	testrec.some_bytes[1] = 222; // for something fancier than just zeroes.
	while (1) {
		clock_t total_time = 0;
		clock_t two_hours = 2*60*60*CLOCKS_PER_SEC;
		int samples = 0;
		int failed_tries = 0;
		while (total_time < two_hours && samples < 100) {
			samples ++;
			int found = 0;
			clock_t start = clock();
			while (!found) {
				int carry, i;
				printf("sample %d\n", samples);
				// generating data for another sample.
				for (i = 0, carry = 0;i < sizeof(testrec.some_bytes);i++) {
					int r = testrec.some_bytes[i] + (i + 1) * 2 + 1 + carry;
					carry = r >> 8;
					testrec.some_bytes[i] = r & 255;
				}
				found = evpow_solve
					   ( testrec.some_bytes
				           , sizeof(testrec.some_bytes)
			        	   , testrec.answer
				           , testrec.hash
				           , EVPOW_ADVISED_CLAUSES_COUNT
				           , complexity_shift
				           , complexity_mantissa
	        			   , 2000
				           , attempts_allowed
				           , 0
				           , 0
					   , NULL
	           			   );
				failed_tries += found == 0;

			}
			clock_t delta = clock() - start;
			total_time += delta;
		}
		double total_time_sec = total_time /((double) CLOCKS_PER_SEC);
		double secs_per_sample = total_time_sec / samples;
		fprintf(report, "complexity shift %3d, mantissa %04x: total time %10.4g sec, time per sample solution %10.4g sec, success percentage %6.2g\n", complexity_shift, complexity_mantissa, total_time_sec, secs_per_sample, 100.0 * samples / ((double)(failed_tries + samples)));
		fflush(report);
		if (total_time > two_hours * 3 / 2) {
			break;
		}
		uint64_t next_complexity = complexity_mantissa * mantissa_multiplier;
		if (next_complexity < (1ULL << 47)) {
			complexity_shift ++;
			next_complexity = next_complexity * 2 + 1;
		}
		complexity_mantissa = next_complexity >> 32;
	}
} /* */


int
main(void) {
	FILE* report = fopen("grid-search-report.txt", "w");
	if (!report) {
		printf("unable to open a report file\n");
		return 1;
	}
        setvbuf(stdout, NULL, _IONBF, 0);
        setvbuf(stderr, NULL, _IONBF, 0);
	grid_search(report);
	return 0;
} /* main */

