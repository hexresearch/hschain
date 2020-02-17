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

#define MIN_FIXED_BITS (7)
#define MAX_FIXED_BITS (8)
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
	int fixed_bits_count, complexity_shift;
	test_rec_t testrec;
	memset(&testrec, 0, sizeof(testrec));
	testrec.some_bytes[0] = 123;
	testrec.some_bytes[1] = 222; // for something fancier than just zeroes.
	for (complexity_shift = 4; complexity_shift < 10; complexity_shift ++) {
		for (fixed_bits_count = MIN_FIXED_BITS; fixed_bits_count < MAX_FIXED_BITS; fixed_bits_count ++) {
			uint32_t fixed_bits;
			int64_t min_answer_time = 1000000000; // 1e6 seconds - much more that 100 seconds of time allowed.
			int found_count = 0;
			printf("fixed bits count %d, complexity shift %d\n", fixed_bits_count, complexity_shift);
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
				           , complexity_shift
				           , 0xffff
			        	   , 30000
				           , -1
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
				if (ms > min_answer_time) {
					// we think we can stop stale searches immediately. good enough.
					// also note that we can find no answer faster than minimum time
					// needed for an answer.
					ms = min_answer_time;
				}
				sum_time += ms;
			}
			fprintf(report, "complexity shift %d, fixed bits count %d, summary search time %ld (ms), number of branches with answer %d, minimum time to answer %ld\n", complexity_shift, fixed_bits_count, sum_time, found_count, min_answer_time);
			fflush(report);
			evpow_display_stats(0);
		}
	}
} /* energy_expenditure_performance_for_advised_parameters */

void
find_complexity_drop(FILE* report) {
	int64_t search_time[MAX_NUMBER_OF_SEARCHES];
	int64_t not_found_times[65];
	int not_founds[65];
	int founds[65];
	int64_t found_times[65];
	int sample;
	int found_answer[MAX_NUMBER_OF_SEARCHES];
	int fixed_bits_count, complexity_shift;
	test_rec_t testrec;
	memset(&testrec, 0, sizeof(testrec));
	testrec.some_bytes[0] = 123;
	testrec.some_bytes[1] = 222; // for something fancier than just zeroes.
	complexity_shift = 6; // more realistic setting.
	int64_t attempts_allowed = 2*250000; // about 250000 flips per one tenths of a second - 0.2 seconds to look for solution.
	for (sample = 0; sample < 1000; sample++) {
		int i, carry;
		clock_t start, delta;
		// generating data for another sample.
		for (i = 0, carry = 0;i < sizeof(testrec.some_bytes);i++) {
			int r = testrec.some_bytes[i] + (i + 1) * 2 + 1 + carry;
			carry = r >> 8;
			testrec.some_bytes[i] = r & 255;
		}
		start = clock();
		int found = evpow_solve
			   ( testrec.some_bytes
		           , sizeof(testrec.some_bytes)
	        	   , testrec.answer
		           , testrec.hash
		           , EVPOW_ADVISED_CLAUSES_COUNT
		           , complexity_shift
		           , 0xffff
	        	   , 2000
		           , attempts_allowed
		           , 0
		           , 0
			   , NULL
           		   );
		delta = clock() - start;
		if (!found) {
			printf("not found at all\n");
			not_founds[0] ++;
			not_found_times[0] += delta;
			continue;
		}
		printf("answer bytes %02x %02x %02x\n", testrec.answer[0], testrec.answer[1], testrec.answer[2]);
		founds[0] ++;
		found_times[0] += delta;
		printf("found %ld ticks\n", delta);
		int byte_index;
		uint64_t fixed_bits = 0;
		for (byte_index = 7; byte_index >= 0; byte_index --) {
			fixed_bits <<= 8;
			fixed_bits |= testrec.answer[byte_index];
		}
		for(fixed_bits_count = 1; fixed_bits_count <= 64; fixed_bits_count ++) {
			start = clock();
			found = evpow_solve
				   ( testrec.some_bytes
			           , sizeof(testrec.some_bytes)
	        		   , testrec.answer
		        	   , testrec.hash
			           , EVPOW_ADVISED_CLAUSES_COUNT
			           , complexity_shift
			           , 0xffff
	        		   , 2000
			           , attempts_allowed
			           , fixed_bits_count
		        	   , fixed_bits
				   , NULL
        	   		   );
			delta = clock () - start;
			if (!found) {
				not_founds[fixed_bits_count] ++;
				not_found_times[fixed_bits_count] += delta;
				printf("somehow we managed not to find solution in time for simpler problem\n");
				continue;
			}
			printf("answer bytes %02x %02x %02x\n", testrec.answer[0], testrec.answer[1], testrec.answer[2]);
			found_times[fixed_bits_count] += delta;
			founds[fixed_bits_count] ++;
			printf("found for %d bits fixed in %ld ticks\n", fixed_bits_count, delta);
		}

	}
} /* find_complexity_drop */

void
measure_time_to_solution_for_diff_complexities(FILE* report) {
	int sample;
	test_rec_t testrec;
	int complexity_shift = 0;
	uint16_t complexity_mantissa = 0xffff;
	// A magic constant:
	// Prelude> Text.Printf.printf "%016x\n" (floor (2**(-1/8)*(2^32)) :: Integer)
        // 00000000eac0c6e7
	// It represents 2^(-8), multiplied by 2^32. We will use this to increase complexity.
	int64_t mantissa_multiplier = 0x0eac0c6e7;
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
} /* measure_time_to_solution_for_diff_complexities */


int
main(void) {
	FILE* report = fopen("report.txt", "w");
	if (!report) {
		printf("unable to open a report file\n");
		return 1;
	}
        setvbuf(stdout, NULL, _IONBF, 0);
        setvbuf(stderr, NULL, _IONBF, 0);
	//energy_expenditure_performance_for_advised_parameters(report);
	//find_complexity_drop(report); // it has proven that reducing problem complexity (naive parallelisation) does not necessarily benefit.
	measure_time_to_solution_for_diff_complexities(report);
	return 0;
} /* main */

