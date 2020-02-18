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
	int i, carry;
	for (i = 0, carry = 0; i < sizeof(sample->some_bytes); i ++) {
		int r = sample->some_bytes[i] + i * 2 + 1 + carry;
		carry = r >> 8;
		sample->some_bytes[i] = r;
	}
} /* next_sample */

double
measure_average_solution_time
	( int complexity_shift, uint16_t complexity_mantissa, test_rec_t* sample
	, int64_t search_time_ms
	, int64_t attempts_allowed
	, int num_samples
	) {
	int found;
	int i;
	clock_t start = clock(), end;
	for (i = 0; i < num_samples; i ++) {
		do {	
			next_sample(sample);
			found = evpow_solve
				( sample->some_bytes
        	                , sizeof(sample->some_bytes)
                	        , sample->answer
	                        , sample->hash
        	                , EVPOW_ADVISED_CLAUSES_COUNT
                	        , complexity_shift
	                        , complexity_mantissa
        	                , search_time_ms
                	        , attempts_allowed
	                        , 0
        	                , 0
                	        , NULL
	                        );
			printf("%c", found ? '+' : '-');
		} while(!found);
	}
	end = clock();
	printf("\n");
	return ((double)end - start)*1000.0/CLOCKS_PER_SEC/num_samples;
} /* measure_average_solution_time */

#define ARGS \
	ARG(complexity_shift, int, 7) \
	ARG(complexity_mantissa, uint16_t, 0xc000) \
	ARG(search_time_ms, int64_t, 2000) \
	ARG(attempts_allowed, uint64_t, 250000 * 2) \
	ARG(num_samples, int, 100) \
	ARG(nonce, uint64_t, 0xdeadbeef600df00dULL) \
	ARG(break_power, double, 2.0) \
	/* done with args */

void
usage(char*arg) {
	printf("incorrect argument %s\n\nusage:\n", arg);
#define ARG(n, ty, v) printf("  --" #n ": type " #ty ", value " #v "\n");
	ARGS
#undef  ARG
	printf("\nplease note that you can use dash instead of underscore in switch names\n");
	exit(1);
} /* usage */

int
match_arg(char**parg, char*match) {
	char* arg = *parg;
	if (arg[0] != '-' || arg[1] != '-') {
		return 0;
	}
	arg += 2;
	while (*match && *arg) {
		char c = *match;
		char ac = *arg;
		int equal = c == ac;
		if (!equal && (c != '_' || ac != '-')) {
			return 0;
		}
		match ++;
		arg ++;
	}
	if (*arg != '=') {
		return 0;
	}
	*parg = arg + 1; // past equal sign.
	return 1;
} /* match_arg */

double
parse_double(char*val) {
	char* endptr;
	double r = strtod(val, &endptr);
	if (!*val || *endptr) {
		printf("not a double: '%s'\n", val);
		exit(1);
	}
	return r;
} /* parse_double */

#define parse_int64_t parse_int
#define parse_uint64_t parse_int
#define parse_uint16_t parse_int
int64_t
parse_int(char*val) {
	char* endptr = "<none>";
	int64_t r = strtoll(val, &endptr, 0);
	if (!*val || *endptr) {
		printf("not an integer: %s/%s\n", val, endptr);
		exit(1);
	}
	return r;
} /* parse_int */

void
parse_measure(int argc, char**argv) {
	test_rec_t testrec;
	int i;
#define ARG(n, ty, val) ty n = val;
	ARGS
#undef ARG
	memset(&testrec, 0, sizeof(testrec));
	testrec.some_bytes[0] = 123;
	testrec.some_bytes[1] = 222; // for something fancier than just zeroes.
	for (i = 1; i < argc; i++) {
		char* a = argv[i];
		if (a[0] != '-' || a[1] != '-') {
			usage(a);
		}
#define ARG(n, ty, v) else if (match_arg(&a, #n)) { n = parse_##ty(a); }
		ARGS
#undef  ARG
		else {
			usage(argv[i]);
		}
	}
	testrec.some_bytes[2] = nonce >>  0;
	testrec.some_bytes[3] = nonce >>  8;
	testrec.some_bytes[4] = nonce >> 16;
	testrec.some_bytes[5] = nonce >> 24;
	testrec.some_bytes[6] = nonce >> 32;
	testrec.some_bytes[7] = nonce >> 40;
	testrec.some_bytes[8] = nonce >> 48;
	testrec.some_bytes[9] = nonce >> 56;
	printf("%g\n", measure_average_solution_time
			( complexity_shift, complexity_mantissa, &testrec
			, search_time_ms
			, attempts_allowed
			, num_samples
			)
		);
} /* parse_measure */


int
main(int argc, char**argv) {
        setvbuf(stdout, NULL, _IONBF, 0);
        setvbuf(stderr, NULL, _IONBF, 0);
	parse_measure(argc, argv);
	return 0;
} /* main */

