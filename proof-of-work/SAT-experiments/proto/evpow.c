/* EVPOW implementation.
 *
 */

#include <time.h>

#include <openssl/sha.h>

#include "picosat.h"

#include "evpow.h"

static int
check_clock(void*p) {
	clock_t* clk = (clock_t*)p;
	return clock() > *clk;
} /* check_clock */

static void
create_instance(uint8_t* prefix_hash, PicoSAT* solver) {
	int clause;
	int literal_index;
	SHA256_CTX ctx_after_hash;
	SHA256_Init(&ctx_after_hash);
	SHA256_Update(&ctx_after_hash, prefix_hash, SHA256_DIGEST_LENGTH);
	for (clause = 0; clause < EVPOW_CLAUSES_COUNT; clause++) {
		SHA256_CTX hash_ctx_for_clause = ctx_after_hash;
		uint8_t buffer[4];
		uint8_t clause_hash[SHA256_DIGEST_LENGTH];
		buffer[0] = clause >>  0;
		buffer[1] = clause >>  8;
		buffer[2] = clause >> 16;
		buffer[3] = clause >> 24;
		SHA256_Update(&hash_ctx_for_clause, buffer, sizeof(buffer));
		SHA256_Final(clause_hash, &hash_ctx_for_clause);
#if EVPOW_K > 16
#   error "Algorithm in its current version won't work properly for such a big K (k=4..6 are good choices)"
#endif
		for (literal_index = 0; literal_index < EVPOW_K; literal_index ++) {
			uint16_t vplow  = clause_hash[literal_index * 2 + 0];
			uint16_t vphigh = clause_hash[literal_index * 2 + 1];
			uint16_t variable_polarity = (vphigh << 8) | vplow;
			// variables for clauses are numbered 1..EVPOW_ANSWER_BITS.
			// bits are numberef 0..EVPOW_ANSWER_BITS-1.
			int variable = ((variable_polarity >> 1) % EVPOW_ANSWER_BITS) + 1;
			int literal = (variable_polarity & 1) ? variable : -variable;
			picosat_add(solver, literal);
		}
		picosat_add(solver, 0); // close the current clause.
	}
} /* create_instance */

static int
find_answer(SHA256_CTX* context_after_prefix, PicoSAT* solver, int32_t find_attempts, int32_t* attempts_count) {
	return 0;
} /* find_answer */

int
evpow_solve( uint8_t* prefix
	   , size_t prefix_size
	   , uint8_t* answer
	   , uint8_t* solution_hash
	   , int complexity_shift
	   , uint16_t complexity_mantissa
	   , int32_t milliseconds_allowance
	   , int32_t attempts_allowed
	   , int32_t* attempts_done
) {
	SHA256_CTX prefix_hash_context;
	SHA256_CTX intermediate_prefix_hash_context;
	uint8_t prefix_hash[SHA256_DIGEST_LENGTH];
	clock_t end_time;
	PicoSAT* solver;
	int r;

	// Compute hash of prefix.
	SHA256_Init(&prefix_hash_context);
	SHA256_Update(&prefix_hash_context, prefix, prefix_size);
	intermediate_prefix_hash_context = prefix_hash_context;
	SHA256_Final(prefix_hash, &intermediate_prefix_hash_context);

	// clear reporting when asked.
	if (attempts_done) {
		*attempts_done = 0;
	}

	// Create and configure picosat instance.
	solver = picosat_init();
	if (!solver) {
		return 0;
	}
	picosat_set_seed(solver, 1); // get predictable results.

	// configure timeout - it will include time to generate problem.
	if (milliseconds_allowance > 0) {
		end_time = clock() + (milliseconds_allowance * CLOCKS_PER_SEC + 999)/1000;
		picosat_set_interrupt(solver, (void*)&end_time, check_clock);
	}

	// Create instance and feed it to solver.
	create_instance(prefix_hash, solver);

	// find solution if we can.
	r = find_answer(&prefix_hash_context, solver, attempts_allowed, attempts_done);

	picosat_reset(solver);
	return r;
} /* evpow_solve */

