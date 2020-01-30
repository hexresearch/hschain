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
	printf("checking clock (%ld ms remains)\n", ((*clk - clock()) * 1000 + CLOCKS_PER_SEC - 1)/CLOCKS_PER_SEC);
	return clock() > *clk;
} /* check_clock */

#if EVPOW_K > 16
#   error "Algorithm in its current version won't work properly for such a big K (k=4..6 are good choices)"
#endif
static void
create_clause(SHA256_CTX* hash_ctx_for_clause, int clause, int* clause_literals) {
	uint8_t buffer[4];
	uint8_t clause_hash[SHA256_DIGEST_LENGTH];
	int literal_index;
	buffer[0] = clause >>  0;
	buffer[1] = clause >>  8;
	buffer[2] = clause >> 16;
	buffer[3] = clause >> 24;
	SHA256_Update(hash_ctx_for_clause, buffer, sizeof(buffer));
	SHA256_Final(clause_hash, hash_ctx_for_clause);
	for (literal_index = 0; literal_index < EVPOW_K; literal_index ++) {
		uint16_t vplow  = clause_hash[literal_index * 2 + 0];
		uint16_t vphigh = clause_hash[literal_index * 2 + 1];
		uint16_t variable_polarity = (vphigh << 8) | vplow;
		int variable_index = ((variable_polarity / 2) % EVPOW_ANSWER_BITS) + 1;
		int assign_true = variable_polarity % 2;
		int literal;
		// and here we try to find a free variable index that is not used in clause.
		// otherwise we can get reduced clause (with variables less than K) and
		// trivial clause (where x and ~x are both present).
		// Both reduce complexity of the problem.
		while (1) {
			int check_index;
			for (check_index = 0; check_index < literal_index; check_index ++) {
				if (abs(clause_literals[check_index]) == variable_index) {
					break;
				}
			}
			if (check_index >= literal_index) {
				break;
			}
			printf("found repeated literal\n");
			variable_index = (variable_index % EVPOW_ANSWER_BITS) + 1; // we put it into range [1,EVPOW_ANSWER_BITS]
		}
		literal = assign_true ? variable_index : -variable_index;
		clause_literals[literal_index] = literal;
	}
} /* create_clause */
static void
create_instance(uint8_t* prefix_hash, PicoSAT* solver) {
	int clause;
	int literal_index;
	SHA256_CTX ctx_after_hash;
	SHA256_Init(&ctx_after_hash);
	SHA256_Update(&ctx_after_hash, prefix_hash, SHA256_DIGEST_LENGTH);
	for (clause = 0; clause < EVPOW_CLAUSES_COUNT; clause++) {
		SHA256_CTX hash_ctx_for_clause = ctx_after_hash;
		int literals[EVPOW_K];
		create_clause(&hash_ctx_for_clause, clause, literals);
		printf("clause:");
		for (literal_index = 0; literal_index < EVPOW_K; literal_index ++) {
			printf(" %d", literals[literal_index]);
			picosat_add(solver, literals[literal_index]);
		}
		printf("\n");
		picosat_add(solver, 0); // close clause.
	}
	printf("clauses added %d\n", picosat_added_original_clauses(solver));
} /* create_instance */

static void
extract_solution_answer(PicoSAT* solver, uint8_t* answer) {
	int i;
	for (i = 0; i < EVPOW_ANSWER_BYTES; i ++) {
		answer[i] = 0;
	}
	printf("answer (least significant bit first):");
	for (i = 0; i < EVPOW_ANSWER_BITS; i ++) {
		int lit = i + 1;
		int is_true = picosat_deref(solver, lit) > 0; // 1 means "true", -1 means "false" and 0 is unknown (must not be).
		answer[i / 8] |= is_true << (i % 8);
		if (0 == (i % 8)) { printf(" "); }
		printf("%d", is_true);
	}
	printf("\n");
} /* extract_solution_answer */

// hash is treated as little-endian integer of SHA256_DIGEST_LENGTH*8 bits.
static int
under_complexity_threshold(uint8_t* hash, int complexity_shift, uint16_t complexity_mantissa) {
	int i;
	int bytes_zero = complexity_shift / 8;
	int shift_within_byte = complexity_shift % 8;
	int byte_index = SHA256_DIGEST_LENGTH - 1 - (complexity_shift + 16 + 7) / 8;
	uint32_t accum;
#if 01
	printf("full hash as integer:");
	for (i = SHA256_DIGEST_LENGTH - 1; i >= 0; i --) {
		printf(" %02x", hash[i]);
	}
	printf("\n");
#endif
	// check whether required number of leading bytes are zero.
	for (i=0;i<bytes_zero;i++) {
		if (hash[SHA256_DIGEST_LENGTH - 1 - i]) {
			return 0;
		}
	}
	printf("checking mantissa\n");
	accum = 0;
	if (byte_index + 2 >=0 && byte_index + 2 < SHA256_DIGEST_LENGTH) {
		accum |= hash[byte_index + 2];
	}
	accum <<= 8;
	if (byte_index + 1 >= 0 && byte_index + 1 < SHA256_DIGEST_LENGTH) {
		accum |= hash[byte_index + 1];
	}
	accum <<= 8;
	if (byte_index     >= 0) {
		accum |= hash[byte_index];
	}
	// now accum has three hash bytes around what we will compare to mantissa.
	accum >>= (8 - shift_within_byte) % 8;

	// compare mantissas.
	return accum <= complexity_mantissa;
} /* under_complexity_threshold */

static int
find_answer(SHA256_CTX* context_after_prefix, uint8_t* answer, uint8_t* full_hash, int complexity_shift, uint16_t complexity_mantissa, PicoSAT* solver, int32_t find_attempts, int32_t* attempts_count) {
	int attempts = 0;
	while (attempts_count <= 0 || attempts < attempts_count) {
		SHA256_CTX full_hash_context;
		// obtain a solution.
		int status;
		int i;
	       	status = picosat_sat(solver, -1);
		printf("decisions made %llu, propagations made %llu\n", picosat_decisions(solver), picosat_propagations(solver));
		if (status != PICOSAT_SATISFIABLE) {
			break;
		}
		// extract a solution into the answer.
		extract_solution_answer(solver, answer);
		// compute full hash.
		full_hash_context = *context_after_prefix;
		SHA256_Update(&full_hash_context, answer, EVPOW_ANSWER_BYTES);
		SHA256_Final(full_hash, &full_hash_context);
		if (under_complexity_threshold(full_hash, complexity_shift, complexity_mantissa)) {
			return 1; // and everything is in place - answer filled, hash computed.
		}
		attempts ++;

		// here we form a clause that blocks current assignment.
		// For current satisfying assignment, say, x1 and !x2 we add
		// clause !x1 \/ x2 - either x1 will be assigned to false
		// or x2 to true, or both will be assigned to different values.
		for (i = 0; i < EVPOW_ANSWER_BITS; i ++) {
			int variable = i + 1;
			if ((answer [i / 8] & (1 << (i % 8))) == 0) {
				picosat_add(solver, variable);
			} else {
				picosat_add(solver, -variable);
			}
		}
		picosat_add(solver, 0); // finalize clause addition. we are ready for another picosat_sat() call.
	}
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
	r = find_answer(&prefix_hash_context, answer, solution_hash, complexity_shift, complexity_mantissa, solver, attempts_allowed, attempts_done);

	picosat_reset(solver);
	return r;
} /* evpow_solve */

