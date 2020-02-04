/* EVPOW implementation.
 *
 */

#include <string.h>
#include <time.h>

#include <openssl/sha.h>

#include "picosat.h"

#include "evpow.h"

static int
check_clock(void*p) {
	clock_t* clk = (clock_t*)p;
	//printf("checking clock (%ld ms remains)\n", ((*clk - clock()) * 1000 + CLOCKS_PER_SEC - 1)/CLOCKS_PER_SEC);
	return clock() > *clk;
} /* check_clock */

#if EVPOW_K > 16
#   error "Algorithm in its current version won't work properly for such a big K (k=4..6 are good choices)"
#endif
static void
create_clause(SHA256_CTX* hash_ctx_for_clause, int clause, int* clause_literals, int last_clause) {
	uint8_t clause_hash[SHA256_DIGEST_LENGTH];
	int literal_index;
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
	if (!last_clause) {
		SHA256_Init(hash_ctx_for_clause);
		SHA256_Update(hash_ctx_for_clause, clause_hash, sizeof(clause_hash));
	}
} /* create_clause */

static void
create_instance(uint8_t* prefix_hash, PicoSAT* solver, int clauses_count) {
	int clause;
	int literal_index;
	SHA256_CTX ctx_after_hash;
	SHA256_Init(&ctx_after_hash);
	SHA256_Update(&ctx_after_hash, prefix_hash, SHA256_DIGEST_LENGTH);
	for (clause = 0; clause < clauses_count; clause++) {
		int literals[EVPOW_K];
		create_clause(&ctx_after_hash, clause, literals, clause == clauses_count - 1);
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
find_answer(SHA256_CTX* context_after_prefix, uint8_t* answer, uint8_t* full_hash, int milliseconds_allowance, int complexity_shift, uint16_t complexity_mantissa, PicoSAT* solver, int32_t attempts_count, int32_t* attempts_done) {
	int attempts = 0;
	clock_t end_time;
	clock_t last_time;

	// configure timeout - it will include time to generate problem.
	last_time = clock();
	if (milliseconds_allowance > 0) {
		end_time = last_time + (milliseconds_allowance * CLOCKS_PER_SEC + 999)/1000;
		picosat_set_interrupt(solver, (void*)&end_time, check_clock);
	}

	while (attempts_count <= 0 || attempts < attempts_count) {
		SHA256_CTX full_hash_context;
		// obtain a solution.
		int status;
		int i;
	       	status = picosat_sat(solver, -1);
		printf("decisions made %llu, propagations made %llu\n", picosat_decisions(solver), picosat_propagations(solver));
		if (status != PICOSAT_SATISFIABLE) {
			break;
		} else {
			clock_t curr_time = clock();
			printf("solution found in %ld ms\n", ((last_time - curr_time) * 1000 + CLOCKS_PER_SEC - 1)/CLOCKS_PER_SEC);
			last_time = curr_time;
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
		*attempts_done ++;

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
	   , int clauses_count
	   , int complexity_shift
	   , uint16_t complexity_mantissa
	   , int32_t milliseconds_allowance
	   , int32_t attempts_allowed
	   , int32_t* attempts_done
	   , int fixed_bits_count
	   , uint32_t fixed_bits
) {
	SHA256_CTX prefix_hash_context;
	SHA256_CTX intermediate_prefix_hash_context;
	uint8_t prefix_hash[SHA256_DIGEST_LENGTH];
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

	// Create instance and feed it to solver.
	create_instance(prefix_hash, solver, clauses_count);

	// find solution if we can.
	r = find_answer(&prefix_hash_context, answer, solution_hash, milliseconds_allowance, complexity_shift, complexity_mantissa, solver, attempts_allowed, attempts_done);

	picosat_reset(solver);
	return r;
} /* evpow_solve */

int
evpow_check( uint8_t* prefix
           , size_t prefix_size
           , uint8_t* answer
           , uint8_t* hash_to_compare
	   , int clauses_count
           , int complexity_shift
           , uint16_t complexity_mantissa
           ) {
	SHA256_CTX partial_ctx, full_ctx, ctx_after_hash;
	uint8_t hash[SHA256_DIGEST_LENGTH];
	int clause;
	// fastest first - rarity compliance check.
	if (!under_complexity_threshold(hash_to_compare, complexity_shift, complexity_mantissa)) {
		printf("NOT RARE ENOUGH!\n");
		return 0;
	}
	SHA256_Init(&partial_ctx);
	SHA256_Update(&partial_ctx, prefix, prefix_size);
	full_ctx = partial_ctx;
	SHA256_Update(&full_ctx, answer, EVPOW_ANSWER_BYTES);
	SHA256_Final(hash, &full_ctx);
	// second fastest second - hashes are equal.
	if (0 != memcmp(hash, hash_to_compare, SHA256_DIGEST_LENGTH)) {
		printf("HASH MISMATCH!\n");
		return 0;
	}
	// slowest one last - does answer really answer the puzzle?
	SHA256_Final(hash, &partial_ctx);
	SHA256_Init(&ctx_after_hash);
	SHA256_Update(&ctx_after_hash, hash, SHA256_DIGEST_LENGTH);
	{
		int i;
		printf("answer to check against:");
		for (i=0;i<EVPOW_ANSWER_BITS;i++) {
			int bit = (answer[i/8] >> (i%8))&1;
			printf("%s%d",(i % 8) == 0? " ": "", bit);
		}
		printf("\n");
	}
	for (clause = 0; clause < clauses_count; clause ++) {
		int literals[EVPOW_K];
		int literal_index;
		create_clause(&ctx_after_hash, clause, literals, clause == clauses_count - 1);
#if 0
		printf("clause:");
		for (literal_index = 0; literal_index < EVPOW_K; literal_index ++) {
			printf(" %d", literals[literal_index]);
		}
		printf("\n");
#endif
		for (literal_index = 0; literal_index < EVPOW_K; literal_index ++) {
			int literal = literals[literal_index];
			int positive = literal > 0;
			int variable = abs(literal) - 1;
			int flag = (answer[variable / 8] & (1 << (variable % 8)));
			if ((flag == 0) == (positive == 0)) {
				// at least one satisfying literal satisfies complete clause.
				break;
			}
		}
		if (literal_index >= EVPOW_K) {
			printf("ANSWER DOES NOT ANSWER!\n");
			return 0;
		}
	}
	return 1;
} /* evpow_check */

