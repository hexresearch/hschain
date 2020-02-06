/* EVPOW implementation.
 *
 */

#include <string.h>
#include <time.h>

#include <openssl/sha.h>

#include "evpow.h"

#if 01
#include "picosat.h"

typedef PicoSAT Solver;
#define solver_new picosat_init
#define solver_delete picosat_reset
#define solver_print picosat_print
#define solver_add picosat_add
#define solver_sat picosat_sat
#define solver_deref picosat_deref
#define solver_set_interrupt picosat_set_interrupt
#define solver_set_seed picosat_set_seed

#define SOLVER_SATISFIABLE PICOSAT_SATISFIABLE

#else
#include "yals.h"

typedef struct solver_s {
	Yals*    inner_solver;
	int      seed;
	int*     literals_added;
	int      literals_added_count;
	int      literals_added_capacity;
} Solver;
Solver* solver_new(void) {
	Solver* solver = malloc(sizeof(*solver));
	if (!solver) {
		printf("unable to allocate solver\n");
		exit(1);
	}
	solver->inner_solver = NULL;
	solver->seed = 0;
	solver->literals_added_count = 0;
	solver->literals_added_capacity = 16384;
	solver->literals_added = malloc(solver->literals_added_capacity * sizeof(*solver->literals_added));
	if (!solver->literals_added) {
		printf("unable to allocate literals cache\n");
		exit(1);
	}
	return solver;
} /* solver_new */
void solver_delete(Solver*solver) {
	if (!solver) { return ; }
	if (solver->inner_solver) {
		yals_del(solver->inner_solver);
	}
	if (solver->literals_added) {
		free(solver->literals_added);
	}
	free(solver);
} /* solver_delete */
#define solver_set_interrupt(s, d, i) ((void)0)
void solver_add(Solver*solver, int literal) {
	if (solver->literals_added_count >= solver->literals_added_capacity) {
		solver->literals_added_capacity *= 2;
		solver->literals_added = reaalloc(solver->literals_added, solver->literals_added_capacity * sizeof(*solver->literals_added));
		if (!solver->literals_added) {
			printf("unable to realloc added literals cache\n");
			exit(1);
		}
	}
	soiver->literals_added[solver->literals_added_count] = literal;
	solver->literals_added_count ++;
} /* solver_add */
void solver_sat(Solver* solver, int decision_limit_unused) {
	int i;
	(void)decision_limit_unused;
	if (solver->inner_solver) {
		yals_del(solver->inner_solver);
	}
	solver->inner_solver = yals_new();
	yals_srand(solver->inner_solver, solver->seed);
	solver->seed ++; // choose some other value than last call.
        for (i=0;i<solver->literals_added_count; i++) {
		yals_add(solver->inner_solver, solver->literals_added);
	}
	return yals_sat(solver->inner_solver);
} /* solver_sat */
#define solver_deref(solver, lit) yals_deref(solver->inner_solver, lit)
void solver_set_seed(Solver*solver, int seed) {
	solver->seed = seed;
} /* solver_set_seed */
void solver_print(Solver*solver, FILE*f) {
	int i, maxvar = 0, clause_count = 0;
	for (i=0;i<solver->literals_added_count;i++) {
		int var = abs(solver->literals_added[i]);
		clause_count += var == 0 ? 1 : 0;
		maxlit = var > maxlit ? var : maxlit;
	}
	fprintf(f, "p cnf %d %d\n", maxvar, clause_count);
	for (i=0;i<solver->literals_added_count;i++) {
		fprintf(f, " %d", solver->literals_added[i]);
		if (!solver->literals_added[i]) {
			fprintf(f, "\n");
		}
	}
} /* solver_print */

#define SOLVER_SATISFIABLE (10)

#endif

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
			//printf("found repeated literal\n");
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
create_instance(uint8_t* prefix_hash, Solver* solver, int clauses_count, int fixed_bits_count, uint32_t fixed_bits) {
	int clause;
	int literal_index;
	SHA256_CTX ctx_after_hash;
	SHA256_Init(&ctx_after_hash);
	SHA256_Update(&ctx_after_hash, prefix_hash, SHA256_DIGEST_LENGTH);
	for (literal_index = 0; literal_index < fixed_bits_count; literal_index ++) {
		int variable = literal_index + 1;
		int literal = variable;
		if ((fixed_bits & (1 << literal_index)) == 0) {
			literal = - literal;
		}
		solver_add(solver, literal);
		solver_add(solver, 0);
	}
	for (clause = 0; clause < clauses_count; clause++) {
		int literals[EVPOW_K];
		create_clause(&ctx_after_hash, clause, literals, clause == clauses_count - 1);
#if 0
		printf("clause:");
		for (literal_index = 0; literal_index < EVPOW_K; literal_index ++) {
			printf(" %d", literals[literal_index]);
			picosat_add(solver, literals[literal_index]);
		}
		printf("\n");
#endif
		int num_literals_remain = EVPOW_K;
#if 0
		if (fixed_bits_count > 0) {
			num_literals_remain = 0;
			for (literal_index = 0; literal_index < EVPOW_K; literal_index++) {
				int literal = literals[literal_index];
				int variable = abs(literal) - 1;
				if (variable < fixed_bits_count) {
					int must_positive = ((fixed_bits >> variable) & 1) != 0;
					int this_positive = literal > 0;
					if (must_positive == this_positive) {
						// completely satisfied, skip.
						num_literals_remain = -1;
						break;
					} else {
						// nothing to do - we do not copy unsatisfied literal.
					}
				} else {
					literals[num_literals_remain] = literal;
					num_literals_remain ++;
				}
			}
		}
#endif
		if (num_literals_remain >= 0) { // not trivial.
			for (literal_index = 0; literal_index < num_literals_remain; literal_index ++) {
				solver_add(solver, literals[literal_index]);
			}
			solver_add(solver, 0); // close clause.
		}
	}
	//printf("clauses added %d\n", picosat_added_original_clauses(solver));
} /* create_instance */

static void
extract_solution_answer(Solver* solver, uint8_t* answer) {
	int i;
	for (i = 0; i < EVPOW_ANSWER_BYTES; i ++) {
		answer[i] = 0;
	}
#if 01
	//printf("answer (least significant bit first):");
	for (i = 0; i < EVPOW_ANSWER_BITS; i ++) {
		int lit = i + 1;
		int is_true = solver_deref(solver, lit) > 0; // 1 means "true", -1 means "false" and 0 is unknown (must not be).
		answer[i / 8] |= is_true << (i % 8);
		//if (0 == (i % 8)) { printf(" "); }
		//printf("%d", is_true);
	}
	//printf("\n");
#endif
} /* extract_solution_answer */

// hash is treated as little-endian integer of SHA256_DIGEST_LENGTH*8 bits.
static int
under_complexity_threshold(uint8_t* hash, int complexity_shift, uint16_t complexity_mantissa) {
	int i;
	int bytes_zero = complexity_shift / 8;
	int shift_within_byte = complexity_shift % 8;
	int byte_index = SHA256_DIGEST_LENGTH - (complexity_shift + 16 + 7) / 8;
	uint32_t accum;
#if 0
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
	//printf("checking mantissa\n");
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
find_answer(SHA256_CTX* context_after_prefix, uint8_t* answer, uint8_t* full_hash, int milliseconds_allowance, int complexity_shift, uint16_t complexity_mantissa, Solver* solver, int32_t attempts_count, int32_t* attempts_done) {
	int attempts = 0;
	clock_t end_time;
	clock_t last_time;

	// configure timeout - it will include time to generate problem.
	last_time = clock();
	if (milliseconds_allowance > 0) {
		end_time = last_time + (milliseconds_allowance * CLOCKS_PER_SEC + 999)/1000;
		solver_set_interrupt(solver, (void*)&end_time, check_clock);
	}
	//printf("complexity shift %d, mantissa %04x\n", complexity_shift, complexity_mantissa);
	while (attempts_count <= 0 || attempts < attempts_count) {
		SHA256_CTX full_hash_context;
		// obtain a solution.
		int status;
		int i;
	       	status = solver_sat(solver, -1);
		//printf("decisions made %llu, propagations made %llu\n", picosat_decisions(solver), picosat_propagations(solver));
		//printf("status %d\n", status);
		if (status != SOLVER_SATISFIABLE) {
			break;
		} else {
			clock_t curr_time = clock();
			//printf("solution found in %ld ms\n", ((curr_time - last_time) * 1000 + CLOCKS_PER_SEC - 1)/CLOCKS_PER_SEC);
			last_time = curr_time;
			//printf("."); fflush(stdout);
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
		if (attempts_done) {
			*attempts_done ++;
		}

		// here we form a clause that blocks current assignment.
		// For current satisfying assignment, say, x1 and !x2 we add
		// clause !x1 \/ x2 - either x1 will be assigned to false
		// or x2 to true, or both will be assigned to different values.
		for (i = 0; i < EVPOW_ANSWER_BITS; i ++) {
			int variable = i + 1;
			int literal = variable;
			if ((answer [i / 8] & (1 << (i % 8))) != 0) {
				literal = -variable;
			}
			solver_add(solver, literal);
		}
		solver_add(solver, 0); // finalize clause addition. we are ready for another solver_sat() call.
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
	   , char* cnf_fn
) {
	SHA256_CTX prefix_hash_context;
	SHA256_CTX intermediate_prefix_hash_context;
	uint8_t prefix_hash[SHA256_DIGEST_LENGTH];
	Solver* solver;
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
	solver = solver_new();
	if (!solver) {
		return 0;
	}
	solver_set_seed(solver, 1); // get predictable results.

	// Create instance and feed it to solver.
	create_instance(prefix_hash, solver, clauses_count, fixed_bits_count, fixed_bits);

	if (cnf_fn) {
		FILE* f=fopen(cnf_fn, "w");
		if (f) {
			solver_print(solver, f);
			fclose(f);
		} else {
			printf("unable to create CNF file %s\n", cnf_fn);
		}
	}

	// find solution if we can.
	r = find_answer(&prefix_hash_context, answer, solution_hash, milliseconds_allowance, complexity_shift, complexity_mantissa, solver, attempts_allowed, attempts_done);

	solver_delete(solver);
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

