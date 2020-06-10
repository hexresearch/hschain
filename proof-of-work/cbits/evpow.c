/* EVPOW implementation.
 *
 */

#include <string.h>
#include <time.h>

#include <openssl/sha.h>

#include "evpow.h"

#if 0
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
#define solver_set_attempts(solver, attempts) ((void)0)
#define solver_set_attempts_between_restarts(solve, attempts) ((void)0)

#define SOLVER_SATISFIABLE PICOSAT_SATISFIABLE

#else
#include "yals.h"

#define MAX_RUNS (256)
int64_t runs_counts[MAX_RUNS];
clock_t runs_clocks[MAX_RUNS];

typedef struct solver_s {
	Yals*    inner_solver;
	int32_t  attempts_allowed;
	int32_t  attempts_between_restarts;
	int      seed;
	int      runs;
	int*     literals_added;
	int      literals_added_count;
	int      literals_added_capacity;
	void*    interrupt_check_data;
	int      (*interrupt)(void*);
} Solver;
Solver* solver_new(void) {
	Solver* solver = malloc(sizeof(*solver));
	if (!solver) {
		printf("unable to allocate solver\n");
		exit(1);
	}
	solver->inner_solver = NULL;
	solver->seed = 0;
	solver->runs = 0;
	solver->literals_added_count = 0;
	solver->literals_added_capacity = 16384;
	solver->literals_added = malloc(solver->literals_added_capacity * sizeof(*solver->literals_added));
	solver->attempts_allowed = 25000000; // 10 seconds for YalSAT speed of 2.5M flips/second.
	solver->attempts_between_restarts = 100000;
	if (!solver->literals_added) {
		printf("unable to allocate literals cache\n");
		exit(1);
	}
	solver->interrupt = NULL;
	return solver;
} /* solver_new */
void solver_set_attempts(Solver*solver, int32_t attempts_allowed) {
	solver->attempts_allowed = attempts_allowed;
} /* solver_set_attempts */
void solver_set_attempts_between_restarts(Solver*solver, int32_t attempts_between_restarts) {
	solver->attempts_between_restarts = attempts_between_restarts;
} /* solver_set_attempts_between_restarts */
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
void solver_set_interrupt(Solver* solver,void* interrupt_data,int (*interrupt)(void*)) {
	solver->interrupt_check_data = interrupt_data;
	solver->interrupt = interrupt;
} /* solver_set_interrupt */
void solver_add(Solver*solver, int literal) {
	if (solver->literals_added_count >= solver->literals_added_capacity) {
		solver->literals_added_capacity *= 2;
		solver->literals_added = realloc(solver->literals_added, solver->literals_added_capacity * sizeof(*solver->literals_added));
		if (!solver->literals_added) {
			printf("unable to realloc added literals cache\n");
			exit(1);
		}
	}
	solver->literals_added[solver->literals_added_count] = literal;
	solver->literals_added_count ++;
} /* solver_add */
int solver_sat(Solver* solver, int decision_limit_unused) {
	int i;
	Yals* old_solver = solver->inner_solver;
	if (solver->interrupt && solver->interrupt(solver->interrupt_check_data)) {
		return 0;
	}
	(void)decision_limit_unused;
	solver->inner_solver = yals_new();
	if (old_solver) {
		// we have run solve process before, copy old solution into new.
		int i;
		for (i = 0; i < EVPOW_ANSWER_BITS; i++) {
			int lit = i + 1;
			int phase = yals_deref(old_solver, lit);
			if (phase <= 0) {
				lit = -lit;
			}
			yals_setphase(solver->inner_solver, lit);
		}
		yals_del(old_solver);
	}
	if (solver->interrupt) {
		yals_seterm(solver->inner_solver, solver->interrupt, solver->interrupt_check_data);
	}
	yals_srand(solver->inner_solver, solver->seed);
	//yals_setopt(solver->inner_solver, "pick", 1);
	yals_setopt(solver->inner_solver, "restart", solver->attempts_between_restarts);
	solver->seed ++; // choose some other value than last call.
        for (i=0;i<solver->literals_added_count; i++) {
		yals_add(solver->inner_solver, solver->literals_added[i]);
	}
	yals_setflipslimit(solver->inner_solver, solver->attempts_allowed);
	clock_t start_time = clock();
	int result = yals_sat(solver->inner_solver);
	clock_t end_time = clock();
	int current_run = solver->runs;
	if (current_run >= MAX_RUNS) {
		current_run = MAX_RUNS - 1;
	}
	solver->runs ++;
	runs_counts[current_run] ++;
	runs_clocks[current_run] += end_time-start_time;
	return result;
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
		maxvar = var > maxvar ? var : maxvar;
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
create_instance(uint8_t* suffix_hash, Solver* solver, int clauses_count, int fixed_bits_count, uint64_t fixed_bits) {
	int clause;
	int literal_index;
	SHA256_CTX ctx_after_hash;
	SHA256_Init(&ctx_after_hash);
	SHA256_Update(&ctx_after_hash, suffix_hash, SHA256_DIGEST_LENGTH);
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
		}
		printf("\n");
#endif
		int num_literals_remain = EVPOW_K;
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
under_complexity_threshold(uint8_t* hash, uint8_t* target) {
	int i;
#if 0
	printf("full hash as integer:");
	for (i = SHA256_DIGEST_LENGTH - 1; i >= 0; i --) {
		printf(" %02x", hash[i]);
	}
	printf("\n");
	printf("target as integer   :");
	for (i = SHA256_DIGEST_LENGTH - 1; i >= 0; i --) {
		printf(" %02x", target[i]);
	}
	printf("\n");
#endif
	// compare from most significant bytes downto to least signigicant.
	for (i = SHA256_DIGEST_LENGTH - 1; i >= 0; i--) {
		if (hash[i] > target[i]) {
			return 0;
		}
		if (hash[i] < target[i]) {
			return 1;
		}
	}
	return 1;
} /* under_complexity_threshold */

static int
find_answer(uint8_t* suffix, size_t suffix_size, uint8_t* answer, uint8_t* full_hash, int milliseconds_allowance, uint8_t* target, Solver* solver, double* first_result_ms) {
	clock_t end_time;
	clock_t last_time;

	// configure timeout - it will include time to generate problem.
	last_time = clock();
	if (milliseconds_allowance > 0) {
		end_time = last_time + (milliseconds_allowance * CLOCKS_PER_SEC + 999)/1000;
		solver_set_interrupt(solver, (void*)&end_time, check_clock);
	}
	while (1) {
		SHA256_CTX full_hash_context;
		// obtain a solution.
		int status;
		int i;
	       	status = solver_sat(solver, -1);
		//printf("status %d\n", status);
		clock_t curr_time = clock();
		if (first_result_ms) {
			*first_result_ms = (1000.0 * (curr_time - last_time))/CLOCKS_PER_SEC;
			first_result_ms = NULL; // use it as a flag to not store other results.
		}
		if (status != SOLVER_SATISFIABLE) {
			break;
		}
		//printf("solution found in %ld ms\n", ((curr_time - last_time) * 1000 + CLOCKS_PER_SEC - 1)/CLOCKS_PER_SEC);
		last_time = curr_time;
		//printf("."); fflush(stdout);
		// extract a solution into the answer.
		extract_solution_answer(solver, answer);
		// compute full hash.
		SHA256_Init(&full_hash_context);
		SHA256_Update(&full_hash_context, answer, EVPOW_ANSWER_BYTES);
		SHA256_Update(&full_hash_context, suffix, suffix_size);
		SHA256_Final(full_hash, &full_hash_context);
		if (under_complexity_threshold(full_hash, target)) {
			//printf("FOUND!\n");
			return 1; // and everything is in place - answer filled, hash computed.
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
evpow_solve( uint8_t* suffix
	   , size_t suffix_size
	   , uint8_t* answer
	   , uint8_t* solution_hash
	   , int clauses_count
	   , uint8_t* target
	   , int32_t milliseconds_allowance
	   , int32_t attempts_allowed
	   , int32_t attempts_between_restarts
	   , int fixed_bits_count
	   , uint64_t fixed_bits
	   , double* first_result_ms
) {
	SHA256_CTX suffix_hash_context;
	uint8_t suffix_hash[SHA256_DIGEST_LENGTH];
	Solver* solver;
	int r;

	// Compute hash of prefix.
	SHA256_Init(&suffix_hash_context);

	SHA256_Update(&suffix_hash_context, suffix, suffix_size);
	SHA256_Final(suffix_hash, &suffix_hash_context);

	// Create and configure solver instance.
	solver = solver_new();
	if (!solver) {
		return 0;
	}
	solver_set_seed(solver, 1); // get predictable results.
	if (attempts_allowed > 0) {
		solver_set_attempts(solver, attempts_allowed);
	}

	if (attempts_between_restarts > 0) {
		solver_set_attempts_between_restarts(solver, attempts_between_restarts);
	}

	// Create instance and feed it to solver.
	create_instance(suffix_hash, solver, clauses_count, fixed_bits_count, fixed_bits);

	// find solution if we can.
	r = find_answer(suffix, suffix_size, answer, solution_hash, milliseconds_allowance, target, solver, first_result_ms);

	solver_delete(solver);
	return r;
} /* evpow_solve */

int
evpow_check( uint8_t* suffix
           , size_t suffix_size
           , uint8_t* answer
           , uint8_t* hash_to_compare
	   , int clauses_count
	   , uint8_t* target
           ) {
	SHA256_CTX partial_ctx, full_ctx, ctx_after_hash;
	uint8_t hash[SHA256_DIGEST_LENGTH];
	int clause;
	// fastest first - rarity compliance check.
	//printf("checking rareness\n");
	if (!under_complexity_threshold(hash_to_compare, target)) {
		printf("NOT RARE ENOUGH!\n");
		return 0;
	}
	SHA256_Init(&full_ctx);
	SHA256_Update(&full_ctx, answer, EVPOW_ANSWER_BYTES);
	SHA256_Update(&full_ctx, suffix, suffix_size);
	SHA256_Final(hash, &full_ctx);
	// second fastest second - hashes are equal.
	if (0 != memcmp(hash, hash_to_compare, SHA256_DIGEST_LENGTH)) {
		return 0;
	}
	// slowest one last - does answer really answer the puzzle?
	SHA256_Init(&partial_ctx);
	SHA256_Update(&partial_ctx, suffix, suffix_size);
	SHA256_Final(hash, &partial_ctx);
	SHA256_Init(&ctx_after_hash);
	SHA256_Update(&ctx_after_hash, hash, SHA256_DIGEST_LENGTH);
	for (clause = 0; clause < clauses_count; clause ++) {
		int literals[EVPOW_K];
		int literal_index;
		create_clause(&ctx_after_hash, clause, literals, clause == clauses_count - 1);
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
			return 0;
		}
	}
	return 1;
} /* evpow_check */

