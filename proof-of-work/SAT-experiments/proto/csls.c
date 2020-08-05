#include <stdio.h>
#include <stdlib.h>

#include "csls.h"

#define VARS_MAX (512)

typedef struct clause_s clause;
struct csls_s {
	// building a clause.
	int16_t     clause_temp[VARS_MAX];
	int16_t     clause_temp_lit_count;
        // literals - 
};

typedef struct clause_literal_s {
} clause_literal;

struct clause_s {
	uint16_t     literals_count;
	int16_t*     literals;         // sorted by variable's index for speedier search.
	clause**     next_for_literal; // each clause is in lists for each literal in it.
	clause**     prev_for_literal;
	clause*      next_queue;       // each clause can be in the queue of unsatified clauses for BFS search.
	clause*      prev_queue;
};

csls* csls_init(int max_var, int max_clauses) {
	if (max_var > VARS_MAX) {
		printf("too many variables\n");
		exit(1);
	}
} /* csls_init */
void csls_setmaxflips(csls*solver, uint64_t maxflips);
void csls_add(csls*solver, int literal);
int  csls_sat(csls*solver);


