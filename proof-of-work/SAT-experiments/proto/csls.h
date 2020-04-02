/**
 * Compact stochastic local search SAT solver.
 */
#ifndef __CSLS_H
#define __CSLS_H

#include <stdint.h>

typedef struct csls_s csls;

csls* csls_init(int max_var, int max_clauses);
void csls_setmaxflips(csls*solver, uint64_t maxflips);
void csls_add(csls*solver, int literal);
int  csls_sat(csls*solver);

#endif /* __CSLS_H */
