/* Prototype for PoW using random kSAT problem.
 *
 */

#ifndef __ERGVEIN_POW_H
#define __ERGVEIN_POW_H

#include <stdlib.h>
#include <stdint.h>

/* bits per puzzle answer, also hints at number of variables used */
#define	EVPOW_ANSWER_BITS (512)

/* bytes per puzzle answer */
#define EVPOW_ANSWER_BYTES (EVPOW_ANSWER_BITS / 8)

typedef uint8_t evpow_answer[EVPOW_ANSWER_BYTES];

/* number of literals per clause */
#define EVPOW_K (4)

/* number of clauses per puzzle, depends on EVPOW_K */
#if EVPOW_K == 4
#define EVPOW_CLAUSES_COUNT (((990)*EVPOW_ANSWER_BITS + 999)/1000) /* it is about 9.9 for 4SAT */
#else
#   error "cannot compute number of clauses for current EVPOW_K"
#endif

#define EVPOW_HASH_TYPE_SHA256

#if defined(EVPOW_HASH_TYPE_SHA256)
#   define EVPOW_HASH_BYTES	(32)
#else
#   error "at least one type of hashing must be defined."
#endif

typedef uint8_t evpow_hash [EVPOW_HASH_BYTES];


/**
 * The puzzle solving process.
 *
 * The prefix argument is a header data before answer. The hash of that data will be used
 * to deterministically calculate kSAT problem of predetermined size.
 *
 * That kSAT problem should be solved and solution provides an answer to a puzzle, laid out
 * as least-significant-bit-least-significant-byte first. It will be put into an answer.
 *
 * Then, the hash of byte-concatenated prefix and answer is computed and is compared to
 * complexity threshold (again, hash is treated as little endian integer value).
 *
 * The result of evpow_solve() is non-zero if answer is found (then that answer is in
 * buffer of namesake argument).
 *
 * Typical code may look like the following:
 *
 * typedef struct header_s {
 *   sha256d  previous_block;
 *   sha256d  root;
 *   int      complexity_shift;
 *   uint16_t complexity_mantissa;
 * } header_t;
 * typedef struct solved_header_s {
 *   header_t header;
 *   uint8_t  answer[EVPOW_ANSWER_BYTES];
 *   sha256d  solution_hash;
 * } solved_header_t;
 *
 * ...
 *    solved_header_t solved_header;
 *    while (???) {
 *      construct_block_header(&solved_header.header);
 *      if (evpow_solve(&solved_header.header, sizeof(solved_header.header), solved_header.answer, solved_header.solution_hash, solved_header.complexity_shift, solved_header.complexity.mantissa)) {
 *        break;
 *      }
 *    }
 */
int
evpow_solve( uint8_t* prefix
	   , size_t prefix_size
	   , uint8_t* answer
	   , uint8_t* solution_hash
	   , int complexity_shift
	   , uint16_t complexity_mantissa
	   , int32_t milliseconds_allowance // zero or negative value means exhaustive search within attempts limit
	   , int32_t attempts_allowed // zero or negative value means exhaustive search within time limit
	   );


#endif /* __ERGVEIN_POW_H */

