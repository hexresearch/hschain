/* Prototype for PoW using random kSAT problem.
 *
 */

#ifndef __ERGVEIN_POW_H
#define __ERGVEIN_POW_H

#include <stdlib.h>
#include <stdint.h>

/* bits per puzzle answer, also hints at number of variables used */
#define	EVPOW_ANSWER_BITS (256)

#if (EVPOW_ANSWER_BITS) >= 32768
#   error "For such a big answer we have to update inner workings of problem generation (go up from 15 bits per variable index)"
#endif

/* bytes per puzzle answer */
#define EVPOW_ANSWER_BYTES (EVPOW_ANSWER_BITS / 8)

typedef uint8_t evpow_answer[EVPOW_ANSWER_BYTES];

/* number of literals per clause */
#define EVPOW_K (5)

/* number of clauses per puzzle, depends on EVPOW_K */
#if EVPOW_K == 4
#define EVPOW_ADVISED_CLAUSES_COUNT (((1020)*EVPOW_ANSWER_BITS + 99)/100) /* phase transition is at about 9.9 for 4SAT */
#elif EVPOW_K == 5
#define EVPOW_ADVISED_CLAUSES_COUNT (5250) // good complexity, about 3 initial solutions/sec.
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
 * The suffix argument is a header data after answer. The hash of that data will be used
 * to deterministically calculate kSAT problem of predetermined size.
 *
 * That kSAT problem should be solved and solution provides an answer to a puzzle, laid out
 * as least-significant-bit-least-significant-byte first. It will be put into an answer.
 *
 * Then, the hash of byte-concatenated answer and suffix is computed and is compared to
 * complexity threshold (again, hash is treated as little endian integer value).
 *
 * The result of evpow_solve() is non-zero if answer is found (then that answer is in
 * buffer of namesake argument). The @solution_hash@ buffer will always contain smallest
 * hash found during (even unsuccessful) search and can be used as a proxy to work done
 * for optimization purposes.
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
 *   uint8_t  answer[EVPOW_ANSWER_BYTES];
 *   header_t header;
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
evpow_solve( uint8_t* suffix
	   , size_t suffix_size
	   , uint8_t* answer
	   , uint8_t* solution_hash
	   , int clauses_count
	   , uint8_t* target // target, encoded as least-significant-byte first.
	   , int32_t milliseconds_allowance // zero or negative value means exhaustive search within attempts limit
	   , int32_t attempts_allowed // zero or negative value means exhaustive search within time limit
	   , int attempts_between_restarts // if not succeed at this number of flips, restart with new assignment.
	   , int fixed_bits_count     // you may try to solve the puzzle with some bits fixed (up to 64 bits)
	   , uint64_t fixed_bits      // to what value these bits are fixed
	   , double* first_result_ms // report time to obtain first result (SAT/UNK).
	   );

/**
 * Check the valitity of puzzle.
 */
int
evpow_check( uint8_t* prefix
	   , size_t prefix_size
	   , uint8_t* answer
	   , uint8_t* hash_to_compare
	   , int clauses_count
	   , uint8_t* target
	   );

#endif /* __ERGVEIN_POW_H */

