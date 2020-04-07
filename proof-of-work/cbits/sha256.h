/*********************************************************************
* Filename:   sha256.h
* Author:     Brad Conte (brad AT bradconte.com)
* Copyright:
* Disclaimer: This code is presented "as is" without any guarantees.
* Details:    Defines the API for the corresponding SHA1 implementation.
*********************************************************************/

#ifndef __SHA256_H
#define __SHA256_H

/*************************** HEADER FILES ***************************/
#include <stddef.h>
#include <stdint.h>

/****************************** MACROS ******************************/
#define SHA256_BLOCK_SIZE 32            // SHA256 outputs a 32 byte digest
#define SHA256_DIGEST_LENGTH (SHA256_BLOCK_SIZE)

/**************************** DATA TYPES ****************************/
typedef uint8_t   BYTE;                 // 8-bit byte
typedef uint32_t  WORD;                 // 32-bit word, change to "long" for 16-bit machines

typedef struct {
	BYTE data[64];
	WORD datalen;
	unsigned long long bitlen;
	WORD state[8];
} SHA256_CTX;

/*********************** FUNCTION DECLARATIONS **********************/
void SHA256_Init(SHA256_CTX *ctx);
void SHA256_Update(SHA256_CTX *ctx, const BYTE data[], size_t len);
void SHA256_Final(BYTE hash[], SHA256_CTX *ctx);

#endif   // __SHA256_H

