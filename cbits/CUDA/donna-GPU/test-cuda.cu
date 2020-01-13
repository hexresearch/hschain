/*
	Validate ed25519 implementation against the official test vectors from 
	http://ed25519.cr.yp.to/software.html
*/

#include <stdio.h>
#include <string.h>
#include "ed25519.h"

#include "test-ticks.h"

#include "ed25519.cu"

static void
edassert(int check, int round, const char *failreason) {
	if (check)
		return;
	printf("round %d, %s\n", round, failreason);
	exit(1);
}

static void
edassert_die(const unsigned char *a, const unsigned char *b, size_t len, int round, const char *failreason) {
	size_t i;
	if (round > 0)
		printf("round %d, %s\n", round, failreason);
	else
		printf("%s\n", failreason);
	printf("want: "); for (i = 0; i < len; i++) printf("%02x,", a[i]); printf("\n");
	printf("got : "); for (i = 0; i < len; i++) printf("%02x,", b[i]); printf("\n");
	printf("diff: "); for (i = 0; i < len; i++) if (a[i] ^ b[i]) printf("%02x,", a[i] ^ b[i]); else printf("  ,"); printf("\n\n");
	exit(1);
}

static void
edassert_equal(const unsigned char *a, const unsigned char *b, size_t len, const char *failreason) {
	if (memcmp(a, b, len) == 0)
		return;
	edassert_die(a, b, len, -1, failreason);
}

static void
edassert_equal_round(const unsigned char *a, const unsigned char *b, size_t len, int round, const char *failreason) {
	if (memcmp(a, b, len) == 0)
		return;
	edassert_die(a, b, len, round, failreason);
}


/* test data */
typedef struct test_data_t {
	unsigned char sk[32], pk[32], sig[64];
	char m[1024];
} test_data;


test_data dataset[] = {
#include "regression.h"
};

/* test data with much shorter messages */
#define MIN_MESSAGE_LEN (135)
#define MAX_MESSAGE_LEN (144)
typedef struct short_test_data_t {
	unsigned char sk[32], pk[32], sig[64];
	char m[((MAX_MESSAGE_LEN + 16 - 1)/16) * 16];
} short_test_data;

#define MESSAGES_UNDER_TEST (16384)
int interesting_dataset_msglen[MESSAGES_UNDER_TEST];
short_test_data interesting_dataset[MESSAGES_UNDER_TEST];

/* result of the curve25519 scalarmult ((|255| * basepoint) * basepoint)... 1024 times */
const curved25519_key curved25519_expected = {
	0xac,0xce,0x24,0xb1,0xd4,0xa2,0x36,0x21,
	0x15,0xe2,0x3e,0x84,0x3c,0x23,0x2b,0x5f,
	0x95,0x6c,0xc0,0x7b,0x95,0x82,0xd7,0x93,
	0xd5,0x19,0xb6,0xf1,0xfb,0x96,0xd6,0x04
};


/* from ed25519-donna-batchverify.h */
extern unsigned char batch_point_buffer[3][32];

/* y coordinate of the final point from 'amd64-51-30k' with the same random generator */
static const unsigned char batch_verify_y[32] = {
	0x51,0xe7,0x68,0xe0,0xf7,0xa1,0x88,0x45,
	0xde,0xa1,0xcb,0xd9,0x37,0xd4,0x78,0x53,
	0x1b,0x95,0xdb,0xbe,0x66,0x59,0x29,0x3b,
	0x94,0x51,0x2f,0xbc,0x0d,0x66,0xba,0x3f
};

/*
static const unsigned char batch_verify_y[32] = {
	0x5c,0x63,0x96,0x26,0xca,0xfe,0xfd,0xc4,
	0x2d,0x11,0xa8,0xe4,0xc4,0x46,0x42,0x97,
	0x97,0x92,0xbe,0xe0,0x3c,0xef,0x96,0x01,
	0x50,0xa1,0xcc,0x8f,0x50,0x85,0x76,0x7d
};

Introducing the 128 bit r scalars to the heap _before_ the largest scalar
fits in to 128 bits alters the heap shape and produces a different,
yet still neutral/valid y/z value.

This was the value of introducing the r scalars when the largest scalar fit
in to 135-256 bits. You can produce it with amd64-64-24k / amd64-51-32k
with the random sequence used in the first pass by changing

    unsigned long long hlen=((npoints+1)/2)|1;

to

    unsigned long long hlen=npoints;

in ge25519_multi_scalarmult.c

ed25519-donna-batchverify.h has been modified to match the 
default amd64-64-24k / amd64-51-32k behaviour
*/



/* batch test */
#define test_batch_count 64
#define test_batch_rounds 96

typedef enum batch_test_t {
	batch_no_errors = 0,
	batch_wrong_message = 1,
	batch_wrong_pk = 2,
	batch_wrong_sig = 3
} batch_test;

void EDKERNEL
ed25519_sign_open_kernel(short_test_data* data, int* msg_size, unsigned char* result, int N) {
	int i = threadIdx.x + blockIdx.x * blockDim.x;
	int r;
	if (i>= N) {
		return ;
	}
#if 0
	if (i < 2) {
# define PP(bb) printf("%d: " #bb " xyz: %d %d %d\n", i, bb.x, bb.y, bb.z)
		PP(threadIdx);
		PP(blockIdx);
		PP(blockDim);
	}
#endif
	//printf("GPU at %d\n", i);
	r = ed25519_sign_open((unsigned char*)data[i].m, msg_size[i], data[i].pk, data[i].sig);
	//printf("GPU result at %d is %d\n", i, r);
	result[i] = r;
}

static void
test_main_CUDA(void) {
	int i, j, N = 5, res;
	ed25519_public_key pk;
	ed25519_signature sig;
	unsigned char forge[1024] = {'x'};
	unsigned char results[MESSAGES_UNDER_TEST], *gpu_results;
	int* lengths;
	short_test_data* gpu_test_data;
	curved25519_key csk[2] = {{255}};
	uint64_t ticks, pkticks = maxticks, signticks = maxticks, openticks = maxticks, curvedticks = maxticks;

	for (i=0, j = MIN_MESSAGE_LEN;i<MESSAGES_UNDER_TEST; i++) {
		memcpy(&interesting_dataset[i], &dataset[j], sizeof(interesting_dataset[i]));
		interesting_dataset_msglen[i] = j;
		j ++;
		if (j > MAX_MESSAGE_LEN) {
			j = MIN_MESSAGE_LEN;
		}
	}
	ticks = get_ticks();
	for (j=0;j<N;j++) {
		for (i = 0; i < MESSAGES_UNDER_TEST; i++) {
			results[i] = ed25519_sign_open((unsigned char *)interesting_dataset[i].m, interesting_dataset_msglen[i], interesting_dataset[i].pk, interesting_dataset[i].sig);
		}
	}
	ticks = get_ticks() - ticks;
	printf("%.0f ticks to verify %d * %d signatures on CPU, data set size %zu, one message time %.0f\n", (double)ticks, MESSAGES_UNDER_TEST, N, sizeof(interesting_dataset), ((double)ticks)/(N * MESSAGES_UNDER_TEST));

	int block_size = 256;
        int num_blocks = (MESSAGES_UNDER_TEST + (block_size) - 1) / (block_size);
	CUCHK(cudaMallocManaged(&lengths, sizeof(*lengths) * MESSAGES_UNDER_TEST));
	memcpy(lengths, interesting_dataset_msglen, sizeof(*lengths) * MESSAGES_UNDER_TEST);

	CUCHK(cudaMallocManaged(&gpu_test_data, sizeof(interesting_dataset)));
	CUCHK(cudaMallocManaged(&gpu_results, sizeof(*gpu_results) * MESSAGES_UNDER_TEST));
	memset(gpu_results, 111, sizeof(*gpu_results) * MESSAGES_UNDER_TEST);
	ticks = get_ticks();
	for (j=0;j<N;j++) {
		//CUCHK(cudaMemcpy(gpu_test_data, interesting_dataset, sizeof(interesting_dataset), cudaMemcpyHostToDevice));
		memcpy(gpu_test_data, interesting_dataset, sizeof(interesting_dataset));
	        ed25519_sign_open_kernel<<<num_blocks,block_size>>>(gpu_test_data, lengths, gpu_results, MESSAGES_UNDER_TEST);
		CUCHK(cudaPeekAtLastError());
		CUCHK(cudaDeviceSynchronize());
		for (i = 0;i < MESSAGES_UNDER_TEST;i ++) {
			if (gpu_results[i] != results[i]) {
				printf("difference %d/%d at %d\n", gpu_results[i], results[i], i);
				break;
			}
		}
	}
	ticks = get_ticks() - ticks;
	printf("%.0f ticks to verify %d * %d signatures on GPU, %.0f ticks per message\n", (double)ticks, MESSAGES_UNDER_TEST, N, ((double)ticks)/(MESSAGES_UNDER_TEST * N));

}

int
main(void) {
	test_main_CUDA();
	return 0;
}

