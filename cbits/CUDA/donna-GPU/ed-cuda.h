#ifndef __ED_CUDA_H__
#define __ED_CUDA_H__

#if defined(__CUDA_ARCH__)
#   define EDHOST   __host__
#   define EDDEVICE __device__
#   define EDCONSTANT __constant__
#   define EDSYNCHRONIZE __syncthreads()
#else
#   define EDHOST
#   define EDDEVICE
#   define EDCONSTANT
#   define EDSYNCHRONIZE ((void)0)
#endif

#if defined(__CUDACC__)
#   define EDKERNEL __global__
#else
#   define EDKERNEL
#endif

#define EDHOSTDEVICE EDHOST EDDEVICE

#define CUCHK(call) do { if (call) { printf("CUDA fail: call %s at %s:%d\nerror: %s\n", #call, __FILE__, __LINE__, cudaGetErrorString(cudaPeekAtLastError())); } } while(0)

#endif /* __ED_CUDA_H__ */

