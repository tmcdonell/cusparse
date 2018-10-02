/*
 * Extra bits for cuSPARSE bindings
 */

#ifndef C_STUBS_H
#define C_STUBS_H

#ifdef __MINGW32__
#include <host_defines.h>
#undef CUDARTAPI
#define CUDARTAPI __stdcall
#endif

#include <cuda.h>
#include <cusparse_v2.h>

/*
 * We need to redeclare these functions, as they are now hidden behind a #if
 * defined(__cplusplus) guard.
 */
#if CUDA_VERSION >= 9010
typedef struct __align__(2) {
   unsigned short x;
} __half;

cusparseStatus_t CUSPARSEAPI cusparseHpruneDense2csr_bufferSizeExt(
    cusparseHandle_t handle,
    int m,
    int n,
    const __half *A,
    int lda,
    const __half *threshold,
    const cusparseMatDescr_t descrC,
    const __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    const int *csrSortedColIndC,
    size_t *pBufferSizeInBytes);

cusparseStatus_t CUSPARSEAPI cusparseHpruneDense2csrNnz(
    cusparseHandle_t handle,
    int m,
    int n,
    const __half *A,
    int lda,
    const __half *threshold,
    const cusparseMatDescr_t descrC,
    int *csrRowPtrC,
    int *nnzTotalDevHostPtr,
    void *pBuffer);

cusparseStatus_t CUSPARSEAPI cusparseHpruneDense2csr(
    cusparseHandle_t handle,
    int m,
    int n,
    const __half *A,
    int lda,
    const __half *threshold,
    const cusparseMatDescr_t descrC,
    __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    int *csrSortedColIndC,
    void *pBuffer);

cusparseStatus_t CUSPARSEAPI cusparseHpruneCsr2csr_bufferSizeExt(
    cusparseHandle_t handle,
    int m,
    int n,
    int nnzA,
    const cusparseMatDescr_t descrA,
    const __half *csrSortedValA,
    const int *csrSortedRowPtrA,
    const int *csrSortedColIndA,
    const __half *threshold,
    const cusparseMatDescr_t descrC,
    const __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    const int *csrSortedColIndC,
    size_t *pBufferSizeInBytes);

cusparseStatus_t CUSPARSEAPI cusparseHpruneCsr2csrNnz(
    cusparseHandle_t handle,
    int m,
    int n,
    int nnzA,
    const cusparseMatDescr_t descrA,
    const __half *csrSortedValA,
    const int *csrSortedRowPtrA,
    const int *csrSortedColIndA,
    const __half *threshold,
    const cusparseMatDescr_t descrC,
    int *csrSortedRowPtrC,
    int *nnzTotalDevHostPtr, /* can be on host or device */
    void *pBuffer);

cusparseStatus_t CUSPARSEAPI cusparseHpruneCsr2csr(
    cusparseHandle_t handle,
    int m,
    int n,
    int nnzA,
    const cusparseMatDescr_t descrA,
    const __half *csrSortedValA,
    const int *csrSortedRowPtrA,
    const int *csrSortedColIndA,
    const __half *threshold,
    const cusparseMatDescr_t descrC,
    __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    int *csrSortedColIndC,
    void *pBuffer);

cusparseStatus_t CUSPARSEAPI cusparseHpruneDense2csrByPercentage_bufferSizeExt(
    cusparseHandle_t handle,
    int m,
    int n,
    const __half *A,
    int lda,
    float percentage, /* between 0 to 100 */
    const cusparseMatDescr_t descrC,
    const __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    const int *csrSortedColIndC,
    pruneInfo_t info,
    size_t *pBufferSizeInBytes);

cusparseStatus_t CUSPARSEAPI cusparseHpruneDense2csrNnzByPercentage(
    cusparseHandle_t handle,
    int m,
    int n,
    const __half *A,
    int lda,
    float percentage, /* between 0 to 100 */
    const cusparseMatDescr_t descrC,
    int *csrRowPtrC,
    int *nnzTotalDevHostPtr, /* can be on host or device */
    pruneInfo_t info,
    void *pBuffer);

cusparseStatus_t CUSPARSEAPI cusparseHpruneDense2csrByPercentage(
    cusparseHandle_t handle,
    int m,
    int n,
    const __half *A,
    int lda,
    float percentage, /* between 0 to 100 */
    const cusparseMatDescr_t descrC,
    __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    int *csrSortedColIndC,
    pruneInfo_t info,
    void *pBuffer);

cusparseStatus_t CUSPARSEAPI cusparseHpruneCsr2csrByPercentage_bufferSizeExt(
    cusparseHandle_t handle,
    int m,
    int n,
    int nnzA,
    const cusparseMatDescr_t descrA,
    const __half *csrSortedValA,
    const int *csrSortedRowPtrA,
    const int *csrSortedColIndA,
    float percentage, /* between 0 to 100 */
    const cusparseMatDescr_t descrC,
    const __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    const int *csrSortedColIndC,
    pruneInfo_t info,
    size_t *pBufferSizeInBytes);

cusparseStatus_t CUSPARSEAPI cusparseHpruneCsr2csrNnzByPercentage(
    cusparseHandle_t handle,
    int m,
    int n,
    int nnzA,
    const cusparseMatDescr_t descrA,
    const __half *csrSortedValA,
    const int *csrSortedRowPtrA,
    const int *csrSortedColIndA,
    float percentage, /* between 0 to 100 */
    const cusparseMatDescr_t descrC,
    int *csrSortedRowPtrC,
    int *nnzTotalDevHostPtr, /* can be on host or device */
    pruneInfo_t info,
    void *pBuffer);

cusparseStatus_t CUSPARSEAPI cusparseHpruneCsr2csrByPercentage(
    cusparseHandle_t handle,
    int m,
    int n,
    int nnzA,
    const cusparseMatDescr_t descrA,
    const __half *csrSortedValA,
    const int *csrSortedRowPtrA,
    const int *csrSortedColIndA,
    float percentage, /* between 0 to 100 */
    const cusparseMatDescr_t descrC,
    __half *csrSortedValC,
    const int *csrSortedRowPtrC,
    int *csrSortedColIndC,
    pruneInfo_t info,
    void *pBuffer);

#endif /* CUDA_VERSION */
#endif /* C_STUBS_H */

