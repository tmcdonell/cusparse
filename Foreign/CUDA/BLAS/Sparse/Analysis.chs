{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Analysis
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Sparse.Analysis
  where

-- Friends
import Foreign.CUDA.BLAS.Sparse.Error
import Foreign.CUDA.BLAS.Sparse.Internal.C2HS

-- System
import Foreign
import Foreign.C
import Control.Monad                                      ( liftM )

#include "cbits/stubs.h"
{# context lib="cusparse" #}


-- | An opaque structure holding the information collected in the analysis phase
-- of the solution of the sparse triangular linear system.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsesolveanalysisinfot>
--
newtype Info = Info { useInfo :: {# type cusparseSolveAnalysisInfo_t #}}

-- | Create and initialise the solve and analysis structure to default values.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsecreatesolveanalysisinfo>
--
{-# INLINEABLE createInfo #-}
createInfo :: IO Info
createInfo = resultIfOk =<< cusparseCreateSolveAnalysisInfo
  where
    {# fun unsafe cusparseCreateSolveAnalysisInfo
      { alloca- `Info' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info . peek

-- | Release memory associated with a matrix solver structure.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsedestroysolveanalysisinfo>
--
{-# INLINEABLE destroyInfo #-}
{# fun unsafe cusparseDestroySolveAnalysisInfo as destroyInfo
  { useInfo `Info' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#csrsv2infot>
--
newtype Info_csrsv2 = Info_csrsv2 { useInfo_csrsv2 :: {# type csrsv2Info_t #}}

{-# INLINEABLE createInfo_csrsv2 #-}
createInfo_csrsv2 :: IO Info_csrsv2
createInfo_csrsv2 = resultIfOk =<< cusparseCreateCsrsv2Info
  where
    {# fun unsafe cusparseCreateCsrsv2Info
      { alloca- `Info_csrsv2' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_csrsv2 . peek

{-# INLINEABLE destroyInfo_csrsv2 #-}
{# fun unsafe cusparseDestroyCsrsv2Info as destroyInfo_csrsv2
  { useInfo_csrsv2 `Info_csrsv2' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#csric02infot>
--

-- | <http://docs.nvidia.com/cuda/cusparse/index.html#csrilu02infot>
--

-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsrsv2infot>
--
newtype Info_bsrsv2 = Info_bsrsv2 { useInfo_bsrsv2 :: {# type bsrsv2Info_t #}}

{-# INLINEABLE createInfo_bsrsv2 #-}
createInfo_bsrsv2 :: IO Info_bsrsv2
createInfo_bsrsv2 = resultIfOk =<< cusparseCreateBsrsv2Info
  where
    {# fun unsafe cusparseCreateBsrsv2Info
      { alloca- `Info_bsrsv2' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_bsrsv2 . peek

{-# INLINEABLE destroyInfo_bsrsv2 #-}
{# fun unsafe cusparseDestroyBsrsv2Info as destroyInfo_bsrsv2
  { useInfo_bsrsv2 `Info_bsrsv2' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsrsvminfot>
--
newtype Info_bsrsm2 = Info_bsrsm2 { useInfo_bsrsm2 :: {# type bsrsm2Info_t #}}

{-# INLINEABLE createInfo_bsrsm2 #-}
createInfo_bsrsm2 :: IO Info_bsrsm2
createInfo_bsrsm2 = resultIfOk =<< cusparseCreateBsrsm2Info
  where
    {# fun unsafe cusparseCreateBsrsm2Info
      { alloca- `Info_bsrsm2' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_bsrsm2 . peek

{-# INLINEABLE destroyInfo_bsrsm2 #-}
{# fun unsafe cusparseDestroyBsrsm2Info as destroyInfo_bsrsm2
  { useInfo_bsrsm2 `Info_bsrsm2' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsric02infot>
--

-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsrilu02infot>
--

-- | <http://docs.nvidia.com/cuda/cusparse/index.html#csrgemm2infot>
--
#if CUDA_VERSION >= 7000
newtype Info_csrgemm2 = Info_csrgemm2 { useInfo_csrgemm2 :: {# type csrgemm2Info_t #}}

{-# INLINEABLE createInfo_csrgemm2 #-}
createInfo_csrgemm2 :: IO Info_csrgemm2
createInfo_csrgemm2 = resultIfOk =<< cusparseCreateCsrgemm2Info
  where
    {# fun unsafe cusparseCreateCsrgemm2Info
      { alloca- `Info_csrgemm2' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_csrgemm2 . peek

{-# INLINEABLE destroyInfo_csrgemm2 #-}
{# fun unsafe cusparseDestroyCsrgemm2Info as destroyInfo_csrgemm2
  { useInfo_csrgemm2 `Info_csrgemm2' } -> `()' checkStatus* #}

#else
data Info_csrgemm2

createInfo_csrgemm2 :: IO Info_csrgemm2
createInfo_csrgemm2 = cusparseError "'createInfo_csrgemm2' requires at least cuda-7.0"

destroyInfo_csrgemm2 :: Info_csrgemm2 -> IO ()
destroyInfo_csrgemm2 _ = cusparseError "'destroyInfo_csrgemm2' requires at least cuda-7.0"

#endif

