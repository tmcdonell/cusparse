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

module Foreign.CUDA.BLAS.Sparse.Analysis (

  Info(..), createInfo, destroyInfo,
  Info_csrsv2(..), createInfo_csrsv2, destroyInfo_csrsv2,
  Info_csric02(..), createInfo_csric02, destroyInfo_csric02,
  Info_csrilu02(..), createInfo_csrilu02, destroyInfo_csrilu02,
  Info_bsrsv2(..), createInfo_bsrsv2, destroyInfo_bsrsv2,
  Info_bsrsm2(..), createInfo_bsrsm2, destroyInfo_bsrsm2,
  Info_bsric02(..), createInfo_bsric02, destroyInfo_bsric02,
  Info_bsrilu02(..), createInfo_bsrilu02, destroyInfo_bsrilu02,
  Info_csrgemm2(..), createInfo_csrgemm2, destroyInfo_csrgemm2,
  Info_colour(..), createInfo_colour, destroyInfo_colour,

) where

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
newtype Info_csric02 = Info_csric02 { useInfo_csric02 :: {# type csric02Info_t #}}

{-# INLINEABLE createInfo_csric02 #-}
createInfo_csric02 :: IO Info_csric02
createInfo_csric02 = resultIfOk =<< cusparseCreateCsric02Info
  where
    {# fun unsafe cusparseCreateCsric02Info
      { alloca- `Info_csric02' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_csric02 . peek

{-# INLINEABLE destroyInfo_csric02 #-}
{# fun unsafe cusparseDestroyCsric02Info as destroyInfo_csric02
  { useInfo_csric02 `Info_csric02' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#csrilu02infot>
--
newtype Info_csrilu02 = Info_csrilu02 { useInfo_csrilu02 :: {# type csrilu02Info_t #}}

{-# INLINEABLE createInfo_csrilu02 #-}
createInfo_csrilu02 :: IO Info_csrilu02
createInfo_csrilu02 = resultIfOk =<< cusparseCreateCsrilu02Info
  where
    {# fun unsafe cusparseCreateCsrilu02Info
      { alloca- `Info_csrilu02' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_csrilu02 . peek

{-# INLINEABLE destroyInfo_csrilu02 #-}
{# fun unsafe cusparseDestroyCsrilu02Info as destroyInfo_csrilu02
  { useInfo_csrilu02 `Info_csrilu02' } -> `()' checkStatus* #}


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
newtype Info_bsric02 = Info_bsric02 { useInfo_bsric02 :: {# type bsric02Info_t #}}

{-# INLINEABLE createInfo_bsric02 #-}
createInfo_bsric02 :: IO Info_bsric02
createInfo_bsric02 = resultIfOk =<< cusparseCreateBsric02Info
  where
    {# fun unsafe cusparseCreateBsric02Info
      { alloca- `Info_bsric02' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_bsric02 . peek

{-# INLINEABLE destroyInfo_bsric02 #-}
{# fun unsafe cusparseDestroyBsric02Info as destroyInfo_bsric02
  { useInfo_bsric02 `Info_bsric02' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsrilu02infot>
--
newtype Info_bsrilu02 = Info_bsrilu02 { useInfo_bsrilu02 :: {# type bsrilu02Info_t #}}

{-# INLINEABLE createInfo_bsrilu02 #-}
createInfo_bsrilu02 :: IO Info_bsrilu02
createInfo_bsrilu02 = resultIfOk =<< cusparseCreateBsrilu02Info
  where
    {# fun unsafe cusparseCreateBsrilu02Info
      { alloca- `Info_bsrilu02' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_bsrilu02 . peek

{-# INLINEABLE destroyInfo_bsrilu02 #-}
{# fun unsafe cusparseDestroyBsrilu02Info as destroyInfo_bsrilu02
  { useInfo_bsrilu02 `Info_bsrilu02' } -> `()' checkStatus* #}


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


-- /undocumented/
--
newtype Info_colour = Info_colour { useInfo_colour :: {# type cusparseColorInfo_t #}}

{-# INLINEABLE createInfo_colour #-}
createInfo_colour :: IO Info_colour
createInfo_colour = resultIfOk =<< cusparseCreateColorInfo
  where
    {# fun unsafe cusparseCreateColorInfo
      { alloca- `Info_colour' peekI* } -> `Status' cToEnum #}
      where
        peekI = liftM Info_colour . peek

{-# INLINEABLE destroyInfo_colour #-}
{# fun unsafe cusparseDestroyColorInfo as destroyInfo_colour
  { useInfo_colour `Info_colour' } -> `()' checkStatus* #}

