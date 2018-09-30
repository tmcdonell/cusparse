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
  Info_csrsm2(..), createInfo_csrsm2, destroyInfo_csrsm2,
  Info_color(..), createInfo_color, destroyInfo_color,
  Info_csru2csr(..), createInfo_csru2csr, destroyInfo_csru2csr,
  Info_prune(..), createInfo_prune, destroyInfo_prune,

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
{# fun unsafe cusparseCreateSolveAnalysisInfo as createInfo
  { alloca- `Info' peekI* } -> `()' checkStatus*- #}
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
{# fun unsafe cusparseCreateCsrsv2Info as createInfo_csrsv2
  { alloca- `Info_csrsv2' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_csrsv2 . peek

{-# INLINEABLE destroyInfo_csrsv2 #-}
{# fun unsafe cusparseDestroyCsrsv2Info as destroyInfo_csrsv2
  { useInfo_csrsv2 `Info_csrsv2' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#csric02infot>
--
newtype Info_csric02 = Info_csric02 { useInfo_csric02 :: {# type csric02Info_t #}}

{-# INLINEABLE createInfo_csric02 #-}
{# fun unsafe cusparseCreateCsric02Info as createInfo_csric02
  { alloca- `Info_csric02' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_csric02 . peek

{-# INLINEABLE destroyInfo_csric02 #-}
{# fun unsafe cusparseDestroyCsric02Info as destroyInfo_csric02
  { useInfo_csric02 `Info_csric02' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#csrilu02infot>
--
newtype Info_csrilu02 = Info_csrilu02 { useInfo_csrilu02 :: {# type csrilu02Info_t #}}

{-# INLINEABLE createInfo_csrilu02 #-}
{# fun unsafe cusparseCreateCsrilu02Info as createInfo_csrilu02
  { alloca- `Info_csrilu02' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_csrilu02 . peek

{-# INLINEABLE destroyInfo_csrilu02 #-}
{# fun unsafe cusparseDestroyCsrilu02Info as destroyInfo_csrilu02
  { useInfo_csrilu02 `Info_csrilu02' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsrsv2infot>
--
newtype Info_bsrsv2 = Info_bsrsv2 { useInfo_bsrsv2 :: {# type bsrsv2Info_t #}}

{-# INLINEABLE createInfo_bsrsv2 #-}
{# fun unsafe cusparseCreateBsrsv2Info as createInfo_bsrsv2
  { alloca- `Info_bsrsv2' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_bsrsv2 . peek

{-# INLINEABLE destroyInfo_bsrsv2 #-}
{# fun unsafe cusparseDestroyBsrsv2Info as destroyInfo_bsrsv2
  { useInfo_bsrsv2 `Info_bsrsv2' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsrsvminfot>
--
newtype Info_bsrsm2 = Info_bsrsm2 { useInfo_bsrsm2 :: {# type bsrsm2Info_t #}}

{-# INLINEABLE createInfo_bsrsm2 #-}
{# fun unsafe cusparseCreateBsrsm2Info as createInfo_bsrsm2
  { alloca- `Info_bsrsm2' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_bsrsm2 . peek

{-# INLINEABLE destroyInfo_bsrsm2 #-}
{# fun unsafe cusparseDestroyBsrsm2Info as destroyInfo_bsrsm2
  { useInfo_bsrsm2 `Info_bsrsm2' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsric02infot>
--
newtype Info_bsric02 = Info_bsric02 { useInfo_bsric02 :: {# type bsric02Info_t #}}

{-# INLINEABLE createInfo_bsric02 #-}
{# fun unsafe cusparseCreateBsric02Info as createInfo_bsric02
  { alloca- `Info_bsric02' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_bsric02 . peek

{-# INLINEABLE destroyInfo_bsric02 #-}
{# fun unsafe cusparseDestroyBsric02Info as destroyInfo_bsric02
  { useInfo_bsric02 `Info_bsric02' } -> `()' checkStatus* #}


-- | <http://docs.nvidia.com/cuda/cusparse/index.html#bsrilu02infot>
--
newtype Info_bsrilu02 = Info_bsrilu02 { useInfo_bsrilu02 :: {# type bsrilu02Info_t #}}

{-# INLINEABLE createInfo_bsrilu02 #-}
{# fun unsafe cusparseCreateBsrilu02Info as createInfo_bsrilu02
  { alloca- `Info_bsrilu02' peekI* } -> `()' checkStatus*- #}
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
{# fun unsafe cusparseCreateCsrgemm2Info as createInfo_csrgemm2
  { alloca- `Info_csrgemm2' peekI* } -> `()' checkStatus*- #}
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


-- | <https://docs.nvidia.com/cuda/cusparse/index.html#csrsm2infot>
--
#if CUDA_VERSION >= 9020
newtype Info_csrsm2 = Info_csrsm2 { useInfo_csrsm2 :: {# type csrsm2Info_t #}}

{-# INLINEABLE createInfo_csrsm2 #-}
{# fun unsafe cusparseCreateCsrsm2Info as createInfo_csrsm2
  { alloca- `Info_csrsm2' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_csrsm2 . peek

{-# INLINE destroyInfo_csrsm2 #-}
{# fun unsafe cusparseDestroyCsrsm2Info as destroyInfo_csrsm2
  { useInfo_csrsm2 `Info_csrsm2' } -> `()' checkStatus*- #}

#else
data Info_csrsm2

createInfo_csrsm2 :: IO Info_csrsm2
createInfo_csrsm2 = cusparseError "'createInfo_csrsm2 requires at least cuda-9.2"

destroyInfo_csrsm2 :: Info_csrsm2 -> IO ()
destroyInfo_csrsm2 _ = cusparseError "'destroyInfo_csrsm2 requires at least cuda-9.2"

#endif



-- /undocumented/
--
#if CUDA_VERSION >= 7000
newtype Info_color = Info_color { useInfo_color :: {# type cusparseColorInfo_t #}}

{-# INLINEABLE createInfo_color #-}
{# fun unsafe cusparseCreateColorInfo as createInfo_color
  { alloca- `Info_color' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_color . peek

{-# INLINEABLE destroyInfo_color #-}
{# fun unsafe cusparseDestroyColorInfo as destroyInfo_color
  { useInfo_color `Info_color' } -> `()' checkStatus* #}

#else
data Info_color

createInfo_color :: IO Info_color
createInfo_color = cusparseError "'createInfo_color' requires at least cuda-7.0"

destroyInfo_color :: Info_color -> IO ()
destroyInfo_color _ = cusparseError "'destroyInfo_color' requires at least cuda-7.0"

#endif


-- /undocumented/
--
#if CUDA_VERSION >= 7000
newtype Info_csru2csr = Info_csru2csr { useInfo_csru2csr :: {# type csru2csrInfo_t #}}

{-# INLINEABLE createInfo_csru2csr #-}
{# fun unsafe cusparseCreateCsru2csrInfo as createInfo_csru2csr
  { alloca- `Info_csru2csr' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_csru2csr . peek

{-# INLINEABLE destroyInfo_csru2csr #-}
{# fun unsafe cusparseDestroyCsru2csrInfo as destroyInfo_csru2csr
  { useInfo_csru2csr `Info_csru2csr' } -> `()' checkStatus* #}

#else
data Info_csru2csr

createInfo_csru2csr :: IO Info_csru2csr
createInfo_csru2csr = cusparseError "'createInfo_csru2csr' requires at least cuda-7.0"

destroyInfo_csru2csr :: Info_csru2csr -> IO ()
destroyInfo_csru2csr _ = cusparseError "'destroyInfo_csru2csr' requires at least cuda-7.0"

#endif


-- /undocumented/
--
#if CUDA_VERSION >= 9000
newtype Info_prune = Info_prune { useInfo_prune :: {# type pruneInfo_t #}}

{-# INLINEABLE createInfo_prune #-}
{# fun unsafe cusparseCreatePruneInfo as createInfo_prune
  { alloca- `Info_prune' peekI* } -> `()' checkStatus*- #}
  where
    peekI = liftM Info_prune . peek

{-# INLINEABLE destroyInfo_prune #-}
{# fun unsafe cusparseDestroyPruneInfo as destroyInfo_prune
  { useInfo_prune `Info_prune' } -> `()' checkStatus* #}

#else
data Info_prune

createInfo_prune :: IO Info_prune
createInfo_prune = cusparseError "'createInfo_prune' requires at least cuda-8.0"

destroyInfo_prune :: Info_prune -> IO ()
destroyInfo_prune _ = cusparseError "'destroyInfo_prune' requires at least cuda-8.0"

#endif

