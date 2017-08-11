{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Matrix.Hybrid
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Sparse.Matrix.Hybrid (

  -- * Hybrid matrices
  Hybrid(..),
  HybridPartition(..),

  -- ** Creation
  create,
  destroy,

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


-- | An opaque structure holding the matrix in hybrid (HYB) format.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsehybmatt>
--
newtype Hybrid = Hybrid { useHYB :: {# type cusparseHybMat_t #}}

-- | Indicates how to perform the partitioning of the matrix into regular (ELL)
-- and irregular (COO) parts of the HYB format.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsehybpartitiont>
--
{# enum cusparseHybPartition_t as HybridPartition
  { underscoreToCase }
  with prefix="CUSPARSE_HYB_PARTITION" deriving (Eq, Show) #}


-- | Create a new (opaque) hybrid matrix.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsecreatehybmat>
--
{-# INLINEABLE create #-}
create :: IO Hybrid
create = resultIfOk =<< cusparseCreateHybMat
  where
    {# fun unsafe cusparseCreateHybMat
      { alloca- `Hybrid' peekHYB* } -> `Status' cToEnum #}
      where
        peekHYB = liftM Hybrid . peek


-- | Destroy and release any memory associated with a hybrid matrix.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsedestroyhybmat>
--
{-# INLINEABLE destroy #-}
{# fun unsafe cusparseDestroyHybMat as destroy
  { useHYB `Hybrid' } -> `()' checkStatus* #}

