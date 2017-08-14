{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Internal.Types
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Sparse.Internal.Types
  where

-- other
import Prelude                                            hiding ( Either(..) )
import Foreign.Ptr

#include "cbits/stubs.h"
{# context lib="cusparse" #}


-- | This type indicates whether the operation is performed only on indices
-- ('Symbolic') or on data and indices ('Numeric').
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparseactiont>
--
{# enum cusparseAction_t as Action
  { underscoreToCase }
  with prefix="CUSPARSE_ACTION" deriving (Eq, Show) #}

-- | Indicates the underlying storage model for elements of matrices.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsedirectiont>
--
{# enum cusparseDirection_t as Direction
  { underscoreToCase }
  with prefix="CUSPARSE_DIRECTION" deriving (Eq, Show) #}

-- | Indicates which operations need to be performed with the sparse matrix.
--
--   * @N@: no transpose selected
--   * @T@: transpose operation
--   * @C@: conjugate transpose
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparseoperationt>
--
{# enum cusparseOperation_t as Operation
  { CUSPARSE_OPERATION_NON_TRANSPOSE       as N
  , CUSPARSE_OPERATION_TRANSPOSE           as T
  , CUSPARSE_OPERATION_CONJUGATE_TRANSPOSE as C
  }
  deriving (Eq, Show) #}

-- | Used to specify the type of data underlying a @void*@ pointer. For example,
-- it is used in the routine <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-csrmvEx csrmvEx>.
--
#if CUDA_VERSION < 8000
data Type
#else
{# enum cudaDataType_t as Type
  { underscoreToCase }
  with prefix="CUDA" deriving (Eq, Show) #}
#endif


-- | Specify the algorithm to use, for example used in the routine
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-csrmvEx csrmvEx>.
--
#if CUDA_VERSION < 8000
data Algorithm
#else
{# enum cusparseAlgMode_t as Algorithm
  { underscoreToCase }
  with prefix="CUSPARSE" deriving (Eq, Show) #}
#endif

-- | Indicates whether level information is used by some solver algorithms.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsesolvepolicy_t>
--
{# enum cusparseSolvePolicy_t as Policy
  { underscoreToCase }
  with prefix="CUSPARSE_SOLVE_POLICY" deriving (Eq, Show) #}

