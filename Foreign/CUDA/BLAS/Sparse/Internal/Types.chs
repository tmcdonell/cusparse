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

import Prelude                                            hiding ( Either(..) )
import Foreign.Ptr

#include "cbits/stubs.h"
{# context lib="cusparse" #}


-- | An opaque handle to the cuSPARSE library context, which is passed to all
-- library function calls.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsehandlet>
--
newtype Handle = Handle { useHandle :: {# type cusparseHandle_t #}}


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

-- | For functions which take scalar value arguments, determines whether those
-- values are passed by reference on the host or device.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsepointermode_t>
-- | Used to specify the type of data underlying a @void*@ pointer. For example,
-- it is used in the routine <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-csrmvEx csrmvEx>.
--
{# enum cusparsePointerMode_t as PointerMode
#if CUDA_VERSION < 8000
data Type
#else
{# enum cudaDataType_t as Type
  { underscoreToCase }
  with prefix="CUSPARSE_POINTER_MODE" deriving (Eq, Show) #}
  with prefix="CUDA" deriving (Eq, Show) #}
#endif


-- Matrix formats
-- --------------

-- | An opaque type used to describe the shape and properties of a matrix.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsematdescrt>
--
newtype MatrixDescriptor = MatrixDescriptor { useMatDescr :: {# type cusparseMatDescr_t #}}

-- | An opaque structure holding the matrix in hybrid (HYB) format.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsehybmatt>
--
newtype HYB = HYB { useHYB :: {# type cusparseHybMat_t #}}


-- | Indicates how to perform the partitioning of the matrix into regular (ELL)
-- and irregular (COO) parts of the HYB format.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsehybpartitiont>
--
{# enum cusparseHybPartition_t as HybridPartition
  { underscoreToCase }
  with prefix="CUSPARSE_HYB_PARTITION" deriving (Eq, Show) #}


-- | Indicates whether the diagonal elements of the matrix are unity. The
-- diagonal elements are always assumed to be present, but if 'Unit' is passed
-- to an API routine, then the routine assumes that all diagonal entries are
-- unity and will not read or modify those entries.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsediagtypet>
--
{# enum cusparseDiagType_t as Diagonal
  { underscoreToCase }
  with prefix="CUSPARSE_DIAG_TYPE" deriving (Eq, Show) #}

-- | Indicates whether the upper or lower part of the sparse matrix is stored.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsefillmodet>
--
{# enum cusparseFillMode_t as Fill
  { underscoreToCase }
  with prefix="CUSPARSE_FILL_MODE" deriving (Eq, Show) #}

-- | Indicates whether indexing of matrix elements starts at zero or one.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparseindexbaset>
--
{# enum cusparseIndexBase_t as IndexBase
  { underscoreToCase }
  with prefix="CUSPARSE_INDEX_BASE" deriving (Eq, Show) #}

-- | Indicates the type of matrix.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsematrixtypet>
--
{# enum cusparseMatrixType_t as MatrixType
  { underscoreToCase }
  with prefix="CUSPARSE_MATRIX_TYPE" deriving (Eq, Show) #}

