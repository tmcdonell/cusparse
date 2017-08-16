{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Matrix.Descriptor
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Sparse.Matrix.Descriptor (

  MatrixDescriptor(..),
  MatrixType(..),
  IndexBase(..),
  Diagonal(..),
  Fill(..),

  createMatDescr,
  destroyMatDescr,

  -- Querying properties
  getDiagonal,
  getFillMode,
  getIndexBase,
  getMatrixType,

  -- Setting properties
  setDiagonal,
  setFillMode,
  setIndexBase,
  setMatrixType,

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


-- | An opaque type used to describe the shape and properties of a matrix.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsematdescrt>
--
newtype MatrixDescriptor = MatrixDescriptor { useMatDescr :: {# type cusparseMatDescr_t #}}

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


-- | Create a new matrix descriptor, with matrix type 'General' and index base
-- 'Zero', while leaving other fields uninitialised.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsecreatematdescr>
--
{-# INLINEABLE createMatDescr #-}
{# fun unsafe cusparseCreateMatDescr as createMatDescr
  { alloca- `MatrixDescriptor' peekMD* } -> `()' checkStatus*- #}
  where
    peekMD = liftM MatrixDescriptor . peek

-- | Release memory associated with a matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsedestroymatdescr>
--
{-# INLINEABLE destroyMatDescr #-}
{# fun unsafe cusparseDestroyMatDescr as destroyMatDescr
  { useMatDescr `MatrixDescriptor' } -> `()' checkStatus* #}


-- | Get the 'Diagonal' type field of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsegetmatdiagtype>
--
{-# INLINEABLE getDiagonal #-}
{# fun unsafe cusparseGetMatDiagType as getDiagonal
  { useMatDescr `MatrixDescriptor' } -> `Diagonal' cToEnum #}

-- | Get the 'Fill' mode of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsegetmatfillmode>
--
{-# INLINEABLE getFillMode #-}
{# fun unsafe cusparseGetMatFillMode as getFillMode
  { useMatDescr `MatrixDescriptor' } -> `Fill' cToEnum #}

-- | Get the 'IndexBase' mode of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsegetmatindexbase>
--
{-# INLINEABLE getIndexBase #-}
{# fun unsafe cusparseGetMatIndexBase as getIndexBase
  { useMatDescr `MatrixDescriptor' } -> `IndexBase' cToEnum #}

-- | Get the 'MatrixType' mode of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsegetmattype>
--
{-# INLINEABLE getMatrixType #-}
{# fun unsafe cusparseGetMatType as getMatrixType
  { useMatDescr `MatrixDescriptor' } -> `MatrixType' cToEnum #}


-- | Set the 'Diagonal' type field of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsesetmatdiagtype>
--
{-# INLINEABLE setDiagonal #-}
{# fun unsafe cusparseSetMatDiagType as setDiagonal
  { useMatDescr `MatrixDescriptor', cFromEnum `Diagonal' } -> `()' checkStatus* #}

-- | Set the 'Fill' mode of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsesetmatfillmode>
--
{-# INLINEABLE setFillMode #-}
{# fun unsafe cusparseSetMatFillMode as setFillMode
  { useMatDescr `MatrixDescriptor', cFromEnum `Fill' } -> `()' checkStatus* #}

-- | Set the 'IndexBase' mode of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsesetmatindexbase>
--
{-# INLINEABLE setIndexBase #-}
{# fun unsafe cusparseSetMatIndexBase as setIndexBase
  { useMatDescr `MatrixDescriptor', cFromEnum `IndexBase' } -> `()' checkStatus* #}

-- | Set the 'MatrixType' mode of the matrix descriptor.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsesetmattype>
--
{-# INLINEABLE setMatrixType #-}
{# fun unsafe cusparseSetMatType as setMatrixType
  { useMatDescr `MatrixDescriptor', cFromEnum `MatrixType' } -> `()' checkStatus* #}

