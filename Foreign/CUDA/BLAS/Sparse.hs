-- |
-- Module      : Foreign.CUDA.BLAS.Sparse
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The cuSPARSE library is an implementation of Sparse BLAS (Basic Linear
-- Algebra Subprograms) for NVIDIA GPUs. Sparse matrices are those where the
-- majority of elements are zero. Sparse BLAS routines are specifically
-- implemented to take advantage of this sparsity.
--
-- To use operations from the cuSPARSE library, the user must allocate the
-- required matrices and vectors in the GPU memory space, fill them with data,
-- call the desired sequence of cuSPARSE functions, then copy the results from
-- the GPU memory space back to the host.
--
-- The <http://hackage.haskell.org/package/cuda cuda> package can be used for
-- writing to and retrieving data from the GPU.
--
-- [/Example/]
--
-- [/Additional information/]
--
-- For more information, see the NVIDIA cuSPARSE documentation:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html>
--
--

module Foreign.CUDA.BLAS.Sparse (

  -- * Control
  module Foreign.CUDA.BLAS.Sparse.Context,
  module Foreign.CUDA.BLAS.Sparse.Stream,
  module Foreign.CUDA.BLAS.Sparse.Error,

  -- * Operations
  module Foreign.CUDA.BLAS.Sparse.Level1,
  module Foreign.CUDA.BLAS.Sparse.Level2,
  module Foreign.CUDA.BLAS.Sparse.Level3,

) where

import Foreign.CUDA.BLAS.Sparse.Context         hiding ( useHandle )
import Foreign.CUDA.BLAS.Sparse.Error           hiding ( resultIfOk, nothingIfOk )
import Foreign.CUDA.BLAS.Sparse.Stream

import Foreign.CUDA.BLAS.Sparse.Level1
import Foreign.CUDA.BLAS.Sparse.Level2
import Foreign.CUDA.BLAS.Sparse.Level3

