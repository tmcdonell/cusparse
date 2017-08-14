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
-- The following is based on the following example:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#appendix-b-cusparse-library-c---example>
--
-- It assumes basic familiarity with the <http://hackage.haskell.org/package/cuda cuda>
-- package, as described in the "Foreign.CUDA.Driver" module.
--
-- >>> import Foreign.CUDA.Driver      as CUDA
-- >>> import Foreign.CUDA.BLAS.Sparse as Sparse
-- >>> CUDA.initialise []
-- >>> dev <- CUDA.device 0
-- >>> ctx <- CUDA.create dev []
--
-- We begin by creating the following matrix in COO format and transferring to
-- the GPU:
--
-- \[
-- \left(\begin{matrix}
--   1.0 &     & 2.0 & 3.0 \\
--       & 4.0 &     &     \\
--   5.0 &     & 6.0 & 7.0 \\
--       & 8.0 &     & 9.0
-- \end{matrix}\right)
-- \]
--
-- >>> let n = 4
-- >>> let nnz = 9
-- >>> d_cooRowIdx <- newListArray [ 0,0,0, 1, 2,2,2, 3,3 ]  :: IO (DevicePtr Int32)
-- >>> d_cooColIdx <- newListArray [ 0,2,3, 1, 0,2,3, 1,3 ]  :: IO (DevicePtr Int32)
-- >>> d_vals      <- newListArray [ 1..9 ]                  :: IO (DevicePtr Double)
--
-- Create a sparse and dense vector:
--
-- >>> let nnz_vector = 3
-- >>> d_xVal <- newListArray [ 100, 200, 400 ] :: IO (DevicePtr Double)
-- >>> d_xIdx <- newListArray [ 0,   1,   3 ]   :: IO (DevicePtr Int32)
-- >>> d_y    <- newListArray [ 10, 20 .. 80 ]  :: IO (DevicePtr Double)
--
-- Initialise the cuSPARSE library and set up the matrix descriptor:
--
-- >>> hdl <- Sparse.create
-- >>> mat <- Sparse.createMatDescr
-- >>> Sparse.setMatrixType mat General
-- >>> Sparse.setIndexBase mat Zero
--
-- Exercise the conversion routines to convert from COO to CSR format:
--
-- >>> d_csrRowPtr <- CUDA.mallocArray (n+1) :: IO (DevicePtr Int32)
-- >>> xcoo2csr hdl d_cooRowIdx nnz n d_csrRowPtr Zero
-- >>> peekListArray (n+1) d_csrRowPtr
-- [0,3,4,7,9]
--
-- Scatter elements from the sparse vector into the dense vector:
--
-- >>> dsctr hdl nnz_vector d_xVal d_xIdx (d_y `plusDevPtr` (n * sizeOf (undefined::Double))) Zero
-- >>> peekListArray 8 d_y
-- [10.0,20.0,30.0,40.0,100.0,200.0,70.0,400.0]
--
-- Multiply the matrix in CSR format with the dense vector:
--
-- >>> with 2.0 $ \alpha ->
-- >>> with 3.0 $ \beta  ->
-- >>>   dcsrmv hdl N n n nnz alpha mat d_vals d_csrRowPtr d_cooColIdx d_y beta (d_y `plusDevPtr` (n * sizeOf (undefined::Double)))
-- >>> peekListArray 8 d_y
-- [10.0,20.0,30.0,40.0,680.0,760.0,1230.0,2240.0]
--
-- Multiply the matrix in CSR format with a dense matrix:
--
-- >>> d_z <- CUDA.mallocArray (2*(n+1)) :: IO (DevicePtr Double)
-- >>> memset (castDevPtr d_z :: DevicePtr Word8) (2*(n+1)*sizeOf (undefined::Double)) 0
-- >>> with 5.0 $ \alpha ->
-- >>> with 0.0 $ \beta  ->
-- >>>   dcsrmm hdl N n 2 n nnz alpha mat d_vals d_csrRowPtr d_cooColIdx d_y n beta d_z (n+1)
-- >> peekListArray (2*(n+1)) d_z
-- [950.0,400.0,2550.0,2600.0,0.0,49300.0,15200.0,132300.0,131200.0,0.0]
--
-- Finally, we should 'Foreign.CUDA.Driver.free' the device memory we allocated,
-- and release the Sparse BLAS context handle:
--
-- >>> Sparse.destroy hdl
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
  module Foreign.CUDA.BLAS.Sparse.Analysis,
  module Foreign.CUDA.BLAS.Sparse.Error,
  module Foreign.CUDA.BLAS.Sparse.Matrix.Descriptor,
  module Foreign.CUDA.BLAS.Sparse.Matrix.Hybrid,
  module Foreign.CUDA.BLAS.Sparse.Stream,

  -- * Operations
  module Foreign.CUDA.BLAS.Sparse.Level1,
  module Foreign.CUDA.BLAS.Sparse.Level2,
  module Foreign.CUDA.BLAS.Sparse.Level3,
  module Foreign.CUDA.BLAS.Sparse.Precondition,
  module Foreign.CUDA.BLAS.Sparse.Reorder,
  module Foreign.CUDA.BLAS.Sparse.Convert,

) where

import Foreign.CUDA.BLAS.Sparse.Analysis                  hiding ( useInfo, useInfo_bsrsv2, useInfo_csrsv2, useInfo_bsrsm2, useInfo_csrgemm2 )
import Foreign.CUDA.BLAS.Sparse.Context                   hiding ( useHandle )
import Foreign.CUDA.BLAS.Sparse.Error                     hiding ( resultIfOk, nothingIfOk )
import Foreign.CUDA.BLAS.Sparse.Matrix.Descriptor         hiding ( useMatDescr )
import Foreign.CUDA.BLAS.Sparse.Matrix.Hybrid             hiding ( useHYB )
import Foreign.CUDA.BLAS.Sparse.Stream

import Foreign.CUDA.BLAS.Sparse.Level1
import Foreign.CUDA.BLAS.Sparse.Level2
import Foreign.CUDA.BLAS.Sparse.Level3
import Foreign.CUDA.BLAS.Sparse.Precondition
import Foreign.CUDA.BLAS.Sparse.Reorder
import Foreign.CUDA.BLAS.Sparse.Convert

