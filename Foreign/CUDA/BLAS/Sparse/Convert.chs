--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Convert
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuSPARSE function reference:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-format-conversion-reference>
--

module Foreign.CUDA.BLAS.Sparse.Convert (

  Direction(..),
  Action(..),
  Hybrid,
  HybridPartition(..),
  Info_csru2csr,
  sbsr2csr,
  dbsr2csr,
  cbsr2csr,
  zbsr2csr,
  sgebsr2gebsc_bufferSize,
  dgebsr2gebsc_bufferSize,
  cgebsr2gebsc_bufferSize,
  zgebsr2gebsc_bufferSize,
  sgebsr2gebsc,
  dgebsr2gebsc,
  cgebsr2gebsc,
  zgebsr2gebsc,
  sgebsr2gebsr_bufferSize,
  dgebsr2gebsr_bufferSize,
  cgebsr2gebsr_bufferSize,
  zgebsr2gebsr_bufferSize,
  sgebsr2gebsr,
  dgebsr2gebsr,
  cgebsr2gebsr,
  zgebsr2gebsr,
  xgebsr2gebsrNnz,
  sgebsr2csr,
  dgebsr2csr,
  cgebsr2csr,
  zgebsr2csr,
  scsr2gebsr_bufferSize,
  dcsr2gebsr_bufferSize,
  ccsr2gebsr_bufferSize,
  zcsr2gebsr_bufferSize,
  scsr2gebsr,
  dcsr2gebsr,
  ccsr2gebsr,
  zcsr2gebsr,
  xcsr2gebsrNnz,
  xcoo2csr,
  scsc2dense,
  dcsc2dense,
  ccsc2dense,
  zcsc2dense,
  scsc2hyb,
  dcsc2hyb,
  ccsc2hyb,
  zcsc2hyb,
  scsr2bsr,
  dcsr2bsr,
  ccsr2bsr,
  zcsr2bsr,
  xcsr2bsrNnz,
  xcsr2coo,
  scsr2csc,
  dcsr2csc,
  ccsr2csc,
  zcsr2csc,
  scsr2dense,
  dcsr2dense,
  ccsr2dense,
  zcsr2dense,
  scsr2hyb,
  dcsr2hyb,
  ccsr2hyb,
  zcsr2hyb,
  sdense2csc,
  ddense2csc,
  cdense2csc,
  zdense2csc,
  sdense2csr,
  ddense2csr,
  cdense2csr,
  zdense2csr,
  sdense2hyb,
  ddense2hyb,
  cdense2hyb,
  zdense2hyb,
  shyb2csc,
  dhyb2csc,
  chyb2csc,
  zhyb2csc,
  shyb2csr,
  dhyb2csr,
  chyb2csr,
  zhyb2csr,
  shyb2dense,
  dhyb2dense,
  chyb2dense,
  zhyb2dense,
  snnz,
  dnnz,
  cnnz,
  znnz,
  createIdentityPermutation,
  xcoosort_bufferSizeExt,
  xcoosortByRow,
  xcoosortByColumn,
  xcsrsort_bufferSizeExt,
  xcsrsort,
  xcscsort_bufferSizeExt,
  xcscsort,
  scsru2csr_bufferSizeExt,
  dcsru2csr_bufferSizeExt,
  ccsru2csr_bufferSizeExt,
  zcsru2csr_bufferSizeExt,
  scsru2csr,
  dcsru2csr,
  ccsru2csr,
  zcsru2csr,
  scsr2csru,
  dcsr2csru,
  ccsr2csru,
  zcsr2csru,
  csr2cscEx,
  scsr2csr_compress,
  dcsr2csr_compress,
  ccsr2csr_compress,
  zcsr2csr_compress,

) where

import Data.Complex
import Numeric.Half
import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import Foreign.CUDA.Ptr
import Foreign.CUDA.BLAS.Sparse.Analysis
import Foreign.CUDA.BLAS.Sparse.Context
import Foreign.CUDA.BLAS.Sparse.Error
import Foreign.CUDA.BLAS.Sparse.Internal.C2HS
import Foreign.CUDA.BLAS.Sparse.Internal.Types
import Foreign.CUDA.BLAS.Sparse.Matrix.Descriptor
import Foreign.CUDA.BLAS.Sparse.Matrix.Hybrid

#include "cbits/stubs.h"
{# context lib="cusparse" #}

{-# INLINE useDevP #-}
useDevP :: DevicePtr a -> Ptr b
useDevP = useDevicePtr . castDevPtr

{-# INLINE useHostP #-}
useHostP :: HostPtr a -> Ptr b
useHostP = useHostPtr . castHostPtr


{-# INLINEABLE sbsr2csr #-}
{# fun unsafe cusparseSbsr2csr as sbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dbsr2csr #-}
{# fun unsafe cusparseDbsr2csr as dbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cbsr2csr #-}
{# fun unsafe cusparseCbsr2csr as cbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zbsr2csr #-}
{# fun unsafe cusparseZbsr2csr as zbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseSgebsr2gebsc_bufferSize as sgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseDgebsr2gebsc_bufferSize as dgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseCgebsr2gebsc_bufferSize as cgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseZgebsr2gebsc_bufferSize as zgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sgebsr2gebsc #-}
{# fun unsafe cusparseSgebsr2gebsc as sgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dgebsr2gebsc #-}
{# fun unsafe cusparseDgebsr2gebsc as dgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cgebsr2gebsc #-}
{# fun unsafe cusparseCgebsr2gebsc as cgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zgebsr2gebsc #-}
{# fun unsafe cusparseZgebsr2gebsc as zgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE sgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseSgebsr2gebsr_bufferSize as sgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseDgebsr2gebsr_bufferSize as dgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseCgebsr2gebsr_bufferSize as cgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseZgebsr2gebsr_bufferSize as zgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sgebsr2gebsr #-}
{# fun unsafe cusparseSgebsr2gebsr as sgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dgebsr2gebsr #-}
{# fun unsafe cusparseDgebsr2gebsr as dgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cgebsr2gebsr #-}
{# fun unsafe cusparseCgebsr2gebsr as cgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zgebsr2gebsr #-}
{# fun unsafe cusparseZgebsr2gebsr as zgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xgebsr2gebsrNnz #-}
{# fun unsafe cusparseXgebsr2gebsrNnz as xgebsr2gebsrNnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE sgebsr2csr #-}
{# fun unsafe cusparseSgebsr2csr as sgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dgebsr2csr #-}
{# fun unsafe cusparseDgebsr2csr as dgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cgebsr2csr #-}
{# fun unsafe cusparseCgebsr2csr as cgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zgebsr2csr #-}
{# fun unsafe cusparseZgebsr2csr as zgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2gebsr_bufferSize #-}
{# fun unsafe cusparseScsr2gebsr_bufferSize as scsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2gebsr_bufferSize #-}
{# fun unsafe cusparseDcsr2gebsr_bufferSize as dcsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2gebsr_bufferSize #-}
{# fun unsafe cusparseCcsr2gebsr_bufferSize as ccsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2gebsr_bufferSize #-}
{# fun unsafe cusparseZcsr2gebsr_bufferSize as zcsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2gebsr #-}
{# fun unsafe cusparseScsr2gebsr as scsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2gebsr #-}
{# fun unsafe cusparseDcsr2gebsr as dcsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2gebsr #-}
{# fun unsafe cusparseCcsr2gebsr as ccsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2gebsr #-}
{# fun unsafe cusparseZcsr2gebsr as zcsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xcsr2gebsrNnz #-}
{# fun unsafe cusparseXcsr2gebsrNnz as xcsr2gebsrNnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xcoo2csr #-}
{# fun unsafe cusparseXcoo2csr as xcoo2csr { useHandle `Handle', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Int32', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE scsc2dense #-}
{# fun unsafe cusparseScsc2dense as scsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcsc2dense #-}
{# fun unsafe cusparseDcsc2dense as dcsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccsc2dense #-}
{# fun unsafe cusparseCcsc2dense as ccsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcsc2dense #-}
{# fun unsafe cusparseZcsc2dense as zcsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE scsc2hyb #-}
{# fun unsafe cusparseScsc2hyb as scsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE dcsc2hyb #-}
{# fun unsafe cusparseDcsc2hyb as dcsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE ccsc2hyb #-}
{# fun unsafe cusparseCcsc2hyb as ccsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE zcsc2hyb #-}
{# fun unsafe cusparseZcsc2hyb as zcsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2bsr #-}
{# fun unsafe cusparseScsr2bsr as scsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2bsr #-}
{# fun unsafe cusparseDcsr2bsr as dcsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2bsr #-}
{# fun unsafe cusparseCcsr2bsr as ccsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2bsr #-}
{# fun unsafe cusparseZcsr2bsr as zcsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE xcsr2bsrNnz #-}
{# fun unsafe cusparseXcsr2bsrNnz as xcsr2bsrNnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE xcsr2coo #-}
{# fun unsafe cusparseXcsr2coo as xcsr2coo { useHandle `Handle', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Int32', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2csc #-}
{# fun unsafe cusparseScsr2csc as scsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2csc #-}
{# fun unsafe cusparseDcsr2csc as dcsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2csc #-}
{# fun unsafe cusparseCcsr2csc as ccsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2csc #-}
{# fun unsafe cusparseZcsr2csc as zcsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2dense #-}
{# fun unsafe cusparseScsr2dense as scsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2dense #-}
{# fun unsafe cusparseDcsr2dense as dcsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2dense #-}
{# fun unsafe cusparseCcsr2dense as ccsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2dense #-}
{# fun unsafe cusparseZcsr2dense as zcsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2hyb #-}
{# fun unsafe cusparseScsr2hyb as scsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2hyb #-}
{# fun unsafe cusparseDcsr2hyb as dcsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2hyb #-}
{# fun unsafe cusparseCcsr2hyb as ccsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2hyb #-}
{# fun unsafe cusparseZcsr2hyb as zcsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE sdense2csc #-}
{# fun unsafe cusparseSdense2csc as sdense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE ddense2csc #-}
{# fun unsafe cusparseDdense2csc as ddense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cdense2csc #-}
{# fun unsafe cusparseCdense2csc as cdense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zdense2csc #-}
{# fun unsafe cusparseZdense2csc as zdense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sdense2csr #-}
{# fun unsafe cusparseSdense2csr as sdense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE ddense2csr #-}
{# fun unsafe cusparseDdense2csr as ddense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cdense2csr #-}
{# fun unsafe cusparseCdense2csr as cdense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zdense2csr #-}
{# fun unsafe cusparseZdense2csr as zdense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sdense2hyb #-}
{# fun unsafe cusparseSdense2hyb as sdense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE ddense2hyb #-}
{# fun unsafe cusparseDdense2hyb as ddense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE cdense2hyb #-}
{# fun unsafe cusparseCdense2hyb as cdense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE zdense2hyb #-}
{# fun unsafe cusparseZdense2hyb as zdense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus* #}

{-# INLINEABLE shyb2csc #-}
{# fun unsafe cusparseShyb2csc as shyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dhyb2csc #-}
{# fun unsafe cusparseDhyb2csc as dhyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE chyb2csc #-}
{# fun unsafe cusparseChyb2csc as chyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zhyb2csc #-}
{# fun unsafe cusparseZhyb2csc as zhyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE shyb2csr #-}
{# fun unsafe cusparseShyb2csr as shyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dhyb2csr #-}
{# fun unsafe cusparseDhyb2csr as dhyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE chyb2csr #-}
{# fun unsafe cusparseChyb2csr as chyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zhyb2csr #-}
{# fun unsafe cusparseZhyb2csr as zhyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE shyb2dense #-}
{# fun unsafe cusparseShyb2dense as shyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dhyb2dense #-}
{# fun unsafe cusparseDhyb2dense as dhyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE chyb2dense #-}
{# fun unsafe cusparseChyb2dense as chyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zhyb2dense #-}
{# fun unsafe cusparseZhyb2dense as zhyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE snnz #-}
{# fun unsafe cusparseSnnz as snnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dnnz #-}
{# fun unsafe cusparseDnnz as dnnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cnnz #-}
{# fun unsafe cusparseCnnz as cnnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE znnz #-}
{# fun unsafe cusparseZnnz as znnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE createIdentityPermutation #-}
{# fun unsafe cusparseCreateIdentityPermutation as createIdentityPermutation { useHandle `Handle', `Int', useDevP `DevicePtr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE xcoosort_bufferSizeExt #-}
{# fun unsafe cusparseXcoosort_bufferSizeExt as xcoosort_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE xcoosortByRow #-}
{# fun unsafe cusparseXcoosortByRow as xcoosortByRow { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xcoosortByColumn #-}
{# fun unsafe cusparseXcoosortByColumn as xcoosortByColumn { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xcsrsort_bufferSizeExt #-}
{# fun unsafe cusparseXcsrsort_bufferSizeExt as xcsrsort_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE xcsrsort #-}
{# fun unsafe cusparseXcsrsort as xcsrsort { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xcscsort_bufferSizeExt #-}
{# fun unsafe cusparseXcscsort_bufferSizeExt as xcscsort_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE xcscsort #-}
{# fun unsafe cusparseXcscsort as xcscsort { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus* #}
#if CUDA_VERSION >= 7000

{-# INLINEABLE scsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseScsru2csr_bufferSizeExt as scsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE dcsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseDcsru2csr_bufferSizeExt as dcsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE ccsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseCcsru2csr_bufferSizeExt as ccsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE zcsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseZcsru2csr_bufferSizeExt as zcsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE scsru2csr #-}
{# fun unsafe cusparseScsru2csr as scsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsru2csr #-}
{# fun unsafe cusparseDcsru2csr as dcsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsru2csr #-}
{# fun unsafe cusparseCcsru2csr as ccsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsru2csr #-}
{# fun unsafe cusparseZcsru2csr as zcsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2csru #-}
{# fun unsafe cusparseScsr2csru as scsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2csru #-}
{# fun unsafe cusparseDcsr2csru as dcsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2csru #-}
{# fun unsafe cusparseCcsr2csru as ccsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2csru #-}
{# fun unsafe cusparseZcsr2csru as zcsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus* #}
#else

scsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Ptr Int64 -> IO ()
scsru2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'scsru2csr_bufferSizeExt' requires at least cuda-7.0"

dcsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Ptr Int64 -> IO ()
dcsru2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'dcsru2csr_bufferSizeExt' requires at least cuda-7.0"

ccsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Ptr Int64 -> IO ()
ccsru2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'ccsru2csr_bufferSizeExt' requires at least cuda-7.0"

zcsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Ptr Int64 -> IO ()
zcsru2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'zcsru2csr_bufferSizeExt' requires at least cuda-7.0"

scsru2csr :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
scsru2csr _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsru2csr' requires at least cuda-7.0"

dcsru2csr :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
dcsru2csr _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsru2csr' requires at least cuda-7.0"

ccsru2csr :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
ccsru2csr _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsru2csr' requires at least cuda-7.0"

zcsru2csr :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
zcsru2csr _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsru2csr' requires at least cuda-7.0"

scsr2csru :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
scsr2csru _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsr2csru' requires at least cuda-7.0"

dcsr2csru :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
dcsr2csru _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsr2csru' requires at least cuda-7.0"

ccsr2csru :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
ccsr2csru _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsr2csru' requires at least cuda-7.0"

zcsr2csru :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> DevicePtr () -> IO ()
zcsr2csru _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsr2csru' requires at least cuda-7.0"
#endif
#if CUDA_VERSION >= 8000

{-# INLINEABLE csr2cscEx #-}
{# fun unsafe cusparseCsr2cscEx as csr2cscEx { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', cFromEnum `Type' } -> `()' checkStatus* #}

{-# INLINEABLE scsr2csr_compress #-}
{# fun unsafe cusparseScsr2csr_compress as scsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float' } -> `()' checkStatus* #}

{-# INLINEABLE dcsr2csr_compress #-}
{# fun unsafe cusparseDcsr2csr_compress as dcsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CDouble `Double' } -> `()' checkStatus* #}

{-# INLINEABLE ccsr2csr_compress #-}
{# fun unsafe cusparseCcsr2csr_compress as ccsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', withComplex* `(Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zcsr2csr_compress #-}
{# fun unsafe cusparseZcsr2csr_compress as zcsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', withComplex* `(Complex Double)' } -> `()' checkStatus* #}
#else

csr2cscEx :: Handle -> Int -> Int -> Int -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> Action -> IndexBase -> Type -> IO ()
csr2cscEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'csr2cscEx' requires at least cuda-8.0"

scsr2csr_compress :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> DevicePtr Int32 -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> IO ()
scsr2csr_compress _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsr2csr_compress' requires at least cuda-8.0"

dcsr2csr_compress :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> DevicePtr Int32 -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Double -> IO ()
dcsr2csr_compress _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsr2csr_compress' requires at least cuda-8.0"

ccsr2csr_compress :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> DevicePtr Int32 -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> (Complex Float) -> IO ()
ccsr2csr_compress _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsr2csr_compress' requires at least cuda-8.0"

zcsr2csr_compress :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> DevicePtr Int32 -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> (Complex Double) -> IO ()
zcsr2csr_compress _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsr2csr_compress' requires at least cuda-8.0"
#endif
