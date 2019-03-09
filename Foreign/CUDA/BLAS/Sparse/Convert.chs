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
  Algorithm_csr2csc(..),
  Info_csru2csr,
  Info_prune,
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
  spruneDense2csr_bufferSizeExt,
  dpruneDense2csr_bufferSizeExt,
  hpruneDense2csr_bufferSizeExt,
  spruneDense2csrNnz,
  dpruneDense2csrNnz,
  hpruneDense2csrNnz,
  spruneDense2csr,
  dpruneDense2csr,
  hpruneDense2csr,
  spruneCsr2csr_bufferSizeExt,
  dpruneCsr2csr_bufferSizeExt,
  hpruneCsr2csr_bufferSizeExt,
  spruneCsr2csrNnz,
  dpruneCsr2csrNnz,
  hpruneCsr2csrNnz,
  spruneCsr2csr,
  dpruneCsr2csr,
  hpruneCsr2csr,
  spruneDense2csrByPercentage_bufferSizeExt,
  dpruneDense2csrByPercentage_bufferSizeExt,
  hpruneDense2csrByPercentage_bufferSizeExt,
  spruneDense2csrNnzByPercentage,
  dpruneDense2csrNnzByPercentage,
  hpruneDense2csrNnzByPercentage,
  spruneDense2csrByPercentage,
  dpruneDense2csrByPercentage,
  hpruneDense2csrByPercentage,
  spruneCsr2csrByPercentage_bufferSizeExt,
  dpruneCsr2csrByPercentage_bufferSizeExt,
  hpruneCsr2csrByPercentage_bufferSizeExt,
  spruneCsr2csrNnzByPercentage,
  dpruneCsr2csrNnzByPercentage,
  hpruneCsr2csrNnzByPercentage,
  spruneCsr2csrByPercentage,
  dpruneCsr2csrByPercentage,
  hpruneCsr2csrByPercentage,
  snnz_compress,
  dnnz_compress,
  cnnz_compress,
  znnz_compress,
  csr2cscEx2,
  csr2cscEx2_bufferSize,

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
{# fun unsafe cusparseSbsr2csr as sbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsr2csr #-}
{# fun unsafe cusparseDbsr2csr as dbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsr2csr #-}
{# fun unsafe cusparseCbsr2csr as cbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsr2csr #-}
{# fun unsafe cusparseZbsr2csr as zbsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE sgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseSgebsr2gebsc_bufferSize as sgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseDgebsr2gebsc_bufferSize as dgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseCgebsr2gebsc_bufferSize as cgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgebsr2gebsc_bufferSize #-}
{# fun unsafe cusparseZgebsr2gebsc_bufferSize as zgebsr2gebsc_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sgebsr2gebsc #-}
{# fun unsafe cusparseSgebsr2gebsc as sgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgebsr2gebsc #-}
{# fun unsafe cusparseDgebsr2gebsc as dgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgebsr2gebsc #-}
{# fun unsafe cusparseCgebsr2gebsc as cgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgebsr2gebsc #-}
{# fun unsafe cusparseZgebsr2gebsc as zgebsr2gebsc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseSgebsr2gebsr_bufferSize as sgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseDgebsr2gebsr_bufferSize as dgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseCgebsr2gebsr_bufferSize as cgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgebsr2gebsr_bufferSize #-}
{# fun unsafe cusparseZgebsr2gebsr_bufferSize as zgebsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sgebsr2gebsr #-}
{# fun unsafe cusparseSgebsr2gebsr as sgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgebsr2gebsr #-}
{# fun unsafe cusparseDgebsr2gebsr as dgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgebsr2gebsr #-}
{# fun unsafe cusparseCgebsr2gebsr as cgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgebsr2gebsr #-}
{# fun unsafe cusparseZgebsr2gebsr as zgebsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xgebsr2gebsrNnz #-}
{# fun unsafe cusparseXgebsr2gebsrNnz as xgebsr2gebsrNnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sgebsr2csr #-}
{# fun unsafe cusparseSgebsr2csr as sgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dgebsr2csr #-}
{# fun unsafe cusparseDgebsr2csr as dgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE cgebsr2csr #-}
{# fun unsafe cusparseCgebsr2csr as cgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zgebsr2csr #-}
{# fun unsafe cusparseZgebsr2csr as zgebsr2csr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2gebsr_bufferSize #-}
{# fun unsafe cusparseScsr2gebsr_bufferSize as scsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2gebsr_bufferSize #-}
{# fun unsafe cusparseDcsr2gebsr_bufferSize as dcsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2gebsr_bufferSize #-}
{# fun unsafe cusparseCcsr2gebsr_bufferSize as ccsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2gebsr_bufferSize #-}
{# fun unsafe cusparseZcsr2gebsr_bufferSize as zcsr2gebsr_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2gebsr #-}
{# fun unsafe cusparseScsr2gebsr as scsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2gebsr #-}
{# fun unsafe cusparseDcsr2gebsr as dcsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2gebsr #-}
{# fun unsafe cusparseCcsr2gebsr as ccsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2gebsr #-}
{# fun unsafe cusparseZcsr2gebsr as zcsr2gebsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsr2gebsrNnz #-}
{# fun unsafe cusparseXcsr2gebsrNnz as xcsr2gebsrNnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', `Int', `Int', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcoo2csr #-}
{# fun unsafe cusparseXcoo2csr as xcoo2csr { useHandle `Handle', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Int32', cFromEnum `IndexBase' } -> `()' checkStatus*- #}

{-# INLINEABLE scsc2dense #-}
{# fun unsafe cusparseScsc2dense as scsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsc2dense #-}
{# fun unsafe cusparseDcsc2dense as dcsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsc2dense #-}
{# fun unsafe cusparseCcsc2dense as ccsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsc2dense #-}
{# fun unsafe cusparseZcsc2dense as zcsc2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE scsc2hyb #-}
{# fun unsafe cusparseScsc2hyb as scsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsc2hyb #-}
{# fun unsafe cusparseDcsc2hyb as dcsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsc2hyb #-}
{# fun unsafe cusparseCcsc2hyb as ccsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsc2hyb #-}
{# fun unsafe cusparseZcsc2hyb as zcsc2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2bsr #-}
{# fun unsafe cusparseScsr2bsr as scsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2bsr #-}
{# fun unsafe cusparseDcsr2bsr as dcsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2bsr #-}
{# fun unsafe cusparseCcsr2bsr as ccsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2bsr #-}
{# fun unsafe cusparseZcsr2bsr as zcsr2bsr { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsr2bsrNnz #-}
{# fun unsafe cusparseXcsr2bsrNnz as xcsr2bsrNnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsr2coo #-}
{# fun unsafe cusparseXcsr2coo as xcsr2coo { useHandle `Handle', useDevP `DevicePtr Int32', `Int', `Int', useDevP `DevicePtr Int32', cFromEnum `IndexBase' } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2csc #-}
{# fun unsafe cusparseScsr2csc as scsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2csc #-}
{# fun unsafe cusparseDcsr2csc as dcsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2csc #-}
{# fun unsafe cusparseCcsr2csc as ccsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2csc #-}
{# fun unsafe cusparseZcsr2csc as zcsr2csc { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase' } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2dense #-}
{# fun unsafe cusparseScsr2dense as scsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2dense #-}
{# fun unsafe cusparseDcsr2dense as dcsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2dense #-}
{# fun unsafe cusparseCcsr2dense as ccsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2dense #-}
{# fun unsafe cusparseZcsr2dense as zcsr2dense { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2hyb #-}
{# fun unsafe cusparseScsr2hyb as scsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2hyb #-}
{# fun unsafe cusparseDcsr2hyb as dcsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2hyb #-}
{# fun unsafe cusparseCcsr2hyb as ccsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2hyb #-}
{# fun unsafe cusparseZcsr2hyb as zcsr2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE sdense2csc #-}
{# fun unsafe cusparseSdense2csc as sdense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE ddense2csc #-}
{# fun unsafe cusparseDdense2csc as ddense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE cdense2csc #-}
{# fun unsafe cusparseCdense2csc as cdense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zdense2csc #-}
{# fun unsafe cusparseZdense2csc as zdense2csc { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE sdense2csr #-}
{# fun unsafe cusparseSdense2csr as sdense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE ddense2csr #-}
{# fun unsafe cusparseDdense2csr as ddense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE cdense2csr #-}
{# fun unsafe cusparseCdense2csr as cdense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zdense2csr #-}
{# fun unsafe cusparseZdense2csr as zdense2csr { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE sdense2hyb #-}
{# fun unsafe cusparseSdense2hyb as sdense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE ddense2hyb #-}
{# fun unsafe cusparseDdense2hyb as ddense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE cdense2hyb #-}
{# fun unsafe cusparseCdense2hyb as cdense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE zdense2hyb #-}
{# fun unsafe cusparseZdense2hyb as zdense2hyb { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', useHYB `Hybrid', `Int', cFromEnum `HybridPartition' } -> `()' checkStatus*- #}

{-# INLINEABLE shyb2csc #-}
{# fun unsafe cusparseShyb2csc as shyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dhyb2csc #-}
{# fun unsafe cusparseDhyb2csc as dhyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE chyb2csc #-}
{# fun unsafe cusparseChyb2csc as chyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zhyb2csc #-}
{# fun unsafe cusparseZhyb2csc as zhyb2csc { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE shyb2csr #-}
{# fun unsafe cusparseShyb2csr as shyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dhyb2csr #-}
{# fun unsafe cusparseDhyb2csr as dhyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE chyb2csr #-}
{# fun unsafe cusparseChyb2csr as chyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zhyb2csr #-}
{# fun unsafe cusparseZhyb2csr as zhyb2csr { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE shyb2dense #-}
{# fun unsafe cusparseShyb2dense as shyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dhyb2dense #-}
{# fun unsafe cusparseDhyb2dense as dhyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE chyb2dense #-}
{# fun unsafe cusparseChyb2dense as chyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zhyb2dense #-}
{# fun unsafe cusparseZhyb2dense as zhyb2dense { useHandle `Handle', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE snnz #-}
{# fun unsafe cusparseSnnz as snnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dnnz #-}
{# fun unsafe cusparseDnnz as dnnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE cnnz #-}
{# fun unsafe cusparseCnnz as cnnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE znnz #-}
{# fun unsafe cusparseZnnz as znnz { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus*- #}
#if CUDA_VERSION >= 7000

{-# INLINEABLE createIdentityPermutation #-}
{# fun unsafe cusparseCreateIdentityPermutation as createIdentityPermutation { useHandle `Handle', `Int', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE xcoosort_bufferSizeExt #-}
{# fun unsafe cusparseXcoosort_bufferSizeExt as xcoosort_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE xcoosortByRow #-}
{# fun unsafe cusparseXcoosortByRow as xcoosortByRow { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcoosortByColumn #-}
{# fun unsafe cusparseXcoosortByColumn as xcoosortByColumn { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrsort_bufferSizeExt #-}
{# fun unsafe cusparseXcsrsort_bufferSizeExt as xcsrsort_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrsort #-}
{# fun unsafe cusparseXcsrsort as xcsrsort { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcscsort_bufferSizeExt #-}
{# fun unsafe cusparseXcscsort_bufferSizeExt as xcscsort_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE xcscsort #-}
{# fun unsafe cusparseXcscsort as xcscsort { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseScsru2csr_bufferSizeExt as scsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseDcsru2csr_bufferSizeExt as dcsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseCcsru2csr_bufferSizeExt as ccsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsru2csr_bufferSizeExt #-}
{# fun unsafe cusparseZcsru2csr_bufferSizeExt as zcsru2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsru2csr #-}
{# fun unsafe cusparseScsru2csr as scsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsru2csr #-}
{# fun unsafe cusparseDcsru2csr as dcsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsru2csr #-}
{# fun unsafe cusparseCcsru2csr as ccsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsru2csr #-}
{# fun unsafe cusparseZcsru2csr as zcsru2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2csru #-}
{# fun unsafe cusparseScsr2csru as scsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2csru #-}
{# fun unsafe cusparseDcsr2csru as dcsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2csru #-}
{# fun unsafe cusparseCcsr2csru as ccsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2csru #-}
{# fun unsafe cusparseZcsr2csru as zcsr2csru { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csru2csr `Info_csru2csr', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}
#else

createIdentityPermutation :: Handle -> Int -> DevicePtr Int32 -> IO ()
createIdentityPermutation _ _ _ = cusparseError "'createIdentityPermutation' requires at least cuda-7.0"

xcoosort_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
xcoosort_bufferSizeExt _ _ _ _ _ _ _ = cusparseError "'xcoosort_bufferSizeExt' requires at least cuda-7.0"

xcoosortByRow :: Handle -> Int -> Int -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
xcoosortByRow _ _ _ _ _ _ _ _ = cusparseError "'xcoosortByRow' requires at least cuda-7.0"

xcoosortByColumn :: Handle -> Int -> Int -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
xcoosortByColumn _ _ _ _ _ _ _ _ = cusparseError "'xcoosortByColumn' requires at least cuda-7.0"

xcsrsort_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
xcsrsort_bufferSizeExt _ _ _ _ _ _ _ = cusparseError "'xcsrsort_bufferSizeExt' requires at least cuda-7.0"

xcsrsort :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
xcsrsort _ _ _ _ _ _ _ _ _ = cusparseError "'xcsrsort' requires at least cuda-7.0"

xcscsort_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
xcscsort_bufferSizeExt _ _ _ _ _ _ _ = cusparseError "'xcscsort_bufferSizeExt' requires at least cuda-7.0"

xcscsort :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
xcscsort _ _ _ _ _ _ _ _ _ = cusparseError "'xcscsort' requires at least cuda-7.0"

scsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Int -> IO ()
scsru2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'scsru2csr_bufferSizeExt' requires at least cuda-7.0"

dcsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Int -> IO ()
dcsru2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'dcsru2csr_bufferSizeExt' requires at least cuda-7.0"

ccsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Int -> IO ()
ccsru2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'ccsru2csr_bufferSizeExt' requires at least cuda-7.0"

zcsru2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csru2csr -> Int -> IO ()
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
{# fun unsafe cusparseCsr2cscEx as csr2cscEx { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Action', cFromEnum `IndexBase', cFromEnum `Type' } -> `()' checkStatus*- #}

{-# INLINEABLE scsr2csr_compress #-}
{# fun unsafe cusparseScsr2csr_compress as scsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsr2csr_compress #-}
{# fun unsafe cusparseDcsr2csr_compress as dcsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CDouble `Double' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsr2csr_compress #-}
{# fun unsafe cusparseCcsr2csr_compress as ccsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', withComplex* `(Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsr2csr_compress #-}
{# fun unsafe cusparseZcsr2csr_compress as zcsr2csr_compress { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', withComplex* `(Complex Double)' } -> `()' checkStatus*- #}
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
#if CUDA_VERSION >= 9010

{-# INLINEABLE spruneDense2csr_bufferSizeExt #-}
{# fun unsafe cusparseSpruneDense2csr_bufferSizeExt as spruneDense2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneDense2csr_bufferSizeExt #-}
{# fun unsafe cusparseDpruneDense2csr_bufferSizeExt as dpruneDense2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneDense2csr_bufferSizeExt #-}
{# fun unsafe cusparseHpruneDense2csr_bufferSizeExt as hpruneDense2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Half', `Int', castPtr `Ptr Half', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE spruneDense2csrNnz #-}
{# fun unsafe cusparseSpruneDense2csrNnz as spruneDense2csrNnz { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneDense2csrNnz #-}
{# fun unsafe cusparseDpruneDense2csrNnz as dpruneDense2csrNnz { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneDense2csrNnz #-}
{# fun unsafe cusparseHpruneDense2csrNnz as hpruneDense2csrNnz { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Half', `Int', castPtr `Ptr Half', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE spruneDense2csr #-}
{# fun unsafe cusparseSpruneDense2csr as spruneDense2csr { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneDense2csr #-}
{# fun unsafe cusparseDpruneDense2csr as dpruneDense2csr { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneDense2csr #-}
{# fun unsafe cusparseHpruneDense2csr as hpruneDense2csr { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Half', `Int', castPtr `Ptr Half', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE spruneCsr2csr_bufferSizeExt #-}
{# fun unsafe cusparseSpruneCsr2csr_bufferSizeExt as spruneCsr2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneCsr2csr_bufferSizeExt #-}
{# fun unsafe cusparseDpruneCsr2csr_bufferSizeExt as dpruneCsr2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneCsr2csr_bufferSizeExt #-}
{# fun unsafe cusparseHpruneCsr2csr_bufferSizeExt as hpruneCsr2csr_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Half', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE spruneCsr2csrNnz #-}
{# fun unsafe cusparseSpruneCsr2csrNnz as spruneCsr2csrNnz { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneCsr2csrNnz #-}
{# fun unsafe cusparseDpruneCsr2csrNnz as dpruneCsr2csrNnz { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneCsr2csrNnz #-}
{# fun unsafe cusparseHpruneCsr2csrNnz as hpruneCsr2csrNnz { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Half', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE spruneCsr2csr #-}
{# fun unsafe cusparseSpruneCsr2csr as spruneCsr2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneCsr2csr #-}
{# fun unsafe cusparseDpruneCsr2csr as dpruneCsr2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneCsr2csr #-}
{# fun unsafe cusparseHpruneCsr2csr as hpruneCsr2csr { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Half', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE spruneDense2csrByPercentage_bufferSizeExt #-}
{# fun unsafe cusparseSpruneDense2csrByPercentage_bufferSizeExt as spruneDense2csrByPercentage_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneDense2csrByPercentage_bufferSizeExt #-}
{# fun unsafe cusparseDpruneDense2csrByPercentage_bufferSizeExt as dpruneDense2csrByPercentage_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneDense2csrByPercentage_bufferSizeExt #-}
{# fun unsafe cusparseHpruneDense2csrByPercentage_bufferSizeExt as hpruneDense2csrByPercentage_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Half', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE spruneDense2csrNnzByPercentage #-}
{# fun unsafe cusparseSpruneDense2csrNnzByPercentage as spruneDense2csrNnzByPercentage { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneDense2csrNnzByPercentage #-}
{# fun unsafe cusparseDpruneDense2csrNnzByPercentage as dpruneDense2csrNnzByPercentage { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneDense2csrNnzByPercentage #-}
{# fun unsafe cusparseHpruneDense2csrNnzByPercentage as hpruneDense2csrNnzByPercentage { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Half', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE spruneDense2csrByPercentage #-}
{# fun unsafe cusparseSpruneDense2csrByPercentage as spruneDense2csrByPercentage { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneDense2csrByPercentage #-}
{# fun unsafe cusparseDpruneDense2csrByPercentage as dpruneDense2csrByPercentage { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneDense2csrByPercentage #-}
{# fun unsafe cusparseHpruneDense2csrByPercentage as hpruneDense2csrByPercentage { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Half', `Int', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE spruneCsr2csrByPercentage_bufferSizeExt #-}
{# fun unsafe cusparseSpruneCsr2csrByPercentage_bufferSizeExt as spruneCsr2csrByPercentage_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneCsr2csrByPercentage_bufferSizeExt #-}
{# fun unsafe cusparseDpruneCsr2csrByPercentage_bufferSizeExt as dpruneCsr2csrByPercentage_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneCsr2csrByPercentage_bufferSizeExt #-}
{# fun unsafe cusparseHpruneCsr2csrByPercentage_bufferSizeExt as hpruneCsr2csrByPercentage_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE spruneCsr2csrNnzByPercentage #-}
{# fun unsafe cusparseSpruneCsr2csrNnzByPercentage as spruneCsr2csrNnzByPercentage { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneCsr2csrNnzByPercentage #-}
{# fun unsafe cusparseDpruneCsr2csrNnzByPercentage as dpruneCsr2csrNnzByPercentage { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneCsr2csrNnzByPercentage #-}
{# fun unsafe cusparseHpruneCsr2csrNnzByPercentage as hpruneCsr2csrNnzByPercentage { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE spruneCsr2csrByPercentage #-}
{# fun unsafe cusparseSpruneCsr2csrByPercentage as spruneCsr2csrByPercentage { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dpruneCsr2csrByPercentage #-}
{# fun unsafe cusparseDpruneCsr2csrByPercentage as dpruneCsr2csrByPercentage { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE hpruneCsr2csrByPercentage #-}
{# fun unsafe cusparseHpruneCsr2csrByPercentage as hpruneCsr2csrByPercentage { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', CFloat `Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Half', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_prune `Info_prune', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE snnz_compress #-}
{# fun unsafe cusparseSnnz_compress as snnz_compress { useHandle `Handle', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Int32', CFloat `Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dnnz_compress #-}
{# fun unsafe cusparseDnnz_compress as dnnz_compress { useHandle `Handle', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Int32', CDouble `Double' } -> `()' checkStatus*- #}

{-# INLINEABLE cnnz_compress #-}
{# fun unsafe cusparseCnnz_compress as cnnz_compress { useHandle `Handle', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Int32', withComplex* `(Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE znnz_compress #-}
{# fun unsafe cusparseZnnz_compress as znnz_compress { useHandle `Handle', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Int32', withComplex* `(Complex Double)' } -> `()' checkStatus*- #}
#else

spruneDense2csr_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Float -> Int -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
spruneDense2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneDense2csr_bufferSizeExt' requires at least cuda-9.0"

dpruneDense2csr_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Double -> Int -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
dpruneDense2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneDense2csr_bufferSizeExt' requires at least cuda-9.0"

hpruneDense2csr_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Half -> Int -> Ptr Half -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
hpruneDense2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneDense2csr_bufferSizeExt' requires at least cuda-9.0"

spruneDense2csrNnz :: Handle -> Int -> Int -> DevicePtr Float -> Int -> Ptr Float -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> DevicePtr () -> IO ()
spruneDense2csrNnz _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneDense2csrNnz' requires at least cuda-9.0"

dpruneDense2csrNnz :: Handle -> Int -> Int -> DevicePtr Double -> Int -> Ptr Double -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> DevicePtr () -> IO ()
dpruneDense2csrNnz _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneDense2csrNnz' requires at least cuda-9.0"

hpruneDense2csrNnz :: Handle -> Int -> Int -> DevicePtr Half -> Int -> Ptr Half -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> DevicePtr () -> IO ()
hpruneDense2csrNnz _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneDense2csrNnz' requires at least cuda-9.0"

spruneDense2csr :: Handle -> Int -> Int -> DevicePtr Float -> Int -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
spruneDense2csr _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneDense2csr' requires at least cuda-9.0"

dpruneDense2csr :: Handle -> Int -> Int -> DevicePtr Double -> Int -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
dpruneDense2csr _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneDense2csr' requires at least cuda-9.0"

hpruneDense2csr :: Handle -> Int -> Int -> DevicePtr Half -> Int -> Ptr Half -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
hpruneDense2csr _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneDense2csr' requires at least cuda-9.0"

spruneCsr2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
spruneCsr2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneCsr2csr_bufferSizeExt' requires at least cuda-9.0"

dpruneCsr2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
dpruneCsr2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneCsr2csr_bufferSizeExt' requires at least cuda-9.0"

hpruneCsr2csr_bufferSizeExt :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Half -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
hpruneCsr2csr_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneCsr2csr_bufferSizeExt' requires at least cuda-9.0"

spruneCsr2csrNnz :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> Ptr Int32 -> DevicePtr () -> IO ()
spruneCsr2csrNnz _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneCsr2csrNnz' requires at least cuda-9.0"

dpruneCsr2csrNnz :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> Ptr Int32 -> DevicePtr () -> IO ()
dpruneCsr2csrNnz _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneCsr2csrNnz' requires at least cuda-9.0"

hpruneCsr2csrNnz :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Half -> MatrixDescriptor -> DevicePtr Half -> Ptr Int32 -> DevicePtr () -> IO ()
hpruneCsr2csrNnz _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneCsr2csrNnz' requires at least cuda-9.0"

spruneCsr2csr :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
spruneCsr2csr _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneCsr2csr' requires at least cuda-9.0"

dpruneCsr2csr :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
dpruneCsr2csr _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneCsr2csr' requires at least cuda-9.0"

hpruneCsr2csr :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Half -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
hpruneCsr2csr _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneCsr2csr' requires at least cuda-9.0"

spruneDense2csrByPercentage_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Float -> Int -> Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> Int -> IO ()
spruneDense2csrByPercentage_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneDense2csrByPercentage_bufferSizeExt' requires at least cuda-9.0"

dpruneDense2csrByPercentage_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Double -> Int -> Float -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> Int -> IO ()
dpruneDense2csrByPercentage_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneDense2csrByPercentage_bufferSizeExt' requires at least cuda-9.0"

hpruneDense2csrByPercentage_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Half -> Int -> Float -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> Int -> IO ()
hpruneDense2csrByPercentage_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneDense2csrByPercentage_bufferSizeExt' requires at least cuda-9.0"

spruneDense2csrNnzByPercentage :: Handle -> Int -> Int -> DevicePtr Float -> Int -> Float -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> Info_prune -> DevicePtr () -> IO ()
spruneDense2csrNnzByPercentage _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneDense2csrNnzByPercentage' requires at least cuda-9.0"

dpruneDense2csrNnzByPercentage :: Handle -> Int -> Int -> DevicePtr Double -> Int -> Float -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> Info_prune -> DevicePtr () -> IO ()
dpruneDense2csrNnzByPercentage _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneDense2csrNnzByPercentage' requires at least cuda-9.0"

hpruneDense2csrNnzByPercentage :: Handle -> Int -> Int -> DevicePtr Half -> Int -> Float -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> Info_prune -> DevicePtr () -> IO ()
hpruneDense2csrNnzByPercentage _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneDense2csrNnzByPercentage' requires at least cuda-9.0"

spruneDense2csrByPercentage :: Handle -> Int -> Int -> DevicePtr Float -> Int -> Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> DevicePtr () -> IO ()
spruneDense2csrByPercentage _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneDense2csrByPercentage' requires at least cuda-9.0"

dpruneDense2csrByPercentage :: Handle -> Int -> Int -> DevicePtr Double -> Int -> Float -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> DevicePtr () -> IO ()
dpruneDense2csrByPercentage _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneDense2csrByPercentage' requires at least cuda-9.0"

hpruneDense2csrByPercentage :: Handle -> Int -> Int -> DevicePtr Half -> Int -> Float -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> DevicePtr () -> IO ()
hpruneDense2csrByPercentage _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneDense2csrByPercentage' requires at least cuda-9.0"

spruneCsr2csrByPercentage_bufferSizeExt :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> Int -> IO ()
spruneCsr2csrByPercentage_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneCsr2csrByPercentage_bufferSizeExt' requires at least cuda-9.0"

dpruneCsr2csrByPercentage_bufferSizeExt :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> Int -> IO ()
dpruneCsr2csrByPercentage_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneCsr2csrByPercentage_bufferSizeExt' requires at least cuda-9.0"

hpruneCsr2csrByPercentage_bufferSizeExt :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> Int -> IO ()
hpruneCsr2csrByPercentage_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneCsr2csrByPercentage_bufferSizeExt' requires at least cuda-9.0"

spruneCsr2csrNnzByPercentage :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int -> Info_prune -> DevicePtr () -> IO ()
spruneCsr2csrNnzByPercentage _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneCsr2csrNnzByPercentage' requires at least cuda-9.0"

dpruneCsr2csrNnzByPercentage :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int -> Info_prune -> DevicePtr () -> IO ()
dpruneCsr2csrNnzByPercentage _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneCsr2csrNnzByPercentage' requires at least cuda-9.0"

hpruneCsr2csrNnzByPercentage :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int -> Info_prune -> DevicePtr () -> IO ()
hpruneCsr2csrNnzByPercentage _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneCsr2csrNnzByPercentage' requires at least cuda-9.0"

spruneCsr2csrByPercentage :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> DevicePtr () -> IO ()
spruneCsr2csrByPercentage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'spruneCsr2csrByPercentage' requires at least cuda-9.0"

dpruneCsr2csrByPercentage :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> DevicePtr () -> IO ()
dpruneCsr2csrByPercentage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dpruneCsr2csrByPercentage' requires at least cuda-9.0"

hpruneCsr2csrByPercentage :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Float -> MatrixDescriptor -> DevicePtr Half -> DevicePtr Int32 -> DevicePtr Int32 -> Info_prune -> DevicePtr () -> IO ()
hpruneCsr2csrByPercentage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'hpruneCsr2csrByPercentage' requires at least cuda-9.0"

snnz_compress :: Handle -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Int32 -> Float -> IO ()
snnz_compress _ _ _ _ _ _ _ _ = cusparseError "'snnz_compress' requires at least cuda-9.0"

dnnz_compress :: Handle -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Int32 -> Double -> IO ()
dnnz_compress _ _ _ _ _ _ _ _ = cusparseError "'dnnz_compress' requires at least cuda-9.0"

cnnz_compress :: Handle -> Int -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Int32 -> (Complex Float) -> IO ()
cnnz_compress _ _ _ _ _ _ _ _ = cusparseError "'cnnz_compress' requires at least cuda-9.0"

znnz_compress :: Handle -> Int -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Int32 -> (Complex Double) -> IO ()
znnz_compress _ _ _ _ _ _ _ _ = cusparseError "'znnz_compress' requires at least cuda-9.0"
#endif
#if CUDA_VERSION >= 10010

{-# INLINEABLE csr2cscEx2 #-}
{# fun unsafe cusparseCsr2cscEx2 as csr2cscEx2 { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr ()', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Type', cFromEnum `Action', cFromEnum `IndexBase', cFromEnum `Algorithm_csr2csc', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE csr2cscEx2_bufferSize #-}
{# fun unsafe cusparseCsr2cscEx2_bufferSize as csr2cscEx2_bufferSize { useHandle `Handle', `Int', `Int', `Int', useDevP `DevicePtr ()', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', cFromEnum `Type', cFromEnum `Action', cFromEnum `IndexBase', cFromEnum `Algorithm_csr2csc', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}
#else

csr2cscEx2 :: Handle -> Int -> Int -> Int -> DevicePtr () -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> DevicePtr Int32 -> DevicePtr Int32 -> Type -> Action -> IndexBase -> Algorithm_csr2csc -> DevicePtr () -> IO ()
csr2cscEx2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'csr2cscEx2' requires at least cuda-10.0"

csr2cscEx2_bufferSize :: Handle -> Int -> Int -> Int -> DevicePtr () -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> DevicePtr Int32 -> DevicePtr Int32 -> Type -> Action -> IndexBase -> Algorithm_csr2csc -> Int -> IO ()
csr2cscEx2_bufferSize _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'csr2cscEx2_bufferSize' requires at least cuda-10.0"
#endif
