--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Level3
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuSPARSE Level-3 function reference:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-level-3-function-reference>
--

module Foreign.CUDA.BLAS.Sparse.Level3 (

  Operation(..),
  Policy(..),
  MatrixDescriptor,
  Info,
  Info_bsrsm2,
  Info_csrgemm2,
  Info_csrsm2,
  scsrmm,
  dcsrmm,
  ccsrmm,
  zcsrmm,
  scsrmm2,
  dcsrmm2,
  ccsrmm2,
  zcsrmm2,
  scsrsm_analysis,
  dcsrsm_analysis,
  ccsrsm_analysis,
  zcsrsm_analysis,
  scsrsm_solve,
  dcsrsm_solve,
  ccsrsm_solve,
  zcsrsm_solve,
  sbsrmm,
  dbsrmm,
  cbsrmm,
  zbsrmm,
  sbsrsm2_bufferSize,
  dbsrsm2_bufferSize,
  cbsrsm2_bufferSize,
  zbsrsm2_bufferSize,
  sbsrsm2_analysis,
  dbsrsm2_analysis,
  cbsrsm2_analysis,
  zbsrsm2_analysis,
  sbsrsm2_solve,
  dbsrsm2_solve,
  cbsrsm2_solve,
  zbsrsm2_solve,
  xbsrsm2_zeroPivot,
  xcsrgeamNnz,
  scsrgeam,
  dcsrgeam,
  ccsrgeam,
  zcsrgeam,
  xcsrgemmNnz,
  scsrgemm,
  dcsrgemm,
  ccsrgemm,
  zcsrgemm,
  scsrgemm2_bufferSizeExt,
  dcsrgemm2_bufferSizeExt,
  ccsrgemm2_bufferSizeExt,
  zcsrgemm2_bufferSizeExt,
  xcsrgemm2Nnz,
  scsrgemm2,
  dcsrgemm2,
  ccsrgemm2,
  zcsrgemm2,
  scsrsm2_bufferSizeExt,
  dcsrsm2_bufferSizeExt,
  ccsrsm2_bufferSizeExt,
  zcsrsm2_bufferSizeExt,
  scsrsm2_analysis,
  dcsrsm2_analysis,
  ccsrsm2_analysis,
  zcsrsm2_analysis,
  scsrsm2_solve,
  dcsrsm2_solve,
  ccsrsm2_solve,
  zcsrsm2_solve,
  xcsrsm2_zeroPivot,
  sgemmi,
  dgemmi,
  cgemmi,
  zgemmi,
  xcsrgeam2Nnz,
  scsrgeam2_bufferSizeExt,
  dcsrgeam2_bufferSizeExt,
  ccsrgeam2_bufferSizeExt,
  zcsrgeam2_bufferSizeExt,
  scsrgeam2,
  dcsrgeam2,
  ccsrgeam2,
  zcsrgeam2,

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


{-# INLINEABLE scsrmm #-}
{# fun unsafe cusparseScsrmm as scsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrmm #-}
{# fun unsafe cusparseDcsrmm as dcsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrmm #-}
{# fun unsafe cusparseCcsrmm as ccsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrmm #-}
{# fun unsafe cusparseZcsrmm as zcsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrmm2 #-}
{# fun unsafe cusparseScsrmm2 as scsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrmm2 #-}
{# fun unsafe cusparseDcsrmm2 as dcsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrmm2 #-}
{# fun unsafe cusparseCcsrmm2 as ccsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrmm2 #-}
{# fun unsafe cusparseZcsrmm2 as zcsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsm_analysis #-}
{# fun unsafe cusparseScsrsm_analysis as scsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsm_analysis #-}
{# fun unsafe cusparseDcsrsm_analysis as dcsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsm_analysis #-}
{# fun unsafe cusparseCcsrsm_analysis as ccsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsm_analysis #-}
{# fun unsafe cusparseZcsrsm_analysis as zcsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsm_solve #-}
{# fun unsafe cusparseScsrsm_solve as scsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsm_solve #-}
{# fun unsafe cusparseDcsrsm_solve as dcsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsm_solve #-}
{# fun unsafe cusparseCcsrsm_solve as ccsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsm_solve #-}
{# fun unsafe cusparseZcsrsm_solve as zcsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrmm #-}
{# fun unsafe cusparseSbsrmm as sbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrmm #-}
{# fun unsafe cusparseDbsrmm as dbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrmm #-}
{# fun unsafe cusparseCbsrmm as cbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrmm #-}
{# fun unsafe cusparseZbsrmm as zbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrsm2_bufferSize #-}
{# fun unsafe cusparseSbsrsm2_bufferSize as sbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrsm2_bufferSize #-}
{# fun unsafe cusparseDbsrsm2_bufferSize as dbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrsm2_bufferSize #-}
{# fun unsafe cusparseCbsrsm2_bufferSize as cbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrsm2_bufferSize #-}
{# fun unsafe cusparseZbsrsm2_bufferSize as zbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrsm2_analysis #-}
{# fun unsafe cusparseSbsrsm2_analysis as sbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrsm2_analysis #-}
{# fun unsafe cusparseDbsrsm2_analysis as dbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrsm2_analysis #-}
{# fun unsafe cusparseCbsrsm2_analysis as cbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrsm2_analysis #-}
{# fun unsafe cusparseZbsrsm2_analysis as zbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrsm2_solve #-}
{# fun unsafe cusparseSbsrsm2_solve as sbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrsm2_solve #-}
{# fun unsafe cusparseDbsrsm2_solve as dbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrsm2_solve #-}
{# fun unsafe cusparseCbsrsm2_solve as cbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrsm2_solve #-}
{# fun unsafe cusparseZbsrsm2_solve as zbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xbsrsm2_zeroPivot #-}
{# fun unsafe cusparseXbsrsm2_zeroPivot as xbsrsm2_zeroPivot { useHandle `Handle', useInfo_bsrsm2 `Info_bsrsm2', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrgeamNnz #-}
{# fun unsafe cusparseXcsrgeamNnz as xcsrgeamNnz { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrgeam #-}
{# fun unsafe cusparseScsrgeam as scsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrgeam #-}
{# fun unsafe cusparseDcsrgeam as dcsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrgeam #-}
{# fun unsafe cusparseCcsrgeam as ccsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrgeam #-}
{# fun unsafe cusparseZcsrgeam as zcsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrgemmNnz #-}
{# fun unsafe cusparseXcsrgemmNnz as xcsrgemmNnz { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrgemm #-}
{# fun unsafe cusparseScsrgemm as scsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrgemm #-}
{# fun unsafe cusparseDcsrgemm as dcsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrgemm #-}
{# fun unsafe cusparseCcsrgemm as ccsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrgemm #-}
{# fun unsafe cusparseZcsrgemm as zcsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32' } -> `()' checkStatus*- #}
#if CUDA_VERSION >= 7000

{-# INLINEABLE scsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseScsrgemm2_bufferSizeExt as scsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseDcsrgemm2_bufferSizeExt as dcsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseCcsrgemm2_bufferSizeExt as ccsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseZcsrgemm2_bufferSizeExt as zcsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrgemm2Nnz #-}
{# fun unsafe cusparseXcsrgemm2Nnz as xcsrgemm2Nnz { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrgemm2 #-}
{# fun unsafe cusparseScsrgemm2 as scsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrgemm2 #-}
{# fun unsafe cusparseDcsrgemm2 as dcsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrgemm2 #-}
{# fun unsafe cusparseCcsrgemm2 as ccsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrgemm2 #-}
{# fun unsafe cusparseZcsrgemm2 as zcsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}
#else

scsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> Int -> IO ()
scsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrgemm2_bufferSizeExt' requires at least cuda-7.0"

dcsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> Int -> IO ()
dcsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrgemm2_bufferSizeExt' requires at least cuda-7.0"

ccsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> Int -> IO ()
ccsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrgemm2_bufferSizeExt' requires at least cuda-7.0"

zcsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> Int -> IO ()
zcsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrgemm2_bufferSizeExt' requires at least cuda-7.0"

xcsrgemm2Nnz :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> Info_csrgemm2 -> DevicePtr () -> IO ()
xcsrgemm2Nnz _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'xcsrgemm2Nnz' requires at least cuda-7.0"

scsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> DevicePtr () -> IO ()
scsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrgemm2' requires at least cuda-7.0"

dcsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> DevicePtr () -> IO ()
dcsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrgemm2' requires at least cuda-7.0"

ccsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> DevicePtr () -> IO ()
ccsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrgemm2' requires at least cuda-7.0"

zcsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Info_csrgemm2 -> DevicePtr () -> IO ()
zcsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrgemm2' requires at least cuda-7.0"
#endif
#if CUDA_VERSION >= 9020

{-# INLINEABLE scsrsm2_bufferSizeExt #-}
{# fun unsafe cusparseScsrsm2_bufferSizeExt as scsrsm2_bufferSizeExt { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsm2_bufferSizeExt #-}
{# fun unsafe cusparseDcsrsm2_bufferSizeExt as dcsrsm2_bufferSizeExt { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsm2_bufferSizeExt #-}
{# fun unsafe cusparseCcsrsm2_bufferSizeExt as ccsrsm2_bufferSizeExt { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsm2_bufferSizeExt #-}
{# fun unsafe cusparseZcsrsm2_bufferSizeExt as zcsrsm2_bufferSizeExt { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsm2_analysis #-}
{# fun unsafe cusparseScsrsm2_analysis as scsrsm2_analysis { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsm2_analysis #-}
{# fun unsafe cusparseDcsrsm2_analysis as dcsrsm2_analysis { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsm2_analysis #-}
{# fun unsafe cusparseCcsrsm2_analysis as ccsrsm2_analysis { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsm2_analysis #-}
{# fun unsafe cusparseZcsrsm2_analysis as zcsrsm2_analysis { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsm2_solve #-}
{# fun unsafe cusparseScsrsm2_solve as scsrsm2_solve { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsm2_solve #-}
{# fun unsafe cusparseDcsrsm2_solve as dcsrsm2_solve { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsm2_solve #-}
{# fun unsafe cusparseCcsrsm2_solve as ccsrsm2_solve { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsm2_solve #-}
{# fun unsafe cusparseZcsrsm2_solve as zcsrsm2_solve { useHandle `Handle', `Int', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', `Int', useInfo_csrsm2 `Info_csrsm2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrsm2_zeroPivot #-}
{# fun unsafe cusparseXcsrsm2_zeroPivot as xcsrsm2_zeroPivot { useHandle `Handle', useInfo_csrsm2 `Info_csrsm2', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE sgemmi #-}
{# fun unsafe cusparseSgemmi as sgemmi { useHandle `Handle', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dgemmi #-}
{# fun unsafe cusparseDgemmi as dgemmi { useHandle `Handle', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE cgemmi #-}
{# fun unsafe cusparseCgemmi as cgemmi { useHandle `Handle', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zgemmi #-}
{# fun unsafe cusparseZgemmi as zgemmi { useHandle `Handle', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrgeam2Nnz #-}
{# fun unsafe cusparseXcsrgeam2Nnz as xcsrgeam2Nnz { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int32', castPtr `Ptr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrgeam2_bufferSizeExt #-}
{# fun unsafe cusparseScsrgeam2_bufferSizeExt as scsrgeam2_bufferSizeExt { useHandle `Handle', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrgeam2_bufferSizeExt #-}
{# fun unsafe cusparseDcsrgeam2_bufferSizeExt as dcsrgeam2_bufferSizeExt { useHandle `Handle', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrgeam2_bufferSizeExt #-}
{# fun unsafe cusparseCcsrgeam2_bufferSizeExt as ccsrgeam2_bufferSizeExt { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrgeam2_bufferSizeExt #-}
{# fun unsafe cusparseZcsrgeam2_bufferSizeExt as zcsrgeam2_bufferSizeExt { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsrgeam2 #-}
{# fun unsafe cusparseScsrgeam2 as scsrgeam2 { useHandle `Handle', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrgeam2 #-}
{# fun unsafe cusparseDcsrgeam2 as dcsrgeam2 { useHandle `Handle', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrgeam2 #-}
{# fun unsafe cusparseCcsrgeam2 as ccsrgeam2 { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrgeam2 #-}
{# fun unsafe cusparseZcsrgeam2 as zcsrgeam2 { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}
#else

scsrsm2_bufferSizeExt :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Float -> Int -> Info_csrsm2 -> Policy -> Int -> IO ()
scsrsm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrsm2_bufferSizeExt' requires at least cuda-9.0"

dcsrsm2_bufferSizeExt :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Double -> Int -> Info_csrsm2 -> Policy -> Int -> IO ()
dcsrsm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrsm2_bufferSizeExt' requires at least cuda-9.0"

ccsrsm2_bufferSizeExt :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Float) -> Int -> Info_csrsm2 -> Policy -> Int -> IO ()
ccsrsm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrsm2_bufferSizeExt' requires at least cuda-9.0"

zcsrsm2_bufferSizeExt :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Double) -> Int -> Info_csrsm2 -> Policy -> Int -> IO ()
zcsrsm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrsm2_bufferSizeExt' requires at least cuda-9.0"

scsrsm2_analysis :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Float -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
scsrsm2_analysis _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrsm2_analysis' requires at least cuda-9.0"

dcsrsm2_analysis :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Double -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
dcsrsm2_analysis _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrsm2_analysis' requires at least cuda-9.0"

ccsrsm2_analysis :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Float) -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
ccsrsm2_analysis _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrsm2_analysis' requires at least cuda-9.0"

zcsrsm2_analysis :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Double) -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
zcsrsm2_analysis _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrsm2_analysis' requires at least cuda-9.0"

scsrsm2_solve :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Float -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
scsrsm2_solve _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrsm2_solve' requires at least cuda-9.0"

dcsrsm2_solve :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Double -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
dcsrsm2_solve _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrsm2_solve' requires at least cuda-9.0"

ccsrsm2_solve :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Float) -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
ccsrsm2_solve _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrsm2_solve' requires at least cuda-9.0"

zcsrsm2_solve :: Handle -> Int -> Operation -> Operation -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Double) -> Int -> Info_csrsm2 -> Policy -> DevicePtr () -> IO ()
zcsrsm2_solve _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrsm2_solve' requires at least cuda-9.0"

xcsrsm2_zeroPivot :: Handle -> Info_csrsm2 -> Ptr Int32 -> IO ()
xcsrsm2_zeroPivot _ _ _ = cusparseError "'xcsrsm2_zeroPivot' requires at least cuda-9.0"

sgemmi :: Handle -> Int -> Int -> Int -> Int -> Ptr Float -> DevicePtr Float -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> DevicePtr Float -> Int -> IO ()
sgemmi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'sgemmi' requires at least cuda-9.0"

dgemmi :: Handle -> Int -> Int -> Int -> Int -> Ptr Double -> DevicePtr Double -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> DevicePtr Double -> Int -> IO ()
dgemmi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dgemmi' requires at least cuda-9.0"

cgemmi :: Handle -> Int -> Int -> Int -> Int -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> IO ()
cgemmi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'cgemmi' requires at least cuda-9.0"

zgemmi :: Handle -> Int -> Int -> Int -> Int -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> Int -> IO ()
zgemmi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zgemmi' requires at least cuda-9.0"

xcsrgeam2Nnz :: Handle -> Int -> Int -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> Int -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Int32 -> Ptr Int32 -> DevicePtr () -> IO ()
xcsrgeam2Nnz _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'xcsrgeam2Nnz' requires at least cuda-9.0"

scsrgeam2_bufferSizeExt :: Handle -> Int -> Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
scsrgeam2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrgeam2_bufferSizeExt' requires at least cuda-9.0"

dcsrgeam2_bufferSizeExt :: Handle -> Int -> Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
dcsrgeam2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrgeam2_bufferSizeExt' requires at least cuda-9.0"

ccsrgeam2_bufferSizeExt :: Handle -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
ccsrgeam2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrgeam2_bufferSizeExt' requires at least cuda-9.0"

zcsrgeam2_bufferSizeExt :: Handle -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Int -> IO ()
zcsrgeam2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrgeam2_bufferSizeExt' requires at least cuda-9.0"

scsrgeam2 :: Handle -> Int -> Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
scsrgeam2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrgeam2' requires at least cuda-9.0"

dcsrgeam2 :: Handle -> Int -> Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
dcsrgeam2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrgeam2' requires at least cuda-9.0"

ccsrgeam2 :: Handle -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
ccsrgeam2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrgeam2' requires at least cuda-9.0"

zcsrgeam2 :: Handle -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> IO ()
zcsrgeam2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrgeam2' requires at least cuda-9.0"
#endif
