--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Level3
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
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
{# fun unsafe cusparseScsrmm as scsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrmm #-}
{# fun unsafe cusparseDcsrmm as dcsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrmm #-}
{# fun unsafe cusparseCcsrmm as ccsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrmm #-}
{# fun unsafe cusparseZcsrmm as zcsrmm { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE scsrmm2 #-}
{# fun unsafe cusparseScsrmm2 as scsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrmm2 #-}
{# fun unsafe cusparseDcsrmm2 as dcsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrmm2 #-}
{# fun unsafe cusparseCcsrmm2 as ccsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrmm2 #-}
{# fun unsafe cusparseZcsrmm2 as zcsrmm2 { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE scsrsm_analysis #-}
{# fun unsafe cusparseScsrsm_analysis as scsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrsm_analysis #-}
{# fun unsafe cusparseDcsrsm_analysis as dcsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrsm_analysis #-}
{# fun unsafe cusparseCcsrsm_analysis as ccsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrsm_analysis #-}
{# fun unsafe cusparseZcsrsm_analysis as zcsrsm_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE scsrsm_solve #-}
{# fun unsafe cusparseScsrsm_solve as scsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrsm_solve #-}
{# fun unsafe cusparseDcsrsm_solve as dcsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrsm_solve #-}
{# fun unsafe cusparseCcsrsm_solve as ccsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrsm_solve #-}
{# fun unsafe cusparseZcsrsm_solve as zcsrsm_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo `Info', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrmm #-}
{# fun unsafe cusparseSbsrmm as sbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useDevP `DevicePtr Float', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrmm #-}
{# fun unsafe cusparseDbsrmm as dbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useDevP `DevicePtr Double', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrmm #-}
{# fun unsafe cusparseCbsrmm as cbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useDevP `DevicePtr (Complex Float)', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrmm #-}
{# fun unsafe cusparseZbsrmm as zbsrmm { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useDevP `DevicePtr (Complex Double)', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrsm2_bufferSize #-}
{# fun unsafe cusparseSbsrsm2_bufferSize as sbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', castPtr `Ptr Int' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrsm2_bufferSize #-}
{# fun unsafe cusparseDbsrsm2_bufferSize as dbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', castPtr `Ptr Int' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrsm2_bufferSize #-}
{# fun unsafe cusparseCbsrsm2_bufferSize as cbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', castPtr `Ptr Int' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrsm2_bufferSize #-}
{# fun unsafe cusparseZbsrsm2_bufferSize as zbsrsm2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', castPtr `Ptr Int' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrsm2_analysis #-}
{# fun unsafe cusparseSbsrsm2_analysis as sbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrsm2_analysis #-}
{# fun unsafe cusparseDbsrsm2_analysis as dbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrsm2_analysis #-}
{# fun unsafe cusparseCbsrsm2_analysis as cbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrsm2_analysis #-}
{# fun unsafe cusparseZbsrsm2_analysis as zbsrsm2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrsm2_solve #-}
{# fun unsafe cusparseSbsrsm2_solve as sbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr Float', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrsm2_solve #-}
{# fun unsafe cusparseDbsrsm2_solve as dbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr Double', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrsm2_solve #-}
{# fun unsafe cusparseCbsrsm2_solve as cbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr (Complex Float)', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrsm2_solve #-}
{# fun unsafe cusparseZbsrsm2_solve as zbsrsm2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', `Int', useInfo_bsrsm2 `Info_bsrsm2', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr (Complex Double)', `Int', cFromEnum `Policy', castPtr `Ptr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xbsrsm2_zeroPivot #-}
{# fun unsafe cusparseXbsrsm2_zeroPivot as xbsrsm2_zeroPivot { useHandle `Handle', useInfo_bsrsm2 `Info_bsrsm2', castPtr `Ptr Int' } -> `()' checkStatus* #}

{-# INLINEABLE xcsrgeamNnz #-}
{# fun unsafe cusparseXcsrgeamNnz as xcsrgeamNnz { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int', castPtr `Ptr Int' } -> `()' checkStatus* #}

{-# INLINEABLE scsrgeam #-}
{# fun unsafe cusparseScsrgeam as scsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrgeam #-}
{# fun unsafe cusparseDcsrgeam as dcsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrgeam #-}
{# fun unsafe cusparseCcsrgeam as ccsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrgeam #-}
{# fun unsafe cusparseZcsrgeam as zcsrgeam { useHandle `Handle', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}

{-# INLINEABLE xcsrgemmNnz #-}
{# fun unsafe cusparseXcsrgemmNnz as xcsrgemmNnz { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int', castPtr `Ptr Int' } -> `()' checkStatus* #}

{-# INLINEABLE scsrgemm #-}
{# fun unsafe cusparseScsrgemm as scsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrgemm #-}
{# fun unsafe cusparseDcsrgemm as dcsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrgemm #-}
{# fun unsafe cusparseCcsrgemm as ccsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrgemm #-}
{# fun unsafe cusparseZcsrgemm as zcsrgemm { useHandle `Handle', cFromEnum `Operation', cFromEnum `Operation', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int' } -> `()' checkStatus* #}
#if CUDA_VERSION >= 7000

{-# INLINEABLE scsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseScsrgemm2_bufferSizeExt as scsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseDcsrgemm2_bufferSizeExt as dcsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseCcsrgemm2_bufferSizeExt as ccsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrgemm2_bufferSizeExt #-}
{# fun unsafe cusparseZcsrgemm2_bufferSizeExt as zcsrgemm2_bufferSizeExt { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', castPtr `Ptr Int64' } -> `()' checkStatus* #}

{-# INLINEABLE xcsrgemm2Nnz #-}
{# fun unsafe cusparseXcsrgemm2Nnz as xcsrgemm2Nnz { useHandle `Handle', `Int', `Int', `Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Int', castPtr `Ptr Int', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE scsrgemm2 #-}
{# fun unsafe cusparseScsrgemm2 as scsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrgemm2 #-}
{# fun unsafe cusparseDcsrgemm2 as dcsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrgemm2 #-}
{# fun unsafe cusparseCcsrgemm2 as ccsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrgemm2 #-}
{# fun unsafe cusparseZcsrgemm2 as zcsrgemm2 { useHandle `Handle', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_csrgemm2 `Info_csrgemm2', useDevP `DevicePtr ()' } -> `()' checkStatus* #}
#else

scsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> Ptr Int64 -> IO ()
scsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrgemm2_bufferSizeExt' requires at least cuda-7.0"

dcsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> Ptr Int64 -> IO ()
dcsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrgemm2_bufferSizeExt' requires at least cuda-7.0"

ccsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> Ptr Int64 -> IO ()
ccsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrgemm2_bufferSizeExt' requires at least cuda-7.0"

zcsrgemm2_bufferSizeExt :: Handle -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> Ptr Int64 -> IO ()
zcsrgemm2_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrgemm2_bufferSizeExt' requires at least cuda-7.0"

xcsrgemm2Nnz :: Handle -> Int -> Int -> Int -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> DevicePtr Int -> Ptr Int -> Info_csrgemm2 -> DevicePtr () -> IO ()
xcsrgemm2Nnz _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'xcsrgemm2Nnz' requires at least cuda-7.0"

scsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int -> DevicePtr Int -> Ptr Float -> MatrixDescriptor -> Int -> DevicePtr Float -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> DevicePtr () -> IO ()
scsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrgemm2' requires at least cuda-7.0"

dcsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int -> DevicePtr Int -> Ptr Double -> MatrixDescriptor -> Int -> DevicePtr Double -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> DevicePtr () -> IO ()
dcsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrgemm2' requires at least cuda-7.0"

ccsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int -> DevicePtr Int -> Ptr (Complex Float) -> MatrixDescriptor -> Int -> DevicePtr (Complex Float) -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> DevicePtr () -> IO ()
ccsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrgemm2' requires at least cuda-7.0"

zcsrgemm2 :: Handle -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int -> DevicePtr Int -> Ptr (Complex Double) -> MatrixDescriptor -> Int -> DevicePtr (Complex Double) -> DevicePtr Int -> DevicePtr Int -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int -> DevicePtr Int -> Info_csrgemm2 -> DevicePtr () -> IO ()
zcsrgemm2 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrgemm2' requires at least cuda-7.0"
#endif
