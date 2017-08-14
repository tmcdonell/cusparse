--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Precondition
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuSPARSE function reference:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-preconditioners-reference>
--

module Foreign.CUDA.BLAS.Sparse.Precondition (

  Operation(..),
  Direction(..),
  Policy(..),
  MatrixDescriptor,
  Info,
  Info_csric02,
  Info_csrilu02,
  Info_bsric02,
  Info_bsrilu02,
  scsric0,
  dcsric0,
  ccsric0,
  zcsric0,
  scsric02_bufferSize,
  dcsric02_bufferSize,
  ccsric02_bufferSize,
  zcsric02_bufferSize,
  scsric02_analysis,
  dcsric02_analysis,
  ccsric02_analysis,
  zcsric02_analysis,
  scsric02,
  dcsric02,
  ccsric02,
  zcsric02,
  xcsric02_zeroPivot,
  scsrilu0,
  dcsrilu0,
  ccsrilu0,
  zcsrilu0,
  scsrilu02_numericBoost,
  dcsrilu02_numericBoost,
  ccsrilu02_numericBoost,
  zcsrilu02_numericBoost,
  scsrilu02_bufferSize,
  dcsrilu02_bufferSize,
  ccsrilu02_bufferSize,
  zcsrilu02_bufferSize,
  scsrilu02_analysis,
  dcsrilu02_analysis,
  ccsrilu02_analysis,
  zcsrilu02_analysis,
  scsrilu02,
  dcsrilu02,
  ccsrilu02,
  zcsrilu02,
  xcsrilu02_zeroPivot,
  sbsric02_bufferSize,
  dbsric02_bufferSize,
  cbsric02_bufferSize,
  zbsric02_bufferSize,
  sbsric02_analysis,
  dbsric02_analysis,
  cbsric02_analysis,
  zbsric02_analysis,
  sbsric02,
  dbsric02,
  cbsric02,
  zbsric02,
  xbsric02_zeroPivot,
  sbsrilu02_numericBoost,
  dbsrilu02_numericBoost,
  cbsrilu02_numericBoost,
  zbsrilu02_numericBoost,
  sbsrilu02_bufferSize,
  dbsrilu02_bufferSize,
  cbsrilu02_bufferSize,
  zbsrilu02_bufferSize,
  sbsrilu02_analysis,
  dbsrilu02_analysis,
  cbsrilu02_analysis,
  zbsrilu02_analysis,
  sbsrilu02,
  dbsrilu02,
  cbsrilu02,
  zbsrilu02,
  xbsrilu02_zeroPivot,
  sgtsv,
  dgtsv,
  cgtsv,
  zgtsv,
  sgtsv_nopivot,
  dgtsv_nopivot,
  cgtsv_nopivot,
  zgtsv_nopivot,
  sgtsvStridedBatch,
  dgtsvStridedBatch,
  cgtsvStridedBatch,
  zgtsvStridedBatch,
  csrilu0Ex,

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


{-# INLINEABLE scsric0 #-}
{# fun unsafe cusparseScsric0 as scsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE dcsric0 #-}
{# fun unsafe cusparseDcsric0 as dcsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE ccsric0 #-}
{# fun unsafe cusparseCcsric0 as ccsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE zcsric0 #-}
{# fun unsafe cusparseZcsric0 as zcsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE scsric02_bufferSize #-}
{# fun unsafe cusparseScsric02_bufferSize as scsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dcsric02_bufferSize #-}
{# fun unsafe cusparseDcsric02_bufferSize as dcsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE ccsric02_bufferSize #-}
{# fun unsafe cusparseCcsric02_bufferSize as ccsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zcsric02_bufferSize #-}
{# fun unsafe cusparseZcsric02_bufferSize as zcsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE scsric02_analysis #-}
{# fun unsafe cusparseScsric02_analysis as scsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsric02_analysis #-}
{# fun unsafe cusparseDcsric02_analysis as dcsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsric02_analysis #-}
{# fun unsafe cusparseCcsric02_analysis as ccsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsric02_analysis #-}
{# fun unsafe cusparseZcsric02_analysis as zcsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE scsric02 #-}
{# fun unsafe cusparseScsric02 as scsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsric02 #-}
{# fun unsafe cusparseDcsric02 as dcsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsric02 #-}
{# fun unsafe cusparseCcsric02 as ccsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsric02 #-}
{# fun unsafe cusparseZcsric02 as zcsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xcsric02_zeroPivot #-}
{# fun unsafe cusparseXcsric02_zeroPivot as xcsric02_zeroPivot { useHandle `Handle', useInfo_csric02 `Info_csric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE scsrilu0 #-}
{# fun unsafe cusparseScsrilu0 as scsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrilu0 #-}
{# fun unsafe cusparseDcsrilu0 as dcsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrilu0 #-}
{# fun unsafe cusparseCcsrilu0 as ccsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrilu0 #-}
{# fun unsafe cusparseZcsrilu0 as zcsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus* #}

{-# INLINEABLE scsrilu02_numericBoost #-}
{# fun unsafe cusparseScsrilu02_numericBoost as scsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrilu02_numericBoost #-}
{# fun unsafe cusparseDcsrilu02_numericBoost as dcsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrilu02_numericBoost #-}
{# fun unsafe cusparseCcsrilu02_numericBoost as ccsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrilu02_numericBoost #-}
{# fun unsafe cusparseZcsrilu02_numericBoost as zcsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Double)' } -> `()' checkStatus* #}

{-# INLINEABLE scsrilu02_bufferSize #-}
{# fun unsafe cusparseScsrilu02_bufferSize as scsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrilu02_bufferSize #-}
{# fun unsafe cusparseDcsrilu02_bufferSize as dcsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrilu02_bufferSize #-}
{# fun unsafe cusparseCcsrilu02_bufferSize as ccsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrilu02_bufferSize #-}
{# fun unsafe cusparseZcsrilu02_bufferSize as zcsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE scsrilu02_analysis #-}
{# fun unsafe cusparseScsrilu02_analysis as scsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrilu02_analysis #-}
{# fun unsafe cusparseDcsrilu02_analysis as dcsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrilu02_analysis #-}
{# fun unsafe cusparseCcsrilu02_analysis as ccsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrilu02_analysis #-}
{# fun unsafe cusparseZcsrilu02_analysis as zcsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE scsrilu02 #-}
{# fun unsafe cusparseScsrilu02 as scsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrilu02 #-}
{# fun unsafe cusparseDcsrilu02 as dcsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrilu02 #-}
{# fun unsafe cusparseCcsrilu02 as ccsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrilu02 #-}
{# fun unsafe cusparseZcsrilu02 as zcsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xcsrilu02_zeroPivot #-}
{# fun unsafe cusparseXcsrilu02_zeroPivot as xcsrilu02_zeroPivot { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sbsric02_bufferSize #-}
{# fun unsafe cusparseSbsric02_bufferSize as sbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dbsric02_bufferSize #-}
{# fun unsafe cusparseDbsric02_bufferSize as dbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cbsric02_bufferSize #-}
{# fun unsafe cusparseCbsric02_bufferSize as cbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zbsric02_bufferSize #-}
{# fun unsafe cusparseZbsric02_bufferSize as zbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sbsric02_analysis #-}
{# fun unsafe cusparseSbsric02_analysis as sbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dbsric02_analysis #-}
{# fun unsafe cusparseDbsric02_analysis as dbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cbsric02_analysis #-}
{# fun unsafe cusparseCbsric02_analysis as cbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zbsric02_analysis #-}
{# fun unsafe cusparseZbsric02_analysis as zbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE sbsric02 #-}
{# fun unsafe cusparseSbsric02 as sbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dbsric02 #-}
{# fun unsafe cusparseDbsric02 as dbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cbsric02 #-}
{# fun unsafe cusparseCbsric02 as cbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zbsric02 #-}
{# fun unsafe cusparseZbsric02 as zbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xbsric02_zeroPivot #-}
{# fun unsafe cusparseXbsric02_zeroPivot as xbsric02_zeroPivot { useHandle `Handle', useInfo_bsric02 `Info_bsric02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrilu02_numericBoost #-}
{# fun unsafe cusparseSbsrilu02_numericBoost as sbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Float' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrilu02_numericBoost #-}
{# fun unsafe cusparseDbsrilu02_numericBoost as dbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrilu02_numericBoost #-}
{# fun unsafe cusparseCbsrilu02_numericBoost as cbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Float)' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrilu02_numericBoost #-}
{# fun unsafe cusparseZbsrilu02_numericBoost as zbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Double)' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrilu02_bufferSize #-}
{# fun unsafe cusparseSbsrilu02_bufferSize as sbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrilu02_bufferSize #-}
{# fun unsafe cusparseDbsrilu02_bufferSize as dbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrilu02_bufferSize #-}
{# fun unsafe cusparseCbsrilu02_bufferSize as cbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrilu02_bufferSize #-}
{# fun unsafe cusparseZbsrilu02_bufferSize as zbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrilu02_analysis #-}
{# fun unsafe cusparseSbsrilu02_analysis as sbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrilu02_analysis #-}
{# fun unsafe cusparseDbsrilu02_analysis as dbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrilu02_analysis #-}
{# fun unsafe cusparseCbsrilu02_analysis as cbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrilu02_analysis #-}
{# fun unsafe cusparseZbsrilu02_analysis as zbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE sbsrilu02 #-}
{# fun unsafe cusparseSbsrilu02 as sbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE dbsrilu02 #-}
{# fun unsafe cusparseDbsrilu02 as dbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE cbsrilu02 #-}
{# fun unsafe cusparseCbsrilu02 as cbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE zbsrilu02 #-}
{# fun unsafe cusparseZbsrilu02 as zbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus* #}

{-# INLINEABLE xbsrilu02_zeroPivot #-}
{# fun unsafe cusparseXbsrilu02_zeroPivot as xbsrilu02_zeroPivot { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', castPtr `Ptr Int32' } -> `()' checkStatus* #}

{-# INLINEABLE sgtsv #-}
{# fun unsafe cusparseSgtsv as sgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgtsv #-}
{# fun unsafe cusparseDgtsv as dgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgtsv #-}
{# fun unsafe cusparseCgtsv as cgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgtsv #-}
{# fun unsafe cusparseZgtsv as zgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgtsv_nopivot #-}
{# fun unsafe cusparseSgtsv_nopivot as sgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgtsv_nopivot #-}
{# fun unsafe cusparseDgtsv_nopivot as dgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgtsv_nopivot #-}
{# fun unsafe cusparseCgtsv_nopivot as cgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgtsv_nopivot #-}
{# fun unsafe cusparseZgtsv_nopivot as zgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE sgtsvStridedBatch #-}
{# fun unsafe cusparseSgtsvStridedBatch as sgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE dgtsvStridedBatch #-}
{# fun unsafe cusparseDgtsvStridedBatch as dgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE cgtsvStridedBatch #-}
{# fun unsafe cusparseCgtsvStridedBatch as cgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int' } -> `()' checkStatus* #}

{-# INLINEABLE zgtsvStridedBatch #-}
{# fun unsafe cusparseZgtsvStridedBatch as zgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', `Int' } -> `()' checkStatus* #}
#if CUDA_VERSION >= 8000

{-# INLINEABLE csrilu0Ex #-}
{# fun unsafe cusparseCsrilu0Ex as csrilu0Ex { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', cFromEnum `Type' } -> `()' checkStatus* #}
#else

csrilu0Ex :: Handle -> Operation -> Int -> MatrixDescriptor -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> Info -> Type -> IO ()
csrilu0Ex _ _ _ _ _ _ _ _ _ _ = cusparseError "'csrilu0Ex' requires at least cuda-8.0"
#endif
