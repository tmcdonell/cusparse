--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Precondition
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
  sgtsv2_bufferSizeExt,
  dgtsv2_bufferSizeExt,
  cgtsv2_bufferSizeExt,
  zgtsv2_bufferSizeExt,
  sgtsv2,
  dgtsv2,
  cgtsv2,
  zgtsv2,
  sgtsv2_nopivot_bufferSizeExt,
  dgtsv2_nopivot_bufferSizeExt,
  cgtsv2_nopivot_bufferSizeExt,
  zgtsv2_nopivot_bufferSizeExt,
  sgtsv2_nopivot,
  dgtsv2_nopivot,
  cgtsv2_nopivot,
  zgtsv2_nopivot,
  sgtsv2StridedBatch_bufferSizeExt,
  dgtsv2StridedBatch_bufferSizeExt,
  cgtsv2StridedBatch_bufferSizeExt,
  zgtsv2StridedBatch_bufferSizeExt,
  sgtsv2StridedBatch,
  dgtsv2StridedBatch,
  cgtsv2StridedBatch,
  zgtsv2StridedBatch,
  sgtsvInterleavedBatch_bufferSizeExt,
  dgtsvInterleavedBatch_bufferSizeExt,
  cgtsvInterleavedBatch_bufferSizeExt,
  zgtsvInterleavedBatch_bufferSizeExt,
  sgtsvInterleavedBatch,
  dgtsvInterleavedBatch,
  cgtsvInterleavedBatch,
  zgtsvInterleavedBatch,
  sgpsvInterleavedBatch_bufferSizeExt,
  dgpsvInterleavedBatch_bufferSizeExt,
  cgpsvInterleavedBatch_bufferSizeExt,
  zgpsvInterleavedBatch_bufferSizeExt,
  sgpsvInterleavedBatch,
  dgpsvInterleavedBatch,
  cgpsvInterleavedBatch,
  zgpsvInterleavedBatch,

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
{# fun unsafe cusparseScsric0 as scsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsric0 #-}
{# fun unsafe cusparseDcsric0 as dcsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsric0 #-}
{# fun unsafe cusparseCcsric0 as ccsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsric0 #-}
{# fun unsafe cusparseZcsric0 as zcsric0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE scsric02_bufferSize #-}
{# fun unsafe cusparseScsric02_bufferSize as scsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsric02_bufferSize #-}
{# fun unsafe cusparseDcsric02_bufferSize as dcsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsric02_bufferSize #-}
{# fun unsafe cusparseCcsric02_bufferSize as ccsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsric02_bufferSize #-}
{# fun unsafe cusparseZcsric02_bufferSize as zcsric02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsric02_analysis #-}
{# fun unsafe cusparseScsric02_analysis as scsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsric02_analysis #-}
{# fun unsafe cusparseDcsric02_analysis as dcsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsric02_analysis #-}
{# fun unsafe cusparseCcsric02_analysis as ccsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsric02_analysis #-}
{# fun unsafe cusparseZcsric02_analysis as zcsric02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsric02 #-}
{# fun unsafe cusparseScsric02 as scsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsric02 #-}
{# fun unsafe cusparseDcsric02 as dcsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsric02 #-}
{# fun unsafe cusparseCcsric02 as ccsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsric02 #-}
{# fun unsafe cusparseZcsric02 as zcsric02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csric02 `Info_csric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsric02_zeroPivot #-}
{# fun unsafe cusparseXcsric02_zeroPivot as xcsric02_zeroPivot { useHandle `Handle', useInfo_csric02 `Info_csric02', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrilu0 #-}
{# fun unsafe cusparseScsrilu0 as scsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrilu0 #-}
{# fun unsafe cusparseDcsrilu0 as dcsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrilu0 #-}
{# fun unsafe cusparseCcsrilu0 as ccsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrilu0 #-}
{# fun unsafe cusparseZcsrilu0 as zcsrilu0 { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrilu02_numericBoost #-}
{# fun unsafe cusparseScsrilu02_numericBoost as scsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrilu02_numericBoost #-}
{# fun unsafe cusparseDcsrilu02_numericBoost as dcsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrilu02_numericBoost #-}
{# fun unsafe cusparseCcsrilu02_numericBoost as ccsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrilu02_numericBoost #-}
{# fun unsafe cusparseZcsrilu02_numericBoost as zcsrilu02_numericBoost { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrilu02_bufferSize #-}
{# fun unsafe cusparseScsrilu02_bufferSize as scsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrilu02_bufferSize #-}
{# fun unsafe cusparseDcsrilu02_bufferSize as dcsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrilu02_bufferSize #-}
{# fun unsafe cusparseCcsrilu02_bufferSize as ccsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrilu02_bufferSize #-}
{# fun unsafe cusparseZcsrilu02_bufferSize as zcsrilu02_bufferSize { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsrilu02_analysis #-}
{# fun unsafe cusparseScsrilu02_analysis as scsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrilu02_analysis #-}
{# fun unsafe cusparseDcsrilu02_analysis as dcsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrilu02_analysis #-}
{# fun unsafe cusparseCcsrilu02_analysis as ccsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrilu02_analysis #-}
{# fun unsafe cusparseZcsrilu02_analysis as zcsrilu02_analysis { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrilu02 #-}
{# fun unsafe cusparseScsrilu02 as scsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrilu02 #-}
{# fun unsafe cusparseDcsrilu02 as dcsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrilu02 #-}
{# fun unsafe cusparseCcsrilu02 as ccsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrilu02 #-}
{# fun unsafe cusparseZcsrilu02 as zcsrilu02 { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrilu02 `Info_csrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrilu02_zeroPivot #-}
{# fun unsafe cusparseXcsrilu02_zeroPivot as xcsrilu02_zeroPivot { useHandle `Handle', useInfo_csrilu02 `Info_csrilu02', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsric02_bufferSize #-}
{# fun unsafe cusparseSbsric02_bufferSize as sbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dbsric02_bufferSize #-}
{# fun unsafe cusparseDbsric02_bufferSize as dbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cbsric02_bufferSize #-}
{# fun unsafe cusparseCbsric02_bufferSize as cbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zbsric02_bufferSize #-}
{# fun unsafe cusparseZbsric02_bufferSize as zbsric02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sbsric02_analysis #-}
{# fun unsafe cusparseSbsric02_analysis as sbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsric02_analysis #-}
{# fun unsafe cusparseDbsric02_analysis as dbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsric02_analysis #-}
{# fun unsafe cusparseCbsric02_analysis as cbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsric02_analysis #-}
{# fun unsafe cusparseZbsric02_analysis as zbsric02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsric02 #-}
{# fun unsafe cusparseSbsric02 as sbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsric02 #-}
{# fun unsafe cusparseDbsric02 as dbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsric02 #-}
{# fun unsafe cusparseCbsric02 as cbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsric02 #-}
{# fun unsafe cusparseZbsric02 as zbsric02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsric02 `Info_bsric02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xbsric02_zeroPivot #-}
{# fun unsafe cusparseXbsric02_zeroPivot as xbsric02_zeroPivot { useHandle `Handle', useInfo_bsric02 `Info_bsric02', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrilu02_numericBoost #-}
{# fun unsafe cusparseSbsrilu02_numericBoost as sbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrilu02_numericBoost #-}
{# fun unsafe cusparseDbsrilu02_numericBoost as dbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrilu02_numericBoost #-}
{# fun unsafe cusparseCbsrilu02_numericBoost as cbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrilu02_numericBoost #-}
{# fun unsafe cusparseZbsrilu02_numericBoost as zbsrilu02_numericBoost { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', `Int', castPtr `Ptr Double', castPtr `Ptr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrilu02_bufferSize #-}
{# fun unsafe cusparseSbsrilu02_bufferSize as sbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrilu02_bufferSize #-}
{# fun unsafe cusparseDbsrilu02_bufferSize as dbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrilu02_bufferSize #-}
{# fun unsafe cusparseCbsrilu02_bufferSize as cbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrilu02_bufferSize #-}
{# fun unsafe cusparseZbsrilu02_bufferSize as zbsrilu02_bufferSize { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrilu02_analysis #-}
{# fun unsafe cusparseSbsrilu02_analysis as sbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrilu02_analysis #-}
{# fun unsafe cusparseDbsrilu02_analysis as dbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrilu02_analysis #-}
{# fun unsafe cusparseCbsrilu02_analysis as cbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrilu02_analysis #-}
{# fun unsafe cusparseZbsrilu02_analysis as zbsrilu02_analysis { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrilu02 #-}
{# fun unsafe cusparseSbsrilu02 as sbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrilu02 #-}
{# fun unsafe cusparseDbsrilu02 as dbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrilu02 #-}
{# fun unsafe cusparseCbsrilu02 as cbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrilu02 #-}
{# fun unsafe cusparseZbsrilu02 as zbsrilu02 { useHandle `Handle', cFromEnum `Direction', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrilu02 `Info_bsrilu02', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xbsrilu02_zeroPivot #-}
{# fun unsafe cusparseXbsrilu02_zeroPivot as xbsrilu02_zeroPivot { useHandle `Handle', useInfo_bsrilu02 `Info_bsrilu02', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsv #-}
{# fun unsafe cusparseSgtsv as sgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv #-}
{# fun unsafe cusparseDgtsv as dgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv #-}
{# fun unsafe cusparseCgtsv as cgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv #-}
{# fun unsafe cusparseZgtsv as zgtsv { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsv_nopivot #-}
{# fun unsafe cusparseSgtsv_nopivot as sgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv_nopivot #-}
{# fun unsafe cusparseDgtsv_nopivot as dgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv_nopivot #-}
{# fun unsafe cusparseCgtsv_nopivot as cgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv_nopivot #-}
{# fun unsafe cusparseZgtsv_nopivot as zgtsv_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsvStridedBatch #-}
{# fun unsafe cusparseSgtsvStridedBatch as sgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsvStridedBatch #-}
{# fun unsafe cusparseDgtsvStridedBatch as dgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsvStridedBatch #-}
{# fun unsafe cusparseCgtsvStridedBatch as cgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int' } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsvStridedBatch #-}
{# fun unsafe cusparseZgtsvStridedBatch as zgtsvStridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', `Int' } -> `()' checkStatus*- #}
#if CUDA_VERSION >= 8000

{-# INLINEABLE csrilu0Ex #-}
{# fun unsafe cusparseCsrilu0Ex as csrilu0Ex { useHandle `Handle', cFromEnum `Operation', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', cFromEnum `Type' } -> `()' checkStatus*- #}
#else

csrilu0Ex :: Handle -> Operation -> Int -> MatrixDescriptor -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> Info -> Type -> IO ()
csrilu0Ex _ _ _ _ _ _ _ _ _ _ = cusparseError "'csrilu0Ex' requires at least cuda-8.0"
#endif
#if CUDA_VERSION >= 9000

{-# INLINEABLE sgtsv2_bufferSizeExt #-}
{# fun unsafe cusparseSgtsv2_bufferSizeExt as sgtsv2_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv2_bufferSizeExt #-}
{# fun unsafe cusparseDgtsv2_bufferSizeExt as dgtsv2_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv2_bufferSizeExt #-}
{# fun unsafe cusparseCgtsv2_bufferSizeExt as cgtsv2_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv2_bufferSizeExt #-}
{# fun unsafe cusparseZgtsv2_bufferSizeExt as zgtsv2_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsv2 #-}
{# fun unsafe cusparseSgtsv2 as sgtsv2 { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv2 #-}
{# fun unsafe cusparseDgtsv2 as dgtsv2 { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv2 #-}
{# fun unsafe cusparseCgtsv2 as cgtsv2 { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv2 #-}
{# fun unsafe cusparseZgtsv2 as zgtsv2 { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsv2_nopivot_bufferSizeExt #-}
{# fun unsafe cusparseSgtsv2_nopivot_bufferSizeExt as sgtsv2_nopivot_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv2_nopivot_bufferSizeExt #-}
{# fun unsafe cusparseDgtsv2_nopivot_bufferSizeExt as dgtsv2_nopivot_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv2_nopivot_bufferSizeExt #-}
{# fun unsafe cusparseCgtsv2_nopivot_bufferSizeExt as cgtsv2_nopivot_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv2_nopivot_bufferSizeExt #-}
{# fun unsafe cusparseZgtsv2_nopivot_bufferSizeExt as zgtsv2_nopivot_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsv2_nopivot #-}
{# fun unsafe cusparseSgtsv2_nopivot as sgtsv2_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv2_nopivot #-}
{# fun unsafe cusparseDgtsv2_nopivot as dgtsv2_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv2_nopivot #-}
{# fun unsafe cusparseCgtsv2_nopivot as cgtsv2_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv2_nopivot #-}
{# fun unsafe cusparseZgtsv2_nopivot as zgtsv2_nopivot { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsv2StridedBatch_bufferSizeExt #-}
{# fun unsafe cusparseSgtsv2StridedBatch_bufferSizeExt as sgtsv2StridedBatch_bufferSizeExt { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv2StridedBatch_bufferSizeExt #-}
{# fun unsafe cusparseDgtsv2StridedBatch_bufferSizeExt as dgtsv2StridedBatch_bufferSizeExt { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv2StridedBatch_bufferSizeExt #-}
{# fun unsafe cusparseCgtsv2StridedBatch_bufferSizeExt as cgtsv2StridedBatch_bufferSizeExt { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv2StridedBatch_bufferSizeExt #-}
{# fun unsafe cusparseZgtsv2StridedBatch_bufferSizeExt as zgtsv2StridedBatch_bufferSizeExt { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsv2StridedBatch #-}
{# fun unsafe cusparseSgtsv2StridedBatch as sgtsv2StridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsv2StridedBatch #-}
{# fun unsafe cusparseDgtsv2StridedBatch as dgtsv2StridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsv2StridedBatch #-}
{# fun unsafe cusparseCgtsv2StridedBatch as cgtsv2StridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsv2StridedBatch #-}
{# fun unsafe cusparseZgtsv2StridedBatch as zgtsv2StridedBatch { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}
#else

sgtsv2_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> Int -> IO ()
sgtsv2_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsv2_bufferSizeExt' requires at least cuda-9.0"

dgtsv2_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> Int -> IO ()
dgtsv2_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsv2_bufferSizeExt' requires at least cuda-9.0"

cgtsv2_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int -> IO ()
cgtsv2_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsv2_bufferSizeExt' requires at least cuda-9.0"

zgtsv2_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int -> IO ()
zgtsv2_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsv2_bufferSizeExt' requires at least cuda-9.0"

sgtsv2 :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> DevicePtr () -> IO ()
sgtsv2 _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsv2' requires at least cuda-9.0"

dgtsv2 :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> DevicePtr () -> IO ()
dgtsv2 _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsv2' requires at least cuda-9.0"

cgtsv2 :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> DevicePtr () -> IO ()
cgtsv2 _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsv2' requires at least cuda-9.0"

zgtsv2 :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> DevicePtr () -> IO ()
zgtsv2 _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsv2' requires at least cuda-9.0"

sgtsv2_nopivot_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> Int -> IO ()
sgtsv2_nopivot_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsv2_nopivot_bufferSizeExt' requires at least cuda-9.0"

dgtsv2_nopivot_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> Int -> IO ()
dgtsv2_nopivot_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsv2_nopivot_bufferSizeExt' requires at least cuda-9.0"

cgtsv2_nopivot_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int -> IO ()
cgtsv2_nopivot_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsv2_nopivot_bufferSizeExt' requires at least cuda-9.0"

zgtsv2_nopivot_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int -> IO ()
zgtsv2_nopivot_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsv2_nopivot_bufferSizeExt' requires at least cuda-9.0"

sgtsv2_nopivot :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> DevicePtr () -> IO ()
sgtsv2_nopivot _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsv2_nopivot' requires at least cuda-9.0"

dgtsv2_nopivot :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> DevicePtr () -> IO ()
dgtsv2_nopivot _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsv2_nopivot' requires at least cuda-9.0"

cgtsv2_nopivot :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> DevicePtr () -> IO ()
cgtsv2_nopivot _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsv2_nopivot' requires at least cuda-9.0"

zgtsv2_nopivot :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> DevicePtr () -> IO ()
zgtsv2_nopivot _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsv2_nopivot' requires at least cuda-9.0"

sgtsv2StridedBatch_bufferSizeExt :: Handle -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> Int -> Int -> IO ()
sgtsv2StridedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsv2StridedBatch_bufferSizeExt' requires at least cuda-9.0"

dgtsv2StridedBatch_bufferSizeExt :: Handle -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> Int -> Int -> IO ()
dgtsv2StridedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsv2StridedBatch_bufferSizeExt' requires at least cuda-9.0"

cgtsv2StridedBatch_bufferSizeExt :: Handle -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int -> Int -> IO ()
cgtsv2StridedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsv2StridedBatch_bufferSizeExt' requires at least cuda-9.0"

zgtsv2StridedBatch_bufferSizeExt :: Handle -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int -> Int -> IO ()
zgtsv2StridedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsv2StridedBatch_bufferSizeExt' requires at least cuda-9.0"

sgtsv2StridedBatch :: Handle -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> Int -> DevicePtr () -> IO ()
sgtsv2StridedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsv2StridedBatch' requires at least cuda-9.0"

dgtsv2StridedBatch :: Handle -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> Int -> DevicePtr () -> IO ()
dgtsv2StridedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsv2StridedBatch' requires at least cuda-9.0"

cgtsv2StridedBatch :: Handle -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int -> DevicePtr () -> IO ()
cgtsv2StridedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsv2StridedBatch' requires at least cuda-9.0"

zgtsv2StridedBatch :: Handle -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int -> DevicePtr () -> IO ()
zgtsv2StridedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsv2StridedBatch' requires at least cuda-9.0"
#endif
#if CUDA_VERSION >= 9020

{-# INLINEABLE sgtsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseSgtsvInterleavedBatch_bufferSizeExt as sgtsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseDgtsvInterleavedBatch_bufferSizeExt as dgtsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseCgtsvInterleavedBatch_bufferSizeExt as cgtsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseZgtsvInterleavedBatch_bufferSizeExt as zgtsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sgtsvInterleavedBatch #-}
{# fun unsafe cusparseSgtsvInterleavedBatch as sgtsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgtsvInterleavedBatch #-}
{# fun unsafe cusparseDgtsvInterleavedBatch as dgtsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgtsvInterleavedBatch #-}
{# fun unsafe cusparseCgtsvInterleavedBatch as cgtsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgtsvInterleavedBatch #-}
{# fun unsafe cusparseZgtsvInterleavedBatch as zgtsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sgpsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseSgpsvInterleavedBatch_bufferSizeExt as sgpsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgpsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseDgpsvInterleavedBatch_bufferSizeExt as dgpsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgpsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseCgpsvInterleavedBatch_bufferSizeExt as cgpsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgpsvInterleavedBatch_bufferSizeExt #-}
{# fun unsafe cusparseZgpsvInterleavedBatch_bufferSizeExt as zgpsvInterleavedBatch_bufferSizeExt { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sgpsvInterleavedBatch #-}
{# fun unsafe cusparseSgpsvInterleavedBatch as sgpsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Float', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgpsvInterleavedBatch #-}
{# fun unsafe cusparseDgpsvInterleavedBatch as dgpsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Double', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgpsvInterleavedBatch #-}
{# fun unsafe cusparseCgpsvInterleavedBatch as cgpsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgpsvInterleavedBatch #-}
{# fun unsafe cusparseZgpsvInterleavedBatch as zgpsvInterleavedBatch { useHandle `Handle', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}
#else

sgtsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> Int -> IO ()
sgtsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

dgtsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> Int -> IO ()
dgtsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

cgtsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int -> IO ()
cgtsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

zgtsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int -> IO ()
zgtsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

sgtsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> DevicePtr () -> IO ()
sgtsvInterleavedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'sgtsvInterleavedBatch' requires at least cuda-9.0"

dgtsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> DevicePtr () -> IO ()
dgtsvInterleavedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'dgtsvInterleavedBatch' requires at least cuda-9.0"

cgtsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> DevicePtr () -> IO ()
cgtsvInterleavedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'cgtsvInterleavedBatch' requires at least cuda-9.0"

zgtsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> DevicePtr () -> IO ()
zgtsvInterleavedBatch _ _ _ _ _ _ _ _ _ = cusparseError "'zgtsvInterleavedBatch' requires at least cuda-9.0"

sgpsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> Int -> IO ()
sgpsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'sgpsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

dgpsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> Int -> IO ()
dgpsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dgpsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

cgpsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int -> IO ()
cgpsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'cgpsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

zgpsvInterleavedBatch_bufferSizeExt :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int -> IO ()
zgpsvInterleavedBatch_bufferSizeExt _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zgpsvInterleavedBatch_bufferSizeExt' requires at least cuda-9.0"

sgpsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> DevicePtr Float -> Int -> DevicePtr () -> IO ()
sgpsvInterleavedBatch _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'sgpsvInterleavedBatch' requires at least cuda-9.0"

dgpsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> DevicePtr Double -> Int -> DevicePtr () -> IO ()
dgpsvInterleavedBatch _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dgpsvInterleavedBatch' requires at least cuda-9.0"

cgpsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> DevicePtr (Complex Float) -> Int -> DevicePtr () -> IO ()
cgpsvInterleavedBatch _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'cgpsvInterleavedBatch' requires at least cuda-9.0"

zgpsvInterleavedBatch :: Handle -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> DevicePtr (Complex Double) -> Int -> DevicePtr () -> IO ()
zgpsvInterleavedBatch _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zgpsvInterleavedBatch' requires at least cuda-9.0"
#endif
