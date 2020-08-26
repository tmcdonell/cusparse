--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Level2
-- Copyright   : [2017..2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuSPARSE Level-2 function reference:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-level-2-function-reference>
--

module Foreign.CUDA.BLAS.Sparse.Level2 (

  Operation(..),
  Direction(..),
  Type(..),
  Algorithm(..),
  Policy(..),
  MatrixDescriptor,
  Hybrid,
  Info,
  Info_bsrsv2,
  Info_csrsv2,
  sbsrmv,
  dbsrmv,
  cbsrmv,
  zbsrmv,
  sbsrxmv,
  dbsrxmv,
  cbsrxmv,
  zbsrxmv,
  scsrmv,
  dcsrmv,
  ccsrmv,
  zcsrmv,
  sbsrsv2_bufferSize,
  dbsrsv2_bufferSize,
  cbsrsv2_bufferSize,
  zbsrsv2_bufferSize,
  sbsrsv2_analysis,
  dbsrsv2_analysis,
  cbsrsv2_analysis,
  zbsrsv2_analysis,
  sbsrsv2_solve,
  dbsrsv2_solve,
  cbsrsv2_solve,
  zbsrsv2_solve,
  xbsrsv2_zeroPivot,
  scsrsv_analysis,
  dcsrsv_analysis,
  ccsrsv_analysis,
  zcsrsv_analysis,
  scsrsv_solve,
  dcsrsv_solve,
  ccsrsv_solve,
  zcsrsv_solve,
  scsrsv2_bufferSize,
  dcsrsv2_bufferSize,
  ccsrsv2_bufferSize,
  zcsrsv2_bufferSize,
  scsrsv2_analysis,
  dcsrsv2_analysis,
  ccsrsv2_analysis,
  zcsrsv2_analysis,
  scsrsv2_solve,
  dcsrsv2_solve,
  ccsrsv2_solve,
  zcsrsv2_solve,
  xcsrsv2_zeroPivot,
  shybmv,
  dhybmv,
  chybmv,
  zhybmv,
  shybsv_analysis,
  dhybsv_analysis,
  chybsv_analysis,
  zhybsv_analysis,
  shybsv_solve,
  dhybsv_solve,
  chybsv_solve,
  zhybsv_solve,
  sgemvi,
  dgemvi,
  cgemvi,
  zgemvi,
  sgemvi_bufferSize,
  dgemvi_bufferSize,
  cgemvi_bufferSize,
  zgemvi_bufferSize,
  csrmvEx,
  csrmvEx_bufferSize,
  scsrmv_mp,
  dcsrmv_mp,
  ccsrmv_mp,
  zcsrmv_mp,
  csrsv_analysisEx,
  csrsv_solveEx,

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


{-# INLINEABLE sbsrmv #-}
{# fun unsafe cusparseSbsrmv as sbsrmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Float', castPtr `Ptr Float', useDevP `DevicePtr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrmv #-}
{# fun unsafe cusparseDbsrmv as dbsrmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Double', castPtr `Ptr Double', useDevP `DevicePtr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrmv #-}
{# fun unsafe cusparseCbsrmv as cbsrmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr (Complex Float)', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrmv #-}
{# fun unsafe cusparseZbsrmv as zbsrmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr (Complex Double)', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrxmv #-}
{# fun unsafe cusparseSbsrxmv as sbsrxmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Float', castPtr `Ptr Float', useDevP `DevicePtr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrxmv #-}
{# fun unsafe cusparseDbsrxmv as dbsrxmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr Double', castPtr `Ptr Double', useDevP `DevicePtr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrxmv #-}
{# fun unsafe cusparseCbsrxmv as cbsrxmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr (Complex Float)', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrxmv #-}
{# fun unsafe cusparseZbsrxmv as zbsrxmv { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useDevP `DevicePtr (Complex Double)', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrmv #-}
{# fun unsafe cusparseScsrmv as scsrmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', castPtr `Ptr Float', useDevP `DevicePtr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrmv #-}
{# fun unsafe cusparseDcsrmv as dcsrmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', castPtr `Ptr Double', useDevP `DevicePtr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrmv #-}
{# fun unsafe cusparseCcsrmv as ccsrmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrmv #-}
{# fun unsafe cusparseZcsrmv as zcsrmv { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrsv2_bufferSize #-}
{# fun unsafe cusparseSbsrsv2_bufferSize as sbsrsv2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrsv2_bufferSize #-}
{# fun unsafe cusparseDbsrsv2_bufferSize as dbsrsv2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrsv2_bufferSize #-}
{# fun unsafe cusparseCbsrsv2_bufferSize as cbsrsv2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrsv2_bufferSize #-}
{# fun unsafe cusparseZbsrsv2_bufferSize as zbsrsv2_bufferSize { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrsv2_analysis #-}
{# fun unsafe cusparseSbsrsv2_analysis as sbsrsv2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrsv2_analysis #-}
{# fun unsafe cusparseDbsrsv2_analysis as dbsrsv2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrsv2_analysis #-}
{# fun unsafe cusparseCbsrsv2_analysis as cbsrsv2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrsv2_analysis #-}
{# fun unsafe cusparseZbsrsv2_analysis as zbsrsv2_analysis { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sbsrsv2_solve #-}
{# fun unsafe cusparseSbsrsv2_solve as sbsrsv2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', useDevP `DevicePtr Float', useDevP `DevicePtr Float', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dbsrsv2_solve #-}
{# fun unsafe cusparseDbsrsv2_solve as dbsrsv2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', useDevP `DevicePtr Double', useDevP `DevicePtr Double', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cbsrsv2_solve #-}
{# fun unsafe cusparseCbsrsv2_solve as cbsrsv2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zbsrsv2_solve #-}
{# fun unsafe cusparseZbsrsv2_solve as zbsrsv2_solve { useHandle `Handle', cFromEnum `Direction', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', `Int', useInfo_bsrsv2 `Info_bsrsv2', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xbsrsv2_zeroPivot #-}
{# fun unsafe cusparseXbsrsv2_zeroPivot as xbsrsv2_zeroPivot { useHandle `Handle', useInfo_bsrsv2 `Info_bsrsv2', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsv_analysis #-}
{# fun unsafe cusparseScsrsv_analysis as scsrsv_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsv_analysis #-}
{# fun unsafe cusparseDcsrsv_analysis as dcsrsv_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsv_analysis #-}
{# fun unsafe cusparseCcsrsv_analysis as ccsrsv_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsv_analysis #-}
{# fun unsafe cusparseZcsrsv_analysis as zcsrsv_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsv_solve #-}
{# fun unsafe cusparseScsrsv_solve as scsrsv_solve { useHandle `Handle', cFromEnum `Operation', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr Float', useDevP `DevicePtr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsv_solve #-}
{# fun unsafe cusparseDcsrsv_solve as dcsrsv_solve { useHandle `Handle', cFromEnum `Operation', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr Double', useDevP `DevicePtr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsv_solve #-}
{# fun unsafe cusparseCcsrsv_solve as ccsrsv_solve { useHandle `Handle', cFromEnum `Operation', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsv_solve #-}
{# fun unsafe cusparseZcsrsv_solve as zcsrsv_solve { useHandle `Handle', cFromEnum `Operation', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsv2_bufferSize #-}
{# fun unsafe cusparseScsrsv2_bufferSize as scsrsv2_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsv2_bufferSize #-}
{# fun unsafe cusparseDcsrsv2_bufferSize as dcsrsv2_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsv2_bufferSize #-}
{# fun unsafe cusparseCcsrsv2_bufferSize as ccsrsv2_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsv2_bufferSize #-}
{# fun unsafe cusparseZcsrsv2_bufferSize as zcsrsv2_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsv2_analysis #-}
{# fun unsafe cusparseScsrsv2_analysis as scsrsv2_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsv2_analysis #-}
{# fun unsafe cusparseDcsrsv2_analysis as dcsrsv2_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsv2_analysis #-}
{# fun unsafe cusparseCcsrsv2_analysis as ccsrsv2_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsv2_analysis #-}
{# fun unsafe cusparseZcsrsv2_analysis as zcsrsv2_analysis { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE scsrsv2_solve #-}
{# fun unsafe cusparseScsrsv2_solve as scsrsv2_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', useDevP `DevicePtr Float', useDevP `DevicePtr Float', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrsv2_solve #-}
{# fun unsafe cusparseDcsrsv2_solve as dcsrsv2_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', useDevP `DevicePtr Double', useDevP `DevicePtr Double', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrsv2_solve #-}
{# fun unsafe cusparseCcsrsv2_solve as ccsrsv2_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrsv2_solve #-}
{# fun unsafe cusparseZcsrsv2_solve as zcsrsv2_solve { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_csrsv2 `Info_csrsv2', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', cFromEnum `Policy', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE xcsrsv2_zeroPivot #-}
{# fun unsafe cusparseXcsrsv2_zeroPivot as xcsrsv2_zeroPivot { useHandle `Handle', useInfo_csrsv2 `Info_csrsv2', castPtr `Ptr Int32' } -> `()' checkStatus*- #}

{-# INLINEABLE shybmv #-}
{# fun unsafe cusparseShybmv as shybmv { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Float', castPtr `Ptr Float', useDevP `DevicePtr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dhybmv #-}
{# fun unsafe cusparseDhybmv as dhybmv { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr Double', castPtr `Ptr Double', useDevP `DevicePtr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE chybmv #-}
{# fun unsafe cusparseChybmv as chybmv { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Float)', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zhybmv #-}
{# fun unsafe cusparseZhybmv as zhybmv { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useDevP `DevicePtr (Complex Double)', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE shybsv_analysis #-}
{# fun unsafe cusparseShybsv_analysis as shybsv_analysis { useHandle `Handle', cFromEnum `Operation', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE dhybsv_analysis #-}
{# fun unsafe cusparseDhybsv_analysis as dhybsv_analysis { useHandle `Handle', cFromEnum `Operation', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE chybsv_analysis #-}
{# fun unsafe cusparseChybsv_analysis as chybsv_analysis { useHandle `Handle', cFromEnum `Operation', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE zhybsv_analysis #-}
{# fun unsafe cusparseZhybsv_analysis as zhybsv_analysis { useHandle `Handle', cFromEnum `Operation', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info' } -> `()' checkStatus*- #}

{-# INLINEABLE shybsv_solve #-}
{# fun unsafe cusparseShybsv_solve as shybsv_solve { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info', useDevP `DevicePtr Float', useDevP `DevicePtr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dhybsv_solve #-}
{# fun unsafe cusparseDhybsv_solve as dhybsv_solve { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info', useDevP `DevicePtr Double', useDevP `DevicePtr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE chybsv_solve #-}
{# fun unsafe cusparseChybsv_solve as chybsv_solve { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zhybsv_solve #-}
{# fun unsafe cusparseZhybsv_solve as zhybsv_solve { useHandle `Handle', cFromEnum `Operation', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useHYB `Hybrid', useInfo `Info', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus*- #}
#if CUDA_VERSION >= 7500

{-# INLINEABLE sgemvi #-}
{# fun unsafe cusparseSgemvi as sgemvi { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Float', useDevP `DevicePtr Float', `Int', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', castPtr `Ptr Float', useDevP `DevicePtr Float', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE dgemvi #-}
{# fun unsafe cusparseDgemvi as dgemvi { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr Double', useDevP `DevicePtr Double', `Int', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', castPtr `Ptr Double', useDevP `DevicePtr Double', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE cgemvi #-}
{# fun unsafe cusparseCgemvi as cgemvi { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', `Int', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE zgemvi #-}
{# fun unsafe cusparseZgemvi as zgemvi { useHandle `Handle', cFromEnum `Operation', `Int', `Int', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', `Int', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)', cFromEnum `IndexBase', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE sgemvi_bufferSize #-}
{# fun unsafe cusparseSgemvi_bufferSize as sgemvi_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE dgemvi_bufferSize #-}
{# fun unsafe cusparseDgemvi_bufferSize as dgemvi_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE cgemvi_bufferSize #-}
{# fun unsafe cusparseCgemvi_bufferSize as cgemvi_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE zgemvi_bufferSize #-}
{# fun unsafe cusparseZgemvi_bufferSize as zgemvi_bufferSize { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}
#else

sgemvi :: Handle -> Operation -> Int -> Int -> Ptr Float -> DevicePtr Float -> Int -> Int -> DevicePtr Float -> DevicePtr Int32 -> Ptr Float -> DevicePtr Float -> IndexBase -> DevicePtr () -> IO ()
sgemvi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'sgemvi' requires at least cuda-7.5"

dgemvi :: Handle -> Operation -> Int -> Int -> Ptr Double -> DevicePtr Double -> Int -> Int -> DevicePtr Double -> DevicePtr Int32 -> Ptr Double -> DevicePtr Double -> IndexBase -> DevicePtr () -> IO ()
dgemvi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dgemvi' requires at least cuda-7.5"

cgemvi :: Handle -> Operation -> Int -> Int -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> Int -> Int -> DevicePtr (Complex Float) -> DevicePtr Int32 -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> IndexBase -> DevicePtr () -> IO ()
cgemvi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'cgemvi' requires at least cuda-7.5"

zgemvi :: Handle -> Operation -> Int -> Int -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> Int -> Int -> DevicePtr (Complex Double) -> DevicePtr Int32 -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> IndexBase -> DevicePtr () -> IO ()
zgemvi _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zgemvi' requires at least cuda-7.5"

sgemvi_bufferSize :: Handle -> Operation -> Int -> Int -> Int -> Int -> IO ()
sgemvi_bufferSize _ _ _ _ _ _ = cusparseError "'sgemvi_bufferSize' requires at least cuda-7.5"

dgemvi_bufferSize :: Handle -> Operation -> Int -> Int -> Int -> Int -> IO ()
dgemvi_bufferSize _ _ _ _ _ _ = cusparseError "'dgemvi_bufferSize' requires at least cuda-7.5"

cgemvi_bufferSize :: Handle -> Operation -> Int -> Int -> Int -> Int -> IO ()
cgemvi_bufferSize _ _ _ _ _ _ = cusparseError "'cgemvi_bufferSize' requires at least cuda-7.5"

zgemvi_bufferSize :: Handle -> Operation -> Int -> Int -> Int -> Int -> IO ()
zgemvi_bufferSize _ _ _ _ _ _ = cusparseError "'zgemvi_bufferSize' requires at least cuda-7.5"
#endif
#if CUDA_VERSION >= 8000

{-# INLINEABLE csrmvEx #-}
{# fun unsafe cusparseCsrmvEx as csrmvEx { useHandle `Handle', cFromEnum `Algorithm', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr ()', cFromEnum `Type', useMatDescr `MatrixDescriptor', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()', cFromEnum `Type', castPtr `Ptr ()', cFromEnum `Type', useDevP `DevicePtr ()', cFromEnum `Type', cFromEnum `Type', useDevP `DevicePtr ()' } -> `()' checkStatus*- #}

{-# INLINEABLE csrmvEx_bufferSize #-}
{# fun unsafe cusparseCsrmvEx_bufferSize as csrmvEx_bufferSize { useHandle `Handle', cFromEnum `Algorithm', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr ()', cFromEnum `Type', useMatDescr `MatrixDescriptor', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr ()', cFromEnum `Type', castPtr `Ptr ()', cFromEnum `Type', useDevP `DevicePtr ()', cFromEnum `Type', cFromEnum `Type', alloca- `Int' peekIntConv* } -> `()' checkStatus*- #}

{-# INLINEABLE scsrmv_mp #-}
{# fun unsafe cusparseScsrmv_mp as scsrmv_mp { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Float', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', castPtr `Ptr Float', useDevP `DevicePtr Float' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrmv_mp #-}
{# fun unsafe cusparseDcsrmv_mp as dcsrmv_mp { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr Double', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', castPtr `Ptr Double', useDevP `DevicePtr Double' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrmv_mp #-}
{# fun unsafe cusparseCcsrmv_mp as ccsrmv_mp { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Float)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', castPtr `Ptr (Complex Float)', useDevP `DevicePtr (Complex Float)' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrmv_mp #-}
{# fun unsafe cusparseZcsrmv_mp as zcsrmv_mp { useHandle `Handle', cFromEnum `Operation', `Int', `Int', `Int', castPtr `Ptr (Complex Double)', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', castPtr `Ptr (Complex Double)', useDevP `DevicePtr (Complex Double)' } -> `()' checkStatus*- #}

{-# INLINEABLE csrsv_analysisEx #-}
{# fun unsafe cusparseCsrsv_analysisEx as csrsv_analysisEx { useHandle `Handle', cFromEnum `Operation', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', cFromEnum `Type' } -> `()' checkStatus*- #}

{-# INLINEABLE csrsv_solveEx #-}
{# fun unsafe cusparseCsrsv_solveEx as csrsv_solveEx { useHandle `Handle', cFromEnum `Operation', `Int', castPtr `Ptr ()', cFromEnum `Type', useMatDescr `MatrixDescriptor', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo `Info', useDevP `DevicePtr ()', cFromEnum `Type', useDevP `DevicePtr ()', cFromEnum `Type', cFromEnum `Type' } -> `()' checkStatus*- #}
#else

csrmvEx :: Handle -> Algorithm -> Operation -> Int -> Int -> Int -> Ptr () -> Type -> MatrixDescriptor -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> Type -> Ptr () -> Type -> DevicePtr () -> Type -> Type -> DevicePtr () -> IO ()
csrmvEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'csrmvEx' requires at least cuda-8.0"

csrmvEx_bufferSize :: Handle -> Algorithm -> Operation -> Int -> Int -> Int -> Ptr () -> Type -> MatrixDescriptor -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr () -> Type -> Ptr () -> Type -> DevicePtr () -> Type -> Type -> Int -> IO ()
csrmvEx_bufferSize _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'csrmvEx_bufferSize' requires at least cuda-8.0"

scsrmv_mp :: Handle -> Operation -> Int -> Int -> Int -> Ptr Float -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Float -> Ptr Float -> DevicePtr Float -> IO ()
scsrmv_mp _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrmv_mp' requires at least cuda-8.0"

dcsrmv_mp :: Handle -> Operation -> Int -> Int -> Int -> Ptr Double -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Double -> Ptr Double -> DevicePtr Double -> IO ()
dcsrmv_mp _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrmv_mp' requires at least cuda-8.0"

ccsrmv_mp :: Handle -> Operation -> Int -> Int -> Int -> Ptr (Complex Float) -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Float) -> Ptr (Complex Float) -> DevicePtr (Complex Float) -> IO ()
ccsrmv_mp _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrmv_mp' requires at least cuda-8.0"

zcsrmv_mp :: Handle -> Operation -> Int -> Int -> Int -> Ptr (Complex Double) -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Double) -> Ptr (Complex Double) -> DevicePtr (Complex Double) -> IO ()
zcsrmv_mp _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrmv_mp' requires at least cuda-8.0"

csrsv_analysisEx :: Handle -> Operation -> Int -> Int -> MatrixDescriptor -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> Info -> Type -> IO ()
csrsv_analysisEx _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'csrsv_analysisEx' requires at least cuda-8.0"

csrsv_solveEx :: Handle -> Operation -> Int -> Ptr () -> Type -> MatrixDescriptor -> DevicePtr () -> Type -> DevicePtr Int32 -> DevicePtr Int32 -> Info -> DevicePtr () -> Type -> DevicePtr () -> Type -> Type -> IO ()
csrsv_solveEx _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'csrsv_solveEx' requires at least cuda-8.0"
#endif
