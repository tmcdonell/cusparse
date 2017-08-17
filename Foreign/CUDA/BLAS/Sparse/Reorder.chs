--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Reorder
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuSPARSE function reference:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-reorderings-reference>
--

module Foreign.CUDA.BLAS.Sparse.Reorder (

  Info_color,
  scsrcolor,
  dcsrcolor,
  ccsrcolor,
  zcsrcolor,

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

#if CUDA_VERSION >= 7000

{-# INLINEABLE scsrcolor #-}
{# fun unsafe cusparseScsrcolor as scsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Float', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_color `Info_color' } -> `()' checkStatus*- #}

{-# INLINEABLE dcsrcolor #-}
{# fun unsafe cusparseDcsrcolor as dcsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Double', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_color `Info_color' } -> `()' checkStatus*- #}

{-# INLINEABLE ccsrcolor #-}
{# fun unsafe cusparseCcsrcolor as ccsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_color `Info_color' } -> `()' checkStatus*- #}

{-# INLINEABLE zcsrcolor #-}
{# fun unsafe cusparseZcsrcolor as zcsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useDevP `DevicePtr Int32', useInfo_color `Info_color' } -> `()' checkStatus*- #}
#else

scsrcolor :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Float -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> Info_color -> IO ()
scsrcolor _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'scsrcolor' requires at least cuda-7.0"

dcsrcolor :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Double -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> Info_color -> IO ()
dcsrcolor _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'dcsrcolor' requires at least cuda-7.0"

ccsrcolor :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Float) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> Info_color -> IO ()
ccsrcolor _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'ccsrcolor' requires at least cuda-7.0"

zcsrcolor :: Handle -> Int -> Int -> MatrixDescriptor -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr (Complex Double) -> DevicePtr Int32 -> DevicePtr Int32 -> DevicePtr Int32 -> Info_color -> IO ()
zcsrcolor _ _ _ _ _ _ _ _ _ _ _ _ = cusparseError "'zcsrcolor' requires at least cuda-7.0"
#endif
