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

  Info_colour,
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


{-# INLINEABLE scsrcolor #-}
{# fun unsafe cusparseScsrcolor as scsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_colour `Info_colour' } -> `()' checkStatus* #}

{-# INLINEABLE dcsrcolor #-}
{# fun unsafe cusparseDcsrcolor as dcsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_colour `Info_colour' } -> `()' checkStatus* #}

{-# INLINEABLE ccsrcolor #-}
{# fun unsafe cusparseCcsrcolor as ccsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_colour `Info_colour' } -> `()' checkStatus* #}

{-# INLINEABLE zcsrcolor #-}
{# fun unsafe cusparseZcsrcolor as zcsrcolor { useHandle `Handle', `Int', `Int', useMatDescr `MatrixDescriptor', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useDevP `DevicePtr Int', useInfo_colour `Info_colour' } -> `()' checkStatus* #}
