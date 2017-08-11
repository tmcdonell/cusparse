--
-- This module is auto-generated. Do not edit directly.
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Level1
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- For more information see the cuSPARSE Level-1 function reference:
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-level-1-function-reference>
--

module Foreign.CUDA.BLAS.Sparse.Level1 (

  saxpyi,
  daxpyi,
  caxpyi,
  zaxpyi,
  sdoti,
  ddoti,
  cdoti,
  zdoti,
  cdotci,
  zdotci,
  sgthr,
  dgthr,
  cgthr,
  zgthr,
  sgthrz,
  dgthrz,
  cgthrz,
  zgthrz,
  sroti,
  droti,
  ssctr,
  dsctr,
  csctr,
  zsctr,

) where

import Data.Complex
import Foreign
import Foreign.C
import Foreign.Storable.Complex ()
import Foreign.CUDA.Ptr
import Foreign.CUDA.BLAS.Sparse.Error
import Foreign.CUDA.BLAS.Sparse.Internal.C2HS
import Foreign.CUDA.BLAS.Sparse.Internal.Types

#include "cbits/stubs.h"
{# context lib="cusparse" #}

{-# INLINE useDevP #-}
useDevP :: DevicePtr a -> Ptr b
useDevP = useDevicePtr . castDevPtr

{-# INLINE useHostP #-}
useHostP :: HostPtr a -> Ptr b
useHostP = useHostPtr . castHostPtr


{-# INLINEABLE saxpyi #-}
{# fun unsafe cusparseSaxpyi as saxpyi { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Int', castPtr `Ptr Float', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE daxpyi #-}
{# fun unsafe cusparseDaxpyi as daxpyi { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Int', castPtr `Ptr Double', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE caxpyi #-}
{# fun unsafe cusparseCaxpyi as caxpyi { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', castPtr `Ptr (Complex Float)', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE zaxpyi #-}
{# fun unsafe cusparseZaxpyi as zaxpyi { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', castPtr `Ptr (Complex Double)', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE sdoti #-}
{# fun unsafe cusparseSdoti as sdoti { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Float', castPtr `Ptr Float', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE ddoti #-}
{# fun unsafe cusparseDdoti as ddoti { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Double', castPtr `Ptr Double', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE cdoti #-}
{# fun unsafe cusparseCdoti as cdoti { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Float)', castPtr `Ptr (Complex Float)', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE zdoti #-}
{# fun unsafe cusparseZdoti as zdoti { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Double)', castPtr `Ptr (Complex Double)', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE cdotci #-}
{# fun unsafe cusparseCdotci as cdotci { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Float)', castPtr `Ptr (Complex Float)', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE zdotci #-}
{# fun unsafe cusparseZdotci as zdotci { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Double)', castPtr `Ptr (Complex Double)', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE sgthr #-}
{# fun unsafe cusparseSgthr as sgthr { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE dgthr #-}
{# fun unsafe cusparseDgthr as dgthr { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE cgthr #-}
{# fun unsafe cusparseCgthr as cgthr { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE zgthr #-}
{# fun unsafe cusparseZgthr as zgthr { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE sgthrz #-}
{# fun unsafe cusparseSgthrz as sgthrz { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Float', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE dgthrz #-}
{# fun unsafe cusparseDgthrz as dgthrz { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Double', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE cgthrz #-}
{# fun unsafe cusparseCgthrz as cgthrz { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE zgthrz #-}
{# fun unsafe cusparseZgthrz as zgthrz { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE sroti #-}
{# fun unsafe cusparseSroti as sroti { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Float', castPtr `Ptr Float', castPtr `Ptr Float', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE droti #-}
{# fun unsafe cusparseDroti as droti { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Double', castPtr `Ptr Double', castPtr `Ptr Double', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE ssctr #-}
{# fun unsafe cusparseSsctr as ssctr { useHandle `Handle', `Int', useDevP `DevicePtr Float', useDevP `DevicePtr Int', useDevP `DevicePtr Float', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE dsctr #-}
{# fun unsafe cusparseDsctr as dsctr { useHandle `Handle', `Int', useDevP `DevicePtr Double', useDevP `DevicePtr Int', useDevP `DevicePtr Double', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE csctr #-}
{# fun unsafe cusparseCsctr as csctr { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Float)', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Float)', cFromEnum `IndexBase' } -> `()' checkStatus* #}

{-# INLINEABLE zsctr #-}
{# fun unsafe cusparseZsctr as zsctr { useHandle `Handle', `Int', useDevP `DevicePtr (Complex Double)', useDevP `DevicePtr Int', useDevP `DevicePtr (Complex Double)', cFromEnum `IndexBase' } -> `()' checkStatus* #}
