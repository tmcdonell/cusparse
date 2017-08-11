{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Context
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Sparse.Context (

  -- * Context management
  Handle(..),
  create, destroy,

  -- ** Utilities
  PointerMode(..),
  setPointerMode,
  getPointerMode,

) where

-- Friends
import Foreign.CUDA.BLAS.Sparse.Error
import Foreign.CUDA.BLAS.Sparse.Internal.C2HS

-- System
import Foreign
import Foreign.C
import Control.Monad                                      ( liftM )

#include "cbits/stubs.h"
{# context lib="cusparse" #}


-- | An opaque handle to the cuSPARSE library context, which is passed to all
-- library function calls.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsehandlet>
--
newtype Handle = Handle { useHandle :: {# type cusparseHandle_t #}}

-- | This function initializes the cuSPARSE library and creates a handle on the
-- cuSPARSE context. It must be called before any other cuSPARSE API function is
-- invoked. It allocates hardware resources necessary for accessing the GPU.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsecreate>
--
{-# INLINEABLE create #-}
create :: IO Handle
create = resultIfOk =<< cusparseCreate
  where
    {# fun unsafe cusparseCreate
      { alloca- `Handle' peekHdl* } -> `Status' cToEnum #}
      where
        peekHdl = liftM Handle . peek


-- | This function releases CPU-side resources used by the cuSPARSE library. The
-- release of GPU-side resources may be deferred until the application shuts
-- down.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsedestroy>
--
{-# INLINEABLE destroy #-}
{# fun unsafe cusparseDestroy as destroy
  { useHandle `Handle' } -> `()' checkStatus* #}


-- | For functions which take scalar value arguments, determines whether those
-- values are passed by reference on the host or device.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsepointermode_t>
--
{# enum cusparsePointerMode_t as PointerMode
  { underscoreToCase }
  with prefix="CUSPARSE_POINTER_MODE" deriving (Eq, Show) #}


-- | Set the pointer mode used by cuSPARSE library functions.
--
-- The default mode is for values to be passed by reference from the host.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsesetpointermode>
--
{-# INLINEABLE setPointerMode #-}
{# fun unsafe cusparseSetPointerMode as setPointerMode
  { useHandle `Handle'
  , cFromEnum `PointerMode'
  }
  -> `()' checkStatus* #}

-- | Get the pointer mode used by cuSPARSE library functions to pass scalar
-- arguments.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsegetpointermode>
--
{-# INLINEABLE getPointerMode #-}
getPointerMode :: Handle -> IO PointerMode
getPointerMode h = resultIfOk =<< cusparseGetPointerMode h
  where
    {# fun unsafe cusparseGetPointerMode
      { useHandle `Handle'
      , alloca-   `PointerMode' peekPM*
      }
      -> `Status' cToEnum #}

    peekPM = liftM cToEnum . peek

