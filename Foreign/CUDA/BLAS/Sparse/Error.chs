{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.CUDA.BLAS.Sparse.Error
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.CUDA.BLAS.Sparse.Error
  where

-- System
import Data.Typeable
import Control.Exception

#include "cbits/stubs.h"
{# context lib="cusparse" #}


-- | Error codes used by cuSPARSE library functions
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparsestatust>
--
{# enum cusparseStatus_t as Status
  { underscoreToCase }
  with prefix="CUSPARSE_STATUS" deriving (Eq, Show) #}

-- Describe each error code
--
describe :: Status -> String
describe Success                = "success"
describe NotInitialized         = "library not initialised"
describe AllocFailed            = "resource allocation failed"
describe InvalidValue           = "unsupported value or parameter passed to a function"
describe ArchMismatch           = "unsupported on current architecture"
describe MappingError           = "access to GPU memory failed"
describe ExecutionFailed        = "execution failed"
describe InternalError          = "internal error"
describe MatrixTypeNotSupported = "matrix type not supported for this function"
describe ZeroPivot              = "zero pivot"


-- Exceptions ------------------------------------------------------------------
--
data CUSparseException
  = ExitCode  Status
  | UserError String
  deriving Typeable

instance Exception CUSparseException

instance Show CUSparseException where
  showsPrec _ (ExitCode  s) = showString ("CUSparse Exception: " ++ describe s)
  showsPrec _ (UserError s) = showString ("CUSparse Exception: " ++ s)


-- | Raise a CUSparseException in the IO Monad
--
cusparseError :: String -> IO a
cusparseError s = throwIO (UserError s)


-- | Return the results of a function on successful execution, otherwise throw
-- an exception with an error string associated with the return code
--
resultIfOk :: (Status, a) -> IO a
resultIfOk (status,result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)


-- | Throw an exception with an error string associated with an unsuccessful
-- return code, otherwise return unit.
--
nothingIfOk :: Status -> IO ()
nothingIfOk status =
    case status of
        Success -> return  ()
        _       -> throwIO (ExitCode status)

