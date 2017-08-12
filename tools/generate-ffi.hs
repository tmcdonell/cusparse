#!/usr/bin/env runhaskell
-- vim: filetype=haskell
--
-- Generate c2hs FFI binding hooks
--
-- Based on: https://github.com/Rufflewind/blas-hs/blob/f8e90b26bc9865618802dce9ccf21fc2b5c032be/tools/generate-ffi
--
module Main (main) where

import Data.Char                                                    ( toUpper )
import Data.Functor                                                 ( (<$>) )
import Data.List                                                    ( intercalate )
import Data.Monoid                                                  ( (<>) )
import Text.Printf                                                  ( printf )


main :: IO ()
main = do
  let
      docs :: Int -> [String]
      docs l  = [ printf "For more information see the cuSPARSE Level-%d function reference:" l
                , ""
                , printf "<http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-level-%d-function-reference>" l
                , ""
                ]
      l1exps  = [ "IndexBase(..)"
                ]
      l2exps  = [ "Operation(..)"
                , "Direction(..)"
                , "IndexBase(..)"
                , "MatrixDescriptor"
                , "Type(..)"
                , "Algorithm(..)"
                , "Policy(..)"
                , "Hybrid(..)"
                , "Info"
                , "Info_bsrsv2"
                , "Info_csrsv2"
                ]
      l3exps  = [ "Operation(..)"
                , "MatrixDescriptor"
                , "Policy(..)"
                , "Info(..)"
                , "Info_bsrsm2"
                , "Info_csrgemm2"
                ]
  --
  mkC2HS "Level1" (docs 1) l1exps [(Nothing,   funsL1)]
  mkC2HS "Level2" (docs 2) l2exps [(Nothing,   funsL2)
                                  ,(Just 7500, funsL2_cuda75)
                                  ,(Just 8000, funsL2_cuda80)
                                  ]
  mkC2HS "Level3" (docs 3) l3exps [(Nothing,   funsL3)
                                  ,(Just 7000, funsL3_cuda70)
                                  ]


mkC2HS :: String -> [String] -> [String] -> [(Maybe Int, [FunGroup])] -> IO ()
mkC2HS mdl docs exps funs =
  let exts    = [ "CPP"
                , "ForeignFunctionInterface"
                ]
      name    = [ "Foreign", "CUDA", "BLAS", "Sparse", mdl ]
      path    = intercalate "/" name ++ ".chs"
      imps    = [ "Data.Complex"
                , "Numeric.Half"
                , "Foreign"
                , "Foreign.C"
                , "Foreign.Storable.Complex ()"
                , "Foreign.CUDA.Ptr"
                , "Foreign.CUDA.BLAS.Sparse.Error"
                , "Foreign.CUDA.BLAS.Sparse.Internal.C2HS"
                , "Foreign.CUDA.BLAS.Sparse.Internal.Types"
                ]
      body    = "{-# INLINE useDevP #-}"
              : "useDevP :: DevicePtr a -> Ptr b"
              : "useDevP = useDevicePtr . castDevPtr"
              : ""
              : "{-# INLINE useHostP #-}"
              : "useHostP :: HostPtr a -> Ptr b"
              : "useHostP = useHostPtr . castHostPtr"
              : ""
              : content

      mkFuns (Nothing, fg) = map mkFun fg
      mkFuns (Just v,  fg) = printf "#if CUDA_VERSION >= %d" v
                           : map mkFun fg
                          ++ "#else"
                           : map (mkDummyFun (fromIntegral v / 1000)) fg
                          ++ "#endif"
                           : []

      fis     = map (\(r,f) -> (r, funInsts Unsafe f)) funs
      exps'   = exps ++ concatMap (map cfName . snd) fis
      content = concatMap mkFuns fis
  in
  writeFile path $ mkModule exts name docs exps' imps body


mkModule
    :: [String]       -- ^ extensions
    -> [String]       -- ^ module name segments
    -> [String]       -- ^ module documentation
    -> [String]       -- ^ exports
    -> [String]       -- ^ imports
    -> [String]       -- ^ module contents
    -> String
mkModule exts name docs exps imps body =
  unlines
    $ "--"
    : "-- This module is auto-generated. Do not edit directly."
    : "--"
    : ""
    : map (printf "{-# LANGUAGE %s #-}") exts
   ++ "{-# OPTIONS_GHC -fno-warn-unused-imports #-}"
    : "{-# OPTIONS_GHC -fno-warn-unused-binds #-}"
    : "-- |"
    :("-- Module      : " ++ intercalate "." name)
    : "-- Copyright   : [2017] Trevor L. McDonell"
    : "-- License     : BSD3"
    : "--"
    : "-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>"
    : "-- Stability   : experimental"
    : "-- Portability : non-portable (GHC extensions)"
    : "--"
    : map (\x -> if null x then "--" else "-- " ++ x) docs
   ++ ""
    : printf "module %s (\n" (intercalate "." name)
    : map (\x -> if head x == '#' then x else printf "  %s," x) exps
   ++ printf "\n) where"
    : ""
    : map (printf "import %s") imps
   ++ ""
    : "#include \"cbits/stubs.h\""
    : "{# context lib=\"cusparse\" #}"
    : ""
    : body


-- | Generates a c2hs hook for the function.
--
mkFun :: CFun -> String
mkFun (CFun safe name suffix params ret doc) =
  intercalate "\n"
    [ if null doc then "" else "-- | " <> doc
    , printf "{-# INLINEABLE %s #-}" name
    , printf "{# fun%s %s%s { %s } -> %s #}" safe' cName hName params' ret'
    ]
  where
    cName   = funMangler name suffix
    hName   = if name == cName then "" else " as " <> name
    safe'   = if safe then "" else " unsafe"
    params' = intercalate ", " $ fmap (mkParamType . convType) params
    ret'    = mkRetType $ convType ret

mkDummyFun :: Double -> CFun -> String
mkDummyFun minv (CFun _ name _ params ret doc) =
  intercalate "\n"
    [ if null doc then "" else "-- | " <> doc
    , printf "%s :: %s -> IO %s" name params' ret'
    , printf "%s %s = cusparseError \"'%s' requires at least cuda-%3.1f\"" name ignore name minv
    ]
  where
    params' = intercalate " -> " $ map (hsType . convType) params
    ignore  = unwords $ replicate (length params) "_"
    ret'    = hsType (convType ret)
    hsType (HType _ s _) = s


data Safety
  = Safe
  | Unsafe
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Represents a C type.
--
data Type
  = TVoid
  | THandle
  | TStatus
  | TData String String String
  | TPtr (Maybe AddrSpace) Type
  | TInt (Maybe Int)  -- ^ signed integer, with optional precision
  | THalf             -- ^ 16-bit floating-point type
  | TFloat            -- ^ 32-bit floating-point
  | TDouble           -- ^ 64-bit floating-point
  | TComplex Type
  | TEnum String
  | TDummy Int        -- ^ Used for extracting the bound variables
  deriving (Eq, Show)

data AddrSpace
  = Host | Device
  deriving (Eq, Show)

realTypes :: [Type]
realTypes = [ float, double ]

complexTypes :: [Type]
complexTypes = complex <$> realTypes

floatingTypes :: [Type]
floatingTypes = realTypes <> complexTypes

floatingTypesB :: [(Type, Type)]
floatingTypesB = do
  t <- floatingTypes
  return $ case t of
    TComplex t' -> (t', t)
    _           -> (t,  t)

floatingTypesE :: [(Type, Type)]
floatingTypesE = do
  t <- floatingTypes
  case t of
    TComplex t' -> [(t, t), (t, t')]
    _           -> [(t, t)]

-- | Represents a C function.
--
data Fun
  = Fun
    { fName     :: String
    , _fSuffix  :: String
    , fTypes    :: [Type]
    , _fDoc     :: String
    }

-- | Construct a 'Fun'.
--
fun :: String -> [Type] -> Fun
fun name types = Fun name "" types ""

-- | Represents a marshallable C type for c2hs.
--
data HType = HType
             String                     -- in marshaller
             String                     -- type
             String                     -- out marshaller
             deriving Show

mkParamType :: HType -> String
mkParamType (HType m s _) =
  if null m then s' else m <> " " <> s'
  where s' = "`" <> s <> "'"

mkRetType :: HType -> String
mkRetType (HType _ s m) =
  if null m then s' else s' <> " " <> m
  where s' = "`" <> s <> "'"

-- | Represents a C function hook for c2hs.
--
data CFun
  = CFun
    { _cfSafe   :: Bool
    , cfName    :: String
    , _cfSuffix :: String
    , _cfParams :: [Type]
    , _cfRet    :: Type
    , _cfDoc    :: String
    }

-- | Construct a 'CFun'.
--
-- cFun :: String -> String -> [Type] -> Type -> CFun
-- cFun name suffix params ret = CFun True name suffix params ret ""

substitute :: String -> String -> String
substitute s y = case y of
  []     -> []
  x : xs ->
    let xs' = substitute s xs in
    case x of
      '?' -> s <> xs'
      _   -> x : xs'

typeAbbrev :: Type -> String
typeAbbrev t = case t of
  THalf            -> "h"
  TFloat           -> "s"
  TDouble          -> "d"
  TComplex TFloat  -> "c"
  TComplex TDouble -> "z"
  _                -> error ("no valid abbreviation for: " <> show t)

decorate :: [Type] -> String -> String
decorate [a]                = substitute $ typeAbbrev a
decorate [a, b] | a == b    = substitute $ typeAbbrev a
                | otherwise = substitute $ typeAbbrev a <> typeAbbrev b
decorate _                  = error "decorate: bad args"

-- NOTE: Here we assume that both the C and Haskell types have identical
-- representations; this isn't in the specs but in practice the Storable
-- instances are identical so it should work fine
--
convType :: Type -> HType
convType t = case t of
  TVoid             -> simple "()"
  TInt ms           -> simple (maybe "Int" (printf "Int%d") ms)
  TEnum t'          -> enum t'
  THalf             -> floating "Half"
  TFloat            -> floating "Float"
  TDouble           -> floating "Double"
  TComplex TFloat   -> simple "(Complex Float)"
  TComplex TDouble  -> simple "(Complex Double)"
  TPtr as t'        -> pointer as
                     $ case convType t' of
                         HType _ s _ -> case t' of
                                          TPtr{} -> printf "(%s)" s
                                          _      -> s
  THandle           -> HType "useHandle" "Handle" ""
  TStatus           -> HType "" "()" "checkStatus*"
  TData i t o       -> HType i t o
  _                 -> error $ "unmarshallable type: " <> show t
  where
    simple s    = HType "" s ""
    enum s      = HType "cFromEnum" s "cToEnum"
    floating s  = HType ("C" <> s) s ("fromC" <> s)
    --
    pointer Nothing s       = HType "castPtr"  ("Ptr " <> s) ""
    pointer (Just Host) s   = HType "useHostP" ("HostPtr " <> s) ""
    pointer (Just Device) s = HType "useDevP"  ("DevicePtr " <> s) ""


-- shortcuts

ptr :: Type -> Type
ptr = TPtr Nothing

dptr :: Type -> Type
dptr = TPtr (Just Device)

hptr :: Type -> Type
hptr = TPtr (Just Host)

void :: Type
void = TVoid

int :: Type
int = TInt Nothing

int32 :: Type
int32 = TInt (Just 32)

int64 :: Type
int64 = TInt (Just 64)

half :: Type
half = THalf

float :: Type
float = TFloat

double :: Type
double = TDouble

complex :: Type -> Type
complex = TComplex

transpose :: Type
transpose = TEnum "Operation"

uplo :: Type
uplo = TEnum "Fill"

diag :: Type
diag = TEnum "Diagonal"

dtype :: Type
dtype = TEnum "Type"

dir :: Type
dir = TEnum "Direction"

alg :: Type
alg = TEnum "Algorithm"

idxBase :: Type
idxBase = TEnum "IndexBase"

policy :: Type
policy = TEnum "Policy"

info :: Type
info = TData "useInfo" "Info" ""

mkInfo :: String -> Type
mkInfo t = TData ("useInfo_" <> t) ("Info_" <> t) ""

info_bsrsv2 :: Type
info_bsrsv2 = mkInfo "bsrsv2"

info_bsrsm2 :: Type
info_bsrsm2 = mkInfo "bsrsm2"

info_csrsv2 :: Type
info_csrsv2 = mkInfo "csrsv2"

info_csrgemm2 :: Type
info_csrgemm2 = mkInfo "csrgemm2"

matdescr :: Type
matdescr = TData "useMatDescr" "MatrixDescriptor" ""

hyb :: Type
hyb = TData "useHYB" "Hybrid" ""

funInsts :: Safety -> [FunGroup] -> [CFun]
funInsts safety funs = mangleFun safety <$> concatFunInstances funs

-- | cuSPARSE function signatures. The initial context handle argument is added
-- implicitly.
--
-- Level 1 (vector-vector) operations.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-level-1-function-reference>
--
funsL1 :: [FunGroup]
funsL1 =
  [ gpA $ \ a   -> fun "?axpyi" [ int, dptr a, dptr a, dptr int, ptr a, idxBase ]
  , gpA $ \ a   -> fun "?doti"  [ int, dptr a, dptr int, dptr a, ptr a, idxBase ]
  , gpC $ \ a   -> fun "?dotci" [ int, dptr a, dptr int, dptr a, ptr a, idxBase ]
  , gpA $ \ a   -> fun "?gthr"  [ int, dptr a, dptr a, dptr int, idxBase ]
  , gpA $ \ a   -> fun "?gthrz" [ int, dptr a, dptr a, dptr int, idxBase ]
  , gpR $ \ a   -> fun "?roti"  [ int, dptr a, dptr int, dptr a, ptr a, ptr a, idxBase ]
  , gpA $ \ a   -> fun "?sctr"  [ int, dptr a, dptr int, dptr a, idxBase ]
  ]

-- Level 2 (matrix-vector) operations.
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-level-2-function-reference>
--
funsL2 :: [FunGroup]
funsL2 =
  [ gpA $ \ a   -> fun "?bsrmv"             [ dir, transpose, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, int, dptr a, ptr a, dptr a ]
  , gpA $ \ a   -> fun "?bsrxmv"            [ dir, transpose, int, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, dptr int, dptr int, int, dptr a, ptr a, dptr a ]
  , gpA $ \ a   -> fun "?csrmv"             [ transpose, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, dptr a, ptr a, dptr a ]
  , gpA $ \ a   -> fun "?bsrsv2_bufferSize" [ dir, transpose, int, int, matdescr, dptr a, dptr int, dptr int, int, info_bsrsv2, ptr int ]
  , gpA $ \ a   -> fun "?bsrsv2_analysis"   [ dir, transpose, int, int, matdescr, dptr a, dptr int, dptr int, int, info_bsrsv2, policy, dptr void ]
  , gpA $ \ a   -> fun "?bsrsv2_solve"      [ dir, transpose, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, int, info_bsrsv2, dptr a, dptr a, policy, dptr void ]
  , gp  $          fun "xbsrsv2_zeroPivot"  [ info_bsrsv2, ptr int ]
  , gpA $ \ a   -> fun "?csrsv_analysis"    [ transpose, int, int, matdescr, dptr a, dptr int, dptr int, info ]
  , gpA $ \ a   -> fun "?csrsv_solve"       [ transpose, int, ptr a, matdescr, dptr a, dptr int, dptr int, info, dptr a, dptr a ]
  , gpA $ \ a   -> fun "?csrsv2_bufferSize" [ transpose, int, int, matdescr, dptr a, dptr int, dptr int, info_csrsv2, ptr int ]
  , gpA $ \ a   -> fun "?csrsv2_analysis"   [ transpose, int, int, matdescr, dptr a, dptr int, dptr int, info_csrsv2, policy, dptr void ]
  , gpA $ \ a   -> fun "?csrsv2_solve"      [ transpose, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, info_csrsv2, dptr a, dptr a, policy, dptr void ]
  , gp  $          fun "xcsrsv2_zeroPivot"  [ info_csrsv2, ptr int ]
  , gpA $ \ a   -> fun "?hybmv"             [ transpose, ptr a, matdescr, hyb, dptr a, ptr a, dptr a ]
  , gpA $ \ a   -> fun "?hybsv_analysis"    [ transpose, matdescr, hyb, info ]
  , gpA $ \ a   -> fun "?hybsv_solve"       [ transpose, ptr a, matdescr, hyb, info, dptr a, dptr a ]
  ]

-- Level 2 operations introduced in CUDA-7.5
--
funsL2_cuda75 :: [FunGroup]
funsL2_cuda75 =
  [ gpA $ \ a   -> fun "?gemvi"             [ transpose, int, int, ptr a, dptr a, int, int, dptr a, dptr int, ptr a, dptr a, idxBase, dptr void ]
  , gpA $ \ a   -> fun "?gemvi_bufferSize"  [ transpose, int, int, int, ptr int ]
  ]

-- Level 2 operations introduced in CUDA-8.0
--
funsL2_cuda80 :: [FunGroup]
funsL2_cuda80 =
  [ gp  $          fun "csrmvEx"            [ alg, transpose, int, int, int, ptr void, dtype, matdescr, dptr void, dtype, dptr int, dptr int, dptr void, dtype, ptr void, dtype, dptr void, dtype, dtype, dptr void ]
  , gp  $          fun "csrmvEx_bufferSize" [ alg, transpose, int, int, int, ptr void, dtype, matdescr, dptr void, dtype, dptr int, dptr int, dptr void, dtype, ptr void, dtype, dptr void, dtype, dtype, ptr int64 ]
  , gpA $ \ a   -> fun "?csrmv_mp"          [ transpose, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, dptr a, ptr a, dptr a ]
  , gp  $          fun "csrsv_analysisEx"   [ transpose, int, int, matdescr, dptr void, dtype, dptr int, dptr int, info, dtype ]
  , gp  $          fun "csrsv_solveEx"      [ transpose, int, ptr void, dtype, matdescr, dptr void, dtype, dptr int, dptr int, info, dptr void, dtype, dptr void, dtype, dtype ]
  ]

-- Level 3 (matrix-vector) operations (and extensions)
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-level-3-function-reference>
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-extra-function-reference>
--
funsL3 :: [FunGroup]
funsL3 =
  [ gpA $ \ a   -> fun "?csrmm"             [ transpose, int, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?csrmm2"            [ transpose, transpose, int, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?csrsm_analysis"    [ transpose, int, int, matdescr, dptr a, dptr int, dptr int, info ]
  , gpA $ \ a   -> fun "?csrsm_solve"       [ transpose, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, info, dptr a, int, dptr a, int ]
  , gpA $ \ a   -> fun "?bsrmm"             [ dir, transpose, transpose, int, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, int, dptr a, int, ptr a, dptr a, int ]
  , gpA $ \ a   -> fun "?bsrsm2_bufferSize" [ dir, transpose, transpose, int, int, int, matdescr, dptr a, dptr int, dptr int, int, info_bsrsm2, ptr int ]
  , gpA $ \ a   -> fun "?bsrsm2_analysis"   [ dir, transpose, transpose, int, int, int, matdescr, dptr a, dptr int, dptr int, int, info_bsrsm2, policy, ptr void ]
  , gpA $ \ a   -> fun "?bsrsm2_solve"      [ dir, transpose, transpose, int, int, int, ptr a, matdescr, dptr a, dptr int, dptr int, int, info_bsrsm2, dptr a, int, dptr a, int, policy, ptr void ]
  , gp  $          fun "xbsrsm2_zeroPivot"  [ info_bsrsm2, ptr int ]

  -- BLAS-like extensions
  , gp  $          fun "xcsrgeamNnz"              [ int, int, matdescr, int, dptr int, dptr int, matdescr, int, dptr int, dptr int, matdescr, dptr int, ptr int ]
  , gpA $ \ a   -> fun "?csrgeam"                 [ int, int, ptr a, matdescr, int, dptr a, dptr int, dptr int, ptr a, matdescr, int, dptr a, dptr int, dptr int, matdescr, dptr a, dptr int, dptr int ]
  , gp  $          fun "xcsrgemmNnz"              [ transpose, transpose, int, int, int, matdescr, int, dptr int, dptr int, matdescr, int, dptr int, dptr int, matdescr, dptr int, ptr int ]
  , gpA $ \ a   -> fun "?csrgemm"                 [ transpose, transpose, int, int, int, matdescr, int, dptr a, dptr int, dptr int, matdescr, int, dptr a, dptr int, dptr int, matdescr, dptr a, dptr int, dptr int ]
  ]

funsL3_cuda70 :: [FunGroup]
funsL3_cuda70 =
  [ gpA $ \ a   -> fun "?csrgemm2_bufferSizeExt"  [ int, int, int, ptr a, matdescr, int, dptr int, dptr int, matdescr, int, dptr int, dptr int, ptr a, matdescr, int, dptr int, dptr int, info_csrgemm2, ptr int64 ]
  , gp  $          fun "xcsrgemm2Nnz"             [ int, int, int, matdescr, int, dptr int, dptr int, matdescr, int, dptr int, dptr int, matdescr, int, dptr int, dptr int, matdescr, dptr int, ptr int, info_csrgemm2, dptr void ]
  , gpA $ \ a   -> fun "?csrgemm2"                [ int, int, int, ptr a, matdescr, int, dptr a, dptr int, dptr int, matdescr, int, dptr a, dptr int, dptr int, ptr a, matdescr, int, dptr a, dptr int, dptr int, matdescr, dptr a, dptr int, dptr int, info_csrgemm2, dptr void ]
  ]


data FunGroup
  = FunGroup
    { _gpName :: String
    , _gpType :: [Type]
    , gpInsts :: [FunInstance]
    }

gp :: Fun -> FunGroup
gp f = FunGroup (fName f) (fTypes f) [FunInstance [] f]

-- | Function group over @s d c z@.
gpA :: (Type -> Fun) -> FunGroup
gpA = makeFunGroup1 decorate floatingTypes

-- | Function group over @s d c z h@
gpH :: (Type -> Fun) -> FunGroup
gpH = makeFunGroup1 decorate (floatingTypes <> return half)

-- | Function group over @s d@.
gpR :: (Type -> Fun) -> FunGroup
gpR = makeFunGroup1 decorate realTypes

-- | Function group over @s d@ but relabel them as @c z@.
gpQ :: (Type -> Fun) -> FunGroup
gpQ = makeFunGroup1 (decorate . (complex <$>)) realTypes

-- | Function group over @c z@.
gpC :: (Type -> Fun) -> FunGroup
gpC = makeFunGroup1 decorate complexTypes

-- | Function group over @ss dd sc dz@.
gpB :: (Type -> Type -> Fun) -> FunGroup
gpB = makeFunGroup2 decorate floatingTypesB

-- | Function group over @ss dd cc zz cs zd@.
gpE :: (Type -> Type -> Fun) -> FunGroup
gpE = makeFunGroup2 decorate floatingTypesE

makeFunGroup1 :: ([Type] -> String -> String)
              -> [Type]
              -> (Type -> Fun)
              -> FunGroup
makeFunGroup1 d ts ff = makeFunGroup 1 d ts' ff'
  where ts'      = [ [a] | a <- ts ]
        ff' args = ff a   where [a]    = args

makeFunGroup2 :: ([Type] -> String -> String)
              -> [(Type, Type)]
              -> (Type -> Type -> Fun)
              -> FunGroup
makeFunGroup2 d ts ff = makeFunGroup 2 d ts' ff'
  where ts'      = [ [a, b] | (a, b) <- ts ]
        ff' args = ff a b where [a, b] = args

makeFunGroup :: Int
             -> ([Type] -> String -> String)
             -> [[Type]]
             -> ([Type] -> Fun)
             -> FunGroup
makeFunGroup n decorator ts ff =
  let f = ff (take n (TDummy <$> [0 ..])) in
  FunGroup (substitute "" $ fName f) (fTypes f) $ do
    t <- ts
    let f' = ff t
    return $ FunInstance t (f' { fName = decorator t $ fName f'})

data FunInstance
  = FunInstance
    { _fiArgs :: [Type]
    , fiFun   :: Fun
    }

concatFunInstances :: [FunGroup] -> [Fun]
concatFunInstances = (>>= (>>= return . fiFun) . gpInsts)

funMangler :: String -> String -> String
funMangler []     _   = error "funMangler: empty input"
funMangler (x:xs) suf = printf "cusparse%c%s%s" (toUpper x) xs suf

mangleFun :: Safety -> Fun -> CFun
mangleFun safety (Fun name suffix params doc) =
  CFun (safety==Safe) name suffix (THandle : params) TStatus doc

