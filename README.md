Haskell FFI Bindings to cuSPARSE
================================

[![Travis build status](https://img.shields.io/travis/tmcdonell/cusparse/master.svg?label=linux)](https://travis-ci.org/tmcdonell/cusparse)
[![AppVeyor build status](https://img.shields.io/appveyor/ci/tmcdonell/cusparse/master.svg?label=windows)](https://ci.appveyor.com/project/tmcdonell/cusparse)
[![Stackage LTS](https://stackage.org/package/cusparse/badge/lts)](https://stackage.org/lts/package/cusparse)
[![Stackage Nightly](https://stackage.org/package/cusparse/badge/nightly)](https://stackage.org/nightly/package/cusparse)
[![Hackage](https://img.shields.io/hackage/v/cusparse.svg)](https://hackage.haskell.org/package/cusparse)

The cuSPARSE library contains a set of basic linear algebra subroutines for
handling sparse matrices. Sparse vectors and matrices are those where the
majority of elements are zero. Sparse BLAS routines are specifically implemented
to take advantage of this sparsity. This package provides FFI bindings to the
functions of the cuSPARSE library. You will need to install the CUDA driver and
developer toolkit:

  <http://developer.nvidia.com/cuda-downloads>

  <http://docs.nvidia.com/cuda/cusparse/index.html>

