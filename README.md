Haskell FFI Bindings to cuSPARSE
================================

[![Build status](https://travis-ci.org/tmcdonell/cusparse.svg?branch=master)](https://travis-ci.org/tmcdonell/cusparse)
[![Hackage](https://img.shields.io/hackage/v/cusparse.svg)](https://hackage.haskell.org/package/cusparse)

The cuSPARSE library contains a set of basic linear algebra subroutines used
for handling sparse matrices. Sparse vectors and matrices are those where the
majority of elements are zero. Sparse BLAS routines are specifically
implemented to take advantage of this sparsity. This package provides FFI
bindings to the functions of the cuSPARSE library. You will need to install the
CUDA driver and developer toolkit:

  <http://developer.nvidia.com/cuda-downloads>

  <http://docs.nvidia.com/cuda/cusparse/index.html>

