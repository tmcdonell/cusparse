/*
 * Extra bits for cuSPARSE bindings
 */

#ifndef C_STUBS_H
#define C_STUBS_H

#ifdef __MINGW32__
#include <host_defines.h>
#undef CUDARTAPI
#define CUDARTAPI __stdcall
#endif

#include <cuda.h>
#include <cusparse_v2.h>

#endif /* C_STUBS_H */

