/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2010-2025. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Common structures for both erl_driver.h and erl_nif.h
 */

#ifndef __ERL_DRV_NIF_H__
#define __ERL_DRV_NIF_H__

typedef struct {
    int driver_major_version;
    int driver_minor_version;
    char *erts_version;
    char *otp_release;
    int thread_support;
    int smp_support;
    int async_threads;
    int scheduler_threads;
    int nif_major_version;
    int nif_minor_version;
    int dirty_scheduler_support;
}  ErlDrvSysInfo;

typedef struct {
    int suggested_stack_size;
} ErlDrvThreadOpts;


typedef enum {
    ERL_DIRTY_JOB_CPU_BOUND = 1,
    ERL_DIRTY_JOB_IO_BOUND  = 2
} ErlDirtyJobFlags;

/* Values for enif_select AND mode arg for driver_select() */
enum ErlNifSelectFlags {
    ERL_NIF_SELECT_READ      = (1 << 0),
    ERL_NIF_SELECT_WRITE     = (1 << 1),
    ERL_NIF_SELECT_STOP      = (1 << 2),
    ERL_NIF_SELECT_CANCEL    = (1 << 3),
    ERL_NIF_SELECT_CUSTOM_MSG= (1 << 4),
    ERL_NIF_SELECT_ERROR     = (1 << 5)
};

/*
 * A driver monitor
 */
typedef struct {
    unsigned char data[sizeof(void *)*4];
} ErlDrvMonitor;


#ifdef SIZEOF_CHAR
#  define SIZEOF_CHAR_SAVED__ SIZEOF_CHAR
#  undef SIZEOF_CHAR
#endif
#ifdef SIZEOF_SHORT
#  define SIZEOF_SHORT_SAVED__ SIZEOF_SHORT
#  undef SIZEOF_SHORT
#endif
#ifdef SIZEOF_INT
#  define SIZEOF_INT_SAVED__ SIZEOF_INT
#  undef SIZEOF_INT
#endif
#ifdef SIZEOF_LONG
#  define SIZEOF_LONG_SAVED__ SIZEOF_LONG
#  undef SIZEOF_LONG
#endif
#ifdef SIZEOF_LONG_LONG
#  define SIZEOF_LONG_LONG_SAVED__ SIZEOF_LONG_LONG
#  undef SIZEOF_LONG_LONG
#endif
#include "erl_int_sizes_config.h"
#if defined(SIZEOF_CHAR_SAVED__) && SIZEOF_CHAR_SAVED__ != SIZEOF_CHAR
#  error SIZEOF_CHAR mismatch
#endif
#if defined(SIZEOF_SHORT_SAVED__) && SIZEOF_SHORT_SAVED__ != SIZEOF_SHORT
#  error SIZEOF_SHORT mismatch
#endif
#if defined(SIZEOF_INT_SAVED__) && SIZEOF_INT_SAVED__ != SIZEOF_INT
#  error SIZEOF_INT mismatch
#endif
#if defined(SIZEOF_LONG_SAVED__) && SIZEOF_LONG_SAVED__ != SIZEOF_LONG
#  error SIZEOF_LONG mismatch
#endif
#if defined(SIZEOF_LONG_LONG_SAVED__) && SIZEOF_LONG_LONG_SAVED__ != SIZEOF_LONG_LONG
#  error SIZEOF_LONG_LONG mismatch
#endif

#if !defined(__GNUC__) && (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
typedef unsigned __int64 ErlNapiUInt64;
typedef signed __int64 ErlNapiSInt64;
#define ERL_NAPI_SINT64_MAX__ 9223372036854775807i64
#define ERL_NAPI_SINT64_MIN__ (-ERL_NAPI_SINT64_MAX__ - 1i64)
#elif SIZEOF_LONG == 8
typedef unsigned long ErlNapiUInt64;
typedef signed long ErlNapiSInt64;
#define ERL_NAPI_SINT64_MAX__ 9223372036854775807L
#define ERL_NAPI_SINT64_MIN__ (-ERL_NAPI_SINT64_MAX__ - 1L)
#elif SIZEOF_LONG_LONG == 8
typedef unsigned long long ErlNapiUInt64;
typedef signed long long ErlNapiSInt64;
#define ERL_NAPI_SINT64_MAX__ 9223372036854775807LL
#define ERL_NAPI_SINT64_MIN__ (-ERL_NAPI_SINT64_MAX__ - 1LL)
#else
#  error No 64-bit integer type
#endif

#if SIZEOF_VOID_P == 8
typedef ErlNapiUInt64 ErlNapiUInt;
typedef ErlNapiSInt64 ErlNapiSInt;
#elif SIZEOF_VOID_P == 4
#  if SIZEOF_LONG == SIZEOF_VOID_P
typedef unsigned long ErlNapiUInt;
typedef signed long ErlNapiSInt;
#  elif SIZEOF_INT == SIZEOF_VOID_P
typedef unsigned int ErlNapiUInt;
typedef signed int ErlNapiSInt;
#  else
#    error No 32-bit integer type
#  endif
#else
#  error Not support arch
#endif

#define ERTS_NAPI_TIME_ERROR__ ERL_NAPI_SINT64_MIN__

#define ERTS_NAPI_SEC__		0
#define ERTS_NAPI_MSEC__	1
#define ERTS_NAPI_USEC__	2
#define ERTS_NAPI_NSEC__	3

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
/*
 * This structure can be cast to a WSABUF structure.
 */
typedef struct _SysIOVec {
    unsigned long iov_len;
    char* iov_base;
} SysIOVec;
#else  /* Unix */
#  include <sys/types.h>
#  ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
typedef struct iovec SysIOVec;
#  else
typedef struct {
    char* iov_base;
    size_t iov_len;
} SysIOVec;
#  endif
#endif

#define ERL_NAPI_ATTR_WUR
#define ERL_NAPI_ATTR_ALLOC_SIZE(SZPOS)
#define ERL_NAPI_ATTR_MALLOC_U
#define ERL_NAPI_ATTR_MALLOC_US(SZPOS)
#define ERL_NAPI_ATTR_MALLOC_UD(DTOR, PTRPOS)
#define ERL_NAPI_ATTR_MALLOC_USD(SZPOS, DTOR, PTRPOS)
#define ERL_NAPI_ATTR_MALLOC_D(DTOR, PTRPOS)

/* ERL_NAPI_ATTR_MALLOC_xxx:
 * U: Returns pointer to Undefined data. ((malloc))
 * S: Has Size argument with nr of bytes of returned data. ((alloc_size(SZPOS)))
 * D: Has 1-to-1 Deallocator function with ptr argument. ((malloc(DTOR,PTRPOS)))
 */

#if defined(__has_attribute) && !defined(__WIN32__)
#  if __has_attribute(warn_unused_result)
#    undef  ERL_NAPI_ATTR_WUR
#    define ERL_NAPI_ATTR_WUR __attribute__((warn_unused_result))
#  endif
#  if __has_attribute(alloc_size)
#    undef  ERL_NAPI_ATTR_ALLOC_SIZE
#    define ERL_NAPI_ATTR_ALLOC_SIZE(SZPOS)                                \
         __attribute__((alloc_size(SZPOS))) ERL_NAPI_ATTR_WUR
#  endif
#  if __has_attribute(malloc)
#    undef  ERL_NAPI_ATTR_MALLOC_U
#    define ERL_NAPI_ATTR_MALLOC_U __attribute__((malloc)) ERL_NAPI_ATTR_WUR

#    undef  ERL_NAPI_ATTR_MALLOC_US
#    define ERL_NAPI_ATTR_MALLOC_US(SZPOS)                                 \
         __attribute__((malloc)) ERL_NAPI_ATTR_ALLOC_SIZE(SZPOS)

#    undef  ERL_NAPI_ATTR_MALLOC_D
#    if defined(__GNUC__) && __GNUC__ >= 11
#      define ERL_NAPI_ATTR_MALLOC_D(DTOR, PTRPOS)                         \
         __attribute__((malloc(DTOR,PTRPOS)))                              \
         ERL_NAPI_ATTR_WUR
#    else
#      define ERL_NAPI_ATTR_MALLOC_D(DTOR, PTRPOS)                         \
         ERL_NAPI_ATTR_WUR
#    endif

#    undef  ERL_NAPI_ATTR_MALLOC_UD
#    define ERL_NAPI_ATTR_MALLOC_UD(DTOR, PTRPOS)                          \
       ERL_NAPI_ATTR_MALLOC_U                                              \
       ERL_NAPI_ATTR_MALLOC_D(DTOR, PTRPOS)

#    undef  ERL_NAPI_ATTR_MALLOC_USD
#    define ERL_NAPI_ATTR_MALLOC_USD(SZPOS, DTOR, PTRPOS)                  \
       ERL_NAPI_ATTR_MALLOC_US(SZPOS)                                      \
       ERL_NAPI_ATTR_MALLOC_D(DTOR, PTRPOS)
#  endif
#endif

#endif  /* __ERL_DRV_NIF_H__ */
