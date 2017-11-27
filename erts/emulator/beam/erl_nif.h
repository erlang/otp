/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2017. All Rights Reserved.
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

/* Include file for writers of Native Implemented Functions. 
*/

#ifndef __ERL_NIF_H__
#define __ERL_NIF_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_drv_nif.h"

/* Version history:
** 0.1: R13B03
** 1.0: R13B04
** 2.0: R14A
** 2.1: R14B02 "vm_variant"
** 2.2: R14B03 enif_is_exception
** 2.3: R15 enif_make_reverse_list, enif_is_number
** 2.4: R16 enif_consume_timeslice
** 2.5: First experimental maps API additions (libs of this version is not compatible with any other VM)
** 2.5: R17 Maps API additions
** 2.6: R17 with maps
**      R17 dirty schedulers
** 2.7: 17.3 add enif_schedule_nif
**           remove enif_schedule_dirty_nif, enif_schedule_dirty_nif_finalizer, enif_dirty_nif_finalizer
**           add ErlNifEntry options
**           add ErlNifFunc flags
** 2.8: 18.0 add enif_has_pending_exception
** 2.9: 18.2 enif_getenv
** 2.10: Time API
** 2.11: 19.0 enif_snprintf 
** 2.12: 20.0 add enif_select, enif_open_resource_type_x
** 2.13: 20.1 add enif_ioq
*/
#define ERL_NIF_MAJOR_VERSION 2
#define ERL_NIF_MINOR_VERSION 13

/*
 * The emulator will refuse to load a nif-lib with a major version
 * lower than ERL_NIF_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD. The load
 * may however fail if user have not removed use of deprecated
 * symbols.
 *
 * The ERL_NIF_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD have to allow
 * loading of nif-libs built at least two major OTP releases
 * ago.
 */
#define ERL_NIF_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD 2

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef ErlNapiUInt64 ErlNifUInt64;
typedef ErlNapiSInt64 ErlNifSInt64;
typedef ErlNapiUInt ErlNifUInt;
typedef ErlNapiSInt ErlNifSInt;

#  define ERL_NIF_VM_VARIANT "beam.vanilla" 
typedef ErlNifUInt ERL_NIF_TERM;

typedef ERL_NIF_TERM ERL_NIF_UINT;

typedef ErlNifSInt64 ErlNifTime;

#define ERL_NIF_TIME_ERROR ((ErlNifSInt64) ERTS_NAPI_TIME_ERROR__)

typedef enum {
    ERL_NIF_SEC    = ERTS_NAPI_SEC__,
    ERL_NIF_MSEC   = ERTS_NAPI_MSEC__,
    ERL_NIF_USEC   = ERTS_NAPI_USEC__,
    ERL_NIF_NSEC   = ERTS_NAPI_NSEC__
} ErlNifTimeUnit;

struct enif_environment_t;
typedef struct enif_environment_t ErlNifEnv;

typedef struct enif_func_t
{
    const char* name;
    unsigned arity;
    ERL_NIF_TERM (*fptr)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    unsigned flags;
}ErlNifFunc;


typedef struct enif_entry_t
{
    int major;
    int minor;
    const char* name;
    int num_of_funcs;
    ErlNifFunc* funcs;
    int  (*load)   (ErlNifEnv*, void** priv_data, ERL_NIF_TERM load_info);
    int  (*reload) (ErlNifEnv*, void** priv_data, ERL_NIF_TERM load_info);
    int  (*upgrade)(ErlNifEnv*, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
    void (*unload) (ErlNifEnv*, void* priv_data);

    /* Added in 2.1 */
    const char* vm_variant;

    /* Added in 2.7 */
    unsigned options;   /* Unused. Can be set to 0 or 1 (dirty sched config) */

    /* Added in 2.12 */
    size_t sizeof_ErlNifResourceTypeInit;
}ErlNifEntry;


typedef struct
{
    size_t size;
    unsigned char* data;

    /* Internals (avert your eyes) */
    ERL_NIF_TERM bin_term;
    void* ref_bin;
}ErlNifBinary;

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
typedef void* ErlNifEvent; /* FIXME: Use 'HANDLE' somehow without breaking existing source */
#else
typedef int ErlNifEvent;
#endif

/* Return bits from enif_select: */
#define ERL_NIF_SELECT_STOP_CALLED    (1 << 0)
#define ERL_NIF_SELECT_STOP_SCHEDULED (1 << 1)
#define ERL_NIF_SELECT_INVALID_EVENT  (1 << 2)
#define ERL_NIF_SELECT_FAILED         (1 << 3)

typedef enum
{
    ERL_NIF_RT_CREATE = 1,
    ERL_NIF_RT_TAKEOVER = 2
}ErlNifResourceFlags;

typedef enum
{
    ERL_NIF_LATIN1 = 1
}ErlNifCharEncoding;

typedef struct
{
    ERL_NIF_TERM pid;  /* internal, may change */
} ErlNifPid;

typedef struct
{
    ERL_NIF_TERM port_id;  /* internal, may change */
}ErlNifPort;

typedef ErlDrvMonitor ErlNifMonitor;

typedef struct enif_resource_type_t ErlNifResourceType;
typedef void ErlNifResourceDtor(ErlNifEnv*, void*);
typedef void ErlNifResourceStop(ErlNifEnv*, void*, ErlNifEvent, int is_direct_call);
typedef void ErlNifResourceDown(ErlNifEnv*, void*, ErlNifPid*, ErlNifMonitor*);

typedef struct {
    ErlNifResourceDtor* dtor;
    ErlNifResourceStop* stop;  /* at ERL_NIF_SELECT_STOP event */
    ErlNifResourceDown* down;  /* enif_monitor_process */
} ErlNifResourceTypeInit;

typedef ErlDrvSysInfo ErlNifSysInfo;

typedef struct ErlDrvTid_ *ErlNifTid;
typedef struct ErlDrvMutex_ ErlNifMutex;
typedef struct ErlDrvCond_ ErlNifCond;
typedef struct ErlDrvRWLock_ ErlNifRWLock;
typedef int ErlNifTSDKey;

typedef ErlDrvThreadOpts ErlNifThreadOpts;

typedef enum
{
    ERL_NIF_DIRTY_JOB_CPU_BOUND = ERL_DIRTY_JOB_CPU_BOUND,
    ERL_NIF_DIRTY_JOB_IO_BOUND  = ERL_DIRTY_JOB_IO_BOUND
}ErlNifDirtyTaskFlags;

typedef struct /* All fields all internal and may change */
{
    ERL_NIF_TERM map;
    ERL_NIF_UINT size;
    ERL_NIF_UINT idx;
    union {
        struct {
            ERL_NIF_TERM *ks;
            ERL_NIF_TERM *vs;
        }flat;
        struct {
            struct ErtsDynamicWStack_* wstack;
            ERL_NIF_TERM* kv;
        }hash;
    }u;
    void* __spare__[2]; /* for future additions to be ABI compatible (same struct size) */
} ErlNifMapIterator;

typedef enum {
    ERL_NIF_MAP_ITERATOR_FIRST = 1,
    ERL_NIF_MAP_ITERATOR_LAST = 2,

    /* deprecated synonyms (undocumented in 17 and 18-rc) */
    ERL_NIF_MAP_ITERATOR_HEAD = ERL_NIF_MAP_ITERATOR_FIRST,
    ERL_NIF_MAP_ITERATOR_TAIL = ERL_NIF_MAP_ITERATOR_LAST
} ErlNifMapIteratorEntry;

typedef enum {
    ERL_NIF_UNIQUE_POSITIVE = (1 << 0),
    ERL_NIF_UNIQUE_MONOTONIC = (1 << 1)
} ErlNifUniqueInteger;

typedef enum {
    ERL_NIF_BIN2TERM_SAFE = 0x20000000
} ErlNifBinaryToTerm;

typedef enum {
    ERL_NIF_INTERNAL_HASH = 1,
    ERL_NIF_PHASH2 = 2
} ErlNifHash;

#define ERL_NIF_IOVEC_SIZE 16

typedef struct erl_nif_io_vec {
    int iovcnt;  /* length of vectors */
    size_t size; /* total size in bytes */
    SysIOVec *iov;

    /* internals (avert your eyes) */
    void **ref_bins; /* Binary[] */
    int flags;

    /* Used when stack allocating the io vec */
    SysIOVec small_iov[ERL_NIF_IOVEC_SIZE];
    void *small_ref_bin[ERL_NIF_IOVEC_SIZE];
} ErlNifIOVec;

typedef struct erts_io_queue ErlNifIOQueue;

typedef enum {
    ERL_NIF_IOQ_NORMAL = 1
} ErlNifIOQueueOpts;

/*
 * Return values from enif_thread_type(). Negative values
 * reserved for specific types of non-scheduler threads.
 * Positive values reserved for scheduler thread types.
 */

#define ERL_NIF_THR_UNDEFINED 0
#define ERL_NIF_THR_NORMAL_SCHEDULER 1
#define ERL_NIF_THR_DIRTY_CPU_SCHEDULER 2
#define ERL_NIF_THR_DIRTY_IO_SCHEDULER 3

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
#  define ERL_NIF_API_FUNC_DECL(RET_TYPE, NAME, ARGS) RET_TYPE (*NAME) ARGS
typedef struct {
#  include "erl_nif_api_funcs.h"
   void* erts_alc_test;
} TWinDynNifCallbacks;
extern TWinDynNifCallbacks WinDynNifCallbacks;
#  undef ERL_NIF_API_FUNC_DECL
#endif

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_)) && !defined(STATIC_ERLANG_DRIVER) && !defined(STATIC_ERLANG_NIF)
#  define ERL_NIF_API_FUNC_MACRO(NAME) (WinDynNifCallbacks.NAME)
#  include "erl_nif_api_funcs.h"
/* note that we have to keep ERL_NIF_API_FUNC_MACRO defined */

#else /* non windows or included from emulator itself */

#  define ERL_NIF_API_FUNC_DECL(RET_TYPE, NAME, ARGS) extern RET_TYPE NAME ARGS
#  include "erl_nif_api_funcs.h"
#  undef ERL_NIF_API_FUNC_DECL
#endif

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
#  define ERL_NIF_INIT_GLOB TWinDynNifCallbacks WinDynNifCallbacks;
#  define ERL_NIF_INIT_ARGS TWinDynNifCallbacks* callbacks
#  define ERL_NIF_INIT_BODY memcpy(&WinDynNifCallbacks,callbacks,sizeof(TWinDynNifCallbacks))
#  define ERL_NIF_INIT_EXPORT __declspec(dllexport)
#else 
#  define ERL_NIF_INIT_GLOB
#  define ERL_NIF_INIT_ARGS void
#  define ERL_NIF_INIT_BODY
#  if defined(__GNUC__) && __GNUC__ >= 4
#    define ERL_NIF_INIT_EXPORT __attribute__ ((visibility("default")))
#  elif defined (__SUNPRO_C) && (__SUNPRO_C >= 0x550)
#    define ERL_NIF_INIT_EXPORT __global
#  else
#    define ERL_NIF_INIT_EXPORT
#  endif
#endif

#ifdef STATIC_ERLANG_NIF
#  define ERL_NIF_INIT_DECL(MODNAME) ErlNifEntry* MODNAME ## _nif_init(ERL_NIF_INIT_ARGS)
#else
#  define ERL_NIF_INIT_DECL(MODNAME) ERL_NIF_INIT_EXPORT ErlNifEntry* nif_init(ERL_NIF_INIT_ARGS)
#endif

#ifdef __cplusplus
}
#  define ERL_NIF_INIT_PROLOGUE extern "C" {
#  define ERL_NIF_INIT_EPILOGUE }
#else
#  define ERL_NIF_INIT_PROLOGUE
#  define ERL_NIF_INIT_EPILOGUE
#endif


#define ERL_NIF_INIT(NAME, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD) \
ERL_NIF_INIT_PROLOGUE                   \
ERL_NIF_INIT_GLOB                       \
ERL_NIF_INIT_DECL(NAME);		\
ERL_NIF_INIT_DECL(NAME)			\
{					\
    static ErlNifEntry entry = 		\
    {					\
	ERL_NIF_MAJOR_VERSION,		\
	ERL_NIF_MINOR_VERSION,		\
	#NAME,				\
	sizeof(FUNCS) / sizeof(*FUNCS),	\
	FUNCS,				\
	LOAD, RELOAD, UPGRADE, UNLOAD,	\
	ERL_NIF_VM_VARIANT,		\
        1,                              \
        sizeof(ErlNifResourceTypeInit)  \
    };                                  \
    ERL_NIF_INIT_BODY;                  \
    return &entry;			\
}                                       \
ERL_NIF_INIT_EPILOGUE

#if defined(USE_DYNAMIC_TRACE) && (defined(USE_DTRACE) || defined(USE_SYSTEMTAP))
#define HAVE_USE_DTRACE 1
#endif

#ifdef HAVE_USE_DTRACE
ERL_NIF_TERM erl_nif_user_trace_s1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM erl_nif_user_trace_i4s4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM erl_nif_user_trace_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#endif

#endif /* __ERL_NIF_H__ */

