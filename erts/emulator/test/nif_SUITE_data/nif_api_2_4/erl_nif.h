/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2017. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

/* Include file for writers of Native Implemented Functions. 
*/

#ifndef __ERL_NIF_H__
#define __ERL_NIF_H__


#include "erl_drv_nif.h"

/* Version history:
** 0.1: R13B03
** 1.0: R13B04
** 2.0: R14A
** 2.1: R14B02 "vm_variant"
** 2.2: R14B03 enif_is_exception
** 2.3: R15 enif_make_reverse_list, enif_is_number
** 2.4: R16 enif_consume_timeslice
*/
#define ERL_NIF_MAJOR_VERSION 2
#define ERL_NIF_MINOR_VERSION 4

#include <stdlib.h>

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
#ifdef HALFWORD_HEAP_EMULATOR
#  define HALFWORD_HEAP_EMULATOR_SAVED__ HALFWORD_HEAP_EMULATOR
#  undef HALFWORD_HEAP_EMULATOR
#endif
#include "erl_int_sizes_config.h"

#ifdef __cplusplus
extern "C" {
#endif

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
typedef unsigned __int64 ErlNifUInt64;
typedef __int64 ErlNifSInt64;
#elif SIZEOF_LONG == 8
typedef unsigned long ErlNifUInt64;
typedef long ErlNifSInt64;
#elif SIZEOF_LONG_LONG == 8
typedef unsigned long long ErlNifUInt64;
typedef long long ErlNifSInt64;
#else
#error No 64-bit integer type
#endif

#ifdef HALFWORD_HEAP_EMULATOR
#  define ERL_NIF_VM_VARIANT "beam.halfword" 
typedef unsigned int ERL_NIF_TERM;
#else
#  define ERL_NIF_VM_VARIANT "beam.vanilla" 
#  if SIZEOF_LONG == SIZEOF_VOID_P
typedef unsigned long ERL_NIF_TERM;
#  elif SIZEOF_LONG_LONG == SIZEOF_VOID_P
typedef unsigned long long ERL_NIF_TERM;
#  endif
#endif

struct enif_environment_t;
typedef struct enif_environment_t ErlNifEnv;

typedef struct
{
    const char* name;
    unsigned arity;
    ERL_NIF_TERM (*fptr)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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
    const char* vm_variant;
}ErlNifEntry;



typedef struct
{
    size_t size;
    unsigned char* data;

    /* Internals (avert your eyes) */
    ERL_NIF_TERM bin_term;
    void* ref_bin;
}ErlNifBinary;

typedef struct enif_resource_type_t ErlNifResourceType;
typedef void ErlNifResourceDtor(ErlNifEnv*, void*);
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
}ErlNifPid;

typedef ErlDrvSysInfo ErlNifSysInfo;

typedef struct ErlDrvTid_ *ErlNifTid;
typedef struct ErlDrvMutex_ ErlNifMutex;
typedef struct ErlDrvCond_ ErlNifCond;
typedef struct ErlDrvRWLock_ ErlNifRWLock;
typedef int ErlNifTSDKey;

typedef ErlDrvThreadOpts ErlNifThreadOpts;

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
#  define ERL_NIF_API_FUNC_DECL(RET_TYPE, NAME, ARGS) RET_TYPE (*NAME) ARGS
typedef struct {
#  include "erl_nif_api_funcs.h"
} TWinDynNifCallbacks;
extern TWinDynNifCallbacks WinDynNifCallbacks;
#  undef ERL_NIF_API_FUNC_DECL
#endif

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_)) && !defined(STATIC_ERLANG_DRIVER)
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
#  define ERL_NIF_INIT_DECL(MODNAME) __declspec(dllexport) ErlNifEntry* nif_init(TWinDynNifCallbacks* callbacks)
#  define ERL_NIF_INIT_BODY memcpy(&WinDynNifCallbacks,callbacks,sizeof(TWinDynNifCallbacks))
#else 
#  define ERL_NIF_INIT_GLOB
#  define ERL_NIF_INIT_BODY
#  define ERL_NIF_INIT_DECL(MODNAME) ErlNifEntry* nif_init(void)
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
	ERL_NIF_VM_VARIANT		\
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

