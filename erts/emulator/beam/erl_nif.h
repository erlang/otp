/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009. All Rights Reserved.
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

#define ERL_NIF_MAJOR_VERSION 0
#define ERL_NIF_MINOR_VERSION 1

#include <stdlib.h>

typedef unsigned long ERL_NIF_TERM;

typedef struct
{
    const char* name;
    unsigned arity;
    void* fptr; //ERL_NIF_TERM (*fptr)(void*, ...);
}ErlNifFunc;

struct enif_environment_t;
typedef struct enif_environment_t ErlNifEnv;

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
}ErlNifEntry;



typedef struct
{
    unsigned size;
    unsigned char* data;

    /* Internals (avert your eyes) */
    ERL_NIF_TERM bin_term;
    unsigned char* tmp_alloc;
    void* ref_bin;
    
}ErlNifBinary;

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
#  if defined(VXWORKS)
#    define ERL_NIF_INIT_DECL(MODNAME) ErlNifEntry* MODNAME  ## _init(void)
#  else
#    define ERL_NIF_INIT_DECL(MODNAME) ErlNifEntry* nif_init(void)
#  endif
#endif


#define ERL_NIF_INIT(NAME, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD) \
ERL_NIF_INIT_GLOB                       \
ERL_NIF_INIT_DECL(NAME)			\
{					\
    static ErlNifEntry entry = 		\
    {					\
	ERL_NIF_MAJOR_VERSION,		\
	ERL_NIF_MINOR_VERSION,		\
	#NAME,				\
	sizeof(FUNCS) / sizeof(*FUNCS),	\
	FUNCS,				\
	LOAD, RELOAD, UPGRADE, UNLOAD	\
    };                                  \
    ERL_NIF_INIT_BODY;                  \
    return &entry;			\
}

