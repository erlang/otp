/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2015. All Rights Reserved.
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

#ifndef ERL_MSACC_H__
#define ERL_MSACC_H__

/* Can be enabled/disabled via configure */
#if ERTS_ENABLE_MSACC == 2
#define ERTS_MSACC_EXTENDED_STATES 1
#endif

/* Uncomment this to also count the number of
   transitions to a state. This will add a count
   to the counter map. */
/* #define ERTS_MSACC_STATE_COUNTERS 1 */

/* Uncomment this to make msacc to always be on,
   this reduces overhead a little bit when profiling */
/* #define ERTS_MSACC_ALWAYS_ON 1 */

/* Uncomment this to keep individual stats for all
   of the bifs when extended states is enabled */
/* #define ERTS_MSACC_EXTENDED_BIFS 1 */

#define ERTS_MSACC_DISABLE 0
#define ERTS_MSACC_ENABLE  1
#define ERTS_MSACC_RESET   2
#define ERTS_MSACC_GATHER  3

/*
 * When adding a new state, you have to:
 * * Add it here
 * * Increment ERTS_MSACC_STATE_COUNT
 * * Add string value to erts_msacc_states
 * * Have to be in alphabetical order!
 * * Only add states to the non-extended section after
 *   careful benchmarking to make sure the overhead
 *   when disabled is minimal.
 */

#ifndef ERTS_MSACC_EXTENDED_STATES
#define ERTS_MSACC_STATE_AUX       0
#define ERTS_MSACC_STATE_CHECK_IO  1
#define ERTS_MSACC_STATE_EMULATOR  2
#define ERTS_MSACC_STATE_GC        3
#define ERTS_MSACC_STATE_OTHER     4
#define ERTS_MSACC_STATE_PORT      5
#define ERTS_MSACC_STATE_SLEEP     6

#define ERTS_MSACC_STATE_COUNT 7

#if ERTS_MSACC_STATE_STRINGS && ERTS_ENABLE_MSACC
static char *erts_msacc_states[] = {
    "aux",
    "check_io",
    "emulator",
    "gc",
    "other",
    "port",
    "sleep"
};
#endif

#else

#define ERTS_MSACC_STATE_ALLOC     0
#define ERTS_MSACC_STATE_AUX       1
#define ERTS_MSACC_STATE_BIF       2
#define ERTS_MSACC_STATE_BUSY_WAIT 3
#define ERTS_MSACC_STATE_CHECK_IO  4
#define ERTS_MSACC_STATE_EMULATOR  5
#define ERTS_MSACC_STATE_ETS       6
#define ERTS_MSACC_STATE_GC        7
#define ERTS_MSACC_STATE_GC_FULL   8
#define ERTS_MSACC_STATE_NIF       9
#define ERTS_MSACC_STATE_OTHER     10
#define ERTS_MSACC_STATE_PORT      11
#define ERTS_MSACC_STATE_SEND      12
#define ERTS_MSACC_STATE_SLEEP     13
#define ERTS_MSACC_STATE_TIMERS    14

#define ERTS_MSACC_STATIC_STATE_COUNT 15

#ifdef ERTS_MSACC_EXTENDED_BIFS
#define ERTS_MSACC_STATE_COUNT (ERTS_MSACC_STATIC_STATE_COUNT + BIF_SIZE)
#else
#define ERTS_MSACC_STATE_COUNT ERTS_MSACC_STATIC_STATE_COUNT
#endif

#if ERTS_MSACC_STATE_STRINGS
static char *erts_msacc_states[] = {
    "alloc",
    "aux",
    "bif",
    "busy_wait",
    "check_io",
    "emulator",
    "ets",
    "gc",
    "gc_full",
    "nif",
    "other",
    "port",
    "send",
    "sleep",
    "timers"
#ifdef ERTS_MSACC_EXTENDED_BIFS
#define BIF_LIST(Mod,Func,Arity,FuncAddr,Num)   \
        ,"bif_" #Mod "_" #Func "_" #Arity
#include "erl_bif_list.h"
#undef BIF_LIST
#endif
};
#endif

#endif

typedef struct erl_msacc_t_ ErtsMsAcc;
typedef struct erl_msacc_p_cnt_t_ {
    ErtsSysPerfCounter pc;
#ifdef ERTS_MSACC_STATE_COUNTERS
    Uint64 sc;
#endif
} ErtsMsAccPerfCntr;

struct erl_msacc_t_ {

    /* protected by msacc_mutex in erl_msacc.c, and should be constant */
    int unmanaged;
    erts_mtx_t mtx;
    ErtsMsAcc *next;
    erts_tid_t tid;
    Eterm id;
    char *type;

    /* the the values below are protected by mtx iff unmanaged = 1 */
    ErtsSysPerfCounter perf_counter;
    Uint state;
    ErtsMsAccPerfCntr counters[];

};

#if ERTS_ENABLE_MSACC

#ifdef USE_THREADS
extern erts_tsd_key_t erts_msacc_key;
#else
extern ErtsMsAcc *erts_msacc;
#endif

#ifdef ERTS_MSACC_ALWAYS_ON
#define erts_msacc_enabled 1
#else
extern int erts_msacc_enabled;
#endif

#ifdef USE_THREADS
#define ERTS_MSACC_TSD_GET() erts_tsd_get(erts_msacc_key)
#define ERTS_MSACC_TSD_SET(tsd) erts_tsd_set(erts_msacc_key,tsd)
#else
#define ERTS_MSACC_TSD_GET() erts_msacc
#define ERTS_MSACC_TSD_SET(tsd) erts_msacc = tsd
#endif

void erts_msacc_early_init(void);
void erts_msacc_init(void);
void erts_msacc_init_thread(char *type, int id, int liberty);

/* The defines below are used to instrument the vm code
 * with different state changes. There are two variants
 * of each define. One that has a cached ErtsMsAcc *
 * that it can use, and one that does not.
 * The cached values are necessary to have in order to get
 * low enough overhead when running without msacc enabled.
 *
 * The two most common patterns to use the function with are:
 *
 *   ERTS_MSACC_PUSH_AND_SET_STATE(ERTS_MSACC_STATE_NEWSTATE);
 *     ... call other function in new state ...
 *   ERTS_MSACC_POP_STATE();
 *
 * Note that the erts_msacc_push* function declare new variables, so
 * to conform with C89 we have to call it in the beginning of a function.
 * We might not want to change state it the beginning though, so we use this:
 *
 *   ERTS_MSACC_PUSH_STATE();
 *     ... some other code ...
 *   ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_NEWSTATE);
 *     ... call other function in new state ...
 *   ERTS_MSACC_POP_STATE();
 *
 * Notice that we used the cached version of set_state as push_state already
 * read the erts_msacc_enabled to the cache.
 *
 * Most macros also have other variants with the suffix _m which means that
 * they are known to only be called in managed threads, or with the _x suffix
 * which means that it should only be used in an emulator compiled with
 * extended states.
 *
 * Here is a listing of the entire api:
 *
 *  void ERTS_MSACC_DECLARE_CACHE()
 *  void ERTS_MSACC_UPDATE_CACHE()
 *  void ERTS_MSACC_IS_ENABLED()
 *  void ERTS_MSACC_IS_ENABLED_CACHED()
 *
 *  void ERTS_MSACC_PUSH_STATE()
 *  void ERTS_MSACC_SET_STATE(int state)
 *  void ERTS_MSACC_PUSH_AND_SET_STATE(int state)
 *
 *  void ERTS_MSACC_PUSH_STATE_CACHED()
 *  void ERTS_MSACC_SET_STATE_CACHED(int state)
 *  void ERTS_MSACC_PUSH_AND_SET_STATE_CACHED(int state)
 *  void ERTS_MSACC_POP_STATE()
 *
 *  void ERTS_MSACC_PUSH_STATE_M()
 *  void ERTS_MSACC_PUSH_STATE_CACHED_M()
 *  void ERTS_MSACC_SET_STATE_CACHED_M(int state)
 *  void ERTS_MSACC_SET_STATE_M(int state)
 *  void ERTS_MSACC_POP_STATE_M()
 *  void ERTS_MSACC_PUSH_AND_SET_STATE_M(int state)
 *
 *  Most functions are also available with an _x suffix that are only enabled
 *  when using the extra states. If they are not, just add them to the end
 *  of this file.
 */

/* cache handling functions */
#define ERTS_MSACC_IS_ENABLED() ERTS_UNLIKELY(erts_msacc_enabled)
#define ERTS_MSACC_DECLARE_CACHE()                                      \
    ErtsMsAcc *ERTS_MSACC_UPDATE_CACHE();                                 \
    ERTS_DECLARE_DUMMY(Uint __erts_msacc_state) = ERTS_MSACC_STATE_OTHER;
#define ERTS_MSACC_IS_ENABLED_CACHED() ERTS_UNLIKELY(__erts_msacc_cache != NULL)
#define ERTS_MSACC_UPDATE_CACHE()                                       \
    __erts_msacc_cache = erts_msacc_enabled ? ERTS_MSACC_TSD_GET() : NULL


/* The defines below implicitly declare and load a new cache */
#define ERTS_MSACC_PUSH_STATE()                           \
    ERTS_MSACC_DECLARE_CACHE();                           \
    ERTS_MSACC_PUSH_STATE_CACHED()
#define ERTS_MSACC_SET_STATE(state)                                     \
    ERTS_MSACC_DECLARE_CACHE();                                         \
    ERTS_MSACC_SET_STATE_CACHED(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE(state)                    \
    ERTS_MSACC_PUSH_STATE(); ERTS_MSACC_SET_STATE_CACHED(state)

/* The defines below need an already declared cache to work */
#define ERTS_MSACC_PUSH_STATE_CACHED()                                  \
    __erts_msacc_state = ERTS_MSACC_IS_ENABLED_CACHED() ?               \
        erts_msacc_get_state_um__(__erts_msacc_cache) : ERTS_MSACC_STATE_OTHER
#define ERTS_MSACC_SET_STATE_CACHED(state) \
    if (ERTS_MSACC_IS_ENABLED_CACHED())                         \
        erts_msacc_set_state_um__(__erts_msacc_cache, state, 1)
#define ERTS_MSACC_PUSH_AND_SET_STATE_CACHED(state) \
    ERTS_MSACC_PUSH_STATE_CACHED(); ERTS_MSACC_SET_STATE_CACHED(state)
#define ERTS_MSACC_POP_STATE()                                          \
    if (ERTS_MSACC_IS_ENABLED_CACHED())                                 \
        erts_msacc_set_state_um__(__erts_msacc_cache, __erts_msacc_state, 0)

/* Only use these defines when we know that we have in a managed thread */
#define ERTS_MSACC_PUSH_STATE_M()                         \
    ERTS_MSACC_DECLARE_CACHE();                           \
    ERTS_MSACC_PUSH_STATE_CACHED_M()
#define ERTS_MSACC_PUSH_STATE_CACHED_M()                                \
    __erts_msacc_state = ERTS_MSACC_IS_ENABLED_CACHED() ?    \
        erts_msacc_get_state_m__(__erts_msacc_cache) : ERTS_MSACC_STATE_OTHER
#define ERTS_MSACC_SET_STATE_M(state)                   \
    ERTS_MSACC_DECLARE_CACHE();                         \
    ERTS_MSACC_SET_STATE_CACHED_M(state)
#define ERTS_MSACC_SET_STATE_CACHED_M(state)            \
    if (ERTS_MSACC_IS_ENABLED_CACHED())      \
        erts_msacc_set_state_m__(__erts_msacc_cache, state, 1)
#define ERTS_MSACC_POP_STATE_M()                                  \
    if (ERTS_MSACC_IS_ENABLED_CACHED())                      \
        erts_msacc_set_state_m__(__erts_msacc_cache, __erts_msacc_state, 0)
#define ERTS_MSACC_PUSH_AND_SET_STATE_M(state)                    \
    ERTS_MSACC_PUSH_STATE_M(); ERTS_MSACC_SET_STATE_CACHED_M(state)

ERTS_GLB_INLINE
void erts_msacc_set_state_um__(ErtsMsAcc *msacc,Uint state,int increment);
ERTS_GLB_INLINE
void erts_msacc_set_state_m__(ErtsMsAcc *msacc,Uint state,int increment);

ERTS_GLB_INLINE
Uint erts_msacc_get_state_um__(ErtsMsAcc *msacc);
ERTS_GLB_INLINE
Uint erts_msacc_get_state_m__(ErtsMsAcc *msacc);


#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
Uint erts_msacc_get_state_um__(ErtsMsAcc *msacc) {
    Uint state;
    if (msacc->unmanaged)
        erts_mtx_lock(&msacc->mtx);
    state = msacc->state;
    if (msacc->unmanaged)
        erts_mtx_unlock(&msacc->mtx);
    return state;
}

ERTS_GLB_INLINE
Uint erts_msacc_get_state_m__(ErtsMsAcc *msacc) {
    return msacc->state;
}

ERTS_GLB_INLINE
void erts_msacc_set_state_um__(ErtsMsAcc *msacc, Uint new_state, int increment) {
    if (ERTS_UNLIKELY(msacc->unmanaged)) {
        erts_mtx_lock(&msacc->mtx);
        msacc->state = new_state;
        if (ERTS_LIKELY(!msacc->perf_counter)) {
            erts_mtx_unlock(&msacc->mtx);
            return;
        }
    }

    erts_msacc_set_state_m__(msacc,new_state,increment);

    if (ERTS_UNLIKELY(msacc->unmanaged))
        erts_mtx_unlock(&msacc->mtx);
}

ERTS_GLB_INLINE
void erts_msacc_set_state_m__(ErtsMsAcc *msacc, Uint new_state, int increment) {
    ErtsSysPerfCounter prev_perf_counter;
    Sint64 diff;

    if (new_state == msacc->state)
        return;

    prev_perf_counter = msacc->perf_counter;
    msacc->perf_counter = erts_sys_perf_counter();
    diff = msacc->perf_counter - prev_perf_counter;
    ASSERT(diff >= 0);
    msacc->counters[msacc->state].pc += diff;
#ifdef ERTS_MSACC_STATE_COUNTERS
    msacc->counters[new_state].sc += increment;
#endif
    msacc->state = new_state;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#else

#define ERTS_MSACC_IS_ENABLED() 0
#define erts_msacc_early_init()
#define erts_msacc_init()
#define erts_msacc_init_thread(type, id, liberty)
#define ERTS_MSACC_PUSH_STATE()
#define ERTS_MSACC_PUSH_STATE_CACHED()
#define ERTS_MSACC_POP_STATE()
#define ERTS_MSACC_SET_STATE(state)
#define ERTS_MSACC_SET_STATE_CACHED(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE_CACHED(state)
#define ERTS_MSACC_UPDATE_CACHE()
#define ERTS_MSACC_IS_ENABLED_CACHED()
#define ERTS_MSACC_DECLARE_CACHE()
#define ERTS_MSACC_PUSH_STATE_M()
#define ERTS_MSACC_PUSH_STATE_CACHED_M()
#define ERTS_MSACC_SET_STATE_CACHED_M(state)
#define ERTS_MSACC_POP_STATE_M()
#define ERTS_MSACC_PUSH_AND_SET_STATE_M(state)
#define ERTS_MSACC_SET_BIF_STATE_CACHED_X(Mod,Addr)

#endif /* ERTS_ENABLE_MSACC */

#ifndef ERTS_MSACC_EXTENDED_STATES

#define ERTS_MSACC_PUSH_STATE_X()
#define ERTS_MSACC_POP_STATE_X()
#define ERTS_MSACC_SET_STATE_X(state)
#define ERTS_MSACC_SET_STATE_M_X(state)
#define ERTS_MSACC_SET_STATE_CACHED_X(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE_X(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE_CACHED_X(state)
#define ERTS_MSACC_UPDATE_CACHE_X()
#define ERTS_MSACC_IS_ENABLED_CACHED_X() 0
#define ERTS_MSACC_DECLARE_CACHE_X()
#define ERTS_MSACC_PUSH_STATE_M_X()
#define ERTS_MSACC_PUSH_STATE_CACHED_M_X()
#define ERTS_MSACC_SET_STATE_CACHED_M_X(state)
#define ERTS_MSACC_POP_STATE_M_X()
#define ERTS_MSACC_PUSH_AND_SET_STATE_M_X(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE_CACHED_M_X(state)
#define ERTS_MSACC_SET_BIF_STATE_CACHED_X(Mod,Addr)

#else

void erts_msacc_set_bif_state(ErtsMsAcc *msacc, Eterm mod, void *addr);

#define ERTS_MSACC_PUSH_STATE_X() ERTS_MSACC_PUSH_STATE()
#define ERTS_MSACC_POP_STATE_X() ERTS_MSACC_POP_STATE()
#define ERTS_MSACC_SET_STATE_X(state) ERTS_MSACC_SET_STATE(state)
#define ERTS_MSACC_SET_STATE_M_X(state) ERTS_MSACC_SET_STATE_M(state)
#define ERTS_MSACC_SET_STATE_CACHED_X(state) ERTS_MSACC_SET_STATE_CACHED(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE_X(state) ERTS_MSACC_PUSH_AND_SET_STATE(state)
#define ERTS_MSACC_PUSH_AND_SET_STATE_CACHED_X(state) ERTS_MSACC_PUSH_AND_SET_STATE_CACHED(state)
#define ERTS_MSACC_UPDATE_CACHE_X() ERTS_MSACC_UPDATE_CACHE()
#define ERTS_MSACC_IS_ENABLED_CACHED_X() ERTS_MSACC_IS_ENABLED_CACHED()
#define ERTS_MSACC_DECLARE_CACHE_X() ERTS_MSACC_DECLARE_CACHE()
#define ERTS_MSACC_PUSH_STATE_M_X() ERTS_MSACC_PUSH_STATE_M()
#define ERTS_MSACC_PUSH_STATE_CACHED_M_X() ERTS_MSACC_PUSH_STATE_CACHED_M()
#define ERTS_MSACC_SET_STATE_CACHED_M_X(state) ERTS_MSACC_SET_STATE_CACHED_M(state)
#define ERTS_MSACC_POP_STATE_M_X() ERTS_MSACC_POP_STATE_M()
#define ERTS_MSACC_PUSH_AND_SET_STATE_M_X(state) ERTS_MSACC_PUSH_AND_SET_STATE_M(state)
#define ERTS_MSACC_SET_BIF_STATE_CACHED_X(Mod,Addr)       \
    if (ERTS_MSACC_IS_ENABLED_CACHED_X())               \
        erts_msacc_set_bif_state(__erts_msacc_cache, Mod, Addr)

#endif /* !ERTS_MSACC_EXTENDED_STATES */

#endif /* ERL_MSACC_H__ */
