/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2000-2025. All Rights Reserved.
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

#ifndef __ERLFUNTABLE_H__
#define __ERLFUNTABLE_H__

#include "erl_threads.h"

/*
 * Fun entry.
 */
typedef struct erl_fun_entry {
    /* We start with an `ErtsDispatchable`, similar to export entries, so that
     * we can mostly use the same code for both. This greatly reduces the
     * complexity of instructions like `call_fun` and `is_function2`. */
    ErtsDispatchable dispatch;

    ErtsCodePtr pend_purge_address; /* Address during a pending purge */

    /* These fields identify the function and must not be altered after fun
     * creation. */
    Eterm module;                   /* Tagged atom for module. */
    byte uniq[16];                  /* MD5 for module. */
    int arity;                      /* The arity of the fun. */
    int index;                      /* New style index. */

    /* These have been prepared for removal in OTP 29. */
    int old_uniq;                   /* Unique number (old_style) */
    int old_index;                  /* Old style index */
} ErlFunEntry;

/* This structure represents a 'fun' (lambda), whether local or external. It is
 * stored on process heaps, and has variable size depending on the size of the
 * environment. */

typedef struct erl_fun_thing {
    /* The header contains FUN_SUBTAG, arity, and the size of the environment,
     * the latter being zero for external funs and non-zero for local ones. */
    Eterm thing_word;

    union {
        /* Both `ErlFunEntry` and `Export` begin with an `ErtsDispatchable`, so
         * code that doesn't really care which (e.g. calls) can use this
         * pointer to improve performance. */
        const ErtsDispatchable *disp;

        /* Pointer to function entry, valid iff this is local fun. */
        const ErlFunEntry *fun;

        /* Pointer to export entry, valid iff this is an external fun. */
        const Export *exp;
    } entry;

    /* Environment (free variables), may be compound terms. */
    Eterm env[];
} ErlFunThing;

#define is_external_fun(FunThing)                                             \
    (!!((FunThing)->thing_word >> FUN_HEADER_KIND_OFFS))
#define is_local_fun(FunThing)                                                \
    (!is_external_fun(FunThing))

#define fun_arity(FunThing)                                                   \
    (((FunThing)->thing_word >> FUN_HEADER_ARITY_OFFS) & 0xFF)
#define fun_num_free(FunThing)                                                \
    (((FunThing)->thing_word >> FUN_HEADER_ENV_SIZE_OFFS) & 0xFF)

/* ERL_FUN_SIZE does _not_ include space for the environment which is a
 * C99-style flexible array */
#define ERL_FUN_SIZE ((sizeof(ErlFunThing)/sizeof(Eterm)))

void erts_init_fun_table(void);
void erts_fun_info(fmtfn_t, void *);
int erts_fun_table_sz(void);
int erts_fun_entries_sz(void);

/* Finds or inserts a fun entry that matches the given signature. */
ErlFunEntry *erts_fun_entry_put(Eterm mod, int old_uniq, int old_index,
                                const byte* uniq, int index, int arity);
const ErlFunEntry *erts_fun_entry_get_or_make_stub(Eterm mod,
                                                   int old_uniq,
                                                   int old_index,
                                                   const byte* uniq,
                                                   int index,
                                                   int arity);

const ErtsCodeMFA *erts_get_fun_mfa(const ErlFunEntry *fe, ErtsCodeIndex ix);

void erts_set_fun_code(ErlFunEntry *fe, ErtsCodeIndex ix, ErtsCodePtr address);

ERTS_GLB_INLINE
ErtsCodePtr erts_get_fun_code(ErlFunEntry *fe, ErtsCodeIndex ix);

int erts_is_fun_loaded(const ErlFunEntry* fe, ErtsCodeIndex ix);

struct erl_module_instance;
void erts_fun_purge_prepare(struct erl_module_instance* modi);
void erts_fun_purge_abort_prepare(ErlFunEntry **funs, Uint no);
void erts_fun_purge_abort_finalize(ErlFunEntry **funs, Uint no);
void erts_fun_purge_complete(ErlFunEntry **funs, Uint no);
void erts_dump_fun_entries(fmtfn_t, void *);

void erts_fun_start_staging(void);
void erts_fun_end_staging(int commit);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
ErtsCodePtr erts_get_fun_code(ErlFunEntry *fe, ErtsCodeIndex ix) {
    return fe->dispatch.addresses[ix];
}

#endif

#endif
