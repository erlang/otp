/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2023. All Rights Reserved.
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

    /* These fields identify the function and must not be altered after fun
     * creation. */
    Eterm module;                   /* Tagged atom for module. */
    Uint arity;                     /* The arity of the fun. */
    int index;                      /* New style index. */
    byte uniq[16];                  /* MD5 for module. */
    int old_uniq;                   /* Unique number (old_style) */
    int old_index;                  /* Old style index */

    erts_refc_t refc;               /* Reference count: One for code + one for
                                     * each FunRef. */
    ErtsCodePtr pend_purge_address; /* Address during a pending purge */
} ErlFunEntry;

/* Reference-holding structure for funs. As these normally live in the literal
 * area of their module instance and are shared with all lambdas pointing to
 * the same function, having it separated like this saves us from having to
 * reference-count every single lambda.
 *
 * These references copied onto the heap under some circumstances, for example
 * trips through ETS or the external term format, but generally they'll live
 * off-heap. */
typedef struct erl_fun_ref {
    Eterm thing_word;
    ErlFunEntry *entry;
    struct erl_off_heap_header *next;
} FunRef;

#define ERL_FUN_REF_SIZE ((sizeof(FunRef)/sizeof(Eterm)))
#define HEADER_FUN_REF _make_header(ERL_FUN_REF_SIZE-1,_TAG_HEADER_FUN_REF)

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
        ErtsDispatchable *disp;

        /* Pointer to function entry, valid iff this is local fun. */
        ErlFunEntry *fun;

        /* Pointer to export entry, valid iff this is an external fun. */
        Export *exp;
    } entry;

    /* Environment (free variables), may be compound terms.
     *
     * External funs lack this altogether, and local funs _always_ reference a
     * `FunRef` just past the last free variable. This ensures that the
     * `ErlFunEntry` above will always be valid. */
    Eterm env[];
} ErlFunThing;

#define is_external_fun(FunThing) (fun_env_size(FunThing) == 0)
#define is_local_fun(FunThing) (!is_external_fun(FunThing))

#define fun_arity(FunThing)                                                   \
    (((FunThing)->thing_word >> FUN_HEADER_ARITY_OFFS) & 0xFF)
#define fun_env_size(FunThing)                                                \
    ((FunThing)->thing_word >> FUN_HEADER_ENV_SIZE_OFFS)
#define fun_num_free(FunThing)                                                \
    (ASSERT(is_local_fun(FunThing)), fun_env_size(FunThing) - 1)

/* ERL_FUN_SIZE does _not_ include space for the environment which is a
 * C99-style flexible array */
#define ERL_FUN_SIZE ((sizeof(ErlFunThing)/sizeof(Eterm)))

void erts_init_fun_table(void);
void erts_fun_info(fmtfn_t, void *);
int erts_fun_table_sz(void);

/* Finds or inserts a fun entry that matches the given signature. */
ErlFunEntry* erts_put_fun_entry2(Eterm mod, int old_uniq, int old_index,
                                 const byte* uniq, int index, int arity);

const ErtsCodeMFA *erts_get_fun_mfa(const ErlFunEntry *fe, ErtsCodeIndex ix);

void erts_set_fun_code(ErlFunEntry *fe, ErtsCodeIndex ix, ErtsCodePtr address);

ERTS_GLB_INLINE
ErtsCodePtr erts_get_fun_code(ErlFunEntry *fe, ErtsCodeIndex ix);

int erts_is_fun_loaded(const ErlFunEntry* fe, ErtsCodeIndex ix);

void erts_erase_fun_entry(ErlFunEntry* fe);
void erts_cleanup_funs(ErlFunThing* funp);

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
