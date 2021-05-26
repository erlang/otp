/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2017. All Rights Reserved.
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
    HashBucket bucket;

    ErtsCodePtr address;            /* Pointer to code for actual function */

    /* These fields identify the function and must not be altered after fun
     * creation. */
    Eterm module;                   /* Tagged atom for module. */
    Uint arity;                     /* The arity of the fun. */
    int index;                      /* New style index. */
    byte uniq[16];                  /* MD5 for module. */
    int old_uniq;                   /* Unique number (old_style) */
    int old_index;                  /* Old style index */

    erts_refc_t refc;               /* Reference count: One for code + one for
                                     * each fun object in each process. */
    ErtsCodePtr pend_purge_address; /* Address during a pending purge */
} ErlFunEntry;

/*
 * This structure represents a 'fun' (lambda). It is stored on
 * process heaps. It has variable size depending on the size
 * of the environment.
 */

typedef struct erl_fun_thing {
    Eterm thing_word;		/* Subtag FUN_SUBTAG. */
    ErlFunEntry* fe;		/* Pointer to fun entry. */
    struct erl_off_heap_header* next;
    Uint arity;			/* The arity of the fun. */
    Uint num_free;		/* Number of free variables (in env). */
  /* -- The following may be compound Erlang terms ---------------------- */
    Eterm creator;		/* Pid of creator process (contains node). */
    Eterm env[1];		/* Environment (free variables). */
} ErlFunThing;

/* ERL_FUN_SIZE does _not_ include space for the environment */
#define ERL_FUN_SIZE ((sizeof(ErlFunThing)/sizeof(Eterm))-1)

void erts_init_fun_table(void);
void erts_fun_info(fmtfn_t, void *);
int erts_fun_table_sz(void);

/* Finds or inserts a fun entry that matches the given signature. */
ErlFunEntry* erts_put_fun_entry2(Eterm mod, int old_uniq, int old_index,
                                 const byte* uniq, int index, int arity);

const ErtsCodeMFA *erts_get_fun_mfa(const ErlFunEntry *fe);

int erts_is_fun_loaded(const ErlFunEntry* fe);

void erts_erase_fun_entry(ErlFunEntry* fe);
void erts_cleanup_funs(ErlFunThing* funp);

struct erl_module_instance;
void erts_fun_purge_prepare(struct erl_module_instance* modi);
void erts_fun_purge_abort_prepare(ErlFunEntry **funs, Uint no);
void erts_fun_purge_abort_finalize(ErlFunEntry **funs, Uint no);
void erts_fun_purge_complete(ErlFunEntry **funs, Uint no);
void erts_dump_fun_entries(fmtfn_t, void *);

#endif
