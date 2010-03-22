/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2010. All Rights Reserved.
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

#ifndef __ERLFUNTABLE_H__
#define __ERLFUNTABLE_H__

#include "erl_smp.h"

/*
 * Fun entry.
 */

typedef struct erl_fun_entry {
    HashBucket bucket;		/* MUST BE LOCATED AT TOP OF STRUCT!!! */

    byte uniq[16];		/* MD5 for module. */
    int index;			/* New style index. */
    int old_uniq;		/* Unique number (old_style) */
    int old_index;		/* Old style index */
    BeamInstr* address;		/* Pointer to code for fun */

#ifdef HIPE
    UWord* native_address;	/* Native entry code for fun. */
#endif

    Uint arity;			/* The arity of the fun. */
    Eterm module;		/* Tagged atom for module. */
    erts_refc_t refc;		/* Reference count: One for code + one for each
				   fun object in each process. */
} ErlFunEntry;

/*
 * This structure represents a 'fun' (lambda). It is stored on
 * process heaps. It has variable size depending on the size
 * of the environment.
 */

typedef struct erl_fun_thing {
    Eterm thing_word;		/* Subtag FUN_SUBTAG. */
#ifndef HYBRID /* FIND ME! */
    struct erl_fun_thing* next;	/* Next fun in mso list. */
#endif
    ErlFunEntry* fe;		/* Pointer to fun entry. */
#ifdef HIPE
    UWord* native_address;	/* Native code for the fun. */
#endif
    Uint arity;			/* The arity of the fun. */
    Uint num_free;		/* Number of free variables (in env). */
  /* -- The following may be compound Erlang terms ---------------------- */
    Eterm creator;		/* Pid of creator process (contains node). */
    Eterm env[1];		/* Environment (free variables). */
} ErlFunThing;

/* ERL_FUN_SIZE does _not_ include space for the environment */
#define ERL_FUN_SIZE ((sizeof(ErlFunThing)/sizeof(Eterm))-1)

void erts_init_fun_table(void);
void erts_fun_info(int, void *);
int erts_fun_table_sz(void);

ErlFunEntry* erts_put_fun_entry(Eterm mod, int uniq, int index);
ErlFunEntry* erts_get_fun_entry(Eterm mod, int uniq, int index);

ErlFunEntry* erts_put_fun_entry2(Eterm mod, int old_uniq, int old_index,
				byte* uniq, int index, int arity);
ErlFunEntry* erts_get_fun_entry2(Eterm mod, int old_uniq, int old_index,
				byte* uniq, int index, int arity);

void erts_erase_fun_entry(ErlFunEntry* fe);
#ifndef HYBRID /* FIND ME! */
void erts_cleanup_funs(ErlFunThing* funp);
#endif
void erts_cleanup_funs_on_purge(BeamInstr* start, BeamInstr* end);
void erts_dump_fun_entries(int, void *);

#endif
