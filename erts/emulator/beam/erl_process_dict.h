/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2009. All Rights Reserved.
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

#ifndef _ERL_PROCESS_DICT_H
#define _ERL_PROCESS_DICT_H
#include "sys.h"

typedef struct proc_dict {
    unsigned int size;
    unsigned int used;
    unsigned int homeSize;
    unsigned int splitPosition;
    Uint numElements;
    Eterm data[1]; /* The beginning of an array of erlang terms */
} ProcDict;

Uint erts_dicts_mem_size(struct process *p);
void erts_erase_dicts(struct process *p);
void erts_dictionary_dump(int to, void *to_arg, ProcDict *pd);
void erts_deep_dictionary_dump(int to, void *to_arg,
			       ProcDict* pd, void (*cb)(int, void *, Eterm obj));
Eterm erts_dictionary_copy(struct process *p, ProcDict *pd);

Eterm erts_pd_hash_get(struct process *p, Eterm id);

#endif
