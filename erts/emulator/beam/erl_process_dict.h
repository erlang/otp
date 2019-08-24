/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2018. All Rights Reserved.
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

#ifndef _ERL_PROCESS_DICT_H
#define _ERL_PROCESS_DICT_H
#include "sys.h"

typedef struct proc_dict {
    unsigned int sizeMask;
    unsigned int usedSlots;
    unsigned int arraySize;
    unsigned int splitPosition;
    Uint numElements;
    Eterm data[1]; /* The beginning of an array of erlang terms */
} ProcDict;

#define ERTS_PD_START(PD) ((PD)->data)
#define ERTS_PD_SIZE(PD)  ((PD)->usedSlots)

int erts_pd_set_initial_size(int size);
Uint erts_dicts_mem_size(struct process *p);
void erts_erase_dicts(struct process *p);
void erts_dictionary_dump(fmtfn_t to, void *to_arg, ProcDict *pd);
void erts_deep_dictionary_dump(fmtfn_t to, void *to_arg,
			       ProcDict* pd, void (*cb)(fmtfn_t, void *, Eterm obj));
Eterm erts_dictionary_copy(ErtsHeapFactory *hfact, ProcDict *pd, Uint reserve_size);

Eterm erts_pd_hash_get(struct process *p, Eterm id);
Uint32 erts_pd_make_hx(Eterm key);
Eterm erts_pd_hash_get_with_hx(Process *p, Uint32 hx, Eterm id);

#endif
