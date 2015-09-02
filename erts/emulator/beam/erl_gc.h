/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

#ifndef __ERL_GC_H__
#define __ERL_GC_H__

#if defined(ERL_WANT_GC_INTERNALS__) || defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF)

/* GC declarations shared by beam/erl_gc.c and hipe/hipe_gc.c */

#include "erl_map.h"

#if defined(DEBUG) && !ERTS_GLB_INLINE_INCL_FUNC_DEF
#  define HARDDEBUG 1
#endif

#define IS_MOVED_BOXED(x)	(!is_header((x)))
#define IS_MOVED_CONS(x)	(is_non_value((x)))

#define MOVE_CONS(PTR,CAR,HTOP,ORIG)					\
do {									\
    Eterm gval;								\
									\
    HTOP[0] = CAR;		/* copy car */				\
    HTOP[1] = PTR[1];		/* copy cdr */				\
    gval = make_list(HTOP);	/* new location */			\
    *ORIG = gval;		/* redirect original reference */	\
    PTR[0] = THE_NON_VALUE;	/* store forwarding indicator */	\
    PTR[1] = gval;		/* store forwarding address */		\
    HTOP += 2;			/* update tospace htop */		\
} while(0)

#define MOVE_BOXED(PTR,HDR,HTOP,ORIG)                                   \
do {                                                                    \
    Eterm gval;                                                         \
    Sint nelts;                                                         \
                                                                        \
    ASSERT(is_header(HDR));                                             \
    nelts = header_arity(HDR);                                          \
    switch ((HDR) & _HEADER_SUBTAG_MASK) {                              \
    case SUB_BINARY_SUBTAG: nelts++; break;                             \
    case MAP_SUBTAG:                                                    \
        if (is_flatmap_header(HDR)) nelts+=flatmap_get_size(PTR) + 1;   \
        else nelts += hashmap_bitcount(MAP_HEADER_VAL(HDR));            \
    break;                                                              \
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(PTR))->num_free+1; break;   \
    }                                                                   \
    gval    = make_boxed(HTOP);                                         \
    *ORIG   = gval;                                                     \
    *HTOP++ = HDR;                                                      \
    *PTR++  = gval;                                                     \
    while (nelts--) *HTOP++ = *PTR++;                                   \
} while(0)

#define in_area(ptr,start,nbytes) \
 ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

#if defined(DEBUG) || defined(ERTS_OFFHEAP_DEBUG)
int within(Eterm *ptr, Process *p);
#endif

ERTS_GLB_INLINE Eterm follow_moved(Eterm term);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm follow_moved(Eterm term)
{
    Eterm* ptr;
    switch (primary_tag(term)) {
    case TAG_PRIMARY_IMMED1:
	break;
    case TAG_PRIMARY_BOXED:
	ptr = boxed_val(term);
	if (IS_MOVED_BOXED(*ptr)) term = *ptr;
	break;
    case TAG_PRIMARY_LIST:
	ptr = list_val(term);
	if (IS_MOVED_CONS(ptr[0])) term = ptr[1];
	break;
    default:
	ASSERT(!"strange tag in follow_moved");
    }
    return term;
}
#endif

#endif /* ERL_GC_C__ || HIPE_GC_C__ */

/*
 * Global exported
 */

extern Uint erts_test_long_gc_sleep;

typedef struct {
  Uint64 reclaimed;
  Uint64 garbage_cols;
} ErtsGCInfo;

void erts_gc_info(ErtsGCInfo *gcip);
void erts_init_gc(void);
int erts_garbage_collect(struct process*, int, Eterm*, int);
void erts_garbage_collect_hibernate(struct process* p);
Eterm erts_gc_after_bif_call(struct process* p, Eterm result, Eterm* regs, Uint arity);
void erts_garbage_collect_literals(struct process* p, Eterm* literals,
				   Uint lit_size,
				   struct erl_off_heap_header* oh);
Uint erts_next_heap_size(Uint, Uint);
Eterm erts_heap_sizes(struct process* p);

void erts_offset_off_heap(struct erl_off_heap*, Sint, Eterm*, Eterm*);
void erts_offset_heap_ptr(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_offset_heap(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_free_heap_frags(struct process* p);

#endif /* __ERL_GC_H__ */
