/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2018. All Rights Reserved.
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

#define ERTS_POTENTIALLY_LONG_GC_HSIZE (128*1024) /* Words */

#include "erl_map.h"
#include "erl_fun.h"
#include "erl_bits.h"

#define IS_MOVED_BOXED(x)	(!is_header((x)))
#define IS_MOVED_CONS(x)	(is_non_value((x)))
Eterm* erts_sub_binary_to_heap_binary(Eterm *ptr, Eterm **hpp, Eterm *orig);

ERTS_GLB_INLINE void move_cons(Eterm *ERTS_RESTRICT ptr, Eterm car, Eterm **hpp,
                               Eterm *orig);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE void move_cons(Eterm *ERTS_RESTRICT ptr, Eterm car, Eterm **hpp,
                               Eterm *orig)
{
    Eterm *ERTS_RESTRICT htop = *hpp;
    Eterm gval;

    htop[0] = car;               /* copy car */
    htop[1] = ptr[1];            /* copy cdr */
    gval    = make_list(htop);   /* new location */
    *orig   = gval;              /* redirect original reference */
    ptr[0]  = THE_NON_VALUE;     /* store forwarding indicator */
    ptr[1]  = gval;              /* store forwarding address */
    *hpp   += 2;                 /* update tospace htop */
}
#endif

ERTS_GLB_INLINE Eterm* move_boxed(Eterm *ERTS_RESTRICT ptr, Eterm hdr, Eterm **hpp,
                                  Eterm *orig);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm* move_boxed(Eterm *ERTS_RESTRICT ptr, Eterm hdr, Eterm **hpp,
                                  Eterm *orig)
{
    Eterm gval;
    Sint nelts;
    Eterm *ERTS_RESTRICT htop = *hpp;

    ASSERT(is_header(hdr));
    nelts = header_arity(hdr);
    switch ((hdr) & _HEADER_SUBTAG_MASK) {
    case SUB_BINARY_SUBTAG:
        {
            ErlSubBin *sb = (ErlSubBin *)ptr;
            /* convert sub-binary to heap-binary if applicable */
            if (sb->bitsize == 0 && sb->bitoffs == 0 &&
                sb->is_writable == 0 && sb->size <= sizeof(Eterm) * 3) {
                return erts_sub_binary_to_heap_binary(ptr, hpp, orig);
            }
        }
        nelts++;
        break;
    case MAP_SUBTAG:
        if (is_flatmap_header(hdr)) nelts+=flatmap_get_size(ptr) + 1;
        else nelts += hashmap_bitcount(MAP_HEADER_VAL(hdr));
    break;
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(ptr))->num_free+1; break;
    }
    gval    = make_boxed(htop);
    *orig   = gval;
    *htop++ = hdr;
    *ptr++  = gval;
    while (nelts--) *htop++ = *ptr++;

    *hpp = htop;
    return ptr;
}
#endif

#define ErtsInYoungGen(TPtr, Ptr, OldHeap, OldHeapSz)			\
    (!erts_is_literal((TPtr), (Ptr))					\
     & !ErtsInArea((Ptr), (OldHeap), (OldHeapSz)))

ERTS_GLB_INLINE Eterm follow_moved(Eterm term, Eterm xptr_tag);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm follow_moved(Eterm term, Eterm xptr_tag)
{
    Eterm* ptr;
    switch (primary_tag(term)) {
    case TAG_PRIMARY_IMMED1:
	break;
    case TAG_PRIMARY_BOXED:
	ptr = boxed_val(term);
	if (IS_MOVED_BOXED(*ptr)) term = (*ptr) | xptr_tag;
	break;
    case TAG_PRIMARY_LIST:
	ptr = list_val(term);
	if (IS_MOVED_CONS(ptr[0])) term = (ptr[1]) | xptr_tag;
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

#define ERTS_IS_GC_DESIRED_INTERNAL(Proc, HTop, STop)			\
    ((((STop) - (HTop) < (Proc)->mbuf_sz))				\
     | ((Proc)->off_heap.overhead > (Proc)->bin_vheap_sz)		\
     | !!((Proc)->flags & F_FORCE_GC))

#define ERTS_IS_GC_DESIRED(Proc)					\
    ERTS_IS_GC_DESIRED_INTERNAL((Proc), (Proc)->htop, (Proc)->stop)

#define ERTS_FORCE_GC_INTERNAL(Proc, FCalls)				\
    do {								\
	(Proc)->flags |= F_FORCE_GC;					\
	ERTS_VBUMP_ALL_REDS_INTERNAL((Proc), (FCalls));			\
    } while (0)

#define ERTS_FORCE_GC(Proc)						\
    ERTS_FORCE_GC_INTERNAL((Proc), (Proc)->fcalls)

extern Uint erts_test_long_gc_sleep;

typedef struct {
  Uint64 reclaimed;
  Uint64 garbage_cols;
} ErtsGCInfo;

#define ERTS_MAX_HEAP_SIZE_MAP_SZ (2*3 + 1 + MAP_HEADER_FLATMAP_SZ)

#define ERTS_PROCESS_GC_INFO_MAX_TERMS (11)  /* number of elements in process_gc_info*/
#define ERTS_PROCESS_GC_INFO_MAX_SIZE                                   \
    (ERTS_PROCESS_GC_INFO_MAX_TERMS * (2/*cons*/ + 3/*2-tuple*/ + BIG_UINT_HEAP_SIZE))
Eterm erts_process_gc_info(struct process*, Uint *, Eterm **, Uint, Uint);

void erts_gc_info(ErtsGCInfo *gcip);
void erts_init_gc(void);
int erts_garbage_collect_nobump(struct process*, int, Eterm*, int, int);
void erts_garbage_collect(struct process*, int, Eterm*, int);
void erts_garbage_collect_hibernate(struct process* p);
Eterm erts_gc_after_bif_call_lhf(struct process* p, ErlHeapFragment *live_hf_end,
				 Eterm result, Eterm* regs, Uint arity);
Eterm erts_gc_after_bif_call(struct process* p, Eterm result, Eterm* regs, Uint arity);
int erts_garbage_collect_literals(struct process* p, Eterm* literals,
				  Uint lit_size,
				  struct erl_off_heap_header* oh,
				  int fcalls);
Uint erts_next_heap_size(Uint, Uint);
Eterm erts_heap_sizes(struct process* p);

void erts_offset_off_heap(struct erl_off_heap*, Sint, Eterm*, Eterm*);
void erts_offset_heap_ptr(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_offset_heap(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_free_heap_frags(struct process* p);
Eterm erts_max_heap_size_map(Sint, Uint, Eterm **, Uint *);
int erts_max_heap_size(Eterm, Uint *, Uint *);
void erts_deallocate_young_generation(Process *c_p);
void erts_copy_one_frag(Eterm** hpp, ErlOffHeap* off_heap,
                        ErlHeapFragment *bp, Eterm *refs, int nrefs);
#if defined(DEBUG) || defined(ERTS_OFFHEAP_DEBUG)
int erts_dbg_within_proc(Eterm *ptr, Process *p, Eterm* real_htop);
#endif

#endif /* __ERL_GC_H__ */
