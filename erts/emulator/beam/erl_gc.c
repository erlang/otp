/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERL_WANT_GC_INTERNALS__

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_db.h"
#include "beam_catches.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_map.h"
#include "error.h"
#include "big.h"
#include "erl_gc.h"
#include "dtrace-wrapper.h"
#include "erl_bif_unique.h"
#include "dist.h"
#include "erl_nfunc_sched.h"
#include "erl_proc_sig_queue.h"
#include "beam_common.h"
#include "beam_bp.h"

#define ERTS_INACT_WR_PB_LEAVE_MUCH_LIMIT 1
#define ERTS_INACT_WR_PB_LEAVE_MUCH_PERCENTAGE 20
#define ERTS_INACT_WR_PB_LEAVE_LIMIT 10
#define ERTS_INACT_WR_PB_LEAVE_PERCENTAGE 10

#define ERTS_LONG_GC_MAX_NMSIGS 1000

#if defined(DEBUG) || 0
#define ERTS_GC_DEBUG
#else
#undef ERTS_GC_DEBUG
#endif
#ifdef ERTS_GC_DEBUG
#  define ERTS_GC_ASSERT ASSERT
#else
#  define ERTS_GC_ASSERT(B) ((void) 1)
#endif

#if defined(DEBUG) && 0
#  define HARDDEBUG 1
#endif

/*
 * Returns number of elements in an array.
 */
#define ALENGTH(a) (sizeof(a)/sizeof(a[0]))

/* Actual stack usage, note that this may include words in the redzone. */
# define STACK_SZ_ON_HEAP(p) (STACK_START(p) - STACK_TOP(p))

# define OverRunCheck(P) \
    if ((P)->stop < (P)->htop) { \
        erts_fprintf(stderr, "hend=%p\n", (P)->hend); \
        erts_fprintf(stderr, "stop=%p\n", (P)->stop); \
        erts_fprintf(stderr, "htop=%p\n", (P)->htop); \
        erts_fprintf(stderr, "heap=%p\n", (P)->heap); \
        erts_exit(ERTS_ABORT_EXIT, "%s, line %d: %T: Overrun stack and heap\n", \
		 __FILE__,__LINE__,(P)->common.id); \
    }

#ifdef DEBUG
#define ErtsGcQuickSanityCheck(P)					\
do {									\
    ASSERT((P)->heap < (P)->hend);					\
    ASSERT((p)->abandoned_heap || (P)->heap_sz == (P)->hend - (P)->heap); \
    ASSERT((P)->heap <= (P)->htop && (P)->htop <= (P)->hend);		\
    ASSERT((P)->heap <= (P)->stop && (P)->stop <= (P)->hend);		\
    ASSERT(((P)->heap <= (P)->high_water && (P)->high_water <= (P)->hend)); \
    OverRunCheck((P));							\
} while (0)
#else
#define ErtsGcQuickSanityCheck(P)					\
do {									\
    OverRunCheck((P));							\
} while (0)
#endif
/*
 * This structure describes the rootset for the GC.
 */
typedef struct roots {
    Eterm* v;		/* Pointers to vectors with terms to GC
			 * (e.g. the stack).
			 */
    Uint sz;		/* Size of each vector. */
} Roots;

typedef struct {
    Roots def[32];		/* Default storage. */
    Roots* roots;		/* Pointer to root set array. */
    Uint size;			/* Storage size. */
    int num_roots;		/* Number of root arrays. */
} Rootset;

static void copy_erlang_stack(Process *p, Eterm *new_heap, SWord new_sz);
static Uint setup_rootset(Process*, Eterm*, int, Rootset*);
static void cleanup_rootset(Rootset *rootset);
static Eterm *full_sweep_heaps(Process *p,
                               ErlHeapFragment *live_hf_end,
			       int hibernate,
			       Eterm *n_heap, Eterm* n_htop,
			       char *oh, Uint oh_size,
			       Eterm *objv, int nobj);
static int garbage_collect(Process* p, ErlHeapFragment *live_hf_end,
			   Uint need, Eterm* objv, int nobj, int fcalls,
			   Uint max_young_gen_usage);
static int major_collection(Process* p, ErlHeapFragment *live_hf_end,
			    Uint need, Eterm* objv, int nobj,
			    Uint ygen_usage, Uint *recl);
static int minor_collection(Process* p, ErlHeapFragment *live_hf_end,
			    Uint need, Eterm* objv, int nobj,
			    Uint ygen_usage, Uint *recl);
static void do_minor(Process *p, ErlHeapFragment *live_hf_end,
		     char *mature, Uint mature_size,
		     Uint new_sz, Eterm* objv, int nobj);
static Eterm *sweep_new_heap(Eterm *n_hp, Eterm *n_htop,
			     char* old_heap, Uint old_heap_size);
static Eterm *sweep_heaps(Eterm *n_hp, Eterm *n_htop,
			  char* old_heap, Uint old_heap_size);
static Eterm* sweep_literal_area(Eterm* n_hp, Eterm* n_htop,
				 char* old_heap, Uint old_heap_size,
				 char* src, Uint src_size);
static Eterm* sweep_literals_to_old_heap(Eterm* heap_ptr, Eterm* heap_end, Eterm* htop,
					 char* src, Uint src_size);
static Eterm* collect_live_heap_frags(Process* p, ErlHeapFragment *live_hf_end,
				      Eterm* htop);
static int adjust_after_fullsweep(Process *p, int need, Eterm *objv, int nobj);
static void shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj);
static void grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj);
static void sweep_off_heap(Process *p, int fullsweep);
static void offset_heap(Eterm *hp, Uint sz, Sint offs, const char* area, Uint area_sz);
static void offset_stack(Eterm *stack, Uint sz,
                         Sint heap_offset, const char *heap_area, Uint heap_area_sz,
                         Sint stack_offset, const char *stack_area, Uint stack_area_sz);
static void offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, const char *area, Uint area_sz);
static void offset_rootset(Process *p,
                           Sint heap_offs, const char *heap_area, Uint heap_area_sz,
                           Sint stack_offs, const char *stack_area, Uint stack_area_sz,
                           Eterm* objv, int nobj);
static void offset_off_heap(Process *p, Sint offs, const char *area, Uint area_sz);
static void offset_mqueue(Process *p, Sint offs, const char *area, Uint area_sz);
static int has_reached_max_heap_size(Process *p, Uint total_heap_size);
static int reached_max_heap_size(Process *p, Uint total_heap_size,
                                 Uint extra_heap_size, Uint extra_old_heap_size);
static void init_gc_info(ErtsGCInfo *gcip);
static Uint64 next_vheap_size(Process* p, Uint64 vheap, Uint64 vheap_sz);

#ifdef HARDDEBUG
static void disallow_heap_frag_ref_in_heap(Process *p, Eterm *heap, Eterm *htop);
static void disallow_heap_frag_ref_in_old_heap(Process* p);
#endif

#if defined(ARCH_64)
# define MAX_HEAP_SIZES 154
#else
# define MAX_HEAP_SIZES 59
#endif

static Sint heap_sizes[MAX_HEAP_SIZES];	/* Suitable heap sizes. */
static int num_heap_sizes;	/* Number of heap sizes. */

Uint erts_test_long_gc_sleep; /* Only used for testing... */

typedef struct {
    Process *proc;
    Eterm ref;
    Eterm ref_heap[ERTS_REF_THING_SIZE];
    Uint req_sched;
    erts_atomic32_t refc;
} ErtsGCInfoReq;

static struct {
    erts_mtx_t mtx;
    ErtsGCInfo info;
} dirty_gc;

static void move_msgs_to_heap(Process *c_p)
{
    Uint64 pre_oh, post_oh;

    pre_oh = c_p->off_heap.overhead;
    erts_proc_sig_move_msgs_to_heap(c_p);
    post_oh = c_p->off_heap.overhead;

    if (pre_oh != post_oh) {
	/* Got new binaries; update bin vheap size... */
        c_p->bin_vheap_sz = next_vheap_size(c_p, post_oh,
                                            c_p->bin_vheap_sz);
    }
}

static ERTS_INLINE int
gc_cost(Uint gc_moved_live_words, Uint resize_moved_words)
{
    Sint reds;

    reds = gc_moved_live_words/10;
    reds += resize_moved_words/100;
    if (reds < 1)
	return 1;
    if (reds > INT_MAX)
	return INT_MAX;
    return (int) reds;
}

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(gcireq,
                                 ErtsGCInfoReq,
                                 5,
                                 ERTS_ALC_T_GC_INFO_REQ)

/*
 * Initialize GC global data.
 */
void
erts_init_gc(void)
{
    int i = 0, ix;
    Sint max_heap_size = 0;

    ERTS_CT_ASSERT(offsetof(BinRef,thing_word) == offsetof(struct erl_off_heap_header,thing_word));
    ERTS_CT_ASSERT(offsetof(BinRef,thing_word) == offsetof(ErlFunThing,thing_word));
    ERTS_CT_ASSERT(offsetof(BinRef,thing_word) == offsetof(ExternalThing,header));
    ERTS_CT_ASSERT(offsetof(BinRef,next) == offsetof(struct erl_off_heap_header,next));
    ERTS_CT_ASSERT(offsetof(BinRef,next) == offsetof(ExternalThing,next));

    erts_test_long_gc_sleep = 0;

    /*
     * Heap sizes start growing in a Fibonacci sequence.
     *
     * Fib growth is not really ok for really large heaps, for
     * example is fib(35) == 14meg, whereas fib(36) == 24meg;
     * we really don't want that growth when the heaps are that big.
     */
	    
    /* Growth stage 1 - Fibonacci + 1*/
    /* 12,38 will hit size 233, the old default */

    heap_sizes[0] = 12;
    heap_sizes[1] = 38;

    for(i = 2; i < 23; i++) {
        /* one extra word for block header */
        heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-2] + 1;
    }


    /* for 32 bit we want max_heap_size to be MAX(32bit) / 4 [words]
     * for 64 bit we want max_heap_size to be MAX(52bit) / 8 [words]
     */

    max_heap_size = sizeof(Eterm) < 8 ? (Sint)((~(Uint)0)/(sizeof(Eterm))) : 
					(Sint)(((Uint64)1 << 53)/sizeof(Eterm));

    /* Growth stage 2 - 20% growth */
    /* At 1.3 mega words heap, we start to slow down. */
    for (i = 23; i < ALENGTH(heap_sizes); i++) {
	heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-1]/5;
	if ((heap_sizes[i] < 0) || heap_sizes[i] > max_heap_size) {
	    /* Size turned negative. Discard this last size. */
	    i--;
	    break;
	}
    }
    num_heap_sizes = i;
    
    for (ix = 0; ix < erts_no_schedulers; ix++) {
      ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(ix);
      init_gc_info(&esdp->gc_info);
    }

    erts_mtx_init(&dirty_gc.mtx, "dirty_gc_info", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    init_gc_info(&dirty_gc.info);

    init_gcireq_alloc();
}

/*
 * Find the next heap size equal to or greater than the given size (if offset == 0).
 *
 * If offset is 1, the next higher heap size is returned (always greater than size).
 */
Uint
erts_next_heap_size(Uint size, Uint offset)
{
    if (size < heap_sizes[0]) {
	return heap_sizes[0];
    } else {
	Sint* low = heap_sizes;
	Sint* high = heap_sizes + num_heap_sizes;
	Sint* mid;

	while (low < high) {
	    mid = low + (high-low) / 2;
	    if (size < mid[0]) {
		high = mid;
	    } else if (size == mid[0]) {
		ASSERT(mid+offset-heap_sizes < num_heap_sizes);
		return mid[offset];
	    } else if (size < mid[1]) {
		ASSERT(mid[0] < size && size <= mid[1]);
		ASSERT(mid+offset-heap_sizes < num_heap_sizes);
		return mid[offset+1];
	    } else {
		low = mid + 1;
	    }
	}
	erts_exit(ERTS_ERROR_EXIT, "no next heap size found: %beu, offset %beu\n", size, offset);
    }
    return 0;
}
/*
 * Return the next heap size to use. Make sure we never return
 * a smaller heap size than the minimum heap size for the process.
 * (Use of the erlang:hibernate/3 BIF could have shrunk the
 * heap below the minimum heap size.)
 */
static Uint
next_heap_size(Process* p, Uint size, Uint offset)
{
    size = erts_next_heap_size(size, offset);
    return size < p->min_heap_size ? p->min_heap_size : size;
}

Eterm
erts_heap_sizes(Process* p)
{
    int i;
    int n = 0;
    int big = 0;
    Eterm res = NIL;
    Eterm* hp;
    Eterm* bigp;

    for (i = num_heap_sizes-1; i >= 0; i--) {
	n += 2;
	if (!IS_SSMALL(heap_sizes[i])) {
	    big += BIG_UINT_HEAP_SIZE;
	}
    }

    /*
     * We store all big numbers first on the heap, followed
     * by all the cons cells.
     */
    bigp = HAlloc(p, n+big);
    hp = bigp+big;
    for (i = num_heap_sizes-1; i >= 0; i--) {
	Eterm num;
	Sint sz = heap_sizes[i];

	if (IS_SSMALL(sz)) {
	    num = make_small(sz);
	} else {
	    num = uint_to_big(sz, bigp);
	    bigp += BIG_UINT_HEAP_SIZE;
	}
        res = CONS(hp, num, res);
        hp += 2;
    }
    return res;
}

void 
erts_offset_heap(Eterm* hp, Uint sz, Sint offs, Eterm* low, Eterm* high)
{
    offset_heap(hp, sz, offs, (char*) low, ((char *)high)-((char *)low));
}

void 
erts_offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, 
		     Eterm* low, Eterm* high)
{
    offset_heap_ptr(hp, sz, offs, (char *) low, ((char *)high)-((char *)low));
}


#define ptr_within(ptr, low, high) ((ptr) < (high) && (ptr) >= (low))

void
erts_offset_off_heap(ErlOffHeap *ohp, Sint offs, Eterm* low, Eterm* high)
{
    if (ohp->first && ptr_within((Eterm *)ohp->first, low, high)) {
        Eterm** uptr = (Eterm**) (void *) &ohp->first;
        *uptr += offs;
    }
}
#undef ptr_within

Eterm
erts_gc_after_bif_call_lhf(Process* p, ErlHeapFragment *live_hf_end,
			   Eterm result, Eterm* regs, Uint arity)
{
    int cost;

    if (!p->mbuf) {
        /* Must have GC:d in BIF call... invalidate live_hf_end */
        live_hf_end = ERTS_INVALID_HFRAG_PTR;
    }

    if (p->flags & (F_HIBERNATE_SCHED | F_DISABLE_GC)) {

        if ((p->flags & F_DISABLE_GC)
            && p->live_hf_end == ERTS_INVALID_HFRAG_PTR
            && is_non_value(result)
            && p->freason == TRAP) {
            /* This is first trap with disabled GC. Save live_hf_end marker. */
            p->live_hf_end = live_hf_end;
        }
        /*else:
         * a subsequent trap with disabled GC
         *
         * OR
         *
         * We just hibernated. We do *not* want to mess
         * up the hibernation by an ordinary GC...
         */
	return result;
    }

    if (p->sig_qs.flags & (FS_ON_HEAP_MSGQ|FS_OFF_HEAP_MSGQ_CHNG)) {
        erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
        erts_proc_sig_fetch(p);
	erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
    }
    
    if (is_non_value(result)) {
	if (p->freason == TRAP) {
	    cost = garbage_collect(p, live_hf_end, 0, regs, p->arity, p->fcalls, 0);
	} else {
	    cost = garbage_collect(p, live_hf_end, 0, regs, arity, p->fcalls, 0);
	}
    } else {
	Eterm val[1];

	val[0] = result;
	cost = garbage_collect(p, live_hf_end, 0, val, 1, p->fcalls, 0);
        if (ERTS_PROC_IS_EXITING(p)) {
            result = THE_NON_VALUE;
        }
        else {
            result = val[0];
        }

    }
    BUMP_REDS(p, cost);

    return result;
}

Eterm
erts_gc_after_bif_call(Process* p, Eterm result, Eterm* regs, Uint arity)
{
    return erts_gc_after_bif_call_lhf(p, ERTS_INVALID_HFRAG_PTR,
                                      result, regs, arity);
}

static ERTS_INLINE void assert_no_active_writers(Process *p)
{
#ifdef DEBUG
    BinRef *br = (BinRef*)p->wrt_bins;
    while (br) {
        ASSERT(br->thing_word == HEADER_BIN_REF);
        ERTS_ASSERT(!((br->val)->intern.flags & BIN_FLAG_ACTIVE_WRITER));
        br = (BinRef*)br->next;
    }
#endif
}

#define ERTS_DELAY_GC_EXTRA_FREE 40
#define ERTS_ABANDON_HEAP_COST 10

static int
delay_garbage_collection(Process *p, int need, int fcalls)
{
    ErlHeapFragment *hfrag;
    Eterm *orig_heap, *orig_hend, *orig_htop, *orig_stop;
    Eterm *hend;
    Uint hsz, ssz;
    int reds_left;

    ERTS_HOLE_CHECK(p);

    if (need == 0) {
        if (p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC)) {
            ASSERT(!ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(p)));
            goto force_reschedule;
        }
	return 1;
    }
    /*
     * Satisfy need in a heap fragment...
     */
    ASSERT(need > 0);

    orig_heap = p->heap;
    orig_hend = p->hend;
    orig_htop = p->htop;
    orig_stop = p->stop;

    ssz = orig_hend - orig_stop;
    hsz = ssz + need + ERTS_DELAY_GC_EXTRA_FREE + S_RESERVED;

    /* Allocate one extra word at the end to save the high water mark. */
    hfrag = new_message_buffer(hsz + 1);

    copy_erlang_stack(p, &hfrag->mem[0], hsz);

    p->heap = p->htop = &hfrag->mem[0];
    hend = &hfrag->mem[hsz];

    /* Save the original high water mark at the end of the current
     * heap to make it possible to do a minor GC later. */
    if (p->abandoned_heap) {
        *hend = (Eterm) (p->hend[0]);
    } else {
        *hend = (Eterm) p->high_water;
    }

    p->hend = hend;

    /* Keep the high water mark pointing into the current heap to ensure
     * that the test for the safe range in the update_record_in_place (JIT)
     * stays honest. */
    p->high_water = p->heap;

    if (p->abandoned_heap) {
	/*
         * Active heap already in a fragment; adjust it and
         * save it into mbuf list...
         */
        ErlHeapFragment *hfrag = ((ErlHeapFragment *)
                                  (((char *) orig_heap)
                                   - offsetof(ErlHeapFragment, mem)));
        Uint used = orig_htop - orig_heap;
        hfrag->used_size = used;
        p->mbuf_sz += used;
        ASSERT(hfrag->used_size <= hfrag->alloc_size-1);
        ASSERT(!hfrag->off_heap.first && !hfrag->off_heap.overhead);
        hfrag->next = p->mbuf;
        p->mbuf = hfrag;
    }
    else {
	/* Do not leave a hole in the abandoned heap... */
	if (orig_htop < orig_hend) {
	    erts_write_heap_filler(orig_htop, orig_hend-orig_htop);
	    if (orig_htop + 1 < orig_hend) {
		orig_hend[-1] = (Uint) (orig_htop - orig_heap);
		p->flags |= F_ABANDONED_HEAP_USE;
	    }
	}
	p->abandoned_heap = orig_heap;
        erts_adjust_memory_break(p, orig_htop - p->high_water);
    }

#ifdef CHECK_FOR_HOLES
    p->last_htop = p->htop;
    p->heap_hfrag = hfrag;
#endif

force_reschedule:

    /* Make sure that we do a proper GC as soon as possible... */
    p->flags |= F_FORCE_GC;
    reds_left = ERTS_REDS_LEFT(p, fcalls);
    ASSERT(CONTEXT_REDS - reds_left >= erts_proc_sched_data(p)->virtual_reds);

    if (reds_left > ERTS_ABANDON_HEAP_COST) {
	int vreds = reds_left - ERTS_ABANDON_HEAP_COST;
	erts_proc_sched_data((p))->virtual_reds += vreds;
    }

    ERTS_CHK_MBUF_SZ(p);

    ASSERT(CONTEXT_REDS >= erts_proc_sched_data(p)->virtual_reds);
    return reds_left;
}

static ERTS_FORCE_INLINE Uint
young_gen_usage(Process *p, Uint *ext_msg_usage)
{
    Uint hsz;
    Eterm *aheap;

    ERTS_CHK_MBUF_SZ(p);

    hsz = p->mbuf_sz;

    if (p->sig_qs.flags & FS_ON_HEAP_MSGQ) {
        ERTS_FOREACH_SIG_PRIVQS(
            p, mp,
            {
                /*
                 * We leave not yet decoded distribution messages
                 * as they are in the queue since it is not
                 * possible to determine a maximum size until
                 * actual decoding. However, we use their estimated
                 * size when calculating need, and by this making
                 * it more likely that they will fit on the heap
                 * when actually decoded.
                 *
                 * We however ignore off heap messages...
                 */
                if (ERTS_SIG_IS_MSG(mp)
                    && mp->data.attached
                    && mp->data.attached != ERTS_MSG_COMBINED_HFRAG) {
                    Uint sz = erts_msg_attached_data_size(mp);
                    if (ERTS_SIG_IS_EXTERNAL_MSG(mp))
                        *ext_msg_usage += sz;
                    hsz += sz;
                }
            });
    }

    hsz += p->htop - p->heap;
    aheap = p->abandoned_heap;
    if (aheap) {
	/* used in orig heap */
	if (p->flags & F_ABANDONED_HEAP_USE)
	    hsz += aheap[p->heap_sz-1];
	else
	    hsz += p->heap_sz;
    }
    return hsz;
}

static Eterm*
get_orig_heap(Process *p, Eterm **p_htop, Eterm **p_high_water) {
    Eterm *aheap = p->abandoned_heap;
    Eterm *htop;

    /* See delay_garbage_collection(). */

    ASSERT(aheap != NULL);

    if (p->flags & F_ABANDONED_HEAP_USE) {
        htop = aheap + aheap[p->heap_sz-1];
    } else {
        htop = aheap + p->heap_sz;
    }

    *p_htop = htop;

    if (p_high_water) {
        Eterm *high_water;

        high_water = (Eterm *)(p->hend[0]);

        ASSERT(aheap <= high_water);
        ASSERT(high_water <= htop);

        /* The high water pointer must be aligned to a word boundary. */
        ASSERT(((UWord) high_water) % sizeof(UWord) == 0);

        *p_high_water = high_water;
    }

    return aheap;
}

static ERTS_INLINE void
check_for_possibly_long_gc(Process *p, Uint ygen_usage)
{
    int major;
    Sint sz;

    major = (p->flags & F_NEED_FULLSWEEP) || GEN_GCS(p) >= MAX_GEN_GCS(p);

    sz = ygen_usage;
    sz += p->hend - p->stop;
    if (major)
	sz += p->old_htop - p->old_heap;
    if (p->sig_qs.flags & FS_ON_HEAP_MSGQ) {
        Sint len, max_len = ERTS_POTENTIALLY_LONG_GC_HSIZE - sz;
        if (max_len < 0)
            len = -1;
        else
            len = erts_proc_sig_privqs_len(p, max_len, ERTS_LONG_GC_MAX_NMSIGS);
        if (len < 0)
            sz = ERTS_POTENTIALLY_LONG_GC_HSIZE;
        else
            sz += (Uint) len;
    }
    if (sz >= ERTS_POTENTIALLY_LONG_GC_HSIZE) {
        ASSERT(!(p->flags & (F_DISABLE_GC|F_DELAY_GC)));
	p->flags |= major ? F_DIRTY_MAJOR_GC : F_DIRTY_MINOR_GC;
        erts_schedule_dirty_sys_execution(p);
    }
}


/*
 * Garbage collect a process.
 *
 * p: Pointer to the process structure.
 * need: Number of Eterm words needed on the heap.
 * objv: Array of terms to add to rootset; that is to preserve.
 * nobj: Number of objects in objv.
 */
static int
garbage_collect(Process* p, ErlHeapFragment *live_hf_end,
		Uint need, Eterm* objv, int nobj, int fcalls,
		Uint max_young_gen_usage)
{
    Uint reclaimed_now = 0;
    Uint ygen_usage;
    Uint ext_msg_usage = 0;
    Eterm gc_trace_end_tag;
    int reds;
    ErtsMonotonicTime start_time = 0;
    ErtsSchedulerData *esdp = erts_proc_sched_data(p);
    erts_aint32_t state;
#ifdef USE_VM_PROBES
    DTRACE_CHARBUF(pidbuf, DTRACE_TERM_BUF_SIZE);
#endif
    ERTS_MSACC_PUSH_STATE();

    ERTS_CHK_MBUF_SZ(p);

    ASSERT(CONTEXT_REDS - ERTS_REDS_LEFT(p, fcalls) >= esdp->virtual_reds);

    state = erts_atomic32_read_nob(&p->state);

    if ((p->flags & (F_DISABLE_GC|F_DELAY_GC)) || state & ERTS_PSFLG_EXITING) {
    delay_gc_before_start:
	return delay_garbage_collection(p, need, fcalls);
    }

    ygen_usage = max_young_gen_usage ? max_young_gen_usage : young_gen_usage(p, &ext_msg_usage);

    if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
	check_for_possibly_long_gc(p, ygen_usage);
	if (p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC))
	    goto delay_gc_before_start;
    }

    if (p->abandoned_heap)
	live_hf_end = ERTS_INVALID_HFRAG_PTR;
    else if (p->live_hf_end != ERTS_INVALID_HFRAG_PTR)
	live_hf_end = p->live_hf_end;

    ERTS_MSACC_SET_STATE_CACHED(ERTS_MSACC_STATE_GC);

    erts_atomic32_read_bor_nob(&p->state, ERTS_PSFLG_GC);
    if (erts_system_monitor_long_gc)
	start_time = erts_get_monotonic_time(esdp);

    ERTS_CHK_OFFHEAP(p);

    ErtsGcQuickSanityCheck(p);
#ifdef DEBUG
    erts_dbg_check_no_empty_boxed_non_literal_on_heap(p, NULL);
#endif
#ifdef USE_VM_PROBES
    *pidbuf = '\0';
    if (DTRACE_ENABLED(gc_major_start)
        || DTRACE_ENABLED(gc_major_end)
        || DTRACE_ENABLED(gc_minor_start)
        || DTRACE_ENABLED(gc_minor_end)) {
        dtrace_proc_str(p, pidbuf);
    }
#endif

    if (p->abandoned_heap)
        erts_adjust_memory_break(p, p->htop - p->heap + p->mbuf_sz);
    else
        erts_adjust_memory_break(p, p->htop - p->high_water + p->mbuf_sz);

    /*
     * Test which type of GC to do.
     */

    if (GEN_GCS(p) < MAX_GEN_GCS(p) && !(FLAGS(p) & F_NEED_FULLSWEEP)) {
        if (ERTS_IS_P_TRACED_FL(p, F_TRACE_GC)) {
            trace_gc(p, am_gc_minor_start, need, THE_NON_VALUE);
        }
        DTRACE2(gc_minor_start, pidbuf, need);
        reds = minor_collection(p, live_hf_end, need + ext_msg_usage, objv, nobj,
				ygen_usage, &reclaimed_now);
        DTRACE2(gc_minor_end, pidbuf, reclaimed_now);
        if (reds == -1) {
            if (ERTS_IS_P_TRACED_FL(p, F_TRACE_GC)) {
                trace_gc(p, am_gc_minor_end, reclaimed_now, THE_NON_VALUE);
            }
	    if (!ERTS_SCHEDULER_IS_DIRTY(esdp)) {
		p->flags |= F_NEED_FULLSWEEP;
		check_for_possibly_long_gc(p, ygen_usage);
		if (p->flags & F_DIRTY_MAJOR_GC)
		    goto delay_gc_after_start;
	    }
            goto do_major_collection;
        }
        if (ERTS_SCHEDULER_IS_DIRTY(esdp))
            p->flags &= ~F_DIRTY_MINOR_GC;
        gc_trace_end_tag = am_gc_minor_end;
    } else {
do_major_collection:
        ERTS_MSACC_SET_STATE_CACHED_X(ERTS_MSACC_STATE_GC_FULL);
        if (ERTS_IS_P_TRACED_FL(p, F_TRACE_GC)) {
            trace_gc(p, am_gc_major_start, need, THE_NON_VALUE);
        }
        DTRACE2(gc_major_start, pidbuf, need);
        reds = major_collection(p, live_hf_end, need + ext_msg_usage, objv, nobj,
				ygen_usage, &reclaimed_now);
        if (ERTS_SCHEDULER_IS_DIRTY(esdp))
            p->flags &= ~(F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC);
        DTRACE2(gc_major_end, pidbuf, reclaimed_now);
        gc_trace_end_tag = am_gc_major_end;
        ERTS_MSACC_SET_STATE_CACHED_X(ERTS_MSACC_STATE_GC);
    }

    /* Max heap size has been reached and the process was configured
       to be killed, so we kill it and set it in a delayed garbage
       collecting state. There should be no gc_end trace or
       long_gc/large_gc triggers when this happens as process was
       killed before a GC could be done. */
    if (reds == -2) {
        int res;

        erts_set_self_exiting(p, am_killed);

    delay_gc_after_start:
        /* erts_send_exit_signal looks for ERTS_PSFLG_GC, so
           we have to remove it after the signal is sent */
        erts_atomic32_read_band_nob(&p->state, ~ERTS_PSFLG_GC);

        /* We have to make sure that we have space for need on the heap */
        res = delay_garbage_collection(p, need, fcalls);
        ERTS_MSACC_POP_STATE();
        return res;
    }

    /*
     * Finish.
     */
    assert_no_active_writers(p);
    ERTS_CHK_OFFHEAP(p);
    ErtsGcQuickSanityCheck(p);

    erts_atomic32_read_band_nob(&p->state, ~ERTS_PSFLG_GC);

    if (ERTS_IS_P_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, gc_trace_end_tag, reclaimed_now, THE_NON_VALUE);
    }

    if (start_time && erts_system_monitor_long_gc) {
	ErtsMonotonicTime end_time;
	Uint gc_time;
	if (erts_test_long_gc_sleep)
	    while (0 != erts_milli_sleep(erts_test_long_gc_sleep));
	end_time = erts_get_monotonic_time(esdp);
	gc_time = (Uint) ERTS_MONOTONIC_TO_MSEC(end_time - start_time);
	if (gc_time && gc_time > erts_system_monitor_long_gc) {
	    monitor_long_gc(p, gc_time);
	}
    }
    if (erts_system_monitor_large_heap != 0) {
	Uint size = HEAP_SIZE(p);
	size += OLD_HEAP(p) ? OLD_HEND(p) - OLD_HEAP(p) : 0;
	if (size >= erts_system_monitor_large_heap)
	    monitor_large_heap(p, size);
    }

    if (ERTS_SCHEDULER_IS_DIRTY(esdp)) {
	erts_mtx_lock(&dirty_gc.mtx);
	dirty_gc.info.garbage_cols++;
	dirty_gc.info.reclaimed += reclaimed_now;
	erts_mtx_unlock(&dirty_gc.mtx);
    }
    else
    {
	esdp->gc_info.garbage_cols++;
	esdp->gc_info.reclaimed += reclaimed_now;
    }
    
    FLAGS(p) &= ~(F_FORCE_GC|F_HIBERNATED);
    p->live_hf_end = ERTS_INVALID_HFRAG_PTR;

    ERTS_MSACC_POP_STATE();

#ifdef CHECK_FOR_HOLES
    /*
     * We intentionally do not rescan the areas copied by the GC.
     * We trust the GC not to leave any holes.
     */
    p->last_htop = p->htop;
    p->last_mbuf = 0;
#endif    

#ifdef DEBUG
    /*
     * The scanning for pointers from the old_heap into the new_heap or
     * heap fragments turned out to be costly, so we remember how far we
     * have scanned this time and will start scanning there next time.
     * (We will not detect wild writes into the old heap, or modifications
     * of the old heap in-between garbage collections.)
     */
    p->last_old_htop = p->old_htop;
#endif

    ASSERT(!p->mbuf);
    ASSERT(!ERTS_IS_GC_DESIRED(p));
    ASSERT(need <= HEAP_LIMIT(p) - HEAP_TOP(p));

    return reds;
}

int
erts_garbage_collect_nobump(Process* p, Uint need, Eterm* objv, int nobj, int fcalls)
{
    int reds, reds_left;
    if (p->sig_qs.flags & (FS_ON_HEAP_MSGQ|FS_OFF_HEAP_MSGQ_CHNG)) {
        erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
        erts_proc_sig_fetch(p);
	erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
    }
    reds = garbage_collect(p, ERTS_INVALID_HFRAG_PTR, need, objv, nobj, fcalls, 0);
    reds_left = ERTS_REDS_LEFT(p, fcalls);
    if (reds > reds_left)
	reds = reds_left;
    ASSERT(CONTEXT_REDS - (reds_left - reds) >= erts_proc_sched_data(p)->virtual_reds);
    return reds;
}

void
erts_garbage_collect(Process* p, Uint need, Eterm* objv, int nobj)
{
    int reds;
    if (p->sig_qs.flags & (FS_ON_HEAP_MSGQ|FS_OFF_HEAP_MSGQ_CHNG)) {
        erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
        erts_proc_sig_fetch(p);
	erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
    }
    reds = garbage_collect(p, ERTS_INVALID_HFRAG_PTR, need, objv, nobj, p->fcalls, 0);
    BUMP_REDS(p, reds);
    ASSERT(CONTEXT_REDS - ERTS_BIF_REDS_LEFT(p)
	   >= erts_proc_sched_data(p)->virtual_reds);
}


/*
 * Place all living data on a the new heap; deallocate any old heap.
 * Meant to be used by hibernate/3.
 */
static int
garbage_collect_hibernate(Process* p, int check_long_gc)
{
    Eterm *collection_heap, *collection_htop, *final_heap, *final_stack;
    Uint final_size, heap_size, stack_size;
    Sint stack_offset, heap_offset;
    const char *heap_area, *stack_area;
    Uint heap_area_sz, stack_area_sz;

    ERTS_ASSERT(!(p->flags & F_DISABLE_GC));

    if (p->sig_qs.flags & FS_ON_HEAP_MSGQ) {
        erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
        erts_proc_sig_fetch(p);
        erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
    }

    if (ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(p))) {
        p->flags &= ~(F_DIRTY_GC_HIBERNATE|F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC);
    } else if (check_long_gc) {
        Uint flags = p->flags;

        p->flags |= F_NEED_FULLSWEEP;

        check_for_possibly_long_gc(p, ((p->htop - p->heap) +
                                       p->mbuf_sz + S_RESERVED));

        if (p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC)) {
            p->flags = flags|F_DIRTY_GC_HIBERNATE;
            return 1;
        }

        p->flags = flags;
    }

    erts_atomic32_read_bor_nob(&p->state, ERTS_PSFLG_GC);
    ErtsGcQuickSanityCheck(p);

    heap_size = p->heap_sz + (p->old_htop - p->old_heap) + p->mbuf_sz;
    stack_size = STACK_START(p) - STACK_TOP(p);

    /* Allocate a new heap and move all living terms to it. Note that this
     * temporary heap does not need to include space for the stack. */
    collection_heap = (Eterm*)ERTS_HEAP_ALLOC(ERTS_ALC_T_TMP_HEAP,
                                              heap_size * sizeof(Eterm));
    collection_htop = full_sweep_heaps(p,
                                       ERTS_INVALID_HFRAG_PTR,
                                       1,
                                       collection_heap,
                                       collection_heap,
                                       (char*)p->old_heap,
                                       (char*)p->old_htop - (char*)p->old_heap,
                                       p->arg_reg,
                                       p->arity);

#ifdef HARDDEBUG
    disallow_heap_frag_ref_in_heap(p, heap, htop);
#endif

    heap_size = collection_htop - collection_heap;
    final_size = heap_size + S_RESERVED + stack_size;

    /* Move the heap to its final destination, compacting it together with the
     * stack.
     *
     * IMPORTANT: We have garbage collected to a temporary heap and then copy
     * the result to a newly allocated heap of exact size.
     *
     * !! This is intentional !! Garbage collecting as usual and then shrinking
     * the heap by reallocating it caused serious fragmentation problems when
     * large amounts of processes were hibernated. */
    final_heap = ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm) * final_size);
    final_stack = &final_heap[final_size - stack_size];
    sys_memcpy(final_heap, collection_heap, heap_size * sizeof(Eterm));
    sys_memcpy(final_stack, p->stop, stack_size * sizeof(Eterm));

    stack_offset = final_stack - p->stop;
    heap_offset = final_heap - collection_heap;

    heap_area = (const char *) collection_heap;
    heap_area_sz = heap_size * sizeof(Eterm);

    stack_area = (const char *) p->stop;
    stack_area_sz = stack_size * sizeof(Eterm);

    erts_deallocate_young_generation(p);
    erts_free(ERTS_ALC_T_TMP_HEAP, collection_heap);

    p->heap = final_heap;
    p->heap_sz = final_size;
    p->high_water = &final_heap[heap_size];
    p->htop = &final_heap[heap_size];
    p->hend = &final_heap[final_size];
    p->stop = final_stack;

    FLAGS(p) &= ~F_FORCE_GC;
    p->live_hf_end = ERTS_INVALID_HFRAG_PTR;

    offset_heap(final_heap, heap_size, heap_offset, heap_area, heap_area_sz);
    offset_rootset(p,
                   heap_offset, heap_area, heap_area_sz,
                   stack_offset, stack_area, stack_area_sz,
                   p->arg_reg, p->arity);

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
        FRAME_POINTER(p) += stack_offset;
    }

#ifdef CHECK_FOR_HOLES
    p->last_htop = p->htop;
    p->last_mbuf = NULL;
#endif
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

    ErtsGcQuickSanityCheck(p);

    p->flags |= F_HIBERNATED;
    erts_atomic32_read_band_nob(&p->state, ~ERTS_PSFLG_GC);

    return gc_cost(final_size, final_size);
}

void
erts_garbage_collect_hibernate(Process* p)
{
    int reds = garbage_collect_hibernate(p, 1);
    BUMP_REDS(p, reds);
}

int
erts_garbage_collect_literals(Process* p, Eterm* literals,
			      Uint byte_lit_size,
			      struct erl_off_heap_header* oh,
			      int fcalls)
{
    Uint lit_size = byte_lit_size / sizeof(Eterm);
    Uint old_heap_size;
    Eterm* temp_lit;
    Sint offs;
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
    Roots* roots;
    char* area;
    Uint area_sz;
    Eterm* old_htop;
    Uint n;
    Uint ygen_usage = 0;
    Uint ext_msg_usage = 0;
    struct erl_off_heap_header** prev = NULL;
    Sint64 reds;
    int hibernated = !!(p->flags & F_HIBERNATED);

    if (p->flags & (F_DISABLE_GC|F_DELAY_GC))
	ERTS_INTERNAL_ERROR("GC disabled");

    /*
     * First an ordinary major collection...
     */

    p->flags |= F_NEED_FULLSWEEP;

    if (p->sig_qs.flags & (FS_ON_HEAP_MSGQ|FS_OFF_HEAP_MSGQ_CHNG)) {
        erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
        erts_proc_sig_fetch(p);
	erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
    }
    
    if (ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(p)))
	p->flags &= ~F_DIRTY_CLA;
    else {
        Uint size = byte_lit_size/sizeof(Uint);
	ygen_usage = young_gen_usage(p, &ext_msg_usage);
        if (hibernated)
            size = size*2 + 3*ygen_usage;
        else
            size = size + 2*ygen_usage;
	check_for_possibly_long_gc(p, size);
	if (p->flags & F_DIRTY_MAJOR_GC) {
	    p->flags |= F_DIRTY_CLA;
	    return 10;
	}
    }

    reds = (Sint64) garbage_collect(p, ERTS_INVALID_HFRAG_PTR, ext_msg_usage,
				    p->arg_reg, p->arity, fcalls,
				    ygen_usage);
    if (ERTS_PROC_IS_EXITING(p)) {
        return 0;
    }

    ASSERT(!(p->flags & (F_DIRTY_MAJOR_GC|F_DIRTY_MINOR_GC)));

    if (MAX_HEAP_SIZE_GET(p)) {
        Uint new_heap_size;
        Uint old_heap_size;
        Uint total_heap_size;

        new_heap_size = HEAP_END(p) - HEAP_START(p);
        old_heap_size = erts_next_heap_size(lit_size, 0);
        total_heap_size = new_heap_size + old_heap_size;
        if (has_reached_max_heap_size(p, total_heap_size) &&
            reached_max_heap_size(p, total_heap_size,
                                  new_heap_size, old_heap_size)) {
            erts_set_self_exiting(p, am_killed);
            return 0;
        }
    }

    /*
     * Set GC state.
     */
    erts_atomic32_read_bor_nob(&p->state, ERTS_PSFLG_GC);

    /*
     * Just did a major collection (which has discarded the old heap),
     * so that we don't have to cope with pointer to literals on the
     * old heap. We will now allocate an old heap to contain the literals.
     */
    
    ASSERT(p->old_heap == 0);	/* Must NOT have an old heap yet. */
    old_heap_size = erts_next_heap_size(lit_size, 0);
    p->old_heap = p->old_htop = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,
							 sizeof(Eterm)*old_heap_size);
    p->old_hend = p->old_heap + old_heap_size;

    /*
     * We soon want to garbage collect the literals. But since a GC is
     * destructive (MOVED markers are written), we must copy the literals
     * to a temporary area and change all references to literals.
     */
    temp_lit = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, byte_lit_size);
    sys_memcpy(temp_lit, literals, byte_lit_size);
    offs = temp_lit - literals;
    offset_heap(temp_lit, lit_size, offs, (char *) literals, byte_lit_size);
    offset_heap(p->heap, p->htop - p->heap, offs, (char *) literals, byte_lit_size);
    offset_rootset(p,
                   offs, (char *) literals, byte_lit_size,
                   0, NULL, 0,
                   p->arg_reg, p->arity);
    if (oh) {
	oh = (struct erl_off_heap_header *) ((Eterm *)(void *) oh + offs);
    }

    /*
     * Now the literals are placed in memory that is safe to write into,
     * so now we GC the literals into the old heap. First we go through the
     * rootset.
     */

    area = (char *) temp_lit;
    area_sz = byte_lit_size;
    n = setup_rootset(p, p->arg_reg, p->arity, &rootset);
    roots = rootset.roots;
    old_htop = p->old_htop;
    while (n--) {
        Eterm* g_ptr = roots->v;
        Uint g_sz = roots->sz;
	Eterm* ptr;
	Eterm val;

	roots++;

        for ( ; g_sz--; g_ptr++) {
            Eterm gval = *g_ptr;

            switch (primary_tag(gval)) {
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(gval);
		val = *ptr;
                if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
                    *g_ptr = val;
		} else if (ErtsInArea(ptr, area, area_sz)) {
                    move_boxed(ptr,val,&old_htop,g_ptr);
		}
		break;
	    case TAG_PRIMARY_LIST:
                ptr = list_val(gval);
                val = *ptr;
                if (IS_MOVED_CONS(val)) { /* Moved */
                    *g_ptr = ptr[1];
		} else if (ErtsInArea(ptr, area, area_sz)) {
                    move_cons(ptr,val,&old_htop,g_ptr);
                }
		break;
	    default:
		break;
	    }
	}
    }
    ASSERT(p->old_htop <= old_htop && old_htop <= p->old_hend);
    cleanup_rootset(&rootset);

    /*
     * Now all references in the rootset to the literals have been updated.
     * Now we'll have to go through all heaps updating all other references.
     */

    old_htop = sweep_literals_to_old_heap(p->heap, p->htop, old_htop, area, area_sz);
    old_htop = sweep_literal_area(p->old_heap, old_htop,
				  (char *) p->old_heap, sizeof(Eterm)*old_heap_size,
				  area, area_sz);
    ASSERT(p->old_htop <= old_htop && old_htop <= p->old_hend);
    p->old_htop = old_htop;

    /*
     * Prepare to sweep off-heap objects. Since all MSOs on the new
     * heap must be come before MSOs on the old heap, find the end of
     * current MSO list and use that as a starting point.
     */

    if (oh) {
        prev = &MSO(p).first;
        while (*prev) {
            prev = &(*prev)->next;
        }
    }

    /*
     * Sweep through all off-heap objects in the temporary literal area.
     */

    while (oh) {
        if (IS_MOVED_BOXED(oh->thing_word)) {
            struct erl_off_heap_header* ptr;

            /* This off-heap object has been copied to the heap.
             * We must increment its reference count and
             * link it into the MSO list for the process.*/

            ptr = (struct erl_off_heap_header*) boxed_val(oh->thing_word);
            switch (thing_subtag(ptr->thing_word)) {
            case BIN_REF_SUBTAG:
                {
                    Binary *refc_binary = ((BinRef*)ptr)->val;
                    ASSERT((refc_binary->intern.flags &
                           (BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER)) == 0);
                    erts_refc_inc(&refc_binary->intern.refc, 2);
                    break;
                }
            case REF_SUBTAG:
                {
                    ErtsMagicBinary *bptr;
                    ASSERT(is_magic_ref_thing(ptr));
                    bptr = ((ErtsMRefThing *) ptr)->mb;
                    erts_refc_inc(&bptr->intern.refc, 2);
                    break;
                }
            default:
                {
                    ExternalThing *etp;
                    ASSERT(is_external_header(ptr->thing_word));
                    etp = (ExternalThing *) ptr;
                    erts_ref_node_entry(etp->node, 2,
                                        make_boxed(&oh->thing_word));
                    break;
                }
            }

            *prev = ptr;
            prev = &ptr->next;
        }

        oh = oh->next;
    }

    if (prev) {
        *prev = NULL;
    }

    /*
     * We no longer need this temporary area.
     */
    erts_free(ERTS_ALC_T_TMP, (void *) temp_lit);

    /*
     * Restore status.
     */
    erts_atomic32_read_band_nob(&p->state, ~ERTS_PSFLG_GC);

    reds += (Sint64) gc_cost((p->htop - p->heap) + byte_lit_size/sizeof(Uint), 0);

    if (hibernated) {
        /* Restore the process into hibernated state... */
        reds += garbage_collect_hibernate(p, 0);
    }

    if (reds > INT_MAX)
	return INT_MAX;
    return (int) reds;
}

static int
minor_collection(Process* p, ErlHeapFragment *live_hf_end,
		 Uint need, Eterm* objv, int nobj,
		 Uint ygen_usage, Uint *recl)
{
    Eterm *mature = p->abandoned_heap ? p->abandoned_heap : p->heap;
    Eterm *high_water;
    Uint mature_size;
    Uint size_before = ygen_usage;
#ifdef DEBUG
    Uint debug_tmp = 0;
#endif

    if (p->abandoned_heap) {
        /* See delay_garbage_collection(). */
        high_water = (Eterm *)(p->hend[0]);
    } else {
        high_water = p->high_water;
    }

#ifdef DEBUG
    if (p->abandoned_heap) {
        ASSERT(p->abandoned_heap <= high_water);
        ASSERT(high_water - p->abandoned_heap <= size_before);

        /* The high water pointer must be aligned to a word boundary. */
        ASSERT(((UWord) high_water) % sizeof(UWord) == 0);
    }
#endif

    mature_size = high_water - mature;

    need += S_RESERVED;

    /*
     * Check if we have gone past the max heap size limit
     */

    if (MAX_HEAP_SIZE_GET(p)) {
        Uint heap_size = size_before,
            /* Note that we also count the un-allocated area
               in between the stack and heap */
            stack_size = HEAP_END(p) - HEAP_TOP(p),
            extra_heap_size,
            extra_old_heap_size = 0;

        /* Add potential old heap size */
        if (OLD_HEAP(p) == NULL && mature_size != 0) {
            extra_old_heap_size = erts_next_heap_size(size_before, 1);
            heap_size += extra_old_heap_size;
        } else if (OLD_HEAP(p))
            heap_size += OLD_HEND(p) - OLD_HEAP(p);

        /* Add potential new young heap size */
        extra_heap_size = next_heap_size(p, stack_size + MAX(size_before,need), 0);
        heap_size += extra_heap_size;

        if (has_reached_max_heap_size(p, heap_size))
            if (reached_max_heap_size(p, heap_size, extra_heap_size, extra_old_heap_size))
                return -2;
    }

    /*
     * Allocate an old heap if we don't have one and if we'll need one.
     */

    if (OLD_HEAP(p) == NULL && mature_size != 0) {
        Eterm* n_old;

        /* Note: We choose a larger heap size than strictly needed,
         * which seems to reduce the number of fullsweeps.
         * This improved Estone by more than 1200 estones on my computer
         * (Ultra Sparc 10).
         */
        Uint n_old_sz = erts_next_heap_size(size_before, 1);

        /* Create new, empty old_heap */
        n_old = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,
					  sizeof(Eterm)*n_old_sz);

        OLD_HEND(p) = n_old + n_old_sz;
        OLD_HEAP(p) = OLD_HTOP(p) = n_old;
    }

    /*
     * Do a minor collection if there is an old heap and if it
     * is large enough.
     */

    if (OLD_HEAP(p) &&
	((mature_size <= OLD_HEND(p) - OLD_HTOP(p)) &&
	 ((p->bin_old_vheap_sz > p->bin_old_vheap)) ) ) {
	Eterm *prev_old_htop;
	Uint stack_size, size_after, adjust_size, need_after, new_sz, new_mature;

	stack_size = STACK_START(p) - STACK_TOP(p);
	new_sz = stack_size + MAX(size_before, need);
        new_sz = next_heap_size(p, new_sz, 0);

	prev_old_htop = p->old_htop;
        do_minor(p, live_hf_end, (char *) mature, mature_size*sizeof(Eterm),
		 new_sz, objv, nobj);

	if (p->sig_qs.flags & FS_ON_HEAP_MSGQ)
	    move_msgs_to_heap(p);

	new_mature = p->old_htop - prev_old_htop;

	size_after = new_mature;
        size_after += HEAP_TOP(p) - HEAP_START(p) + p->mbuf_sz;
        *recl += (size_before - size_after);

	ErtsGcQuickSanityCheck(p);

        GEN_GCS(p)++;
        need_after = ((HEAP_TOP(p) - HEAP_START(p))
                      + need
                      + stack_size);
	
        /*
         * Excessively large heaps should be shrunk, but
         * don't even bother on reasonable small heaps.
         *
         * The reason for this is that after tenuring, we often
         * use a really small portion of new heap, therefore, unless
         * the heap size is substantial, we don't want to shrink.
         */

	adjust_size = 0;

        if ((HEAP_SIZE(p) > 3000) && (4 * need_after < HEAP_SIZE(p)) &&
            ((HEAP_SIZE(p) > 8000) ||
             (HEAP_SIZE(p) > (OLD_HEND(p) - OLD_HEAP(p))))) {
	    Uint wanted = 3 * need_after;
	    Uint old_heap_sz = OLD_HEND(p) - OLD_HEAP(p);

	    /*
	     * Additional test to make sure we don't make the heap too small
	     * compared to the size of the older generation heap.
	     */
	    if (wanted*9 < old_heap_sz) {
		Uint new_wanted = old_heap_sz / 8;
		if (new_wanted > wanted) {
		    wanted = new_wanted;
		}
	    }

	    wanted = wanted < MIN_HEAP_SIZE(p) ? MIN_HEAP_SIZE(p)
					       : next_heap_size(p, wanted, 0);
            if (wanted < HEAP_SIZE(p)) {
                shrink_new_heap(p, wanted, objv, nobj);
		adjust_size = p->htop - p->heap;
            }

        }
        else if (need_after > HEAP_SIZE(p)) {
            grow_new_heap(p, next_heap_size(p, need_after, 0), objv, nobj);
            adjust_size = p->htop - p->heap;
        }
	/*else: The heap size turned out to be just right. We are done. */

	ASSERT(HEAP_SIZE(p) == next_heap_size(p, HEAP_SIZE(p), 0));

        /* The heap usage during GC should be larger than what we end up
           after a GC, even if we grow it. If this assertion is not true
           we have to check size in grow_new_heap and potentially kill the
           process from there */
        ASSERT(!MAX_HEAP_SIZE_GET(p) ||
               !(MAX_HEAP_SIZE_FLAGS_GET(p) & MAX_HEAP_SIZE_KILL) ||
               MAX_HEAP_SIZE_GET(p) > (young_gen_usage(p, &debug_tmp) +
                                       (OLD_HEND(p) - OLD_HEAP(p)) +
                                       (HEAP_END(p) - HEAP_TOP(p))));

	return gc_cost(size_after, adjust_size);
    }

    /*
     * Not enough room for a minor collection. Must force a major collection.
     */
    return -1;
}

/* Copies the Erlang stack to the end of the new heap, adjusting continuation
 * pointers as needed. */
static ERTS_INLINE void copy_erlang_stack(Process *p,
                                          Eterm *new_heap,
                                          SWord new_sz) {
    const ErtsFrameLayout frame_layout = erts_frame_layout;

    Eterm *prev_stack_top, *prev_stack_end;
    Eterm *new_stack_top;
    SWord stack_size;

    ASSERT(new_heap != HEAP_START(p));

    prev_stack_top = STACK_TOP(p);
    prev_stack_end = STACK_START(p);

    stack_size = prev_stack_end - prev_stack_top;
    new_stack_top = &new_heap[new_sz - stack_size];

#if defined(DEBUG) && defined(ERLANG_FRAME_POINTERS)
    erts_validate_stack(p, FRAME_POINTER(p), prev_stack_top);
#endif

    if (frame_layout == ERTS_FRAME_LAYOUT_RA) {
        sys_memcpy(new_stack_top, prev_stack_top, stack_size * sizeof(Eterm));
    } else {
        Eterm *new_p, *old_p;
        SWord stack_offset;

        ASSERT(frame_layout == ERTS_FRAME_LAYOUT_FP_RA);

        old_p = prev_stack_top;
        new_p = new_stack_top;

        stack_offset = new_stack_top - prev_stack_top;

        while (old_p < prev_stack_end) {
            Eterm val = old_p[0];

            if (is_CP(val)) {
                Eterm *frame_ptr = (Eterm*)cp_val(val);

                if (old_p < frame_ptr && frame_ptr < prev_stack_end) {
                    val = offset_ptr(val, stack_offset);
                }
            }

            new_p[0] = val;

            new_p++;
            old_p++;
        }

        FRAME_POINTER(p) += stack_offset;
    }

    p->stop = new_stack_top;
}

static void
do_minor(Process *p, ErlHeapFragment *live_hf_end,
	 char *mature, Uint mature_size,
	 Uint new_sz, Eterm* objv, int nobj)
{
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
    Roots* roots;
    Eterm* n_htop;
    Uint n;
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    Eterm* old_htop = OLD_HTOP(p);
    Eterm* n_heap;
    char* oh = (char *) OLD_HEAP(p);
    Uint oh_size = (char *) OLD_HTOP(p) - oh;

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] MINOR GC: %p %p %p %p\n", p->common.id,
                           HEAP_START(p), HEAP_END(p), OLD_HEAP(p), OLD_HEND(p)));

    n_htop = n_heap = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
					       sizeof(Eterm)*new_sz);

    n = setup_rootset(p, objv, nobj, &rootset);
    roots = rootset.roots;

    /*
     * All allocations done. Start defile heap with move markers.
     * A crash dump due to allocation failure above will see a healthy heap.
     */

    if (live_hf_end != ERTS_INVALID_HFRAG_PTR) {
	/*
	 * Move heap frags that we know are completely live
	 * directly into the new young heap generation.
	 */
	n_htop = collect_live_heap_frags(p, live_hf_end, n_htop);
    }

    while (n--) {
        Eterm* g_ptr = roots->v;
        Uint g_sz = roots->sz;

	roots++;
        for ( ; g_sz--; g_ptr++) {
            gval = *g_ptr;

            switch (primary_tag(gval)) {

	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
                val = *ptr;
                if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
                    *g_ptr = val;
                } else if (ErtsInArea(ptr, mature, mature_size)) {
                    move_boxed(ptr,val,&old_htop,g_ptr);
                } else if (ErtsInYoungGen(gval, ptr, oh, oh_size)) {
                    move_boxed(ptr,val,&n_htop,g_ptr);
                }
                break;
	    }

	    case TAG_PRIMARY_LIST: {
                ptr = list_val(gval);
                val = *ptr;
                if (IS_MOVED_CONS(val)) { /* Moved */
                    *g_ptr = ptr[1];
                } else if (ErtsInArea(ptr, mature, mature_size)) {
                    move_cons(ptr,val,&old_htop,g_ptr);
                } else if (ErtsInYoungGen(gval, ptr, oh, oh_size)) {
                    move_cons(ptr,val,&n_htop,g_ptr);
                }
		break;
	    }
	    default:
		break;
            }
        }
    }

    cleanup_rootset(&rootset);

    /*
     * Now all references in the rootset point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is changed.
     */

    if (mature_size == 0) {
	n_htop = sweep_new_heap(n_heap, n_htop, oh, oh_size);
    } else {
	Eterm* n_hp = n_heap;
	Eterm* ptr;
	Eterm val;
	Eterm gval;

	while (n_hp != n_htop) {
	    ASSERT(n_hp < n_htop);
	    gval = *n_hp;
	    switch (primary_tag(gval)) {
	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
		    *n_hp++ = val;
		} else if (ErtsInArea(ptr, mature, mature_size)) {
		    move_boxed(ptr,val,&old_htop,n_hp++);
		} else if (ErtsInYoungGen(gval, ptr, oh, oh_size)) {
		    move_boxed(ptr,val,&n_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (IS_MOVED_CONS(val)) {
		    *n_hp++ = ptr[1];
		} else if (ErtsInArea(ptr, mature, mature_size)) {
		    move_cons(ptr,val,&old_htop,n_hp++);
		} else if (ErtsInYoungGen(gval, ptr, oh, oh_size)) {
		    move_cons(ptr,val,&n_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_HEADER: {
                if (!header_is_thing(gval)) {
                    n_hp++;
                } else {
                    if (gval == HEADER_SUB_BITS) {
                        ErlSubBits *sb = (ErlSubBits*)n_hp;
                        Eterm *underlying = &sb->orig;

                        ptr = boxed_val(*underlying);
                        val = *ptr;

                        if (IS_MOVED_BOXED(val)) {
                            *underlying = val;
                            erl_sub_bits_update_moved(sb, val);
                        } else if (ErtsInArea(ptr, mature, mature_size)) {
                            move_boxed(ptr, val, &old_htop, underlying);
                            erl_sub_bits_update_moved(sb, sb->orig);
                        } else if (ErtsInYoungGen(*underlying, ptr, oh, oh_size)) {
                            move_boxed(ptr, val, &n_htop, underlying);
                            erl_sub_bits_update_moved(sb, sb->orig);
                        }
                    }
		    n_hp += (thing_arityval(gval)+1);
		}
		break;
	    }
	    default:
		n_hp++;
		break;
	    }
	}
    }

    /*
     * And also if we have been tenuring, references on the second generation
     * may point to the old (soon to be deleted) new_heap.
     */

    if (OLD_HTOP(p) < old_htop)
	old_htop = sweep_new_heap(OLD_HTOP(p), old_htop, oh, oh_size);
    OLD_HTOP(p) = old_htop;
    HIGH_WATER(p) = n_htop;

    if (MSO(p).first || p->wrt_bins) {
	sweep_off_heap(p, 0);
    }

#ifdef HARDDEBUG
    /*
     * Go through the old_heap before, and try to find references from the old_heap
     * into the old new_heap that has just been evacuated and is about to be freed
     * (as well as looking for reference into heap fragments, of course).
     */
    disallow_heap_frag_ref_in_old_heap(p);
#endif

    copy_erlang_stack(p, n_heap, new_sz);

#ifdef HARDDEBUG
    disallow_heap_frag_ref_in_heap(p, n_heap, n_htop);
#endif

    erts_deallocate_young_generation(p);

    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_END(p) = n_heap + new_sz;

#ifdef USE_VM_PROBES
    if (HEAP_SIZE(p) != new_sz && DTRACE_ENABLED(process_heap_grow)) {
        DTRACE_CHARBUF(pidbuf, DTRACE_TERM_BUF_SIZE);
        Uint old_sz = HEAP_SIZE(p);

        HEAP_SIZE(p) = new_sz;
        dtrace_proc_str(p, pidbuf);
        DTRACE3(process_heap_grow, pidbuf, old_sz, new_sz);
    }
#endif
    HEAP_SIZE(p) = new_sz;
}

/*
 * Major collection. DISCARD the old heap.
 */

static int
major_collection(Process* p, ErlHeapFragment *live_hf_end,
		 Uint need, Eterm* objv, int nobj,
		 Uint ygen_usage, Uint *recl)
{
    Uint size_before, size_after, stack_size;
    Eterm* n_heap;
    Eterm* n_htop;
    char* oh = (char *) OLD_HEAP(p);
    Uint oh_size = (char *) OLD_HTOP(p) - oh;
    Uint new_sz;
    int adjusted;

    VERBOSE(DEBUG_SHCOPY, ("[pid=%T] MAJOR GC: %p %p %p %p\n", p->common.id,
                           HEAP_START(p), HEAP_END(p), OLD_HEAP(p), OLD_HEND(p)));

    /*
     * Do a fullsweep GC. First figure out the size of the heap
     * to receive all live data.
     */

    size_before = ygen_usage;
    size_before += p->old_htop - p->old_heap;
    stack_size = p->hend - p->stop;

    new_sz = stack_size + size_before + S_RESERVED;
    new_sz = next_heap_size(p, new_sz, 0);

    /*
     * Should we grow although we don't actually need to?
     */

    if (new_sz == HEAP_SIZE(p) && FLAGS(p) & F_HEAP_GROW) {
        new_sz = next_heap_size(p, HEAP_SIZE(p), 1);
    }


    if (MAX_HEAP_SIZE_GET(p)) {
        Uint heap_size = size_before;

        /* Add unused space in old heap */
        heap_size += OLD_HEND(p) - OLD_HTOP(p);

        /* Add stack + unused space in young heap */
        heap_size += HEAP_END(p) - HEAP_TOP(p);

        /* Add size of new young heap */
        heap_size += new_sz;

        if (has_reached_max_heap_size(p, heap_size))
            if (reached_max_heap_size(p, heap_size, new_sz, 0))
                return -2;
    }

    FLAGS(p) &= ~(F_HEAP_GROW|F_NEED_FULLSWEEP);
    n_htop = n_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
						sizeof(Eterm)*new_sz);

    n_htop = full_sweep_heaps(p, live_hf_end, 0, n_heap, n_htop, oh, oh_size,
                              objv, nobj);

    copy_erlang_stack(p, n_heap, new_sz);

#ifdef HARDDEBUG
    disallow_heap_frag_ref_in_heap(p, n_heap, n_htop);
#endif

    erts_deallocate_young_generation(p);

    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_END(p) = n_heap + new_sz;

#ifdef USE_VM_PROBES
    /* Fire process_heap_grow tracepoint after all heap references have
     * been updated. This allows to walk the stack. */
    if (HEAP_SIZE(p) != new_sz && DTRACE_ENABLED(process_heap_grow)) {
        DTRACE_CHARBUF(pidbuf, DTRACE_TERM_BUF_SIZE);
        Uint old_sz = HEAP_SIZE(p);

        /* Update the heap size before firing tracepoint */
        HEAP_SIZE(p) = new_sz;

        dtrace_proc_str(p, pidbuf);
        DTRACE3(process_heap_grow, pidbuf, old_sz, new_sz);
    }
#endif
    HEAP_SIZE(p) = new_sz;

    GEN_GCS(p) = 0;

    HIGH_WATER(p) = HEAP_TOP(p);

    if (p->sig_qs.flags & FS_ON_HEAP_MSGQ)
	move_msgs_to_heap(p);

    ErtsGcQuickSanityCheck(p);

    size_after = HEAP_TOP(p) - HEAP_START(p) + p->mbuf_sz;
    *recl += size_before - size_after;

    adjusted = adjust_after_fullsweep(p, need, objv, nobj);

    ErtsGcQuickSanityCheck(p);

    return gc_cost(size_after, adjusted ? size_after : 0);
}

static Eterm *
full_sweep_heaps(Process *p,
                 ErlHeapFragment *live_hf_end,
		 int hibernate,
		 Eterm *n_heap, Eterm* n_htop,
		 char *oh, Uint oh_size,
		 Eterm *objv, int nobj)
{
    Rootset rootset;
    Roots *roots;
    Uint n;

    /*
     * Copy all top-level terms directly referenced by the rootset to
     * the new new_heap.
     */

    n = setup_rootset(p, objv, nobj, &rootset);

    /*
     * All allocations done. Start defile heap with move markers.
     * A crash dump due to allocation failure above will see a healthy heap.
     */

    if (live_hf_end != ERTS_INVALID_HFRAG_PTR) {
        /*
         * Move heap frags that we know are completely live
         * directly into the heap.
         */
        n_htop = collect_live_heap_frags(p, live_hf_end, n_htop);
    }

    roots = rootset.roots;
    while (n--) {
	Eterm* g_ptr = roots->v;
	Eterm g_sz = roots->sz;

	roots++;
	for ( ; g_sz--; g_ptr++) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *g_ptr;

	    switch (primary_tag(gval)) {

	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
		    *g_ptr = val;
		} else if (!erts_is_literal(gval, ptr)) {
		    move_boxed(ptr,val,&n_htop,g_ptr);
		}
                break;
	    }

	    case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (IS_MOVED_CONS(val)) {
		    *g_ptr = ptr[1];
		} else if (!erts_is_literal(gval, ptr)) {
		    move_cons(ptr,val,&n_htop,g_ptr);
		}
                break;
	    }

            default:
                break;
	    }
	}
    }

    cleanup_rootset(&rootset);

    /*
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is copied.
     */

    n_htop = sweep_heaps(n_heap, n_htop, oh, oh_size);

    if (MSO(p).first || p->wrt_bins) {
	sweep_off_heap(p, 1);
    }

    if (OLD_HEAP(p) != NULL) {       
	ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       OLD_HEAP(p),
		       (OLD_HEND(p) - OLD_HEAP(p)) * sizeof(Eterm));
	OLD_HEAP(p) = OLD_HTOP(p) = OLD_HEND(p) = NULL;
    }

    return n_htop;
}

static int
adjust_after_fullsweep(Process *p, int need, Eterm *objv, int nobj)
{
    int adjusted = 0;
    Uint wanted, sz, need_after;
    Uint stack_size = STACK_SZ_ON_HEAP(p);

    /*
     * Resize the heap if needed.
     */
    
    need_after = (HEAP_TOP(p) - HEAP_START(p)) + need + stack_size + S_RESERVED;
    if (HEAP_SIZE(p) < need_after) {
        /* Too small - grow to match requested need */
        sz = next_heap_size(p, need_after, 0);
        grow_new_heap(p, sz, objv, nobj);
        adjusted = 1;
    } else if (3 * HEAP_SIZE(p) < 4 * need_after){
        /* Need more than 75% of current, postpone to next GC.*/
        FLAGS(p) |= F_HEAP_GROW;
    } else if (4 * need_after < HEAP_SIZE(p) && HEAP_SIZE(p) > H_MIN_SIZE){
        /* We need less than 25% of the current heap, shrink.*/
        /* XXX - This is how it was done in the old GC:
           wanted = 4 * need_after;
           I think this is better as fullsweep is used mainly on
           small memory systems, but I could be wrong... */
        wanted = 2 * need_after;
	
	sz = wanted < p->min_heap_size ? p->min_heap_size
				       : next_heap_size(p, wanted, 0);

        if (sz < HEAP_SIZE(p)) {
            shrink_new_heap(p, sz, objv, nobj);
	    adjusted = 1;
        }
    }
    return adjusted;
}

void
erts_deallocate_young_generation(Process *c_p)
{
    Eterm *orig_heap;

    if (!c_p->abandoned_heap) {
	orig_heap = c_p->heap;
	ASSERT(!(c_p->flags & F_ABANDONED_HEAP_USE));
    }
    else {
	ErlHeapFragment *hfrag;

	orig_heap = c_p->abandoned_heap;
	c_p->abandoned_heap = NULL;
	c_p->flags &= ~F_ABANDONED_HEAP_USE;

	/*
	 * Temporary heap located in heap fragment
	 * only referred to by 'c_p->heap'. Add it to
	 * 'c_p->mbuf' list and deallocate it as any
	 * other heap fragment...
	 */
	hfrag = ((ErlHeapFragment *)
		 (((char *) c_p->heap)
		  - offsetof(ErlHeapFragment, mem)));

	ASSERT(!hfrag->off_heap.first);
	ASSERT(!hfrag->off_heap.overhead);
	ASSERT(!hfrag->next);
	ASSERT(c_p->htop - c_p->heap <= hfrag->alloc_size);

	hfrag->next = c_p->mbuf;
	c_p->mbuf = hfrag;
    }

    if (c_p->mbuf) {
	free_message_buffer(c_p->mbuf);
	c_p->mbuf = NULL;
    }    
    if (c_p->msg_frag) {
	erts_cleanup_messages(c_p->msg_frag);
	c_p->msg_frag = NULL;
    }
    c_p->mbuf_sz = 0;  

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   orig_heap,
		   c_p->heap_sz * sizeof(Eterm));
}

#ifdef HARDDEBUG

/*
 * Routines to verify that we don't have pointer into heap fragments from
 * that are not allowed to have them.
 *
 * For performance reasons, we use _unchecked_list_val(), _unchecked_boxed_val(),
 * and so on to avoid a function call.
 */

static void
disallow_heap_frag_ref_in_heap(Process *p, Eterm *heap, Eterm *htop)
{
    Eterm* hp;
    Uint heap_size;

    if (p->mbuf == 0) {
	return;
    }

    heap_size = (htop - heap)*sizeof(Eterm);

    hp = heap;
    while (hp < htop) {
	ErlHeapFragment* qb;
	Eterm* ptr;
	Eterm val;

	val = *hp++;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_BOXED:
	    ptr = _unchecked_boxed_val(val);
	    if (!ErtsInArea(ptr, heap, heap_size)) {
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (ErtsInArea(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
	    }
	    break;
	case TAG_PRIMARY_LIST:
	    ptr = _unchecked_list_val(val);
	    if (!ErtsInArea(ptr, heap, heap_size)) {
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (ErtsInArea(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(val)) {
		hp += _unchecked_thing_arityval(val);
	    }
	    break;
	}
    }
}

static void
disallow_heap_frag_ref_in_old_heap(Process* p)
{
    Eterm* hp;
    Eterm* htop;
    Eterm* old_heap;
    Uint old_heap_size;
    Eterm* new_heap;
    Uint new_heap_size;

    htop = p->old_htop;
    old_heap = p->old_heap;
    old_heap_size = (htop - old_heap)*sizeof(Eterm);
    new_heap = p->heap;
    new_heap_size = (p->htop - new_heap)*sizeof(Eterm);

    ASSERT(!p->last_old_htop
	   || (old_heap <= p->last_old_htop && p->last_old_htop <= htop));
    hp = p->last_old_htop ? p->last_old_htop : old_heap;
    while (hp < htop) {
	ErlHeapFragment* qb;
	Eterm* ptr;
	Eterm val;

	val = *hp++;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_BOXED:
	    ptr = (Eterm *) val;
	    if (!ErtsInArea(ptr, old_heap, old_heap_size)) {
		if (ErtsInArea(ptr, new_heap, new_heap_size)) {
		    abort();
		}
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (ErtsInArea(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
	    }
	    break;
	case TAG_PRIMARY_LIST:
	    ptr = (Eterm *) val;
	    if (!ErtsInArea(ptr, old_heap, old_heap_size)) {
		if (ErtsInArea(ptr, new_heap, new_heap_size)) {
		    abort();
		}
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (ErtsInArea(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(val)) {
		hp += _unchecked_thing_arityval(val);
		if (!ErtsInArea(hp, old_heap, old_heap_size+1)) {
		    abort();
		}
	    }
	    break;
	}
    }
}
#endif

typedef enum {
    ErtsSweepNewHeap,
    ErtsSweepHeaps,
    ErtsSweepLiteralArea
} ErtsSweepType;

static ERTS_FORCE_INLINE Eterm *
sweep(Eterm *n_hp, Eterm *n_htop,
      ErtsSweepType type,
      char *oh, Uint ohsz,
      char *src, Uint src_size)
{
    Eterm* ptr;
    Eterm val;
    Eterm gval;

#undef ERTS_IS_IN_SWEEP_AREA

#define ERTS_IS_IN_SWEEP_AREA(TPtr, Ptr)				\
    (type == ErtsSweepHeaps						\
     ? !erts_is_literal((TPtr), (Ptr))					\
     : (type == ErtsSweepNewHeap					\
	? ErtsInYoungGen((TPtr), (Ptr), oh, ohsz)			\
	: ErtsInArea((Ptr), src, src_size)))

    while (n_hp != n_htop) {
	ASSERT(n_hp < n_htop);
	gval = *n_hp;
	switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED_BOXED(val)) {
		ASSERT(is_boxed(val));
		*n_hp++ = val;
	    } else if (ERTS_IS_IN_SWEEP_AREA(gval, ptr)) {
		move_boxed(ptr,val,&n_htop,n_hp++);
	    } else {
		n_hp++;
	    }
	    break;
	}
	case TAG_PRIMARY_LIST: {
	    ptr = list_val(gval);
	    val = *ptr;
	    if (IS_MOVED_CONS(val)) {
		*n_hp++ = ptr[1];
	    } else if (ERTS_IS_IN_SWEEP_AREA(gval, ptr)) {
		move_cons(ptr,val,&n_htop,n_hp++);
	    } else {
		n_hp++;
	    }
	    break;
	}
	case TAG_PRIMARY_HEADER: {
            if (!header_is_thing(gval)) {
                n_hp++;
            } else {
                if (gval == HEADER_SUB_BITS) {
                    ErlSubBits *sb = (ErlSubBits*)n_hp;
                    Eterm *underlying = &sb->orig;

                    ptr = boxed_val(*underlying);
                    val = *ptr;

                    if (IS_MOVED_BOXED(val)) {
                        *underlying = val;
                        erl_sub_bits_update_moved(sb, *underlying);
                    } else if (ERTS_IS_IN_SWEEP_AREA(*underlying, ptr)) {
                        move_boxed(ptr, val, &n_htop, underlying);
                        erl_sub_bits_update_moved(sb, *underlying);
                    }
                }
                n_hp += (thing_arityval(gval)+1);
            }
	    break;
	}
	default:
	    n_hp++;
	    break;
	}
    }
    return n_htop;
#undef ERTS_IS_IN_SWEEP_AREA
}

static Eterm *
sweep_new_heap(Eterm *n_hp, Eterm *n_htop, char* old_heap, Uint old_heap_size)
{
    return sweep(n_hp, n_htop,
		 ErtsSweepNewHeap,
		 old_heap, old_heap_size,
		 NULL, 0);
}

static Eterm *
sweep_heaps(Eterm *n_hp, Eterm *n_htop, char* old_heap, Uint old_heap_size)
{
    return sweep(n_hp, n_htop,
		 ErtsSweepHeaps,
		 old_heap, old_heap_size,
		 NULL, 0);
}

static Eterm *
sweep_literal_area(Eterm *n_hp, Eterm *n_htop,
		   char* old_heap, Uint old_heap_size,
		   char* src, Uint src_size)
{
    return sweep(n_hp, n_htop,
		 ErtsSweepLiteralArea,
		 old_heap, old_heap_size,
		 src, src_size);
}

static Eterm*
sweep_literals_to_old_heap(Eterm* heap_ptr, Eterm* heap_end, Eterm* htop,
			   char* src, Uint src_size)
{
    while (heap_ptr < heap_end) {
	Eterm* ptr;
	Eterm val;
	Eterm gval = *heap_ptr;

	switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED_BOXED(val)) {
		ASSERT(is_boxed(val));
		*heap_ptr++ = val;
	    } else if (ErtsInArea(ptr, src, src_size)) {
		move_boxed(ptr,val,&htop,heap_ptr++);
	    } else {
		heap_ptr++;
	    }
	    break;
	}
	case TAG_PRIMARY_LIST: {
	    ptr = list_val(gval);
	    val = *ptr;
	    if (IS_MOVED_CONS(val)) {
		*heap_ptr++ = ptr[1];
	    } else if (ErtsInArea(ptr, src, src_size)) {
		move_cons(ptr,val,&htop,heap_ptr++);
	    } else {
		heap_ptr++;
	    }
	    break;
	}
	case TAG_PRIMARY_HEADER: {
            if (!header_is_thing(gval)) {
                heap_ptr++;
            } else {
                if (gval == HEADER_SUB_BITS) {
                    ErlSubBits *sb = (ErlSubBits*)heap_ptr;
                    Eterm *underlying = &sb->orig;

                    ptr = boxed_val(*underlying);
                    val = *ptr;

                    if (IS_MOVED_BOXED(val)) {
                        *underlying = val;
                        erl_sub_bits_update_moved(sb, *underlying);
                    } else if (ErtsInArea(ptr, src, src_size)) {
                        move_boxed(ptr, val, &htop, underlying);
                        erl_sub_bits_update_moved(sb, *underlying);
                    }
                }
                heap_ptr += (thing_arityval(gval)+1);
            }
	    break;
	}
	default:
	    heap_ptr++;
	    break;
	}
    }
    return htop;
}

/*
 * Move an area (heap fragment) by sweeping over it and set move markers.
 */
static Eterm*
move_one_area(Eterm* n_htop, char* src, Uint src_size)
{
    Eterm* ptr = (Eterm*) src;
    Eterm* end = ptr + src_size/sizeof(Eterm);
    Eterm dummy_ref;

    while (ptr != end) {
	Eterm val;
	ASSERT(ptr < end);
	val = *ptr;
	ASSERT(val != ERTS_HOLE_MARKER);
	if (is_header(val)) {
	    ASSERT(ptr + header_arity(val) < end);
	    ptr = move_boxed(ptr, val, &n_htop, &dummy_ref);
	}
	else { /* must be a cons cell */
	    ASSERT(ptr+1 < end);
	    move_cons(ptr, val, &n_htop, &dummy_ref);
	    ptr += 2;
	}
    }

    return n_htop;
}

/*
 * Collect heap fragments and check that they point in the correct direction.
 */

static Eterm*
collect_live_heap_frags(Process* p, ErlHeapFragment *live_hf_end, Eterm* n_htop)
{
    ErlHeapFragment* qb;
    char* frag_begin;
    Uint frag_size;

    /*
     * Move the heap fragments to the new heap. Note that no GC is done on
     * the heap fragments. Any garbage will thus be moved as well and survive
     * until next GC.  
     */ 
    qb = MBUF(p);
    while (qb != live_hf_end) {
        ASSERT(!qb->off_heap.first);  /* process fragments use the MSO(p) list */
	frag_size = qb->used_size * sizeof(Eterm);
	if (frag_size != 0) {
	    frag_begin = (char *) qb->mem;
	    n_htop = move_one_area(n_htop, frag_begin, frag_size);
	}
	qb = qb->next;
    }
    return n_htop;
}

void
erts_copy_one_frag(Eterm** hpp, ErlOffHeap* off_heap,
                   ErlHeapFragment *bp, Eterm *refs, int nrefs)
{
    Uint sz;
    int i;
    Sint offs;
    struct erl_off_heap_header* oh;
    Eterm *fhp, *hp;

    OH_OVERHEAD(off_heap, bp->off_heap.overhead);
    sz = bp->used_size;

    fhp = bp->mem;
    hp = *hpp;
    offs = hp - fhp;

    oh = NULL;
    while (sz--) {
	Uint cpy_sz;
	Eterm val = *fhp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
            if (erts_is_literal(val,list_val(val))) {
                *hp++ = val;
            } else {
                *hp++ = offset_ptr(val, offs);
            }
            break;
	case TAG_PRIMARY_BOXED:
            if (erts_is_literal(val,boxed_val(val))) {
                *hp++ = val;
            } else {
                *hp++ = offset_ptr(val, offs);
            }
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
		break;
	    case REF_SUBTAG:
		if (!is_magic_ref_thing(fhp - 1))
		    goto the_default;
                ERTS_FALLTHROUGH();
	    case BIN_REF_SUBTAG:
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		oh = (struct erl_off_heap_header*) (hp-1);
		cpy_sz = thing_arityval(val);
		goto cpy_words;
            case SUB_BITS_SUBTAG:
            {
                /* Match contexts and writable binaries should never be present
                 * in signals. */
                ASSERT(erl_sub_bits_is_normal((ErlSubBits*)(fhp - 1)));

                /* Offset the `orig` field, as it's the last field inside the
                 * thing we can handle it by pretending it's not part of it. */
                cpy_sz = thing_arityval(val) - 1;
                goto cpy_words;
            }
	    default:
	    the_default:
		cpy_sz = header_arity(val);

	    cpy_words:
		ASSERT(sz >= cpy_sz);
		sz -= cpy_sz;
                sys_memcpy(hp, fhp, cpy_sz * sizeof(Eterm));
                hp += cpy_sz;
                fhp += cpy_sz;
		if (oh) {
		    /* Add to offheap list */
		    oh->next = off_heap->first;
		    off_heap->first = oh;
		    ASSERT(*hpp <= (Eterm*)oh);
		    ASSERT(hp > (Eterm*)oh);
		    oh = NULL;
		}
		break;
	    }
	    break;
	}
    }

    ASSERT(bp->used_size == hp - *hpp);
    *hpp = hp;

    for (i = 0; i < nrefs; i++) {
	if (is_not_immed(refs[i]) && !erts_is_literal(refs[i],ptr_val(refs[i])))
	    refs[i] = offset_ptr(refs[i], offs);
    }
    bp->off_heap.first = NULL;
}

static Uint
setup_rootset(Process *p, Eterm *objv, int nobj, Rootset *rootset)
{
    /*
     * NOTE!
     *   Remember to update offset_rootset() when changing
     *   this function.
     */
    Roots* roots;
    Uint n;

    n = 0;
    roots = rootset->roots = rootset->def;
    rootset->size = ALENGTH(rootset->def);

    roots[n].v  = p->stop;
    roots[n].sz = STACK_START(p) - p->stop;
    ++n;

    if (p->dictionary != NULL) {
        roots[n].v = ERTS_PD_START(p->dictionary);
        roots[n].sz = ERTS_PD_SIZE(p->dictionary);
        ++n;
    }
    if (nobj > 0) {
        roots[n].v  = objv;
        roots[n].sz = nobj;
        ++n;
    }

    ASSERT((is_nil(p->seq_trace_token) ||
	    is_tuple(follow_moved(p->seq_trace_token, (Eterm) 0)) ||
	    is_atom(p->seq_trace_token)));
    if (is_not_immed(p->seq_trace_token)) {
	roots[n].v = &p->seq_trace_token;
	roots[n].sz = 1;
	n++;
    }
#ifdef USE_VM_PROBES
    if (is_not_immed(p->dt_utag)) {
	roots[n].v = &p->dt_utag;
	roots[n].sz = 1;
	n++;
    }
#endif

    ASSERT(is_pid(follow_moved(p->group_leader, (Eterm) 0)));
    if (is_not_immed(p->group_leader)) {
	roots[n].v  = &p->group_leader;
	roots[n].sz = 1;
	n++;
    }

    ASSERT(p->parent == am_undefined
           || is_pid(follow_moved(p->parent, (Eterm) 0)));
    if (is_not_immed(p->parent)) {
	roots[n].v  = &p->parent;
	roots[n].sz = 1;
	n++;
    }

    /*
     * The process may be garbage-collected while it is terminating.
     * fvalue contains the EXIT reason.
     */
    if (is_not_immed(p->fvalue)) {
	roots[n].v  = &p->fvalue;
	roots[n].sz = 1;
	n++;
    }

    /*
     * The raise/3 BIF will store the stacktrace in ftrace.
     */
    if (is_not_immed(p->ftrace)) {
	roots[n].v  = &p->ftrace;
	roots[n].sz = 1;
	n++;
    }

    if (p->sig_qs.recv_mrk_blk) {
	roots[n].v  = &p->sig_qs.recv_mrk_blk->ref[0];
	roots[n].sz = ERTS_RECV_MARKER_BLOCK_SIZE;
	n++;
    }

    /*
     * If a NIF or BIF has saved arguments, they need to be added
     */
    if (erts_setup_nfunc_rootset(p, &roots[n].v, &roots[n].sz))
	n++;

    ASSERT(n <= rootset->size);

    if ((p->sig_qs.flags & (FS_OFF_HEAP_MSGQ
			    | FS_OFF_HEAP_MSGQ_CHNG)) != FS_OFF_HEAP_MSGQ) {
	Uint size;
	ErtsMessage *mp;
	/*
	 * We do not have off heap message queue enabled, i.e. we
	 * need to add signal queues to rootset...
	 */

#ifdef DEBUG
        if (p->sig_qs.flags & (FS_ON_HEAP_MSGQ|FS_OFF_HEAP_MSGQ_CHNG)) {
            erts_proc_lock(p, ERTS_PROC_LOCK_MSGQ);
            /*
             * Verify that we do not have any messages in the outer
             * queue that might refer to the heap...
             */
            for (mp = p->sig_inq.first; mp; mp = mp->next) {
                if ((ERTS_SIG_IS_INTERNAL_MSG(mp) && !mp->data.attached)
                    || ERTS_SIG_IS_HEAP_ALTACT_MSG(mp)) {
                    int i = ERTS_SIG_IS_INTERNAL_MSG(mp) ? 0 : 1;
                    for (; i < ERL_MESSAGE_REF_ARRAY_SZ; i++) {
                        ASSERT(is_immed(mp->m[i])
                               || erts_is_literal(mp->m[i],
                                                  ptr_val(mp->m[i])));
                    }
                }
            }
            erts_proc_unlock(p, ERTS_PROC_LOCK_MSGQ);
        }
#endif

	size = n + erts_proc_sig_privqs_len(p, -1, -1);
	if (size > rootset->size) {
	    ERTS_GC_ASSERT(roots == rootset->def);
	    roots = erts_alloc(ERTS_ALC_T_ROOTSET,
			       size*sizeof(Roots));
	    sys_memcpy(roots, rootset->def, n*sizeof(Roots));
	    rootset->size = size;
	}

	mp = p->sig_qs.first;
	while (mp) {
	    if (ERTS_SIG_IS_INTERNAL_MSG(mp) && !mp->data.attached) {
		roots[n].v = mp->m;
		roots[n].sz = ERL_MESSAGE_REF_ARRAY_SZ;
		n++;
	    }
	    mp = mp->next;
	}
	mp = p->sig_qs.cont;
	while (mp) {
	    ASSERT(n < size);
	    if (ERTS_SIG_IS_INTERNAL_MSG(mp) && !mp->data.attached) {
		roots[n].v = mp->m;
		roots[n].sz = ERL_MESSAGE_REF_ARRAY_SZ;
		n++;
	    }
	    else if (ERTS_SIG_IS_HEAP_ALTACT_MSG(mp)) {
		/*
                 * Exclude message and token slots since they do
                 * not yet contain valid Erlang terms...
                 */
		roots[n].v = &mp->m[2];
		roots[n].sz = ERL_MESSAGE_REF_ARRAY_SZ - 2;
		n++;
	    }
	    mp = mp->next;
	}
    }

    ASSERT(rootset->size >= n);

    rootset->roots = roots;
    rootset->num_roots = n;
    return n;
}

static
void cleanup_rootset(Rootset* rootset)
{
    if (rootset->roots != rootset->def) {
        erts_free(ERTS_ALC_T_ROOTSET, rootset->roots);
    }
}

static void resize_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj)
{
    Eterm *new_stack, *prev_stack;
    Eterm *new_heap, *prev_heap;
    Sint heap_offs, stack_offs;
    Uint heap_used, stack_used;
    Uint prev_sz;

    const char *heap_area, *stack_area;
    Uint heap_area_sz, stack_area_sz;

    prev_heap = HEAP_START(p);
    prev_sz = HEAP_SIZE(p);

    stack_used = STACK_START(p) - STACK_TOP(p);
    prev_stack = &prev_heap[prev_sz - stack_used];

    if (new_sz <= prev_sz) {
        /* When shrinking, we need to move the stack prior to reallocating as
         * the upper part of the stack will disappear. */
        sys_memmove(prev_stack - (prev_sz - new_sz),
                    prev_stack, stack_used * sizeof(Eterm));
    }

    new_heap = ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP, prev_heap,
                                 prev_sz * sizeof(Eterm),
                                 new_sz * sizeof(Eterm));
    new_stack = &new_heap[new_sz - stack_used];

    if (new_sz > prev_sz) {
        /* We've grown and the previous stack has either remained in place or
         * been copied over to its _previous position_ as part of reallocation,
         * so we need to copy it to its new position at the end of the heap.
         *
         * Note that its pointers are still unchanged, so offset calculation
         * should still use `prev_stack` as set above.*/
        sys_memmove(new_stack,
                    &new_heap[prev_sz - stack_used],
                    stack_used * sizeof(Eterm));
    }

    heap_used = HEAP_TOP(p) - HEAP_START(p);

    heap_offs = new_heap - prev_heap;
    stack_offs = new_stack - prev_stack;

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
        FRAME_POINTER(p) += stack_offs;
    }

    HEAP_TOP(p) = &new_heap[heap_used];
    HEAP_START(p) = new_heap;

    STACK_START(p) = &new_heap[new_sz];
    STACK_TOP(p) = new_stack;

    HIGH_WATER(p) = &new_heap[HIGH_WATER(p) - prev_heap];
    HEAP_END(p) = &new_heap[new_sz];

    HEAP_SIZE(p) = new_sz;

    heap_area = (char *) prev_heap;
    heap_area_sz = prev_sz * sizeof(Eterm);

    stack_area = (char *) prev_stack;
    stack_area_sz = stack_used * sizeof(Eterm);

    if (new_heap == prev_heap) {
        offset_stack(new_stack, stack_used,
                     heap_offs, heap_area, heap_area_sz,
                     stack_offs, stack_area, stack_area_sz);
    } else {
        offset_heap(new_heap, heap_used, heap_offs, heap_area, heap_area_sz);
        offset_rootset(p,
                       heap_offs, heap_area, heap_area_sz,
                       stack_offs, stack_area, stack_area_sz,
                       objv, nobj);
    }
}

static void
grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj)
{
#ifdef USE_VM_PROBES
    Uint prev_sz = HEAP_SIZE(p);
#endif

    ASSERT(HEAP_SIZE(p) < new_sz);
    resize_new_heap(p, new_sz, objv, nobj);

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_heap_grow)) {
	DTRACE_CHARBUF(pidbuf, DTRACE_TERM_BUF_SIZE);

        dtrace_proc_str(p, pidbuf);
	DTRACE3(process_heap_grow, pidbuf, prev_sz, new_sz);
    }
#endif
}

static void
shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj)
{
#ifdef USE_VM_PROBES
    Uint prev_sz = HEAP_SIZE(p);
#endif

    ASSERT(HEAP_SIZE(p) > new_sz);
    resize_new_heap(p, new_sz, objv, nobj);

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_heap_shrink)) {
	DTRACE_CHARBUF(pidbuf, DTRACE_TERM_BUF_SIZE);

        dtrace_proc_str(p, pidbuf);
	DTRACE3(process_heap_shrink, pidbuf, prev_sz, new_sz);
    }
#endif
}

static Uint64
do_next_vheap_size(Uint64 vheap, Uint64 vheap_sz) {

    /*                grow
     *
     * vheap_sz ======================
     *
     * vheap 75% +    grow
     *          ----------------------
     *
     * vheap 25 - 75% same
     *          ----------------------
     *
     * vheap ~ - 25% shrink
     *
     *          ----------------------
     */

    if ((Uint64) vheap/3 > (Uint64) (vheap_sz/4)) {
	Uint64 new_vheap_sz = vheap_sz;

	while((Uint64) vheap/3 > (Uint64) (vheap_sz/4)) {
	    /* the golden ratio = 1.618 */
	    new_vheap_sz = (Uint64) vheap_sz * 1.618;
	    if (new_vheap_sz < vheap_sz ) {
	        return vheap_sz;
	    }
	    vheap_sz = new_vheap_sz;
	}

	return vheap_sz;
    }

    if (vheap < (Uint64) (vheap_sz/4)) {
	return (vheap_sz >> 1);
    }

    return vheap_sz;

}

static Uint64
next_vheap_size(Process* p, Uint64 vheap, Uint64 vheap_sz) {
    Uint64 new_vheap_sz = do_next_vheap_size(vheap, vheap_sz);
    return new_vheap_sz < p->min_vheap_size ? p->min_vheap_size : new_vheap_sz;
}

static ERTS_INLINE void
shrink_writable_binary(BinRef *br, Uint leave_unused)
{
    Binary *binary;
    Uint new_size;

    binary = br->val;

    new_size = binary->intern.apparent_size;
    ASSERT(binary->orig_size >= new_size);

    new_size += (new_size * leave_unused) / 100;

    /* Our allocators are 8 byte aligned, i.e. shrinking with less than 8 bytes
     * will have no real effect */
    if ((new_size + 8) < binary->orig_size) {
        ASSERT(erts_refc_read(&binary->intern.refc, 1) == 1);
        binary = erts_bin_realloc(binary, new_size);

        br->val = binary;
    }
}

#ifdef ERTS_MAGIC_REF_THING_HEADER
/*
 * ERTS_MAGIC_REF_THING_HEADER only defined when there
 * is a size difference between magic and ordinary references...
 */
# define ERTS_USED_MAGIC_REF_THING_HEADER__ ERTS_MAGIC_REF_THING_HEADER
#else
# define ERTS_USED_MAGIC_REF_THING_HEADER__ ERTS_REF_THING_HEADER
#endif


static void
sweep_off_heap(Process *p, int fullsweep)
{
    struct erl_off_heap_header* ptr;
    struct erl_off_heap_header** prev;
    struct erl_off_heap_header** insert_old_here;
    char* oheap = NULL;
    Uint oheap_sz = 0;
    Uint64 bin_vheap = 0;
#ifdef DEBUG
    Uint64 orig_bin_old_vheap = p->bin_old_vheap;
    int seen_mature = 0;
#endif
    Uint shrink_ncandidates;
    Uint shrink_nactive;
    BinRef *shrink_unresolved_end;
    BinRef *br;

    if (fullsweep == 0) {
	oheap = (char *) OLD_HEAP(p);
	oheap_sz = (char *) OLD_HEND(p) - oheap;
    }

    prev = &MSO(p).first;
    ptr = MSO(p).first;

    /* First part of the list will reside on the (old) new-heap.
     * Keep if moved, otherwise deref.
     */
    while (ptr) {
	if (IS_MOVED_BOXED(ptr->thing_word)) {
	    ASSERT(!ErtsInArea(ptr, oheap, oheap_sz));
            if (is_external_header(((struct erl_off_heap_header*) boxed_val(ptr->thing_word))->thing_word)) {
                erts_node_bookkeep(((ExternalThing*)ptr)->node,
                                   make_boxed(&ptr->thing_word),
                                   ERL_NODE_DEC, __FILE__, __LINE__);
            }
            *prev = ptr = (struct erl_off_heap_header*) boxed_val(ptr->thing_word);
	    ASSERT(!IS_MOVED_BOXED(ptr->thing_word));
	    switch (ptr->thing_word) {
	    case HEADER_BIN_REF: {
                Binary *refc_binary;
                int to_new_heap;
                Uint overhead;

                br = (BinRef*)ptr;
                refc_binary = br->val;

                ASSERT(!(refc_binary->intern.flags &
                         (BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER)));

                overhead = refc_binary->orig_size;
                to_new_heap = !ErtsInArea(ptr, oheap, oheap_sz);
                ASSERT(to_new_heap == !seen_mature || (!to_new_heap && (seen_mature=1)));

                if (to_new_heap) {
                    bin_vheap += overhead / sizeof(Eterm);
                } else {
                    p->bin_old_vheap += overhead / sizeof(Eterm);
                }
                break;
            }
            case ERTS_USED_MAGIC_REF_THING_HEADER__: {
                Uint size;
		int to_new_heap = !ErtsInArea(ptr, oheap, oheap_sz);
                ASSERT(is_magic_ref_thing(ptr));
		ASSERT(to_new_heap == !seen_mature || (!to_new_heap && (seen_mature=1)));
                size = (Uint) ((ErtsMRefThing *) ptr)->mb->orig_size;
		if (to_new_heap)
		    bin_vheap += size / sizeof(Eterm);
                else
		    p->bin_old_vheap += size / sizeof(Eterm); /* for binary gc (words)*/
                ERTS_FALLTHROUGH();
            }
            default:
                if (is_external_header(ptr->thing_word)) {
                    erts_node_bookkeep(((ExternalThing*)ptr)->node,
                                       make_boxed(&ptr->thing_word),
                                       ERL_NODE_INC, __FILE__, __LINE__);
                }
	    }
            prev = &ptr->next;
            ptr = ptr->next;
	}
	else if (ErtsInArea(ptr, oheap, oheap_sz)) {
            /*
             * The rest of the list resides on the old heap and needs no
             * attention during a minor gc.
             */
            ASSERT(!fullsweep);
            break;
        }
        else {
	    /* garbage */
	    switch (thing_subtag(ptr->thing_word)) {
	    case BIN_REF_SUBTAG:
		{
                    erts_bin_release(((BinRef*)ptr)->val);
		    break;
		}
	    case REF_SUBTAG:
		{
		    ErtsMagicBinary *bptr;
		    ASSERT(is_magic_ref_thing(ptr));
		    bptr = ((ErtsMRefThing *) ptr)->mb;
                    erts_bin_release((Binary *) bptr);
		    break;
		}
	    default:
		ASSERT(is_external_header(ptr->thing_word));
		erts_deref_node_entry(((ExternalThing*)ptr)->node,
                                      make_boxed(&ptr->thing_word));
	    }
	    *prev = ptr = ptr->next;
	}
    }

    insert_old_here = prev;

#ifdef DEBUG
    if (fullsweep) {
        ASSERT(ptr == NULL);
        ASSERT(p->bin_old_vheap == orig_bin_old_vheap);
    }
    else {
        /* The rest of the list resides on the old heap and needs no
         * attention during a minor gc. In a DEBUG build, verify
         * that the binaries in the list are not writable and that
         * the other terms are of the allowed types.
         */
        while (ptr) {
            ASSERT(ErtsInArea(ptr, oheap, oheap_sz));
            ASSERT(!IS_MOVED_BOXED(ptr->thing_word));
            switch (ptr->thing_word) {
            case HEADER_BIN_REF:
                {
                    const BinRef *br = (BinRef*)ptr;
                    ASSERT(!((br->val)->intern.flags &
                             (BIN_FLAG_WRITABLE | BIN_FLAG_ACTIVE_WRITER)));
                    break;
                }
            default:
                ASSERT(is_external_header(ptr->thing_word) ||
                       is_magic_ref_thing(ptr));
                break;
            }
            ptr = ptr->next;
        }
    }
#endif /* DEBUG */

    /*
     * Traverse writable binaries.
     * As writable binaries may reside on the old heap we traverse
     * the entire wrt_bins list even during minor gc.
     */
    shrink_nactive = 0;             /* number of active writable binaries */
    shrink_ncandidates = 0;         /* number of candidates for shrinking */
    shrink_unresolved_end = NULL;   /* end marker for second traversal */

    br = (BinRef*)p->wrt_bins;
    prev = &p->wrt_bins;

    while (br) {
        Binary *refc_binary;
        int on_old_heap;

        refc_binary = br->val;

        if (IS_MOVED_BOXED(br->thing_word)) {
            Uint overhead;

            ASSERT(!ErtsInArea(br, oheap, oheap_sz));

            br = (BinRef*)boxed_val(br->thing_word);
            overhead = (br->val)->orig_size;

            *prev = (struct erl_off_heap_header*) br;
            ASSERT(br->thing_word == HEADER_BIN_REF);

            on_old_heap = ErtsInArea(br, oheap, oheap_sz);
            if (!on_old_heap) {
                bin_vheap += overhead / sizeof(Eterm);
            } else {
                p->bin_old_vheap += overhead / sizeof(Eterm);
            }
        }
        else {
            ASSERT(br->thing_word == HEADER_BIN_REF);
            on_old_heap = ErtsInArea(br, oheap, oheap_sz);
            if (!on_old_heap) {
                /* garbage */
                erts_bin_release(br->val);
                br = (BinRef*) br->next;
                *prev = (struct erl_off_heap_header*) br;
                continue;
            }
        }

        if (refc_binary->intern.flags) {
            ASSERT(refc_binary->intern.flags & BIN_FLAG_WRITABLE);

            /*
             * How to shrink writable binaries. There are two distinct cases:
             *
             * + There are one or more active writers. We will shrink all
             *   writable binaries without active writers down to their
             *   original sizes.
             *
             * + There are no active writers. We will shrink all writable
             *   binaries, but not fully. How much margin we will leave
             *   depends on the number of writable binaries.
             *
             * That is, we don't know how to shrink the binaries before either
             * + finding the first active writer, or
             * + finding more than ERTS_INACT_WR_PB_LEAVE_LIMIT
             *   shrink candidates
             */

            if (refc_binary->intern.flags & BIN_FLAG_ACTIVE_WRITER) {
                refc_binary->intern.flags &= ~BIN_FLAG_ACTIVE_WRITER;
                shrink_nactive++;
                if (!shrink_unresolved_end)
                    shrink_unresolved_end = br;
            }
            else { /* inactive */
                /* Our allocators are 8 byte aligned, i.e., shrinking with
                   less than 8 bytes will have no real effect */
                Uint used_size = refc_binary->intern.apparent_size;
                if (refc_binary->orig_size - used_size >= 8) {
                    /* A shrink candidate */
                    if (shrink_unresolved_end) {
                        shrink_writable_binary(br, 0);
                    } else {
                        if (shrink_ncandidates >= ERTS_INACT_WR_PB_LEAVE_LIMIT) {
                            shrink_unresolved_end = br;
                            shrink_writable_binary(br, 0);
                        }

                        /* else unresolved, handle in second traversal below */
                        shrink_ncandidates++;
                    }
                }
            }

            prev = &br->next;
            br = (BinRef*) br->next;
        }
        else {   /* emasculated, move to regular off-heap list */
            struct erl_off_heap_header* next = br->next;
            if (on_old_heap) {
                br->next = *insert_old_here;
                *insert_old_here = (struct erl_off_heap_header*)br;
            }
            else {
                br->next = p->off_heap.first;
                p->off_heap.first = (struct erl_off_heap_header*)br;
                if (insert_old_here == &p->off_heap.first)
                    insert_old_here = &br->next;
            }
            br = (BinRef*) next;
            *prev = next;
        }
    }

    /*
     * Handle any unresolved shrink candidates left at the head of wrt_bins.
     */
    if (shrink_ncandidates) {
	Uint leave_unused = 0;

        if (shrink_nactive == 0) {
            if (shrink_ncandidates <= ERTS_INACT_WR_PB_LEAVE_MUCH_LIMIT) {
                leave_unused = ERTS_INACT_WR_PB_LEAVE_MUCH_PERCENTAGE;
            } else if (shrink_ncandidates <= ERTS_INACT_WR_PB_LEAVE_LIMIT) {
                leave_unused = ERTS_INACT_WR_PB_LEAVE_PERCENTAGE;
            }
        }

        for (br = (BinRef*)p->wrt_bins;
             br != shrink_unresolved_end;
             br = (BinRef*)br->next) {
            ASSERT(br && (br->val)->intern.flags == BIN_FLAG_WRITABLE);
            shrink_writable_binary(br, leave_unused);
        }
    }

    if (fullsweep) {
        ASSERT(p->bin_old_vheap == orig_bin_old_vheap);
        p->bin_old_vheap = 0;
        p->bin_old_vheap_sz = next_vheap_size(p, MSO(p).overhead,
                                              p->bin_old_vheap_sz);
    }
    p->bin_vheap_sz     = next_vheap_size(p, bin_vheap, p->bin_vheap_sz);
    MSO(p).overhead     = bin_vheap;
}

/*
 * Offset pointers into the heap (not stack).
 */

static void 
offset_heap(Eterm* hp, Uint sz, Sint offs, const char* area, Uint area_size)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	  case TAG_PRIMARY_LIST:
	  case TAG_PRIMARY_BOXED:
	      if (ErtsInArea(ptr_val(val), area, area_size)) {
		  *hp = offset_ptr(val, offs);
	      }
	      hp++;
	      break;
	  case TAG_PRIMARY_HEADER: {
	      Uint tari;

	      if (header_is_transparent(val)) {
		  hp++;
		  continue;
	      }
	      tari = thing_arityval(val);
	      switch (thing_subtag(val)) {
	      case REF_SUBTAG:
		  if (!is_magic_ref_thing(hp))
		      break;
                  ERTS_FALLTHROUGH();
	      case BIN_REF_SUBTAG:
	      case EXTERNAL_PID_SUBTAG:
	      case EXTERNAL_PORT_SUBTAG:
	      case EXTERNAL_REF_SUBTAG:
		  {
		      struct erl_off_heap_header* oh = (struct erl_off_heap_header*) hp;

                      if (is_external_header(oh->thing_word)) {
                          erts_node_bookkeep(((ExternalThing*)oh)->node,
                                             make_boxed(((Eterm*)oh)-offs),
                                             ERL_NODE_DEC, __FILE__, __LINE__);
                          erts_node_bookkeep(((ExternalThing*)oh)->node,
                                             make_boxed((Eterm*)oh), ERL_NODE_INC,
                                             __FILE__, __LINE__);
                      }

		      if (ErtsInArea(oh->next, area, area_size)) {
			  Eterm** uptr = (Eterm **) (void *) &oh->next;
			  *uptr += offs; /* Patch the mso chain */
		      }
		  }
		  break;
            case SUB_BITS_SUBTAG:
                {
                    ErlSubBits *sb = (ErlSubBits*) hp;

                    if (ErtsInArea(ptr_val(sb->orig), area, area_size)) {
                        sb->orig = offset_ptr(sb->orig, offs);
                        erl_sub_bits_update_moved(sb, sb->orig);
                    }
                }
                break;
	      }
	      sz -= tari;
	      hp += tari + 1;
	      break;
	  }
	  default:
	      hp++;
	      continue;
	}
    }
}

/* Offset on-stack pointers to stack and heap. */
static void
offset_stack(Eterm *stack, Uint sz,
             Sint heap_offset, const char *heap_area, Uint heap_area_sz,
             Sint stack_offset, const char *stack_area, Uint stack_area_sz) {
    if (erts_frame_layout == ERTS_FRAME_LAYOUT_RA || stack_offset == 0) {
        /* No need to update self-references, just update pointers to the
         * heap. */
        offset_heap_ptr(stack, sz, heap_offset, heap_area, heap_area_sz);
    } else {
        Sint i = 0;

        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);

        while (i < sz) {
            Eterm val = stack[i];

            switch (primary_tag(val)) {
            case TAG_PRIMARY_HEADER:
                if (ErtsInArea(val, stack_area, stack_area_sz)) {
                    stack[i] = offset_ptr(val, stack_offset);
                }

                i++;
                break;
            case TAG_PRIMARY_LIST:
            case TAG_PRIMARY_BOXED:
                if (ErtsInArea(ptr_val(val), heap_area, heap_area_sz)) {
                    stack[i] = offset_ptr(val, heap_offset);
                }

                i++;
                break;
            default:
                i++;
                break;
            }
        }
    }
}

/* Offset pointers to heap from a root set. */
static void
offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, const char* area, Uint area_sz)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    if (ErtsInArea(ptr_val(val), area, area_sz)) {
		*hp = offset_ptr(val, offs);
	    }
	    hp++;
	    break;
	default:
	    hp++;
	    break;
	}
    }
}

static void
offset_off_heap(Process* p, Sint offs, const char *area, Uint area_size)
{
    if (MSO(p).first && ErtsInArea((Eterm *)MSO(p).first, area, area_size)) {
        Eterm** uptr = (Eterm**) (void *) &MSO(p).first;
        *uptr += offs;
    }
    if (p->wrt_bins && ErtsInArea(p->wrt_bins, area, area_size)) {
        Eterm** uptr = (Eterm**) (void *) &p->wrt_bins;
        *uptr += offs;
    }
}

#ifndef USE_VM_PROBES
#define ERTS_OFFSET_DT_UTAG(MP, A, ASZ, O)
#else
#define ERTS_OFFSET_DT_UTAG(MP, A, ASZ, O)                                      \
    do {                                                                        \
        Eterm utag__ = ERL_MESSAGE_DT_UTAG((MP));                               \
        if (is_boxed(utag__) && ErtsInArea(ptr_val(utag__), (A), (ASZ))) {      \
            ERL_MESSAGE_DT_UTAG((MP)) = offset_ptr(utag__, (O));                \
        }                                                                       \
    } while (0)
#endif

static ERTS_INLINE void
offset_message(ErtsMessage *mp, Sint offs, const char *area, Uint area_size)
{
    Eterm mesg = ERL_MESSAGE_TERM(mp);
    if (ERTS_SIG_IS_MSG_TAG(mesg) || ERTS_SIG_IS_HEAP_ALTACT_MSG_TAG(mesg)) {
        if (ERTS_SIG_IS_INTERNAL_MSG_TAG(mesg)) {
            switch (primary_tag(mesg)) {
            case TAG_PRIMARY_LIST:
            case TAG_PRIMARY_BOXED:
                if (ErtsInArea(ptr_val(mesg), area, area_size)) {
                    ERL_MESSAGE_TERM(mp) = offset_ptr(mesg, offs);
                }
                break;
            }
            mesg = ERL_MESSAGE_TOKEN(mp);
            if (is_boxed(mesg) && ErtsInArea(ptr_val(mesg), area, area_size)) {
                ERL_MESSAGE_TOKEN(mp) = offset_ptr(mesg, offs);
            }
            ASSERT((is_nil(ERL_MESSAGE_TOKEN(mp)) ||
                    is_tuple(ERL_MESSAGE_TOKEN(mp)) ||
                    is_atom(ERL_MESSAGE_TOKEN(mp))));
        }
        /*
         * In the altact message case, both reference to actual message and
         * reference to a potential token are contained in the 'from'
         * entry...
         */
        mesg = ERL_MESSAGE_FROM(mp);
        if (is_boxed(mesg) && ErtsInArea(ptr_val(mesg), area, area_size)) {
            ERL_MESSAGE_FROM(mp) = offset_ptr(mesg, offs);
        }

        ERTS_OFFSET_DT_UTAG(mp, area, area_size, offs);
    }
}

/*
 * Offset pointers in message queue.
 */
static void
offset_mqueue(Process *p, Sint offs, const char *area, Uint area_size)
{
    if ((p->sig_qs.flags & (FS_OFF_HEAP_MSGQ|FS_OFF_HEAP_MSGQ_CHNG)) != FS_OFF_HEAP_MSGQ)
        ERTS_FOREACH_SIG_PRIVQS(p, mp, offset_message(mp, offs, area, area_size));
}

static void
offset_rootset(Process *p,
               Sint heap_offs, const char *heap_area, Uint heap_area_sz,
               Sint stack_offs, const char *stack_area, Uint stack_area_sz,
               Eterm* objv, int nobj)
{
    Eterm *v;
    Uint sz;
    if (p->dictionary)  {
        offset_heap(ERTS_PD_START(p->dictionary),
                    ERTS_PD_SIZE(p->dictionary),
                    heap_offs, heap_area, heap_area_sz);
    }

    offset_heap_ptr(&p->fvalue, 1, heap_offs, heap_area, heap_area_sz);
    offset_heap_ptr(&p->ftrace, 1, heap_offs, heap_area, heap_area_sz);
    offset_heap_ptr(&p->seq_trace_token, 1, heap_offs, heap_area, heap_area_sz);
#ifdef USE_VM_PROBES
    offset_heap_ptr(&p->dt_utag, 1, heap_offs, heap_area, heap_area_sz);
#endif
    offset_heap_ptr(&p->group_leader, 1, heap_offs, heap_area, heap_area_sz);
    offset_heap_ptr(&p->parent, 1, heap_offs, heap_area, heap_area_sz);
    if (p->sig_qs.recv_mrk_blk) {
        offset_heap_ptr(&p->sig_qs.recv_mrk_blk->ref[0],
                        ERTS_RECV_MARKER_BLOCK_SIZE, heap_offs,
                        heap_area, heap_area_sz);
    }
    offset_mqueue(p, heap_offs, heap_area, heap_area_sz);
    offset_stack(p->stop, (STACK_START(p) - p->stop),
                 heap_offs, heap_area, heap_area_sz,
                 stack_offs, stack_area, stack_area_sz);
    if (nobj > 0) {
        offset_heap_ptr(objv, nobj, heap_offs, heap_area, heap_area_sz);
    }
    offset_off_heap(p, heap_offs, heap_area, heap_area_sz);
    if (erts_setup_nfunc_rootset(p, &v, &sz)) {
        offset_heap_ptr(v, sz, heap_offs, heap_area, heap_area_sz);
    }
}

static void
init_gc_info(ErtsGCInfo *gcip)
{
  gcip->reclaimed = 0;
  gcip->garbage_cols = 0;
}

static void
reply_gc_info(void *vgcirp)
{
    Uint64 reclaimed = 0, garbage_cols = 0;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsGCInfoReq *gcirp = (ErtsGCInfoReq *) vgcirp;
    ErtsProcLocks rp_locks = (gcirp->req_sched == esdp->no
			      ? ERTS_PROC_LOCK_MAIN
			      : 0);
    Process *rp = gcirp->proc;
    Eterm ref_copy = NIL, msg;
    Eterm *hp = NULL;
    Eterm **hpp;
    Uint sz, *szp;
    ErlOffHeap *ohp = NULL;
    ErtsMessage *mp = NULL;

    ASSERT(esdp);

    reclaimed = esdp->gc_info.reclaimed;
    garbage_cols = esdp->gc_info.garbage_cols;
    /*
     * Add dirty schedulers info on requesting
     * schedulers info
     */
    if (gcirp->req_sched == esdp->no) {
	erts_mtx_lock(&dirty_gc.mtx);
	reclaimed += dirty_gc.info.reclaimed;
	garbage_cols += dirty_gc.info.garbage_cols;
	erts_mtx_unlock(&dirty_gc.mtx);
    }

    sz = 0;
    hpp = NULL;
    szp = &sz;

    while (1) {
	if (hpp)
	    ref_copy = STORE_NC(hpp, ohp, gcirp->ref);
	else
	    *szp += ERTS_REF_THING_SIZE;

	msg = erts_bld_tuple(hpp, szp, 3,
			     make_small(esdp->no),
			     erts_bld_uint64(hpp, szp, garbage_cols),
			     erts_bld_uint64(hpp, szp, reclaimed));
	
	msg = erts_bld_tuple(hpp, szp, 2, ref_copy, msg);
	if (hpp)
	  break;
	
	mp = erts_alloc_message_heap(rp, &rp_locks, sz, &hp, &ohp);

	szp = NULL;
	hpp = &hp;
    }

    erts_queue_message(rp, rp_locks, mp, msg, am_system);

    if (gcirp->req_sched == esdp->no)
	rp_locks &= ~ERTS_PROC_LOCK_MAIN;
 
    if (rp_locks)
	erts_proc_unlock(rp, rp_locks);

    erts_proc_dec_refc(rp);

    if (erts_atomic32_dec_read_nob(&gcirp->refc) == 0)
	gcireq_free(vgcirp);
}

Eterm
erts_gc_info_request(Process *c_p)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    Eterm ref;
    ErtsGCInfoReq *gcirp;
    Eterm *hp;

    gcirp = gcireq_alloc();
    ref = erts_make_ref(c_p);
    hp = &gcirp->ref_heap[0];

    gcirp->proc = c_p;
    gcirp->ref = STORE_NC(&hp, NULL, ref);
    gcirp->req_sched = esdp->no;
    erts_atomic32_init_nob(&gcirp->refc,
			       (erts_aint32_t) erts_no_schedulers);

    erts_proc_add_refc(c_p, (Sint) erts_no_schedulers);

    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
                                          1,
					  erts_no_schedulers,
					  reply_gc_info,
					  (void *) gcirp);

    reply_gc_info((void *) gcirp);

    return ref;
}

Eterm
erts_process_gc_info(Process *p, Uint *sizep, Eterm **hpp,
                     Uint extra_heap_block,
                     Uint extra_old_heap_block_size)
{
    ERTS_DECL_AM(bin_vheap_size);
    ERTS_DECL_AM(bin_vheap_block_size);
    ERTS_DECL_AM(bin_old_vheap_size);
    ERTS_DECL_AM(bin_old_vheap_block_size);
    Eterm tags[] = {
        /* If you increase the number of elements here, make sure to update
           any call sites as they may have stack allocations that depend
           on the number of elements here. */
        am_old_heap_block_size,
        am_heap_block_size,
        am_mbuf_size,
        am_recent_size,
        am_stack_size,
        am_old_heap_size,
        am_heap_size,
        AM_bin_vheap_size,
        AM_bin_vheap_block_size,
        AM_bin_old_vheap_size,
        AM_bin_old_vheap_block_size
    };
    UWord values[] = {
        OLD_HEAP(p) ? OLD_HEND(p) - OLD_HEAP(p) + extra_old_heap_block_size
                    : extra_old_heap_block_size,
        HEAP_SIZE(p) + extra_heap_block,
        MBUF_SIZE(p),
        HIGH_WATER(p) - HEAP_START(p),
        STACK_START(p) - p->stop,
        OLD_HEAP(p) ? OLD_HTOP(p) - OLD_HEAP(p) : 0,
        HEAP_TOP(p) - HEAP_START(p),
        MSO(p).overhead,
        p->bin_vheap_sz,
        p->bin_old_vheap,
        p->bin_old_vheap_sz
    };

    Eterm res = THE_NON_VALUE;

    ERTS_CT_ASSERT(sizeof(values)/sizeof(*values) == sizeof(tags)/sizeof(*tags));
    ERTS_CT_ASSERT(sizeof(values)/sizeof(*values) == ERTS_PROCESS_GC_INFO_MAX_TERMS);

    if (p->abandoned_heap) {
        Eterm *htop, *heap, *high_water;

        heap = get_orig_heap(p, &htop, &high_water);

        values[3] = high_water - heap;
        values[6] = htop - heap;
    }

    if (p->sig_qs.flags & FS_ON_HEAP_MSGQ) {
        /* If on heap messages in the internal queue are counted
           as being part of the heap, so we have to add them to the
           am_mbuf_size value. process_info(total_heap_size) should
           be the same as adding old_heap_block_size + heap_block_size
           + mbuf_size.
        */
        ERTS_FOREACH_SIG_PRIVQS(
            p, mp,
            {
                if (ERTS_SIG_IS_MSG(mp)
                    && mp->data.attached
                    && mp->data.attached != ERTS_MSG_COMBINED_HFRAG) {
                    values[2] += erts_msg_attached_data_size(mp);
                }
            });
    }

    res = erts_bld_atom_uword_2tup_list(hpp,
                                        sizep,
                                        sizeof(values)/sizeof(*values),
                                        tags,
                                        values);

    return res;
}

static int has_reached_max_heap_size(Process *p, Uint total_heap_size)
{
    Uint used = total_heap_size;

    if (MAX_HEAP_SIZE_FLAGS_GET(p) & MAX_HEAP_SIZE_INCLUDE_OH_BINS) {
        used += p->bin_old_vheap + p->off_heap.overhead;
    }
    return (used > MAX_HEAP_SIZE_GET(p));
}

static int
reached_max_heap_size(Process *p, Uint total_heap_size,
                      Uint extra_heap_size, Uint extra_old_heap_size)
{
    Uint max_heap_flags = MAX_HEAP_SIZE_FLAGS_GET(p);
    if (ERTS_IS_P_TRACED_FL(p, F_TRACE_GC) ||
        max_heap_flags & MAX_HEAP_SIZE_LOG) {
        Eterm msg;
        Uint size = 0;
        Eterm *o_hp , *hp;
        erts_process_gc_info(p, &size, NULL, extra_heap_size,
                             extra_old_heap_size);
        o_hp = hp = erts_alloc(ERTS_ALC_T_TMP, size * sizeof(Eterm));
        msg = erts_process_gc_info(p, NULL, &hp, extra_heap_size,
                                   extra_old_heap_size);

        if (max_heap_flags & MAX_HEAP_SIZE_LOG) {
            int alive = erts_is_alive;
            erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
            Eterm *hp, args = NIL, stacktrace;
            ErtsHeapFactory hfact;

            /* Build the format message */
            erts_dsprintf(dsbufp, "     Process:            ~p ");
            if (alive)
                erts_dsprintf(dsbufp, "on node ~p");
            erts_dsprintf(dsbufp, "~n     Context:            maximum heap size reached~n");
            erts_dsprintf(dsbufp, "     Max Heap Size:      ~p~n");
            erts_dsprintf(dsbufp, "     Total Heap Size:    ~p~n");
            erts_dsprintf(dsbufp, "     Kill:               ~p~n");
            erts_dsprintf(dsbufp, "     Error Logger:       ~p~n");
            erts_dsprintf(dsbufp, "     Message Queue Len:  ~p~n");
            erts_dsprintf(dsbufp, "     GC Info:            ~p~n");
            erts_dsprintf(dsbufp, "     Stacktrace:         ~p~n");

            /* Build the args in reverse order */
            erts_factory_tmp_init(&hfact, NULL, 0, ERTS_ALC_T_TMP);
            stacktrace = erts_build_stacktrace(&hfact, p, 0,
                                               erts_backtrace_depth, 1);
            hp = erts_produce_heap(&hfact, 2*(alive ? 9 : 8), 0);
            args = CONS(hp, stacktrace, args); hp += 2;
            args = CONS(hp, msg, args); hp += 2;
            args = CONS(hp, make_small((p)->sig_qs.mq_len), args); hp += 2;
            args = CONS(hp, am_true, args); hp += 2;
            args = CONS(hp, (max_heap_flags & MAX_HEAP_SIZE_KILL ? am_true : am_false), args); hp += 2;
            args = CONS(hp, make_small(total_heap_size), args); hp += 2;
            args = CONS(hp, make_small(MAX_HEAP_SIZE_GET(p)), args); hp += 2;
            if (alive) {
                args = CONS(hp, erts_this_node->sysname, args); hp += 2;
            }
            args = CONS(hp, p->common.id, args); hp += 2;
            ASSERT(hp == hfact.hp);

            erts_send_error_term_to_logger(p->group_leader, dsbufp, args);
            erts_factory_close(&hfact);
        }

        if (ERTS_IS_P_TRACED_FL(p, F_TRACE_GC))
            trace_gc(p, am_gc_max_heap_size, 0, msg);

        erts_free(ERTS_ALC_T_TMP, o_hp);
    }
    /* returns true if we should kill the process */
    return max_heap_flags & MAX_HEAP_SIZE_KILL;
}

Eterm
erts_max_heap_size_map(ErtsHeapFactory *factory,
                       Sint max_heap_size, Uint max_heap_flags)
{
    Eterm keys[] = {
        am_error_logger, am_include_shared_binaries, am_kill, am_size
    };
    Eterm values[] = {
        max_heap_flags & MAX_HEAP_SIZE_LOG ? am_true : am_false,
        max_heap_flags & MAX_HEAP_SIZE_INCLUDE_OH_BINS ? am_true : am_false,
        max_heap_flags & MAX_HEAP_SIZE_KILL ? am_true : am_false,
        make_small(max_heap_size)
    };
    ERTS_CT_ASSERT(sizeof(keys) == sizeof(values));
    return erts_map_from_ks_and_vs(factory, keys, values,
                                   sizeof(keys) / sizeof(keys[0]));
}

int
erts_max_heap_size(Eterm arg, Uint *max_heap_size, Uint *max_heap_flags)
{
    Sint sz;
    *max_heap_flags = H_MAX_FLAGS;
    if (is_small(arg)) {
        sz = signed_val(arg);
        *max_heap_flags = H_MAX_FLAGS;
    } else if (is_map(arg)) {
        const Eterm *size = erts_maps_get(am_size, arg);
        const Eterm *kill = erts_maps_get(am_kill, arg);
        const Eterm *log = erts_maps_get(am_error_logger, arg);
        const Eterm *incl_bins = erts_maps_get(am_include_shared_binaries, arg);
        if (size && is_small(*size)) {
            sz = signed_val(*size);
        } else {
            /* size is mandatory */
            return 0;
        }
        if (kill) {
            if (*kill == am_true)
                *max_heap_flags |= MAX_HEAP_SIZE_KILL;
            else if (*kill == am_false)
                *max_heap_flags &= ~MAX_HEAP_SIZE_KILL;
            else
                return 0;
        }
        if (log) {
            if (*log == am_true)
                *max_heap_flags |= MAX_HEAP_SIZE_LOG;
            else if (*log == am_false)
                *max_heap_flags &= ~MAX_HEAP_SIZE_LOG;
            else
                return 0;
        }
        if (incl_bins) {
            if (*incl_bins == am_true)
                *max_heap_flags |= MAX_HEAP_SIZE_INCLUDE_OH_BINS;
            else if (*incl_bins == am_false)
                *max_heap_flags &= ~MAX_HEAP_SIZE_INCLUDE_OH_BINS;
            else
                return 0;
        }
    } else
        return 0;
    if (sz < 0)
        return 0;
    *max_heap_size = sz;
    return 1;
}

#ifdef DEBUG
void erts_validate_stack(Process *p, Eterm *frame_ptr, Eterm *stack_top) {
    Eterm *stack_bottom = HEAP_END(p);
    Eterm *next_fp = frame_ptr;
    Eterm *scanner = stack_top;

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_RA) {
        return;
    }

    /* We must have a frame pointer or an empty stack, but not both. */
    ASSERT((next_fp != NULL) ^ (stack_top == stack_bottom));

    /* If the GC happens when we are about to execute a trace we
       need to skip the trace instructions */
    if (BeamIsReturnTrace(p->i)) {
        /* Skip MFA and tracer. */
        ASSERT_MFA((ErtsCodeMFA*)cp_val(scanner[0]));
        ASSERT(IS_TRACER_VALID(scanner[1]));
        scanner += BEAM_RETURN_TRACE_FRAME_SZ;
    } else if (BeamIsReturnCallAccTrace(p->i)) {
        /* Skip prev_info. */
        scanner += BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
    } else if (BeamIsReturnToTrace(p->i)) {
        scanner += BEAM_RETURN_TO_TRACE_FRAME_SZ;
    }

    while (next_fp) {
        ASSERT(next_fp >= stack_top && next_fp <= stack_bottom);

        /* We may not skip any frames. */
        while (scanner < next_fp) {
            ASSERT(is_not_CP(scanner[0]));
            scanner++;
        }

        /* {Next frame, Return address} or vice versa */
        ASSERT(is_CP(scanner[0]) && is_CP(scanner[1]));
        next_fp = (Eterm*)cp_val(scanner[0]);

        /* Call tracing may store raw pointers on the stack. This is explicitly
         * handled in all routines that deal with the stack. */
        if (BeamIsReturnTrace((ErtsCodePtr)scanner[1])) {
            /* Skip MFA and tracer. */
            ASSERT_MFA((ErtsCodeMFA*)cp_val(scanner[2]));
            ASSERT(IS_TRACER_VALID(scanner[3]));
            scanner += BEAM_RETURN_TRACE_FRAME_SZ;
        } else if (BeamIsReturnCallAccTrace((ErtsCodePtr)scanner[1])) {
            /* Skip prev_info. */
            scanner += BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
        } else if (BeamIsReturnToTrace((ErtsCodePtr)scanner[1])) {
            scanner += BEAM_RETURN_TO_TRACE_FRAME_SZ;
        }

        scanner += CP_SIZE;
    }
}
#endif

#if defined(DEBUG) || defined(ERTS_OFFHEAP_DEBUG)

int
erts_dbg_within_proc(Eterm *ptr, Process *p, Eterm *real_htop)
{
    ErlHeapFragment* bp;
    ErtsMessage* mp;
    Eterm *htop, *heap;

    if (p->abandoned_heap) {
        heap = get_orig_heap(p, &htop, NULL);
	if (heap <= ptr && ptr < htop)
	    return 1;
    }

    heap = p->heap;
    htop = real_htop ? real_htop : HEAP_TOP(p);

    if (OLD_HEAP(p) && (OLD_HEAP(p) <= ptr && ptr < OLD_HEND(p))) {
        return 1;
    }
    if (heap <= ptr && ptr < htop) {
        return 1;
    }

    mp = p->msg_frag;
    bp = p->mbuf;

    if (bp)
	goto search_heap_frags;

    while (mp) {

        bp = erts_message_to_heap_frag(mp);
	mp = mp->next;

    search_heap_frags:

	while (bp) {
	    if (bp->mem <= ptr && ptr < bp->mem + bp->used_size) {
		return 1;
	    }
	    bp = bp->next;
	}
    }

    return 0;
}

#endif

#ifdef DEBUG

#include "erl_global_literals.h"

static int
check_all_heap_terms_in_range(int (*check_eterm)(Eterm),
                              Eterm* region_start,
                              Eterm* region_end)
{
    Eterm* tp = region_start;
    while (tp < region_end) {
        Eterm val = *tp++;

        switch (primary_tag(val)) {
        case TAG_PRIMARY_IMMED1:
            if (!check_eterm(val)) {
                return 0;
            }
            break;
        case TAG_PRIMARY_LIST:
        case TAG_PRIMARY_BOXED:
            if (!check_eterm(val)) {
                return 0;
            }
            break;
        case TAG_PRIMARY_HEADER:
            switch (val & _HEADER_SUBTAG_MASK) {
            case ARITYVAL_SUBTAG:
                break;
            case BIN_REF_SUBTAG:
            case EXTERNAL_PID_SUBTAG:
            case EXTERNAL_PORT_SUBTAG:
            case EXTERNAL_REF_SUBTAG:
            off_heap_common:
                {
                    int tari = thing_arityval(val);
                    tp += tari;
                }
                break;
            case REF_SUBTAG: {
                ErtsRefThing *rtp = (ErtsRefThing *) (tp - 1);
                if (is_magic_ref_thing(rtp)) {
                    goto off_heap_common;
                }
                ERTS_FALLTHROUGH();
            }
            default:
                {
                    int tari = header_arity(val);
                    tp += tari;
                }
                break;
            }
            break;
        }
    }
    return 1;
}

int
erts_dbg_check_heap_terms(int (*check_eterm)(Eterm),
                          Process *p,
                          Eterm *real_htop)
{
    ErlHeapFragment* bp;
    ErtsMessage* mp;
    Eterm *htop, *heap;

    if (p->abandoned_heap) {
        heap = get_orig_heap(p, &htop, NULL);
	if (!check_all_heap_terms_in_range(check_eterm,
                                           heap, htop))
	    return 0;
    }

    heap = p->heap;
    htop = real_htop ? real_htop : HEAP_TOP(p);

    if (OLD_HEAP(p) &&
        !check_all_heap_terms_in_range(check_eterm,
                                       OLD_HEAP(p), OLD_HTOP(p) /*OLD_HEND(p)*/)) {
        return 0;
    }

    if (!check_all_heap_terms_in_range(check_eterm, heap, htop)) {
        return 0;
    }

    mp = p->msg_frag;
    bp = p->mbuf;

    if (bp)
	goto search_heap_frags;

    while (mp) {

        bp = erts_message_to_heap_frag(mp);
	mp = mp->next;

    search_heap_frags:

	while (bp) {
            if (!check_all_heap_terms_in_range(check_eterm,
                                               bp->mem,
                                               bp->mem + bp->used_size)) {
                return 0;
            }
	    bp = bp->next;
	}
    }

    return 1;
}

static int check_no_empty_boxed_non_literal_term(Eterm term) {
   if (is_boxed(term)) {
        Uint arity = header_arity(*boxed_val(term));

        /* Maps can have 0 arity even though they have something after the
         * arity word. */
        if (arity == 0 && !is_map(term)) {
            if (term != ERTS_GLOBAL_LIT_EMPTY_TUPLE) {
                /* Empty tuples are the only type of boxed value that can
                 * have an arity of 0. This can change in the feature and
                 * the condition above needs to be changed if it does. */
                erts_exit(ERTS_ABORT_EXIT,
                          "Non-literal empty tuple found in heap.\n"
                          "This is not allowed due to an optimization\n"
                          "that assumes that the word after the arity\n"
                          "word is allocated.\n");
            }
        }
    }
    return 1;
}

void
erts_dbg_check_no_empty_boxed_non_literal_on_heap(Process *p,
                                                  Eterm *real_htop)
{
    erts_dbg_check_heap_terms(check_no_empty_boxed_non_literal_term,
                              p,
                              real_htop);
}

#endif

#ifdef ERTS_OFFHEAP_DEBUG

#define ERTS_CHK_OFFHEAP_ASSERT(EXP)			\
do {							\
    if (!(EXP))						\
	erts_exit(ERTS_ABORT_EXIT,			\
		 "%s:%d: Assertion failed: %s\n",	\
		 __FILE__, __LINE__, #EXP);		\
} while (0)


#ifdef ERTS_OFFHEAP_DEBUG_CHK_CIRCULAR_LIST
#  define ERTS_OFFHEAP_VISITED_BIT ((Eterm) 1 << 31)
#endif

void
erts_check_off_heap2(Process *p, Eterm *htop)
{
    Eterm *oheap = (Eterm *) OLD_HEAP(p);
    Eterm *ohtop = (Eterm *) OLD_HTOP(p);
    enum { NEW_PART, OLD_PART, WRT_BIN_PART} part;
    union erl_off_heap_ptr u;

    part = NEW_PART;
    u.hdr = MSO(p).first;
repeat:
    for (; u.hdr; u.hdr = u.hdr->next) {
	erts_aint_t refc;
	switch (thing_subtag(u.hdr->thing_word)) {
	case BIN_REF_SUBTAG:
	    refc = erts_refc_read(&(u.br->val)->intern.refc, 1);
	    break;
	case EXTERNAL_PID_SUBTAG:
	case EXTERNAL_PORT_SUBTAG:
	case EXTERNAL_REF_SUBTAG:
	    refc = erts_refc_read(&u.ext->node->refc, 1);
	    break;
	case REF_SUBTAG:
	    ASSERT(is_magic_ref_thing(u.hdr));
	    refc = erts_refc_read(&u.mref->mb->intern.refc, 1);
	    break;
	default:
	    ASSERT(!"erts_check_off_heap2: Invalid thing_word");
	}
	ERTS_CHK_OFFHEAP_ASSERT(refc >= 1);
#ifdef ERTS_OFFHEAP_DEBUG_CHK_CIRCULAR_LIST
	ERTS_CHK_OFFHEAP_ASSERT(!(u.hdr->thing_word & ERTS_OFFHEAP_VISITED_BIT));
	u.hdr->thing_word |= ERTS_OFFHEAP_VISITED_BIT;
#endif
        if (part == OLD_PART)
            ERTS_CHK_OFFHEAP_ASSERT(oheap <= u.ep && u.ep < ohtop);
        else if (part == NEW_PART && oheap <= u.ep && u.ep < ohtop)
            part = OLD_PART;
        else
            ERTS_CHK_OFFHEAP_ASSERT(erts_dbg_within_proc(u.ep, p, htop));
    }

    if (part != WRT_BIN_PART) {
        part = WRT_BIN_PART;
        u.hdr = p->wrt_bins;
        goto repeat;
    }


#ifdef ERTS_OFFHEAP_DEBUG_CHK_CIRCULAR_LIST
    for (u.hdr = MSO(p).first; u.hdr; u.hdr = u.hdr->next)
	u.hdr->thing_word &= ~ERTS_OFFHEAP_VISITED_BIT;
    for (u.hdr = p->wrt_bins; u.hdr; u.hdr = u.hdr->next)
        u.hdr->thing_word &= ~ERTS_OFFHEAP_VISITED_BIT;
#endif
}

void
erts_check_off_heap(Process *p)
{
    erts_check_off_heap2(p, NULL);
}

#endif
