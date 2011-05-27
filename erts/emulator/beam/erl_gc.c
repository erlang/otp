/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2011. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_db.h"
#include "beam_catches.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "error.h"
#include "big.h"
#include "erl_gc.h"
#if HIPE
#include "hipe_stack.h"
#include "hipe_mode_switch.h"
#endif

#define ERTS_INACT_WR_PB_LEAVE_MUCH_LIMIT 1
#define ERTS_INACT_WR_PB_LEAVE_MUCH_PERCENTAGE 20
#define ERTS_INACT_WR_PB_LEAVE_LIMIT 10
#define ERTS_INACT_WR_PB_LEAVE_PERCENTAGE 10

/*
 * Returns number of elements in an array.
 */
#define ALENGTH(a) (sizeof(a)/sizeof(a[0]))

static erts_smp_spinlock_t info_lck;
static Uint garbage_cols;		/* no of garbage collections */
static Uint reclaimed;			/* no of words reclaimed in GCs */

# define STACK_SZ_ON_HEAP(p) ((p)->hend - (p)->stop)
# define OverRunCheck(P) \
    if ((P)->stop < (P)->htop) { \
        erts_fprintf(stderr, "hend=%p\n", (p)->hend); \
        erts_fprintf(stderr, "stop=%p\n", (p)->stop); \
        erts_fprintf(stderr, "htop=%p\n", (p)->htop); \
        erts_fprintf(stderr, "heap=%p\n", (p)->heap); \
        erl_exit(ERTS_ABORT_EXIT, "%s, line %d: %T: Overrun stack and heap\n", \
		 __FILE__,__LINE__,(P)->id); \
    }

#ifdef DEBUG
#define ErtsGcQuickSanityCheck(P)					\
do {									\
    ASSERT((P)->heap < (P)->hend);					\
    ASSERT((P)->heap_sz == (P)->hend - (P)->heap);			\
    ASSERT((P)->heap <= (P)->htop && (P)->htop <= (P)->hend);		\
    ASSERT((P)->heap <= (P)->stop && (P)->stop <= (P)->hend);		\
    ASSERT((P)->heap <= (P)->high_water && (P)->high_water <= (P)->hend);\
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

static Uint setup_rootset(Process*, Eterm*, int, Rootset*);
static void cleanup_rootset(Rootset *rootset);
static Uint combined_message_size(Process* p);
static void remove_message_buffers(Process* p);
static int major_collection(Process* p, int need, Eterm* objv, int nobj, Uint *recl);
static int minor_collection(Process* p, int need, Eterm* objv, int nobj, Uint *recl);
static void do_minor(Process *p, int new_sz, Eterm* objv, int nobj);
static Eterm* sweep_rootset(Rootset *rootset, Eterm* htop, char* src, Uint src_size);
static Eterm* sweep_one_area(Eterm* n_hp, Eterm* n_htop, char* src, Uint src_size);
static Eterm* sweep_one_heap(Eterm* heap_ptr, Eterm* heap_end, Eterm* htop,
			     char* src, Uint src_size);
static Eterm* collect_heap_frags(Process* p, Eterm* heap,
				 Eterm* htop, Eterm* objv, int nobj);
static Uint adjust_after_fullsweep(Process *p, int size_before,
				   int need, Eterm *objv, int nobj);
static void shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj);
static void grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj);
static void sweep_off_heap(Process *p, int fullsweep);
static void offset_heap(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size);
static void offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size);
static void offset_rootset(Process *p, Sint offs, char* area, Uint area_size,
			   Eterm* objv, int nobj);
static void offset_off_heap(Process* p, Sint offs, char* area, Uint area_size);
static void offset_mqueue(Process *p, Sint offs, char* area, Uint area_size);

#ifdef HARDDEBUG
static void disallow_heap_frag_ref_in_heap(Process* p);
static void disallow_heap_frag_ref_in_old_heap(Process* p);
static void disallow_heap_frag_ref(Process* p, Eterm* n_htop, Eterm* objv, int nobj);
#endif

#if defined(ARCH_64) && !HALFWORD_HEAP
# define MAX_HEAP_SIZES 154
#else
# define MAX_HEAP_SIZES 55
#endif

static Sint heap_sizes[MAX_HEAP_SIZES];	/* Suitable heap sizes. */
static int num_heap_sizes;	/* Number of heap sizes. */

Uint erts_test_long_gc_sleep; /* Only used for testing... */

/*
 * Initialize GC global data.
 */
void
erts_init_gc(void)
{
    int i = 0;

    ASSERT(offsetof(ProcBin,thing_word) == offsetof(struct erl_off_heap_header,thing_word));
    ASSERT(offsetof(ProcBin,thing_word) == offsetof(ErlFunThing,thing_word));
    ASSERT(offsetof(ProcBin,thing_word) == offsetof(ExternalThing,header));
    ASSERT(offsetof(ProcBin,size) == offsetof(struct erl_off_heap_header,size));
    ASSERT(offsetof(ProcBin,size) == offsetof(ErlSubBin,size));
    ASSERT(offsetof(ProcBin,size) == offsetof(ErlHeapBin,size));
    ASSERT(offsetof(ProcBin,next) == offsetof(struct erl_off_heap_header,next));
    ASSERT(offsetof(ProcBin,next) == offsetof(ErlFunThing,next));
    ASSERT(offsetof(ProcBin,next) == offsetof(ExternalThing,next));

    erts_smp_spinlock_init(&info_lck, "gc_info");
    garbage_cols = 0;
    reclaimed = 0;
    erts_test_long_gc_sleep = 0;

    /*
     * Heap sizes start growing in a Fibonacci sequence.
     *
     * Fib growth is not really ok for really large heaps, for
     * example is fib(35) == 14meg, whereas fib(36) == 24meg;
     * we really don't want that growth when the heaps are that big.
     */
	    
    heap_sizes[0] = 34;
    heap_sizes[1] = 55;
    for (i = 2; i < 23; i++) {
	heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-2];
    }

    /* At 1.3 mega words heap, we start to slow down. */
    for (i = 23; i < ALENGTH(heap_sizes); i++) {
	heap_sizes[i] = 5*(heap_sizes[i-1]/4);
	if (heap_sizes[i] < 0) {
	    /* Size turned negative. Discard this last size. */
	    i--;
	    break;
	}
    }
    num_heap_sizes = i;
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
	erl_exit(1, "no next heap size found: %lu, offset %lu\n", (unsigned long)size, (unsigned long)offset);
    }
    return 0;
}
/*
 * Return the next heap size to use. Make sure we never return
 * a smaller heap size than the minimum heap size for the process.
 * (Use of the erlang:hibernate/3 BIF could have shrinked the
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
	if (!MY_IS_SSMALL(heap_sizes[i])) {
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

	if (MY_IS_SSMALL(sz)) {
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
erts_gc_info(ErtsGCInfo *gcip)
{
    if (gcip) {
	erts_smp_spin_lock(&info_lck);
	gcip->garbage_collections = garbage_cols;
	gcip->reclaimed = reclaimed;
	erts_smp_spin_unlock(&info_lck);
    }
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
erts_gc_after_bif_call(Process* p, Eterm result, Eterm* regs, Uint arity)
{
    int cost;

    if (is_non_value(result)) {
	if (p->freason == TRAP) {
	    cost = erts_garbage_collect(p, 0, p->def_arg_reg, p->arity);
	} else {
	    cost = erts_garbage_collect(p, 0, regs, arity);
	}
    } else {
	Eterm val[1];

	val[0] = result;
	cost = erts_garbage_collect(p, 0, val, 1);
	result = val[0];
    }
    BUMP_REDS(p, cost);
    return result;
}

/*
 * Garbage collect a process.
 *
 * p: Pointer to the process structure.
 * need: Number of Eterm words needed on the heap.
 * objv: Array of terms to add to rootset; that is to preserve.
 * nobj: Number of objects in objv.
 */
int
erts_garbage_collect(Process* p, int need, Eterm* objv, int nobj)
{
    Uint reclaimed_now = 0;
    int done = 0;
    Uint ms1, s1, us1;

    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_start);
    }

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    p->gcstatus = p->status;
    p->status = P_GARBING;
    if (erts_system_monitor_long_gc != 0) {
	get_now(&ms1, &s1, &us1);
    }
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

    erts_smp_locked_activity_begin(ERTS_ACTIVITY_GC);

    ERTS_CHK_OFFHEAP(p);

    ErtsGcQuickSanityCheck(p);
    if (GEN_GCS(p) >= MAX_GEN_GCS(p)) {
        FLAGS(p) |= F_NEED_FULLSWEEP;
    }

    /*
     * Test which type of GC to do.
     */
    while (!done) {
	if ((FLAGS(p) & F_NEED_FULLSWEEP) != 0) {
	    done = major_collection(p, need, objv, nobj, &reclaimed_now);
	} else {
	    done = minor_collection(p, need, objv, nobj, &reclaimed_now);
	}
    }

    /*
     * Finish.
     */

    ERTS_CHK_OFFHEAP(p);

    ErtsGcQuickSanityCheck(p);

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    p->status = p->gcstatus;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_end);
    }

    erts_smp_locked_activity_end(ERTS_ACTIVITY_GC);

    if (erts_system_monitor_long_gc != 0) {
	Uint ms2, s2, us2;
	Sint t;
	if (erts_test_long_gc_sleep)
	    while (0 != erts_milli_sleep(erts_test_long_gc_sleep));
	get_now(&ms2, &s2, &us2);
	t = ms2 - ms1;
	t = t*1000000 + s2 - s1;
	t = t*1000 + ((Sint) (us2 - us1))/1000;
	if (t > 0 && (Uint)t > erts_system_monitor_long_gc) {
	    monitor_long_gc(p, t);
	}
    }
    if (erts_system_monitor_large_heap != 0) {
	Uint size = HEAP_SIZE(p);
	size += OLD_HEAP(p) ? OLD_HEND(p) - OLD_HEAP(p) : 0;
	if (size >= erts_system_monitor_large_heap)
	    monitor_large_heap(p);
    }

    erts_smp_spin_lock(&info_lck);
    garbage_cols++;
    reclaimed += reclaimed_now;
    erts_smp_spin_unlock(&info_lck);

    FLAGS(p) &= ~F_FORCE_GC;

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

    return ((int) (HEAP_TOP(p) - HEAP_START(p)) / 10);
}

/*
 * Place all living data on a the new heap; deallocate any old heap.
 * Meant to be used by hibernate/3.
 */
void
erts_garbage_collect_hibernate(Process* p)
{
    Uint heap_size;
    Eterm* heap;
    Eterm* htop;
    Rootset rootset;
    char* src;
    Uint src_size;
    Uint actual_size;
    char* area;
    Uint area_size;
    Sint offs;

    /*
     * Preliminaries.
     */
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    p->gcstatus = p->status;
    p->status = P_GARBING;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_smp_locked_activity_begin(ERTS_ACTIVITY_GC);
    ErtsGcQuickSanityCheck(p);
    ASSERT(p->mbuf_sz == 0);
    ASSERT(p->mbuf == 0);
    ASSERT(p->stop == p->hend);	/* Stack must be empty. */

    /*
     * Do it.
     */


    heap_size = p->heap_sz + (p->old_htop - p->old_heap);
    heap = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_TMP_HEAP,
				    sizeof(Eterm)*heap_size);
    htop = heap;

    (void) setup_rootset(p, p->arg_reg, p->arity, &rootset);
#if HIPE
    hipe_empty_nstack(p);
#endif

    src = (char *) p->heap;
    src_size = (char *) p->htop - src;
    htop = sweep_rootset(&rootset, htop, src, src_size);
    htop = sweep_one_area(heap, htop, src, src_size);

    if (p->old_heap) {
	src = (char *) p->old_heap;
	src_size = (char *) p->old_htop - src;
	htop = sweep_rootset(&rootset, htop, src, src_size);
	htop = sweep_one_area(heap, htop, src, src_size);
    }

    cleanup_rootset(&rootset);

    if (MSO(p).first) {
        sweep_off_heap(p, 1);
    }

    /*
     *  Update all pointers.
     */
    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void*)HEAP_START(p),
		   HEAP_SIZE(p) * sizeof(Eterm));
    if (p->old_heap) {
	ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       (void*)p->old_heap,
		       (p->old_hend - p->old_heap) * sizeof(Eterm));
	p->old_heap = p->old_htop = p->old_hend = 0;
    }

    p->heap = heap;
    p->high_water = htop;
    p->htop = htop;
    p->hend = p->heap + heap_size;
    p->stop = p->hend;
    p->heap_sz = heap_size;

    heap_size = actual_size = p->htop - p->heap;
    if (heap_size == 0) {
	heap_size = 1; /* We want a heap... */
    }

    FLAGS(p) &= ~F_FORCE_GC;

    /*
     * Move the heap to its final destination.
     *
     * IMPORTANT: We have garbage collected to a temporary heap and
     * then copy the result to a newly allocated heap of exact size.
     * This is intentional and important! Garbage collecting as usual
     * and then shrinking the heap by reallocating it caused serious
     * fragmentation problems when large amounts of processes were
     * hibernated.
     */

    ASSERT(p->hend - p->stop == 0); /* Empty stack */
    ASSERT(actual_size < p->heap_sz);

    heap = ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*heap_size);
    sys_memcpy((void *) heap, (void *) p->heap, actual_size*sizeof(Eterm));
    ERTS_HEAP_FREE(ERTS_ALC_T_TMP_HEAP, p->heap, p->heap_sz*sizeof(Eterm));

    p->stop = p->hend = heap + heap_size;

    offs = heap - p->heap;
    area = (char *) p->heap;
    area_size = ((char *) p->htop) - area;
    offset_heap(heap, actual_size, offs, area, area_size);
    p->high_water = heap + (p->high_water - p->heap);
    offset_rootset(p, offs, area, area_size, p->arg_reg, p->arity);
    p->htop = heap + actual_size;
    p->heap = heap;
    p->heap_sz = heap_size;


#ifdef CHECK_FOR_HOLES
    p->last_htop = p->htop;
    p->last_mbuf = 0;
#endif    
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

    /*
     * Finishing.
     */

    ErtsGcQuickSanityCheck(p);

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    p->status = p->gcstatus;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_smp_locked_activity_end(ERTS_ACTIVITY_GC);
}


void
erts_garbage_collect_literals(Process* p, Eterm* literals, Uint lit_size)
{
    Uint byte_lit_size = sizeof(Eterm)*lit_size;
    Uint old_heap_size;
    Eterm* temp_lit;
    Sint offs;
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
    Roots* roots;
    char* area;
    Uint area_size;
    Eterm* old_htop;
    int n;

    /*
     * Set GC state.
     */
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    p->gcstatus = p->status;
    p->status = P_GARBING;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_smp_locked_activity_begin(ERTS_ACTIVITY_GC);

    /*
     * We assume that the caller has already done a major collection
     * (which has discarded the old heap), so that we don't have to cope
     * with pointer to literals on the old heap. We will now allocate
     * an old heap to contain the literals.
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
    offset_rootset(p, offs, (char *) literals, byte_lit_size, p->arg_reg, p->arity);

    /*
     * Now the literals are placed in memory that is safe to write into,
     * so now we GC the literals into the old heap. First we go through the
     * rootset.
     */

    area = (char *) temp_lit;
    area_size = byte_lit_size;
    n = setup_rootset(p, p->arg_reg, p->arity, &rootset);
    roots = rootset.roots;
    old_htop = p->old_htop;
    while (n--) {
        Eterm* g_ptr = roots->v;
        Uint g_sz = roots->sz;
	Eterm* ptr;
	Eterm val;

	roots++;

        while (g_sz--) {
            Eterm gval = *g_ptr;

            switch (primary_tag(gval)) {
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(gval);
		val = *ptr;
                if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
                    *g_ptr++ = val;
		} else if (in_area(ptr, area, area_size)) {
                    MOVE_BOXED(ptr,val,old_htop,g_ptr++);
		} else {
		    g_ptr++;
		}
		break;
	    case TAG_PRIMARY_LIST:
                ptr = list_val(gval);
                val = *ptr;
                if (IS_MOVED_CONS(val)) { /* Moved */
                    *g_ptr++ = ptr[1];
		} else if (in_area(ptr, area, area_size)) {
                    MOVE_CONS(ptr,val,old_htop,g_ptr++);
                } else {
		    g_ptr++;
		}
		break;
	    default:
                g_ptr++;
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

    old_htop = sweep_one_heap(p->heap, p->htop, old_htop, area, area_size);
    old_htop = sweep_one_area(p->old_heap, old_htop, area, area_size);
    ASSERT(p->old_htop <= old_htop && old_htop <= p->old_hend);
    p->old_htop = old_htop;

    /*
     * We no longer need this temporary area.
     */
    erts_free(ERTS_ALC_T_TMP, (void *) temp_lit);

    /*
     * Restore status.
     */
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    p->status = p->gcstatus;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_smp_locked_activity_end(ERTS_ACTIVITY_GC);
}

static int
minor_collection(Process* p, int need, Eterm* objv, int nobj, Uint *recl)
{
    Uint mature = HIGH_WATER(p) - HEAP_START(p);

    /*
     * Allocate an old heap if we don't have one and if we'll need one.
     */

    if (OLD_HEAP(p) == NULL && mature != 0) {
        Eterm* n_old;

        /* Note: We choose a larger heap size than strictly needed,
         * which seems to reduce the number of fullsweeps.
         * This improved Estone by more than 1200 estones on my computer
         * (Ultra Sparc 10).
         */
        size_t new_sz = erts_next_heap_size(HEAP_TOP(p) - HEAP_START(p), 1);

        /* Create new, empty old_heap */
        n_old = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,
					  sizeof(Eterm)*new_sz);

        OLD_HEND(p) = n_old + new_sz;
        OLD_HEAP(p) = OLD_HTOP(p) = n_old;
    }

    /*
     * Do a minor collection if there is an old heap and if it
     * is large enough.
     */

    if (OLD_HEAP(p) &&
	    ((mature <= OLD_HEND(p) - OLD_HTOP(p)) &&
	    ((BIN_VHEAP_MATURE(p) < ( BIN_OLD_VHEAP_SZ(p) - BIN_OLD_VHEAP(p)))) &&
	    ((BIN_OLD_VHEAP_SZ(p) > BIN_OLD_VHEAP(p))) ) ) {
	ErlMessage *msgp;
	Uint size_after;
	Uint need_after;
	Uint stack_size = STACK_SZ_ON_HEAP(p);
	Uint fragments = MBUF_SIZE(p) + combined_message_size(p);
	Uint size_before = fragments + (HEAP_TOP(p) - HEAP_START(p));
	Uint new_sz = next_heap_size(p, HEAP_SIZE(p) + fragments, 0);

        do_minor(p, new_sz, objv, nobj);

	/*
	 * Copy newly received message onto the end of the new heap.
	 */
	ErtsGcQuickSanityCheck(p);
	for (msgp = p->msg.first; msgp; msgp = msgp->next) {
	    if (msgp->data.attached) {
		erts_move_msg_attached_data_to_heap(&p->htop, &p->off_heap, msgp);
		ErtsGcQuickSanityCheck(p);
	    }
	}
	ErtsGcQuickSanityCheck(p);

        GEN_GCS(p)++;
        size_after = HEAP_TOP(p) - HEAP_START(p);
        need_after = size_after + need + stack_size;
        *recl += (size_before - size_after);
	
        /*
         * Excessively large heaps should be shrunk, but
         * don't even bother on reasonable small heaps.
         *
         * The reason for this is that after tenuring, we often
         * use a really small portion of new heap, therefore, unless
         * the heap size is substantial, we don't want to shrink.
         */

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

            if (wanted < MIN_HEAP_SIZE(p)) {
                wanted = MIN_HEAP_SIZE(p);
            } else {
                wanted = next_heap_size(p, wanted, 0);
            }
            if (wanted < HEAP_SIZE(p)) {
                shrink_new_heap(p, wanted, objv, nobj);
            }
            ASSERT(HEAP_SIZE(p) == next_heap_size(p, HEAP_SIZE(p), 0));
	    return 1;		/* We are done. */
        }

        if (HEAP_SIZE(p) >= need_after) {
	    /*
	     * The heap size turned out to be just right. We are done.
	     */
            ASSERT(HEAP_SIZE(p) == next_heap_size(p, HEAP_SIZE(p), 0));
            return 1;
	}
    }

    /*
     * Still not enough room after minor collection. Must force a major collection.
     */
    FLAGS(p) |= F_NEED_FULLSWEEP;
    return 0;
}

/*
 * HiPE native code stack scanning procedures:
 * - fullsweep_nstack()
 * - gensweep_nstack()
 * - offset_nstack()
 */
#if defined(HIPE)

#define GENSWEEP_NSTACK(p,old_htop,n_htop)				\
	do {								\
		Eterm *tmp_old_htop = old_htop;				\
		Eterm *tmp_n_htop = n_htop;				\
		gensweep_nstack((p), &tmp_old_htop, &tmp_n_htop);	\
		old_htop = tmp_old_htop;				\
		n_htop = tmp_n_htop;					\
	} while(0)

/*
 * offset_nstack() can ignore the descriptor-based traversal the other
 * nstack procedures use and simply call offset_heap_ptr() instead.
 * This relies on two facts:
 * 1. The only live non-Erlang terms on an nstack are return addresses,
 *    and they will be skipped thanks to the low/high range check.
 * 2. Dead values, even if mistaken for pointers into the low/high area,
 *    can be offset safely since they won't be dereferenced.
 *
 * XXX: WARNING: If HiPE starts storing other non-Erlang values on the
 * nstack, such as floats, then this will have to be changed.
 */
#define offset_nstack(p,offs,area,area_size) offset_heap_ptr(hipe_nstack_start((p)),hipe_nstack_used((p)),(offs),(area),(area_size))

#else /* !HIPE */

#define fullsweep_nstack(p,n_htop)		(n_htop)
#define GENSWEEP_NSTACK(p,old_htop,n_htop)	do{}while(0)
#define offset_nstack(p,offs,area,area_size)	do{}while(0)

#endif /* HIPE */

static void
do_minor(Process *p, int new_sz, Eterm* objv, int nobj)
{
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
    Roots* roots;
    Eterm* n_htop;
    int n;
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    char* heap = (char *) HEAP_START(p);
    Uint heap_size = (char *) HEAP_TOP(p) - heap;
    Uint mature_size = (char *) HIGH_WATER(p) - heap;
    Eterm* old_htop = OLD_HTOP(p);
    Eterm* n_heap;

    n_htop = n_heap = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
					       sizeof(Eterm)*new_sz);

    if (MBUF(p) != NULL) {
	n_htop = collect_heap_frags(p, n_heap, n_htop, objv, nobj);
    }

    n = setup_rootset(p, objv, nobj, &rootset);
    roots = rootset.roots;

    GENSWEEP_NSTACK(p, old_htop, n_htop);
    while (n--) {
        Eterm* g_ptr = roots->v;
        Uint g_sz = roots->sz;

	roots++;
        while (g_sz--) {
            gval = *g_ptr;

            switch (primary_tag(gval)) {

	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
                val = *ptr;
                if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
                    *g_ptr++ = val;
                } else if (in_area(ptr, heap, mature_size)) {
                    MOVE_BOXED(ptr,val,old_htop,g_ptr++);
                } else if (in_area(ptr, heap, heap_size)) {
                    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
                } else {
		    g_ptr++;
		}
                break;
	    }

	    case TAG_PRIMARY_LIST: {
                ptr = list_val(gval);
                val = *ptr;
                if (IS_MOVED_CONS(val)) { /* Moved */
                    *g_ptr++ = ptr[1];
                } else if (in_area(ptr, heap, mature_size)) {
                    MOVE_CONS(ptr,val,old_htop,g_ptr++);
                } else if (in_area(ptr, heap, heap_size)) {
                    MOVE_CONS(ptr,val,n_htop,g_ptr++);
                } else {
		    g_ptr++;
		}
		break;
	    }

	    default:
                g_ptr++;
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
	n_htop = sweep_one_area(n_heap, n_htop, heap, heap_size);
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
		} else if (in_area(ptr, heap, mature_size)) {
		    MOVE_BOXED(ptr,val,old_htop,n_hp++);
		} else if (in_area(ptr, heap, heap_size)) {
		    MOVE_BOXED(ptr,val,n_htop,n_hp++);
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
		} else if (in_area(ptr, heap, mature_size)) {
		    MOVE_CONS(ptr,val,old_htop,n_hp++);
		} else if (in_area(ptr, heap, heap_size)) {
		    MOVE_CONS(ptr,val,n_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_HEADER: {
		if (!header_is_thing(gval))
		    n_hp++;
		else {
		    if (header_is_bin_matchstate(gval)) {
			ErlBinMatchState *ms = (ErlBinMatchState*) n_hp;
			ErlBinMatchBuffer *mb = &(ms->mb);
			Eterm* origptr = &(mb->orig);
			ptr = boxed_val(*origptr);
			val = *ptr;
			if (IS_MOVED_BOXED(val)) {
			    *origptr = val;
			    mb->base = binary_bytes(val);
			} else if (in_area(ptr, heap, mature_size)) {
			    MOVE_BOXED(ptr,val,old_htop,origptr);
			    mb->base = binary_bytes(mb->orig);
			} else if (in_area(ptr, heap, heap_size)) {
			    MOVE_BOXED(ptr,val,n_htop,origptr);
			    mb->base = binary_bytes(mb->orig);
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

    if (OLD_HTOP(p) < old_htop) {
	old_htop = sweep_one_area(OLD_HTOP(p), old_htop, heap, heap_size);
    }
    OLD_HTOP(p) = old_htop;
    HIGH_WATER(p) = (HEAP_START(p) != HIGH_WATER(p)) ? n_heap : n_htop;

    if (MSO(p).first) {
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

    /* Copy stack to end of new heap */
    n = p->hend - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
    p->stop = n_heap + new_sz - n;

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void*)HEAP_START(p),
		   HEAP_SIZE(p) * sizeof(Eterm));
    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
    HEAP_END(p) = n_heap + new_sz;

#ifdef HARDDEBUG
    disallow_heap_frag_ref_in_heap(p);
#endif
    remove_message_buffers(p);
}

/*
 * Major collection. DISCARD the old heap.
 */

static int
major_collection(Process* p, int need, Eterm* objv, int nobj, Uint *recl)
{
    Rootset rootset;
    Roots* roots;
    int size_before;
    Eterm* n_heap;
    Eterm* n_htop;
    char* src = (char *) HEAP_START(p);
    Uint src_size = (char *) HEAP_TOP(p) - src;
    char* oh = (char *) OLD_HEAP(p);
    Uint oh_size = (char *) OLD_HTOP(p) - oh;
    int n;
    Uint new_sz;
    Uint fragments = MBUF_SIZE(p) + combined_message_size(p);
    ErlMessage *msgp;

    size_before = fragments + (HEAP_TOP(p) - HEAP_START(p));

    /*
     * Do a fullsweep GC. First figure out the size of the heap
     * to receive all live data.
     */

    new_sz = HEAP_SIZE(p) + fragments + (OLD_HTOP(p) - OLD_HEAP(p));
    /*
     * We used to do
     *
     * new_sz += STACK_SZ_ON_HEAP(p);
     *
     * here for no obvious reason. (The stack size is already counted once
     * in HEAP_SIZE(p).)
     */
    new_sz = next_heap_size(p, new_sz, 0);

    /*
     * Should we grow although we don't actually need to?
     */

    if (new_sz == HEAP_SIZE(p) && FLAGS(p) & F_HEAP_GROW) {
        new_sz = next_heap_size(p, HEAP_SIZE(p), 1);
    }
    FLAGS(p) &= ~(F_HEAP_GROW|F_NEED_FULLSWEEP);
    n_htop = n_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
						sizeof(Eterm)*new_sz);

    /*
     * Get rid of heap fragments.
     */

    if (MBUF(p) != NULL) {
	n_htop = collect_heap_frags(p, n_heap, n_htop, objv, nobj);
    }

    /*
     * Copy all top-level terms directly referenced by the rootset to
     * the new new_heap.
     */

    n = setup_rootset(p, objv, nobj, &rootset);
    n_htop = fullsweep_nstack(p, n_htop);
    roots = rootset.roots;
    while (n--) {
	Eterm* g_ptr = roots->v;
	Eterm g_sz = roots->sz;

	roots++;
	while (g_sz--) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *g_ptr;
	    
	    switch (primary_tag(gval)) {

	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
		    *g_ptr++ = val;
		} else if (in_area(ptr, src, src_size) || in_area(ptr, oh, oh_size)) {
		    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
		} else {
		    g_ptr++;
		}
		continue;
	    }

	    case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (IS_MOVED_CONS(val)) {
		    *g_ptr++ = ptr[1];
		} else if (in_area(ptr, src, src_size) || in_area(ptr, oh, oh_size)) {
		    MOVE_CONS(ptr,val,n_htop,g_ptr++);
		} else {
		    g_ptr++;
		}
		continue;
	    }

	    default: {
		g_ptr++;
		continue;
	    }
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

    if (oh_size == 0) {
	n_htop = sweep_one_area(n_heap, n_htop, src, src_size);
    } else {
	Eterm* n_hp = n_heap;

	while (n_hp != n_htop) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *n_hp;

	    switch (primary_tag(gval)) {
	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
		    *n_hp++ = val;
		} else if (in_area(ptr, src, src_size) || in_area(ptr, oh, oh_size)) {
		    MOVE_BOXED(ptr,val,n_htop,n_hp++);
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
		} else if (in_area(ptr, src, src_size) || in_area(ptr, oh, oh_size)) {
		    MOVE_CONS(ptr,val,n_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_HEADER: {
		if (!header_is_thing(gval))
		    n_hp++;
		else {
		    if (header_is_bin_matchstate(gval)) {
			ErlBinMatchState *ms = (ErlBinMatchState*) n_hp;
			ErlBinMatchBuffer *mb = &(ms->mb);
			Eterm* origptr;	
			origptr = &(mb->orig);
			ptr = boxed_val(*origptr);
			val = *ptr;
			if (IS_MOVED_BOXED(val)) {
			    *origptr = val;
			    mb->base = binary_bytes(*origptr);
			} else if (in_area(ptr, src, src_size) ||
				   in_area(ptr, oh, oh_size)) {
			    MOVE_BOXED(ptr,val,n_htop,origptr); 
			    mb->base = binary_bytes(*origptr);
			    ptr = boxed_val(*origptr);
			    val = *ptr;
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

    if (MSO(p).first) {
	sweep_off_heap(p, 1);
    }

    if (OLD_HEAP(p) != NULL) {       
	ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       OLD_HEAP(p),
		       (OLD_HEND(p) - OLD_HEAP(p)) * sizeof(Eterm));
	OLD_HEAP(p) = OLD_HTOP(p) = OLD_HEND(p) = NULL;
    }

    /* Move the stack to the end of the heap */
    n = HEAP_END(p) - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
    p->stop = n_heap + new_sz - n;

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void *) HEAP_START(p),
		   (HEAP_END(p) - HEAP_START(p)) * sizeof(Eterm));
    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
    HEAP_END(p) = n_heap + new_sz;
    GEN_GCS(p) = 0;

    HIGH_WATER(p) = HEAP_TOP(p);

    ErtsGcQuickSanityCheck(p);

    /*
     * Copy newly received message onto the end of the new heap.
     */
    for (msgp = p->msg.first; msgp; msgp = msgp->next) {
	if (msgp->data.attached) {
	    erts_move_msg_attached_data_to_heap(&p->htop, &p->off_heap, msgp);
	    ErtsGcQuickSanityCheck(p);
	}
    }

    *recl += adjust_after_fullsweep(p, size_before, need, objv, nobj);

#ifdef HARDDEBUG
    disallow_heap_frag_ref_in_heap(p);
#endif
    remove_message_buffers(p);

    ErtsGcQuickSanityCheck(p);
    return 1;			/* We are done. */
}

static Uint
adjust_after_fullsweep(Process *p, int size_before, int need, Eterm *objv, int nobj)
{
    int wanted, sz, size_after, need_after;
    int stack_size = STACK_SZ_ON_HEAP(p);
    Uint reclaimed_now;

    size_after = (HEAP_TOP(p) - HEAP_START(p));
    reclaimed_now = (size_before - size_after);
    
    /*
     * Resize the heap if needed.
     */
    
    need_after = size_after + need + stack_size;
    if (HEAP_SIZE(p) < need_after) {
        /* Too small - grow to match requested need */
        sz = next_heap_size(p, need_after, 0);
        grow_new_heap(p, sz, objv, nobj);
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
        if (wanted < p->min_heap_size) {
            sz = p->min_heap_size;
        } else {
            sz = next_heap_size(p, wanted, 0);
        }
        if (sz < HEAP_SIZE(p)) {
            shrink_new_heap(p, sz, objv, nobj);
        }
    }

    return reclaimed_now;
}

/*
 * Return the size of all message buffers that are NOT linked in the
 * mbuf list.
 */
static Uint
combined_message_size(Process* p)
{
    Uint sz = 0;
    ErlMessage *msgp;

    for (msgp = p->msg.first; msgp; msgp = msgp->next) {
	if (msgp->data.attached) {
	    sz += erts_msg_attached_data_size(msgp);
	}
    }
    return sz;
}

/*
 * Remove all message buffers.
 */
static void
remove_message_buffers(Process* p)
{
    if (MBUF(p) != NULL) {
	free_message_buffer(MBUF(p));
	MBUF(p) = NULL;
    }    
    MBUF_SIZE(p) = 0;    
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
disallow_heap_frag_ref(Process* p, Eterm* n_htop, Eterm* objv, int nobj)
{
    ErlHeapFragment* mbuf;
    ErlHeapFragment* qb;
    Eterm gval;
    Eterm* ptr;
    Eterm val;

    ASSERT(p->htop != NULL);
    mbuf = MBUF(p);

    while (nobj--) {
	gval = *objv;
	
	switch (primary_tag(gval)) {

	case TAG_PRIMARY_BOXED: {
	    ptr = _unchecked_boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED_BOXED(val)) {
		ASSERT(is_boxed(val));
		objv++;
	    } else {
 		for (qb = mbuf; qb != NULL; qb = qb->next) {
		    if (in_area(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
		objv++;
	    }
	    break;
	}

	case TAG_PRIMARY_LIST: {
	    ptr = _unchecked_list_val(gval);
	    val = *ptr;
	    if (IS_MOVED_CONS(val)) {
		objv++;
	    } else {
		for (qb = mbuf; qb != NULL; qb = qb->next) {
		    if (in_area(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
		objv++;
	    }
	    break;
	}

	default: {
	    objv++;
	    break;
	}
	}
    }
}

static void
disallow_heap_frag_ref_in_heap(Process* p)
{
    Eterm* hp;
    Eterm* htop;
    Eterm* heap;
    Uint heap_size;

    if (p->mbuf == 0) {
	return;
    }

    htop = p->htop;
    heap = p->heap;
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
	    if (!in_area(ptr, heap, heap_size)) {
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (in_area(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
	    }
	    break;
	case TAG_PRIMARY_LIST:
	    ptr = _unchecked_list_val(val);
	    if (!in_area(ptr, heap, heap_size)) {
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (in_area(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
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
	    ptr = (Eterm *) EXPAND_POINTER(val);
	    if (!in_area(ptr, old_heap, old_heap_size)) {
		if (in_area(ptr, new_heap, new_heap_size)) {
		    abort();
		}
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (in_area(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
	    }
	    break;
	case TAG_PRIMARY_LIST:
	    ptr = (Eterm *) EXPAND_POINTER(val);
	    if (!in_area(ptr, old_heap, old_heap_size)) {
		if (in_area(ptr, new_heap, new_heap_size)) {
		    abort();
		}
		for (qb = MBUF(p); qb != NULL; qb = qb->next) {
		    if (in_area(ptr, qb->mem, qb->alloc_size*sizeof(Eterm))) {
			abort();
		    }
		}
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(val)) {
		hp += _unchecked_thing_arityval(val);
		if (!in_area(hp, old_heap, old_heap_size+1)) {
		    abort();
		}
	    }
	    break;
	}
    }
}
#endif

static Eterm*
sweep_rootset(Rootset* rootset, Eterm* htop, char* src, Uint src_size)
{
    Roots* roots = rootset->roots;
    Uint n = rootset->num_roots;
    Eterm* ptr;
    Eterm gval;
    Eterm val;

    while (n--) {
        Eterm* g_ptr = roots->v;
        Uint g_sz = roots->sz;

	roots++;
        while (g_sz--) {
            gval = *g_ptr;

            switch (primary_tag(gval)) {
	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
                val = *ptr;
                if (IS_MOVED_BOXED(val)) {
		    ASSERT(is_boxed(val));
                    *g_ptr++ = val;
                } else if (in_area(ptr, src, src_size)) {
                    MOVE_BOXED(ptr,val,htop,g_ptr++);
                } else {
		    g_ptr++;
		}
                break;
	    }
	    case TAG_PRIMARY_LIST: {
                ptr = list_val(gval);
                val = *ptr;
                if (IS_MOVED_CONS(val)) {
                    *g_ptr++ = ptr[1];
                } else if (in_area(ptr, src, src_size)) {
                    MOVE_CONS(ptr,val,htop,g_ptr++);
                } else {
		    g_ptr++;
		}
		break;
	    }

	    default:
                g_ptr++;
		break;
            }
        }
    }
    return htop;
}


static Eterm*
sweep_one_area(Eterm* n_hp, Eterm* n_htop, char* src, Uint src_size)
{
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
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_BOXED(ptr,val,n_htop,n_hp++);
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
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_CONS(ptr,val,n_htop,n_hp++);
	    } else {
		n_hp++;
	    }
	    break;
	}
	case TAG_PRIMARY_HEADER: {
	    if (!header_is_thing(gval)) {
		n_hp++;
	    } else {
		if (header_is_bin_matchstate(gval)) {
		    ErlBinMatchState *ms = (ErlBinMatchState*) n_hp;
		    ErlBinMatchBuffer *mb = &(ms->mb);
		    Eterm* origptr;	
		    origptr = &(mb->orig);
		    ptr = boxed_val(*origptr);
		    val = *ptr;
		    if (IS_MOVED_BOXED(val)) {
			*origptr = val;
			mb->base = binary_bytes(*origptr);
		    } else if (in_area(ptr, src, src_size)) {
			MOVE_BOXED(ptr,val,n_htop,origptr); 
			mb->base = binary_bytes(*origptr);
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
}

static Eterm*
sweep_one_heap(Eterm* heap_ptr, Eterm* heap_end, Eterm* htop, char* src, Uint src_size)
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
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_BOXED(ptr,val,htop,heap_ptr++);
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
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_CONS(ptr,val,htop,heap_ptr++);
	    } else {
		heap_ptr++;
	    }
	    break;
	}
	case TAG_PRIMARY_HEADER: {
	    if (!header_is_thing(gval)) {
		heap_ptr++;
	    } else {
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
	    MOVE_BOXED(ptr, val, n_htop, &dummy_ref);	    
	}
	else { /* must be a cons cell */
	    ASSERT(ptr+1 < end);
	    MOVE_CONS(ptr, val, n_htop, &dummy_ref);
	    ptr += 2;
	}
    }

    return n_htop;
}

/*
 * Collect heap fragments and check that they point in the correct direction.
 */

static Eterm*
collect_heap_frags(Process* p, Eterm* n_hstart, Eterm* n_htop,
		   Eterm* objv, int nobj)
{
    ErlHeapFragment* qb;
    char* frag_begin;
    Uint frag_size;

    /*
     * We don't allow references to a heap fragments from the stack, heap,
     * or process dictionary.
     */
#ifdef HARDDEBUG
    disallow_heap_frag_ref(p, n_htop, p->stop, STACK_START(p) - p->stop);
    if (p->dictionary != NULL) {
	disallow_heap_frag_ref(p, n_htop, p->dictionary->data, p->dictionary->used);
    }
    disallow_heap_frag_ref_in_heap(p);
#endif

    /*
     * Move the heap fragments to the new heap. Note that no GC is done on
     * the heap fragments. Any garbage will thus be moved as well and survive
     * until next GC.  
     */ 
    qb = MBUF(p);
    while (qb != NULL) {      
	frag_size = qb->used_size * sizeof(Eterm);
	if (frag_size != 0) {
	    frag_begin = (char *) qb->mem;
	    n_htop = move_one_area(n_htop, frag_begin, frag_size);
	}
	qb = qb->next;
    }
    return n_htop;
}

static Uint
setup_rootset(Process *p, Eterm *objv, int nobj, Rootset *rootset)
{
    Uint avail;
    Roots* roots;
    ErlMessage* mp;
    Uint n;

    n = 0;
    roots = rootset->roots = rootset->def;
    rootset->size = ALENGTH(rootset->def);

    roots[n].v  = p->stop;
    roots[n].sz = STACK_START(p) - p->stop;
    ++n;

    if (p->dictionary != NULL) {
        roots[n].v = p->dictionary->data;
        roots[n].sz = p->dictionary->used;
        ++n;
    }
    if (nobj > 0) {
        roots[n].v  = objv;
        roots[n].sz = nobj;
        ++n;
    }

    ASSERT((is_nil(p->seq_trace_token) ||
	    is_tuple(follow_moved(p->seq_trace_token)) ||
	    is_atom(p->seq_trace_token)));
    if (is_not_immed(p->seq_trace_token)) {
	roots[n].v = &p->seq_trace_token;
	roots[n].sz = 1;
	n++;
    }

    ASSERT(is_nil(p->tracer_proc) ||
	   is_internal_pid(p->tracer_proc) ||
	   is_internal_port(p->tracer_proc));

    ASSERT(is_pid(follow_moved(p->group_leader)));
    if (is_not_immed(p->group_leader)) {
	roots[n].v  = &p->group_leader;
	roots[n].sz = 1;
	n++;
    }

    /*
     * The process may be garbage-collected while it is terminating.
     * (fvalue contains the EXIT reason and ftrace the saved stack trace.)
     */
    if (is_not_immed(p->fvalue)) {
	roots[n].v  = &p->fvalue;
	roots[n].sz = 1;
	n++;
    }
    if (is_not_immed(p->ftrace)) {
	roots[n].v  = &p->ftrace;
	roots[n].sz = 1;
	n++;
    }
    ASSERT(n <= rootset->size);

    mp = p->msg.first;
    avail = rootset->size - n;
    while (mp != NULL) {
	if (avail == 0) {
	    Uint new_size = 2*rootset->size;
	    if (roots == rootset->def) {
		roots = erts_alloc(ERTS_ALC_T_ROOTSET,
				   new_size*sizeof(Roots));
		sys_memcpy(roots, rootset->def, sizeof(rootset->def));
	    } else {
		roots = erts_realloc(ERTS_ALC_T_ROOTSET,
				     (void *) roots,
				     new_size*sizeof(Roots));
	    }
	    rootset->size = new_size;
	    avail = new_size - n;
	}
	if (mp->data.attached == NULL) {
	    roots[n].v = mp->m;
	    roots[n].sz = 2;
	    n++;
	    avail--;
	}
        mp = mp->next;
    }
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

static void
grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = HEAP_TOP(p) - HEAP_START(p);
    int stack_size = p->hend - p->stop;
    Sint offs;

    ASSERT(HEAP_SIZE(p) < new_sz);
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void *) HEAP_START(p),
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);

    if ((offs = new_heap - HEAP_START(p)) == 0) { /* No move. */
        HEAP_END(p) = new_heap + new_sz;
        sys_memmove(p->hend - stack_size, p->stop, stack_size * sizeof(Eterm));
        p->stop = p->hend - stack_size;
    } else {
	char* area = (char *) HEAP_START(p);
	Uint area_size = (char *) HEAP_TOP(p) - area;
        Eterm* prev_stop = p->stop;

        offset_heap(new_heap, heap_size, offs, area, area_size);

        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));

        HEAP_END(p) = new_heap + new_sz;
        prev_stop = new_heap + (p->stop - p->heap);
        p->stop = p->hend - stack_size;
        sys_memmove(p->stop, prev_stop, stack_size * sizeof(Eterm));

        offset_rootset(p, offs, area, area_size, objv, nobj);
        HEAP_TOP(p) = new_heap + heap_size;
        HEAP_START(p) = new_heap;
    }
    HEAP_SIZE(p) = new_sz;
}

static void
shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = HEAP_TOP(p) - HEAP_START(p);
    Sint offs;

    int stack_size = p->hend - p->stop;

    ASSERT(new_sz < p->heap_sz);
    sys_memmove(p->heap + new_sz - stack_size, p->stop, stack_size *
                                                        sizeof(Eterm));
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void*)p->heap,
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);
    p->hend = new_heap + new_sz;
    p->stop = p->hend - stack_size;

    if ((offs = new_heap - HEAP_START(p)) != 0) {
	char* area = (char *) HEAP_START(p);
	Uint area_size = (char *) HEAP_TOP(p) - area;

        /*
         * Normally, we don't expect a shrunk heap to move, but you never
         * know on some strange embedded systems...  Or when using purify.
         */

        offset_heap(new_heap, heap_size, offs, area, area_size);

        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));
        offset_rootset(p, offs, area, area_size, objv, nobj);
        HEAP_TOP(p) = new_heap + heap_size;
        HEAP_START(p) = new_heap;
    }
    HEAP_SIZE(p) = new_sz;
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

struct shrink_cand_data {
    struct erl_off_heap_header* new_candidates;
    struct erl_off_heap_header* new_candidates_end;
    struct erl_off_heap_header* old_candidates;
    Uint no_of_candidates;
    Uint no_of_active;
};

static ERTS_INLINE void
link_live_proc_bin(struct shrink_cand_data *shrink,
		   struct erl_off_heap_header*** prevppp,
		   struct erl_off_heap_header** currpp,
		   int new_heap)
{
    ProcBin *pbp = (ProcBin*) *currpp;
    ASSERT(**prevppp == *currpp);

    *currpp = pbp->next;
    if (pbp->flags & (PB_ACTIVE_WRITER|PB_IS_WRITABLE)) {
	ASSERT(((pbp->flags & (PB_ACTIVE_WRITER|PB_IS_WRITABLE))
		== (PB_ACTIVE_WRITER|PB_IS_WRITABLE))
	       || ((pbp->flags & (PB_ACTIVE_WRITER|PB_IS_WRITABLE))
		   == PB_IS_WRITABLE));


	if (pbp->flags & PB_ACTIVE_WRITER) {
	    pbp->flags &= ~PB_ACTIVE_WRITER;
	    shrink->no_of_active++;
	}
	else { /* inactive */
	    Uint unused = pbp->val->orig_size - pbp->size;
	    /* Our allocators are 8 byte aligned, i.e., shrinking with
	       less than 8 bytes will have no real effect */
	    if (unused >= 8) { /* A shrink candidate; save in candidate list */
		**prevppp = pbp->next;
		if (new_heap) {
		    if (!shrink->new_candidates)
			shrink->new_candidates_end = (struct erl_off_heap_header*)pbp;
		    pbp->next = shrink->new_candidates;
		    shrink->new_candidates = (struct erl_off_heap_header*)pbp;
		}
		else {
		    pbp->next = shrink->old_candidates;
		    shrink->old_candidates = (struct erl_off_heap_header*)pbp;
		}
		shrink->no_of_candidates++;
		return;
	    }
	}
    }

    /* Not a shrink candidate; keep in original mso list */ 
    *prevppp = &pbp->next;
}


static void
sweep_off_heap(Process *p, int fullsweep)
{
    struct shrink_cand_data shrink = {0};
    struct erl_off_heap_header* ptr;
    struct erl_off_heap_header** prev;
    char* oheap = NULL;
    Uint oheap_sz = 0;
    Uint64 bin_vheap = 0;
#ifdef DEBUG
    int seen_mature = 0;
#endif

    if (fullsweep == 0) {
	oheap = (char *) OLD_HEAP(p);
	oheap_sz = (char *) OLD_HEND(p) - oheap;
    }

    BIN_OLD_VHEAP(p) = 0;

    prev = &MSO(p).first;
    ptr = MSO(p).first;

    /* Firts part of the list will reside on the (old) new-heap.
     * Keep if moved, otherwise deref.
     */
    while (ptr) {
	if (IS_MOVED_BOXED(ptr->thing_word)) {
	    ASSERT(!in_area(ptr, oheap, oheap_sz));
	    *prev = ptr = (struct erl_off_heap_header*) boxed_val(ptr->thing_word);
	    ASSERT(!IS_MOVED_BOXED(ptr->thing_word));
	    if (ptr->thing_word == HEADER_PROC_BIN) {
		int to_new_heap = !in_area(ptr, oheap, oheap_sz);
		ASSERT(to_new_heap == !seen_mature || (!to_new_heap && (seen_mature=1)));
		if (to_new_heap) {
		    bin_vheap += ptr->size / sizeof(Eterm);
		} else {
		    BIN_OLD_VHEAP(p) += ptr->size / sizeof(Eterm); /* for binary gc (words)*/
		}		
		link_live_proc_bin(&shrink, &prev, &ptr, to_new_heap);
	    }
	    else {
		prev = &ptr->next;
		ptr = ptr->next;
	    }
	}
	else if (!in_area(ptr, oheap, oheap_sz)) {
	    /* garbage */
	    switch (thing_subtag(ptr->thing_word)) {
	    case REFC_BINARY_SUBTAG:
		{
		    Binary* bptr = ((ProcBin*)ptr)->val;	
		    if (erts_refc_dectest(&bptr->refc, 0) == 0) {
			erts_bin_free(bptr);
		    }
		    break;
		}
	    case FUN_SUBTAG:
		{
		    ErlFunEntry* fe = ((ErlFunThing*)ptr)->fe;
		    if (erts_refc_dectest(&fe->refc, 0) == 0) {
			erts_erase_fun_entry(fe);
		    }
		    break;
		}
	    default:
		ASSERT(is_external_header(ptr->thing_word));
		erts_deref_node_entry(((ExternalThing*)ptr)->node);
	    }
	    *prev = ptr = ptr->next;
	}
	else break; /* and let old-heap loop continue */
    }

    /* The rest of the list resides on old-heap, and we just did a
     * generational collection - keep objects in list.
     */
    while (ptr) {
	ASSERT(in_area(ptr, oheap, oheap_sz));
	ASSERT(!IS_MOVED_BOXED(ptr->thing_word));       
	if (ptr->thing_word == HEADER_PROC_BIN) {
	    BIN_OLD_VHEAP(p) += ptr->size / sizeof(Eterm); /* for binary gc (words)*/
	    link_live_proc_bin(&shrink, &prev, &ptr, 0);
	}
	else {
	    ASSERT(is_fun_header(ptr->thing_word) ||
		   is_external_header(ptr->thing_word));
	    prev = &ptr->next;
	    ptr = ptr->next;
	}
    }

    if (fullsweep) {
	BIN_OLD_VHEAP_SZ(p) = next_vheap_size(p, BIN_OLD_VHEAP(p) + MSO(p).overhead, BIN_OLD_VHEAP_SZ(p));
    }
    BIN_VHEAP_SZ(p)     = next_vheap_size(p, bin_vheap, BIN_VHEAP_SZ(p));
    MSO(p).overhead     = bin_vheap;
    BIN_VHEAP_MATURE(p) = bin_vheap;

    /*
     * If we got any shrink candidates, check them out.
     */

    if (shrink.no_of_candidates) {
	ProcBin *candlist[] = { (ProcBin*)shrink.new_candidates,
	                        (ProcBin*)shrink.old_candidates };
	Uint leave_unused = 0;
	int i;

	if (shrink.no_of_active == 0) {
	    if (shrink.no_of_candidates <= ERTS_INACT_WR_PB_LEAVE_MUCH_LIMIT)
		leave_unused = ERTS_INACT_WR_PB_LEAVE_MUCH_PERCENTAGE;
	    else if (shrink.no_of_candidates <= ERTS_INACT_WR_PB_LEAVE_LIMIT)
		leave_unused = ERTS_INACT_WR_PB_LEAVE_PERCENTAGE;
	}

	for (i = 0; i < sizeof(candlist)/sizeof(candlist[0]); i++) {
	    ProcBin* pb;
	    for (pb = candlist[i]; pb; pb = (ProcBin*)pb->next) {
		Uint new_size = pb->size;

		if (leave_unused) {
		    new_size += (new_size * 100) / leave_unused;
		    /* Our allocators are 8 byte aligned, i.e., shrinking with
		       less than 8 bytes will have no real effect */
		    if (new_size + 8 >= pb->val->orig_size)
			continue;
		}

		pb->val = erts_bin_realloc(pb->val, new_size);
		pb->val->orig_size = new_size;
		pb->bytes = (byte *) pb->val->orig_bytes;
	    }
	}


	/*
	 * We now potentially have the mso list divided into three lists:
	 * - shrink candidates on new heap (inactive writable with unused data)
	 * - shrink candidates on old heap (inactive writable with unused data)
	 * - other binaries (read only + active writable ...) + funs and externals
	 *
	 * Put them back together: new candidates -> other -> old candidates
	 * This order will ensure that the list only refers from new
	 * generation to old and never from old to new *which is important*.
	 */
	if (shrink.new_candidates) {
	    if (prev == &MSO(p).first) /* empty other binaries list */
		prev = &shrink.new_candidates_end->next;
	    else
		shrink.new_candidates_end->next = MSO(p).first;
	    MSO(p).first = shrink.new_candidates;
	}
    }
    *prev = shrink.old_candidates;
}

/*
 * Offset pointers into the heap (not stack).
 */

static void 
offset_heap(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	  case TAG_PRIMARY_LIST:
	  case TAG_PRIMARY_BOXED:
	      if (in_area(ptr_val(val), area, area_size)) {
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
	      case REFC_BINARY_SUBTAG:
	      case FUN_SUBTAG:
	      case EXTERNAL_PID_SUBTAG:
	      case EXTERNAL_PORT_SUBTAG:
	      case EXTERNAL_REF_SUBTAG:
		  {
		      struct erl_off_heap_header* oh = (struct erl_off_heap_header*) hp;

		      if (in_area(oh->next, area, area_size)) {
			  Eterm** uptr = (Eterm **) (void *) &oh->next;
			  *uptr += offs; /* Patch the mso chain */
		      }
		  }
		  break;
	      case BIN_MATCHSTATE_SUBTAG:
		{	
		  ErlBinMatchState *ms = (ErlBinMatchState*) hp;
		  ErlBinMatchBuffer *mb = &(ms->mb);
		  if (in_area(ptr_val(mb->orig), area, area_size)) {
		      mb->orig = offset_ptr(mb->orig, offs);
		      mb->base = binary_bytes(mb->orig);
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

/*
 * Offset pointers to heap from stack.
 */

static void 
offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    if (in_area(ptr_val(val), area, area_size)) {
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
offset_off_heap(Process* p, Sint offs, char* area, Uint area_size)
{
    if (MSO(p).first && in_area((Eterm *)MSO(p).first, area, area_size)) {
        Eterm** uptr = (Eterm**) (void *) &MSO(p).first;
        *uptr += offs;
    }
}

/*
 * Offset pointers in message queue.
 */
static void
offset_mqueue(Process *p, Sint offs, char* area, Uint area_size)
{
    ErlMessage* mp = p->msg.first;

    while (mp != NULL) {
        Eterm mesg = ERL_MESSAGE_TERM(mp);
	if (is_value(mesg)) {
	    switch (primary_tag(mesg)) {
	    case TAG_PRIMARY_LIST:
	    case TAG_PRIMARY_BOXED:
		if (in_area(ptr_val(mesg), area, area_size)) {
		    ERL_MESSAGE_TERM(mp) = offset_ptr(mesg, offs);
		}
		break;
	    }
	}
	mesg = ERL_MESSAGE_TOKEN(mp);
	if (is_boxed(mesg) && in_area(ptr_val(mesg), area, area_size)) {
	    ERL_MESSAGE_TOKEN(mp) = offset_ptr(mesg, offs);
        }
        ASSERT((is_nil(ERL_MESSAGE_TOKEN(mp)) ||
		is_tuple(ERL_MESSAGE_TOKEN(mp)) ||
		is_atom(ERL_MESSAGE_TOKEN(mp))));
        mp = mp->next;
    }
}

static void ERTS_INLINE
offset_one_rootset(Process *p, Sint offs, char* area, Uint area_size,
	       Eterm* objv, int nobj)
{
    if (p->dictionary)  {
	offset_heap(p->dictionary->data, 
		    p->dictionary->used, 
		    offs, area, area_size);
    }
    offset_heap_ptr(&p->fvalue, 1, offs, area, area_size);
    offset_heap_ptr(&p->ftrace, 1, offs, area, area_size);
    offset_heap_ptr(&p->seq_trace_token, 1, offs, area, area_size);
    offset_heap_ptr(&p->group_leader, 1, offs, area, area_size);
    offset_mqueue(p, offs, area, area_size);
    offset_heap_ptr(p->stop, (STACK_START(p) - p->stop), offs, area, area_size);
    offset_nstack(p, offs, area, area_size);
    if (nobj > 0) {
	offset_heap_ptr(objv, nobj, offs, area, area_size);
    }
    offset_off_heap(p, offs, area, area_size);
}

static void
offset_rootset(Process *p, Sint offs, char* area, Uint area_size,
	       Eterm* objv, int nobj)
{
    offset_one_rootset(p, offs, area, area_size, objv, nobj);
}

#if defined(DEBUG) || defined(ERTS_OFFHEAP_DEBUG)

static int
within2(Eterm *ptr, Process *p, Eterm *real_htop)
{
    ErlHeapFragment* bp = MBUF(p);
    ErlMessage* mp = p->msg.first;
    Eterm *htop = real_htop ? real_htop : HEAP_TOP(p);

    if (OLD_HEAP(p) && (OLD_HEAP(p) <= ptr && ptr < OLD_HEND(p))) {
        return 1;
    }
    if (HEAP_START(p) <= ptr && ptr < htop) {
        return 1;
    }
    while (bp != NULL) {
        if (bp->mem <= ptr && ptr < bp->mem + bp->used_size) {
            return 1;
        }
        bp = bp->next;
    }
    while (mp) {
	if (mp->data.attached) {
	    ErlHeapFragment *hfp;
	    if (is_value(ERL_MESSAGE_TERM(mp)))
		hfp = mp->data.heap_frag;
	    else if (is_not_nil(ERL_MESSAGE_TOKEN(mp)))
		hfp = erts_dist_ext_trailer(mp->data.dist_ext);
	    else
		hfp = NULL;
	    if (hfp && hfp->mem <= ptr && ptr < hfp->mem + hfp->used_size)
		return 1;
	}
        mp = mp->next;
    }
    return 0;
}

int
within(Eterm *ptr, Process *p)
{
    return within2(ptr, p, NULL);
}

#endif

#ifdef ERTS_OFFHEAP_DEBUG

#define ERTS_CHK_OFFHEAP_ASSERT(EXP)			\
do {							\
    if (!(EXP))						\
	erl_exit(ERTS_ABORT_EXIT,			\
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
    int old;
    union erl_off_heap_ptr u;

    old = 0;
    for (u.hdr = MSO(p).first; u.hdr; u.hdr = u.hdr->next) {
	erts_aint_t refc;
	switch (thing_subtag(u.hdr->thing_word)) {
	case REFC_BINARY_SUBTAG:
	    refc = erts_refc_read(&u.pb->val->refc, 1);		
	    break;
	case FUN_SUBTAG:
	    refc = erts_refc_read(&u.fun->fe->refc, 1);
	    break;
	case EXTERNAL_PID_SUBTAG:
	case EXTERNAL_PORT_SUBTAG:
	case EXTERNAL_REF_SUBTAG:
	    refc = erts_refc_read(&u.ext->node->refc, 1);
	    break;
	default:
	    ASSERT(!!"erts_check_off_heap2: Invalid thing_word");
	}
	ERTS_CHK_OFFHEAP_ASSERT(refc >= 1);
#ifdef ERTS_OFFHEAP_DEBUG_CHK_CIRCULAR_LIST
	ERTS_CHK_OFFHEAP_ASSERT(!(u.hdr->thing_word & ERTS_EXTERNAL_VISITED_BIT));
	u.hdr->thing_word |= ERTS_OFFHEAP_VISITED_BIT;
#endif
	if (old) {
	    ERTS_CHK_OFFHEAP_ASSERT(oheap <= u.ep && u.ep < ohtop);
	}
	else if (oheap <= u.ep && u.ep < ohtop)
	    old = 1;
	else {
	    ERTS_CHK_OFFHEAP_ASSERT(within2(u.ep, p, htop));
	}
    }

#ifdef ERTS_OFFHEAP_DEBUG_CHK_CIRCULAR_EXTERNAL_LIST
    for (u.hdr = MSO(p).first; u.hdr; u.hdr = u.hdr->next)
	u.hdr->thing_word &= ~ERTS_OFFHEAP_VISITED_BIT;
#endif
}

void
erts_check_off_heap(Process *p)
{
    erts_check_off_heap2(p, NULL);
}

#endif
