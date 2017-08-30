/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

/*
 * Description:	A memory segment allocator. Segments that are deallocated
 *              are kept for a while in a segment "cache" before they are
 *              destroyed. When segments are allocated, cached segments
 *              are used if possible instead of creating new segments.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_WANT_MEM_MAPPERS
#include "sys.h"
#include "erl_mseg.h"
#include "global.h"
#include "erl_threads.h"
#include "erl_mtrace.h"
#include "erl_time.h"
#include "erl_alloc.h"
#include "big.h"
#include "erl_thr_progress.h"
#include "erl_util_queue.h"

#if HAVE_ERTS_MSEG

#define SEGTYPE ERTS_MTRACE_SEGMENT_ID

#ifndef HAVE_GETPAGESIZE
#define HAVE_GETPAGESIZE 0
#endif

#ifdef _SC_PAGESIZE
#  define GET_PAGE_SIZE sysconf(_SC_PAGESIZE)
#elif HAVE_GETPAGESIZE
#  define GET_PAGE_SIZE getpagesize()
#else
#  error "Page size unknown"
     /* Implement some other way to get the real page size if needed! */
#endif

#undef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#undef MAX
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

#define INV_ALIGNED_MASK    ((UWord) ((MSEG_ALIGNED_SIZE) - 1))
#define ALIGNED_MASK        (~INV_ALIGNED_MASK)
#define ALIGNED_FLOOR(X)    (((UWord)(X)) & ALIGNED_MASK)
#define ALIGNED_CEILING(X)  ALIGNED_FLOOR((X) + INV_ALIGNED_MASK)
#define MAP_IS_ALIGNED(X)   (((UWord)(X) & (MSEG_ALIGNED_SIZE - 1)) == 0)

#define IS_2POW(X)          ((X) && !((X) & ((X) - 1)))
static ERTS_INLINE Uint ceil_2pow(Uint x) {
    int i = 1 << (4 + (sizeof(Uint) != 4 ? 1 : 0));
    x--;
    do { x |= x >> i; } while(i >>= 1);
    return x + 1;
}
static const int debruijn[32] = {
     0,  1, 28,  2, 29, 14, 24,  3, 30, 22, 20, 15, 25, 17,  4,  8, 
    31, 27, 13, 23, 21, 19, 16,  7, 26, 12, 18,  6, 11,  5, 10,  9
};

#define LOG2(X) (debruijn[((Uint32)(((X) & -(X)) * 0x077CB531U)) >> 27])

#define CACHE_AREAS      (32 - MSEG_ALIGN_BITS)

#define SIZE_TO_CACHE_AREA_IDX(S)   (LOG2((S)) - MSEG_ALIGN_BITS)
#define MAX_CACHE_SIZE   (30)

#define MSEG_FLG_IS_2POW(X)    ((X) & ERTS_MSEG_FLG_2POW)

#ifdef DEBUG
#define DBG(F,...) fprintf(stderr, (F), __VA_ARGS__ )
#else
#define DBG(F,...) do{}while(0)
#endif

static int atoms_initialized;

const ErtsMsegOpt_t erts_mseg_default_opt = {
    1,			/* Use cache		     */
    1,			/* Preserv data		     */
    0,			/* Absolute shrink threshold */
    0,			/* Relative shrink threshold */
    0			/* Scheduler specific        */
};


typedef struct {
    Uint32 giga_no;
    Uint32 no;
} CallCounter;

typedef struct {
    CallCounter alloc;
    CallCounter dealloc;
    CallCounter realloc;
    CallCounter create;
    CallCounter create_resize;
    CallCounter destroy;
    CallCounter recreate;
    CallCounter clear_cache;
    CallCounter check_cache;
} ErtsMsegCalls;

typedef struct cache_t_ cache_t;

struct cache_t_ {
    UWord size;
    void *seg;
    cache_t *next;
    cache_t *prev;
};


typedef struct ErtsMsegAllctr_t_ ErtsMsegAllctr_t;

struct ErtsMsegAllctr_t_ {
    int ix;

    int is_init_done;
    int is_thread_safe;
    erts_mtx_t mtx;

    int is_cache_check_scheduled;

    cache_t cache[MAX_CACHE_SIZE];
    cache_t cache_unpowered_node;
    cache_t cache_powered_node[CACHE_AREAS];
    cache_t cache_free;

    Sint cache_size;
    Uint cache_hits;

    struct {
	struct {
	    Uint watermark;
	    Uint no;
	    Uint sz;
	} current;
	struct {
	    Uint no;
	    Uint sz;
	} max;
	struct {
	    Uint no;
	    Uint sz;
	} max_ever;
    } segments;

    Uint max_cache_size;
    Uint abs_max_cache_bad_fit;
    Uint rel_max_cache_bad_fit;

    ErtsMsegCalls calls;
};

typedef union {
    ErtsMsegAllctr_t mseg_alloc;
    char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsMsegAllctr_t))];
} ErtsAlgndMsegAllctr_t;

static int no_mseg_allocators;
static ErtsAlgndMsegAllctr_t *aligned_mseg_allctr;

#ifdef ERTS_SMP

#define ERTS_MSEG_ALLCTR_IX(IX) \
  (&aligned_mseg_allctr[(IX)].mseg_alloc)

#define ERTS_MSEG_ALLCTR_SS() \
  ERTS_MSEG_ALLCTR_IX((int) erts_get_scheduler_id())

#define ERTS_MSEG_ALLCTR_OPT(OPT) \
  ((OPT)->sched_spec ? ERTS_MSEG_ALLCTR_SS() : ERTS_MSEG_ALLCTR_IX(0))

#else

#define ERTS_MSEG_ALLCTR_IX(IX) \
  (&aligned_mseg_allctr[0].mseg_alloc)

#define ERTS_MSEG_ALLCTR_SS() \
  (&aligned_mseg_allctr[0].mseg_alloc)

#define ERTS_MSEG_ALLCTR_OPT(OPT) \
  (&aligned_mseg_allctr[0].mseg_alloc)

#endif

#define ERTS_MSEG_LOCK(MA)		\
do {					\
    if ((MA)->is_thread_safe)		\
	erts_mtx_lock(&(MA)->mtx);	\
} while (0)

#define ERTS_MSEG_UNLOCK(MA)		\
do {					\
    if ((MA)->is_thread_safe)		\
	erts_mtx_unlock(&(MA)->mtx);	\
} while (0)

#define ERTS_MSEG_ALLOC_STAT(C,SZ)					\
do {									\
    C->segments.current.no++;						\
    if (C->segments.max.no < C->segments.current.no)			\
	C->segments.max.no = C->segments.current.no;			\
    if (C->segments.current.watermark < C->segments.current.no)		\
	C->segments.current.watermark = C->segments.current.no;		\
    C->segments.current.sz += (SZ);					\
    if (C->segments.max.sz < C->segments.current.sz)			\
	C->segments.max.sz = C->segments.current.sz;			\
} while (0)

#define ERTS_MSEG_DEALLOC_STAT(C,SZ)					\
do {									\
    ASSERT(C->segments.current.no > 0);					\
    C->segments.current.no--;						\
    ASSERT(C->segments.current.sz >= (SZ));				\
    C->segments.current.sz -= (SZ);					\
} while (0) 

#define ERTS_MSEG_REALLOC_STAT(C,OSZ, NSZ)				\
do {									\
    ASSERT(C->segments.current.sz >= (OSZ));				\
    C->segments.current.sz -= (OSZ);					\
    C->segments.current.sz += (NSZ);					\
} while (0)

#define ONE_GIGA (1000000000)

#define ZERO_CC(MA, CC) ((MA)->calls.CC.no = 0,				\
			 (MA)->calls.CC.giga_no = 0)

#define INC_CC(MA, CC) ((MA)->calls.CC.no == ONE_GIGA - 1		\
			? ((MA)->calls.CC.giga_no++,			\
			   (MA)->calls.CC.no = 0)			\
			: (MA)->calls.CC.no++)

#define DEC_CC(MA, CC) ((MA)->calls.CC.no == 0				\
			? ((MA)->calls.CC.giga_no--,			\
			   (MA)->calls.CC.no = ONE_GIGA - 1)		\
			: (MA)->calls.CC.no--)


static erts_mtx_t init_atoms_mutex; /* Also needed when !USE_THREADS */


static ERTS_INLINE void
schedule_cache_check(ErtsMsegAllctr_t *ma) {

    if (!ma->is_cache_check_scheduled && ma->is_init_done) {
	erts_set_aux_work_timeout(ma->ix,
				  ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK,
				  1);
	ma->is_cache_check_scheduled = 1;
    }
}

/* #define ERTS_PRINT_ERTS_MMAP */

static ERTS_INLINE void *
mseg_create(ErtsMsegAllctr_t *ma, Uint flags, UWord *sizep)
{
#ifdef ERTS_PRINT_ERTS_MMAP
    UWord req_size = *sizep;
#endif
    void *seg;
    Uint32 mmap_flags = 0;
    if (MSEG_FLG_IS_2POW(flags))
	mmap_flags |= ERTS_MMAPFLG_SUPERALIGNED;

    seg = erts_mmap(&erts_dflt_mmapper, mmap_flags, sizep);

#ifdef ERTS_PRINT_ERTS_MMAP
    erts_fprintf(stderr, "%p = erts_mmap(%s, {%bpu, %bpu});\n", seg,
		 (mmap_flags & ERTS_MMAPFLG_SUPERALIGNED) ? "sa" : "sua",
		 req_size, *sizep);
#endif

    INC_CC(ma, create);

    return seg;
}

static ERTS_INLINE void
mseg_destroy(ErtsMsegAllctr_t *ma, Uint flags, void *seg_p, UWord size) {
    
    Uint32 mmap_flags = 0;
    if (MSEG_FLG_IS_2POW(flags))
	 mmap_flags |= ERTS_MMAPFLG_SUPERALIGNED;

    erts_munmap(&erts_dflt_mmapper, mmap_flags, seg_p, size);
#ifdef ERTS_PRINT_ERTS_MMAP
    erts_fprintf(stderr, "erts_munmap(%s, %p, %bpu);\n",
		 (mmap_flags & ERTS_MMAPFLG_SUPERALIGNED) ? "sa" : "sua",
		 seg_p, *size);
#endif
    INC_CC(ma, destroy);

}

static ERTS_INLINE void *
mseg_recreate(ErtsMsegAllctr_t *ma, Uint flags, void *old_seg, UWord old_size, UWord *sizep)
{
#ifdef ERTS_PRINT_ERTS_MMAP
    UWord req_size = *sizep;	
#endif
    void *new_seg;
    Uint32 mmap_flags = 0;
    if (MSEG_FLG_IS_2POW(flags))
	mmap_flags |= ERTS_MMAPFLG_SUPERALIGNED;

    new_seg = erts_mremap(&erts_dflt_mmapper, mmap_flags, old_seg, old_size, sizep);

#ifdef ERTS_PRINT_ERTS_MMAP
    erts_fprintf(stderr, "%p = erts_mremap(%s, %p, %bpu, {%bpu, %bpu});\n",
		 new_seg, (mmap_flags & ERTS_MMAPFLG_SUPERALIGNED) ? "sa" : "sua",
		 old_seg, old_size, req_size, *sizep);
#endif
    INC_CC(ma, recreate);

    return new_seg;
}

#ifdef DEBUG
#define ERTS_DBG_MA_CHK_THR_ACCESS(MA)					\
do {									\
    if ((MA)->is_thread_safe)						\
	ERTS_LC_ASSERT(erts_lc_mtx_is_locked(&(MA)->mtx)		\
		       || erts_smp_thr_progress_is_blocking()		\
		       || ERTS_IS_CRASH_DUMPING);			\
    else								\
	ERTS_LC_ASSERT((MA)->ix == (int) erts_get_scheduler_id()	\
		       || erts_smp_thr_progress_is_blocking()		\
		       || ERTS_IS_CRASH_DUMPING);			\
} while (0)
#else
#define ERTS_DBG_MA_CHK_THR_ACCESS(MA)
#endif

/* Cache interface */


static ERTS_INLINE void mseg_cache_clear_node(cache_t *c) {
    c->seg = NULL;
    c->size = 0;
    c->next = c;
    c->prev = c;
}

static ERTS_INLINE int cache_bless_segment(ErtsMsegAllctr_t *ma, void *seg, UWord size, Uint flags) {

    cache_t *c;
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);

    ASSERT(!MSEG_FLG_IS_2POW(flags) || (MSEG_FLG_IS_2POW(flags) && MAP_IS_ALIGNED(seg) && IS_2POW(size)));

    /* The idea is that sbc caching is prefered over mbc caching.
     * Blocks are normally allocated in mb carriers and thus cached there.
     * Large blocks has no such cache and it is up to mseg to cache them to speed things up.
     */

    if (!erts_circleq_is_empty(&(ma->cache_free))) {

	/* We have free slots, use one to cache the segment */

	c = erts_circleq_head(&(ma->cache_free));
	erts_circleq_remove(c);

	c->seg  = seg;
	c->size = size;

	if (MSEG_FLG_IS_2POW(flags)) {
	    int ix = SIZE_TO_CACHE_AREA_IDX(size);

	    ASSERT(ix < CACHE_AREAS);
	    ASSERT((1 << (ix + MSEG_ALIGN_BITS)) == size);

	    erts_circleq_push_head(&(ma->cache_powered_node[ix]), c);

	} else
	    erts_circleq_push_head(&(ma->cache_unpowered_node), c);

	ma->cache_size++;

	return 1;
    } else if (!MSEG_FLG_IS_2POW(flags) && !erts_circleq_is_empty(&(ma->cache_unpowered_node))) {
	/* No free slots.
	 * Evict oldest slot from unpowered cache so we can cache an unpowered (sbc) segment  */

	c = erts_circleq_tail(&(ma->cache_unpowered_node));
	erts_circleq_remove(c);

	mseg_destroy(ma, ERTS_MSEG_FLG_NONE, c->seg, c->size);
	mseg_cache_clear_node(c);

	c->seg  = seg;
	c->size = size;

	erts_circleq_push_head(&(ma->cache_unpowered_node), c);

	return 1;
    } else if (!MSEG_FLG_IS_2POW(flags)) {

	/* No free slots and no unpowered (sbc) slots.
	 * Evict smallest slot from powered cache so we can cache an unpowered (sbc) segment.
	 * Note: Though this is the wanted policy I don't think it is used significantly.
	 * This branch could probably be removed without serious impact.
	 */

	int i;

	for( i = 0; i < CACHE_AREAS; i++) {
	    if (erts_circleq_is_empty(&(ma->cache_powered_node[i])))
		continue;

	    c = erts_circleq_tail(&(ma->cache_powered_node[i]));
	    erts_circleq_remove(c);

	    mseg_destroy(ma, ERTS_MSEG_FLG_2POW, c->seg, c->size);

	    mseg_cache_clear_node(c);

	    c->seg  = seg;
	    c->size = size;

	    erts_circleq_push_head(&(ma->cache_unpowered_node), c);

	    return 1;
	}
    }

    return 0;
}

static ERTS_INLINE void *cache_get_segment(ErtsMsegAllctr_t *ma, UWord *size_p, Uint flags) {

    UWord size = *size_p;

    ERTS_DBG_MA_CHK_THR_ACCESS(ma);

    if (MSEG_FLG_IS_2POW(flags)) {

	int i, ix = SIZE_TO_CACHE_AREA_IDX(size);
	char *seg;
	cache_t *c;
	UWord csize;

	ASSERT(IS_2POW(size));

	for( i = ix; i < CACHE_AREAS; i++) {

	    if (erts_circleq_is_empty(&(ma->cache_powered_node[i])))
		continue;

	    c = erts_circleq_head(&(ma->cache_powered_node[i]));
	    erts_circleq_remove(c);

	    ASSERT(IS_2POW(c->size));
	    ASSERT(MAP_IS_ALIGNED(c->seg));

	    csize = c->size;
	    seg   = (char*) c->seg;

	    ma->cache_size--;
	    ma->cache_hits++;

	    /* link to free cache list */
	    mseg_cache_clear_node(c);
	    erts_circleq_push_head(&(ma->cache_free), c);

	    ASSERT(!(ma->cache_size < 0));

	    if (csize != size)
		mseg_destroy(ma, ERTS_MSEG_FLG_2POW, seg + size, csize - size);

	    return seg;
	}
    } 
    else if (!erts_circleq_is_empty(&(ma->cache_unpowered_node))) {
	void *seg;
	cache_t *c;
	cache_t *best = NULL;
	UWord bdiff = 0;
	UWord csize;
	UWord bad_max_abs = ma->abs_max_cache_bad_fit;
	UWord bad_max_rel = ma->rel_max_cache_bad_fit;

	erts_circleq_foreach(c, &(ma->cache_unpowered_node)) {
	    csize = c->size;
	    if (csize >= size) {
		if (((csize - size)*100 < bad_max_rel*size) && (csize - size) < bad_max_abs ) {

		    seg = c->seg;

		    erts_circleq_remove(c);

		    ma->cache_size--;
		    ma->cache_hits++;

		    mseg_cache_clear_node(c);
		    erts_circleq_push_head(&(ma->cache_free), c);

		    *size_p = csize;

		    return seg;

		} else if (!best || (csize - size) < bdiff) {
		    best  = c;
		    bdiff = csize - size;
		}
	    }
	}

	/* No cached segment met our criteria, use the best one found and trim it */

	if (best) {

	    seg   = best->seg;
	    csize = best->size;

	    ASSERT(best->seg);
	    ASSERT(best->size > 0);

	    ma->cache_hits++;

	    /* Use current cache placement for remaining segment space */

	    best->seg  = ((char *)seg) + size;
	    best->size = csize - size;

	    ASSERT((size % GET_PAGE_SIZE) == 0);
	    ASSERT((best->size % GET_PAGE_SIZE) == 0);

	    *size_p = size;

	    return seg;

	}
    }
    return NULL;
}

/* *_mseg_check_*_cache
 * Slowly remove segments cached in the allocator by
 * using callbacks from aux-work in the scheduler.
 */

static ERTS_INLINE Uint mseg_drop_one_cache_size(ErtsMsegAllctr_t *ma, Uint flags, cache_t *head) {
    cache_t *c = NULL;

    c = erts_circleq_tail(head);
    erts_circleq_remove(c);

    if (erts_mtrace_enabled)
	erts_mtrace_crr_free(SEGTYPE, SEGTYPE, c->seg);

    mseg_destroy(ma, flags, c->seg, c->size);
    mseg_cache_clear_node(c);
    erts_circleq_push_head(&(ma->cache_free), c);

    ma->segments.current.watermark--;
    ma->cache_size--;

    ASSERT(ma->cache_size >= 0);

    return ma->cache_size;
}

static ERTS_INLINE Uint mseg_drop_cache_size(ErtsMsegAllctr_t *ma, Uint flags, cache_t *head) {
    cache_t *c = NULL;

    while (!erts_circleq_is_empty(head)) {

	c = erts_circleq_tail(head);
	erts_circleq_remove(c);

	if (erts_mtrace_enabled)
	    erts_mtrace_crr_free(SEGTYPE, SEGTYPE, c->seg);

	mseg_destroy(ma, flags, c->seg, c->size);

	mseg_cache_clear_node(c);
	erts_circleq_push_head(&(ma->cache_free), c);

	ma->segments.current.watermark--;
	ma->cache_size--;
    }

    ASSERT(ma->cache_size >= 0);

    return ma->cache_size;
}

/* mseg_check_cache
 * - Check if we can empty some cached segments in this allocator
 */


static Uint mseg_check_cache(ErtsMsegAllctr_t *ma) {
    int i;

    ERTS_DBG_MA_CHK_THR_ACCESS(ma);

    for (i = 0; i < CACHE_AREAS; i++) {
	if (!erts_circleq_is_empty(&(ma->cache_powered_node[i])))
	    return mseg_drop_one_cache_size(ma, ERTS_MSEG_FLG_2POW, &(ma->cache_powered_node[i]));
    }

    if (!erts_circleq_is_empty(&(ma->cache_unpowered_node)))
	return mseg_drop_one_cache_size(ma, ERTS_MSEG_FLG_NONE, &(ma->cache_unpowered_node));

    return 0;
}

/* mseg_cache_check
 * - Check if we have some cache we can purge
 */

static void mseg_cache_check(ErtsMsegAllctr_t *ma) {
    Uint empty_cache = 1;

    ERTS_MSEG_LOCK(ma);

    if (mseg_check_cache(ma))
        empty_cache = 0;

    /* If all MemKinds caches are empty,
     * remove aux-work callback
     */
    if (empty_cache) {
	ma->is_cache_check_scheduled = 0;
	erts_set_aux_work_timeout(ma->ix, ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK, 0);
    }

    INC_CC(ma, check_cache);

    ERTS_MSEG_UNLOCK(ma);
}

/* erts_mseg_cache_check
 * - This is a callback that is scheduled as aux-work from
 *   schedulers and is called at some interval if we have a cache
 *   on this mseg-allocator.
 * - Purpose: Empty cache slowly so we don't collect mapped areas
 *   and bloat memory.
 */

void erts_mseg_cache_check(void) {
    mseg_cache_check(ERTS_MSEG_ALLCTR_SS());
}


/* mseg_clear_cache
 * Remove cached segments from the allocator completely
 */


static void mseg_clear_cache(ErtsMsegAllctr_t *ma) {
    int i;
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);

    /* drop pow2 caches */
    for (i = 0; i < CACHE_AREAS; i++) {
	if (erts_circleq_is_empty(&(ma->cache_powered_node[i])))
	    continue;

	mseg_drop_cache_size(ma, ERTS_MSEG_FLG_2POW, &(ma->cache_powered_node[i]));
	ASSERT(erts_circleq_is_empty(&(ma->cache_powered_node[i])));
    }
    /* drop varied caches */
    if (!erts_circleq_is_empty(&(ma->cache_unpowered_node)))
	mseg_drop_cache_size(ma, ERTS_MSEG_FLG_NONE, &(ma->cache_unpowered_node));

    ASSERT(erts_circleq_is_empty(&(ma->cache_unpowered_node)));
    ASSERT(ma->cache_size == 0);

    INC_CC(ma, clear_cache);
    ERTS_MSEG_UNLOCK(ma);
}

void erts_mseg_clear_cache(void) {
    mseg_clear_cache(ERTS_MSEG_ALLCTR_SS());
    mseg_clear_cache(ERTS_MSEG_ALLCTR_IX(0));
}

static void *
mseg_alloc(ErtsMsegAllctr_t *ma, ErtsAlcType_t atype, UWord *size_p,
	   Uint flags, const ErtsMsegOpt_t *opt)
{
    UWord size;
    void *seg;

    INC_CC(ma, alloc);

    if (!MSEG_FLG_IS_2POW(flags))
	size = ERTS_PAGEALIGNED_CEILING(*size_p);
    else {
	size = ALIGNED_CEILING(*size_p);
	if (!IS_2POW(size)) {
	    /* Cache optim (if applicable) */
	    size = ceil_2pow(size);
	}
    }

    if (opt->cache && ma->cache_size > 0 && (seg = cache_get_segment(ma, &size, flags)) != NULL)
	goto done;

    seg = mseg_create(ma, flags, &size);

    if (!seg)
	*size_p = 0;
    else {
done:
	*size_p = size;
	if (erts_mtrace_enabled)
	    erts_mtrace_crr_alloc(seg, atype, ERTS_MTRACE_SEGMENT_ID, size);

	ERTS_MSEG_ALLOC_STAT(ma,size);
    }

    return seg;
}


static void
mseg_dealloc(ErtsMsegAllctr_t *ma, ErtsAlcType_t atype, void *seg, UWord size,
	     Uint flags, const ErtsMsegOpt_t *opt)
{
    ERTS_MSEG_DEALLOC_STAT(ma,size);

    if (opt->cache && cache_bless_segment(ma, seg, size, flags)) {
	schedule_cache_check(ma);
	goto done;
    }

    if (erts_mtrace_enabled)
	erts_mtrace_crr_free(atype, SEGTYPE, seg);

    mseg_destroy(ma, flags, seg, size);

done:

    INC_CC(ma, dealloc);
}

static void *
mseg_realloc(ErtsMsegAllctr_t *ma, ErtsAlcType_t atype, void *seg,
	     UWord old_size, UWord *new_size_p, Uint flags, const ErtsMsegOpt_t *opt)
{
    void *new_seg;
    UWord new_size;

    /* Just allocate a new segment if we didn't have one before */
    if (!seg || !old_size) {
	new_seg = mseg_alloc(ma, atype, new_size_p, flags, opt);
	DEC_CC(ma, alloc);
	return new_seg;
    }


    /* Dealloc old segment if new segment is of size 0 */
    if (!(*new_size_p)) {
	mseg_dealloc(ma, atype, seg, old_size, flags, opt);
	DEC_CC(ma, dealloc);
	return NULL;
    }

    new_seg = seg;

    if (!MSEG_FLG_IS_2POW(flags))
	new_size = ERTS_PAGEALIGNED_CEILING(*new_size_p);
    else {
	new_size = ALIGNED_CEILING(*new_size_p);
	if (!IS_2POW(new_size)) {
	    /* Cache optim (if applicable) */
	    new_size = ceil_2pow(new_size);
	}
    }

    if (new_size > old_size) {
	if (opt->preserv) {
	    new_seg = mseg_recreate(ma, flags, (void *) seg, old_size, &new_size);
	    if (!new_seg)
		new_size = old_size;
	}
	else {
	    mseg_dealloc(ma, atype, seg, old_size, flags, opt);
	    new_seg = mseg_alloc(ma, atype, &new_size, flags, opt);
	    if (!new_seg)
		new_size = 0;
	}
    }
    else if (new_size < old_size) {
	UWord shrink_sz = old_size - new_size;

	/* +M<S>rsbcst <ratio> */
	if (shrink_sz < opt->abs_shrink_th
	    && 100*shrink_sz < opt->rel_shrink_th*old_size) {
	    new_size = old_size;
	}
	else {
	    new_seg = mseg_recreate(ma, flags, (void *) seg, old_size, &new_size);
	    if (!new_seg)
		new_size = old_size;
	}
    }

    if (erts_mtrace_enabled)
	erts_mtrace_crr_realloc(new_seg, atype, SEGTYPE, seg, new_size);

    INC_CC(ma, realloc);

    ASSERT(!MSEG_FLG_IS_2POW(flags) || IS_2POW(new_size));
    *new_size_p = new_size;

    ERTS_MSEG_REALLOC_STAT(ma, old_size, new_size);

    return new_seg;
}

/* --- Info stuff ---------------------------------------------------------- */

static struct {
    Eterm version;

    Eterm options;
    Eterm amcbf;
    Eterm rmcbf;
    Eterm mcs;

    Eterm memkind;
    Eterm name;
    Eterm status;
    Eterm cached_segments;
    Eterm cache_hits;
    Eterm segments;
    Eterm segments_size;
    Eterm segments_watermark;


    Eterm calls;
    Eterm mseg_alloc;
    Eterm mseg_dealloc;
    Eterm mseg_realloc;
    Eterm mseg_create;
    Eterm mseg_create_resize;
    Eterm mseg_destroy;
    Eterm mseg_recreate;
    Eterm mseg_clear_cache;
    Eterm mseg_check_cache;

#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static void ERTS_INLINE atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void
init_atoms(ErtsMsegAllctr_t *ma)
{
#ifdef DEBUG
    Eterm *atom;
#endif
    erts_mtx_lock(&init_atoms_mutex);

    if (!atoms_initialized) {
#ifdef DEBUG
	for (atom = (Eterm *) &am; atom <= &am.end_of_atoms; atom++) {
	    *atom = THE_NON_VALUE;
	}
#endif

	AM_INIT(version);
	AM_INIT(memkind);
	AM_INIT(name);

	AM_INIT(options);
	AM_INIT(amcbf);
	AM_INIT(rmcbf);
	AM_INIT(mcs);

	AM_INIT(status);
	AM_INIT(cached_segments);
	AM_INIT(cache_hits);
	AM_INIT(segments);
	AM_INIT(segments_size);
	AM_INIT(segments_watermark);

	AM_INIT(calls);
	AM_INIT(mseg_alloc);
	AM_INIT(mseg_dealloc);
	AM_INIT(mseg_realloc);
	AM_INIT(mseg_create);
	AM_INIT(mseg_create_resize);
	AM_INIT(mseg_destroy);
	AM_INIT(mseg_recreate);
	AM_INIT(mseg_clear_cache);
	AM_INIT(mseg_check_cache);

#ifdef DEBUG
	for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	    ASSERT(*atom != THE_NON_VALUE);
	}
#endif
    }

    atoms_initialized = 1;
    erts_mtx_unlock(&init_atoms_mutex);
}

#define bld_uint	erts_bld_uint
#define bld_cons	erts_bld_cons
#define bld_tuple	erts_bld_tuple
#define bld_string	erts_bld_string
#define bld_2tup_list	erts_bld_2tup_list

/*
 * bld_unstable_uint() (instead of bld_uint()) is used when values may
 * change between size check and actual build. This because a value
 * that would fit a small when size check is done may need to be built
 * as a big when the actual build is performed. Caller is required to
 * HRelease after build.
 */
static ERTS_INLINE Eterm
bld_unstable_uint(Uint **hpp, Uint *szp, Uint ui)
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += BIG_UINT_HEAP_SIZE;
    if (hpp) {
	if (IS_USMALL(0, ui))
	    res = make_small(ui);
	else {
	    res = uint_to_big(ui, *hpp);
	    *hpp += BIG_UINT_HEAP_SIZE;
	}
    }
    return res;
}

static ERTS_INLINE void
add_2tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 2, el1, el2), *lp);
}

static ERTS_INLINE void
add_3tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2, Eterm el3)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 3, el1, el2, el3), *lp);
}

static ERTS_INLINE void
add_4tup(Uint **hpp, Uint *szp, Eterm *lp,
	 Eterm el1, Eterm el2, Eterm el3, Eterm el4)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 4, el1, el2, el3, el4), *lp);
}


static Eterm
info_options(ErtsMsegAllctr_t *ma,
	     char *prefix,
	     int *print_to_p,
	     void *print_to_arg,
	     Uint **hpp,
	     Uint *szp)
{
    Eterm res = NIL;

    if (print_to_p) {
	int to = *print_to_p;
	void *arg = print_to_arg;
	erts_print(to, arg, "%samcbf: %beu\n", prefix, ma->abs_max_cache_bad_fit);
	erts_print(to, arg, "%srmcbf: %beu\n", prefix, ma->rel_max_cache_bad_fit);
	erts_print(to, arg, "%smcs: %beu\n", prefix, ma->max_cache_size);
    }

    if (hpp || szp) {

	if (!atoms_initialized)
	    init_atoms(ma);

	add_2tup(hpp, szp, &res,
		 am.mcs,
		 bld_uint(hpp, szp, ma->max_cache_size));
	add_2tup(hpp, szp, &res,
		 am.rmcbf,
		 bld_uint(hpp, szp, ma->rel_max_cache_bad_fit));
	add_2tup(hpp, szp, &res,
		 am.amcbf,
		 bld_uint(hpp, szp, ma->abs_max_cache_bad_fit));

    }

    return res;
}

static Eterm
info_calls(ErtsMsegAllctr_t *ma, int *print_to_p, void *print_to_arg, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (print_to_p) {

#define PRINT_CC(TO, TOA, CC)							\
    if (ma->calls.CC.giga_no == 0)						\
	erts_print(TO, TOA, "mseg_%s calls: %b32u\n", #CC, ma->calls.CC.no);	\
    else									\
	erts_print(TO, TOA, "mseg_%s calls: %b32u%09b32u\n", #CC,		\
		   ma->calls.CC.giga_no, ma->calls.CC.no)

	int to = *print_to_p;
	void *arg = print_to_arg;

	PRINT_CC(to, arg, alloc);
	PRINT_CC(to, arg, dealloc);
	PRINT_CC(to, arg, realloc);
	PRINT_CC(to, arg, create);
	PRINT_CC(to, arg, create_resize);
	PRINT_CC(to, arg, destroy);
	PRINT_CC(to, arg, recreate);
	PRINT_CC(to, arg, clear_cache);
	PRINT_CC(to, arg, check_cache);

#undef PRINT_CC

    }

    if (hpp || szp) {

	res = NIL;

	add_3tup(hpp, szp, &res,
		 am.mseg_check_cache,
		 bld_unstable_uint(hpp, szp, ma->calls.check_cache.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.check_cache.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_clear_cache,
		 bld_unstable_uint(hpp, szp, ma->calls.clear_cache.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.clear_cache.no));

	add_3tup(hpp, szp, &res,
		 am.mseg_recreate,
		 bld_unstable_uint(hpp, szp, ma->calls.recreate.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.recreate.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_destroy,
		 bld_unstable_uint(hpp, szp, ma->calls.destroy.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.destroy.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_create,
		 bld_unstable_uint(hpp, szp, ma->calls.create.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.create.no));

	add_3tup(hpp, szp, &res,
		 am.mseg_create_resize,
		 bld_unstable_uint(hpp, szp, ma->calls.create_resize.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.create_resize.no));

	add_3tup(hpp, szp, &res,
		 am.mseg_realloc,
		 bld_unstable_uint(hpp, szp, ma->calls.realloc.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.realloc.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_dealloc,
		 bld_unstable_uint(hpp, szp, ma->calls.dealloc.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.dealloc.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_alloc,
		 bld_unstable_uint(hpp, szp, ma->calls.alloc.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.alloc.no));
    }

    return res;
}

static Eterm
info_status(ErtsMsegAllctr_t *ma, int *print_to_p, void *print_to_arg,
	    int begin_new_max_period, int only_sz, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    
    if (ma->segments.max_ever.no < ma->segments.max.no)
	ma->segments.max_ever.no = ma->segments.max.no;
    if (ma->segments.max_ever.sz < ma->segments.max.sz)
	ma->segments.max_ever.sz = ma->segments.max.sz;

    if (print_to_p) {
	int to = *print_to_p;
	void *arg = print_to_arg;

        if (!only_sz) {
            erts_print(to, arg, "cached_segments: %beu\n", ma->cache_size);
            erts_print(to, arg, "cache_hits: %beu\n", ma->cache_hits);
            erts_print(to, arg, "segments: %beu %beu %beu\n",
                       ma->segments.current.no, ma->segments.max.no, ma->segments.max_ever.no);
            erts_print(to, arg, "segments_watermark: %beu\n",
                       ma->segments.current.watermark);
        }
        erts_print(to, arg, "segments_size: %beu %beu %beu\n",
		   ma->segments.current.sz, ma->segments.max.sz, ma->segments.max_ever.sz);
    }

    if (hpp || szp) {
	res = NIL;
        add_4tup(hpp, szp, &res,
                 am.segments_size,
                 bld_unstable_uint(hpp, szp, ma->segments.current.sz),
                 bld_unstable_uint(hpp, szp, ma->segments.max.sz),
                 bld_unstable_uint(hpp, szp, ma->segments.max_ever.sz));
        if (!only_sz) {
            add_2tup(hpp, szp, &res,
                     am.segments_watermark,
                     bld_unstable_uint(hpp, szp, ma->segments.current.watermark));
            add_4tup(hpp, szp, &res,
                     am.segments,
                     bld_unstable_uint(hpp, szp, ma->segments.current.no),
                     bld_unstable_uint(hpp, szp, ma->segments.max.no),
                     bld_unstable_uint(hpp, szp, ma->segments.max_ever.no));
            add_2tup(hpp, szp, &res,
                     am.cache_hits,
                     bld_unstable_uint(hpp, szp, ma->cache_hits));
            add_2tup(hpp, szp, &res,
                     am.cached_segments,
                     bld_unstable_uint(hpp, szp, ma->cache_size));
        }
    }

    if (begin_new_max_period) {
	ma->segments.max.no = ma->segments.current.no;
	ma->segments.max.sz = ma->segments.current.sz;
    }

    return res;
}

static Eterm info_memkind(ErtsMsegAllctr_t *ma, int *print_to_p, void *print_to_arg,
			  int begin_max_per, int only_sz, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    Eterm atoms[3];
    Eterm values[3];

    if (!only_sz) {
        if (print_to_p) {
            erts_print(*print_to_p, print_to_arg, "memory kind: %s\n", "all memory");
        }
        if (hpp || szp) {
            atoms[0] = am.name;
            atoms[1] = am.status;
            atoms[2] = am.calls;
            values[0] = erts_bld_string(hpp, szp, "all memory");
        }
    }
    res = info_status(ma, print_to_p, print_to_arg, begin_max_per, only_sz, hpp, szp);
    if (!only_sz) {
        values[1] = res;
        values[2] = info_calls(ma, print_to_p, print_to_arg, hpp, szp);

        if (hpp || szp)
            res = bld_2tup_list(hpp, szp, 3, atoms, values);
    }

    return res;
}

static Eterm
info_version(ErtsMsegAllctr_t *ma, int *print_to_p, void *print_to_arg, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (print_to_p) {
	erts_print(*print_to_p, print_to_arg, "version: %s\n",
		   ERTS_MSEG_VSN_STR);
    }

    if (hpp || szp) {
	res = bld_string(hpp, szp, ERTS_MSEG_VSN_STR);
    }

    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Exported functions                                                        *
\*                                                                           */

Eterm
erts_mseg_info_options(int ix,
		       int *print_to_p, void *print_to_arg,
		       Uint **hpp, Uint *szp)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_IX(ix);
    Eterm res;

    res = info_options(ma, "option ", print_to_p, print_to_arg, hpp, szp);

    return res;
}

Eterm
erts_mseg_info(int ix,
	       int *print_to_p,
	       void *print_to_arg,
	       int begin_max_per,
               int only_sz,
	       Uint **hpp,
	       Uint *szp)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_IX(ix);
    Eterm res = THE_NON_VALUE;
    Eterm atoms[4];
    Eterm values[4];
    Uint n = 0;

    if (hpp || szp) {
        if (!atoms_initialized)
            init_atoms(ma);
    }
    if (!only_sz) {
        if (hpp || szp) {
            atoms[0] = am.version;
            atoms[1] = am.options;
            atoms[2] = am.memkind;
        }
        values[n++] = info_version(ma, print_to_p, print_to_arg, hpp, szp);
        values[n++] = info_options(ma, "option ", print_to_p, print_to_arg, hpp, szp);
    }

    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);

    res = info_memkind(ma, print_to_p, print_to_arg, begin_max_per, only_sz, hpp, szp);

    if (!only_sz) {
        values[n++] = res;
        if (hpp || szp)
            res = bld_2tup_list(hpp, szp, n, atoms, values);
    }

    ERTS_MSEG_UNLOCK(ma);

    return res;
}

void *
erts_mseg_alloc_opt(ErtsAlcType_t atype, UWord *size_p, Uint flags, const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);
    void *seg;
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    seg = mseg_alloc(ma, atype, size_p, flags, opt);
    ERTS_MSEG_UNLOCK(ma);
    HARD_DBG_INSERT_MSEG(seg, *size_p);
    return seg;
}

void *
erts_mseg_alloc(ErtsAlcType_t atype, UWord *size_p, Uint flags)
{
    return erts_mseg_alloc_opt(atype, size_p, flags, &erts_mseg_default_opt);
}

void
erts_mseg_dealloc_opt(ErtsAlcType_t atype, void *seg,
		      UWord size, Uint flags, const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);

    HARD_DBG_REMOVE_MSEG(seg, size);
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    mseg_dealloc(ma, atype, seg, size, flags, opt);
    ERTS_MSEG_UNLOCK(ma);
}

void
erts_mseg_dealloc(ErtsAlcType_t atype, void *seg, UWord size, Uint flags)
{
    erts_mseg_dealloc_opt(atype, seg, size, flags, &erts_mseg_default_opt);
}

void *
erts_mseg_realloc_opt(ErtsAlcType_t atype, void *seg,
		      UWord old_size, UWord *new_size_p,
		      Uint flags,
		      const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);
    void *new_seg;

    HARD_DBG_REMOVE_MSEG(seg, old_size);
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    new_seg = mseg_realloc(ma, atype, seg, old_size, new_size_p, flags, opt);
    ERTS_MSEG_UNLOCK(ma);
    HARD_DBG_INSERT_MSEG(new_seg, *new_size_p);
    return new_seg;
}

void *
erts_mseg_realloc(ErtsAlcType_t atype, void *seg,
		  UWord old_size, UWord *new_size_p, Uint flags)
{
    return erts_mseg_realloc_opt(atype, seg, old_size, new_size_p,
				 flags, &erts_mseg_default_opt);
}

Uint
erts_mseg_no(const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);
    Uint n;
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    n = ma->segments.current.no;
    ERTS_MSEG_UNLOCK(ma);
    return n;
}

Uint
erts_mseg_unit_size(void)
{
    return MSEG_ALIGNED_SIZE;
}


static void mem_cache_init(ErtsMsegAllctr_t *ma)
{
    int i;

    /* Clear all cache headers */
    mseg_cache_clear_node(&(ma->cache_free));
    mseg_cache_clear_node(&(ma->cache_unpowered_node));

    for (i = 0; i < CACHE_AREAS; i++) {
	mseg_cache_clear_node(&(ma->cache_powered_node[i]));
    }

    /* Populate cache free list */

    ASSERT(ma->max_cache_size <= MAX_CACHE_SIZE);

    for (i = 0; i < ma->max_cache_size; i++) {
	mseg_cache_clear_node(&(ma->cache[i]));
	erts_circleq_push_head(&(ma->cache_free), &(ma->cache[i]));
    }

    ma->cache_size = 0;
    ma->cache_hits = 0;

    ma->segments.current.watermark = 0;
    ma->segments.current.no = 0;
    ma->segments.current.sz = 0;
    ma->segments.max.no = 0;
    ma->segments.max.sz = 0;
    ma->segments.max_ever.no = 0;
    ma->segments.max_ever.sz = 0;
}

void
erts_mseg_init(ErtsMsegInit_t *init)
{
    int i;
    UWord x;

#ifdef ERTS_SMP
    no_mseg_allocators = init->nos + 1;
#else
    no_mseg_allocators = 1;
#endif

    x = (UWord) malloc(sizeof(ErtsAlgndMsegAllctr_t)
		       *no_mseg_allocators
		       + (ERTS_CACHE_LINE_SIZE-1));
    if (x & ERTS_CACHE_LINE_MASK)
	x = (x & ~ERTS_CACHE_LINE_MASK) + ERTS_CACHE_LINE_SIZE;
    ASSERT((x & ERTS_CACHE_LINE_MASK) == 0);
    aligned_mseg_allctr = (ErtsAlgndMsegAllctr_t *) x;

    atoms_initialized = 0;

    erts_mtx_init(&init_atoms_mutex, "mseg_init_atoms");

#ifdef ERTS_ALC_A_EXEC
    /* Initialize erts_exec_mapper *FIRST*, to increase probability
     * of getting low memory for HiPE AMD64's small code model.
     */
    erts_mmap_init(&erts_exec_mmapper, &init->exec_mmap, 1);
#endif
    erts_mmap_init(&erts_dflt_mmapper, &init->dflt_mmap, 0);
#if defined(ARCH_64) && defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
    erts_mmap_init(&erts_literal_mmapper, &init->literal_mmap, 0);
#endif

    if (!IS_2POW(GET_PAGE_SIZE))
	erts_exit(ERTS_ABORT_EXIT, "erts_mseg: Unexpected page_size %beu\n", GET_PAGE_SIZE);

    ASSERT((MSEG_ALIGNED_SIZE % GET_PAGE_SIZE) == 0);

    for (i = 0; i < no_mseg_allocators; i++) {
	ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_IX(i);

	ma->ix = i;

	ma->is_init_done = 0;

	if (i != 0)
	    ma->is_thread_safe = 0;
	else {
	    ma->is_thread_safe = 1;
	    erts_mtx_init(&ma->mtx, "mseg");
	}

	ma->is_cache_check_scheduled = 0;

	/* Options ... */

	ma->abs_max_cache_bad_fit = init->amcbf;
	ma->rel_max_cache_bad_fit = init->rmcbf;
	ma->max_cache_size = init->mcs;

	if (ma->max_cache_size > MAX_CACHE_SIZE)
	    ma->max_cache_size = MAX_CACHE_SIZE;

	mem_cache_init(ma);

	sys_memzero((void *) &ma->calls, sizeof(ErtsMsegCalls));
    }
}


static ERTS_INLINE Uint tot_cache_size(ErtsMsegAllctr_t *ma)
{
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    return ma->cache_size;
}

/*
 * erts_mseg_late_init() have to be called after all allocators,
 * threads and timers have been initialized.
 */
void
erts_mseg_late_init(void)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_SS();
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    ma->is_init_done = 1;
    if (tot_cache_size(ma))
	schedule_cache_check(ma);
    ERTS_MSEG_UNLOCK(ma);
}

#endif /* #if HAVE_ERTS_MSEG */

UWord
erts_mseg_test(UWord op, UWord a1, UWord a2, UWord a3)
{
    switch (op) {
#if HAVE_ERTS_MSEG
    case 0x400: /* Have erts_mseg */
	return (UWord) 1;
    case 0x401:
	return (UWord) erts_mseg_alloc(ERTS_ALC_A_INVALID, (UWord *) a1, (Uint) 0);
    case 0x402:
	erts_mseg_dealloc(ERTS_ALC_A_INVALID, (void *) a1, (Uint) a2, (Uint) 0);
	return (UWord) 0;
    case 0x403:
	return (UWord) erts_mseg_realloc(ERTS_ALC_A_INVALID,
						 (void *) a1,
						 (Uint) a2,
						 (UWord *) a3,
						 (Uint) 0);
    case 0x404:
	erts_mseg_clear_cache();
	return (UWord) 0;
    case 0x405:
	return (UWord) erts_mseg_no(&erts_mseg_default_opt);
    case 0x406: {
	ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_IX(0);
	UWord res;
	ERTS_MSEG_LOCK(ma);
	res = (UWord) tot_cache_size(ma);
	ERTS_MSEG_UNLOCK(ma);
	return res;
    }
#else /* #if HAVE_ERTS_MSEG */
    case 0x400: /* Have erts_mseg */
	return (UWord) 0;
#endif /* #if HAVE_ERTS_MSEG */
    default:	ASSERT(0); return ~((UWord) 0);
    }

}
