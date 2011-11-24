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

#include "sys.h"
#include "erl_mseg.h"
#include "global.h"
#include "erl_threads.h"
#include "erl_mtrace.h"
#include "erl_time.h"
#include "erl_alloc.h"
#include "big.h"
#include "erl_thr_progress.h"

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

#define MAX_CACHE_SIZE 30

#undef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#undef MAX
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

#undef  PAGE_MASK
#define INV_PAGE_MASK	((Uint) (page_size - 1))
#define PAGE_MASK	(~INV_PAGE_MASK)
#define PAGE_FLOOR(X)	((X) & PAGE_MASK)
#define PAGE_CEILING(X)	PAGE_FLOOR((X) + INV_PAGE_MASK)
#define PAGES(X)	((X) >> page_shift)

static int atoms_initialized;

typedef struct mem_kind_t MemKind;

static void mseg_clear_cache(MemKind*);

#if HALFWORD_HEAP
static int initialize_pmmap(void);
static void *pmmap(size_t size);
static int pmunmap(void *p, size_t size);
static void *pmremap(void *old_address, size_t old_size,
		     size_t new_size);
#endif

#if HAVE_MMAP
/* Mmap ... */

#define MMAP_PROT		(PROT_READ|PROT_WRITE)


#ifdef MAP_ANON
#  define MMAP_FLAGS		(MAP_ANON|MAP_PRIVATE)
#  define MMAP_FD		(-1)
#else
#  define MMAP_FLAGS		(MAP_PRIVATE)
#  define MMAP_FD		mmap_fd
static int mmap_fd;
#endif

#if HAVE_MREMAP
#  define HAVE_MSEG_RECREATE 1
#else
#  define HAVE_MSEG_RECREATE 0
#endif

#if HALFWORD_HEAP
#define CAN_PARTLY_DESTROY 0
#else
#define CAN_PARTLY_DESTROY 1
#endif
#else  /* #if HAVE_MMAP */
#define CAN_PARTLY_DESTROY 0
#error "Not supported"
#endif /* #if HAVE_MMAP */

#if defined(ERTS_MSEG_FAKE_SEGMENTS) && HALFWORD_HEAP
#  warning "ERTS_MSEG_FAKE_SEGMENTS will only be used for high memory segments" 
#endif 

#if defined(ERTS_MSEG_FAKE_SEGMENTS)
#undef CAN_PARTLY_DESTROY
#define CAN_PARTLY_DESTROY 0
#endif

const ErtsMsegOpt_t erts_mseg_default_opt = {
    1,			/* Use cache		     */
    1,			/* Preserv data		     */
    0,			/* Absolute shrink threshold */
    0,			/* Relative shrink threshold */
    0			/* Scheduler specific        */
#if HALFWORD_HEAP
    ,0                  /* need low memory */
#endif
};


typedef struct cache_desc_t_ {
    void *seg;
    Uint size;
    struct cache_desc_t_ *next;
    struct cache_desc_t_ *prev;
} cache_desc_t;

typedef struct {
    Uint32 giga_no;
    Uint32 no;
} CallCounter;

static Uint page_size;
static Uint page_shift;

typedef struct {
    CallCounter alloc;
    CallCounter dealloc;
    CallCounter realloc;
    CallCounter create;
    CallCounter destroy;
#if HAVE_MSEG_RECREATE
    CallCounter recreate;
#endif
    CallCounter clear_cache;
    CallCounter check_cache;
} ErtsMsegCalls;

typedef struct ErtsMsegAllctr_t_ ErtsMsegAllctr_t;

struct mem_kind_t {
    cache_desc_t cache_descs[MAX_CACHE_SIZE];
    cache_desc_t *free_cache_descs;
    cache_desc_t *cache;
    cache_desc_t *cache_end;

    Uint cache_size;
    Uint min_cached_seg_size;
    Uint max_cached_seg_size;
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

    ErtsMsegAllctr_t *ma;
    const char* name;
    MemKind* next;
};/*MemKind*/

struct ErtsMsegAllctr_t_ {
    int ix;

    int is_init_done;
    int is_thread_safe;
    erts_mtx_t mtx;

    int is_cache_check_scheduled;

    MemKind* mk_list;

#if HALFWORD_HEAP
    MemKind low_mem;
    MemKind hi_mem;
#else
    MemKind the_mem;
#endif

    Uint max_cache_size;
    Uint abs_max_cache_bad_fit;
    Uint rel_max_cache_bad_fit;

    ErtsMsegCalls calls;

#if CAN_PARTLY_DESTROY
    Uint min_seg_size;
#endif

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
schedule_cache_check(ErtsMsegAllctr_t *ma)
{

    if (!ma->is_cache_check_scheduled && ma->is_init_done) {
	erts_set_aux_work_timeout(ma->ix,
				  ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK,
				  1);
	ma->is_cache_check_scheduled = 1;
    }
}

static ERTS_INLINE void *
mseg_create(ErtsMsegAllctr_t *ma, MemKind* mk, Uint size)
{
    void *seg;

    ASSERT(size % page_size == 0);

#if HALFWORD_HEAP
    if (mk == &ma->low_mem) {
	seg = pmmap(size);
	if ((unsigned long) seg & CHECK_POINTER_MASK) {
	    erts_fprintf(stderr,"Pointer mask failure (0x%08lx)\n",(unsigned long) seg);
	    return NULL;
	}
    }
    else
#endif
    {
#if defined(ERTS_MSEG_FAKE_SEGMENTS)
	seg = erts_sys_alloc(ERTS_ALC_N_INVALID, NULL, size);
#elif HAVE_MMAP
	{
	    seg = (void *) mmap((void *) 0, (size_t) size,
				MMAP_PROT, MMAP_FLAGS, MMAP_FD, 0);
	    if (seg == (void *) MAP_FAILED)
		seg = NULL;
	}
#else
# error "Missing mseg_create() implementation"
#endif
    }

    INC_CC(ma, create);

    return seg;
}

static ERTS_INLINE void
mseg_destroy(ErtsMsegAllctr_t *ma, MemKind* mk, void *seg, Uint size)
{
#ifdef DEBUG
    int res;
#endif

#if HALFWORD_HEAP
    if (mk == &ma->low_mem) {
#ifdef DEBUG
	res = 
#endif
	    pmunmap((void *) seg, size);
    }
    else
#endif
    {
#ifdef ERTS_MSEG_FAKE_SEGMENTS
	erts_sys_free(ERTS_ALC_N_INVALID, NULL, seg);
#ifdef DEBUG
	res = 0;
#endif
#elif HAVE_MMAP
#ifdef DEBUG
	res = 
#endif
	    munmap((void *) seg, size);
#else
# error "Missing mseg_destroy() implementation"
#endif
    }

    ASSERT(size % page_size == 0);
    ASSERT(res == 0);

    INC_CC(ma, destroy);

}

#if HAVE_MSEG_RECREATE

static ERTS_INLINE void *
mseg_recreate(ErtsMsegAllctr_t *ma, MemKind* mk, void *old_seg, Uint old_size, Uint new_size)
{
    void *new_seg;

    ASSERT(old_size % page_size == 0);
    ASSERT(new_size % page_size == 0);

#if HALFWORD_HEAP
    if (mk == &ma->low_mem) {
	new_seg = (void *) pmremap((void *) old_seg,
				   (size_t) old_size,
				   (size_t) new_size);
    }
    else
#endif
    {
#if defined(ERTS_MSEG_FAKE_SEGMENTS)
	new_seg = erts_sys_realloc(ERTS_ALC_N_INVALID, NULL, old_seg, new_size);
#elif HAVE_MREMAP

    #if defined(__NetBSD__)
	new_seg = (void *) mremap((void *) old_seg,
				  (size_t) old_size,
				  NULL,
				  (size_t) new_size,
				  0);
    #else
	new_seg = (void *) mremap((void *) old_seg,
				  (size_t) old_size,
				  (size_t) new_size,
				  MREMAP_MAYMOVE);
    #endif
	if (new_seg == (void *) MAP_FAILED)
	    new_seg = NULL;
#else
#error "Missing mseg_recreate() implementation"
#endif
    }

    INC_CC(ma, recreate);

    return new_seg;
}

#endif /* #if HAVE_MSEG_RECREATE */

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
#define ERTS_DBG_MK_CHK_THR_ACCESS(MK) \
  ERTS_DBG_MA_CHK_THR_ACCESS((MK)->ma)
#else
#define ERTS_DBG_MA_CHK_THR_ACCESS(MA)
#define ERTS_DBG_MK_CHK_THR_ACCESS(MK)
#endif

static ERTS_INLINE cache_desc_t * 
alloc_cd(MemKind* mk)
{    
    cache_desc_t *cd = mk->free_cache_descs;
    ERTS_DBG_MK_CHK_THR_ACCESS(mk);
    if (cd)
	mk->free_cache_descs = cd->next;
    return cd;
}

static ERTS_INLINE void
free_cd(MemKind* mk, cache_desc_t *cd)
{
    ERTS_DBG_MK_CHK_THR_ACCESS(mk);
    cd->next = mk->free_cache_descs;
    mk->free_cache_descs = cd;
}


static ERTS_INLINE void
link_cd(MemKind* mk, cache_desc_t *cd)
{
    ERTS_DBG_MK_CHK_THR_ACCESS(mk);
    if (mk->cache)
	mk->cache->prev = cd;
    cd->next = mk->cache;
    cd->prev = NULL;
    mk->cache = cd;

    if (!mk->cache_end) {
	ASSERT(!cd->next);
	mk->cache_end = cd;
    }

    mk->cache_size++;
}

#if CAN_PARTLY_DESTROY
static ERTS_INLINE void
end_link_cd(MemKind* mk, cache_desc_t *cd)
{
    ERTS_DBG_MK_CHK_THR_ACCESS(mk);
    if (mk->cache_end)
	mk->cache_end->next = cd;
    cd->next = NULL;
    cd->prev = mk->cache_end;
    mk->cache_end = cd;

    if (!mk->cache) {
	ASSERT(!cd->prev);
	mk->cache = cd;
    }

    mk->cache_size++;
}
#endif

static ERTS_INLINE void
unlink_cd(MemKind* mk, cache_desc_t *cd)
{
    ERTS_DBG_MK_CHK_THR_ACCESS(mk);
    if (cd->next)
	cd->next->prev = cd->prev;
    else
	mk->cache_end = cd->prev;
    
    if (cd->prev)
	cd->prev->next = cd->next;
    else
	mk->cache = cd->next;
    ASSERT(mk->cache_size > 0);
    mk->cache_size--;
}

static ERTS_INLINE void
check_cache_limits(MemKind* mk)
{
    cache_desc_t *cd;
    ERTS_DBG_MK_CHK_THR_ACCESS(mk);
    mk->max_cached_seg_size = 0;
    mk->min_cached_seg_size = ~((Uint) 0);
    for (cd = mk->cache; cd; cd = cd->next) {
	if (cd->size < mk->min_cached_seg_size)
	    mk->min_cached_seg_size = cd->size;
	if (cd->size > mk->max_cached_seg_size)
	    mk->max_cached_seg_size = cd->size;
    }
}

static ERTS_INLINE void
adjust_cache_size(MemKind* mk, int force_check_limits)
{
    cache_desc_t *cd;
    int check_limits = force_check_limits;
    Sint max_cached = ((Sint) mk->segments.current.watermark
		       - (Sint) mk->segments.current.no);
    ERTS_DBG_MK_CHK_THR_ACCESS(mk);
    while (((Sint) mk->cache_size) > max_cached && ((Sint) mk->cache_size) > 0) {
	ASSERT(mk->cache_end);
	cd = mk->cache_end;
	if (!check_limits &&
	    !(mk->min_cached_seg_size < cd->size
	      && cd->size < mk->max_cached_seg_size)) {
	    check_limits = 1;
	}
	if (erts_mtrace_enabled)
	    erts_mtrace_crr_free(SEGTYPE, SEGTYPE, cd->seg);
	mseg_destroy(mk->ma, mk, cd->seg, cd->size);
	unlink_cd(mk,cd);
	free_cd(mk,cd);
    }

    if (check_limits)
	check_cache_limits(mk);
}

static Uint
check_one_cache(MemKind* mk)
{
    if (mk->segments.current.watermark > mk->segments.current.no)
	mk->segments.current.watermark--;
    adjust_cache_size(mk, 0);

    if (mk->cache_size)
	schedule_cache_check(mk->ma);
    return mk->cache_size;
}

static void do_cache_check(ErtsMsegAllctr_t *ma)
{
    int empty_cache = 1;
    MemKind* mk;

    ERTS_MSEG_LOCK(ma);

    for (mk=ma->mk_list; mk; mk=mk->next) {
	if (check_one_cache(mk))
	    empty_cache = 0;
    }

    if (empty_cache) {
	ma->is_cache_check_scheduled = 0;
	erts_set_aux_work_timeout(ma->ix,
				  ERTS_SSI_AUX_WORK_MSEG_CACHE_CHECK,
				  0);
    }

    INC_CC(ma, check_cache);

    ERTS_MSEG_UNLOCK(ma);
}

void erts_mseg_cache_check(void)
{
    do_cache_check(ERTS_MSEG_ALLCTR_SS());
}

static void
mseg_clear_cache(MemKind* mk)
{
    mk->segments.current.watermark = 0;

    adjust_cache_size(mk, 1);

    ASSERT(!mk->cache);
    ASSERT(!mk->cache_end);
    ASSERT(!mk->cache_size);

    mk->segments.current.watermark = mk->segments.current.no;

    INC_CC(mk->ma, clear_cache);
}

static ERTS_INLINE MemKind* memkind(ErtsMsegAllctr_t *ma,
				    const ErtsMsegOpt_t *opt)
{
#if HALFWORD_HEAP
    return opt->low_mem ? &ma->low_mem : &ma->hi_mem;
#else
    return &ma->the_mem;
#endif
}

static void *
mseg_alloc(ErtsMsegAllctr_t *ma, ErtsAlcType_t atype, Uint *size_p,
	   const ErtsMsegOpt_t *opt)
{
    Uint max, min, diff_size, size;
    cache_desc_t *cd, *cand_cd;
    void *seg;
    MemKind* mk = memkind(ma, opt);

    INC_CC(ma, alloc);

    size = PAGE_CEILING(*size_p);

#if CAN_PARTLY_DESTROY
    if (size < ma->min_seg_size)	
	ma->min_seg_size = size;
#endif

    if (!opt->cache) {
    create_seg:
	adjust_cache_size(mk,0);
	seg = mseg_create(ma, mk, size);
	if (!seg) {
	    mseg_clear_cache(mk);
	    seg = mseg_create(ma, mk, size);
	    if (!seg)
		size = 0;
	}

	*size_p = size;
	if (seg) {
	    if (erts_mtrace_enabled)
		erts_mtrace_crr_alloc(seg, atype, ERTS_MTRACE_SEGMENT_ID, size);
	    ERTS_MSEG_ALLOC_STAT(mk,size);
	}
	return seg;
    }

    if (size > mk->max_cached_seg_size)
	goto create_seg;

    if (size < mk->min_cached_seg_size) {

	diff_size = mk->min_cached_seg_size - size;

	if (diff_size > ma->abs_max_cache_bad_fit)
	    goto create_seg;

	if (100*PAGES(diff_size) > ma->rel_max_cache_bad_fit*PAGES(size))
	    goto create_seg;

    }

    max = 0;
    min = ~((Uint) 0);
    cand_cd = NULL;

    for (cd = mk->cache; cd; cd = cd->next) {
	if (cd->size >= size) {
	    if (!cand_cd) {
		cand_cd = cd;
		continue;
	    }
	    else if (cd->size < cand_cd->size) {
		if (max < cand_cd->size)
		    max = cand_cd->size;
		if (min > cand_cd->size)
		    min = cand_cd->size;
		cand_cd = cd;
		continue;
	    }
	}
	if (max < cd->size)
	    max = cd->size;
	if (min > cd->size)
	    min = cd->size;
    }

    mk->min_cached_seg_size = min;
    mk->max_cached_seg_size = max;

    if (!cand_cd)
	goto create_seg;

    diff_size = cand_cd->size - size;

    if (diff_size > ma->abs_max_cache_bad_fit
	|| 100*PAGES(diff_size) > ma->rel_max_cache_bad_fit*PAGES(size)) {
	if (mk->max_cached_seg_size < cand_cd->size)
	    mk->max_cached_seg_size = cand_cd->size;
	if (mk->min_cached_seg_size > cand_cd->size)
	    mk->min_cached_seg_size = cand_cd->size;
	goto create_seg;
    }

    mk->cache_hits++;

    size = cand_cd->size;
    seg = cand_cd->seg;

    unlink_cd(mk,cand_cd);
    free_cd(mk,cand_cd);

    *size_p = size;

    if (erts_mtrace_enabled) {
	erts_mtrace_crr_free(SEGTYPE, SEGTYPE, seg);
	erts_mtrace_crr_alloc(seg, atype, SEGTYPE, size);
    }

    if (seg)
	ERTS_MSEG_ALLOC_STAT(mk,size);

    return seg;
}


static void
mseg_dealloc(ErtsMsegAllctr_t *ma, ErtsAlcType_t atype, void *seg, Uint size,
	     const ErtsMsegOpt_t *opt)
{
    MemKind* mk = memkind(ma, opt);
    cache_desc_t *cd;

    ERTS_MSEG_DEALLOC_STAT(mk,size);

    if (!opt->cache || ma->max_cache_size == 0) {
	if (erts_mtrace_enabled)
	    erts_mtrace_crr_free(atype, SEGTYPE, seg);
	mseg_destroy(ma, mk, seg, size);
    }
    else {
	int check_limits = 0;
	
	if (size < mk->min_cached_seg_size)
	    mk->min_cached_seg_size = size;
	if (size > mk->max_cached_seg_size)
	    mk->max_cached_seg_size = size;

	if (!mk->free_cache_descs) {
	    cd = mk->cache_end;
	    if (!(mk->min_cached_seg_size < cd->size
		  && cd->size < mk->max_cached_seg_size)) {
		check_limits = 1;
	    }
	    if (erts_mtrace_enabled)
		erts_mtrace_crr_free(SEGTYPE, SEGTYPE, cd->seg);
	    mseg_destroy(ma, mk, cd->seg, cd->size);
	    unlink_cd(mk,cd);
	    free_cd(mk,cd);
	}

	cd = alloc_cd(mk);
	ASSERT(cd);
	cd->seg = seg;
	cd->size = size;
	link_cd(mk,cd);

	if (erts_mtrace_enabled) {
	    erts_mtrace_crr_free(atype, SEGTYPE, seg);
	    erts_mtrace_crr_alloc(seg, SEGTYPE, SEGTYPE, size);
	}

	/* ASSERT(segments.current.watermark >= segments.current.no + cache_size); */

	if (check_limits)
	    check_cache_limits(mk);

	schedule_cache_check(ma);

    }

    INC_CC(ma, dealloc);
}

static void *
mseg_realloc(ErtsMsegAllctr_t *ma, ErtsAlcType_t atype, void *seg,
	     Uint old_size, Uint *new_size_p, const ErtsMsegOpt_t *opt)
{
    MemKind* mk;
    void *new_seg;
    Uint new_size;

    if (!seg || !old_size) {
	new_seg = mseg_alloc(ma, atype, new_size_p, opt);
	DEC_CC(ma, alloc);
	return new_seg;
    }

    if (!(*new_size_p)) {
	mseg_dealloc(ma, atype, seg, old_size, opt);
	DEC_CC(ma, dealloc);
	return NULL;
    }

    mk = memkind(ma, opt);
    new_seg = seg;
    new_size = PAGE_CEILING(*new_size_p);

    if (new_size == old_size)
	;
    else if (new_size < old_size) {
	Uint shrink_sz = old_size - new_size;

#if CAN_PARTLY_DESTROY
	if (new_size < ma->min_seg_size)	
	    ma->min_seg_size = new_size;
#endif

	if (shrink_sz < opt->abs_shrink_th
	    && 100*PAGES(shrink_sz) < opt->rel_shrink_th*PAGES(old_size)) {
	    new_size = old_size;
	}
	else {

#if CAN_PARTLY_DESTROY

	    if (shrink_sz > ma->min_seg_size
		&& mk->free_cache_descs
		&& opt->cache) {
		cache_desc_t *cd;

		cd = alloc_cd(mk);
		ASSERT(cd);
		cd->seg = ((char *) seg) + new_size;
		cd->size = shrink_sz;
		end_link_cd(mk,cd);

		if (erts_mtrace_enabled) {
		    erts_mtrace_crr_realloc(new_seg,
					    atype,
					    SEGTYPE,
					    seg,
					    new_size);
		    erts_mtrace_crr_alloc(cd->seg, SEGTYPE, SEGTYPE, cd->size);
		}
		schedule_cache_check(ma);
	    }
	    else {
		if (erts_mtrace_enabled)
		    erts_mtrace_crr_realloc(new_seg,
					    atype,
					    SEGTYPE,
					    seg,
					    new_size);
		mseg_destroy(ma, mk, ((char *) seg) + new_size, shrink_sz);
	    }

#elif HAVE_MSEG_RECREATE

	    goto do_recreate;

#else

	    new_seg = mseg_alloc(ma, atype, &new_size, opt);
	    if (!new_seg)
		new_size = old_size;
	    else {
		sys_memcpy(((char *) new_seg),
			   ((char *) seg),
			   MIN(new_size, old_size));
		mseg_dealloc(ma, atype, seg, old_size, opt);
	    }

#endif

	}
    }
    else {

	if (!opt->preserv) {
	    mseg_dealloc(ma, atype, seg, old_size, opt);
	    new_seg = mseg_alloc(ma, atype, &new_size, opt);
	}
	else {
#if HAVE_MSEG_RECREATE
#if !CAN_PARTLY_DESTROY
	do_recreate:
#endif
	    new_seg = mseg_recreate(ma, mk, (void *) seg, old_size, new_size);
	    if (erts_mtrace_enabled)
		erts_mtrace_crr_realloc(new_seg, atype, SEGTYPE, seg, new_size);
	    if (!new_seg)
		new_size = old_size;
#else
	    new_seg = mseg_alloc(ma, atype, &new_size, opt);
	    if (!new_seg)
		new_size = old_size;
	    else {
		sys_memcpy(((char *) new_seg),
			   ((char *) seg),
			   MIN(new_size, old_size));
		mseg_dealloc(ma, atype, seg, old_size, opt);
	    }
#endif
	}
    }

    INC_CC(ma, realloc);

    *new_size_p = new_size;

    ERTS_MSEG_REALLOC_STAT(mk, old_size, new_size);

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
    Eterm mseg_destroy;
#if HAVE_MSEG_RECREATE
    Eterm mseg_recreate;
#endif
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

    ERTS_MSEG_UNLOCK(ma);
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
	AM_INIT(mseg_destroy);
#if HAVE_MSEG_RECREATE
	AM_INIT(mseg_recreate);
#endif
	AM_INIT(mseg_clear_cache);
	AM_INIT(mseg_check_cache);

#ifdef DEBUG
	for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	    ASSERT(*atom != THE_NON_VALUE);
	}
#endif
    }

    ERTS_MSEG_LOCK(ma);
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
    Eterm res = THE_NON_VALUE;

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

	res = NIL;
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
	PRINT_CC(to, arg, destroy);
#if HAVE_MSEG_RECREATE
	PRINT_CC(to, arg, recreate);
#endif
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

#if HAVE_MSEG_RECREATE
	add_3tup(hpp, szp, &res,
		 am.mseg_recreate,
		 bld_unstable_uint(hpp, szp, ma->calls.recreate.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.recreate.no));
#endif
	add_3tup(hpp, szp, &res,
		 am.mseg_destroy,
		 bld_unstable_uint(hpp, szp, ma->calls.destroy.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.destroy.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_create,
		 bld_unstable_uint(hpp, szp, ma->calls.create.giga_no),
		 bld_unstable_uint(hpp, szp, ma->calls.create.no));


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
info_status(ErtsMsegAllctr_t *ma, MemKind* mk, int *print_to_p, void *print_to_arg,
	    int begin_new_max_period, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    
    if (mk->segments.max_ever.no < mk->segments.max.no)
	mk->segments.max_ever.no = mk->segments.max.no;
    if (mk->segments.max_ever.sz < mk->segments.max.sz)
	mk->segments.max_ever.sz = mk->segments.max.sz;

    if (print_to_p) {
	int to = *print_to_p;
	void *arg = print_to_arg;

	erts_print(to, arg, "cached_segments: %beu\n", mk->cache_size);
	erts_print(to, arg, "cache_hits: %beu\n", mk->cache_hits);
	erts_print(to, arg, "segments: %beu %beu %beu\n",
		   mk->segments.current.no, mk->segments.max.no, mk->segments.max_ever.no);
	erts_print(to, arg, "segments_size: %beu %beu %beu\n",
		   mk->segments.current.sz, mk->segments.max.sz, mk->segments.max_ever.sz);
	erts_print(to, arg, "segments_watermark: %beu\n",
		   mk->segments.current.watermark);
    }

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res,
		 am.segments_watermark,
		 bld_unstable_uint(hpp, szp, mk->segments.current.watermark));
	add_4tup(hpp, szp, &res,
		 am.segments_size,
		 bld_unstable_uint(hpp, szp, mk->segments.current.sz),
		 bld_unstable_uint(hpp, szp, mk->segments.max.sz),
		 bld_unstable_uint(hpp, szp, mk->segments.max_ever.sz));
	add_4tup(hpp, szp, &res,
		 am.segments,
		 bld_unstable_uint(hpp, szp, mk->segments.current.no),
		 bld_unstable_uint(hpp, szp, mk->segments.max.no),
		 bld_unstable_uint(hpp, szp, mk->segments.max_ever.no));
	add_2tup(hpp, szp, &res,
		 am.cache_hits,
		 bld_unstable_uint(hpp, szp, mk->cache_hits));
	add_2tup(hpp, szp, &res,
		 am.cached_segments,
		 bld_unstable_uint(hpp, szp, mk->cache_size));

    }

    if (begin_new_max_period) {
	mk->segments.max.no = mk->segments.current.no;
	mk->segments.max.sz = mk->segments.current.sz;
    }

    return res;
}

static Eterm info_memkind(ErtsMsegAllctr_t *ma, MemKind* mk, int *print_to_p, void *print_to_arg,
			  int begin_max_per, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    Eterm atoms[3];
    Eterm values[3];

    if (print_to_p) {
	erts_print(*print_to_p, print_to_arg, "memory kind: %s\n", mk->name);
    }
    if (hpp || szp) {
	atoms[0] = am.name;
	atoms[1] = am.status;
	atoms[2] = am.calls;
	values[0] = erts_bld_string(hpp, szp, mk->name);
    }
    values[1] = info_status(ma, mk, print_to_p, print_to_arg, begin_max_per, hpp, szp);
    values[2] = info_calls(ma, print_to_p, print_to_arg, hpp, szp);

    if (hpp || szp)
	res = bld_2tup_list(hpp, szp, 3, atoms, values);

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

    ERTS_MSEG_LOCK(ma);

    ERTS_DBG_MA_CHK_THR_ACCESS(ma);

    res = info_options(ma, "option ", print_to_p, print_to_arg, hpp, szp);

    ERTS_MSEG_UNLOCK(ma);

    return res;
}

Eterm
erts_mseg_info(int ix,
	       int *print_to_p,
	       void *print_to_arg,
	       int begin_max_per,
	       Uint **hpp,
	       Uint *szp)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_IX(ix);
    Eterm res = THE_NON_VALUE;
    Eterm atoms[4];
    Eterm values[4];
    Uint n = 0;

    ERTS_MSEG_LOCK(ma);

    ERTS_DBG_MA_CHK_THR_ACCESS(ma);

    if (hpp || szp) {
	
	if (!atoms_initialized)
	    init_atoms(ma);

	atoms[0] = am.version;
	atoms[1] = am.options;
	atoms[2] = am.memkind;
	atoms[3] = am.memkind;
    }
    values[n++] = info_version(ma, print_to_p, print_to_arg, hpp, szp);
    values[n++] = info_options(ma, "option ", print_to_p, print_to_arg, hpp, szp);
#if HALFWORD_HEAP
    values[n++] = info_memkind(ma, &ma->low_mem, print_to_p, print_to_arg, begin_max_per, hpp, szp);
    values[n++] = info_memkind(ma, &ma->hi_mem, print_to_p, print_to_arg, begin_max_per, hpp, szp);
#else
    values[n++] = info_memkind(ma, &ma->the_mem, print_to_p, print_to_arg, begin_max_per, hpp, szp);
#endif
    if (hpp || szp)
	res = bld_2tup_list(hpp, szp, n, atoms, values);

    ERTS_MSEG_UNLOCK(ma);

    return res;
}

void *
erts_mseg_alloc_opt(ErtsAlcType_t atype, Uint *size_p, const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);
    void *seg;
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    seg = mseg_alloc(ma, atype, size_p, opt);
    ERTS_MSEG_UNLOCK(ma);
    return seg;
}

void *
erts_mseg_alloc(ErtsAlcType_t atype, Uint *size_p)
{
    return erts_mseg_alloc_opt(atype, size_p, &erts_mseg_default_opt);
}

void
erts_mseg_dealloc_opt(ErtsAlcType_t atype, void *seg,
		      Uint size, const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    mseg_dealloc(ma, atype, seg, size, opt);
    ERTS_MSEG_UNLOCK(ma);
}

void
erts_mseg_dealloc(ErtsAlcType_t atype, void *seg, Uint size)
{
    erts_mseg_dealloc_opt(atype, seg, size, &erts_mseg_default_opt);
}

void *
erts_mseg_realloc_opt(ErtsAlcType_t atype, void *seg,
		      Uint old_size, Uint *new_size_p,
		      const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);
    void *new_seg;
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    new_seg = mseg_realloc(ma, atype, seg, old_size, new_size_p, opt);
    ERTS_MSEG_UNLOCK(ma);
    return new_seg;
}

void *
erts_mseg_realloc(ErtsAlcType_t atype, void *seg,
		  Uint old_size, Uint *new_size_p)
{
    return erts_mseg_realloc_opt(atype, seg, old_size, new_size_p,
				 &erts_mseg_default_opt);
}

void
erts_mseg_clear_cache(void)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_SS();
    MemKind* mk;

start:

    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    for (mk=ma->mk_list; mk; mk=mk->next) {
	mseg_clear_cache(mk);
    }
    ERTS_MSEG_UNLOCK(ma);

    if (ma->ix != 0) {
	ma = ERTS_MSEG_ALLCTR_IX(0);
	goto start;
    }
}

Uint
erts_mseg_no(const ErtsMsegOpt_t *opt)
{
    ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_OPT(opt);
    MemKind* mk;
    Uint n = 0;
    ERTS_MSEG_LOCK(ma);
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    for (mk=ma->mk_list; mk; mk=mk->next) {
	n += mk->segments.current.no;
    }
    ERTS_MSEG_UNLOCK(ma);
    return n;
}

Uint
erts_mseg_unit_size(void)
{
    return page_size;
}

static void mem_kind_init(ErtsMsegAllctr_t *ma, MemKind* mk, const char* name)
{
    unsigned i;

    mk->cache = NULL;
    mk->cache_end = NULL;
    mk->max_cached_seg_size = 0;
    mk->min_cached_seg_size = ~((Uint) 0);
    mk->cache_size = 0;
    mk->cache_hits = 0;

    if (ma->max_cache_size > 0) {
	for (i = 0; i < ma->max_cache_size - 1; i++)
	    mk->cache_descs[i].next = &mk->cache_descs[i + 1];
	mk->cache_descs[ma->max_cache_size - 1].next = NULL;
	mk->free_cache_descs = &mk->cache_descs[0];
    }
    else
	mk->free_cache_descs = NULL;

    mk->segments.current.watermark = 0;
    mk->segments.current.no = 0;
    mk->segments.current.sz = 0;
    mk->segments.max.no = 0;
    mk->segments.max.sz = 0;
    mk->segments.max_ever.no = 0;
    mk->segments.max_ever.sz = 0;

    mk->ma = ma;
    mk->name = name;
    mk->next = ma->mk_list;
    ma->mk_list = mk;
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

#if HAVE_MMAP && !defined(MAP_ANON)
    mmap_fd = open("/dev/zero", O_RDWR);
    if (mmap_fd < 0)
	erl_exit(ERTS_ABORT_EXIT, "erts_mseg: unable to open /dev/zero\n");
#endif

#if HAVE_MMAP && HALFWORD_HEAP
    initialize_pmmap();
#endif

    page_size = GET_PAGE_SIZE;

    page_shift = 1;
    while ((page_size >> page_shift) != 1) {
	if ((page_size & (1 << (page_shift - 1))) != 0)
	    erl_exit(ERTS_ABORT_EXIT,
		     "erts_mseg: Unexpected page_size %beu\n", page_size);
	page_shift++;
    }

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

	ma->mk_list = NULL;

#if HALFWORD_HEAP
	mem_kind_init(ma, &ma->low_mem, "low memory");
	mem_kind_init(ma, &ma->hi_mem, "high memory");
#else
	mem_kind_init(ma, &ma->the_mem, "all memory");
#endif

	sys_memzero((void *) &ma->calls, sizeof(ErtsMsegCalls));

#if CAN_PARTLY_DESTROY
	ma->min_seg_size = ~((Uint) 0);
#endif
    }
}


static ERTS_INLINE Uint tot_cache_size(ErtsMsegAllctr_t *ma)
{
    MemKind* mk;
    Uint sz = 0;
    ERTS_DBG_MA_CHK_THR_ACCESS(ma);
    for (mk=ma->mk_list; mk; mk=mk->next) {
	sz += mk->cache_size;
    }
    return sz;
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

unsigned long
erts_mseg_test(unsigned long op,
	       unsigned long a1,
	       unsigned long a2,
	       unsigned long a3)
{
    switch (op) {
#if HAVE_ERTS_MSEG
    case 0x400: /* Have erts_mseg */
	return (unsigned long) 1;
    case 0x401:
	return (unsigned long) erts_mseg_alloc(ERTS_ALC_A_INVALID, (Uint *) a1);
    case 0x402:
	erts_mseg_dealloc(ERTS_ALC_A_INVALID, (void *) a1, (Uint) a2);
	return (unsigned long) 0;
    case 0x403:
	return (unsigned long) erts_mseg_realloc(ERTS_ALC_A_INVALID,
						 (void *) a1,
						 (Uint) a2,
						 (Uint *) a3);
    case 0x404:
	erts_mseg_clear_cache();
	return (unsigned long) 0;
    case 0x405:
	return (unsigned long) erts_mseg_no(&erts_mseg_default_opt);
    case 0x406: {
	ErtsMsegAllctr_t *ma = ERTS_MSEG_ALLCTR_IX(0);
	unsigned long res;
	ERTS_MSEG_LOCK(ma);
	res = (unsigned long) tot_cache_size(ma);
	ERTS_MSEG_UNLOCK(ma);
	return res;
    }
#else /* #if HAVE_ERTS_MSEG */
    case 0x400: /* Have erts_mseg */
	return (unsigned long) 0;
#endif /* #if HAVE_ERTS_MSEG */
    default:	ASSERT(0); return ~((unsigned long) 0);
    }

}


#if HALFWORD_HEAP
/*
 * Very simple page oriented mmap replacer. Works in the lower
 * 32 bit address range of a 64bit program.
 * Implements anonymous mmap mremap and munmap with address order first fit.
 * The free list is expected to be very short...
 * To be used for compressed pointers in Erlang halfword emulator
 * implementation. The MacOS X version is more of a toy, it's not really
 * for production as the halfword erlang VM relies on Linux specific memory
 * mapping tricks.
 */

/*#define HARDDEBUG 1*/

#ifdef __APPLE__
#define MAP_ANONYMOUS MAP_ANON
#endif

#define INIT_LOCK() do {erts_mtx_init(&pmmap_mutex, "pmmap");} while(0)

#define TAKE_LOCK()  do {erts_mtx_lock(&pmmap_mutex);} while(0)

#define RELEASE_LOCK() do {erts_mtx_unlock(&pmmap_mutex);} while(0)

static erts_mtx_t pmmap_mutex; /* Also needed when !USE_THREADS */

typedef struct _free_block {
    unsigned long num; /*pages*/
    struct _free_block *next;
} FreeBlock;

/* Assigned once and for all */
static size_t pagsz;

/* Protect with lock */
static FreeBlock *first;

static size_t round_up_to_pagesize(size_t size)
{
    size_t x  = size / pagsz;

    if ((size % pagsz)) {
	++x;
    }

    return pagsz * x;
}

static size_t round_down_to_pagesize(size_t size)
{
    size_t x  = size / pagsz;

    return pagsz * x;
}

static void *do_map(void *ptr, size_t sz)
{
    void *res;

    if (round_up_to_pagesize(sz) != sz) {
#ifdef HARDDEBUG
	fprintf(stderr,"Mapping of address %p with size %ld "
		"does not map complete pages\r\n",
		(void *) ptr, (unsigned long) sz);
#endif
	return NULL;
    }

    if (((unsigned long) ptr) % pagsz) {
#ifdef HARDDEBUG
	fprintf(stderr,"Mapping of address %p with size %ld "
		"is not page aligned\r\n",
		(void *) ptr, (unsigned long) sz);
#endif
	return NULL;
    }

#if HAVE_MMAP
    res = mmap(ptr, sz,
	       PROT_READ | PROT_WRITE, MAP_PRIVATE |
	       MAP_ANONYMOUS | MAP_FIXED,
	       -1 , 0);
#else
#  error "Missing mmap support"
#endif

    if (res == MAP_FAILED) {
#ifdef HARDDEBUG
	fprintf(stderr,"Mapping of address %p with size %ld failed!\r\n",
		(void *) ptr, (unsigned long) sz);
#endif
	return NULL;
    }

    return res;
}

static int do_unmap(void *ptr, size_t sz)
{
    void *res;

    if (round_up_to_pagesize(sz) != sz) {
#ifdef HARDDEBUG
	fprintf(stderr,"Mapping of address %p with size %ld "
		"does not map complete pages\r\n",
		(void *) ptr, (unsigned long) sz);
#endif
	return 1;
    }

    if (((unsigned long) ptr) % pagsz) {
#ifdef HARDDEBUG
	fprintf(stderr,"Mapping of address %p with size %ld "
		"is not page aligned\r\n",
		(void *) ptr, (unsigned long) sz);
#endif
	return 1;
    }


    res = mmap(ptr, sz,
	       PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE
	       | MAP_FIXED,
	       -1 , 0);

    if (res == MAP_FAILED) {
#ifdef HARDDEBUG
	fprintf(stderr,"Mapping of address %p with size %ld failed!\r\n",
		(void *) ptr, (unsigned long) sz);
#endif
	return 1;
    }

    return 0;
}

#ifdef __APPLE__
/*
 * The first 4 gig's are protected on Macos X for 64bit processes :(
 * The range 0x1000000000 - 0x10FFFFFFFF is selected as an arbitrary
 * value of a normally unused range... Real MMAP's will avoid
 * it and all 32bit compressed pointers can be in that range...
 * More expensive than on Linux where expansion of compressed
 * poiters involves no masking (as they are in the first 4 gig's).
 * It's also very uncertain if the MAP_NORESERVE flag really has
 * any effect in MacOS X. Swap space may always be allocated...
 */
#define SET_RANGE_MIN() /* nothing */
#define RANGE_MIN 0x1000000000UL
#define RANGE_MAX 0x1100000000UL
#define RANGE_MASK (RANGE_MIN)
#define EXTRA_MAP_FLAGS (MAP_FIXED)
#else
static size_t range_min;
#define SET_RANGE_MIN() do { range_min = (size_t) sbrk(0); } while (0)
#define RANGE_MIN range_min
#define RANGE_MAX 0x100000000UL
#define RANGE_MASK 0UL
#define EXTRA_MAP_FLAGS (0)
#endif

static int initialize_pmmap(void)
{
    char *p,*q,*rptr;
    size_t rsz;
    FreeBlock *initial;


    pagsz = getpagesize();
    SET_RANGE_MIN();
    if (sizeof(void *) != 8) {
	erl_exit(1,"Halfword emulator cannot be run in 32bit mode");
    }

    p = (char *) RANGE_MIN;
    q = (char *) RANGE_MAX;

    rsz = round_down_to_pagesize(q - p);

    rptr = mmap((void *) p, rsz,
		PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS |
		MAP_NORESERVE | EXTRA_MAP_FLAGS,
		-1 , 0);
#ifdef HARDDEBUG
    printf("p=%p, rsz = %ld, pages = %ld, got range = %p -> %p\r\n",
	   p, (unsigned long) rsz, (unsigned long) (rsz / pagsz),
	   (void *) rptr, (void*)(rptr + rsz));
#endif
    if ((UWord)(rptr + rsz) > RANGE_MAX) {
	size_t rsz_trunc = RANGE_MAX - (UWord)rptr;
#ifdef HARDDEBUG
	printf("Reducing mmap'ed memory from %lu to %lu Mb, reduced range = %p -> %p\r\n",
		rsz/(1024*1024), rsz_trunc/(1024*1024), rptr, rptr+rsz_trunc);
#endif
	munmap((void*)RANGE_MAX, rsz - rsz_trunc);
	rsz = rsz_trunc;
    }
    if (!do_map(rptr,pagsz)) {
	erl_exit(1,"Could not actually mmap first page for halfword emulator...\n");
    }
    initial = (FreeBlock *) rptr;
    initial->num = (rsz / pagsz);
    initial->next = NULL;
    first = initial;
    INIT_LOCK();
    return 0;
}

#ifdef HARDDEBUG
static void dump_freelist(void)
{
    FreeBlock *p = first;

    while (p) {
	printf("p = %p\r\np->num = %ld\r\np->next = %p\r\n\r\n",
	       (void *) p, (unsigned long) p->num, (void *) p->next);
	p = p->next;
    }
}
#endif


static void *pmmap(size_t size)
{
    size_t real_size = round_up_to_pagesize(size);
    size_t num_pages = real_size / pagsz;
    FreeBlock **block;
    FreeBlock *tail;
    FreeBlock *res;
    TAKE_LOCK();
    for (block = &first;
	 *block != NULL && (*block)->num < num_pages;
	 block = &((*block)->next))
	;
    if (!(*block)) {
	RELEASE_LOCK();
	return NULL;
    }
    if ((*block)->num == num_pages) {
	/* nice, perfect fit */
	res = *block;
	*block = (*block)->next;
    } else {
	tail = (FreeBlock *) (((char *) ((void *) (*block))) + real_size);
	if (!do_map(tail,pagsz)) {
#ifdef HARDDEBUG
	    fprintf(stderr, "Could not actually allocate page at %p...\r\n",
		    (void *) tail);
#endif
	    RELEASE_LOCK();
	    return NULL;
	}
	tail->num = (*block)->num - num_pages;
	tail->next = (*block)->next;
	res = *block;
	*block = tail;
    }
    RELEASE_LOCK();
    if (!do_map(res,real_size)) {
#ifdef HARDDEBUG
	fprintf(stderr, "Could not actually allocate %ld at %p...\r\n",
		(unsigned long) real_size, (void *) res);
#endif
	return NULL;
    }

    return (void *) res;
}

static int pmunmap(void *p, size_t size)
{
    size_t real_size = round_up_to_pagesize(size);
    size_t num_pages = real_size / pagsz;
    FreeBlock *block;
    FreeBlock *last;
    FreeBlock *nb = (FreeBlock *) p;

    ASSERT(((unsigned long)p & CHECK_POINTER_MASK)==0);
    if (real_size > pagsz) {
	if (do_unmap(((char *) p) + pagsz,real_size - pagsz)) {
	    return 1;
	}
    }

    TAKE_LOCK();

    last = NULL;
    block = first;
    while(block != NULL && ((void *) block) < p) {
	last = block;
	block = block->next;
    }

    if (block != NULL &&
	((void *) block) == ((void *) (((char *) p) + real_size))) {
	/* Merge new free block with following */
	nb->num = block->num + num_pages;
	nb->next = block->next;
	if (do_unmap(block,pagsz)) {
	    RELEASE_LOCK();
	    return 1;
	}
    } else {
	/* just link in */
	nb->num = num_pages;
	nb->next = block;
    }
    if (last != NULL) {
	if (p == ((void *) (((char *) last) + (last->num * pagsz)))) {
	    /* Merge with previous */
	    last->num += nb->num;
	    last->next = nb->next;
	    if (do_unmap(nb,pagsz)) {
		RELEASE_LOCK();
		return 1;
	    }
	} else {
	    last->next = nb;
	}
    } else {
	first = nb;
    }
    RELEASE_LOCK();
    return 0;
}

static void *pmremap(void *old_address, size_t old_size,
	      size_t new_size)
{
    size_t new_real_size = round_up_to_pagesize(new_size);
    size_t new_num_pages = new_real_size / pagsz;
    size_t old_real_size = round_up_to_pagesize(old_size);
    size_t old_num_pages = old_real_size / pagsz;
    if (new_num_pages == old_num_pages) {
	return old_address;
    } else if (new_num_pages < old_num_pages) { /* Shrink */
	size_t nfb_pages = old_num_pages - new_num_pages;
	size_t nfb_real_size = old_real_size - new_real_size;
	void *vnfb = (void *) (((char *)old_address) + new_real_size);
	FreeBlock *nfb = (FreeBlock *) vnfb;
	FreeBlock **block;
	TAKE_LOCK();
	for (block = &first;
	     *block != NULL && (*block) < nfb;
	     block = &((*block)->next))
	;
	if (!(*block) ||
	    (*block) > ((FreeBlock *)(((char *) vnfb) + nfb_real_size))) {
	    /* Normal link in */
	    if (nfb_pages > 1) {
		if (do_unmap((void *)(((char *) vnfb) + pagsz),
			     (nfb_pages - 1)*pagsz)) {
		    return NULL;
		}
	    }
	    nfb->next = (*block);
	    nfb->num = nfb_pages;
	    (*block) = nfb;
	} else { /* block merge */
	    nfb->next = (*block)->next;
	    nfb->num = nfb_pages + (*block)->num;
	    /* unmap also the first page of the next freeblock */
	    (*block) = nfb;
	    if (do_unmap((void *)(((char *) vnfb) + pagsz),
			 nfb_pages*pagsz)) {
		return NULL;
	    }
	}
	RELEASE_LOCK();
	return old_address;
    } else { /* Enlarge */
	FreeBlock **block;
	void *old_end = (void *) (((char *)old_address) + old_real_size);
	TAKE_LOCK();
	for (block = &first;
	     *block != NULL && (*block) < (FreeBlock *) old_address;
	     block = &((*block)->next))
	    ;
	if ((*block) == NULL || old_end > ((void *) RANGE_MAX) ||
	    (*block) != old_end ||
	    (*block)->num < (new_num_pages - old_num_pages)) {
	    /* cannot extend */
	    void *result;
	    RELEASE_LOCK();
	    result = pmmap(new_size);
	    if (result == NULL) {
		return NULL;
	    }
	    memcpy(result,old_address,old_size);
	    if (pmunmap(old_address,old_size)) {
		/* Oups... */
		pmunmap(result,new_size);
		return NULL;
	    }
	    return result;
	} else { /* extend */
	    size_t remaining_pages = (*block)->num -
		(new_num_pages - old_num_pages);
	    if (!remaining_pages) {
		void *p = (void *) (((char *) (*block)) + pagsz);
		void *n = (*block)->next;
		size_t x = ((*block)->num - 1) * pagsz;
		if (x > 0) {
		    if (do_map(p,x) == NULL) {
			RELEASE_LOCK();
			return NULL;
		    }
		}
		(*block) = n;
	    } else {
		FreeBlock *nfb = (FreeBlock *) ((void *)
						(((char *) old_address) +
						 new_real_size));
		void *p = (void *) (((char *) (*block)) + pagsz);
		if (do_map(p,new_real_size - old_real_size) == NULL) {
		    RELEASE_LOCK();
		    return NULL;
		}
		nfb->num = remaining_pages;
		nfb->next = (*block)->next;
		(*block) = nfb;
	    }
	    RELEASE_LOCK();
	    return old_address;
	}
    }
}

#endif /* HALFWORD_HEAP */
