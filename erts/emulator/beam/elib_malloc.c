/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
** Description: Faster malloc().
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"

#ifdef ENABLE_ELIB_MALLOC

#undef THREAD_SAFE_ELIB_MALLOC
#ifdef USE_THREADS
#define THREAD_SAFE_ELIB_MALLOC 1
#else
#define THREAD_SAFE_ELIB_MALLOC 0
#endif

#include "erl_driver.h"
#include "erl_threads.h"
#include "elib_stat.h"
#include <stdio.h>
#include <stdlib.h>

/* To avoid clobbering of names becaure of reclaim on VxWorks,
   we undefine all possible malloc, calloc etc. */
#undef malloc
#undef calloc
#undef free
#undef realloc

#define ELIB_INLINE         /* inline all possible functions */

#ifndef ELIB_ALIGN
#define ELIB_ALIGN             sizeof(double)
#endif

#ifndef ELIB_HEAP_SIZE
#define ELIB_HEAP_SIZE         (64*1024)  /* Default 64K */
#endif

#ifndef ELIB_HEAP_INCREAMENT
#define ELIB_HEAP_INCREAMENT   (32*1024)  /* Default 32K */
#endif

#ifndef ELIB_FAILURE
#define ELIB_FAILURE    abort()
#endif

#undef ASSERT
#ifdef DEBUG
#define ASSERT(B) \
 ((void) ((B) ? 1 : (fprintf(stderr, "%s:%d: Assertion failed: %s\n", \
			     __FILE__, __LINE__, #B), abort(), 0)))
#else
#define ASSERT(B) ((void) 1)
#endif

#ifndef USE_RECURSIVE_MALLOC_MUTEX
#define USE_RECURSIVE_MALLOC_MUTEX 0
#endif

#if USE_RECURSIVE_MALLOC_MUTEX
static erts_mtx_t malloc_mutex = ERTS_REC_MTX_INITER;
#else /* #if USE_RECURSIVE_MALLOC_MUTEX */
static erts_mtx_t malloc_mutex = ERTS_MTX_INITER;
#if THREAD_SAFE_ELIB_MALLOC
static erts_cnd_t  malloc_cond  = ERTS_CND_INITER;
#endif
#endif  /* #if USE_RECURSIVE_MALLOC_MUTEX */

typedef unsigned long EWord;       /* Assume 32-bit in this implementation */
typedef unsigned short EHalfWord;  /* Assume 16-bit in this implementation */
typedef unsigned char EByte;       /* Assume 8-bit byte */


#define elib_printf fprintf
#define elib_putc   fputc


#if defined(__STDC__) || defined(__WIN32__)
#define CONCAT(x,y) x##y
#else
#define CONCAT(x,y) x/**/y
#endif


#ifdef ELIB_DEBUG
#define ELIB_PREFIX(fun, args) CONCAT(elib__,fun) args
#else
#define ELIB_PREFIX(fun, args) CONCAT(elib_,fun) args
#endif

#if defined(__STDC__)
void *ELIB_PREFIX(malloc, (size_t));
void *ELIB_PREFIX(calloc, (size_t, size_t));
void ELIB_PREFIX(cfree, (EWord *));
void ELIB_PREFIX(free, (EWord *));
void *ELIB_PREFIX(realloc, (EWord *, size_t));
void* ELIB_PREFIX(memresize, (EWord *, int));
void* ELIB_PREFIX(memalign, (int, int));
void* ELIB_PREFIX(valloc, (int));
void* ELIB_PREFIX(pvalloc, (int));
int ELIB_PREFIX(memsize, (EWord *));
/* Extern interfaces used by VxWorks */
size_t elib_sizeof(void *);
void elib_init(EWord *, EWord);
void elib_force_init(EWord *, EWord);
#endif

#if defined(__STDC__)
/* define prototypes for missing */
void* memalign(size_t a, size_t s);
void* pvalloc(size_t nb);
void* memresize(void *p, int nb);
int memsize(void *p);
#endif

/* bytes to pages */
#define PAGES(x)      (((x)+page_size-1) / page_size)
#define PAGE_ALIGN(p) ((char*)((((EWord)(p))+page_size-1)&~(page_size-1)))

/* bytes to words */
#define WORDS(x)      (((x)+sizeof(EWord)-1) / sizeof(EWord))

/* Align an address */
#define ALIGN(p)     ((EWord*)((((EWord)(p)+ELIB_ALIGN-1)&~(ELIB_ALIGN-1))))

/* Calculate the size needed to keep alignment */

#define ALIGN_BSZ(nb)  ((nb+sizeof(EWord)+ELIB_ALIGN-1) & ~(ELIB_ALIGN-1))

#define ALIGN_WSZ(nb)  WORDS(ALIGN_BSZ(nb))

#define ALIGN_SIZE(nb) (ALIGN_WSZ(nb) - 1)


/* PARAMETERS */

#if defined(ELIB_HEAP_SBRK)

#undef PAGE_SIZE

/* Get the system page size (NEED MORE DEFINES HERE) */
#ifdef _SC_PAGESIZE
#define PAGE_SIZE   sysconf(_SC_PAGESIZE)
#elif defined(_MSC_VER)
#  ifdef _M_ALPHA
#    define PAGE_SIZE      0x2000
#  else
#    define PAGE_SIZE      0x1000
#  endif
#else
#define PAGE_SIZE   getpagesize()
#endif

#define ELIB_EXPAND(need)  expand_sbrk(need)
static FUNCTION(int, expand_sbrk, (EWord));

#elif defined(ELIB_HEAP_FIXED)

#define PAGE_SIZE 1024
#define ELIB_EXPAND(need) -1
static EWord fix_heap[WORDS(ELIB_HEAP_SIZE)];

#elif defined(ELIB_HEAP_USER)

#define PAGE_SIZE 1024
#define ELIB_EXPAND(need) -1

#else

#error "ELIB HEAP TYPE NOT SET"

#endif


#define STAT_ALLOCED_BLOCK(SZ)		\
do {					\
    tot_allocated += (SZ);		\
    if (max_allocated < tot_allocated)	\
	max_allocated = tot_allocated;	\
} while (0)

#define STAT_FREED_BLOCK(SZ)		\
do {					\
    tot_allocated -= (SZ);		\
} while (0)

static int max_allocated = 0;
static int tot_allocated = 0;
static EWord* eheap;        /* Align heap start */
static EWord* eheap_top;    /* Point to end of heap */
EWord page_size = 0;        /* Set by elib_init */

#if defined(ELIB_DEBUG) || defined(DEBUG)
#define ALIGN_CHECK(a, p)						\
 do {									\
    if ((EWord)(p) & (a-1)) {						\
	elib_printf(stderr,						\
		    "RUNTIME ERROR: bad alignment (0x%lx:%d:%d)\n",	\
		    (unsigned long) (p), (int) a, __LINE__);			\
	ELIB_FAILURE;							\
    }									\
  } while(0)
#define ELIB_ALIGN_CHECK(p) ALIGN_CHECK(ELIB_ALIGN, p)
#else
#define ALIGN_CHECK(a, p)
#define ELIB_ALIGN_CHECK(p)
#endif

#define DYNAMIC 32

/*
** Free block layout
**   1 1          30
**  +--------------------------+
**  |F|P|        Size          |
**  +--------------------------+
**
** Where F is the free bit
**       P is the free above bit
**       Size is messured in words and does not include the hdr word
**
** If block is on the free list the size is also stored last in the block.
** 
*/
typedef struct _free_block FreeBlock;
struct _free_block {
    EWord hdr;
    Uint flags;
    FreeBlock* parent;
    FreeBlock* left;
    FreeBlock* right;
    EWord v[1];
};

typedef struct _allocated_block {
    EWord hdr;
    EWord v[5];
} AllocatedBlock;


/*
 * Interface to tree routines.
 */
typedef Uint Block_t;

static Block_t*	get_free_block(Uint);
static void link_free_block(Block_t *);
static void unlink_free_block(Block_t *del);

#define FREE_BIT       0x80000000
#define FREE_ABOVE_BIT 0x40000000
#define SIZE_MASK      0x3fffffff     /* 2^30 words = 2^32 bytes */

/* Work on both FreeBlock and AllocatedBlock */
#define SIZEOF(p)         ((p)->hdr & SIZE_MASK)
#define IS_FREE(p)        (((p)->hdr & FREE_BIT) != 0)
#define IS_FREE_ABOVE(p)  (((p)->hdr & FREE_ABOVE_BIT) != 0)

/* Given that we have a free block above find its size */
#define SIZEOF_ABOVE(p)    *(((EWord*) (p)) - 1)

#define MIN_BLOCK_SIZE      (sizeof(FreeBlock)/sizeof(EWord))
#define MIN_WORD_SIZE       (MIN_BLOCK_SIZE-1)
#define MIN_BYTE_SIZE       (sizeof(FreeBlock)-sizeof(EWord))

#define MIN_ALIGN_SIZE      ALIGN_SIZE(MIN_BYTE_SIZE)


static AllocatedBlock* heap_head = 0;
static AllocatedBlock* heap_tail = 0;
static EWord eheap_size = 0;

static int heap_locked;

static int elib_need_init = 1;
#if THREAD_SAFE_ELIB_MALLOC
static int elib_is_initing = 0;
#endif

typedef FreeBlock RBTree_t;

static RBTree_t* root = NULL;


static FUNCTION(void, deallocate, (AllocatedBlock*, int));

/*
 * Unlink a free block
 */

#define mark_allocated(p, szp) do { \
      (p)->hdr = ((p)->hdr & FREE_ABOVE_BIT) | (szp); \
      (p)->v[szp] &= ~FREE_ABOVE_BIT; \
   } while(0)

#define mark_free(p, szp) do { \
      (p)->hdr = FREE_BIT | (szp); \
      ((FreeBlock *)p)->v[szp-sizeof(FreeBlock)/sizeof(EWord)+1] = (szp); \
   } while(0)

#if 0
/* Help macros to log2 */
#define LOG_1(x)  (((x) > 1) ? 1 : 0)
#define LOG_2(x)  (((x) > 3) ? 2+LOG_1((x) >> 2) : LOG_1(x))
#define LOG_4(x)  (((x) > 15) ? 4+LOG_2((x) >> 4) : LOG_2(x))
#define LOG_8(x)  (((x) > 255) ? 8+LOG_4((x)>>8) : LOG_4(x))
#define LOG_16(x) (((x) > 65535) ? 16+LOG_8((x)>>16) : LOG_8(x))

#define log2(x)   LOG_16(x)
#endif

/*
 * Split a block to be allocated.
 * Mark block as ALLOCATED and clear
 * FREE_ABOVE_BIT on next block
 *
 * nw is SIZE aligned and szp is SIZE aligned + 1
 */
static void
split_block(FreeBlock* p, EWord nw, EWord szp)
{
    EWord szq;
    FreeBlock* q;

    szq = szp - nw;
    /* Preserve FREE_ABOVE bit in p->hdr !!! */

    if (szq >= MIN_ALIGN_SIZE+1) {
	szq--;
	p->hdr = (p->hdr & FREE_ABOVE_BIT) | nw;

	q = (FreeBlock*) (((EWord*) p) + nw + 1);
	mark_free(q, szq);
	link_free_block((Block_t *) q);

	q = (FreeBlock*) (((EWord*) q) + szq + 1);
	q->hdr |= FREE_ABOVE_BIT;
    }
    else {
	mark_allocated((AllocatedBlock*)p, szp);
    }
}

/*
 * Find a free block
 */
static FreeBlock*
alloc_block(EWord nw)
{
    for (;;) {
	FreeBlock* p = (FreeBlock *) get_free_block(nw);

	if (p != NULL) {
	    return p;
	} else if (ELIB_EXPAND(nw+MIN_WORD_SIZE)) {
	    return 0;
	}
    }
}


size_t elib_sizeof(void *p)
{
    AllocatedBlock* pp;

    if (p != 0) {
	pp = (AllocatedBlock*) (((char *)p)-1);
	return SIZEOF(pp);
    }
    return 0;
}

static void locked_elib_init(EWord*, EWord);
static void init_elib_malloc(EWord*, EWord);

/*
** Initialize the elib
** The addr and sz is only used when compiled with EXPAND_ADDR 
*/
/* Not static, this is used by VxWorks */
void elib_init(EWord* addr, EWord sz)
{
    if (!elib_need_init)
	return;
    erts_mtx_lock(&malloc_mutex);
    locked_elib_init(addr, sz);
    erts_mtx_unlock(&malloc_mutex);
}

static void locked_elib_init(EWord* addr, EWord sz)
{
    if (!elib_need_init)
	return;

#if THREAD_SAFE_ELIB_MALLOC

#if !USE_RECURSIVE_MALLOC_MUTEX
    {
	static erts_tid_t initer_tid;

	if(elib_is_initing) {

	    if(erts_equal_tids(initer_tid, erts_thr_self()))
		return;

	    /* Wait until initializing thread is done with initialization */

	    while(elib_need_init)
		erts_cnd_wait(&malloc_cond, &malloc_mutex);

	    return;
	}
	else {
	    initer_tid = erts_thr_self();
	    elib_is_initing = 1;
	}
    }
#else
    if(elib_is_initing)
	return;
    elib_is_initing = 1;
#endif

#endif /* #if THREAD_SAFE_ELIB_MALLOC */

    /* Do the actual initialization of the malloc implementation */
    init_elib_malloc(addr, sz);

#if THREAD_SAFE_ELIB_MALLOC

#if !USE_RECURSIVE_MALLOC_MUTEX
    erts_mtx_unlock(&malloc_mutex);
#endif

    /* Recursive calls to malloc are allowed here... */
    erts_mtx_set_forksafe(&malloc_mutex);

#if !USE_RECURSIVE_MALLOC_MUTEX
    erts_mtx_lock(&malloc_mutex);
    elib_is_initing = 0;
#endif

#endif /* #if THREAD_SAFE_ELIB_MALLOC */

    elib_need_init = 0;

#if THREAD_SAFE_ELIB_MALLOC && !USE_RECURSIVE_MALLOC_MUTEX
    erts_cnd_broadcast(&malloc_cond);
#endif

}

static void init_elib_malloc(EWord* addr, EWord sz)
{
    int i;
    FreeBlock* freep;
    EWord tmp_sz;
#ifdef ELIB_HEAP_SBRK
    char* top;
    EWord n;
#endif

    max_allocated = 0;
    tot_allocated = 0;
    root = NULL;

    /* Get the page size (may involve system call!!!) */
    page_size = PAGE_SIZE;

#if defined(ELIB_HEAP_SBRK)
    sz = PAGES(ELIB_HEAP_SIZE)*page_size;

    if ((top = (char*) sbrk(0)) == (char*)-1) {
	elib_printf(stderr, "could not initialize elib, sbrk(0)");
	ELIB_FAILURE;
    }
    n = PAGE_ALIGN(top) - top;
    if ((top = (char*) sbrk(n)) == (char*)-1) {
	elib_printf(stderr, "could not initialize elib, sbrk(n)");
	ELIB_FAILURE;
    }
    if ((eheap = (EWord*) sbrk(sz)) == (EWord*)-1) {
	elib_printf(stderr, "could not initialize elib, sbrk(SIZE)");
	ELIB_FAILURE;
    }
    sz = WORDS(ELIB_HEAP_SIZE);
#elif defined(ELIB_HEAP_FIXED)
    eheap = fix_heap;
    sz = WORDS(ELIB_HEAP_SIZE);
#elif defined(ELIB_HEAP_USER)
    eheap = addr;
    sz = WORDS(sz);
#else
    return -1;
#endif
    eheap_size = 0;

    /* Make sure that the first word of the heap_head is aligned */
    addr = ALIGN(eheap+1);
    sz -= ((addr - 1) - eheap);      /* Subtract unusable size */
    eheap_top = eheap = addr - 1;    /* Set new aligned heap start */

    eheap_top[sz-1] = 0;	     /* Heap stop mark */

    addr = eheap;
    heap_head = (AllocatedBlock*) addr;
    heap_head->hdr = MIN_ALIGN_SIZE;
    for (i = 0; i < MIN_ALIGN_SIZE; i++)
	heap_head->v[i] = 0;

    addr += (MIN_ALIGN_SIZE+1);
    freep = (FreeBlock*) addr;
    tmp_sz = sz - (((MIN_ALIGN_SIZE+1) + MIN_BLOCK_SIZE) + 1 + 1);
    mark_free(freep, tmp_sz);
    link_free_block((Block_t *) freep);

    /* No need to align heap tail */
    heap_tail = (AllocatedBlock*) &eheap_top[sz-MIN_BLOCK_SIZE-1];
    heap_tail->hdr = FREE_ABOVE_BIT | MIN_WORD_SIZE;
    heap_tail->v[0] = 0;
    heap_tail->v[1] = 0;
    heap_tail->v[2] = 0;

    eheap_top += sz;
    eheap_size += sz;

    heap_locked = 0;
}

#ifdef ELIB_HEAP_USER
void elib_force_init(EWord* addr, EWord sz)
{
    elib_need_init = 1;
    elib_init(addr,sz);
}
#endif

#ifdef ELIB_HEAP_SBRK

/*
** need in number of words (should include head and tail words)
*/
static int expand_sbrk(EWord sz)
{
    EWord* p;
    EWord  bytes = sz * sizeof(EWord);
    EWord  size;
    AllocatedBlock* tail;

    if (bytes < ELIB_HEAP_SIZE)
	size = PAGES(ELIB_HEAP_INCREAMENT)*page_size;
    else
	size = PAGES(bytes)*page_size;

    if ((p = (EWord*) sbrk(size)) == ((EWord*) -1))
	return -1;

    if (p != eheap_top) {
	elib_printf(stderr, "panic: sbrk moved\n");
	ELIB_FAILURE;
    }

    sz = WORDS(size);

    /* Set new endof heap marker and a new heap tail */
    eheap_top[sz-1] = 0;

    tail = (AllocatedBlock*) &eheap_top[sz-MIN_BLOCK_SIZE-1];
    tail->hdr = FREE_ABOVE_BIT | MIN_WORD_SIZE;
    tail->v[0] = 0;
    tail->v[1] = 0;
    tail->v[2] = 0;

    /* Patch old tail with new appended size */
    heap_tail->hdr = (heap_tail->hdr & FREE_ABOVE_BIT) |
	(MIN_WORD_SIZE+1+(sz-MIN_BLOCK_SIZE-1));
    deallocate(heap_tail, 0);

    heap_tail = tail;

    eheap_size += sz;
    eheap_top += sz;

    return 0;
}

#endif /* ELIB_HEAP_SBRK */


/*
** Scan heap and check for corrupted heap
*/
int elib_check_heap(void)
{
    AllocatedBlock* p = heap_head;
    EWord sz;

    if (heap_locked) {
	elib_printf(stderr, "heap is locked no info avaiable\n");
	return 0;
    }

    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    if (p->v[sz-1] != sz) {
		elib_printf(stderr, "panic: heap corrupted\r\n"); 
		ELIB_FAILURE;	
	    }
	    p = (AllocatedBlock*) (p->v + sz);
	    if (!IS_FREE_ABOVE(p)) {
		elib_printf(stderr, "panic: heap corrupted\r\n"); 
		ELIB_FAILURE;	
	    }
	}
	else
	    p = (AllocatedBlock*) (p->v + sz);
    }
    return 1;
}

/*
** Load the byte vector pointed to by v of length vsz
** with a heap image
** The scale is defined by vsz and the current heap size
** free = 0, full = 255
** 
** 
*/
int elib_heap_map(EByte* v, int vsz)
{
    AllocatedBlock* p = heap_head;
    EWord sz;
    int gsz = eheap_size / vsz;  /* The granuality used */
    int fsz = 0;
    int usz = 0;

    if (gsz == 0)
	return -1;  /* too good reolution */

    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    fsz += sz;
	    if ((fsz + usz) > gsz) {
		*v++ = (255*usz)/gsz;
		fsz -= (gsz - usz);
		usz = 0;
		while(fsz >= gsz) {
		    *v++ = 0;
		    fsz -= gsz;
		}
	    }
	}
	else {
	    usz += sz;
	    if ((fsz + usz) > gsz) {
		*v++ = 255 - (255*fsz)/gsz;
		usz -= (gsz - fsz);
		fsz = 0;
		while(usz >= gsz) {
		    *v++ = 255;
		    usz -= gsz;
		}
	    }
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
    return 0;
}

/*
** Generate a histogram of free/allocated blocks
** Count granuality of 10 gives
** (0-10],(10-100],(100-1000],(1000-10000] ...
** (0-2], (2-4], (4-8], (8-16], ....
*/
static int i_logb(EWord size, int base)
{
    int lg = 0;
    while(size >= base) {
	size /= base;
	lg++;
    }
    return lg;
}

int elib_histo(EWord* vf, EWord* va, int vsz, int base)
{
    AllocatedBlock* p = heap_head;
    EWord sz;
    int i;
    int linear;

    if ((vsz <= 1) || (vf == 0 && va == 0))
	return -1;

    if (base < 0) {
	linear = 1;
	base = -base;
    }
    else
	linear = 0;

    if (base <= 1)
	return -1;

    if (vf != 0) {
	for (i = 0; i < vsz; i++)
	    vf[i] = 0;
    }
    if (va != 0) {
	for (i = 0; i < vsz; i++)
	    va[i] = 0;
    }

    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    if (vf != 0) {
		int val;
		if (linear)
		    val = sz / base;
		else
		    val = i_logb(sz, base);
		if (val >= vsz)
		    vf[vsz-1]++;
		else
		    vf[val]++;
	    }
	}
	else {
	    if (va != 0) {
		int val;
		if (linear)
		    val = sz / base;
		else
		    val = i_logb(sz, base);
		if (val >= vsz)
		    va[vsz-1]++;
		else
		    va[val]++;
	    }
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
    return 0;
}

/*
** Fill the info structure with actual values
** Total
** Allocated
** Free
** maxMaxFree     
*/
void elib_stat(struct elib_stat* info)
{
    EWord blks = 0;
    EWord sz_free = 0;
    EWord sz_alloc = 0;
    EWord sz_max_free = 0;
    EWord sz_min_used = 0x7fffffff;
    EWord sz;
    EWord num_free = 0;
    AllocatedBlock* p = heap_head;

    info->mem_total = eheap_size;

    p = (AllocatedBlock*) (p->v + SIZEOF(p));

    while((sz = SIZEOF(p)) != 0) {
	blks++;
	if (IS_FREE(p)) {
	    if (sz > sz_max_free)
		sz_max_free = sz;
	    sz_free += sz;
	    ++num_free;
	}
	else {
	    if (sz < sz_min_used)
		sz_min_used = sz;
	    sz_alloc += sz;
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
    info->mem_blocks = blks;
    info->free_blocks = num_free;
    info->mem_alloc = sz_alloc;
    info->mem_free = sz_free;
    info->min_used = sz_min_used;
    info->max_free = sz_max_free;
    info->mem_max_alloc = max_allocated;
    ASSERT(sz_alloc == tot_allocated);
}

/*
** Dump the heap
*/
void elib_heap_dump(char* label)
{
    AllocatedBlock* p = heap_head;
    EWord sz;

    elib_printf(stderr, "HEAP DUMP (%s)\n", label);
    if (!elib_check_heap())
	return;
    
    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    elib_printf(stderr, "%p: FREE, size = %d\n", p, (int) sz);
	}
	else {
	    elib_printf(stderr, "%p: USED, size = %d %s\n", p, (int) sz,
		       IS_FREE_ABOVE(p)?"(FREE ABOVE)":"");
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
}

/*
**  Scan heaps and count:
**  free_size, allocated_size, max_free_block
*/
void elib_statistics(void* to)
{
    struct elib_stat info;
    EWord frag;

    if (!elib_check_heap())
	return;

    elib_stat(&info);

    frag = 1000 - ((1000 * info.max_free) / info.mem_free);

    elib_printf(to, "Heap Statistics: total(%d), blocks(%d), frag(%d.%d%%)\n", 
	       info.mem_total, info.mem_blocks,
	       (int) frag/10, (int) frag % 10);

    elib_printf(to, "                 allocated(%d), free(%d), "
		"free_blocks(%d)\n",
	       info.mem_alloc, info.mem_free,info.free_blocks);
    elib_printf(to, "                 max_free(%d),  min_used(%d)\n",
	       info.max_free, info.min_used);
}

/*
** Allocate a least nb bytes with alignment a
** Algorithm:
**    1) Try locate a block which match exacly among the by direct index.
**    2) Try using a fix block of greater size
**    3) Try locate a block by searching in lists where block sizes
**       X may vary between 2^i < X <= 2^(i+1)
**
** Reset memory to zero if clear is true
*/
static AllocatedBlock* allocate(EWord nb, EWord a, int clear)
{
    FreeBlock* p;
    EWord nw;

    if (a == ELIB_ALIGN) {
	/*
	 * Common case: Called by malloc(), realloc(), calloc().
	 */
	nw = nb < MIN_BYTE_SIZE ? MIN_ALIGN_SIZE : ALIGN_SIZE(nb);

	if ((p = alloc_block(nw)) == 0)
	    return NULL;
    } else {
	/*
	 * Special case: Called by memalign().
	 */
	EWord asz, szp, szq, tmpsz;
	FreeBlock *q;

	if ((p = alloc_block((1+MIN_ALIGN_SIZE)*sizeof(EWord)+a-1+nb)) == 0)
	    return NULL;

	asz = a - ((EWord) ((AllocatedBlock *)p)->v) % a;

	if (asz != a) {
	    /* Enforce the alignment requirement by cutting of a free
	       block at the beginning of the block. */

	    if (asz < (1+MIN_ALIGN_SIZE)*sizeof(EWord) && !IS_FREE_ABOVE(p)) {
		/* Not enough room to cut of a free block;
		   increase align size */
		asz += (((1+MIN_ALIGN_SIZE)*sizeof(EWord) + a - 1)/a)*a;
	    }

	    szq = ALIGN_SIZE(asz - sizeof(EWord));
	    szp = SIZEOF(p) - szq - 1;

	    q = p;
	    p = (FreeBlock*) (((EWord*) q) + szq + 1);
	    p->hdr = FREE_ABOVE_BIT | FREE_BIT | szp;

	    if (IS_FREE_ABOVE(q)) { /* This should not be possible I think,
				       but just in case... */
		tmpsz = SIZEOF_ABOVE(q) + 1;
		szq += tmpsz;
		q = (FreeBlock*) (((EWord*) q) - tmpsz);
		unlink_free_block((Block_t *) q);
		q->hdr = (q->hdr & FREE_ABOVE_BIT) | FREE_BIT | szq;
	    }
	    mark_free(q, szq);
	    link_free_block((Block_t *) q);

	} /* else already had the correct alignment */
 
	nw = nb < MIN_BYTE_SIZE ? MIN_ALIGN_SIZE : ALIGN_SIZE(nb);
    }

    split_block(p, nw, SIZEOF(p));

    STAT_ALLOCED_BLOCK(SIZEOF(p));

    if (clear) {
	EWord* pp = ((AllocatedBlock*)p)->v;

	while(nw--)
	    *pp++ = 0;
    }

    return (AllocatedBlock*) p;
}


/*
** Deallocate memory pointed to by p
** 1. Merge with block above if this block is free
** 2. Merge with block below if this block is free
** Link the block to the correct free list
**
** p points to the block header!
**
*/
static void deallocate(AllocatedBlock* p, int stat_count)
{
    FreeBlock* q;
    EWord szq;
    EWord szp;

    szp = SIZEOF(p);

    if (stat_count)
	STAT_FREED_BLOCK(SIZEOF(p));

    if (IS_FREE_ABOVE(p)) {
	szq = SIZEOF_ABOVE(p);
	q = (FreeBlock*) ( ((EWord*) p) - szq - 1);
	unlink_free_block((Block_t *) q);

	p = (AllocatedBlock*) q;
	szp += (szq + 1);
    }
    q = (FreeBlock*) (p->v + szp);
    if (IS_FREE(q)) {
	szq = SIZEOF(q);
	unlink_free_block((Block_t *) q);
	szp += (szq + 1);
    }
    else
	q->hdr |= FREE_ABOVE_BIT;

    /* The block above p can NEVER be free !!! */
    p->hdr = FREE_BIT | szp;
    p->v[szp-1] = szp;

    link_free_block((Block_t *) p);
}

/*
** Reallocate memory
** If preserve is true then data is moved if neccesary
*/
static AllocatedBlock* reallocate(AllocatedBlock* p, EWord nb, int preserve)
{
    EWord szp;
    EWord szq;
    EWord sz;
    EWord nw;
    FreeBlock* q;

    if (nb < MIN_BYTE_SIZE)
	nw = MIN_ALIGN_SIZE;
    else
	nw = ALIGN_SIZE(nb);

    sz = szp = SIZEOF(p);

    STAT_FREED_BLOCK(szp);

    /* Merge with block below */
    q = (FreeBlock*) (p->v + szp);
    if (IS_FREE(q)) {
	szq = SIZEOF(q);
	unlink_free_block((Block_t *) q);
	szp += (szq + 1);
    }

    if (nw <= szp) {
	split_block((FreeBlock *) p, nw, szp);
	STAT_ALLOCED_BLOCK(SIZEOF(p));
	return p;
    }
    else {
	EWord* dp = p->v;
	AllocatedBlock* npp;

	if (IS_FREE_ABOVE(p)) {
	    szq = SIZEOF_ABOVE(p);
	    if (szq + szp + 1 >= nw) {
		q = (FreeBlock*) (((EWord*) p) - szq - 1);
		unlink_free_block((Block_t * )q);
		szp += (szq + 1);
		p = (AllocatedBlock*) q;

		if (preserve) {
		    EWord* pp = p->v;
		    while(sz--)
			*pp++ = *dp++;
		}
		split_block((FreeBlock *) p, nw, szp);
		STAT_ALLOCED_BLOCK(SIZEOF(p));
		return p;
	    }
	}

	/*
	 * Update p so that allocate() and deallocate() works.
	 * (Note that allocate() may call expand_sbrk(), which in
	 * in turn calls deallocate().)
	 */

	p->hdr = (p->hdr & FREE_ABOVE_BIT) | szp;
	p->v[szp] &= ~FREE_ABOVE_BIT;

	npp = allocate(nb, ELIB_ALIGN, 0);
	if(npp == NULL)
	    return NULL;
	if (preserve) {
	    EWord* pp = npp->v;
	    while(sz--)
		*pp++ = *dp++;
	}
	deallocate(p, 0);
	return npp;
    }
}

/*
** What malloc() and friends should do (and return) when the heap is
** exhausted.  [sverkerw]
*/
static void* heap_exhausted(void)
{
    /* Choose behaviour */
#if 0
    /* Crash-and-burn --- leave a usable corpse (hopefully) */
    abort();
#endif    
    /* The usual ANSI-compliant behaviour */
    return NULL;
}

/*
** Allocate size bytes of memory
*/
void* ELIB_PREFIX(malloc, (size_t nb))
{
    void *res;
    AllocatedBlock* p;

    erts_mtx_lock(&malloc_mutex);
    if (elib_need_init)
	locked_elib_init(NULL,(EWord)0);

    if (nb == 0)
	res = NULL;
    else if ((p = allocate(nb, ELIB_ALIGN, 0)) != 0) {
	ELIB_ALIGN_CHECK(p->v);
	res = p->v;
    }
    else
	res = heap_exhausted();

    erts_mtx_unlock(&malloc_mutex);

    return res;
}


void* ELIB_PREFIX(calloc, (size_t nelem, size_t size))
{
    void *res;
    int nb;
    AllocatedBlock* p;
    
    erts_mtx_lock(&malloc_mutex);
    if (elib_need_init)
	locked_elib_init(NULL,(EWord)0);

    if ((nb = nelem * size) == 0)
	res = NULL;
    else if ((p = allocate(nb, ELIB_ALIGN, 1)) != 0) {
	ELIB_ALIGN_CHECK(p->v);
	res = p->v;
    }
    else
	res = heap_exhausted();

    erts_mtx_unlock(&malloc_mutex);

    return res;
}

/*
** Free memory allocated by malloc
*/

void ELIB_PREFIX(free, (EWord* p))
{
    erts_mtx_lock(&malloc_mutex);
    if (elib_need_init)
	locked_elib_init(NULL,(EWord)0);

    if (p != 0)
	deallocate((AllocatedBlock*)(p-1), 1);

    erts_mtx_unlock(&malloc_mutex);
}

void ELIB_PREFIX(cfree, (EWord* p))
{
    ELIB_PREFIX(free, (p));
}


/*
** Realloc the memory allocated in p to nb number of bytes
**
*/

void* ELIB_PREFIX(realloc, (EWord* p, size_t nb))
{
    void *res = NULL;
    AllocatedBlock* pp;

    erts_mtx_lock(&malloc_mutex);
    if (elib_need_init)
	locked_elib_init(NULL,(EWord)0);

    if (p != 0) {
	pp = (AllocatedBlock*) (p-1);
	if (nb > 0) {
	    if ((pp = reallocate(pp, nb, 1)) != 0) {
		ELIB_ALIGN_CHECK(pp->v);
		res = pp->v;
	    }
	}
	else
	    deallocate(pp, 1);
    }
    else if (nb > 0) {
	if ((pp = allocate(nb, ELIB_ALIGN, 0)) != 0) {
	    ELIB_ALIGN_CHECK(pp->v);
	    res = pp->v;
	}
	else
	    res = heap_exhausted();
    }

    erts_mtx_unlock(&malloc_mutex);

    return res;
}

/*
** Resize the memory area pointed to by p with nb number of bytes
*/
void* ELIB_PREFIX(memresize, (EWord* p, int nb))
{
    void *res = NULL;
    AllocatedBlock* pp;

    erts_mtx_lock(&malloc_mutex);
    if (elib_need_init)
	locked_elib_init(NULL,(EWord)0);

    if (p != 0) {
	pp = (AllocatedBlock*) (p-1);
	if (nb > 0) {
	    if ((pp = reallocate(pp, nb, 0)) != 0) {
		ELIB_ALIGN_CHECK(pp->v);
		res = pp->v;
	    }
	}
	else
	    deallocate(pp, 1);
    }
    else if (nb > 0) {
	if ((pp = allocate(nb, ELIB_ALIGN, 0)) != 0) {
	    ELIB_ALIGN_CHECK(pp->v);
	    res = pp->v;
	}
	else
	    res = heap_exhausted();
    }

    erts_mtx_unlock(&malloc_mutex);

    return res;
}


/* Create aligned memory a must be a power of 2 !!! */

void* ELIB_PREFIX(memalign, (int a, int nb))
{
    void *res;
    AllocatedBlock* p;

    erts_mtx_lock(&malloc_mutex);
    if (elib_need_init)
	locked_elib_init(NULL,(EWord)0);

    if (nb == 0 || a <= 0)
	res = NULL;
    else if ((p = allocate(nb, a, 0)) != 0) {
	ALIGN_CHECK(a, p->v);
	res = p->v;
    }
    else
	res = heap_exhausted();

    erts_mtx_unlock(&malloc_mutex);

    return res;
}

void* ELIB_PREFIX(valloc, (int nb))
{
    return ELIB_PREFIX(memalign, (page_size, nb));
}


void* ELIB_PREFIX(pvalloc, (int nb))
{
    return ELIB_PREFIX(memalign, (page_size, PAGES(nb)*page_size));
}
/* Return memory size for pointer p in bytes */

int ELIB_PREFIX(memsize, (p))
EWord* p;
{
    return SIZEOF((AllocatedBlock*)(p-1))*4;
}


/*
** --------------------------------------------------------------------------
**   DEBUG LIBRARY
** --------------------------------------------------------------------------
*/

#ifdef ELIB_DEBUG

#define IN_HEAP(p)  (((p) >= (char*) eheap) && (p) < (char*) eheap_top)
/*
** ptr_to_block: return the pointer to heap block pointed into by ptr
** Returns 0 if not pointing into a block
*/

static EWord* ptr_to_block(char* ptr)
{
    AllocatedBlock* p = heap_head;
    EWord sz;

    while((sz = SIZEOF(p)) != 0) {
	if ((ptr >= (char*) p->v) && (ptr < (char*)(p->v+sz)))
	    return p->v;
	p = (AllocatedBlock*) (p->v + sz);
    }
    return 0;
}

/*
** Validate a pointer
** returns:
**      0  - if points to start of a block
**      1  - if points outsize heap
**     -1  - if points inside block
**
*/
static int check_pointer(char* ptr)
{
    if (IN_HEAP(ptr)) {
	if (ptr_to_block(ptr) == 0)
	    return 1;
	return 0;
    }
    return -1;
}

/*
** Validate a memory area
** returns:
**      0 - if area is included in a block
**     -1 - if area overlap a heap block
**      1 - if area is outside heap
*/
static int check_area(char* ptr, int n)
{
    if (IN_HEAP(ptr)) {
	if (IN_HEAP(ptr+n-1)) {
	    EWord* p1 = ptr_to_block(ptr);
	    EWord* p2 = ptr_to_block(ptr+n-1);

	    if (p1 == p2)
		return (p1 == 0) ? -1 : 0;
	    return -1;
	}
    }
    else if (IN_HEAP(ptr+n-1))
	return -1;
    return 1;
}

/*
** Check if a block write will overwrite heap block
*/
static void check_write(char* ptr, int n, char* file, int line, char* fun)
{
    if (check_area(ptr, n) == -1) {
	elib_printf(stderr, "RUNTIME ERROR: %s heap overwrite\n", fun);
	elib_printf(stderr, "File: %s Line: %d\n", file, line);
	ELIB_FAILURE;
    }
}

/*
** Check if a pointer is an allocated object
*/
static void check_allocated_block(char* ptr, char* file, int line, char* fun)
{
    EWord* q;

    if (!IN_HEAP(ptr) || ((q=ptr_to_block(ptr)) == 0) || (ptr != (char*) q)) {
	elib_printf(stderr, "RUNTIME ERROR: %s non heap pointer\n", fun);
	elib_printf(stderr, "File: %s Line: %d\n", file, line);
	ELIB_FAILURE;
    }

    if (IS_FREE((AllocatedBlock*)(q-1))) {
	elib_printf(stderr, "RUNTIME ERROR: %s free pointer\n", fun);
	elib_printf(stderr, "File: %s Line: %d\n", file, line);
	ELIB_FAILURE;
    }

}

/*
** --------------------------------------------------------------------------
**  DEBUG VERSIONS (COMPILED WITH THE ELIB.H)
** --------------------------------------------------------------------------
*/

void* elib_dbg_malloc(int n, char* file, int line)
{
    return elib__malloc(n);
}

void* elib_dbg_calloc(int n, int s, char* file, int line)
{
    return elib__calloc(n, s);
}

void* elib_dbg_realloc(EWord* p, int n, char* file, int line)
{
    if (p == 0)
	return elib__malloc(n);
    check_allocated_block(p, file, line, "elib_realloc");
    return elib__realloc(p, n);
}

void elib_dbg_free(EWord* p, char* file, int line)
{
    if (p == 0)
	return;
    check_allocated_block(p, file, line, "elib_free");
    elib__free(p);
}

void elib_dbg_cfree(EWord* p, char* file, int line)
{
    if (p == 0)
	return;
    check_allocated_block(p, file, line, "elib_free");
    elib__cfree(p);
}

void* elib_dbg_memalign(int a, int n, char* file, int line)
{
    return elib__memalign(a, n);
}

void* elib_dbg_valloc(int n, char* file, int line)
{
    return elib__valloc(n);
}

void* elib_dbg_pvalloc(int n, char* file, int line)
{
    return elib__pvalloc(n);
}

void* elib_dbg_memresize(EWord* p, int n, char* file, int line)
{
    if (p == 0)
	return elib__malloc(n);
    check_allocated_block(p, file, line, "elib_memresize");
    return elib__memresize(p, n);
}

int elib_dbg_memsize(void* p, char* file, int line)
{
    check_allocated_block(p, file, line, "elib_memsize");
    return elib__memsize(p);
}

/*
** --------------------------------------------------------------------------
**  LINK TIME FUNCTIONS (NOT COMPILED CALLS)
** --------------------------------------------------------------------------
*/

void* elib_malloc(int n)
{
    return elib_dbg_malloc(n, "", -1);
}

void* elib_calloc(int n, int s)
{
    return elib_dbg_calloc(n, s, "", -1);
}

void* elib_realloc(EWord* p, int n)
{
    return elib_dbg_realloc(p, n, "", -1);
}

void elib_free(EWord* p)
{
    elib_dbg_free(p, "", -1);
}

void elib_cfree(EWord* p)
{
    elib_dbg_cfree(p, "", -1);
}

void* elib_memalign(int a, int n)
{
    return elib_dbg_memalign(a, n, "", -1);
}

void* elib_valloc(int n)
{
    return elib_dbg_valloc(n, "", -1);
}

void* elib_pvalloc(int n)
{
    return elib_dbg_pvalloc(n, "", -1);
}

void* elib_memresize(EWord* p, int n)
{
    return elib_dbg_memresize(p, n, "", -1);
}


int elib_memsize(EWord* p)
{
    return elib_dbg_memsize(p, "", -1);
}

#endif /* ELIB_DEBUG */

/*
** --------------------------------------------------------------------------
** Map c library functions to elib
** --------------------------------------------------------------------------
*/

#if defined(ELIB_ALLOC_IS_CLIB)
void* malloc(size_t nb)
{
    return elib_malloc(nb);
}

void* calloc(size_t nelem, size_t size)
{
    return elib_calloc(nelem, size);
}


void free(void *p)
{
    elib_free(p);
}

void cfree(void *p)
{
    elib_cfree(p);
}

void* realloc(void* p, size_t nb)
{
    return elib_realloc(p, nb);
}


void* memalign(size_t a, size_t s)
{
    return elib_memalign(a, s);
}

void* valloc(size_t nb)
{
    return elib_valloc(nb);
}

void* pvalloc(size_t nb)
{
    return elib_pvalloc(nb);
}

#if 0
void* memresize(void* p, int nb)
{
    return elib_memresize(p, nb);
}

int memsize(void* p)
{
    return elib_memsize(p);
}
#endif
#endif /* ELIB_ALLOC_IS_CLIB */

#endif /* ENABLE_ELIB_MALLOC */

void elib_ensure_initialized(void)
{
#ifdef ENABLE_ELIB_MALLOC
#ifndef ELIB_DONT_INITIALIZE
    elib_init(NULL, 0);
#endif
#endif
}

#ifdef ENABLE_ELIB_MALLOC
/**
 ** A Slightly modified version of the "address order best fit" algorithm
 ** used in erl_bestfit_alloc.c. Comments refer to that implementation.
 **/

/*
 * Description:	A combined "address order best fit"/"best fit" allocator
 *              based on a Red-Black (binary search) Tree. The search,
 *              insert, and delete operations are all O(log n) operations
 *              on a Red-Black Tree. In the "address order best fit" case
 *              n equals number of free blocks, and in the "best fit" case
 *              n equals number of distinct sizes of free blocks. Red-Black
 *              Trees are described in "Introduction to Algorithms", by
 *              Thomas H. Cormen, Charles E. Leiserson, and
 *              Ronald L. Riverest.
 *
 *              This module is a callback-module for erl_alloc_util.c
 *
 * Author: 	Rickard Green
 */

#ifdef DEBUG
#if 0
#define HARD_DEBUG
#endif
#else
#undef HARD_DEBUG
#endif

#define SZ_MASK			SIZE_MASK
#define FLG_MASK		(~(SZ_MASK))

#define BLK_SZ(B)  (*((Block_t *) (B)) & SZ_MASK)

#define TREE_NODE_FLG		(((Uint) 1) << 0)
#define RED_FLG			(((Uint) 1) << 1)
#ifdef HARD_DEBUG
#  define LEFT_VISITED_FLG	(((Uint) 1) << 2)
#  define RIGHT_VISITED_FLG	(((Uint) 1) << 3)
#endif

#define IS_TREE_NODE(N)		(((RBTree_t *) (N))->flags & TREE_NODE_FLG)
#define IS_LIST_ELEM(N)		(!IS_TREE_NODE(((RBTree_t *) (N))))

#define SET_TREE_NODE(N)	(((RBTree_t *) (N))->flags |= TREE_NODE_FLG)
#define SET_LIST_ELEM(N)	(((RBTree_t *) (N))->flags &= ~TREE_NODE_FLG)

#define IS_RED(N)		(((RBTree_t *) (N)) \
				 && ((RBTree_t *) (N))->flags & RED_FLG)
#define IS_BLACK(N)		(!IS_RED(((RBTree_t *) (N))))

#define SET_RED(N)		(((RBTree_t *) (N))->flags |= RED_FLG)
#define SET_BLACK(N)		(((RBTree_t *) (N))->flags &= ~RED_FLG)

#undef ASSERT
#define ASSERT ASSERT_EXPR

#if 1
#define RBT_ASSERT	ASSERT
#else
#define RBT_ASSERT(x)
#endif


#ifdef HARD_DEBUG
static RBTree_t * check_tree(Uint);
#endif

#ifdef ERTS_INLINE
#  ifndef ERTS_CAN_INLINE
#    define ERTS_CAN_INLINE 1
#  endif
#else
#  if defined(__GNUC__)
#    define ERTS_CAN_INLINE 1
#    define ERTS_INLINE __inline__
#  elif defined(__WIN32__)
#    define ERTS_CAN_INLINE 1
#    define ERTS_INLINE __inline
#  else
#    define ERTS_CAN_INLINE 0
#    define ERTS_INLINE
#  endif
#endif

/* Types... */
#if 0
typedef struct RBTree_t_ RBTree_t;

struct RBTree_t_ {
    Block_t hdr;
    Uint flags;
    RBTree_t *parent;
    RBTree_t *left;
    RBTree_t *right;
};
#endif

#if 0
typedef struct {
    RBTree_t t;
    RBTree_t *next;
} RBTreeList_t;

#define LIST_NEXT(N) (((RBTreeList_t *) (N))->next)
#define LIST_PREV(N) (((RBTreeList_t *) (N))->t.parent)
#endif

#ifdef DEBUG

/* Destroy all tree fields */
#define DESTROY_TREE_NODE(N)						\
  sys_memset((void *) (((Block_t *) (N)) + 1),				\
	     0xff,							\
	     (sizeof(RBTree_t) - sizeof(Block_t)))

/* Destroy all tree and list fields */
#define DESTROY_LIST_ELEM(N)						\
  sys_memset((void *) (((Block_t *) (N)) + 1),				\
	     0xff,							\
	     (sizeof(RBTreeList_t) - sizeof(Block_t)))

#else

#define DESTROY_TREE_NODE(N)
#define DESTROY_LIST_ELEM(N)

#endif


/*
 * Red-Black Tree operations needed
 */

static ERTS_INLINE void
left_rotate(RBTree_t **root, RBTree_t *x)
{
    RBTree_t *y = x->right;
    x->right = y->left;
    if (y->left)
	y->left->parent = x;
    y->parent = x->parent;
    if (!y->parent) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == x->parent->left)
	x->parent->left = y;
    else {
	RBT_ASSERT(x == x->parent->right);
	x->parent->right = y;
    }
    y->left = x;
    x->parent = y;
}

static ERTS_INLINE void
right_rotate(RBTree_t **root, RBTree_t *x)
{
    RBTree_t *y = x->left;
    x->left = y->right;
    if (y->right)
	y->right->parent = x;
    y->parent = x->parent;
    if (!y->parent) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == x->parent->right)
	x->parent->right = y;
    else {
	RBT_ASSERT(x == x->parent->left);
	x->parent->left = y;
    }
    y->right = x;
    x->parent = y;
}


/*
 * Replace node x with node y
 * NOTE: block header of y is not changed
 */
static ERTS_INLINE void
replace(RBTree_t **root, RBTree_t *x, RBTree_t *y)
{

    if (!x->parent) {
	RBT_ASSERT(*root == x);
	*root = y;
    }
    else if (x == x->parent->left)
	x->parent->left = y;
    else {
	RBT_ASSERT(x == x->parent->right);
	x->parent->right = y;
    }
    if (x->left) {
	RBT_ASSERT(x->left->parent == x);
	x->left->parent = y;
    }
    if (x->right) {
	RBT_ASSERT(x->right->parent == x);
	x->right->parent = y;
    }

    y->flags	= x->flags;
    y->parent	= x->parent;
    y->right	= x->right;
    y->left	= x->left;

    DESTROY_TREE_NODE(x);

}

static void
tree_insert_fixup(RBTree_t *blk)
{
    RBTree_t *x = blk, *y;

    /*
     * Rearrange the tree so that it satisfies the Red-Black Tree properties
     */

    RBT_ASSERT(x != root && IS_RED(x->parent));
    do {

	/*
	 * x and its parent are both red. Move the red pair up the tree
	 * until we get to the root or until we can separate them.
	 */

	RBT_ASSERT(IS_RED(x));
	RBT_ASSERT(IS_BLACK(x->parent->parent));
	RBT_ASSERT(x->parent->parent);

	if (x->parent == x->parent->parent->left) {
	    y = x->parent->parent->right;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		x = x->parent;
		SET_BLACK(x);
		x = x->parent;
		SET_RED(x);
	    }
	    else {

		if (x == x->parent->right) {
		    x = x->parent;
		    left_rotate(&root, x);
		}

		RBT_ASSERT(x == x->parent->parent->left->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent));
		RBT_ASSERT(IS_BLACK(x->parent->parent));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(x->parent);
		SET_RED(x->parent->parent);
		right_rotate(&root, x->parent->parent);

		RBT_ASSERT(x == x->parent->left);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent->right));
		RBT_ASSERT(IS_BLACK(x->parent));
		break;
	    }
	}
	else {
	    RBT_ASSERT(x->parent == x->parent->parent->right);
	    y = x->parent->parent->left;
	    if (IS_RED(y)) {
		SET_BLACK(y);
		x = x->parent;
		SET_BLACK(x);
		x = x->parent;
		SET_RED(x);
	    }
	    else {

		if (x == x->parent->left) {
		    x = x->parent;
		    right_rotate(&root, x);
		}

		RBT_ASSERT(x == x->parent->parent->right->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent));
		RBT_ASSERT(IS_BLACK(x->parent->parent));
		RBT_ASSERT(IS_BLACK(y));

		SET_BLACK(x->parent);
		SET_RED(x->parent->parent);
		left_rotate(&root, x->parent->parent);

		RBT_ASSERT(x == x->parent->right);
		RBT_ASSERT(IS_RED(x));
		RBT_ASSERT(IS_RED(x->parent->left));
		RBT_ASSERT(IS_BLACK(x->parent));
		break;
	    }
	}
    } while (x != root && IS_RED(x->parent));

    SET_BLACK(root);
}

static void
unlink_free_block(Block_t *del)
{
    Uint spliced_is_black;
    RBTree_t *x, *y, *z = (RBTree_t *) del;
    RBTree_t null_x; /* null_x is used to get the fixup started when we
			splice out a node without children. */

    null_x.parent = NULL;

#ifdef HARD_DEBUG
    check_tree(0);
#endif

    /* Remove node from tree... */

    /* Find node to splice out */
    if (!z->left || !z->right)
	y = z;
    else
	/* Set y to z:s successor */
	for(y = z->right; y->left; y = y->left);
    /* splice out y */
    x = y->left ? y->left : y->right;
    spliced_is_black = IS_BLACK(y);
    if (x) {
	x->parent = y->parent;
    }
    else if (!x && spliced_is_black) {
	x = &null_x;
	x->flags = 0;
	SET_BLACK(x);
	x->right = x->left = NULL;
	x->parent = y->parent;
	y->left = x;
    }

    if (!y->parent) {
	RBT_ASSERT(root == y);
	root = x;
    }
    else if (y == y->parent->left)
	y->parent->left = x;
    else {
	RBT_ASSERT(y == y->parent->right);
	y->parent->right = x;
    }
    if (y != z) {
	/* We spliced out the successor of z; replace z by the successor */
	replace(&root, z, y);
    }

    if (spliced_is_black) {
	/* We removed a black node which makes the resulting tree
	   violate the Red-Black Tree properties. Fixup tree... */

	while (IS_BLACK(x) && x->parent) {

	    /*
	     * x has an "extra black" which we move up the tree
	     * until we reach the root or until we can get rid of it.
	     *
	     * y is the sibbling of x
	     */

	    if (x == x->parent->left) {
		y = x->parent->right;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(x->parent));
		    SET_RED(x->parent);
		    left_rotate(&root, x->parent);
		    y = x->parent->right;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->left) && IS_BLACK(y->right)) {
		    SET_RED(y);
		    x = x->parent;
		}
		else {
		    if (IS_BLACK(y->right)) {
			SET_BLACK(y->left);
			SET_RED(y);
			right_rotate(&root, y);
			y = x->parent->right;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(x->parent)) {

			SET_BLACK(x->parent);
			SET_RED(y);
		    }
		    RBT_ASSERT(y->right);
		    SET_BLACK(y->right);
		    left_rotate(&root, x->parent);
		    x = root;
		    break;
		}
	    }
	    else {
		RBT_ASSERT(x == x->parent->right);
		y = x->parent->left;
		RBT_ASSERT(y);
		if (IS_RED(y)) {
		    RBT_ASSERT(y->right);
		    RBT_ASSERT(y->left);
		    SET_BLACK(y);
		    RBT_ASSERT(IS_BLACK(x->parent));
		    SET_RED(x->parent);
		    right_rotate(&root, x->parent);
		    y = x->parent->left;
		}
		RBT_ASSERT(y);
		RBT_ASSERT(IS_BLACK(y));
		if (IS_BLACK(y->right) && IS_BLACK(y->left)) {
		    SET_RED(y);
		    x = x->parent;
		}
		else {
		    if (IS_BLACK(y->left)) {
			SET_BLACK(y->right);
			SET_RED(y);
			left_rotate(&root, y);
			y = x->parent->left;
		    }
		    RBT_ASSERT(y);
		    if (IS_RED(x->parent)) {
			SET_BLACK(x->parent);
			SET_RED(y);
		    }
		    RBT_ASSERT(y->left);
		    SET_BLACK(y->left);
		    right_rotate(&root, x->parent);
		    x = root;
		    break;
		}
	    }
	}
	SET_BLACK(x);

	if (null_x.parent) {
	    if (null_x.parent->left == &null_x)
		null_x.parent->left = NULL;
	    else {
		RBT_ASSERT(null_x.parent->right == &null_x);
		null_x.parent->right = NULL;
	    }
	    RBT_ASSERT(!null_x.left);
	    RBT_ASSERT(!null_x.right);
	}
	else if (root == &null_x) {
	    root = NULL;
	    RBT_ASSERT(!null_x.left);
	    RBT_ASSERT(!null_x.right);
	}
    }


    DESTROY_TREE_NODE(del);

#ifdef HARD_DEBUG
    check_tree(0);
#endif

}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * "Address order best fit" specific callbacks.                              *
\*                                                                           */

static void
link_free_block(Block_t *block)
{
    RBTree_t *blk = (RBTree_t *) block;
    Uint blk_sz = BLK_SZ(blk);

    blk->flags	= 0;
    blk->left	= NULL;
    blk->right	= NULL;

    if (!root) {
	blk->parent = NULL;
	SET_BLACK(blk);
	root = blk;
    } else {
	RBTree_t *x = root;
	while (1) {
	    Uint size;

	    size = BLK_SZ(x);

	    if (blk_sz < size || (blk_sz == size && blk < x)) {
		if (!x->left) {
		    blk->parent = x;
		    x->left = blk;
		    break;
		}
		x = x->left;
	    }
	    else {
		if (!x->right) {
		    blk->parent = x;
		    x->right = blk;
		    break;
		}
		x = x->right;
	    }

	}

	/* Insert block into size tree */
	RBT_ASSERT(blk->parent);

	SET_RED(blk);
	if (IS_RED(blk->parent)) {
	    tree_insert_fixup(blk);
	}
    }

#ifdef HARD_DEBUG
    check_tree(0);
#endif
}


static Block_t *
get_free_block(Uint size)
{
    RBTree_t *x = root;
    RBTree_t *blk = NULL;
    Uint blk_sz;

    while (x) {
	blk_sz = BLK_SZ(x);
	if (blk_sz < size) {
	    x = x->right;
	}
	else {
	    blk = x;
	    x = x->left;
	}
    }
    
    if (!blk)
	return NULL;

#ifdef HARD_DEBUG
    ASSERT(blk == check_tree(size));
#endif

    unlink_free_block((Block_t *) blk);

    return (Block_t *) blk;
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */


#ifdef HARD_DEBUG

#define IS_LEFT_VISITED(FB)	((FB)->flags & LEFT_VISITED_FLG)
#define IS_RIGHT_VISITED(FB)	((FB)->flags & RIGHT_VISITED_FLG)

#define SET_LEFT_VISITED(FB)	((FB)->flags |= LEFT_VISITED_FLG)
#define SET_RIGHT_VISITED(FB)	((FB)->flags |= RIGHT_VISITED_FLG)

#define UNSET_LEFT_VISITED(FB)	((FB)->flags &= ~LEFT_VISITED_FLG)
#define UNSET_RIGHT_VISITED(FB)	((FB)->flags &= ~RIGHT_VISITED_FLG)


#if 0
#  define PRINT_TREE
#else
#  undef PRINT_TREE
#endif

#ifdef PRINT_TREE
static void print_tree(void);
#endif

/*
 * Checks that the order between parent and children are correct,
 * and that the Red-Black Tree properies are satisfied. if size > 0,
 * check_tree() returns a node that satisfies "best fit" resp.
 * "address order best fit".
 *
 * The Red-Black Tree properies are:
 *   1. Every node is either red or black.
 *   2. Every leaf (NIL) is black.
 *   3. If a node is red, then both its children are black.
 *   4. Every simple path from a node to a descendant leaf
 *      contains the same number of black nodes.
 */

static RBTree_t *
check_tree(Uint size)
{
    RBTree_t *res = NULL;
    Sint blacks;
    Sint curr_blacks;
    RBTree_t *x;

#ifdef PRINT_TREE
    print_tree();
#endif

    if (!root)
	return res;

    x = root;
    ASSERT(IS_BLACK(x));
    ASSERT(!x->parent);
    curr_blacks = 1;
    blacks = -1;

    while (x) {
	if (!IS_LEFT_VISITED(x)) {
	    SET_LEFT_VISITED(x);
	    if (x->left) {
		x = x->left;
		if (IS_BLACK(x))
		    curr_blacks++;
		continue;
	    }
	    else {
		if (blacks < 0)
		    blacks = curr_blacks;
		ASSERT(blacks == curr_blacks);
	    }
	}

	if (!IS_RIGHT_VISITED(x)) {
	    SET_RIGHT_VISITED(x);
	    if (x->right) {
		x = x->right;
		if (IS_BLACK(x))
		    curr_blacks++;
		continue;
	    }
	    else {
		if (blacks < 0)
		    blacks = curr_blacks;
		ASSERT(blacks == curr_blacks);
	    }
	}


	if (IS_RED(x)) {
	    ASSERT(IS_BLACK(x->right));
	    ASSERT(IS_BLACK(x->left));
	}

	ASSERT(x->parent || x == root);

	if (x->left) {
	    ASSERT(x->left->parent == x);
	    ASSERT(BLK_SZ(x->left) < BLK_SZ(x)
		   || (BLK_SZ(x->left) == BLK_SZ(x) && x->left < x));
	}

	if (x->right) {
	    ASSERT(x->right->parent == x);
	    ASSERT(BLK_SZ(x->right) > BLK_SZ(x)
		   || (BLK_SZ(x->right) == BLK_SZ(x) && x->right > x));
	}

	if (size && BLK_SZ(x) >= size) {
	    if (!res
		|| BLK_SZ(x) < BLK_SZ(res)
		|| (BLK_SZ(x) == BLK_SZ(res) && x < res))
		res = x;
	}

	UNSET_LEFT_VISITED(x);
	UNSET_RIGHT_VISITED(x);
	if (IS_BLACK(x))
	    curr_blacks--;
	x = x->parent;

    }

    ASSERT(curr_blacks == 0);

    UNSET_LEFT_VISITED(root);
    UNSET_RIGHT_VISITED(root);

    return res;

}


#ifdef PRINT_TREE
#define INDENT_STEP 2

#include <stdio.h>

static void
print_tree_aux(RBTree_t *x, int indent)
{
    int i;

    if (!x) {
	for (i = 0; i < indent; i++) {
	    putc(' ', stderr);
	}
	fprintf(stderr, "BLACK: nil\r\n");
    }
    else {
	print_tree_aux(x->right, indent + INDENT_STEP);
	for (i = 0; i < indent; i++) {
	    putc(' ', stderr);
	}
	fprintf(stderr, "%s: sz=%lu addr=0x%lx\r\n",
		IS_BLACK(x) ? "BLACK" : "RED",
		BLK_SZ(x),
		(Uint) x);
	print_tree_aux(x->left,  indent + INDENT_STEP);
    }
}


static void
print_tree(void)
{
    fprintf(stderr, " --- Size-Adress tree begin ---\r\n");
    print_tree_aux(root, 0);
    fprintf(stderr, " --- Size-Adress tree end ---\r\n");
}

#endif

#endif

#endif /* ENABLE_ELIB_MALLOC */
