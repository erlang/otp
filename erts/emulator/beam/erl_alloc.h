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

#ifndef ERL_ALLOC_H__
#define ERL_ALLOC_H__

#include "erl_alloc_types.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_alloc_util.h"
#ifdef USE_THREADS
#include "erl_threads.h"
#endif
#include "erl_mmap.h"

#ifdef DEBUG
#  undef ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_WANT_INLINE 0
#endif

#ifndef ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_WANT_INLINE 1
#endif

#if ERTS_CAN_INLINE && ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_DO_INLINE 1
#  define ERTS_ALC_INLINE static ERTS_INLINE
#  define ERTS_ALC_FORCE_INLINE static ERTS_FORCE_INLINE
#else
#  define ERTS_ALC_DO_INLINE 0
#  define ERTS_ALC_INLINE
#  define ERTS_ALC_FORCE_INLINE
#endif

#define ERTS_ALC_NO_FIXED_SIZES \
  (ERTS_ALC_N_MAX_A_FIXED_SIZE - ERTS_ALC_N_MIN_A_FIXED_SIZE + 1)

void erts_sys_alloc_init(void);
void *erts_sys_alloc(ErtsAlcType_t, void *, Uint);
void *erts_sys_realloc(ErtsAlcType_t, void *, void *, Uint);
void erts_sys_free(ErtsAlcType_t, void *, void *);
#if ERTS_HAVE_ERTS_SYS_ALIGNED_ALLOC
/*
 * Note 'alignment' must remain the same in calls to
 * 'erts_sys_aligned_realloc()' and 'erts_sys_aligned_free()'
 * as in the initial call to 'erts_sys_aligned_alloc()'.
 */
void *erts_sys_aligned_alloc(UWord alignment, UWord size);
void *erts_sys_aligned_realloc(UWord alignment, void *ptr, UWord size, UWord old_size);
void erts_sys_aligned_free(UWord alignment, void *ptr);
#endif

Eterm erts_memory(int *, void *, void *, Eterm);
Eterm erts_allocated_areas(int *, void *, void *);

Eterm erts_alloc_util_allocators(void *proc);
void erts_allocator_info(int, void *);
Eterm erts_allocator_options(void *proc);

struct process;

int erts_request_alloc_info(struct process *c_p, Eterm ref, Eterm allocs,
			    int only_sz, int internal);

#define ERTS_ALLOC_INIT_DEF_OPTS_INITER {0}
typedef struct {
    int ncpu;
} ErtsAllocInitOpts;

typedef struct {
    Allctr_t *deallctr[ERTS_ALC_A_MAX+1];
    int pref_ix[ERTS_ALC_A_MAX+1];
    int flist_ix[ERTS_ALC_A_MAX+1];
    int pre_alc_ix;
} ErtsSchedAllocData;

void erts_alloc_init(int *argc, char **argv, ErtsAllocInitOpts *eaiop);
void erts_alloc_late_init(void);

#if defined(GET_ERTS_ALC_TEST) || defined(ERTS_ALC_INTERNAL__)
/* Only for testing */
UWord erts_alc_test(UWord,
		    UWord,
		    UWord,
		    UWord);
#endif

#define ERTS_ALC_O_ALLOC		0
#define ERTS_ALC_O_REALLOC		1
#define ERTS_ALC_O_FREE			2

#define ERTS_ALC_E_NOTSUP		0
#define ERTS_ALC_E_NOMEM		1
#define ERTS_ALC_E_NOALLCTR		2

#define ERTS_ALC_MIN_LONG_LIVED_TIME	(10*60*1000)

typedef struct {
    int alloc_util;
    int enabled;
    int thr_spec;
    void *extra;
} ErtsAllocatorInfo_t;

typedef struct {
    void *	(*alloc)	(ErtsAlcType_t, void *, Uint);
    void *	(*realloc)	(ErtsAlcType_t, void *, void *, Uint);
    void	(*free)		(ErtsAlcType_t, void *, void *);
    void *extra;
} ErtsAllocatorFunctions_t;

extern ErtsAllocatorFunctions_t erts_allctrs[ERTS_ALC_A_MAX+1];
extern ErtsAllocatorInfo_t erts_allctrs_info[ERTS_ALC_A_MAX+1];

typedef struct {
    int enabled;
    int dd;
    int aix;
    int size;
    Allctr_t **allctr;
} ErtsAllocatorThrSpec_t;

extern ErtsAllocatorThrSpec_t erts_allctr_thr_spec[ERTS_ALC_A_MAX+1];

typedef struct ErtsAllocatorWrapper_t_ {
    void (*lock)(void);
    void (*unlock)(void);
    struct ErtsAllocatorWrapper_t_* next;
}ErtsAllocatorWrapper_t;
ErtsAllocatorWrapper_t *erts_allctr_wrappers;
extern int erts_allctr_wrapper_prelocked;
extern erts_tsd_key_t erts_allctr_prelock_tsd_key;
void erts_allctr_wrapper_prelock_init(ErtsAllocatorWrapper_t* wrapper);
void erts_allctr_wrapper_pre_lock(void);
void erts_allctr_wrapper_pre_unlock(void);

void erts_alloc_register_scheduler(void *vesdp);
#ifdef ERTS_SMP
void erts_alloc_scheduler_handle_delayed_dealloc(void *vesdp,
						 int *need_thr_progress,
						 ErtsThrPrgrVal *thr_prgr_p,
						 int *more_work);
#endif
erts_aint32_t erts_alloc_fix_alloc_shrink(int ix, erts_aint32_t flgs);

__decl_noreturn void erts_alloc_enomem(ErtsAlcType_t,Uint)		
     __noreturn;
__decl_noreturn void erts_alloc_n_enomem(ErtsAlcType_t,Uint)		
     __noreturn;
__decl_noreturn void erts_realloc_enomem(ErtsAlcType_t,void*,Uint)	
     __noreturn;
__decl_noreturn void erts_realloc_n_enomem(ErtsAlcType_t,void*,Uint)	
     __noreturn;
__decl_noreturn void erts_alc_fatal_error(int,int,ErtsAlcType_t,...)	
     __noreturn;

/* --- DO *NOT* USE THESE DEPRECATED FUNCTIONS ---    Instead use:       */
void *safe_alloc(Uint)               __deprecated; /* erts_alloc()       */
void *safe_realloc(void *, Uint)     __deprecated; /* erts_realloc()     */
void  sys_free(void *)               __deprecated; /* erts_free()        */
void *sys_alloc(Uint )               __deprecated; /* erts_alloc_fnf()   */
void *sys_realloc(void *, Uint)      __deprecated; /* erts_realloc_fnf() */

#undef ERTS_HAVE_IS_IN_LITERAL_RANGE
#if defined(ARCH_32) || defined(ERTS_HAVE_OS_PHYSICAL_MEMORY_RESERVATION)
#  define ERTS_HAVE_IS_IN_LITERAL_RANGE
#endif


/*
 * erts_alloc[_fnf](), erts_realloc[_fnf](), erts_free() works as
 * malloc(), realloc(), and free() with the following exceptions:
 *
 * * They take an extra type argument as first argument which is
 *   the memory type to operate on. Memory types are generated
 *   (as ERTS_ALC_T_[SOMETHING] defines) from the erl_alloc.types
 *   configuration file.
 * * The erts_alloc() and erts_realloc() functions terminate the
 *   emulator if memory cannot be obtained. The _fnf (Failure Not
 *   Fatal) suffixed versions return NULL if memory cannot be
 *   obtained.
 * * They may be static functions so function pointers to "the same"
 *   function may differ.
 *
 * IMPORTANT: Memory allocated or reallocated as type X, can only
 *            be reallocated or deallocated as type X.
 */

#if !ERTS_ALC_DO_INLINE

void *erts_alloc(ErtsAlcType_t type, Uint size);
void *erts_realloc(ErtsAlcType_t type, void *ptr, Uint size);
void erts_free(ErtsAlcType_t type, void *ptr);
void *erts_alloc_fnf(ErtsAlcType_t type, Uint size);
void *erts_realloc_fnf(ErtsAlcType_t type, void *ptr, Uint size);
int erts_is_allctr_wrapper_prelocked(void);
#ifdef ERTS_HAVE_IS_IN_LITERAL_RANGE
int erts_is_in_literal_range(void* ptr);
#endif

#endif /* #if !ERTS_ALC_DO_INLINE */

void *erts_alloc_permanent_cache_aligned(ErtsAlcType_t type, Uint size);

#ifndef ERTS_CACHE_LINE_SIZE
/* Assumed cache line size */
#  define ERTS_CACHE_LINE_SIZE ((UWord) ASSUMED_CACHE_LINE_SIZE)
#  define ERTS_CACHE_LINE_MASK (ERTS_CACHE_LINE_SIZE - 1)
#endif

#if ERTS_ALC_DO_INLINE || defined(ERTS_ALC_INTERNAL__)

ERTS_ALC_INLINE
void *erts_alloc(ErtsAlcType_t type, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].alloc)(
            ERTS_ALC_T2N(type),
            erts_allctrs[ERTS_ALC_T2A(type)].extra,
            size);
    if (!res)
	erts_alloc_n_enomem(ERTS_ALC_T2N(type), size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}

ERTS_ALC_INLINE
void *erts_realloc(ErtsAlcType_t type, void *ptr, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].realloc)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr,
	size);
    if (!res)
	erts_realloc_n_enomem(ERTS_ALC_T2N(type), ptr, size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}

ERTS_ALC_INLINE
void erts_free(ErtsAlcType_t type, void *ptr)
{
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    (*erts_allctrs[ERTS_ALC_T2A(type)].free)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr);
    ERTS_MSACC_POP_STATE_X();
}


ERTS_ALC_INLINE
void *erts_alloc_fnf(ErtsAlcType_t type, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].alloc)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}


ERTS_ALC_INLINE
void *erts_realloc_fnf(ErtsAlcType_t type, void *ptr, Uint size)
{
    void *res;
    ERTS_MSACC_PUSH_AND_SET_STATE_X(ERTS_MSACC_STATE_ALLOC);
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].realloc)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr,
	size);
    ERTS_MSACC_POP_STATE_X();
    return res;
}

ERTS_ALC_INLINE
int erts_is_allctr_wrapper_prelocked(void)
{
    return erts_allctr_wrapper_prelocked                 /* locked */
	&& !!erts_tsd_get(erts_allctr_prelock_tsd_key);  /* by me  */
}

#ifdef ERTS_HAVE_IS_IN_LITERAL_RANGE

ERTS_ALC_FORCE_INLINE
int erts_is_in_literal_range(void* ptr)
{
#if defined(ARCH_32)
    Uint ix = (UWord)ptr >> ERTS_MMAP_SUPERALIGNED_BITS;

    return erts_literal_vspace_map[ix / ERTS_VSPACE_WORD_BITS]
                  & ((UWord)1 << (ix % ERTS_VSPACE_WORD_BITS));

#elif defined(ARCH_64)
    extern char* erts_literals_start;
    extern UWord erts_literals_size;
    return ErtsInArea(ptr, erts_literals_start, erts_literals_size);
#else
# error No ARCH_xx
#endif
}

#endif /* ERTS_HAVE_IS_IN_LITERAL_RANGE */

#endif /* #if ERTS_ALC_DO_INLINE || defined(ERTS_ALC_INTERNAL__) */

#define ERTS_ALC_GET_THR_IX() ((int) erts_get_scheduler_id())

typedef void (*erts_alloc_verify_func_t)(Allctr_t *);

erts_alloc_verify_func_t
erts_alloc_get_verify_unused_temp_alloc(Allctr_t **allctr);

#define ERTS_ALC_DATA_ALIGN_SIZE(SZ) \
  (((((SZ) - 1) / 8) + 1) * 8)

#define ERTS_ALC_CACHE_LINE_ALIGN_SIZE(SZ) \
  (((((SZ) - 1) / ERTS_CACHE_LINE_SIZE) + 1) * ERTS_CACHE_LINE_SIZE)

#define ERTS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT,				\
		      (void) 0, (void) 0, (void) 0)

#define ERTS_SMP_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
static erts_smp_spinlock_t NAME##_lck;					\
ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT,				\
		      erts_smp_spinlock_init(&NAME##_lck, #NAME "_alloc_lock"),\
		      erts_smp_spin_lock(&NAME##_lck),			\
		      erts_smp_spin_unlock(&NAME##_lck))

#ifdef ERTS_SMP

#define ERTS_TS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
ERTS_SMP_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)

#else /* !ERTS_SMP */

#define ERTS_TS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
static erts_mtx_t NAME##_lck;						\
ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT,				\
		      erts_mtx_init(NAME##_lck, #NAME "_alloc_lock"),	\
		      erts_mtx_lock(&NAME##_lck),			\
		      erts_mtx_unlock(&NAME##_lck))


#endif

#define ERTS_PALLOC_IMPL(NAME, TYPE, PASZ)				\
ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ, (void) 0, (void) 0, (void) 0)

#define ERTS_TS_PALLOC_IMPL(NAME, TYPE, PASZ)				\
static erts_spinlock_t NAME##_lck;					\
ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ,					\
		    erts_spinlock_init(&NAME##_lck, #NAME "_alloc_lock"),\
		    erts_spin_lock(&NAME##_lck),			\
		    erts_spin_unlock(&NAME##_lck))

#ifdef ERTS_SMP

#define ERTS_SMP_PALLOC_IMPL(NAME, TYPE, PASZ)				\
  ERTS_TS_PALLOC_IMPL(NAME, TYPE, PASZ)

#else /* !ERTS_SMP */

#define ERTS_SMP_PALLOC_IMPL(NAME, TYPE, PASZ)				\
  ERTS_PALLOC_IMPL(NAME, TYPE, PASZ)

#endif

#define ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT, ILCK, LCK, ULCK)	\
ERTS_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ, ILCK, LCK, ULCK)		\
static void								\
init_##NAME##_alloc(void)						\
{									\
    init_##NAME##_pre_alloc();						\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res = NAME##_pre_alloc();					\
    if (!res)								\
	res = erts_alloc(ALCT, sizeof(TYPE));				\
    return res;								\
}									\
static ERTS_INLINE void							\
NAME##_free(TYPE *p)							\
{									\
    if (!NAME##_pre_free(p))						\
	erts_free(ALCT, (void *) p);					\
}

#ifdef ERTS_SMP
#define ERTS_SCHED_PREF_PALLOC_IMPL(NAME, TYPE, PASZ)			\
  ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME, TYPE, PASZ)
#else
#define ERTS_SCHED_PREF_PALLOC_IMPL(NAME, TYPE, PASZ)			\
  ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ, (void) 0, (void) 0, (void) 0)
#endif

#ifdef ERTS_SMP
#define ERTS_SCHED_PREF_AUX(NAME, TYPE, PASZ)				\
ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ)
#else
#define ERTS_SCHED_PREF_AUX(NAME, TYPE, PASZ)				\
ERTS_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ, (void) 0, (void) 0, (void) 0)
#endif

#define ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT)	\
ERTS_SCHED_PREF_AUX(NAME, TYPE, PASZ)					\
static void								\
init_##NAME##_alloc(void)						\
{									\
    init_##NAME##_pre_alloc();						\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res = NAME##_pre_alloc();					\
    if (!res)								\
	res = erts_alloc(ALCT, sizeof(TYPE));				\
    return res;								\
}									\
static ERTS_INLINE void							\
NAME##_free(TYPE *p)							\
{									\
    if (!NAME##_pre_free(p))						\
	erts_free(ALCT, (void *) p);					\
}

#ifdef DEBUG
#define ERTS_PRE_ALLOC_SIZE(SZ) 2
#define ERTS_PRE_ALLOC_CLOBBER(P, T) memset((void *) (P), 0xfd, sizeof(T))
#else
#define ERTS_PRE_ALLOC_SIZE(SZ) ((SZ) > 1 ? (SZ) : 1)
#define ERTS_PRE_ALLOC_CLOBBER(P, T)
#endif

#define ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ, ILCK, LCK, ULCK)		\
union erts_qa_##NAME##__ {						\
    TYPE type;								\
    union erts_qa_##NAME##__ *next;					\
};									\
static union erts_qa_##NAME##__						\
    qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))];			\
static union erts_qa_##NAME##__ *qa_freelist_##NAME;			\
static void								\
init_##NAME##_alloc(void)						\
{									\
    int i;								\
    qa_freelist_##NAME = &qa_prealcd_##NAME[0];				\
    for (i = 1; i < ERTS_PRE_ALLOC_SIZE((PASZ)); i++) {			\
	ERTS_PRE_ALLOC_CLOBBER(&qa_prealcd_##NAME[i-1],			\
			       union erts_qa_##NAME##__);		\
	qa_prealcd_##NAME[i-1].next = &qa_prealcd_##NAME[i];		\
    }									\
    ERTS_PRE_ALLOC_CLOBBER(&qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))-1],\
			   union erts_qa_##NAME##__);			\
    qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))-1].next = NULL;	\
    ILCK;								\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res;								\
    LCK;								\
    if (!qa_freelist_##NAME)						\
	res = NULL;							\
    else {								\
	res = &qa_freelist_##NAME->type;				\
	qa_freelist_##NAME = qa_freelist_##NAME->next;			\
    }									\
    ULCK;								\
    return res;								\
}									\
static ERTS_INLINE int							\
NAME##_free(TYPE *p)							\
{									\
    union erts_qa_##NAME##__ * up;					\
    up = ((union erts_qa_##NAME##__ *)					\
	  (((char *) p)							\
	   - ((char *) &((union erts_qa_##NAME##__ *) 0)->type)));	\
    if (up > &qa_prealcd_##NAME[ERTS_PRE_ALLOC_SIZE((PASZ))-1]		\
	|| up < &qa_prealcd_##NAME[0])					\
	return 0;							\
    else {								\
	LCK;								\
	ERTS_PRE_ALLOC_CLOBBER(up, union erts_qa_##NAME##__);		\
	up->next = qa_freelist_##NAME;					\
	qa_freelist_##NAME = up;					\
	ULCK;								\
	return 1;							\
    }									\
}

#include "erl_sched_spec_pre_alloc.h"

#define ERTS_SCHED_PREF_PRE_ALLOC_IMPL(NAME, TYPE, PASZ)		\
union erts_sspa_##NAME##__ {						\
    erts_sspa_blk_t next;						\
    TYPE type;								\
};									\
									\
static erts_sspa_data_t *sspa_data_##NAME##__;				\
									\
static void								\
init_##NAME##_alloc(void)						\
{									\
    sspa_data_##NAME##__ =						\
	erts_sspa_create(sizeof(union erts_sspa_##NAME##__),		\
			 ERTS_PRE_ALLOC_SIZE((PASZ)));			\
}									\
									\
static TYPE *								\
NAME##_alloc(void)							\
{									\
    ErtsSchedulerData *esdp = erts_get_scheduler_data();		\
    if (!esdp || ERTS_SCHEDULER_IS_DIRTY(esdp))				\
	return NULL;							\
    return (TYPE *) erts_sspa_alloc(sspa_data_##NAME##__,		\
				    (int) esdp->no - 1);		\
}									\
									\
static int								\
NAME##_free(TYPE *p)							\
{									\
    ErtsSchedulerData *esdp = erts_get_scheduler_data();		\
    return erts_sspa_free(sspa_data_##NAME##__,				\
			  esdp ? (int) esdp->no - 1 : -1,		\
			  (char *) p);					\
}

#ifdef DEBUG
#define ERTS_ALC_DBG_BLK_SZ(PTR) (*(((UWord *) (PTR)) - 2))
#endif /* #ifdef DEBUG */

#undef ERTS_ALC_INLINE
#undef ERTS_ALC_ATTRIBUTES

#endif /* #ifndef ERL_ALLOC_H__ */
