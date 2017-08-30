/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
 * Description:	Process/Port table implementation.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#define ERTS_PTAB_WANT_BIF_IMPL__
#define ERTS_PTAB_WANT_DEBUG_FUNCS__
#include "erl_ptab.h"
#include "global.h"
#include "erl_binary.h"

typedef struct ErtsPTabListBifData_ ErtsPTabListBifData;

#define ERTS_PTAB_NEW_MAX_RESERVE_FAIL 1000

#define ERTS_PTAB_LIST_BIF_TAB_INSPECT_INDICES_PER_RED 25
#define ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE 1000
#define ERTS_PTAB_LIST_BIF_MIN_START_REDS				\
 (ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE					\
  / ERTS_PTAB_LIST_BIF_TAB_INSPECT_INDICES_PER_RED)

#define ERTS_PTAB_LIST_BIF_TAB_FREE_DELETED_REDS 1

#define ERTS_PTAB_LIST_BIF_INSPECT_DELETED_PER_RED 10

#define ERTS_PTAB_LIST_INSPECT_DELETED_MAX_REDS				\
 (ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE					\
  / ERTS_PTAB_LIST_BIF_TAB_INSPECT_INDICES_PER_RED)
 

#define ERTS_PTAB_LIST_BIF_BUILD_RESULT_CONSES_PER_RED 75

#define ERTS_PTAB_LIST_DBG_DO_TRACE 0

#ifdef DEBUG
#  define ERTS_PTAB_LIST_BIF_DEBUGLEVEL 100
#else
#  define ERTS_PTAB_LIST_BIF_DEBUGLEVEL 0
#endif

#define ERTS_PTAB_LIST_DBGLVL_CHK_HALLOC 1
#define ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS 5
#define ERTS_PTAB_LIST_DBGLVL_CHK_PIDS 10
#define ERTS_PTAB_LIST_DBGLVL_CHK_DEL_LIST 20
#define ERTS_PTAB_LIST_DBGLVL_CHK_RESLIST 20

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL == 0
#  define ERTS_PTAB_LIST_ASSERT(EXP)
#else
#  define ERTS_PTAB_LIST_ASSERT(EXP)					\
    ((void) ((EXP)							\
	     ? 1							\
	     : (debug_ptab_list_assert_error(#EXP,			\
					     __FILE__,			\
					     __LINE__,			\
					     __func__),			\
		0)))
#endif


#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >=  ERTS_PTAB_LIST_DBGLVL_CHK_HALLOC
#  define ERTS_PTAB_LIST_DBG_SAVE_HEAP_ALLOC(PTLBDP, HP, SZ)		\
do {									\
    ERTS_PTAB_LIST_ASSERT(!(PTLBDP)->debug.heap);			\
    ERTS_PTAB_LIST_ASSERT(!(PTLBDP)->debug.heap_size);			\
    (PTLBDP)->debug.heap = (HP);					\
    (PTLBDP)->debug.heap_size = (SZ);					\
} while (0)
#  define ERTS_PTAB_LIST_DBG_VERIFY_HEAP_ALLOC_USED(PTLBDP, HP)		\
do {									\
    ERTS_PTAB_LIST_ASSERT((PTLBDP)->debug.heap);			\
    ERTS_PTAB_LIST_ASSERT((PTLBDP)->debug.heap_size);			\
    ERTS_PTAB_LIST_ASSERT(((PTLBDP)->debug.heap				\
			   + (PTLBDP)->debug.heap_size)			\
			  == (HP));					\
    (PTLBDP)->debug.heap = NULL;					\
    (PTLBDP)->debug.heap_size = 0;					\
} while (0)
#  define ERTS_PTAB_LIST_DBG_HEAP_ALLOC_INIT(PTLBDP)			\
do {									\
    (PTLBDP)->debug.heap = NULL;					\
    (PTLBDP)->debug.heap_size = 0;					\
} while (0)
#else
#  define ERTS_PTAB_LIST_DBG_SAVE_HEAP_ALLOC(PTLBDP, HP, SZ)
#  define ERTS_PTAB_LIST_DBG_VERIFY_HEAP_ALLOC_USED(PTLBDP, HP)
#  define ERTS_PTAB_LIST_DBG_HEAP_ALLOC_INIT(PTLBDP)
#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_RESLIST
#  define ERTS_PTAB_LIST_DBG_CHK_RESLIST(R)				\
    debug_ptab_list_check_res_list((R))
#else
#  define ERTS_PTAB_LIST_DBG_CHK_RESLIST(R)
#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_PIDS
#  define ERTS_PTAB_LIST_DBG_SAVE_PIDS(PTLBDP)				\
    debug_ptab_list_save_all_pids((PTLBDP))
#  define ERTS_PTAB_LIST_DBG_VERIFY_PIDS(PTLBDP)			\
do {									\
    if (!(PTLBDP)->debug.correct_pids_verified)				\
	debug_ptab_list_verify_all_pids((PTLBDP));			\
} while (0)
#  define ERTS_PTAB_LIST_DBG_CLEANUP_CHK_PIDS(PTLBDP)			\
do {									\
    if ((PTLBDP)->debug.correct_pids) {					\
	erts_free(ERTS_ALC_T_PTAB_LIST_PIDS,				\
		  (PTLBDP)->debug.correct_pids);			\
	(PTLBDP)->debug.correct_pids = NULL;				\
    }									\
} while(0)
#  define ERTS_PTAB_LIST_DBG_CHK_PIDS_INIT(PTLBDP)			\
do {									\
    (PTLBDP)->debug.correct_pids_verified = 0;				\
    (PTLBDP)->debug.correct_pids = NULL;				\
} while (0)
#else
#  define ERTS_PTAB_LIST_DBG_SAVE_PIDS(PTLBDP)
#  define ERTS_PTAB_LIST_DBG_VERIFY_PIDS(PTLBDP)
#  define ERTS_PTAB_LIST_DBG_CLEANUP_CHK_PIDS(PTLBDP)
#  define ERTS_PTAB_LIST_DBG_CHK_PIDS_INIT(PTLBDP)
#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
#  define ERTS_PTAB_LIST_DBG_CHK_PID_FOUND(PTLBDP, PID, IC)		\
  debug_ptab_list_check_found_pid((PTLBDP), (PID), (IC), 1)
#  define ERTS_PTAB_LIST_DBG_CHK_PID_NOT_FOUND(PTLBDP, PID, IC)		\
  debug_ptab_list_check_found_pid((PTLBDP), (PID), (IC), 0)
#else
#  define ERTS_PTAB_LIST_DBG_CHK_PID_FOUND(PTLBDP, PID, IC)
#  define ERTS_PTAB_LIST_DBG_CHK_PID_NOT_FOUND(PTLBDP, PID, IC)
#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_DEL_LIST
#  define ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(PTab)				\
    debug_ptab_list_check_del_list((PTab))
#  define ERTS_PTAB_LIST_DBG_CHK_FREELIST(PTab, FL)			\
    debug_ptab_list_check_del_free_list((PTab), (FL))
#else
#  define ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(PTab)
#  define ERTS_PTAB_LIST_DBG_CHK_FREELIST(PTab, FL)
#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL == 0
#if ERTS_PTAB_LIST_DBG_DO_TRACE
#    define ERTS_PTAB_LIST_DBG_INIT(P, PTLBDP)				\
    (PTLBDP)->debug.caller = (P)->common.id
#  else
#    define ERTS_PTAB_LIST_DBG_INIT(P, PTLBDP)
#  endif
#  define ERTS_PTAB_LIST_DBG_CLEANUP(PTLBDP)
#else
#  define ERTS_PTAB_LIST_DBG_INIT(P, PTLBDP)				\
do {									\
    (PTLBDP)->debug.caller = (P)->common.id;				\
    ERTS_PTAB_LIST_DBG_HEAP_ALLOC_INIT((PTLBDP));			\
    ERTS_PTAB_LIST_DBG_CHK_PIDS_INIT((PTLBDP));				\
} while (0)
#  define ERTS_PTAB_LIST_DBG_CLEANUP(PTLBDP)				\
do {									\
    ERTS_PTAB_LIST_DBG_CLEANUP_CHK_PIDS((PTLBDP));			\
} while (0)
#endif

#if ERTS_PTAB_LIST_DBG_DO_TRACE
#  define ERTS_PTAB_LIST_DBG_TRACE(PID, WHAT)				\
     erts_fprintf(stderr, "%T %s:%d:%s(): %s\n",			\
		  (PID), __FILE__, __LINE__, __func__, #WHAT)
#else
#  define ERTS_PTAB_LIST_DBG_TRACE(PID, WHAT)
#endif


#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL != 0
static void debug_ptab_list_assert_error(char* expr,
					 const char* file,
					 int line,
					 const char *func);
#endif
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_RESLIST
static void debug_ptab_list_check_res_list(Eterm list);
#endif
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_PIDS
static void debug_ptab_list_save_all_pids(ErtsPTabListBifData *ptlbdp);
static void debug_ptab_list_verify_all_pids(ErtsPTabListBifData *ptlbdp);
#endif
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
static void debug_ptab_list_check_found_pid(ErtsPTabListBifData *ptlbdp,
					    Eterm pid,
					    Uint64 ic,
					    int pid_should_be_found);
#endif
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_DEL_LIST
static void debug_ptab_list_check_del_list(ErtsPTab *ptab);
static void debug_ptab_list_check_del_free_list(ErtsPTab *ptab,
						ErtsPTabDeletedElement *ptdep);
#endif

struct ErtsPTabDeletedElement_ {
    ErtsPTabDeletedElement *next;
    ErtsPTabDeletedElement *prev;
    int ix;
    union {
	struct {
	    Eterm id;
	    Uint64 inserted;
	    Uint64 deleted;
	} element;
	struct {
	    Uint64 interval;
	} bif_invocation;
    } u;
};

static Export ptab_list_continue_export;

typedef struct {
    Uint64 interval;
} ErtsPTabListBifChunkInfo;

typedef enum {
    INITIALIZING,
    INSPECTING_TABLE,
    INSPECTING_DELETED,
    BUILDING_RESULT,
    RETURN_RESULT
} ErtsPTabListBifState;

struct ErtsPTabListBifData_ {
    ErtsPTab *ptab;
    ErtsPTabListBifState state;
    Eterm caller;
    ErtsPTabListBifChunkInfo *chunk;
    int tix;
    int pid_ix;
    int pid_sz;
    Eterm *pid;
    ErtsPTabDeletedElement *bif_invocation; /* Only used when > 1 chunk */

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL != 0 || ERTS_PTAB_LIST_DBG_DO_TRACE
    struct {
	Eterm caller;
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
	Uint64 *pid_started;
#endif
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_HALLOC
	Eterm *heap;
	Uint heap_size;
#endif
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_PIDS
	int correct_pids_verified;
	Eterm *correct_pids;
#endif
    } debug;
#endif

};

static ERTS_INLINE void
last_data_init_nob(ErtsPTab *ptab, Uint64 val)
{
    erts_smp_atomic64_init_nob(&ptab->vola.tile.last_data, (erts_aint64_t) val);
}

static ERTS_INLINE void
last_data_set_relb(ErtsPTab *ptab, Uint64 val)
{
    erts_smp_atomic64_set_relb(&ptab->vola.tile.last_data, (erts_aint64_t) val);
}

static ERTS_INLINE Uint64
last_data_read_nob(ErtsPTab *ptab)
{
    return (Uint64) erts_smp_atomic64_read_nob(&ptab->vola.tile.last_data);
}

static ERTS_INLINE Uint64
last_data_read_acqb(ErtsPTab *ptab)
{
    return (Uint64) erts_smp_atomic64_read_acqb(&ptab->vola.tile.last_data);
}

static ERTS_INLINE Uint64
last_data_cmpxchg_relb(ErtsPTab *ptab, Uint64 new, Uint64 exp)
{
    return (Uint64) erts_smp_atomic64_cmpxchg_relb(&ptab->vola.tile.last_data,
						   (erts_aint64_t) new,
						   (erts_aint64_t) exp);
}

static ERTS_INLINE int
last_data_cmp(Uint64 ld1, Uint64 ld2)
{
    Uint64 ld1_wrap;

    if (ld1 == ld2)
	return 0;

    ld1_wrap = ld1 + (((Uint64) 1) << 63);

    if (ld1 < ld1_wrap)
	return (ld1 < ld2 && ld2 < ld1_wrap) ? -1 : 1;
    else
	return (ld1_wrap <= ld2 && ld2 < ld1) ? 1 : -1;
}

#define ERTS_PTAB_LastData2EtermData(LD) \
    ((Eterm) ((LD) & ~(~((Uint64) 0) << ERTS_PTAB_ID_DATA_SIZE)))

static ERTS_INLINE Uint32
ix_to_free_id_data_ix(ErtsPTab *ptab, Uint32 ix)
{
    Uint32 dix;

    dix = ((ix & ptab->r.o.dix_cl_mask) << ptab->r.o.dix_cl_shift);
    dix += ((ix >> ptab->r.o.dix_cli_shift) & ptab->r.o.dix_cli_mask);
    ASSERT(0 <= dix && dix < ptab->r.o.max);
    return dix;
}

UWord
erts_ptab_mem_size(ErtsPTab *ptab)
{
    UWord size = ptab->r.o.max*sizeof(erts_smp_atomic_t);
    if (ptab->r.o.free_id_data)
	size += ptab->r.o.max*sizeof(erts_smp_atomic32_t);
    return size;
}


void
erts_ptab_init_table(ErtsPTab *ptab,
		     ErtsAlcType_t atype,
		     void (*release_element)(void *),
		     ErtsPTabElementCommon *invalid_element,
		     int size,
		     UWord element_size,
		     char *name,
		     int legacy,
		     int atomic_refc)
{
    size_t tab_sz, alloc_sz;
    Uint32 bits, cl, cli, ix, ix_per_cache_line, tab_cache_lines; 
    char *tab_end;
    erts_smp_atomic_t *tab_entry;
    erts_smp_rwmtx_opt_t rwmtx_opts = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opts.type = ERTS_SMP_RWMTX_TYPE_EXTREMELY_FREQUENT_READ;
    rwmtx_opts.lived = ERTS_SMP_RWMTX_LONG_LIVED;

    erts_smp_rwmtx_init_opt(&ptab->list.data.rwmtx, &rwmtx_opts, name);
    erts_smp_atomic32_init_nob(&ptab->vola.tile.count, 0);
    last_data_init_nob(ptab, ~((Uint64) 0));

    /* A size that is a power of 2 is to prefer performance wise */
    bits = erts_fit_in_bits_int32(size-1);
    size = 1 << bits;
    if (size > ERTS_PTAB_MAX_SIZE) {
	size = ERTS_PTAB_MAX_SIZE;
	bits = erts_fit_in_bits_int32((Sint32) size - 1);
    }

    ptab->r.o.element_size = element_size;
    ptab->r.o.max = size;

    tab_sz = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(size*sizeof(erts_smp_atomic_t));
    alloc_sz = tab_sz;
    if (!legacy)
	alloc_sz += ERTS_ALC_CACHE_LINE_ALIGN_SIZE(size*sizeof(erts_smp_atomic32_t));
    ptab->r.o.tab = erts_alloc_permanent_cache_aligned(atype, alloc_sz);
    tab_end = ((char *) ptab->r.o.tab) + tab_sz;
    tab_entry = ptab->r.o.tab;
    while (tab_end > ((char *) tab_entry)) {
	erts_smp_atomic_init_nob(tab_entry, ERTS_AINT_NULL);
	tab_entry++;
    }

    tab_cache_lines = tab_sz/ERTS_CACHE_LINE_SIZE;
    ix_per_cache_line = (ERTS_CACHE_LINE_SIZE/sizeof(erts_smp_atomic_t));
    ASSERT((ptab->r.o.max & (ptab->r.o.max - 1)) == 0); /* power of 2 */
    ASSERT((ix_per_cache_line & (ix_per_cache_line - 1)) == 0); /* power of 2 */
    ASSERT((tab_cache_lines & (tab_cache_lines - 1)) == 0); /* power of 2 */

    ptab->r.o.pix_mask = (1 << bits) - 1;
    ptab->r.o.pix_cl_mask = tab_cache_lines-1;
    ptab->r.o.pix_cl_shift = erts_fit_in_bits_int32(ix_per_cache_line-1);
    ptab->r.o.pix_cli_shift = erts_fit_in_bits_int32(ptab->r.o.pix_cl_mask);
    ptab->r.o.pix_cli_mask = (1 << (bits - ptab->r.o.pix_cli_shift)) - 1;

    ASSERT(ptab->r.o.pix_cl_shift + ptab->r.o.pix_cli_shift == bits);

    ptab->r.o.invalid_element = invalid_element;
    ptab->r.o.invalid_data = erts_ptab_id2data(ptab, invalid_element->id);
    ptab->r.o.release_element = release_element;

    ptab->r.o.atomic_refc = atomic_refc;

    if (legacy) {
	ptab->r.o.free_id_data = NULL;
	ptab->r.o.dix_cl_mask = 0;
	ptab->r.o.dix_cl_shift = 0;
	ptab->r.o.dix_cli_shift = 0;
	ptab->r.o.dix_cli_mask = 0;
    }
    else {

	tab_sz = ERTS_ALC_CACHE_LINE_ALIGN_SIZE(size*sizeof(erts_smp_atomic32_t));
	ptab->r.o.free_id_data = (erts_smp_atomic32_t *) tab_end;

	tab_cache_lines = tab_sz/ERTS_CACHE_LINE_SIZE;
	ix_per_cache_line = (ERTS_CACHE_LINE_SIZE/sizeof(erts_smp_atomic32_t));

	ptab->r.o.dix_cl_mask = tab_cache_lines-1;
	ptab->r.o.dix_cl_shift = erts_fit_in_bits_int32(ix_per_cache_line-1);
	ptab->r.o.dix_cli_shift = erts_fit_in_bits_int32(ptab->r.o.dix_cl_mask);
	ptab->r.o.dix_cli_mask = (1 << (bits - ptab->r.o.dix_cli_shift)) - 1;

	ASSERT((ix_per_cache_line & (ix_per_cache_line - 1)) == 0); /* power of 2 */
	ASSERT((tab_cache_lines & (tab_cache_lines - 1)) == 0); /* power of 2 */

	ASSERT(ptab->r.o.dix_cl_shift + ptab->r.o.dix_cli_shift == bits);

	ix = 0;
	for (cl = 0; cl < tab_cache_lines; cl++) {
	    for (cli = 0; cli < ix_per_cache_line; cli++) {
		erts_smp_atomic32_init_nob(&ptab->r.o.free_id_data[ix],
					   cli*tab_cache_lines+cl);
		ASSERT(erts_smp_atomic32_read_nob(&ptab->r.o.free_id_data[ix]) != ptab->r.o.invalid_data);
		ix++;
	    }
	}

	erts_smp_atomic32_init_nob(&ptab->vola.tile.aid_ix, -1);
	erts_smp_atomic32_init_nob(&ptab->vola.tile.fid_ix, -1);

    }

    erts_smp_interval_init(&ptab->list.data.interval);
    ptab->list.data.deleted.start = NULL;
    ptab->list.data.deleted.end = NULL;
    ptab->list.data.chunks = (((ptab->r.o.max - 1)
			       / ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE)
			      + 1);

    if (size == ERTS_PTAB_MAX_SIZE) {
	int pix;
	/*
	 * We want a table size of a power of 2 which ERTS_PTAB_MAX_SIZE
	 * is. We only have ERTS_PTAB_MAX_SIZE-1 unique identifiers and
	 * we don't want to shrink the size to ERTS_PTAB_MAX_SIZE/2.
	 *
	 * In order to fix this, we insert a pointer from the table
	 * to the invalid_element, wich will be interpreted as a
	 * slot currently being modified. This way we will be able to
	 * have ERTS_PTAB_MAX_SIZE-1 valid elements in the table while
	 * still having a table size of the power of 2.
	 */
	erts_smp_atomic32_inc_nob(&ptab->vola.tile.count);
	pix = erts_ptab_data2pix(ptab, ptab->r.o.invalid_data);
	erts_smp_atomic_set_relb(&ptab->r.o.tab[pix],
				 (erts_aint_t) ptab->r.o.invalid_element);
    }

}

int
erts_ptab_initialized(ErtsPTab *ptab)
{
    return ptab->r.o.tab != NULL;
}

int
erts_ptab_new_element(ErtsPTab *ptab,
		      ErtsPTabElementCommon *ptab_el,
		      void *init_arg,
		      void (*init_ptab_el)(void *, Eterm))
{
    Uint32 pix, ix, data;
    erts_aint32_t count;
    erts_aint_t invalid = (erts_aint_t) ptab->r.o.invalid_element;

    erts_ptab_rlock(ptab);

    count = erts_smp_atomic32_inc_read_acqb(&ptab->vola.tile.count);
    if (count > ptab->r.o.max) {
	while (1) {
	    erts_aint32_t act_count;

	    act_count = erts_smp_atomic32_cmpxchg_relb(&ptab->vola.tile.count,
						       count-1,
						       count);
	    if (act_count == count) {
		erts_ptab_runlock(ptab);
		return 0;
	    }
	    count = act_count;
	    if (count <= ptab->r.o.max)
		break;
	}
    }

    ptab_el->u.alive.started_interval
	= erts_smp_current_interval_nob(erts_ptab_interval(ptab));

    if (ptab->r.o.free_id_data) {
	do {
	    ix = (Uint32) erts_smp_atomic32_inc_read_acqb(&ptab->vola.tile.aid_ix);
	    ix = ix_to_free_id_data_ix(ptab, ix);

	    data = erts_smp_atomic32_xchg_nob(&ptab->r.o.free_id_data[ix],
					      (erts_aint32_t)ptab->r.o.invalid_data);
	}while ((Eterm)data == ptab->r.o.invalid_data);

	init_ptab_el(init_arg, (Eterm) data);

	if (ptab->r.o.atomic_refc)
	    erts_atomic_init_nob(&ptab_el->refc.atmc, 1);
	else
	    ptab_el->refc.sint = 1;

	pix = erts_ptab_data2pix(ptab, (Eterm) data);

#ifdef DEBUG
	ASSERT(ERTS_AINT_NULL == erts_smp_atomic_xchg_relb(&ptab->r.o.tab[pix],
							   (erts_aint_t) ptab_el));
#else
	erts_smp_atomic_set_relb(&ptab->r.o.tab[pix], (erts_aint_t) ptab_el);
#endif

	erts_ptab_runlock(ptab);

    }
    else {
	int rlocked = ERTS_PTAB_NEW_MAX_RESERVE_FAIL;
	Uint64 ld, exp_ld;
	/* Deprecated legacy algorithm... */

    restart:

	ptab_el->u.alive.started_interval
	    = erts_smp_current_interval_nob(erts_ptab_interval(ptab));

	ld = last_data_read_acqb(ptab);

	/* Reserve slot */
	while (1) {
	    ld++;
	    pix = erts_ptab_data2pix(ptab, ERTS_PTAB_LastData2EtermData(ld));
	    if (erts_smp_atomic_read_nob(&ptab->r.o.tab[pix])
		== ERTS_AINT_NULL) {
		erts_aint_t val;
		val = erts_smp_atomic_cmpxchg_relb(&ptab->r.o.tab[pix],
						   invalid,	
						   ERTS_AINT_NULL);

		if (ERTS_AINT_NULL == val)
		    break;
	    }
	    if (rlocked && --rlocked == 0) {
		erts_ptab_runlock(ptab);
		erts_ptab_rwlock(ptab);
		goto restart;
	    }
	}

	data = ERTS_PTAB_LastData2EtermData(ld);

	if (data == ptab->r.o.invalid_data) {
	    /* Do not use invalid data; fix it... */
	    ld += ptab->r.o.max;
	    ASSERT(pix == erts_ptab_data2pix(ptab,
					     ERTS_PTAB_LastData2EtermData(ld)));
	    data = ERTS_PTAB_LastData2EtermData(ld);
	    ASSERT(data != ptab->r.o.invalid_data);
	}

	exp_ld = last_data_read_nob(ptab);

	/* Move last data forward */
	while (1) {
	    Uint64 act_ld;
	    if (last_data_cmp(ld, exp_ld) < 0)
		break;
	    act_ld = last_data_cmpxchg_relb(ptab, ld, exp_ld);
	    if (act_ld == exp_ld)
		break;
	    exp_ld = act_ld;
	}

	init_ptab_el(init_arg, data);

	if (ptab->r.o.atomic_refc)
	    erts_atomic_init_nob(&ptab_el->refc.atmc, 1);
	else
	    ptab_el->refc.sint = 1;

	/* Move into slot reserved */
#ifdef DEBUG
	ASSERT(invalid == erts_smp_atomic_xchg_relb(&ptab->r.o.tab[pix],
						(erts_aint_t) ptab_el));
#else
	erts_smp_atomic_set_relb(&ptab->r.o.tab[pix], (erts_aint_t) ptab_el);
#endif

	if (rlocked)
	    erts_ptab_runlock(ptab);
	else
	    erts_ptab_rwunlock(ptab);

    }

    return 1;
}

static void
save_deleted_element(ErtsPTab *ptab, ErtsPTabElementCommon *ptab_el)
{
    ErtsPTabDeletedElement *ptdep = erts_alloc(ERTS_ALC_T_PTAB_LIST_DEL,
					       sizeof(ErtsPTabDeletedElement));
    ERTS_PTAB_LIST_ASSERT(ptab->list.data.deleted.start
			  && ptab->list.data.deleted.end);
    ERTS_SMP_LC_ASSERT(erts_smp_lc_ptab_is_rwlocked(ptab));

    ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);

    ptdep->prev = ptab->list.data.deleted.end;
    ptdep->next = NULL;
    ptdep->ix = erts_ptab_id2pix(ptab, ptab_el->id);
    ptdep->u.element.id = ptab_el->id;
    ptdep->u.element.inserted = ptab_el->u.alive.started_interval;
    ptdep->u.element.deleted =
	erts_smp_current_interval_nob(erts_ptab_interval(ptab));

    ptab->list.data.deleted.end->next = ptdep;
    ptab->list.data.deleted.end = ptdep;

    ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);

    ERTS_PTAB_LIST_ASSERT(ptdep->prev->ix >= 0
			  ? (ptdep->u.element.deleted
			     >= ptdep->prev->u.element.deleted)
			  : (ptdep->u.element.deleted
			     >= ptdep->prev->u.bif_invocation.interval));
}

void
erts_ptab_delete_element(ErtsPTab *ptab,
			 ErtsPTabElementCommon *ptab_el)
{
    int maybe_save;
    Uint32 pix, ix, data;

    pix = erts_ptab_id2pix(ptab, ptab_el->id);

    /* *Need* to be an managed thread */
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_managed_thread());

    erts_ptab_rlock(ptab);
    maybe_save = ptab->list.data.deleted.end != NULL;
    if (maybe_save) {
	erts_ptab_runlock(ptab);
	erts_ptab_rwlock(ptab);
    }

    erts_smp_atomic_set_relb(&ptab->r.o.tab[pix], ERTS_AINT_NULL);

    if (ptab->r.o.free_id_data) {
	Uint32 prev_data;
	/* Next data for this slot... */
	data = (Uint32) erts_ptab_id2data(ptab, ptab_el->id);
	data += ptab->r.o.max;
	data &= ~(~((Uint32) 0) << ERTS_PTAB_ID_DATA_SIZE);
	if (data == ptab->r.o.invalid_data) { /* make sure not invalid */
	    data += ptab->r.o.max;
	    data &= ~(~((Uint32) 0) << ERTS_PTAB_ID_DATA_SIZE);
	}
	ASSERT(data != ptab->r.o.invalid_data);
	ASSERT(pix == erts_ptab_data2pix(ptab, data));

	do { 
	    ix = (Uint32) erts_smp_atomic32_inc_read_relb(&ptab->vola.tile.fid_ix);
	    ix = ix_to_free_id_data_ix(ptab, ix);
    
	    prev_data = erts_smp_atomic32_cmpxchg_nob(&ptab->r.o.free_id_data[ix],
						      data,
						      ptab->r.o.invalid_data);
	}while ((Eterm)prev_data != ptab->r.o.invalid_data);
    }

    ASSERT(erts_smp_atomic32_read_nob(&ptab->vola.tile.count) > 0);
    erts_smp_atomic32_dec_relb(&ptab->vola.tile.count);

    if (!maybe_save)
	erts_ptab_runlock(ptab);
    else {
	if (ptab->list.data.deleted.end)
	    save_deleted_element(ptab, ptab_el);
	erts_ptab_rwunlock(ptab);
    }

    if (ptab->r.o.release_element)
	erts_schedule_thr_prgr_later_cleanup_op(ptab->r.o.release_element,
						(void *) ptab_el,
						&ptab_el->u.release,
						ptab->r.o.element_size);
}

/*
 * erts_ptab_list() implements BIFs listing the content of the table,
 * e.g. erlang:processes/0.
 */
static void cleanup_ptab_list_bif_data(Binary *bp);
static int ptab_list_bif_engine(Process *c_p, Eterm *res_accp, Binary *mbp);


BIF_RETTYPE
erts_ptab_list(Process *c_p, ErtsPTab *ptab)
{
    /*
     * A requirement: The list of identifiers returned should be a
     *                consistent snapshot of all elements existing
     *                in the table at some point in time during the
     *                execution of the BIF calling this function.
     *                Since elements might be deleted while the BIF
     *                is executing, we have to keep track of all
     *                deleted elements and add them to the result.
     *                We also ignore elements created after the BIF
     *                has begun executing.
     */
    BIF_RETTYPE ret_val;
    Eterm res_acc = NIL;
    Binary *mbp = erts_create_magic_binary(sizeof(ErtsPTabListBifData),
					   cleanup_ptab_list_bif_data);
    ErtsPTabListBifData *ptlbdp = ERTS_MAGIC_BIN_DATA(mbp);

    ERTS_PTAB_LIST_DBG_TRACE(c_p->common.id, call);
    ptlbdp->ptab = ptab;
    ptlbdp->state = INITIALIZING;
    ERTS_PTAB_LIST_DBG_INIT(c_p, ptlbdp);

    if (ERTS_BIF_REDS_LEFT(c_p) >= ERTS_PTAB_LIST_BIF_MIN_START_REDS
	&& ptab_list_bif_engine(c_p, &res_acc, mbp)) {
	erts_bin_free(mbp);
	ERTS_PTAB_LIST_DBG_CHK_RESLIST(res_acc);
	ERTS_PTAB_LIST_DBG_TRACE(c_p->common.id, return);
	ERTS_BIF_PREP_RET(ret_val, res_acc);
    }
    else {
	Eterm *hp;
	Eterm magic_bin;
	ERTS_PTAB_LIST_DBG_CHK_RESLIST(res_acc);
	hp = HAlloc(c_p, PROC_BIN_SIZE);
	ERTS_PTAB_LIST_DBG_SAVE_HEAP_ALLOC(ptlbdp, hp, PROC_BIN_SIZE);
	magic_bin = erts_mk_magic_binary_term(&hp, &MSO(c_p), mbp);
	ERTS_PTAB_LIST_DBG_VERIFY_HEAP_ALLOC_USED(ptlbdp, hp);
	ERTS_PTAB_LIST_DBG_TRACE(c_p->common.id, trap);
	ERTS_BIF_PREP_YIELD2(ret_val,
			     &ptab_list_continue_export,
			     c_p,
			     res_acc,
			     magic_bin);
    }
    return ret_val;
}

static void
cleanup_ptab_list_bif_data(Binary *bp)
{
    ErtsPTabListBifData *ptlbdp = ERTS_MAGIC_BIN_DATA(bp);
    ErtsPTab *ptab = ptlbdp->ptab;

    ERTS_PTAB_LIST_DBG_TRACE(ptlbdp->debug.caller, call);

    if (ptlbdp->state != INITIALIZING) {

	if (ptlbdp->chunk) {
	    erts_free(ERTS_ALC_T_PTAB_LIST_CNKI, ptlbdp->chunk);
	    ptlbdp->chunk = NULL;
	}
	if (ptlbdp->pid) {
	    erts_free(ERTS_ALC_T_PTAB_LIST_PIDS, ptlbdp->pid);
	    ptlbdp->pid = NULL;
	}

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
	if (ptlbdp->debug.pid_started) {
	    erts_free(ERTS_ALC_T_PTAB_LIST_PIDS, ptlbdp->debug.pid_started);
	    ptlbdp->debug.pid_started = NULL;
	}
#endif

	if (ptlbdp->bif_invocation) {
	    ErtsPTabDeletedElement *ptdep;

	    erts_ptab_rwlock(ptab);

	    ERTS_PTAB_LIST_DBG_TRACE(ptlbdp->debug.caller, deleted_cleanup);

	    ptdep = ptlbdp->bif_invocation;
	    ptlbdp->bif_invocation = NULL;

	    ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);

	    if (ptdep->prev) {
		/*
		 * Only remove this bif invokation when we
		 * have preceding invokations.
		 */
		ptdep->prev->next = ptdep->next;
		if (ptdep->next)
		    ptdep->next->prev = ptdep->prev;
		else {
		    /*
		     * At the time of writing this branch cannot be
		     * reached. I don't want to remove this code though
		     * since it may be possible to reach this line
		     * in the future if the cleanup order in
		     * erts_do_exit_process() is changed. The ASSERT(0)
		     * is only here to make us aware that the reorder
		     * has happened. /rickard
		     */
		    ASSERT(0);
		    ptab->list.data.deleted.end = ptdep->prev;
		}
		erts_free(ERTS_ALC_T_PTAB_LIST_DEL, ptdep);
	    }
	    else {
		/*
		 * Free all elements until next bif invokation
		 * is found.
		 */
		ERTS_PTAB_LIST_ASSERT(ptab->list.data.deleted.start == ptdep);
		do {
		    ErtsPTabDeletedElement *fptdep = ptdep;
		    ptdep = ptdep->next;
		    erts_free(ERTS_ALC_T_PTAB_LIST_DEL, fptdep);
		} while (ptdep && ptdep->ix >= 0);
		ptab->list.data.deleted.start = ptdep;
		if (ptdep)
		    ptdep->prev = NULL;
		else
		    ptab->list.data.deleted.end = NULL;
	    }

	    ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);

	    erts_ptab_rwunlock(ptab);

	}
    }

    ERTS_PTAB_LIST_DBG_TRACE(ptlbdp->debug.caller, return);
    ERTS_PTAB_LIST_DBG_CLEANUP(ptlbdp);
}

static int
ptab_list_bif_engine(Process *c_p, Eterm *res_accp, Binary *mbp)
{
    ErtsPTabListBifData *ptlbdp = ERTS_MAGIC_BIN_DATA(mbp);
    ErtsPTab *ptab = ptlbdp->ptab;
    int have_reds;
    int reds;
    int locked = 0;

    do {
	switch (ptlbdp->state) {
	case INITIALIZING:
	    ptlbdp->chunk = erts_alloc(ERTS_ALC_T_PTAB_LIST_CNKI,
				       (sizeof(ErtsPTabListBifChunkInfo)
					* ptab->list.data.chunks));
	    ptlbdp->tix = 0;
	    ptlbdp->pid_ix = 0;

	    erts_ptab_rwlock(ptab);
	    locked = 1;

	    ERTS_PTAB_LIST_DBG_TRACE(c_p->common.id, init);

	    ptlbdp->pid_sz = erts_ptab_count(ptab);
	    ptlbdp->pid = erts_alloc(ERTS_ALC_T_PTAB_LIST_PIDS,
				     sizeof(Eterm)*ptlbdp->pid_sz);

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
	    ptlbdp->debug.pid_started
		= erts_alloc(ERTS_ALC_T_PTAB_LIST_PIDS,
			     sizeof(Uint64)*ptlbdp->pid_sz);
#endif

	    ERTS_PTAB_LIST_DBG_SAVE_PIDS(ptlbdp);

	    if (ptab->list.data.chunks == 1)
		ptlbdp->bif_invocation = NULL;
	    else {
		/*
		 * We will have to access the table multiple times
		 * releasing the table lock in between chunks.
		 */
		ptlbdp->bif_invocation
		    = erts_alloc(ERTS_ALC_T_PTAB_LIST_DEL,
				 sizeof(ErtsPTabDeletedElement));
		ptlbdp->bif_invocation->ix = -1;
		ptlbdp->bif_invocation->u.bif_invocation.interval
		    = erts_smp_step_interval_nob(erts_ptab_interval(ptab));
		ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);

		ptlbdp->bif_invocation->next = NULL;
		if (ptab->list.data.deleted.end) {
		    ptlbdp->bif_invocation->prev = ptab->list.data.deleted.end;
		    ptab->list.data.deleted.end->next = ptlbdp->bif_invocation;
		    ERTS_PTAB_LIST_ASSERT(ptab->list.data.deleted.start);
		}
		else {
		    ptlbdp->bif_invocation->prev = NULL;
		    ptab->list.data.deleted.start = ptlbdp->bif_invocation;
		}
		ptab->list.data.deleted.end = ptlbdp->bif_invocation;

		ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);

	    }

	    ptlbdp->state = INSPECTING_TABLE;
	    /* Fall through */

	case INSPECTING_TABLE: {
	    int ix = ptlbdp->tix;
	    int indices = ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE;
	    int cix = ix / ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE;
	    int end_ix = ix + indices;
	    Uint64 *invocation_interval_p;
	    ErtsPTabElementCommon *invalid_element;

	    invocation_interval_p
		= (ptlbdp->bif_invocation
		   ? &ptlbdp->bif_invocation->u.bif_invocation.interval
		   : NULL);

	    ERTS_PTAB_LIST_ASSERT(is_nil(*res_accp));
	    if (!locked) {
		erts_ptab_rwlock(ptab);
		locked = 1;
	    }

	    ERTS_SMP_LC_ASSERT(erts_smp_lc_ptab_is_rwlocked(ptab));
	    ERTS_PTAB_LIST_DBG_TRACE(p->common.id, insp_table);

	    if (cix != 0)
		ptlbdp->chunk[cix].interval
		    = erts_smp_step_interval_nob(erts_ptab_interval(ptab));
	    else if (ptlbdp->bif_invocation)
		ptlbdp->chunk[0].interval = *invocation_interval_p;
	    /* else: interval is irrelevant */

	    if (end_ix >= ptab->r.o.max) {
		ERTS_PTAB_LIST_ASSERT(cix+1 == ptab->list.data.chunks);
		end_ix = ptab->r.o.max;
		indices = end_ix - ix;
		/* What to do when done with this chunk */
		ptlbdp->state = (ptab->list.data.chunks == 1
				 ? BUILDING_RESULT
				 : INSPECTING_DELETED);
	    }

	    invalid_element = ptab->r.o.invalid_element;
	    for (; ix < end_ix; ix++) {
		ErtsPTabElementCommon *el;
		el = (ErtsPTabElementCommon *) erts_ptab_pix2intptr_nob(ptab,
									ix);
		if (el
		    && el != invalid_element
		    && (!invocation_interval_p
			|| el->u.alive.started_interval < *invocation_interval_p)) {
		    ERTS_PTAB_LIST_ASSERT(erts_ptab_is_valid_id(el->id));
		    ptlbdp->pid[ptlbdp->pid_ix] = el->id;

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
		    ptlbdp->debug.pid_started[ptlbdp->pid_ix]
			= el->u.alive.started_interval;
#endif

		    ptlbdp->pid_ix++;
		    ERTS_PTAB_LIST_ASSERT(ptlbdp->pid_ix <= ptlbdp->pid_sz);
		}
	    }

	    ptlbdp->tix = end_ix;
	    
	    erts_ptab_rwunlock(ptab);
	    locked = 0;

	    reds = indices/ERTS_PTAB_LIST_BIF_TAB_INSPECT_INDICES_PER_RED;
	    BUMP_REDS(c_p, reds);

	    have_reds = ERTS_BIF_REDS_LEFT(c_p);

	    if (have_reds && ptlbdp->state == INSPECTING_TABLE) {
		ix = ptlbdp->tix;
		indices = ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE;
		end_ix = ix + indices;
		if (end_ix > ptab->r.o.max) {
		    end_ix = ptab->r.o.max;
		    indices = end_ix - ix;
		}
		
		reds = indices/ERTS_PTAB_LIST_BIF_TAB_INSPECT_INDICES_PER_RED;

		/* Pretend we have no reds left if we haven't got enough
		   reductions to complete next chunk */
		if (reds > have_reds)
		    have_reds = 0;
	    }

	    break;
	}

	case INSPECTING_DELETED: {
	    int i;
	    int max_reds;
	    int free_deleted = 0;
	    Uint64 invocation_interval;
	    ErtsPTabDeletedElement *ptdep;
	    ErtsPTabDeletedElement *free_list = NULL;

	    ptdep = ptlbdp->bif_invocation;
	    ERTS_PTAB_LIST_ASSERT(ptdep);
	    invocation_interval = ptdep->u.bif_invocation.interval;

	    max_reds = have_reds = ERTS_BIF_REDS_LEFT(c_p);
	    if (max_reds > ERTS_PTAB_LIST_INSPECT_DELETED_MAX_REDS)
		max_reds = ERTS_PTAB_LIST_INSPECT_DELETED_MAX_REDS;

	    reds = 0;
	    erts_ptab_rwlock(ptab);
	    ERTS_PTAB_LIST_DBG_TRACE(p->common.id, insp_term_procs);

	    ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);

	    if (ptdep->prev)
		ptdep->prev->next = ptdep->next;
	    else {
		ERTS_PTAB_LIST_ASSERT(ptab->list.data.deleted.start == ptdep);
		ptab->list.data.deleted.start = ptdep->next;

		if (ptab->list.data.deleted.start
		    && ptab->list.data.deleted.start->ix >= 0) {
		    free_list = ptab->list.data.deleted.start;
		    free_deleted = 1;
		}
	    }

	    if (ptdep->next)
		ptdep->next->prev = ptdep->prev;
	    else
		ptab->list.data.deleted.end = ptdep->prev;

	    ptdep = ptdep->next;

	    i = 0;
	    while (reds < max_reds && ptdep) {
		if (ptdep->ix < 0) {
		    if (free_deleted) {
			ERTS_PTAB_LIST_ASSERT(free_list);
			ERTS_PTAB_LIST_ASSERT(ptdep->prev);

			ptdep->prev->next = NULL; /* end of free_list */
			ptab->list.data.deleted.start = ptdep;
			ptdep->prev = NULL;
			free_deleted = 0;
		    }
		}
		else {
		    int cix = ptdep->ix/ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE;
		    Uint64 chunk_interval = ptlbdp->chunk[cix].interval;
		    Eterm pid = ptdep->u.element.id;
		    ERTS_PTAB_LIST_ASSERT(erts_ptab_is_valid_id(pid));

		    if (ptdep->u.element.inserted < invocation_interval) {
			if (ptdep->u.element.deleted < chunk_interval) {
			    ERTS_PTAB_LIST_DBG_CHK_PID_NOT_FOUND(
				ptlbdp,
				pid,
				ptdep->u.element.inserted);
			    ptlbdp->pid[ptlbdp->pid_ix] = pid;
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
			    ptlbdp->debug.pid_started[ptlbdp->pid_ix]
				= ptdep->u.element.inserted;
#endif
			    ptlbdp->pid_ix++;
			    ERTS_PTAB_LIST_ASSERT(ptlbdp->pid_ix
						  <= ptlbdp->pid_sz);
			}
			else {
			    ERTS_PTAB_LIST_DBG_CHK_PID_FOUND(
				ptlbdp,
				pid,
				ptdep->u.element.inserted);
			}
		    }
		    else {
			ERTS_PTAB_LIST_DBG_CHK_PID_NOT_FOUND(
			    ptlbdp,
			    pid,
			    ptdep->u.element.inserted);
		    }

		    i++;
		    if (i == ERTS_PTAB_LIST_BIF_INSPECT_DELETED_PER_RED) {
			reds++;
			i = 0;
		    }
		    if (free_deleted)
			reds += ERTS_PTAB_LIST_BIF_TAB_FREE_DELETED_REDS;
		}
		ptdep = ptdep->next;
	    }

	    if (free_deleted) {
 		ERTS_PTAB_LIST_ASSERT(free_list);
		ptab->list.data.deleted.start = ptdep;
		if (!ptdep)
		    ptab->list.data.deleted.end = NULL;
		else {
		    ERTS_PTAB_LIST_ASSERT(ptdep->prev);
		    ptdep->prev->next = NULL; /* end of free_list */
		    ptdep->prev = NULL;
		}
	    }

	    if (!ptdep) {
		/* Done */
		ERTS_PTAB_LIST_ASSERT(ptlbdp->pid_ix == ptlbdp->pid_sz);
		ptlbdp->state = BUILDING_RESULT;
		ptlbdp->bif_invocation->next = free_list;
		free_list = ptlbdp->bif_invocation;
		ptlbdp->bif_invocation = NULL;
	    }
	    else {
		/* Link in bif_invocation again where we left off */
		ptlbdp->bif_invocation->prev = ptdep->prev;
		ptlbdp->bif_invocation->next = ptdep;
		ptdep->prev = ptlbdp->bif_invocation;
		if (ptlbdp->bif_invocation->prev)
		    ptlbdp->bif_invocation->prev->next = ptlbdp->bif_invocation;
		else {
		    ERTS_PTAB_LIST_ASSERT(ptab->list.data.deleted.start
					  == ptdep);
		    ptab->list.data.deleted.start = ptlbdp->bif_invocation;
		}
	    }

	    ERTS_PTAB_LIST_DBG_CHK_DEL_LIST(ptab);
	    ERTS_PTAB_LIST_DBG_CHK_FREELIST(ptab, free_list);
	    erts_ptab_rwunlock(ptab);

	    /*
	     * We do the actual free of deleted structures now when we
	     * have released the table lock instead of when we encountered
	     * them. This since free() isn't for free and we don't want to
	     * unnecessarily block other schedulers.
	     */
	    while (free_list) {
		ptdep = free_list;
		free_list = ptdep->next;
		erts_free(ERTS_ALC_T_PTAB_LIST_DEL, ptdep);
	    }

	    have_reds -= reds;
	    if (have_reds < 0)	
		have_reds = 0;
	    BUMP_REDS(c_p, reds);
	    break;
	}

	case BUILDING_RESULT: {
	    int conses, ix, min_ix;
	    Eterm *hp;
	    Eterm res = *res_accp;

	    ERTS_PTAB_LIST_DBG_VERIFY_PIDS(ptlbdp);
	    ERTS_PTAB_LIST_DBG_CHK_RESLIST(res);

	    ERTS_PTAB_LIST_DBG_TRACE(p->common.id, begin_build_res);

	    have_reds = ERTS_BIF_REDS_LEFT(c_p);
	    conses = ERTS_PTAB_LIST_BIF_BUILD_RESULT_CONSES_PER_RED*have_reds;
	    min_ix = ptlbdp->pid_ix - conses;
	    if (min_ix < 0) {
		min_ix = 0;
		conses = ptlbdp->pid_ix;
	    }

	    if (conses) {
		hp = HAlloc(c_p, conses*2);
		ERTS_PTAB_LIST_DBG_SAVE_HEAP_ALLOC(ptlbdp, hp, conses*2);

		for (ix = ptlbdp->pid_ix - 1; ix >= min_ix; ix--) {
		    ERTS_PTAB_LIST_ASSERT(erts_ptab_is_valid_id(ptlbdp->pid[ix]));
		    res = CONS(hp, ptlbdp->pid[ix], res);
		    hp += 2;
		}

		ERTS_PTAB_LIST_DBG_VERIFY_HEAP_ALLOC_USED(ptlbdp, hp);
	    }

	    ptlbdp->pid_ix = min_ix;
	    if (min_ix == 0)
		ptlbdp->state = RETURN_RESULT;
	    else {
		ptlbdp->pid_sz = min_ix;
		ptlbdp->pid = erts_realloc(ERTS_ALC_T_PTAB_LIST_PIDS,
					   ptlbdp->pid,
					   sizeof(Eterm)*ptlbdp->pid_sz);
#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
		ptlbdp->debug.pid_started
		    = erts_realloc(ERTS_ALC_T_PTAB_LIST_PIDS,
				   ptlbdp->debug.pid_started,
				   sizeof(Uint64) * ptlbdp->pid_sz);
#endif
	    }
	    reds = conses/ERTS_PTAB_LIST_BIF_BUILD_RESULT_CONSES_PER_RED;
	    BUMP_REDS(c_p, reds);
	    have_reds -= reds;

	    ERTS_PTAB_LIST_DBG_CHK_RESLIST(res);
	    ERTS_PTAB_LIST_DBG_TRACE(c_p->common.id, end_build_res);
	    *res_accp = res;
	    break;
	}
	case RETURN_RESULT:
	    cleanup_ptab_list_bif_data(mbp);
	    return 1;

	default:
	    erts_exit(ERTS_ABORT_EXIT,
		     "%s:%d:ptab_list_bif_engine(): Invalid state: %d\n",
		     __FILE__, __LINE__, (int) ptlbdp->state);
	}

	
    } while (have_reds || ptlbdp->state == RETURN_RESULT);

    return 0;
}

/*
 * ptab_list_continue/2 is a hidden BIF that the original BIF traps to
 * if there are too much work to do in one go.
 */

static BIF_RETTYPE ptab_list_continue(BIF_ALIST_2)
{
    Eterm res_acc;
    Binary *mbp;

    /*
     * This bif cannot be called from erlang code. It can only be
     * trapped to from other BIFs; therefore, a bad argument
     * is an internal error and should never occur...
     */

    ERTS_PTAB_LIST_DBG_TRACE(BIF_P->common.id, call);
    ERTS_PTAB_LIST_ASSERT(is_nil(BIF_ARG_1) || is_list(BIF_ARG_1));

    res_acc = BIF_ARG_1;

    ERTS_PTAB_LIST_ASSERT(ERTS_TERM_IS_MAGIC_BINARY(BIF_ARG_2));

    mbp = ((ProcBin *) binary_val(BIF_ARG_2))->val;

    ERTS_PTAB_LIST_ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(mbp)
			  == cleanup_ptab_list_bif_data);
    ERTS_PTAB_LIST_ASSERT(
	((ErtsPTabListBifData *) ERTS_MAGIC_BIN_DATA(mbp))->debug.caller
	== BIF_P->common.id);

    if (ptab_list_bif_engine(BIF_P, &res_acc, mbp)) {
	ERTS_PTAB_LIST_DBG_TRACE(BIF_P->common.id, return);
	BIF_RET(res_acc);
    }
    else {
	ERTS_PTAB_LIST_DBG_TRACE(BIF_P->common.id, trap);
	ERTS_BIF_YIELD2(&ptab_list_continue_export, BIF_P, res_acc, BIF_ARG_2);
    }
}

void
erts_ptab_init(void)
{
    /* ptab_list_continue/2 is a hidden BIF that the original BIF traps to. */
    erts_init_trap_export(&ptab_list_continue_export,
			  am_erlang, am_ptab_list_continue, 2,
			  &ptab_list_continue);

}

/*
 * Debug stuff
 */

static void assert_ptab_consistency(ErtsPTab *ptab)
{
#ifdef DEBUG
    if (ptab->r.o.free_id_data) {
	Uint32 ix, pix, data;
	int free_pids = 0;
	int null_slots = 0;
	
	for (ix=0; ix < ptab->r.o.max; ix++) {
	    if (erts_smp_atomic32_read_nob(&ptab->r.o.free_id_data[ix]) != ptab->r.o.invalid_data) {
		++free_pids;
		data = erts_smp_atomic32_read_nob(&ptab->r.o.free_id_data[ix]);
		pix = erts_ptab_data2pix(ptab, (Eterm) data);
		ASSERT(erts_ptab_pix2intptr_nob(ptab, pix) == ERTS_AINT_NULL);
	    }
	    if (erts_smp_atomic_read_nob(&ptab->r.o.tab[ix]) == ERTS_AINT_NULL) {
		++null_slots;
	    }
	}	
	ASSERT(free_pids == null_slots);
	ASSERT(free_pids == ptab->r.o.max - erts_smp_atomic32_read_nob(&ptab->vola.tile.count));
    }
#endif
}

Sint
erts_ptab_test_next_id(ErtsPTab *ptab, int set, Uint next)
{
    Uint64 ld;
    Sint res;
    Eterm data;
    int first_pix = -1;

    erts_ptab_rwlock(ptab);

    assert_ptab_consistency(ptab);

    if (ptab->r.o.free_id_data) {
	Uint32 id_ix, dix;

	if (set) {
	    Uint32 i, max_ix, num, stop_id_ix;
	    max_ix = ptab->r.o.max - 1;
	    num = next;
	    id_ix = (Uint32) erts_smp_atomic32_read_nob(&ptab->vola.tile.aid_ix);

	    for (i=0; i <= max_ix; ++i) {
		Uint32 pix;
		++num;
		num &= ~(~((Uint32) 0) << ERTS_PTAB_ID_DATA_SIZE);
		if (num == ptab->r.o.invalid_data) {
		    num += ptab->r.o.max;
		    num &= ~(~((Uint32) 0) << ERTS_PTAB_ID_DATA_SIZE);
		}
		pix = erts_ptab_data2pix(ptab, num);
		if (ERTS_AINT_NULL == erts_ptab_pix2intptr_nob(ptab, pix)) {
		    ++id_ix;
		    dix = ix_to_free_id_data_ix(ptab, id_ix);
		    erts_smp_atomic32_set_nob(&ptab->r.o.free_id_data[dix], num);
		    ASSERT(pix == erts_ptab_data2pix(ptab, num));
		}
	    }
	    erts_smp_atomic32_set_nob(&ptab->vola.tile.fid_ix, id_ix);

	    /* Write invalid_data in rest of free_id_data[]: */
	    stop_id_ix = (1 + erts_smp_atomic32_read_nob(&ptab->vola.tile.aid_ix)) & max_ix;
	    while (1) {
		id_ix = (id_ix+1) & max_ix;
		if (id_ix == stop_id_ix)
		    break;
		dix = ix_to_free_id_data_ix(ptab, id_ix);
		erts_smp_atomic32_set_nob(&ptab->r.o.free_id_data[dix],
					  ptab->r.o.invalid_data);
	    }
	}
	id_ix = (Uint32) erts_smp_atomic32_read_nob(&ptab->vola.tile.aid_ix) + 1;
	dix = ix_to_free_id_data_ix(ptab, id_ix);
	res = (Sint) erts_smp_atomic32_read_nob(&ptab->r.o.free_id_data[dix]);
    }
    else {
	/* Deprecated legacy algorithm... */
	if (!set)
	    ld = last_data_read_nob(ptab);
	else {

	    ld = (Uint64) next;
	    data = ERTS_PTAB_LastData2EtermData(ld);
	    if (ptab->r.o.invalid_data == data) {
		ld += ptab->r.o.max;
		ASSERT(erts_ptab_data2pix(ptab, data)
		       == erts_ptab_data2pix(ptab,
					     ERTS_PTAB_LastData2EtermData(ld)));
	    }
	    last_data_set_relb(ptab, ld);
	}

	while (1) {
	    int pix;
	    ld++;
	    pix = (int) (ld % ptab->r.o.max);
	    if (first_pix < 0)
		first_pix = pix;
	    else if (pix == first_pix) {
		res = -1;
		break;
	    }
	    if (ERTS_AINT_NULL == erts_ptab_pix2intptr_nob(ptab, pix)) {
		data = ERTS_PTAB_LastData2EtermData(ld);
		if (ptab->r.o.invalid_data == data) {
		    ld += ptab->r.o.max;
		    ASSERT(erts_ptab_data2pix(ptab, data)
			   == erts_ptab_data2pix(ptab,
						 ERTS_PTAB_LastData2EtermData(ld)));
		    data = ERTS_PTAB_LastData2EtermData(ld);
		}
		res = data;
		break;
	    }
	}
    }

    assert_ptab_consistency(ptab);
    erts_ptab_rwunlock(ptab);

    return res;
}

static ERTS_INLINE ErtsPTabElementCommon *
ptab_pix2el(ErtsPTab *ptab, int ix)
{
    ErtsPTabElementCommon *ptab_el;
    ASSERT(0 <= ix && ix < ptab->r.o.max);
    ptab_el = (ErtsPTabElementCommon *) erts_ptab_pix2intptr_nob(ptab, ix);
    if (ptab_el == ptab->r.o.invalid_element)
	return NULL;
    else
	return ptab_el;
}

Eterm
erts_debug_ptab_list(Process *c_p, ErtsPTab *ptab)
{
    int i;
    Uint need;
    Eterm res;
    Eterm* hp;
    Eterm *hp_end;

    erts_ptab_rwlock(ptab);

    res = NIL;
    need = erts_ptab_count(ptab) * 2;
    hp = HAlloc(c_p, need); /* we need two heap words for each id */
    hp_end = hp + need;
     
    /* make the list by scanning bakward */


    for (i = ptab->r.o.max-1; i >= 0; i--) {
	ErtsPTabElementCommon *el = ptab_pix2el(ptab, i);
	if (el) {
	    res = CONS(hp, el->id, res);
	    hp += 2;
	}
    }

    erts_ptab_rwunlock(ptab);

    HRelease(c_p, hp_end, hp);

    return res;
}

Eterm
erts_debug_ptab_list_bif_info(Process *c_p, ErtsPTab *ptab)
{
    ERTS_DECL_AM(ptab_list_bif_info);
    Eterm elements[] = {
	AM_ptab_list_bif_info,
	make_small((Uint) ERTS_PTAB_LIST_BIF_MIN_START_REDS),
	make_small((Uint) ptab->list.data.chunks),
	make_small((Uint) ERTS_PTAB_LIST_BIF_TAB_CHUNK_SIZE),
	make_small((Uint) ERTS_PTAB_LIST_BIF_TAB_INSPECT_INDICES_PER_RED),
	make_small((Uint) ERTS_PTAB_LIST_BIF_TAB_FREE_DELETED_REDS),
	make_small((Uint) ERTS_PTAB_LIST_BIF_INSPECT_DELETED_PER_RED),
	make_small((Uint) ERTS_PTAB_LIST_INSPECT_DELETED_MAX_REDS),
	make_small((Uint) ERTS_PTAB_LIST_BIF_BUILD_RESULT_CONSES_PER_RED),
	make_small((Uint) ERTS_PTAB_LIST_BIF_DEBUGLEVEL)
    };
    Uint sz = 0;
    Eterm *hp;
    (void) erts_bld_tuplev(NULL, &sz, sizeof(elements)/sizeof(Eterm), elements);
    hp = HAlloc(c_p, sz);
    return erts_bld_tuplev(&hp, NULL, sizeof(elements)/sizeof(Eterm), elements);
}

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_FOUND_PIDS
static void
debug_ptab_list_check_found_pid(ErtsPTabListBifData *ptlbdp,
				Eterm pid,
				Uint64 ic,
				int pid_should_be_found)
{
    int i;
    for (i = 0; i < ptlbdp->pid_ix; i++) {
	if (ptlbdp->pid[i] == pid && ptlbdp->debug.pid_started[i] == ic) {
	    ERTS_PTAB_LIST_ASSERT(pid_should_be_found);
	    return;
	}
    }
    ERTS_PTAB_LIST_ASSERT(!pid_should_be_found);
}
#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_RESLIST
static void
debug_ptab_list_check_res_list(Eterm list)
{
    while (is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);
	ERTS_PTAB_LIST_ASSERT(erts_ptab_is_valid_id(hd));
	list = CDR(consp);
    }

    ERTS_PTAB_LIST_ASSERT(is_nil(list));
}
#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_PIDS

static void
debug_ptab_list_save_all_pids(ErtsPTabListBifData *ptlbdp)
{
    int ix, tix, cpix;
    ErtsPTab *ptab = ptlbdp->ptab;
    ptlbdp->debug.correct_pids_verified = 0;
    ptlbdp->debug.correct_pids = erts_alloc(ERTS_ALC_T_PTAB_LIST_PIDS,
					    sizeof(Eterm)*ptlbdp->pid_sz);

    for (tix = 0, cpix = 0; tix < ptab->r.o.max; tix++) {
	ErtsPTabElementCommon *el = ptab_pix2el(ptab, tix);
	if (el) {
	    ERTS_PTAB_LIST_ASSERT(erts_ptab_is_valid_id(el->id));
	    ptlbdp->debug.correct_pids[cpix++] = el->id;
	    ERTS_PTAB_LIST_ASSERT(cpix <= ptlbdp->pid_sz);
	}
    }
    ERTS_PTAB_LIST_ASSERT(cpix == ptlbdp->pid_sz);

    for (ix = 0; ix < ptlbdp->pid_sz; ix++)
	ptlbdp->pid[ix] = make_small(ix);
}

static void
debug_ptab_list_verify_all_pids(ErtsPTabListBifData *ptlbdp)
{
    int ix, cpix;

    ERTS_PTAB_LIST_ASSERT(ptlbdp->pid_ix == ptlbdp->pid_sz);

    for (ix = 0; ix < ptlbdp->pid_sz; ix++) {
	int found = 0;
	Eterm pid = ptlbdp->pid[ix];
	ERTS_PTAB_LIST_ASSERT(erts_ptab_is_valid_id(pid));
	for (cpix = ix; cpix < ptlbdp->pid_sz; cpix++) {
	    if (ptlbdp->debug.correct_pids[cpix] == pid) {
		ptlbdp->debug.correct_pids[cpix] = NIL;
		found = 1;
		break;
	    }
	}
	if (!found) {
	    for (cpix = 0; cpix < ix; cpix++) {
		if (ptlbdp->debug.correct_pids[cpix] == pid) {
		    ptlbdp->debug.correct_pids[cpix] = NIL;
		    found = 1;
		    break;
		}
	    }
	}
	ERTS_PTAB_LIST_ASSERT(found);
    }
    ptlbdp->debug.correct_pids_verified = 1;

    erts_free(ERTS_ALC_T_PTAB_LIST_PIDS, ptlbdp->debug.correct_pids);
    ptlbdp->debug.correct_pids = NULL;
}
#endif /* ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_PIDS */

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL >= ERTS_PTAB_LIST_DBGLVL_CHK_DEL_LIST
static void
debug_ptab_list_check_del_list(ErtsPTab *ptab)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_ptab_is_rwlocked(ptab));
    if (!ptab->list.data.deleted.start)
	ERTS_PTAB_LIST_ASSERT(!ptab->list.data.deleted.end);
    else {
	Uint64 curr_interval = erts_smp_current_interval_nob(erts_ptab_interval(ptab));
	Uint64 *prev_x_interval_p = NULL;
	ErtsPTabDeletedElement *ptdep;

	for (ptdep = ptab->list.data.deleted.start;
	     ptdep;
	     ptdep = ptdep->next) {
	    if (!ptdep->prev)
		ERTS_PTAB_LIST_ASSERT(ptab->list.data.deleted.start == ptdep);
	    else
		ERTS_PTAB_LIST_ASSERT(ptdep->prev->next == ptdep);
	    if (!ptdep->next)
		ERTS_PTAB_LIST_ASSERT(ptab->list.data.deleted.end == ptdep);
	    else
		ERTS_PTAB_LIST_ASSERT(ptdep->next->prev == ptdep);
	    if (ptdep->ix < 0) {
		Uint64 interval = ptdep->u.bif_invocation.interval;
		ERTS_PTAB_LIST_ASSERT(interval <= curr_interval);
	    }
	    else {
		Uint64 s_interval = ptdep->u.element.inserted;
		Uint64 x_interval = ptdep->u.element.deleted;
		
		ERTS_PTAB_LIST_ASSERT(s_interval <= x_interval);
		if (prev_x_interval_p)
		    ERTS_PTAB_LIST_ASSERT(*prev_x_interval_p <= x_interval);
		prev_x_interval_p = &ptdep->u.element.deleted;
		ERTS_PTAB_LIST_ASSERT(
		    erts_ptab_is_valid_id(ptdep->u.element.id));
		ERTS_PTAB_LIST_ASSERT(erts_ptab_id2pix(ptab,
						       ptdep->u.element.id)
				      == ptdep->ix);

	    }
	}
	
    }
}

static void
debug_ptab_list_check_del_free_list(ErtsPTab *ptab,
				    ErtsPTabDeletedElement *free_list)
{
    if (ptab->list.data.deleted.start) {
	ErtsPTabDeletedElement *fptdep;
	ErtsPTabDeletedElement *ptdep;

	for (fptdep = free_list; fptdep; fptdep = fptdep->next) {
	    for (ptdep = ptab->list.data.deleted.start;
		 ptdep;
		 ptdep = ptdep->next) {
		ERTS_PTAB_LIST_ASSERT(fptdep != ptdep);
	    }
	}
    }
}

#endif

#if ERTS_PTAB_LIST_BIF_DEBUGLEVEL != 0

static void
debug_ptab_list_assert_error(char* expr, const char* file, int line, const char *func)
{   
    fflush(stdout);
    erts_fprintf(stderr, "%s:%d:%s(): Assertion failed: %s\n",
		 (char *) file, line, (char *) func, expr);
    fflush(stderr);
    abort();
}

#endif
