/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012. All Rights Reserved.
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
 * Description:	Process/Port table implementation.
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_PTAB_H__
#define ERL_PTAB_H__

#include "sys.h"
#include "erl_term.h"
#include "erl_time.h"
#include "erl_utils.h"
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_alloc.h"
#include "erl_monitors.h"

#define ERTS_TRACER_PROC(P) 	((P)->common.tracer_proc)
#define ERTS_TRACE_FLAGS(P)	((P)->common.trace_flags)

#define ERTS_P_LINKS(P)		((P)->common.u.alive.links)
#define ERTS_P_MONITORS(P)	((P)->common.u.alive.monitors)

#define IS_TRACED(p) \
    (ERTS_TRACER_PROC((p)) != NIL)
#define ARE_TRACE_FLAGS_ON(p,tf) \
    ((ERTS_TRACE_FLAGS((p)) & (tf|F_SENSITIVE)) == (tf))
#define IS_TRACED_FL(p,tf) \
    ( IS_TRACED(p) && ARE_TRACE_FLAGS_ON(p,tf) )

typedef struct {
    Eterm id;
#ifdef ERTS_SMP
    erts_atomic32_t refc;
#else
    erts_smp_atomic32_t refc; /* Temporary solution during dev; to be removed! */
#endif
    Eterm tracer_proc;
    Uint trace_flags;
    union {
	/* --- While being alive --- */
	struct {
	    Uint64 started_interval;
	    struct reg_proc *reg;
	    ErtsLink *links;
	    ErtsMonitor *monitors;
#ifdef ERTS_SMP
	    ErtsSmpPTimer *ptimer;
#else
	    ErlTimer tm;
#endif
	} alive;

	/* --- While being released --- */
#ifdef ERTS_SMP
	ErtsThrPrgrLaterOp release;
#endif
    } u;
} ErtsPTabElementCommon;

typedef struct ErtsPTabDeletedElement_ ErtsPTabDeletedElement;

typedef struct {
    erts_smp_rwmtx_t rwmtx;
    erts_interval_t interval;
    struct {
	ErtsPTabDeletedElement *start;
	ErtsPTabDeletedElement *end;
    } deleted;
    int chunks;
} ErtsPTabListData;

typedef struct {
#ifdef ARCH_32
    erts_smp_dw_atomic_t last_data;
#else
    erts_smp_atomic_t last_data;
#endif
    erts_smp_atomic32_t count;
} ErtsPTabVolatileData;

typedef struct {
    erts_smp_atomic_t *tab;
    int max;
    int tab_cache_lines;
    int pix_per_cache_line;
    int pix_cl_mask;
    int pix_cl_shift;
    int pix_cli_mask;
    int pix_cli_shift;
    ErtsPTabElementCommon *invalid_element;
    Eterm invalid_data;
    void (*release_element)(void *);
} ErtsPTabReadOnlyData;

typedef struct {
    /*
     * Data mainly modified when someone is listing
     * the content of the table.
     */
    union {
	ErtsPTabListData data;
	char algn[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsPTabListData))];
    } list;

    /*
     * Frequently modified data.
     */
    union {
	ErtsPTabVolatileData tile;
	char algn[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsPTabVolatileData))];
    } vola;

    /*
     * Read only data.
     */
    union {
	ErtsPTabReadOnlyData o;
	char algn[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsPTabReadOnlyData))];
    } r;
} ErtsPTab;

#define ERTS_PTAB_ID_DATA_SIZE	28
#define ERTS_PTAB_ID_DATA_SHIFT	(_TAG_IMMED1_SIZE)

/*
 * Currently pids and ports are allowed.
 */
#if _PID_DATA_SIZE != ERTS_PTAB_ID_DATA_SIZE
# error "Unexpected pid data size"
#endif
#if _PID_DATA_SHIFT != ERTS_PTAB_ID_DATA_SHIFT
# error "Unexpected pid tag size"
#endif
#if _PORT_DATA_SIZE != ERTS_PTAB_ID_DATA_SIZE
# error "Unexpected port data size"
#endif
#if _PORT_DATA_SHIFT != ERTS_PTAB_ID_DATA_SHIFT
# error "Unexpected port tag size"
#endif

#define erts_ptab_is_valid_id(ID)					\
    (is_internal_pid((ID)) || is_internal_port((ID)))

#define ERTS_PTAB_ID2DATA(ID)						\
    (ASSERT_EXPR(erts_ptab_is_valid_id((ID))),				\
     (((ID) >> ERTS_PTAB_ID_DATA_SHIFT)					\
      & ~(~((Uint) 0) << ERTS_PTAB_ID_DATA_SIZE)))

void erts_ptab_init(void);
void erts_ptab_init_table(ErtsPTab *ptab,
			  ErtsAlcType_t atype,
			  void (*release_element)(void *),
			  ErtsPTabElementCommon *invalid_element,
			  int size,
			  char *name);
int erts_ptab_new_element(ErtsPTab *ptab,
			  ErtsPTabElementCommon *ptab_el,
			  void *init_arg,
			  void (*init_ptab_el)(void *, Eterm));
void erts_ptab_delete_element(ErtsPTab *ptab,
			      ErtsPTabElementCommon *ptab_el);
int erts_ptab_initialized(ErtsPTab *ptab);

ERTS_GLB_INLINE erts_interval_t *erts_ptab_interval(ErtsPTab *ptab);
ERTS_GLB_INLINE int erts_ptab_max(ErtsPTab *ptab);
ERTS_GLB_INLINE int erts_ptab_count(ErtsPTab *ptab);
ERTS_GLB_INLINE int erts_ptab_data2ix(ErtsPTab *ptab, Eterm data);
ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_nob(ErtsPTab *ptab, int ix);
ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_ddrb(ErtsPTab *ptab, int ix);
ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_rb(ErtsPTab *ptab, int ix);
ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_acqb(ErtsPTab *ptab, int ix);
ERTS_GLB_INLINE void erts_ptab_inc_refc(ErtsPTabElementCommon *ptab_el);
ERTS_GLB_INLINE int erts_ptab_dec_test_refc(ErtsPTabElementCommon *ptab_el);
ERTS_GLB_INLINE int erts_ptab_add_test_refc(ErtsPTabElementCommon *ptab_el,
					    Sint32 add_refc);
ERTS_GLB_INLINE void erts_ptab_rlock(ErtsPTab *ptab);
ERTS_GLB_INLINE int erts_ptab_tryrlock(ErtsPTab *ptab);
ERTS_GLB_INLINE void erts_ptab_runlock(ErtsPTab *ptab);
ERTS_GLB_INLINE void erts_ptab_rwlock(ErtsPTab *ptab);
ERTS_GLB_INLINE int erts_ptab_tryrwlock(ErtsPTab *ptab);
ERTS_GLB_INLINE void erts_ptab_rwunlock(ErtsPTab *ptab);
ERTS_GLB_INLINE int erts_smp_lc_ptab_is_rlocked(ErtsPTab *ptab);
ERTS_GLB_INLINE int erts_smp_lc_ptab_is_rwlocked(ErtsPTab *ptab);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE erts_interval_t *
erts_ptab_interval(ErtsPTab *ptab)
{
    return &ptab->list.data.interval;
}

ERTS_GLB_INLINE int
erts_ptab_max(ErtsPTab *ptab)
{
    return ptab->r.o.max;
}

ERTS_GLB_INLINE int
erts_ptab_count(ErtsPTab *ptab)
{
    erts_aint32_t res = erts_smp_atomic32_read_nob(&ptab->vola.tile.count);
    if (res > ptab->r.o.max)
	return ptab->r.o.max;
    ASSERT(res >= 0);
    return (int) res;

}

ERTS_GLB_INLINE int erts_ptab_data2ix(ErtsPTab *ptab, Eterm data)
{
    int n, pix;

    n = (int) data;
    if (ptab->r.o.pix_cl_mask) {
	pix = ((n & ptab->r.o.pix_cl_mask) << ptab->r.o.pix_cl_shift);
	pix += ((n >> ptab->r.o.pix_cli_shift) & ptab->r.o.pix_cli_mask);
    }
    else {
	n %= ptab->r.o.max;
	pix = n % ptab->r.o.tab_cache_lines;
	pix *= ptab->r.o.pix_per_cache_line;
	pix += n / ptab->r.o.tab_cache_lines;
    }
    ASSERT(0 <= pix && pix < ptab->r.o.max);
    return pix;
}

ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_nob(ErtsPTab *ptab, int ix)
{
    ASSERT(0 <= ix && ix < ptab->r.o.max);
    return erts_smp_atomic_read_nob(&ptab->r.o.tab[ix]);
}

ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_ddrb(ErtsPTab *ptab, int ix)
{
    ASSERT(0 <= ix && ix < ptab->r.o.max);
    return erts_smp_atomic_read_ddrb(&ptab->r.o.tab[ix]);
}

ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_rb(ErtsPTab *ptab, int ix)
{
    ASSERT(0 <= ix && ix < ptab->r.o.max);
    return erts_smp_atomic_read_rb(&ptab->r.o.tab[ix]);
}

ERTS_GLB_INLINE erts_aint_t erts_ptab_pix2intptr_acqb(ErtsPTab *ptab, int ix)
{
    ASSERT(0 <= ix && ix < ptab->r.o.max);
    return erts_smp_atomic_read_acqb(&ptab->r.o.tab[ix]);
}

ERTS_GLB_INLINE void erts_ptab_inc_refc(ErtsPTabElementCommon *ptab_el)
{
#ifdef ERTS_SMP
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_aint32_t refc = erts_atomic32_inc_read_nob(&ptab_el->refc);
    ERTS_SMP_LC_ASSERT(refc > 1);
#else
    erts_atomic32_inc_nob(&ptab_el->refc);
#endif
#endif
}

ERTS_GLB_INLINE int erts_ptab_dec_test_refc(ErtsPTabElementCommon *ptab_el)
{
#ifdef ERTS_SMP
    erts_aint32_t refc = erts_atomic32_dec_read_nob(&ptab_el->refc);
    ERTS_SMP_LC_ASSERT(refc >= 0);
    return (int) refc;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE int erts_ptab_add_test_refc(ErtsPTabElementCommon *ptab_el,
					    Sint32 add_refc)
{
#ifdef ERTS_SMP
    erts_aint32_t refc;

#ifndef ERTS_ENABLE_LOCK_CHECK
    if (add_refc >= 0) {
	erts_atomic32_add_nob(&ptab_el->refc,
			      (erts_aint32_t) add_refc);
	return 1;
    }
#endif

    refc = erts_atomic32_add_read_nob(&ptab_el->refc,
				      (erts_aint32_t) add_refc);
    ERTS_SMP_LC_ASSERT(refc >= 0);
    return (int) refc;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void erts_ptab_rlock(ErtsPTab *ptab)
{
    erts_smp_rwmtx_rlock(&ptab->list.data.rwmtx);
}

ERTS_GLB_INLINE int erts_ptab_tryrlock(ErtsPTab *ptab)
{
    return erts_smp_rwmtx_tryrlock(&ptab->list.data.rwmtx);
}

ERTS_GLB_INLINE void erts_ptab_runlock(ErtsPTab *ptab)
{
    erts_smp_rwmtx_runlock(&ptab->list.data.rwmtx);
}

ERTS_GLB_INLINE void erts_ptab_rwlock(ErtsPTab *ptab)
{
    erts_smp_rwmtx_rwlock(&ptab->list.data.rwmtx);
}

ERTS_GLB_INLINE int erts_ptab_tryrwlock(ErtsPTab *ptab)
{
    return erts_smp_rwmtx_tryrwlock(&ptab->list.data.rwmtx);
}

ERTS_GLB_INLINE void erts_ptab_rwunlock(ErtsPTab *ptab)
{
    erts_smp_rwmtx_rwunlock(&ptab->list.data.rwmtx);
}

ERTS_GLB_INLINE int erts_smp_lc_ptab_is_rlocked(ErtsPTab *ptab)
{
    return erts_smp_lc_rwmtx_is_rlocked(&ptab->list.data.rwmtx);
}

ERTS_GLB_INLINE int erts_smp_lc_ptab_is_rwlocked(ErtsPTab *ptab)
{
    return erts_smp_lc_rwmtx_is_rwlocked(&ptab->list.data.rwmtx);
}

#endif

#endif

#if defined(ERTS_PTAB_WANT_BIF_IMPL__) && !defined(ERTS_PTAB_LIST__)
#define ERTS_PTAB_LIST__

#include "erl_process.h"
#include "bif.h"

BIF_RETTYPE erts_ptab_list(struct process *c_p, ErtsPTab *ptab);

#endif

#if defined(ERTS_PTAB_WANT_DEBUG_FUNCS__) && !defined(ERTS_PTAB_DEBUG_FUNCS__)
#define ERTS_PTAB_DEBUG_FUNCS__
#include "erl_process.h"

/* Debug functions */
Sint erts_ptab_test_next_id(ErtsPTab *ptab, int set, Uint next);
Eterm erts_debug_ptab_list(Process *c_p, ErtsPTab *ptab);
Eterm erts_debug_ptab_list_bif_info(Process *c_p, ErtsPTab *ptab);

#endif
