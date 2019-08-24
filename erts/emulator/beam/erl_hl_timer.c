/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2015-2018. All Rights Reserved.
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
 * Description: High level timers implementing BIF timers
 *              as well as process and port timers.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "bif.h"
#include "erl_bif_unique.h"
#define ERTS_WANT_TIMER_WHEEL_API
#include "erl_time.h"
#include "erl_hl_timer.h"
#include "erl_proc_sig_queue.h"

#define ERTS_TMR_CHECK_CANCEL_ON_CREATE 0

#if 0
#  define ERTS_HLT_HARD_DEBUG
#endif
#if 0
#  define ERTS_HLT_DEBUG
#endif

#if defined(ERTS_HLT_HARD_DEBUG) || defined(DEBUG)
#  if defined(ERTS_HLT_HARD_DEBUG)
#    undef ERTS_RBT_HARD_DEBUG
#    define ERTS_RBT_HARD_DEBUG 1
#  endif
#  ifndef ERTS_HLT_DEBUG
#    define ERTS_HLT_DEBUG 1
#  endif
#endif

#undef ERTS_HLT_ASSERT
#if defined(ERTS_HLT_DEBUG)
#  define ERTS_HLT_ASSERT(E) ERTS_ASSERT(E)
#  undef ERTS_RBT_DEBUG
#  define ERTS_RBT_DEBUG
#else
#  define ERTS_HLT_ASSERT(E) ((void) 1)
#endif

#if defined(ERTS_HLT_HARD_DEBUG) && defined(__GNUC__)
#warning "* * * * * * * * * * * * * * * * * *"
#warning "* ERTS_HLT_HARD_DEBUG IS ENABLED! *"
#warning "* * * * * * * * * * * * * * * * * *"
#endif

#ifdef ERTS_HLT_HARD_DEBUG
#  define ERTS_HLT_HDBG_CHK_SRV(SRV) hdbg_chk_srv((SRV))
static void hdbg_chk_srv(ErtsHLTimerService *srv);
#else
#  define ERTS_HLT_HDBG_CHK_SRV(SRV) ((void) 1)
#endif

#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif

typedef enum {
    ERTS_TMR_BIF,
    ERTS_TMR_PROC,
    ERTS_TMR_PORT,
    ERTS_TMR_CALLBACK
} ErtsTmrType;

#define ERTS_BIF_TIMER_SHORT_TIME 5000

/* Bit 0 to 10 contains scheduler id (see mask below) */
#define ERTS_TMR_ROFLG_HLT		(((Uint32) 1) << 11)
#define ERTS_TMR_ROFLG_BIF_TMR		(((Uint32) 1) << 12)
#define ERTS_TMR_ROFLG_PRE_ALC		(((Uint32) 1) << 13)
#define ERTS_TMR_ROFLG_REG_NAME		(((Uint32) 1) << 14)
#define ERTS_TMR_ROFLG_PROC		(((Uint32) 1) << 15)
#define ERTS_TMR_ROFLG_PORT		(((Uint32) 1) << 16)
#define ERTS_TMR_ROFLG_CALLBACK		(((Uint32) 1) << 17)

#define ERTS_TMR_ROFLG_SID_MASK	\
    (ERTS_TMR_ROFLG_HLT - (Uint32) 1)

#define ERTS_TMR_STATE_ACTIVE		((erts_aint32_t) 0)
#define ERTS_TMR_STATE_CANCELED		((erts_aint32_t) 1)
#define ERTS_TMR_STATE_TIMED_OUT	((erts_aint32_t) 2)

typedef struct ErtsHLTimer_ ErtsHLTimer;

#define ERTS_HLT_PFLG_RED		(((UWord) 1) << 0)
#define ERTS_HLT_PFLG_SAME_TIME		(((UWord) 1) << 1)

#define ERTS_HLT_PFLGS_MASK \
    (ERTS_HLT_PFLG_RED|ERTS_HLT_PFLG_SAME_TIME)

#define ERTS_HLT_PFIELD_NOT_IN_TABLE	(~((UWord) 0))

typedef struct ErtsBifTimer_ ErtsBifTimer;

typedef struct {
    ErtsBifTimer *next;
    ErtsBifTimer *prev;    
} ErtsBifTimerList;

typedef struct {
    UWord parent; /* parent pointer and flags... */
    union {
	struct {
	    ErtsHLTimer *right;
	    ErtsHLTimer *left;
	} t;
	struct {
	    ErtsHLTimer *prev;
	    ErtsHLTimer *next;
	} l;
    } u;
    ErtsHLTimer *same_time;
} ErtsHLTimerTimeTree;

typedef struct {
    UWord parent; /* parent pointer and flags... */
    ErtsBifTimer *right;
    ErtsBifTimer *left;
} ErtsBifTimerTree;

typedef struct {
    Uint32 roflgs;
    erts_atomic32_t refc;
    union {
	void *arg;
	erts_atomic_t next;
    } u;
    union {
	Process *proc;
	Port *port;
	Eterm name;
	void (*callback)(void *);
    } receiver;
} ErtsTmrHead;

struct ErtsHLTimer_ {
    ErtsTmrHead head; /* NEED to be first! */
    ErtsMonotonicTime timeout;
    union {
	ErtsThrPrgrLaterOp cleanup;
	ErtsHLTimerTimeTree tree;
    } time;

#ifdef ERTS_HLT_HARD_DEBUG
    int pending_timeout;
#endif
};

typedef struct {
    ErtsTmrHead head; /* NEED to be first! */
    union {
        ErtsTWheelTimer tw_tmr;
	ErtsThrPrgrLaterOp cleanup;
    } u;
} ErtsTWTimer;

struct ErtsBifTimer_ {
    union {
        ErtsTmrHead head;
        ErtsHLTimer hlt;
        ErtsTWTimer twt;
    } type;
    struct {
        erts_atomic32_t state;
	Uint32 refn[ERTS_REF_NUMBERS];
	ErtsBifTimerTree proc_tree;
	ErtsBifTimerTree tree;
        Eterm message;
        ErlHeapFragment *bp;
    } btm;
};

typedef union {
    ErtsTmrHead head;
    ErtsHLTimer hlt;
    ErtsTWTimer twt;
    ErtsBifTimer btm;
} ErtsTimer;

typedef ErtsTimer *(*ErtsCreateTimerFunc)(ErtsSchedulerData *esdp,
                                          ErtsMonotonicTime timeout_pos,
                                          int short_time, ErtsTmrType type,
                                          void *rcvrp, Eterm rcvr,
                                          Eterm msg,
                                          Uint32 *refn,
                                          void (*callback)(void *), void *arg);

#ifdef SMALL_MEMORY
#define BIF_TIMER_PREALC_SZ	10
#define PTIMER_PREALC_SZ	10
#else
#define BIF_TIMER_PREALC_SZ	100
#define PTIMER_PREALC_SZ	100
#endif

ERTS_SCHED_PREF_PALLOC_IMPL(bif_timer_pre,
			    ErtsBifTimer,
			    BIF_TIMER_PREALC_SZ)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(tw_timer,
				 ErtsTWTimer,
				 PTIMER_PREALC_SZ,
				 ERTS_ALC_T_LL_PTIMER)

#ifdef ERTS_HLT_DEBUG
#define ERTS_TMR_TIMEOUT_YIELD_LIMIT 5
#else
#define ERTS_TMR_TIMEOUT_YIELD_LIMIT 100
#endif
#define ERTS_TMR_CANCELED_TIMER_LIMIT 100
#define ERTS_TMR_CANCELED_TIMER_SMALL_LIMIT 5

#define ERTS_TMR_TIMEOUT_YIELD_STATE_T same_time_list_yield_state_t
#define ERTS_TMR_YIELDING_TIMEOUT_STATE_INITER {NULL, {0}}
typedef struct {
    int dummy;
} ERTS_TMR_TIMEOUT_YIELD_STATE_T;

typedef struct {
    ErtsTmrHead marker;
    erts_atomic_t last;
} ErtsHLTCncldTmrQTail;


typedef struct {
    /*
     * This structure needs to be cache line aligned for best
     * performance.
     */
    union {
	/*
	 * Modified by threads returning canceled
	 * timers to this timer service.
	 */
	ErtsHLTCncldTmrQTail data;
	char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(
		sizeof(ErtsHLTCncldTmrQTail))];
    } tail;
    /*
     * Everything below this point is *only* accessed by the
     * thread managing this timer service.
     */
    struct {
	ErtsTimer *first;
	ErtsTimer *unref_end;
	struct {
	    ErtsThrPrgrVal thr_progress;
	    int thr_progress_reached;
	    ErtsTimer *unref_end;
	} next;
	int used_marker;
    } head;
} ErtsHLTCncldTmrQ;


typedef struct {
    ErtsHLTimer *root;
    ERTS_TMR_TIMEOUT_YIELD_STATE_T state;
} ErtsYieldingTimeoutState;

struct ErtsHLTimerService_ {
    ErtsHLTCncldTmrQ canceled_queue;
    ErtsHLTimer *time_tree;
    ErtsBifTimer *btm_tree;
    ErtsHLTimer *next_timeout;
    ErtsYieldingTimeoutState yield;
    ErtsTWheelTimer service_timer;
};

static ERTS_INLINE int
refn_is_lt(Uint32 *x, Uint32 *y)
{
    /* !0 if x < y */
    if (x[2] < y[2])
	return 1;
    if (x[2] != y[2])
	return 0;
    if (x[1] < y[1])
	return 1;
    if (x[1] != y[1])
	return 0;
    return x[0] < y[0];
}

static ERTS_INLINE int
refn_is_eq(Uint32 *x, Uint32 *y)
{
    return (x[0] == y[0]) & (x[1] == y[1]) & (x[2] == y[2]);
}

#define ERTS_RBT_PREFIX time
#define ERTS_RBT_T ErtsHLTimer
#define ERTS_RBT_KEY_T ErtsMonotonicTime
#define ERTS_RBT_FLAGS_T UWord
#define ERTS_RBT_INIT_EMPTY_TNODE(T)					\
    do {								\
	(T)->time.tree.parent = (UWord) NULL;				\
	(T)->time.tree.u.t.right = NULL;				\
	(T)->time.tree.u.t.left = NULL;					\
    } while (0)
#define ERTS_RBT_IS_RED(T)						\
    ((int) ((T)->time.tree.parent & ERTS_HLT_PFLG_RED))
#define ERTS_RBT_SET_RED(T)						\
    ((T)->time.tree.parent |= ERTS_HLT_PFLG_RED)
#define ERTS_RBT_IS_BLACK(T)						\
    (!ERTS_RBT_IS_RED((T)))
#define ERTS_RBT_SET_BLACK(T)						\
    ((T)->time.tree.parent &= ~ERTS_HLT_PFLG_RED)
#define ERTS_RBT_GET_FLAGS(T)						\
    ((T)->time.tree.parent & ERTS_HLT_PFLGS_MASK)
#define ERTS_RBT_SET_FLAGS(T, F)					\
    do {								\
	ERTS_HLT_ASSERT((((UWord) (F)) & ~ERTS_HLT_PFLGS_MASK) == 0);	\
	(T)->time.tree.parent &= ~ERTS_HLT_PFLGS_MASK;			\
	(T)->time.tree.parent |= (F);					\
    } while (0)
#define ERTS_RBT_GET_PARENT(T)						\
    ((ErtsHLTimer *) ((T)->time.tree.parent & ~ERTS_HLT_PFLGS_MASK))
#define ERTS_RBT_SET_PARENT(T, P)					\
    do {								\
	ERTS_HLT_ASSERT((((UWord) (P)) & ERTS_HLT_PFLGS_MASK) == 0);	\
	(T)->time.tree.parent &= ERTS_HLT_PFLGS_MASK;			\
	(T)->time.tree.parent |= (UWord) (P);				\
    } while (0)
#define ERTS_RBT_GET_RIGHT(T) ((T)->time.tree.u.t.right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->time.tree.u.t.right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->time.tree.u.t.left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->time.tree.u.t.left = (L))
#define ERTS_RBT_GET_KEY(T) ((T)->timeout)
#define ERTS_RBT_IS_LT(KX, KY) ((KX) < (KY))
#define ERTS_RBT_IS_EQ(KX, KY) ((KX) == (KY))
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_SMALLEST
#define ERTS_RBT_WANT_LOOKUP_INSERT
#define ERTS_RBT_WANT_REPLACE
#define ERTS_RBT_WANT_FOREACH
#ifdef ERTS_HLT_HARD_DEBUG
#  define ERTS_RBT_WANT_LOOKUP
#endif
#define ERTS_RBT_UNDEF

#include "erl_rbtree.h"

/* Use circular list for timers at same time */

static ERTS_INLINE void
same_time_list_insert(ErtsHLTimer **root, ErtsHLTimer *tmr)
{
    ErtsHLTimer *first = *root;
    if (!first) {
	ERTS_HLT_ASSERT((((UWord) root) & ERTS_HLT_PFLG_SAME_TIME) == 0);
	tmr->time.tree.parent = ((UWord) root) | ERTS_HLT_PFLG_SAME_TIME;
	tmr->time.tree.u.l.next = tmr;
	tmr->time.tree.u.l.prev = tmr;
	*root = tmr;
    }
    else {
	tmr->time.tree.parent = ERTS_HLT_PFLG_SAME_TIME;
	tmr->time.tree.u.l.next = first;
	tmr->time.tree.u.l.prev = first->time.tree.u.l.prev;
	first->time.tree.u.l.prev = tmr;
	tmr->time.tree.u.l.prev->time.tree.u.l.next = tmr;
    }
}

static ERTS_INLINE void
same_time_list_delete(ErtsHLTimer *tmr)
{
    ErtsHLTimer **root, *next;
    
    root = (ErtsHLTimer **) (tmr->time.tree.parent & ~ERTS_HLT_PFLG_SAME_TIME);
    next = tmr->time.tree.u.l.next;

    ERTS_HLT_ASSERT((tmr->time.tree.parent
		     == (((UWord) root) | ERTS_HLT_PFLG_SAME_TIME))
		    || (tmr->time.tree.parent
			== ERTS_HLT_PFLG_SAME_TIME));

    if (next == tmr) {
	ERTS_HLT_ASSERT(root && *root == tmr);
	ERTS_HLT_ASSERT(tmr->time.tree.u.l.prev == tmr);
	*root = NULL;
    }
    else {
	if (root) {
	    ERTS_HLT_ASSERT(*root == tmr);
	    *root = next;
	    next->time.tree.parent = ((UWord) root) | ERTS_HLT_PFLG_SAME_TIME;
	}
	tmr->time.tree.u.l.next->time.tree.u.l.prev = tmr->time.tree.u.l.prev;
	tmr->time.tree.u.l.prev->time.tree.u.l.next = next;
    }
}

static ERTS_INLINE void
same_time_list_new_root(ErtsHLTimer **root)
{
    ErtsHLTimer *tmr = *root;
    if (tmr) {
	ERTS_HLT_ASSERT(root);
	tmr->time.tree.parent = ((UWord) root) | ERTS_HLT_PFLG_SAME_TIME;
    }
}

static ERTS_INLINE int
same_time_list_foreach_destroy_yielding(ErtsHLTimer **root,
					void (*op)(ErtsHLTimer *, void *),
					void *arg,
					ERTS_TMR_TIMEOUT_YIELD_STATE_T *ys,
					Sint ylimit)
{
    Sint ycnt = ylimit;
    ErtsHLTimer *end, *tmr = *root;
    if (!tmr)
	return 0;

    ERTS_HLT_ASSERT(tmr->time.tree.parent
		    == (((UWord) root) | ERTS_HLT_PFLG_SAME_TIME));

    end = tmr->time.tree.u.l.prev;
    end->time.tree.u.l.next = NULL;

    while (1) {
	ErtsHLTimer *op_tmr = tmr;

	ERTS_HLT_ASSERT((tmr->time.tree.parent
			 == (((UWord) root) | ERTS_HLT_PFLG_SAME_TIME))
			|| (tmr->time.tree.parent
			    == ERTS_HLT_PFLG_SAME_TIME));

	tmr = tmr->time.tree.u.l.next;
	(*op)(op_tmr, arg);
	if (!tmr) {
	    *root = NULL;
	    return 0;
	}
	if (--ycnt <= 0) {
	    /* Make new circle of timers left to process... */
	    *root = tmr;
	    end->time.tree.u.l.next = tmr;
	    tmr->time.tree.u.l.prev = end;
	    tmr->time.tree.parent = ((UWord) root) | ERTS_HLT_PFLG_SAME_TIME;
	    return 1;
	}
    }    
}

static ERTS_INLINE void
same_time_list_foreach(ErtsHLTimer *root,
		       void (*op)(ErtsHLTimer *, void *),
		       void *arg)
{
    if (root) {
	ErtsHLTimer *tmr = root;
	do {
	    (*op)(tmr, arg);
	    tmr = tmr->time.tree.u.l.next;
	} while (root != tmr);
    }
}

#ifdef ERTS_HLT_HARD_DEBUG

static ERTS_INLINE ErtsHLTimer *
same_time_list_lookup(ErtsHLTimer *root, ErtsHLTimer *x)
{
    if (root) {
	ErtsHLTimer *tmr = root;
	do {
	    if (tmr == x)
		return tmr;
	    tmr = tmr->time.tree.u.l.next;
	} while (root != tmr);
    }
    return NULL;
}

#endif /* ERTS_HLT_HARD_DEBUG */

#define ERTS_BTM_HLT2REFN(T) ((T)->btm.refn)

#define ERTS_RBT_PREFIX btm
#define ERTS_RBT_T ErtsBifTimer
#define ERTS_RBT_KEY_T Uint32 *
#define ERTS_RBT_FLAGS_T UWord
#define ERTS_RBT_INIT_EMPTY_TNODE(T)					\
    do {								\
	(T)->btm.tree.parent = (UWord) NULL;				\
	(T)->btm.tree.right = NULL;					\
	(T)->btm.tree.left = NULL;					\
    } while (0)
#define ERTS_RBT_IS_RED(T)						\
    ((int) ((T)->btm.tree.parent & ERTS_HLT_PFLG_RED))
#define ERTS_RBT_SET_RED(T)						\
    ((T)->btm.tree.parent |= ERTS_HLT_PFLG_RED)
#define ERTS_RBT_IS_BLACK(T)						\
    (!ERTS_RBT_IS_RED((T)))
#define ERTS_RBT_SET_BLACK(T)						\
    ((T)->btm.tree.parent &= ~ERTS_HLT_PFLG_RED)
#define ERTS_RBT_GET_FLAGS(T)						\
    ((T)->btm.tree.parent & ERTS_HLT_PFLGS_MASK)
#define ERTS_RBT_SET_FLAGS(T, F)					\
    do {								\
	ERTS_HLT_ASSERT((((UWord) (F)) & ~ERTS_HLT_PFLGS_MASK) == 0);	\
	(T)->btm.tree.parent &= ~ERTS_HLT_PFLGS_MASK;			\
	(T)->btm.tree.parent |= (F);					\
    } while (0)
#define ERTS_RBT_GET_PARENT(T)						\
    ((ErtsBifTimer *) ((T)->btm.tree.parent & ~ERTS_HLT_PFLGS_MASK))
#define ERTS_RBT_SET_PARENT(T, P)					\
    do {								\
	ERTS_HLT_ASSERT((((UWord) (P)) & ERTS_HLT_PFLGS_MASK) == 0);	\
	(T)->btm.tree.parent &= ERTS_HLT_PFLGS_MASK;			\
	(T)->btm.tree.parent |= (UWord) (P);				\
    } while (0)
#define ERTS_RBT_GET_RIGHT(T) ((T)->btm.tree.right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->btm.tree.right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->btm.tree.left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->btm.tree.left = (L))
#define ERTS_RBT_GET_KEY(T) ERTS_BTM_HLT2REFN((T))
#define ERTS_RBT_IS_LT(KX, KY) refn_is_lt((KX), (KY))
#define ERTS_RBT_IS_EQ(KX, KY) refn_is_eq((KX), (KY))
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_INSERT
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_WANT_FOREACH
#define ERTS_RBT_UNDEF

#include "erl_rbtree.h"

#define ERTS_RBT_PREFIX proc_btm
#define ERTS_RBT_T ErtsBifTimer
#define ERTS_RBT_KEY_T Uint32 *
#define ERTS_RBT_FLAGS_T UWord
#define ERTS_RBT_INIT_EMPTY_TNODE(T)					\
    do {								\
	(T)->btm.proc_tree.parent = (UWord) NULL;			\
	(T)->btm.proc_tree.right = NULL;				\
	(T)->btm.proc_tree.left = NULL;					\
    } while (0)
#define ERTS_RBT_IS_RED(T)						\
    ((int) ((T)->btm.proc_tree.parent & ERTS_HLT_PFLG_RED))
#define ERTS_RBT_SET_RED(T)						\
    ((T)->btm.proc_tree.parent |= ERTS_HLT_PFLG_RED)
#define ERTS_RBT_IS_BLACK(T)						\
    (!ERTS_RBT_IS_RED((T)))
#define ERTS_RBT_SET_BLACK(T)						\
    ((T)->btm.proc_tree.parent &= ~ERTS_HLT_PFLG_RED)
#define ERTS_RBT_GET_FLAGS(T)						\
    ((T)->btm.proc_tree.parent & ERTS_HLT_PFLGS_MASK)
#define ERTS_RBT_SET_FLAGS(T, F)					\
    do {								\
	ERTS_HLT_ASSERT((((UWord) (F)) & ~ERTS_HLT_PFLGS_MASK) == 0);	\
	(T)->btm.proc_tree.parent &= ~ERTS_HLT_PFLGS_MASK;		\
	(T)->btm.proc_tree.parent |= (F);				\
    } while (0)
#define ERTS_RBT_GET_PARENT(T)						\
    ((ErtsBifTimer *) ((T)->btm.proc_tree.parent & ~ERTS_HLT_PFLGS_MASK))
#define ERTS_RBT_SET_PARENT(T, P)					\
    do {								\
	ERTS_HLT_ASSERT((((UWord) (P)) & ERTS_HLT_PFLGS_MASK) == 0);	\
	(T)->btm.proc_tree.parent &= ERTS_HLT_PFLGS_MASK;		\
	(T)->btm.proc_tree.parent |= (UWord) (P);			\
    } while (0)
#define ERTS_RBT_GET_RIGHT(T) ((T)->btm.proc_tree.right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->btm.proc_tree.right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->btm.proc_tree.left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->btm.proc_tree.left = (L))
#define ERTS_RBT_GET_KEY(T) ERTS_BTM_HLT2REFN((T))
#define ERTS_RBT_IS_LT(KX, KY) refn_is_lt((KX), (KY))
#define ERTS_RBT_IS_EQ(KX, KY) refn_is_eq((KX), (KY))
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_INSERT
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_WANT_FOREACH_DESTROY_YIELDING
#define ERTS_RBT_UNDEF

#include "erl_rbtree.h"

static void init_canceled_queue(ErtsHLTCncldTmrQ *cq);

void
erts_hl_timer_init(void)
{
    init_tw_timer_alloc();
    init_bif_timer_pre_alloc();
}

ErtsHLTimerService *
erts_create_timer_service(void)
{
    ErtsYieldingTimeoutState init_yield = ERTS_TMR_YIELDING_TIMEOUT_STATE_INITER;
    ErtsHLTimerService *srv;

    srv = erts_alloc_permanent_cache_aligned(ERTS_ALC_T_TIMER_SERVICE,
					     sizeof(ErtsHLTimerService));
    srv->time_tree = NULL;
    srv->btm_tree = NULL;
    srv->next_timeout = NULL;
    srv->yield = init_yield;
    erts_twheel_init_timer(&srv->service_timer);

    init_canceled_queue(&srv->canceled_queue);

    return srv;
}

size_t
erts_timer_type_size(ErtsAlcType_t type)
{
    switch (type) {
    case ERTS_ALC_T_LL_PTIMER: return sizeof(ErtsTWTimer);
    case ERTS_ALC_T_HL_PTIMER: return sizeof(ErtsHLTimer);
    case ERTS_ALC_T_BIF_TIMER: return sizeof(ErtsBifTimer);
    default: ERTS_INTERNAL_ERROR("Unknown type");
    }
    return 0;
}

static ERTS_INLINE ErtsMonotonicTime
get_timeout_pos(ErtsMonotonicTime now, ErtsMonotonicTime msec)
{
    ErtsMonotonicTime timeout_pos;
    if (msec <= 0)
	return ERTS_MONOTONIC_TO_CLKTCKS(now);
    timeout_pos = ERTS_MONOTONIC_TO_CLKTCKS(now-1);
    timeout_pos += ERTS_MSEC_TO_CLKTCKS(msec) + 1;
    return timeout_pos;
}

static ERTS_INLINE Sint64
get_time_left(ErtsSchedulerData *esdp, ErtsMonotonicTime timeout_pos)
{
    ErtsMonotonicTime now = erts_get_monotonic_time(esdp);

    now = ERTS_MONOTONIC_TO_CLKTCKS(now-1)+1;
    if (timeout_pos <= now)
	return (Sint64) 0;
    return (Sint64) ERTS_CLKTCKS_TO_MSEC(timeout_pos - now);
}

static ERTS_INLINE int
proc_timeout_common(Process *proc, void *tmr)
{
    if (tmr == (void *) erts_atomic_cmpxchg_mb(&proc->common.timer,
						   ERTS_PTMR_TIMEDOUT,
						   (erts_aint_t) tmr)) {
	erts_aint32_t state;
	erts_proc_lock(proc, ERTS_PROC_LOCKS_MSG_RECEIVE);
	state = erts_atomic32_read_acqb(&proc->state);
	erts_proc_unlock(proc, ERTS_PROC_LOCKS_MSG_RECEIVE);
	if (!(state & (ERTS_PSFLG_ACTIVE|ERTS_PSFLG_EXITING)))
	    erts_schedule_process(proc, state, 0);
	return 1;
    }
    return 0;
}

static ERTS_INLINE int
port_timeout_common(Port *port, void *tmr)
{
    if (tmr == (void *) erts_atomic_cmpxchg_mb(&port->common.timer,
						   ERTS_PTMR_TIMEDOUT,
						   (erts_aint_t) tmr)) {
	erts_port_task_schedule(port->common.id,
				&port->timeout_task,
				ERTS_PORT_TASK_TIMEOUT);
	return 1;
    }
    return 0;
}

static ERTS_INLINE erts_aint_t
init_btm_specifics(ErtsSchedulerData *esdp,
                   ErtsBifTimer *tmr, Eterm msg,
                   Uint32 *refn
    )
{
    Uint hsz = is_immed(msg) ? ((Uint) 0) : size_object(msg);
    int refc;
    if (!hsz) {
        tmr->btm.message = msg;
        tmr->btm.bp = NULL;
    }
    else {
        ErlHeapFragment *bp = new_message_buffer(hsz);
        Eterm *hp = bp->mem;
        tmr->btm.message = copy_struct(msg, hsz, &hp, &bp->off_heap);
        tmr->btm.bp = bp;
    }
    refc = 0;
    tmr->btm.refn[0] = refn[0];
    tmr->btm.refn[1] = refn[1];
    tmr->btm.refn[2] = refn[2];

    tmr->btm.proc_tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;

    btm_rbt_insert(&esdp->timer_service->btm_tree, tmr);

    erts_atomic32_init_nob(&tmr->btm.state, ERTS_TMR_STATE_ACTIVE);
    return refc; /* refc from magic binary... */
}

static void tw_bif_timer_timeout(void *vbtmp);

static ERTS_INLINE void
timer_destroy(ErtsTimer *tmr, int twt, int btm)
{
    if (!btm) {
        if (twt)
            tw_timer_free(&tmr->twt);
        else
            erts_free(ERTS_ALC_T_HL_PTIMER, tmr);
    }
    else {
	if (tmr->head.roflgs & ERTS_TMR_ROFLG_PRE_ALC)
	    bif_timer_pre_free(&tmr->btm);
	else
	    erts_free(ERTS_ALC_T_BIF_TIMER, &tmr->btm);
    }
}

static ERTS_INLINE void
timer_pre_dec_refc(ErtsTimer *tmr)
{
#ifdef ERTS_HLT_DEBUG
    erts_aint_t refc;
    refc = erts_atomic32_dec_read_nob(&tmr->head.refc);
    ERTS_HLT_ASSERT(refc > 0);
#else
    erts_atomic32_dec_nob(&tmr->head.refc);
#endif
}

/*
 * Basic timer wheel timer stuff
 */

static void
scheduled_tw_timer_destroy(void *vtmr)
{
    ErtsTimer * tmr = (ErtsTimer *) vtmr;
    int btm = !!(tmr->head.roflgs & ERTS_TMR_ROFLG_BIF_TMR);
    timer_destroy((ErtsTimer *) vtmr, 1, btm);
}

static void
schedule_tw_timer_destroy(ErtsTWTimer *tmr)
{
    Uint size;
    /*
     * Reference to process/port can be
     * dropped at once...
     */
    if (tmr->head.roflgs & ERTS_TMR_ROFLG_PROC)
	erts_proc_dec_refc(tmr->head.receiver.proc);
    else if (tmr->head.roflgs & ERTS_TMR_ROFLG_PORT)
	erts_port_dec_refc(tmr->head.receiver.port);

    if (!(tmr->head.roflgs & ERTS_TMR_ROFLG_BIF_TMR))
	size = sizeof(ErtsHLTimer);
    else {
	/* Message buffer already dropped... */
	size = sizeof(ErtsBifTimer);
    }

    erts_schedule_thr_prgr_later_cleanup_op(
	scheduled_tw_timer_destroy,
	(void *) tmr,
	&tmr->u.cleanup,
	size);
}

static ERTS_INLINE void
tw_timer_dec_refc(ErtsTWTimer *tmr)
{
    if (erts_atomic32_dec_read_relb(&tmr->head.refc) == 0) {
        ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
	schedule_tw_timer_destroy(tmr);
    }
}

static void
tw_proc_timeout(void *vtwtp)
{
    ErtsTWTimer *twtp = (ErtsTWTimer *) vtwtp;
    Process *proc = twtp->head.receiver.proc;
    if (proc_timeout_common(proc, vtwtp))
	tw_timer_dec_refc(twtp);
    tw_timer_dec_refc(twtp);
}

static void
tw_port_timeout(void *vtwtp)
{
    ErtsTWTimer *twtp = (ErtsTWTimer *) vtwtp;
    Port *port = twtp->head.receiver.port;
    if (port_timeout_common(port, vtwtp))
	tw_timer_dec_refc(twtp);
    tw_timer_dec_refc(twtp);
}

static void
cancel_tw_timer(ErtsSchedulerData *esdp, ErtsTWTimer *tmr)
{
    ERTS_HLT_ASSERT((tmr->head.roflgs & ERTS_TMR_ROFLG_SID_MASK)
		    == (Uint32) esdp->no);
    erts_twheel_cancel_timer(esdp->timer_wheel, &tmr->u.tw_tmr);
    tw_timer_dec_refc(tmr);
}

static void
tw_callback_timeout(void *vtwtp)
{
    ErtsTWTimer *twtp = (ErtsTWTimer *) vtwtp;
    void (*callback)(void *) = twtp->head.receiver.callback;
    void *arg = twtp->head.u.arg;
    tw_timer_dec_refc(twtp);
    (*callback)(arg);
}

static ErtsTimer *
create_tw_timer(ErtsSchedulerData *esdp,
		ErtsMonotonicTime timeout_pos,
		int short_time, ErtsTmrType type,
		void *rcvrp, Eterm rcvr,
		Eterm msg,
                Uint32 *refn,
		void (*callback)(void *), void *arg)
{
    ErtsTWTimer *tmr;
    void (*timeout_func)(void *);
    erts_aint32_t refc;

    if (type != ERTS_TMR_BIF) {
        tmr = tw_timer_alloc();
        tmr->head.roflgs = 0;
    }
    else {
        if (short_time) {
            tmr = (ErtsTWTimer *) bif_timer_pre_alloc();
            if (!tmr)
                goto alloc_bif_timer;
            tmr->head.roflgs = (ERTS_TMR_ROFLG_BIF_TMR
                                | ERTS_TMR_ROFLG_PRE_ALC);
        }
        else {
        alloc_bif_timer:
            tmr = (ErtsTWTimer *) erts_alloc(ERTS_ALC_T_BIF_TIMER,
                                             sizeof(ErtsBifTimer));
            tmr->head.roflgs = ERTS_TMR_ROFLG_BIF_TMR;
        }
    }

    erts_twheel_init_timer(&tmr->u.tw_tmr);
    tmr->head.roflgs |= (Uint32) esdp->no;
    ERTS_HLT_ASSERT((((Uint32) esdp->no)
                     & ~ERTS_TMR_ROFLG_SID_MASK) == 0);

    switch (type) {

    case ERTS_TMR_PROC:
	tmr->head.receiver.proc = (Process *) rcvrp;
	tmr->head.roflgs |= ERTS_TMR_ROFLG_PROC;
	timeout_func = tw_proc_timeout;
	erts_proc_inc_refc((Process *) rcvrp);
	refc = 2;
	break;

    case ERTS_TMR_PORT:
	tmr->head.receiver.port = (Port *) rcvrp;
	tmr->head.roflgs |= ERTS_TMR_ROFLG_PORT;
	timeout_func = tw_port_timeout;
	erts_port_inc_refc((Port *) rcvrp);
	refc = 2;
	break;

    case ERTS_TMR_CALLBACK:
	tmr->head.u.arg = arg;
	tmr->head.receiver.callback = callback;

	tmr->head.roflgs |= ERTS_TMR_ROFLG_CALLBACK;
	timeout_func = tw_callback_timeout;
	refc = 1;
	break;

    case ERTS_TMR_BIF:

	timeout_func = tw_bif_timer_timeout;
	if (is_internal_pid(rcvr)) {
	    tmr->head.roflgs |= ERTS_TMR_ROFLG_PROC;
	    tmr->head.receiver.proc = (Process *) rcvrp;
	    refc = 2;
	}
	else {
	    ERTS_HLT_ASSERT(is_atom(rcvr));
	    tmr->head.roflgs |= ERTS_TMR_ROFLG_REG_NAME;
	    tmr->head.receiver.name = (Eterm) rcvr;
	    refc = 1;
	}

        refc += init_btm_specifics(esdp,
                                   (ErtsBifTimer *) tmr,
                                   msg,
                                   refn
            );
        break;

    default:
	ERTS_INTERNAL_ERROR("Unsupported timer type");
	return NULL;
    }

    erts_atomic32_init_nob(&tmr->head.refc, refc);

    erts_twheel_set_timer(esdp->timer_wheel,
			  &tmr->u.tw_tmr,
			  timeout_func,
			  tmr,
			  timeout_pos);

    return (ErtsTimer *) tmr;
}

/*
 * Basic high level timer stuff
 */

static void
scheduled_hl_timer_destroy(void *vtmr)
{
    ErtsTimer * tmr = (ErtsTimer *) vtmr;
    int btm = !!(tmr->head.roflgs & ERTS_TMR_ROFLG_BIF_TMR);
    timer_destroy((ErtsTimer *) vtmr, 0, btm);
}

static void
schedule_hl_timer_destroy(ErtsHLTimer *tmr, Uint32 roflgs)
{
    UWord size;

    /*
     * Reference to process/port can be dropped
     * at once...
     */

    ERTS_HLT_ASSERT(erts_atomic32_read_nob(&tmr->head.refc) == 0);

    if (roflgs & ERTS_TMR_ROFLG_REG_NAME) {
	ERTS_HLT_ASSERT(is_atom(tmr->head.receiver.name));
    }
    else if (roflgs & ERTS_TMR_ROFLG_PROC) {
	ERTS_HLT_ASSERT(tmr->head.receiver.proc);
	erts_proc_dec_refc(tmr->head.receiver.proc);
    }
    else if (roflgs & ERTS_TMR_ROFLG_PORT) {
	ERTS_HLT_ASSERT(tmr->head.receiver.port);
	erts_port_dec_refc(tmr->head.receiver.port);
    }

    if (!(roflgs & ERTS_TMR_ROFLG_BIF_TMR))
	size = sizeof(ErtsHLTimer);
    else {
	/* Message buffer already dropped... */
	size = sizeof(ErtsBifTimer);
    }

    erts_schedule_thr_prgr_later_cleanup_op(
	scheduled_hl_timer_destroy, tmr,
	&tmr->time.cleanup, size);
}

static ERTS_INLINE void
hl_timer_dec_refc(ErtsHLTimer *tmr, Uint32 roflgs)
{
    if (erts_atomic32_dec_read_relb(&tmr->head.refc) == 0) {
        ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);
	schedule_hl_timer_destroy(tmr, roflgs);
    }
}

static void hlt_service_timeout(void *vesdp);
static void handle_canceled_queue(ErtsSchedulerData *esdp,
				  ErtsHLTCncldTmrQ *cq,
				  int use_limit,
				  int ops_limit,
				  int *need_thr_progress,
				  ErtsThrPrgrVal *thr_prgr_p,
				  int *need_more_work);

static ERTS_INLINE void
check_canceled_queue(ErtsSchedulerData *esdp, ErtsHLTimerService *srv)
{
#if ERTS_TMR_CHECK_CANCEL_ON_CREATE
    ErtsHLTCncldTmrQ *cq = &srv->canceled_queue;
    if (cq->head.first != cq->head.unref_end)
	handle_canceled_queue(esdp, cq, 1,
			      ERTS_TMR_CANCELED_TIMER_SMALL_LIMIT,
			      NULL, NULL, NULL);
#endif
}

static ERTS_INLINE void
bif_timer_timeout(ErtsHLTimerService *srv,
                  ErtsBifTimer *tmr,
                  Uint32 roflgs)
{
    erts_aint32_t state;

    ERTS_HLT_ASSERT(tmr->type.head.roflgs == roflgs);    
    ERTS_HLT_ASSERT(roflgs & ERTS_TMR_ROFLG_BIF_TMR);

    state = erts_atomic32_cmpxchg_acqb(&tmr->btm.state,
					   ERTS_TMR_STATE_TIMED_OUT,
					   ERTS_TMR_STATE_ACTIVE);

    ERTS_HLT_ASSERT(state == ERTS_TMR_STATE_CANCELED
		    || state == ERTS_TMR_STATE_ACTIVE);

    if (state == ERTS_TMR_STATE_ACTIVE) {
        Process *proc;

        if (roflgs & ERTS_TMR_ROFLG_REG_NAME) {
            Eterm term;
            term = tmr->type.head.receiver.name;
            ERTS_HLT_ASSERT(is_atom(term));
            term = erts_whereis_name_to_id(NULL, term);
            proc = erts_proc_lookup(term);
        }
        else {
            ERTS_HLT_ASSERT(roflgs & ERTS_TMR_ROFLG_PROC);
            proc = tmr->type.head.receiver.proc;
            ERTS_HLT_ASSERT(proc);
        }
        if (proc) {
            int dec_refc = 0;
            ErtsMessage *mp = erts_alloc_message(0, NULL);
            mp->data.heap_frag = tmr->btm.bp;
            tmr->btm.bp = NULL;
            erts_queue_message(proc, 0, mp, tmr->btm.message,
                               am_clock_service);
            erts_proc_lock(proc, ERTS_PROC_LOCK_BTM);
            /* If the process is exiting do not disturb the cleanup... */
            if (!ERTS_PROC_IS_EXITING(proc)) {
                if (tmr->btm.proc_tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE) {
                    proc_btm_rbt_delete(&proc->bif_timers, tmr);
                    tmr->btm.proc_tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
                    dec_refc = 1;
                }
            }
            erts_proc_unlock(proc, ERTS_PROC_LOCK_BTM);
            if (dec_refc)
                timer_pre_dec_refc((ErtsTimer *) tmr);
        }
        if (tmr->btm.bp)
            free_message_buffer(tmr->btm.bp);
    }

    if (tmr->btm.tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE) {
	btm_rbt_delete(&srv->btm_tree, tmr);
	tmr->btm.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
    }

}

static void
tw_bif_timer_timeout(void *vbtmp)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsHLTimerService *srv = esdp->timer_service;
    ErtsBifTimer *btmp = (ErtsBifTimer *) vbtmp;
    bif_timer_timeout(srv, btmp, btmp->type.head.roflgs);
    tw_timer_dec_refc(&btmp->type.twt);
}

static ErtsTimer *
create_hl_timer(ErtsSchedulerData *esdp,
		ErtsMonotonicTime timeout_pos,
		int short_time, ErtsTmrType type,
		void *rcvrp, Eterm rcvr,
		Eterm msg,
                Uint32 *refn,
		void (*callback)(void *), void *arg)
{
    ErtsHLTimerService *srv = esdp->timer_service;
    ErtsHLTimer *tmr, *st_tmr;
    erts_aint32_t refc;
    Uint32 roflgs;

    ERTS_HLT_HDBG_CHK_SRV(srv);

    check_canceled_queue(esdp, srv);

    ERTS_HLT_ASSERT((esdp->no & ~ERTS_TMR_ROFLG_SID_MASK) == 0);

    roflgs = ((Uint32) esdp->no) | ERTS_TMR_ROFLG_HLT;

    if (type != ERTS_TMR_BIF) {

	tmr = erts_alloc(ERTS_ALC_T_HL_PTIMER,
                         sizeof(ErtsHLTimer));
	tmr->timeout = timeout_pos;

	switch (type) {

	case ERTS_TMR_PROC:
	    ERTS_HLT_ASSERT(is_internal_pid(rcvr));

	    erts_proc_inc_refc((Process *) rcvrp);
	    tmr->head.receiver.proc = (Process *) rcvrp;
	    roflgs |= ERTS_TMR_ROFLG_PROC;
	    refc = 2;
	    break;

	case ERTS_TMR_PORT:
	    ERTS_HLT_ASSERT(is_internal_port(rcvr));
	    erts_port_inc_refc((Port *) rcvrp);
	    tmr->head.receiver.port = (Port *) rcvrp;
	    roflgs |= ERTS_TMR_ROFLG_PORT;
	    refc = 2;
	    break;

	case ERTS_TMR_CALLBACK:
	    roflgs |= ERTS_TMR_ROFLG_CALLBACK;
	    tmr->head.receiver.callback = callback;
	    tmr->head.u.arg = arg;
	    refc = 1;
	    break;

	default:
	    ERTS_INTERNAL_ERROR("Unsupported timer type");
	    return NULL;
	}

    }
    else { /* ERTS_TMR_BIF */

	if (short_time) {
	    tmr = (ErtsHLTimer *) bif_timer_pre_alloc();
	    if (!tmr)
		goto alloc_bif_timer;
	    roflgs |= ERTS_TMR_ROFLG_PRE_ALC;
	}
	else {
	alloc_bif_timer:
            tmr = (ErtsHLTimer *) erts_alloc(ERTS_ALC_T_BIF_TIMER,
                                             sizeof(ErtsBifTimer));
        }

	tmr->timeout = timeout_pos;

	roflgs |= ERTS_TMR_ROFLG_BIF_TMR;
	if (is_internal_pid(rcvr)) {
	    roflgs |= ERTS_TMR_ROFLG_PROC;
	    tmr->head.receiver.proc = (Process *) rcvrp;
	    refc = 2;
	}
	else {
	    ERTS_HLT_ASSERT(is_atom(rcvr));
	    roflgs |= ERTS_TMR_ROFLG_REG_NAME;
	    tmr->head.receiver.name = rcvr;
	    refc = 1;
	}

        refc += init_btm_specifics(esdp,
                                   (ErtsBifTimer *) tmr,
                                   msg,
                                   refn
            );
    }

    tmr->head.roflgs = roflgs;
    erts_atomic32_init_nob(&tmr->head.refc, refc);

    if (!srv->next_timeout
	|| tmr->timeout < srv->next_timeout->timeout) {
	if (srv->next_timeout)
	    erts_twheel_cancel_timer(esdp->timer_wheel,
				     &srv->service_timer);
	erts_twheel_set_timer(esdp->timer_wheel,
			      &srv->service_timer,
			      hlt_service_timeout,
			      (void *) esdp,
			      tmr->timeout);
	srv->next_timeout = tmr;
    }

    st_tmr = time_rbt_lookup_insert(&srv->time_tree, tmr);
    tmr->time.tree.same_time = st_tmr;
    if (st_tmr)
	same_time_list_insert(&st_tmr->time.tree.same_time, tmr);

#ifdef ERTS_HLT_HARD_DEBUG
    tmr->pending_timeout = 0;
#endif

    ERTS_HLT_HDBG_CHK_SRV(srv);

    return (ErtsTimer *) tmr;
}

static ERTS_INLINE void
hlt_proc_timeout(ErtsHLTimer *tmr)
{
    if (proc_timeout_common(tmr->head.receiver.proc, (void *) tmr))
	hl_timer_dec_refc(tmr, tmr->head.roflgs);
}

static ERTS_INLINE void
hlt_port_timeout(ErtsHLTimer *tmr)
{
    if (port_timeout_common(tmr->head.receiver.port, (void *) tmr))
	hl_timer_dec_refc(tmr, tmr->head.roflgs);
}

static void hlt_timeout(ErtsHLTimer *tmr, void *vsrv)
{
    ErtsHLTimerService *srv = (ErtsHLTimerService *) vsrv;
    Uint32 roflgs;

    ERTS_HLT_HDBG_CHK_SRV(srv);

    roflgs = tmr->head.roflgs;
    ERTS_HLT_ASSERT(roflgs & ERTS_TMR_ROFLG_HLT);

    if (roflgs & ERTS_TMR_ROFLG_BIF_TMR)
        bif_timer_timeout(srv, (ErtsBifTimer *) tmr, roflgs);
    else if (roflgs & ERTS_TMR_ROFLG_PROC)
        hlt_proc_timeout(tmr);
    else if (roflgs & ERTS_TMR_ROFLG_PORT)
        hlt_port_timeout(tmr);
    else {
        ERTS_HLT_ASSERT(roflgs & ERTS_TMR_ROFLG_CALLBACK);
        (*tmr->head.receiver.callback)(tmr->head.u.arg);
    }

    tmr->time.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;

    ERTS_HLT_HDBG_CHK_SRV(srv);

    hl_timer_dec_refc(tmr, roflgs);
}

#ifdef ERTS_HLT_HARD_DEBUG
static void
set_pending_timeout(ErtsHLTimer *tmr, void *unused)
{
    tmr->pending_timeout = -1;
}
#endif

static void
hlt_service_timeout(void *vesdp)
{
    ErtsSchedulerData *esdp = (ErtsSchedulerData *) vesdp;
    ErtsHLTimerService *srv = esdp->timer_service;
    ErtsHLTimer *tmr = srv->next_timeout;
    int yield;

    ERTS_HLT_HDBG_CHK_SRV(srv);

    ERTS_HLT_ASSERT(esdp == erts_get_scheduler_data());

    ERTS_HLT_ASSERT(!srv->yield.root || srv->yield.root == tmr);
    ERTS_HLT_ASSERT(tmr);
    ERTS_HLT_ASSERT(tmr->timeout <= erts_get_monotonic_time(esdp));

    if (!srv->yield.root) {
	ERTS_HLT_ASSERT(tmr->time.tree.parent
			!= ERTS_HLT_PFIELD_NOT_IN_TABLE);
	time_rbt_delete(&srv->time_tree, tmr);
	tmr->time.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
#ifdef ERTS_HLT_HARD_DEBUG
	tmr->pending_timeout = 1;
	if (tmr->time.tree.same_time)
	    same_time_list_foreach(tmr->time.tree.same_time, set_pending_timeout, NULL);
#endif
    }

    if (!tmr->time.tree.same_time && !srv->yield.root)
	yield = 0;
    else {
	yield = same_time_list_foreach_destroy_yielding(
	    &tmr->time.tree.same_time, hlt_timeout, (void *) srv,
	    &srv->yield.state, ERTS_TMR_TIMEOUT_YIELD_LIMIT);
    }

    if (yield)
	srv->yield.root = tmr;
    else {
	srv->yield.root = NULL;
	hlt_timeout(tmr, (void *) srv);

	tmr = time_rbt_smallest(srv->time_tree);
	srv->next_timeout = tmr;
    }

    ERTS_HLT_HDBG_CHK_SRV(srv);

    if (tmr)
	erts_twheel_set_timer(esdp->timer_wheel,
			      &srv->service_timer,
			      hlt_service_timeout,
			      vesdp,
			      tmr->timeout);
}

static void
hlt_delete_timer(ErtsSchedulerData *esdp, ErtsHLTimer *tmr)
{
    ErtsHLTimerService *srv = esdp->timer_service;

    ERTS_HLT_HDBG_CHK_SRV(srv);

    if (tmr->time.tree.parent == ERTS_HLT_PFIELD_NOT_IN_TABLE) {
	/* Already removed... */
	ERTS_HLT_HDBG_CHK_SRV(srv);
	return;
    }

    if (tmr->time.tree.parent & ERTS_HLT_PFLG_SAME_TIME) {
	same_time_list_delete(tmr);
    }
    else if (tmr->time.tree.same_time) {
	ErtsHLTimer *st_container;

	ERTS_HLT_ASSERT((tmr->time.tree.parent & ERTS_HLT_PFLG_SAME_TIME) == 0);
	st_container = tmr->time.tree.same_time->time.tree.u.l.prev;

	ERTS_HLT_ASSERT(st_container);
	ERTS_HLT_ASSERT(st_container->time.tree.parent
			 & ERTS_HLT_PFLG_SAME_TIME);
	ERTS_HLT_ASSERT(tmr->timeout == st_container->timeout);

	same_time_list_delete(st_container);
	st_container->time.tree.same_time = tmr->time.tree.same_time;
	same_time_list_new_root(&st_container->time.tree.same_time);

	time_rbt_replace(&srv->time_tree, tmr, st_container);
	ERTS_HLT_ASSERT((st_container->time.tree.parent
			 & ERTS_HLT_PFLG_SAME_TIME) == 0);

	if (srv->next_timeout == tmr)
	    srv->next_timeout = st_container;
    }
    else {
	ERTS_HLT_ASSERT((tmr->time.tree.parent & ERTS_HLT_PFLG_SAME_TIME) == 0);
	time_rbt_delete(&srv->time_tree, tmr);
	if (tmr == srv->next_timeout) {
	    ErtsHLTimer *smlst;
	    erts_twheel_cancel_timer(esdp->timer_wheel,
				     &srv->service_timer);
	    smlst = time_rbt_smallest(srv->time_tree);
	    srv->next_timeout = smlst;
	    if (smlst) {
		ERTS_HLT_ASSERT(smlst->timeout > tmr->timeout);
		erts_twheel_set_timer(esdp->timer_wheel,
				      &srv->service_timer,
				      hlt_service_timeout,
				      (void *) esdp,
				      smlst->timeout);
	    }
	}
    }
    tmr->time.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;

    hl_timer_dec_refc(tmr, tmr->head.roflgs);

    ERTS_HLT_HDBG_CHK_SRV(srv);
}

/*
 * Pass canceled timers back to originating scheduler
 */

static ERTS_INLINE void
cleanup_sched_local_canceled_timer(ErtsSchedulerData *esdp,
				   ErtsTimer *tmr)
{
    Uint32 roflgs = tmr->head.roflgs;
    ERTS_HLT_ASSERT(esdp == erts_get_scheduler_data());
    ERTS_HLT_ASSERT((tmr->head.roflgs & ERTS_TMR_ROFLG_SID_MASK)
		    == (Uint32) esdp->no);

    if (roflgs & ERTS_TMR_ROFLG_BIF_TMR) {
        ErtsBifTimer *btm = (ErtsBifTimer *) tmr;
	if (btm->btm.tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE) {
	    btm_rbt_delete(&esdp->timer_service->btm_tree, btm);
	    btm->btm.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
	}
    }

    if (roflgs & ERTS_TMR_ROFLG_HLT) {
	hlt_delete_timer(esdp, &tmr->hlt);
	hl_timer_dec_refc(&tmr->hlt, roflgs);
    }
    else {
	cancel_tw_timer(esdp, &tmr->twt);
	tw_timer_dec_refc(&tmr->twt);
    }
}


static void
init_canceled_queue(ErtsHLTCncldTmrQ *cq)
{
    erts_atomic_init_nob(&cq->tail.data.marker.u.next, ERTS_AINT_NULL);
    erts_atomic_init_nob(&cq->tail.data.last,
			 (erts_aint_t) &cq->tail.data.marker);
    cq->head.first = (ErtsTimer *) &cq->tail.data.marker;
    cq->head.unref_end = (ErtsTimer *) &cq->tail.data.marker;
    cq->head.next.thr_progress = erts_thr_progress_current();
    cq->head.next.thr_progress_reached = 1;
    cq->head.next.unref_end = (ErtsTimer *) &cq->tail.data.marker;
    cq->head.used_marker = 1;
}

static ERTS_INLINE int
cq_enqueue(ErtsHLTCncldTmrQ *cq, ErtsTimer *tmr, int cinit)
{
    erts_aint_t itmp;
    ErtsTimer *enq, *this = tmr;

    erts_atomic_init_nob(&this->head.u.next, ERTS_AINT_NULL);
    /* Enqueue at end of list... */

    enq = (ErtsTimer *) erts_atomic_read_nob(&cq->tail.data.last);
    itmp = erts_atomic_cmpxchg_relb(&enq->head.u.next,
				    (erts_aint_t) this,
				    ERTS_AINT_NULL);
    if (itmp == ERTS_AINT_NULL) {
	/* We are required to move last pointer */
#ifdef DEBUG
	ASSERT(ERTS_AINT_NULL == erts_atomic_read_nob(&this->head.u.next));
	ASSERT(((erts_aint_t) enq)
	       == erts_atomic_xchg_relb(&cq->tail.data.last,
					(erts_aint_t) this));
#else
	erts_atomic_set_relb(&cq->tail.data.last, (erts_aint_t) this);
#endif
	return 1;
    }
    else {
	/*
	 * We *need* to insert element somewhere in between the
	 * last element we read earlier and the actual last element.
	 */
	int i = cinit;

	while (1) {
	    erts_aint_t itmp2;
	    erts_atomic_set_nob(&this->head.u.next, itmp);
	    itmp2 = erts_atomic_cmpxchg_relb(&enq->head.u.next,
					     (erts_aint_t) this,
					     itmp);
	    if (itmp == itmp2)
		return 0; /* inserted this */
	    if ((i & 1) == 0)
		itmp = itmp2;
	    else {
		enq = (ErtsTimer *) itmp2;
		itmp = erts_atomic_read_acqb(&enq->head.u.next);
		ASSERT(itmp != ERTS_AINT_NULL);
	    }
	    i++;
	}
    }
}

static ERTS_INLINE erts_aint_t
check_insert_marker(ErtsHLTCncldTmrQ *cq, erts_aint_t ilast)
{
    if (!cq->head.used_marker
	&& cq->head.unref_end == (ErtsTimer *) ilast) {
	erts_aint_t itmp;
	ErtsTimer *last = (ErtsTimer *) ilast;

	erts_atomic_init_nob(&cq->tail.data.marker.u.next, ERTS_AINT_NULL);
	itmp = erts_atomic_cmpxchg_relb(&last->head.u.next,
					(erts_aint_t) &cq->tail.data.marker,
					ERTS_AINT_NULL);
	if (itmp == ERTS_AINT_NULL) {
	    ilast = (erts_aint_t) &cq->tail.data.marker;
	    cq->head.used_marker = !0;
	    erts_atomic_set_relb(&cq->tail.data.last, ilast);
	}
    }
    return ilast;
}

static ERTS_INLINE ErtsTimer *
cq_dequeue(ErtsHLTCncldTmrQ *cq)
{
    ErtsTimer *tmr;

    if (cq->head.first == cq->head.unref_end)
	return NULL;

    tmr = cq->head.first;
    if (tmr == (ErtsTimer *) &cq->tail.data.marker) {
	ASSERT(cq->head.used_marker);
	cq->head.used_marker = 0;
	tmr = (ErtsTimer *) erts_atomic_read_nob(&tmr->head.u.next);
	if (tmr == cq->head.unref_end) {
	    cq->head.first = tmr;
	    return NULL;
	}
    }

    cq->head.first = (ErtsTimer *) erts_atomic_read_nob(&tmr->head.u.next);

    ASSERT(cq->head.first);

    return tmr;
}

static int
cq_check_incoming(ErtsSchedulerData *esdp, ErtsHLTCncldTmrQ *cq)
{
    erts_aint_t ilast = erts_atomic_read_nob(&cq->tail.data.last);
    if (((ErtsTimer *) ilast) == (ErtsTimer *) &cq->tail.data.marker
	&& cq->head.first == (ErtsTimer *) &cq->tail.data.marker) {
	/* Nothing more to do... */
	return 0;
    }

    if (cq->head.next.thr_progress_reached
	|| erts_thr_progress_has_reached(cq->head.next.thr_progress)) {
	cq->head.next.thr_progress_reached = 1;
	/* Move unreferenced end pointer forward... */

        ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore);

	cq->head.unref_end = cq->head.next.unref_end;

	ilast = check_insert_marker(cq, ilast);

	if (cq->head.unref_end != (ErtsTimer *) ilast) {
	    cq->head.next.unref_end = (ErtsTimer *) ilast;
	    cq->head.next.thr_progress = erts_thr_progress_later(esdp);
	    cq->head.next.thr_progress_reached = 0;
	}
    }
    return 1;
}

static ERTS_INLINE void
store_earliest_thr_prgr(ErtsThrPrgrVal *prev_val, ErtsHLTCncldTmrQ *cq)
{
    if (!cq->head.next.thr_progress_reached
	&& (*prev_val == ERTS_THR_PRGR_INVALID
	    || erts_thr_progress_cmp(cq->head.next.thr_progress,
				     *prev_val) < 0)) {
	*prev_val = cq->head.next.thr_progress;
    }
}

static void
handle_canceled_queue(ErtsSchedulerData *esdp,
		      ErtsHLTCncldTmrQ *cq,
		      int use_limit,
		      int ops_limit,
		      int *need_thr_progress,
		      ErtsThrPrgrVal *thr_prgr_p,
		      int *need_more_work)
{
    int need_thr_prgr = 0;
    int need_mr_wrk = 0;
    int have_checked_incoming = 0;
    int ops = 0;

    ERTS_HLT_ASSERT(cq == &esdp->timer_service->canceled_queue);

    while (1) {
	ErtsTimer *tmr = cq_dequeue(cq);

	if (tmr)
	    cleanup_sched_local_canceled_timer(esdp, tmr);
	else {
	    if (have_checked_incoming)
		break;
	    need_thr_prgr = cq_check_incoming(esdp, cq);
	    if (need_thr_progress) {
		*need_thr_progress |= need_thr_prgr;
		if (need_thr_prgr)
		    store_earliest_thr_prgr(thr_prgr_p, cq);
	    }
	    have_checked_incoming = 1;
	    continue;
	}

	if (use_limit && ++ops >= ops_limit) {
	    if (cq->head.first != cq->head.unref_end) {
		need_mr_wrk = 1;
		if (need_more_work)
		    *need_more_work |= 1;
	    }
	    break;
	}
    }

    if (need_thr_progress && !(need_thr_prgr | need_mr_wrk)) {
	need_thr_prgr = cq_check_incoming(esdp, cq);
	*need_thr_progress |= need_thr_prgr;
	if (need_thr_prgr)
	    store_earliest_thr_prgr(thr_prgr_p, cq);
    }
}

void
erts_handle_canceled_timers(void *vesdp,
			    int *need_thr_progress,
			    ErtsThrPrgrVal *thr_prgr_p,
			    int *need_more_work)
{
    ErtsSchedulerData *esdp = (ErtsSchedulerData *) vesdp;
    ERTS_HLT_ASSERT(esdp == erts_get_scheduler_data());

    handle_canceled_queue(esdp, &esdp->timer_service->canceled_queue,
			  1, ERTS_TMR_CANCELED_TIMER_LIMIT,
			  need_thr_progress, thr_prgr_p,
			  need_more_work);
}


static void
queue_canceled_timer(ErtsSchedulerData *esdp, int rsched_id, ErtsTimer *tmr)
{
    ErtsHLTCncldTmrQ *cq;
    cq = &ERTS_SCHEDULER_IX(rsched_id-1)->timer_service->canceled_queue;
    if (cq_enqueue(cq, tmr, rsched_id - (int) esdp->no))
	erts_notify_canceled_timer(esdp, rsched_id);
}

static void
continue_cancel_ptimer(ErtsSchedulerData *esdp, ErtsTimer *tmr)
{
    Uint32 sid = (tmr->head.roflgs & ERTS_TMR_ROFLG_SID_MASK);

    if (esdp->no != sid)
	queue_canceled_timer(esdp, sid, tmr);
    else
	cleanup_sched_local_canceled_timer(esdp, tmr);
}

/*
 * BIF timer specific
 */


Uint erts_bif_timer_memory_size(void)
{
    return (Uint) 0;
}

static BIF_RETTYPE
setup_bif_timer(Process *c_p, int twheel, ErtsMonotonicTime timeout_pos,
		int short_time, Eterm rcvr, Eterm msg, int wrap)
{
    BIF_RETTYPE ret;
    Eterm ref, tmo_msg, *hp;
    ErtsBifTimer *tmr;
    ErtsSchedulerData *esdp;
    Eterm tmp_hp[4];
    ErtsCreateTimerFunc create_timer;

    if (is_not_internal_pid(rcvr) && is_not_atom(rcvr))
	goto badarg;

    esdp = erts_proc_sched_data(c_p);

    hp = HAlloc(c_p, ERTS_REF_THING_SIZE);
    ref = erts_sched_make_ref_in_buffer(esdp, hp);
    ASSERT(erts_get_ref_numbers_thr_id(internal_ordinary_ref_numbers(ref))
           == (Uint32) esdp->no);

    tmo_msg = wrap ? TUPLE3(tmp_hp, am_timeout, ref, msg) : msg;

    create_timer = twheel ? create_tw_timer : create_hl_timer;
    tmr = (ErtsBifTimer *) create_timer(esdp, timeout_pos,
                                        short_time, ERTS_TMR_BIF,
                                        NULL, rcvr, tmo_msg,
                                        internal_ordinary_ref_numbers(ref),
                                        NULL, NULL);

    if (is_internal_pid(rcvr)) {
	Process *proc = erts_pid2proc_opt(c_p, ERTS_PROC_LOCK_MAIN,
					  rcvr, ERTS_PROC_LOCK_BTM,
					  ERTS_P2P_FLG_INC_REFC);
	if (!proc) {
            if (tmr->btm.tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE) {
                btm_rbt_delete(&esdp->timer_service->btm_tree, tmr);
                tmr->btm.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
            }
	    if (tmr->btm.bp)
		free_message_buffer(tmr->btm.bp);
            if (twheel)
                cancel_tw_timer(esdp, &tmr->type.twt);
            else
                hlt_delete_timer(esdp, &tmr->type.hlt);
            timer_destroy((ErtsTimer *) tmr, twheel, 1);
	}
	else {
	    proc_btm_rbt_insert(&proc->bif_timers, tmr);
	    erts_proc_unlock(proc, ERTS_PROC_LOCK_BTM);
            tmr->type.head.receiver.proc = proc;
	}
    }

    ERTS_BIF_PREP_RET(ret, ref);
    return ret;

badarg:

    ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);
    return ret;
}

static int
cancel_bif_timer(ErtsBifTimer *tmr)
{
    erts_aint_t state;
    Uint32 roflgs;
    int res;

    state = erts_atomic32_cmpxchg_acqb(&tmr->btm.state,
					   ERTS_TMR_STATE_CANCELED,
					   ERTS_TMR_STATE_ACTIVE);
    if (state != ERTS_TMR_STATE_ACTIVE)
	return 0;

    if (tmr->btm.bp)
	free_message_buffer(tmr->btm.bp);

    res = -1;

    roflgs = tmr->type.head.roflgs;
    if (roflgs & ERTS_TMR_ROFLG_PROC) {
	Process *proc;

        proc = tmr->type.head.receiver.proc;
	ERTS_HLT_ASSERT(!(tmr->type.head.roflgs & ERTS_TMR_ROFLG_REG_NAME));

	erts_proc_lock(proc, ERTS_PROC_LOCK_BTM);
	/*
	 * If process is exiting, let it clean up
	 * the btm tree by itself (it may be in
	 * the middle of tree destruction).
	 */
	if (!ERTS_PROC_IS_EXITING(proc)
	    && tmr->btm.proc_tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE) {
	    proc_btm_rbt_delete(&proc->bif_timers, tmr);
	    tmr->btm.proc_tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
	    res = 1;
	}
	erts_proc_unlock(proc, ERTS_PROC_LOCK_BTM);
    }

    return res;
}

static ERTS_INLINE Sint64
access_btm(ErtsBifTimer *tmr, Uint32 sid, ErtsSchedulerData *esdp, int cancel)
{
    int cncl_res;
    Sint64 time_left;
    ErtsMonotonicTime timeout;
    int is_hlt;

    if (!tmr)
        return -1;

    is_hlt = !!(tmr->type.head.roflgs & ERTS_TMR_ROFLG_HLT);
    timeout = (is_hlt
               ? tmr->type.hlt.timeout
               : erts_tweel_read_timeout(&tmr->type.twt.u.tw_tmr));

    if (!cancel) {
        erts_aint32_t state = erts_atomic32_read_acqb(&tmr->btm.state);
        if (state == ERTS_TMR_STATE_ACTIVE)
            return get_time_left(esdp, timeout);
        return -1;
    }

    cncl_res = cancel_bif_timer(tmr);
    if (!cncl_res)
        return -1;

    time_left = get_time_left(esdp, timeout);

    if (sid != (Uint32) esdp->no) {
        if (cncl_res > 0)
            queue_canceled_timer(esdp, sid, (ErtsTimer *) tmr);
    }
    else {
        if (tmr->btm.tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE) {
	    btm_rbt_delete(&esdp->timer_service->btm_tree, tmr);
	    tmr->btm.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
	}
        if (is_hlt) {
            if (cncl_res > 0)
                hl_timer_dec_refc(&tmr->type.hlt, tmr->type.hlt.head.roflgs);
            hlt_delete_timer(esdp, &tmr->type.hlt);
        }
        else {
            if (cncl_res > 0)
                tw_timer_dec_refc(&tmr->type.twt);
            cancel_tw_timer(esdp, &tmr->type.twt);
        }
    }

    return time_left;
}

static ERTS_INLINE Eterm
return_info(Process *c_p, Sint64 time_left)
{
    Uint hsz;
    Eterm *hp;

    if (time_left < 0)
        return am_false;

    if (time_left <= (Sint64) MAX_SMALL)
        return make_small((Sint) time_left);

    hsz = ERTS_SINT64_HEAP_SIZE(time_left);
    hp = HAlloc(c_p, hsz);
    return erts_sint64_to_big(time_left, &hp);
}

static ERTS_INLINE Eterm
send_async_info(Process *proc, ErtsProcLocks initial_locks,
                Eterm tref, int cancel, Sint64 time_left)
{
    ErtsProcLocks locks = initial_locks;
    ErtsMessage *mp;
    Eterm tag, res, msg, ref;
    Uint hsz;
    Eterm *hp;
    ErlOffHeap *ohp;

    hsz = 4;
    hsz += NC_HEAP_SIZE(tref);

    if (time_left > (Sint64) MAX_SMALL)
        hsz += ERTS_SINT64_HEAP_SIZE(time_left);

    mp = erts_alloc_message_heap(proc, &locks, hsz, &hp, &ohp);

    if (cancel)
        tag = am_cancel_timer;
    else
        tag = am_read_timer;

    ref = STORE_NC(&hp, ohp, tref);

    if (time_left < 0)
        res = am_false;
    else if (time_left <= (Sint64) MAX_SMALL)
        res = make_small((Sint) time_left);
    else
        res = erts_sint64_to_big(time_left, &hp);

    msg = TUPLE3(hp, tag, ref, res);

    erts_queue_message(proc, locks, mp, msg, am_clock_service);

    locks &= ~initial_locks;
    if (locks)
        erts_proc_unlock(proc, locks);

    return am_ok;
}

static ERTS_INLINE Eterm
send_sync_info(Process *proc, ErtsProcLocks initial_locks,
               Uint32 *refn, int cancel, Sint64 time_left)
{
    ErtsProcLocks locks = initial_locks;
    ErtsMessage *mp;
    Eterm res, msg, ref;
    Uint hsz;
    Eterm *hp;
    ErlOffHeap *ohp;

    hsz = 3 + ERTS_REF_THING_SIZE;

    if (time_left > (Sint64) MAX_SMALL)
        hsz += ERTS_SINT64_HEAP_SIZE(time_left);

    mp = erts_alloc_message_heap(proc, &locks, hsz, &hp, &ohp);

    write_ref_thing(hp, refn[0], refn[1], refn[2]);
    ref = make_internal_ref(hp);
    hp += ERTS_REF_THING_SIZE;

    if (time_left < 0)
        res = am_false;
    else if (time_left <= (Sint64) MAX_SMALL)
        res = make_small((Sint) time_left);
    else
        res = erts_sint64_to_big(time_left, &hp);

    msg = TUPLE2(hp, ref, res);

    erts_queue_message(proc, locks, mp, msg, am_clock_service);

    locks &= ~initial_locks;
    if (locks)
        erts_proc_unlock(proc, locks);

    return am_ok;
}

static ERTS_INLINE Eterm
access_sched_local_btm(Process *c_p, Eterm pid,
                       Eterm tref, Uint32 *trefn,
                       Uint32 *rrefn,
                       int async, int cancel,
                       int return_res,
                       int info)
{
    ErtsSchedulerData *esdp;
    ErtsHLTimerService *srv;
    ErtsBifTimer *tmr;
    Sint64 time_left;
    Process *proc;
    ErtsProcLocks proc_locks;

    time_left = -1;

    if (!c_p)
	esdp = erts_get_scheduler_data();
    else {
	esdp = erts_proc_sched_data(c_p);
	ERTS_HLT_ASSERT(esdp == erts_get_scheduler_data());
    }

    ERTS_HLT_ASSERT(erts_get_ref_numbers_thr_id(trefn)
		    == (Uint32) esdp->no);

    srv = esdp->timer_service;

    tmr = btm_rbt_lookup(srv->btm_tree, trefn);

    time_left = access_btm(tmr, (Uint32) esdp->no, esdp, cancel);

    if (!info)
        return am_ok;

    if (c_p) {
        proc = c_p;
        proc_locks = ERTS_PROC_LOCK_MAIN;
    }
    else {
        proc = erts_proc_lookup(pid);
        proc_locks = 0;
    }

    if (!async) {
        if (c_p)
            return return_info(c_p, time_left);

        if (proc)
            return send_sync_info(proc, proc_locks,
                                  rrefn, cancel, time_left);
    }
    else if (proc) {
        Eterm ref;
        Eterm heap[ERTS_REF_THING_SIZE];
        if (is_value(tref))
            ref = tref;
        else {
            write_ref_thing(&heap[0], trefn[0], trefn[1], trefn[2]);
            ref = make_internal_ref(&heap[0]);
        }
        return send_async_info(proc, proc_locks,
                               ref, cancel, time_left);
    }

    return am_ok;
}

#define ERTS_BTM_REQ_FLG_ASYNC		(((Uint32) 1) << 0)
#define ERTS_BTM_REQ_FLG_CANCEL		(((Uint32) 1) << 1)
#define ERTS_BTM_REQ_FLG_INFO		(((Uint32) 1) << 2)

typedef struct {
    Eterm pid;
    Uint32 trefn[ERTS_REF_NUMBERS];
    Uint32 rrefn[ERTS_REF_NUMBERS];
    Uint32 flags;
} ErtsBifTimerRequest;

static void
bif_timer_access_request(void *vreq)
{
    ErtsBifTimerRequest *req = (ErtsBifTimerRequest *) vreq;
    int async = (int) (req->flags & ERTS_BTM_REQ_FLG_ASYNC);
    int cancel = (int) (req->flags & ERTS_BTM_REQ_FLG_CANCEL);
    int info = (int) (req->flags & ERTS_BTM_REQ_FLG_INFO);
    (void) access_sched_local_btm(NULL, req->pid, THE_NON_VALUE,
				  req->trefn, req->rrefn, async,
				  cancel, 0, info);
    erts_free(ERTS_ALC_T_TIMER_REQUEST, vreq);
}

static int
try_access_sched_remote_btm(ErtsSchedulerData *esdp,
			    Process *c_p, Uint32 sid,
			    Eterm tref, Uint32 *trefn,
			    int async, int cancel,
			    int info, Eterm *resp)
{
    ErtsBifTimer *tmr;
    Sint64 time_left;

    ERTS_HLT_ASSERT(c_p);

    /*
     * Check if the timer is aimed at current
     * process...
     */
    erts_proc_lock(c_p, ERTS_PROC_LOCK_BTM);
    tmr = proc_btm_rbt_lookup(c_p->bif_timers, trefn);
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_BTM);
    if (!tmr)
	return 0;

    time_left = access_btm(tmr, sid, esdp, cancel);

    if (!info)
	*resp = am_ok;
    else if (!async)
        *resp = return_info(c_p, time_left);
    else
        *resp = send_async_info(c_p, ERTS_PROC_LOCK_MAIN,
                                tref, cancel, time_left);

    return 1;
}

static Eterm
no_timer_result(Process *c_p, Eterm tref, int cancel, int async, int info)
{
    ErtsMessage *mp;
    Uint hsz;
    Eterm *hp, msg, ref, tag;
    ErlOffHeap *ohp;
    ErtsProcLocks locks;

    if (!async)
        return am_false;
    if (!info)
        return am_ok;

    hsz = 4;
    hsz += NC_HEAP_SIZE(tref);
    locks = ERTS_PROC_LOCK_MAIN;
    mp = erts_alloc_message_heap(c_p, &locks, hsz, &hp, &ohp);
    ref = STORE_NC(&hp, ohp, tref);
    tag = cancel ? am_cancel_timer : am_read_timer;
    msg = TUPLE3(hp, tag, ref, am_false);
    erts_queue_message(c_p, locks, mp, msg, am_clock_service);
    locks &= ~ERTS_PROC_LOCK_MAIN;
    if (locks)
        erts_proc_unlock(c_p, locks);
    return am_ok;
}

static BIF_RETTYPE
access_bif_timer(Process *c_p, Eterm tref, int cancel, int async, int info)
{
    BIF_RETTYPE ret;
    ErtsSchedulerData *esdp;
    Uint32 sid;
    Uint32 *trefn;
    Eterm res;

    if (is_not_internal_ref(tref)) {
	if (is_not_ref(tref))
	    goto badarg;
	else
	    goto no_timer;
    }

    esdp = erts_proc_sched_data(c_p);

    trefn = internal_ref_numbers(tref);
    sid = erts_get_ref_numbers_thr_id(trefn);
    if (sid < 1 || erts_no_schedulers < sid)
	goto no_timer;

    if (sid == (Uint32) esdp->no) {
	res = access_sched_local_btm(c_p, c_p->common.id,
				     tref, trefn, NULL,
				     async, cancel, !async,
				     info);
	ERTS_BIF_PREP_RET(ret, res);
    }
    else if (try_access_sched_remote_btm(esdp, c_p,
					 sid, tref, trefn,
					 async, cancel,
					 info, &res)) {
	ERTS_BIF_PREP_RET(ret, res);
    }
    else {
	/*
	 * Schedule access for execution on
	 * remote scheduler...
	 */
	ErtsBifTimerRequest *req = erts_alloc(ERTS_ALC_T_TIMER_REQUEST,
					      sizeof(ErtsBifTimerRequest));

	req->flags = 0;
	if (cancel)
	    req->flags |= ERTS_BTM_REQ_FLG_CANCEL;
	if (async)
	    req->flags |= ERTS_BTM_REQ_FLG_ASYNC;
	if (info)
	    req->flags |= ERTS_BTM_REQ_FLG_INFO;

	req->pid = c_p->common.id;

	req->trefn[0] = trefn[0];
	req->trefn[1] = trefn[1];
	req->trefn[2] = trefn[2];

	if (async)
	    ERTS_BIF_PREP_RET(ret, am_ok);
	else {
	    Eterm *hp, rref;
	    Uint32 *rrefn;

	    hp = HAlloc(c_p, ERTS_REF_THING_SIZE);
	    rref = erts_sched_make_ref_in_buffer(esdp, hp);
	    rrefn = internal_ref_numbers(rref);

	    req->rrefn[0] = rrefn[0];
	    req->rrefn[1] = rrefn[1];
	    req->rrefn[2] = rrefn[2];

            /*
             * Caller needs to wait for a message containing
             * the ref that we just created. No such message
             * can exist in callers message queue at this time.
             * We therefore move the save pointer of the
             * callers message queue to the end of the queue.
             *
             * NOTE: It is of vital importance that the caller
             *       immediately do a receive unconditionaly
             *       waiting for the message with the reference;
             *       otherwise, next receive will *not* work
             *       as expected!
             */
            ERTS_RECV_MARK_SAVE(c_p);
            ERTS_RECV_MARK_SET(c_p);

	    ERTS_BIF_PREP_TRAP1(ret, erts_await_result, c_p, rref);
	}

	erts_schedule_misc_aux_work(sid,
				    bif_timer_access_request,
				    (void *) req);
    }

    return ret;

badarg:
    ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);
    return ret;

no_timer:
    return no_timer_result(c_p, tref, cancel, async, info);
}

static ERTS_INLINE int
bool_arg(Eterm val, int *argp)
{
    switch (val) {
    case am_true: *argp = 1; return 1;
    case am_false: *argp = 0; return 1;
    default: return 0;
    }
}

static ERTS_INLINE int
parse_bif_timer_options(Eterm option_list, int *async,
                        int *info, int *abs)
{
    Eterm list = option_list;

    if (async)
	*async = 0;
    if (info)
	*info = 1;
    if (abs)
	*abs = 0;

    while (is_list(list)) {
	Eterm *consp, *tp, opt;

	consp = list_val(list);
	opt = CAR(consp);
	if (is_not_tuple(opt))
	    return 0;

	tp = tuple_val(opt);
	if (arityval(tp[0]) != 2)
	    return 0;

	switch (tp[1]) {
	case am_async:
	    if (!async || !bool_arg(tp[2], async))
		return 0;
	    break;
	case am_info:
	    if (!info || !bool_arg(tp[2], info))
		return 0;
	    break;
	case am_abs:
	    if (!abs || !bool_arg(tp[2], abs))
		return 0;
	    break;
	default:
	    return 0;
	}

	list = CDR(consp);	
    }

    if (is_not_nil(list))
	return 0;
    return 1;
}

static int
exit_cancel_bif_timer(ErtsBifTimer *tmr, void *vesdp, Sint reds)
{
#define ERTS_BTM_CANCEL_REDS 80
    ErtsSchedulerData *esdp = (ErtsSchedulerData *) vesdp;
    Uint32 sid, roflgs;
    erts_aint_t state;
    int is_hlt;

    state = erts_atomic32_cmpxchg_acqb(&tmr->btm.state,
					   ERTS_TMR_STATE_CANCELED,
					   ERTS_TMR_STATE_ACTIVE);

    roflgs = tmr->type.head.roflgs;
    sid = roflgs & ERTS_TMR_ROFLG_SID_MASK;
    is_hlt = !!(roflgs & ERTS_TMR_ROFLG_HLT);

    ERTS_HLT_ASSERT(sid == erts_get_ref_numbers_thr_id(ERTS_BTM_HLT2REFN(tmr)));
    ERTS_HLT_ASSERT(tmr->btm.proc_tree.parent
		    != ERTS_HLT_PFIELD_NOT_IN_TABLE);
    tmr->btm.proc_tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;

    if (state == ERTS_TMR_STATE_ACTIVE) {
        if (tmr->btm.bp)
            free_message_buffer(tmr->btm.bp);

        if (sid != (Uint32) esdp->no) {
            queue_canceled_timer(esdp, sid, (ErtsTimer *) tmr);
            return ERTS_BTM_CANCEL_REDS;
        }

        if (tmr->btm.tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE) {
	    btm_rbt_delete(&esdp->timer_service->btm_tree, tmr);
	    tmr->btm.tree.parent = ERTS_HLT_PFIELD_NOT_IN_TABLE;
	}
        if (is_hlt)
            hlt_delete_timer(esdp, &tmr->type.hlt);
        else
            cancel_tw_timer(esdp, &tmr->type.twt);
    }
    if (is_hlt)
        hl_timer_dec_refc(&tmr->type.hlt, roflgs);
    else
        tw_timer_dec_refc(&tmr->type.twt);
    return ERTS_BTM_CANCEL_REDS;
}

typedef struct {
    ErtsBifTimers *bif_timers;
    union {
	proc_btm_rbt_yield_state_t proc_btm_yield_state;
    } u;
} ErtsBifTimerYieldState;

int erts_cancel_bif_timers(Process *p, ErtsBifTimers **btm, void **vyspp, int reds)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(p);
    ErtsBifTimerYieldState ys = {*btm, {ERTS_RBT_YIELD_STAT_INITER}};
    ErtsBifTimerYieldState *ysp;
    int res;

    ysp = (ErtsBifTimerYieldState *) *vyspp;
    if (!ysp)
	ysp = &ys;

    res = proc_btm_rbt_foreach_destroy_yielding(&ysp->bif_timers,
						exit_cancel_bif_timer,
						(void *) esdp,
						&ysp->u.proc_btm_yield_state,
						reds);

    if (res > 0) {
	if (ysp != &ys)
	    erts_free(ERTS_ALC_T_BTM_YIELD_STATE, ysp);
	*vyspp = NULL;
    }
    else {

	if (ysp == &ys) {
	    ysp = erts_alloc(ERTS_ALC_T_BTM_YIELD_STATE,
			     sizeof(ErtsBifTimerYieldState));
	    sys_memcpy((void *) ysp, (void *) &ys,
		       sizeof(ErtsBifTimerYieldState));
	}

	*vyspp = (void *) ysp;
    }

    return res;

}

static ERTS_INLINE int
parse_timeout_pos(ErtsSchedulerData *esdp, Eterm arg,
		  ErtsMonotonicTime *conv_arg, int abs,
		  ErtsMonotonicTime *tposp, int *stimep,
                  ErtsMonotonicTime *msp)
{
    ErtsMonotonicTime t, now;
    
    if (!term_to_Sint64(arg, &t)) {
	ERTS_HLT_ASSERT(!is_small(arg));
	if (!is_big(arg))
	    return -1;

	if (abs || !big_sign(arg))
	    return 1;

	return -1;
    }

    if (conv_arg)
	*conv_arg = t;

    now = erts_get_monotonic_time(esdp);

    if (abs) {
	t += -1*ERTS_MONOTONIC_OFFSET_MSEC; /* external to internal */
	if (t < ERTS_MONOTONIC_TO_MSEC(ERTS_MONOTONIC_BEGIN))
	    return 1;
	if (t > ERTS_MONOTONIC_TO_MSEC(ERTS_MONOTONIC_END))
	    return 1;
        if (msp)
            *msp = t - ERTS_MONOTONIC_TO_MSEC(now);

	*stimep = (t - ERTS_MONOTONIC_TO_MSEC(esdp->last_monotonic_time)
		   < ERTS_BIF_TIMER_SHORT_TIME);
	*tposp = ERTS_MSEC_TO_CLKTCKS(t);
    }
    else {
	ErtsMonotonicTime ticks;
	
	if (t < 0)
	    return -1;

        if (msp)
            *msp = t;

	ticks = ERTS_MSEC_TO_CLKTCKS(t);

	if (ERTS_CLKTCK_RESOLUTION > 1000 && ticks < 0)
	    return 1;

	ERTS_HLT_ASSERT(ticks >= 0);

	ticks += ERTS_MONOTONIC_TO_CLKTCKS(now-1);
	ticks += 1;

	if (ticks < ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_BEGIN))
	    return 1;
	if (ticks > ERTS_MONOTONIC_TO_CLKTCKS(ERTS_MONOTONIC_END))
	    return 1;

	*stimep = (t < ERTS_BIF_TIMER_SHORT_TIME);
	*tposp = ticks;
    }

    return 0;
}

/*
 *
 * The BIF timer BIFs...
 */

BIF_RETTYPE send_after_3(BIF_ALIST_3)
{
    ErtsMonotonicTime timeout_pos, tmo;
    int short_time, tres;

    tres = parse_timeout_pos(erts_proc_sched_data(BIF_P), BIF_ARG_1,
			     NULL, 0, &timeout_pos, &short_time, &tmo);
    if (tres != 0)
	BIF_ERROR(BIF_P, BADARG);

    return setup_bif_timer(BIF_P, tmo < ERTS_TIMER_WHEEL_MSEC,
                           timeout_pos, short_time, BIF_ARG_2,
                           BIF_ARG_3, 0);
}

BIF_RETTYPE send_after_4(BIF_ALIST_4)
{
    ErtsMonotonicTime timeout_pos, tmo;
    int short_time, abs, tres;

    if (!parse_bif_timer_options(BIF_ARG_4, NULL, NULL, &abs))
	BIF_ERROR(BIF_P, BADARG);
    
    tres = parse_timeout_pos(erts_proc_sched_data(BIF_P), BIF_ARG_1, NULL,
			     abs, &timeout_pos, &short_time, &tmo);
    if (tres != 0)
	BIF_ERROR(BIF_P, BADARG);

    return setup_bif_timer(BIF_P, tmo < ERTS_TIMER_WHEEL_MSEC,
                           timeout_pos, short_time, BIF_ARG_2,
                           BIF_ARG_3, 0);
}

BIF_RETTYPE start_timer_3(BIF_ALIST_3)
{
    ErtsMonotonicTime timeout_pos, tmo;
    int short_time, tres;

    tres = parse_timeout_pos(erts_proc_sched_data(BIF_P), BIF_ARG_1, NULL,
			     0, &timeout_pos, &short_time, &tmo);
    if (tres != 0)
	BIF_ERROR(BIF_P, BADARG);

    return setup_bif_timer(BIF_P, tmo < ERTS_TIMER_WHEEL_MSEC,
                           timeout_pos, short_time, BIF_ARG_2,
                           BIF_ARG_3, !0);
}

BIF_RETTYPE start_timer_4(BIF_ALIST_4)
{
    ErtsMonotonicTime timeout_pos, tmo;
    int short_time, abs, tres;

    if (!parse_bif_timer_options(BIF_ARG_4, NULL, NULL, &abs))
	BIF_ERROR(BIF_P, BADARG);

    tres = parse_timeout_pos(erts_proc_sched_data(BIF_P), BIF_ARG_1, NULL,
			     abs, &timeout_pos, &short_time, &tmo);
    if (tres != 0)
	BIF_ERROR(BIF_P, BADARG);

    return setup_bif_timer(BIF_P, tmo < ERTS_TIMER_WHEEL_MSEC,
                           timeout_pos, short_time, BIF_ARG_2,
                           BIF_ARG_3, !0);
}

BIF_RETTYPE cancel_timer_1(BIF_ALIST_1)
{
    return access_bif_timer(BIF_P, BIF_ARG_1, 1, 0, 1);
}

BIF_RETTYPE cancel_timer_2(BIF_ALIST_2)
{
    BIF_RETTYPE ret;
    int async, info;

    if (parse_bif_timer_options(BIF_ARG_2, &async, &info, NULL))
	return access_bif_timer(BIF_P, BIF_ARG_1, 1, async, info);

    ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    return ret;
}

BIF_RETTYPE read_timer_1(BIF_ALIST_1)
{
    return access_bif_timer(BIF_P, BIF_ARG_1, 0, 0, 1);
}

BIF_RETTYPE read_timer_2(BIF_ALIST_2)
{
    BIF_RETTYPE ret;
    int async;

    if (parse_bif_timer_options(BIF_ARG_2, &async, NULL, NULL))
	return access_bif_timer(BIF_P, BIF_ARG_1, 0, async, 1);

    ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    return ret;
}

static void
start_callback_timer(ErtsSchedulerData *esdp,
		     int twt,
		     ErtsMonotonicTime timeout_pos,
		     void (*callback)(void *),
		     void *arg)
		     
{
    ErtsCreateTimerFunc create_timer = (twt
                                        ? create_tw_timer
                                        : create_hl_timer);
    (void) create_timer(esdp, timeout_pos, 0,
                        ERTS_TMR_CALLBACK, NULL,
                        NIL, THE_NON_VALUE, NULL,
                        callback, arg);
}

typedef struct {
    int twt;
    ErtsMonotonicTime timeout_pos;
    void (*callback)(void *);
    void *arg;
} ErtsStartCallbackTimerRequest;

static void
scheduled_start_callback_timer(void *vsctr)
{
    ErtsStartCallbackTimerRequest *sctr
	= (ErtsStartCallbackTimerRequest *) vsctr;

    start_callback_timer(erts_get_scheduler_data(),
			 sctr->twt,
			 sctr->timeout_pos,
			 sctr->callback,	
			 sctr->arg);

    erts_free(ERTS_ALC_T_TIMER_REQUEST, vsctr);
}

void
erts_start_timer_callback(ErtsMonotonicTime tmo,
			  void (*callback)(void *),
			  void *arg)
{
    ErtsSchedulerData *esdp;
    ErtsMonotonicTime timeout_pos;
    int twt;

    esdp = erts_get_scheduler_data();
    timeout_pos = get_timeout_pos(erts_get_monotonic_time(esdp),
				  tmo);
    twt = tmo < ERTS_TIMER_WHEEL_MSEC;

    if (esdp && !ERTS_SCHEDULER_IS_DIRTY(esdp))
	start_callback_timer(esdp,
			     twt,
			     timeout_pos,
			     callback,
			     arg);
    else {
	ErtsStartCallbackTimerRequest *sctr;
	sctr = erts_alloc(ERTS_ALC_T_TIMER_REQUEST,
			  sizeof(ErtsStartCallbackTimerRequest));
	sctr->twt = twt;
	sctr->timeout_pos = timeout_pos;
	sctr->callback = callback;
	sctr->arg = arg;
	erts_schedule_misc_aux_work(1,
				    scheduled_start_callback_timer,
				    (void *) sctr);
    }
}

/*
 * Process and Port timer functionality.
 *
 * NOTE! These are only allowed to be called by a
 *       scheduler thread that currently is
 *       executing the process or port.
 */

static ERTS_INLINE void
set_proc_timer_common(Process *c_p, ErtsSchedulerData *esdp, Sint64 tmo,
		      ErtsMonotonicTime timeout_pos, int short_time)
{
    void *tmr;
    check_canceled_queue(esdp, esdp->timer_service);

    if (tmo == 0)
	c_p->flags |= F_TIMO;
    else {
        ErtsCreateTimerFunc create_timer;

	c_p->flags |= F_INSLPQUEUE;
	c_p->flags &= ~F_TIMO;

        create_timer = (tmo < ERTS_TIMER_WHEEL_MSEC
                        ? create_tw_timer
                        : create_hl_timer);
        tmr = (void *) create_timer(esdp, timeout_pos, short_time,
                                    ERTS_TMR_PROC, (void *) c_p,
                                    c_p->common.id, THE_NON_VALUE,
                                    NULL, NULL, NULL);
	erts_atomic_set_relb(&c_p->common.timer, (erts_aint_t) tmr);
    }
}

int
erts_set_proc_timer_term(Process *c_p, Eterm etmo)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    ErtsMonotonicTime tmo, timeout_pos;
    int short_time, tres;

    ERTS_HLT_ASSERT(erts_atomic_read_nob(&c_p->common.timer)
		    == ERTS_PTMR_NONE);

    tres = parse_timeout_pos(esdp, etmo, &tmo, 0,
			     &timeout_pos, &short_time, NULL);
    if (tres != 0)
	return tres;

    if ((tmo >> 32) != 0)
	return 1;

    set_proc_timer_common(c_p, esdp, tmo, timeout_pos, short_time);
    return 0;
}

void
erts_set_proc_timer_uword(Process *c_p, UWord tmo)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);

    ERTS_HLT_ASSERT(erts_atomic_read_nob(&c_p->common.timer)
		    == ERTS_PTMR_NONE);

#ifndef ARCH_32
    ERTS_HLT_ASSERT((tmo >> 32) == (UWord) 0);
#endif

    if (tmo == 0)
	c_p->flags |= F_TIMO;
    else {
	ErtsMonotonicTime timeout_pos;
	timeout_pos = get_timeout_pos(erts_get_monotonic_time(esdp),
				      (ErtsMonotonicTime) tmo);
	set_proc_timer_common(c_p, esdp, (ErtsMonotonicTime) tmo,
			      timeout_pos,
			      tmo < ERTS_BIF_TIMER_SHORT_TIME);
    }
}

void
erts_cancel_proc_timer(Process *c_p)
{
    erts_aint_t tval;
    tval = erts_atomic_xchg_acqb(&c_p->common.timer,
				     ERTS_PTMR_NONE);
    c_p->flags &= ~(F_INSLPQUEUE|F_TIMO);
    if (tval == ERTS_PTMR_NONE)
	return;
    if (tval == ERTS_PTMR_TIMEDOUT) {
	erts_atomic_set_nob(&c_p->common.timer, ERTS_PTMR_NONE);
	return;
    }
    continue_cancel_ptimer(erts_proc_sched_data(c_p),
			   (ErtsTimer *) tval);
}

void
erts_set_port_timer(Port *c_prt, Sint64 tmo)
{
    void *tmr;
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsMonotonicTime timeout_pos;
    ErtsCreateTimerFunc create_timer;

    if (erts_atomic_read_nob(&c_prt->common.timer) != ERTS_PTMR_NONE)
	erts_cancel_port_timer(c_prt);

    check_canceled_queue(esdp, esdp->timer_service);

    if (tmo == 0) {
        erts_atomic_set_relb(&c_prt->common.timer, ERTS_PTMR_TIMEDOUT);
        erts_port_task_schedule(c_prt->common.id,
				&c_prt->timeout_task,
				ERTS_PORT_TASK_TIMEOUT);
    } else {

        timeout_pos = get_timeout_pos(erts_get_monotonic_time(esdp), tmo);

        create_timer = (tmo < ERTS_TIMER_WHEEL_MSEC
                        ? create_tw_timer
                        : create_hl_timer);
        tmr = (void *) create_timer(esdp, timeout_pos, 0, ERTS_TMR_PORT,
                                    (void *) c_prt, c_prt->common.id,
                                    THE_NON_VALUE, NULL, NULL, NULL);
        erts_atomic_set_relb(&c_prt->common.timer, (erts_aint_t) tmr);
    }
}

void
erts_cancel_port_timer(Port *c_prt)
{
    erts_aint_t tval;
    tval = erts_atomic_xchg_acqb(&c_prt->common.timer,
				     ERTS_PTMR_NONE);
    if (tval == ERTS_PTMR_NONE)
	return;
    if (tval == ERTS_PTMR_TIMEDOUT) {
	while (!erts_port_task_is_scheduled(&c_prt->timeout_task))
	    erts_thr_yield();
	erts_port_task_abort(&c_prt->timeout_task);
	erts_atomic_set_nob(&c_prt->common.timer, ERTS_PTMR_NONE);
	return;
    }
    continue_cancel_ptimer(erts_get_scheduler_data(),
			   (ErtsTimer *) tval);
}

Sint64
erts_read_port_timer(Port *c_prt)
{
    ErtsTimer *tmr;
    erts_aint_t itmr;
    ErtsMonotonicTime timeout_pos;

    itmr = erts_atomic_read_acqb(&c_prt->common.timer);
    if (itmr == ERTS_PTMR_NONE)
	return (Sint64) -1;
    if (itmr == ERTS_PTMR_TIMEDOUT)
	return (Sint64) 0;
    tmr = (ErtsTimer *) itmr;
    if (tmr->head.roflgs & ERTS_TMR_ROFLG_HLT)
	timeout_pos = tmr->hlt.timeout;
    else
	timeout_pos = erts_tweel_read_timeout(&tmr->twt.u.tw_tmr);
    return get_time_left(NULL, timeout_pos);
}

/*
 * Debug stuff...
 */

typedef struct {
    fmtfn_t to;
    void *to_arg;
    ErtsMonotonicTime now;
} ErtsBTMPrint;

static void
btm_print(ErtsBifTimer *tmr, void *vbtmp, ErtsMonotonicTime tpos, int is_hlt)
{
    ErtsBTMPrint *btmp = (ErtsBTMPrint *) vbtmp;
    ErtsMonotonicTime left;
    Eterm receiver;

    if (is_hlt) {
        ERTS_HLT_ASSERT(tmr->type.head.roflgs & ERTS_TMR_ROFLG_HLT);
        if (tmr->type.hlt.timeout <= btmp->now)
            left = 0;
        else
            left = ERTS_CLKTCKS_TO_MSEC(tmr->type.hlt.timeout - btmp->now);
    }
    else {
        ERTS_HLT_ASSERT(!(tmr->type.head.roflgs & ERTS_TMR_ROFLG_HLT));
        if (tpos <= btmp->now)
            left = 0;
        else
            left = ERTS_CLKTCKS_TO_MSEC(tpos - btmp->now);
    }

    receiver = ((tmr->type.head.roflgs & ERTS_TMR_ROFLG_REG_NAME)
                ? tmr->type.head.receiver.name
                : tmr->type.head.receiver.proc->common.id);

    erts_print(btmp->to, btmp->to_arg,
	       "=timer:%T\n"
	       "Message: %T\n"
	       "Time left: %b64d\n",
	       receiver,
	       tmr->btm.message,
	       (Sint64) left);
}

static int
btm_tree_print(ErtsBifTimer *tmr, void *vbtmp, Sint reds)
{
    int is_hlt = !!(tmr->type.head.roflgs & ERTS_TMR_ROFLG_HLT);
    ErtsMonotonicTime tpos;
    if (is_hlt)
        tpos = 0;
    else
        tpos = erts_tweel_read_timeout(&tmr->type.twt.u.tw_tmr);
    btm_print(tmr, vbtmp, tpos, is_hlt);
    return 1;
}

void
erts_print_bif_timer_info(fmtfn_t to, void *to_arg)
{
    ErtsBTMPrint btmp;
    int six;

    if (!ERTS_IS_CRASH_DUMPING)
	ERTS_INTERNAL_ERROR("Not crash dumping");

    btmp.to = to;
    btmp.to_arg = to_arg;
    btmp.now = erts_get_monotonic_time(NULL);
    btmp.now = ERTS_MONOTONIC_TO_CLKTCKS(btmp.now);

    for (six = 0; six < erts_no_schedulers; six++) {
	ErtsHLTimerService *srv =
	    erts_aligned_scheduler_data[six].esd.timer_service;
	btm_rbt_foreach(srv->btm_tree, btm_tree_print, (void *) &btmp);
    }
}

typedef struct {
    void (*func)(Eterm,
		 Eterm,
		 ErlHeapFragment *,
		 void *);
    void *arg;
} ErtsBTMForeachDebug;

static int
debug_btm_foreach(ErtsBifTimer *tmr, void *vbtmfd, Sint reds)
{
    if (erts_atomic32_read_nob(&tmr->btm.state) == ERTS_TMR_STATE_ACTIVE) {
	ErtsBTMForeachDebug *btmfd = (ErtsBTMForeachDebug *) vbtmfd;
        Eterm id = ((tmr->type.head.roflgs & ERTS_TMR_ROFLG_REG_NAME)
                    ? tmr->type.head.receiver.name
                    : tmr->type.head.receiver.proc->common.id);
	(*btmfd->func)(id, tmr->btm.message, tmr->btm.bp, btmfd->arg);
    }
    return 1;
}

void
erts_debug_bif_timer_foreach(void (*func)(Eterm,
					  Eterm,
					  ErlHeapFragment *,
					  void *),
			     void *arg)
{
    ErtsBTMForeachDebug btmfd;
    int six;

    btmfd.func = func;
    btmfd.arg = arg;

    if (!erts_thr_progress_is_blocking())
	ERTS_INTERNAL_ERROR("Not blocking thread progress");

    for (six = 0; six < erts_no_schedulers; six++) {
	ErtsHLTimerService *srv =
	    erts_aligned_scheduler_data[six].esd.timer_service;
	btm_rbt_foreach(srv->btm_tree,
			debug_btm_foreach,
			(void *) &btmfd);
    }
}

typedef struct {
    void (*tclbk)(void *);
    void (*func)(void *,
		 ErtsMonotonicTime,
		 void *);
    void *arg;
} ErtsDebugForeachCallbackTimer;

static void
debug_callback_timer_foreach_list(ErtsHLTimer *tmr, void *vdfct)
{
    ErtsDebugForeachCallbackTimer *dfct
	= (ErtsDebugForeachCallbackTimer *) vdfct;

    if ((tmr->head.roflgs & ERTS_TMR_ROFLG_CALLBACK)
	&& (tmr->head.receiver.callback == dfct->tclbk))
	(*dfct->func)(dfct->arg,
		      tmr->timeout,
		      tmr->head.u.arg);
}

static int
debug_callback_timer_foreach(ErtsHLTimer *tmr, void *vdfct, Sint reds)
{
    ErtsDebugForeachCallbackTimer *dfct
	= (ErtsDebugForeachCallbackTimer *) vdfct;

    if (tmr->time.tree.same_time)
	same_time_list_foreach(tmr->time.tree.same_time,
			       debug_callback_timer_foreach_list,
			       vdfct);

    if ((tmr->head.roflgs & ERTS_TMR_ROFLG_CALLBACK)
	&& (tmr->head.receiver.callback == dfct->tclbk))
	(*dfct->func)(dfct->arg,
		      tmr->timeout,
		      tmr->head.u.arg);
    return 1;
}

static void
debug_tw_callback_timer(void *vdfct,
			ErtsMonotonicTime timeout_pos,
			void *vtwtp)
{
    ErtsTWTimer *twtp = (ErtsTWTimer *) vtwtp;
    ErtsDebugForeachCallbackTimer *dfct
	= (ErtsDebugForeachCallbackTimer *) vdfct;

    if (twtp->head.receiver.callback == dfct->tclbk)
	(*dfct->func)(dfct->arg,
		      timeout_pos,
		      twtp->head.u.arg);
}

void
erts_debug_callback_timer_foreach(void (*tclbk)(void *),
				  void (*func)(void *,
					       ErtsMonotonicTime,
					       void *),
				  void *arg)
{
    int six;
    ErtsDebugForeachCallbackTimer dfct;

    dfct.tclbk = tclbk;
    dfct.func = func;
    dfct.arg = arg;

    if (!erts_thr_progress_is_blocking())
	ERTS_INTERNAL_ERROR("Not blocking thread progress");

    for (six = 0; six < erts_no_schedulers; six++) {
	ErtsHLTimerService *srv =
	    erts_aligned_scheduler_data[six].esd.timer_service;
	ErtsTimerWheel *twheel =
	    erts_aligned_scheduler_data[six].esd.timer_wheel;

	erts_twheel_debug_foreach(twheel,
				  tw_callback_timeout,
				  debug_tw_callback_timer,
				  (void *) &dfct);

	if (srv->yield.root)
	    debug_callback_timer_foreach(srv->yield.root,
					 (void *) &dfct,
                                         -1);

	time_rbt_foreach(srv->time_tree,
			 debug_callback_timer_foreach,
			 (void *) &dfct);
    }
}

#ifdef ERTS_HLT_HARD_DEBUG

typedef struct {
    ErtsHLTimerService *srv;
    int found_root;
    ErtsHLTimer **rootpp;
} ErtsHdbgHLT;

static void
st_hdbg_func(ErtsHLTimer *tmr, void *vhdbg)
{
    ErtsHdbgHLT *hdbg = (ErtsHdbgHLT *) vhdbg;
    ErtsHLTimer **rootpp;
    ERTS_HLT_ASSERT(tmr->time.tree.parent & ERTS_HLT_PFLG_SAME_TIME);
    if (tmr->time.tree.parent == ERTS_HLT_PFLG_SAME_TIME) {
	ERTS_HLT_ASSERT(tmr != *hdbg->rootpp);
    }
    else {
	rootpp = (ErtsHLTimer **) (tmr->time.tree.parent
				   & ~ERTS_HLT_PFLG_SAME_TIME);
	ERTS_HLT_ASSERT(rootpp == hdbg->rootpp);
	ERTS_HLT_ASSERT(tmr == *rootpp);
	ERTS_HLT_ASSERT(!hdbg->found_root);
	hdbg->found_root = 1;
    }
    ERTS_HLT_ASSERT(tmr->time.tree.u.l.next->time.tree.u.l.prev == tmr);
    ERTS_HLT_ASSERT(tmr->time.tree.u.l.prev->time.tree.u.l.next == tmr);
    ERTS_HLT_ASSERT(btm_rbt_lookup(hdbg->srv->btm_tree, ERTS_BTM_HLT2REFN(tmr)) == tmr);
}

static void
tt_hdbg_func(ErtsHLTimer *tmr, void *vhdbg)
{
    ErtsHdbgHLT *hdbg = (ErtsHdbgHLT *) vhdbg;
    ErtsHLTimer *prnt;
    ERTS_HLT_ASSERT((tmr->time.tree.parent & ERTS_HLT_PFLG_SAME_TIME) == 0);
    prnt = (ErtsHLTimer *) (tmr->time.tree.parent & ~ERTS_HLT_PFLGS_MASK);
    if (prnt) {
	ERTS_HLT_ASSERT(prnt->time.tree.u.t.left == tmr
			|| prnt->time.tree.u.t.right == tmr);
    }
    else {
	ERTS_HLT_ASSERT(!hdbg->found_root);
	hdbg->found_root = 1;
	ERTS_HLT_ASSERT(tmr == *hdbg->rootpp);
    }
    if (tmr->time.tree.u.t.left) {
	prnt = (ErtsHLTimer *) (tmr->time.tree.u.t.left->time.tree.parent
				& ~ERTS_HLT_PFLGS_MASK);
	ERTS_HLT_ASSERT(tmr == prnt);
    }
    if (tmr->time.tree.u.t.right) {
	prnt = (ErtsHLTimer *) (tmr->time.tree.u.t.right->time.tree.parent
				& ~ERTS_HLT_PFLGS_MASK);
	ERTS_HLT_ASSERT(tmr == prnt);
    }
    if (tmr->head.roflgs & ERTS_TMR_ROFLG_BIF_TMR)
	ERTS_HLT_ASSERT(btm_rbt_lookup(hdbg->srv->btm_tree, ERTS_BTM_HLT2REFN(tmr)) == tmr);
    if (tmr->time.tree.same_time) {
	ErtsHdbgHLT st_hdbg;
	st_hdbg.srv = hdbg->srv;
	st_hdbg.found_root = 0;
	st_hdbg.rootpp = &tmr->time.tree.same_time;
	same_time_list_foreach(tmr->time.tree.same_time, st_hdbg_func, (void *) &st_hdbg);
	ERTS_HLT_ASSERT(st_hdbg.found_root);
    }
}

static void
bt_hdbg_func(ErtsHLTimer *tmr, void *vhdbg)
{
    ErtsHdbgHLT *hdbg = (ErtsHdbgHLT *) vhdbg;
    ErtsHLTimer *prnt;
    ERTS_HLT_ASSERT((tmr->btm.tree.parent & ERTS_HLT_PFLG_SAME_TIME) == 0);
    prnt = (ErtsHLTimer *) (tmr->btm.tree.parent & ~ERTS_HLT_PFLGS_MASK);
    if (prnt) {
	ERTS_HLT_ASSERT(prnt->btm.tree.left == tmr
			|| prnt->btm.tree.right == tmr);
    }
    else {
	ERTS_HLT_ASSERT(!hdbg->found_root);
	hdbg->found_root = 1;
	ERTS_HLT_ASSERT(tmr == *hdbg->rootpp);
    }
    if (tmr->btm.tree.left) {
	prnt = (ErtsHLTimer *) (tmr->btm.tree.left->btm.tree.parent
				& ~ERTS_HLT_PFLGS_MASK);
	ERTS_HLT_ASSERT(tmr == prnt);
    }
    if (tmr->btm.tree.right) {
	prnt = (ErtsHLTimer *) (tmr->btm.tree.right->btm.tree.parent
				& ~ERTS_HLT_PFLGS_MASK);
	ERTS_HLT_ASSERT(tmr == prnt);
    }
    if (tmr->pending_timeout) {
	if (tmr->pending_timeout > 0) /* container > 0 */
	    ERTS_HLT_ASSERT(tmr->time.tree.parent == ERTS_HLT_PFIELD_NOT_IN_TABLE);
	else {
	    ERTS_HLT_ASSERT(tmr->time.tree.parent != ERTS_HLT_PFIELD_NOT_IN_TABLE);
	    ERTS_HLT_ASSERT(tmr->time.tree.parent & ERTS_HLT_PFLG_SAME_TIME);
	}
    }
    else {
	ErtsHLTimer *ttmr = time_rbt_lookup(hdbg->srv->time_tree, tmr->timeout);
	ERTS_HLT_ASSERT(ttmr);
	if (ttmr != tmr) {
	    ERTS_HLT_ASSERT(ttmr->time.tree.same_time);
	    ERTS_HLT_ASSERT(tmr == same_time_list_lookup(ttmr->time.tree.same_time, tmr));
	}
    }
}

static void
hdbg_chk_srv(ErtsHLTimerService *srv)
{
    if (srv->time_tree) {
	ErtsHdbgHLT hdbg;
	hdbg.srv = srv;
	hdbg.found_root = 0;
	hdbg.rootpp = &srv->time_tree;
	time_rbt_foreach(srv->time_tree, tt_hdbg_func, (void *) &hdbg);
	ERTS_HLT_ASSERT(hdbg.found_root);
    }
    if (srv->btm_tree) {
	ErtsHdbgHLT hdbg;
	hdbg.srv = srv;
	hdbg.found_root = 0;
	hdbg.rootpp = &srv->btm_tree;
	btm_rbt_foreach(srv->btm_tree, bt_hdbg_func, (void *) &hdbg);
	ERTS_HLT_ASSERT(hdbg.found_root);
    }
}

#endif /* ERTS_HLT_HARD_DEBUG */
