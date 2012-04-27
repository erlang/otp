/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2012. All Rights Reserved.
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
#include "beam_load.h"
#include "bif.h"
#include "error.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_term.h"

/* *************************************************************************
** Macros
*/

/*
** Memory allocation macros
*/
/* Breakpoint data */
#define Alloc(SZ)		erts_alloc(ERTS_ALC_T_BPD, (SZ))
#define ReAlloc(P, SIZ)		erts_realloc(ERTS_ALC_T_BPD, (P), (SZ))
#define Free(P)			erts_free(ERTS_ALC_T_BPD, (P))

/*
** Doubly linked ring macros
*/

#define BpInit(a,i)        \
do {                       \
    (a)->orig_instr = (i); \
    (a)->next = (a);       \
    (a)->prev = (a);       \
} while (0)

#define BpSpliceNext(a,b)                   \
do {                                        \
    register BpData *c = (a), *d = (b), *e; \
    e             = c->next->prev;          \
    c->next->prev = d->next->prev;          \
    d->next->prev = e;                      \
    e       = c->next;                      \
    c->next = d->next;                      \
    d->next = e;                            \
} while (0)

#define BpSplicePrev(a,b)                   \
do {                                        \
    register BpData *c = (a), *d = (b), *e; \
    e             = c->prev->next;          \
    c->prev->next = d->prev->next;          \
    d->prev->next = e;                      \
    e       = c->prev;                      \
    c->prev = d->prev;                      \
    d->prev = e;                            \
} while (0)

#ifdef DEBUG
#  define BpSingleton(a) ((a)->next == (a) && (a)->prev == (a))
#else
#  define BpSingleton(a) ((a)->next == (a))
#endif

#define BpInitAndSpliceNext(a,i,b) \
do {                               \
    (a)->orig_instr = (i);         \
    (a)->prev       = (b);         \
    (b)->next->prev = (a);         \
    (a)->next = (b)->next;         \
    (b)->next = (a);               \
} while (0)

#define BpInitAndSplicePrev(a,i,b) \
do {                               \
    (a)->orig_instr = (i);         \
    (a)->next       = (b);         \
    (b)->prev->next = (a);         \
    (a)->prev = (b)->prev;         \
    (b)->prev = (a);               \
} while (0)


#define BREAK_IS_BIF (1)
#define BREAK_IS_ERL (0)


/* *************************************************************************
** Local prototypes
*/

/*
** Helpers
*/

static int set_break(Eterm mfa[3], int specified,
		     Binary *match_spec, BeamInstr break_op,
		     enum erts_break_op count_op, Eterm tracer_pid);
static int set_module_break(Module *modp, Eterm mfa[3], int specified,
			    Binary *match_spec, BeamInstr break_op,
			    enum erts_break_op count_op, Eterm tracer_pid);
static int set_function_break(Module *modp, BeamInstr *pc, int bif,
			      Binary *match_spec, BeamInstr break_op,
			      enum erts_break_op count_op, Eterm tracer_pid); 

static int clear_break(Eterm mfa[3], int specified, 
		       BeamInstr break_op);
static int clear_module_break(Module *modp, Eterm mfa[3], int specified, 
			      BeamInstr break_op);
static int clear_function_break(Module *modp, BeamInstr *pc, int bif,
				BeamInstr break_op);

static BpData *is_break(BeamInstr *pc, BeamInstr break_op);
static BpData *get_break(Process *p, BeamInstr *pc, BeamInstr break_op);

/* bp_hash */
#define BP_TIME_ADD(pi0, pi1)                       \
    do {                                            \
	Uint r;                                     \
	(pi0)->count   += (pi1)->count;             \
	(pi0)->s_time  += (pi1)->s_time;            \
	(pi0)->us_time += (pi1)->us_time;           \
	r = (pi0)->us_time / 1000000;               \
	(pi0)->s_time  += r;                        \
	(pi0)->us_time  = (pi0)->us_time % 1000000; \
    } while(0)

static void bp_hash_init(bp_time_hash_t *hash, Uint n);
static void bp_hash_rehash(bp_time_hash_t *hash, Uint n);
static ERTS_INLINE bp_data_time_item_t * bp_hash_get(bp_time_hash_t *hash, bp_data_time_item_t *sitem);
static ERTS_INLINE bp_data_time_item_t * bp_hash_put(bp_time_hash_t *hash, bp_data_time_item_t *sitem);
static void bp_hash_delete(bp_time_hash_t *hash);


/* *************************************************************************
** External interfaces
*/

erts_smp_spinlock_t erts_bp_lock;

void 
erts_bp_init(void) {
    erts_smp_spinlock_init(&erts_bp_lock, "breakpoints");
}

int 
erts_set_trace_break(Eterm mfa[3], int specified, Binary *match_spec,
		     Eterm tracer_pid) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, match_spec,
		     (BeamInstr) BeamOp(op_i_trace_breakpoint), 0, tracer_pid);
}

int 
erts_set_mtrace_break(Eterm mfa[3], int specified, Binary *match_spec,
		      Eterm tracer_pid) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, match_spec,
		     (BeamInstr) BeamOp(op_i_mtrace_breakpoint), 0, tracer_pid);
}

/* set breakpoint data for on exported bif entry */

void
erts_set_mtrace_bif(BeamInstr *pc, Binary *match_spec, Eterm tracer_pid) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    set_function_break(NULL, pc, BREAK_IS_BIF, match_spec, (BeamInstr) BeamOp(op_i_mtrace_breakpoint), 0, tracer_pid);
}

void erts_set_time_trace_bif(BeamInstr *pc, enum erts_break_op count_op) {
    set_function_break(NULL, pc, BREAK_IS_BIF, NULL, (BeamInstr) BeamOp(op_i_time_breakpoint), count_op, NIL);
}

void erts_clear_time_trace_bif(BeamInstr *pc) {
    clear_function_break(NULL, pc, BREAK_IS_BIF, (BeamInstr) BeamOp(op_i_time_breakpoint));
}

int 
erts_set_debug_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, NULL, 
		     (BeamInstr) BeamOp(op_i_debug_breakpoint), 0, NIL);
}

int 
erts_set_count_break(Eterm mfa[3], int specified, enum erts_break_op count_op) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, NULL, 
		     (BeamInstr) BeamOp(op_i_count_breakpoint), count_op, NIL);
}

int
erts_set_time_break(Eterm mfa[3], int specified, enum erts_break_op count_op) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, NULL,
		     (BeamInstr) BeamOp(op_i_time_breakpoint), count_op, NIL);
}

int
erts_clear_trace_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, 
		       (BeamInstr) BeamOp(op_i_trace_breakpoint));
}

int
erts_clear_mtrace_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, 
		       (BeamInstr) BeamOp(op_i_mtrace_breakpoint));
}

void
erts_clear_mtrace_bif(BeamInstr *pc) {
    clear_function_break(NULL, pc, BREAK_IS_BIF, (BeamInstr) BeamOp(op_i_mtrace_breakpoint));
}

int
erts_clear_debug_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, 
		       (BeamInstr) BeamOp(op_i_debug_breakpoint));
}

int
erts_clear_count_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, 
		       (BeamInstr) BeamOp(op_i_count_breakpoint));
}

int
erts_clear_time_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified,
		       (BeamInstr) BeamOp(op_i_time_breakpoint));
}

int
erts_clear_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, 0);
}

int 
erts_clear_module_break(Module *modp) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    ASSERT(modp);
    return clear_module_break(modp, NULL, 0, 0);
}

int
erts_clear_function_break(Module *modp, BeamInstr *pc) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    ASSERT(modp);
    return clear_function_break(modp, pc, BREAK_IS_ERL, 0);
}



/*
 * SMP NOTE: Process p may have become exiting on return!
 */
BeamInstr
erts_trace_break(Process *p, BeamInstr *pc, Eterm *args,
		 Uint32 *ret_flags, Eterm *tracer_pid) {
    Eterm tpid1, tpid2;
    BpData **bds = (BpData **) (pc)[-4];
    BpDataTrace *bdt = NULL;

    ASSERT(bds);
    ASSERT(pc[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
    bdt = (BpDataTrace *) bds[bp_sched2ix_proc(p)];
    ASSERT(bdt);
    bdt = (BpDataTrace *) bdt->next;
    ASSERT(bdt);
    ASSERT(ret_flags);
    ASSERT(tracer_pid);

    ErtsSmpBPLock(bdt);
    tpid1 = tpid2 = bdt->tracer_pid;
    ErtsSmpBPUnlock(bdt);

    *ret_flags = erts_call_trace(p, pc-3/*mfa*/, bdt->match_spec, args,
				 1, &tpid2);
    *tracer_pid = tpid2;
    if (tpid1 != tpid2) {
	ErtsSmpBPLock(bdt);
	bdt->tracer_pid = tpid2;
	ErtsSmpBPUnlock(bdt);
    }
    bds[bp_sched2ix_proc(p)] = (BpData *) bdt;
    return bdt->orig_instr;
}



/*
 * SMP NOTE: Process p may have become exiting on return!
 */
Uint32
erts_bif_mtrace(Process *p, BeamInstr *pc, Eterm *args, int local,
		Eterm *tracer_pid) {
    BpData **bds = (BpData **) (pc)[-4];
    BpDataTrace *bdt = NULL;


    ASSERT(tracer_pid);
    if (bds) {
	Eterm tpid1, tpid2;
	Uint32 flags;
	bdt = (BpDataTrace *)bds[bp_sched2ix_proc(p)];

	ErtsSmpBPLock(bdt);
	tpid1 = tpid2 = bdt->tracer_pid;
	ErtsSmpBPUnlock(bdt);

	flags = erts_call_trace(p, pc-3/*mfa*/, bdt->match_spec, args,
				local, &tpid2);
	*tracer_pid = tpid2;
	if (tpid1 != tpid2) {
	    ErtsSmpBPLock(bdt);
	    bdt->tracer_pid = tpid2;
	    ErtsSmpBPUnlock(bdt);
	}
	return flags;
    }
    *tracer_pid = NIL;
    return 0;
}



int 
erts_is_trace_break(BeamInstr *pc, Binary **match_spec_ret, Eterm *tracer_pid_ret) {
    BpDataTrace *bdt = 
	(BpDataTrace *) is_break(pc, (BeamInstr) BeamOp(op_i_trace_breakpoint));
    
    if (bdt) {
	if (match_spec_ret) {
	    *match_spec_ret = bdt->match_spec;
	}
	if (tracer_pid_ret) {
	    ErtsSmpBPLock(bdt);
	    *tracer_pid_ret = bdt->tracer_pid;
	    ErtsSmpBPUnlock(bdt);
	}
	return !0;
    }
    return 0;
}

int 
erts_is_mtrace_break(BeamInstr *pc, Binary **match_spec_ret, Eterm *tracer_pid_ret) {
    BpDataTrace *bdt = 
	(BpDataTrace *) is_break(pc, (BeamInstr) BeamOp(op_i_mtrace_breakpoint));
    
    if (bdt) {
	if (match_spec_ret) {
	    *match_spec_ret = bdt->match_spec;
	}
	if (tracer_pid_ret) {
	    ErtsSmpBPLock(bdt);
	    *tracer_pid_ret = bdt->tracer_pid;
	    ErtsSmpBPUnlock(bdt);
	}
	return !0;
    }
    return 0;
}

int
erts_is_native_break(BeamInstr *pc) {
#ifdef HIPE
    ASSERT(pc[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
    return pc[0]  == (BeamInstr) BeamOp(op_hipe_trap_call)
	|| pc[0]  == (BeamInstr) BeamOp(op_hipe_trap_call_closure);
#else
    return 0;
#endif
}

int 
erts_is_count_break(BeamInstr *pc, Sint *count_ret) {
    BpDataCount *bdc = 
	(BpDataCount *) is_break(pc, (BeamInstr) BeamOp(op_i_count_breakpoint));
    
    if (bdc) {
	if (count_ret) {
	    *count_ret = (Sint) erts_smp_atomic_read_nob(&bdc->acount);
	}
	return !0;
    }
    return 0;
}

int erts_is_time_break(Process *p, BeamInstr *pc, Eterm *retval) {
    Uint i, ix;
    bp_time_hash_t hash;
    Uint size;
    Eterm *hp, t;
    bp_data_time_item_t *item = NULL;
    BpDataTime *bdt = (BpDataTime *) is_break(pc, (BeamInstr) BeamOp(op_i_time_breakpoint));

    if (bdt) {
	if (retval) {
	    /* collect all hashes to one hash */
	    bp_hash_init(&hash, 64);
	    /* foreach threadspecific hash */
	    for (i = 0; i < bdt->n; i++) {
		bp_data_time_item_t *sitem;

	        /* foreach hash bucket not NIL*/
		for(ix = 0; ix < bdt->hash[i].n; ix++) {
		    item = &(bdt->hash[i].item[ix]);
		    if (item->pid != NIL) {
			sitem = bp_hash_get(&hash, item);
			if (sitem) {
			    BP_TIME_ADD(sitem, item);
			} else {
			    bp_hash_put(&hash, item);
			}
		    }
		}
	    }
	    /* *retval should be NIL or term from previous bif in export entry */

	    if (hash.used > 0) {
		size = (5 + 2)*hash.used;
		hp   = HAlloc(p, size);

		for(ix = 0; ix < hash.n; ix++) {
		    item = &(hash.item[ix]);
		    if (item->pid != NIL) {
			t = TUPLE4(hp, item->pid,
				make_small(item->count),
				make_small(item->s_time),
				make_small(item->us_time));
			hp += 5;
			*retval = CONS(hp, t, *retval); hp += 2;
		    }
		}
	    }
	    bp_hash_delete(&hash);
	}
	return !0;
    }

    return 0;
}


BeamInstr *
erts_find_local_func(Eterm mfa[3]) {
    Module *modp;
    BeamInstr** code_base;
    BeamInstr* code_ptr;
    Uint i,n;

    if ((modp = erts_get_module(mfa[0], erts_active_code_ix())) == NULL)
	return NULL;
    if ((code_base = (BeamInstr **) modp->curr.code) == NULL)
	return NULL;
    n = (BeamInstr) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	ASSERT(((BeamInstr) BeamOp(op_i_func_info_IaaI)) == code_ptr[0]);
	ASSERT(mfa[0] == ((Eterm) code_ptr[2]) ||
	       is_nil((Eterm) code_ptr[2]));
	if (mfa[1] == ((Eterm) code_ptr[3]) &&
	    ((BeamInstr) mfa[2]) == code_ptr[4]) {
	    return code_ptr + 5;
	}
    }
    return NULL;
}

static void bp_hash_init(bp_time_hash_t *hash, Uint n) {
    Uint size = sizeof(bp_data_time_item_t)*n;
    Uint i;

    hash->n    = n;
    hash->used = 0;

    hash->item = (bp_data_time_item_t *)Alloc(size);
    sys_memzero(hash->item, size);

    for(i = 0; i < n; ++i) {
	hash->item[i].pid = NIL;
    }
}

static void bp_hash_rehash(bp_time_hash_t *hash, Uint n) {
    bp_data_time_item_t *item = NULL;
    Uint size = sizeof(bp_data_time_item_t)*n;
    Uint ix;
    Uint hval;

    item = (bp_data_time_item_t *)Alloc(size);
    sys_memzero(item, size);

    for( ix = 0; ix < n; ++ix) {
	item[ix].pid = NIL;
    }

    /* rehash, old hash -> new hash */

    for( ix = 0; ix < hash->n; ix++) {
	if (hash->item[ix].pid != NIL) {

	    hval = ((hash->item[ix].pid) >> 4) % n; /* new n */

	    while (item[hval].pid != NIL) {
		hval = (hval + 1) % n;
	    }
	    item[hval].pid     = hash->item[ix].pid;
	    item[hval].count   = hash->item[ix].count;
	    item[hval].s_time  = hash->item[ix].s_time;
	    item[hval].us_time = hash->item[ix].us_time;
	}
    }

    Free(hash->item);
    hash->n = n;
    hash->item = item;
}
static ERTS_INLINE bp_data_time_item_t * bp_hash_get(bp_time_hash_t *hash, bp_data_time_item_t *sitem) {
    Eterm pid = sitem->pid;
    Uint hval = (pid >> 4) % hash->n;
    bp_data_time_item_t *item = NULL;

    item = hash->item;

    while (item[hval].pid != pid) {
	if (item[hval].pid == NIL) return NULL;
	hval = (hval + 1) % hash->n;
    }

    return &(item[hval]);
}

static ERTS_INLINE bp_data_time_item_t * bp_hash_put(bp_time_hash_t *hash, bp_data_time_item_t* sitem) {
    Uint hval;
    float r = 0.0;
    bp_data_time_item_t *item;

    /* make sure that the hash is not saturated */
    /* if saturated, rehash it */

    r = hash->used / (float) hash->n;

    if (r > 0.7f) {
	bp_hash_rehash(hash, hash->n * 2);
    }
    /* Do hval after rehash */
    hval = (sitem->pid >> 4) % hash->n;

    /* find free slot */
    item = hash->item;

    while (item[hval].pid != NIL) {
	hval = (hval + 1) % hash->n;
    }
    item = &(hash->item[hval]);

    item->pid     = sitem->pid;
    item->s_time  = sitem->s_time;
    item->us_time = sitem->us_time;
    item->count   = sitem->count;
    hash->used++;

    return item;
}

static void bp_hash_delete(bp_time_hash_t *hash) {
    hash->n = 0;
    hash->used = 0;
    Free(hash->item);
    hash->item = NULL;
}

static void bp_time_diff(bp_data_time_item_t *item, /* out */
	process_breakpoint_time_t *pbt,             /* in  */
	Uint ms, Uint s, Uint us) {
    int ds,dus;
#ifdef DEBUG
    int dms;


    dms = ms - pbt->ms;
#endif
    ds  = s  - pbt->s;
    dus = us - pbt->us;

    /* get_sys_now may return zero difftime,
     * this is ok.
     */

#ifdef DEBUG
    ASSERT(dms >= 0 || ds >= 0 || dus >= 0);
#endif

    if (dus < 0) {
	dus += 1000000;
	ds  -= 1;
    }
    if (ds < 0) {
	ds += 1000000;
    }

    item->s_time  = ds;
    item->us_time = dus;
}

void erts_schedule_time_break(Process *p, Uint schedule) {
    Uint ms, s, us;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;

    ASSERT(p);

    pbt = ERTS_PROC_GET_CALL_TIME(p);

    if (pbt) {

	switch(schedule) {
	case ERTS_BP_CALL_TIME_SCHEDULE_EXITING :
	    break;
	case ERTS_BP_CALL_TIME_SCHEDULE_OUT :
	    /* When a process is scheduled _out_,
	     * timestamp it and add its delta to
	     * the previous breakpoint.
	     */

	    pbdt = (BpDataTime *) get_break(p, pbt->pc, (BeamInstr) BeamOp(op_i_time_breakpoint));
	    if (pbdt) {
		get_sys_now(&ms,&s,&us);
		bp_time_diff(&sitem, pbt, ms, s, us);
		sitem.pid   = p->id;
		sitem.count = 0;

		h = &(pbdt->hash[bp_sched2ix_proc(p)]);

		ASSERT(h);
		ASSERT(h->item);

		item = bp_hash_get(h, &sitem);
		if (!item) {
		    item = bp_hash_put(h, &sitem);
		} else {
		    BP_TIME_ADD(item, &sitem);
		}
	    }
	    break;
	case ERTS_BP_CALL_TIME_SCHEDULE_IN :
	    /* When a process is scheduled _in_,
	     * timestamp it and remove the previous
	     * timestamp in the psd.
	     */
	    get_sys_now(&ms,&s,&us);
	    pbt->ms = ms;
	    pbt->s  = s;
	    pbt->us = us;
	    break;
	default :
	    ASSERT(0);
		/* will never happen */
	    break;
	}
    } /* pbt */
}

/* call_time breakpoint
 * Accumulated times are added to the previous bp,
 * not the current one. The current one is saved
 * for future reference.
 * The previous breakpoint is stored in the process it self, the psd.
 * We do not need to store in a stack frame.
 * There is no need for locking, each thread has its own
 * area in each bp to save data.
 * Since we need to diffrentiate between processes for each bp,
 * every bp has a hash (per thread) to process-bp statistics.
 * - egil
 */

void erts_trace_time_break(Process *p, BeamInstr *pc, BpDataTime *bdt, Uint type) {
    Uint ms,s,us;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;

    ASSERT(p);
    ASSERT(ERTS_PSFLG_RUNNING & erts_smp_atomic32_read_acqb(&p->state));

    /* get previous timestamp and breakpoint
     * from the process psd  */

    pbt = ERTS_PROC_GET_CALL_TIME(p);
    get_sys_now(&ms,&s,&us);

    switch(type) {
	    /* get pbt
	     * timestamp = t0
	     * lookup bdt from code
	     * set ts0 to pbt
	     * add call count here?
	     */
	case ERTS_BP_CALL_TIME_CALL:
	case ERTS_BP_CALL_TIME_TAIL_CALL:

	    if (pbt) {
		ASSERT(pbt->pc);
		/* add time to previous code */
		bp_time_diff(&sitem, pbt, ms, s, us);
		sitem.pid   = p->id;
		sitem.count = 0;

		/* previous breakpoint */
		pbdt = (BpDataTime *) get_break(p, pbt->pc, (BeamInstr) BeamOp(op_i_time_breakpoint));

		/* if null then the breakpoint was removed */
		if (pbdt) {
		    h = &(pbdt->hash[bp_sched2ix_proc(p)]);

		    ASSERT(h);
		    ASSERT(h->item);

		    item = bp_hash_get(h, &sitem);
		    if (!item) {
			item = bp_hash_put(h, &sitem);
		    } else {
			BP_TIME_ADD(item, &sitem);
		    }
		}

	    } else {
		/* first call of process to instrumented function */
		pbt = Alloc(sizeof(process_breakpoint_time_t));
		(void *) ERTS_PROC_SET_CALL_TIME(p, ERTS_PROC_LOCK_MAIN, pbt);
	    }
	    /* add count to this code */
	    sitem.pid     = p->id;
	    sitem.count   = 1;
	    sitem.s_time  = 0;
	    sitem.us_time = 0;

	    /* this breakpoint */
	    ASSERT(bdt);
	    h = &(bdt->hash[bp_sched2ix_proc(p)]);

	    ASSERT(h);
	    ASSERT(h->item);

	    item = bp_hash_get(h, &sitem);
	    if (!item) {
		item = bp_hash_put(h, &sitem);
	    } else {
		BP_TIME_ADD(item, &sitem);
	    }

	    pbt->pc = pc;
	    pbt->ms = ms;
	    pbt->s  = s;
	    pbt->us = us;
	    break;

	case ERTS_BP_CALL_TIME_RETURN:
	    /* get pbt
	     * lookup bdt from code
	     * timestamp = t1
	     * get ts0 from pbt
	     * get item from bdt->hash[bp_hash(p->id)]
	     * ack diff (t1, t0) to item
	     */

	    if(pbt) {
		/* might have been removed due to
		 * trace_pattern(false)
		 */
		ASSERT(pbt->pc);

		bp_time_diff(&sitem, pbt, ms, s, us);
		sitem.pid   = p->id;
		sitem.count = 0;

		/* previous breakpoint */
		pbdt = (BpDataTime *) get_break(p, pbt->pc, (BeamInstr) BeamOp(op_i_time_breakpoint));

		/* beware, the trace_pattern might have been removed */
		if (pbdt) {
		    h = &(pbdt->hash[bp_sched2ix_proc(p)]);

		    ASSERT(h);
		    ASSERT(h->item);

		    item = bp_hash_get(h, &sitem);
		    if (!item) {
			item = bp_hash_put(h, &sitem);
		    } else {
			BP_TIME_ADD(item, &sitem);
		    }
		}

		pbt->pc = pc;
		pbt->ms = ms;
		pbt->s  = s;
		pbt->us = us;
	    }
	    break;
	default :
	    ASSERT(0);
		/* will never happen */
	    break;
    }
}


/* *************************************************************************
** Local helpers
*/


static int set_break(Eterm mfa[3], int specified, 
		     Binary *match_spec, BeamInstr break_op,
		     enum erts_break_op count_op, Eterm tracer_pid)
{
    Module *modp;
    int num_processed = 0;
    ErtsCodeIndex code_ix = erts_active_code_ix();
    if (!specified) {
	/* Find and process all modules in the system... */
	int current;
	int last = module_code_size(code_ix);
	for (current = 0; current < last; current++) {
	    modp = module_code(current, code_ix);
	    ASSERT(modp != NULL);
	    num_processed += 
		set_module_break(modp, mfa, specified, 
				 match_spec, break_op, count_op, 
				 tracer_pid);
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0], code_ix)) != NULL) {
	    num_processed += 
		set_module_break(modp, mfa, specified, 
				 match_spec, break_op, count_op, 
				 tracer_pid);
	}	
    }
    return num_processed;
}

static int set_module_break(Module *modp, Eterm mfa[3], int specified,
			    Binary *match_spec, BeamInstr break_op,
			    enum erts_break_op count_op, Eterm tracer_pid) {
    BeamInstr** code_base;
    BeamInstr* code_ptr;
    int num_processed = 0;
    Uint i,n;

    ASSERT(break_op);
    ASSERT(modp);
    code_base = (BeamInstr **) modp->curr.code;
    if (code_base == NULL) {
	return 0;
    }
    n = (BeamInstr) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	ASSERT(code_ptr[0] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
	if ((specified < 2 || mfa[1] == ((Eterm) code_ptr[3])) &&
	    (specified < 3 || ((int) mfa[2]) == ((int) code_ptr[4]))) {
	    BeamInstr *pc = code_ptr+5;
	    
	    num_processed +=
		set_function_break(modp, pc, BREAK_IS_ERL, match_spec,
				   break_op, count_op, tracer_pid);
	}
    }
    return num_processed;
}

static int set_function_break(Module *modp, BeamInstr *pc, int bif,
			      Binary *match_spec, BeamInstr break_op,
			      enum erts_break_op count_op, Eterm tracer_pid) {

    BeamInstr **code_base = NULL;
    BpData *bd, **r, ***rs;
    size_t size;
    Uint ix = 0;
    
    if (bif == BREAK_IS_ERL) {
	code_base = (BeamInstr **)modp->curr.code;
	ASSERT(code_base);
	ASSERT(code_base <= (BeamInstr **)pc);
	ASSERT((BeamInstr **)pc < code_base + (modp->curr.code_length/sizeof(BeamInstr *)));
    } else {
	ASSERT(*pc == (BeamInstr) em_apply_bif);
	ASSERT(modp == NULL);
    }

    /*
     * Currently no trace support for native code.
     */
    if (erts_is_native_break(pc)) {
	return 0;
    }
    /* Do not allow two breakpoints of the same kind */
    if ( (bd = is_break(pc, break_op))) {
	if (break_op == (BeamInstr) BeamOp(op_i_trace_breakpoint)
	    || break_op == (BeamInstr) BeamOp(op_i_mtrace_breakpoint)) {

	    BpDataTrace *bdt = (BpDataTrace *) bd;
	    Binary *old_match_spec;
	    
	    /* Update match spec and tracer */
	    MatchSetRef(match_spec);
	    ErtsSmpBPLock(bdt);
	    old_match_spec = bdt->match_spec;
	    bdt->match_spec = match_spec;
	    bdt->tracer_pid = tracer_pid;
	    ErtsSmpBPUnlock(bdt);
	    MatchSetUnref(old_match_spec);
	} else {
	    BpDataCount *bdc = (BpDataCount *) bd;
	    erts_aint_t count = 0;
	    erts_aint_t res   = 0;

	    ASSERT(! match_spec);
	    ASSERT(is_nil(tracer_pid));
		
	    if (break_op == (BeamInstr) BeamOp(op_i_count_breakpoint)) {
		if (count_op == erts_break_stop) {
		    count = erts_smp_atomic_read_nob(&bdc->acount);
		    if (count >= 0) {
			while(1) {
			    res = erts_smp_atomic_cmpxchg_nob(&bdc->acount, -count - 1, count);
			    if ((res == count) || count < 0) break;
			    count = res;
			}
		    }
		} else {
		    /* Reset call counter */
		    erts_smp_atomic_set_nob(&bdc->acount, 0);
		}

	    } else if (break_op == (BeamInstr) BeamOp(op_i_time_breakpoint)) {
		BpDataTime *bdt = (BpDataTime *) bd;
		Uint i = 0;

		ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());

		if (count_op == erts_break_stop) {
		    bdt->pause = 1;
		} else {
		    bdt->pause = 0;
		    for (i = 0; i < bdt->n; i++) {
			bp_hash_delete(&(bdt->hash[i]));
			bp_hash_init(&(bdt->hash[i]), 32);
		    }
		}

	    } else {
		ASSERT (! count_op);
	    }
	}
	return 1;
    }
    if (break_op == (BeamInstr) BeamOp(op_i_trace_breakpoint) ||
	break_op == (BeamInstr) BeamOp(op_i_mtrace_breakpoint)) {
	size = sizeof(BpDataTrace);
    } else {
	ASSERT(! match_spec);
	ASSERT(is_nil(tracer_pid));
	if (break_op == (BeamInstr) BeamOp(op_i_count_breakpoint)) {
	    if (count_op == erts_break_reset || count_op == erts_break_stop) {
		/* Do not insert a new breakpoint */
		return 1;
	    }
	    size = sizeof(BpDataCount);
	} else if (break_op == (BeamInstr) BeamOp(op_i_time_breakpoint))  {
	    if (count_op == erts_break_reset || count_op == erts_break_stop) {
		/* Do not insert a new breakpoint */
		return 1;
	    }
	    size = sizeof(BpDataTime);
	} else {
	    ASSERT(! count_op);
	    ASSERT(break_op == (BeamInstr) BeamOp(op_i_debug_breakpoint));
	    size = sizeof(BpDataDebug);
	}
    }
    rs = (BpData ***) (pc-4);
    if (! *rs) {
	size_t ssize = sizeof(BeamInstr) * erts_no_schedulers;
	*rs = (BpData **) Alloc(ssize);
	sys_memzero(*rs, ssize);
    }

    r = &((*rs)[0]);

    if (! *r) {
	ASSERT(*pc != (BeamInstr) BeamOp(op_i_trace_breakpoint));
	ASSERT(*pc != (BeamInstr) BeamOp(op_i_mtrace_breakpoint));
	ASSERT(*pc != (BeamInstr) BeamOp(op_i_debug_breakpoint));
	ASSERT(*pc != (BeamInstr) BeamOp(op_i_count_breakpoint));
	ASSERT(*pc != (BeamInstr) BeamOp(op_i_time_breakpoint));
	/* First breakpoint; create singleton ring */
	bd = Alloc(size);
	BpInit(bd, *pc);
	*r = bd;
	if (bif == BREAK_IS_ERL) {
	    *pc = break_op;
	}
    } else {
	ASSERT(*pc == (BeamInstr) BeamOp(op_i_trace_breakpoint) ||
	       *pc == (BeamInstr) BeamOp(op_i_mtrace_breakpoint) ||
	       *pc == (BeamInstr) BeamOp(op_i_debug_breakpoint) ||
	       *pc == (BeamInstr) BeamOp(op_i_time_breakpoint) ||
	       *pc == (BeamInstr) BeamOp(op_i_count_breakpoint) ||
	       *pc == (BeamInstr) em_apply_bif);
	if (*pc == (BeamInstr) BeamOp(op_i_debug_breakpoint)) {
	    /* Debug bp must be last, so if it is also first; 
	     * it must be singleton. */
	    ASSERT(BpSingleton(*r));
	    /* Insert new bp first in the ring, i.e second to last. */
	    bd = Alloc(size);
	    BpInitAndSpliceNext(bd, *pc, *r);
	    if (bif == BREAK_IS_ERL) {
		*pc = break_op;
	    }
	} else if ((*r)->prev->orig_instr
		   == (BeamInstr) BeamOp(op_i_debug_breakpoint)) {
	    /* Debug bp last in the ring; insert new second to last. */
	    bd = Alloc(size);
	    BpInitAndSplicePrev(bd, (*r)->prev->orig_instr, *r);
	    (*r)->prev->orig_instr = break_op;
	} else {
	    /* Just insert last in the ring */
	    bd = Alloc(size);
	    BpInitAndSpliceNext(bd, (*r)->orig_instr, *r);
	    (*r)->orig_instr = break_op;
	    *r = bd;
	}
    }
    for (ix = 1; ix < erts_no_schedulers; ++ix) {
	(*rs)[ix] = (*rs)[0];
    }

    bd->this_instr = break_op;
    /* Init the bp type specific data */
    if (break_op == (BeamInstr) BeamOp(op_i_trace_breakpoint) ||
	break_op == (BeamInstr) BeamOp(op_i_mtrace_breakpoint)) {
		
	BpDataTrace *bdt = (BpDataTrace *) bd;
		
	MatchSetRef(match_spec);
	bdt->match_spec = match_spec;
	bdt->tracer_pid = tracer_pid;
    } else if (break_op == (BeamInstr) BeamOp(op_i_time_breakpoint)) {
	BpDataTime *bdt = (BpDataTime *) bd;
	Uint i = 0;

	bdt->pause = 0;
	bdt->n     = erts_no_schedulers;
	bdt->hash  = Alloc(sizeof(bp_time_hash_t)*(bdt->n));

	for (i = 0; i < bdt->n; i++) {
	    bp_hash_init(&(bdt->hash[i]), 32);
	}
    } else if (break_op == (BeamInstr) BeamOp(op_i_count_breakpoint)) {
	BpDataCount *bdc = (BpDataCount *) bd;
	erts_smp_atomic_init_nob(&bdc->acount, 0);
    }

    if (bif == BREAK_IS_ERL) {
	++modp->curr.num_breakpoints;
    }
    return 1;
}

static int clear_break(Eterm mfa[3], int specified, BeamInstr break_op)
{
    ErtsCodeIndex code_ix = erts_active_code_ix();
    int num_processed = 0;
    Module *modp;

    if (!specified) {
	/* Iterate over all modules */
	int current;
	int last = module_code_size(code_ix);

	for (current = 0; current < last; current++) {
	    modp = module_code(current, code_ix);
	    ASSERT(modp != NULL);
	    num_processed += clear_module_break(modp, mfa, specified, break_op);
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0], code_ix)) != NULL) {
	    num_processed += 
		clear_module_break(modp, mfa, specified, break_op);
	}	
    }
    return num_processed;
}

static int clear_module_break(Module *m, Eterm mfa[3], int specified, 
			      BeamInstr break_op) {
    BeamInstr** code_base;
    BeamInstr* code_ptr;
    int num_processed = 0;
    Uint i;
    BeamInstr n;
    
    ASSERT(m);
    code_base = (BeamInstr **) m->curr.code;
    if (code_base == NULL) {
	return 0;
    }
    n = (BeamInstr) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	if ((specified < 2 || mfa[1] == ((Eterm) code_ptr[3])) &&
	    (specified < 3 || ((int) mfa[2]) == ((int) code_ptr[4]))) {
	    BeamInstr *pc = code_ptr + 5;
	    
	    num_processed += 
		clear_function_break(m, pc, BREAK_IS_ERL, break_op);
	}
    }
    return num_processed;
}

static int clear_function_break(Module *m, BeamInstr *pc, int bif, BeamInstr break_op) {
    BpData *bd;
    Uint ix = 0;
    BeamInstr **code_base = NULL;

    if (bif == BREAK_IS_ERL) {
	code_base = (BeamInstr **)m->curr.code;
	ASSERT(code_base);
	ASSERT(code_base <= (BeamInstr **)pc);
	ASSERT((BeamInstr **)pc < code_base + (m->curr.code_length/sizeof(BeamInstr *)));
    } else {
	ASSERT(*pc == (BeamInstr) em_apply_bif);
	ASSERT(m == NULL);
    }

    /*
     * Currently no trace support for native code.
     */
    if (erts_is_native_break(pc)) {
	return 0;
    }

    while ( (bd = is_break(pc, break_op))) {
	/* Remove all breakpoints of this type.
	 * There should be only one of each type, 
	 * but break_op may be 0 which matches any type. 
	 */
	BeamInstr op;
	BpData ***rs = (BpData ***) (pc - 4);
	BpData   **r = NULL;

#ifdef DEBUG
	for (ix = 1; ix < erts_no_schedulers; ++ix) {
	    ASSERT((*rs)[ix] == (*rs)[0]);
	}
#endif
	
	r = &((*rs)[0]);

	ASSERT(*r);
	/* Find opcode for this breakpoint */
	if (break_op) {
	    op = break_op;
	} else {
	    if (bd == (*r)->next) {
		/* First breakpoint in ring */
		op = *pc;
	    } else {
		op = bd->prev->orig_instr;
	    }
	}
	if (BpSingleton(bd)) {
	    ASSERT(*r == bd);
	    /* Only one breakpoint to remove */
	    if (bif == BREAK_IS_ERL) {
		*pc = bd->orig_instr;
	    }
	    Free(*rs);
	    *rs = NULL;
	} else {
	    BpData *bd_prev = bd->prev;
	    
	    BpSpliceNext(bd, bd_prev);
	    ASSERT(BpSingleton(bd));
	    if (bd == *r) {
		/* We removed the last breakpoint in the ring */
		*r = bd_prev;
		bd_prev->orig_instr = bd->orig_instr;
	    } else if (bd_prev == *r) {
		/* We removed the first breakpoint in the ring */
		if (bif == BREAK_IS_ERL) {
		    *pc = bd->orig_instr;
		}
	    } else {
		bd_prev->orig_instr = bd->orig_instr;
	    }
	}
	if (op == (BeamInstr) BeamOp(op_i_trace_breakpoint) ||
	    op == (BeamInstr) BeamOp(op_i_mtrace_breakpoint)) {
	    
	    BpDataTrace *bdt = (BpDataTrace *) bd;
	    MatchSetUnref(bdt->match_spec);
	}
	if (op == (BeamInstr) BeamOp(op_i_time_breakpoint)) {
	    BpDataTime *bdt = (BpDataTime *) bd;
	    Uint i = 0;
	    Uint j = 0;
	    Process *h_p = NULL;
	    bp_data_time_item_t *item = NULL;
	    process_breakpoint_time_t *pbt = NULL;

	    /* remove all psd associated with the hash
	     * and then delete the hash.
	     * ... sigh ...
	     */

	    for( i = 0; i < bdt->n; ++i) {
		if (bdt->hash[i].used) {
		    for (j = 0; j < bdt->hash[i].n; ++j) {
			item = &(bdt->hash[i].item[j]);
			if (item->pid != NIL) {
			    h_p = erts_proc_lookup(item->pid);
			    if (h_p) {
				pbt = ERTS_PROC_SET_CALL_TIME(h_p, ERTS_PROC_LOCK_MAIN, NULL);
				if (pbt) {
				    Free(pbt);
				}
			    }
			}
		    }
		}
		bp_hash_delete(&(bdt->hash[i]));
	    }
	    Free(bdt->hash);
	    bdt->hash = NULL;
	    bdt->n = 0;
	}
	Free(bd);
	if (bif == BREAK_IS_ERL) {
	    ASSERT(m->curr.num_breakpoints > 0);
	    --m->curr.num_breakpoints;
	}
	if (*rs) {
	    for (ix = 1; ix < erts_no_schedulers; ++ix) {
		(*rs)[ix] = (*rs)[0];
	    }
	}
    } /* while bd != NULL */
    return 1;
}



/*
** Searches (linear forward) the breakpoint ring for a specified opcode
** and returns a pointer to the breakpoint data structure or NULL if
** not found. If the specified opcode is 0, the last breakpoint is 
** returned. The program counter must point to the first executable
** (breakpoint) instruction of the function.
*/

BpData *erts_get_time_break(Process *p, BeamInstr *pc) {
    return get_break(p, pc, (BeamInstr) BeamOp(op_i_time_breakpoint));
}

static BpData *get_break(Process *p, BeamInstr *pc, BeamInstr break_op) {
    ASSERT(pc[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
    if (! erts_is_native_break(pc)) {
	BpData **rs = (BpData **) pc[-4];
	BpData  *bd = NULL, *ebd = NULL;

	if (! rs) {
	    return NULL;
	}

	bd = ebd = rs[bp_sched2ix_proc(p)];
	ASSERT(bd);
	if (bd->this_instr == break_op) {
	    return bd;
	}

	bd = bd->next;
	while (bd != ebd) {
	    ASSERT(bd);
	    if (bd->this_instr == break_op) {
		ASSERT(bd);
		return bd;
	    }
	    bd = bd->next;
	}
    }
    return NULL;
}

static BpData *is_break(BeamInstr *pc, BeamInstr break_op) {
    BpData **rs;
    BpData  *bd = NULL, *ebd = NULL;
    ASSERT(pc[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));

    if (erts_is_native_break(pc)) {
	return NULL;
    }
    rs = (BpData **) pc[-4];
    if (! rs) {
	return NULL;
    }

    bd = ebd = rs[erts_bp_sched2ix()];
    ASSERT(bd);
    if ( (break_op == 0) || (bd->this_instr == break_op)) {
	return bd;
    }

    bd = bd->next;
    while (bd != ebd) {
	ASSERT(bd);
	if (bd->this_instr == break_op) {
	    ASSERT(bd);
	    return bd;
	}
	bd = bd->next;
    }
    return NULL;
}
