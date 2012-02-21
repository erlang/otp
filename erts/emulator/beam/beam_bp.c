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

#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
#  define ERTS_SMP_REQ_PROC_MAIN_LOCK(P) \
      if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN)
#  define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P) \
      if ((P)) erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN)
#else
#  define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)
#endif

#define BREAK_IS_BIF (1)
#define BREAK_IS_ERL (0)

#define ERTS_BPF_LOCAL_TRACE       0x01
#define ERTS_BPF_META_TRACE        0x02
#define ERTS_BPF_COUNT             0x04
#define ERTS_BPF_COUNT_ACTIVE      0x08
#define ERTS_BPF_DEBUG             0x10
#define ERTS_BPF_TIME_TRACE        0x20
#define ERTS_BPF_TIME_TRACE_ACTIVE 0x40

#define ERTS_BPF_ALL               0x7F

extern Eterm beam_return_to_trace[1];   /* OpCode(i_return_to_trace) */
extern Eterm beam_return_trace[1];      /* OpCode(i_return_trace) */
extern Eterm beam_exception_trace[1];   /* OpCode(i_exception_trace) */
extern Eterm beam_return_time_trace[1]; /* OpCode(i_return_time_trace) */

/* *************************************************************************
** Local prototypes
*/

/*
** Helpers
*/
static Eterm do_call_trace(Process* c_p, BeamInstr* I, Eterm* reg,
			   Binary* ms, Eterm tracer_pid);
static int set_break(Eterm mfa[3], int specified,
		     Binary *match_spec, Uint break_flags,
		     enum erts_break_op count_op, Eterm tracer_pid);
static int set_module_break(Module *modp, Eterm mfa[3], int specified,
			    Binary *match_spec, Uint break_flags,
			    enum erts_break_op count_op, Eterm tracer_pid);
static int set_function_break(Module *modp, BeamInstr *pc, int bif,
			      Binary *match_spec, Uint break_flags,
			      enum erts_break_op count_op, Eterm tracer_pid); 

static int clear_break(Eterm mfa[3], int specified, 
		       Uint break_flags);
static int clear_module_break(Module *modp, Eterm mfa[3], int specified, 
			      Uint break_flags);
static int clear_function_break(Module *modp, BeamInstr *pc, int bif,
				Uint break_flags);

static BpDataTime* get_time_break(BeamInstr *pc);
static GenericBpData* check_break(BeamInstr *pc, Uint break_flags);
static void bp_time_diff(bp_data_time_item_t *item,
			 process_breakpoint_time_t *pbt,
			 Uint ms, Uint s, Uint us);

static void bp_count_unref(BpCount* bcp);
static void bp_time_unref(BpDataTime* bdt);

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

void 
erts_bp_init(void) {
}

int 
erts_set_trace_break(Eterm mfa[3], int specified, Binary *match_spec) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, match_spec,
		     ERTS_BPF_LOCAL_TRACE, 0, am_true);
}

int 
erts_set_mtrace_break(Eterm mfa[3], int specified, Binary *match_spec,
		      Eterm tracer_pid) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, match_spec,
		     ERTS_BPF_META_TRACE, 0, tracer_pid);
}

/* set breakpoint data for on exported bif entry */

void
erts_set_mtrace_bif(BeamInstr *pc, Binary *match_spec, Eterm tracer_pid) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    set_function_break(NULL, pc, BREAK_IS_BIF, match_spec,
		       ERTS_BPF_META_TRACE, 0, tracer_pid);
}

void erts_set_time_trace_bif(BeamInstr *pc, enum erts_break_op count_op) {
    set_function_break(NULL, pc, BREAK_IS_BIF, NULL,
		       ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE,
		       count_op, NIL);
}

void erts_clear_time_trace_bif(BeamInstr *pc) {
    clear_function_break(NULL, pc, BREAK_IS_BIF,
			 ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE);
}

int 
erts_set_debug_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, NULL, ERTS_BPF_DEBUG, 0, NIL);
}

int 
erts_set_count_break(Eterm mfa[3], int specified, enum erts_break_op count_op) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, NULL, 
		     ERTS_BPF_COUNT|ERTS_BPF_COUNT_ACTIVE, count_op, NIL);
}

int
erts_set_time_break(Eterm mfa[3], int specified, enum erts_break_op count_op) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return set_break(mfa, specified, NULL,
		     ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE,
		     count_op, NIL);
}

int
erts_clear_trace_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, ERTS_BPF_LOCAL_TRACE);
}

int
erts_clear_mtrace_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, ERTS_BPF_META_TRACE);
}

void
erts_clear_mtrace_bif(BeamInstr *pc) {
    clear_function_break(NULL, pc, BREAK_IS_BIF, ERTS_BPF_META_TRACE);
}

int
erts_clear_debug_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, ERTS_BPF_DEBUG);
}

int
erts_clear_count_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, ERTS_BPF_COUNT|ERTS_BPF_COUNT_ACTIVE);
}

int
erts_clear_time_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified,
		       ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE);
}

int
erts_clear_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    return clear_break(mfa, specified, ERTS_BPF_ALL);
}

int 
erts_clear_module_break(Module *modp) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    ASSERT(modp);
    return clear_module_break(modp, NULL, 0, ERTS_BPF_ALL);
}

int
erts_clear_function_break(Module *modp, BeamInstr *pc) {
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    ASSERT(modp);
    return clear_function_break(modp, pc, BREAK_IS_ERL, ERTS_BPF_ALL);
}

BeamInstr
erts_generic_breakpoint(Process* c_p, BeamInstr* I, Eterm* reg)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint bp_flags;

    g = (GenericBp *) I[-4];
    bp = &g->data[0];
    bp_flags = bp->flags;
    ASSERT((bp_flags & ~ERTS_BPF_ALL) == 0);
    if (bp_flags & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE) &&
	!IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	bp_flags &= ~(ERTS_BPF_LOCAL_TRACE|
		      ERTS_BPF_TIME_TRACE|
		      ERTS_BPF_TIME_TRACE_ACTIVE);
	if (bp_flags == 0) {	/* Quick exit */
	    return g->orig_instr;
	}
    }

    if (bp_flags & ERTS_BPF_LOCAL_TRACE) {
	(void) do_call_trace(c_p, I, reg, bp->local_ms, am_true);
    }

    if (bp_flags & ERTS_BPF_META_TRACE) {
	Eterm pid;

	pid = (Eterm) erts_smp_atomic_read_nob(&bp->tracer_pid);
	pid = do_call_trace(c_p, I, reg, bp->meta_ms, pid);
	erts_smp_atomic_set_nob(&bp->tracer_pid, pid);
    }

    if (bp_flags & ERTS_BPF_COUNT_ACTIVE) {
	erts_smp_atomic_inc_nob(&bp->count->acount);
    }

    if (bp_flags & ERTS_BPF_TIME_TRACE_ACTIVE) {
	Eterm w;
	erts_trace_time_call(c_p, I, bp->time);
	w = (BeamInstr) *c_p->cp;
	if (! (w == (BeamInstr) BeamOp(op_i_return_time_trace) ||
	       w == (BeamInstr) BeamOp(op_return_trace) ||
	       w == (BeamInstr) BeamOp(op_i_return_to_trace)) ) {
	    Eterm* E = c_p->stop;
	    ASSERT(c_p->htop <= E && E <= c_p->hend);
	    if (E - 2 < c_p->htop) {
		(void) erts_garbage_collect(c_p, 2, reg, I[-1]);
		ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    }
	    E = c_p->stop;

	    ASSERT(c_p->htop <= E && E <= c_p->hend);

	    E -= 2;
	    E[0] = make_cp(I);
	    E[1] = make_cp(c_p->cp);     /* original return address */
	    c_p->cp = beam_return_time_trace;
	    c_p->stop = E;
	}
    }

    if (bp_flags & ERTS_BPF_DEBUG) {
	return (BeamInstr) BeamOp(op_i_debug_breakpoint);
    } else {
	return g->orig_instr;
    }
}

static Eterm
do_call_trace(Process* c_p, BeamInstr* I, Eterm* reg,
	      Binary* ms, Eterm tracer_pid)
{
    Eterm* cpp;
    int return_to_trace = 0;
    BeamInstr w;
    BeamInstr *cp_save;
    Uint32 flags;
    Uint need = 0;
    Eterm* E = c_p->stop;

    w = *c_p->cp;
    if (w == (BeamInstr) BeamOp(op_return_trace)) {
	cpp = &E[2];
    } else if (w == (BeamInstr) BeamOp(op_i_return_to_trace)) {
	return_to_trace = 1;
	cpp = &E[0];
    } else if (w == (BeamInstr) BeamOp(op_i_return_time_trace)) {
	cpp = &E[0];
    } else {
	cpp = NULL;
    }
    if (cpp) {
	for (;;) {
	    BeamInstr w = *cp_val(*cpp);
	    if (w == (BeamInstr) BeamOp(op_return_trace)) {
		cpp += 3;
	    } else if (w == (BeamInstr) BeamOp(op_i_return_to_trace)) {
		return_to_trace = 1;
		cpp += 1;
	    } else if (w == (BeamInstr) BeamOp(op_i_return_time_trace)) {
		cpp += 2;
	    } else {
		break;
	    }
	}
	cp_save = c_p->cp;
	c_p->cp = (BeamInstr *) cp_val(*cpp);
	ASSERT(is_CP(*cpp));
    }
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
    flags = erts_call_trace(c_p, I-3, ms, reg, 1, &tracer_pid);
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    if (cpp) {
	c_p->cp = cp_save;
    }

    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    if ((flags & MATCH_SET_RETURN_TO_TRACE) && !return_to_trace) {
	need += 1;
    }
    if (flags & MATCH_SET_RX_TRACE) {
	need += 3;
    }
    if (need) {
	ASSERT(c_p->htop <= E && E <= c_p->hend);
	if (E - need < c_p->htop) {
	    (void) erts_garbage_collect(c_p, need, reg, I[-1]);
	    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    E = c_p->stop;
	}
    }
    if (flags & MATCH_SET_RETURN_TO_TRACE && !return_to_trace) {
	E -= 1;
	ASSERT(c_p->htop <= E && E <= c_p->hend);
	E[0] = make_cp(c_p->cp);
	c_p->cp = (BeamInstr *) beam_return_to_trace;
    }
    if (flags & MATCH_SET_RX_TRACE) {
	E -= 3;
	ASSERT(c_p->htop <= E && E <= c_p->hend);
	ASSERT(is_CP((Eterm) (UWord) (I - 3)));
	ASSERT(am_true == tracer_pid ||
	       is_internal_pid(tracer_pid) || is_internal_port(tracer_pid));
	E[2] = make_cp(c_p->cp);
	E[1] = tracer_pid;
	E[0] = make_cp(I - 3); /* We ARE at the beginning of an
				  instruction,
				  the funcinfo is above i. */
	c_p->cp = (flags & MATCH_SET_EXCEPTION_TRACE) ?
	    beam_exception_trace : beam_return_trace;
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	c_p->trace_flags |= F_EXCEPTION_TRACE;
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
    }
    c_p->stop = E;
    return tracer_pid;
}

void
erts_trace_time_call(Process* c_p, BeamInstr* I, BpDataTime* bdt)
{
    Uint ms,s,us;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;

    ASSERT(c_p);
    ASSERT(erts_smp_atomic32_read_acqb(&c_p->state) & ERTS_PSFLG_RUNNING);

    /* get previous timestamp and breakpoint
     * from the process psd  */

    pbt = ERTS_PROC_GET_CALL_TIME(c_p);
    get_sys_now(&ms, &s, &us);

    /* get pbt
     * timestamp = t0
     * lookup bdt from code
     * set ts0 to pbt
     * add call count here?
     */
    if (pbt == 0) {
	/* First call of process to instrumented function */
	pbt = Alloc(sizeof(process_breakpoint_time_t));
	(void *) ERTS_PROC_SET_CALL_TIME(c_p, ERTS_PROC_LOCK_MAIN, pbt);
    } else {
	ASSERT(pbt->pc);
	/* add time to previous code */
	bp_time_diff(&sitem, pbt, ms, s, us);
	sitem.pid = c_p->id;
	sitem.count = 0;

	/* previous breakpoint */
	pbdt = get_time_break(pbt->pc);

	/* if null then the breakpoint was removed */
	if (pbdt) {
	    h = &(pbdt->hash[bp_sched2ix_proc(c_p)]);

	    ASSERT(h);
	    ASSERT(h->item);

	    item = bp_hash_get(h, &sitem);
	    if (!item) {
		item = bp_hash_put(h, &sitem);
	    } else {
		BP_TIME_ADD(item, &sitem);
	    }
	}
    }

    /* Add count to this code */
    sitem.pid     = c_p->id;
    sitem.count   = 1;
    sitem.s_time  = 0;
    sitem.us_time = 0;

    /* this breakpoint */
    ASSERT(bdt);
    h = &(bdt->hash[bp_sched2ix_proc(c_p)]);

    ASSERT(h);
    ASSERT(h->item);

    item = bp_hash_get(h, &sitem);
    if (!item) {
	item = bp_hash_put(h, &sitem);
    } else {
	BP_TIME_ADD(item, &sitem);
    }

    pbt->pc = I;
    pbt->ms = ms;
    pbt->s  = s;
    pbt->us = us;
}

void
erts_trace_time_return(Process *p, BeamInstr *pc)
{
    Uint ms,s,us;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;

    ASSERT(p);
    ASSERT(erts_smp_atomic32_read_acqb(&p->state) & ERTS_PSFLG_RUNNING);

    /* get previous timestamp and breakpoint
     * from the process psd  */

    pbt = ERTS_PROC_GET_CALL_TIME(p);
    get_sys_now(&ms,&s,&us);

    /* get pbt
     * lookup bdt from code
     * timestamp = t1
     * get ts0 from pbt
     * get item from bdt->hash[bp_hash(p->id)]
     * ack diff (t1, t0) to item
     */

    if (pbt) {
	/* might have been removed due to
	 * trace_pattern(false)
	 */
	ASSERT(pbt->pc);

	bp_time_diff(&sitem, pbt, ms, s, us);
	sitem.pid   = p->id;
	sitem.count = 0;

	/* previous breakpoint */
	pbdt = get_time_break(pbt->pc);

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
}

/*
 * SMP NOTE: Process p may have become exiting on return!
 */
Uint32
erts_bif_mtrace(Process *p, BeamInstr *pc, Eterm *args, int local,
		Eterm *tracer_pid)
{
    GenericBp* g;

    ASSERT(tracer_pid);
    g = (GenericBp *) pc[-4];
    if (g) {
	Eterm tpid1, tpid2;
	Uint32 flags;
	GenericBpData* bp;

	bp = &g->data[0];
	tpid1 = tpid2 =(Eterm) erts_smp_atomic_read_nob(&bp->tracer_pid);
	flags = erts_call_trace(p, pc-3/*mfa*/, bp->meta_ms, args,
				local, &tpid2);
	*tracer_pid = tpid2;
	if (tpid1 != tpid2) {
	    erts_smp_atomic_set_nob(&bp->tracer_pid, tpid2);
	}
	return flags;
    }
    *tracer_pid = NIL;
    return 0;
}



int 
erts_is_trace_break(BeamInstr *pc, Binary **match_spec_ret)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_LOCAL_TRACE);

    if (bp) {
	if (match_spec_ret) {
	    *match_spec_ret = bp->local_ms;
	}
	return 1;
    }
    return 0;
}

int 
erts_is_mtrace_break(BeamInstr *pc, Binary **match_spec_ret,
		     Eterm *tracer_pid_ret)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_META_TRACE);
    
    if (bp) {
	if (match_spec_ret) {
	    *match_spec_ret = bp->meta_ms;
	}
	if (tracer_pid_ret) {
	    *tracer_pid_ret =
		(Eterm) erts_smp_atomic_read_nob(&bp->tracer_pid);
	}
	return 1;
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
erts_is_count_break(BeamInstr *pc, Uint *count_ret)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_COUNT);
    
    if (bp) {
	if (count_ret) {
	    *count_ret = (Uint) erts_smp_atomic_read_nob(&bp->count->acount);
	}
	return 1;
    }
    return 0;
}

int erts_is_time_break(Process *p, BeamInstr *pc, Eterm *retval) {
    Uint i, ix;
    bp_time_hash_t hash;
    Uint size;
    Eterm *hp, t;
    bp_data_time_item_t *item = NULL;
    BpDataTime *bdt = get_time_break(pc);

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
	return 1;
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

	    pbdt = get_time_break(pbt->pc);
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

/* *************************************************************************
** Local helpers
*/


static int set_break(Eterm mfa[3], int specified, 
		     Binary *match_spec, Uint break_flags,
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
				 match_spec, break_flags, count_op,
				 tracer_pid);
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0], code_ix)) != NULL) {
	    num_processed += 
		set_module_break(modp, mfa, specified, 
				 match_spec, break_flags, count_op,
				 tracer_pid);
	}	
    }
    return num_processed;
}

static int set_module_break(Module *modp, Eterm mfa[3], int specified,
			    Binary *match_spec, Uint break_flags,
			    enum erts_break_op count_op, Eterm tracer_pid) {
    BeamInstr** code_base;
    BeamInstr* code_ptr;
    int num_processed = 0;
    Uint i,n;

    ASSERT(break_flags);
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
				   break_flags, count_op, tracer_pid);
	}
    }
    return num_processed;
}

static int
set_function_break(Module *modp, BeamInstr *pc, int bif,
		   Binary *match_spec, Uint break_flags,
		   enum erts_break_op count_op, Eterm tracer_pid)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint common;
    BeamInstr **code_base = NULL;

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

    /*
     * Initialize the breakpoint data for this breakpoint (if needed).
     */
    g = (GenericBp *) pc[-4];
    if (g == 0) {
	if (count_op == erts_break_reset || count_op == erts_break_stop) {
	    /* Do not insert a new breakpoint */
	    return 1;
	}
	g = Alloc(sizeof(GenericBp));
	g->data[0].flags = 0;
	erts_smp_atomic_init_nob(&g->data[0].tracer_pid, 0);
	pc[-4] = (BeamInstr) g;
    }
    bp = &g->data[0];

    /*
     * If we are changing an existing breakpoint, clean up old data.
     */

    common = break_flags & bp->flags;
    if (common & ERTS_BPF_LOCAL_TRACE) {
	MatchSetUnref(bp->local_ms);
    } else if (common & ERTS_BPF_META_TRACE) {
	MatchSetUnref(bp->meta_ms);
    } else if (common & ERTS_BPF_COUNT) {
	if (count_op == erts_break_stop) {
	    bp->flags &= ~ERTS_BPF_COUNT_ACTIVE;
	} else {
	    bp->flags |= ERTS_BPF_COUNT_ACTIVE;
	    erts_smp_atomic_set_nob(&bp->count->acount, 0);
	}
	ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
	return 1;
    } else if (common & ERTS_BPF_TIME_TRACE) {
	BpDataTime* bdt = bp->time;
	Uint i = 0;

	if (count_op == erts_break_stop) {
	    bp->flags &= ~ERTS_BPF_TIME_TRACE_ACTIVE;
	} else {
	    bp->flags |= ERTS_BPF_TIME_TRACE_ACTIVE;
	    for (i = 0; i < bdt->n; i++) {
		bp_hash_delete(&(bdt->hash[i]));
		bp_hash_init(&(bdt->hash[i]), 32);
	    }
	}
	ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
	return 1;
    }

    /*
     * Initialize the new breakpoint data.
     */

    if (break_flags & ERTS_BPF_LOCAL_TRACE) {
	MatchSetRef(match_spec);
	bp->local_ms = match_spec;
    } else if (break_flags & ERTS_BPF_META_TRACE) {
	MatchSetRef(match_spec);
	bp->meta_ms = match_spec;
	erts_smp_atomic_set_nob(&bp->tracer_pid, tracer_pid);
    } else if (break_flags & ERTS_BPF_COUNT) {
	BpCount* bcp;

	ASSERT((bp->flags & ERTS_BPF_COUNT) == 0);
	bcp = Alloc(sizeof(BpCount));
	erts_refc_init(&bcp->refc, 1);
	erts_smp_atomic_init_nob(&bcp->acount, 0);
	bp->count = bcp;
    } else if (break_flags & ERTS_BPF_TIME_TRACE) {
	BpDataTime* bdt;
	int i;

	ASSERT((bp->flags & ERTS_BPF_TIME_TRACE) == 0);
	bdt = Alloc(sizeof(BpDataTime));
	erts_refc_init(&bdt->refc, 1);
	bdt->n = erts_no_schedulers;
	bdt->hash = Alloc(sizeof(bp_time_hash_t)*(bdt->n));
	for (i = 0; i < bdt->n; i++) {
	    bp_hash_init(&(bdt->hash[i]), 32);
	}
	bp->time = bdt;
    }

    bp->flags |= break_flags;
    if (bif == BREAK_IS_ERL &&
	*pc != (BeamInstr) BeamOp(op_i_generic_breakpoint)) {
	g->orig_instr = *pc;
	*pc = (BeamInstr) BeamOp(op_i_generic_breakpoint);
	modp->curr.num_breakpoints++;
    }
    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
    return 1;
}

static int clear_break(Eterm mfa[3], int specified, Uint break_flags)
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
	    num_processed += clear_module_break(modp, mfa,
						specified, break_flags);
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0], code_ix)) != NULL) {
	    num_processed += 
		clear_module_break(modp, mfa,
				   specified, break_flags);
	}	
    }
    return num_processed;
}

static int clear_module_break(Module *m, Eterm mfa[3], int specified, 
			      Uint break_flags) {
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
		clear_function_break(m, pc, BREAK_IS_ERL, break_flags);
	}
    }
    return num_processed;
}

static int clear_function_break(Module *m, BeamInstr *pc, int bif, Uint break_flags) {
    BeamInstr **code_base = NULL;
    GenericBp* g;
    GenericBpData* bp;
    Uint common;

    if (bif == BREAK_IS_ERL) {
	code_base = (BeamInstr **)m->curr.code;
	ASSERT(code_base);
	ASSERT(code_base <= (BeamInstr **)pc);
	ASSERT((BeamInstr **)pc < code_base + (m->curr.code_length/sizeof(BeamInstr *)));
    } else {
	ASSERT(*pc == (BeamInstr) em_apply_bif);
	ASSERT(m == NULL);
    }

    if (erts_is_native_break(pc)) {
	return 0;
    }

    if ((g = (GenericBp *) pc[-4]) == 0) {
	return 1;
    }
    ASSERT(bif == BREAK_IS_BIF ||
	   *pc == (BeamInstr) BeamOp(op_i_generic_breakpoint));

    bp = &g->data[0];
    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
    common = bp->flags & break_flags;
    bp->flags &= ~break_flags;
    if (common & ERTS_BPF_LOCAL_TRACE) {
	MatchSetUnref(bp->local_ms);
    }
    if (common & ERTS_BPF_META_TRACE) {
	MatchSetUnref(bp->meta_ms);
    }
    if (common & ERTS_BPF_COUNT) {
	ASSERT((bp->flags & ERTS_BPF_COUNT_ACTIVE) == 0);
	bp_count_unref(bp->count);
    }
    if (common & ERTS_BPF_TIME_TRACE) {
	ASSERT((bp->flags & ERTS_BPF_TIME_TRACE_ACTIVE) == 0);
	bp_time_unref(bp->time);
    }

    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
    if (bp->flags == 0) {
	pc[-4] = 0;
	if (bif == BREAK_IS_ERL) {
	    *pc = g->orig_instr;
	    ASSERT(m->curr.num_breakpoints > 0);
	    m->curr.num_breakpoints--;
	}
	Free(g);
    }
    return 1;
}

static void
bp_count_unref(BpCount* bcp)
{
    if (erts_refc_dectest(&bcp->refc, 0) <= 0) {
	Free(bcp);
    }
}

static void
bp_time_unref(BpDataTime* bdt)
{
    if (erts_refc_dectest(&bdt->refc, 0) <= 0) {
	Uint i = 0;
	Uint j = 0;
	Process *h_p = NULL;
	bp_data_time_item_t* item = NULL;
	process_breakpoint_time_t* pbt = NULL;

	/* remove all psd associated with the hash
	 * and then delete the hash.
	 * ... sigh ...
	 */

	for (i = 0; i < bdt->n; ++i) {
	    if (bdt->hash[i].used) {
		for (j = 0; j < bdt->hash[i].n; ++j) {
		    item = &(bdt->hash[i].item[j]);
		    if (item->pid != NIL) {
			h_p = erts_pid2proc(NULL, 0, item->pid,
					    ERTS_PROC_LOCK_MAIN);
			if (h_p) {
			    pbt = ERTS_PROC_SET_CALL_TIME(h_p,
							  ERTS_PROC_LOCK_MAIN,
							  NULL);
			    if (pbt) {
				Free(pbt);
			    }
			    erts_smp_proc_unlock(h_p, ERTS_PROC_LOCK_MAIN);
			}
		    }
		}
	    }
	    bp_hash_delete(&(bdt->hash[i]));
	}
	Free(bdt->hash);
	Free(bdt);
    }
}

static BpDataTime*
get_time_break(BeamInstr *pc)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_TIME_TRACE);
    return bp ? bp->time : 0;
}

BpDataTime*
erts_get_active_time_break(BeamInstr *pc)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_TIME_TRACE_ACTIVE);
    return bp ? bp->time : 0;
}

static GenericBpData*
check_break(BeamInstr *pc, Uint break_flags)
{
    GenericBp* g = (GenericBp *) pc[-4];

    ASSERT(pc[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
    if (erts_is_native_break(pc)) {
	return 0;
    }
    if (g) {
	GenericBpData* bp = &g->data[0];
	ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
	if (bp->flags & break_flags) {
	    return bp;
	}
    }
    return 0;
}
