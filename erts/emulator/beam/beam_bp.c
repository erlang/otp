/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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

/* *************************************************************************
** Local prototypes
*/

/*
** Helpers
*/

static int set_break(Eterm mfa[3], int specified,
		     Binary *match_spec, Uint break_op, 
		     enum erts_break_op count_op, Eterm tracer_pid);
static int set_module_break(Module *modp, Eterm mfa[3], int specified,
			    Binary *match_spec, Uint break_op,
			    enum erts_break_op count_op, Eterm tracer_pid);
static int set_function_break(Module *modp, Uint *pc,
			      Binary *match_spec, Uint break_op,
			      enum erts_break_op count_op, Eterm tracer_pid); 

static int clear_break(Eterm mfa[3], int specified, 
		       Uint break_op);
static int clear_module_break(Module *modp, Eterm mfa[3], int specified, 
			      Uint break_op);
static int clear_function_break(Module *modp, Uint *pc, 
				Uint break_op);

static BpData *is_break(Uint *pc, Uint break_op);



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
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return set_break(mfa, specified, match_spec,
		     (Uint) BeamOp(op_i_trace_breakpoint), 0, tracer_pid);
}

int 
erts_set_mtrace_break(Eterm mfa[3], int specified, Binary *match_spec,
		      Eterm tracer_pid) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return set_break(mfa, specified, match_spec,
		     (Uint) BeamOp(op_i_mtrace_breakpoint), 0, tracer_pid);
}

void
erts_set_mtrace_bif(Uint *pc, Binary *match_spec, Eterm tracer_pid) {
    BpDataTrace *bdt;
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));

    bdt = (BpDataTrace *) pc[-4];
    if (bdt) {
	MatchSetUnref(bdt->match_spec);
	MatchSetRef(match_spec);
	bdt->match_spec = match_spec;
	bdt->tracer_pid = tracer_pid;
    } else {
	bdt = Alloc(sizeof(BpDataTrace));
	BpInit((BpData *) bdt, 0);
	MatchSetRef(match_spec);
	bdt->match_spec = match_spec;
	bdt->tracer_pid = tracer_pid;
	pc[-4] = (Uint) bdt;
    }
}

int 
erts_set_debug_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return set_break(mfa, specified, NULL, 
		     (Uint) BeamOp(op_i_debug_breakpoint), 0, NIL);
}

int 
erts_set_count_break(Eterm mfa[3], int specified, enum erts_break_op count_op) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return set_break(mfa, specified, NULL, 
		     (Uint) BeamOp(op_i_count_breakpoint), count_op, NIL);
}



int
erts_clear_trace_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return clear_break(mfa, specified, 
		       (Uint) BeamOp(op_i_trace_breakpoint));
}

int
erts_clear_mtrace_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return clear_break(mfa, specified, 
		       (Uint) BeamOp(op_i_mtrace_breakpoint));
}

void
erts_clear_mtrace_bif(Uint *pc) {
    BpDataTrace *bdt;
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    
    bdt = (BpDataTrace *) pc[-4];
    if (bdt) {
	if (bdt->match_spec) {
	    MatchSetUnref(bdt->match_spec);
	}
	Free(bdt);
    }
    pc[-4] = (Uint) NULL;
}

int
erts_clear_debug_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return clear_break(mfa, specified, 
		       (Uint) BeamOp(op_i_debug_breakpoint));
}

int
erts_clear_count_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return clear_break(mfa, specified, 
		       (Uint) BeamOp(op_i_count_breakpoint));
}

int
erts_clear_break(Eterm mfa[3], int specified) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    return clear_break(mfa, specified, 0);
}

int 
erts_clear_module_break(Module *modp) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    ASSERT(modp);
    return clear_module_break(modp, NULL, 0, 0);
}

int
erts_clear_function_break(Module *modp, Uint *pc) {
    ERTS_SMP_LC_ASSERT(erts_smp_is_system_blocked(0));
    ASSERT(modp);
    return clear_function_break(modp, pc, 0);
}



/*
 * SMP NOTE: Process p may have become exiting on return!
 */
Uint 
erts_trace_break(Process *p, Uint *pc, Eterm *args, 
		 Uint32 *ret_flags, Eterm *tracer_pid) {
    Eterm tpid1, tpid2;
    BpDataTrace *bdt = (BpDataTrace *) pc[-4];
    
    ASSERT(pc[-5] == (Uint) BeamOp(op_i_func_info_IaaI));
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
    pc[-4] = (Uint) bdt;
    return bdt->orig_instr;
}



/*
 * SMP NOTE: Process p may have become exiting on return!
 */
Uint32
erts_bif_mtrace(Process *p, Uint *pc, Eterm *args, int local, 
		Eterm *tracer_pid) {
    BpDataTrace *bdt = (BpDataTrace *) pc[-4];
    
    ASSERT(tracer_pid);
    if (bdt) {
	Eterm tpid1, tpid2;
	Uint32 flags;

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
erts_is_trace_break(Uint *pc, Binary **match_spec_ret, Eterm *tracer_pid_ret) {
    BpDataTrace *bdt = 
	(BpDataTrace *) is_break(pc, (Uint) BeamOp(op_i_trace_breakpoint));
    
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
erts_is_mtrace_break(Uint *pc, Binary **match_spec_ret, Eterm *tracer_pid_ret) {
    BpDataTrace *bdt = 
	(BpDataTrace *) is_break(pc, (Uint) BeamOp(op_i_mtrace_breakpoint));
    
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
erts_is_mtrace_bif(Uint *pc, Binary **match_spec_ret, Eterm *tracer_pid_ret) {
    BpDataTrace *bdt = (BpDataTrace *) pc[-4];
    
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
erts_is_native_break(Uint *pc) {
#ifdef HIPE
    ASSERT(pc[-5] == (Uint) BeamOp(op_i_func_info_IaaI));
    return pc[0]  == (Uint) BeamOp(op_hipe_trap_call) 
	|| pc[0]  == (Uint) BeamOp(op_hipe_trap_call_closure);
#else
    return 0;
#endif
}

int 
erts_is_count_break(Uint *pc, Sint *count_ret) {
    BpDataCount *bdc = 
	(BpDataCount *) is_break(pc, (Uint) BeamOp(op_i_count_breakpoint));
    
    if (bdc) {
	if (count_ret) {
	    ErtsSmpBPLock(bdc);
	    *count_ret = bdc->count;
	    ErtsSmpBPUnlock(bdc);
	}
	return !0;
    }
    return 0;
}

Uint *
erts_find_local_func(Eterm mfa[3]) {
    Module *modp;
    Uint** code_base;
    Uint* code_ptr;
    Uint i,n;

    if ((modp = erts_get_module(mfa[0])) == NULL)
	return NULL;
    if ((code_base = (Uint **) modp->code) == NULL)
	return NULL;
    n = (Uint) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	ASSERT(((Uint) BeamOp(op_i_func_info_IaaI)) == code_ptr[0]);
	ASSERT(mfa[0] == ((Eterm) code_ptr[2]));
	if (mfa[1] == ((Eterm) code_ptr[3]) &&
	    ((Uint) mfa[2]) == code_ptr[4]) {
	    return code_ptr + 5;
	}
    }
    return NULL;
}



/* *************************************************************************
** Local helpers
*/


static int set_break(Eterm mfa[3], int specified, 
		     Binary *match_spec, Eterm break_op, 
		     enum erts_break_op count_op, Eterm tracer_pid)
{
    Module *modp;
    int num_processed = 0;
    if (!specified) {
	/* Find and process all modules in the system... */
	int current;
	int last = module_code_size();
	for (current = 0; current < last; current++) {
	    modp = module_code(current);
	    ASSERT(modp != NULL);
	    num_processed += 
		set_module_break(modp, mfa, specified, 
				 match_spec, break_op, count_op, 
				 tracer_pid);
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0])) != NULL) {
	    num_processed += 
		set_module_break(modp, mfa, specified, 
				 match_spec, break_op, count_op, 
				 tracer_pid);
	}	
    }
    return num_processed;
}

static int set_module_break(Module *modp, Eterm mfa[3], int specified,
			    Binary *match_spec, Uint break_op, 
			    enum erts_break_op count_op, Eterm tracer_pid) {
    Uint** code_base;
    Uint* code_ptr;
    int num_processed = 0;
    Uint i,n;

    ASSERT(break_op);
    ASSERT(modp);
    code_base = (Uint **) modp->code;
    if (code_base == NULL) {
	return 0;
    }
    n = (Uint) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	ASSERT(code_ptr[0] == (Uint) BeamOp(op_i_func_info_IaaI));
	if ((specified < 2 || mfa[1] == ((Eterm) code_ptr[3])) &&
	    (specified < 3 || ((int) mfa[2]) == ((int) code_ptr[4]))) {
	    Uint   *pc = code_ptr+5;
	    
	    num_processed +=
		set_function_break(modp, pc, match_spec, 
				   break_op, count_op, tracer_pid);
	}
    }
    return num_processed;
}

static int set_function_break(Module *modp, Uint *pc, 
			      Binary *match_spec, Uint break_op, 
			      enum erts_break_op count_op, Eterm tracer_pid) {
    BpData *bd, **r;
    size_t size;
    Uint **code_base = (Uint **)modp->code;
    
    ASSERT(code_base);
    ASSERT(code_base <= (Uint **)pc);
    ASSERT((Uint **)pc < code_base + (modp->code_length/sizeof(Uint *)));
    /*
     * Currently no trace support for native code.
     */
    if (erts_is_native_break(pc)) {
	return 0;
    }
    /* Do not allow two breakpoints of the same kind */
    if ( (bd = is_break(pc, break_op))) {
	if (break_op == (Uint) BeamOp(op_i_trace_breakpoint) 
	    || break_op == (Uint) BeamOp(op_i_mtrace_breakpoint)) {
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
	    ASSERT(! match_spec);
	    ASSERT(is_nil(tracer_pid));
	    if (break_op == (Uint) BeamOp(op_i_count_breakpoint)) {
		BpDataCount *bdc = (BpDataCount *) bd;
		
		ErtsSmpBPLock(bdc);
		if (count_op == erts_break_stop) {
		    if (bdc->count >= 0) {
			bdc->count = -bdc->count-1; /* Stop call counter */
		    }
		} else {
		    bdc->count = 0; /* Reset call counter */
		}
		ErtsSmpBPUnlock(bdc);
	    } else {
		ASSERT (! count_op);
	    }
	}
	return 1;
    }
    if (break_op == (Uint) BeamOp(op_i_trace_breakpoint) ||
	break_op == (Uint) BeamOp(op_i_mtrace_breakpoint)) {
	size = sizeof(BpDataTrace);
    } else {
	ASSERT(! match_spec);
	ASSERT(is_nil(tracer_pid));
	if (break_op == (Uint) BeamOp(op_i_count_breakpoint)) {
	    if (count_op == erts_break_reset
		|| count_op == erts_break_stop) {
		/* Do not insert a new breakpoint */
		return 1;
	    }
	    size = sizeof(BpDataCount);
	} else {
	    ASSERT(! count_op);
	    ASSERT(break_op == (Uint) BeamOp(op_i_debug_breakpoint));
	    size = sizeof(BpDataDebug);
	}
    }
    r = (BpData **) (pc-4);
    if (! *r) {
	ASSERT(*pc != (Uint) BeamOp(op_i_trace_breakpoint));
	ASSERT(*pc != (Uint) BeamOp(op_i_mtrace_breakpoint));
	ASSERT(*pc != (Uint) BeamOp(op_i_debug_breakpoint));
	ASSERT(*pc != (Uint) BeamOp(op_i_count_breakpoint));
	/* First breakpoint; create singleton ring */
	bd = Alloc(size);
	BpInit(bd, *pc);
	*pc = break_op;
	*r = bd;
    } else {
	ASSERT(*pc == (Uint) BeamOp(op_i_trace_breakpoint) ||
	       *pc == (Uint) BeamOp(op_i_mtrace_breakpoint) ||
	       *pc == (Uint) BeamOp(op_i_debug_breakpoint) ||
	       *pc == (Uint) BeamOp(op_i_count_breakpoint));
	if (*pc == (Uint) BeamOp(op_i_debug_breakpoint)) {
	    /* Debug bp must be last, so if it is also first; 
	     * it must be singleton. */
	    ASSERT(BpSingleton(*r)); 
	    /* Insert new bp first in the ring, i.e second to last. */
	    bd = Alloc(size);
	    BpInitAndSpliceNext(bd, *pc, *r);
	    *pc = break_op;
	} else if ((*r)->prev->orig_instr 
		   == (Uint) BeamOp(op_i_debug_breakpoint)) {
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
    /* Init the bp type specific data */
    if (break_op == (Uint) BeamOp(op_i_trace_breakpoint) ||
	break_op == (Uint) BeamOp(op_i_mtrace_breakpoint)) {
		
	BpDataTrace *bdt = (BpDataTrace *) bd;
		
	MatchSetRef(match_spec);
	bdt->match_spec = match_spec;
	bdt->tracer_pid = tracer_pid;
    } else if (break_op == (Uint) BeamOp(op_i_count_breakpoint)) {
	BpDataCount *bdc = (BpDataCount *) bd;

	bdc->count = 0;
    }
    ++(*(Uint*)&code_base[MI_NUM_BREAKPOINTS]);
    return 1;
}

static int clear_break(Eterm mfa[3], int specified, Uint break_op)
{
    int num_processed = 0;
    Module *modp;

    if (!specified) {
	/* Iterate over all modules */
	int current;
	int last = module_code_size();

	for (current = 0; current < last; current++) {
	    modp = module_code(current);
	    ASSERT(modp != NULL);
	    num_processed += clear_module_break(modp, mfa, specified, break_op);
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0])) != NULL) {
	    num_processed += 
		clear_module_break(modp, mfa, specified, break_op);
	}	
    }
    return num_processed;
}

static int clear_module_break(Module *m, Eterm mfa[3], int specified, 
			      Uint break_op) {
    Uint** code_base;
    Uint* code_ptr;
    int num_processed = 0;
    Uint i,n;
    
    ASSERT(m);
    code_base = (Uint **) m->code;
    if (code_base == NULL) {
	return 0;
    }
    n = (Uint) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	if ((specified < 2 || mfa[1] == ((Eterm) code_ptr[3])) &&
	    (specified < 3 || ((int) mfa[2]) == ((int) code_ptr[4]))) {
	    Uint *pc = code_ptr + 5;
	    
	    num_processed += 
		clear_function_break(m, pc, break_op);
	}
    }
    return num_processed;
}

static int clear_function_break(Module *m, Uint *pc, Uint break_op) {
    BpData *bd;
    Uint **code_base = (Uint **)m->code;
    
    ASSERT(code_base);
    ASSERT(code_base <= (Uint **)pc);
    ASSERT((Uint **)pc < code_base + (m->code_length/sizeof(Uint *)));
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
	Uint op;
	BpData **r = (BpData **) (pc-4);
	
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
	    *r  = NULL;
	    *pc = bd->orig_instr;
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
		*pc = bd->orig_instr;
	    } else {
		bd_prev->orig_instr = bd->orig_instr;
	    }
	}
	if (op == (Uint) BeamOp(op_i_trace_breakpoint) ||
	    op == (Uint) BeamOp(op_i_mtrace_breakpoint)) {
	    
	    BpDataTrace *bdt = (BpDataTrace *) bd;
	    
	    MatchSetUnref(bdt->match_spec);
	}
	Free(bd);
	ASSERT(((Uint) code_base[MI_NUM_BREAKPOINTS]) > 0);
	--(*(Uint*)&code_base[MI_NUM_BREAKPOINTS]);
    }
    return 1;
}



/*
** Searches (linear forward) the breakpoint ring for a specified opcode
** and returns a pointer to the breakpoint data structure or NULL if
** not found. If the specified opcode is 0, the last breakpoint is 
** returned. The program counter must point to the first executable
** (breakpoint) instruction of the function.
*/
static BpData *is_break(Uint *pc, Uint break_op) {
    ASSERT(pc[-5] == (Uint) BeamOp(op_i_func_info_IaaI));
    if (! erts_is_native_break(pc)) {
	BpData *bd = (BpData *) pc[-4];
	
	if (break_op == 0) {
	    return bd;
	}
	if (*pc == break_op) {
	    ASSERT(bd);
	    return bd->next;
	}
	if (! bd){
	    return NULL;
	}
	bd = bd->next;
	while (bd != (BpData *) pc[-4]) {
	    ASSERT(bd);
	    if (bd->orig_instr == break_op) {
		bd = bd->next;
		ASSERT(bd);
		return bd;
	    } else {
		bd = bd->next;
	    }
	}
    }
    return NULL;
}
