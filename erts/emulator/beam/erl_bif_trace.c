/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
 * Trace BIFs.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "beam_bp.h"
#include "erl_binary.h"
#include "erl_thr_progress.h"
#include "erl_bif_unique.h"

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

const struct trace_pattern_flags   erts_trace_pattern_flags_off = {0, 0, 0, 0, 0};

/*
 * The following variables are protected by code write permission.
 */
static int                         erts_default_trace_pattern_is_on;
static Binary                     *erts_default_match_spec;
static Binary                     *erts_default_meta_match_spec;
static struct trace_pattern_flags  erts_default_trace_pattern_flags;
static ErtsTracer                  erts_default_meta_tracer;

static struct {			/* Protected by code write permission */
    int current;
    int install;
    int local;
    BpFunctions f;		/* Local functions */
    BpFunctions e;		/* Export entries */
#ifdef ERTS_SMP
    Process* stager;
    ErtsThrPrgrLaterOp lop;
#endif
} finish_bp;

static Eterm
trace_pattern(Process* p, Eterm MFA, Eterm Pattern, Eterm flaglist);
static int
erts_set_tracing_event_pattern(Eterm event, Binary*, int on);

#ifdef ERTS_SMP
static void smp_bp_finisher(void* arg);
#endif
static BIF_RETTYPE
system_monitor(Process *p, Eterm monitor_pid, Eterm list);

static void new_seq_trace_token(Process* p); /* help func for seq_trace_2*/
static Eterm trace_info_pid(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_func(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_on_load(Process* p, Eterm key);
static Eterm trace_info_event(Process* p, Eterm event, Eterm key);


static void reset_bif_trace(void);
static void setup_bif_trace(void);
static void install_exp_breakpoints(BpFunctions* f);
static void uninstall_exp_breakpoints(BpFunctions* f);
static void clean_export_entries(BpFunctions* f);

ErtsTracingEvent erts_send_tracing[ERTS_NUM_BP_IX];
ErtsTracingEvent erts_receive_tracing[ERTS_NUM_BP_IX];

void
erts_bif_trace_init(void)
{
    int i;

    erts_default_trace_pattern_is_on = 0;
    erts_default_match_spec = NULL;
    erts_default_meta_match_spec = NULL;
    erts_default_trace_pattern_flags = erts_trace_pattern_flags_off;
    erts_default_meta_tracer = erts_tracer_nil;

    for (i=0; i<ERTS_NUM_BP_IX; i++) {
        erts_send_tracing[i].on = 1;
        erts_send_tracing[i].match_spec = NULL;
	erts_receive_tracing[i].on = 1;
	erts_receive_tracing[i].match_spec = NULL;
    }
}

/*
 * Turn on/off call tracing for the given function(s).
 */

Eterm
erts_internal_trace_pattern_3(BIF_ALIST_3)
{
    return trace_pattern(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

static Eterm
trace_pattern(Process* p, Eterm MFA, Eterm Pattern, Eterm flaglist)
{
    int i;
    int matches = -1;
    int specified = 0;
    enum erts_break_op on;
    Binary* match_prog_set;
    Eterm l;
    struct trace_pattern_flags flags = erts_trace_pattern_flags_off;
    int is_global;
    ErtsTracer meta_tracer = erts_tracer_nil;

    if (!erts_try_seize_code_write_permission(p)) {
	ERTS_BIF_YIELD3(bif_export[BIF_erts_internal_trace_pattern_3], p, MFA, Pattern, flaglist);
    }
    finish_bp.current = -1;

    UseTmpHeap(3,p);
    /*
     * Check and compile the match specification.
     */
    
    if (Pattern == am_false) {
	match_prog_set = NULL;
	on = 0;
    } else if (is_nil(Pattern) || Pattern == am_true) {
	match_prog_set = NULL;
	on = 1;
    } else if (Pattern == am_restart) {
	match_prog_set = NULL;
	on = ERTS_BREAK_RESTART;
    } else if (Pattern == am_pause) {
	match_prog_set = NULL;
	on = ERTS_BREAK_PAUSE;
    } else {
	match_prog_set = erts_match_set_compile(p, Pattern, MFA);
	if (match_prog_set) {
	    MatchSetRef(match_prog_set);
	    on = 1;
	} else{
	    goto error;
	}
    }
    
    is_global = 0;
    for(l = flaglist; is_list(l); l = CDR(list_val(l))) {
	if (is_tuple(CAR(list_val(l)))) {
            meta_tracer = erts_term_to_tracer(am_meta, CAR(list_val(l)));
            if (meta_tracer == THE_NON_VALUE) {
                meta_tracer = erts_tracer_nil;
                goto error;
            }
	    flags.breakpoint = 1;
	    flags.meta       = 1;
	} else {
	    switch (CAR(list_val(l))) {
	    case am_local:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.local      = 1;
		break;
	    case am_meta:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.meta       = 1;
                if (ERTS_TRACER_IS_NIL(meta_tracer))
                    meta_tracer = erts_term_to_tracer(THE_NON_VALUE, p->common.id);
		break;
	    case am_global:
		if (flags.breakpoint) {
		    goto error;
		}
		is_global = !0;
		break;
	    case am_call_count:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.call_count = 1;
		break;
	    case am_call_time:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.call_time = 1;
		break;

	    default:
		goto error;
	    }
	}
    }
    if (l != NIL) {
	goto error;
    }
    
    if (match_prog_set && !flags.local && !flags.meta && (flags.call_count || flags.call_time)) {
	/* A match prog is not allowed with just call_count or call_time*/
	goto error;
    }

    /*
     * Check the MFA specification.
     */

    if (MFA == am_on_load) {
	if (flags.local || (! flags.breakpoint)) {
	    MatchSetUnref(erts_default_match_spec);
	    erts_default_match_spec = match_prog_set;
	    MatchSetRef(erts_default_match_spec);
	}
	if (flags.meta) {
	    MatchSetUnref(erts_default_meta_match_spec);
	    erts_default_meta_match_spec = match_prog_set;
	    MatchSetRef(erts_default_meta_match_spec);
            erts_tracer_update(&erts_default_meta_tracer, meta_tracer);
	} else if (! flags.breakpoint) {
	    MatchSetUnref(erts_default_meta_match_spec);
	    erts_default_meta_match_spec = NULL;
	    ERTS_TRACER_CLEAR(&erts_default_meta_tracer);
	}
	if (erts_default_trace_pattern_flags.breakpoint &&
	    flags.breakpoint) { 
	    /* Breakpoint trace -> breakpoint trace */
	    ASSERT(erts_default_trace_pattern_is_on);
	    if (on) {
		erts_default_trace_pattern_flags.local
		    |= flags.local;
		erts_default_trace_pattern_flags.meta
		    |= flags.meta;
		erts_default_trace_pattern_flags.call_count
		    |= (on == 1) ? flags.call_count : 0;
		erts_default_trace_pattern_flags.call_time
		    |= (on == 1) ? flags.call_time : 0;
	    } else {
		erts_default_trace_pattern_flags.local
		    &= ~flags.local;
		erts_default_trace_pattern_flags.meta
		    &= ~flags.meta;
		erts_default_trace_pattern_flags.call_count
		    &= ~flags.call_count;
		erts_default_trace_pattern_flags.call_time
		    &= ~flags.call_time;
		if (! (erts_default_trace_pattern_flags.breakpoint =
		       erts_default_trace_pattern_flags.local |
		       erts_default_trace_pattern_flags.meta |
		       erts_default_trace_pattern_flags.call_count |
		       erts_default_trace_pattern_flags.call_time)) {
		    erts_default_trace_pattern_is_on = !!on; /* i.e off */
		}
	    }
	} else if (! erts_default_trace_pattern_flags.breakpoint &&
		   ! flags.breakpoint) {
	    /* Global call trace -> global call trace */
	    erts_default_trace_pattern_is_on = !!on;
	} else if (erts_default_trace_pattern_flags.breakpoint &&
		   ! flags.breakpoint) {
	    /* Breakpoint trace -> global call trace */
	    if (on) {
		erts_default_trace_pattern_flags = flags; /* Struct copy */
		erts_default_trace_pattern_is_on = !!on;
	    }
	} else {
	    ASSERT(! erts_default_trace_pattern_flags.breakpoint &&
		   flags.breakpoint);
	    /* Global call trace -> breakpoint trace */
	    if (on) {
		if (on != 1) {
		    flags.call_count = 0;
		    flags.call_time  = 0;
		}
		flags.breakpoint = flags.local | flags.meta | flags.call_count | flags.call_time;
		erts_default_trace_pattern_flags = flags; /* Struct copy */
		erts_default_trace_pattern_is_on = !!flags.breakpoint;
	    }
	}
	matches = 0;
    } else if (is_tuple(MFA)) {
        ErtsCodeMFA mfa;
	Eterm *tp = tuple_val(MFA);
	if (tp[0] != make_arityval(3)) {
	    goto error;
	}
	if (!is_atom(tp[1]) || !is_atom(tp[2]) ||
	    (!is_small(tp[3]) && tp[3] != am_Underscore)) {
	    goto error;
	}
	for (i = 0; i < 3 && tp[i+1] != am_Underscore; i++, specified++) {
	    /* Empty loop body */
	}
	for (i = specified; i < 3; i++) {
	    if (tp[i+1] != am_Underscore) {
		goto error;
	    }
	}
	mfa.module   = tp[1];
	mfa.function = tp[2];
	if (specified == 3) {
            mfa.arity = signed_val(tp[3]);
	}

	matches = erts_set_trace_pattern(p, &mfa, specified,
					 match_prog_set, match_prog_set,
					 on, flags, meta_tracer, 0);
    } else if (is_atom(MFA)) {
        if (is_global || flags.breakpoint || on > ERTS_BREAK_SET) {
            goto error;
        }
        matches = erts_set_tracing_event_pattern(MFA, match_prog_set, on);
    }

 error:
    MatchSetUnref(match_prog_set);

    ERTS_TRACER_CLEAR(&meta_tracer);

#ifdef ERTS_SMP
    if (finish_bp.current >= 0) {
	ASSERT(matches >= 0);
	ASSERT(finish_bp.stager == NULL);
	finish_bp.stager = p;
	erts_schedule_thr_prgr_later_op(smp_bp_finisher, NULL, &finish_bp.lop);
	erts_proc_inc_refc(p);
	erts_suspend(p, ERTS_PROC_LOCK_MAIN, NULL);
	ERTS_BIF_YIELD_RETURN(p, make_small(matches));
    }
#endif

    erts_release_code_write_permission();

    if (matches >= 0) {
	return make_small(matches);
    }
    else {
	BIF_ERROR(p, BADARG);    
    }
}

#ifdef ERTS_SMP
static void smp_bp_finisher(void* null)
{
    if (erts_finish_breakpointing()) { /* Not done */
	/* Arrange for being called again */
	erts_schedule_thr_prgr_later_op(smp_bp_finisher, NULL, &finish_bp.lop);
    }
    else {			/* Done */
	Process* p = finish_bp.stager;
#ifdef DEBUG
	finish_bp.stager = NULL;
#endif
	erts_release_code_write_permission();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	if (!ERTS_PROC_IS_EXITING(p)) {
	    erts_resume(p, ERTS_PROC_LOCK_STATUS);
	}
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
	erts_proc_dec_refc(p);
    }
}
#endif /* ERTS_SMP */

void
erts_get_default_trace_pattern(int *trace_pattern_is_on,
			       Binary **match_spec,
			       Binary **meta_match_spec,
			       struct trace_pattern_flags *trace_pattern_flags,
			       ErtsTracer *meta_tracer)
{
    ERTS_SMP_LC_ASSERT(erts_has_code_write_permission() ||
		       erts_smp_thr_progress_is_blocking());
    if (trace_pattern_is_on)
	*trace_pattern_is_on = erts_default_trace_pattern_is_on;
    if (match_spec)
	*match_spec = erts_default_match_spec;
    if (meta_match_spec)
	*meta_match_spec = erts_default_meta_match_spec;
    if (trace_pattern_flags)
	*trace_pattern_flags = erts_default_trace_pattern_flags;
    if (meta_tracer)
	*meta_tracer = erts_default_meta_tracer;
}

int erts_is_default_trace_enabled(void)
{
    ERTS_SMP_LC_ASSERT(erts_has_code_write_permission() ||
		       erts_smp_thr_progress_is_blocking());
    return erts_default_trace_pattern_is_on;
}

Uint 
erts_trace_flag2bit(Eterm flag) 
{
    switch (flag) {
    case am_timestamp: return F_NOW_TS;
    case am_strict_monotonic_timestamp: return F_STRICT_MON_TS;
    case am_monotonic_timestamp: return F_MON_TS;
    case am_all: return TRACEE_FLAGS;
    case am_send: return F_TRACE_SEND;
    case am_receive: return F_TRACE_RECEIVE;
    case am_set_on_spawn: return F_TRACE_SOS;
    case am_procs: return F_TRACE_PROCS;
    case am_set_on_first_spawn: return F_TRACE_SOS1;
    case am_set_on_link: return F_TRACE_SOL;
    case am_set_on_first_link: return F_TRACE_SOL1;
    case am_running: return F_TRACE_SCHED;
    case am_exiting: return F_TRACE_SCHED_EXIT;
    case am_garbage_collection: return F_TRACE_GC;
    case am_call: return  F_TRACE_CALLS;
    case am_arity: return F_TRACE_ARITY_ONLY;
    case am_return_to: return F_TRACE_RETURN_TO;
    case am_silent: return F_TRACE_SILENT;
    case am_scheduler_id: return F_TRACE_SCHED_NO;
    case am_running_ports: return F_TRACE_SCHED_PORTS;
    case am_running_procs: return F_TRACE_SCHED_PROCS;
    case am_ports: return F_TRACE_PORTS;
    default: return 0;
    }
}

/* Scan the argument list and sort out the trace flags.
**
** Returns !0 on success, 0 on failure.
**
** Sets the result variables on success, if their flags has
** occurred in the argument list.
*/
int
erts_trace_flags(Eterm List,
                 Uint *pMask, ErtsTracer *pTracer, int *pCpuTimestamp)
{
    Eterm list = List;
    Uint mask = 0;
    ErtsTracer tracer = erts_tracer_nil;
    int cpu_timestamp = 0;
    
    while (is_list(list)) {
	Uint bit;
	Eterm item = CAR(list_val(list));
	if (is_atom(item) && (bit = erts_trace_flag2bit(item))) {
	    mask |= bit;
#ifdef HAVE_ERTS_NOW_CPU
	} else if (item == am_cpu_timestamp) {
	    cpu_timestamp = !0;
#endif
	} else if (is_tuple(item)) {
            tracer = erts_term_to_tracer(am_tracer, item);
            if (tracer == THE_NON_VALUE)
                goto error;
	} else goto error;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) goto error;
    
    if (pMask && mask)                           *pMask         = mask;
    if (pTracer && !ERTS_TRACER_IS_NIL(tracer))  *pTracer       = tracer;
    if (pCpuTimestamp && cpu_timestamp)          *pCpuTimestamp = cpu_timestamp;
    return !0;
 error:
    return 0;
}

static ERTS_INLINE int
start_trace(Process *c_p, ErtsTracer tracer,
            ErtsPTabElementCommon *common,
            int on, int mask)
{
    /* We can use the common part of both port+proc without checking what it is
       In the code below port is used for both proc and port */
    Port *port = (Port*)common;

    /*
     * SMP build assumes that either system is blocked or:
     * * main lock is held on c_p
     * * all locks are held on port common
     */

    if (!ERTS_TRACER_IS_NIL(tracer)) {
        if ((ERTS_TRACE_FLAGS(port) & TRACEE_FLAGS)
            && !ERTS_TRACER_COMPARE(ERTS_TRACER(port), tracer)) {
            /* This tracee is already being traced, and not by the
             * tracer to be */
            if (erts_is_tracer_enabled(ERTS_TRACER(port), common)) {
                /* The tracer is still in use */
                return 1;
            }
            /* Current tracer now invalid */
        }
    }

    if (on)
        ERTS_TRACE_FLAGS(port) |= mask;
    else
        ERTS_TRACE_FLAGS(port) &= ~mask;

    if ((ERTS_TRACE_FLAGS(port) & TRACEE_FLAGS) == 0) {
        tracer = erts_tracer_nil;
        erts_tracer_replace(common, erts_tracer_nil);
    } else if (!ERTS_TRACER_IS_NIL(tracer))
        erts_tracer_replace(common, tracer);

    return 0;
}

Eterm erts_internal_trace_3(BIF_ALIST_3)
{
    Process* p = BIF_P;
    Eterm pid_spec = BIF_ARG_1;
    Eterm how = BIF_ARG_2;
    Eterm list = BIF_ARG_3;
    int on;
    ErtsTracer tracer = erts_tracer_nil;
    int matches = 0;
    Uint mask = 0;
    int cpu_ts = 0;
#ifdef ERTS_SMP
    int system_blocked = 0;
#endif

    if (! erts_trace_flags(list, &mask, &tracer, &cpu_ts)) {
	BIF_ERROR(p, BADARG);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD3(bif_export[BIF_erts_internal_trace_3],
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    switch (how) {
    case am_false: 
	on = 0; 
	break;
    case am_true: 
	on = 1;
        if (ERTS_TRACER_IS_NIL(tracer))
            tracer = erts_term_to_tracer(am_tracer, p->common.id);

        if (tracer == THE_NON_VALUE) {
            tracer = erts_tracer_nil;
            goto error;
        }

	break;
    default: 
	goto error;
    }

    /*
     * Set/reset the call trace flag for the given Pids.
     */

    if (is_port(pid_spec)) {
	Port *tracee_port;

#ifdef HAVE_ERTS_NOW_CPU
	if (cpu_ts) {
	    goto error;
	}
#endif

	tracee_port = erts_id2port_sflgs(pid_spec,
					 p,
					 ERTS_PROC_LOCK_MAIN,
					 ERTS_PORT_SFLGS_INVALID_LOOKUP);

	if (!tracee_port)
	    goto error;

        if (start_trace(p, tracer, &tracee_port->common, on, mask)) {
	    erts_port_release(tracee_port);
	    goto already_traced;
        }
        erts_port_release(tracee_port);
        matches = 1;
    } else if (is_pid(pid_spec)) {
	Process *tracee_p;

#ifdef HAVE_ERTS_NOW_CPU
	if (cpu_ts) {
	    goto error;
	}
#endif
	/* Check that the tracee is not dead, not tracing 
	 * and not about to be tracing.
	 */

	tracee_p = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
				 pid_spec, ERTS_PROC_LOCKS_ALL);
	if (!tracee_p)
	    goto error;

        if (start_trace(tracee_p, tracer, &tracee_p->common, on, mask)) {
	    erts_smp_proc_unlock(tracee_p,
				 (tracee_p == p
				  ? ERTS_PROC_LOCKS_ALL_MINOR
				  : ERTS_PROC_LOCKS_ALL));
	    goto already_traced;
        }
        erts_smp_proc_unlock(tracee_p,
			     (tracee_p == p
			      ? ERTS_PROC_LOCKS_ALL_MINOR
			      : ERTS_PROC_LOCKS_ALL));

	matches = 1;
    } else {
	int ok = 0;

#ifdef HAVE_ERTS_NOW_CPU
	if (cpu_ts) {
	    if (pid_spec == am_all) {
		if (on) {
		    if (!erts_cpu_timestamp) {
#ifdef HAVE_CLOCK_GETTIME_CPU_TIME
			/* 
			   Perhaps clock_gettime was found during config
			   on a different machine than this. We check
			   if it works here and now, then don't bother 
			   about checking return value for error later. 
			*/
			{
			    SysCpuTime start, stop;
			    SysTimespec tp;
			    int i;
			    
			    if (sys_get_proc_cputime(start, tp) < 0)
				goto error;
			    start = ((SysCpuTime)tp.tv_sec * 1000000000LL) + 
				    (SysCpuTime)tp.tv_nsec;
			    for (i = 0; i < 100; i++)
				sys_get_proc_cputime(stop, tp);
			    stop = ((SysCpuTime)tp.tv_sec * 1000000000LL) + 
				   (SysCpuTime)tp.tv_nsec;
			    if (start == 0) goto error;
			    if (start == stop) goto error;
			}
#else /* HAVE_GETHRVTIME */
			if (erts_start_now_cpu() < 0) {
			    goto error;
			}
#endif /* HAVE_CLOCK_GETTIME_CPU_TIME */
			erts_cpu_timestamp = !0;
		    }
		}
	    } else {
		goto error;
	    }
	}
#endif
	
	if (pid_spec == am_all || pid_spec == am_existing ||
            pid_spec == am_ports || pid_spec == am_processes ||
            pid_spec == am_existing_ports || pid_spec == am_existing_processes
            ) {
	    int i;
	    int procs = 0;
	    int ports = 0;
	    int mods = 0;

	    if (mask & (ERTS_PROC_TRACEE_FLAGS & ~ERTS_TRACEE_MODIFIER_FLAGS))
		procs = pid_spec != am_ports && pid_spec != am_existing_ports;
	    if (mask & (ERTS_PORT_TRACEE_FLAGS & ~ERTS_TRACEE_MODIFIER_FLAGS))
		ports = pid_spec != am_processes && pid_spec != am_existing_processes;
	    if (mask & ERTS_TRACEE_MODIFIER_FLAGS) {
                if (pid_spec == am_ports || pid_spec == am_existing_ports)
                    ports = 1;
                else if (pid_spec == am_processes || pid_spec == am_existing_processes)
                    procs = 1;
                else
                    mods = 1;
            }

#ifdef ERTS_SMP
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	    erts_smp_thr_progress_block();
	    system_blocked = 1;
#endif

	    ok = 1;
	    if (procs || mods) {
		int max = erts_ptab_max(&erts_proc);
		/* tracing of processes */
		for (i = 0; i < max; i++) {
		    Process* tracee_p = erts_pix2proc(i);
		    if (! tracee_p) 
			continue;
                    if (!start_trace(p, tracer, &tracee_p->common, on, mask))
                        matches++;
		}
	    }
	    if (ports || mods) {
		int max = erts_ptab_max(&erts_port);
		/* tracing of ports */
		for (i = 0; i < max; i++) {
		    erts_aint32_t state;
		    Port *tracee_port = erts_pix2port(i);
		    if (!tracee_port)
			continue;
		    state = erts_atomic32_read_nob(&tracee_port->state);
		    if (state & ERTS_PORT_SFLGS_DEAD)
			continue;
                    if (!start_trace(p, tracer, &tracee_port->common, on, mask))
                        matches++;
		}
	    }
	}

	if (pid_spec == am_all || pid_spec == am_new
            || pid_spec == am_ports || pid_spec == am_processes
            || pid_spec == am_new_ports || pid_spec == am_new_processes
            ) {

	    ok = 1;
            if (mask & ERTS_PROC_TRACEE_FLAGS &&
                pid_spec != am_ports && pid_spec != am_new_ports)
                erts_change_default_proc_tracing(
                    on, mask & ERTS_PROC_TRACEE_FLAGS, tracer);
            if (mask & ERTS_PORT_TRACEE_FLAGS &&
                pid_spec != am_processes && pid_spec != am_new_processes)
                erts_change_default_port_tracing(
                    on, mask & ERTS_PORT_TRACEE_FLAGS, tracer);

#ifdef HAVE_ERTS_NOW_CPU
	    if (cpu_ts && !on) {
		/* cpu_ts => pid_spec == am_all */
		if (erts_cpu_timestamp) {
#ifdef HAVE_GETHRVTIME
		    erts_stop_now_cpu();
#endif
		    erts_cpu_timestamp = 0;
		}
	    }
#endif
	}

	if (!ok)
	    goto error;
    }

#ifdef ERTS_SMP
    if (system_blocked) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
#endif
    erts_release_code_write_permission();
    ERTS_TRACER_CLEAR(&tracer);

    BIF_RET(make_small(matches));

 already_traced:
    erts_send_error_to_logger_str(p->group_leader,
				  "** can only have one tracer per process\n");

 error:

    ERTS_TRACER_CLEAR(&tracer);

#ifdef ERTS_SMP
    if (system_blocked) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
#endif
    erts_release_code_write_permission();

    BIF_ERROR(p, BADARG);
}

/*
 * Return information about a process or an external function being traced.
 */

Eterm trace_info_2(BIF_ALIST_2)
{
    Process* p = BIF_P;
    Eterm What = BIF_ARG_1;
    Eterm Key = BIF_ARG_2;
    Eterm res;

    if (!erts_try_seize_code_write_permission(p)) {
	ERTS_BIF_YIELD2(bif_export[BIF_trace_info_2], p, What, Key);
    }

    if (What == am_on_load) {
	res = trace_info_on_load(p, Key);
    } else if (What == am_send || What == am_receive) {
        res = trace_info_event(p, What, Key);
    } else if (is_atom(What) || is_pid(What) || is_port(What)) {
	res = trace_info_pid(p, What, Key);
    } else if (is_tuple(What)) {
	res = trace_info_func(p, What, Key);
    } else {
	erts_release_code_write_permission();
	BIF_ERROR(p, BADARG);
    }
    erts_release_code_write_permission();
    BIF_RET(res);
}

static Eterm
trace_info_pid(Process* p, Eterm pid_spec, Eterm key)
{
    Eterm tracer;
    Uint trace_flags = am_false;
    Eterm* hp;

    if (pid_spec == am_new || pid_spec == am_new_processes) {
        ErtsTracer def_tracer;
	erts_get_default_proc_tracing(&trace_flags, &def_tracer);
        tracer = erts_tracer_to_term(p, def_tracer);
        ERTS_TRACER_CLEAR(&def_tracer);
    } else if (pid_spec == am_new_ports) {
        ErtsTracer def_tracer;
	erts_get_default_port_tracing(&trace_flags, &def_tracer);
        tracer = erts_tracer_to_term(p, def_tracer);
        ERTS_TRACER_CLEAR(&def_tracer);
    } else if (is_internal_port(pid_spec)) {
        Port *tracee;
        tracee = erts_id2port_sflgs(pid_spec, p, ERTS_PROC_LOCK_MAIN,
                                    ERTS_PORT_SFLGS_INVALID_LOOKUP);

        if (!tracee)
            return am_undefined;

        if (!ERTS_TRACER_IS_NIL(ERTS_TRACER(tracee)))
            erts_is_tracer_proc_enabled(NULL, 0, &tracee->common);

        tracer = erts_tracer_to_term(p, ERTS_TRACER(tracee));
        trace_flags = ERTS_TRACE_FLAGS(tracee);

        erts_port_release(tracee);

    } else if (is_internal_pid(pid_spec)) {
	Process *tracee = erts_pid2proc_not_running(p, ERTS_PROC_LOCK_MAIN,
                                                    pid_spec, ERTS_PROC_LOCK_MAIN);

        if (tracee == ERTS_PROC_LOCK_BUSY)
            ERTS_BIF_YIELD2(bif_export[BIF_trace_info_2], p, pid_spec, key);

	if (!tracee)
	    return am_undefined;

        if (!ERTS_TRACER_IS_NIL(ERTS_TRACER(tracee)))
            erts_is_tracer_proc_enabled(tracee, ERTS_PROC_LOCK_MAIN,
                                        &tracee->common);

        tracer = erts_tracer_to_term(p, ERTS_TRACER(tracee));
        trace_flags = ERTS_TRACE_FLAGS(tracee);

	if (tracee != p)
	    erts_smp_proc_unlock(tracee, ERTS_PROC_LOCK_MAIN);
    } else if (is_external_pid(pid_spec)
	       && external_pid_dist_entry(pid_spec) == erts_this_dist_entry) {
	    return am_undefined;
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }

    if (key == am_flags) {
	int num_flags = 21;	/* MAXIMUM number of flags. */
	Uint needed = 3+2*num_flags;
	Eterm flag_list = NIL;
	Eterm* limit;

#define FLAG0(flag_mask,flag) \
  if (trace_flags & (flag_mask)) { flag_list = CONS(hp, flag, flag_list); hp += 2; } else {}

#if defined(DEBUG)
    /*
     * Check num_flags if this assertion fires.
     */
#  define FLAG ASSERT(num_flags-- > 0); FLAG0
#else
#  define FLAG FLAG0
#endif
        hp = HAlloc(p, needed);
	limit = hp+needed;
	FLAG(F_NOW_TS, am_timestamp);
	FLAG(F_STRICT_MON_TS, am_strict_monotonic_timestamp);
	FLAG(F_MON_TS, am_monotonic_timestamp);
	FLAG(F_TRACE_SEND, am_send);
	FLAG(F_TRACE_RECEIVE, am_receive);
	FLAG(F_TRACE_SOS, am_set_on_spawn);
	FLAG(F_TRACE_CALLS, am_call);
	FLAG(F_TRACE_PROCS, am_procs);
	FLAG(F_TRACE_SOS1, am_set_on_first_spawn);
	FLAG(F_TRACE_SOL, am_set_on_link);
	FLAG(F_TRACE_SOL1, am_set_on_first_link);
	FLAG(F_TRACE_SCHED, am_running);
	FLAG(F_TRACE_SCHED_EXIT, am_exiting);
	FLAG(F_TRACE_GC, am_garbage_collection);
	FLAG(F_TRACE_ARITY_ONLY, am_arity);
	FLAG(F_TRACE_RETURN_TO, am_return_to);
	FLAG(F_TRACE_SILENT, am_silent);
	FLAG(F_TRACE_SCHED_NO, am_scheduler_id);
	FLAG(F_TRACE_PORTS, am_ports);
	FLAG(F_TRACE_SCHED_PORTS, am_running_ports);
	FLAG(F_TRACE_SCHED_PROCS, am_running_procs);
#undef FLAG0
#undef FLAG
	HRelease(p,limit,hp+3);
	return TUPLE2(hp, key, flag_list);
    } else if (key == am_tracer) {
        if (tracer == am_false)
            tracer = NIL;
        hp = HAlloc(p, 3);
        return TUPLE2(hp, key, tracer);
    } else {
	goto error;
    }
}

#define FUNC_TRACE_NOEXIST      0
#define FUNC_TRACE_UNTRACED     (1<<0)
#define FUNC_TRACE_GLOBAL_TRACE (1<<1)
#define FUNC_TRACE_LOCAL_TRACE  (1<<2)
#define FUNC_TRACE_META_TRACE   (1<<3)
#define FUNC_TRACE_COUNT_TRACE  (1<<4)
#define FUNC_TRACE_TIME_TRACE   (1<<5)
/*
 * Returns either FUNC_TRACE_NOEXIST, FUNC_TRACE_UNTRACED,
 * FUNC_TRACE_GLOBAL_TRACE, or,
 * an or'ed combination of at least one of FUNC_TRACE_LOCAL_TRACE,
 * FUNC_TRACE_META_TRACE, FUNC_TRACE_COUNT_TRACE.
 *
 * If the return value contains FUNC_TRACE_GLOBAL_TRACE 
 * or FUNC_TRACE_LOCAL_TRACE *ms is set.
 *
 * If the return value contains FUNC_TRACE_META_TRACE, 
 * *ms_meta or *tracer_pid_meta is set.
 *
 * If the return value contains FUNC_TRACE_COUNT_TRACE, *count is set.
 */
static int function_is_traced(Process *p,
			      Eterm mfa[3],
			      Binary    **ms,              /* out */
			      Binary    **ms_meta,         /* out */
			      ErtsTracer *tracer_pid_meta, /* out */
			      Uint       *count,           /* out */
			      Eterm      *call_time)       /* out */
{
    Export e;
    Export* ep;
    BeamInstr* pc;
    ErtsCodeInfo *ci;

    /* First look for an export entry */
    e.info.mfa.module = mfa[0];
    e.info.mfa.function = mfa[1];
    e.info.mfa.arity = mfa[2];
    if ((ep = export_get(&e)) != NULL) {
	pc = ep->beam;
	if (ep->addressv[erts_active_code_ix()] == pc &&
	    *pc != (BeamInstr) em_call_error_handler) {

	    int r = 0;

	    ASSERT(*pc == (BeamInstr) em_apply_bif ||
		   *pc == (BeamInstr) BeamOp(op_i_generic_breakpoint));

	    if (erts_is_trace_break(&ep->info, ms, 0)) {
		return FUNC_TRACE_GLOBAL_TRACE;
	    }

	    if (erts_is_trace_break(&ep->info, ms, 1)) {
		r |= FUNC_TRACE_LOCAL_TRACE;
	    }
	    if (erts_is_mtrace_break(&ep->info, ms_meta, tracer_pid_meta)) {
		r |= FUNC_TRACE_META_TRACE;
	    }
	    if (erts_is_time_break(p, &ep->info, call_time)) {
		r |= FUNC_TRACE_TIME_TRACE;
	    }
	    return r ? r : FUNC_TRACE_UNTRACED;
	}
    }
    
    /* OK, now look for breakpoint tracing */
    if ((ci = erts_find_local_func(&e.info.mfa)) != NULL) {
	int r = 
	    (erts_is_trace_break(ci, ms, 1)
	     ? FUNC_TRACE_LOCAL_TRACE : 0) 
	    | (erts_is_mtrace_break(ci, ms_meta, tracer_pid_meta)
	       ? FUNC_TRACE_META_TRACE : 0)
	    | (erts_is_count_break(ci, count)
	       ? FUNC_TRACE_COUNT_TRACE : 0)
	    | (erts_is_time_break(p, ci, call_time)
	       ? FUNC_TRACE_TIME_TRACE : 0);
	
	return r ? r : FUNC_TRACE_UNTRACED;
    } 
    return FUNC_TRACE_NOEXIST;
}

static Eterm
trace_info_func(Process* p, Eterm func_spec, Eterm key)
{
    Eterm* tp;
    Eterm* hp;
    DeclareTmpHeap(mfa,3,p); /* Not really heap here, but might be when setting pattern */
    Binary *ms = NULL, *ms_meta = NULL;
    Uint count = 0;
    Eterm traced = am_false;
    Eterm match_spec = am_false;
    Eterm retval = am_false;
    ErtsTracer meta = erts_tracer_nil;
    Eterm call_time = NIL;
    int r;


    UseTmpHeap(3,p);

    if (!is_tuple(func_spec)) {
	goto error;
    }
    tp = tuple_val(func_spec);
    if (tp[0] != make_arityval(3)) {
	goto error;
    }
    if (!is_atom(tp[1]) || !is_atom(tp[2]) || !is_small(tp[3])) {
	goto error;
    }
    mfa[0] = tp[1];
    mfa[1] = tp[2];
    mfa[2] = signed_val(tp[3]);

#ifdef ERTS_SMP
    if ( (key == am_call_time) || (key == am_all)) {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();
    }
#endif
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_smp_mtx_lock(&erts_dirty_bp_ix_mtx);
#endif


    r = function_is_traced(p, mfa, &ms, &ms_meta, &meta, &count, &call_time);

#ifdef ERTS_DIRTY_SCHEDULERS
    erts_smp_mtx_unlock(&erts_dirty_bp_ix_mtx);
#endif
#ifdef ERTS_SMP
    if ( (key == am_call_time) || (key == am_all)) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
#endif

    switch (r) {
    case FUNC_TRACE_NOEXIST:
	UnUseTmpHeap(3,p);
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_undefined);
    case FUNC_TRACE_UNTRACED:
	UnUseTmpHeap(3,p);
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_false);
    case FUNC_TRACE_GLOBAL_TRACE:
	traced = am_global;
	match_spec = NIL; /* Fix up later if it's asked for*/
	break;
    default:
	if (r & FUNC_TRACE_LOCAL_TRACE) {
	    traced = am_local;
	    match_spec = NIL; /* Fix up later if it's asked for*/
	}
	break;
    }

    switch (key) {
    case am_traced:
	retval = traced;
	break;
    case am_match_spec:
	if (ms) {
	    match_spec = MatchSetGetSource(ms);
	    match_spec = copy_object(match_spec, p);
	}
	retval = match_spec;
	break;
    case am_meta:
        retval = erts_tracer_to_term(p, meta);
        if (retval == am_false)
            /* backwards compatibility */
            retval = NIL;
	break;
    case am_meta_match_spec:
	if (r & FUNC_TRACE_META_TRACE) {
	    if (ms_meta) {
		retval = MatchSetGetSource(ms_meta);
		retval = copy_object(retval, p);
	    } else {
		retval = NIL;
	    }
	}
	break;
    case am_call_count:
	if (r & FUNC_TRACE_COUNT_TRACE) {
	    retval = erts_make_integer(count, p);
	}
	break;
    case am_call_time:
	if (r & FUNC_TRACE_TIME_TRACE) {
	    retval = call_time;
	}
	break;
    case am_all: {
	Eterm match_spec_meta = am_false, c = am_false, t, ct = am_false,
            m = am_false;
	
	if (ms) {
	    match_spec = MatchSetGetSource(ms);
	    match_spec = copy_object(match_spec, p);
	}
	if (r & FUNC_TRACE_META_TRACE) {
	    if (ms_meta) {
		match_spec_meta = MatchSetGetSource(ms_meta);
		match_spec_meta = copy_object(match_spec_meta, p);
	    } else
		match_spec_meta = NIL;
	}
	if (r & FUNC_TRACE_COUNT_TRACE) {
	    c = erts_make_integer(count, p);
	}
	if (r & FUNC_TRACE_TIME_TRACE) {
	    ct = call_time;
	}

        m = erts_tracer_to_term(p, meta);

	hp = HAlloc(p, (3+2)*6);
	retval = NIL;
	t = TUPLE2(hp, am_call_count, c); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_call_time, ct); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_meta_match_spec, match_spec_meta); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_meta, m); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_match_spec, match_spec); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_traced, traced); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
    }   break;
    default:
	goto error;
    }
    UnUseTmpHeap(3,p);
    hp = HAlloc(p, 3);
    return TUPLE2(hp, key, retval);

 error:
    UnUseTmpHeap(3,p);
    BIF_ERROR(p, BADARG);
}

static Eterm
trace_info_on_load(Process* p, Eterm key)
{
    Eterm* hp;
    
    if (! erts_default_trace_pattern_is_on) {
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_false);
    }
    switch (key) {
    case am_traced:
	{
	    Eterm traced = am_false;
	    
	    if (! erts_default_trace_pattern_flags.breakpoint) {
		traced = am_global;
	    } else if (erts_default_trace_pattern_flags.local) {
		traced = am_local;
	    }
	    hp = HAlloc(p, 3);
	    return TUPLE2(hp, key, traced);
	}
    case am_match_spec:
	{
	    Eterm match_spec = am_false;
	    
	    if ((! erts_default_trace_pattern_flags.breakpoint) ||
		erts_default_trace_pattern_flags.local) {
		if (erts_default_match_spec) {
		    match_spec = MatchSetGetSource(erts_default_match_spec);
		    match_spec = copy_object(match_spec, p);
		    hp = HAlloc(p, 3);
		} else {
		    match_spec = NIL;
		    hp = HAlloc(p, 3);
		}
	    } else {
		hp = HAlloc(p, 3);
	    }
	    return TUPLE2(hp, key, match_spec);
	}
    case am_meta:
	hp = HAlloc(p, 3);
	if (erts_default_trace_pattern_flags.meta) {
            ASSERT(!ERTS_TRACER_IS_NIL(erts_default_meta_tracer));
	    return TUPLE2(hp, key, erts_tracer_to_term(p, erts_default_meta_tracer));
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_meta_match_spec:
	{
	    Eterm match_spec = am_false;
	    
	    if (erts_default_trace_pattern_flags.meta) {
		if (erts_default_meta_match_spec) {
		    match_spec = 
			MatchSetGetSource(erts_default_meta_match_spec);
		    match_spec = copy_object(match_spec, p);
		    hp = HAlloc(p, 3);
		} else {
		    match_spec = NIL;
		    hp = HAlloc(p, 3);
		}
	    } else {
		hp = HAlloc(p, 3);
	    }
	    return TUPLE2(hp, key, match_spec);
	}
    case am_call_count:
	hp = HAlloc(p, 3);
	if (erts_default_trace_pattern_flags.call_count) {
	    return TUPLE2(hp, key, am_true);
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_call_time:
	hp = HAlloc(p, 3);
	if (erts_default_trace_pattern_flags.call_time) {
	    return TUPLE2(hp, key, am_true);
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_all:
	{
	    Eterm match_spec = am_false, meta_match_spec = am_false, r = NIL, t, m;
	    
	    if (erts_default_trace_pattern_flags.local ||
		(! erts_default_trace_pattern_flags.breakpoint)) {
		match_spec = NIL;
	    }
	    if (erts_default_match_spec) {
		match_spec = MatchSetGetSource(erts_default_match_spec);
		match_spec = copy_object(match_spec, p);
	    }
	    if (erts_default_trace_pattern_flags.meta) {
		meta_match_spec = NIL;
	    }
	    if (erts_default_meta_match_spec) {
		meta_match_spec = 
		    MatchSetGetSource(erts_default_meta_match_spec);
		meta_match_spec = copy_object(meta_match_spec, p);
	    }
            m = (erts_default_trace_pattern_flags.meta
                 ? erts_tracer_to_term(p, erts_default_meta_tracer) : am_false);
	    hp = HAlloc(p, (3+2)*5 + 3);
	    t = TUPLE2(hp, am_call_count, 
		       (erts_default_trace_pattern_flags.call_count
			? am_true : am_false)); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta_match_spec, meta_match_spec); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta, m); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_match_spec, match_spec); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_traced,
		       (! erts_default_trace_pattern_flags.breakpoint ?
			am_global : (erts_default_trace_pattern_flags.local ?
				     am_local : am_false))); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    return TUPLE2(hp, key, r);
	}
    default:
	BIF_ERROR(p, BADARG);
    }
}

static Eterm
trace_info_event(Process* p, Eterm event, Eterm key)
{
    ErtsTracingEvent* te;
    Eterm retval;
    Eterm* hp;

    switch (event) {
    case am_send:    te = erts_send_tracing;    break;
    case am_receive: te = erts_receive_tracing; break;
    default:
        goto error;
    }

    if (key != am_match_spec)
        goto error;

    te = &te[erts_active_bp_ix()];

    if (te->on) {
        if (!te->match_spec)
            retval = am_true;
        else
            retval = copy_object(MatchSetGetSource(te->match_spec), p);
    }
    else
        retval = am_false;

    hp = HAlloc(p, 3);
    return TUPLE2(hp, key, retval);

 error:
    BIF_ERROR(p, BADARG);
}


#undef FUNC_TRACE_NOEXIST
#undef FUNC_TRACE_UNTRACED
#undef FUNC_TRACE_GLOBAL_TRACE
#undef FUNC_TRACE_LOCAL_TRACE

int
erts_set_trace_pattern(Process*p, ErtsCodeMFA *mfa, int specified,
		       Binary* match_prog_set, Binary *meta_match_prog_set,
		       int on, struct trace_pattern_flags flags,
		       ErtsTracer meta_tracer, int is_blocking)
{
    const ErtsCodeIndex code_ix = erts_active_code_ix();
    int matches = 0;
    int i;
    int n;
    BpFunction* fp;

    /*
     * First work on normal functions (not real BIFs).
     */

    erts_bp_match_export(&finish_bp.e, mfa, specified);
    fp = finish_bp.e.matching;
    n = finish_bp.e.matched;

    for (i = 0; i < n; i++) {
        ErtsCodeInfo *ci = fp[i].ci;
	BeamInstr* pc = erts_codeinfo_to_code(ci);
	Export* ep = ErtsContainerStruct(ci, Export, info);

	if (on && !flags.breakpoint) {
	    /* Turn on global call tracing */
	    if (ep->addressv[code_ix] != pc) {
		fp[i].mod->curr.num_traced_exports++;
#ifdef DEBUG
		ep->info.op = (BeamInstr) BeamOp(op_i_func_info_IaaI);
#endif
                ep->beam[0] = (BeamInstr) BeamOp(op_jump_f);
		ep->beam[1] = (BeamInstr) ep->addressv[code_ix];
	    }
	    erts_set_call_trace_bif(ci, match_prog_set, 0);
	    if (ep->addressv[code_ix] != pc) {
		ep->beam[0] = (BeamInstr) BeamOp(op_i_generic_breakpoint);
	    }
	} else if (!on && flags.breakpoint) {
	    /* Turn off breakpoint tracing -- nothing to do here. */
	} else {
	    /*
	     * Turn off global tracing, either explicitly or implicitly
	     * before turning on breakpoint tracing.
	     */
	    erts_clear_call_trace_bif(ci, 0);
	    if (ep->beam[0] == (BeamInstr) BeamOp(op_i_generic_breakpoint)) {
		ep->beam[0] = (BeamInstr) BeamOp(op_jump_f);
	    }
	}
    }

    /*
    ** OK, now for the bif's
    */
    for (i = 0; i < BIF_SIZE; ++i) {
	Export *ep = bif_export[i];

	if (!ExportIsBuiltIn(ep)) {
	    continue;
	}

	if (bif_table[i].f == bif_table[i].traced) {
	    /* Trace wrapper same as regular function - untraceable */
	    continue;
	}

        switch (specified) {
        case 3:
            if (mfa->arity != ep->info.mfa.arity)
                continue;
        case 2:
            if (mfa->function != ep->info.mfa.function)
                continue;
        case 1:
            if (mfa->module != ep->info.mfa.module)
                continue;
        case 0:
            break;
        default:
            ASSERT(0);
        }

        if (! flags.breakpoint) { /* Export entry call trace */
            if (on) {
                erts_clear_call_trace_bif(&ep->info, 1);
                erts_clear_mtrace_bif(&ep->info);
                erts_set_call_trace_bif(&ep->info, match_prog_set, 0);
            } else { /* off */
                erts_clear_call_trace_bif(&ep->info, 0);
            }
            matches++;
        } else { /* Breakpoint call trace */
            int m = 0;

            if (on) {
                if (flags.local) {
                    erts_clear_call_trace_bif(&ep->info, 0);
                    erts_set_call_trace_bif(&ep->info, match_prog_set, 1);
                    m = 1;
                }
                if (flags.meta) {
                    erts_set_mtrace_bif(&ep->info, meta_match_prog_set,
                                        meta_tracer);
                    m = 1;
                }
                if (flags.call_time) {
                    erts_set_time_trace_bif(&ep->info, on);
                    /* I don't want to remove any other tracers */
                    m = 1;
                }
            } else { /* off */
                if (flags.local) {
                    erts_clear_call_trace_bif(&ep->info, 1);
                    m = 1;
                }
                if (flags.meta) {
                    erts_clear_mtrace_bif(&ep->info);
                    m = 1;
                }
                if (flags.call_time) {
                    erts_clear_time_trace_bif(&ep->info);
                    m = 1;
                }
            }
            matches += m;
        }
    }

    /*
    ** So, now for breakpoint tracing
    */
    erts_bp_match_functions(&finish_bp.f, mfa, specified);
    if (on) {
	if (! flags.breakpoint) {
	    erts_clear_all_breaks(&finish_bp.f);
	} else {
	    if (flags.local) {
		erts_set_trace_break(&finish_bp.f, match_prog_set);
	    }
	    if (flags.meta) {
		erts_set_mtrace_break(&finish_bp.f, meta_match_prog_set,
				      meta_tracer);
	    }
	    if (flags.call_count) {
		erts_set_count_break(&finish_bp.f, on);
	    }
	    if (flags.call_time) {
		erts_set_time_break(&finish_bp.f, on);
	    }
	}
    } else {
	if (flags.local) {
	    erts_clear_trace_break(&finish_bp.f);
	}
	if (flags.meta) {
	    erts_clear_mtrace_break(&finish_bp.f);
	}
	if (flags.call_count) {
	    erts_clear_count_break(&finish_bp.f);
	}
	if (flags.call_time) {
	    erts_clear_time_break(&finish_bp.f);
	}
    }

    finish_bp.current = 0;
    finish_bp.install = on;
    finish_bp.local = flags.breakpoint;

#ifdef ERTS_SMP
    if (is_blocking) {
	ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
#endif
	while (erts_finish_breakpointing()) {
	    /* Empty loop body */
	}
#ifdef ERTS_SMP
	finish_bp.current = -1;
    }
#endif

    if (flags.breakpoint) {
	matches += finish_bp.f.matched;
    } else {
	matches += finish_bp.e.matched;
    }
    return matches;
}

int
erts_set_tracing_event_pattern(Eterm event, Binary* match_spec, int on)
{
    ErtsBpIndex ix = erts_staging_bp_ix();
    ErtsTracingEvent* st;

    switch (event) {
    case am_send: st = &erts_send_tracing[ix]; break;
    case am_receive: st = &erts_receive_tracing[ix]; break;
    default: return -1;
    }

    MatchSetUnref(st->match_spec);

    st->on = on;
    st->match_spec = match_spec;
    MatchSetRef(match_spec);

    finish_bp.current = 1;  /* prepare phase not needed for event trace */
    finish_bp.install = on;
    finish_bp.e.matched = 0;
    finish_bp.e.matching = NULL;
    finish_bp.f.matched = 0;
    finish_bp.f.matching = NULL;

#ifndef ERTS_SMP
    while (erts_finish_breakpointing()) {
        /* Empty loop body */
    }
#endif
    return 1;
}

static void
consolidate_event_tracing(ErtsTracingEvent te[])
{
    ErtsTracingEvent* src = &te[erts_active_bp_ix()];
    ErtsTracingEvent* dst = &te[erts_staging_bp_ix()];

    MatchSetUnref(dst->match_spec);
    dst->on = src->on;
    dst->match_spec = src->match_spec;
    MatchSetRef(dst->match_spec);
}

int
erts_finish_breakpointing(void)
{
    ERTS_SMP_LC_ASSERT(erts_has_code_write_permission());

    /*
     * Memory barriers will be issued for all schedulers *before*
     * each of the stages below. (Unless the other schedulers
     * are blocked, in which case memory barriers will be issued
     * when they are awaken.)
     */
    switch (finish_bp.current++) {
    case 0:
	/*
	 * At this point, in all functions that are to be breakpointed,
	 * a pointer to a GenericBp struct has already been added,
	 *
	 * Insert the new breakpoints (if any) into the
	 * code. Different schedulers may see breakpoint instruction
	 * at different times, but it does not matter since the newly
	 * added breakpoints are disabled.
	 */
	if (finish_bp.install) {
	    if (finish_bp.local) {
		erts_install_breakpoints(&finish_bp.f);
	    } else {
		install_exp_breakpoints(&finish_bp.e);
	    }
	}
	setup_bif_trace();
	return 1;
    case 1:
	/*
	 * Switch index for the breakpoint data, activating the staged
	 * data. (Depending on the changes in the breakpoint data,
	 * that could either activate breakpoints or disable
	 * breakpoints.)
	 */
	erts_commit_staged_bp();
	return 1;
    case 2:
	/*
	 * Remove breakpoints instructions for disabled breakpoints
	 * (if any).
	 */
	if (finish_bp.install) {
	    if (finish_bp.local) {
		uninstall_exp_breakpoints(&finish_bp.e);
	    } else {
		erts_uninstall_breakpoints(&finish_bp.f);
	    }
	} else {
	    if (finish_bp.local) {
		erts_uninstall_breakpoints(&finish_bp.f);
	    } else {
		uninstall_exp_breakpoints(&finish_bp.e);
	    }
	}
	reset_bif_trace();
	return 1;
    case 3:
	/*
	 * Now all breakpoints have either been inserted or removed.
	 * For all updated breakpoints, copy the active breakpoint
	 * data to the staged breakpoint data to make them equal
	 * (simplifying for the next time breakpoints are to be
	 * updated).  If any breakpoints have been totally disabled,
	 * deallocate the GenericBp structs for them.
	 */
	erts_consolidate_bif_bp_data();
	clean_export_entries(&finish_bp.e);
	erts_consolidate_bp_data(&finish_bp.e, 0);
	erts_consolidate_bp_data(&finish_bp.f, 1);
	erts_bp_free_matched_functions(&finish_bp.e);
	erts_bp_free_matched_functions(&finish_bp.f);
        consolidate_event_tracing(erts_send_tracing);
	consolidate_event_tracing(erts_receive_tracing);
	return 0;
    default:
	ASSERT(0);
    }
    return 0;
}

static void
install_exp_breakpoints(BpFunctions* f)
{
    const ErtsCodeIndex code_ix = erts_active_code_ix();
    BpFunction* fp = f->matching;
    Uint ne = f->matched;
    Uint i;

    for (i = 0; i < ne; i++) {
	Export* ep = ErtsContainerStruct(fp[i].ci, Export, info);

	ep->addressv[code_ix] = ep->beam;
    }
}

static void
uninstall_exp_breakpoints(BpFunctions* f)
{
    const ErtsCodeIndex code_ix = erts_active_code_ix();
    BpFunction* fp = f->matching;
    Uint ne = f->matched;
    Uint i;

    for (i = 0; i < ne; i++) {
	Export* ep = ErtsContainerStruct(fp[i].ci, Export, info);

	if (ep->addressv[code_ix] != ep->beam) {
	    continue;
	}
	ASSERT(ep->beam[0] == (BeamInstr) BeamOp(op_jump_f));
	ep->addressv[code_ix] = (BeamInstr *) ep->beam[1];
    }
}

static void
clean_export_entries(BpFunctions* f)
{
    const ErtsCodeIndex code_ix = erts_active_code_ix();
    BpFunction* fp = f->matching;
    Uint ne = f->matched;
    Uint i;

    for (i = 0; i < ne; i++) {
	Export* ep = ErtsContainerStruct(fp[i].ci, Export, info);

	if (ep->addressv[code_ix] == ep->beam) {
	    continue;
	}
	if (ep->beam[0] == (BeamInstr) BeamOp(op_jump_f)) {
	    ep->beam[0] = (BeamInstr) 0;
	    ep->beam[1] = (BeamInstr) 0;
	}
    }
}

static void
setup_bif_trace(void)
{
    int i;

    for (i = 0; i < BIF_SIZE; ++i) {
	Export *ep = bif_export[i];
	GenericBp* g = ep->info.u.gen_bp;
	if (g) {
	    if (ExportIsBuiltIn(ep)) {
		ASSERT(ep->beam[1]);
		ep->beam[1] = (BeamInstr) bif_table[i].traced;
	    }
	}
    }
}

static void
reset_bif_trace(void)
{
    int i;
    ErtsBpIndex active = erts_active_bp_ix();

    for (i = 0; i < BIF_SIZE; ++i) {
	Export *ep = bif_export[i];
	GenericBp* g = ep->info.u.gen_bp;
	if (g && g->data[active].flags == 0) {
	    if (ExportIsBuiltIn(ep)) {
		ASSERT(ep->beam[1]);
		ep->beam[1] = (BeamInstr) bif_table[i].f;
	    }
	}
    }
}

/*
 * Sequential tracing
 *
 * The sequential trace token is internally implemented as
 * a tuple
 *         {Flags, Label, Serial, Sender, LastSerial}
 * 
 * where 
 *       - Flags is an integer (using masks 1, 2, and 4, for send,
 *         receive and print, respectively), 
 *       - Label is any term, Serial (for now XXX) is an integer (it should
 *         be a list reflecting split traces), and 
 *       - Sender is the Pid of the sender (i.e. the current process, 
 *         except immediately after a message reception, in case it is
 *         the pid of the process that sent the message).
 *
 */

BIF_RETTYPE seq_trace_2(BIF_ALIST_2)    
{
    Eterm res;
    res = erts_seq_trace(BIF_P, BIF_ARG_1, BIF_ARG_2, 1);
    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
}

Eterm erts_seq_trace(Process *p, Eterm arg1, Eterm arg2, 
			  int build_result)
{
    Eterm flags;
    Eterm old_value = am_true;
    Eterm* hp;
    int current_flag;

    if (!is_atom(arg1)) {
	return THE_NON_VALUE;
    }


    if (arg1 == am_send) {
	current_flag = SEQ_TRACE_SEND;
    } else if (arg1 == am_receive) {
	current_flag = SEQ_TRACE_RECEIVE; 
    } else if (arg1 == am_print) {
	current_flag = SEQ_TRACE_PRINT; 
    } else if (arg1 == am_timestamp) {
	current_flag = SEQ_TRACE_NOW_TS; 
    } else if (arg1 == am_strict_monotonic_timestamp) {
	current_flag = SEQ_TRACE_STRICT_MON_TS; 
    } else if (arg1 == am_monotonic_timestamp) {
	current_flag = SEQ_TRACE_MON_TS; 
    }
    else
	current_flag = 0;

    if (current_flag && ( (arg2 == am_true) || (arg2 == am_false)) ) {
	/* Flags */
        new_seq_trace_token(p);
        flags = unsigned_val(SEQ_TRACE_TOKEN_FLAGS(p));
	if (build_result) {
	    old_value = flags & current_flag ? am_true : am_false;
	} 
	if (arg2 == am_true)
	    SEQ_TRACE_TOKEN_FLAGS(p) = make_small(flags|current_flag);
	else if (arg2 == am_false)
	    SEQ_TRACE_TOKEN_FLAGS(p) = make_small(flags&~current_flag);
	else { 
	    return THE_NON_VALUE;
	}
	return old_value;
    }
    else if (arg1 == am_label) {
	if (! is_small(arg2)) {
	    return THE_NON_VALUE;
	}
        new_seq_trace_token(p);
	if (build_result) {
	    old_value = SEQ_TRACE_TOKEN_LABEL(p);
	}
	SEQ_TRACE_TOKEN_LABEL(p) = arg2;
    	return old_value;
    }
    else if (arg1 == am_serial) {
	Eterm* tp;
	if (is_not_tuple(arg2)) {
	    return THE_NON_VALUE;
	}
	tp = tuple_val(arg2);
	if ((*tp != make_arityval(2)) || is_not_small(*(tp+1)) || is_not_small(*(tp+2))) {
	    return THE_NON_VALUE;
        }
        new_seq_trace_token(p);
	if (build_result) {
	    hp = HAlloc(p,3);
	    old_value = TUPLE2(hp, SEQ_TRACE_TOKEN_LASTCNT(p),
			       SEQ_TRACE_TOKEN_SERIAL(p));
	}
	SEQ_TRACE_TOKEN_LASTCNT(p) = *(tp+1);
 	SEQ_TRACE_TOKEN_SERIAL(p) = *(tp+2);
	p->seq_trace_clock = unsigned_val(*(tp+2));
	p->seq_trace_lastcnt = unsigned_val(*(tp+1));
    	return old_value;
    }
    else if (arg1 == am_sequential_trace_token) {
	if (is_not_nil(arg2)) {
	    return THE_NON_VALUE;
        }
	if (build_result) {
#ifdef USE_VM_PROBES
	    old_value = (SEQ_TRACE_TOKEN(p) == am_have_dt_utag) ? NIL : SEQ_TRACE_TOKEN(p);
#else
	    old_value = SEQ_TRACE_TOKEN(p);
#endif
	}
#ifdef USE_VM_PROBES
        SEQ_TRACE_TOKEN(p) = (DT_UTAG(p) != NIL) ? am_have_dt_utag : NIL;
#else
        SEQ_TRACE_TOKEN(p) = NIL;
#endif
        return old_value;
    }
    else {
	return THE_NON_VALUE;
    }
}

void
new_seq_trace_token(Process* p)
{
    Eterm* hp;

    if (have_no_seqtrace(SEQ_TRACE_TOKEN(p))) {
	hp = HAlloc(p, 6);
	SEQ_TRACE_TOKEN(p) = TUPLE5(hp, make_small(0),		/* Flags  */ 
				    make_small(0),		/* Label  */
				    make_small(0),		/* Serial */
				    p->common.id, /* Internal pid */	/* From   */
				    make_small(p->seq_trace_lastcnt));
    }
}

BIF_RETTYPE erl_seq_trace_info(Process *p, Eterm item)
{
    Eterm res;
    Eterm* hp;
    Uint current_flag;

    if (is_not_atom(item)) {
	BIF_ERROR(p, BADARG);
    }

    if (have_no_seqtrace(SEQ_TRACE_TOKEN(p))) {
	if ((item == am_send)  || (item == am_receive) || 
	    (item == am_print) || (item == am_timestamp)
	    || (item == am_monotonic_timestamp)
	    || (item == am_strict_monotonic_timestamp)) {
	    hp = HAlloc(p,3);
	    res = TUPLE2(hp, item, am_false);
	    BIF_RET(res);
	} else if ((item == am_label) || (item == am_serial)) {
	    BIF_RET(NIL);
	} else {
	    goto error;
	}
    }

    if (item == am_send) {
	current_flag = SEQ_TRACE_SEND;
    } else if (item == am_receive) {
	current_flag = SEQ_TRACE_RECEIVE; 
    } else if (item == am_print) {
	current_flag = SEQ_TRACE_PRINT; 
    } else if (item == am_timestamp) {
	current_flag = SEQ_TRACE_NOW_TS; 
    } else if (item == am_strict_monotonic_timestamp) {
	current_flag = SEQ_TRACE_STRICT_MON_TS; 
    } else if (item == am_monotonic_timestamp) {
	current_flag = SEQ_TRACE_MON_TS; 
    } else {
	current_flag = 0;
    }

    if (current_flag) {
	res = unsigned_val(SEQ_TRACE_TOKEN_FLAGS(p)) & current_flag ?
	    am_true : am_false;
    } else if (item == am_label) {
	res = SEQ_TRACE_TOKEN_LABEL(p);
    } else if (item  == am_serial) {
	hp = HAlloc(p, 3);
	res = TUPLE2(hp, SEQ_TRACE_TOKEN_LASTCNT(p), SEQ_TRACE_TOKEN_SERIAL(p));
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }
    hp = HAlloc(p, 3);
    res = TUPLE2(hp, item, res);
    BIF_RET(res);
}

BIF_RETTYPE seq_trace_info_1(BIF_ALIST_1)
{
    BIF_RET(erl_seq_trace_info(BIF_P, BIF_ARG_1));
}

/*
   seq_trace_print(Message) -> true | false
   This function passes Message to the system_tracer
   if the trace_token is not NIL.
   Returns true if Message is passed else false
   Note! That true is returned if the conditions to pass Message is
   fulfilled, but nothing is passed if system_seq_tracer is not set.
 */
BIF_RETTYPE seq_trace_print_1(BIF_ALIST_1)    
{
    if (have_no_seqtrace(SEQ_TRACE_TOKEN(BIF_P))) {
	BIF_RET(am_false);
    }
    seq_trace_update_send(BIF_P);
    seq_trace_output(SEQ_TRACE_TOKEN(BIF_P), BIF_ARG_1, 
		     SEQ_TRACE_PRINT, NIL, BIF_P);
    BIF_RET(am_true);
}

/*
   seq_trace_print(Label,Message) -> true | false
   This function passes Message to the system_tracer
   if the trace_token is not NIL and the trace_token label is equal to
   Label. Returns true if Message is passed else false
   Note! That true is returned if the conditions to pass Message is
   fulfilled, but nothing is passed if system_seq_tracer is not set.
 */
BIF_RETTYPE seq_trace_print_2(BIF_ALIST_2)    
{
    if (have_no_seqtrace(SEQ_TRACE_TOKEN(BIF_P))) {
	BIF_RET(am_false);
    }
    if (!(is_atom(BIF_ARG_1) || is_small(BIF_ARG_1))) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (SEQ_TRACE_TOKEN_LABEL(BIF_P) != BIF_ARG_1)
	BIF_RET(am_false);
    seq_trace_update_send(BIF_P);
    seq_trace_output(SEQ_TRACE_TOKEN(BIF_P), BIF_ARG_2, 
		     SEQ_TRACE_PRINT, NIL, BIF_P);
    BIF_RET(am_true);
}

void erts_system_monitor_clear(Process *c_p) {
#ifdef ERTS_SMP
    if (c_p) {
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();
    }
#endif
    erts_set_system_monitor(NIL);
    erts_system_monitor_long_gc = 0;
    erts_system_monitor_long_schedule = 0;
    erts_system_monitor_large_heap = 0;
    erts_system_monitor_flags.busy_port = 0;
    erts_system_monitor_flags.busy_dist_port = 0;
#ifdef ERTS_SMP
    if (c_p) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
#endif
}


static Eterm system_monitor_get(Process *p)
{
    Eterm *hp;
    Eterm system_monitor = erts_get_system_monitor();
    
    if (system_monitor == NIL) {
	return am_undefined;
    } else {
	Eterm res;
	Uint hsz = 3 + (erts_system_monitor_flags.busy_dist_port ? 2 : 0) +
	    (erts_system_monitor_flags.busy_port ? 2 : 0); 
	Eterm long_gc = NIL;
	Eterm long_schedule = NIL;
	Eterm large_heap = NIL;

	if (erts_system_monitor_long_gc != 0) {
	    hsz += 2+3;
	    (void) erts_bld_uint(NULL, &hsz, erts_system_monitor_long_gc);
	}
	if (erts_system_monitor_long_schedule != 0) {
	    hsz += 2+3;
	    (void) erts_bld_uint(NULL, &hsz, erts_system_monitor_long_schedule);
	}
	if (erts_system_monitor_large_heap != 0) {
	    hsz += 2+3;
	    (void) erts_bld_uint(NULL, &hsz, erts_system_monitor_large_heap);
	}

	hp = HAlloc(p, hsz);
	if (erts_system_monitor_long_gc != 0) {
	    long_gc = erts_bld_uint(&hp, NULL, erts_system_monitor_long_gc);
	}
	if (erts_system_monitor_long_schedule != 0) {
	    long_schedule = erts_bld_uint(&hp, NULL, 
					  erts_system_monitor_long_schedule);
	}
	if (erts_system_monitor_large_heap != 0) {
	    large_heap = erts_bld_uint(&hp, NULL, erts_system_monitor_large_heap);
	}
	res = NIL;
	if (long_gc != NIL) {
	    Eterm t = TUPLE2(hp, am_long_gc, long_gc); hp += 3;
	    res = CONS(hp, t, res); hp += 2;
	}
	if (long_schedule != NIL) {
	    Eterm t = TUPLE2(hp, am_long_schedule, long_schedule); hp += 3;
	    res = CONS(hp, t, res); hp += 2;
	}
	if (large_heap != NIL) {
	    Eterm t = TUPLE2(hp, am_large_heap, large_heap); hp += 3;
	    res = CONS(hp, t, res); hp += 2;
	}
	if (erts_system_monitor_flags.busy_port) {
	    res = CONS(hp, am_busy_port, res); hp += 2;
	}
	if (erts_system_monitor_flags.busy_dist_port) {
	    res = CONS(hp, am_busy_dist_port, res); hp += 2;
	}
	return TUPLE2(hp, system_monitor, res);
    }
}


BIF_RETTYPE system_monitor_0(BIF_ALIST_0)
{
    BIF_RET(system_monitor_get(BIF_P));
}

BIF_RETTYPE system_monitor_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm spec = BIF_ARG_1;

    if (spec == am_undefined) {
	BIF_RET(system_monitor(p, spec, NIL));
    } else if (is_tuple(spec)) {
	Eterm *tp = tuple_val(spec);
	if (tp[0] != make_arityval(2)) goto error;
	BIF_RET(system_monitor(p, tp[1], tp[2]));
    }
 error:
    BIF_ERROR(p, BADARG);
}

BIF_RETTYPE system_monitor_2(BIF_ALIST_2)
{
    return system_monitor(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

static BIF_RETTYPE
system_monitor(Process *p, Eterm monitor_pid, Eterm list)
{
    Eterm prev;
    int system_blocked = 0;

    if (monitor_pid == am_undefined || list == NIL) {
	prev = system_monitor_get(p);
	erts_system_monitor_clear(p);
	BIF_RET(prev);
    }
    if (is_not_list(list)) goto error;
    else {
	Uint long_gc, long_schedule, large_heap;
	int busy_port, busy_dist_port;

	system_blocked = 1;
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	if (!erts_pid2proc(p, ERTS_PROC_LOCK_MAIN, monitor_pid, 0))
	    goto error;

	for (long_gc = 0, long_schedule = 0, large_heap = 0, 
		 busy_port = 0, busy_dist_port = 0;
	     is_list(list);
	     list = CDR(list_val(list))) {
	    Eterm t = CAR(list_val(list));
	    if (is_tuple(t)) {
		Eterm *tp = tuple_val(t);
		if (arityval(tp[0]) != 2) goto error;
		if (tp[1] == am_long_gc) {
		    if (! term_to_Uint(tp[2], &long_gc)) goto error;
		    if (long_gc < 1) long_gc = 1;
		} else if (tp[1] == am_long_schedule) {
		    if (! term_to_Uint(tp[2], &long_schedule)) goto error;
		    if (long_schedule < 1) long_schedule = 1;
		} else if (tp[1] == am_large_heap) {
		    if (! term_to_Uint(tp[2], &large_heap)) goto error;
		    if (large_heap < 16384) large_heap = 16384;
		    /* 16 Kword is not an unnatural heap size */
		} else goto error;
	    } else if (t == am_busy_port) {
		busy_port = !0;
	    } else if (t == am_busy_dist_port) {
		busy_dist_port = !0;
	    } else goto error;
	}
	if (is_not_nil(list)) goto error;
	prev = system_monitor_get(p);
	erts_set_system_monitor(monitor_pid);
	erts_system_monitor_long_gc = long_gc;
	erts_system_monitor_long_schedule = long_schedule;
	erts_system_monitor_large_heap = large_heap;
	erts_system_monitor_flags.busy_port = !!busy_port;
	erts_system_monitor_flags.busy_dist_port = !!busy_dist_port;

	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
	BIF_RET(prev);
    }

 error:

    if (system_blocked) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }

    BIF_ERROR(p, BADARG);
}

/* Begin: Trace for System Profiling */

void erts_system_profile_clear(Process *c_p) {
#ifdef ERTS_SMP
    if (c_p) {
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();
    }
#endif
    erts_set_system_profile(NIL);
    erts_system_profile_flags.scheduler = 0;
    erts_system_profile_flags.runnable_procs = 0;
    erts_system_profile_flags.runnable_ports = 0;
    erts_system_profile_flags.exclusive = 0;
#ifdef ERTS_SMP
    if (c_p) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
#endif
}

static Eterm system_profile_get(Process *p) {
    Eterm *hp;
    Eterm system_profile = erts_get_system_profile();
    if (system_profile == NIL) {
    	return am_undefined;
    } else {
    	Eterm res;
	Uint hsz = 3
		 + (erts_system_profile_flags.scheduler ? 2 : 0) 
		 + (erts_system_profile_flags.runnable_ports ? 2 : 0) 
		 + (erts_system_profile_flags.exclusive ? 2 : 0) 
		 + (erts_system_profile_flags.runnable_procs ? 2 : 0); 
	
	hp = HAlloc(p, hsz);
	res = NIL;
	if (erts_system_profile_flags.runnable_ports) {
	    res = CONS(hp, am_runnable_ports, res); hp += 2;
	}
	if (erts_system_profile_flags.runnable_procs) {
	    res = CONS(hp, am_runnable_procs, res); hp += 2;
	}
	if (erts_system_profile_flags.scheduler) {
	    res = CONS(hp, am_scheduler, res); hp += 2;
	}
	if (erts_system_profile_flags.exclusive) {
	    res = CONS(hp, am_exclusive, res); hp += 2;
	}

    	return TUPLE2(hp, system_profile, res);
    }
}

BIF_RETTYPE system_profile_0(BIF_ALIST_0)
{
    BIF_RET(system_profile_get(BIF_P));
}

BIF_RETTYPE system_profile_2(BIF_ALIST_2)
{
    Process *p = BIF_P;
    Eterm profiler = BIF_ARG_1;
    Eterm list = BIF_ARG_2;
    Eterm prev;
    int system_blocked = 0;
    Process *profiler_p = NULL;
    Port *profiler_port = NULL;
    int ts;

    if (profiler == am_undefined || list == NIL) {
	prev = system_profile_get(p);
	erts_system_profile_clear(p);
	BIF_RET(prev);
    }
    if (is_not_list(list)) {
	goto error;
    } else {
	int scheduler, runnable_procs, runnable_ports, exclusive;
	system_blocked = 1;
	
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();

	/* Check if valid process, no locks are taken */

	if (is_internal_pid(profiler)) {
	    profiler_p = erts_proc_lookup(profiler);
	    if (!profiler_p)
		goto error;
	} else if (is_internal_port(profiler)) {
	    profiler_port = (erts_port_lookup(
				 profiler,
				 ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP));
	    if (!profiler_port)
		goto error;
	} else {
	    goto error;
	}

	for (ts = ERTS_TRACE_FLG_NOW_TIMESTAMP, scheduler = 0,
		 runnable_ports = 0, runnable_procs = 0, exclusive = 0;
	    is_list(list);
	    list = CDR(list_val(list))) {
	    
	    Eterm t = CAR(list_val(list));
	    if (t == am_runnable_procs) {
	   	 runnable_procs = !0;
	    } else if (t == am_runnable_ports) {
		runnable_ports = !0;
	    } else if (t == am_exclusive) {
		exclusive = !0;
	    } else if (t == am_scheduler) {
		scheduler = !0;
	    } else if (t == am_timestamp) {
		ts = ERTS_TRACE_FLG_NOW_TIMESTAMP;
	    } else if (t == am_strict_monotonic_timestamp) {
		ts = ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP;
	    } else if (t == am_monotonic_timestamp) {
		ts = ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP;
	    } else goto error;
	}	
	if (is_not_nil(list)) goto error;
	prev = system_profile_get(p);
	erts_set_system_profile(profiler);

	erts_system_profile_flags.scheduler = !!scheduler;
	if (erts_system_profile_flags.scheduler)
	    erts_system_profile_setup_active_schedulers();
	erts_system_profile_flags.runnable_ports = !!runnable_ports;
	erts_system_profile_flags.runnable_procs = !!runnable_procs;
	erts_system_profile_flags.exclusive = !!exclusive;
	erts_system_profile_ts_type = ts;
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
	
	BIF_RET(prev);
		
    }

    error:
	if (system_blocked) {
	    erts_smp_thr_progress_unblock();
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    	}

    BIF_ERROR(p, BADARG);
}
/* End: Trace for System Profiling */

/* Trace delivered send an aux work message to all schedulers
   and when all schedulers have acknowledged that they have seen
   the message the message is sent to the requesting process.

   IMPORTANT: We have to make sure that the all messages sent
   using enif_send have been delivered before we send the message
   to the caller.

   There used to be a separate implementation for when only a pid
   is passed in, but since this is not performance critical code
   we now use the same approach for both.
*/

typedef struct {
    Process *proc;
    Eterm ref;
    Eterm ref_heap[ERTS_REF_THING_SIZE];
    Eterm target;
    erts_smp_atomic32_t refc;
} ErtsTraceDeliveredAll;

static void
reply_trace_delivered_all(void *vtdarp)
{
    ErtsTraceDeliveredAll *tdarp = (ErtsTraceDeliveredAll *) vtdarp;

    if (erts_smp_atomic32_dec_read_nob(&tdarp->refc) == 0) {
        Eterm ref_copy, msg;
        Process *rp = tdarp->proc;
        Eterm *hp = NULL;
        ErlOffHeap *ohp;
#ifdef ERTS_SMP
        ErlHeapFragment *bp;
        bp = new_message_buffer(4 + NC_HEAP_SIZE(tdarp->ref));
        hp = &bp->mem[0];
        ohp = &bp->off_heap;
#else
        ErtsProcLocks rp_locks = 0;
        ErtsMessage *mp;
        mp = erts_alloc_message_heap(
            rp, &rp_locks, 4 + NC_HEAP_SIZE(tdarp->ref), &hp, &ohp);
#endif

        ref_copy = STORE_NC(&hp, ohp, tdarp->ref);
        msg = TUPLE3(hp, am_trace_delivered, tdarp->target, ref_copy);

#ifdef ERTS_SMP
        erts_send_sys_msg_proc(rp->common.id, rp->common.id, msg, bp);
#else
        erts_queue_message(rp, rp_locks, mp, msg, am_system);
#endif

	erts_free(ERTS_ALC_T_MISC_AUX_WORK, vtdarp);
        erts_proc_dec_refc(rp);
    }
}

BIF_RETTYPE
trace_delivered_1(BIF_ALIST_1)
{

    if (BIF_ARG_1 == am_all || is_internal_pid(BIF_ARG_1)) {
        Eterm *hp, ref;
        ErtsTraceDeliveredAll *tdarp =
            erts_alloc(ERTS_ALC_T_MISC_AUX_WORK, sizeof(ErtsTraceDeliveredAll));

        tdarp->proc = BIF_P;
        ref = erts_make_ref(BIF_P);
        hp = &tdarp->ref_heap[0];
        tdarp->ref = STORE_NC(&hp, NULL, ref);
        tdarp->target = BIF_ARG_1;
        erts_smp_atomic32_init_nob(&tdarp->refc,
                                   (erts_aint32_t) erts_no_schedulers);
        erts_proc_add_refc(BIF_P, 1);
        erts_schedule_multi_misc_aux_work(0,
                                          erts_no_schedulers,
                                          reply_trace_delivered_all,
                                          (void *) tdarp);
        BIF_RET(ref);
    } else {
        BIF_ERROR(BIF_P, BADARG);
    }
}
