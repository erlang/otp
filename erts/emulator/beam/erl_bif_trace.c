/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

const struct trace_pattern_flags   erts_trace_pattern_flags_off = {0, 0, 0, 0, 0};
static int                         erts_default_trace_pattern_is_on;
static Binary                     *erts_default_match_spec;
static Binary                     *erts_default_meta_match_spec;
static struct trace_pattern_flags  erts_default_trace_pattern_flags;
static Eterm                       erts_default_meta_tracer_pid;

static void new_seq_trace_token(Process* p); /* help func for seq_trace_2*/
static int already_traced(Process *p, Process *tracee_p, Eterm tracer);
static int port_already_traced(Process *p, Port *tracee_port, Eterm tracer);
static Eterm trace_info_pid(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_func(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_on_load(Process* p, Eterm key);

static int setup_func_trace(Export* ep, void* match_prog);
static int reset_func_trace(Export* ep);
static void reset_bif_trace(int bif_index);
static void setup_bif_trace(int bif_index);
static void set_trace_bif(int bif_index, void* match_prog);
static void clear_trace_bif(int bif_index);

void
erts_bif_trace_init(void)
{
    erts_default_trace_pattern_is_on = 0;
    erts_default_match_spec = NULL;
    erts_default_meta_match_spec = NULL;
    erts_default_trace_pattern_flags = erts_trace_pattern_flags_off;
    erts_default_meta_tracer_pid = NIL;
}

/*
 * Turn on/off call tracing for the given function(s).
 */
  
Eterm
trace_pattern_2(Process* p, Eterm MFA, Eterm Pattern)
{
    return trace_pattern_3(p,MFA,Pattern,NIL);
}

Eterm
trace_pattern_3(Process* p, Eterm MFA, Eterm Pattern, Eterm flaglist)       
{
    DeclareTmpHeap(mfa,3,p); /* Not really heap here, but might be when setting pattern */
    int i;
    int matches = 0;
    int specified = 0;
    enum erts_break_op on;
    Binary* match_prog_set;
    Eterm l;
    struct trace_pattern_flags flags = erts_trace_pattern_flags_off;
    int is_global;
    Process *meta_tracer_proc = p;
    Eterm meta_tracer_pid = p->id;

    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

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
	on = erts_break_reset;
    } else if (Pattern == am_pause) {
	match_prog_set = NULL;
	on = erts_break_stop;
    } else if ((match_prog_set = erts_match_set_compile(p, Pattern)) != NULL) {
	MatchSetRef(match_prog_set);
	on = 1;
    } else{
	goto error;
    }
    
    is_global = 0;
    for(l = flaglist; is_list(l); l = CDR(list_val(l))) {
	if (is_tuple(CAR(list_val(l)))) {
	    Eterm *tp = tuple_val(CAR(list_val(l)));
	    
	    if (arityval(tp[0]) != 2 || tp[1] != am_meta) {
		goto error;
	    }
	    meta_tracer_pid = tp[2];
	    if (is_internal_pid(meta_tracer_pid)) {
		meta_tracer_proc = erts_pid2proc(NULL, 0, meta_tracer_pid, 0);
		if (!meta_tracer_proc) {
		    goto error;
		}
	    } else if (is_internal_port(meta_tracer_pid)) {
		Port *meta_tracer_port;
		meta_tracer_proc = NULL;		
		if (internal_port_index(meta_tracer_pid) >= erts_max_ports)
		    goto error;
		meta_tracer_port = 
		    &erts_port[internal_port_index(meta_tracer_pid)];
		if (INVALID_TRACER_PORT(meta_tracer_port, meta_tracer_pid)) {
		    goto error;
		}
	    } else {
		goto error;
	    }
	    if (is_global) {
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
	    erts_default_meta_tracer_pid = meta_tracer_pid;
	    if (meta_tracer_proc) {
		meta_tracer_proc->trace_flags |= F_TRACER;
	    }
	} else if (! flags.breakpoint) {
	    MatchSetUnref(erts_default_meta_match_spec);
	    erts_default_meta_match_spec = NULL;
	    erts_default_meta_tracer_pid = NIL;
	}
	MatchSetUnref(match_prog_set);
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

	goto done;
    } else if (is_tuple(MFA)) {
	Eterm *tp = tuple_val(MFA);
	if (tp[0] != make_arityval(3)) {
	    goto error;
	}
	mfa[0] = tp[1];
	mfa[1] = tp[2];
	mfa[2] = tp[3];
	if (!is_atom(mfa[0]) || !is_atom(mfa[1]) ||
	    (!is_small(mfa[2]) && mfa[2] != am_Underscore)) {
	    goto error;
	}
	for (i = 0; i < 3 && mfa[i] != am_Underscore; i++, specified++) {
	    /* Empty loop body */
	}
	for (i = specified; i < 3; i++) {
	    if (mfa[i] != am_Underscore) {
		goto error;
	    }
	}
	if (is_small(mfa[2])) {
	    mfa[2] = signed_val(mfa[2]);
	}
    } else {
	goto error;
    }
    
    if (meta_tracer_proc) {
	meta_tracer_proc->trace_flags |= F_TRACER;
    }


    matches = erts_set_trace_pattern(mfa, specified, 
				     match_prog_set, match_prog_set,
				     on, flags, meta_tracer_pid);
    MatchSetUnref(match_prog_set);

 done:
    UnUseTmpHeap(3,p);
    erts_smp_release_system();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);

    return make_small(matches);

 error:

    MatchSetUnref(match_prog_set);

    UnUseTmpHeap(3,p);
    erts_smp_release_system();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    BIF_ERROR(p, BADARG);
}

void
erts_get_default_trace_pattern(int *trace_pattern_is_on,
			       Binary **match_spec,
			       Binary **meta_match_spec,
			       struct trace_pattern_flags *trace_pattern_flags,
			       Eterm *meta_tracer_pid)
{
    if (trace_pattern_is_on)
	*trace_pattern_is_on = erts_default_trace_pattern_is_on;
    if (match_spec)
	*match_spec = erts_default_match_spec;
    if (meta_match_spec)
	*meta_match_spec = erts_default_meta_match_spec;
    if (trace_pattern_flags)
	*trace_pattern_flags = erts_default_trace_pattern_flags;
    if (meta_tracer_pid)
	*meta_tracer_pid = erts_default_meta_tracer_pid;
}



Uint 
erts_trace_flag2bit(Eterm flag) 
{
    switch (flag) {
    case am_all: return TRACEE_FLAGS;
    case am_send: return F_TRACE_SEND;
    case am_receive: return F_TRACE_RECEIVE;
    case am_set_on_spawn: return F_TRACE_SOS;
    case am_procs: return F_TRACE_PROCS;
    case am_set_on_first_spawn: return F_TRACE_SOS1;
    case am_set_on_link: return F_TRACE_SOL;
    case am_set_on_first_link: return F_TRACE_SOL1;
    case am_timestamp: return F_TIMESTAMP;
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
		 Uint *pMask, Eterm *pTracer, int *pCpuTimestamp)
{
    Eterm list = List;
    Uint mask = 0;
    Eterm tracer = NIL;
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
	    Eterm* tp = tuple_val(item);
	    
	    if (arityval(tp[0]) != 2 || tp[1] != am_tracer) goto error;
	    if (is_internal_pid(tp[2]) || is_internal_port(tp[2])) {
		tracer = tp[2];
	    } else goto error;
	} else goto error;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) goto error;
    
    if (pMask && mask)                  *pMask         = mask;
    if (pTracer && tracer != NIL)       *pTracer       = tracer;
    if (pCpuTimestamp && cpu_timestamp) *pCpuTimestamp = cpu_timestamp;
    return !0;
 error:
    return 0;
}

Eterm
trace_3(Process* p, Eterm pid_spec, Eterm how, Eterm list)
{
    int on;
    Eterm tracer = NIL;
    int matches = 0;
    Uint mask = 0;
    int cpu_ts = 0;
#ifdef ERTS_SMP
    int system_blocked = 0;
#endif

    if (! erts_trace_flags(list, &mask, &tracer, &cpu_ts)) {
	BIF_ERROR(p, BADARG);
    }

    if (is_nil(tracer) || is_internal_pid(tracer)) {
	Process *tracer_proc = erts_pid2proc(p,
					     ERTS_PROC_LOCK_MAIN,
					     is_nil(tracer) ? p->id : tracer,
					     ERTS_PROC_LOCKS_ALL);
	if (!tracer_proc)
	    goto error;
	tracer_proc->trace_flags |= F_TRACER;
	erts_smp_proc_unlock(tracer_proc,
			     (tracer_proc == p
			      ? ERTS_PROC_LOCKS_ALL_MINOR
			      : ERTS_PROC_LOCKS_ALL));
    } else if (is_internal_port(tracer)) {
	Port *tracer_port = erts_id2port(tracer, p, ERTS_PROC_LOCK_MAIN);
	if (!erts_is_valid_tracer_port(tracer)) {
	    if (tracer_port)
		erts_smp_port_unlock(tracer_port);
	    goto error;
	}
	tracer_port->trace_flags |= F_TRACER;
	erts_smp_port_unlock(tracer_port);
    } else
	goto error;

    switch (how) {
    case am_false: 
	on = 0; 
	break;
    case am_true: 
	on = 1;
	if (is_nil(tracer))
	    tracer = p->id;
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

	if (pid_spec == tracer)
	    goto error;

	tracee_port = erts_id2port(pid_spec, p, ERTS_PROC_LOCK_MAIN);
	if (!tracee_port)
	    goto error;
	
	if (tracer != NIL && port_already_traced(p, tracee_port, tracer)) {
	    erts_smp_port_unlock(tracee_port);
	    goto already_traced;
	}

	if (on)
	    tracee_port->trace_flags |= mask;
	else
	    tracee_port->trace_flags &= ~mask;
	
	if (!tracee_port->trace_flags)
	    tracee_port->tracer_proc = NIL;
	else if (tracer != NIL)
	    tracee_port->tracer_proc = tracer;

	erts_smp_port_unlock(tracee_port);

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

	if (pid_spec == tracer)
	    goto error;

	tracee_p = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
				 pid_spec, ERTS_PROC_LOCKS_ALL);
	if (!tracee_p)
	    goto error;

	if (tracer != NIL && already_traced(p, tracee_p, tracer)) {
	    erts_smp_proc_unlock(tracee_p,
				 (tracee_p == p
				  ? ERTS_PROC_LOCKS_ALL_MINOR
				  : ERTS_PROC_LOCKS_ALL));
	    goto already_traced;
	}

	if (on)
	    tracee_p->trace_flags |= mask;
	else
	    tracee_p->trace_flags &= ~mask;

	if ((tracee_p->trace_flags & TRACEE_FLAGS) == 0)
	    tracee_p->tracer_proc = NIL;
	else if (tracer != NIL)
	    tracee_p->tracer_proc = tracer;

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
#ifdef HAVE_CLOCK_GETTIME
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
#endif /* HAVE_CLOCK_GETTIME */
			erts_cpu_timestamp = !0;
		    }
		}
	    } else {
		goto error;
	    }
	}
#endif
	
	if (pid_spec == am_all || pid_spec == am_existing) {
	    int i;
	    int procs = 0;
	    int ports = 0;
	    int mods = 0;

	    if (mask & (ERTS_PROC_TRACEE_FLAGS & ~ERTS_TRACEE_MODIFIER_FLAGS))
		procs = 1;
	    if (mask & (ERTS_PORT_TRACEE_FLAGS & ~ERTS_TRACEE_MODIFIER_FLAGS))
		ports = 1;
	    if (mask & ERTS_TRACEE_MODIFIER_FLAGS)
		mods = 1;

#ifdef ERTS_SMP
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	    erts_smp_block_system(0);
	    system_blocked = 1;
#endif

	    ok = 1;
	    if (procs || mods) {
		/* tracing of processes */
		for (i = 0; i < erts_max_processes; i++) {
		    Process* tracee_p = process_tab[i];

		    if (! tracee_p) 
			continue;
		    if (tracer != NIL) {
			if (tracee_p->id == tracer)
			    continue;
			if (already_traced(NULL, tracee_p, tracer))
			    continue;
		    }
		    if (on) {
			tracee_p->trace_flags |= mask;
		    } else {
			tracee_p->trace_flags &= ~mask;
		    }
		    if(!(tracee_p->trace_flags & TRACEE_FLAGS)) {
			tracee_p->tracer_proc = NIL;
		    } else if (tracer != NIL) {
			tracee_p->tracer_proc = tracer;
		    }
		    matches++;
		}
	    }
	    if (ports || mods) {
		/* tracing of ports */
		for (i = 0; i < erts_max_ports; i++) {
		    Port *tracee_port = &erts_port[i];
		    if (tracee_port->status & ERTS_PORT_SFLGS_DEAD) continue;
		    if (tracer != NIL) {
			if (tracee_port->id == tracer) continue;
			if (port_already_traced(NULL, tracee_port, tracer)) continue;
		    }

		    if (on) tracee_port->trace_flags |= mask;
		    else tracee_port->trace_flags &= ~mask;
		
		    if (!(tracee_port->trace_flags & TRACEE_FLAGS)) {
			tracee_port->tracer_proc = NIL;
		    } else if (tracer != NIL) {
			tracee_port->tracer_proc = tracer;
		    }
		    /* matches are not counted for ports since it would violate compability */	
		    /* This could be a reason to modify this function or make a new one. */
		}
	    }
	}

	if (pid_spec == am_all || pid_spec == am_new) {
	    Uint def_flags = mask;
	    Eterm def_tracer = tracer;

	    ok = 1;
	    erts_change_default_tracing(on, &def_flags, &def_tracer);

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
	erts_smp_release_system();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
#endif

    BIF_RET(make_small(matches));

 already_traced:
    erts_send_error_to_logger_str(p->group_leader,
				  "** can only have one tracer per process\n");

 error:

#ifdef ERTS_SMP
    if (system_blocked) {
	erts_smp_release_system();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
#endif

    BIF_ERROR(p, BADARG);
}

/* Check that the process to be traced is not already traced
 * by a valid other tracer than the tracer to be.
 */
static int port_already_traced(Process *c_p, Port *tracee_port, Eterm tracer)
{
    /*
     * SMP build assumes that either system is blocked or:
     * * main lock is held on c_p
     * * all locks are held on port tracee_p
     */
    if ((tracee_port->trace_flags & TRACEE_FLAGS)
	&& tracee_port->tracer_proc != tracer) {
	/* This tracee is already being traced, and not by the 
	 * tracer to be */
	if (is_internal_port(tracee_port->tracer_proc)) {
	    if (!erts_is_valid_tracer_port(tracee_port->tracer_proc)) {
		/* Current trace port now invalid 
		 * - discard it and approve the new. */
		goto remove_tracer;
	    } else
		return 1;
	}
	else if(is_internal_pid(tracee_port->tracer_proc)) {
	    Process *tracer_p = erts_pid2proc(c_p, ERTS_PROC_LOCK_MAIN,
					      tracee_port->tracer_proc, 0);
	    if (!tracer_p) {
		/* Current trace process now invalid
		 * - discard it and approve the new. */
		goto remove_tracer;
	    } else
		return 1;
	}
	else {
	remove_tracer:
	    tracee_port->trace_flags &= ~TRACEE_FLAGS;
	    tracee_port->tracer_proc = NIL;
	}
    }
    return 0;
}

/* Check that the process to be traced is not already traced
 * by a valid other tracer than the tracer to be.
 */
static int already_traced(Process *c_p, Process *tracee_p, Eterm tracer)
{
    /*
     * SMP build assumes that either system is blocked or:
     * * main lock is held on c_p
     * * all locks multiple are held on tracee_p
     */
    if ((tracee_p->trace_flags & TRACEE_FLAGS)
	&& tracee_p->tracer_proc != tracer) {
	/* This tracee is already being traced, and not by the 
	 * tracer to be */
	if (is_internal_port(tracee_p->tracer_proc)) {
	    if (!erts_is_valid_tracer_port(tracee_p->tracer_proc)) {
		/* Current trace port now invalid 
		 * - discard it and approve the new. */
		goto remove_tracer;
	    } else
		return 1;
	}
	else if(is_internal_pid(tracee_p->tracer_proc)) {
	    Process *tracer_p = erts_pid2proc(c_p, ERTS_PROC_LOCK_MAIN,
					      tracee_p->tracer_proc, 0);
	    if (!tracer_p) {
		/* Current trace process now invalid
		 * - discard it and approve the new. */
		goto remove_tracer;
	    } else
		return 1;
	}
	else {
	remove_tracer:
	    tracee_p->trace_flags &= ~TRACEE_FLAGS;
	    tracee_p->tracer_proc = NIL;
	}
    }
    return 0;
}

/*
 * Return information about a process or an external function being traced.
 */

Eterm
trace_info_2(Process* p, Eterm What, Eterm Key)
{
    Eterm res;
    if (What == am_on_load) {
	res = trace_info_on_load(p, Key);
    } else if (is_atom(What) || is_pid(What)) {
	res = trace_info_pid(p, What, Key);
    } else if (is_tuple(What)) {
	res = trace_info_func(p, What, Key);
    } else {
	BIF_ERROR(p, BADARG);
    }
    BIF_RET(res);
}

static Eterm
trace_info_pid(Process* p, Eterm pid_spec, Eterm key)
{
    Eterm tracer;
    Uint trace_flags;
    Eterm* hp;

    if (pid_spec == am_new) {
	erts_get_default_tracing(&trace_flags, &tracer);
    } else if (is_internal_pid(pid_spec)
	       && internal_pid_index(pid_spec) < erts_max_processes) {
	Process *tracee;
	tracee = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
			       pid_spec, ERTS_PROC_LOCKS_ALL);

	if (!tracee) {
	    return am_undefined;
	} else {
	    tracer = tracee->tracer_proc;
	    trace_flags = tracee->trace_flags;
	}

	if (is_internal_pid(tracer)) {
	    if (!erts_pid2proc(p, ERTS_PROC_LOCK_MAIN, tracer, 0)) {
	    reset_tracer:
		tracee->trace_flags &= ~TRACEE_FLAGS;
		trace_flags = tracee->trace_flags;
		tracer = tracee->tracer_proc = NIL;
	    }
	}
	else if (is_internal_port(tracer)) {
	    if (!erts_is_valid_tracer_port(tracer))
		goto reset_tracer;
	}
#ifdef ERTS_SMP
	erts_smp_proc_unlock(tracee,
			     (tracee == p
			      ? ERTS_PROC_LOCKS_ALL_MINOR
			      : ERTS_PROC_LOCKS_ALL));
#endif
    } else if (is_external_pid(pid_spec)
	       && external_pid_dist_entry(pid_spec) == erts_this_dist_entry) {
	    return am_undefined;
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }

    if (key == am_flags) {
	int num_flags = 19;	/* MAXIMUM number of flags. */
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
	FLAG(F_TIMESTAMP, am_timestamp);
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
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, tracer); /* Local pid or port */
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
			      Binary **ms,              /* out */
			      Binary **ms_meta,         /* out */
			      Eterm   *tracer_pid_meta, /* out */
			      Sint    *count,           /* out */
			      Eterm   *call_time)       /* out */
{
    Export e;
    Export* ep;
    int i;
    BeamInstr *code;

    /* First look for an export entry */
    e.code[0] = mfa[0];
    e.code[1] = mfa[1];
    e.code[2] = mfa[2];
    if ((ep = export_get(&e)) != NULL) {
	if (ep->address == ep->code+3 &&
	    ep->code[3] != (BeamInstr) em_call_error_handler) {
	    if (ep->code[3] == (BeamInstr) em_call_traced_function) {
		*ms = ep->match_prog_set;
		return FUNC_TRACE_GLOBAL_TRACE;
	    }
	    if (ep->code[3] == (BeamInstr) em_apply_bif) {
		for (i = 0; i < BIF_SIZE; ++i) {
		    if (bif_export[i] == ep) {
			int r = 0;
			
			if (erts_bif_trace_flags[i] & BIF_TRACE_AS_GLOBAL) {
			    *ms = ep->match_prog_set;
			    return FUNC_TRACE_GLOBAL_TRACE;
			} else {
			    if (erts_bif_trace_flags[i] & BIF_TRACE_AS_LOCAL) {
				r |= FUNC_TRACE_LOCAL_TRACE;
				*ms = ep->match_prog_set;
			    }
			    if (erts_is_mtrace_break(ep->code+3, ms_meta,
						   tracer_pid_meta)) {
				r |= FUNC_TRACE_META_TRACE;
			    }
			    if (erts_is_time_break(p, ep->code+3, call_time)) {
				r |= FUNC_TRACE_TIME_TRACE;
			    }
			}
			return r ? r : FUNC_TRACE_UNTRACED;
		    }
		}
		erl_exit(1,"Impossible ghost bif encountered in trace_info.");
	    }
	}
    }
    
    /* OK, now look for breakpoint tracing */
    if ((code = erts_find_local_func(mfa)) != NULL) {
	int r = 
	    (erts_is_trace_break(code, ms, NULL)
	     ? FUNC_TRACE_LOCAL_TRACE : 0) 
	    | (erts_is_mtrace_break(code, ms_meta, tracer_pid_meta)
	       ? FUNC_TRACE_META_TRACE : 0)
	    | (erts_is_count_break(code, count)
	       ? FUNC_TRACE_COUNT_TRACE : 0)
	    | (erts_is_time_break(p, code, call_time)
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
    Sint count = 0;
    Eterm traced = am_false;
    Eterm match_spec = am_false;
    Eterm retval = am_false;
    Eterm meta = am_false;
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
	erts_smp_block_system(0);
    }
#endif

    r = function_is_traced(p, mfa, &ms, &ms_meta, &meta, &count, &call_time);

#ifdef ERTS_SMP
    if ( (key == am_call_time) || (key == am_all)) {
	erts_smp_release_system();
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
	retval = meta;
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
	    retval = count < 0 ? 
		erts_make_integer(-count-1, p) : 
		erts_make_integer(count, p);
	}
	break;
    case am_call_time:
	if (r & FUNC_TRACE_TIME_TRACE) {
	    retval = call_time;
	}
	break;
    case am_all: {
	Eterm match_spec_meta = am_false, c = am_false, t, ct = am_false;
	
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
	    c = count < 0 ? 
		erts_make_integer(-count-1, p) : 
		erts_make_integer(count, p);
	}
	if (r & FUNC_TRACE_TIME_TRACE) {
	    ct = call_time;
	}
	hp = HAlloc(p, (3+2)*6);
	retval = NIL;
	t = TUPLE2(hp, am_call_count, c); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_call_time, ct); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_meta_match_spec, match_spec_meta); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_meta, meta); hp += 3;
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
	    return TUPLE2(hp, key, erts_default_meta_tracer_pid);
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
	    Eterm match_spec = am_false, meta_match_spec = am_false, r = NIL, t;
	    
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
	    hp = HAlloc(p, (3+2)*5 + 3);
	    t = TUPLE2(hp, am_call_count, 
		       (erts_default_trace_pattern_flags.call_count
			? am_true : am_false)); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta_match_spec, meta_match_spec); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta, 
		       (erts_default_trace_pattern_flags.meta
			? erts_default_meta_tracer_pid : am_false)); hp += 3;
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

#undef FUNC_TRACE_NOEXIST
#undef FUNC_TRACE_UNTRACED
#undef FUNC_TRACE_GLOBAL_TRACE
#undef FUNC_TRACE_LOCAL_TRACE

int
erts_set_trace_pattern(Eterm* mfa, int specified, 
		       Binary* match_prog_set, Binary *meta_match_prog_set,
		       int on, struct trace_pattern_flags flags,
		       Eterm meta_tracer_pid)
{
    int matches = 0;
    int i;

    /*
     * First work on normal functions (not real BIFs).
     */
    
    for (i = 0; i < export_list_size(); i++) {
	Export* ep = export_list(i);
	int j;
	
	if (ExportIsBuiltIn(ep)) {
	    continue;
	}
	
	for (j = 0; j < specified && mfa[j] == ep->code[j]; j++) {
	    /* Empty loop body */
	}

	if (j == specified) {
	    if (on) {
		if (! flags.breakpoint)
		    matches += setup_func_trace(ep, match_prog_set);
		else
		    reset_func_trace(ep);
	    } else if (! flags.breakpoint) {
		matches += reset_func_trace(ep);
	    }
	}
    }

    /*
    ** OK, now for the bif's
    */
    for (i = 0; i < BIF_SIZE; ++i) {
	Export *ep = bif_export[i];
	int j;
	
	if (!ExportIsBuiltIn(ep)) {
	    continue;
	}
	
	if (bif_table[i].f == bif_table[i].traced) {
	    /* Trace wrapper same as regular function - untraceable */
	    continue;
	}
	
	for (j = 0; j < specified && mfa[j] == ep->code[j]; j++) {
	    /* Empty loop body */
	}
	if (j == specified) {
	    if (! flags.breakpoint) { /* Export entry call trace */
		if (on) {
		    if (erts_bif_trace_flags[i] & BIF_TRACE_AS_META) {
			ASSERT(ExportIsBuiltIn(bif_export[i]));
			erts_clear_mtrace_bif
			    ((BeamInstr *)bif_export[i]->code + 3);
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_META;
		    }
		    set_trace_bif(i, match_prog_set);
		    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_LOCAL;
		    erts_bif_trace_flags[i] |= BIF_TRACE_AS_GLOBAL;
		    setup_bif_trace(i);
		} else { /* off */
		    if (erts_bif_trace_flags[i] & BIF_TRACE_AS_GLOBAL) {
			clear_trace_bif(i);
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_GLOBAL;
		    }
		    if (! erts_bif_trace_flags[i]) {
			reset_bif_trace(i);
		    }
		}
		matches++;
	    } else { /* Breakpoint call trace */
		int m = 0;
		
		if (on) {
		    if (flags.local) {
			set_trace_bif(i, match_prog_set);
			erts_bif_trace_flags[i] |= BIF_TRACE_AS_LOCAL;
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_GLOBAL;
			m = 1;
		    }
		    if (flags.meta) {
			erts_set_mtrace_bif
			    ((BeamInstr *)bif_export[i]->code + 3,
			     meta_match_prog_set, meta_tracer_pid);
			erts_bif_trace_flags[i] |= BIF_TRACE_AS_META;
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_GLOBAL;
			m = 1;
		    }
		    if (flags.call_time) {
			erts_set_time_trace_bif(bif_export[i]->code + 3, on);
			/* I don't want to remove any other tracers */
			erts_bif_trace_flags[i] |= BIF_TRACE_AS_CALL_TIME;
			m = 1;
		    }
		    if (erts_bif_trace_flags[i]) {
			setup_bif_trace(i);
		    }
		} else { /* off */
		    if (flags.local) {
			if (erts_bif_trace_flags[i] & BIF_TRACE_AS_LOCAL) {
			    clear_trace_bif(i);
			    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_LOCAL;
			}
			m = 1;
		    }
		    if (flags.meta) {
			if (erts_bif_trace_flags[i] & BIF_TRACE_AS_META) {
			    erts_clear_mtrace_bif
				((BeamInstr *)bif_export[i]->code + 3);
			    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_META;
			}
			m = 1;
		    }
		    if (flags.call_time) {
			erts_clear_time_trace_bif(bif_export[i]->code + 3);
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_CALL_TIME;
			m = 1;
		    }
		    if (! erts_bif_trace_flags[i]) {
			reset_bif_trace(i);
		    }
		}
		matches += m;
	    }
	}
    }

    /*
    ** So, now for breakpoint tracing
    */
    if (on) {
	if (! flags.breakpoint) {
	    erts_clear_trace_break(mfa, specified);
	    erts_clear_mtrace_break(mfa, specified);
	    erts_clear_count_break(mfa, specified);
	    erts_clear_time_break(mfa, specified);
	} else {
	    int m = 0;
	    if (flags.local) {
		m = erts_set_trace_break(mfa, specified, match_prog_set,
					 am_true);
	    }
	    if (flags.meta) {
		m = erts_set_mtrace_break(mfa, specified, meta_match_prog_set,
					  meta_tracer_pid);
	    }
	    if (flags.call_count) {
		m = erts_set_count_break(mfa, specified, on);
	    }
	    if (flags.call_time) {
		m = erts_set_time_break(mfa, specified, on);
	    }
	    /* All assignments to 'm' above should give the same value,
	     * so just use the last */
	    matches += m;
	}
    } else {
	int m = 0;
	if (flags.local) {
	    m = erts_clear_trace_break(mfa, specified);
	}
	if (flags.meta) {
	    m = erts_clear_mtrace_break(mfa, specified);
	}
	if (flags.call_count) {
	    m = erts_clear_count_break(mfa, specified);
	}
	if (flags.call_time) {
	    m = erts_clear_time_break(mfa, specified);
	}
	/* All assignments to 'm' above should give the same value,
	 * so just use the last */
	matches += m;
    }

    return matches;
}

/*
 * Setup function tracing for the given exported function.
 *
 * Return Value: 1 if entry refers to a BIF or loaded function,
 * 0 if the entry refers to a function not loaded.
 */

static int
setup_func_trace(Export* ep, void* match_prog)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (BeamInstr) em_call_error_handler) {
	    return 0;
	} else if (ep->code[3] == (BeamInstr) em_call_traced_function) {
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = match_prog;
	    MatchSetRef(ep->match_prog_set);
	    return 1;
	} else {
	    /*
	     * We ignore apply/3 and anything else.
	     */
	    return 0;
	}
    }
    
    /*
     * Currently no trace support for native code.
     */
    if (erts_is_native_break(ep->address)) {
	return 0;
    }
    
    ep->code[3] = (BeamInstr) em_call_traced_function;
    ep->code[4] = (BeamInstr) ep->address;
    ep->address = ep->code+3;
    ep->match_prog_set = match_prog;
    MatchSetRef(ep->match_prog_set);
    return 1;
}

static void setup_bif_trace(int bif_index) {
    Export *ep = bif_export[bif_index];
    
    ASSERT(ExportIsBuiltIn(ep));
    ASSERT(ep->code[4]);
    ep->code[4] = (BeamInstr) bif_table[bif_index].traced;
}

static void set_trace_bif(int bif_index, void* match_prog) {
    Export *ep = bif_export[bif_index];
    
#ifdef HARDDEBUG
    erts_fprintf(stderr, "set_trace_bif: %T:%T/%bpu\n",
		 ep->code[0], ep->code[1], ep->code[2]);
#endif
    ASSERT(ExportIsBuiltIn(ep));
    MatchSetUnref(ep->match_prog_set);
    ep->match_prog_set = match_prog;
    MatchSetRef(ep->match_prog_set);
}

/*
 * Reset function tracing for the given exported function.
 *
 * Return Value: 1 if entry refers to a BIF or loaded function,
 * 0 if the entry refers to a function not loaded.
 */

static int
reset_func_trace(Export* ep)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (BeamInstr) em_call_error_handler) {
	    return 0;
	} else if (ep->code[3] == (BeamInstr) em_call_traced_function) {
	    ep->address = (Uint *) ep->code[4];
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = NULL;
	    return 1;
	} else {
	    /*
	     * We ignore apply/3 and anything else.
	     */
	    return 0;
	}
    }
    
    /*
     * Currently no trace support for native code.
     */
    if (erts_is_native_break(ep->address)) {
	return 0;
    }
    
    /*
     * Nothing to do, but the export entry matches.
     */

    return 1;
}

static void reset_bif_trace(int bif_index) {
    Export *ep = bif_export[bif_index];
    
    ASSERT(ExportIsBuiltIn(ep));
    ASSERT(ep->code[4]);
    ASSERT(! ep->match_prog_set);
    ASSERT(! erts_is_mtrace_break((BeamInstr *)ep->code+3, NULL, NULL));
    ep->code[4] = (BeamInstr) bif_table[bif_index].f;
}

static void clear_trace_bif(int bif_index) {
    Export *ep = bif_export[bif_index];
    
#ifdef HARDDEBUG
    erts_fprintf(stderr, "clear_trace_bif: %T:%T/%bpu\n",
		 ep->code[0], ep->code[1], ep->code[2]);
#endif
    ASSERT(ExportIsBuiltIn(ep));
    MatchSetUnref(ep->match_prog_set);
    ep->match_prog_set = NULL;
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
	current_flag = SEQ_TRACE_TIMESTAMP; 
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
	    old_value = SEQ_TRACE_TOKEN(p);
	}
        SEQ_TRACE_TOKEN(p) = NIL;
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

    if (SEQ_TRACE_TOKEN(p) == NIL) {
	hp = HAlloc(p, 6);
	SEQ_TRACE_TOKEN(p) = TUPLE5(hp, make_small(0),		/* Flags  */ 
				    make_small(0),		/* Label  */
				    make_small(0),		/* Serial */
				    p->id, /* Internal pid */	/* From   */
				    make_small(p->seq_trace_lastcnt));
    }
}

BIF_RETTYPE seq_trace_info_1(BIF_ALIST_1)
{
    Eterm item;
    Eterm res;
    Eterm* hp;
    Uint current_flag;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    item = BIF_ARG_1;

    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) {
	if ((item == am_send) || (item == am_receive) || 
	    (item == am_print) || (item == am_timestamp)) {
	    hp = HAlloc(BIF_P,3);
	    res = TUPLE2(hp, item, am_false);
	    BIF_RET(res);
	} else if ((item == am_label) || (item == am_serial)) {
	    BIF_RET(NIL);
	} else {
	    goto error;
	}
    }

    if (BIF_ARG_1 == am_send) {
	current_flag = SEQ_TRACE_SEND;
    } else if (BIF_ARG_1 == am_receive) {
	current_flag = SEQ_TRACE_RECEIVE; 
    } else if (BIF_ARG_1 == am_print) {
	current_flag = SEQ_TRACE_PRINT; 
    } else if (BIF_ARG_1 == am_timestamp) {
	current_flag = SEQ_TRACE_TIMESTAMP; 
    } else {
	current_flag = 0;
    }

    if (current_flag) {
	res = unsigned_val(SEQ_TRACE_TOKEN_FLAGS(BIF_P)) & current_flag ? 
	    am_true : am_false;
    } else if (item == am_label) {
	res = SEQ_TRACE_TOKEN_LABEL(BIF_P);
    } else if (item  == am_serial) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, SEQ_TRACE_TOKEN_LASTCNT(BIF_P), SEQ_TRACE_TOKEN_SERIAL(BIF_P));
    } else {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    hp = HAlloc(BIF_P, 3);
    res = TUPLE2(hp, item, res);
    BIF_RET(res);
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
    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) 
	BIF_RET(am_false);
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
    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) 
	BIF_RET(am_false);
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
	erts_smp_block_system(0);
    }
#endif
    erts_set_system_monitor(NIL);
    erts_system_monitor_long_gc = 0;
    erts_system_monitor_large_heap = 0;
    erts_system_monitor_flags.busy_port = 0;
    erts_system_monitor_flags.busy_dist_port = 0;
#ifdef ERTS_SMP
    if (c_p) {
	erts_smp_release_system();
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
	Eterm large_heap = NIL;

	if (erts_system_monitor_long_gc != 0) {
	    hsz += 2+3;
	    (void) erts_bld_uint(NULL, &hsz, erts_system_monitor_long_gc);
	}
	if (erts_system_monitor_large_heap != 0) {
	    hsz += 2+3;
	    (void) erts_bld_uint(NULL, &hsz, erts_system_monitor_large_heap);
	}

	hp = HAlloc(p, hsz);
	if (erts_system_monitor_long_gc != 0) {
	    long_gc = erts_bld_uint(&hp, NULL, erts_system_monitor_long_gc);
	}
	if (erts_system_monitor_large_heap != 0) {
	    large_heap = erts_bld_uint(&hp, NULL, erts_system_monitor_large_heap);
	}
	res = NIL;
	if (long_gc != NIL) {
	    Eterm t = TUPLE2(hp, am_long_gc, long_gc); hp += 3;
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


BIF_RETTYPE system_monitor_0(Process *p) {
    BIF_RET(system_monitor_get(p));
}

BIF_RETTYPE system_monitor_1(Process *p, Eterm spec) {
    if (spec == am_undefined) {
	BIF_RET(system_monitor_2(p, spec, NIL));
    } else if (is_tuple(spec)) {
	Eterm *tp = tuple_val(spec);
	if (tp[0] != make_arityval(2)) goto error;
	BIF_RET(system_monitor_2(p, tp[1], tp[2]));
    }
 error:
    BIF_ERROR(p, BADARG);
}

BIF_RETTYPE system_monitor_2(Process *p, Eterm monitor_pid, Eterm list) {
    Eterm prev;
    int system_blocked = 0;

    if (monitor_pid == am_undefined || list == NIL) {
	prev = system_monitor_get(p);
	erts_system_monitor_clear(p);
	BIF_RET(prev);
    }
    if (is_not_list(list)) goto error;
    else {
	Uint long_gc, large_heap;
	int busy_port, busy_dist_port;

	system_blocked = 1;
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);

	if (!erts_pid2proc(p, ERTS_PROC_LOCK_MAIN, monitor_pid, 0))
	    goto error;

	for (long_gc = 0, large_heap = 0, busy_port = 0, busy_dist_port = 0;
	     is_list(list);
	     list = CDR(list_val(list))) {
	    Eterm t = CAR(list_val(list));
	    if (is_tuple(t)) {
		Eterm *tp = tuple_val(t);
		if (arityval(tp[0]) != 2) goto error;
		if (tp[1] == am_long_gc) {
		    if (! term_to_Uint(tp[2], &long_gc)) goto error;
		    if (long_gc < 1) long_gc = 1;
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
	erts_system_monitor_large_heap = large_heap;
	erts_system_monitor_flags.busy_port = !!busy_port;
	erts_system_monitor_flags.busy_dist_port = !!busy_dist_port;

	erts_smp_release_system();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
	BIF_RET(prev);
    }

 error:

    if (system_blocked) {
	erts_smp_release_system();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }

    BIF_ERROR(p, BADARG);
}

/* Begin: Trace for System Profiling */

void erts_system_profile_clear(Process *c_p) {
#ifdef ERTS_SMP
    if (c_p) {
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);
    }
#endif
    erts_set_system_profile(NIL);
    erts_system_profile_flags.scheduler = 0;
    erts_system_profile_flags.runnable_procs = 0;
    erts_system_profile_flags.runnable_ports = 0;
    erts_system_profile_flags.exclusive = 0;
#ifdef ERTS_SMP
    if (c_p) {
	erts_smp_release_system();
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

BIF_RETTYPE system_profile_0(Process *p) {
    BIF_RET(system_profile_get(p));
}

BIF_RETTYPE system_profile_2(Process *p, Eterm profiler, Eterm list) {
    Eterm prev;
    int system_blocked = 0;
    Process *profiler_p = NULL;
    Port *profiler_port = NULL;

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
	erts_smp_block_system(0);

	/* Check if valid process, no locks are taken */

	if (is_internal_pid(profiler)) {
	    if (internal_pid_index(profiler) >= erts_max_processes) goto error;
	    profiler_p = process_tab[internal_pid_index(profiler)];
	    if (INVALID_PID(profiler_p, profiler)) goto error;
	} else if (is_internal_port(profiler)) {
	    if (internal_port_index(profiler) >= erts_max_ports) goto error;
	    profiler_port = &erts_port[internal_port_index(profiler)];
	    if (INVALID_TRACER_PORT(profiler_port, profiler)) goto error;
	} else {
	    goto error;
	}

	for (scheduler = 0, runnable_ports = 0, runnable_procs = 0, exclusive = 0;
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

	erts_smp_release_system();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
	
	BIF_RET(prev);
		
    }

    error:
	if (system_blocked) {
	    erts_smp_release_system();
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    	}

    BIF_ERROR(p, BADARG);
}
/* End: Trace for System Profiling */

BIF_RETTYPE
trace_delivered_1(BIF_ALIST_1)
{
    DECL_AM(trace_delivered);
#ifdef ERTS_SMP
    ErlHeapFragment *bp;
#else
    ErtsProcLocks locks = 0;
#endif
    Eterm *hp;
    Eterm msg, ref, msg_ref;
    Process *p;
    if (BIF_ARG_1 == am_all) {
	p = NULL;
    } else if (! (p = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
				    BIF_ARG_1, ERTS_PROC_LOCKS_ALL))) {
	if (is_not_internal_pid(BIF_ARG_1)
	    || internal_pid_index(BIF_ARG_1) >= erts_max_processes) {
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
    
    ref = erts_make_ref(BIF_P);

#ifdef ERTS_SMP
    bp = new_message_buffer(REF_THING_SIZE + 4);
    hp = &bp->mem[0];
    msg_ref = STORE_NC(&hp, &bp->off_heap.externals, ref);
#else
    hp = HAlloc(BIF_P, 4);
    msg_ref = ref;
#endif

    msg = TUPLE3(hp, AM_trace_delivered, BIF_ARG_1, msg_ref);

#ifdef ERTS_SMP
    erts_send_sys_msg_proc(BIF_P->id, BIF_P->id, msg, bp);
    if (p)
	erts_smp_proc_unlock(p,
			     (BIF_P == p
			      ? ERTS_PROC_LOCKS_ALL_MINOR
			      : ERTS_PROC_LOCKS_ALL));
#else
    erts_send_message(BIF_P, BIF_P, &locks, msg, ERTS_SND_FLG_NO_SEQ_TRACE);
#endif

    BIF_RET(ref);
}
