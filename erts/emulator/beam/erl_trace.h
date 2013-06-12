/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2013. All Rights Reserved.
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


#ifndef ERL_TRACE_H__
#define ERL_TRACE_H__

struct binary;

/* erl_bif_trace.c */
Eterm erl_seq_trace_info(Process *p, Eterm arg1);
void erts_system_monitor_clear(Process *c_p);
void erts_system_profile_clear(Process *c_p);

/* erl_trace.c */
void erts_init_trace(void);
void erts_trace_check_exiting(Eterm exiting);
Eterm erts_set_system_seq_tracer(Process *c_p,
				 ErtsProcLocks c_p_locks,
				 Eterm new);
Eterm erts_get_system_seq_tracer(void);
void erts_change_default_tracing(int setflags, Uint *flagsp, Eterm *tracerp);
void erts_get_default_tracing(Uint *flagsp, Eterm *tracerp);
void erts_set_system_monitor(Eterm monitor);
Eterm erts_get_system_monitor(void);

#ifdef ERTS_SMP
void erts_check_my_tracer_proc(Process *);
void erts_block_sys_msg_dispatcher(void);
void erts_release_sys_msg_dispatcher(void);
void erts_foreach_sys_msg_in_q(void (*func)(Eterm,
					    Eterm,
					    Eterm,
					    ErlHeapFragment *));
void erts_queue_error_logger_message(Eterm, Eterm, ErlHeapFragment *);
#endif

void erts_send_sys_msg_proc(Eterm, Eterm, Eterm, ErlHeapFragment *);
void trace_send(Process*, Eterm, Eterm);
void trace_receive(Process*, Eterm);
Uint32 erts_call_trace(Process *p, BeamInstr mfa[], struct binary *match_spec, Eterm* args,
		       int local, Eterm *tracer_pid);
void erts_trace_return(Process* p, BeamInstr* fi, Eterm retval, Eterm *tracer_pid);
void erts_trace_exception(Process* p, BeamInstr mfa[], Eterm class, Eterm value,
			  Eterm *tracer);
void erts_trace_return_to(Process *p, BeamInstr *pc);
void trace_sched(Process*, Eterm);
void trace_proc(Process*, Process*, Eterm, Eterm);
void trace_proc_spawn(Process*, Eterm pid, Eterm mod, Eterm func, Eterm args);
void save_calls(Process *p, Export *);
void trace_gc(Process *p, Eterm what);
/* port tracing */
void trace_virtual_sched(Process*, Eterm);
void trace_sched_ports(Port *pp, Eterm);
void trace_sched_ports_where(Port *pp, Eterm, Eterm);
void trace_port(Port *, Eterm what, Eterm data);
void trace_port_open(Port *, Eterm calling_pid, Eterm drv_name);

/* system_profile */
void erts_set_system_profile(Eterm profile);
Eterm erts_get_system_profile(void);
void profile_scheduler(Eterm scheduler_id, Eterm);
void profile_scheduler_q(Eterm scheduler_id, Eterm state, Eterm no_schedulers, Uint Ms, Uint s, Uint us);
void profile_runnable_proc(Process* p, Eterm status);
void profile_runnable_port(Port* p, Eterm status);
void erts_system_profile_setup_active_schedulers(void);

/* system_monitor */
void monitor_long_gc(Process *p, Uint time);
void monitor_long_schedule_proc(Process *p, BeamInstr *in_i, BeamInstr *out_i, Uint time);
void monitor_long_schedule_port(Port *pp, ErtsPortTaskType type, Uint time);
void monitor_large_heap(Process *p);
void monitor_generic(Process *p, Eterm type, Eterm spec);
Uint erts_trace_flag2bit(Eterm flag);
int erts_trace_flags(Eterm List, 
		 Uint *pMask, Eterm *pTracer, int *pCpuTimestamp);
Eterm erts_bif_trace(int bif_index, Process* p, Eterm* args, BeamInstr *I);

#ifdef ERTS_SMP
void erts_send_pending_trace_msgs(ErtsSchedulerData *esdp);
#define ERTS_SMP_CHK_PEND_TRACE_MSGS(ESDP)				\
do {									\
    if ((ESDP)->pending_trace_msgs)					\
	erts_send_pending_trace_msgs((ESDP));				\
} while (0)
#else
#define ERTS_SMP_CHK_PEND_TRACE_MSGS(ESDP)
#endif

#define seq_trace_output(token, msg, type, receiver, process) \
seq_trace_output_generic((token), (msg), (type), (receiver), (process), NIL)
#define seq_trace_output_exit(token, msg, type, receiver, exitfrom) \
seq_trace_output_generic((token), (msg), (type), (receiver), NULL, (exitfrom))
void seq_trace_output_generic(Eterm token, Eterm msg, Uint type, 
			      Eterm receiver, Process *process, Eterm exitfrom);

int seq_trace_update_send(Process *process);

Eterm erts_seq_trace(Process *process, 
		     Eterm atom_type, Eterm atom_true_or_false, 
		     int build_result);

struct trace_pattern_flags {
    unsigned int breakpoint : 1; /* Set if any other is set */
    unsigned int local      : 1; /* Local call trace breakpoint */
    unsigned int meta       : 1; /* Metadata trace breakpoint */
    unsigned int call_count : 1; /* Fast call count breakpoint */
    unsigned int call_time  : 1; /* Fast call time breakpoint */
};
extern const struct trace_pattern_flags erts_trace_pattern_flags_off;
extern int erts_call_time_breakpoint_tracing;
int erts_set_trace_pattern(Process*p, Eterm* mfa, int specified,
			   struct binary* match_prog_set,
			   struct binary *meta_match_prog_set,
			   int on, struct trace_pattern_flags,
			   Eterm meta_tracer_pid, int is_blocking);
void
erts_get_default_trace_pattern(int *trace_pattern_is_on,
			       struct binary **match_spec,
			       struct binary **meta_match_spec,
			       struct trace_pattern_flags *trace_pattern_flags,
			       Eterm *meta_tracer_pid);
int erts_is_default_trace_enabled(void);
void erts_bif_trace_init(void);
int erts_finish_breakpointing(void);

#endif /* ERL_TRACE_H__ */
