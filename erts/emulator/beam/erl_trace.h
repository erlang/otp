/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2024. All Rights Reserved.
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

#ifndef ERL_TRACE_H__FLAGS__
#define ERL_TRACE_H__FLAGS__
/*
 * NOTE! The bits used for these flags matter. The flag with
 * the least significant bit will take precedence!
 *
 * The "now timestamp" has highest precedence due to
 * compatibility reasons.
 */
#define ERTS_TRACE_FLG_NOW_TIMESTAMP			(1 << 0)
#define ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP	(1 << 1)
#define ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP		(1 << 2)

/*
 * The bits used effects trace flags (of processes and ports)
 * as well as sequential trace flags. If changed make sure
 * these aren't messed up...
 */
#define ERTS_TRACE_TS_TYPE_BITS 			3
#define ERTS_TRACE_TS_TYPE_MASK					\
    ((1 << ERTS_TRACE_TS_TYPE_BITS) - 1)

#define ERTS_TFLGS2TSTYPE(TFLGS)				\
    ((int) (((TFLGS) >> ERTS_TRACE_FLAGS_TS_TYPE_SHIFT) 	\
	    & ERTS_TRACE_TS_TYPE_MASK))
#define ERTS_SEQTFLGS2TSTYPE(SEQTFLGS)				\
    ((int) (((SEQTFLGS) >> ERTS_SEQ_TRACE_FLAGS_TS_TYPE_SHIFT)	\
	    & ERTS_TRACE_TS_TYPE_MASK))
#define ERTS_SEQTFLGS2TFLGS(SEQTFLGS)                                   \
    (ERTS_SEQTFLGS2TSTYPE(SEQTFLGS) << ERTS_TRACE_FLAGS_TS_TYPE_SHIFT)

#endif /* ERL_TRACE_H__FLAGS__ */

#if !defined(ERL_TRACE_H__) && !defined(ERTS_ONLY_INCLUDE_TRACE_FLAGS)
#define ERL_TRACE_H__

struct binary;

typedef struct
{
    int on;
    struct binary* match_spec;
} ErtsTracingEvent;

#ifndef ERTS_NUM_BP_IX
//#  include "beam_bp.h"
#  define ERTS_NUM_BP_IX 2   // ToDo FIXME UGLY
#endif
struct trace_pattern_flags {
    unsigned int breakpoint : 1; /* Set if any other is set */
    unsigned int local      : 1; /* Local call trace breakpoint */
    unsigned int meta       : 1; /* Metadata trace breakpoint */
    unsigned int call_count : 1; /* Fast call count breakpoint */
    unsigned int call_time  : 1; /* Fast call time breakpoint */
    unsigned int call_memory: 1; /* Fast memory tracing breakpoint */
};
extern const struct trace_pattern_flags erts_trace_pattern_flags_off;

enum erts_trace_session_state {
    ERTS_TRACE_SESSION_ALIVE,
    ERTS_TRACE_SESSION_CLEARING,
    ERTS_TRACE_SESSION_DEAD
};

typedef struct ErtsTraceSession {
    struct ErtsTraceSession* next;
    struct ErtsTraceSession* prev;

    erts_atomic_t state; /* enum erts_trace_session_state */

    ErtsTracer tracer;
    Eterm name_atom;
    Eterm weak_id;
    erts_atomic32_t trace_control_word;
    ErtsTracingEvent send_tracing[ERTS_NUM_BP_IX];
    ErtsTracingEvent receive_tracing[ERTS_NUM_BP_IX];
    int                         on_load_trace_pattern_is_on;
    Binary                     *on_load_match_spec;
    Binary                     *on_load_meta_match_spec;
    struct trace_pattern_flags  on_load_trace_pattern_flags;
    ErtsTracer                  on_load_meta_tracer;

    Uint32 on_spawn_proc_trace_flags;
    ErtsTracer on_spawn_proc_tracer;
    Uint32 on_open_port_trace_flags;
    ErtsTracer on_open_port_tracer;

#ifdef DEBUG
    erts_refc_t dbg_bp_refc;  /* Number of breakpoints */
    erts_refc_t dbg_p_refc;   /* Number of processes and ports */
#endif
}ErtsTraceSession;

#define ERTS_TRACE_SESSION_WEAK_REF_SZ 3
Eterm erts_make_trace_session_weak_ref(ErtsTraceSession*, Eterm **hpp);

extern ErtsTraceSession erts_trace_session_0;
extern erts_rwmtx_t erts_trace_session_list_lock;

void erts_ref_trace_session(ErtsTraceSession*);
void erts_deref_trace_session(ErtsTraceSession*);
ErtsTracerRef* get_tracer_ref(ErtsPTabElementCommon*, ErtsTraceSession*);
ErtsTracerRef* get_tracer_ref_from_weak_id(ErtsPTabElementCommon* t_p,
                                           Eterm weak_id);
ErtsTracerRef* new_tracer_ref(ErtsPTabElementCommon*, ErtsTraceSession*);
void clear_tracer_ref(ErtsPTabElementCommon*, ErtsTracerRef*);
void delete_tracer_ref(ErtsPTabElementCommon*, ErtsTracerRef*);
Uint delete_unalive_trace_refs(ErtsPTabElementCommon*);
void delete_all_trace_refs(ErtsPTabElementCommon*);
Uint32 erts_sum_all_trace_flags(ErtsPTabElementCommon* t_p);
void erts_change_proc_trace_session_flags(Process*, ErtsTraceSession*,
                                          Uint32 clear_flags, Uint32 set_flags);
#ifdef DEBUG
void erts_assert_tracer_refs(ErtsPTabElementCommon* t_p);
# define ERTS_ASSERT_TRACER_REFS(TP) erts_assert_tracer_refs(TP)
#else
# define ERTS_ASSERT_TRACER_REFS(TP)
#endif


/* erl_bif_trace.c */
Eterm erl_seq_trace_info(Process *p, Eterm arg1);
void erts_system_monitor_clear(Process *c_p);
void erts_system_profile_clear(Process *c_p);

/* erl_trace.c */
void erts_init_trace(void);
void erts_trace_check_exiting(Eterm exiting);
ErtsTracer erts_set_system_seq_tracer(Process *c_p,
                                      ErtsProcLocks c_p_locks,
                                      ErtsTracer new_);
ErtsTracer erts_get_system_seq_tracer(void);
void erts_change_default_proc_tracing(ErtsTraceSession* session,
                                      int setflags, Uint32 flags,
                                      const ErtsTracer tracerp);
void erts_get_on_spawn_tracing(ErtsTraceSession*,
                               Uint32 *flagsp, ErtsTracer *tracerp);
void erts_change_default_port_tracing(ErtsTraceSession* session,
                                      int setflags, Uint32 flags,
                                      const ErtsTracer tracerp);
void erts_get_on_open_port_tracing(ErtsTraceSession*,
                                   Uint32 *flagsp, ErtsTracer *tracerp);
void erts_set_system_monitor(Eterm monitor);
Eterm erts_get_system_monitor(void);
int erts_is_tracer_valid(Process* p);

void erts_check_my_tracer_proc(Process *);
void erts_block_sys_msg_dispatcher(void);
void erts_release_sys_msg_dispatcher(void);
void erts_debug_foreach_sys_msg_in_q(void (*func)(Eterm,
                                                  Eterm,
                                                  Eterm,
                                                  ErlHeapFragment *));
Eterm erts_set_system_logger(Eterm);
Eterm erts_get_system_logger(void);
void erts_queue_error_logger_message(Eterm, Eterm, ErlHeapFragment *);
void erts_send_sys_msg_proc(Eterm, Eterm, Eterm, ErlHeapFragment *);

void trace_send(Process*, Eterm, Eterm);
void trace_receive(Process*, Eterm, Eterm);
Uint32 erts_call_trace(Process *p, ErtsCodeInfo *info, struct binary *match_spec,
                       Eterm* args, int local, ErtsTracerRef*, ErtsTracer *tracer);
void erts_trace_return(Process* p, ErtsCodeMFA *mfa, Eterm retval,
                       ErtsTracer tracer, Eterm session_weak_id);
void erts_trace_exception(Process* p, ErtsCodeMFA *mfa, Eterm class_, Eterm value,
                          ErtsTracer tracer, Eterm session_weak_id);
void erts_trace_return_to(Process *p, ErtsCodePtr pc, Eterm session_weak_id);
void trace_sched(Process*, ErtsProcLocks, Eterm, Uint32 trace_flag);
void trace_sched_session(Process*, ErtsProcLocks, Eterm what, ErtsTracerRef*);
void trace_proc(Process*, ErtsProcLocks, Process*, Eterm, Eterm);
void trace_proc_spawn(Process*, Eterm what, Eterm pid, Eterm mod, Eterm func, Eterm args);
void save_calls(Process *p, Export *);
void trace_gc(Process *p, Eterm what, Uint size, Eterm msg);
/* port tracing */
void trace_virtual_sched(Process*, ErtsProcLocks, Eterm);
void trace_sched_ports(Port *pp, Eterm);
void trace_sched_ports_where(Port *pp, Eterm, Eterm);
void trace_port(Port *, Eterm what, Eterm data, Uint32 trace_flag);
void trace_port_open(Port *, Eterm calling_pid, Eterm drv_name);
void trace_port_receive(Port *, Eterm calling_pid, Eterm tag, ...);
void trace_port_send(Port *, Eterm to, Eterm msg, int exists);
void trace_port_send_binary(Port *, Eterm to, Eterm what, char *bin, Sint sz);

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
void monitor_long_schedule_proc(Process *p, const ErtsCodeMFA *in_i,
                                const ErtsCodeMFA *out_i, Uint time);
void monitor_long_schedule_port(Port *pp, ErtsPortTaskType type, Uint time);
void monitor_large_heap(Process *p);
void monitor_generic(Process *p, Eterm type, Eterm spec);
Uint erts_trace_flag2bit(Eterm flag);
int erts_trace_flags(ErtsTraceSession*, Eterm List,
                     Uint *pMask, ErtsTracer *pTracer, int *pCpuTimestamp);

void erts_send_pending_trace_msgs(ErtsSchedulerData *esdp);
#define ERTS_CHK_PEND_TRACE_MSGS(ESDP)				\
do {									\
    if ((ESDP)->pending_trace_msgs)					\
	erts_send_pending_trace_msgs((ESDP));				\
} while (0)

#define seq_trace_output(token, msg, type, receiver, process) \
seq_trace_output_generic((token), (msg), (type), (receiver), (process), NIL)
#define seq_trace_output_exit(token, msg, type, receiver, exitfrom) \
seq_trace_output_generic((token), (msg), (type), (receiver), NULL, (exitfrom))
void seq_trace_output_generic(Eterm token, Eterm msg, Uint type, 
			      Eterm receiver, Process *process, Eterm exitfrom);

/* Bump the sequence number if tracing is enabled; must be used before sending
 * send trace messages. */
int seq_trace_update_serial(Process *process);
void erts_seq_trace_update_node_token(Eterm token);

Eterm erts_seq_trace(Process *process, 
		     Eterm atom_type, Eterm atom_true_or_false, 
		     int build_result);

extern int erts_call_time_breakpoint_tracing;
int erts_set_trace_pattern(ErtsCodeMFA *mfa, int specified,
			   struct binary* match_prog_set,
			   struct binary *meta_match_prog_set,
			   int on, struct trace_pattern_flags,
			   ErtsTracer meta_tracer, int is_blocking);
int erts_is_on_load_trace_enabled(void);
void erts_bif_trace_init(void);
int erts_finish_breakpointing(void);

/* Nif tracer functions */
int erts_is_tracer_proc_enabled(Process *c_p, ErtsProcLocks c_p_locks,
                                ErtsPTabElementCommon *t_p);
int erts_is_tracer_ref_proc_enabled(Process *c_p, ErtsProcLocks c_p_locks,
                                    ErtsPTabElementCommon *t_p,
                                    ErtsTracerRef*);
int erts_is_tracer_ref_proc_enabled_send(Process* c_p, ErtsProcLocks c_p_locks,
                                         ErtsPTabElementCommon *t_p,
                                         ErtsTracerRef *ref);
int erts_is_tracer_enabled(const ErtsTracer tracer, ErtsPTabElementCommon *t_p);
Eterm erts_tracer_to_term(Process *p, ErtsTracer tracer);
Eterm erts_build_tracer_to_term(Eterm **hpp, ErlOffHeap *ohp, Uint *szp, ErtsTracer tracer);

ErtsTracer erts_term_to_tracer(Eterm prefix, Eterm term);
void erts_tracer_replace(ErtsPTabElementCommon *t_p,
                         ErtsTracerRef *ref,
                         const ErtsTracer new_tracer);
void erts_tracer_update_impl(ErtsTracer *tracer, const ErtsTracer new_tracer);
int erts_tracer_nif_clear(void);

#define erts_tracer_update(t,n) do { if (*(t) != (n)) erts_tracer_update_impl(t,n); } while(0)
#define ERTS_TRACER_CLEAR(t) erts_tracer_update(t, erts_tracer_nil)

static const ErtsTracer
ERTS_DECLARE_DUMMY(erts_tracer_true) = am_true;

static const ErtsTracer
ERTS_DECLARE_DUMMY(erts_tracer_nil) = NIL;

#define ERTS_TRACER_COMPARE(t1, t2)                     \
    (EQ((t1), (t2)))

#define ERTS_TRACER_IS_NIL(t1) ERTS_TRACER_COMPARE(t1, erts_tracer_nil)

#define IS_TRACER_VALID(tracer)                                         \
    (ERTS_TRACER_COMPARE(tracer,erts_tracer_true)                       \
     || ERTS_TRACER_IS_NIL(tracer)                                      \
     || (is_list(tracer) && is_atom(CAR(list_val(tracer)))))

#define ERTS_TRACER_FROM_ETERM(termp) \
    ((ErtsTracer*)(termp))

ERTS_GLB_INLINE Eterm erts_is_trace_session_alive(ErtsTraceSession*);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Eterm erts_is_trace_session_alive(ErtsTraceSession *s) {
    return ERTS_LIKELY(erts_atomic_read_nob(&s->state)
                       == ERTS_TRACE_SESSION_ALIVE);

}
#endif


#endif /* ERL_TRACE_H__ */
