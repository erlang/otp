/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2013. All Rights Reserved.
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
 * Support functions for tracing.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "big.h"
#include "bif.h"
#include "dist.h"
#include "beam_bp.h"
#include "error.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_thr_progress.h"

#if 0
#define DEBUG_PRINTOUTS
#else
#undef DEBUG_PRINTOUTS
#endif

extern BeamInstr beam_return_to_trace[1];   /* OpCode(i_return_to_trace) */
extern BeamInstr beam_return_trace[1];      /* OpCode(i_return_trace) */
extern BeamInstr beam_return_time_trace[1]; /* OpCode(i_return_time_trace) */

/* Pseudo export entries. Never filled in with data, only used to
   yield unique pointers of the correct type. */
Export exp_send, exp_receive, exp_timeout;

static Eterm system_seq_tracer;
static Uint default_trace_flags;
static Eterm default_tracer;

static Eterm system_monitor;
static Eterm system_profile;

#ifdef HAVE_ERTS_NOW_CPU
int erts_cpu_timestamp;
#endif

static erts_smp_mtx_t smq_mtx;
static erts_smp_rwmtx_t sys_trace_rwmtx;

enum ErtsSysMsgType {
    SYS_MSG_TYPE_UNDEFINED,
    SYS_MSG_TYPE_TRACE,
    SYS_MSG_TYPE_SEQTRACE,
    SYS_MSG_TYPE_SYSMON,
    SYS_MSG_TYPE_ERRLGR,
    SYS_MSG_TYPE_PROC_MSG,
    SYS_MSG_TYPE_SYSPROF
};

#ifdef ERTS_SMP
static void enqueue_sys_msg_unlocked(enum ErtsSysMsgType type,
				     Eterm from,
				     Eterm to,
				     Eterm msg,
				     ErlHeapFragment *bp);
static void enqueue_sys_msg(enum ErtsSysMsgType type,
			    Eterm from,
			    Eterm to,
			    Eterm msg,
			    ErlHeapFragment *bp);
static void init_sys_msg_dispatcher(void);
#endif

void erts_init_trace(void) {
    erts_smp_rwmtx_opt_t rwmtx_opts = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opts.type = ERTS_SMP_RWMTX_TYPE_EXTREMELY_FREQUENT_READ;
    rwmtx_opts.lived = ERTS_SMP_RWMTX_LONG_LIVED;

    erts_smp_rwmtx_init_opt(&sys_trace_rwmtx, &rwmtx_opts, "sys_tracers");

#ifdef HAVE_ERTS_NOW_CPU
    erts_cpu_timestamp = 0;
#endif
    erts_bif_trace_init();
    erts_system_monitor_clear(NULL);
    erts_system_profile_clear(NULL);
    default_trace_flags = F_INITIAL_TRACE_FLAGS;
    default_tracer = NIL;
    system_seq_tracer = am_false;
#ifdef ERTS_SMP
    init_sys_msg_dispatcher();
#endif
}

static Eterm system_seq_tracer;

#ifdef ERTS_SMP
#define ERTS_ALLOC_SYSMSG_HEAP(SZ, BPP, OHPP, UNUSED) \
  (*(BPP) = new_message_buffer((SZ)), \
   *(OHPP) = &(*(BPP))->off_heap, \
   (*(BPP))->mem)
#else
#define ERTS_ALLOC_SYSMSG_HEAP(SZ, BPP, OHPP, RPP) \
  erts_alloc_message_heap((SZ), (BPP), (OHPP), (RPP), 0)
#endif

#ifdef ERTS_SMP
#define ERTS_ENQ_TRACE_MSG(FPID, TPID, MSG, BP) \
do { \
    ERTS_LC_ASSERT(erts_smp_lc_mtx_is_locked(&smq_mtx)); \
    enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE, (FPID), (TPID), (MSG), (BP)); \
} while(0)
#else
#ifdef USE_VM_PROBES
#define ERTS_ENQ_TRACE_MSG(FPID, TPROC, MSG, BP) \
    erts_queue_message((TPROC), NULL, (BP), (MSG), NIL, NIL)
#else
#define ERTS_ENQ_TRACE_MSG(FPID, TPROC, MSG, BP) \
    erts_queue_message((TPROC), NULL, (BP), (MSG), NIL)
#endif
#endif

/*
 * NOTE that the ERTS_GET_TRACER_REF() returns from the function  (!!!)
 * using it, and resets the parameters used if the tracer is invalid, i.e.,
 * use it with extreme care!
 */
#ifdef ERTS_SMP
#define ERTS_NULL_TRACER_REF NIL
#define ERTS_TRACER_REF_TYPE Eterm
 /* In the smp case, we never find the tracer invalid here (the sys
    message dispatcher thread takes care of that). */
#define ERTS_GET_TRACER_REF(RES, TPID, TRACEE_FLGS) \
do { (RES) = (TPID); } while(0)
#else
#define ERTS_NULL_TRACER_REF NULL
#define ERTS_TRACER_REF_TYPE Process *
#define ERTS_GET_TRACER_REF(RES, TPID, TRACEE_FLGS) \
do { \
    (RES) = erts_proc_lookup((TPID)); \
    if (!(RES) || !(ERTS_TRACE_FLAGS((RES)) & F_TRACER)) { \
	(TPID) = NIL; \
	(TRACEE_FLGS) &= ~TRACEE_FLAGS; \
	return; \
    } \
} while (0)
#endif

static Uint active_sched;

void
erts_system_profile_setup_active_schedulers(void)
{
    ERTS_SMP_LC_ASSERT(erts_thr_progress_is_blocking());
    active_sched = erts_active_schedulers();
}

static void
exiting_reset(Eterm exiting)
{
    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    if (exiting == default_tracer) {
	default_tracer = NIL;
	default_trace_flags &= TRACEE_FLAGS;
#ifdef DEBUG
	default_trace_flags |= F_INITIAL_TRACE_FLAGS;
#endif
    }
    if (exiting == system_seq_tracer) {
#ifdef DEBUG_PRINTOUTS
	erts_fprintf(stderr, "seq tracer %T exited\n", exiting);
#endif
	system_seq_tracer = am_false;
    }
    if (exiting == system_monitor) {
#ifdef ERTS_SMP
	system_monitor = NIL;
	/* Let the trace message dispatcher clear flags, etc */
#else
	erts_system_monitor_clear(NULL);
#endif
    }
    if (exiting == system_profile) {
#ifdef ERTS_SMP
	system_profile = NIL;
	/* Let the trace message dispatcher clear flags, etc */
#else
	erts_system_profile_clear(NULL);
#endif
    }
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
}

void
erts_trace_check_exiting(Eterm exiting)
{
    int reset = 0;
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    if (exiting == default_tracer)
	reset = 1;
    else if (exiting == system_seq_tracer)
	reset = 1;
    else if (exiting == system_monitor)
	reset = 1;
    else if (exiting == system_profile)
	reset = 1;
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
    if (reset)
	exiting_reset(exiting);
}

static ERTS_INLINE int
is_valid_tracer(Eterm tracer)
{
    return erts_proc_lookup(tracer) || erts_is_valid_tracer_port(tracer);
}

Eterm
erts_set_system_seq_tracer(Process *c_p, ErtsProcLocks c_p_locks, Eterm new)
{
    Eterm old;

    if (new != am_false && !is_valid_tracer(new))
	return THE_NON_VALUE;

    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    old = system_seq_tracer;
    system_seq_tracer = new;

#ifdef DEBUG_PRINTOUTS
    erts_fprintf(stderr, "set seq tracer new=%T old=%T\n", new, old);
#endif
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
    return old;
}

Eterm
erts_get_system_seq_tracer(void)
{
    Eterm st;
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    st = system_seq_tracer;
#ifdef DEBUG_PRINTOUTS
    erts_fprintf(stderr, "get seq tracer %T\n", st);
#endif
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
    return st;
}

static ERTS_INLINE void
get_default_tracing(Uint *flagsp, Eterm *tracerp)
{
    if (!(default_trace_flags & TRACEE_FLAGS))
	default_tracer = NIL;

    if (is_nil(default_tracer)) {
	default_trace_flags &= ~TRACEE_FLAGS;
    } else if (is_internal_pid(default_tracer)) {
	if (!erts_proc_lookup(default_tracer)) {
	reset_tracer:
	    default_trace_flags &= ~TRACEE_FLAGS;
	    default_tracer = NIL;
	}
    } else {
	ASSERT(is_internal_port(default_tracer));
	if (!erts_is_valid_tracer_port(default_tracer))
	    goto reset_tracer;
    }

    if (flagsp)
	*flagsp = default_trace_flags;
    if (tracerp)
	*tracerp = default_tracer;
}

void
erts_change_default_tracing(int setflags, Uint *flagsp, Eterm *tracerp)
{
    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    if (flagsp) {
	if (setflags)
	    default_trace_flags |= *flagsp;
	else
	    default_trace_flags &= ~(*flagsp);
    }
    if (tracerp)
	default_tracer = *tracerp;
    get_default_tracing(flagsp, tracerp);
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
}

void
erts_get_default_tracing(Uint *flagsp, Eterm *tracerp)
{
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    get_default_tracing(flagsp, tracerp);
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
}

void
erts_set_system_monitor(Eterm monitor)
{
    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    system_monitor = monitor;
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
}

Eterm
erts_get_system_monitor(void)
{
    Eterm monitor;
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    monitor = system_monitor;
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
    return monitor;
}

/* Performance monitoring */
void erts_set_system_profile(Eterm profile) {
    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    system_profile = profile;
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
}

Eterm
erts_get_system_profile(void) {
    Eterm profile;
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    profile = system_profile;
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
    return profile;
}


#ifdef HAVE_ERTS_NOW_CPU
#  define GET_NOW(m, s, u) \
do { \
    if (erts_cpu_timestamp) \
	erts_get_now_cpu(m, s, u); \
    else \
	get_now(m, s, u); \
} while (0)
#else
#  define GET_NOW(m, s, u) do {get_now(m, s, u);} while (0)
#endif



static Eterm* patch_ts(Eterm tuple4, Eterm* hp);

#ifdef ERTS_SMP
static void
do_send_to_port(Eterm to,
		Port* unused_port,
		Eterm from,
		enum ErtsSysMsgType type,
		Eterm message)
{
    Uint sz = size_object(message);
    ErlHeapFragment *bp = new_message_buffer(sz);
    Uint *hp = bp->mem;
    Eterm msg = copy_struct(message, sz, &hp, &bp->off_heap);

    enqueue_sys_msg_unlocked(type, from, to, msg, bp);
}

#define WRITE_SYS_MSG_TO_PORT write_sys_msg_to_port
#else
#define WRITE_SYS_MSG_TO_PORT do_send_to_port
#endif

static void
WRITE_SYS_MSG_TO_PORT(Eterm unused_to,
		      Port* trace_port,
		      Eterm unused_from,
		      enum ErtsSysMsgType unused_type,
		      Eterm message) {
    byte *buffer;
    byte *ptr;
    unsigned size;

    size = erts_encode_ext_size(message);
    buffer = (byte *) erts_alloc(ERTS_ALC_T_TMP, size);

    ptr = buffer;

    erts_encode_ext(message, &ptr);
    if (!(ptr <= buffer+size)) {
	erl_exit(1, "Internal error in do_send_to_port: %d\n", ptr-buffer);
    }

#ifndef ERTS_SMP
    if (!INVALID_TRACER_PORT(trace_port, trace_port->common.id))
#endif
	erts_raw_port_command(trace_port, buffer, ptr-buffer);

    erts_free(ERTS_ALC_T_TMP, (void *) buffer);
}


#ifndef ERTS_SMP
/* Send        {trace_ts, Pid, out, 0, Timestamp}
 * followed by {trace_ts, Pid, in, 0, NewTimestamp}
 *
 * 'NewTimestamp' is fetched from GET_NOW() through patch_ts().
 */
static void 
do_send_schedfix_to_port(Port *trace_port, Eterm pid, Eterm timestamp) {
#define LOCAL_HEAP_SIZE (4+5+5)
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    Eterm message;
    Eterm *hp;
    Eterm mfarity;

    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);
    ASSERT(is_pid(pid));
    ASSERT(is_tuple(timestamp));
    ASSERT(*tuple_val(timestamp) == make_arityval(3));
    
    hp = local_heap;
    mfarity = make_small(0);
    message = TUPLE5(hp, am_trace_ts, pid, am_out, mfarity, timestamp);
    /* Note, hp is deliberately NOT incremented since it will be reused */

    do_send_to_port(trace_port->common.id,
		    trace_port,
		    pid,
		    SYS_MSG_TYPE_UNDEFINED,
		    message);

    message = TUPLE4(hp, am_trace_ts, pid, am_in, mfarity);
    hp += 5;
    hp = patch_ts(message, hp);

    do_send_to_port(trace_port->common.id,
		    trace_port,
		    pid,
		    SYS_MSG_TYPE_UNDEFINED,
		    message);
    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
}
#endif

/* If (c_p != NULL), a fake schedule out/in message pair will be sent,
 * if the driver so requests. 
 * It is assumed that 'message' is not an 'out' message.
 *
 * 'c_p' is the currently executing process, "tracee" is the traced process
 * which 'message' concerns => if (*tracee_flags & F_TIMESTAMP), 
 * 'message' must contain a timestamp.
 */
static void
send_to_port(Process *c_p, Eterm message, 
	     Eterm *tracer_pid, Uint *tracee_flags) {
    Port* trace_port;
#ifndef ERTS_SMP
#define LOCAL_HEAP_SIZE (4)
    Eterm ts, *hp;
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
#endif

    ASSERT(is_internal_port(*tracer_pid));
#ifdef ERTS_SMP
    if (is_not_internal_port(*tracer_pid))
	return;

    trace_port = NULL;
#else

    trace_port = erts_id2port_sflgs(*tracer_pid,
				    NULL,
				    0,
				    ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP);

    if (!trace_port) {
	*tracee_flags &= ~TRACEE_FLAGS;
	*tracer_pid = NIL;
	return;
    }

    /*
     * Make a fake schedule only if the current process is traced
     * with 'running' and 'timestamp'.
     */

    if ( c_p == NULL || 
	 (! IS_TRACED_FL(c_p, F_TRACE_SCHED | F_TIMESTAMP))) { 
#endif
	do_send_to_port(*tracer_pid,
			trace_port,
			c_p ? c_p->common.id : NIL,
			SYS_MSG_TYPE_TRACE,
			message);
#ifndef ERTS_SMP
	erts_port_release(trace_port);
	return;
    }

    /*
     * Note that the process being traced for some type of trace messages
     * (e.g. getting_linked) need not be the current process. That other
     * process might not have timestamps enabled.
     */
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

    if (*tracee_flags & F_TIMESTAMP) {
	ASSERT(is_tuple(message));
	hp = tuple_val(message);
	ts = hp[arityval(hp[0])];
    } else {
	/* A fake schedule might be needed,
	 * but this message does not contain a timestamp.
	 * Create a dummy trace message with timestamp to be
	 * passed to do_send_schedfix_to_port().
	 */
	Uint ms,s,us;
	GET_NOW(&ms, &s, &us);
	hp = local_heap;
	ts = TUPLE3(hp, make_small(ms), make_small(s), make_small(us));
	hp += 4;
    }

    trace_port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
    do_send_to_port(*tracer_pid,
		    trace_port,
		    c_p ? c_p->common.id : NIL,
		    SYS_MSG_TYPE_TRACE,
		    message);

    if (trace_port->control_flags & PORT_CONTROL_FLAG_HEAVY) {
	/* The driver has just informed us that the last write took a 
	 * non-neglectible amount of time.
	 *
	 * We need to fake some trace messages to compensate for the time the
	 * current process had to sacrifice for the writing of the previous
	 * trace message. We pretend that the process got scheduled out
	 * just after writning the real trace message, and now gets scheduled
	 * in again.
	 */
	do_send_schedfix_to_port(trace_port, c_p->common.id, ts);
    }

    erts_port_release(trace_port);

    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
#endif
}

#ifndef ERTS_SMP
/* Profile send
 * Checks if profiler is port or process
 * Eterm msg is local, need copying.
 */

static void 
profile_send(Eterm from, Eterm message) {
    Uint sz = 0;
    ErlHeapFragment *bp = NULL;
    Uint *hp = NULL;
    Eterm msg = NIL;
    Process *profile_p = NULL;
    ErlOffHeap *off_heap = NULL;

    Eterm profiler = erts_get_system_profile();

    /* do not profile profiler pid */
    if (from == profiler) return;

    if (is_internal_port(profiler)) {
    	Port *profiler_port = NULL;

	/* not smp */

	profiler_port = erts_id2port_sflgs(profiler,
					   NULL,
					   0,
					   ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP);
	if (profiler_port) {
	    do_send_to_port(profiler,
			    profiler_port,
			    NIL, /* or current process->common.id */
			    SYS_MSG_TYPE_SYSPROF,
			    message);
	    erts_port_release(profiler_port);
	}
    	
    } else {
	ASSERT(is_internal_pid(profiler));
        
	profile_p = erts_proc_lookup(profiler);

	if (!profile_p)
	    return;

	sz = size_object(message);
	hp = erts_alloc_message_heap(sz, &bp, &off_heap, profile_p, 0);
	msg = copy_struct(message, sz, &hp, &bp->off_heap);
	
    	erts_queue_message(profile_p, NULL, bp, msg, NIL
#ifdef USE_VM_PROBES
			   , NIL
#endif
			   );
    }
}

#endif


/* A fake schedule out/in message pair will be sent,
 * if the driver so requests.
 * If (timestamp == NIL), one is fetched from GET_NOW().
 *
 * 'c_p' is the currently executing process, may be NULL.
 */
static void
seq_trace_send_to_port(Process *c_p,
		       Eterm seq_tracer,
		       Eterm message,
		       Eterm timestamp)
{
    Port* trace_port;
#ifndef ERTS_SMP
    Eterm ts, *hp;
#define LOCAL_HEAP_SIZE (4)
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#endif

    ASSERT(is_internal_port(seq_tracer));
#ifdef ERTS_SMP
    if (is_not_internal_port(seq_tracer))
	return;

    trace_port = NULL;
#else
    trace_port = erts_id2port_sflgs(seq_tracer,
				    NULL,
				    0,
				    ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP);
    if (!trace_port) {
	system_seq_tracer = am_false;
#ifndef ERTS_SMP
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#endif
	return;
    }

    if (c_p == NULL
	|| (! IS_TRACED_FL(c_p, F_TRACE_SCHED | F_TIMESTAMP))) {
#endif
	do_send_to_port(seq_tracer,
			trace_port,
			c_p ? c_p->common.id : NIL,
			SYS_MSG_TYPE_SEQTRACE,
			message);

#ifndef ERTS_SMP
	erts_port_release(trace_port);
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
	return;
    }
    /* Make a fake schedule only if the current process is traced
     * with 'running' and 'timestamp'.
     */

    if (timestamp != NIL) {
	ts = timestamp;
    } else {
	/* A fake schedule might be needed,
	 * but this message does not contain a timestamp.
	 * Create a dummy trace message with timestamp to be
	 * passed to do_send_schedfix_to_port().
	 */
	Uint ms,s,us;
	GET_NOW(&ms, &s, &us);
	hp = local_heap;
	ts = TUPLE3(hp, make_small(ms), make_small(s), make_small(us));
	hp += 4;
    }

    trace_port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
    do_send_to_port(seq_tracer,
		    trace_port,
		    c_p ? c_p->common.id : NIL,
		    SYS_MSG_TYPE_SEQTRACE,
		    message);

    if (trace_port->control_flags & PORT_CONTROL_FLAG_HEAVY) {
	/* The driver has just informed us that the last write took a 
	 * non-neglectible amount of time.
	 *
	 * We need to fake some trace messages to compensate for the time the
	 * current process had to sacrifice for the writing of the previous
	 * trace message. We pretend that the process got scheduled out
	 * just after writing the real trace message, and now gets scheduled
	 * in again.
	 */
	do_send_schedfix_to_port(trace_port, c_p->common.id, ts);
    }

    erts_port_release(trace_port);

    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
#endif
}

#define TS_HEAP_WORDS 5
#define TS_SIZE(p) ((ERTS_TRACE_FLAGS((p)) & F_TIMESTAMP) \
		    ? TS_HEAP_WORDS \
		    : 0)

/*
 * Patch a timestamp into a tuple.  The tuple must be the last thing
 * built on the heap.
 *
 * Returns the new hp pointer.
*/
static Eterm*
patch_ts(Eterm tuple, Eterm* hp)
{
    Uint ms, s, us;
    Eterm* ptr = tuple_val(tuple);
    int arity = arityval(*ptr);

    ASSERT((ptr+arity+1) == hp);
    ptr[0] = make_arityval(arity+1);
    ptr[1] = am_trace_ts;
    GET_NOW(&ms, &s, &us);
    *hp = TUPLE3(hp+1, make_small(ms), make_small(s), make_small(us));
    return hp+5;
}

static ERTS_INLINE void
send_to_tracer(Process *tracee,
	       ERTS_TRACER_REF_TYPE tracer_ref,
	       Eterm msg,
	       Eterm **hpp,
	       ErlHeapFragment *bp,
	       int no_fake_sched)
{
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(tracee));

    erts_smp_mtx_lock(&smq_mtx);

    if (ERTS_TRACE_FLAGS(tracee) & F_TIMESTAMP)
	*hpp = patch_ts(msg, *hpp);

    if (is_internal_pid(ERTS_TRACER_PROC(tracee)))
	ERTS_ENQ_TRACE_MSG(tracee->common.id, tracer_ref, msg, bp);
    else {
	ASSERT(is_internal_port(ERTS_TRACER_PROC(tracee)));
	send_to_port(no_fake_sched ? NULL : tracee,
		     msg,
		     &ERTS_TRACER_PROC(tracee),
		     &ERTS_TRACE_FLAGS(tracee));
    }

    erts_smp_mtx_unlock(&smq_mtx);

}

static void
trace_sched_aux(Process *p, Eterm what, int never_fake_sched)
{
#define LOCAL_HEAP_SIZE (5+4+1+TS_HEAP_WORDS)
    DeclareTmpHeap(local_heap,LOCAL_HEAP_SIZE,p);
    Eterm tmp, mess, *hp;
    ErlHeapFragment *bp = NULL;
    ErlOffHeap *off_heap;
    ERTS_TRACER_REF_TYPE tracer_ref = ERTS_NULL_TRACER_REF;
    int sched_no, curr_func, to_port, no_fake_sched;

    if (is_nil(ERTS_TRACER_PROC(p)))
	return;

    no_fake_sched = never_fake_sched;

    switch (what) {
    case am_out:
    case am_out_exiting:
    case am_out_exited:
	no_fake_sched = 1;
	break;
    case am_in:
    case am_in_exiting:
	break;
    default:
	ASSERT(0);
	break;
    }

    sched_no = IS_TRACED_FL(p, F_TRACE_SCHED_NO);
    to_port = is_internal_port(ERTS_TRACER_PROC(p));

    if (!to_port) {
	ASSERT(is_internal_pid(ERTS_TRACER_PROC(p)));

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(p),
			    ERTS_TRACE_FLAGS(p));
    }

    if (ERTS_PROC_IS_EXITING(p))
	curr_func = 0;
    else {
	if (!p->current)
	    p->current = find_function_from_pc(p->i);
	curr_func = p->current != NULL;
    }

    UseTmpHeap(LOCAL_HEAP_SIZE,p);

    if (to_port)
	hp = local_heap;
    else {
	Uint size = 5;
	if (curr_func)
	    size += 4;
	if (sched_no)
	    size += 1;
	size += TS_SIZE(p);
	hp = ERTS_ALLOC_SYSMSG_HEAP(size, &bp, &off_heap, tracer_ref);
    }

    if (!curr_func) {
	tmp = make_small(0);
    } else {
	tmp = TUPLE3(hp,p->current[0],p->current[1],make_small(p->current[2]));
	hp += 4;
    }

    if (!sched_no) {
	mess = TUPLE4(hp, am_trace, p->common.id, what, tmp);
	hp += 5;
    }
    else {
#ifdef ERTS_SMP
	Eterm sched_id = make_small(p->scheduler_data->no);
#else
	Eterm sched_id = make_small(1);
#endif
	mess = TUPLE5(hp, am_trace, p->common.id, what, sched_id, tmp);
	hp += 6;
    }

    send_to_tracer(p, tracer_ref, mess, &hp, bp, no_fake_sched);
    UnUseTmpHeap(LOCAL_HEAP_SIZE,p);
#undef LOCAL_HEAP_SIZE
}

/* Send {trace_ts, Pid, What, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, What, {Mod, Func, Arity}}
 *
 * where 'What' is supposed to be 'in', 'out', 'in_exiting',
 * 'out_exiting', or 'out_exited'.
 */
void
trace_sched(Process *p, Eterm what)
{
    trace_sched_aux(p, what, 0);
}

/* Send {trace_ts, Pid, Send, Msg, DestPid, Timestamp}
 * or   {trace, Pid, Send, Msg, DestPid}
 *
 * where 'Send' is 'send' or 'send_to_non_existing_process'.
 */
void
trace_send(Process *p, Eterm to, Eterm msg)
{
    Eterm operation;
    unsigned sz_msg;
    unsigned sz_to;
    Eterm* hp;
    Eterm mess;
    
    if (!ARE_TRACE_FLAGS_ON(p, F_TRACE_SEND)) {
	return;
    }

    operation = am_send;
    if (is_internal_pid(to)) {
	if (!erts_proc_lookup(to))
	    goto send_to_non_existing_process;
    }
    else if(is_external_pid(to)
	    && external_pid_dist_entry(to) == erts_this_dist_entry) {
	char *s;
    send_to_non_existing_process:
	s = "send_to_non_existing_process";
	operation = am_atom_put(s, sys_strlen(s));
    }

    if (is_internal_port(ERTS_TRACER_PROC(p))) {
#define LOCAL_HEAP_SIZE (11)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;
	mess = TUPLE5(hp, am_trace, p->common.id, operation, msg, to);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, &ERTS_TRACER_PROC(p), &ERTS_TRACE_FLAGS(p));
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	Uint need;
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;

	ASSERT(is_internal_pid(ERTS_TRACER_PROC(p)));

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(p),
			    ERTS_TRACE_FLAGS(p));

	sz_msg = size_object(msg);
	sz_to  = size_object(to);
	need = sz_msg + sz_to + 6 + TS_SIZE(p);
	
	hp = ERTS_ALLOC_SYSMSG_HEAP(need, &bp, &off_heap, tracer_ref);

	to = copy_struct(to,
			 sz_to,
			 &hp,
			 off_heap);
	msg = copy_struct(msg,
			  sz_msg,
			  &hp,
			  off_heap);
	mess = TUPLE5(hp, am_trace, p->common.id, operation, msg, to);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    patch_ts(mess, hp);
	}

	ERTS_ENQ_TRACE_MSG(p->common.id, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}

/* Send {trace_ts, Pid, receive, Msg, Timestamp}
 * or   {trace, Pid, receive, Msg}
 */
void
trace_receive(Process *rp, Eterm msg)
{
    Eterm mess;
    size_t sz_msg;
    Eterm* hp;

    if (is_internal_port(ERTS_TRACER_PROC(rp))) {
#define LOCAL_HEAP_SIZE (10)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;
	mess = TUPLE4(hp, am_trace, rp->common.id, am_receive, msg);
	hp += 5;
	erts_smp_mtx_lock(&smq_mtx);
	if (ERTS_TRACE_FLAGS(rp) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(rp, mess, &ERTS_TRACER_PROC(rp), &ERTS_TRACE_FLAGS(rp));
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	Uint hsz;
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;

	ASSERT(is_internal_pid(ERTS_TRACER_PROC(rp)));

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(rp),
			    ERTS_TRACE_FLAGS(rp));

	sz_msg = size_object(msg);

	hsz = sz_msg + 5 + TS_SIZE(rp);

	hp = ERTS_ALLOC_SYSMSG_HEAP(hsz, &bp, &off_heap, tracer_ref);

	msg = copy_struct(msg, sz_msg, &hp, off_heap);
	mess = TUPLE4(hp, am_trace, rp->common.id, am_receive, msg);
	hp += 5;

	erts_smp_mtx_lock(&smq_mtx);

	if (ERTS_TRACE_FLAGS(rp) & F_TIMESTAMP) {
	    patch_ts(mess, hp);
	}

	ERTS_ENQ_TRACE_MSG(rp->common.id, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}

int
seq_trace_update_send(Process *p)
{
    Eterm seq_tracer = erts_get_system_seq_tracer();
    ASSERT((is_tuple(SEQ_TRACE_TOKEN(p)) || is_nil(SEQ_TRACE_TOKEN(p))));
    if ( (p->common.id == seq_tracer) || (SEQ_TRACE_TOKEN(p) == NIL)
#ifdef USE_VM_PROBES
	 || (SEQ_TRACE_TOKEN(p) == am_have_dt_utag)
#endif
	 ) {
	return 0;
    }
    SEQ_TRACE_TOKEN_SENDER(p) = p->common.id;
    SEQ_TRACE_TOKEN_SERIAL(p) = 
	make_small(++(p -> seq_trace_clock));
    SEQ_TRACE_TOKEN_LASTCNT(p) = 
	make_small(p -> seq_trace_lastcnt);
    return 1;
}


/* Send a sequential trace message to the sequential tracer.
 * p is the caller (which contains the trace token), 
 * msg is the original message, type is trace type (SEQ_TRACE_SEND etc),
 * and receiver is the receiver of the message.
 *
 * The message to be received by the sequential tracer is:
 * 
 *    TraceMsg = 
 *   {seq_trace, Label, {Type, {Lastcnt, Serial}, Sender, Receiver, Msg} [,Timestamp] }
 *
 */
void 
seq_trace_output_generic(Eterm token, Eterm msg, Uint type,
			 Eterm receiver, Process *process, Eterm exitfrom)
{
    Eterm mess;
    ErlHeapFragment* bp;
    Eterm* hp;
    Eterm label;
    Eterm lastcnt_serial;
    Eterm type_atom;
    int sz_exit;
    Eterm seq_tracer;

    seq_tracer = erts_get_system_seq_tracer();

    ASSERT(is_tuple(token) || is_nil(token));
    if (SEQ_TRACE_T_SENDER(token) == seq_tracer || token == NIL ||
	(process && ERTS_TRACE_FLAGS(process) & F_SENSITIVE)) {
	return;
    }

    switch (type) {
    case SEQ_TRACE_SEND:    type_atom = am_send; break;
    case SEQ_TRACE_PRINT:   type_atom = am_print; break;
    case SEQ_TRACE_RECEIVE: type_atom = am_receive; break;
    default:
	erl_exit(1, "invalid type in seq_trace_output_generic: %d:\n", type);
	return;			/* To avoid warning */
    }

    if ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & type) == 0) {
	/* No flags set, nothing to do */
	return;
    }

    if (seq_tracer == am_false) {
	return;			/* no need to send anything */
    }

    if (is_internal_port(seq_tracer)) {
#define LOCAL_HEAP_SIZE (64)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;
	label = SEQ_TRACE_T_LABEL(token);
	lastcnt_serial = TUPLE2(hp, SEQ_TRACE_T_LASTCNT(token),
				SEQ_TRACE_T_SERIAL(token));
	hp += 3;
	if (exitfrom != NIL) {
	    msg = TUPLE3(hp, am_EXIT, exitfrom, msg);
	    hp += 4;
	}
	mess = TUPLE5(hp, type_atom, lastcnt_serial, SEQ_TRACE_T_SENDER(token),
		      receiver, msg);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & SEQ_TRACE_TIMESTAMP) == 0) {
	    mess = TUPLE3(hp, am_seq_trace, label, mess);
	    seq_trace_send_to_port(NULL, seq_tracer, mess, NIL);
	} else {
	    Uint ms,s,us,ts;
	    GET_NOW(&ms, &s, &us);
	    ts = TUPLE3(hp, make_small(ms),make_small(s), make_small(us));
	    hp += 4;
	    mess = TUPLE4(hp, am_seq_trace, label, mess, ts);
	    seq_trace_send_to_port(process, seq_tracer, mess, ts);
	}
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
#ifndef ERTS_SMP
	Process* tracer;
#endif
	Eterm sender_copy;
	Eterm receiver_copy;
	Eterm m2;
	Uint sz_label, sz_lastcnt_serial, sz_msg, sz_ts, sz_sender,
	    sz_exitfrom, sz_receiver;

	ASSERT(is_internal_pid(seq_tracer));

#ifndef ERTS_SMP

	tracer = erts_proc_lookup(seq_tracer);
	if (!tracer) {
	    system_seq_tracer = am_false;
	    return; /* no need to send anything */
	}
#endif
	if (receiver == seq_tracer) {
	    return; /* no need to send anything */
	}

	sz_label = size_object(SEQ_TRACE_T_LABEL(token));
	sz_sender = size_object(SEQ_TRACE_T_SENDER(token));
	sz_receiver = size_object(receiver);
	sz_lastcnt_serial = 3; /* TUPLE2 */
	sz_msg = size_object(msg);

	sz_ts = ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & SEQ_TRACE_TIMESTAMP) ? 
		 5 : 0);
	if (exitfrom != NIL) {
	    sz_exit = 4; /* create {'EXIT',exitfrom,msg} */
	    sz_exitfrom = size_object(exitfrom);
	}
	else {
	    sz_exit = 0;
	    sz_exitfrom = 0;
	}
	bp = new_message_buffer(4 /* TUPLE3 */ + sz_ts + 6 /* TUPLE5 */ 
				+ sz_lastcnt_serial + sz_label + sz_msg
				+ sz_exit + sz_exitfrom
				+ sz_sender + sz_receiver);
	hp = bp->mem;
	label = copy_struct(SEQ_TRACE_T_LABEL(token), sz_label, &hp, &bp->off_heap);
	lastcnt_serial = TUPLE2(hp,SEQ_TRACE_T_LASTCNT(token),SEQ_TRACE_T_SERIAL(token));
	hp += 3;
	m2 = copy_struct(msg, sz_msg, &hp, &bp->off_heap);
	if (sz_exit) {
	    Eterm exitfrom_copy = copy_struct(exitfrom,
					      sz_exitfrom,
					      &hp,
					      &bp->off_heap);
	    m2 = TUPLE3(hp, am_EXIT, exitfrom_copy, m2);
	    hp += 4;
	}
	sender_copy = copy_struct(SEQ_TRACE_T_SENDER(token),
				  sz_sender,
				  &hp,
				  &bp->off_heap);
	receiver_copy = copy_struct(receiver,
				    sz_receiver,
				    &hp,
				    &bp->off_heap);
	mess = TUPLE5(hp,
		      type_atom,
		      lastcnt_serial,
		      sender_copy,
		      receiver_copy,
		      m2);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (sz_ts) {/* timestamp should be included */
	    Uint ms,s,us,ts;
	    GET_NOW(&ms, &s, &us);
	    ts = TUPLE3(hp, make_small(ms),make_small(s), make_small(us));
	    hp += 4;
	    mess = TUPLE4(hp, am_seq_trace, label, mess, ts);
	} else {
	    mess = TUPLE3(hp, am_seq_trace, label, mess);
	}

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_SEQTRACE, NIL, NIL, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, NULL, bp, mess, NIL
#ifdef USE_VM_PROBES
			   , NIL
#endif
			   ); /* trace_token must be NIL here */
#endif
    }
}

/* Send {trace_ts, Pid, return_to, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, return_to, {Mod, Func, Arity}}
 */
void 
erts_trace_return_to(Process *p, BeamInstr *pc)
{
#define LOCAL_HEAP_SIZE (4+5+5)
    Eterm* hp;
    Eterm mfa;
    Eterm mess;
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);

    BeamInstr *code_ptr = find_function_from_pc(pc);


    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

    hp = local_heap;

    if (!code_ptr) {
	mfa = am_undefined;
    } else {
	mfa = TUPLE3(hp, code_ptr[0], code_ptr[1], make_small(code_ptr[2]));
	hp += 4;
    }
	
    mess = TUPLE4(hp, am_trace, p->common.id, am_return_to, mfa);
    hp += 5;

    erts_smp_mtx_lock(&smq_mtx);

    if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	hp = patch_ts(mess, hp);
    }

    if (is_internal_port(ERTS_TRACER_PROC(p))) {
	send_to_port(p, mess, &ERTS_TRACER_PROC(p), &ERTS_TRACE_FLAGS(p));
    } else {
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;
	unsigned size;

	/*
	 * Find the tracer.
	 */
	ASSERT(is_internal_pid(ERTS_TRACER_PROC(p)));

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(p),
			    ERTS_TRACE_FLAGS(p));

	size = size_object(mess);

	hp = ERTS_ALLOC_SYSMSG_HEAP(size, &bp, &off_heap, tracer_ref);
	
	/*
	 * Copy the trace message into the buffer and enqueue it.
	 */
	mess = copy_struct(mess, size, &hp, off_heap);
	ERTS_ENQ_TRACE_MSG(p->common.id, tracer_ref, mess, bp);
    }
    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
    erts_smp_mtx_unlock(&smq_mtx);
}


/* Send {trace_ts, Pid, return_from, {Mod, Name, Arity}, Retval, Timestamp}
 * or   {trace, Pid, return_from, {Mod, Name, Arity}, Retval}
 */
void
erts_trace_return(Process* p, BeamInstr* fi, Eterm retval, Eterm *tracer_pid)
{
    Eterm* hp;
    Eterm mfa;
    Eterm mess;
    Eterm mod, name;
    int arity;
    Uint meta_flags, *tracee_flags;
#ifdef ERTS_SMP
    Eterm tracee;
#endif
    
    ASSERT(tracer_pid);
    if (*tracer_pid == am_true) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer_pid = &ERTS_TRACER_PROC(p);
    }
    if (is_nil(*tracer_pid)) {
	/* Trace disabled */
	return;
    }
    ASSERT(is_internal_pid(*tracer_pid) || is_internal_port(*tracer_pid));
    if (*tracer_pid == p->common.id) {
	/* Do not generate trace messages to oneself */
	return;
    }
    if (tracer_pid == &ERTS_TRACER_PROC(p)) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &ERTS_TRACE_FLAGS(p);
#ifdef ERTS_SMP
	tracee = p->common.id;
#endif
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
#ifdef ERTS_SMP
	tracee = NIL;
#endif
    }
    if (! (*tracee_flags & F_TRACE_CALLS)) {
	return;
    }
    
    mod = fi[0];
    name = fi[1];
    arity = fi[2];
    
    if (is_internal_port(*tracer_pid)) {
#define LOCAL_HEAP_SIZE (4+6+5)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);
	hp = local_heap;
	mfa = TUPLE3(hp, mod, name, make_small(arity));
	hp += 4;
	mess = TUPLE5(hp, am_trace, p->common.id, am_return_from, mfa, retval);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;
	unsigned size;
	unsigned retval_size;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid));

	ERTS_GET_TRACER_REF(tracer_ref, *tracer_pid, *tracee_flags);
	
	retval_size = size_object(retval);
	size = 6 + 4 + retval_size;
	if (*tracee_flags & F_TIMESTAMP) {
	    size += 1+4;
	}

	hp = ERTS_ALLOC_SYSMSG_HEAP(size, &bp, &off_heap, tracer_ref);
#ifdef DEBUG
	limit = hp + size;
#endif

	/*
	 * Build the trace tuple and put it into receive queue of the tracer process.
	 */
	
	mfa = TUPLE3(hp, mod, name, make_small(arity));
	hp += 4;
	retval = copy_struct(retval, retval_size, &hp, off_heap);
	mess = TUPLE5(hp, am_trace, p->common.id, am_return_from, mfa, retval);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ASSERT(hp == limit);

	ERTS_ENQ_TRACE_MSG(tracee, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}

/* Send {trace_ts, Pid, exception_from, {Mod, Name, Arity}, {Class,Value}, 
 *       Timestamp}
 * or   {trace, Pid, exception_from, {Mod, Name, Arity}, {Class,Value}, 
 *       Timestamp}
 *
 * Where Class is atomic but Value is any term.
 */
void
erts_trace_exception(Process* p, BeamInstr mfa[3], Eterm class, Eterm value,
		     Eterm *tracer_pid)
{
    Eterm* hp;
    Eterm mfa_tuple;
    Eterm cv;
    Eterm mess;
    Uint meta_flags, *tracee_flags;
#ifdef ERTS_SMP
    Eterm tracee;
#endif
    
    ASSERT(tracer_pid);
    if (*tracer_pid == am_true) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer_pid = &ERTS_TRACER_PROC(p);
    }
    if (is_nil(*tracer_pid)) {
	/* Trace disabled */
	return;
    }
    ASSERT(is_internal_pid(*tracer_pid) || is_internal_port(*tracer_pid));
    if (*tracer_pid == p->common.id) {
	/* Do not generate trace messages to oneself */
	return;
    }
    if (tracer_pid == &ERTS_TRACER_PROC(p)) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &ERTS_TRACE_FLAGS(p);
#ifdef ERTS_SMP
	tracee = p->common.id;
#endif
	if (! (*tracee_flags & F_TRACE_CALLS)) {
	    return;
	}
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
#ifdef ERTS_SMP
	tracee = NIL;
#endif
    }
    
    if (is_internal_port(*tracer_pid)) {
#define LOCAL_HEAP_SIZE (4+3+6+5)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;
	mfa_tuple = TUPLE3(hp, (Eterm) mfa[0], (Eterm) mfa[1], make_small((Eterm)mfa[2]));
	hp += 4;
	cv = TUPLE2(hp, class, value);
	hp += 3;
	mess = TUPLE5(hp, am_trace, p->common.id, am_exception_from, mfa_tuple, cv);
	hp += 6;
	ASSERT((hp - local_heap) <= LOCAL_HEAP_SIZE);
	erts_smp_mtx_lock(&smq_mtx);
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp); /* hp += 5 */
	    ASSERT((hp - local_heap) == LOCAL_HEAP_SIZE);
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;
	unsigned size;
	unsigned value_size;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid));

	ERTS_GET_TRACER_REF(tracer_ref, *tracer_pid, *tracee_flags);
	
	value_size = size_object(value);
	size = 6 + 4 + 3 + value_size;
	if (*tracee_flags & F_TIMESTAMP) {
	    size += 1+4;
	}

	hp = ERTS_ALLOC_SYSMSG_HEAP(size, &bp, &off_heap, tracer_ref);
#ifdef DEBUG
	limit = hp + size;
#endif

	/*
	 * Build the trace tuple and put it into receive queue of the tracer process.
	 */
	
	mfa_tuple = TUPLE3(hp, (Eterm) mfa[0], (Eterm) mfa[1], make_small((Eterm) mfa[2]));
	hp += 4;
	value = copy_struct(value, value_size, &hp, off_heap);
	cv = TUPLE2(hp, class, value);
	hp += 3;
	mess = TUPLE5(hp, am_trace, p->common.id, 
		      am_exception_from, mfa_tuple, cv);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ASSERT(hp == limit);

	ERTS_ENQ_TRACE_MSG(tracee, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}

/*
 * This function implements the new call trace.
 *
 * Send {trace_ts, Pid, call, {Mod, Func, A}, PamResult, Timestamp}
 * or   {trace_ts, Pid, call, {Mod, Func, A}, Timestamp}
 * or   {trace, Pid, call, {Mod, Func, A}, PamResult}
 * or   {trace, Pid, call, {Mod, Func, A}
 *
 * where 'A' is arity or argument list depending on trace flag 'arity'.
 *
 * If *tracer_pid is am_true, it is a breakpoint trace that shall use
 * the process tracer, if it is NIL no trace message is generated, 
 * if it is a pid or port we do a meta trace.
 */
Uint32
erts_call_trace(Process* p, BeamInstr mfa[3], Binary *match_spec,
		Eterm* args, int local, Eterm *tracer_pid)
{
    Eterm* hp;
    Eterm mfa_tuple;
    int arity;
    int i;
    Uint32 return_flags;
    Eterm pam_result = am_true;
    Eterm mess;
    Uint meta_flags, *tracee_flags;
#ifdef ERTS_SMP
    Eterm tracee;
#endif
    Eterm transformed_args[MAX_ARG];
    DeclareTypedTmpHeap(ErlSubBin,sub_bin_heap,p);

    ASSERT(tracer_pid);
    if (*tracer_pid == am_true) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer_pid = &ERTS_TRACER_PROC(p);
    } 
    if (is_nil(*tracer_pid)) {
	/* Trace disabled */
	return 0;
    }
    ASSERT(is_internal_pid(*tracer_pid) || is_internal_port(*tracer_pid));
    if (*tracer_pid == p->common.id) {
	/* Do not generate trace messages to oneself */
	return 0;
    }
    if (tracer_pid == &ERTS_TRACER_PROC(p)) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &ERTS_TRACE_FLAGS(p);
#ifdef ERTS_SMP
	tracee = p->common.id;
#endif
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	if (ERTS_TRACE_FLAGS(p) & F_SENSITIVE) {
	    /* No trace messages for sensitive processes. */
	    return 0;
	}
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
#ifdef ERTS_SMP
	tracee = NIL;
#endif
    }

    /*
     * Because of the delayed sub-binary creation optimization introduced in
     * R12B, (at most) one of arguments can be a match context instead of
     * a binary. Since we don't want to handle match contexts in utility functions
     * such as size_object() and copy_struct(), we must make sure that we
     * temporarily convert any match contexts to sub binaries.
     */
    arity = (Eterm) mfa[2];
    UseTmpHeap(ERL_SUB_BIN_SIZE,p);
#ifdef DEBUG
    sub_bin_heap->thing_word = 0;
#endif
    for (i = 0; i < arity; i++) {
	Eterm arg = args[i];
	if (is_boxed(arg) && header_is_bin_matchstate(*boxed_val(arg))) {
	    ErlBinMatchState* ms = (ErlBinMatchState *) boxed_val(arg);
	    ErlBinMatchBuffer* mb = &ms->mb;
	    Uint bit_size;

	    ASSERT(sub_bin_heap->thing_word == 0); /* At most one of match context */

	    bit_size = mb->size - mb->offset;
	    sub_bin_heap->thing_word = HEADER_SUB_BIN;
	    sub_bin_heap->size = BYTE_OFFSET(bit_size);
	    sub_bin_heap->bitsize = BIT_OFFSET(bit_size);
	    sub_bin_heap->offs = BYTE_OFFSET(mb->offset);
	    sub_bin_heap->bitoffs = BIT_OFFSET(mb->offset);
	    sub_bin_heap->is_writable = 0;
	    sub_bin_heap->orig = mb->orig;

	    arg = make_binary(sub_bin_heap);
	}
	transformed_args[i] = arg;
    }
    args = transformed_args;

    if (is_internal_port(*tracer_pid)) {
#if HEAP_ON_C_STACK
	Eterm local_heap[64+MAX_ARG];
#else
	Eterm *local_heap = erts_alloc(ERTS_ALC_T_TEMP_TERM,
				       sizeof(Eterm)*(64+MAX_ARG));
#endif
	hp = local_heap;

	if (!erts_is_valid_tracer_port(*tracer_pid)) {
#ifdef ERTS_SMP
	    ASSERT(is_nil(tracee) || tracer_pid == &ERTS_TRACER_PROC(p));
	    if (is_not_nil(tracee))
		erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
	    *tracee_flags &= ~TRACEE_FLAGS;
	    *tracer_pid = NIL;
#ifdef ERTS_SMP
	    if (is_not_nil(tracee))
		erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
#if !HEAP_ON_C_STACK
	    erts_free(ERTS_ALC_T_TEMP_TERM,local_heap);
#endif
	    UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
	    return 0;
	}
	
	/*
	 * If there is a PAM program, run it.  Return if it fails.
	 *
	 * Some precedence rules:
	 * 
	 * - No proc flags, e.g 'silent' or 'return_to' 
	 *   has any effect on meta trace.
	 * - The 'silent' process trace flag silences all call 
	 *   related messages, e.g 'call', 'return_to' and 'return_from'.
	 * - The {message,_} PAM function does not affect {return_trace}.
	 * - The {message,false} PAM function shall give the same
	 *   'call' trace message as no PAM match.
	 * - The {message,true} PAM function shall give the same
	 *   'call' trace message as a nonexistent PAM program.
	 */
	
	/* BEGIN this code should be the same for port and pid trace */
	return_flags = 0;
	if (match_spec) {
	    pam_result = erts_match_set_run(p, match_spec, args, arity,
					    ERTS_PAM_TMP_RESULT, &return_flags);
	    if (is_non_value(pam_result)) {
		erts_match_set_release_result(p);
#if !HEAP_ON_C_STACK
		erts_free(ERTS_ALC_T_TEMP_TERM,local_heap);
#endif
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return 0;
	    }
	}
	if (tracee_flags == &meta_flags) {
	    /* Meta trace */
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
#if !HEAP_ON_C_STACK
		erts_free(ERTS_ALC_T_TEMP_TERM,local_heap);
#endif
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return return_flags;
	    }
	} else {
	    /* Non-meta trace */
	    if (*tracee_flags & F_TRACE_SILENT) { 
		erts_match_set_release_result(p);
#if !HEAP_ON_C_STACK
		erts_free(ERTS_ALC_T_TEMP_TERM,local_heap);
#endif
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return 0;
	    }
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
#if !HEAP_ON_C_STACK
		erts_free(ERTS_ALC_T_TEMP_TERM,local_heap);
#endif
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return return_flags;
	    }
	    if (local && (*tracee_flags & F_TRACE_RETURN_TO)) {
		return_flags |= MATCH_SET_RETURN_TO_TRACE;
	    }
	}
	/* END this code should be the same for port and pid trace */
	
	/*
	 * Build the the {M,F,A} tuple in the local heap. 
	 * (A is arguments or arity.)
	 */
	
	if (*tracee_flags & F_TRACE_ARITY_ONLY) {
	    mfa_tuple = make_small(arity);
	} else {
	    mfa_tuple = NIL;
	    for (i = arity-1; i >= 0; i--) {
		mfa_tuple = CONS(hp, args[i], mfa_tuple);
		hp += 2;
	    }
	}
	mfa_tuple = TUPLE3(hp, (Eterm) mfa[0], (Eterm) mfa[1], mfa_tuple);
	hp += 4;
	
	/*
	 * Build the trace tuple and send it to the port.
	 */
	
	mess = TUPLE4(hp, am_trace, p->common.id, am_call, mfa_tuple);
	hp += 5;
	if (pam_result != am_true) {
	    hp[-5] = make_arityval(5);
	    *hp++ = pam_result;
	}
	erts_smp_mtx_lock(&smq_mtx);
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
	erts_smp_mtx_unlock(&smq_mtx);
	erts_match_set_release_result(p);
#if !HEAP_ON_C_STACK
	erts_free(ERTS_ALC_T_TEMP_TERM,local_heap);
#endif
	UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
	return *tracer_pid == NIL ? 0 : return_flags;

    } else {
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	Process *tracer;
	ERTS_TRACER_REF_TYPE tracer_ref;
#ifdef ERTS_SMP
	Eterm tpid;
#endif
	unsigned size;
	unsigned sizes[MAX_ARG];
	unsigned pam_result_size = 0;
	int invalid_tracer;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid));
	
	tracer = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
			       *tracer_pid, ERTS_PROC_LOCK_STATUS);
	if (!tracer)
	    invalid_tracer = 1;
	else {
	    invalid_tracer = !(ERTS_TRACE_FLAGS(tracer) & F_TRACER);
	    erts_smp_proc_unlock(tracer, ERTS_PROC_LOCK_STATUS);
	}

	if (invalid_tracer) {
#ifdef ERTS_SMP
	    ASSERT(is_nil(tracee)
		   || tracer_pid == &ERTS_TRACER_PROC(p));
	    if (is_not_nil(tracee))
		erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
	    *tracee_flags &= ~TRACEE_FLAGS;
	    *tracer_pid = NIL;
#ifdef ERTS_SMP
	    if (is_not_nil(tracee))
		erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
	    UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
	    return 0;
	}

#ifdef ERTS_SMP
	tpid = *tracer_pid; /* Need to save tracer pid,
			       since *tracer_pid might
			       be reset by erts_match_set_run() */
	tracer_ref = tpid;
#else
	tracer_ref = tracer;
#endif
	
	/*
	 * If there is a PAM program, run it.  Return if it fails.
	 *
	 * See the rules above in the port trace code.
	 */
	
	/* BEGIN this code should be the same for port and pid trace */
	return_flags = 0;
	if (match_spec) {
	    pam_result = erts_match_set_run(p, match_spec, args, arity,
					    ERTS_PAM_TMP_RESULT, &return_flags);
	    if (is_non_value(pam_result)) {
		erts_match_set_release_result(p);
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return 0;
	    }
	}
	if (tracee_flags == &meta_flags) {
	    /* Meta trace */
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return return_flags;
	    }
	} else {
	    /* Non-meta trace */
	    if (*tracee_flags & F_TRACE_SILENT) { 
		erts_match_set_release_result(p);
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return 0;
	    }
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
		UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
		return return_flags;
	    }
	    if (local && (*tracee_flags & F_TRACE_RETURN_TO)) {
		return_flags |= MATCH_SET_RETURN_TO_TRACE;
	    }
	}
	/* END this code should be the same for port and pid trace */
	
	/*
	 * Calculate number of words needed on heap.
	 */
	
	size = 4 + 5;		/* Trace tuple + MFA tuple. */
	if (! (*tracee_flags & F_TRACE_ARITY_ONLY)) {
	    size += 2*arity;
	    for (i = arity-1; i >= 0; i--) {
		sizes[i] = size_object(args[i]);
		size += sizes[i];
	    }
	}
	if (*tracee_flags & F_TIMESTAMP) {
	    size += 1 + 4;
	    /* One element in trace tuple + timestamp tuple. */
	}
	if (pam_result != am_true) {
	    pam_result_size = size_object(pam_result);
	    size += 1 + pam_result_size;
	    /* One element in trace tuple + term size. */
	}
	
	hp = ERTS_ALLOC_SYSMSG_HEAP(size, &bp, &off_heap, tracer_ref);
#ifdef DEBUG
	limit = hp + size;
#endif

	/*
	 * Build the the {M,F,A} tuple in the message buffer. 
	 * (A is arguments or arity.)
	 */
	
	if (*tracee_flags & F_TRACE_ARITY_ONLY) {
	    mfa_tuple = make_small(arity);
	} else {
	    mfa_tuple = NIL;
	    for (i = arity-1; i >= 0; i--) {
		Eterm term = copy_struct(args[i], sizes[i], &hp, off_heap);
		mfa_tuple = CONS(hp, term, mfa_tuple);
		hp += 2;
	    }
	}
	mfa_tuple = TUPLE3(hp, (Eterm) mfa[0], (Eterm) mfa[1], mfa_tuple);
	hp += 4;
	
	/*
	 * Copy the PAM result (if any) onto the heap.
	 */
	
	if (pam_result != am_true) {
	    pam_result = copy_struct(pam_result, pam_result_size, &hp, off_heap);
	}

	erts_match_set_release_result(p);

	/*
	 * Build the trace tuple and enqueue it.
	 */
	
	mess = TUPLE4(hp, am_trace, p->common.id, am_call, mfa_tuple);
	hp += 5;
	if (pam_result != am_true) {
	    hp[-5] = make_arityval(5);
	    *hp++ = pam_result;
	}

	erts_smp_mtx_lock(&smq_mtx);

	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ASSERT(hp == limit);
	ERTS_ENQ_TRACE_MSG(tracee, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
	UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
	return return_flags;
    }
}

/* Sends trace message:
 *    {trace_ts, ProcessPid, What, Data, Timestamp}
 * or {trace, ProcessPid, What, Data}
 *
 * 'what' must be atomic, 'data' may be a deep term.
 * 'c_p' is the currently executing process, may be NULL.
 * 't_p' is the traced process.
 */
void
trace_proc(Process *c_p, Process *t_p, Eterm what, Eterm data)
{
    Eterm mess;
    Eterm* hp;
    int need;

    ERTS_SMP_LC_ASSERT((erts_proc_lc_my_proc_locks(t_p) != 0)
		       || erts_thr_progress_is_blocking());
    if (is_internal_port(ERTS_TRACER_PROC(t_p))) {
#define LOCAL_HEAP_SIZE (5+5)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);


	hp = local_heap;
	mess = TUPLE4(hp, am_trace, t_p->common.id, what, data);
	hp += 5;
	erts_smp_mtx_lock(&smq_mtx);
	if (ERTS_TRACE_FLAGS(t_p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(
#ifndef ERTS_SMP
	    /* No fake schedule out and in again after an exit */
	    what == am_exit ? NULL : c_p,
#else
	    /* Fake schedule out and in are never sent when smp enabled */
	    c_p,
#endif
	    mess,
	    &ERTS_TRACER_PROC(t_p),
	    &ERTS_TRACE_FLAGS(t_p));
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	Eterm tmp;
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;
	size_t sz_data;

	ASSERT(is_internal_pid(ERTS_TRACER_PROC(t_p)));

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(t_p),
			    ERTS_TRACE_FLAGS(t_p));

	sz_data = size_object(data);

	need = sz_data + 5 + TS_SIZE(t_p);

	hp = ERTS_ALLOC_SYSMSG_HEAP(need, &bp, &off_heap, tracer_ref);

	tmp = copy_struct(data, sz_data, &hp, off_heap);
	mess = TUPLE4(hp, am_trace, t_p->common.id, what, tmp);
	hp += 5;

	erts_smp_mtx_lock(&smq_mtx);

	if (ERTS_TRACE_FLAGS(t_p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ERTS_ENQ_TRACE_MSG(t_p->common.id, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}


/* Sends trace message:
 *    {trace_ts, ParentPid, spawn, ChildPid, {Mod, Func, Args}, Timestamp}
 * or {trace, ParentPid, spawn, ChildPid, {Mod, Func, Args}}
 *
 * 'pid' is the ChildPid, 'mod' and 'func' must be atomic,
 * and 'args' may be a deep term.
 */
void
trace_proc_spawn(Process *p, Eterm pid, 
		 Eterm mod, Eterm func, Eterm args)
{
    Eterm mfa;
    Eterm mess;
    Eterm* hp;

    if (is_internal_port(ERTS_TRACER_PROC(p))) {
#define LOCAL_HEAP_SIZE (4+6+5)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;
	mfa = TUPLE3(hp, mod, func, args);
	hp += 4;
	mess = TUPLE5(hp, am_trace, p->common.id, am_spawn, pid, mfa);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, &ERTS_TRACER_PROC(p), &ERTS_TRACE_FLAGS(p));
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	Eterm tmp;
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;
	size_t sz_args, sz_pid;
	Uint need;

	ASSERT(is_internal_pid(ERTS_TRACER_PROC(p)));

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(p),
			    ERTS_TRACE_FLAGS(p));

	sz_args = size_object(args);
	sz_pid = size_object(pid);
	need = sz_args + 4 + 6 + TS_SIZE(p);

	hp = ERTS_ALLOC_SYSMSG_HEAP(need, &bp, &off_heap, tracer_ref);

	tmp = copy_struct(args, sz_args, &hp, off_heap);
	mfa = TUPLE3(hp, mod, func, tmp);
	hp += 4;
	tmp = copy_struct(pid, sz_pid, &hp, off_heap);
	mess = TUPLE5(hp, am_trace, p->common.id, am_spawn, tmp, mfa);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ERTS_ENQ_TRACE_MSG(p->common.id, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}

void save_calls(Process *p, Export *e)
{
    struct saved_calls *scb = ERTS_PROC_GET_SAVED_CALLS_BUF(p);
    if (scb) {
	Export **ct = &scb->ct[0];
	int len = scb->len;

	ct[scb->cur] = e;
	if (++scb->cur >= len)
	    scb->cur = 0;
	if (scb->n < len)
	    scb->n++;
    }
}

/* Sends trace message:
 *    {trace_ts, Pid, What, Msg, Timestamp}
 * or {trace, Pid, What, Msg}
 *
 * where 'What' must be atomic and 'Msg' is: 
 * [{heap_size, HeapSize}, {old_heap_size, OldHeapSize}, 
 *  {stack_size, StackSize}, {recent_size, RecentSize}, 
 *  {mbuf_size, MbufSize}]
 *
 * where 'HeapSize', 'OldHeapSize', 'StackSize', 'RecentSize and 'MbufSize'
 * are all small (atomic) integers.
 */
void
trace_gc(Process *p, Eterm what)
{
    ERTS_DECL_AM(bin_vheap_size);
    ERTS_DECL_AM(bin_vheap_block_size);
    ERTS_DECL_AM(bin_old_vheap_size);
    ERTS_DECL_AM(bin_old_vheap_block_size);

    ErlHeapFragment *bp = NULL;
    ErlOffHeap *off_heap;
    ERTS_TRACER_REF_TYPE tracer_ref = ERTS_NULL_TRACER_REF;	/* Initialized
								   to eliminate
								   compiler
								   warning */
    Eterm* hp;
    Eterm msg = NIL;
    Uint size;

    Eterm tags[] = {
	am_old_heap_block_size,
	am_heap_block_size,
	am_mbuf_size,
	am_recent_size,
	am_stack_size,
	am_old_heap_size,
	am_heap_size,
	AM_bin_vheap_size,
	AM_bin_vheap_block_size,
	AM_bin_old_vheap_size,
	AM_bin_old_vheap_block_size
    };

    UWord values[] = {
	OLD_HEAP(p) ? OLD_HEND(p) - OLD_HEAP(p) : 0,
	HEAP_SIZE(p),
	MBUF_SIZE(p),
	HIGH_WATER(p) - HEAP_START(p),
	STACK_START(p) - p->stop,
	OLD_HEAP(p) ? OLD_HTOP(p) - OLD_HEAP(p) : 0,
	HEAP_TOP(p) - HEAP_START(p),
	MSO(p).overhead,
	BIN_VHEAP_SZ(p),
	BIN_OLD_VHEAP(p),
	BIN_OLD_VHEAP_SZ(p)
    };
#define LOCAL_HEAP_SIZE						\
    (sizeof(values)/sizeof(*values)) *				\
	(2/*cons*/ + 3/*2-tuple*/ + BIG_UINT_HEAP_SIZE) +	\
	5/*4-tuple */ + TS_HEAP_WORDS
    DeclareTmpHeap(local_heap,LOCAL_HEAP_SIZE,p);
#ifdef DEBUG
    Eterm* limit;
#endif

    ASSERT(sizeof(values)/sizeof(*values) == sizeof(tags)/sizeof(Eterm));

    UseTmpHeap(LOCAL_HEAP_SIZE,p);

    if (is_internal_port(ERTS_TRACER_PROC(p))) {
	hp = local_heap;
#ifdef DEBUG
	size = 0;
	(void) erts_bld_atom_uword_2tup_list(NULL,
					    &size,
					    sizeof(values)/sizeof(*values),
					    tags,
					    values);
	size += 5/*4-tuple*/ + TS_SIZE(p);
#endif
    } else {
	ASSERT(is_internal_pid(ERTS_TRACER_PROC(p)));

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(p),
			    ERTS_TRACE_FLAGS(p));

	size = 0;
	(void) erts_bld_atom_uword_2tup_list(NULL,
					    &size,
					    sizeof(values)/sizeof(*values),
					    tags,
					    values);
	size += 5/*4-tuple*/ + TS_SIZE(p);

	hp = ERTS_ALLOC_SYSMSG_HEAP(size, &bp, &off_heap, tracer_ref);
    }

#ifdef DEBUG
    limit = hp + size;
    ASSERT(size <= LOCAL_HEAP_SIZE);
#endif

    msg = erts_bld_atom_uword_2tup_list(&hp,
				       NULL,
				       sizeof(values)/sizeof(*values),
				       tags,
				       values);

    msg = TUPLE4(hp, am_trace, p->common.id, what, msg);
    hp += 5;

    erts_smp_mtx_lock(&smq_mtx);

    if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	hp = patch_ts(msg, hp);
    }
    ASSERT(hp == limit);
    if (is_internal_port(ERTS_TRACER_PROC(p)))
	send_to_port(p, msg, &ERTS_TRACER_PROC(p), &ERTS_TRACE_FLAGS(p));
    else
	ERTS_ENQ_TRACE_MSG(p->common.id, tracer_ref, msg, bp);
    erts_smp_mtx_unlock(&smq_mtx);
    UnUseTmpHeap(LOCAL_HEAP_SIZE,p);
#undef LOCAL_HEAP_SIZE
}

void 
monitor_long_schedule_proc(Process *p, BeamInstr *in_fp, BeamInstr *out_fp, Uint time)
{
    ErlHeapFragment *bp;
    ErlOffHeap *off_heap;
#ifndef ERTS_SMP
    Process *monitor_p;
#endif
    Uint hsz;
    Eterm *hp, list, in_mfa = am_undefined, out_mfa = am_undefined;
    Eterm in_tpl, out_tpl, tmo_tpl, tmo, msg;
 

#ifndef ERTS_SMP
    ASSERT(is_internal_pid(system_monitor));
    monitor_p = erts_proc_lookup(system_monitor);
    if (!monitor_p || p == monitor_p) {
	return;
    }
#endif
    /* 
     * Size: {monitor, pid, long_schedule, [{timeout, T}, {in, {M,F,A}},{out,{M,F,A}}]} ->
     * 5 (top tuple of 4), (3 (elements) * 2 (cons)) + 3 (timeout tuple of 2) + size of Timeout +
     * (2 * 3 (in/out tuple of 2)) + 
     * 0 (unknown) or 4 (MFA tuple of 3) + 0 (unknown) or 4 (MFA tuple of 3)
     * = 20 + (in_fp != NULL) ? 4 : 0 + (out_fp != NULL) ? 4 : 0 + size of Timeout
     */
    hsz = 20 + ((in_fp != NULL) ? 4 : 0) + ((out_fp != NULL) ? 4 : 0);
    (void) erts_bld_uint(NULL, &hsz, time);
    hp = ERTS_ALLOC_SYSMSG_HEAP(hsz, &bp, &off_heap, monitor_p);
    tmo = erts_bld_uint(&hp, NULL, time);
    if (in_fp != NULL) {
	in_mfa = TUPLE3(hp,(Eterm) in_fp[0], (Eterm) in_fp[1], make_small(in_fp[2]));
	hp +=4;
    } 
    if (out_fp != NULL) {
	out_mfa = TUPLE3(hp,(Eterm) out_fp[0], (Eterm) out_fp[1], make_small(out_fp[2]));
	hp +=4;
    } 
    tmo_tpl = TUPLE2(hp,am_timeout, tmo);
    hp += 3;
    in_tpl = TUPLE2(hp,am_in,in_mfa);
    hp += 3;
    out_tpl = TUPLE2(hp,am_out,out_mfa);
    hp += 3;
    list = CONS(hp,out_tpl,NIL); 
    hp += 2;
    list = CONS(hp,in_tpl,list);
    hp += 2;
    list = CONS(hp,tmo_tpl,list);
    hp += 2;
    msg = TUPLE4(hp, am_monitor, p->common.id, am_long_schedule, list);
    hp += 5;
#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, p->common.id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, NULL, bp, msg, NIL
#ifdef USE_VM_PROBES
			   , NIL
#endif
		       );
#endif
}
void 
monitor_long_schedule_port(Port *pp, ErtsPortTaskType type, Uint time)
{
    ErlHeapFragment *bp;
    ErlOffHeap *off_heap;
#ifndef ERTS_SMP
    Process *monitor_p;
#endif
    Uint hsz;
    Eterm *hp, list, op;
    Eterm op_tpl, tmo_tpl, tmo, msg;
 

#ifndef ERTS_SMP
    ASSERT(is_internal_pid(system_monitor));
    monitor_p = erts_proc_lookup(system_monitor);
    if (!monitor_p) {
	return;
    }
#endif
    /* 
     * Size: {monitor, port, long_schedule, [{timeout, T}, {op, Operation}]} ->
     * 5 (top tuple of 4), (2 (elements) * 2 (cons)) + 3 (timeout tuple of 2) 
     * + size of Timeout + 3 (op tuple of 2 atoms)
     * = 15 + size of Timeout
     */
    hsz = 15;
    (void) erts_bld_uint(NULL, &hsz, time);

    hp = ERTS_ALLOC_SYSMSG_HEAP(hsz, &bp, &off_heap, monitor_p);

    switch (type) {
    case ERTS_PORT_TASK_PROC_SIG: op = am_proc_sig; break;
    case ERTS_PORT_TASK_TIMEOUT: op = am_timeout; break;
    case ERTS_PORT_TASK_INPUT: op = am_input; break;
    case ERTS_PORT_TASK_OUTPUT: op = am_output; break;
    case ERTS_PORT_TASK_EVENT: op = am_event; break;
    case ERTS_PORT_TASK_DIST_CMD: op = am_dist_cmd; break;
    default: op = am_undefined; break;
    }

    tmo = erts_bld_uint(&hp, NULL, time);

    op_tpl = TUPLE2(hp,am_port_op,op); 
    hp += 3;

    tmo_tpl = TUPLE2(hp,am_timeout, tmo);
    hp += 3;

    list = CONS(hp,op_tpl,NIL);
    hp += 2;
    list = CONS(hp,tmo_tpl,list);
    hp += 2;
    msg = TUPLE4(hp, am_monitor, pp->common.id, am_long_schedule, list);
    hp += 5;
#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, pp->common.id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, NULL, bp, msg, NIL
#ifdef USE_VM_PROBES
			   , NIL
#endif
		       );
#endif
}

void
monitor_long_gc(Process *p, Uint time) {
    ErlHeapFragment *bp;
    ErlOffHeap *off_heap;
#ifndef ERTS_SMP
    Process *monitor_p;
#endif
    Uint hsz;
    Eterm *hp, list, msg;
    Eterm tags[] = {
	am_timeout,
	am_old_heap_block_size,
	am_heap_block_size,
	am_mbuf_size,
	am_stack_size,
	am_old_heap_size,
	am_heap_size
    };
    UWord values[] = {
	time,
	OLD_HEAP(p) ? OLD_HEND(p) - OLD_HEAP(p) : 0,
	HEAP_SIZE(p),
	MBUF_SIZE(p),
	STACK_START(p) - p->stop,
	OLD_HEAP(p) ? OLD_HTOP(p) - OLD_HEAP(p) : 0,
	HEAP_TOP(p) - HEAP_START(p)
    };
#ifdef DEBUG
    Eterm *hp_end;
#endif
	
#ifndef ERTS_SMP
    ASSERT(is_internal_pid(system_monitor));
    monitor_p = erts_proc_lookup(system_monitor);
    if (!monitor_p || p == monitor_p)
	return;
#endif

    hsz = 0;
    (void) erts_bld_atom_uword_2tup_list(NULL,
					&hsz,
					sizeof(values)/sizeof(*values),
					tags,
					values);
    hsz += 5 /* 4-tuple */;

    hp = ERTS_ALLOC_SYSMSG_HEAP(hsz, &bp, &off_heap, monitor_p);

#ifdef DEBUG
    hp_end = hp + hsz;
#endif

    list = erts_bld_atom_uword_2tup_list(&hp,
					NULL,
					sizeof(values)/sizeof(*values),
					tags,
					values);
    msg = TUPLE4(hp, am_monitor, p->common.id, am_long_gc, list); 

#ifdef DEBUG
    hp += 5 /* 4-tuple */;
    ASSERT(hp == hp_end);
#endif

#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, p->common.id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, NULL, bp, msg, NIL
#ifdef USE_VM_PROBES
			   , NIL
#endif
		       );
#endif
}

void
monitor_large_heap(Process *p) {
    ErlHeapFragment *bp;
    ErlOffHeap *off_heap;
#ifndef ERTS_SMP
    Process *monitor_p;
#endif
    Uint hsz;
    Eterm *hp, list, msg;
    Eterm tags[] = {
	am_old_heap_block_size,
	am_heap_block_size,
	am_mbuf_size,
	am_stack_size,
	am_old_heap_size,
	am_heap_size
    };
    UWord values[] = {
	OLD_HEAP(p) ? OLD_HEND(p) - OLD_HEAP(p) : 0,
	HEAP_SIZE(p),
	MBUF_SIZE(p),
	STACK_START(p) - p->stop,
	OLD_HEAP(p) ? OLD_HTOP(p) - OLD_HEAP(p) : 0,
	HEAP_TOP(p) - HEAP_START(p)
    };
#ifdef DEBUG
    Eterm *hp_end;
#endif


#ifndef ERTS_SMP 
    ASSERT(is_internal_pid(system_monitor));
    monitor_p = erts_proc_lookup(system_monitor);
    if (monitor_p || p == monitor_p) {
	return;
    }
#endif

    hsz = 0;
    (void) erts_bld_atom_uword_2tup_list(NULL,
					&hsz,
					sizeof(values)/sizeof(*values),
					tags,
					values);
    hsz += 5 /* 4-tuple */;

    hp = ERTS_ALLOC_SYSMSG_HEAP(hsz, &bp, &off_heap, monitor_p);

#ifdef DEBUG
    hp_end = hp + hsz;
#endif

    list = erts_bld_atom_uword_2tup_list(&hp,
					NULL,
					sizeof(values)/sizeof(*values),
					tags,
					values);
    msg = TUPLE4(hp, am_monitor, p->common.id, am_large_heap, list); 

#ifdef DEBUG
    hp += 5 /* 4-tuple */;
    ASSERT(hp == hp_end);
#endif

#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, p->common.id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, NULL, bp, msg, NIL
#ifdef USE_VM_PROBES
		       , NIL
#endif
		       );
#endif
}

void
monitor_generic(Process *p, Eterm type, Eterm spec) {
    ErlHeapFragment *bp;
    ErlOffHeap *off_heap;
#ifndef ERTS_SMP
    Process *monitor_p;
#endif
    Eterm *hp, msg;

#ifndef ERTS_SMP
    ASSERT(is_internal_pid(system_monitor));
    monitor_p = erts_proc_lookup(system_monitor);
    if (!monitor_p || p == monitor_p)
	return;
#endif

    hp = ERTS_ALLOC_SYSMSG_HEAP(5, &bp, &off_heap, monitor_p);

    msg = TUPLE4(hp, am_monitor, p->common.id, type, spec); 
    hp += 5;

#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, p->common.id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, NULL, bp, msg, NIL
#ifdef USE_VM_PROBES
		       , NIL
#endif
		       );
#endif

}


/* Begin system_profile tracing */
/* Scheduler profiling */

void
profile_scheduler(Eterm scheduler_id, Eterm state) {
    Eterm *hp, msg, timestamp;
    Uint Ms, s, us;

#ifndef ERTS_SMP
#define LOCAL_HEAP_SIZE (4 + 7)
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);
    hp = local_heap;
#else
    ErlHeapFragment *bp;
    Uint hsz;

    hsz = 4 + 7;
	
    bp = new_message_buffer(hsz);
    hp = bp->mem;
#endif

    erts_smp_mtx_lock(&smq_mtx);

    switch (state) {
    case am_active:
	active_sched++;
	break;
    case am_inactive:
	active_sched--;
	break;
    default:
	ASSERT(!"Invalid state");
	break;
    }

    GET_NOW(&Ms, &s, &us);
    timestamp = TUPLE3(hp, make_small(Ms), make_small(s), make_small(us)); hp += 4;
    msg = TUPLE6(hp, am_profile, am_scheduler, scheduler_id, state,
		 make_small(active_sched), timestamp); hp += 7;

#ifndef ERTS_SMP
    profile_send(NIL, msg);
    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
#else
    enqueue_sys_msg_unlocked(SYS_MSG_TYPE_SYSPROF, NIL, NIL, msg, bp);
#endif
    erts_smp_mtx_unlock(&smq_mtx);

}

void
profile_scheduler_q(Eterm scheduler_id, Eterm state, Eterm no_schedulers, Uint Ms, Uint s, Uint us) {
    Eterm *hp, msg, timestamp;
    
#ifndef ERTS_SMP	
#define LOCAL_HEAP_SIZE (4 + 7)
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

    hp = local_heap;
#else    
    ErlHeapFragment *bp;
    Uint hsz;

    hsz = 4 + 7;
	
    bp = new_message_buffer(hsz);
    hp = bp->mem;
#endif

    erts_smp_mtx_lock(&smq_mtx);

    timestamp = TUPLE3(hp, make_small(Ms), make_small(s), make_small(us)); hp += 4;
    msg = TUPLE6(hp, am_profile, am_scheduler, scheduler_id, state, no_schedulers, timestamp); hp += 7;
#ifndef ERTS_SMP
    profile_send(NIL, msg);
    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
#else
    enqueue_sys_msg_unlocked(SYS_MSG_TYPE_SYSPROF, NIL, NIL, msg, bp);
#endif
    erts_smp_mtx_unlock(&smq_mtx);

}


/* Send {trace_ts, Pid, What, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, What, {Mod, Func, Arity}}
 *
 * where 'What' is supposed to be 'in' or 'out'.
 *
 * Virtual scheduling do not fake scheduling for ports.
 */


void trace_virtual_sched(Process *p, Eterm what)
{
    trace_sched_aux(p, what, 1);
}

/* Port profiling */

void
trace_port_open(Port *p, Eterm calling_pid, Eterm drv_name) {
    Eterm mess;
    Eterm* hp;

    if (is_internal_port(ERTS_TRACER_PROC(p))) {
#define LOCAL_HEAP_SIZE (5+6)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;

	mess = TUPLE5(hp, am_trace, calling_pid, am_open, p->common.id, drv_name);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	/* No fake schedule */
	send_to_port(NULL, mess, &ERTS_TRACER_PROC(p), &ERTS_TRACE_FLAGS(p));
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	size_t sz_data;
	ERTS_TRACER_REF_TYPE tracer_ref;
	
	ASSERT(is_internal_pid(ERTS_TRACER_PROC(p)));

	sz_data = 6 + TS_SIZE(p);

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(p),
			    ERTS_TRACE_FLAGS(p));

	hp = ERTS_ALLOC_SYSMSG_HEAP(sz_data, &bp, &off_heap, tracer_ref);

	mess = TUPLE5(hp, am_trace, calling_pid, am_open, p->common.id, drv_name);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ERTS_ENQ_TRACE_MSG(p->common.id, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }

}

/* Sends trace message:
 *    {trace_ts, PortPid, What, Data, Timestamp}
 * or {trace, PortPid, What, Data}
 *
 * 'what' must be atomic, 'data' must be atomic.
 * 't_p' is the traced port.
 */
void
trace_port(Port *t_p, Eterm what, Eterm data) {
    Eterm mess;
    Eterm* hp;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(t_p)
		       || erts_thr_progress_is_blocking());

    if (is_internal_port(ERTS_TRACER_PROC(t_p))) {
#define LOCAL_HEAP_SIZE (5+5)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;
	mess = TUPLE4(hp, am_trace, t_p->common.id, what, data);
	hp += 5;
	erts_smp_mtx_lock(&smq_mtx);
	if (ERTS_TRACE_FLAGS(t_p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	/* No fake schedule */
	send_to_port(NULL,mess,&ERTS_TRACER_PROC(t_p),&ERTS_TRACE_FLAGS(t_p));
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	ErlHeapFragment *bp;
	ErlOffHeap *off_heap;
	size_t sz_data;
	ERTS_TRACER_REF_TYPE tracer_ref;

	ASSERT(is_internal_pid(ERTS_TRACER_PROC(t_p)));

	sz_data = 5 + TS_SIZE(t_p);

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(t_p),
			    ERTS_TRACE_FLAGS(t_p));

	hp = ERTS_ALLOC_SYSMSG_HEAP(sz_data, &bp, &off_heap, tracer_ref);

	mess = TUPLE4(hp, am_trace, t_p->common.id, what, data);
	hp += 5;

	erts_smp_mtx_lock(&smq_mtx);

	if (ERTS_TRACE_FLAGS(t_p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ERTS_ENQ_TRACE_MSG(t_p->common.id, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}

/* Send {trace_ts, Pid, What, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, What, {Mod, Func, Arity}}
 *
 * where 'What' is supposed to be 'in' or 'out' and
 * where 'where' is supposed to be location (callback) 
 * for the port.
 */

void
trace_sched_ports(Port *p, Eterm what) {
    trace_sched_ports_where(p,what, make_small(0));
}

void 
trace_sched_ports_where(Port *p, Eterm what, Eterm where) {
    Eterm mess;
    Eterm* hp;
    int ws = 5;	
    Eterm sched_id = am_undefined;
    
    if (is_internal_port(ERTS_TRACER_PROC(p))) {
#define LOCAL_HEAP_SIZE (5+6)
	DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
	UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

	hp = local_heap;
	
	if (IS_TRACED_FL(p, F_TRACE_SCHED_NO)) {
#ifdef ERTS_SMP
		ErtsSchedulerData *esd = erts_get_scheduler_data();
		if (esd) sched_id = make_small(esd->no);
		else sched_id = am_undefined;
#else
		sched_id = make_small(1);
#endif
		mess = TUPLE5(hp, am_trace, p->common.id, what, sched_id, where);
		ws = 6;
	} else {
		mess = TUPLE4(hp, am_trace, p->common.id, what, where);
		ws = 5;
	}
	hp += ws;
	
	erts_smp_mtx_lock(&smq_mtx);
	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	
	/* No fake scheduling */
	send_to_port(NULL, mess, &ERTS_TRACER_PROC(p), &ERTS_TRACE_FLAGS(p));
	UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
	erts_smp_mtx_unlock(&smq_mtx);
    } else  {
	ErlHeapFragment *bp;
    	ErlOffHeap *off_heap;
	ERTS_TRACER_REF_TYPE tracer_ref;

	ASSERT(is_internal_pid(ERTS_TRACER_PROC(p)));
	
	if (IS_TRACED_FL(p, F_TRACE_SCHED_NO)) ws = 6; /* Make place for scheduler id */

	ERTS_GET_TRACER_REF(tracer_ref,
			    ERTS_TRACER_PROC(p),
			    ERTS_TRACE_FLAGS(p));

	hp = ERTS_ALLOC_SYSMSG_HEAP(ws+TS_SIZE(p), &bp, &off_heap, tracer_ref);

	if (IS_TRACED_FL(p, F_TRACE_SCHED_NO)) {
#ifdef ERTS_SMP
		ErtsSchedulerData *esd = erts_get_scheduler_data();
		if (esd) sched_id = make_small(esd->no);
		else sched_id = am_undefined;
#else
		sched_id = make_small(1);
#endif
		mess = TUPLE5(hp, am_trace, p->common.id, what, sched_id, where);
	} else {
		mess = TUPLE4(hp, am_trace, p->common.id, what, where);
	}
	hp += ws;

	erts_smp_mtx_lock(&smq_mtx);

	if (ERTS_TRACE_FLAGS(p) & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ERTS_ENQ_TRACE_MSG(p->common.id, tracer_ref, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
    }
}

/* Port profiling */

void
profile_runnable_port(Port *p, Eterm status) {
    Uint Ms, s, us;
    Eterm *hp, msg, timestamp;

    Eterm count = make_small(0);

#ifndef ERTS_SMP
#define LOCAL_HEAP_SIZE (4 + 6)

    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

    hp = local_heap;
    
#else
    ErlHeapFragment *bp;
    Uint hsz;

    hsz = 4 + 6;

    bp = new_message_buffer(hsz);
    hp = bp->mem;
#endif

    erts_smp_mtx_lock(&smq_mtx);

    GET_NOW(&Ms, &s, &us);
    timestamp = TUPLE3(hp, make_small(Ms), make_small(s), make_small(us)); hp += 4;
    msg = TUPLE5(hp, am_profile, p->common.id, status, count, timestamp); hp += 6;

#ifndef ERTS_SMP
    profile_send(p->common.id, msg);
    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
#else
    enqueue_sys_msg_unlocked(SYS_MSG_TYPE_SYSPROF, p->common.id, NIL, msg, bp);
#endif
    erts_smp_mtx_unlock(&smq_mtx);
}

/* Process profiling */
void 
profile_runnable_proc(Process *p, Eterm status){
    Uint Ms, s, us;
    Eterm *hp, msg, timestamp;
    Eterm where = am_undefined;

#ifndef ERTS_SMP
#define LOCAL_HEAP_SIZE (4 + 6 + 4)

    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

    hp = local_heap;
#else
    ErlHeapFragment *bp;
    Uint hsz = 4 + 6 + 4;
#endif
	
    if (!p->current) {
        p->current = find_function_from_pc(p->i);
    }

#ifdef ERTS_SMP
    if (!p->current) {
	hsz = 4 + 6;
    }

    bp = new_message_buffer(hsz);
    hp = bp->mem;
#endif

    if (p->current) {
	where = TUPLE3(hp, p->current[0], p->current[1], make_small(p->current[2])); hp += 4;
    } else {
	where = make_small(0);
    }
	
    erts_smp_mtx_lock(&smq_mtx);

    GET_NOW(&Ms, &s, &us);
    timestamp = TUPLE3(hp, make_small(Ms), make_small(s), make_small(us)); hp += 4;
    msg = TUPLE5(hp, am_profile, p->common.id, status, where, timestamp); hp += 6;
#ifndef ERTS_SMP
    profile_send(p->common.id, msg);
    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
#else
    enqueue_sys_msg_unlocked(SYS_MSG_TYPE_SYSPROF, p->common.id, NIL, msg, bp);
#endif
    erts_smp_mtx_unlock(&smq_mtx);
}
/* End system_profile tracing */



#ifdef ERTS_SMP

void
erts_check_my_tracer_proc(Process *p)
{
    if (is_internal_pid(ERTS_TRACER_PROC(p))) {
	Process *tracer = erts_pid2proc(p,
					ERTS_PROC_LOCK_MAIN,
					ERTS_TRACER_PROC(p),
					ERTS_PROC_LOCK_STATUS);
	int invalid_tracer = (!tracer
			      || !(ERTS_TRACE_FLAGS(tracer) & F_TRACER));
	if (tracer)
	    erts_smp_proc_unlock(tracer, ERTS_PROC_LOCK_STATUS);
	if (invalid_tracer) {
	    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
	    ERTS_TRACE_FLAGS(p) &= ~TRACEE_FLAGS;
	    ERTS_TRACER_PROC(p) = NIL;
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
	}
    }
}


typedef struct ErtsSysMsgQ_ ErtsSysMsgQ;
struct  ErtsSysMsgQ_ {
    ErtsSysMsgQ *next;
    enum ErtsSysMsgType type;
    Eterm from;
    Eterm to;
    Eterm msg;
    ErlHeapFragment *bp;
};

static ErtsSysMsgQ *sys_message_queue;
static ErtsSysMsgQ *sys_message_queue_end;

static erts_tid_t sys_msg_dispatcher_tid;
static erts_cnd_t smq_cnd;

ERTS_QUALLOC_IMPL(smq_element, ErtsSysMsgQ, 20, ERTS_ALC_T_SYS_MSG_Q)

static void
enqueue_sys_msg_unlocked(enum ErtsSysMsgType type,
			 Eterm from,
			 Eterm to,
			 Eterm msg,
			 ErlHeapFragment *bp)
{
    ErtsSysMsgQ *smqp;

    smqp	= smq_element_alloc();
    smqp->next	= NULL;
    smqp->type	= type;
    smqp->from	= from;
    smqp->to	= to;
    smqp->msg	= msg;
    smqp->bp	= bp;
    
    if (sys_message_queue_end) {
	ASSERT(sys_message_queue);
	sys_message_queue_end->next = smqp;
    }
    else {
	ASSERT(!sys_message_queue);
	sys_message_queue = smqp;
    }
    sys_message_queue_end = smqp;
    erts_smp_cnd_signal(&smq_cnd);
}

static void
enqueue_sys_msg(enum ErtsSysMsgType type,
		Eterm from,
		Eterm to,
		Eterm msg,
		ErlHeapFragment *bp)
{
    erts_smp_mtx_lock(&smq_mtx);
    enqueue_sys_msg_unlocked(type, from, to, msg, bp);
    erts_smp_mtx_unlock(&smq_mtx);
}

void
erts_queue_error_logger_message(Eterm from, Eterm msg, ErlHeapFragment *bp)
{
    enqueue_sys_msg(SYS_MSG_TYPE_ERRLGR, from, am_error_logger, msg, bp);
}

void
erts_send_sys_msg_proc(Eterm from, Eterm to, Eterm msg, ErlHeapFragment *bp)
{
    ASSERT(is_internal_pid(to));
    enqueue_sys_msg(SYS_MSG_TYPE_PROC_MSG, from, to, msg, bp);
}

#ifdef DEBUG_PRINTOUTS
static void
print_msg_type(ErtsSysMsgQ *smqp)
{
    switch (smqp->type) {
    case SYS_MSG_TYPE_TRACE:
	erts_fprintf(stderr, "TRACE ");
	break;
    case SYS_MSG_TYPE_SEQTRACE:
	erts_fprintf(stderr, "SEQTRACE ");
	break;
    case SYS_MSG_TYPE_SYSMON:
	erts_fprintf(stderr, "SYSMON ");
	break;
	 case SYS_MSG_TYPE_SYSPROF:
	erts_fprintf(stderr, "SYSPROF ");
	break;
    case SYS_MSG_TYPE_ERRLGR:
	erts_fprintf(stderr, "ERRLGR ");
	break;
    case SYS_MSG_TYPE_PROC_MSG:
	erts_fprintf(stderr, "PROC_MSG ");
	break;
    default:
	erts_fprintf(stderr, "??? ");
	break;
    }
}
#endif

static void
sys_msg_disp_failure(ErtsSysMsgQ *smqp, Eterm receiver)
{
    switch (smqp->type) {
    case SYS_MSG_TYPE_TRACE:
	/* Invalid tracer_proc's are removed when processes
	   are scheduled in. */
	break;
    case SYS_MSG_TYPE_SEQTRACE:
	/* Reset seq_tracer if it hasn't changed */
	erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
	if (system_seq_tracer == receiver)
	    system_seq_tracer = am_false;
	erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
	break;
    case SYS_MSG_TYPE_SYSMON:
	if (receiver == NIL
	    && !erts_system_monitor_long_gc
	    && !erts_system_monitor_long_schedule
	    && !erts_system_monitor_large_heap
	    && !erts_system_monitor_flags.busy_port
	    && !erts_system_monitor_flags.busy_dist_port)
	    break; /* Everything is disabled */
	erts_smp_thr_progress_block();
	if (system_monitor == receiver || receiver == NIL)
	    erts_system_monitor_clear(NULL);
	erts_smp_thr_progress_unblock();
	break;
	 case SYS_MSG_TYPE_SYSPROF:
	if (receiver == NIL
	    && !erts_system_profile_flags.runnable_procs
	    && !erts_system_profile_flags.runnable_ports
	    && !erts_system_profile_flags.exclusive
	    && !erts_system_profile_flags.scheduler)
		 break;
	/* Block system to clear flags */
	erts_smp_thr_progress_block();
	if (system_profile == receiver || receiver == NIL) { 
		erts_system_profile_clear(NULL);
	}
	erts_smp_thr_progress_unblock();
	break;
    case SYS_MSG_TYPE_ERRLGR: {
	char *no_elgger = "(no error logger present)";
	Eterm *tp;
	Eterm tag;
	if (is_not_tuple(smqp->msg)) {
	unexpected_elmsg:
	    erts_fprintf(stderr,
			 "%s unexpected error logger message: %T\n",
			 no_elgger,
			 smqp->msg);
	}

	tp = tuple_val(smqp->msg);
	if (arityval(tp[0]) != 2)
	    goto unexpected_elmsg;
	if (is_not_tuple(tp[2]))
	    goto unexpected_elmsg;
	tp = tuple_val(tp[2]);
	if (arityval(tp[0]) != 3)
	    goto unexpected_elmsg;
	tag = tp[1];
	if (is_not_tuple(tp[3]))
	    goto unexpected_elmsg;
	tp = tuple_val(tp[3]);
	if (arityval(tp[0]) != 3)
	    goto unexpected_elmsg;
	if (is_not_list(tp[3]))
	    goto unexpected_elmsg;
	erts_fprintf(stderr, "%s %T: %T\n",
		     no_elgger, tag, CAR(list_val(tp[3])));
	break;
    }
    case SYS_MSG_TYPE_PROC_MSG:
	break;
    default:
	ASSERT(0);
    }
}

static void
sys_msg_dispatcher_wakeup(void *vwait_p)
{
    int *wait_p = (int *) vwait_p;
    erts_smp_mtx_lock(&smq_mtx);
    *wait_p = 0;
    erts_smp_cnd_signal(&smq_cnd);
    erts_smp_mtx_unlock(&smq_mtx);
}

static void
sys_msg_dispatcher_prep_wait(void *vwait_p)
{
    int *wait_p = (int *) vwait_p;
    erts_smp_mtx_lock(&smq_mtx);
    *wait_p = 1;
    erts_smp_mtx_unlock(&smq_mtx);
}

static void
sys_msg_dispatcher_fin_wait(void *vwait_p)
{
    int *wait_p = (int *) vwait_p;
    erts_smp_mtx_lock(&smq_mtx);
    *wait_p = 0;
    erts_smp_mtx_unlock(&smq_mtx);
}

static void
sys_msg_dispatcher_wait(void *vwait_p)
{
    int *wait_p = (int *) vwait_p;
    erts_smp_mtx_lock(&smq_mtx);
    while (*wait_p)
	erts_smp_cnd_wait(&smq_cnd, &smq_mtx);
    erts_smp_mtx_unlock(&smq_mtx);
}

static void *
sys_msg_dispatcher_func(void *unused)
{
    ErtsThrPrgrCallbacks callbacks;
    ErtsSysMsgQ *local_sys_message_queue = NULL;
    int wait = 0;

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("system message dispatcher");
#endif

    callbacks.arg = (void *) &wait;
    callbacks.wakeup = sys_msg_dispatcher_wakeup;
    callbacks.prepare_wait = sys_msg_dispatcher_prep_wait;
    callbacks.wait = sys_msg_dispatcher_wait;
    callbacks.finalize_wait = sys_msg_dispatcher_fin_wait;

    erts_thr_progress_register_managed_thread(NULL, &callbacks, 0);

    while (1) {
	int end_wait = 0;
	ErtsSysMsgQ *smqp;

	ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());

	erts_smp_mtx_lock(&smq_mtx);

	/* Free previously used queue ... */
	while (local_sys_message_queue) {
	    smqp = local_sys_message_queue;
	    local_sys_message_queue = smqp->next;
	    smq_element_free(smqp);
	}

	/* Fetch current trace message queue ... */
	if (!sys_message_queue) {
	    erts_smp_mtx_unlock(&smq_mtx);
	    end_wait = 1;
	    erts_thr_progress_active(NULL, 0);
	    erts_thr_progress_prepare_wait(NULL);
	    erts_smp_mtx_lock(&smq_mtx);
	}

	while (!sys_message_queue)
	    erts_smp_cnd_wait(&smq_cnd, &smq_mtx);

	local_sys_message_queue = sys_message_queue;
	sys_message_queue = NULL;
	sys_message_queue_end = NULL;

	erts_smp_mtx_unlock(&smq_mtx);

	if (end_wait) {
	    erts_thr_progress_finalize_wait(NULL);
	    erts_thr_progress_active(NULL, 1);
	}

	/* Send trace messages ... */

	ASSERT(local_sys_message_queue);

	for (smqp = local_sys_message_queue; smqp; smqp = smqp->next) {
	    Eterm receiver;
	    ErtsProcLocks proc_locks = ERTS_PROC_LOCKS_MSG_SEND;
	    Process *proc = NULL;
	    Port *port = NULL;

	    if (erts_thr_progress_update(NULL))
		erts_thr_progress_leader_update(NULL);

#ifdef DEBUG_PRINTOUTS
	    print_msg_type(smqp);
#endif
	    switch (smqp->type) {
	    case SYS_MSG_TYPE_TRACE:
	    case SYS_MSG_TYPE_PROC_MSG:
		receiver = smqp->to;
		break;
	    case SYS_MSG_TYPE_SEQTRACE:
		receiver = erts_get_system_seq_tracer();
		break;
	    case SYS_MSG_TYPE_SYSMON:
		receiver = erts_get_system_monitor();
		if (smqp->from == receiver) {
#ifdef DEBUG_PRINTOUTS
		    erts_fprintf(stderr, "MSG=%T to %T... ",
				 smqp->msg, receiver);
#endif
		    goto drop_sys_msg;
		}
		break;
	    case SYS_MSG_TYPE_SYSPROF:
		receiver = erts_get_system_profile();
		if (smqp->from == receiver) {
#ifdef DEBUG_PRINTOUTS
		    erts_fprintf(stderr, "MSG=%T to %T... ",
				 smqp->msg, receiver);
#endif
	   	    goto drop_sys_msg;
		}
		break;
	    case SYS_MSG_TYPE_ERRLGR:
		receiver = am_error_logger;
		break;
	    default:
		receiver = NIL;
		break;
	    }

#ifdef DEBUG_PRINTOUTS
	    erts_fprintf(stderr, "MSG=%T to %T... ", smqp->msg, receiver);
#endif

	    if (is_internal_pid(receiver)) {
		proc = erts_pid2proc(NULL, 0, receiver, proc_locks);
		if (!proc
		    || (smqp->type == SYS_MSG_TYPE_TRACE
			&& !(ERTS_TRACE_FLAGS(proc) & F_TRACER))) {
		    /* Bad tracer */
#ifdef DEBUG_PRINTOUTS
		    if (smqp->type == SYS_MSG_TYPE_TRACE && proc)
			erts_fprintf(stderr,
				     "<tracer alive but missing  "
				     "F_TRACER flag> ");
#endif
		    goto failure;
		}
		else {
		queue_proc_msg:
		    erts_queue_message(proc,&proc_locks,smqp->bp,smqp->msg,NIL
#ifdef USE_VM_PROBES
				       , NIL
#endif
				       );
#ifdef DEBUG_PRINTOUTS
		    erts_fprintf(stderr, "delivered\n");
#endif
		    erts_smp_proc_unlock(proc, proc_locks);
		}
	    }
	    else if (receiver == am_error_logger) {
		proc = erts_whereis_process(NULL,0,receiver,proc_locks,0);
		if (!proc)
		    goto failure;
		else if (smqp->from == proc->common.id)
		    goto drop_sys_msg;
		else
		    goto queue_proc_msg;
	    }
	    else if (is_internal_port(receiver)) {
		port = erts_thr_id2port_sflgs(receiver,
					      ERTS_PORT_SFLGS_INVALID_TRACER_LOOKUP);
		if (!port)
		    goto failure;
		else {
		    write_sys_msg_to_port(receiver,
					  port,
					  smqp->from,
					  smqp->type,
					  smqp->msg);
		    if (port->control_flags & PORT_CONTROL_FLAG_HEAVY)
			port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
#ifdef DEBUG_PRINTOUTS
		    erts_fprintf(stderr, "delivered\n");
#endif
		    erts_thr_port_release(port);
		    if (smqp->bp)
			free_message_buffer(smqp->bp);
		}
	    }
	    else {
	    failure:
		sys_msg_disp_failure(smqp, receiver);
	    drop_sys_msg:
		if (proc)
		    erts_smp_proc_unlock(proc, proc_locks);
		if (smqp->bp)
		    free_message_buffer(smqp->bp);
#ifdef DEBUG_PRINTOUTS
		erts_fprintf(stderr, "dropped\n");
#endif
	    }
	}
    }

    return NULL;
}

void
erts_foreach_sys_msg_in_q(void (*func)(Eterm,
				       Eterm,
				       Eterm,
				       ErlHeapFragment *))
{
    ErtsSysMsgQ *sm;
    erts_smp_mtx_lock(&smq_mtx);
    for (sm = sys_message_queue; sm; sm = sm->next) {
	Eterm to;
	switch (sm->type) {
	case SYS_MSG_TYPE_TRACE:
	    to = sm->to;
	    break;
	case SYS_MSG_TYPE_SEQTRACE:
	    to = erts_get_system_seq_tracer();
	    break;
	case SYS_MSG_TYPE_SYSMON:
	    to = erts_get_system_monitor();
	    break;
	case SYS_MSG_TYPE_SYSPROF:
	    to = erts_get_system_profile();
	    break;
	case SYS_MSG_TYPE_ERRLGR:
	    to = am_error_logger;
	    break;
	default:
	    to = NIL;
	    break;
	}
	(*func)(sm->from, to, sm->msg, sm->bp);
    }
    erts_smp_mtx_unlock(&smq_mtx);
}


static void
init_sys_msg_dispatcher(void)
{
    erts_smp_thr_opts_t thr_opts = ERTS_SMP_THR_OPTS_DEFAULT_INITER;
    thr_opts.detached = 1;
    init_smq_element_alloc();
    sys_message_queue = NULL;
    sys_message_queue_end = NULL;
    erts_smp_cnd_init(&smq_cnd);
    erts_smp_mtx_init(&smq_mtx, "sys_msg_q");
    erts_smp_thr_create(&sys_msg_dispatcher_tid,
			sys_msg_dispatcher_func,
			NULL,
			&thr_opts);
}

#endif
