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
 * Support functions for tracing.
 *
 * Ideas for future speed improvements in tracing framework:
 *  * Move ErtsTracerNif into ErtsTracer
 *     + Removes need for locking
 *     + Removes hash lookup overhead
 *     + Use a refc on the ErtsTracerNif to know when it can
 *       be freed. We don't want to allocate a separate
 *       ErtsTracerNif for each module used.
 *  * Optimize GenericBp for cache locality by reusing equivalent
 *    GenericBp and GenericBpData in multiple tracer points.
 *     + Possibly we want to use specialized instructions for different
 *       types of trace so that the knowledge of which struct is used
 *       can be in the instruction.
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
#include "erl_bif_unique.h"
#include "erl_map.h"

#if 0
#define DEBUG_PRINTOUTS
#else
#undef DEBUG_PRINTOUTS
#endif

/* Pseudo export entries. Never filled in with data, only used to
   yield unique pointers of the correct type. */
Export exp_send, exp_receive, exp_timeout;

static ErtsTracer system_seq_tracer;
static Uint default_proc_trace_flags;
static ErtsTracer default_proc_tracer;
static Uint default_port_trace_flags;
static ErtsTracer default_port_tracer;

static Eterm system_monitor;
static Eterm system_profile;

#ifdef HAVE_ERTS_NOW_CPU
int erts_cpu_timestamp;
#endif

static erts_smp_mtx_t smq_mtx;
static erts_smp_rwmtx_t sys_trace_rwmtx;

enum ErtsSysMsgType {
    SYS_MSG_TYPE_UNDEFINED,
    SYS_MSG_TYPE_SYSMON,
    SYS_MSG_TYPE_ERRLGR,
    SYS_MSG_TYPE_PROC_MSG,
    SYS_MSG_TYPE_SYSPROF
};

#define ERTS_TRACE_TS_NOW_MAX_SIZE				\
    4
#define ERTS_TRACE_TS_MONOTONIC_MAX_SIZE			\
    ERTS_MAX_SINT64_HEAP_SIZE
#define ERTS_TRACE_TS_STRICT_MONOTONIC_MAX_SIZE			\
    (3 + ERTS_MAX_SINT64_HEAP_SIZE				\
     + ERTS_MAX_UINT64_HEAP_SIZE)

#define ERTS_TRACE_PATCH_TS_MAX_SIZE				\
    (1 + ((ERTS_TRACE_TS_NOW_MAX_SIZE				\
	   > ERTS_TRACE_TS_MONOTONIC_MAX_SIZE)			\
	  ? ((ERTS_TRACE_TS_NOW_MAX_SIZE			\
	      > ERTS_TRACE_TS_STRICT_MONOTONIC_MAX_SIZE)	\
	     ? ERTS_TRACE_TS_NOW_MAX_SIZE			\
	     : ERTS_TRACE_TS_STRICT_MONOTONIC_MAX_SIZE)		\
	  : ((ERTS_TRACE_TS_MONOTONIC_MAX_SIZE			\
	      > ERTS_TRACE_TS_STRICT_MONOTONIC_MAX_SIZE)	\
	     ? ERTS_TRACE_TS_MONOTONIC_MAX_SIZE			\
	     : ERTS_TRACE_TS_STRICT_MONOTONIC_MAX_SIZE)))

#define TFLGS_TS_TYPE(p) ERTS_TFLGS2TSTYPE(ERTS_TRACE_FLAGS((p)))

/*
 * FUTURE CHANGES:
 *
 * The timestamp functionality has intentionally been
 * split in two parts for future use even though it
 * is not used like this today. take_timestamp() takes
 * the timestamp and calculate heap need for it (which
 * is not constant). write_timestamp() writes the
 * timestamp to the allocated heap. That is, one typically
 * want to take the timestamp before allocating the heap
 * and then write it to the heap.
 *
 * The trace output functionality now use patch_ts_size(),
 * write_ts(), and patch_ts(). write_ts() both takes the
 * timestamp and writes it. Since we don't know the
 * heap need when allocating the heap area we need to
 * over allocate (maximum size from patch_ts_size()) and
 * then potentially (often) shrink the heap area after the
 * timestamp has been written. The only reason it is
 * currently done this way is because we do not want to
 * make major changes of the trace behavior in a patch.
 * This is planned to be changed in next major release.
 */

typedef struct {
    int ts_type_flag;
    union {
	struct {
	    Uint ms;
	    Uint s;
	    Uint us;
	} now;
	struct {
	    ErtsMonotonicTime time;
	    Sint64 raw_unique;
	} monotonic;
    } u;
} ErtsTraceTimeStamp;

static ERTS_INLINE Uint
take_timestamp(ErtsTraceTimeStamp *tsp, int ts_type)
{
    int ts_type_flag = ts_type & -ts_type; /* least significant flag */

    ASSERT(ts_type_flag == ERTS_TRACE_FLG_NOW_TIMESTAMP
	   || ts_type_flag == ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP
	   || ts_type_flag == ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP
	   || ts_type_flag == 0);

    tsp->ts_type_flag = ts_type_flag;
    switch (ts_type_flag) {
    case 0:
	return (Uint) 0;
    case ERTS_TRACE_FLG_NOW_TIMESTAMP:
#ifdef HAVE_ERTS_NOW_CPU
	if (erts_cpu_timestamp)
	    erts_get_now_cpu(&tsp->u.now.ms, &tsp->u.now.s, &tsp->u.now.us);
	else
#endif
	    get_now(&tsp->u.now.ms, &tsp->u.now.s, &tsp->u.now.us);
	return (Uint) 4;
    case ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP:
    case ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP: {
	Uint hsz = 0;
	ErtsMonotonicTime mtime = erts_get_monotonic_time(NULL);
	mtime = ERTS_MONOTONIC_TO_NSEC(mtime);
	mtime += ERTS_MONOTONIC_OFFSET_NSEC;
	hsz = (IS_SSMALL(mtime) ?
	       (Uint) 0
	       : ERTS_SINT64_HEAP_SIZE((Sint64) mtime));
	tsp->u.monotonic.time = mtime;
	if (ts_type_flag == ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP) {
	    Sint64 raw_unique;
	    hsz += 3; /* 2-tuple */
	    raw_unique = erts_raw_get_unique_monotonic_integer();
	    tsp->u.monotonic.raw_unique = raw_unique;
	    hsz += erts_raw_unique_monotonic_integer_heap_size(raw_unique, 0);
	}
	return hsz;
    }
    default:
	ERTS_INTERNAL_ERROR("invalid timestamp type");
	return 0;
    }
}

static ERTS_INLINE Eterm
write_timestamp(ErtsTraceTimeStamp *tsp, Eterm **hpp)
{
    int ts_type_flag = tsp->ts_type_flag;
    Eterm res;

    switch (ts_type_flag) {
    case 0:
	return NIL;
    case ERTS_TRACE_FLG_NOW_TIMESTAMP:
	res = TUPLE3(*hpp,
		     make_small(tsp->u.now.ms),
		     make_small(tsp->u.now.s),
		     make_small(tsp->u.now.us));
	*hpp += 4;
	return res;
    case ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP:
    case ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP: {
	Sint64 mtime, raw;
	Eterm unique, emtime;

	mtime = (Sint64) tsp->u.monotonic.time;
	emtime = (IS_SSMALL(mtime)
		  ? make_small((Sint64) mtime)
		  : erts_sint64_to_big((Sint64) mtime, hpp));

	if (ts_type_flag == ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP)
	    return emtime;

	raw = tsp->u.monotonic.raw_unique;
	unique = erts_raw_make_unique_monotonic_integer_value(hpp, raw, 0);
	res = TUPLE2(*hpp, emtime, unique);
	*hpp += 3;
	return res;
    }
    default:
	ERTS_INTERNAL_ERROR("invalid timestamp type");
	return THE_NON_VALUE;
    }
}

#ifdef ERTS_SMP

static ERTS_INLINE Uint
patch_ts_size(int ts_type)
{
    int ts_type_flag = ts_type & -ts_type; /* least significant flag */
    switch (ts_type_flag) {
    case 0:
	return 0;
    case ERTS_TRACE_FLG_NOW_TIMESTAMP:
	return 1 + ERTS_TRACE_TS_NOW_MAX_SIZE;
    case ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP:
	return 1 + ERTS_TRACE_TS_MONOTONIC_MAX_SIZE;
    case ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP:
	return 1 + ERTS_TRACE_TS_STRICT_MONOTONIC_MAX_SIZE;
    default:
	ERTS_INTERNAL_ERROR("invalid timestamp type");
	return 0;
    }
}
#endif /* ERTS_SMP */

/*
 * Write a timestamp. The timestamp MUST be the last
 * thing built on the heap. This since write_ts() might
 * adjust the size of the used area.
 */
static Eterm
write_ts(int ts_type, Eterm *hp, ErlHeapFragment *bp, Process *tracer)
{
    ErtsTraceTimeStamp ts;
    Sint shrink;
    Eterm res, *ts_hp = hp;
    Uint hsz;

    ASSERT(ts_type);

    hsz = take_timestamp(&ts, ts_type);

    res = write_timestamp(&ts, &ts_hp);

    ASSERT(ts_hp == hp + hsz);

    switch (ts.ts_type_flag) {
    case ERTS_TRACE_FLG_MONOTONIC_TIMESTAMP:
	shrink = ERTS_TRACE_TS_MONOTONIC_MAX_SIZE;
	break;
    case ERTS_TRACE_FLG_STRICT_MONOTONIC_TIMESTAMP:
	shrink = ERTS_TRACE_TS_STRICT_MONOTONIC_MAX_SIZE;
	break;
    default:
	return res;
    }

    shrink -= hsz;

    ASSERT(shrink >= 0);

    if (shrink) {
	if (bp)
	    bp->used_size -= shrink;
#ifndef ERTS_SMP
	else if (tracer) {
	    Eterm *endp = ts_hp + shrink;
	    HRelease(tracer, endp, ts_hp);
	}
#endif
    }

    return res;
}

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

static void init_tracer_nif(void);
static int tracer_cmp_fun(void*, void*);
static HashValue tracer_hash_fun(void*);
static void *tracer_alloc_fun(void*);
static void tracer_free_fun(void*);

typedef struct ErtsTracerNif_ ErtsTracerNif;

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
    default_proc_trace_flags = F_INITIAL_TRACE_FLAGS;
    default_proc_tracer = erts_tracer_nil;
    default_port_trace_flags = F_INITIAL_TRACE_FLAGS;
    default_port_tracer = erts_tracer_nil;
    system_seq_tracer = erts_tracer_nil;
#ifdef ERTS_SMP
    init_sys_msg_dispatcher();
#endif
    init_tracer_nif();
}

#define ERTS_ALLOC_SYSMSG_HEAP(SZ, BPP, OHPP, UNUSED) \
  (*(BPP) = new_message_buffer((SZ)), \
   *(OHPP) = &(*(BPP))->off_heap, \
   (*(BPP))->mem)

enum ErtsTracerOpt {
    TRACE_FUN_DEFAULT   = 0,
    TRACE_FUN_ENABLED   = 1,
    TRACE_FUN_T_SEND    = 2,
    TRACE_FUN_T_RECEIVE = 3,
    TRACE_FUN_T_CALL    = 4,
    TRACE_FUN_T_SCHED_PROC = 5,
    TRACE_FUN_T_SCHED_PORT = 6,
    TRACE_FUN_T_GC      = 7,
    TRACE_FUN_T_PROCS   = 8,
    TRACE_FUN_T_PORTS   = 9,
    TRACE_FUN_E_SEND    = 10,
    TRACE_FUN_E_RECEIVE = 11,
    TRACE_FUN_E_CALL    = 12,
    TRACE_FUN_E_SCHED_PROC = 13,
    TRACE_FUN_E_SCHED_PORT = 14,
    TRACE_FUN_E_GC      = 15,
    TRACE_FUN_E_PROCS   = 16,
    TRACE_FUN_E_PORTS   = 17
};

#define NIF_TRACER_TYPES (18)


static ERTS_INLINE int
send_to_tracer_nif_raw(Process *c_p, Process *tracee, const ErtsTracer tracer,
                       Uint trace_flags, Eterm t_p_id, ErtsTracerNif *tnif,
                       enum ErtsTracerOpt topt,
                       Eterm tag, Eterm msg, Eterm extra, Eterm pam_result);
static ERTS_INLINE int
send_to_tracer_nif(Process *c_p, ErtsPTabElementCommon *t_p,
                   Eterm t_p_id, ErtsTracerNif *tnif,
                   enum ErtsTracerOpt topt,
                   Eterm tag, Eterm msg, Eterm extra,
                   Eterm pam_result);
static ERTS_INLINE Eterm
call_enabled_tracer(const ErtsTracer tracer,
                    ErtsTracerNif **tnif_ref,
                    enum ErtsTracerOpt topt,
                    Eterm tag, Eterm t_p_id);
static int
is_tracer_enabled(Process* c_p, ErtsProcLocks c_p_locks,
                  ErtsPTabElementCommon *t_p,
                  ErtsTracerNif **tnif_ret,
                  enum ErtsTracerOpt topt, Eterm tag);

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
    if (exiting == system_monitor)
	reset = 1;
    else if (exiting == system_profile)
	reset = 1;
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
    if (reset)
	exiting_reset(exiting);
}

ErtsTracer
erts_set_system_seq_tracer(Process *c_p, ErtsProcLocks c_p_locks, ErtsTracer new)
{
    ErtsTracer old;

    if (!ERTS_TRACER_IS_NIL(new)) {
        Eterm nif_result = call_enabled_tracer(
            new, NULL, TRACE_FUN_ENABLED, am_trace_status, am_undefined);
        switch (nif_result) {
        case am_trace: break;
        default:
            return THE_NON_VALUE;
        }
    }

    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    old = system_seq_tracer;
    system_seq_tracer = erts_tracer_nil;
    erts_tracer_update(&system_seq_tracer, new);

#ifdef DEBUG_PRINTOUTS
    erts_fprintf(stderr, "set seq tracer new=%T old=%T\n", new, old);
#endif
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
    return old;
}

ErtsTracer
erts_get_system_seq_tracer(void)
{
    ErtsTracer st;
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    st = system_seq_tracer;
#ifdef DEBUG_PRINTOUTS
    erts_fprintf(stderr, "get seq tracer %T\n", st);
#endif
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);

    if (st != erts_tracer_nil &&
        call_enabled_tracer(st, NULL, TRACE_FUN_ENABLED,
                            am_trace_status, am_undefined) == am_remove) {
        st = erts_set_system_seq_tracer(NULL, 0, erts_tracer_nil);
        ERTS_TRACER_CLEAR(&st);
    }

    return st;
}

static ERTS_INLINE void
get_default_tracing(Uint *flagsp, ErtsTracer *tracerp,
                    Uint *default_trace_flags,
                    ErtsTracer *default_tracer)
{
    if (!(*default_trace_flags & TRACEE_FLAGS))
	ERTS_TRACER_CLEAR(default_tracer);

    if (ERTS_TRACER_IS_NIL(*default_tracer)) {
	*default_trace_flags &= ~TRACEE_FLAGS;
    } else {
        Eterm nif_res;
        nif_res = call_enabled_tracer(*default_tracer,
                                      NULL, TRACE_FUN_ENABLED,
                                      am_trace_status, am_undefined);
        switch (nif_res) {
        case am_trace: break;
        default: {
            ErtsTracer curr_default_tracer = *default_tracer;
            if (tracerp) {
                /* we only have a rlock, so we have to unlock and then rwlock */
                erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
                erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
            }
            /* check if someone else changed default tracer
               while we got the write lock, if so we don't do
               anything. */
            if (curr_default_tracer == *default_tracer) {
                *default_trace_flags &= ~TRACEE_FLAGS;
                ERTS_TRACER_CLEAR(default_tracer);
            }
            if (tracerp) {
                erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
                erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
            }
        }
        }
    }

    if (flagsp)
	*flagsp = *default_trace_flags;
    if (tracerp) {
	erts_tracer_update(tracerp,*default_tracer);
    }
}

static ERTS_INLINE void
erts_change_default_tracing(int setflags, Uint flags,
                            const ErtsTracer tracer,
                            Uint *default_trace_flags,
                            ErtsTracer *default_tracer)
{
    if (setflags)
        *default_trace_flags |= flags;
    else
        *default_trace_flags &= ~flags;

    erts_tracer_update(default_tracer, tracer);

    get_default_tracing(NULL, NULL, default_trace_flags, default_tracer);
}

void
erts_change_default_proc_tracing(int setflags, Uint flagsp,
                                 const ErtsTracer tracer)
{
    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    erts_change_default_tracing(
        setflags, flagsp, tracer,
        &default_proc_trace_flags,
        &default_proc_tracer);
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
}

void
erts_change_default_port_tracing(int setflags, Uint flagsp,
                                 const ErtsTracer tracer)
{
    erts_smp_rwmtx_rwlock(&sys_trace_rwmtx);
    erts_change_default_tracing(
        setflags, flagsp, tracer,
        &default_port_trace_flags,
        &default_port_tracer);
    erts_smp_rwmtx_rwunlock(&sys_trace_rwmtx);
}

void
erts_get_default_proc_tracing(Uint *flagsp, ErtsTracer *tracerp)
{
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    *tracerp = erts_tracer_nil; /* initialize */
    get_default_tracing(
        flagsp, tracerp,
        &default_proc_trace_flags,
        &default_proc_tracer);
    erts_smp_rwmtx_runlock(&sys_trace_rwmtx);
}

void
erts_get_default_port_tracing(Uint *flagsp, ErtsTracer *tracerp)
{
    erts_smp_rwmtx_rlock(&sys_trace_rwmtx);
    *tracerp = erts_tracer_nil; /* initialize */
    get_default_tracing(
        flagsp, tracerp,
        &default_port_trace_flags,
        &default_port_tracer);
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


static void
write_sys_msg_to_port(Eterm unused_to,
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
	erts_exit(ERTS_ERROR_EXIT, "Internal error in do_send_to_port: %d\n", ptr-buffer);
    }

#ifndef ERTS_SMP
    if (!INVALID_TRACER_PORT(trace_port, trace_port->common.id))
#endif
	erts_raw_port_command(trace_port, buffer, ptr-buffer);

    erts_free(ERTS_ALC_T_TMP, (void *) buffer);
}

#ifndef ERTS_SMP
/* Profile send
 * Checks if profiler is port or process
 * Eterm msg is local, need copying.
 */

static void 
profile_send(Eterm from, Eterm message) {
    Uint sz = 0;
    Uint *hp = NULL;
    Eterm msg = NIL;
    Process *profile_p = NULL;

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
	    write_sys_msg_to_port(profiler,
                                  profiler_port,
                                  NIL, /* or current process->common.id */
                                  SYS_MSG_TYPE_SYSPROF,
                                  message);
	    erts_port_release(profiler_port);
	}
    	
    } else {
	ErtsMessage *mp;
	ASSERT(is_internal_pid(profiler));
        
	profile_p = erts_proc_lookup(profiler);

	if (!profile_p)
	    return;

	sz = size_object(message);
	mp = erts_alloc_message(sz, &hp);
	if (sz == 0)
	    msg = message;
	else
	    msg = copy_struct(message, sz, &hp, &mp->hfrag.off_heap);

        erts_queue_message(profile_p, 0, mp, msg, from);
    }
}

#endif

static void
trace_sched_aux(Process *p, ErtsProcLocks locks, Eterm what)
{
    Eterm tmp, *hp;
    int curr_func;
    ErtsTracerNif *tnif = NULL;

    if (ERTS_TRACER_IS_NIL(ERTS_TRACER(p)))
	return;

    switch (what) {
    case am_out:
    case am_out_exiting:
    case am_out_exited:
    case am_in:
    case am_in_exiting:
	break;
    default:
	ASSERT(0);
	break;
    }

    if (!is_tracer_enabled(p, locks, &p->common, &tnif, TRACE_FUN_E_SCHED_PROC, what))
        return;

    if (ERTS_PROC_IS_EXITING(p))
	curr_func = 0;
    else {
	if (!p->current)
	    p->current = find_function_from_pc(p->i);
	curr_func = p->current != NULL;
    }

    if (!curr_func) {
	tmp = make_small(0);
    } else {
        hp = HAlloc(p, 4);
	tmp = TUPLE3(hp,p->current->module,p->current->function,
                     make_small(p->current->arity));
	hp += 4;
    }

    send_to_tracer_nif(p, &p->common, p->common.id, tnif, TRACE_FUN_T_SCHED_PROC,
                       what, tmp, THE_NON_VALUE, am_true);
}

/* Send {trace_ts, Pid, What, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, What, {Mod, Func, Arity}}
 *
 * where 'What' is supposed to be 'in', 'out', 'in_exiting',
 * 'out_exiting', or 'out_exited'.
 */
void
trace_sched(Process *p, ErtsProcLocks locks, Eterm what)
{
    trace_sched_aux(p, locks, what);
}

/* Send {trace_ts, Pid, Send, Msg, DestPid, Timestamp}
 * or   {trace, Pid, Send, Msg, DestPid}
 *
 * where 'Send' is 'send' or 'send_to_non_existing_process'.
 */
void
trace_send(Process *p, Eterm to, Eterm msg)
{
    Eterm operation = am_send;
    ErtsTracerNif *tnif = NULL;
    ErtsTracingEvent* te;
    Eterm pam_result;
#ifdef ERTS_SMP
    ErtsThrPrgrDelayHandle dhndl;
#endif

    ASSERT(ARE_TRACE_FLAGS_ON(p, F_TRACE_SEND));

    te = &erts_send_tracing[erts_active_bp_ix()];
    if (!te->on) {
	return;
    }
    if (te->match_spec) {
	Eterm args[2];
	Uint32 return_flags;
	args[0] = to;
	args[1] = msg;
	pam_result = erts_match_set_run_trace(p, p,
                                              te->match_spec, args, 2,
                                              ERTS_PAM_TMP_RESULT, &return_flags);
	if (pam_result == am_false)
            return;
        if (ERTS_TRACE_FLAGS(p) & F_TRACE_SILENT) {
            erts_match_set_release_result_trace(p, pam_result);
	    return;
	}
    } else
        pam_result = am_true;

#ifdef ERTS_SMP
    dhndl = erts_thr_progress_unmanaged_delay();
#endif

    if (is_internal_pid(to)) {
	if (!erts_proc_lookup(to))
	    goto send_to_non_existing_process;
    }
    else if(is_external_pid(to)
	    && external_pid_dist_entry(to) == erts_this_dist_entry) {
    send_to_non_existing_process:
	operation = am_send_to_non_existing_process;
    }

    if (is_tracer_enabled(p, ERTS_PROC_LOCK_MAIN, &p->common, &tnif,
                          TRACE_FUN_E_SEND, operation)) {
        send_to_tracer_nif(p, &p->common, p->common.id, tnif, TRACE_FUN_T_SEND,
                           operation, msg, to, pam_result);
    }

#ifdef ERTS_SMP
    erts_thr_progress_unmanaged_continue(dhndl);
#endif

    erts_match_set_release_result_trace(p, pam_result);
}

/* Send {trace_ts, Pid, receive, Msg, Timestamp}
 * or   {trace, Pid, receive, Msg}
 */
void
trace_receive(Process* receiver,
              Eterm from,
              Eterm msg, ErtsTracingEvent* te)
{
    ErtsTracerNif *tnif = NULL;
    Eterm pam_result;

    if (!te) {
        te = &erts_receive_tracing[erts_active_bp_ix()];
        if (!te->on)
            return;
    }
    else ASSERT(te->on);

    if (te->match_spec) {
        Eterm args[3];
        Uint32 return_flags;
        if (is_pid(from)) {
            args[0] = pid_node_name(from);
            args[1] = from;
        }
        else {
            ASSERT(is_atom(from));
            args[0] = from;  /* node name or other atom (e.g 'system') */
            args[1] = am_undefined;
        }
        args[2] = msg;
        pam_result = erts_match_set_run_trace(NULL, receiver,
                                              te->match_spec, args, 3,
                                              ERTS_PAM_TMP_RESULT, &return_flags);
        if (pam_result == am_false)
            return;
        if (ERTS_TRACE_FLAGS(receiver) & F_TRACE_SILENT) {
            erts_match_set_release_result_trace(NULL, pam_result);
            return;
        }
    } else
        pam_result = am_true;

    if (is_tracer_enabled(NULL, 0, &receiver->common, &tnif,
                          TRACE_FUN_E_RECEIVE, am_receive)) {
        send_to_tracer_nif(NULL, &receiver->common, receiver->common.id,
                           tnif, TRACE_FUN_T_RECEIVE,
                           am_receive, msg, THE_NON_VALUE, pam_result);
    }
    erts_match_set_release_result_trace(NULL, pam_result);
}

int
seq_trace_update_send(Process *p)
{
    ErtsTracer seq_tracer = erts_get_system_seq_tracer();
    ASSERT((is_tuple(SEQ_TRACE_TOKEN(p)) || is_nil(SEQ_TRACE_TOKEN(p))));
    if (have_no_seqtrace(SEQ_TRACE_TOKEN(p)) ||
        (seq_tracer != NIL &&
         call_enabled_tracer(seq_tracer, NULL,
                             TRACE_FUN_ENABLED, am_seq_trace,
                             p ? p->common.id : am_undefined) != am_trace)
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
    Eterm* hp;
    Eterm label;
    Eterm lastcnt_serial;
    Eterm type_atom;
    ErtsTracer seq_tracer;
    int seq_tracer_flags = 0;
#define LOCAL_HEAP_SIZE (64)
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);

    seq_tracer = erts_get_system_seq_tracer();

    ASSERT(is_tuple(token) || is_nil(token));
    if (token == NIL || (process && ERTS_TRACE_FLAGS(process) & F_SENSITIVE) ||
        ERTS_TRACER_IS_NIL(seq_tracer) ||
        call_enabled_tracer(seq_tracer,
                            NULL, TRACE_FUN_ENABLED,
                            am_seq_trace,
                            process ? process->common.id : am_undefined) != am_trace) {
	return;
    }

    if ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & type) == 0) {
	/* No flags set, nothing to do */
	return;
    }

    switch (type) {
    case SEQ_TRACE_SEND:    type_atom = am_send; break;
    case SEQ_TRACE_PRINT:   type_atom = am_print; break;
    case SEQ_TRACE_RECEIVE: type_atom = am_receive; break;
    default:
	erts_exit(ERTS_ERROR_EXIT, "invalid type in seq_trace_output_generic: %d:\n", type);
	return;			/* To avoid warning */
    }

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
    mess = TUPLE5(hp, type_atom, lastcnt_serial, SEQ_TRACE_T_SENDER(token), receiver, msg);
    hp += 6;

    seq_tracer_flags |=  ERTS_SEQTFLGS2TFLGS(unsigned_val(SEQ_TRACE_T_FLAGS(token)));

    send_to_tracer_nif_raw(NULL, process, seq_tracer, seq_tracer_flags,
                           label, NULL, TRACE_FUN_DEFAULT, am_seq_trace, mess,
                           THE_NON_VALUE, am_true);

    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
}

/* Send {trace_ts, Pid, return_to, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, return_to, {Mod, Func, Arity}}
 */
void 
erts_trace_return_to(Process *p, BeamInstr *pc)
{
    Eterm mfa;

    ErtsCodeMFA *cmfa = find_function_from_pc(pc);

    if (!cmfa) {
	mfa = am_undefined;
    } else {
        Eterm *hp = HAlloc(p, 4);
	mfa = TUPLE3(hp, cmfa->module, cmfa->function,
                     make_small(cmfa->arity));
    }

    send_to_tracer_nif(p, &p->common, p->common.id, NULL, TRACE_FUN_T_CALL,
                       am_return_to, mfa, THE_NON_VALUE, am_true);
}


/* Send {trace_ts, Pid, return_from, {Mod, Name, Arity}, Retval, Timestamp}
 * or   {trace, Pid, return_from, {Mod, Name, Arity}, Retval}
 */
void
erts_trace_return(Process* p, ErtsCodeMFA *mfa,
                  Eterm retval, ErtsTracer *tracer)
{
    Eterm* hp;
    Eterm mfa_tuple;
    Uint meta_flags, *tracee_flags;

    ASSERT(tracer);
    if (ERTS_TRACER_COMPARE(*tracer, erts_tracer_true)) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer = &ERTS_TRACER(p);
    }
    if (ERTS_TRACER_IS_NIL(*tracer)) {
	/* Trace disabled */
	return;
    }
    ASSERT(IS_TRACER_VALID(*tracer));
    if (tracer == &ERTS_TRACER(p)) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &ERTS_TRACE_FLAGS(p);
        if (! (*tracee_flags & F_TRACE_CALLS)) {
            return;
        }
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */
	meta_flags = F_TRACE_CALLS | F_NOW_TS;
	tracee_flags = &meta_flags;
    }

    hp = HAlloc(p, 4);
    mfa_tuple = TUPLE3(hp, mfa->module, mfa->function,
                       make_small(mfa->arity));
    hp += 4;
    send_to_tracer_nif_raw(p, NULL, *tracer, *tracee_flags, p->common.id,
                           NULL, TRACE_FUN_T_CALL, am_return_from, mfa_tuple,
                           retval, am_true);
}

/* Send {trace_ts, Pid, exception_from, {Mod, Name, Arity}, {Class,Value}, 
 *       Timestamp}
 * or   {trace, Pid, exception_from, {Mod, Name, Arity}, {Class,Value}, 
 *       Timestamp}
 *
 * Where Class is atomic but Value is any term.
 */
void
erts_trace_exception(Process* p, ErtsCodeMFA *mfa, Eterm class, Eterm value,
		     ErtsTracer *tracer)
{
    Eterm* hp;
    Eterm mfa_tuple, cv;
    Uint meta_flags, *tracee_flags;

    ASSERT(tracer);
    if (ERTS_TRACER_COMPARE(*tracer, erts_tracer_true)) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer = &ERTS_TRACER(p);
    }
    if (ERTS_TRACER_IS_NIL(*tracer)) {
	/* Trace disabled */
	return;
    }
    ASSERT(IS_TRACER_VALID(*tracer));
    if (tracer == &ERTS_TRACER(p)) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &ERTS_TRACE_FLAGS(p);
        if (! (*tracee_flags & F_TRACE_CALLS)) {
            return;
        }
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */
	meta_flags = F_TRACE_CALLS | F_NOW_TS;
	tracee_flags = &meta_flags;
    }

    hp = HAlloc(p, 7);;
    mfa_tuple = TUPLE3(hp, mfa->module, mfa->function, make_small(mfa->arity));
    hp += 4;
    cv = TUPLE2(hp, class, value);
    hp += 3;
    send_to_tracer_nif_raw(p, NULL, *tracer, *tracee_flags, p->common.id,
                           NULL, TRACE_FUN_T_CALL, am_exception_from, mfa_tuple, cv, am_true);
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
erts_call_trace(Process* p, ErtsCodeInfo *info, Binary *match_spec,
		Eterm* args, int local, ErtsTracer *tracer)
{
    Eterm* hp;
    Eterm mfa_tuple;
    int arity;
    int i;
    Uint32 return_flags;
    Eterm pam_result = am_true;
    Uint meta_flags, *tracee_flags;
    ErtsTracerNif *tnif = NULL;
    Eterm transformed_args[MAX_ARG];
    ErtsTracer pre_ms_tracer = erts_tracer_nil;

    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p) & ERTS_PROC_LOCK_MAIN);

    ASSERT(tracer);
    if (ERTS_TRACER_COMPARE(*tracer, erts_tracer_true)) {
        /* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
        tracer = &ERTS_TRACER(p);
    }
    if (ERTS_TRACER_IS_NIL(*tracer)) {
	/* Trace disabled */
	return 0;
    }
    ASSERT(IS_TRACER_VALID(*tracer));
    if (tracer == &ERTS_TRACER(p)) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &ERTS_TRACE_FLAGS(p);
        /* Is is not ideal at all to call this check twice,
           it should be optimized so that only one call is made. */
        if (!is_tracer_enabled(p, ERTS_PROC_LOCK_MAIN, &p->common, &tnif,
                               TRACE_FUN_ENABLED, am_trace_status)
            || !is_tracer_enabled(p, ERTS_PROC_LOCK_MAIN, &p->common, &tnif,
                    TRACE_FUN_E_CALL, am_call)) {
            return 0;
        }
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
	meta_flags = F_TRACE_CALLS | F_NOW_TS;
	tracee_flags = &meta_flags;
        switch (call_enabled_tracer(*tracer,
                                    &tnif, TRACE_FUN_ENABLED,
                                    am_trace_status, p->common.id)) {
        default:
        case am_remove: *tracer = erts_tracer_nil;
        case am_discard: return 0;
        case am_trace:
            switch (call_enabled_tracer(*tracer,
                                        &tnif, TRACE_FUN_T_CALL,
                                        am_call, p->common.id)) {
            default:
            case am_discard: return 0;
            case am_trace: break;
            }
            break;
        }
    }

    /*
     * Because of the delayed sub-binary creation optimization introduced in
     * R12B, (at most) one of arguments can be a match context instead of
     * a binary. Since we don't want to handle match contexts in utility functions
     * such as size_object() and copy_struct(), we must make sure that we
     * temporarily convert any match contexts to sub binaries.
     */
    arity = info->mfa.arity;
    for (i = 0; i < arity; i++) {
	Eterm arg = args[i];
	if (is_boxed(arg) && header_is_bin_matchstate(*boxed_val(arg))) {
	    ErlBinMatchState* ms = (ErlBinMatchState *) boxed_val(arg);
	    ErlBinMatchBuffer* mb = &ms->mb;
	    Uint bit_size;
            ErlSubBin *sub_bin_heap = (ErlSubBin *)HAlloc(p, ERL_SUB_BIN_SIZE);

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

    return_flags = 0;
    if (match_spec) {
        /* we have to make a copy of the tracer here as the match spec
           may remove it, and we still want to generate a trace message */
        erts_tracer_update(&pre_ms_tracer, *tracer);
        tracer = &pre_ms_tracer;
        pam_result = erts_match_set_run_trace(p, p,
                                              match_spec, args, arity,
                                              ERTS_PAM_TMP_RESULT, &return_flags);
    }

    if (tracee_flags == &meta_flags) {
        /* Meta trace */
        if (pam_result == am_false) {
            UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
            ERTS_TRACER_CLEAR(&pre_ms_tracer);
            return return_flags;
        }
    } else {
        /* Non-meta trace */
        if (*tracee_flags & F_TRACE_SILENT) {
            erts_match_set_release_result_trace(p, pam_result);
            UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
            ERTS_TRACER_CLEAR(&pre_ms_tracer);
            return 0;
        }
        if (pam_result == am_false) {
            UnUseTmpHeap(ERL_SUB_BIN_SIZE,p);
            ERTS_TRACER_CLEAR(&pre_ms_tracer);
            return return_flags;
        }
        if (local && (*tracee_flags & F_TRACE_RETURN_TO)) {
            return_flags |= MATCH_SET_RETURN_TO_TRACE;
        }
    }

    ASSERT(!ERTS_TRACER_IS_NIL(*tracer));

    /*
     * Build the the {M,F,A} tuple in the local heap.
     * (A is arguments or arity.)
     */


    if (*tracee_flags & F_TRACE_ARITY_ONLY) {
        hp = HAlloc(p, 4);
        mfa_tuple = make_small(arity);
    } else {
        hp = HAlloc(p, 4 + arity * 2);
        mfa_tuple = NIL;
        for (i = arity-1; i >= 0; i--) {
            mfa_tuple = CONS(hp, args[i], mfa_tuple);
            hp += 2;
        }
    }
    mfa_tuple = TUPLE3(hp, info->mfa.module, info->mfa.function, mfa_tuple);
    hp += 4;

    /*
     * Build the trace tuple and send it to the port.
     */
    send_to_tracer_nif_raw(p, NULL, *tracer, *tracee_flags, p->common.id,
                           tnif, TRACE_FUN_T_CALL, am_call, mfa_tuple,
                           THE_NON_VALUE, pam_result);

    if (match_spec) {
        erts_match_set_release_result_trace(p, pam_result);
        if (tracer == &pre_ms_tracer)
            ERTS_TRACER_CLEAR(&pre_ms_tracer);
    }

    return return_flags;
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
trace_proc(Process *c_p, ErtsProcLocks c_p_locks,
           Process *t_p, Eterm what, Eterm data)
{
    ErtsTracerNif *tnif = NULL;
    if (is_tracer_enabled(NULL, 0, &t_p->common, &tnif,
                TRACE_FUN_E_PROCS, what))
        send_to_tracer_nif(NULL, &t_p->common, t_p->common.id, tnif, TRACE_FUN_T_PROCS,
                           what, data, THE_NON_VALUE, am_true);
}


/* Sends trace message:
 *    {trace_ts, ParentPid, spawn, ChildPid, {Mod, Func, Args}, Timestamp}
 * or {trace, ParentPid, spawn, ChildPid, {Mod, Func, Args}}
 *
 * 'pid' is the ChildPid, 'mod' and 'func' must be atomic,
 * and 'args' may be a deep term.
 */
void
trace_proc_spawn(Process *p, Eterm what, Eterm pid,
		 Eterm mod, Eterm func, Eterm args)
{
    ErtsTracerNif *tnif = NULL;
    if (is_tracer_enabled(NULL, 0,
			  &p->common, &tnif, TRACE_FUN_E_PROCS, what)) {
        Eterm mfa;
        Eterm* hp;

        hp = HAlloc(p, 4);
        mfa = TUPLE3(hp, mod, func, args);
        hp += 4;
        send_to_tracer_nif(NULL, &p->common, p->common.id, tnif, TRACE_FUN_T_PROCS,
                           what, pid, mfa, am_true);
    }
}

void save_calls(Process *p, Export *e)
{
    if ((ERTS_TRACE_FLAGS(p) & F_SENSITIVE) == 0) {
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
trace_gc(Process *p, Eterm what, Uint size, Eterm msg)
{
    ErtsTracerNif *tnif = NULL;
    Eterm* o_hp = NULL;
    Eterm* hp;
    Uint sz = 0;
    Eterm tup;

    if (is_tracer_enabled(p, ERTS_PROC_LOCK_MAIN, &p->common, &tnif,
                          TRACE_FUN_E_GC, what)) {

        if (is_non_value(msg)) {

            (void) erts_process_gc_info(p, &sz, NULL, 0, 0);
            o_hp = hp = erts_alloc(ERTS_ALC_T_TMP, (sz + 3 + 2) * sizeof(Eterm));

            msg = erts_process_gc_info(p, NULL, &hp, 0, 0);
            tup = TUPLE2(hp, am_wordsize, make_small(size)); hp += 3;
            msg = CONS(hp, tup, msg); hp += 2;
        }

        send_to_tracer_nif(p, &p->common, p->common.id, tnif, TRACE_FUN_T_GC,
                           what, msg, THE_NON_VALUE, am_true);
        if (o_hp)
            erts_free(ERTS_ALC_T_TMP, o_hp);
    }
}

void 
monitor_long_schedule_proc(Process *p, ErtsCodeMFA *in_fp,
                           ErtsCodeMFA *out_fp, Uint time)
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
	in_mfa = TUPLE3(hp, in_fp->module, in_fp->function,
                        make_small(in_fp->arity));
	hp +=4;
    } 
    if (out_fp != NULL) {
	out_mfa = TUPLE3(hp, out_fp->module, out_fp->function,
                         make_small(out_fp->arity));
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
    {
	ErtsMessage *mp = erts_alloc_message(0, NULL);
	mp->data.heap_frag = bp;
	erts_queue_message(monitor_p, 0, mp, msg, am_system);
    }
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
    {
	ErtsMessage *mp = erts_alloc_message(0, NULL);
	mp->data.heap_frag = bp;
	erts_queue_message(monitor_p, 0, mp, msg, am_system);
    }
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
    {
	ErtsMessage *mp = erts_alloc_message(0, NULL);
	mp->data.heap_frag = bp;
	erts_queue_message(monitor_p, 0, mp, msg, am_system);
    }
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
    if (!monitor_p || p == monitor_p) {
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
    {
	ErtsMessage *mp = erts_alloc_message(0, NULL);
	mp->data.heap_frag = bp;
	erts_queue_message(monitor_p, 0, mp, msg, am_system);
    }
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
    {
	ErtsMessage *mp = erts_alloc_message(0, NULL);
	mp->data.heap_frag = bp;
	erts_queue_message(monitor_p, 0, mp, msg, am_system);
    }
#endif

}


/* Begin system_profile tracing */
/* Scheduler profiling */

void
profile_scheduler(Eterm scheduler_id, Eterm state) {
    Eterm *hp, msg;
    ErlHeapFragment *bp = NULL;

#ifndef ERTS_SMP
#define LOCAL_HEAP_SIZE (7 + ERTS_TRACE_PATCH_TS_MAX_SIZE)
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);
    hp = local_heap;
#else
    Uint hsz;

    hsz = 7 + patch_ts_size(erts_system_profile_ts_type)-1;
	
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

    msg = TUPLE6(hp, am_profile, am_scheduler, scheduler_id,
		 state, make_small(active_sched),
		 NIL /* Will be overwritten by timestamp */);
    hp += 7;

    /* Write timestamp in element 6 of the 'msg' tuple */
    hp[-1] = write_ts(erts_system_profile_ts_type, hp, bp, NULL);

#ifndef ERTS_SMP
    profile_send(NIL, msg);
    UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
#else
    enqueue_sys_msg_unlocked(SYS_MSG_TYPE_SYSPROF, NIL, NIL, msg, bp);
#endif
    erts_smp_mtx_unlock(&smq_mtx);

}

/* Port profiling */

void
trace_port_open(Port *p, Eterm calling_pid, Eterm drv_name) {
    ErtsTracerNif *tnif = NULL;
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (is_tracer_enabled(NULL, 0, &p->common, &tnif, TRACE_FUN_E_PORTS, am_open))
        send_to_tracer_nif(NULL, &p->common, p->common.id, tnif, TRACE_FUN_T_PORTS,
                am_open, calling_pid, drv_name, am_true);
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

    ErtsTracerNif *tnif = NULL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(t_p)
		       || erts_thr_progress_is_blocking());
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (is_tracer_enabled(NULL, 0, &t_p->common, &tnif, TRACE_FUN_E_PORTS, what))
        send_to_tracer_nif(NULL, &t_p->common, t_p->common.id, tnif, TRACE_FUN_T_PORTS,
                           what, data, THE_NON_VALUE, am_true);
}


static Eterm
trace_port_tmp_binary(char *bin, Sint sz, Binary **bptrp, Eterm **hp)
{
    if (sz <= ERL_ONHEAP_BIN_LIMIT) {
        ErlHeapBin *hb = (ErlHeapBin *)*hp;
        hb->thing_word = header_heap_bin(sz);
        hb->size = sz;
        sys_memcpy(hb->data, bin, sz);
        *hp += heap_bin_size(sz);
        return make_binary(hb);
    } else {
        ProcBin* pb = (ProcBin *)*hp;
        Binary *bptr = erts_bin_nrml_alloc(sz);
        sys_memcpy(bptr->orig_bytes, bin, sz);
        pb->thing_word = HEADER_PROC_BIN;
        pb->size = sz;
        pb->next = NULL;
        pb->val = bptr;
        pb->bytes = (byte*) bptr->orig_bytes;
        pb->flags = 0;
        *bptrp = bptr;
        *hp += PROC_BIN_SIZE;
        return make_binary(pb);
    }
}

/* Sends trace message:
 *    {trace, PortPid, 'receive', {pid(), {command, iolist()}}}
 *    {trace, PortPid, 'receive', {pid(), {control, pid()}}}
 *    {trace, PortPid, 'receive', {pid(), exit}}
 *
 */
void
trace_port_receive(Port *t_p, Eterm caller, Eterm what, ...)
{
    ErtsTracerNif *tnif = NULL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(t_p)
		       || erts_thr_progress_is_blocking());
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (is_tracer_enabled(NULL, 0, &t_p->common, &tnif, TRACE_FUN_E_RECEIVE, am_receive)) {
        /* We can use a stack heap here, as the nif is called in the
           context of a port */
#define LOCAL_HEAP_SIZE (3 + 3 + heap_bin_size(ERL_ONHEAP_BIN_LIMIT) + 3)
        DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);

        Eterm *hp, data, *orig_hp = NULL;
        Binary *bptr = NULL;
        va_list args;
        UseTmpHeapNoproc(LOCAL_HEAP_SIZE);
        hp = local_heap;

        if (what == am_close) {
            data = what;
        } else {
            Eterm arg;
            va_start(args, what);
            if (what == am_command) {
                char *bin = va_arg(args, char *);
                Sint sz = va_arg(args, Sint);
                va_end(args);
                arg = trace_port_tmp_binary(bin, sz, &bptr, &hp);
            } else if (what == am_call || what == am_control) {
                unsigned int command = va_arg(args, unsigned int);
                char *bin = va_arg(args, char *);
                Sint sz = va_arg(args, Sint);
                Eterm cmd;
                va_end(args);
                arg = trace_port_tmp_binary(bin, sz, &bptr, &hp);
#if defined(ARCH_32)
                if (!IS_USMALL(0, command)) {
                    *hp = make_pos_bignum_header(1);
                    BIG_DIGIT(hp, 0) = (Uint)command;
                    cmd = make_big(hp);
                    hp += 2;
                } else
#endif
                {
                    cmd = make_small((Sint)command);
                }
                arg = TUPLE2(hp, cmd, arg);
                hp += 3;
            } else if (what == am_commandv) {
                ErlIOVec *evp = va_arg(args, ErlIOVec*);
                int i;
                va_end(args);
                if ((6 + evp->vsize * (2+PROC_BIN_SIZE+ERL_SUB_BIN_SIZE)) > LOCAL_HEAP_SIZE) {
                    hp = erts_alloc(ERTS_ALC_T_TMP,
                                    (6 + evp->vsize * (2+PROC_BIN_SIZE+ERL_SUB_BIN_SIZE)) * sizeof(Eterm));
                    orig_hp = hp;
                }
                arg = NIL;
                /* Convert each element in the ErlIOVec to a sub bin that points
                   to a procbin. We don't have to increment the proc bin refc as
                   the port task keeps the reference alive. */
                for (i = evp->vsize-1; i >= 0; i--) {
                    if (evp->iov[i].iov_len) {
                        ProcBin* pb = (ProcBin*)hp;
                        ErlSubBin *sb;
                        ASSERT(evp->binv[i]);
                        pb->thing_word = HEADER_PROC_BIN;
                        pb->val = ErlDrvBinary2Binary(evp->binv[i]);
                        pb->size = pb->val->orig_size;
                        pb->next = NULL;
                        pb->bytes = (byte*) pb->val->orig_bytes;
                        pb->flags = 0;
                        hp += PROC_BIN_SIZE;

                        sb = (ErlSubBin*) hp;
                        sb->thing_word = HEADER_SUB_BIN;
                        sb->size = evp->iov[i].iov_len;
                        sb->offs = (byte*)(evp->iov[i].iov_base) - pb->bytes;
                        sb->orig = make_binary(pb);
                        sb->bitoffs = 0;
                        sb->bitsize = 0;
                        sb->is_writable = 0;
                        hp += ERL_SUB_BIN_SIZE;

                        arg = CONS(hp, make_binary(sb), arg);
                        hp += 2;
                    }
                }
                what = am_command;
            } else {
                arg = va_arg(args, Eterm);
                va_end(args);
            }
            data = TUPLE2(hp, what, arg);
            hp += 3;
        }

        data = TUPLE2(hp, caller, data);
        hp += 3;
        ASSERT(hp <= (local_heap + LOCAL_HEAP_SIZE) || orig_hp);
        send_to_tracer_nif(NULL, &t_p->common, t_p->common.id, tnif,
                           TRACE_FUN_T_RECEIVE,
                           am_receive, data, THE_NON_VALUE, am_true);

        if (bptr)
            erts_bin_release(bptr);

        if (orig_hp)
            erts_free(ERTS_ALC_T_TMP, orig_hp);

        UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
    }
#undef LOCAL_HEAP_SIZE
}

void
trace_port_send(Port *t_p, Eterm receiver, Eterm msg, int exists)
{
    ErtsTracerNif *tnif = NULL;
    Eterm op = exists ? am_send : am_send_to_non_existing_process;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(t_p)
		       || erts_thr_progress_is_blocking());
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (is_tracer_enabled(NULL, 0, &t_p->common, &tnif, TRACE_FUN_E_SEND, op))
        send_to_tracer_nif(NULL, &t_p->common, t_p->common.id, tnif, TRACE_FUN_T_SEND,
                           op, msg, receiver, am_true);
}

void trace_port_send_binary(Port *t_p, Eterm to, Eterm what, char *bin, Sint sz)
{
    ErtsTracerNif *tnif = NULL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(t_p)
		       || erts_thr_progress_is_blocking());
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (is_tracer_enabled(NULL, 0, &t_p->common, &tnif, TRACE_FUN_E_SEND, am_send)) {
        Eterm msg;
        Binary* bptr = NULL;
#define LOCAL_HEAP_SIZE (3 + 3 + heap_bin_size(ERL_ONHEAP_BIN_LIMIT))
        DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);

        Eterm *hp;

        ERTS_CT_ASSERT(heap_bin_size(ERL_ONHEAP_BIN_LIMIT) >= PROC_BIN_SIZE);
        UseTmpHeapNoproc(LOCAL_HEAP_SIZE);
        hp = local_heap;

        msg = trace_port_tmp_binary(bin, sz, &bptr, &hp);

        msg = TUPLE2(hp, what, msg);
        hp += 3;
        msg = TUPLE2(hp, t_p->common.id, msg);
        hp += 3;

        send_to_tracer_nif(NULL, &t_p->common, t_p->common.id, tnif, TRACE_FUN_T_SEND,
                           am_send, msg, to, am_true);
        if (bptr)
            erts_bin_release(bptr);

        UnUseTmpHeapNoproc(LOCAL_HEAP_SIZE);
#undef LOCAL_HEAP_SIZE
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
    trace_sched_ports_where(p, what, make_small(0));
}

void
trace_sched_ports_where(Port *t_p, Eterm what, Eterm where) {
    ErtsTracerNif *tnif = NULL;
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(t_p)
		       || erts_thr_progress_is_blocking());
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    if (is_tracer_enabled(NULL, 0, &t_p->common, &tnif, TRACE_FUN_E_SCHED_PORT, what))
        send_to_tracer_nif(NULL, &t_p->common, t_p->common.id,
                           tnif, TRACE_FUN_T_SCHED_PORT,
                           what, where, THE_NON_VALUE, am_true);
}

/* Port profiling */

void
profile_runnable_port(Port *p, Eterm status) {
    Eterm *hp, msg;
    ErlHeapFragment *bp = NULL;
    Eterm count = make_small(0);

#ifndef ERTS_SMP
#define LOCAL_HEAP_SIZE (6 + ERTS_TRACE_PATCH_TS_MAX_SIZE)

    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

    hp = local_heap;
    
#else
    Uint hsz;

    hsz = 6 + patch_ts_size(erts_system_profile_ts_type)-1;

    bp = new_message_buffer(hsz);
    hp = bp->mem;
#endif

    erts_smp_mtx_lock(&smq_mtx);

    msg = TUPLE5(hp, am_profile, p->common.id, status, count,
		 NIL /* Will be overwritten by timestamp */);
    hp += 6;

    /* Write timestamp in element 5 of the 'msg' tuple */
    hp[-1] = write_ts(erts_system_profile_ts_type, hp, bp, NULL);

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
    Eterm *hp, msg;
    Eterm where = am_undefined;
    ErlHeapFragment *bp = NULL;
    ErtsCodeMFA *cmfa = NULL;

#ifndef ERTS_SMP
#define LOCAL_HEAP_SIZE (4 + 6 + ERTS_TRACE_PATCH_TS_MAX_SIZE)
    DeclareTmpHeapNoproc(local_heap,LOCAL_HEAP_SIZE);
    UseTmpHeapNoproc(LOCAL_HEAP_SIZE);

    hp = local_heap;
#else
    ErtsThrPrgrDelayHandle dhndl;
    Uint hsz = 4 + 6 + patch_ts_size(erts_system_profile_ts_type)-1;
#endif
    /* Assumptions:
     * We possibly don't have the MAIN_LOCK for the process p here.
     * We assume that we can read from p->current and p->i atomically
     */
#ifdef ERTS_SMP
    dhndl = erts_thr_progress_unmanaged_delay(); /* suspend purge operations */
#endif

    if (!ERTS_PROC_IS_EXITING(p)) {
        if (p->current) {
            cmfa = p->current;
        } else {
            cmfa = find_function_from_pc(p->i);
        }
    }

#ifdef ERTS_SMP
    if (!cmfa) {
	hsz -= 4;
    }

    bp = new_message_buffer(hsz);
    hp = bp->mem;
#endif

    if (cmfa) {
	where = TUPLE3(hp, cmfa->module, cmfa->function,
                       make_small(cmfa->arity));
        hp += 4;
    } else {
	where = make_small(0);
    }

#ifdef ERTS_SMP
    erts_thr_progress_unmanaged_continue(dhndl);
#endif
	
    erts_smp_mtx_lock(&smq_mtx);

    msg = TUPLE5(hp, am_profile, p->common.id, status, where,
		 NIL /* Will be overwritten by timestamp */);
    hp += 6;

    /* Write timestamp in element 5 of the 'msg' tuple */
    hp[-1] = write_ts(erts_system_profile_ts_type, hp, bp, NULL);

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
            case SYS_MSG_TYPE_PROC_MSG:
                receiver = smqp->to;
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
		if (!proc) {
		    /* Bad tracer */
		    goto failure;
		}
		else {
		    ErtsMessage *mp;
		queue_proc_msg:
		    mp = erts_alloc_message(0, NULL);
		    mp->data.heap_frag = smqp->bp;
		    erts_queue_message(proc,proc_locks,mp,smqp->msg,am_system);
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
    thr_opts.name = "sys_msg_dispatcher";
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

#include "erl_nif.h"

typedef struct {
    char *name;
    Uint arity;
    ErlNifFunc *cb;
} ErtsTracerType;

struct ErtsTracerNif_ {
    HashBucket hb;
    Eterm module;
    struct erl_module_nif* nif_mod;
    ErtsTracerType tracers[NIF_TRACER_TYPES];
};

static void init_tracer_template(ErtsTracerNif *tnif) {

    /* default tracer functions */
    tnif->tracers[TRACE_FUN_DEFAULT].name  = "trace";
    tnif->tracers[TRACE_FUN_DEFAULT].arity = 5;
    tnif->tracers[TRACE_FUN_DEFAULT].cb    = NULL;

    tnif->tracers[TRACE_FUN_ENABLED].name  = "enabled";
    tnif->tracers[TRACE_FUN_ENABLED].arity = 3;
    tnif->tracers[TRACE_FUN_ENABLED].cb    = NULL;

    /* specific tracer functions */
    tnif->tracers[TRACE_FUN_T_SEND].name  = "trace_send";
    tnif->tracers[TRACE_FUN_T_SEND].arity = 5;
    tnif->tracers[TRACE_FUN_T_SEND].cb    = NULL;

    tnif->tracers[TRACE_FUN_T_RECEIVE].name  = "trace_receive";
    tnif->tracers[TRACE_FUN_T_RECEIVE].arity = 5;
    tnif->tracers[TRACE_FUN_T_RECEIVE].cb    = NULL;

    tnif->tracers[TRACE_FUN_T_CALL].name  = "trace_call";
    tnif->tracers[TRACE_FUN_T_CALL].arity = 5;
    tnif->tracers[TRACE_FUN_T_CALL].cb    = NULL;

    tnif->tracers[TRACE_FUN_T_SCHED_PROC].name  = "trace_running_procs";
    tnif->tracers[TRACE_FUN_T_SCHED_PROC].arity = 5;
    tnif->tracers[TRACE_FUN_T_SCHED_PROC].cb    = NULL;

    tnif->tracers[TRACE_FUN_T_SCHED_PORT].name  = "trace_running_ports";
    tnif->tracers[TRACE_FUN_T_SCHED_PORT].arity = 5;
    tnif->tracers[TRACE_FUN_T_SCHED_PORT].cb    = NULL;

    tnif->tracers[TRACE_FUN_T_GC].name  = "trace_garbage_collection";
    tnif->tracers[TRACE_FUN_T_GC].arity = 5;
    tnif->tracers[TRACE_FUN_T_GC].cb    = NULL;

    tnif->tracers[TRACE_FUN_T_PROCS].name  = "trace_procs";
    tnif->tracers[TRACE_FUN_T_PROCS].arity = 5;
    tnif->tracers[TRACE_FUN_T_PROCS].cb    = NULL;

    tnif->tracers[TRACE_FUN_T_PORTS].name  = "trace_ports";
    tnif->tracers[TRACE_FUN_T_PORTS].arity = 5;
    tnif->tracers[TRACE_FUN_T_PORTS].cb    = NULL;

    /* specific enabled functions */
    tnif->tracers[TRACE_FUN_E_SEND].name  = "enabled_send";
    tnif->tracers[TRACE_FUN_E_SEND].arity = 3;
    tnif->tracers[TRACE_FUN_E_SEND].cb    = NULL;

    tnif->tracers[TRACE_FUN_E_RECEIVE].name  = "enabled_receive";
    tnif->tracers[TRACE_FUN_E_RECEIVE].arity = 3;
    tnif->tracers[TRACE_FUN_E_RECEIVE].cb    = NULL;

    tnif->tracers[TRACE_FUN_E_CALL].name  = "enabled_call";
    tnif->tracers[TRACE_FUN_E_CALL].arity = 3;
    tnif->tracers[TRACE_FUN_E_CALL].cb    = NULL;

    tnif->tracers[TRACE_FUN_E_SCHED_PROC].name  = "enabled_running_procs";
    tnif->tracers[TRACE_FUN_E_SCHED_PROC].arity = 3;
    tnif->tracers[TRACE_FUN_E_SCHED_PROC].cb    = NULL;

    tnif->tracers[TRACE_FUN_E_SCHED_PORT].name  = "enabled_running_ports";
    tnif->tracers[TRACE_FUN_E_SCHED_PORT].arity = 3;
    tnif->tracers[TRACE_FUN_E_SCHED_PORT].cb    = NULL;

    tnif->tracers[TRACE_FUN_E_GC].name  = "enabled_garbage_collection";
    tnif->tracers[TRACE_FUN_E_GC].arity = 3;
    tnif->tracers[TRACE_FUN_E_GC].cb    = NULL;

    tnif->tracers[TRACE_FUN_E_PROCS].name  = "enabled_procs";
    tnif->tracers[TRACE_FUN_E_PROCS].arity = 3;
    tnif->tracers[TRACE_FUN_E_PROCS].cb    = NULL;

    tnif->tracers[TRACE_FUN_E_PORTS].name  = "enabled_ports";
    tnif->tracers[TRACE_FUN_E_PORTS].arity = 3;
    tnif->tracers[TRACE_FUN_E_PORTS].cb    = NULL;
}

static Hash *tracer_hash = NULL;
static erts_smp_rwmtx_t tracer_mtx;

static ErtsTracerNif *
load_tracer_nif(const ErtsTracer tracer)
{
    Module* mod = erts_get_module(ERTS_TRACER_MODULE(tracer),
                                  erts_active_code_ix());
    struct erl_module_instance *instance;
    ErlNifFunc *funcs;
    int num_of_funcs;
    ErtsTracerNif tnif_tmpl, *tnif;
    ErtsTracerType *tracers;
    int i,j;

    if (!mod || !mod->curr.nif) {
        return NULL;
    }

    instance = &mod->curr;

    init_tracer_template(&tnif_tmpl);
    tnif_tmpl.nif_mod = instance->nif;
    tnif_tmpl.module = ERTS_TRACER_MODULE(tracer);
    tracers = tnif_tmpl.tracers;

    num_of_funcs = erts_nif_get_funcs(instance->nif, &funcs);

    for(i = 0; i < num_of_funcs; i++) {
        for (j = 0; j < NIF_TRACER_TYPES; j++) {
            if (strcmp(tracers[j].name, funcs[i].name) == 0 && tracers[j].arity == funcs[i].arity) {
                tracers[j].cb = &(funcs[i]);
                break;
            }
        }
    }

    if (tracers[TRACE_FUN_DEFAULT].cb == NULL || tracers[TRACE_FUN_ENABLED].cb == NULL ) {
        return NULL;
    }

    erts_smp_rwmtx_rwlock(&tracer_mtx);
    tnif = hash_put(tracer_hash, &tnif_tmpl);
    erts_smp_rwmtx_rwunlock(&tracer_mtx);

    return tnif;
}

static ERTS_INLINE ErtsTracerNif *
lookup_tracer_nif(const ErtsTracer tracer)
{
    ErtsTracerNif tnif_tmpl;
    ErtsTracerNif *tnif;
    tnif_tmpl.module = ERTS_TRACER_MODULE(tracer);
    erts_smp_rwmtx_rlock(&tracer_mtx);
    if ((tnif = hash_get(tracer_hash, &tnif_tmpl)) == NULL) {
        erts_smp_rwmtx_runlock(&tracer_mtx);
        tnif = load_tracer_nif(tracer);
        ASSERT(!tnif || tnif->nif_mod);
        return tnif;
    }
    erts_smp_rwmtx_runlock(&tracer_mtx);
    ASSERT(tnif->nif_mod);
    return tnif;
}

/* This function converts an Erlang tracer term to ErtsTracer.
   It returns THE_NON_VALUE if an invalid tracer term was given.
   Accepted input is:
     pid() || port() || {prefix, pid()} || {prefix, port()} ||
     {prefix, atom(), term()} || {atom(), term()}
 */
ErtsTracer
erts_term_to_tracer(Eterm prefix, Eterm t)
{
    ErtsTracer tracer = erts_tracer_nil;
    ASSERT(is_atom(prefix) || prefix == THE_NON_VALUE);
    if (!is_nil(t)) {
        Eterm module = am_erl_tracer, state = THE_NON_VALUE;
        Eterm hp[2];
        if (is_tuple(t)) {
            Eterm *tp = tuple_val(t);
            if (prefix != THE_NON_VALUE) {
                if (arityval(tp[0]) == 2 && tp[1] == prefix)
                    t = tp[2];
                else if (arityval(tp[0]) == 3 && tp[1] == prefix && is_atom(tp[2])) {
                    module = tp[2];
                    state = tp[3];
                }
            } else {
                if (arityval(tp[0]) == 2 && is_atom(tp[2])) {
                    module = tp[1];
                    state = tp[2];
                }
            }
        }
        if (state == THE_NON_VALUE && (is_internal_pid(t) || is_internal_port(t)))
            state = t;
        if (state == THE_NON_VALUE)
            return THE_NON_VALUE;
        erts_tracer_update(&tracer, CONS(hp, module, state));
    }
    if (!lookup_tracer_nif(tracer)) {
        ASSERT(ERTS_TRACER_MODULE(tracer) != am_erl_tracer);
        ERTS_TRACER_CLEAR(&tracer);
        return THE_NON_VALUE;
    }
    return tracer;
}

Eterm
erts_tracer_to_term(Process *p, ErtsTracer tracer)
{
    if (ERTS_TRACER_IS_NIL(tracer))
        return am_false;
    if (ERTS_TRACER_MODULE(tracer) == am_erl_tracer)
        /* Have to manage these specifically in order to be
           backwards compatible */
        return ERTS_TRACER_STATE(tracer);
    else {
        Eterm *hp = HAlloc(p, 3);
        return TUPLE2(hp, ERTS_TRACER_MODULE(tracer),
                      copy_object(ERTS_TRACER_STATE(tracer), p));
    }
}


static ERTS_INLINE int
send_to_tracer_nif_raw(Process *c_p, Process *tracee,
                       const ErtsTracer tracer, Uint tracee_flags,
                       Eterm t_p_id, ErtsTracerNif *tnif,
                       enum ErtsTracerOpt topt,
                       Eterm tag, Eterm msg, Eterm extra, Eterm pam_result)
{
    if (tnif || (tnif = lookup_tracer_nif(tracer)) != NULL) {
#define MAP_SIZE 4
        Eterm argv[5], local_heap[3+MAP_SIZE /* values */ + (MAP_SIZE+1 /* keys */)];
        flatmap_t *map = (flatmap_t*)(local_heap+(MAP_SIZE+1));
        Eterm *map_values = flatmap_get_values(map);
        Eterm *map_keys = local_heap + 1;
        Uint map_elem_count = 0;

        topt = (tnif->tracers[topt].cb) ? topt : TRACE_FUN_DEFAULT;
        ASSERT(topt < NIF_TRACER_TYPES);

        argv[0] = tag;
        argv[1] = ERTS_TRACER_STATE(tracer);
        argv[2] = t_p_id;
        argv[3] = msg;
        argv[4] = make_flatmap(map);

        map->thing_word = MAP_HEADER_FLATMAP;

        if (extra != THE_NON_VALUE) {
            map_keys[map_elem_count] = am_extra;
            map_values[map_elem_count++] = extra;
        }

        if (pam_result != am_true) {
            map_keys[map_elem_count] = am_match_spec_result;
            map_values[map_elem_count++] = pam_result;
        }

        if (tracee_flags & F_TRACE_SCHED_NO) {
            map_keys[map_elem_count] = am_scheduler_id;
            map_values[map_elem_count++] = make_small(erts_get_scheduler_id());
        }
        map_keys[map_elem_count] = am_timestamp;
        if (tracee_flags & F_NOW_TS)
#ifdef HAVE_ERTS_NOW_CPU
            if (erts_cpu_timestamp)
                map_values[map_elem_count++] = am_cpu_timestamp;
            else
#endif
                map_values[map_elem_count++] = am_timestamp;
        else if (tracee_flags & F_STRICT_MON_TS)
            map_values[map_elem_count++] = am_strict_monotonic;
        else if (tracee_flags & F_MON_TS)
            map_values[map_elem_count++] = am_monotonic;

        map->size = map_elem_count;
        map->keys = make_tuple(local_heap);
        local_heap[0] = make_arityval(map_elem_count);

#undef MAP_SIZE
        erts_nif_call_function(c_p, tracee ? tracee : c_p,
                               tnif->nif_mod,
                               tnif->tracers[topt].cb,
                               tnif->tracers[topt].arity,
                               argv);
    }
    return 1;
}

static ERTS_INLINE int
send_to_tracer_nif(Process *c_p, ErtsPTabElementCommon *t_p,
                   Eterm t_p_id, ErtsTracerNif *tnif, enum ErtsTracerOpt topt,
                   Eterm tag, Eterm msg, Eterm extra, Eterm pam_result)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    if (c_p) {
        /* We have to hold the main lock of the currently executing process */
        erts_proc_lc_chk_have_proc_locks(c_p, ERTS_PROC_LOCK_MAIN);
    }
    if (is_internal_pid(t_p->id)) {
        /* We have to have at least one lock */
        ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((Process*)t_p) & ERTS_PROC_LOCKS_ALL);
    } else {
        ASSERT(is_internal_port(t_p->id));
        ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked((Port*)t_p));
    }
#endif

    return send_to_tracer_nif_raw(c_p,
                                  is_internal_pid(t_p->id) ? (Process*)t_p : NULL,
                                  t_p->tracer, t_p->trace_flags,
                                  t_p_id, tnif, topt, tag, msg, extra,
                                  pam_result);
}

static ERTS_INLINE Eterm
call_enabled_tracer(const ErtsTracer tracer,
                    ErtsTracerNif **tnif_ret,
                    enum ErtsTracerOpt topt,
                    Eterm tag, Eterm t_p_id) {
    ErtsTracerNif *tnif = lookup_tracer_nif(tracer);
    if (tnif) {
        Eterm argv[] = {tag, ERTS_TRACER_STATE(tracer), t_p_id},
            ret;
        topt = (tnif->tracers[topt].cb) ? topt : TRACE_FUN_ENABLED;
        ASSERT(topt < NIF_TRACER_TYPES);
        ASSERT(tnif->tracers[topt].cb != NULL);
        if (tnif_ret) *tnif_ret = tnif;
        ret = erts_nif_call_function(NULL, NULL, tnif->nif_mod,
                                     tnif->tracers[topt].cb,
                                     tnif->tracers[topt].arity,
                                     argv);
        if (tag == am_trace_status && ret != am_remove)
            return am_trace;
        ASSERT(tag == am_trace_status || ret != am_remove);
        return ret;
    }
    return tag == am_trace_status ? am_remove : am_discard;
}

static int
is_tracer_enabled(Process* c_p, ErtsProcLocks c_p_locks,
                  ErtsPTabElementCommon *t_p,
                  ErtsTracerNif **tnif_ret,
                  enum ErtsTracerOpt topt, Eterm tag) {
    Eterm nif_result;

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    if (c_p)
        ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == c_p_locks
                           || erts_thr_progress_is_blocking());
    if (is_internal_pid(t_p->id)) {
        /* We have to have at least one lock */
        ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((Process*)t_p) & ERTS_PROC_LOCKS_ALL
                           || erts_thr_progress_is_blocking());
    } else {
        ASSERT(is_internal_port(t_p->id));
        ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked((Port*)t_p)
                           || erts_thr_progress_is_blocking());
    }
#endif

    nif_result = call_enabled_tracer(t_p->tracer, tnif_ret, topt, tag, t_p->id);
    switch (nif_result) {
    case am_discard: return 0;
    case am_trace: return 1;
    case THE_NON_VALUE:
    case am_remove: ASSERT(tag == am_trace_status); break;
    default:
        /* only am_remove should be returned, but if
           something else is returned we fall-through
           and remove the tracer. */
        ASSERT(0);
    }

    /* Only remove tracer on self() and ports */
    if (is_internal_port(t_p->id) || (c_p && c_p->common.id == t_p->id)) {
        ErtsProcLocks c_p_xlocks = 0;
        if (is_internal_pid(t_p->id)) {
            ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) & ERTS_PROC_LOCK_MAIN);
            if (c_p_locks != ERTS_PROC_LOCKS_ALL) {
                c_p_xlocks = ~c_p_locks & ERTS_PROC_LOCKS_ALL;
                if (erts_smp_proc_trylock(c_p, c_p_xlocks) == EBUSY) {
                    erts_smp_proc_unlock(c_p, c_p_locks & ~ERTS_PROC_LOCK_MAIN);
                    erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
                }
            }
        }
        erts_tracer_replace(t_p, erts_tracer_nil);
        t_p->trace_flags &= ~TRACEE_FLAGS;

        if (c_p_xlocks)
            erts_smp_proc_unlock(c_p, c_p_xlocks);
    }

    return 0;
}

int erts_is_tracer_enabled(const ErtsTracer tracer, ErtsPTabElementCommon *t_p)
{
    ErtsTracerNif *tnif = lookup_tracer_nif(tracer);
    if (tnif) {
        Eterm nif_result = call_enabled_tracer(tracer, &tnif,
                                               TRACE_FUN_ENABLED,
                                               am_trace_status,
                                               t_p->id);
        switch (nif_result) {
        case am_discard:
        case am_trace: return 1;
        default:
            break;
        }
    }
    return 0;
}

int erts_is_tracer_proc_enabled(Process* c_p, ErtsProcLocks c_p_locks,
                                ErtsPTabElementCommon *t_p)
{
    return is_tracer_enabled(c_p, c_p_locks, t_p, NULL, TRACE_FUN_ENABLED,
                             am_trace_status);
}

int erts_is_tracer_proc_enabled_send(Process* c_p, ErtsProcLocks c_p_locks,
                                     ErtsPTabElementCommon *t_p)
{
    return is_tracer_enabled(c_p, c_p_locks, t_p, NULL, TRACE_FUN_T_SEND, am_send);
}


void erts_tracer_replace(ErtsPTabElementCommon *t_p, const ErtsTracer tracer)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    if (is_internal_pid(t_p->id) && !erts_thr_progress_is_blocking()) {
        erts_proc_lc_chk_have_proc_locks((Process*)t_p, ERTS_PROC_LOCKS_ALL);
    } else if (is_internal_port(t_p->id)) {
        ERTS_LC_ASSERT(erts_lc_is_port_locked((Port*)t_p)
                       || erts_thr_progress_is_blocking());
    }
#endif
    if (ERTS_TRACER_COMPARE(t_p->tracer, tracer))
        return;

    erts_tracer_update(&t_p->tracer, tracer);
}

static void free_tracer(void *p)
{
    ErtsTracer tracer = (ErtsTracer)p;

    if (is_immed(ERTS_TRACER_STATE(tracer))) {
        erts_free(ERTS_ALC_T_HEAP_FRAG, ptr_val(tracer));
    } else {
        ErlHeapFragment *hf = (void*)((char*)(ptr_val(tracer)) - offsetof(ErlHeapFragment, mem));
        free_message_buffer(hf);
    }
}

/* un-define erts_tracer_update before implementation */
#ifdef erts_tracer_update
#undef erts_tracer_update
#endif

/*
 * ErtsTracer is either NIL, 'true' or [Mod | State]
 *
 * - If State is immediate then the memory for
 *   the cons cell is just two words + sizeof(ErtsThrPrgrLaterOp) large.
 * - If State is a complex term then the cons cell
 *   is allocated in an ErlHeapFragment where the cons
 *   ptr points to the mem field. So in order to get the
 *   ptr to the fragment you do this:
 *   (char*)(ptr_val(tracer)) - offsetof(ErlHeapFragment, mem)
 *   Normally you shouldn't have to care about this though
 *   as erts_tracer_update takes care of it for you.
 *
 *   When ErtsTracer is stored in the stack as part of a
 *   return trace, the cons cell is stored on the heap of
 *   the process.
 *
 *   The cons cell is not always stored on the heap as:
 *     1) for port/meta tracing there is no heap
 *     2) we would need the main lock in order to
 *        read the tracer which is undesirable.
 *
 * One way to optimize this (memory wise) is to keep an refc and only bump
 * the refc when *tracer is NIL.
 */
void
erts_tracer_update(ErtsTracer *tracer, const ErtsTracer new_tracer)
{
    ErlHeapFragment *hf;

    if (is_not_nil(*tracer)) {
        Uint offs = 2;
        UWord size = 2 * sizeof(Eterm) + sizeof(ErtsThrPrgrLaterOp);
        ErtsThrPrgrLaterOp *lop;
        ASSERT(is_list(*tracer));
        if (is_not_immed(ERTS_TRACER_STATE(*tracer))) {
            hf = (void*)(((char*)(ptr_val(*tracer)) - offsetof(ErlHeapFragment, mem)));
            offs = hf->used_size;
            size = hf->alloc_size * sizeof(Eterm) + sizeof(ErlHeapFragment);
            ASSERT(offs == size_object(*tracer));
        }

        /* sparc assumes that all structs are double word aligned, so we
           have to align the ErtsThrPrgrLaterOp struct otherwise it may
           segfault.*/
        if ((UWord)(ptr_val(*tracer) + offs) % (sizeof(UWord)*2) == sizeof(UWord))
            offs += 1;

        lop = (ErtsThrPrgrLaterOp*)(ptr_val(*tracer) + offs);
        ASSERT((UWord)lop % (sizeof(UWord)*2) == 0);

        /* We schedule the free:ing of the tracer until after a thread progress
           has been made so that we know that no schedulers have any references
           to it. Because we do this, it is possible to release all locks of a
           process/port and still use the ErtsTracer of that port/process
           without having to worry if it is free'd.
        */
        erts_schedule_thr_prgr_later_cleanup_op(
            free_tracer, (void*)(*tracer), lop, size);
    }

    if (is_nil(new_tracer)) {
        *tracer = new_tracer;
    } else if (is_immed(ERTS_TRACER_STATE(new_tracer))) {
        /* If tracer state is an immediate we only allocate a 2 Eterm heap.
           Not sure if it is worth it, we save 4 words (sizeof(ErlHeapFragment))
           per tracer. */
        Eterm *hp = erts_alloc(ERTS_ALC_T_HEAP_FRAG,
                               3*sizeof(Eterm) + sizeof(ErtsThrPrgrLaterOp));
        *tracer = CONS(hp, ERTS_TRACER_MODULE(new_tracer),
                       ERTS_TRACER_STATE(new_tracer));
    } else {
        Eterm *hp, tracer_state = ERTS_TRACER_STATE(new_tracer),
            tracer_module = ERTS_TRACER_MODULE(new_tracer);
        Uint sz = size_object(tracer_state);
        hf = new_message_buffer(sz + 2  /* cons cell */ +
                                (sizeof(ErtsThrPrgrLaterOp)+sizeof(Eterm)-1)/sizeof(Eterm) + 1);
        hp = hf->mem + 2;
        hf->used_size -= (sizeof(ErtsThrPrgrLaterOp)+sizeof(Eterm)-1)/sizeof(Eterm) + 1;
        *tracer = copy_struct(tracer_state, sz, &hp, &hf->off_heap);
        *tracer = CONS(hf->mem, tracer_module, *tracer);
        ASSERT((void*)(((char*)(ptr_val(*tracer)) - offsetof(ErlHeapFragment, mem))) == hf);
    }
}

static void init_tracer_nif()
{
    erts_smp_rwmtx_opt_t rwmtx_opt = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_EXTREMELY_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_SMP_RWMTX_LONG_LIVED;
    erts_smp_rwmtx_init_opt(&tracer_mtx, &rwmtx_opt, "tracer_mtx");

    erts_tracer_nif_clear();

}

int erts_tracer_nif_clear()
{

    erts_smp_rwmtx_rlock(&tracer_mtx);
    if (!tracer_hash || tracer_hash->nobjs) {

        HashFunctions hf;
        hf.hash = tracer_hash_fun;
        hf.cmp = tracer_cmp_fun;
        hf.alloc = tracer_alloc_fun;
        hf.free = tracer_free_fun;
        hf.meta_alloc = (HMALLOC_FUN) erts_alloc;
        hf.meta_free = (HMFREE_FUN) erts_free;
        hf.meta_print = (HMPRINT_FUN) erts_print;

        erts_smp_rwmtx_runlock(&tracer_mtx);
        erts_smp_rwmtx_rwlock(&tracer_mtx);

        if (tracer_hash)
            hash_delete(tracer_hash);

        tracer_hash = hash_new(ERTS_ALC_T_TRACER_NIF, "tracer_hash", 10, hf);

        erts_smp_rwmtx_rwunlock(&tracer_mtx);
        return 1;
    }

    erts_smp_rwmtx_runlock(&tracer_mtx);
    return 0;
}

static int tracer_cmp_fun(void* a, void* b)
{
    return ((ErtsTracerNif*)a)->module != ((ErtsTracerNif*)b)->module;
}

static HashValue tracer_hash_fun(void* obj)
{
    return make_internal_hash(((ErtsTracerNif*)obj)->module, 0);
}

static void *tracer_alloc_fun(void* tmpl)
{
    ErtsTracerNif *obj = erts_alloc(ERTS_ALC_T_TRACER_NIF,
                                    sizeof(ErtsTracerNif) +
                                    sizeof(ErtsThrPrgrLaterOp));
    memcpy(obj, tmpl, sizeof(*obj));
    return obj;
}

static void tracer_free_fun_cb(void* obj)
{
    erts_free(ERTS_ALC_T_TRACER_NIF, obj);
}

static void tracer_free_fun(void* obj)
{
    ErtsTracerNif *tnif = obj;
    erts_schedule_thr_prgr_later_op(
        tracer_free_fun_cb, obj,
        (ErtsThrPrgrLaterOp*)(tnif + 1));

}
