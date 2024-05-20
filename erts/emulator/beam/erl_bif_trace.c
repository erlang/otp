/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2024. All Rights Reserved.
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
#include "erl_proc_sig_queue.h"

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

const struct trace_pattern_flags erts_trace_pattern_flags_off = {0, 0, 0, 0, 0, 0};

static struct {			/* Protected by code modification permission */
    int current;
    int install;
    int local;
    int global;
    BpFunctions f;		/* Local functions */
    BpFunctions e;		/* Export entries */
    Process* stager;
    ErtsCodeBarrier barrier;
} finish_bp;

Process *erts_trace_cleaner = NULL;

static Eterm trace(Process* p, ErtsTraceSession *session,
                   Eterm pid_spec, Eterm how, Eterm list);
static Eterm
trace_pattern(Process* p, ErtsTraceSession*, Eterm MFA, Eterm Pattern, Eterm flaglist);
static void change_on_load_trace_pattern(ErtsTraceSession *s,
                                         enum erts_break_op on,
                                         struct trace_pattern_flags flags,
                                         Binary* match_prog_set,
                                         ErtsTracer meta_tracer);
static void clear_on_load_trace_pattern(ErtsTraceSession *s);
static void prepare_clear_all_trace_pattern(ErtsTraceSession*);
static int stage_trace_event_pattern(Eterm event, Binary*, int on);

static void smp_bp_finisher(void* arg);
static BIF_RETTYPE
system_monitor(Process *p, Eterm monitor_pid, Eterm list);
static Eterm trace_session_create(Process*, Eterm name, Eterm tracer_term, Eterm opts);
static void trace_session_destroy_aux(void *session_v);
static void trace_session_destroy(ErtsTraceSession*);
static int tracer_session_destructor(Binary *btid);
static void free_session(ErtsTraceSession *session);
static void send_trace_cleaner_notification(void);
static Eterm handle_trace_clean_rpc(Process *p, void *arg, int *reds, ErlHeapFragment **hf);
static void send_trace_clean_ack(Process *p);


static void new_seq_trace_token(Process* p, int); /* help func for seq_trace_2*/
static Eterm trace_info(Process*, ErtsTraceSession*, Eterm What, Eterm Key);
static Eterm trace_info_pid(Process* p, ErtsTraceSession*, Eterm pid_spec, Eterm key);
static Eterm trace_info_func(Process* p, ErtsTraceSession*, Eterm pid_spec, Eterm key);
static Eterm trace_info_func_sessions(Process* p, Eterm func_spec, Eterm key);
static Eterm trace_info_on_load(Process* p, ErtsTraceSession*, Eterm key);
static Eterm trace_info_sessions(Process* p, Eterm What, Eterm key);

static Eterm trace_info_event(Process* p, ErtsTraceSession*, Eterm event, Eterm key);
static void clear_event_trace(ErtsTracingEvent *et);
static void set_event_trace(ErtsTracingEvent *et, Binary* match_spec);

static void install_exp_breakpoints(BpFunctions* f);
static void uninstall_exp_breakpoints(BpFunctions* f);
static void clean_export_entries(BpFunctions* f);

ErtsTraceSession erts_trace_session_0;
erts_rwmtx_t erts_trace_session_list_lock;

ErtsTraceSession* erts_trace_cleaner_wait_list;
erts_mtx_t erts_trace_cleaner_lock;

ErtsTraceSession* erts_trace_cleaner_do_list;



static
int erts_trace_session_init(ErtsTraceSession* s, ErtsTracer tracer,
                            Eterm name_atom)
{
    static Uint next_weak_id = 0;
    int i;

    for (i=0; i<ERTS_NUM_BP_IX; i++) {
        s->send_tracing[i].on = 1;
        s->send_tracing[i].match_spec = NULL;
        s->receive_tracing[i].on = 1;
        s->receive_tracing[i].match_spec = NULL;
    }
    s->on_load_trace_pattern_is_on = 0;
    s->on_load_match_spec = NULL;
    s->on_load_meta_match_spec = NULL;
    s->on_load_trace_pattern_flags = erts_trace_pattern_flags_off;
    s->on_load_meta_tracer = erts_tracer_nil;

    s->on_spawn_proc_trace_flags = F_INITIAL_TRACE_FLAGS;
    s->on_spawn_proc_tracer = erts_tracer_nil;
    s->on_open_port_trace_flags = F_INITIAL_TRACE_FLAGS;
    s->on_open_port_tracer = erts_tracer_nil;


    erts_atomic32_init_nob(&s->trace_control_word, 0);
    s->tracer = tracer;
    s->name_atom = name_atom;
    erts_atomic_init_nob(&s->state, ERTS_TRACE_SESSION_ALIVE);
#ifdef DEBUG
    erts_refc_init(&s->dbg_bp_refc, 0);
    erts_refc_init(&s->dbg_p_refc, 0);
#endif

    if(s == &erts_trace_session_0){
	s->next = NULL;
	s->prev = NULL;
        s->weak_id = am_default;
    }
    else{
	/* Link the session into the global list
	 */
	erts_rwmtx_rwlock(&erts_trace_session_list_lock);

	s->next = erts_trace_session_0.next;
	s->prev = &erts_trace_session_0;
	erts_trace_session_0.next = s;
	if (s->next) {
	    s->next->prev = s;
	}
        /* We ignore duplicates at wrap around. */
        s->weak_id = make_small(next_weak_id++);
	erts_rwmtx_rwunlock(&erts_trace_session_list_lock);
    }
    return 1;
}

/*
 * Does refc++ on returned trace session.
 */
static int term_to_session(Eterm term, ErtsTraceSession **session_p,
                           int allow_dead)
{
    ErtsTraceSession *s = NULL;
    Binary *bin;
    const Eterm *tpl;

    if (!is_tuple_arity(term, 2)) {
        return 0;
    }
    tpl = tuple_val(term);

    if (is_atom(tpl[1]) && is_small(tpl[2])) {
        const Eterm name = tpl[1];
        const Eterm weak_id = tpl[2];

        erts_rwmtx_rlock(&erts_trace_session_list_lock);
        for (s = erts_trace_session_0.next; s; s = s->next) {
            ASSERT(s != &erts_trace_session_0);
            if (s->name_atom == name && s->weak_id == weak_id) {
                if (!allow_dead && !erts_is_trace_session_alive(s)) {
                    s = NULL;
                    break;
                }
                bin = &ERTS_MAGIC_BIN_FROM_DATA(s)->binary;
                /*
                 * Make sure we don't resurrect a dying session with refc==0.
                 */
                if (erts_refc_inc_unless(&bin->intern.refc, 0, 0) == 0) {
                    s =  NULL;
                }
                break;
            }
        }
        erts_rwmtx_runlock(&erts_trace_session_list_lock);
    }
    else if (is_internal_magic_ref(tpl[1]) && is_tuple_arity(tpl[2], 2)) {
        const Eterm *weak_tpl = tuple_val(tpl[2]);

        bin = erts_magic_ref2bin(tpl[1]);
        if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) != tracer_session_destructor)
            return 0;

        s = (ErtsTraceSession*) ERTS_MAGIC_BIN_DATA(bin);
        ASSERT(s != &erts_trace_session_0);

        if (s->name_atom != weak_tpl[1]
            || s->weak_id != weak_tpl[2]
            || (!allow_dead && !erts_is_trace_session_alive(s))) {

            return 0;
        }

        erts_refc_inc(&bin->intern.refc, 2);
    }

    if (!s) {
        return 0;
    }

    *session_p = s;
    return 1;
}

#define ERTS_TRACE_SESSION_WEAK_REF_SZ 3

Eterm erts_make_trace_session_weak_ref(ErtsTraceSession *s, Eterm **hpp)
{
    Eterm weak_ref = TUPLE2(*hpp, s->name_atom, s->weak_id);
    *hpp += ERTS_TRACE_SESSION_WEAK_REF_SZ;
    return weak_ref;
}

void
erts_bif_trace_init(void)
{
    erts_trace_session_init(&erts_trace_session_0, erts_tracer_nil, am_legacy);
    erts_rwmtx_init(&erts_trace_session_list_lock, "trace_session_list", NIL,
		    ERTS_LOCK_FLAGS_PROPERTY_STATIC |
		    ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    erts_mtx_init(&erts_trace_cleaner_lock, "trace_cleaner", NIL,
                    ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                    ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
}

/*
 * Turn on/off call tracing for the given function(s).
 */

Eterm
erts_internal_trace_pattern_3(BIF_ALIST_3)
{
    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        ERTS_BIF_YIELD3(BIF_TRAP_EXPORT(BIF_erts_internal_trace_pattern_3),
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    return trace_pattern(BIF_P, &erts_trace_session_0,
                         BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

Eterm
erts_internal_trace_pattern_4(BIF_ALIST_4)
{
    ErtsTraceSession* session;

    if (!term_to_session(BIF_ARG_1, &session, 0)) {
        goto session_error;
    }

    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        erts_deref_trace_session(session);
        ERTS_BIF_YIELD4(BIF_TRAP_EXPORT(BIF_erts_internal_trace_pattern_4),
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, BIF_ARG_4);
    }

    /* Double check liveness with seized code mod permission */
    if (!erts_is_trace_session_alive(session)) {
        erts_release_code_mod_permission();
        erts_deref_trace_session(session);
        goto session_error;
    }

    return trace_pattern(BIF_P, session, BIF_ARG_2, BIF_ARG_3, BIF_ARG_4);

session_error:
    BIF_P->fvalue = am_session;
    BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
}

static Eterm
trace_pattern(Process* p, ErtsTraceSession *session,
              Eterm MFA, Eterm Pattern, Eterm flaglist)
{
    int i;
    int matches = -1;
    int specified = 0;
    enum erts_break_op on;
    Binary* match_prog_set = NULL;
    Eterm l;
    struct trace_pattern_flags flags = erts_trace_pattern_flags_off;
    int is_global;
    ErtsTracer meta_tracer = erts_tracer_nil;
    Uint freason = BADARG;

    ERTS_LC_ASSERT(erts_has_code_mod_permission());
    ASSERT(erts_staging_trace_session == NULL);
    erts_staging_trace_session = session;
    finish_bp.current = -1;

    UseTmpHeap(3,p);

    p->fvalue = am_badopt;
    is_global = 0;
    for(l = flaglist; is_list(l); l = CDR(list_val(l))) {
	if (is_tuple(CAR(list_val(l))) && ERTS_TRACER_IS_NIL(session->tracer)) {
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
            case am_call_memory:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.call_memory = 1;
		break;

	    default:
		goto error;
	    }
	}
    }
    if (l != NIL) {
	goto error;
    }

    /*
     * Check and compile the match specification.
     */
    if (Pattern == am_false) {
        on = 0;
    } else if (is_nil(Pattern) || Pattern == am_true) {
		/* Shortway of specifying [{'_', [], []}]
		 */
        on = 1;
    } else if (Pattern == am_restart) {
        on = ERTS_BREAK_RESTART;
    } else if (Pattern == am_pause) {
        on = ERTS_BREAK_PAUSE;
    } else {
        match_prog_set = erts_match_set_compile_trace(p, Pattern,
                                                      erts_staging_trace_session,
                                                      MFA, &freason);
        if (match_prog_set) {
            MatchSetRef(match_prog_set);
            on = 1;
        } else {
            p->fvalue = am_match_spec;
            goto error;
        }
    }

    p->fvalue = am_none;

    if (match_prog_set && !flags.local && !flags.meta && (flags.call_count || flags.call_time || flags.call_memory)) {
	/* A match prog is not allowed with just call_count, call_time or call_memory */
        p->fvalue = am_call_count;
	goto error;
    }

    /*
     * Check the MFA specification.
     */

    if (MFA == am_on_load) {
        change_on_load_trace_pattern(erts_staging_trace_session, on, flags,
                                     match_prog_set, meta_tracer);
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

	matches = erts_set_trace_pattern(&mfa, specified,
					 match_prog_set, match_prog_set,
					 on, flags, meta_tracer, 0);
    } else if (is_atom(MFA)) {
        if (is_global || flags.breakpoint || on > ERTS_BREAK_SET) {
            goto error;
        }
        matches = stage_trace_event_pattern(MFA, match_prog_set, on);
    }

 error:
    MatchSetUnref(match_prog_set);

    ERTS_TRACER_CLEAR(&meta_tracer);

    if (finish_bp.current >= 0) {
	ASSERT(matches >= 0);
	ASSERT(finish_bp.stager == NULL);
	finish_bp.stager = p;
	erts_schedule_code_barrier(&finish_bp.barrier, smp_bp_finisher, NULL);
	erts_proc_inc_refc(p);
	erts_suspend(p, ERTS_PROC_LOCK_MAIN, NULL);
	ERTS_BIF_YIELD_RETURN(p, make_small(matches));
    }
    else {
        erts_deref_trace_session(erts_staging_trace_session);
    }

#ifdef DEBUG
    erts_staging_trace_session = NULL;
#endif
    erts_release_code_mod_permission();

    if (matches >= 0) {
	return make_small(matches);
    }
    else {
	BIF_ERROR(p, freason | EXF_HAS_EXT_INFO);
    }
}

static void smp_bp_finisher(void* null)
{
    if (erts_finish_breakpointing()) { /* Not done */
        /* Arrange for being called again */
        erts_schedule_code_barrier(&finish_bp.barrier, smp_bp_finisher, NULL);
    }
    else {			/* Done */
        ErtsTraceSession *session = erts_staging_trace_session;
	Process* p = finish_bp.stager;
        const enum erts_trace_session_state state =
            erts_atomic_read_nob(&session->state);

        if (state == ERTS_TRACE_SESSION_CLEARING) {

            ASSERT(!finish_bp.install && finish_bp.local && finish_bp.global);
            ASSERT(erts_refc_read(&session->dbg_bp_refc, 0) == 0);

            /*
             * Link into trace_cleaner_wait_list
             */
            erts_mtx_lock(&erts_trace_cleaner_lock);
            session->next = erts_trace_cleaner_wait_list;
            session->prev = NULL;
            erts_trace_cleaner_wait_list = session;
            erts_mtx_unlock(&erts_trace_cleaner_lock);

            send_trace_cleaner_notification();
        }
#ifdef DEBUG
	finish_bp.stager = NULL;
        erts_staging_trace_session = NULL;
#endif
	erts_release_code_mod_permission();
        if (p) {
            /*
             * Operation initiated by BIF call which did refc++
             * to keep session alive during entire call.
             */
            if (state == ERTS_TRACE_SESSION_ALIVE) {
                erts_deref_trace_session(session);
            }
            /*else erts_trace_cleaner will do deref when done */

            erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
            if (!ERTS_PROC_IS_EXITING(p)) {
                erts_resume(p, ERTS_PROC_LOCK_STATUS);
            }
            erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
            erts_proc_dec_refc(p);
        }
        else {
            /*
             * Operation initiated by session destructor as refc was 0.
             */
            ASSERT(erts_refc_read(&ERTS_MAGIC_BIN_FROM_DATA(session)->binary.intern.refc, 0) == 0);
        }
    }
}

static void
send_trace_cleaner_notification(void)
{
    erts_queue_message(erts_trace_cleaner,
                       0,
                       erts_alloc_message(0, NULL),
                       am_notify,
                       am_system);
}

BIF_RETTYPE
erts_trace_cleaner_check_0(BIF_ALIST_1)
{
    ErtsTraceSession *s, *next;
    Binary* bin;

    /*
     * First release our list of cleaned sessions.
     */
    for (s = erts_trace_cleaner_do_list; s; s = next) {
        next = s->next;

        ASSERT(erts_refc_read(&s->dbg_bp_refc, 0) == 0);
        ASSERT(erts_refc_read(&s->dbg_p_refc, 0) == 0);
        ASSERT(erts_atomic_read_nob(&s->state) == ERTS_TRACE_SESSION_CLEARING);
        erts_atomic_set_nob(&s->state, ERTS_TRACE_SESSION_DEAD);

        bin = &ERTS_MAGIC_BIN_FROM_DATA(s)->binary;
        if (erts_refc_read(&bin->intern.refc, 0) == 0) {
            /*
             * Initiated by destructor as refc became 0.
             * No other references exist.
            */
            free_session(s);
        }
        else {
            /*
             * Initiated by trace_session_destroy() which did refc++
             * do corresponding refc-- here.
             */
            erts_deref_trace_session(s);
        }
    }

    /*
     * Start a new cleaning round if there are sessions waiting.
     */
    erts_mtx_lock(&erts_trace_cleaner_lock);
    erts_trace_cleaner_do_list = erts_trace_cleaner_wait_list;
    erts_trace_cleaner_wait_list = NULL;
    erts_mtx_unlock(&erts_trace_cleaner_lock);

    return erts_trace_cleaner_do_list ? am_true : am_false;
}

BIF_RETTYPE
erts_trace_cleaner_send_trace_clean_signal_1(BIF_ALIST_1)
{
    const Eterm to = BIF_ARG_1;

    if (BIF_P != erts_trace_cleaner) {
        BIF_ERROR(BIF_P, EXC_NOTSUP);
    }

    if (is_internal_pid(to)) {
        const Eterm ret = erts_proc_sig_send_rpc_request(BIF_P, to, 0,
                                                         handle_trace_clean_rpc,
                                                         NULL);
        return is_value(ret) ? am_true : am_false;
    }
    else if (is_internal_port(to)) {
        Port *prt = erts_id2port_sflgs(to,
                                       BIF_P,
                                       ERTS_PROC_LOCK_MAIN,
                                       ERTS_PORT_SFLGS_INVALID_LOOKUP);
        if (prt) {
            Uint reds = delete_unalive_trace_refs(&prt->common);
            erts_port_release(prt);
            BUMP_REDS(BIF_P, reds);
        }
        return am_false; /* done, no async signal sent */
    }
    BIF_ERROR(BIF_P, BADARG);
}

static Eterm
handle_trace_clean_rpc(Process *p, void *arg, int *reds, ErlHeapFragment **hf)
{
    ErtsTraceSession *s;
    ErtsTracerRef *ref;
    int locked_all = 0;

    ASSERT(arg == NULL);
    ASSERT(erts_trace_cleaner_do_list);

    for (s = erts_trace_cleaner_do_list; s; s = s->next) {

        ASSERT(erts_atomic_read_nob(&s->state) == ERTS_TRACE_SESSION_CLEARING);

        ref = get_tracer_ref(&p->common, s);
        if (ref) {
            if (!locked_all) {
                erts_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
                locked_all = 1;
            }
            clear_tracer_ref(&p->common, ref);
            delete_tracer_ref(&p->common, ref);
        }
    }

    if (locked_all) {
        erts_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
    }

    send_trace_clean_ack(p);

    *reds = 0;
    return NIL;
}

static void
send_trace_clean_ack(Process *p)
{
    ErtsMessage *mp;
    const Eterm msg = p->common.id;

    /*
     * Send own pid as ack back to erts_trace_cleaner
     */
    mp = erts_alloc_message(0, NULL);
    ERL_MESSAGE_TOKEN(mp) = am_undefined;
    erts_queue_proc_message(p, erts_trace_cleaner, 0, mp, msg);
}

int erts_is_on_load_trace_enabled(void)
{
    int ret = 0;
    ERTS_LC_ASSERT(erts_has_code_mod_permission() ||
                   erts_thr_progress_is_blocking());

    erts_rwmtx_rlock(&erts_trace_session_list_lock);
    for(ErtsTraceSession* s_p = &erts_trace_session_0; s_p; s_p = s_p->next) {
	if(s_p->on_load_trace_pattern_is_on) {
	    ret = 1;
	    break;
	}
    }
    erts_rwmtx_runlock(&erts_trace_session_list_lock);
    return ret;
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
erts_trace_flags(ErtsTraceSession *session, Eterm List,
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
        } else if (is_tuple(item) && ERTS_TRACER_IS_NIL(session->tracer)) {
            ERTS_TRACER_CLEAR(&tracer);
            tracer = erts_term_to_tracer(am_tracer, item);
            if (tracer == THE_NON_VALUE)
                goto error;
	} else goto error;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) {
        goto error;
    }
    
    if (mask)                        *pMask         = mask;
    if (!ERTS_TRACER_IS_NIL(tracer)) *pTracer       = tracer;
    if (cpu_timestamp)               *pCpuTimestamp = cpu_timestamp;
    return !0;

 error:
    if (tracer != THE_NON_VALUE)
	ERTS_TRACER_CLEAR(&tracer);
    return 0;
}

static int
start_trace(Process *c_p,
	    ErtsTraceSession* session,
	    ErtsTracer tracer,
            ErtsPTabElementCommon *common,
            int on, int mask)
{
    /* We can use the common part of both port+proc without checking what it is
       In the code below port is used for both proc and port */
    Port *port = (Port*)common;
    ErtsTracerRef* ref;

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (is_internal_pid(common->id)) {
        ERTS_LC_ASSERT(erts_proc_lc_my_proc_locks((Process*)common)
                       == ERTS_PROC_LOCKS_ALL);
    } else {
        ASSERT(is_internal_port(common->id));
        ERTS_LC_ASSERT(erts_lc_is_port_locked(port));
    }
#endif

    ref = get_tracer_ref(common, session);
    if (!ref) {
	if (on)
	    ref = new_tracer_ref(common, session);
	else
	    return 0;
    }

    if (on && !ERTS_TRACER_IS_NIL(ref->tracer)) {
	if (ref->flags & TRACEE_FLAGS
            && !ERTS_TRACER_COMPARE(ref->tracer, tracer)) {
            /* This tracee is already being traced, and not by the
             * tracer to be */
            if (erts_is_tracer_enabled(ref->tracer, common)) {
                /* The tracer is still in use */
                return 1;
            }
            /* Current tracer now invalid */
        }
    }

    if (!on) {
	ref->flags &= ~mask;
    }
    else {
	ref->flags |= mask;

        if ((mask & F_TRACE_RECEIVE) && is_internal_pid(common->id)) {
            Process *proc = (Process *) common;
            erts_aint32_t state = erts_atomic32_read_nob(&proc->state);
            if (state & ERTS_PSFLG_MSG_SIG_IN_Q)
                erts_proc_notify_new_message(proc, ERTS_PROC_LOCKS_ALL);
        }
    }
    ERTS_P_ALL_TRACE_FLAGS(port) = erts_sum_all_trace_flags(common);

    if ((ref->flags & TRACEE_FLAGS) == 0) {
	clear_tracer_ref(common, ref);
	delete_tracer_ref(common, ref);
    } else if (!ERTS_TRACER_IS_NIL(tracer))
        erts_tracer_replace(common, ref, tracer);

    return 0;
}

Eterm erts_internal_trace_3(BIF_ALIST_3)
{
    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        ERTS_BIF_YIELD3(BIF_TRAP_EXPORT(BIF_erts_internal_trace_3),
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }
    return trace(BIF_P, &erts_trace_session_0, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

Eterm erts_internal_trace_4(BIF_ALIST_4)
{
    ErtsTraceSession* session;
    Eterm ret;

    if (!term_to_session(BIF_ARG_1, &session, 0)) {
        goto session_error;
    }
    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        erts_deref_trace_session(session);
        ERTS_BIF_YIELD4(BIF_TRAP_EXPORT(BIF_erts_internal_trace_4),
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, BIF_ARG_4);
    }

    /* Double check liveness with seized code mod permission */
    if (!erts_is_trace_session_alive(session)) {
        erts_release_code_mod_permission();
        erts_deref_trace_session(session);
        goto session_error;
    }

    ret = trace(BIF_P, session, BIF_ARG_2, BIF_ARG_3, BIF_ARG_4);

    erts_deref_trace_session(session);
    return ret;

session_error:
    BIF_P->fvalue = am_session;
    BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
}

static
Eterm trace(Process* p, ErtsTraceSession *session,
            Eterm pid_spec, Eterm how, Eterm list)
{
    int on;
    ErtsTracer tracer = erts_tracer_nil;
    int matches = 0;
    Uint mask = 0;
    int cpu_ts = 0;
    int system_blocked = 0;

    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    if (! erts_trace_flags(session, list, &mask, &tracer, &cpu_ts)) {
        erts_release_code_mod_permission();
        p->fvalue = am_badopt;
	BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
    }

    switch (how) {
    case am_false: 
	on = 0; 
	break;
    case am_true: 
	on = 1;
        if (ERTS_TRACER_IS_NIL(tracer)) {
	    if (ERTS_TRACER_IS_NIL(session->tracer)) {
		tracer = erts_term_to_tracer(am_tracer, p->common.id);
	    }
	    else {
		erts_tracer_update(&tracer, session->tracer);
	    }
	}

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

        if (start_trace(p, session, tracer, &tracee_port->common, on, mask)) {
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

        if (start_trace(tracee_p, session, tracer, &tracee_p->common, on, mask)) {
	    erts_proc_unlock(tracee_p,
				 (tracee_p == p
				  ? ERTS_PROC_LOCKS_ALL_MINOR
				  : ERTS_PROC_LOCKS_ALL));
	    goto already_traced;
        }
        erts_proc_unlock(tracee_p,
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
			    
			    if (sys_get_cputime(start, tp) < 0)
				goto error;
			    start = ((SysCpuTime)tp.tv_sec * 1000000000LL) + 
				    (SysCpuTime)tp.tv_nsec;
			    for (i = 0; i < 100; i++)
				sys_get_cputime(stop, tp);
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

	    erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	    erts_thr_progress_block();
	    system_blocked = 1;
            /*
             * This will not block dirty schedulers, so we still need to lock
             * each process/port tracee when modifying their trace settings
             * below.
             */

	    ok = 1;
	    if (procs || mods) {
		int max = erts_ptab_max(&erts_proc);
		/* tracing of processes */
		for (i = 0; i < max; i++) {
		    Process* tracee_p = erts_pix2proc(i);
		    if (! tracee_p) 
			continue;
                    erts_proc_lock(tracee_p, ERTS_PROC_LOCKS_ALL);
                    if (!start_trace(p, session, tracer, &tracee_p->common, on, mask))
                        matches++;
                    erts_proc_unlock(tracee_p, ERTS_PROC_LOCKS_ALL);
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
                    erts_port_lock(tracee_port);
                    if (!start_trace(p, session, tracer, &tracee_port->common, on, mask))
                        matches++;
                    erts_port_release(tracee_port);
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
                erts_change_default_proc_tracing(session,
                    on, mask & ERTS_PROC_TRACEE_FLAGS, tracer);
            if (mask & ERTS_PORT_TRACEE_FLAGS &&
                pid_spec != am_processes && pid_spec != am_new_processes)
                erts_change_default_port_tracing(
                    session, on, mask & ERTS_PORT_TRACEE_FLAGS, tracer);

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

    if (system_blocked) {
	erts_thr_progress_unblock();
	erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
    erts_release_code_mod_permission();
    ERTS_TRACER_CLEAR(&tracer);

    BIF_RET(make_small(matches));

 already_traced:
    erts_send_error_to_logger_str(p->group_leader,
				  "** can only have one tracer per process\n");

 error:
    ERTS_TRACER_CLEAR(&tracer);

    if (system_blocked) {
	erts_thr_progress_unblock();
	erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
    erts_release_code_mod_permission();

    BIF_ERROR(p, BADARG);
}

static int
tracer_session_destructor(Binary *bin)
{
    ErtsTraceSession *s = (ErtsTraceSession*) ERTS_MAGIC_BIN_DATA(bin);

    if (erts_is_trace_session_alive(s)) {
        /*
         * trace_session_destroy() not called.
         * We must initiate cleanup here without a process.
         * We start with an aux job in order to seize the code mod permission.
         */
        Uint sched_id = erts_get_scheduler_id();
        if (!sched_id)
            sched_id = 1;
        erts_schedule_misc_aux_work(sched_id, trace_session_destroy_aux, s);
        return 0; /* don't free bin */
    }

    /*
     * Session already cleared by trace_session_destroy().
     * It must be DEAD now as it was kept alive during CLEARING with refc bump.
     * Go ahead and free it.
     */
    ASSERT(erts_atomic_read_nob(&s->state) == ERTS_TRACE_SESSION_DEAD);
    free_session(s);
    return 0; /* already freed */
}

static void free_session(ErtsTraceSession *session)
{
    Binary* bin = &(ERTS_MAGIC_BIN_FROM_DATA(session)->binary);
    ASSERT(erts_refc_read(&bin->intern.refc, 0) == 0);
    ASSERT(erts_refc_read(&session->dbg_bp_refc, 0) == 0);
    ASSERT(erts_refc_read(&session->dbg_p_refc, 0) == 0);
    ASSERT(erts_atomic_read_nob(&session->state) == ERTS_TRACE_SESSION_DEAD);
    ASSERT(ERTS_TRACER_IS_NIL(session->tracer));
    ASSERT(ERTS_TRACER_IS_NIL(session->on_spawn_proc_tracer));
    ASSERT(ERTS_TRACER_IS_NIL(session->on_open_port_tracer));
    ASSERT(ERTS_TRACER_IS_NIL(session->on_load_meta_tracer));
    erts_magic_binary_free(bin);
}


Eterm
erts_internal_trace_session_create_3(BIF_ALIST_3)
{
    return trace_session_create(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

static Eterm
trace_session_create(Process* p, Eterm name, Eterm tracer_term, Eterm opts)
{
    Binary* bptr;
    Eterm* hp;
    Eterm m_ref;
    Eterm weak_ref;
    ErtsTracer tracer;
    ErtsTraceSession* session;

    if (is_not_nil(opts) || !is_atom(name)) {
        BIF_ERROR(p, BADARG);
    }

    if (tracer_term == am_undefined) {
        tracer = erts_tracer_nil;
    }
    else {
        tracer = erts_term_to_tracer(THE_NON_VALUE, tracer_term);
        if (tracer == THE_NON_VALUE) {
            BIF_ERROR(p, BADARG);
        }
    }

    bptr = erts_create_magic_binary_x(sizeof(ErtsTraceSession),
                                      tracer_session_destructor,
                                      ERTS_ALC_T_BINARY,
                                      0);
    session = (ErtsTraceSession*) ERTS_MAGIC_BIN_DATA(bptr);
    if (!erts_trace_session_init(session, tracer, name)) {
        ASSERT(erts_refc_read(&bptr->intern.refc, 0) == 0);
        erts_bin_free(bptr);
        BIF_ERROR(p, BADARG);
    }

    hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE + ERTS_TRACE_SESSION_WEAK_REF_SZ + 3);
    m_ref = erts_mk_magic_ref(&hp, &MSO(p), bptr);
    weak_ref = erts_make_trace_session_weak_ref(session, &hp);
    return TUPLE2(hp, m_ref, weak_ref);
}

Eterm
erts_internal_trace_session_destroy_1(BIF_ALIST_1)
{
    ErtsTraceSession* session;
    if (!term_to_session(BIF_ARG_1, &session, 1)) {
        BIF_P->fvalue = am_badopt;
        BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
    }
    if (!erts_is_trace_session_alive(session)) {
        erts_deref_trace_session(session);
        BIF_RET(am_false);
    }
    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        erts_deref_trace_session(session);
        ERTS_BIF_YIELD1(BIF_TRAP_EXPORT(BIF_erts_internal_trace_session_destroy_1),
                        BIF_P, BIF_ARG_1);
    }
    if (erts_atomic_cmpxchg_nob(&session->state,
                                ERTS_TRACE_SESSION_CLEARING,
                                ERTS_TRACE_SESSION_ALIVE)
        == ERTS_TRACE_SESSION_ALIVE) {

        ASSERT(!erts_staging_trace_session);
        erts_staging_trace_session = session;

        trace_session_destroy(session);

        finish_bp.stager = BIF_P;
        erts_schedule_code_barrier(&finish_bp.barrier, smp_bp_finisher, NULL);

        erts_proc_inc_refc(BIF_P);
        erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
        ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
    }
    else {
        erts_release_code_mod_permission();
        erts_deref_trace_session(erts_staging_trace_session);
    }
    BIF_RET(am_false);
}

static void trace_session_destroy_aux(void *session_v)
{
    ErtsTraceSession *s = (ErtsTraceSession*) session_v;

    if (!erts_try_seize_code_mod_permission_aux(trace_session_destroy_aux,
                                                session_v)) {
        return;
    }

    erts_lc_soften_code_mod_permission_check();

    /*
     * We came here from destructor as refc==0 and session was ALIVE.
     * No one else could have done anything to the session.
     */
    ASSERT(erts_atomic_read_nob(&s->state) == ERTS_TRACE_SESSION_ALIVE);
    erts_atomic_set_nob(&s->state, ERTS_TRACE_SESSION_CLEARING);

    ASSERT(!erts_staging_trace_session);
    erts_staging_trace_session = s;

    trace_session_destroy(s);

    finish_bp.stager = NULL;
    erts_schedule_code_barrier(&finish_bp.barrier, smp_bp_finisher, NULL);
}

static void
trace_session_destroy(ErtsTraceSession* session)
{
    erts_rwmtx_rwlock(&erts_trace_session_list_lock);
    ASSERT(session->prev);
#ifdef DEBUG
    {
        ErtsTraceSession *s;
        for (s = erts_trace_session_0.next; s; s = s->next) {
            if (s == session)
                break;
        }
        ASSERT(s);
    }
#endif
    session->prev->next = session->next;
    if (session->next){
        session->next->prev = session->prev;
    }
    session->next = NULL;
    session->prev = NULL;

    erts_rwmtx_rwunlock(&erts_trace_session_list_lock);

    ERTS_TRACER_CLEAR(&session->tracer);
    ERTS_TRACER_CLEAR(&session->on_spawn_proc_tracer);
    ERTS_TRACER_CLEAR(&session->on_open_port_tracer);
    clear_on_load_trace_pattern(session);

    prepare_clear_all_trace_pattern(session);
}

/*
 * Return information about a process or an external function being traced.
 */
Eterm trace_info_2(BIF_ALIST_2)
{
    Eterm ret;
    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        ERTS_BIF_YIELD2(BIF_TRAP_EXPORT(BIF_trace_info_2),
                        BIF_P, BIF_ARG_1, BIF_ARG_2);
    }
    ret = trace_info(BIF_P, &erts_trace_session_0, BIF_ARG_1, BIF_ARG_2);
    erts_release_code_mod_permission();
    return ret;
}

/* Called by erlang:trace_info/2
 *           trace:info/3
 *           trace:session_info/1
 */
Eterm erts_internal_trace_info_3(BIF_ALIST_3)
{
    ErtsTraceSession* session;
    Eterm ret;

    if (BIF_ARG_1 == am_any) {
        /* trace:session_info */
        session = NULL;
    }
    else if (!term_to_session(BIF_ARG_1, &session, 0)) {
        goto session_error;
    }

    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        if (session) {
            erts_deref_trace_session(session);
        }
        ERTS_BIF_YIELD3(BIF_TRAP_EXPORT(BIF_erts_internal_trace_info_3),
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    /* Double check session liveness with seized code mod permission */
    if (session && !erts_is_trace_session_alive(session)) {
        erts_release_code_mod_permission();
        erts_deref_trace_session(session);
        goto session_error;
    }

    ret = trace_info(BIF_P, session, BIF_ARG_2, BIF_ARG_3);
    erts_release_code_mod_permission();
    if (session) {
        erts_deref_trace_session(session);
    }
    return ret;

session_error:
    BIF_P->fvalue = am_session;
    BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
}

static
Eterm trace_info(Process* p, ErtsTraceSession* session, Eterm What, Eterm Key)
{
    Eterm res = THE_NON_VALUE;

    if (session) {
        if (What == am_on_load) {
            res = trace_info_on_load(p, session, Key);
        } else if (What == am_send || What == am_receive) {
            res = trace_info_event(p, session, What, Key);
        } else if (is_atom(What) || is_pid(What) || is_port(What)) {
            res = trace_info_pid(p, session, What, Key);
        } else if (is_tuple(What)) {
            res = trace_info_func(p, session, What, Key);
        } else {
            goto badopt;
        }
    }
    else {
        if (is_atom(What)) {
            res = trace_info_sessions(p, What, Key);
        } else if (is_pid(What) || is_port(What)) {
            res = trace_info_pid(p, NULL, What, Key);
        } else if (is_tuple(What)) {
            res = trace_info_func_sessions(p, What, Key);
        } else {
            goto badopt;
        }
    }

    if (is_value(res) && is_internal_ref(res)) {
        BIF_TRAP1(erts_await_result, p, res);
    }
    BIF_RET(res);

badopt:
    p->fvalue = am_badopt;
    BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
}

static Eterm
build_trace_flags_term(Eterm **hpp, Uint *szp, Uint32 trace_flags)
{

#define ERTS_TFLAG__(F, FN)                             \
    if (trace_flags & F) {                              \
        if (szp)                                        \
            sz += 2;                                    \
        if (hp) {                                       \
            res = CONS(hp, FN, res);                    \
            hp += 2;                                    \
        }                                               \
    }

    Eterm res;
    Uint sz = 0;
    Eterm *hp;

    if (hpp) {
        hp = *hpp;
        res = NIL;
    }
    else {
        hp = NULL;
        res = THE_NON_VALUE;
    }

    ERTS_TFLAG__(F_NOW_TS, am_timestamp);
    ERTS_TFLAG__(F_STRICT_MON_TS, am_strict_monotonic_timestamp);
    ERTS_TFLAG__(F_MON_TS, am_monotonic_timestamp);
    ERTS_TFLAG__(F_TRACE_SEND, am_send);
    ERTS_TFLAG__(F_TRACE_RECEIVE, am_receive);
    ERTS_TFLAG__(F_TRACE_SOS, am_set_on_spawn);
    ERTS_TFLAG__(F_TRACE_CALLS, am_call);
    ERTS_TFLAG__(F_TRACE_PROCS, am_procs);
    ERTS_TFLAG__(F_TRACE_SOS1, am_set_on_first_spawn);
    ERTS_TFLAG__(F_TRACE_SOL, am_set_on_link);
    ERTS_TFLAG__(F_TRACE_SOL1, am_set_on_first_link);
    ERTS_TFLAG__(F_TRACE_SCHED, am_running);
    ERTS_TFLAG__(F_TRACE_SCHED_EXIT, am_exiting);
    ERTS_TFLAG__(F_TRACE_GC, am_garbage_collection);
    ERTS_TFLAG__(F_TRACE_ARITY_ONLY, am_arity);
    ERTS_TFLAG__(F_TRACE_RETURN_TO, am_return_to);
    ERTS_TFLAG__(F_TRACE_SILENT, am_silent);
    ERTS_TFLAG__(F_TRACE_SCHED_NO, am_scheduler_id);
    ERTS_TFLAG__(F_TRACE_PORTS, am_ports);
    ERTS_TFLAG__(F_TRACE_SCHED_PORTS, am_running_ports);
    ERTS_TFLAG__(F_TRACE_SCHED_PROCS, am_running_procs);

    if (szp)
        *szp += sz;

    if (hpp)
        *hpp = hp;

    return res;

#undef ERTS_TFLAG__
}

static Eterm
build_trace_sessions_term(Eterm **hpp, Uint *szp, ErtsPTabElementCommon *t_p)
{
    ErtsTracerRef *ref;
    Eterm res;
    Uint sz = 0;
    Eterm *hp;

    if (hpp) {
        hp = *hpp;
        res = NIL;
    }
    else {
        hp = NULL;
        res = THE_NON_VALUE;
    }

    for (ref = t_p->tracee.first_ref; ref; ref = ref->next) {
        if (erts_is_trace_session_alive(ref->session)) {
            sz += ERTS_TRACE_SESSION_WEAK_REF_SZ + 2;
            if (hp) {
                Eterm weak_ref = erts_make_trace_session_weak_ref(ref->session, &hp);
                res = CONS(hp, weak_ref, res);
                hp += 2;
            }
        }
    }

    if (szp)
        *szp += sz;

    if (hpp)
        *hpp = hp;

    return res;

#undef ERTS_TFLAG__
}


typedef struct {
    Eterm key;
    ErtsTraceSession* session;
} ErtsTraceInfoProcReq;

static Eterm
trace_info_tracee(Process *c_p, void *arg, int *redsp, ErlHeapFragment **bpp)
{
    ErtsTraceSession *session;
    ErtsTracerRef *ref;
    ErtsTracer tracer = NIL;
    Uint32 flags = 0;
    Eterm key;
    ErlHeapFragment *bp;
    Eterm *hp, res;
    Uint sz;

    {
        ErtsTraceInfoProcReq *tipr = (ErtsTraceInfoProcReq*) arg;
        session = tipr->session;
        key = tipr->key;
        erts_free(ERTS_ALC_T_TRACE_INFO_REQ, tipr);
    }

    *redsp = 1;

    if (ERTS_PROC_IS_EXITING(c_p)) {
        if (session) {
            erts_deref_trace_session(session);
        }
        return am_undefined;
    }

    sz = 3;

    if (session) {
        ref = get_tracer_ref(&c_p->common, session);
        if (ref && erts_is_tracer_ref_proc_enabled(c_p, ERTS_PROC_LOCK_MAIN,
                                                   &c_p->common, ref)) {
            tracer = ref->tracer;
            flags = ref->flags;
        }
        erts_deref_trace_session(session);
    }

    switch (key) {
    case am_tracer:
        erts_build_tracer_to_term(NULL, NULL, &sz, tracer);
        bp = new_message_buffer(sz);
        hp = bp->mem;
        res = erts_build_tracer_to_term(&hp, &bp->off_heap,
                                        NULL, tracer);
        if (res == am_false)
            res = NIL;
        break;

    case am_flags:
        build_trace_flags_term(NULL, &sz, flags);
        bp = new_message_buffer(sz);
        hp = bp->mem;
        res = build_trace_flags_term(&hp, NULL, flags);
        break;

    case am_session:
        build_trace_sessions_term(NULL, &sz, &c_p->common);
        bp = new_message_buffer(sz);
        hp = bp->mem;
        res = build_trace_sessions_term(&hp, NULL, &c_p->common);
        break;

    default:

        ERTS_INTERNAL_ERROR("Key not supported");
        res = NIL;
        bp = NULL;
        hp = NULL;
        break;
    }

    *redsp += 2;

    res = TUPLE2(hp, key, res);
    *bpp = bp;
    return res;
}

static Eterm
trace_info_pid(Process* p, ErtsTraceSession* session, Eterm pid_spec, Eterm key)
{
    Eterm tracer = NIL;
    Uint32 trace_flags = 0;
    Eterm* hp;
    Eterm res_term;

    ERTS_UNDEF(res_term, THE_NON_VALUE);

    if (pid_spec == am_new || pid_spec == am_new_processes) {
        ErtsTracer def_tracer;
	erts_get_on_spawn_tracing(session, &trace_flags, &def_tracer);
        tracer = erts_tracer_to_term(p, def_tracer);
        ERTS_TRACER_CLEAR(&def_tracer);
    } else if (pid_spec == am_new_ports) {
        ErtsTracer def_tracer;
	erts_get_on_open_port_tracing(session, &trace_flags, &def_tracer);
        tracer = erts_tracer_to_term(p, def_tracer);
        ERTS_TRACER_CLEAR(&def_tracer);
    } else if (is_internal_port(pid_spec)) {
        ErtsTracerRef *ref;
        Port *port = erts_id2port_sflgs(pid_spec, p, ERTS_PROC_LOCK_MAIN,
                                        ERTS_PORT_SFLGS_INVALID_LOOKUP);

        if (!port)
            return am_undefined;

        if (session) {
            ref = get_tracer_ref(&port->common, session);
            if (ref && erts_is_tracer_ref_proc_enabled(NULL, 0, &port->common, ref)) {
                tracer = erts_tracer_to_term(p, ref->tracer);
                trace_flags = ref->flags;
            }
        }
        else {
            Uint sz = 0;
            build_trace_sessions_term(NULL, &sz, &port->common);
            hp = HAllocX(p, sz, 3);
            res_term = build_trace_sessions_term(&hp, NULL, &port->common);
        }
        erts_port_release(port);

    } else if (is_internal_pid(pid_spec)) {
        ErtsTraceInfoProcReq *tipr;
        Eterm ref;

        if (session) {
            if (key != am_flags && key != am_tracer)
                goto error;
        } else {
            if (key != am_session)
                goto error;
        }

        tipr = erts_alloc(ERTS_ALC_T_TRACE_INFO_REQ, sizeof(ErtsTraceInfoProcReq));
        tipr->key = key;
        tipr->session = session;
        if (session) {
            erts_ref_trace_session(session);
        }
        ref = erts_proc_sig_send_rpc_request(p, pid_spec, !0,
                                             trace_info_tracee,
                                             tipr);

        if (is_non_value(ref)) {
            erts_free(ERTS_ALC_T_TRACE_INFO_REQ, tipr);
            return am_undefined;
        }

        return ref;
    } else if (is_external_pid(pid_spec)
	       && external_pid_dist_entry(pid_spec) == erts_this_dist_entry) {
	    return am_undefined;
    } else {
        p->fvalue = am_badopt;
        BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
    }

    if (key == am_flags && session) {
        Uint sz = 0;
        Eterm *hp;

        build_trace_flags_term(NULL, &sz, trace_flags);

        hp = HAllocX(p, sz, 3);

        res_term = build_trace_flags_term(&hp, NULL, trace_flags);

    } else if (key == am_tracer && session) {
        if (tracer == am_false)
            tracer = NIL;
        res_term = tracer;
    } else if (key == am_session && !session) {
        ASSERT(is_list(res_term) || is_nil(res_term));
    } else {
    error:
        BIF_ERROR(p, BADARG);
    }
    hp = HAlloc(p, 3);
    return TUPLE2(hp, key, res_term);
}

#define FUNC_TRACE_NOEXIST      0
#define FUNC_TRACE_UNTRACED     (1<<0)
#define FUNC_TRACE_GLOBAL_TRACE (1<<1)
#define FUNC_TRACE_LOCAL_TRACE  (1<<2)
#define FUNC_TRACE_META_TRACE   (1<<3)
#define FUNC_TRACE_COUNT_TRACE  (1<<4)
#define FUNC_TRACE_TIME_TRACE   (1<<5)
#define FUNC_TRACE_MEMORY_TRACE (1<<6)
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
 * If the return value contains FUNC_TRACE_TIME_TRACE, *call_time is set.
 * If the return value contains FUNC_TRACE_MEMORY_TRACE, *call_memory is set.
 */
static int function_is_traced(Process *p,
                              ErtsTraceSession *session,
                              const ErtsCodeMFA *mfa,
			      Binary    **ms,              /* out */
			      Binary    **ms_meta,         /* out */
			      ErtsTracer *tracer_pid_meta, /* out */
			      Uint       *count,           /* out */
			      Eterm      *call_time,       /* out */
                              Eterm      *call_memory)     /* out */
{
    const ErtsCodeInfo *ci;
    Export e;
    Export* ep;

    /* First look for an export entry */
    e.info.mfa = *mfa;
    if ((ep = export_get(&e)) != NULL) {
	if (erts_is_export_trampoline_active(ep, erts_active_code_ix()) &&
	    ! BeamIsOpCode(ep->trampoline.common.op, op_call_error_handler)) {

	    ASSERT(BeamIsOpCode(ep->trampoline.common.op, op_i_generic_breakpoint));

	    if (erts_is_trace_break(session, &ep->info, ms, 0)) {
		return FUNC_TRACE_GLOBAL_TRACE;
	    }

            ASSERT(!erts_is_trace_break(session, &ep->info, ms, 1));
            ASSERT(!erts_is_mtrace_break(session, &ep->info, ms_meta, tracer_pid_meta));
            ASSERT(!erts_is_call_break(p, session, 1, &ep->info, call_time));
            ASSERT(!erts_is_call_break(p, session, 0, &ep->info, call_memory));
	}
    }
    
    /* OK, now look for breakpoint tracing */
    if ((ci = erts_find_local_func(mfa)) != NULL) {
	int r = 0;
        if (erts_is_trace_break(session, ci, ms, 1))
            r |= FUNC_TRACE_LOCAL_TRACE;
        if (erts_is_mtrace_break(session, ci, ms_meta, tracer_pid_meta))
            r |= FUNC_TRACE_META_TRACE;
        if (erts_is_count_break(session, ci, count))
            r |= FUNC_TRACE_COUNT_TRACE;
        if (erts_is_call_break(p, session, 1, ci, call_time))
            r |= FUNC_TRACE_TIME_TRACE;
        if (erts_is_call_break(p, session, 0, ci, call_memory))
            r |= FUNC_TRACE_MEMORY_TRACE;
	
	return r ? r : FUNC_TRACE_UNTRACED;
    } 
    return FUNC_TRACE_NOEXIST;
}

static Eterm function_traced_by_sessions(Process *p, const ErtsCodeMFA *mfa)
{
    const ErtsCodeInfo *ci;
    Export e;
    Export* ep;
    ErtsHeapFactory factory;
    Eterm list = NIL;

    ci = erts_find_local_func(mfa);
    if (!ci) {
        /* Function does not exists */
        return am_undefined;
    }

    erts_factory_proc_init(&factory, p);

    /* First look for an export entry */
    e.info.mfa = *mfa;
    ep = export_get(&e);
    if (ep) {
        if (erts_is_export_trampoline_active(ep, erts_active_code_ix()) &&
            ! BeamIsOpCode(ep->trampoline.common.op, op_call_error_handler)) {

	    ASSERT(BeamIsOpCode(ep->trampoline.common.op, op_i_generic_breakpoint));

            list = erts_make_bp_session_list(&factory, &ep->info, list);
	}
    }

    /* OK, now look for local breakpoint tracing */
    if (ci) {
        list = erts_make_bp_session_list(&factory, ci, list);
    }

    erts_factory_close(&factory);
    return list;
}

static int get_mfa_tuple(Eterm func_spec, ErtsCodeMFA* mfa)
{
    Eterm* tp;

    if (!is_tuple(func_spec)) {
        return 0;
    }
    tp = tuple_val(func_spec);
    if (tp[0] != make_arityval(3)) {
        return 0;
    }
    if (!is_atom(tp[1]) || !is_atom(tp[2]) || !is_small(tp[3])) {
        return 0;
    }
    mfa->module = tp[1];
    mfa->function = tp[2];
    mfa->arity = signed_val(tp[3]);
    return 1;
}

static Eterm
trace_info_func(Process* p, ErtsTraceSession* session,
                Eterm func_spec, Eterm key)
{
    Eterm* hp;
    ErtsCodeMFA mfa;
    Binary *ms = NULL, *ms_meta = NULL;
    Uint count = 0;
    Eterm traced = am_false;
    Eterm match_spec = am_false;
    Eterm retval = am_false;
    ErtsTracer meta = erts_tracer_nil;
    Eterm call_time = NIL;
    Eterm call_memory = NIL;
    int r;

    ASSERT(session);

    if (!get_mfa_tuple(func_spec, &mfa)) {
        goto error;
    }

    if (key == am_call_time || key == am_call_memory || key == am_all) {
        erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
        erts_thr_progress_block();
        erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
    erts_mtx_lock(&erts_dirty_bp_ix_mtx);

    r = function_is_traced(p, session, &mfa, &ms, &ms_meta, &meta, &count,
                           &call_time, &call_memory);

    erts_mtx_unlock(&erts_dirty_bp_ix_mtx);
    if ( (key == am_call_time) || (key == am_call_memory) || (key == am_all)) {
	erts_thr_progress_unblock();
    }

    switch (r) {
    case FUNC_TRACE_NOEXIST:
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_undefined);
    case FUNC_TRACE_UNTRACED:
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
    case am_call_memory:
	if (r & FUNC_TRACE_MEMORY_TRACE) {
	    retval = call_memory;
	}
	break;
    case am_all: {
        Eterm match_spec_meta = am_false;
        Eterm call_count = am_false;
        Eterm t, m;
	
        /* ToDo: Rewrite this to loop and reuse the above cases */

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
            call_count = erts_make_integer(count, p);
	}
	if (!(r & FUNC_TRACE_TIME_TRACE)) {
            call_time = am_false;
	}
        if (!(r & FUNC_TRACE_MEMORY_TRACE)) {
            call_memory = am_false;
	}

        m = erts_tracer_to_term(p, meta);

	hp = HAlloc(p, (3+2)*7);
	retval = NIL;
	t = TUPLE2(hp, am_call_count, call_count); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_call_time, call_time); hp += 3;
        retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_call_memory, call_memory); hp += 3;
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
    hp = HAlloc(p, 3);
    return TUPLE2(hp, key, retval);

 error:
    BIF_ERROR(p, BADARG);
}

static Eterm
trace_info_func_sessions(Process* p, Eterm func_spec, Eterm key)
{
    Eterm* hp;
    ErtsCodeMFA mfa;
    Eterm session_list;

    if (!get_mfa_tuple(func_spec, &mfa) || key != am_session) {
        BIF_ERROR(p, BADARG);
    }

    erts_mtx_lock(&erts_dirty_bp_ix_mtx);

    session_list = function_traced_by_sessions(p, &mfa);

    erts_mtx_unlock(&erts_dirty_bp_ix_mtx);

    hp = HAlloc(p, 3);
    return TUPLE2(hp, am_session, session_list);
}

static Eterm
trace_info_on_load(Process* p, ErtsTraceSession *session, Eterm key)
{
    Eterm* hp;
    if (! session->on_load_trace_pattern_is_on) {
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_false);
    }
    switch (key) {
    case am_traced:
	{
	    Eterm traced = am_false;
	    
	    if (! session->on_load_trace_pattern_flags.breakpoint) {
		traced = am_global;
	    } else if (session->on_load_trace_pattern_flags.local) {
		traced = am_local;
	    }
	    hp = HAlloc(p, 3);
	    return TUPLE2(hp, key, traced);
	}
    case am_match_spec:
	{
	    Eterm match_spec = am_false;
	    
	    if ((! session->on_load_trace_pattern_flags.breakpoint) ||
		session->on_load_trace_pattern_flags.local) {
		if (session->on_load_match_spec) {
		    match_spec = MatchSetGetSource(session->on_load_match_spec);
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
	if (session->on_load_trace_pattern_flags.meta) {
            ASSERT(!ERTS_TRACER_IS_NIL(session->on_load_meta_tracer));
	    return TUPLE2(hp, key, erts_tracer_to_term(p, session->on_load_meta_tracer));
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_meta_match_spec:
	{
	    Eterm match_spec = am_false;
	    
	    if (session->on_load_trace_pattern_flags.meta) {
		if (session->on_load_meta_match_spec) {
		    match_spec = 
			MatchSetGetSource(session->on_load_meta_match_spec);
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
	if (session->on_load_trace_pattern_flags.call_count) {
	    return TUPLE2(hp, key, am_true);
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_call_time:
	hp = HAlloc(p, 3);
	if (session->on_load_trace_pattern_flags.call_time) {
	    return TUPLE2(hp, key, am_true);
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_call_memory:
	hp = HAlloc(p, 3);
	if (session->on_load_trace_pattern_flags.call_memory) {
	    return TUPLE2(hp, key, am_true);
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_all:
	{
	    Eterm match_spec = am_false, meta_match_spec = am_false, r = NIL, t, m;
	    
	    if (session->on_load_trace_pattern_flags.local ||
		(! session->on_load_trace_pattern_flags.breakpoint)) {
		match_spec = NIL;
	    }
	    if (session->on_load_match_spec) {
		match_spec = MatchSetGetSource(session->on_load_match_spec);
		match_spec = copy_object(match_spec, p);
	    }
	    if (session->on_load_trace_pattern_flags.meta) {
		meta_match_spec = NIL;
	    }
	    if (session->on_load_meta_match_spec) {
		meta_match_spec = 
		    MatchSetGetSource(session->on_load_meta_match_spec);
		meta_match_spec = copy_object(meta_match_spec, p);
	    }
            m = (session->on_load_trace_pattern_flags.meta
                 ? erts_tracer_to_term(p, session->on_load_meta_tracer) : am_false);
	    hp = HAlloc(p, (3+2)*5 + 3);
	    t = TUPLE2(hp, am_call_count, 
		       (session->on_load_trace_pattern_flags.call_count
			? am_true : am_false)); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta_match_spec, meta_match_spec); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta, m); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_match_spec, match_spec); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_traced,
		       (! session->on_load_trace_pattern_flags.breakpoint ?
			am_global : (session->on_load_trace_pattern_flags.local ?
				     am_local : am_false))); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    return TUPLE2(hp, key, r);
	}
    default:
	BIF_ERROR(p, BADARG);
    }
}

static Eterm
trace_info_sessions(Process* p, Eterm What, Eterm key)
{
    const ErtsBpIndex bp_ix = erts_active_bp_ix();
    ErtsTraceSession *s;
    ErtsHeapFactory factory;
    Eterm *hp;
    Eterm list = NIL;
    Eterm ret;

    if (key != am_session) {
        BIF_ERROR(p, BADARG);
    }

    erts_factory_proc_init(&factory, p);
    erts_rwmtx_rlock(&erts_trace_session_list_lock);
    for (s = &erts_trace_session_0; s; s = s->next) {
        int on;
        switch (What) {
        case am_any:       on = 1; break;
        case am_on_load:   on = s->on_load_trace_pattern_is_on; break;
        case am_send:      on = s->send_tracing[bp_ix].on; break;
        case am_receive:   on = s->receive_tracing[bp_ix].on; break;
        case am_new_processes:
        case am_new:       on = s->on_spawn_proc_trace_flags; break;
        case am_new_ports: on = s->on_open_port_trace_flags; break;
        default:
            erts_rwmtx_runlock(&erts_trace_session_list_lock);
            erts_factory_undo(&factory);
            p->fvalue = am_badopt;
            BIF_ERROR(p, BADARG | EXF_HAS_EXT_INFO);
        }
        if (on) {
            Eterm weak_ref;
            hp = erts_produce_heap(&factory, ERTS_TRACE_SESSION_WEAK_REF_SZ + 2, 30);
            weak_ref = erts_make_trace_session_weak_ref(s, &hp);
            list = CONS(hp, weak_ref, list);
        }
    }
    erts_rwmtx_runlock(&erts_trace_session_list_lock);

    hp = erts_produce_heap(&factory, 3, 0);
    ret = TUPLE2(hp, am_session, list);
    erts_factory_close(&factory);
    return ret;
}

static Eterm
trace_info_event(Process* p, ErtsTraceSession* session, Eterm event, Eterm key)
{
    ErtsTracingEvent* te;
    Eterm retval;
    Eterm* hp;

    switch (event) {
    case am_send: te = session->send_tracing;    break;
    case am_receive: te = session->receive_tracing; break;
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

void change_on_load_trace_pattern(ErtsTraceSession *s,
                                  enum erts_break_op on,
                                  struct trace_pattern_flags flags,
                                  Binary* match_prog_set,
                                  ErtsTracer meta_tracer)
{
    if (flags.local || (! flags.breakpoint)) {
        MatchSetUnref(s->on_load_match_spec);
        s->on_load_match_spec = match_prog_set;
        MatchSetRef(s->on_load_match_spec);
    }
    if (flags.meta) {
        MatchSetUnref(s->on_load_meta_match_spec);
        s->on_load_meta_match_spec = match_prog_set;
        MatchSetRef(s->on_load_meta_match_spec);
        erts_tracer_update(&s->on_load_meta_tracer, meta_tracer);
    } else if (! flags.breakpoint) {
        MatchSetUnref(s->on_load_meta_match_spec);
        s->on_load_meta_match_spec = NULL;
        ERTS_TRACER_CLEAR(&s->on_load_meta_tracer);
    }
    if (s->on_load_trace_pattern_flags.breakpoint &&
        flags.breakpoint) {
        /* Breakpoint trace -> breakpoint trace */
        ASSERT(s->on_load_trace_pattern_is_on);
        if (on) {
            s->on_load_trace_pattern_flags.local
                |= flags.local;
            s->on_load_trace_pattern_flags.meta
                |= flags.meta;
            s->on_load_trace_pattern_flags.call_count
                |= (on == 1) ? flags.call_count : 0;
            s->on_load_trace_pattern_flags.call_time
                |= (on == 1) ? flags.call_time : 0;
            s->on_load_trace_pattern_flags.call_memory
                |= (on == 1) ? flags.call_memory : 0;
        } else {
            s->on_load_trace_pattern_flags.local
                &= ~flags.local;
            s->on_load_trace_pattern_flags.meta
                &= ~flags.meta;
            s->on_load_trace_pattern_flags.call_count
                &= ~flags.call_count;
            s->on_load_trace_pattern_flags.call_time
                &= ~flags.call_time;
            s->on_load_trace_pattern_flags.call_memory
                &= ~flags.call_memory;
            if (! (s->on_load_trace_pattern_flags.breakpoint =
                   s->on_load_trace_pattern_flags.local |
                   s->on_load_trace_pattern_flags.meta |
                   s->on_load_trace_pattern_flags.call_count |
                   s->on_load_trace_pattern_flags.call_time |
                   s->on_load_trace_pattern_flags.call_memory)) {
                s->on_load_trace_pattern_is_on = !!on; /* i.e off */
            }
        }
    } else if (! s->on_load_trace_pattern_flags.breakpoint &&
               ! flags.breakpoint) {
        /* Global call trace -> global call trace */
        s->on_load_trace_pattern_is_on = !!on;
    } else if (s->on_load_trace_pattern_flags.breakpoint &&
               ! flags.breakpoint) {
        /* Breakpoint trace -> global call trace */
        if (on) {
            s->on_load_trace_pattern_flags = flags; /* Struct copy */
            s->on_load_trace_pattern_is_on = !!on;
        }
    } else {
        ASSERT(! s->on_load_trace_pattern_flags.breakpoint &&
               flags.breakpoint);
        /* Global call trace -> breakpoint trace */
        if (on) {
            if (on != 1) {
                flags.call_count = 0;
                flags.call_time  = 0;
                flags.call_memory  = 0;
            }
            flags.breakpoint = flags.local | flags.meta | flags.call_count |
                flags.call_time| flags.call_memory;
            s->on_load_trace_pattern_flags = flags; /* Struct copy */
            s->on_load_trace_pattern_is_on = !!flags.breakpoint;
        }
    }
}

static void clear_on_load_trace_pattern(ErtsTraceSession *s)
{
    s->on_load_trace_pattern_is_on = 0;
    s->on_load_trace_pattern_flags = erts_trace_pattern_flags_off;
    MatchSetUnref(s->on_load_match_spec);
    s->on_load_match_spec = NULL;
    MatchSetUnref(s->on_load_meta_match_spec);
    s->on_load_meta_match_spec = NULL;
    ERTS_TRACER_CLEAR(&s->on_load_meta_tracer);
}


int
erts_set_trace_pattern(ErtsCodeMFA *mfa, int specified,
		       Binary* match_prog_set, Binary *meta_match_prog_set,
		       int on, struct trace_pattern_flags flags,
		       ErtsTracer meta_tracer, int is_blocking)
{
    const ErtsCodeIndex code_ix = erts_active_code_ix();
    Uint i, n, matches;
    BpFunction* fp;

    erts_bp_match_export(&finish_bp.e, mfa, specified);

    fp = finish_bp.e.matching;
    n = finish_bp.e.matched;
    matches = 0;

    for (i = 0; i < n; i++) {
        ErtsCodeInfo *ci_rw;
        Export* ep;

        /* Export entries are always writable, discard const. */
        ci_rw = (ErtsCodeInfo *)fp[i].code_info;
        ep = ErtsContainerStruct(ci_rw, Export, info);

        if (ep->bif_number != -1) {
            ep->is_bif_traced = !!on;
        }

        if (on && !flags.breakpoint) {
            /* Turn on global call tracing */
            if (!erts_is_export_trampoline_active(ep, code_ix)) {
                fp[i].mod->curr.num_traced_exports++;
#if defined(DEBUG) && !defined(BEAMASM)
                ep->info.u.op = BeamOpCodeAddr(op_i_func_info_IaaI);
#endif
                ep->trampoline.breakpoint.op = BeamOpCodeAddr(op_i_generic_breakpoint);
                ep->trampoline.breakpoint.address =
                    (BeamInstr) ep->dispatch.addresses[code_ix];
            }
            erts_set_export_trace(ci_rw, match_prog_set);

	} else if (!on && flags.breakpoint) {
	    /* Turn off breakpoint tracing -- nothing to do here. */
	} else {
	    /*
	     * Turn off global tracing, either explicitly or implicitly
	     * before turning on breakpoint tracing.
	     */
            erts_clear_export_trace(ci_rw);
	}
    }

    /*
    ** So, now for code breakpoint tracing
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
            if (flags.call_memory) {
		erts_set_memory_break(&finish_bp.f, on);
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
        if (flags.call_memory) {
	    erts_clear_memory_break(&finish_bp.f);
	}
    }

    finish_bp.current = 0;
    finish_bp.install = on;
    finish_bp.local = flags.breakpoint;
    finish_bp.global = !flags.breakpoint;

    if (is_blocking) {
	ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
	while (erts_finish_breakpointing()) {
	    /* Empty loop body */
	}
	finish_bp.current = -1;
    }

    if (flags.breakpoint) {
	matches += finish_bp.f.matched;
    } else {
	matches += finish_bp.e.matched;
    }
    return matches;
}

static void
prepare_clear_all_trace_pattern(ErtsTraceSession* session)
{
    Uint i, n;
    BpFunction* fp;

    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    /*
     * Clear all breakpoints in export entries for session
     */
    erts_bp_match_export(&finish_bp.e, NULL, 0);

    fp = finish_bp.e.matching;
    n = finish_bp.e.matched;

    for (i = 0; i < n; i++) {
        ErtsCodeInfo *ci_rw;
        Export* ep;

        /* Export entries are always writable, discard const. */
        ci_rw = (ErtsCodeInfo *)fp[i].code_info;
        ep = ErtsContainerStruct(ci_rw, Export, info);

        if (ep->bif_number != -1) {
            ep->is_bif_traced = 0; // ToDo: multi sessions?
        }
        erts_clear_export_trace(ci_rw);
    }

    /*
     * Clear all breakpoints in code for session
     */
    erts_bp_match_functions(&finish_bp.f, NULL, 0);
    erts_clear_all_breaks(&finish_bp.f);

    clear_event_trace(erts_staging_trace_session->send_tracing);
    clear_event_trace(erts_staging_trace_session->receive_tracing);

    finish_bp.current = 0;
    finish_bp.install = 0;
    finish_bp.local = 1;
    finish_bp.global = 1;
}

static void clear_event_trace(ErtsTracingEvent *et)
{
    const ErtsBpIndex ix = erts_staging_bp_ix();

    MatchSetUnref(et[ix].match_spec);
    et[ix].match_spec = NULL;
    et[ix].on = 0;
}

static void set_event_trace(ErtsTracingEvent *et, Binary* match_spec)
{
    const ErtsBpIndex ix = erts_staging_bp_ix();

    MatchSetUnref(et[ix].match_spec);
    et[ix].match_spec = match_spec;
    et[ix].on = 1;
    MatchSetRef(match_spec);
}


static int
stage_trace_event_pattern(Eterm event, Binary* match_spec, int on)
{
    ErtsTracingEvent* et;

    switch (event) {
    case am_send: et = erts_staging_trace_session->send_tracing; break;
    case am_receive: et = erts_staging_trace_session->receive_tracing; break;
    default: return -1;
    }

    if (on) {
        set_event_trace(et, match_spec);
    }
    else {
        clear_event_trace(et);
    }

    finish_bp.current = 0;
    finish_bp.install = on;
    finish_bp.local = 0;
    finish_bp.global = 0;
    finish_bp.e.matched = 0;
    finish_bp.e.matching = NULL;
    finish_bp.f.matched = 0;
    finish_bp.f.matching = NULL;

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
    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    /*
     * Memory and instruction barriers will be issued for all schedulers
     * *before* each of the stages below. (Unless the other schedulers
     * are blocked, in which case memory barriers will be issued when
     * they are awakened.)
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
                ASSERT(!finish_bp.global);
		erts_install_breakpoints(&finish_bp.f);
                return 1;
	    }
            if (finish_bp.global) {
                install_exp_breakpoints(&finish_bp.e);
                return 1;
	    }
            /* Neither local or global set for event tracing */
	}
        /* Nothing to do here. Fall through to next stage. */
        finish_bp.current++;
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
                ASSERT(!finish_bp.global);
		uninstall_exp_breakpoints(&finish_bp.e);
	    }
            if (finish_bp.global) {
                erts_uninstall_breakpoints(&finish_bp.f);
            }
	} else {
            /* Both local and global can be set when a session is cleared */
	    if (finish_bp.local) {
		erts_uninstall_breakpoints(&finish_bp.f);
	    }
            if (finish_bp.global) {
		uninstall_exp_breakpoints(&finish_bp.e);
	    }
	}
        if (finish_bp.local || finish_bp.global) {
            return 1;
        }
        /* Nothing done here. Fall through to next stage. */
        finish_bp.current++;
    case 3:
	/*
	 * Now all breakpoints have either been inserted or removed.
	 * For all updated breakpoints, copy the active breakpoint
	 * data to the staged breakpoint data to make them equal
	 * (simplifying for the next time breakpoints are to be
	 * updated).  If any breakpoints have been totally disabled,
	 * deallocate the GenericBp structs for them.
	 */
	clean_export_entries(&finish_bp.e);
	erts_consolidate_export_bp_data(&finish_bp.e);
	erts_consolidate_local_bp_data(&finish_bp.f);
	erts_bp_free_matched_functions(&finish_bp.e);
	erts_bp_free_matched_functions(&finish_bp.f);
        consolidate_event_tracing(erts_staging_trace_session->send_tracing);
        consolidate_event_tracing(erts_staging_trace_session->receive_tracing);
        return 1;
    case 4:
        /* All schedulers have run a code barrier (or will as soon as they
         * awaken) after updating all breakpoints, it's safe to return now. */
	erts_free_breakpoints();
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
        /* Export entries are always writable, discard const. */
        ErtsCodeInfo *ci_rw = (ErtsCodeInfo*)fp[i].code_info;
        Export* ep = ErtsContainerStruct(ci_rw, Export, info);
        erts_activate_export_trampoline(ep, code_ix);

	erts_install_additional_session_bp(ci_rw);
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
        /* Export entries are always writable, discard const. */
        ErtsCodeInfo *ci_rw = (ErtsCodeInfo*)fp[i].code_info;
        Export* ep = ErtsContainerStruct(ci_rw, Export, info);

        if (erts_is_export_trampoline_active(ep, code_ix)
            && erts_sum_all_session_flags(ci_rw) == 0) {

            ASSERT(BeamIsOpCode(ep->trampoline.common.op, op_i_generic_breakpoint));
            ep->dispatch.addresses[code_ix] =
                (ErtsCodePtr)ep->trampoline.breakpoint.address;
        }
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
        /* Export entries are always writable, discard const. */
        ErtsCodeInfo *ci_rw = (ErtsCodeInfo*)fp[i].code_info;
        Export* ep = ErtsContainerStruct(ci_rw, Export, info);

        if (erts_is_export_trampoline_active(ep, code_ix)) {
            continue;
        }

        if (BeamIsOpCode(ep->trampoline.common.op, op_i_generic_breakpoint)) {
            ep->trampoline.breakpoint.op = (BeamInstr) 0;
            ep->trampoline.breakpoint.address = (BeamInstr) 0;
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
        new_seq_trace_token(p, 0);
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
        new_seq_trace_token(p, is_not_immed(arg2));
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
        new_seq_trace_token(p, 0);
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

static void
new_seq_trace_token(Process* p, int ensure_new_heap)
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
    else if (ensure_new_heap) {
        Eterm *mature = p->abandoned_heap ? p->abandoned_heap : p->heap;
        Uint mature_size = p->high_water - mature;
        Eterm* tpl = tuple_val(SEQ_TRACE_TOKEN(p));
        ASSERT(arityval(tpl[0]) == 5);
        if (ErtsInBetween(tpl, OLD_HEAP(p), OLD_HEND(p)) ||
            ErtsInArea(tpl, mature, mature_size*sizeof(Eterm))) {
            hp = HAlloc(p, 6);
            sys_memcpy(hp, tpl, 6*sizeof(Eterm));
            SEQ_TRACE_TOKEN(p) = make_tuple(hp);
        }
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
	if ((item == am_send) || (item == am_spawn) ||
        (item == am_receive) || (item == am_print)
        || (item == am_timestamp)
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
    seq_trace_update_serial(BIF_P);
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
    if (!EQ(BIF_ARG_1, SEQ_TRACE_TOKEN_LABEL(BIF_P)))
	BIF_RET(am_false);
    seq_trace_update_serial(BIF_P);
    seq_trace_output(SEQ_TRACE_TOKEN(BIF_P), BIF_ARG_2, 
		     SEQ_TRACE_PRINT, NIL, BIF_P);
    BIF_RET(am_true);
}

void erts_system_monitor_clear(Process *c_p) {
    if (c_p) {
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_thr_progress_block();
    }
    erts_set_system_monitor(NIL);
    erts_system_monitor_long_gc = 0;
    erts_system_monitor_long_schedule = 0;
    erts_system_monitor_large_heap = 0;
    erts_system_monitor_flags.busy_port = 0;
    erts_system_monitor_flags.busy_dist_port = 0;
    erts_system_monitor_long_msgq_on = ERTS_SWORD_MAX;
    erts_system_monitor_long_msgq_off = -1;
    if (c_p) {
	erts_thr_progress_unblock();
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
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
        Eterm long_msgq_off = NIL;
        Eterm long_msgq_on = NIL;

        if (erts_system_monitor_long_msgq_off >= 0) {
            ASSERT(erts_system_monitor_long_msgq_on
                   > erts_system_monitor_long_msgq_off);
	    hsz += 2+3+3;
	    (void) erts_bld_uint(NULL, &hsz,
                                 (Sint) erts_system_monitor_long_msgq_off);
	    (void) erts_bld_uint(NULL, &hsz,
                                 (Sint) erts_system_monitor_long_msgq_on);
        }
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
        if (erts_system_monitor_long_msgq_off >= 0) {
	    long_msgq_off = erts_bld_uint(&hp, NULL,
                                          (Sint) erts_system_monitor_long_msgq_off);
	    long_msgq_on =  erts_bld_uint(&hp, NULL,
                                          (Sint) erts_system_monitor_long_msgq_on);
        }
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
        if (long_msgq_off != NIL) {
	    Eterm t;
            ASSERT(long_msgq_on != NIL);
            t = TUPLE2(hp, long_msgq_off, long_msgq_on); hp += 3;
            t = TUPLE2(hp, am_long_message_queue, t); hp += 3;
	    res = CONS(hp, t, res); hp += 2;
        }
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
        Sint long_msgq_on, long_msgq_off;
	int busy_port, busy_dist_port;

	system_blocked = 1;
	erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_thr_progress_block();
        erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);

	if (!erts_pid2proc(p, ERTS_PROC_LOCK_MAIN, monitor_pid, 0))
	    goto error;

	for (long_gc = 0, long_schedule = 0, large_heap = 0, 
		 busy_port = 0, busy_dist_port = 0,
                 long_msgq_on = ERTS_SWORD_MAX, long_msgq_off = -1;
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
                } else if (tp[1] == am_long_message_queue) {
                    if (!is_tuple_arity(tp[2], 2)) goto error;
                    tp = tuple_val(tp[2]);
		    if (!term_to_Sint(tp[1], &long_msgq_off)) goto error;
		    if (!term_to_Sint(tp[2], &long_msgq_on)) goto error;
                    if (long_msgq_off < 0) goto error;
                    if (long_msgq_on <= 0) goto error;
                    if (long_msgq_off >= long_msgq_on) goto error;
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
        erts_system_monitor_long_msgq_off = long_msgq_off;
        erts_system_monitor_long_msgq_on = long_msgq_on;

	erts_thr_progress_unblock();
	BIF_RET(prev);
    }

 error:

    if (system_blocked) {
	erts_thr_progress_unblock();
    }

    BIF_ERROR(p, BADARG);
}

/* Begin: Trace for System Profiling */

void erts_system_profile_clear(Process *c_p) {
    if (c_p) {
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_thr_progress_block();
    }
    erts_set_system_profile(NIL);
    erts_system_profile_flags.scheduler = 0;
    erts_system_profile_flags.runnable_procs = 0;
    erts_system_profile_flags.runnable_ports = 0;
    erts_system_profile_flags.exclusive = 0;
    if (c_p) {
	erts_thr_progress_unblock();
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
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
	
	erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_thr_progress_block();

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
	erts_thr_progress_unblock();
	erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);
	
	BIF_RET(prev);
		
    }

    error:
	if (system_blocked) {
	    erts_thr_progress_unblock();
	    erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);
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
    erts_atomic32_t refc;
} ErtsTraceDeliveredAll;

static void
reply_trace_delivered_all(void *vtdarp)
{
    ErtsTraceDeliveredAll *tdarp = (ErtsTraceDeliveredAll *) vtdarp;

    if (erts_atomic32_dec_read_nob(&tdarp->refc) == 0) {
        Eterm ref_copy, msg;
        Process *rp = tdarp->proc;
        Eterm *hp = NULL;
        ErlOffHeap *ohp;
        ErlHeapFragment *bp;
        bp = new_message_buffer(4 + NC_HEAP_SIZE(tdarp->ref));
        hp = &bp->mem[0];
        ohp = &bp->off_heap;

        ref_copy = STORE_NC(&hp, ohp, tdarp->ref);
        msg = TUPLE3(hp, am_trace_delivered, tdarp->target, ref_copy);

        erts_send_sys_msg_proc(rp->common.id, rp->common.id, msg, bp);

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
        erts_atomic32_init_nob(&tdarp->refc,
                                   (erts_aint32_t) erts_no_schedulers);
        erts_proc_add_refc(BIF_P, 1);
        erts_schedule_multi_misc_aux_work(0,
                                          1,
                                          erts_no_schedulers,
                                          reply_trace_delivered_all,
                                          (void *) tdarp);
        BIF_RET(ref);
    } else {
        BIF_ERROR(BIF_P, BADARG);
    }
}
