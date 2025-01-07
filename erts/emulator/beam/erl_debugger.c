/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "bif.h"
#include "erl_debugger.h"
#include "erl_map.h"

static erts_rwmtx_t debugger_rwmtx;

Uint erts_debugger_flags = /* -D: enable debugger, -Dxxxx for features */
    (~ERTS_DEBUGGER_ENABLED & ERTS_DEBUGGER_LINE_BREAKPOINTS);

#define BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT() do { \
    if (!ERTS_DEBUGGER_IS_ENABLED(0)) {         \
        BIF_ERROR(BIF_P, EXC_UNDEF);            \
    }                                           \
} while(0)

/* Protected by debugger lock.  */
static Eterm debugger_pid = NIL;
static Uint32 debugger_ref = 0;


void erts_init_debugger(void)
{
    erts_rwmtx_init(&debugger_rwmtx, "debugger", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DEBUG);
}

/* Capabilities */

BIF_RETTYPE
erl_debugger_supported_0(BIF_ALIST_0) {
    int supported = ERTS_DEBUGGER_IS_ENABLED(0);
    BIF_RET(supported ? am_true : am_false);
}

BIF_RETTYPE
erl_debugger_instrumentations_0(BIF_ALIST_0)
{
    Eterm *hp;
    int line_bp = ERTS_DEBUGGER_IS_ENABLED(ERTS_DEBUGGER_LINE_BREAKPOINTS);

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    hp = HAlloc(BIF_P, MAP1_SZ);
    return MAP1(hp, am_line_breakpoint, line_bp ? am_true : am_false);
}

BIF_RETTYPE
erl_debugger_toggle_instrumentations_1(BIF_ALIST_1)
{
    const int instr_count = 1;
    const struct {Eterm key; Uint flag;} instrumentations[] = {
        {am_line_breakpoint, ERTS_DEBUGGER_LINE_BREAKPOINTS},
    };

    Eterm toggles;
    int count_ok = 0;
    int new_flags;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    erts_rwmtx_rwlock(&debugger_rwmtx);
    new_flags = erts_debugger_flags;

    toggles = BIF_ARG_1;
    if (!is_map(toggles)) {
        goto badarg;
    }

    for(int i=0; i < instr_count; i++) {
        const Eterm *val = erts_maps_get(instrumentations[i].key, toggles);
        if (val) {
            if (*val == am_true) {
                new_flags |= instrumentations[i].flag;
            } else if (*val == am_false) {
                new_flags &= ~instrumentations[i].flag;
            } else {
                goto badarg;
            }
            count_ok++;
        }
    }

    if (count_ok != erts_map_size(toggles)) {
        goto badarg;
    }

    erts_debugger_flags = new_flags;
    erts_rwmtx_rwunlock(&debugger_rwmtx);

    return am_ok;

    badarg: {
        erts_rwmtx_rwunlock(&debugger_rwmtx);
        BIF_ERROR(BIF_P, BADARG);
    }
}

/* Debugger registration */

BIF_RETTYPE
erl_debugger_register_1(BIF_ALIST_1)
{
    Eterm result_tag, result_val;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    if (is_not_internal_pid(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    erts_rwmtx_rwlock(&debugger_rwmtx);

    if (is_internal_pid(debugger_pid) && erts_proc_lookup(debugger_pid)) {
        result_tag = am_error;
        result_val = am_already_exists;
        goto end;
    }

    debugger_pid = BIF_ARG_1;
    debugger_ref = erts_sched_local_random(debugger_ref);

    result_tag = am_ok;
    result_val = make_small(debugger_ref);

     end: {
        Eterm *hp = HAlloc(BIF_P, 3);
        Eterm result = TUPLE2(hp, result_tag, result_val);

        erts_rwmtx_rwunlock(&debugger_rwmtx);
        BIF_RET(result);
    }
}

BIF_RETTYPE
erl_debugger_unregister_2(BIF_ALIST_2)
{
    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    if (is_not_internal_pid(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_small(BIF_ARG_2)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    erts_rwmtx_rwlock(&debugger_rwmtx);

    if (debugger_pid == BIF_ARG_1) {
        if (make_small(debugger_ref) != BIF_ARG_2) {
            erts_rwmtx_rwunlock(&debugger_rwmtx);
            BIF_ERROR(BIF_P, BADARG);
        }

        debugger_pid = NIL;
    }

    erts_rwmtx_rwunlock(&debugger_rwmtx);
    BIF_RET(am_ok);
}


BIF_RETTYPE
erl_debugger_whereis_0(BIF_ALIST_0)
{
    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    if (is_internal_pid(debugger_pid) && erts_proc_lookup(debugger_pid)) {
        BIF_RET(debugger_pid);
    }

    BIF_RET(am_undefined);
}

/* Debugger events */

int
erts_send_debugger_event(Process *c_p, Eterm event)
{
    Process *debugger = NULL;
    int event_sent = 0;
    ErtsProcLocks initial_locks = 0, locks = 0;

    erts_rwmtx_rlock(&debugger_rwmtx);

    if (debugger_pid == NIL) {
        goto end;
    }

    debugger = erts_proc_lookup(debugger_pid);
    if (debugger) {
        Eterm *hp, event_copy, msg;
        Uint event_sz;
        ErtsMessage *mp;
        ErlOffHeap *ohp;

        if (c_p == debugger) {
            locks = initial_locks = ERTS_PROC_LOCK_MAIN;
        }

        event_sz = is_immed(event) ? 0 : size_object(event);
        mp = erts_alloc_message_heap(debugger, &locks,
                                     4 + event_sz,
                                     &hp, &ohp);
        ERL_MESSAGE_TOKEN(mp) = am_undefined;

        event_copy = is_immed(event) ? event : copy_struct(event, event_sz,
                                                           &hp, ohp);
        msg = TUPLE3(hp,
                     am_debugger_event, make_small(debugger_ref),
                     event_copy);

        erts_queue_proc_message(debugger, debugger, locks, mp, msg);
        event_sent = 1;
    }

    end:{
        ErtsProcLocks acquired_locks = locks & ~initial_locks;
        if (debugger && acquired_locks) {
            erts_proc_unlock(debugger, acquired_locks);
        }

        erts_rwmtx_runlock(&debugger_rwmtx);
        return event_sent;
    }
}

/* Line breakpoints */

BIF_RETTYPE
erl_debugger_breakpoint_3(BIF_ALIST_3) {
    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    /* TO BE IMPLEMENTED */
    BIF_ERROR(BIF_P, EXC_UNDEF);
}

BIF_RETTYPE
erts_internal_notify_breakpoint_hit_3(BIF_ALIST_3) {
    Eterm mfa, *mfav, line, resume_fun;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    mfa = BIF_ARG_1;
    if (is_not_tuple_arity(BIF_ARG_1, 3)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    mfav = tuple_val(mfa);
    if (is_not_atom(mfav[1]) || is_not_atom(mfav[2]) || is_not_small(mfav[3])) {
        BIF_ERROR(BIF_P, BADARG);
    }

    line = BIF_ARG_2;
    if (is_not_small(line)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    resume_fun = BIF_ARG_3;
    if (is_not_any_fun(resume_fun) ||
        fun_arity((ErlFunThing*) fun_val(resume_fun)) != 0 ) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (BIF_P->common.id == debugger_pid) {
        BIF_RET(am_abort);
    } else {
        Eterm pid = BIF_P->common.id;
        Eterm *hp = HAlloc(BIF_P, 6);

        Eterm bp_event = TUPLE5(hp, am_breakpoint, pid, mfa, line, resume_fun);

        if (!erts_send_debugger_event(BIF_P, bp_event)) {
            BIF_RET(am_noproc);
        }
    }

    BIF_RET(am_ok);
}
