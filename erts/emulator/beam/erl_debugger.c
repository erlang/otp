/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
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
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif


#include "global.h"
#include "beam_bp.h"
#include "beam_catches.h"
#include "beam_common.h"
#include "bif.h"
#include "big.h"
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

/* Protected by code modification permission */
static struct {
    ErtsCodeBarrier barrier;
    Process* process;

    Module *module;
    ErtsCodePtr first_target;
    unsigned int search_next_from;
    unsigned int line;
    int enable;
    int stage;
} finish_line_bp;

static void line_breakpoint_finisher(void *ignored)
{
    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    (void)ignored;

    if (finish_line_bp.stage++ == 0) {
        struct erl_module_instance *mi = &finish_line_bp.module->curr;
        const BeamCodeHeader *code_hdr = finish_line_bp.module->curr.code_hdr;
        ErtsCodePtr cp_exec = finish_line_bp.first_target;
        unsigned int start_from = finish_line_bp.search_next_from;
        unsigned int line = finish_line_bp.line;

        do {
            enum erts_is_line_breakpoint curr = erts_is_line_breakpoint_code(cp_exec);
            if (finish_line_bp.enable && curr == IS_DISABLED_LINE_BP) {
                erts_install_line_breakpoint(mi, cp_exec);
            } else if (!finish_line_bp.enable && curr == IS_ENABLED_LINE_BP) {
                erts_uninstall_line_breakpoint(mi, cp_exec);
            }
            cp_exec = erts_find_next_code_for_line(code_hdr,
                                                   line,
                                                   &start_from);
        } while (cp_exec);

        erts_schedule_code_barrier(&finish_line_bp.barrier,
                                   line_breakpoint_finisher, NULL);
    } else {
        Process* p = finish_line_bp.process;

        erts_release_code_mod_permission();

        erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
        if (!ERTS_PROC_IS_EXITING(p)) {
            erts_resume(p, ERTS_PROC_LOCK_STATUS);
        }
        erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
        erts_proc_dec_refc(p);
    }
}

BIF_RETTYPE
erl_debugger_breakpoint_3(BIF_ALIST_3) {
    Eterm module_name, line_term, enable;
    int line, found_at_least_once = 0;
    Eterm error_type, error_source;
    const BeamCodeHeader *code_hdr;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    module_name = BIF_ARG_1;
    if (is_not_atom(module_name)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    line_term = BIF_ARG_2;
    if (is_not_small(line_term)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    line = signed_val(line_term);

    if (line <= 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    enable = BIF_ARG_3;
    if (enable != am_true && enable != am_false) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (!erts_try_seize_code_mod_permission(BIF_P)) {
        ERTS_BIF_YIELD3(BIF_TRAP_EXPORT(BIF_erl_debugger_breakpoint_3),
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    finish_line_bp.process = BIF_P;
    finish_line_bp.enable = (enable == am_true);
    finish_line_bp.stage = 0;
    finish_line_bp.module = erts_get_module(module_name,
                                            erts_active_code_ix());
    finish_line_bp.line = line;
    finish_line_bp.search_next_from = 0;

    if (!finish_line_bp.module) {
        error_type = am_badkey, error_source = module_name;
        goto error;
    }

    code_hdr = finish_line_bp.module->curr.code_hdr;
    if (!code_hdr) {
        error_type = am_badkey, error_source = module_name;
        goto error;
    }

    if (!ERTS_DEBUGGER_IS_ENABLED_IN(code_hdr->debugger_flags,
                                     ERTS_DEBUGGER_LINE_BREAKPOINTS)) {
        error_type = am_unsupported, error_source = module_name;
        goto error;
    }

    do {
        finish_line_bp.first_target = erts_find_next_code_for_line(code_hdr,
                                                                   line,
                                                                   &finish_line_bp.search_next_from);
        found_at_least_once |= !!(finish_line_bp.first_target);
    } while (finish_line_bp.first_target &&
             !erts_is_line_breakpoint_code(finish_line_bp.first_target));

    if (!finish_line_bp.first_target) {
        if (found_at_least_once) {
            error_type = am_unsupported, error_source = line_term;
        } else {
            error_type = am_badkey, error_source = line_term;
        }
        goto error;
    }

    erts_schedule_code_barrier(&finish_line_bp.barrier,
                               line_breakpoint_finisher, NULL);
    erts_proc_inc_refc(BIF_P);
    erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
    ERTS_BIF_YIELD_RETURN(BIF_P, am_ok);

    {
        Eterm *hp1, *hp2;
    error:
        erts_release_code_mod_permission();

        hp1 = HAlloc(BIF_P, 6);
        hp2 = hp1 + 3;
        return TUPLE2(hp2, am_error, TUPLE2(hp1, error_type, error_source));
    }
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

/* Inspecting stack-frames and X registers */

/* A replacement for erts_inspect_frame() since here we traverse
 * the stack in the opposite direction as done everywhere else.
 */
static ERTS_INLINE void maybe_skip_fp(Eterm **sp) {
    if (ERTS_UNLIKELY(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA)) {
        (*sp)--; /* This puts us on the frame-pointer, to be skipped */
        ASSERT(cp_val(*sp[0]) == NULL || *sp < (Eterm*)cp_val(*sp[0]));
    }
}

static Process*
suspended_proc_lock(Eterm pid, ErtsProcLocks locks) {
    erts_aint32_t state;
    erts_aint32_t fail_state = ERTS_PSFLG_FREE | ERTS_PSFLG_RUNNING;
    Process *rp = erts_proc_lookup_raw(pid);

    if (!rp) {
        return NULL;
    }

    state = erts_atomic32_read_nob(&rp -> state);
    if (state & fail_state) {
        return NULL;
    }

    if (!(state & ERTS_PSFLG_SUSPENDED)) {
        return NULL;
    }

    erts_proc_lock(rp, locks);
    state = erts_atomic32_read_nob(&rp -> state);

    if (!(state & ERTS_PSFLG_SUSPENDED)) {
        erts_proc_unlock(rp, locks);
        rp = NULL;
    }

    return rp;
}

static Eterm
stack_frame_fun_info(Process *c_p, ErtsCodePtr pc, Process *rp, int is_return_addr) {
    Eterm fun_info;
    FunctionInfo fi;

    if (!is_return_addr) {
        if (pc != beam_run_process) {
            erts_lookup_function_info(&fi, pc, 1);
        } else {
            ERTS_ASSUME(rp);
            fi.mfa = rp->current;
            fi.loc = LINE_INVALID_LOCATION;
        }
    } else {
        ErtsCodePtr return_address = pc;
        ErtsCodePtr approx_caller_addr;

        ASSERT(pc != beam_run_process);

#ifdef BEAMASM
        /* Some instructions can be shorter than one word (e.g. call in x86_64),
         * so we subtract just one byte from the return address to avoid
         * over-shooting the caller.
         * */
        approx_caller_addr = ((char*)return_address) - 1;
#else
        approx_caller_addr = ((char*)return_address) - sizeof(UWord);
#endif

        erts_lookup_function_info(&fi, approx_caller_addr, 1);
    }

    if (fi.mfa == NULL) {
        const char *fname = erts_internal_fun_description_from_pc(pc);
        fun_info = am_atom_put(fname, sys_strlen(fname));
    } else {
        Eterm *hp, mfa, line;
        int mfa_arity = 3;

        hp = HAlloc(c_p, MAP2_SZ + (mfa_arity + 1));

        mfa = make_tuple(hp);
        *hp++ = make_arityval(mfa_arity);
        *hp++ = fi.mfa->module;
        *hp++ = fi.mfa->function;
        *hp++ = make_small(fi.mfa->arity);

        line = fi.loc == LINE_INVALID_LOCATION
                ? am_undefined
                : make_small(LOC_LINE(fi.loc));

        fun_info = MAP2(hp, am_function, mfa, am_line, line);
    }

    return fun_info;
}

static Eterm
make_value_or_too_large_tuple(Process *p, Eterm val, Uint max_size) {
    Uint val_size;
    int tup_arity = 2;
    Eterm result, *hp;

    hp = HAlloc(p, tup_arity + 1);
    result = make_tuple(hp);
    *hp++ = make_arityval(tup_arity);

    val_size = size_object(val);
    if (val_size <= max_size) {
        *hp++ = am_value;
        *hp++ = copy_object(val, p);
    } else {
        *hp++ = ERTS_MAKE_AM("too_large");
        *hp++ = make_small(val_size);
    }

    return result;
}

static Eterm
make_catch_tuple(Process *c_p, ErtsCodePtr catch_addr) {
    int tup_arity = 2;
    Eterm result, *hp;

    hp = HAlloc(c_p, tup_arity + 1);
    result = make_tuple(hp);
    *hp++ = make_arityval(tup_arity);

    *hp++ = ERTS_MAKE_AM("catch");
    *hp++ = stack_frame_fun_info(c_p, catch_addr, NULL, 0);

    return result;
}

BIF_RETTYPE
erl_debugger_stack_frames_2(BIF_ALIST_2)
{
    Eterm pid;
    Process *rp = NULL;
    int frame_no, max_term_size = -1;
    Eterm *stack_top;
    Eterm result = NIL, yregs;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    pid = BIF_ARG_1;
    if (is_not_internal_pid(pid)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (pid == BIF_P->common.id) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_2)) {
        max_term_size = signed_val(BIF_ARG_2);
    }

    if (max_term_size < 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    rp = suspended_proc_lock(pid, ERTS_PROC_LOCK_MAIN);
    if (!rp) {
        BIF_RET(am_running);
    }

    stack_top = rp->stop;
#ifndef BEAMASM
    /* On emu, the top word on the stack is NIL,
     * reserved for the CP when calling another function */
    if (*stack_top == NIL) {
        stack_top++;
    }
#endif

    frame_no = 0;
    yregs = NIL;
    for(Eterm *sp = STACK_START(rp) - 1; stack_top - 1 <= sp; sp--) {
        int is_last_iter = (stack_top - 1 == sp);
        int tup_arity;
        Eterm *hp, x;

        /* On the last iteration, past the stack end, x is the current pc,
         * so we get the location of the current stack-frame. */
        x = is_last_iter ? (Eterm) rp->i : *sp;

        if (is_CP(x)) {
            int is_return_addr = !is_last_iter;
            int frame_info_map_sz;
            ErtsCodePtr code_ptr = cp_val(x);
            Eterm this_frame, frame_info_map, addr;

            if (!is_last_iter) {
                maybe_skip_fp(&sp);
            }

            addr = erts_make_integer((Uint) code_ptr, BIF_P);

            tup_arity = 3;
            frame_info_map_sz = MAP2_SZ;
            hp = HAlloc(BIF_P,
                        2 /* cons */ +
                        (tup_arity + 1) /* this_frame */ +
                        frame_info_map_sz /* frame_info_map */);

            frame_info_map = MAP2(hp, am_code, addr, am_slots, yregs);
            hp += frame_info_map_sz;

            this_frame = make_tuple(hp);
            *hp++ = make_arityval(tup_arity);
            *hp++ = make_small(frame_no++);
            *hp++ = stack_frame_fun_info(BIF_P, code_ptr, rp, is_return_addr);
            *hp++ = frame_info_map;

            result = CONS(hp, this_frame, result);

            yregs = NIL;
        } else {
            Eterm yreg_info;

            if (is_catch(x)) {
                yreg_info = make_catch_tuple(BIF_P, catch_pc(x));
            } else {
                yreg_info = make_value_or_too_large_tuple(BIF_P, x, max_term_size);
            }

            hp = HAlloc(BIF_P, 2 /* cons */);
            yregs = CONS(hp, yreg_info, yregs);
        }
    }

    erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

    return result;
}

BIF_RETTYPE
erl_debugger_peek_stack_frame_slot_4(BIF_ALIST_4)
{
    Eterm pid;
    Process *rp = NULL;
    int frame_no = -1, yreg_no = -1, max_term_size = -1;
    int current_frame, yreg_count;
    Eterm *stack_top;
    Eterm result = am_undefined;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    pid = BIF_ARG_1;
    if (is_not_internal_pid(pid)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (pid == BIF_P->common.id) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_2)) {
        frame_no = signed_val(BIF_ARG_2);
    }

    if (is_small(BIF_ARG_3)) {
        yreg_no = signed_val(BIF_ARG_3);
    }

    if (is_small(BIF_ARG_4)) {
        max_term_size = signed_val(BIF_ARG_4);
    }

    if (frame_no < 0 || yreg_no < 0 || max_term_size < 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    rp = suspended_proc_lock(pid, ERTS_PROC_LOCK_MAIN);
    if (!rp) {
        BIF_RET(am_running);
    }

    stack_top = rp->stop;
#ifndef BEAMASM
    /* On emu, the top word on the stack is NIL,
     * reserved for the CP when calling another function */
    if (*stack_top == NIL) {
        stack_top++;
    }
#endif

    current_frame = 0, yreg_count = 0;
    for(Eterm *sp = STACK_START(rp) - 1; stack_top - 1 <= sp; sp--) {
        Eterm x;

        /* On the last iteration, past the stack end, x is the current pc,
         * so we get the location of the current stack-frame. */
        x = stack_top <= sp ? *sp : (Eterm) rp->i;

        if (is_not_CP(x)) {
            yreg_count++;
        } else if (current_frame != frame_no) {
            current_frame++;
            yreg_count = 0;

            maybe_skip_fp(&sp);
        } else if (yreg_no >= yreg_count) {
            result = am_undefined;
            break;
        } else {
            Eterm val = sp[yreg_no + 1];

            if (is_catch(val)) {
                result = make_catch_tuple(BIF_P, catch_pc(val));
            } else {
                result = make_value_or_too_large_tuple(BIF_P, val, max_term_size);
            }

            break;
        }
    }

    erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

    return result;
}

BIF_RETTYPE
erl_debugger_xregs_count_1(BIF_ALIST_1) {
    Eterm result, pid;
    Process *rp = NULL;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    pid = BIF_ARG_1;
    if (is_not_internal_pid(pid)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (pid == BIF_P->common.id) {
        BIF_ERROR(BIF_P, BADARG);
    }

    rp = suspended_proc_lock(pid, ERTS_PROC_LOCK_MAIN);
    if (!rp) {
        BIF_RET(am_running);
    }

    result = make_small(rp->arity);
    erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

    return result;
}

BIF_RETTYPE
erl_debugger_peek_xreg_3(BIF_ALIST_3)
{
    Eterm result, pid;
    int xreg_no, max_term_size;
    Process *rp = NULL;

    BIF_UNDEF_IF_NO_DEBUGGER_SUPPORT();

    pid = BIF_ARG_1;
    if (is_not_internal_pid(pid)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (pid == BIF_P->common.id) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_2)) {
        xreg_no = signed_val(BIF_ARG_2);
        if (xreg_no < 0) {
            BIF_ERROR(BIF_P, BADARG);
        }
    } else {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_3)) {
        max_term_size = signed_val(BIF_ARG_3);
        if (max_term_size < 0) {
            BIF_ERROR(BIF_P, BADARG);
        }
    } else {
        BIF_ERROR(BIF_P, BADARG);
    }

    rp = suspended_proc_lock(pid, ERTS_PROC_LOCK_MAIN);
    if (!rp) {
        BIF_RET(am_running);
    }

    if (xreg_no >= (int) rp->arity) {
        result = am_undefined;
    } else {
        Eterm val = rp->arg_reg[xreg_no];
        result = make_value_or_too_large_tuple(BIF_P, val, max_term_size);
    }

    erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
    return result;
}
