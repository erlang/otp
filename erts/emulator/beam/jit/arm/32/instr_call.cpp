/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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

#include "beam_asm.hpp"

extern "C"
{
#include "beam_common.h"
}

void BeamGlobalAssembler::emit_dispatch_return() {
    a.mov(ARG3, a32::lr);
    mov_imm(TMP, 0);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, current)));
    mov_imm(TMP, 1);
    a.strb(TMP, arm::Mem(c_p, offsetof(Process, arity)));
    a.b(labels[context_switch_simplified]);
}

void BeamModuleAssembler::emit_dispatch_return() {
#ifdef JIT_HARD_DEBUG
    /* Validate return address and {x,0} */
    emit_validate(ArgWord(1));
#endif

    if (erts_alcu_enable_code_atags) {
        /* See emit_i_test_yield. */
        a.str(a32::lr, arm::Mem(c_p, offsetof(Process, i)));
    }

    /* The reduction test is kept in module code because moving it to a shared
     * fragment caused major performance regressions in dialyzer. */
    a.subs(FCALLS, FCALLS, imm(1));
    a.b_le(resolve_fragment(ga->get_dispatch_return(), disp32MB));

    a.bx(a32::lr);

    mark_unreachable();
}

void BeamModuleAssembler::emit_return() {
    emit_leave_erlang_frame();
    emit_dispatch_return();
}

void BeamModuleAssembler::emit_move_deallocate_return() {
    // TODO
    emit_nyi("emit_move_deallocate_return");
}

void BeamModuleAssembler::emit_i_call(const ArgLabel &CallTarget) {
    erlang_call(resolve_beam_label(CallTarget, disp32MB));
}

void BeamModuleAssembler::emit_i_call_last(const ArgLabel &CallTarget,
                                           const ArgWord &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_call_only(CallTarget);
}

void BeamModuleAssembler::emit_move_call_last(const ArgYRegister &Src,
                                              const ArgRegister &Dst,
                                              const ArgLabel &CallTarget,
                                              const ArgWord &Deallocate) {
    // TODO: Optimize this, see arm64 implementation...
    mov_arg(Dst, Src);
    emit_deallocate(Deallocate);
    emit_i_call_only(CallTarget);
}

void BeamModuleAssembler::emit_i_call_only(const ArgLabel &CallTarget) {
    emit_leave_erlang_frame();
    a.b(resolve_beam_label(CallTarget, disp32MB));
    mark_unreachable();
}

/* Handles save_calls for remote calls. When the active code index is
 * ERTS_SAVE_CALLS_CODE_IX, all remote calls will land here.
 *
 * Export entry is in ARG1, return address is in LR (x30). Both of these must
 * be preserved since this runs between caller and callee. */
void BeamGlobalAssembler::emit_dispatch_save_calls_export() {
    // TODO
    emit_nyi("emit_dispatch_save_calls_export");
}

void BeamModuleAssembler::emit_i_call_ext(const ArgExport &Exp) {
    mov_arg(ARG1, Exp);

    arm::Mem target = emit_setup_dispatchable_call(ARG1);
    erlang_call(target);
}

void BeamModuleAssembler::emit_i_call_ext_only(const ArgExport &Exp) {
    mov_arg(ARG1, Exp);

    arm::Mem target = emit_setup_dispatchable_call(ARG1);
    emit_leave_erlang_frame();
    branch(target);
    mark_unreachable();
}

void BeamModuleAssembler::emit_i_call_ext_last(const ArgExport &Exp,
                                               const ArgWord &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_call_ext_only(Exp);
}

void BeamModuleAssembler::emit_move_call_ext_last(const ArgYRegister &Src,
                                                  const ArgRegister &Dst,
                                                  const ArgExport &Exp,
                                                  const ArgWord &Deallocate) {
    // TODO
    emit_nyi("emit_move_call_ext_last");
}

static ErtsCodeMFA apply3_mfa = {am_erlang, am_apply, 3};

arm::Mem BeamModuleAssembler::emit_variable_apply(bool includeI) {
    Label dispatch = a.newLabel(), entry = a.newLabel();

    a.bind(entry);

    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);

    if (includeI) {
        a.adr(ARG3, entry);
    } else {
        mov_imm(ARG3, 0);
    }

    mov_imm(ARG4, 0);

    comment("apply()");
    // Using the basic runtime_call instead of the BeamModuleAssembler version
    // allows to skip veneer management
    BeamAssembler::runtime_call<4>(apply);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.tst(ARG1, ARG1);
    a.b_ne(dispatch);
    emit_raise_exception(entry, &apply3_mfa);

    a.bind(dispatch);
    return emit_setup_dispatchable_call(ARG1);
}

void BeamModuleAssembler::emit_i_apply() {
    arm::Mem target = emit_variable_apply(false);
    erlang_call(target);
}

void BeamModuleAssembler::emit_i_apply_last(const ArgWord &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_apply_only();
}

void BeamModuleAssembler::emit_i_apply_only() {
    arm::Mem target = emit_variable_apply(true);

    emit_leave_erlang_frame();
    branch(target);
    mark_unreachable();
}

arm::Mem BeamModuleAssembler::emit_fixed_apply(const ArgWord &Arity,
                                               bool includeI) {
    Label dispatch = a.newLabel(), entry = a.newLabel();

    a.bind(entry);

    mov_arg(ARG3, Arity);

    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);

    if (includeI) {
        a.adr(ARG4, entry);
    } else {
        mov_imm(ARG4, 0);
    }

    mov_imm(TMP, 0);
    a.sub(a32::sp, a32::sp, 8);
    a.str(TMP, arm::Mem(a32::sp));
    runtime_call<5>(fixed_apply);
    a.add(a32::sp, a32::sp, 8);

    /* We will need to reload all X registers in case there has been
     * an error. */
    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    emit_branch_if_value(ARG1, dispatch);
    emit_raise_exception(entry, &apply3_mfa);

    a.bind(dispatch);

    return emit_setup_dispatchable_call(ARG1);
}

void BeamModuleAssembler::emit_apply(const ArgWord &Arity) {
    arm::Mem target = emit_fixed_apply(Arity, false);
    erlang_call(target);
}

void BeamModuleAssembler::emit_apply_last(const ArgWord &Arity,
                                          const ArgWord &Deallocate) {
    emit_deallocate(Deallocate);

    arm::Mem target = emit_fixed_apply(Arity, true);

    emit_leave_erlang_frame();
    branch(target);
    mark_unreachable();
}
