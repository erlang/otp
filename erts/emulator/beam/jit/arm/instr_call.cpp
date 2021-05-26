/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
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
    a.mov(ARG3, a64::x30);
    a.str(ZERO, arm::Mem(c_p, offsetof(Process, current)));
    mov_imm(TMP1, 1);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, arity)));
    a.b(labels[context_switch_simplified]);
}

void BeamModuleAssembler::emit_return() {
    emit_leave_erlang_frame();

#ifdef JIT_HARD_DEBUG
    /* Validate return address and {x,0} */
    emit_validate(ArgVal(ArgVal::Word, 1));
#endif

    /* The reduction test is kept in module code because moving it to a shared
     * fragment caused major performance regressions in dialyzer. */
    a.subs(FCALLS, FCALLS, imm(1));
    a.cond_mi().b(resolve_fragment(ga->get_dispatch_return(), disp1MB));

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_i_call(const ArgVal &CallTarget) {
    erlang_call(resolve_beam_label(CallTarget, disp128MB));
}

void BeamModuleAssembler::emit_i_call_last(const ArgVal &CallTarget,
                                           const ArgVal &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_call_only(CallTarget);
}

void BeamModuleAssembler::emit_i_call_only(const ArgVal &CallTarget) {
    emit_leave_erlang_frame();
    a.b(resolve_beam_label(CallTarget, disp128MB));
}

/* Handles save_calls. When the active code index is ERTS_SAVE_CALLS_CODE_IX,
 * all remote calls will land here.
 *
 * Export entry is in ARG1, return address is in LR (x30). Both of these must
 * be preserved since this runs between caller and callee. */
void BeamGlobalAssembler::emit_dispatch_save_calls() {
    a.str(ARG1, TMP_MEM1q);

    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.mov(ARG2, ARG1);
    a.mov(ARG1, c_p);
    runtime_call<2>(save_calls);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.ldr(ARG1, TMP_MEM1q);

    /* Keep going with the actual code index. */
    a.mov(TMP1, imm(&the_active_code_index));
    a.ldr(TMP1.w(), arm::Mem(TMP1));

    branch(emit_setup_export_call(ARG1, TMP1));
}

void BeamModuleAssembler::emit_i_call_ext(const ArgVal &Exp) {
    mov_arg(ARG1, Exp);

    arm::Mem target = emit_setup_export_call(ARG1);
    erlang_call(target);
}

void BeamModuleAssembler::emit_i_call_ext_only(const ArgVal &Exp) {
    mov_arg(ARG1, Exp);

    arm::Mem target = emit_setup_export_call(ARG1);
    emit_leave_erlang_frame();
    branch(target);
}

void BeamModuleAssembler::emit_i_call_ext_last(const ArgVal &Exp,
                                               const ArgVal &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_call_ext_only(Exp);
}

static ErtsCodeMFA apply3_mfa = {am_erlang, am_apply, 3};

arm::Mem BeamModuleAssembler::emit_variable_apply(bool includeI) {
    Label dispatch = a.newLabel(), entry = a.newLabel();

    a.bind(entry);

    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>(3);

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);

    if (includeI) {
        a.adr(ARG3, entry);
    } else {
        mov_imm(ARG3, 0);
    }

    mov_imm(ARG4, 0);

    comment("apply()");
    runtime_call<4>(apply);

    /* Any number of X registers can be live at this point. */
    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

    a.cbnz(ARG1, dispatch);
    emit_raise_exception(entry, &apply3_mfa);

    a.bind(dispatch);
    return emit_setup_export_call(ARG1);
}

void BeamModuleAssembler::emit_i_apply() {
    arm::Mem target = emit_variable_apply(false);
    erlang_call(target);
}

void BeamModuleAssembler::emit_i_apply_last(const ArgVal &Deallocate) {
    emit_deallocate(Deallocate);
    emit_i_apply_only();
}

void BeamModuleAssembler::emit_i_apply_only() {
    arm::Mem target = emit_variable_apply(true);

    emit_leave_erlang_frame();
    branch(target);
}

arm::Mem BeamModuleAssembler::emit_fixed_apply(const ArgVal &Arity,
                                               bool includeI) {
    Label dispatch = a.newLabel(), entry = a.newLabel();

    a.bind(entry);

    mov_arg(ARG3, Arity);

    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>(
            Arity.getValue() + 2);

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);

    if (includeI) {
        a.adr(ARG4, entry);
    } else {
        mov_imm(ARG4, 0);
    }

    mov_imm(ARG5, 0);

    runtime_call<5>(fixed_apply);

    /* We will need to reload all X registers in case there has been
     * an error. */
    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

    a.cbnz(ARG1, dispatch);
    emit_raise_exception(entry, &apply3_mfa);

    a.bind(dispatch);

    return emit_setup_export_call(ARG1);
}

void BeamModuleAssembler::emit_apply(const ArgVal &Arity) {
    arm::Mem target = emit_fixed_apply(Arity, false);
    erlang_call(target);
}

void BeamModuleAssembler::emit_apply_last(const ArgVal &Arity,
                                          const ArgVal &Deallocate) {
    emit_deallocate(Deallocate);

    arm::Mem target = emit_fixed_apply(Arity, true);

    emit_leave_erlang_frame();
    branch(target);
}
