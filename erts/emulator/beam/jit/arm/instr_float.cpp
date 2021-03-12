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
#include "big.h"
}

/* Checks whether d0 contains a finite value.
 *
 * Clobbers d30 and d31. */
void BeamGlobalAssembler::emit_check_float_error() {
    Label double_max = a.newLabel(), error = a.newLabel();

    a.fabs(a64::d30, a64::d0);
    a.ldr(a64::d31, arm::Mem(double_max));
    a.fcmp(a64::d30, a64::d31);
    a.cond_hi().b(error);
    a.ret(a64::x30);

    a.align(kAlignCode, 8);
    a.bind(double_max);
    a.embedUInt64(0x7FEFFFFFFFFFFFFFul);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        mov_imm(TMP1, EXC_BADARITH);
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_float_instr(uint32_t instId,
                                           const ArgVal &LHS,
                                           const ArgVal &RHS,
                                           const ArgVal &Dst) {
    auto lhs = load_source(LHS, a64::d0);
    auto rhs = load_source(RHS, a64::d1);
    auto dst = init_destination(Dst, a64::d2);

    a.emit(instId, a64::d0, lhs.reg, rhs.reg);
    fragment_call(ga->get_check_float_error());
    a.mov(dst.reg, a64::d0);
    flush_var(dst);
}

/* * * * */

void BeamModuleAssembler::emit_fload(const ArgVal &Src, const ArgVal &Dst) {
    auto src = load_source(Src, TMP1);
    auto dst = init_destination(Dst, a64::d0);
    arm::Gp float_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(dst.reg, emit_boxed_val(float_ptr, sizeof(Eterm)));
    flush_var(dst);
}

void BeamModuleAssembler::emit_fstore(const ArgVal &Src, const ArgVal &Dst) {
    auto src = load_source(Src, a64::d0);
    auto dst = init_destination(Dst, TMP1);

    a.add(dst.reg, HTOP, imm(TAG_PRIMARY_BOXED));

    mov_imm(TMP2, HEADER_FLONUM);
    a.str(TMP2, arm::Mem(HTOP).post(sizeof(Eterm)));
    a.str(src.reg, arm::Mem(HTOP).post(sizeof(Eterm)));

    flush_var(dst);
}

static double handle_fconv(Eterm src) {
    if (is_small(src)) {
        return (double)signed_val(src);
    } else if (is_float(src)) {
        double res;
        GET_DOUBLE(src, *(FloatDef *)&res);
        return res;
    } else if (is_big(src)) {
        double res;
        if (big_to_double(src, &res) < 0) {
            return NAN;
        }
        return res;
    } else {
        return NAN;
    }
}

void BeamGlobalAssembler::emit_fconv_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime();

    runtime_call<1>(handle_fconv);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.b(labels[check_float_error]);
}

void BeamModuleAssembler::emit_fconv(const ArgVal &Src, const ArgVal &Dst) {
    auto dst = init_destination(Dst, a64::d0);

    mov_arg(ARG1, Src);
    fragment_call(ga->get_fconv_shared());
    a.mov(dst.reg, a64::d0);
    flush_var(dst);
}

void BeamModuleAssembler::emit_i_fadd(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(a64::Inst::kIdFadd_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fsub(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(a64::Inst::kIdFsub_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fmul(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(a64::Inst::kIdFmul_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(a64::Inst::kIdFdiv_v, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgVal &Src, const ArgVal &Dst) {
    auto src = load_source(Src, a64::d0);
    auto dst = init_destination(Dst, a64::d1);

    a.fneg(a64::d0, src.reg);
    fragment_call(ga->get_check_float_error());
    a.mov(dst.reg, a64::d0);
    flush_var(dst);
}
