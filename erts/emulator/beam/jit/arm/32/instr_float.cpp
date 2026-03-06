/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
    Label error = a.newLabel();

    /* ARM64 uses FCMP against DBL_MAX. On ARM32 we keep the same semantics
     * (error on non-finite values) by checking the exponent field after fabs:
     * exponent == 0x7ff means NaN or infinity. */
    a.vabs_f64(a32::d30, a32::d0);
    a.vmov(TMP, VAR, a32::d30);
    mov_imm(ARG1, 0x7FF00000u);
    a.and_(VAR, VAR, ARG1);
    a.cmp(VAR, ARG1);
    a.b_eq(error);
    a.bx(a32::lr);

    a.bind(error);
    {
        mov_imm(ARG4, 0);
        mov_imm(TMP, EXC_BADARITH);
        a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_float_instr(uint32_t instId,
                                           const ArgFRegister &LHS,
                                           const ArgFRegister &RHS,
                                           const ArgFRegister &Dst) {
    auto lhs = load_source(LHS, a32::d0);
    auto rhs = load_source(RHS, a32::d1);
    auto dst = init_destination(Dst, a32::d2);

    switch (instId) {
    case a32::Inst::kIdVadd:
        a.vadd_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    case a32::Inst::kIdVsub:
        a.vsub_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    case a32::Inst::kIdVmul:
        a.vmul_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    case a32::Inst::kIdVdiv:
        a.vdiv_f64(a32::d0, lhs.reg, rhs.reg);
        break;
    default:
        emit_nyi("emit_float_instr(instId)");
        return;
    }

    fragment_call(ga->get_check_float_error());
    a.vmov_f64(dst.reg, a32::d0);
    flush_var(dst);
}

/* * * * */

void BeamModuleAssembler::emit_fload(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    auto src = load_source(Src, TMP);
    auto dst = init_destination(Dst, a32::d0);
    a32::Gp float_ptr = emit_ptr_val(TMP, src.reg);

    lea(TMP, emit_boxed_val(float_ptr, sizeof(Eterm)));
    a.vldr_64(dst.reg, arm::Mem(TMP));
    flush_var(dst);
}

void BeamModuleAssembler::emit_fstore(const ArgFRegister &Src,
                                      const ArgRegister &Dst) {
    auto src = load_source(Src, a32::d0);
    auto dst = init_destination(Dst, VAR);

    a.add(dst.reg, HTOP, imm(TAG_PRIMARY_BOXED));

    mov_imm(TMP, HEADER_FLONUM);
    a.str(TMP, arm::Mem(HTOP).post(sizeof(Eterm)));

    a.vstr_64(src.reg, arm::Mem(HTOP));
    ERTS_CT_ASSERT(sizeof(Eterm) == 4);
    a.add(HTOP, HTOP, imm(2 * sizeof(Eterm)));

    flush_var(dst);
}

/* ARG1 = source term */
void BeamGlobalAssembler::emit_fconv_shared() {
    // TODO
    emit_nyi("emit_fconv_shared");
}

void BeamModuleAssembler::emit_fconv(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    // TODO
    emit_nyi("emit_fconv");
}

void BeamModuleAssembler::emit_i_fadd(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVadd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fsub(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVsub, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fmul(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVmul, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(a32::Inst::kIdVdiv, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgFRegister &Src,
                                         const ArgFRegister &Dst) {
    auto src = load_source(Src, a32::d0);
    auto dst = init_destination(Dst, a32::d1);

    /* Note that there is no need to check for errors since flipping the sign
     * of a finite float is guaranteed to produce a finite float. */
    a.vneg_f64(a32::d0, src.reg);
    a.vmov_f64(dst.reg, a32::d0);
    flush_var(dst);
}
