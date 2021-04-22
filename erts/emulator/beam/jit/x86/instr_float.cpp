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

/* Fixes a silly compilation error on Windows, where the following macro
 * prevents us from using `std::numeric_limits<T>::max()` */
#ifdef max
#    undef max
#endif

void BeamGlobalAssembler::emit_check_float_error() {
    Label error = a.newLabel(), floatMax = a.newLabel(),
          floatSignMask = a.newLabel();

    a.movsd(x86::xmm2, x86::xmm0);
    a.movsd(x86::xmm1, x86::qword_ptr(floatMax));
    a.andpd(x86::xmm2, x86::xmmword_ptr(floatSignMask));
    a.ucomisd(x86::xmm1, x86::xmm2);
    a.short_().jb(error);
    a.ret();

    a.bind(error);
    {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)),
              imm(EXC_BADARITH));
        a.sub(ARG4, ARG4);
        a.jmp(labels[raise_exception]);
    }

    a.align(kAlignCode, 16);
    a.bind(floatSignMask);
    a.embedUInt64(0x7FFFFFFFFFFFFFFFul);
    a.bind(floatMax);
    a.embedDouble(std::numeric_limits<double>::max());
}

void BeamModuleAssembler::emit_float_instr(uint32_t instId,
                                           const ArgVal &LHS,
                                           const ArgVal &RHS,
                                           const ArgVal &Dst) {
    a.movsd(x86::xmm0, getArgRef(LHS));
    a.movsd(x86::xmm1, getArgRef(RHS));

    a.emit(instId, x86::xmm0, x86::xmm1);
    safe_fragment_call(ga->get_check_float_error());
    a.movsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_fload(const ArgVal &Src, const ArgVal &Dst) {
    /* {thing_word,double} */
    mov_arg(ARG1, Src);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.mov(ARG1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.mov(getArgRef(Dst), ARG1);
}

void BeamModuleAssembler::emit_fstore(const ArgVal &Src, const ArgVal &Dst) {
    a.mov(ARG1, getArgRef(Src));

    /* {thing_word,double} */
    a.mov(x86::qword_ptr(HTOP), imm(HEADER_FLONUM));
    a.mov(x86::qword_ptr(HTOP, sizeof(Eterm)), ARG1);

    a.lea(ARG1, x86::qword_ptr(HTOP, make_float(0)));
    mov_arg(Dst, ARG1);

    a.add(HTOP, imm(FLOAT_SIZE_OBJECT * sizeof(Eterm)));
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
    emit_enter_runtime();
    runtime_call<1>(handle_fconv);
    emit_leave_runtime();

    a.jmp(labels[check_float_error]);
}

void BeamModuleAssembler::emit_fconv(const ArgVal &Src, const ArgVal &Dst) {
    mov_arg(ARG1, Src);
    safe_fragment_call(ga->get_fconv_shared());
    a.movsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_i_fadd(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(x86::Inst::kIdAddpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fsub(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(x86::Inst::kIdSubpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fmul(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(x86::Inst::kIdMulpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_float_instr(x86::Inst::kIdDivpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgVal &Src, const ArgVal &Dst) {
    /* xmm0 = 0.0 */
    a.psubd(x86::xmm0, x86::xmm0);
    a.movsd(x86::xmm1, getArgRef(Src));
    a.subpd(x86::xmm0, x86::xmm1);
    safe_fragment_call(ga->get_check_float_error());
    a.movsd(getArgRef(Dst), x86::xmm0);
}
