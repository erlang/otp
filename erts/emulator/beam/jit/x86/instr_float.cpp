/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2021. All Rights Reserved.
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

static int handle_fconv(Eterm src, double *dst) {
    if (is_small(src)) {
        *dst = (double)signed_val(src);
    } else if (is_float(src)) {
        GET_DOUBLE(src, *(FloatDef *)dst);
    } else if (is_big(src)) {
        if (big_to_double(src, dst) < 0) {
            return 1;
        }
    } else {
        return 1;
    }

    return 0;
}

void BeamModuleAssembler::emit_fconv(const ArgVal &Src, const ArgVal &Dst) {
    Label next = a.newLabel();

    mov_arg(ARG1, Src);
    a.lea(ARG2, getArgRef(Dst));

    emit_enter_runtime();

    runtime_call<2>(handle_fconv);

    emit_leave_runtime();

    a.test(RET, RET);
    a.short_().je(next);

    emit_error(EXC_BADARITH);

    a.bind(next);
}

void BeamModuleAssembler::emit_check_float(Label next, x86::Xmm value) {
    a.movsd(x86::xmm2, value);
    a.movsd(x86::xmm1, x86::qword_ptr(floatMax));
    a.andpd(x86::xmm2, x86::xmmword_ptr(floatSignMask));
    a.ucomisd(x86::xmm1, x86::xmm2);
    a.short_().jnb(next);

    emit_error(EXC_BADARITH);
}

void BeamModuleAssembler::emit_i_fadd(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();

    a.movsd(x86::xmm0, getArgRef(LHS));
    a.movsd(x86::xmm1, getArgRef(RHS));
    a.addpd(x86::xmm0, x86::xmm1);

    emit_check_float(next, x86::xmm0);

    a.bind(next);
    a.movsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_i_fsub(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();

    a.movsd(x86::xmm0, getArgRef(LHS));
    a.movsd(x86::xmm1, getArgRef(RHS));
    a.subpd(x86::xmm0, x86::xmm1);

    emit_check_float(next, x86::xmm0);

    a.bind(next);
    a.movsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_i_fmul(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();

    a.movsd(x86::xmm0, getArgRef(LHS));
    a.movsd(x86::xmm1, getArgRef(RHS));
    a.mulpd(x86::xmm0, x86::xmm1);

    emit_check_float(next, x86::xmm0);

    a.bind(next);
    a.movsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    Label next = a.newLabel();

    a.movsd(x86::xmm0, getArgRef(LHS));
    a.movsd(x86::xmm1, getArgRef(RHS));
    a.divpd(x86::xmm0, x86::xmm1);

    emit_check_float(next, x86::xmm0);

    a.bind(next);
    a.movsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgVal &Src, const ArgVal &Dst) {
    Label next = a.newLabel();

    /* xmm0 = 0.0 */
    a.pxor(x86::xmm0, x86::xmm0);
    a.movsd(x86::xmm1, getArgRef(Src));
    a.subpd(x86::xmm0, x86::xmm1);

    emit_check_float(next, x86::xmm0);

    a.bind(next);
    a.movsd(getArgRef(Dst), x86::xmm0);
}
