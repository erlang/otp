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

    a.movsd(x86::xmm0, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.movsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_fstore(const ArgVal &Src, const ArgVal &Dst) {
    a.movsd(x86::xmm0, getArgRef(Src));

    /* {thing_word,double} */
    a.mov(x86::qword_ptr(HTOP), imm(HEADER_FLONUM));
    a.movsd(x86::qword_ptr(HTOP, sizeof(Eterm)), x86::xmm0);

    a.lea(ARG1, x86::qword_ptr(HTOP, make_float(0)));
    mov_arg(Dst, ARG1);

    a.add(HTOP, imm(FLOAT_SIZE_OBJECT * sizeof(Eterm)));
}

/* ARG2 = source term */
void BeamGlobalAssembler::emit_fconv_shared() {
    Label error = a.newLabel();

    /* big_to_double expects source in ARG1 */
    a.mov(ARG1, ARG2);

    emit_is_boxed(error, ARG2, dShort);

    auto boxed_ptr = emit_ptr_val(ARG2, ARG2);
    a.mov(ARG2, emit_boxed_val(boxed_ptr));
    a.and_(ARG2, imm(_TAG_HEADER_MASK - _BIG_SIGN_BIT));
    a.cmp(ARG2, imm(_TAG_HEADER_POS_BIG));
    a.short_().jne(error);

    emit_enter_frame();
    emit_enter_runtime();

    /* ARG1 already contains the source term. */
    a.lea(ARG2, TMP_MEM1q);
    runtime_call<2>(big_to_double);

    emit_leave_runtime();
    emit_leave_frame();

    a.test(RETd, RETd);
    a.short_().js(error);

    a.movsd(x86::xmm0, TMP_MEM1q);
    a.ret();

    a.bind(error);
    {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)),
              imm(EXC_BADARITH));
        a.sub(ARG4, ARG4);
        a.jmp(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_fconv(const ArgVal &Src, const ArgVal &Dst) {
    Label next = a.newLabel(), not_small = a.newLabel(),
          fallback = a.newLabel();

    mov_arg(ARG2, Src);

    emit_is_small(not_small, Src, ARG2);

    a.sar(ARG2, imm(_TAG_IMMED1_SIZE));
    a.cvtsi2sd(x86::xmm0, ARG2);
    a.short_().jmp(next);

    a.bind(not_small);
    {
        if (masked_types(Src, BEAM_TYPE_FLOAT) == BEAM_TYPE_NONE) {
            comment("skipped float path since source cannot be a float");
        } else {
            /* If the source is always a number, we can skip the box test when
             * it's not a small. */
            if (always_one_of(Src, BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER)) {
                comment("skipped box test since source is always a number");
            } else {
                emit_is_boxed(fallback, Src, ARG2, dShort);
            }

            /* Speculatively load the float value, this is safe since all boxed
             * terms are at least two words long. */
            auto boxed_ptr = emit_ptr_val(ARG1, ARG2);
            a.movsd(x86::xmm0, emit_boxed_val(boxed_ptr, sizeof(Eterm)));

            a.cmp(emit_boxed_val(boxed_ptr), imm(HEADER_FLONUM));
            a.short_().je(next);
        }

        /* Bignum or invalid input, handle it in a shared fragment. */
        a.bind(fallback);
        safe_fragment_call(ga->get_fconv_shared());
    }

    a.bind(next);
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
    a.pxor(x86::xmm0, x86::xmm0);
    a.movsd(x86::xmm1, getArgRef(Src));
    a.subpd(x86::xmm0, x86::xmm1);
    safe_fragment_call(ga->get_check_float_error());
    a.movsd(getArgRef(Dst), x86::xmm0);
}
