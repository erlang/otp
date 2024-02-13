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

    vmovsd(x86::xmm1, x86::qword_ptr(floatMax));
    if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
        a.vandpd(x86::xmm2, x86::xmm0, x86::xmmword_ptr(floatSignMask));
    } else {
        a.movq(x86::xmm2, x86::xmm0);
        a.andpd(x86::xmm2, x86::xmmword_ptr(floatSignMask));
    }
    vucomisd(x86::xmm1, x86::xmm2);
    a.short_().jb(error);
    a.ret();

    a.bind(error);
    {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)),
              imm(EXC_BADARITH));
        a.sub(ARG4, ARG4);
        a.jmp(labels[raise_exception]);
    }

    a.align(AlignMode::kCode, 16);
    a.bind(floatSignMask);
    a.embedUInt64(0x7FFFFFFFFFFFFFFFul);
    a.bind(floatMax);
    a.embedDouble(std::numeric_limits<double>::max());
}

void BeamModuleAssembler::emit_float_instr(uint32_t instIdSSE,
                                           uint32_t instIdAVX,
                                           const ArgFRegister &LHS,
                                           const ArgFRegister &RHS,
                                           const ArgFRegister &Dst) {
    vmovsd(x86::xmm0, getArgRef(LHS));
    vmovsd(x86::xmm1, getArgRef(RHS));

    if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
        a.emit(instIdAVX, x86::xmm0, x86::xmm0, x86::xmm1);
    } else {
        a.emit(instIdSSE, x86::xmm0, x86::xmm1);
    }
    safe_fragment_call(ga->get_check_float_error());
    vmovsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_fload(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    /* {thing_word,double} */
    mov_arg(ARG1, Src);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);

    vmovsd(x86::xmm0, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    vmovsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_fstore(const ArgFRegister &Src,
                                      const ArgRegister &Dst) {
    vmovsd(x86::xmm0, getArgRef(Src));

    /* {thing_word,double} */
    a.mov(x86::qword_ptr(HTOP), imm(HEADER_FLONUM));
    vmovsd(x86::qword_ptr(HTOP, sizeof(Eterm)), x86::xmm0);

    a.lea(ARG1, x86::qword_ptr(HTOP, make_float(0)));
    mov_arg(Dst, ARG1);

    preserve_cache(
            [&]() {
                a.add(HTOP, imm(FLOAT_SIZE_OBJECT * sizeof(Eterm)));
            },
            HTOP);
}

/* ARG2 = source term */
void BeamGlobalAssembler::emit_fconv_shared() {
    Label error = a.newLabel();

    /* big_to_double expects source in ARG1 */
    a.mov(ARG1, ARG2);

    emit_is_boxed(error, ARG2, dShort);

    auto boxed_ptr = emit_ptr_val(ARG2, ARG2);
    a.mov(ARG2, emit_boxed_val(boxed_ptr));
    a.and_(ARG2, imm(_BIG_TAG_MASK));
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

    vmovsd(x86::xmm0, TMP_MEM1q);
    a.ret();

    a.bind(error);
    {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)),
              imm(EXC_BADARITH));
        a.sub(ARG4, ARG4);
        a.jmp(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_fconv(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    if (always_small(Src)) {
        comment("simplified fconv since source is always small");
        mov_arg(ARG2, Src);
        a.sar(ARG2, imm(_TAG_IMMED1_SIZE));
        if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
            a.vcvtsi2sd(x86::xmm0, x86::xmm0, ARG2);
        } else {
            a.cvtsi2sd(x86::xmm0, ARG2);
        }
        vmovsd(getArgRef(Dst), x86::xmm0);
        return;
    }

    Label next = a.newLabel(), not_small = a.newLabel(),
          fallback = a.newLabel();

    mov_arg(ARG2, Src);

    emit_is_small(not_small, Src, ARG2);

    a.sar(ARG2, imm(_TAG_IMMED1_SIZE));
    if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
        a.vcvtsi2sd(x86::xmm0, x86::xmm0, ARG2);
    } else {
        a.cvtsi2sd(x86::xmm0, ARG2);
    }
    a.short_().jmp(next);

    a.bind(not_small);
    {
        if (never_one_of<BeamTypeId::Float>(Src)) {
            comment("skipped float path since source cannot be a float");
        } else {
            /* If the source is always a number, we can skip the box test when
             * it's not a small. */
            if (always_one_of<BeamTypeId::Number>(Src)) {
                comment("skipped box test since source is always a number");
            } else {
                emit_is_boxed(fallback, Src, ARG2, dShort);
            }

            /* Speculatively load the float value, this is safe since all boxed
             * terms are at least two words long. */
            auto boxed_ptr = emit_ptr_val(ARG1, ARG2);
            vmovsd(x86::xmm0, emit_boxed_val(boxed_ptr, sizeof(Eterm)));

            a.cmp(emit_boxed_val(boxed_ptr), imm(HEADER_FLONUM));
            a.short_().je(next);
        }

        /* Bignum or invalid input, handle it in a shared fragment. */
        a.bind(fallback);
        safe_fragment_call(ga->get_fconv_shared());
    }

    a.bind(next);
    vmovsd(getArgRef(Dst), x86::xmm0);
}

void BeamModuleAssembler::emit_i_fadd(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(x86::Inst::kIdAddpd, x86::Inst::kIdVaddpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fsub(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(x86::Inst::kIdSubpd, x86::Inst::kIdVsubpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fmul(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(x86::Inst::kIdMulpd, x86::Inst::kIdVmulpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    emit_float_instr(x86::Inst::kIdDivpd, x86::Inst::kIdVdivpd, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgFRegister &Src,
                                         const ArgFRegister &Dst) {
    /* Note that there is no need to check for errors since flipping the sign
     * of a finite float is guaranteed to produce a finite float. */
    if (Src != Dst) {
        mov_arg(RET, Src);
        a.btc(RET, imm(63));
        mov_arg(Dst, RET);
    } else {
        a.btc(getArgRef(Dst), 63);
    }
}
