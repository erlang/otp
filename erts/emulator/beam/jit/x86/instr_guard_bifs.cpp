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

#include <algorithm>
#include <numeric>
#include "beam_asm.hpp"

extern "C"
{
#include "erl_bif_table.h"
#include "big.h"
#include "beam_catches.h"
#include "beam_common.h"
#include "code_ix.h"
#include "erl_map.h"
}

using namespace asmjit;

/* ================================================================
 *  '=:='/2
 *  '=/='/2
 *  '>='/2
 *  '<'/2
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_is_eq_ne_exact(const ArgSource &LHS,
                                                  const ArgSource &RHS,
                                                  const ArgRegister &Dst,
                                                  Eterm fail_value,
                                                  Eterm succ_value) {
    /* `mov_imm` may clobber the flags if either value is zero. */
    ASSERT(fail_value && succ_value);

    cmp_arg(getArgRef(LHS), RHS);
    mov_imm(RET, succ_value);

    if (always_immediate(LHS) || always_immediate(RHS)) {
        if (!LHS.isImmed() && !RHS.isImmed()) {
            comment("simplified check since one argument is an immediate");
        }
        mov_imm(ARG1, fail_value);
        a.cmovne(RET, ARG1);
    } else {
        Label next = a.newLabel();

        a.je(next);

        mov_arg(ARG1, LHS);
        mov_arg(ARG2, RHS);

        emit_enter_runtime();
        runtime_call<2>(eq);
        emit_leave_runtime();

        a.test(RET, RET);

        mov_imm(RET, succ_value);
        mov_imm(ARG1, fail_value);
        a.cmove(RET, ARG1);

        a.bind(next);
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_bif_is_eq_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    emit_bif_is_eq_ne_exact(LHS, RHS, Dst, am_false, am_true);
}

void BeamModuleAssembler::emit_bif_is_ne_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    emit_bif_is_eq_ne_exact(LHS, RHS, Dst, am_true, am_false);
}

void BeamModuleAssembler::emit_cond_to_bool(uint32_t instId,
                                            const ArgRegister &Dst) {
    mov_imm(RET, am_true);
    mov_imm(ARG1, am_false);
    a.emit(instId, RET, ARG1);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_bif_is_ge_lt(uint32_t instId,
                                            const ArgSource &LHS,
                                            const ArgSource &RHS,
                                            const ArgRegister &Dst) {
    Label generic = a.newLabel(), make_boolean = a.newLabel();

    mov_arg(ARG2, RHS); /* May clobber ARG1 */
    mov_arg(ARG1, LHS);

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(LHS) &&
        always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(RHS)) {
        /* The only possible kind of immediate is a small and all other
         * values are boxed, so we can test for smalls by testing boxed. */
        comment("simplified small test since all other types are boxed");
        if (always_small(LHS)) {
            emit_is_not_boxed(generic, ARG2, dShort);
        } else if (always_small(RHS)) {
            emit_is_not_boxed(generic, ARG1, dShort);
        } else {
            a.mov(RETd, ARG1d);
            a.and_(RETd, ARG2d);
            emit_is_not_boxed(generic, RET, dShort);
        }
    } else {
        /* Relative comparisons are overwhelmingly likely to be used on
         * smalls, so we'll specialize those and keep the rest in a shared
         * fragment. */
        if (always_small(RHS)) {
            a.mov(RETd, ARG1d);
        } else if (always_small(LHS)) {
            a.mov(RETd, ARG2d);
        } else {
            a.mov(RETd, ARG1d);
            a.and_(RETd, ARG2d);
        }

        a.and_(RETb, imm(_TAG_IMMED1_MASK));
        a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
        a.short_().jne(generic);
    }

    /* Both arguments are smalls. */
    a.cmp(ARG1, ARG2);
    a.short_().jmp(make_boolean);

    a.bind(generic);
    {
        a.cmp(ARG1, ARG2);
        a.short_().je(make_boolean);
        safe_fragment_call(ga->get_arith_compare_shared());
    }

    a.bind(make_boolean);
    emit_cond_to_bool(instId, Dst);
}

void BeamModuleAssembler::emit_bif_is_ge(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    bool both_small = always_small(LHS) && always_small(RHS);

    if (both_small && LHS.isRegister() && RHS.isImmed() &&
        Support::isInt32(RHS.as<ArgImmed>().get())) {
        comment("simplified compare because one operand is an immediate small");
        a.cmp(getArgRef(LHS.as<ArgRegister>()), imm(RHS.as<ArgImmed>().get()));
        emit_cond_to_bool(x86::Inst::kIdCmovl, Dst);

        return;
    } else if (both_small && RHS.isRegister() && LHS.isImmed() &&
               Support::isInt32(LHS.as<ArgImmed>().get())) {
        comment("simplified compare because one operand is an immediate small");
        a.cmp(getArgRef(RHS.as<ArgRegister>()), imm(LHS.as<ArgImmed>().get()));
        emit_cond_to_bool(x86::Inst::kIdCmovg, Dst);

        return;
    }

    emit_bif_is_ge_lt(x86::Inst::kIdCmovl, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_bif_is_lt(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    emit_bif_is_ge_lt(x86::Inst::kIdCmovge, LHS, RHS, Dst);
}

/* ================================================================
 *  bit_size/1
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_bit_size(const ArgWord &Bif,
                                            const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    if (!exact_type<BeamTypeId::Bitstring>(Src)) {
        /* Unknown type. Use the standard BIF instruction. */
        emit_i_bif1(Src, Fail, Bif, Dst);
        return;
    }

    comment("inlined bit_size/1 because its argument is always a bitstring");
    mov_arg(ARG2, Src);
    x86::Gp boxed_ptr = emit_ptr_val(ARG2, ARG2);

    ERTS_CT_ASSERT(offsetof(ErlHeapBits, size) == sizeof(Eterm));
    a.mov(ARG1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));

    Label not_sub_bits = a.newLabel();
    a.cmp(emit_boxed_val(boxed_ptr), imm(HEADER_SUB_BITS));
    a.short_().jne(not_sub_bits);
    {
        a.mov(ARG1, emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, end)));
        a.sub(ARG1, emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, start)));
    }
    a.bind(not_sub_bits);

    a.shl(ARG1, imm(_TAG_IMMED1_SIZE));
    a.or_(ARG1, imm(_TAG_IMMED1_SMALL));

    mov_arg(Dst, ARG1);
}

/* ================================================================
 *  byte_size/1
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_byte_size(const ArgWord &Bif,
                                             const ArgLabel &Fail,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    if (!exact_type<BeamTypeId::Bitstring>(Src)) {
        /* Unknown type. Use the standard BIF instruction. */
        emit_i_bif1(Src, Fail, Bif, Dst);
        return;
    }

    comment("inlined byte_size/1 because its argument is always a bitstring");
    mov_arg(ARG2, Src);
    x86::Gp boxed_ptr = emit_ptr_val(ARG2, ARG2);

    ERTS_CT_ASSERT(offsetof(ErlHeapBits, size) == sizeof(Eterm));
    a.mov(ARG1, emit_boxed_val(boxed_ptr, offsetof(ErlHeapBits, size)));

    Label not_sub_bits = a.newLabel();
    a.cmp(emit_boxed_val(boxed_ptr), imm(HEADER_SUB_BITS));
    a.short_().jne(not_sub_bits);
    {
        a.mov(ARG1, emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, end)));
        a.sub(ARG1, emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, start)));
    }
    a.bind(not_sub_bits);

    /* Round up to the nearest byte. */
    a.add(ARG1, imm(7));
    a.shl(ARG1, imm(_TAG_IMMED1_SIZE - 3));
    a.or_(ARG1, imm(_TAG_IMMED1_SMALL));

    mov_arg(Dst, ARG1);
}

/* ================================================================
 *  element/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_element_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_element, 2};

    a.mov(getXRef(0), ARG1);
    a.mov(getXRef(1), ARG2);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(BADARG));
    a.mov(ARG4, imm(&mfa));

    a.jmp(labels[raise_exception]);
}

/* ARG1 = Position (1-based)
 * ARG2 = Tuple
 * ARG3 = 0 if if in body, otherwise address of failure label.
 *
 * Will return with a value in RET only if the element operation succeeds. */
void BeamGlobalAssembler::emit_bif_element_shared() {
    Label error = a.newLabel();

    emit_enter_frame();

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.short_().jne(error);

    a.mov(ARG4, ARG1);
    a.sar(ARG4, imm(_TAG_IMMED1_SIZE));

    emit_is_boxed(error, ARG2, dShort);

    a.mov(ARG5, ARG2);
    (void)emit_ptr_val(ARG5, ARG5);
    a.lea(ARG5, emit_boxed_val(ARG5));
    a.mov(ARG6, x86::qword_ptr(ARG5));
    a.mov(RETd, ARG6d);
    ERTS_CT_ASSERT(make_arityval_zero() == 0);
    a.and_(RETb, imm(_TAG_HEADER_MASK));
    a.short_().jne(error);

    a.shr(ARG6, imm(_HEADER_ARITY_OFFS));
    a.dec(ARG4);
    a.cmp(ARG6, ARG4);
    a.short_().jbe(error);

    a.inc(ARG4);
    a.mov(RET, x86::qword_ptr(ARG5, ARG4, 3));

    emit_leave_frame();
    a.ret();

    a.bind(error);
    {
        emit_leave_frame();

        a.test(ARG3, ARG3);
        a.je(labels[handle_element_error]);

        /* Discard return address and jump to fail label. */
        a.add(x86::rsp, imm(8));
        a.jmp(ARG3);
    }
}

/*
 * At the time of implementation, there were 3678 uses of element/2 in
 * the OTP source code. 3137 of those uses had a literal first argument
 * (the position in the tuple), while 540 uses had a variable first
 * argument. Calls to element/2 (with a literal first argument) is
 * especially common in code generated by yecc.
 */
void BeamModuleAssembler::emit_bif_element(const ArgLabel &Fail,
                                           const ArgSource &Pos,
                                           const ArgSource &Tuple,
                                           const ArgRegister &Dst) {
    bool const_position;

    const_position = Pos.isSmall() && Pos.as<ArgSmall>().getSigned() > 0 &&
                     Pos.as<ArgSmall>().getSigned() <= (Sint)MAX_ARITYVAL;

    /*
     * Try to optimize the use of a tuple as a lookup table.
     */
    if (exact_type<BeamTypeId::Integer>(Pos) && Tuple.isLiteral()) {
        Eterm tuple = beamfile_get_literal(beam, Tuple.as<ArgLiteral>().get());

        if (is_tuple(tuple)) {
            Label error = a.newLabel(), next = a.newLabel();
            Sint size = Sint(arityval(*tuple_val(tuple)));
            auto [min, max] = getClampedRange(Pos);
            bool can_fail = min < 1 || size < max;

            comment("skipped tuple test since source is always a literal "
                    "tuple");
            mov_arg(ARG2, Tuple);
            mov_arg(ARG1, Pos);
            x86::Gp boxed_ptr = emit_ptr_val(ARG3, ARG2);
            a.lea(ARG4, emit_boxed_val(boxed_ptr));
            if (always_small(Pos)) {
                comment("skipped test for small position since it is always "
                        "small");
            } else {
                comment("simplified test for small position since it is an "
                        "integer");
                a.test(ARG1.r8(), imm(TAG_PRIMARY_LIST));
                a.short_().je(error);
            }

            a.mov(RET, ARG1);
            a.sar(RET, imm(_TAG_IMMED1_SIZE));
            if (min >= 1) {
                comment("skipped check for position =:= 0 since it is always "
                        ">= 1");
            } else {
                a.short_().jz(error);
            }
            if (min >= 0 && size >= max) {
                comment("skipped check for negative position and position "
                        "beyond tuple");
            } else {
                /* Note: Also checks for negative size. */
                a.cmp(RET, imm(size));
                a.short_().ja(error);
            }

            a.mov(RET, x86::qword_ptr(ARG4, RET, 3));
            if (can_fail) {
                a.short_().jmp(next);
            }

            a.bind(error);
            if (can_fail) {
                if (Fail.get() == 0) {
                    safe_fragment_call(ga->get_handle_element_error());
                } else {
                    a.jmp(resolve_beam_label(Fail));
                }
            }

            a.bind(next);
            mov_arg(Dst, RET);

            return;
        }
    }

    if (const_position) {
        /* The position is a valid small integer. Inline the code.
         *
         * The size of the code is 40 bytes, while the size of the bif2
         * instruction is 36 bytes. */
        Uint position = Pos.as<ArgSmall>().getSigned();

        mov_arg(ARG2, Tuple);

        x86::Gp boxed_ptr = emit_ptr_val(ARG3, ARG2);

        if (exact_type<BeamTypeId::Tuple>(Tuple)) {
            comment("skipped tuple test since source is always a tuple");
            ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));
            a.cmp(emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)),
                  imm(make_arityval_unchecked(position)));

            if (Fail.get() == 0) {
                Label next = a.newLabel();

                a.short_().jae(next);

                mov_imm(ARG1, make_small(position));
                safe_fragment_call(ga->get_handle_element_error());

                a.bind(next);
            } else {
                a.jb(resolve_beam_label(Fail));
            }
        } else {
            Distance dist;
            Label error;

            if (Fail.get() == 0) {
                error = a.newLabel();
                dist = dShort;
            } else {
                error = resolve_beam_label(Fail);
                dist = dLong;
            }

            emit_is_boxed(error, Tuple, ARG2, dist);

            a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
            a.cmp(RETd, imm(make_arityval_unchecked(position)));

            if (Fail.get() == 0) {
                a.short_().jb(error);
            } else {
                a.jb(error);
            }

            ERTS_CT_ASSERT(make_arityval_zero() == 0);
            a.and_(RETb, imm(_TAG_HEADER_MASK));

            if (Fail.get() == 0) {
                Label next = a.newLabel();

                a.short_().je(next);

                a.bind(error);
                {
                    mov_imm(ARG1, make_small(position));
                    safe_fragment_call(ga->get_handle_element_error());
                }

                a.bind(next);
            } else {
                a.jne(error);
            }
        }

        a.mov(RET, emit_boxed_val(boxed_ptr, position * sizeof(Eterm)));
    } else {
        /* The code is too large to inline. Call a shared fragment.
         *
         * The size of the code that calls the shared fragment is 19 bytes,
         * while the size of the bif2 instruction is 36 bytes. */
        mov_arg(ARG2, Tuple);
        mov_arg(ARG1, Pos);

        if (Fail.get() != 0) {
            a.lea(ARG3, x86::qword_ptr(resolve_beam_label(Fail)));
        } else {
            mov_imm(ARG3, 0);
        }

        safe_fragment_call(ga->get_bif_element_shared());
    }

    mov_arg(Dst, RET);
}

/* ================================================================
 *  hd/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_hd_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_hd, 1};

    a.mov(getXRef(0), RET);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(BADARG));
    a.mov(ARG4, imm(&mfa));
    a.jmp(labels[raise_exception]);
}

/*
 * At the time of implementation, there were 3285 uses of hd/1 in
 * the OTP source code. Most of them were in code generated by
 * yecc.
 *
 * The code size for this specialization of hd/1 is 21 bytes,
 * while the code size for the bif1 instruction is 24 bytes.
 */

void BeamModuleAssembler::emit_bif_hd(const ArgSource &Src,
                                      const ArgRegister &Hd) {
    Label good_cons = a.newLabel();

    mov_arg(RET, Src);
    a.test(RETb, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));

    a.short_().je(good_cons);
    safe_fragment_call(ga->get_handle_hd_error());

    a.bind(good_cons);
    {
        x86::Gp boxed_ptr = emit_ptr_val(RET, RET);
        a.mov(ARG2, getCARRef(boxed_ptr));
        mov_arg(Hd, ARG2);
    }
}

/* ================================================================
 *  is_map_key/2
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_is_map_key(const ArgWord &Bif,
                                              const ArgLabel &Fail,
                                              const ArgSource &Key,
                                              const ArgSource &Src,
                                              const ArgRegister &Dst) {
    if (!exact_type<BeamTypeId::Map>(Src)) {
        emit_i_bif2(Key, Src, Fail, Bif, Dst);
        return;
    }

    comment("inlined BIF is_map_key/2");

    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);

    if (maybe_one_of<BeamTypeId::MaybeImmediate>(Key) &&
        hasCpuFeature(CpuFeatures::X86::kBMI2)) {
        safe_fragment_call(ga->get_i_get_map_element_shared());
        emit_cond_to_bool(x86::Inst::kIdCmovne, Dst);
    } else {
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        emit_test_the_non_value(RET);
        emit_cond_to_bool(x86::Inst::kIdCmove, Dst);
    }
}

/* ================================================================
 *  map_get/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_get_badmap() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_get, 2};
    a.mov(getXRef(0), ARG2);
    a.mov(getXRef(1), ARG1);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(BADMAP));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), ARG1);
    a.mov(ARG4, imm(&mfa));
    a.jmp(labels[raise_exception]);
}

void BeamGlobalAssembler::emit_handle_map_get_badkey() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_get, 2};
    a.mov(getXRef(0), ARG2);
    a.mov(getXRef(1), ARG1);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(BADKEY));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), ARG2);
    a.mov(ARG4, imm(&mfa));
    a.jmp(labels[raise_exception]);
}

void BeamModuleAssembler::emit_bif_map_get(const ArgLabel &Fail,
                                           const ArgSource &Key,
                                           const ArgSource &Src,
                                           const ArgRegister &Dst) {
    Label good_key = a.newLabel();

    mov_arg(ARG1, Src);
    mov_arg(ARG2, Key);

    if (exact_type<BeamTypeId::Map>(Src)) {
        comment("skipped test for map for known map argument");
    } else {
        Label bad_map = a.newLabel();
        Label good_map = a.newLabel();

        if (Fail.get() == 0) {
            emit_is_boxed(bad_map, Src, ARG1);
        } else {
            emit_is_boxed(resolve_beam_label(Fail), Src, ARG1);
        }

        /* As an optimization for the `error | #{}` case, skip checking the
         * header word when we know that the only possible boxed type
         * is a map. */
        if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Map) {
            comment("skipped header test since we know it's a map when boxed");
            if (Fail.get() == 0) {
                a.short_().jmp(good_map);
            }
        } else {
            x86::Gp boxed_ptr = emit_ptr_val(RET, ARG1);
            a.mov(RET, emit_boxed_val(boxed_ptr));
            a.and_(RETb, imm(_TAG_HEADER_MASK));
            a.cmp(RETb, imm(_TAG_HEADER_MAP));
            if (Fail.get() == 0) {
                a.short_().je(good_map);
            } else {
                a.jne(resolve_beam_label(Fail));
            }
        }

        a.bind(bad_map);
        if (Fail.get() == 0) {
            fragment_call(ga->get_handle_map_get_badmap());
        }

        a.bind(good_map);
    }

    if (maybe_one_of<BeamTypeId::MaybeImmediate>(Key) &&
        hasCpuFeature(CpuFeatures::X86::kBMI2)) {
        safe_fragment_call(ga->get_i_get_map_element_shared());
        if (Fail.get() == 0) {
            a.je(good_key);
        } else {
            a.jne(resolve_beam_label(Fail));
        }
    } else {
        emit_enter_runtime();
        runtime_call<2>(get_map_element);
        emit_leave_runtime();

        emit_test_the_non_value(RET);
        if (Fail.get() == 0) {
            a.short_().jne(good_key);
        } else {
            a.je(resolve_beam_label(Fail));
        }
    }

    if (Fail.get() == 0) {
        mov_arg(ARG1, Src);
        mov_arg(ARG2, Key);
        fragment_call(ga->get_handle_map_get_badkey());
    }

    a.bind(good_key);
    mov_arg(Dst, RET);
}

/* ================================================================
 *  map_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_size_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_map_size, 1};

    a.mov(getXRef(0), RET);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), RET);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(BADMAP));
    a.mov(ARG4, imm(&mfa));
    a.jmp(labels[raise_exception]);
}

void BeamModuleAssembler::emit_bif_map_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    Label error = a.newLabel(), good_map = a.newLabel();

    mov_arg(RET, Src);

    if (Fail.get() == 0) {
        emit_is_boxed(error, Src, RET);
    } else {
        emit_is_boxed(resolve_beam_label(Fail), Src, RET);
    }

    x86::Gp boxed_ptr = emit_ptr_val(x86::rdx, RET);

    if (exact_type<BeamTypeId::Map>(Src)) {
        comment("skipped type check because the argument is always a map");
        a.bind(error); /* Never referenced. */
    } else {
        a.mov(x86::ecx, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
        a.and_(x86::cl, imm(_TAG_HEADER_MASK));
        a.cmp(x86::cl, imm(_TAG_HEADER_MAP));
        if (Fail.get() == 0) {
            a.short_().je(good_map);

            a.bind(error);
            safe_fragment_call(ga->get_handle_map_size_error());
        } else {
            a.jne(resolve_beam_label(Fail));
            a.bind(error); /* Never referenced. */
        }
    }

    a.bind(good_map);
    {
        ERTS_CT_ASSERT(offsetof(flatmap_t, size) == sizeof(Eterm));
        preserve_cache(
                [&]() {
                    a.mov(RET, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
                    a.shl(RET, imm(4));
                    a.or_(RETb, imm(_TAG_IMMED1_SMALL));
                },
                RET);
        mov_arg(Dst, RET);
    }
}

/* ================================================================
 *  min/2
 *  max/2
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_min_max(uint32_t instId,
                                           const ArgSource &LHS,
                                           const ArgSource &RHS,
                                           const ArgRegister &Dst) {
    Label generic = a.newLabel(), do_cmov = a.newLabel();
    bool both_small = always_small(LHS) && always_small(RHS);
    bool need_generic = !both_small;

    mov_arg(ARG2, RHS); /* May clobber ARG1 */
    mov_arg(ARG1, LHS);

    if (both_small) {
        comment("skipped test for small operands since they are always small");
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_small(RHS)) {
        emit_is_not_boxed(generic, ARG1, dShort);
    } else if (always_small(LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        emit_is_not_boxed(generic, ARG2, dShort);
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        comment("simplified small test since all other types are boxed");
        a.mov(RETd, ARG1d);
        a.and_(RETd, ARG2d);
        emit_is_not_boxed(generic, RETb, dShort);
    } else {
        /* Relative comparisons are overwhelmingly likely to be used on
         * smalls, so we'll specialize those and keep the rest in a shared
         * fragment. */
        if (RHS.isSmall()) {
            a.mov(RETd, ARG1d);
        } else if (LHS.isSmall()) {
            a.mov(RETd, ARG2d);
        } else {
            a.mov(RETd, ARG1d);
            a.and_(RETd, ARG2d);
        }

        a.and_(RETb, imm(_TAG_IMMED1_MASK));
        a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
        a.short_().jne(generic);
    }

    /* Both arguments are smalls. */
    a.cmp(ARG1, ARG2);
    if (need_generic) {
        a.short_().jmp(do_cmov);
    }

    a.bind(generic);
    if (need_generic) {
        a.cmp(ARG1, ARG2);
        a.short_().je(do_cmov);
        a.push(ARG1);
        a.push(ARG2);
        safe_fragment_call(ga->get_arith_compare_shared());
        a.pop(ARG2);
        a.pop(ARG1);
    }

    a.bind(do_cmov);
    a.emit(instId, ARG1, ARG2);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_bif_max(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    emit_bif_min_max(x86::Inst::kIdCmovl, LHS, RHS, Dst);
}

void BeamModuleAssembler::emit_bif_min(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    emit_bif_min_max(x86::Inst::kIdCmovg, LHS, RHS, Dst);
}

/* ================================================================
 *  node/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_node_error() {
    static ErtsCodeMFA mfa = {am_erlang, am_node, 1};
    a.mov(getXRef(0), ARG1);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(BADARG));
    a.mov(ARG4, imm(&mfa));

    a.jmp(labels[raise_exception]);
}

void BeamModuleAssembler::emit_bif_node(const ArgLabel &Fail,
                                        const ArgRegister &Src,
                                        const ArgRegister &Dst) {
    bool always_identifier = always_one_of<BeamTypeId::Identifier>(Src);
    Label test_internal = a.newLabel();
    Label internal = a.newLabel();
    Label next = a.newLabel();
    Label fail;

    if (Fail.get() == 0 && !always_identifier) {
        fail = a.newLabel();
    }

    mov_arg(ARG1, Src);
    emit_is_boxed(test_internal, Src, ARG1);

    x86::Gp boxed_ptr = emit_ptr_val(ARG2, ARG1);

    if (!always_one_of<BeamTypeId::Pid, BeamTypeId::Port>(Src)) {
        a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
        a.and_(RETd, imm(_TAG_HEADER_MASK));
    }

    if (maybe_one_of<BeamTypeId::Reference>(Src)) {
        a.cmp(RETb, imm(_TAG_HEADER_REF));
        a.short_().je(internal);
    }

    if (!always_identifier) {
        Label external = a.newLabel();

        ERTS_CT_ASSERT((_TAG_HEADER_EXTERNAL_PORT - _TAG_HEADER_EXTERNAL_PID) >>
                               _TAG_PRIMARY_SIZE ==
                       1);
        ERTS_CT_ASSERT((_TAG_HEADER_EXTERNAL_REF - _TAG_HEADER_EXTERNAL_PORT) >>
                               _TAG_PRIMARY_SIZE ==
                       1);
        a.sub(RETb, imm(_TAG_HEADER_EXTERNAL_PID));
        a.cmp(RETb, imm(_TAG_HEADER_EXTERNAL_REF - _TAG_HEADER_EXTERNAL_PID));
        if (Fail.get() == 0) {
            a.short_().jbe(external);

            a.bind(fail);
            fragment_call(ga->get_handle_node_error());
        } else {
            a.ja(resolve_beam_label(Fail));
        }

        a.bind(external);
    }

    a.mov(ARG1, emit_boxed_val(boxed_ptr, offsetof(ExternalThing, node)));
    a.short_().jmp(next);

    a.bind(test_internal);
    if (!always_identifier) {
        /* Since pids and ports differ by a single bit, we can
         * simplify the check by clearing said bit and comparing
         * against the lesser one. */
        ERTS_CT_ASSERT(_TAG_IMMED1_PORT - _TAG_IMMED1_PID == 0x4);
        a.mov(RETd, ARG1d);
        a.and_(RETb,
               imm(~(_TAG_IMMED1_PORT - _TAG_IMMED1_PID) & _TAG_IMMED1_MASK));
        a.cmp(RETb, imm(_TAG_IMMED1_PID));
        if (Fail.get() == 0) {
            a.short_().jne(fail);
        } else {
            a.jne(resolve_beam_label(Fail));
        }
    }

    a.bind(internal);
    a.mov(ARG1, imm(&erts_this_node));
    a.mov(ARG1, x86::qword_ptr(ARG1));

    a.bind(next);
    a.mov(ARG1, x86::qword_ptr(ARG1, offsetof(ErlNode, sysname)));
    mov_arg(Dst, ARG1);
}

/* ================================================================
 *  tuple_size/1
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_tuple_size(const ArgWord &Bif,
                                              const ArgLabel &Fail,
                                              const ArgRegister &Src,
                                              const ArgRegister &Dst) {
    if (exact_type<BeamTypeId::Tuple>(Src)) {
        comment("inlined tuple_size/1 because the argument is always a tuple");
        mov_arg(RET, Src);

        /* Instructions operating on dwords are shorter. */
        ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));
        x86::Gp boxed_ptr = emit_ptr_val(RET, RET);
        a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));

        ERTS_CT_ASSERT(_HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE > 0);
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.shr(RETd, imm(_HEADER_ARITY_OFFS - _TAG_IMMED1_SIZE));
        a.or_(RETb, imm(_TAG_IMMED1_SMALL));
        mov_arg(Dst, RET);
    } else {
        /* Unknown type. Use the standard BIF instruction. */
        emit_i_bif1(Src, Fail, Bif, Dst);
    }
}
