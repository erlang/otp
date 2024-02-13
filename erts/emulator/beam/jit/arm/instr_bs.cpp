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
#include <numeric>

extern "C"
{
#include "erl_binary.h"
#include "erl_bits.h"
#include "beam_common.h"
}

/* Clobbers TMP1+TMP2
 *
 * Returns -1 when the field check always fails, 1 if it may fail, and 0 if it
 * never fails. */
int BeamModuleAssembler::emit_bs_get_field_size(const ArgSource &Size,
                                                int unit,
                                                Label fail,
                                                const a64::Gp &out) {
    if (Size.isImmed()) {
        if (Size.isSmall()) {
            Sint sval = Size.as<ArgSmall>().getSigned();

            if (sval < 0) {
                /* badarg */
            } else if (sval > (MAX_SMALL / unit)) {
                /* system_limit */
            } else {
                mov_imm(out, sval * unit);
                return 0;
            }
        }

        a.b(fail);
        return -1;
    } else {
        auto size_reg = load_source(Size, TMP2);
        bool can_fail = true;

        if (always_small(Size)) {
            auto [min, max] = getClampedRange(Size);
            can_fail =
                    !(0 <= min && (max >> (SMALL_BITS - ERL_UNIT_BITS)) == 0);
        }

        /* Negating the tag bits lets us guard against non-smalls, negative
         * numbers, and overflow with a single `tst` instruction. */
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        ASSERT(unit <= 1024);

        if (!can_fail) {
            comment("simplified segment size checks because "
                    "the types are known");
        }

        if (unit == 1 && !can_fail) {
            a.lsr(out, size_reg.reg, imm(_TAG_IMMED1_SIZE));
        } else {
            a.eor(out, size_reg.reg, imm(_TAG_IMMED1_SMALL));
        }

        if (can_fail) {
            a.tst(out, imm(0xFFF0000000000000UL | _TAG_IMMED1_MASK));
        }

        if (unit == 0) {
            /* Silly but legal.*/
            mov_imm(out, 0);
        } else if (unit == 1 && !can_fail) {
            /* The result is already in the out register. */
            ;
        } else if (Support::isPowerOf2(unit)) {
            int trailing_bits = Support::ctz<Eterm>(unit);

            /* The tag bits were cleared out by the argument check, so all we
             * need to do is shift the result into place. */
            if (trailing_bits < _TAG_IMMED1_SIZE) {
                a.lsr(out, out, imm(_TAG_IMMED1_SIZE - trailing_bits));
            } else if (trailing_bits > _TAG_IMMED1_SIZE) {
                a.lsl(out, out, imm(trailing_bits - _TAG_IMMED1_SIZE));
            }
        } else {
            if (unit >= (1 << _TAG_IMMED1_SIZE)) {
                mov_imm(TMP1, unit >> _TAG_IMMED1_SIZE);
            } else {
                a.lsr(out, out, imm(_TAG_IMMED1_SIZE));
                mov_imm(TMP1, unit);
            }

            a.mul(out, out, TMP1);
        }

        if (can_fail) {
            a.b_ne(fail);
        }

        return can_fail;
    }
}

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgWord &Size,
                                              const ArgWord &Heap,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    mov_arg(ARG4, Size);
    mov_arg(ARG5, Heap);
    mov_arg(ARG6, Live);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    mov_arg(Dst, ARG1);
}

/* Set the error reason when a size check has failed. */
void BeamGlobalAssembler::emit_bs_size_check_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime(0);

    a.mov(ARG1, c_p);
    runtime_call<2>(beam_jit_bs_field_size_argument_error);

    emit_leave_runtime(0);
    emit_leave_runtime_frame();

    mov_imm(ARG4, 0);
    a.b(labels[raise_exception]);
}

void BeamModuleAssembler::emit_i_bs_init_fail_heap(const ArgSource &Size,
                                                   const ArgWord &Heap,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    Label fail;

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, dispUnknown);
    } else {
        fail = a.newLabel();
    }

    if (emit_bs_get_field_size(Size, 1, fail, ARG4) >= 0) {
        mov_arg(ARG5, Heap);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                           Update::eReductions>(Live.get());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init);

        emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                           Update::eReductions>(Live.get());

        mov_arg(Dst, ARG1);
    }

    if (Fail.get() == 0) {
        Label next = a.newLabel();

        a.b(next);

        a.bind(fail);
        {
            mov_arg(ARG2, Size);
            fragment_call(ga->get_bs_size_check_shared());
        }

        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_bs_init(const ArgWord &Size,
                                         const ArgWord &Live,
                                         const ArgRegister &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);
    emit_i_bs_init_heap(Size, Heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_fail(const ArgRegister &Size,
                                              const ArgLabel &Fail,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);
    emit_i_bs_init_fail_heap(Size, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits(const ArgWord &NumBits,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    const ArgVal heap(ArgVal::Word, 0);
    emit_i_bs_init_bits_heap(NumBits, heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_heap(const ArgWord &NumBits,
                                                   const ArgWord &Alloc,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    mov_arg(ARG4, NumBits);
    mov_arg(ARG5, Alloc);
    mov_arg(ARG6, Live);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init_bits);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get());

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail(const ArgRegister &NumBits,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    const ArgVal Heap(ArgVal::Word, 0);

    emit_i_bs_init_bits_fail_heap(NumBits, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail_heap(
        const ArgSource &NumBits,
        const ArgWord &Alloc,
        const ArgLabel &Fail,
        const ArgWord &Live,
        const ArgRegister &Dst) {
    Label fail;

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, dispUnknown);
    } else {
        fail = a.newLabel();
    }

    if (emit_bs_get_field_size(NumBits, 1, fail, ARG4) >= 0) {
        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                           Update::eReductions>(Live.get());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init_bits);

        emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                           Update::eReductions>(Live.get());

        mov_arg(Dst, ARG1);
    }

    if (Fail.get() == 0) {
        Label next = a.newLabel();

        a.b(next);
        a.bind(fail);
        {
            mov_arg(ARG2, NumBits);
            fragment_call(ga->get_bs_size_check_shared());
        }

        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_put_string(const ArgWord &Size,
                                             const ArgBytePtr &Ptr) {
    mov_arg(ARG2, Ptr);
    mov_arg(ARG3, Size);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<3>(erts_new_bs_put_string);

    emit_leave_runtime();
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(const ArgSource &Src,
                                                        const ArgLabel &Fail,
                                                        const ArgWord &Sz,
                                                        const ArgWord &Flags) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);
    mov_arg(ARG4, Flags);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<4>(erts_new_bs_put_integer);

    emit_leave_runtime();

    if (Fail.get() != 0) {
        a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));
    } else {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_integer(const ArgLabel &Fail,
                                                    const ArgRegister &Sz,
                                                    const ArgWord &Flags,
                                                    const ArgSource &Src) {
    int unit = Flags.get() >> 3;
    Label next, fail;

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, dispUnknown);
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);
        mov_arg(ARG4, Flags);

        emit_enter_runtime();

        load_erl_bits_state(ARG1);
        runtime_call<4>(erts_new_bs_put_integer);

        emit_leave_runtime();

        if (Fail.get() != 0) {
            a.cbz(ARG1, fail);
        } else {
            a.cbnz(ARG1, next);
        }
    }

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary(const ArgLabel &Fail,
                                                   const ArgSource &Sz,
                                                   const ArgWord &Flags,
                                                   const ArgSource &Src) {
    int unit = Flags.get() >> 3;
    Label next, fail;

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, dispUnknown);
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);

        emit_enter_runtime<Update::eReductions>();

        a.mov(ARG1, c_p);
        runtime_call<3>(erts_new_bs_put_binary);

        emit_leave_runtime<Update::eReductions>();

        if (Fail.get() != 0) {
            a.cbz(ARG1, fail);
        } else {
            a.cbnz(ARG1, next);
        }
    }

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(const ArgSource &Src,
                                                       const ArgLabel &Fail,
                                                       const ArgWord &Unit) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Unit);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary_all);

    emit_leave_runtime<Update::eReductions>();

    if (Fail.get() == 0) {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(const ArgLabel &Fail,
                                                       const ArgWord &Sz,
                                                       const ArgSource &Src) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary);

    emit_leave_runtime<Update::eReductions>();

    if (Fail.get() == 0) {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float(const ArgLabel &Fail,
                                                  const ArgRegister &Sz,
                                                  const ArgWord &Flags,
                                                  const ArgSource &Src) {
    int unit = Flags.get() >> 3;
    Label next, fail;

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, dispUnknown);
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);
        mov_arg(ARG4, Flags);

        emit_enter_runtime();

        a.mov(ARG1, c_p);
        runtime_call<4>(erts_new_bs_put_float);

        emit_leave_runtime();

        if (Fail.get() != 0) {
            emit_branch_if_value(ARG1, fail);
        } else {
            emit_branch_if_not_value(ARG1, next);
        }
    }

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(const ArgLabel &Fail,
                                                      const ArgWord &Sz,
                                                      const ArgWord &Flags,
                                                      const ArgSource &Src) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);
    mov_arg(ARG4, Flags);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_new_bs_put_float);

    emit_leave_runtime();

    if (Fail.get() != 0) {
        emit_branch_if_value(ARG1, resolve_beam_label(Fail, disp1MB));
    } else {
        Label next = a.newLabel();

        emit_branch_if_not_value(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_bs_start_match3(const ArgRegister &Src,
                                                 const ArgWord &Live,
                                                 const ArgLabel &Fail,
                                                 const ArgRegister &Dst) {
    Label next = a.newLabel();

    mov_arg(ARG2, Src);

    if (Fail.get() != 0) {
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, ARG2);
    } else {
        /* bs_start_match3 may not throw, and the compiler will only emit {f,0}
         * when it knows that the source is a match state or binary, so we're
         * free to skip the binary tests. */
    }

    emit_untag_ptr(TMP1, ARG2);

    ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, thing_word, base_flags);
    a.ldp(TMP2, TMP3, arm::Mem(TMP1));

    ERTS_CT_ASSERT((HEADER_SUB_BITS & _TAG_PRIMARY_MASK) == 0 &&
                   (ERL_SUB_BITS_FLAG_MASK == _TAG_PRIMARY_MASK));
    a.bfi(TMP2, TMP3, imm(0), imm(2));
    a.cmp(TMP2, imm(HEADER_SUB_BITS | ERL_SUB_BITS_FLAGS_MATCH_CONTEXT));
    a.b_eq(next);

    {
        if (Fail.get() != 0) {
            const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
            ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
            ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                           (_TAG_HEADER_HEAP_BITS & mask));

            a.and_(TMP2, TMP2, imm(mask));
            a.cmp(TMP2, imm(_TAG_HEADER_HEAP_BITS));
            a.b_ne(resolve_beam_label(Fail, disp1MB));
        }

        emit_gc_test_preserve(ArgWord(ERL_SUB_BITS_SIZE), Live, Src, ARG2);

        emit_enter_runtime<Update::eHeapOnlyAlloc>(Live.get());

        a.mov(ARG1, c_p);
        /* ARG2 was set above */
        runtime_call<2>(erts_bs_start_match_3);

        emit_leave_runtime<Update::eHeapOnlyAlloc>(Live.get());

        a.add(ARG2, ARG1, imm(TAG_PRIMARY_BOXED));
    }

    a.bind(next);
    mov_arg(Dst, ARG2);
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgRegister &Ctx,
                                                 const ArgLabel &Fail,
                                                 const ArgWord &Bits,
                                                 const ArgBytePtr &Ptr) {
    const auto size = Bits.get();

    {
        auto ctx_reg = load_source(Ctx, TMP1);

        emit_untag_ptr(TMP5, ctx_reg.reg);
        ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
        a.ldp(TMP2, TMP3, arm::Mem(TMP5, offsetof(ErlSubBits, start)));
        add(TMP4, TMP2, size);
        a.cmp(TMP4, TMP3);
        a.b_hi(resolve_beam_label(Fail, disp1MB));

        /* ARG3 = (sb->base_flags & ~mask) + (sb->start >> 3) */
        a.ldr(TMP1, arm::Mem(TMP5, offsetof(ErlSubBits, base_flags)));
        a.and_(TMP1, TMP1, imm(~ERL_SUB_BITS_FLAG_MASK));
        a.add(ARG3, TMP1, TMP2, arm::lsr(3));

        /* ARG4 = sb->start & 7 */
        a.and_(ARG4, TMP2, imm(7));
    }

    emit_enter_runtime();

    mov_arg(ARG1, Ptr);
    mov_imm(ARG2, 0);
    mov_imm(ARG5, size);
    runtime_call<5>(erts_cmp_bits);

    emit_leave_runtime();
    a.cbnz(ARG1, resolve_beam_label(Fail, disp1MB));

    {
        auto ctx_reg = load_source(Ctx, TMP1);

        a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, offsetof(ErlSubBits, start)));
        add(TMP2, TMP2, size);
        a.stur(TMP2, emit_boxed_val(ctx_reg.reg, offsetof(ErlSubBits, start)));
    }
}

void BeamModuleAssembler::emit_i_bs_get_position(const ArgRegister &Ctx,
                                                 const ArgRegister &Dst) {
    const int start_offset = offsetof(ErlSubBits, start);
    auto ctx_reg = load_source(Ctx);
    auto dst_reg = init_destination(Dst, TMP2);

    /* Match contexts can never be literals, so we can skip clearing literal
     * tags. */
    a.ldur(dst_reg.reg, emit_boxed_val(ctx_reg.reg, start_offset));
    a.lsl(dst_reg.reg, dst_reg.reg, imm(_TAG_IMMED1_SIZE));
    a.orr(dst_reg.reg, dst_reg.reg, imm(_TAG_IMMED1_SMALL));

    flush_var(dst_reg);
}

void BeamModuleAssembler::emit_bs_get_integer2(const ArgLabel &Fail,
                                               const ArgRegister &Ctx,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Unit,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    Uint size;
    Uint flags = Flags.get();

    if (flags & BSF_NATIVE) {
        flags &= ~BSF_NATIVE;
        flags |= BSF_LITTLE;
    }

    if (Sz.isSmall() && Sz.as<ArgSmall>().getUnsigned() < 8 * sizeof(Uint) &&
        (size = Sz.as<ArgSmall>().getUnsigned() * Unit.get()) <
                8 * sizeof(Uint)) {
        /* Segment of a fixed size supported by bs_match. */
        const ArgVal match[] = {ArgAtom(am_ensure_at_least),
                                ArgWord(size),
                                ArgWord(1),
                                ArgAtom(am_integer),
                                Live,
                                ArgWord(flags),
                                ArgWord(size),
                                ArgWord(1),
                                Dst};

        const Span<ArgVal> args(match, sizeof(match) / sizeof(match[0]));
        emit_i_bs_match(Fail, Ctx, args);
    } else {
        Label fail = resolve_beam_label(Fail, dispUnknown);
        int unit = Unit.get();

        if (emit_bs_get_field_size(Sz, unit, fail, ARG5) >= 0) {
            /* This operation can be expensive if a bignum can be
             * created because there can be a garbage collection. */
            auto max = std::get<1>(getClampedRange(Sz));
            bool potentially_expensive =
                    max >= SMALL_BITS || (max * Unit.get()) >= SMALL_BITS;

            mov_arg(ARG3, Ctx);
            mov_imm(ARG4, flags);
            if (potentially_expensive) {
                mov_arg(ARG6, Live);
            } else {
#ifdef DEBUG
                /* Never actually used. */
                mov_imm(ARG6, 1023);
#endif
            }

            if (potentially_expensive) {
                emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                                   Update::eReductions>(Live.get());
            } else {
                comment("simplified entering runtime because result is always "
                        "small");
                emit_enter_runtime(Live.get());
            }

            a.mov(ARG1, c_p);
            if (potentially_expensive) {
                load_x_reg_array(ARG2);
            } else {
#ifdef DEBUG
                /* Never actually used. */
                mov_imm(ARG2, 0);
#endif
            }
            runtime_call<6>(beam_jit_bs_get_integer);

            if (potentially_expensive) {
                emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                                   Update::eReductions>(Live.get());
            } else {
                emit_leave_runtime(Live.get());
            }

            emit_branch_if_not_value(ARG1, fail);
            mov_arg(Dst, ARG1);
        }
    }
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgLabel &Fail,
                                             const ArgRegister &Ctx,
                                             const ArgWord &Offset) {
    const int start_offset = offsetof(ErlSubBits, start);
    const int end_offset = offsetof(ErlSubBits, end);

    auto ctx_reg = load_source(Ctx, TMP1);

    ASSERT(Offset.isWord());

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, end_offset));
    a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, start_offset));
    a.sub(TMP2, TMP2, TMP3);

    if (Offset.get() != 0) {
        cmp(TMP2, Offset.get());
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    } else {
        a.cbnz(TMP2, resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_bs_set_position(const ArgRegister &Ctx,
                                               const ArgRegister &Pos) {
    const int start_offset = offsetof(ErlSubBits, start);
    auto [ctx, pos] = load_sources(Ctx, TMP1, Pos, TMP2);

    a.lsr(TMP2, pos.reg, imm(_TAG_IMMED1_SIZE));
    a.stur(TMP2, emit_boxed_val(ctx.reg, start_offset));
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgRegister &Ctx,
                                                    const ArgLabel &Fail,
                                                    const ArgWord &Live,
                                                    const ArgWord &Unit,
                                                    const ArgRegister &Dst) {
    unsigned unit = Unit.get();

    mov_arg(ARG1, Ctx);

    emit_gc_test_preserve(ArgWord(BUILD_SUB_BITSTRING_HEAP_NEED),
                          Live,
                          Ctx,
                          ARG1);

    /* Make field fetching slightly more compact by pre-loading the match
     * context into the right argument slot for `erts_bs_get_binary_all_2`. */
    emit_untag_ptr(ARG2, ARG1);
    ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
    a.ldp(TMP2, TMP3, arm::Mem(ARG2, offsetof(ErlSubBits, start)));

    /* Remainder = Size - Offset */
    a.sub(TMP1, TMP3, TMP2);

    /* Unit may be 1 if compiling with +no_bsm3, which lacks the
     * bs_get_tail instruction. */
    if (unit > 1) {
        if ((unit & (unit - 1))) {
            mov_imm(TMP2, unit);

            a.udiv(TMP3, TMP1, TMP2);
            a.msub(TMP1, TMP3, TMP2, TMP1);

            a.cbnz(TMP1, resolve_beam_label(Fail, disp1MB));
        } else {
            a.tst(TMP1, imm(unit - 1));
            a.b_ne(resolve_beam_label(Fail, disp1MB));
        }
    }

    emit_enter_runtime<Update::eHeapOnlyAlloc>(Live.get());

    a.mov(ARG1, c_p);
    /* ARG2 was set above. */
    runtime_call<2>(erts_bs_get_binary_all_2);

    emit_leave_runtime<Update::eHeapOnlyAlloc>(Live.get());

    mov_arg(Dst, ARG1);
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    emit_untag_ptr(TMP1, ARG1);

    a.ldr(ARG4, arm::Mem(TMP1, offsetof(ErlSubBits, base_flags)));
    a.ldr(ARG2, arm::Mem(TMP1, offsetof(ErlSubBits, orig)));

    a.and_(ARG4, ARG4, imm(~ERL_SUB_BITS_FLAG_MASK));
    a.and_(ARG3, ARG2, imm(~TAG_PTR_MASK__));
    a.and_(ARG2, ARG2, imm(TAG_PTR_MASK__));

    /* Extracted size = sb->end - sb->start */
    ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
    a.ldp(ARG5, TMP1, arm::Mem(TMP1, offsetof(ErlSubBits, start)));
    a.sub(ARG6, TMP1, ARG5);

    lea(ARG1, arm::Mem(c_p, offsetof(Process, htop)));

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapOnlyAlloc>();

    runtime_call<6>(erts_build_sub_bitstring);

    emit_leave_runtime<Update::eHeapOnlyAlloc>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgRegister &Ctx,
                                           const ArgRegister &Dst,
                                           const ArgWord &Live) {
    mov_arg(ARG1, Ctx);

    emit_gc_test_preserve(ArgWord(BUILD_SUB_BITSTRING_HEAP_NEED),
                          Live,
                          Ctx,
                          ARG1);

    fragment_call(ga->get_bs_get_tail_shared());

    mov_arg(Dst, ARG1);
}

/* Bits to skip are passed in ARG1 */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgLabel &Fail,
                                            const ArgRegister &Ctx) {
    const int start_offset = offsetof(ErlSubBits, start);
    const int end_offset = offsetof(ErlSubBits, end);

    auto ctx_reg = load_source(Ctx, TMP1);

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, start_offset));
    a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, end_offset));

    a.add(TMP2, TMP2, ARG1);
    a.cmp(TMP2, TMP3);
    a.b_hi(resolve_beam_label(Fail, disp1MB));

    a.stur(TMP2, emit_boxed_val(ctx_reg.reg, start_offset));
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgRegister &Ctx,
                                               const ArgRegister &Size,
                                               const ArgLabel &Fail,
                                               const ArgWord &Unit) {
    Label fail = resolve_beam_label(Fail, dispUnknown);

    bool can_fail = true;

    if (always_small(Size)) {
        auto [min, max] = getClampedRange(Size);
        can_fail = !(0 <= min && (max >> (SMALL_BITS - ERL_UNIT_BITS)) == 0);
    }

    if (!can_fail && Unit.get() == 1) {
        comment("simplified skipping because the types are known");

        auto [ctx, size] = load_sources(Ctx, TMP1, Size, TMP2);

        emit_untag_ptr(TMP5, ctx.reg);
        ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
        a.ldp(TMP3, TMP4, arm::Mem(TMP5, offsetof(ErlSubBits, start)));

        a.add(TMP3, TMP3, size.reg, arm::lsr(_TAG_IMMED1_SIZE));
        a.cmp(TMP3, TMP4);
        a.b_hi(resolve_beam_label(Fail, disp1MB));

        a.str(TMP3, arm::Mem(TMP5, offsetof(ErlSubBits, start)));
    } else if (emit_bs_get_field_size(Size, Unit.get(), fail, ARG1) >= 0) {
        emit_bs_skip_bits(Fail, Ctx);
    }
}

void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(const ArgLabel &Fail,
                                                   const ArgRegister &Ctx,
                                                   const ArgWord &Bits) {
    mov_arg(ARG1, Bits);
    emit_bs_skip_bits(Fail, Ctx);
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgRegister &Ctx,
                                                const ArgLabel &Fail,
                                                const ArgWord &Live,
                                                const ArgSource &Size,
                                                const ArgWord &Flags,
                                                const ArgRegister &Dst) {
    Label fail;
    int unit;

    fail = resolve_beam_label(Fail, dispUnknown);
    unit = Flags.get() >> 3;

    if (emit_bs_get_field_size(Size, unit, fail, ARG2) >= 0) {
        a.str(ARG2, TMP_MEM1q);

        mov_arg(ARG4, Ctx);

        emit_gc_test_preserve(ArgWord(BUILD_SUB_BITSTRING_HEAP_NEED),
                              Live,
                              Ctx,
                              ARG4);

        emit_untag_ptr(ARG4, ARG4);

        emit_enter_runtime<Update::eHeapOnlyAlloc>(Live.get());

        a.mov(ARG1, c_p);
        a.ldr(ARG2, TMP_MEM1q);
        mov_imm(ARG3, Flags.get());
        runtime_call<4>(erts_bs_get_binary_2);

        emit_leave_runtime<Update::eHeapOnlyAlloc>(Live.get());

        emit_branch_if_not_value(ARG1, fail);

        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    Label fail;
    Sint unit;

    fail = resolve_beam_label(Fail, dispUnknown);
    unit = Flags.get() >> 3;

    mov_arg(ARG4, Ctx);

    emit_gc_test_preserve(ArgWord(FLOAT_SIZE_OBJECT), Live, Ctx, ARG4);

    if (emit_bs_get_field_size(Sz, unit, fail, ARG2) >= 0) {
        emit_untag_ptr(ARG4, ARG4);

        emit_enter_runtime<Update::eHeapOnlyAlloc>(Live.get());

        a.mov(ARG1, c_p);
        mov_imm(ARG3, Flags.get());
        runtime_call<4>(erts_bs_get_float_2);

        emit_leave_runtime<Update::eHeapOnlyAlloc>(Live.get());

        emit_branch_if_not_value(ARG1, fail);

        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_bs_utf8_size(const ArgSource &Src,
                                              const ArgXRegister &Dst) {
    auto src_reg = load_source(Src, TMP1);
    auto dst_reg = init_destination(Dst, TMP2);

    Label next = a.newLabel();

    /* Note that the source and destination registers could be the
     * same register. Therefore, we must copy the source register
     * before writing to the destination register. */
    a.lsr(TMP1, src_reg.reg, imm(_TAG_IMMED1_SIZE));
    mov_imm(dst_reg.reg, make_small(1));
    a.cmp(TMP1, imm(0x7F));
    a.b_ls(next);

    mov_imm(dst_reg.reg, make_small(2));
    a.cmp(TMP1, imm(0x7FFUL));
    a.b_ls(next);

    a.cmp(TMP1, imm(0x10000UL));
    mov_imm(TMP2, make_small(3));
    mov_imm(TMP3, make_small(4));
    a.csel(dst_reg.reg, TMP2, TMP3, arm::CondCode::kLO);

    a.bind(next);
    flush_var(dst_reg);
}

void BeamModuleAssembler::emit_i_bs_put_utf8(const ArgLabel &Fail,
                                             const ArgSource &Src) {
    mov_arg(ARG2, Src);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<2>(erts_bs_put_utf8);

    emit_leave_runtime();

    if (Fail.get() != 0) {
        a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));
    } else {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

/*
 * ARG1 = pointer to match state
 * ARG2 = number of bits left in binary (< 32)
 * ARG3 = position in binary in bits
 * ARG4 = base pointer to binary data
 *
 * See the comment for emit_bs_get_utf8_shared() for details about the
 * return value.
 */
void BeamGlobalAssembler::emit_bs_get_utf8_short_shared() {
    const int start_offset = offsetof(ErlSubBits, start);

    const a64::Gp match_context = ARG1;
    const a64::Gp bitdata = ARG2;
    const a64::Gp bin_position = ARG3;
    const a64::Gp bin_base = ARG4;

    Label two = a.newLabel();
    Label three_or_more = a.newLabel();
    Label four = a.newLabel();
    Label read_done = a.newLabel();
    Label ascii = a.newLabel();
    Label error = a.newLabel();

    /* Calculate the number of bytes remaining in the binary and error
     * out if less than one. */
    a.lsr(bitdata, bitdata, imm(3));
    a.cbz(bitdata, error);

    /* Calculate a byte mask so we can zero out trailing garbage. */
    a.neg(TMP5, bitdata, arm::lsl(3));
    mov_imm(TMP4, -1);
    a.lsl(TMP4, TMP4, TMP5);

    /* If the position in the binary is not byte-aligned, we'll need
     * to read one more byte. */
    a.ands(TMP1, bin_position, imm(7));
    a.cinc(bitdata, bitdata, imm(arm::CondCode::kNE));

    /* Set up pointer to the first byte to read. */
    a.add(TMP2, bin_base, bin_position, arm::lsr(3));

    a.cmp(bitdata, 2);
    a.b_eq(two);
    a.b_hi(three_or_more);

    /* Read one byte (always byte-aligned). */
    a.ldrb(bitdata.w(), arm::Mem(TMP2));
    a.b(read_done);

    /* Read two bytes. */
    a.bind(two);
    a.ldrh(bitdata.w(), arm::Mem(TMP2));
    a.b(read_done);

    a.bind(three_or_more);
    a.cmp(bitdata, 3);
    a.b_ne(four);

    /* Read three bytes. */
    a.ldrh(bitdata.w(), arm::Mem(TMP2));
    a.ldrb(TMP3.w(), arm::Mem(TMP2, 2));
    a.orr(bitdata, bitdata, TMP3, arm::lsl(16));
    a.b(read_done);

    /* Read four bytes (always unaligned). */
    a.bind(four);
    a.ldr(bitdata.w(), arm::Mem(TMP2));

    /* Handle the bytes read. */
    a.bind(read_done);
    a.rev64(bitdata, bitdata);
    a.lsl(bitdata, bitdata, TMP1);
    a.and_(bitdata, bitdata, TMP4);
    a.tbz(bitdata, imm(63), ascii);
    a.b(labels[bs_get_utf8_shared]);

    /* Handle plain old ASCII (code point < 128). */
    a.bind(ascii);
    a.add(bin_position, bin_position, imm(8));
    a.str(bin_position, arm::Mem(match_context, start_offset));
    a.mov(ARG1, imm(_TAG_IMMED1_SMALL));
    a.orr(ARG1, ARG1, bitdata, arm::lsr(56 - _TAG_IMMED1_SIZE));
    a.ret(a64::x30);

    /* Signal error. */
    a.bind(error);
    mov_imm(ARG1, 0);
    a.ret(a64::x30);
}

/*
 * ARG1 = pointer to match state
 * ARG2 = 4 bytes read from the binary in big-endian order
 * ARG3 = position in binary in bits
 *
 * On successful return, the extracted code point is a term tagged
 * small in ARG1 and the position in the match state has been updated. On
 * failure, ARG1 contains an invalid term where the tags bits are zero.
 */
void BeamGlobalAssembler::emit_bs_get_utf8_shared() {
    const int start_offset = offsetof(ErlSubBits, start);

    const a64::Gp match_context = ARG1;
    const a64::Gp bitdata = ARG2;
    const a64::Gp bin_position = ARG3;

    const a64::Gp byte_count = ARG4;

    const a64::Gp shift = TMP4;
    const a64::Gp control_mask = TMP5;
    const a64::Gp error_mask = TMP6;

    /* UTF-8 has the following layout, where 'x' are data bits:
     *
     * 1 byte:  0xxxxxxx (not handled by this path)
     * 2 bytes: 110xxxxx, 10xxxxxx
     * 3 bytes: 1110xxxx, 10xxxxxx 10xxxxxx
     * 4 bytes: 11110xxx, 10xxxxxx 10xxxxxx 10xxxxxx
     *
     * Note that the number of leading bits is equal to the number of bytes,
     * which makes it very easy to create masks for extraction and error
     * checking. */

    /* Calculate the number of bytes. */
    a.cls(byte_count, bitdata);
    a.add(byte_count, byte_count, imm(1));

    /* Get rid of the prefix bits. */
    a.lsl(bitdata, bitdata, byte_count);
    a.lsr(bitdata, bitdata, byte_count);

    /* Calculate the bit shift now before we start to corrupt the
     * byte_count. */
    mov_imm(shift, 64);
    a.sub(shift, shift, byte_count, arm::lsl(3));

    /* Shift down the value to the least significant part of the word. */
    a.lsr(bitdata, bitdata, shift);

    /* Matches the '10xxxxxx' components, leaving the header byte alone. */
    mov_imm(error_mask, 0x00808080ull << 32);
    a.lsr(error_mask, error_mask, shift);

    /* Construct the control mask '0x00C0C0C0' (already shifted). */
    a.orr(control_mask, error_mask, error_mask, arm::lsr(1));

    /* Assert that the header bits of each '10xxxxxx' component are correct,
     * signaling errors by trashing the byte count with an illegal
     * value (0). */
    a.and_(TMP3, bitdata, control_mask);
    a.cmp(TMP3, error_mask);

    a.ubfx(TMP1, bitdata, imm(8), imm(6));
    a.ubfx(TMP2, bitdata, imm(16), imm(6));
    a.ubfx(TMP3, bitdata, imm(24), imm(3));
    a.ubfx(bitdata, bitdata, imm(0), imm(6));

    a.orr(bitdata, bitdata, TMP1, arm::lsl(6));
    a.orr(bitdata, bitdata, TMP2, arm::lsl(12));
    a.orr(bitdata, bitdata, TMP3, arm::lsl(18));

    /* Check for too large code point. */
    mov_imm(TMP1, 0x10FFFF);
    a.ccmp(bitdata, TMP1, imm(NZCV::kCF), arm::CondCode::kEQ);

    /* Check for the illegal range 16#D800 - 16#DFFF. */
    a.lsr(TMP1, bitdata, imm(11));
    a.ccmp(TMP1, imm(0xD800 >> 11), imm(NZCV::kZF), arm::CondCode::kLS);
    a.csel(byte_count, byte_count, ZERO, imm(arm::CondCode::kNE));

    /* Test for overlong UTF-8 sequence. That can be done by testing
     * that the bits marked y below are all zero.
     *
     * 1 byte:  0xxxxxxx (not handled by this path)
     * 2 bytes: 110yyyyx, 10xxxxxx
     * 3 bytes: 1110yyyy, 10yxxxxx 10xxxxxx
     * 4 bytes: 11110yyy, 10yyxxxx 10xxxxxx 10xxxxxx
     *
     * 1 byte:                   xx'xxxxx
     * 2 bytes:             y'yyyxx'xxxxx
     * 3 bytes:       y'yyyyx'xxxxx'xxxxx
     * 4 bytes: y'yyyyx'xxxxx'xxxxx'xxxxx
     *
     * The y bits can be isolated by shifting down by the number of bits
     * shown in this table:
     *
     * 2:  7    (byte_count * 4 - 1)
     * 3: 11    (byte_count * 4 - 1)
     * 4: 16    (byte_count * 4)
     */

    /* Calculate number of bits to shift. */
    a.lsl(TMP1, byte_count, imm(2));
    a.cmp(byte_count, imm(4));
    a.csetm(TMP2, imm(arm::CondCode::kNE));
    a.add(TMP1, TMP1, TMP2);

    /* Pre-fill the tag bits so that we can clear them on error. */
    mov_imm(TMP2, _TAG_IMMED1_SMALL);

    /* Now isolate the y bits and compare to zero. This check will
     * be used in a CCMP further down. */
    a.lsr(TMP1, bitdata, TMP1);
    a.cmp(TMP1, 0);

    /* Byte count must be 2, 3, or 4. */
    a.sub(TMP1, byte_count, imm(2));
    a.ccmp(TMP1, imm(2), imm(NZCV::kCF), imm(arm::CondCode::kNE));

    /* If we have failed, we set byte_count to zero to ensure that the
     * position update nops, and set the pre-tagged result to zero so
     * that we can check for error in module code by testing the tag
     * bits. */
    a.csel(byte_count, byte_count, ZERO, imm(arm::CondCode::kLS));
    a.csel(TMP2, TMP2, ZERO, imm(arm::CondCode::kLS));

    a.add(bin_position, bin_position, byte_count, arm::lsl(3));
    a.str(bin_position, arm::Mem(match_context, start_offset));
    a.orr(ARG1, TMP2, bitdata, arm::lsl(_TAG_IMMED1_SIZE));

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgRegister &Ctx,
                                           const ArgLabel &Fail) {
    const int base_offset = offsetof(ErlSubBits, base_flags);
    const int start_offset = offsetof(ErlSubBits, start);

    const a64::Gp match_context = ARG1;
    const a64::Gp bitdata = ARG2;
    const a64::Gp bin_position = ARG3;
    const a64::Gp bin_base = ARG4;
    const a64::Gp bin_size = ARG5;

    auto ctx = load_source(Ctx, ARG6);

    Label non_ascii = a.newLabel();
    Label fallback = a.newLabel();
    Label check = a.newLabel();
    Label done = a.newLabel();

    emit_untag_ptr(match_context, ctx.reg);

    ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
    a.ldp(bin_position, bin_size, arm::Mem(ARG1, start_offset));
    a.ldr(bin_base, arm::Mem(ARG1, base_offset));
    a.and_(bin_base, bin_base, imm(~ERL_SUB_BITS_FLAG_MASK));
    a.sub(bitdata, bin_size, bin_position);
    a.cmp(bitdata, imm(32));
    a.b_lo(fallback);

    emit_read_bits(32, bin_base, bin_position, bitdata);
    a.tbnz(bitdata, imm(63), non_ascii);

    /* Handle plain old ASCII (code point < 128). */
    a.add(bin_position, bin_position, imm(8));
    a.str(bin_position, arm::Mem(ARG1, start_offset));
    a.mov(ARG1, imm(_TAG_IMMED1_SMALL));
    a.orr(ARG1, ARG1, bitdata, arm::lsr(56 - _TAG_IMMED1_SIZE));
    a.b(done);

    /* Handle code point >= 128. */
    a.bind(non_ascii);
    fragment_call(ga->get_bs_get_utf8_shared());
    a.b(check);

    /*
     * Handle the case that there are not 4 bytes available in the binary.
     */

    a.bind(fallback);
    fragment_call(ga->get_bs_get_utf8_short_shared());

    a.bind(check);
    ERTS_CT_ASSERT((_TAG_IMMED1_SMALL & 1) != 0);
    a.tbz(ARG1, imm(0), resolve_beam_label(Fail, disp32K));

    a.bind(done);
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgRegister &Ctx,
                                             const ArgLabel &Fail,
                                             const ArgRegister &Dst) {
    emit_bs_get_utf8(Ctx, Fail);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgRegister &Ctx,
                                              const ArgLabel &Fail) {
    emit_bs_get_utf8(Ctx, Fail);
}

void BeamModuleAssembler::emit_i_bs_utf16_size(const ArgSource &Src,
                                               const ArgXRegister &Dst) {
    auto src_reg = load_source(Src, TMP1);
    auto dst_reg = init_destination(Dst, TMP2);

    /* erts_bs_put_utf16 errors out whenever something's fishy, so we can
     * return garbage (2 or 4) if our input is not a small. */
    a.asr(TMP1, src_reg.reg, imm(_TAG_IMMED1_SIZE));
    a.cmp(TMP1, imm(0x10000UL));
    mov_imm(TMP1, make_small(2));
    mov_imm(TMP2, make_small(4));
    a.csel(dst_reg.reg, TMP1, TMP2, arm::CondCode::kLO);

    flush_var(dst_reg);
}

void BeamModuleAssembler::emit_i_bs_put_utf16(const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgSource &Src) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Flags);

    emit_enter_runtime();

    load_erl_bits_state(ARG1);
    runtime_call<3>(erts_bs_put_utf16);

    emit_leave_runtime();

    if (Fail.get() != 0) {
        a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));
    } else {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgRegister &Ctx,
                                            const ArgLabel &Fail,
                                            const ArgWord &Flags) {
    auto ctx_reg = load_source(Ctx, TMP1);

    emit_untag_ptr(ARG1, ctx_reg.reg);

    emit_enter_runtime();

    mov_imm(ARG2, Flags.get());
    runtime_call<2>(erts_bs_get_utf16);

    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgRegister &Ctx,
                                              const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgRegister &Dst) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Flags) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
}

void BeamModuleAssembler::emit_validate_unicode(Label next,
                                                Label fail,
                                                a64::Gp value) {
    ASSERT(value != TMP2);

    a.and_(TMP2, value, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
    a.b_ne(fail);

    mov_imm(TMP2, make_small(0xD800UL));
    a.cmp(value, TMP2);
    a.b_lo(next);

    mov_imm(TMP2, make_small(0xDFFFUL));
    a.cmp(value, TMP2);
    a.b_ls(fail);

    mov_imm(TMP2, make_small(0x10FFFFUL));
    a.cmp(value, TMP2);
    a.b_hi(fail);

    a.b(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgLabel &Fail,
                                                     const ArgSource &Src) {
    auto src_reg = load_source(Src, TMP1);
    Label fail, next = a.newLabel();

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, dispUnknown);
    } else {
        fail = a.newLabel();
    }

    emit_validate_unicode(next, fail, src_reg.reg);

    if (Fail.get() == 0) {
        a.bind(fail);
        emit_error(BADARG);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(
        const ArgLabel &Fail,
        const ArgSource &Src,
        const ArgRegister &Ms) {
    Label fail = a.newLabel(), next = a.newLabel();
    auto src_reg = load_source(Src, TMP1);

    emit_validate_unicode(next, fail, src_reg.reg);

    a.bind(fail);
    {
        const int start_offset = offsetof(ErlSubBits, start);
        auto ctx_reg = load_source(Ms, TMP2);

        a.ldur(TMP1, emit_boxed_val(ctx_reg.reg, start_offset));
        a.sub(TMP1, TMP1, imm(32));
        a.stur(TMP1, emit_boxed_val(ctx_reg.reg, start_offset));

        if (Fail.get() != 0) {
            a.b(resolve_beam_label(Fail, disp128MB));
        } else {
            emit_error(BADARG);
        }
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgLabel &Fail,
                                            const ArgRegister &Ctx,
                                            const ArgWord &Unit) {
    auto ctx_reg = load_source(Ctx, TMP1);
    unsigned int unit = Unit.get();

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, offsetof(ErlSubBits, end)));
    a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, offsetof(ErlSubBits, start)));

    a.sub(TMP1, TMP2, TMP3);

    if ((unit & (unit - 1))) {
        mov_imm(TMP2, unit);

        a.udiv(TMP3, TMP1, TMP2);
        a.msub(TMP1, TMP3, TMP2, TMP1);

        a.cbnz(TMP1, resolve_beam_label(Fail, disp1MB));
    } else {
        a.tst(TMP1, imm(unit - 1));
        a.b_ne(resolve_beam_label(Fail, dispUnknown));
    }
}

/* ARG2 = current `Size`,
 * ARG3 = elements to `Add`,
 * ARG4 = element `Unit`
 *
 * Error is indicated through cond_ne() */
void BeamGlobalAssembler::emit_bs_add_guard_shared() {
    Label error = a.newLabel();

    /* Since `Unit` is guaranteed to be less than 1024, we can check overflow
     * and negative numbers by testing whether any of the highest 12 value bits
     * are set on either argument. */
    a.orr(TMP1, ARG2, ARG3);
    a.tst(TMP1, imm(0xFFF0000000000000UL));

    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.ccmp(TMP1,
           imm(_TAG_IMMED1_SMALL),
           imm(NZCV::kNone),
           imm(arm::CondCode::kEQ));
    a.b_ne(error);

    /* Return `Size` + `Add` * `Unit`
     *
     * Note that the result takes the tag bits from `Size` */
    a.and_(TMP2, ARG3, imm(~_TAG_IMMED1_SMALL));
    a.mul(TMP2, TMP2, ARG4);
    a.add(ARG1, TMP2, ARG2);

    a.bind(error);
    a.ret(a64::x30);
}

/* ARG2 = current `Size`,
 * ARG3 = elements to `Add`,
 * ARG4 = element `Unit` */
void BeamGlobalAssembler::emit_bs_add_body_shared() {
    Label error = a.newLabel();

    /* Since `Unit` is guaranteed to be less than 1024, we can check overflow
     * and negative numbers by testing whether any of the highest 12 value bits
     * are set on either argument. */
    a.orr(TMP1, ARG2, ARG3);
    a.tst(TMP1, imm(0xFFF0000000000000UL));

    a.and_(TMP1, ARG2, ARG3);
    a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    a.ccmp(TMP1,
           imm(_TAG_IMMED1_SMALL),
           imm(NZCV::kNone),
           imm(arm::CondCode::kEQ));
    a.b_ne(error);

    /* Return `Size` + `Add` * `Unit`
     *
     * Note that the result takes the tag bits from `Size` */
    a.and_(TMP2, ARG3, imm(~_TAG_IMMED1_SMALL));
    a.mul(TMP2, TMP2, ARG4);
    a.add(ARG1, TMP2, ARG2);
    a.ret(a64::x30);

    a.bind(error);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime(0);

        a.mov(ARG1, c_p);
        runtime_call<3>(beam_jit_bs_add_argument_error);

        emit_leave_runtime(0);
        emit_leave_runtime_frame();

        mov_imm(ARG4, 0);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_bs_add(const ArgLabel &Fail,
                                      const ArgSource &Size,
                                      const ArgSource &Add,
                                      const ArgWord &Unit,
                                      const ArgXRegister &Dst) {
    ASSERT(Unit.get() < 1024);

    mov_arg(ARG2, Size);
    mov_arg(ARG3, Add);
    mov_arg(ARG4, Unit);

    if (Fail.get() == 0) {
        fragment_call(ga->get_bs_add_body_shared());
    } else {
        fragment_call(ga->get_bs_add_guard_shared());
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_append(const ArgLabel &Fail,
                                           const ArgWord &ExtraHeap,
                                           const ArgWord &Live,
                                           const ArgWord &Unit,
                                           const ArgSource &Size,
                                           const ArgSource &Bin,
                                           const ArgRegister &Dst) {
    mov_arg(ARG3, Live);
    mov_arg(ARG4, Size);
    mov_arg(ARG5, ExtraHeap);
    mov_arg(ARG6, Unit);

    mov_arg(ArgXRegister(Live.get()), Bin);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get() + 1);

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<6>(erts_bs_append);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>(Live.get() + 1);

    if (Fail.get() != 0) {
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        Label next = a.newLabel();

        emit_branch_if_value(ARG1, next);
        /* The error has been prepared in `erts_bs_append` */
        emit_raise_exception();

        a.bind(next);
    }

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_private_append(const ArgLabel &Fail,
                                                   const ArgWord &Unit,
                                                   const ArgSource &Size,
                                                   const ArgRegister &Src,
                                                   const ArgXRegister &Dst) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Size);
    mov_arg(ARG4, Unit);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_bs_private_append);

    emit_leave_runtime();

    if (Fail.get() != 0) {
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        Label next = a.newLabel();

        emit_branch_if_value(ARG1, next);
        /* The error has been prepared in `erts_bs_private_append` */
        emit_raise_exception();

        a.bind(next);
    }

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_bs_init_writable() {
    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 0);

    a.mov(ARG1, c_p);
    a.mov(ARG2, XREG0);

    /* We have an implicit liveness of 0, so we don't need to stash X
     * registers. */
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>(0);

    runtime_call<2>(erts_bs_init_writable);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>(0);

    a.mov(XREG0, ARG1);
}

void BeamGlobalAssembler::emit_bs_create_bin_error_shared() {
    a.mov(XREG0, a64::x30);

    emit_enter_runtime<Update::eHeapAlloc>(0);

    /* ARG3 is already set by the caller */
    a.mov(ARG2, ARG4);
    a.mov(ARG4, ARG1);
    a.mov(ARG1, c_p);
    runtime_call<4>(beam_jit_bs_construct_fail_info);

    emit_leave_runtime<Update::eHeapAlloc>(0);

    a.mov(ARG4, ZERO);
    a.mov(ARG2, XREG0);
    a.b(labels[raise_exception_shared]);
}

/*
 * ARG1 = tagged bignum term
 */
void BeamGlobalAssembler::emit_get_sint64_shared() {
    Label success = a.newLabel();
    Label fail = a.newLabel();

    emit_is_boxed(fail, ARG1);
    a64::Gp boxed_ptr = emit_ptr_val(TMP3, ARG1);
    a.ldr(TMP1, emit_boxed_val(boxed_ptr));
    a.ldr(TMP2, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(POS_BIG_SUBTAG));
    a.b_eq(success);

    a.cmp(TMP1, imm(NEG_BIG_SUBTAG));
    a.b_ne(fail);

    a.neg(TMP2, TMP2);

    a.bind(success);
    {
        a.mov(ARG1, TMP2);
        /* Clear Z flag.
         *
         * TMP1 is known to be POS_BIG_SUBTAG or NEG_BIG_SUBTAG at this point.
         */
        ERTS_CT_ASSERT(POS_BIG_SUBTAG != 0 && NEG_BIG_SUBTAG != 0);
        a.tst(TMP1, TMP1);
        a.ret(a64::x30);
    }

    a.bind(fail);
    {
        a.tst(ZERO, ZERO);
        a.ret(a64::x30);
    }
}

struct BscSegment {
    BscSegment()
            : type(am_false), unit(1), flags(0), src(ArgNil()), size(ArgNil()),
              error_info(0), offsetInAccumulator(0), effectiveSize(-1),
              action(action::DIRECT) {
    }

    Eterm type;
    Uint unit;
    Uint flags;
    ArgVal src;
    ArgVal size;

    Uint error_info;
    Uint offsetInAccumulator;
    Sint effectiveSize;

    /* Here are sub actions for storing integer segments.
     *
     * We use the ACCUMULATE action to accumulator values of segments
     * with known, small sizes (no more than 64 bits) into an
     * accumulator register.
     *
     * When no more segments can be accumulated, the STORE action is
     * used to store the value of the accumulator into the binary.
     *
     * The DIRECT action is used when it is not possible to use the
     * accumulator (for unknown or too large sizes).
     */
    enum class action { DIRECT, ACCUMULATE, STORE } action;
};

static std::vector<BscSegment> bs_combine_segments(
        const std::vector<BscSegment> segments) {
    std::vector<BscSegment> segs;

    for (auto seg : segments) {
        switch (seg.type) {
        case am_integer: {
            if (!(0 < seg.effectiveSize && seg.effectiveSize <= 64)) {
                /* Unknown or too large size. Handle using the default
                 * DIRECT action. */
                segs.push_back(seg);
                continue;
            }

            if (seg.flags & BSF_LITTLE || segs.size() == 0 ||
                segs.back().action == BscSegment::action::DIRECT) {
                /* There are no previous compatible ACCUMULATE / STORE
                 * actions. Create the first ones. */
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            auto prev = segs.back();
            if (prev.flags & BSF_LITTLE) {
                /* Little-endian segments cannot be combined with other
                 * segments. Create new ACCUMULATE / STORE actions. */
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            /* The current segment is compatible with the previous
             * segment. Try combining them. */
            if (prev.effectiveSize + seg.effectiveSize <= 64) {
                /* The combined values of the segments fit in the
                 * accumulator. Insert an ACCUMULATE action for the
                 * current segment before the pre-existing STORE
                 * action. */
                segs.pop_back();
                prev.effectiveSize += seg.effectiveSize;
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                segs.push_back(prev);
            } else {
                /* The size exceeds 64 bits. Can't combine. */
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
            }
            break;
        }
        default:
            segs.push_back(seg);
            break;
        }
    }

    /* Calculate bit offsets for each ACCUMULATE segment. */

    Uint offset = 0;
    for (int i = segs.size() - 1; i >= 0; i--) {
        switch (segs[i].action) {
        case BscSegment::action::STORE:
            offset = 64 - segs[i].effectiveSize;
            break;
        case BscSegment::action::ACCUMULATE:
            segs[i].offsetInAccumulator = offset;
            offset += segs[i].effectiveSize;
            break;
        default:
            break;
        }
    }

    return segs;
}

/*
 * In:
 *    bin_offset = register to store the bit offset into the binary
 *    bit_offset = current bit offset into binary, or -1 if unknown
 *    size = size of segment to be constructed
 *           (ignored if size_reg is valid register)
 *    size_reg = if a valid register, it contains the size of
 *               the segment to be constructed
 *
 * Out:
 *    bin_offset register = if bit_offset is not byte aligned, the bit
 *          offset into the binary
 *    TMP1 = pointer to the current byte in the binary
 *
 *    Preserves all other ARG* registers.
 */
void BeamModuleAssembler::update_bin_state(a64::Gp bin_offset,
                                           Sint bit_offset,
                                           Sint size,
                                           a64::Gp size_reg) {
    int cur_bin_offset = offsetof(ErtsSchedulerRegisters,
                                  aux_regs.d.erl_bits_state.erts_current_bin_);
    arm::Mem mem_bin_base = arm::Mem(scheduler_registers, cur_bin_offset);
    arm::Mem mem_bin_offset =
            arm::Mem(scheduler_registers, cur_bin_offset + sizeof(Eterm));

    if (bit_offset % 8 != 0) {
        /* The bit offset is unknown or not byte-aligned. */
        ERTS_CT_ASSERT_FIELD_PAIR(struct erl_bits_state,
                                  erts_current_bin_,
                                  erts_bin_offset_);
        a.ldp(TMP2, bin_offset, mem_bin_base);

        if (size_reg.isValid()) {
            a.add(TMP1, bin_offset, size_reg);
        } else {
            add(TMP1, bin_offset, size);
        }
        a.str(TMP1, mem_bin_offset);

        a.add(TMP1, TMP2, bin_offset, arm::lsr(3));
    } else {
        comment("optimized updating of binary construction state");
        ASSERT(size >= 0 || size_reg.isValid());
        ASSERT(bit_offset % 8 == 0);
        a.ldr(TMP1, mem_bin_base);
        if (size_reg.isValid()) {
            if (bit_offset == 0) {
                a.str(size_reg, mem_bin_offset);
            } else {
                add(TMP2, size_reg, bit_offset);
                a.str(TMP2, mem_bin_offset);
            }
        } else {
            mov_imm(TMP2, bit_offset + size);
            a.str(TMP2, mem_bin_offset);
        }
        if (bit_offset != 0) {
            add(TMP1, TMP1, bit_offset >> 3);
        }
    }
}

/*
 * The size of the segment is assumed to be in ARG3.
 */
void BeamModuleAssembler::set_zero(Sint effectiveSize) {
    Label store_units = a.newLabel();
    Label less_than_a_store_unit = a.newLabel();
    Sint store_unit = 1;

    update_bin_state(ARG2, -1, -1, ARG3);

    if (effectiveSize >= 256) {
        /* Store four 64-bit words machine words when the size is
         * known and at least 256 bits. */
        store_unit = 4;
        a.movi(a64::d31, 0);
    } else if (effectiveSize >= 128) {
        /* Store two 64-bit words machine words when the size is
         * known and at least 128 bits. */
        store_unit = 2;
    }

    if (effectiveSize < Sint(store_unit * 8 * sizeof(Eterm))) {
        /* The size is either not known or smaller than a word. */
        a.cmp(ARG3, imm(store_unit * 8 * sizeof(Eterm)));
        a.b_lt(less_than_a_store_unit);
    }

    a.bind(store_units);
    if (store_unit == 4) {
        a.stp(a64::q31, a64::q31, arm::Mem(TMP1).post(sizeof(Eterm[4])));
    } else if (store_unit == 2) {
        a.stp(ZERO, ZERO, arm::Mem(TMP1).post(sizeof(Eterm[2])));
    } else {
        a.str(ZERO, arm::Mem(TMP1).post(sizeof(Eterm)));
    }
    a.sub(ARG3, ARG3, imm(store_unit * 8 * sizeof(Eterm)));

    a.cmp(ARG3, imm(store_unit * 8 * sizeof(Eterm)));
    a.b_ge(store_units);

    a.bind(less_than_a_store_unit);
    if (effectiveSize < 0) {
        /* Unknown size. */
        Label byte_loop = a.newLabel();
        Label done = a.newLabel();

        ASSERT(store_unit = 1);

        a.cbz(ARG3, done);

        a.bind(byte_loop);
        a.strb(ZERO.w(), arm::Mem(TMP1).post(1));
        a.subs(ARG3, ARG3, imm(8));
        a.b_gt(byte_loop);

        a.bind(done);
    } else if (effectiveSize % (store_unit * 8 * sizeof(Eterm)) != 0) {
        /* The size is known, and we know that there are less than
         * 256 bits to initialize. */
        if (store_unit == 4 && (effectiveSize & 255) >= 128) {
            a.stp(ZERO, ZERO, arm::Mem(TMP1).post(16));
        }

        if ((effectiveSize & 127) >= 64) {
            a.str(ZERO, arm::Mem(TMP1).post(8));
        }

        if ((effectiveSize & 63) >= 32) {
            a.str(ZERO.w(), arm::Mem(TMP1).post(4));
        }

        if ((effectiveSize & 31) >= 16) {
            a.strh(ZERO.w(), arm::Mem(TMP1).post(2));
        }

        if ((effectiveSize & 15) >= 8) {
            a.strb(ZERO.w(), arm::Mem(TMP1).post(1));
        }

        if ((effectiveSize & 7) > 0) {
            a.strb(ZERO.w(), arm::Mem(TMP1));
        }
    }
}

/*
 * In:
 *
 *   ARG1 = valid unicode code point (=> 0x80) to encode
 *
 * Out:
 *
 *   ARG1 = the code point encoded in UTF-8.
 *   ARG4 = number of bits of result (16, 24, or 32)
 *
 *   Preserves other ARG* registers, clobbers TMP* registers
 */
void BeamGlobalAssembler::emit_construct_utf8_shared() {
    Label more_than_two_bytes = a.newLabel();
    Label four_bytes = a.newLabel();
    const a64::Gp value = ARG1;
    const a64::Gp num_bits = ARG4;

    a.cmp(value, imm(0x800));
    a.b_hs(more_than_two_bytes);

    /* Encode Unicode code point in two bytes. */
    a.ubfiz(TMP1, value, imm(8), imm(6));
    mov_imm(TMP2, 0x80c0);
    a.orr(TMP1, TMP1, value, arm::lsr(6));
    mov_imm(num_bits, 16);
    a.orr(value, TMP1, TMP2);
    a.ret(a64::x30);

    /* Test whether the value should be encoded in four bytes. */
    a.bind(more_than_two_bytes);
    a.lsr(TMP1, value, imm(16));
    a.cbnz(TMP1, four_bytes);

    /* Encode Unicode code point in three bytes. */
    a.lsl(TMP1, value, imm(2));
    a.ubfiz(TMP2, value, imm(16), imm(6));
    a.and_(TMP1, TMP1, imm(0x3f00));
    mov_imm(num_bits, 24);
    a.orr(TMP1, TMP1, value, arm::lsr(12));
    a.orr(TMP1, TMP1, TMP2);
    mov_imm(TMP2, 0x8080e0);
    a.orr(value, TMP1, TMP2);
    a.ret(a64::x30);

    /* Encode Unicode code point in four bytes. */
    a.bind(four_bytes);
    a.lsl(TMP1, value, imm(10));
    a.lsr(TMP2, value, imm(4));
    a.and_(TMP1, TMP1, imm(0x3f0000));
    a.and_(TMP2, TMP2, imm(0x3f00));
    a.bfxil(TMP1, value, imm(18), imm(14));
    mov_imm(num_bits, 32);
    a.bfi(TMP1, value, imm(24), imm(6));
    a.orr(TMP1, TMP1, TMP2);
    mov_imm(TMP2, 0x808080f0);
    a.orr(value, TMP1, TMP2);
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_construct_utf8(const ArgVal &Src,
                                              Sint bit_offset,
                                              bool is_byte_aligned) {
    Label prepare_store = a.newLabel();
    Label store = a.newLabel();
    Label next = a.newLabel();

    comment("construct utf8 segment");
    auto src = load_source(Src, ARG1);

    a.lsr(ARG1, src.reg, imm(_TAG_IMMED1_SIZE));
    mov_imm(ARG4, 8);
    a.cmp(ARG1, imm(0x80));
    a.b_lo(prepare_store);

    fragment_call(ga->get_construct_utf8_shared());

    a.bind(prepare_store);
    a64::Gp bin_offset = ARG3;
    update_bin_state(bin_offset, bit_offset, -1, ARG4);

    if (!is_byte_aligned) {
        /* Not known to be byte-aligned. Must test alignment. */
        a.ands(TMP2, bin_offset, imm(7));
        a.b_eq(store);

        /* We must combine the last partial byte with the UTF-8
         * encoded code point. */
        a.ldrb(TMP5.w(), arm::Mem(TMP1));

        a.rev64(TMP4, ARG1);
        a.lsr(TMP4, TMP4, TMP2);
        a.rev64(TMP4, TMP4);

        a.lsl(TMP5, TMP5, TMP2);
        a.and_(TMP5, TMP5, imm(~0xff));
        a.lsr(TMP5, TMP5, TMP2);

        a.orr(ARG1, TMP4, TMP5);

        a.add(ARG4, ARG4, imm(8));
    }

    a.bind(store);
    if (bit_offset % (4 * 8) == 0) {
        /* This segment is aligned on a 4-byte boundary. This implies
         * that a 4-byte write will be inside the allocated binary. */
        a.str(ARG1.w(), arm::Mem(TMP1));
    } else {
        Label do_store_1 = a.newLabel();
        Label do_store_2 = a.newLabel();

        /* Unsuitable or unknown alignment. We must be careful not
         * to write beyound the allocated end of the binary. */
        a.cmp(ARG4, imm(8));
        a.b_ne(do_store_1);

        a.strb(ARG1.w(), arm::Mem(TMP1));
        a.b(next);

        a.bind(do_store_1);
        a.cmp(ARG4, imm(24));
        a.b_hi(do_store_2);

        a.strh(ARG1.w(), arm::Mem(TMP1));
        a.cmp(ARG4, imm(16));
        a.b_eq(next);

        a.lsr(ARG1, ARG1, imm(16));
        a.strb(ARG1.w(), arm::Mem(TMP1, 2));
        a.b(next);

        a.bind(do_store_2);
        a.str(ARG1.w(), arm::Mem(TMP1));

        if (!is_byte_aligned) {
            a.cmp(ARG4, imm(32));
            a.b_eq(next);

            a.lsr(ARG1, ARG1, imm(32));
            a.strb(ARG1.w(), arm::Mem(TMP1, 4));
        }
    }

    a.bind(next);
}

/*
 * In:
 *   TMP1 = pointer to current byte
 *   ARG3 = bit offset
 *   ARG4 = number of bits to write
 *   ARG8 = data to write
 */
void BeamGlobalAssembler::emit_store_unaligned() {
    Label loop = a.newLabel();
    Label done = a.newLabel();
    const a64::Gp left_bit_offset = ARG3;
    const a64::Gp right_bit_offset = TMP6;
    const a64::Gp num_bits = ARG4;
    const a64::Gp bitdata = ARG8;

    a.ldrb(TMP5.w(), arm::Mem(TMP1));

    a.and_(TMP4, bitdata, imm(0xff));
    a.lsr(TMP4, TMP4, left_bit_offset);

    a.lsl(TMP5, TMP5, left_bit_offset);
    a.and_(TMP5, TMP5, imm(~0xff));
    a.lsr(TMP5, TMP5, left_bit_offset);

    a.orr(TMP5, TMP4, TMP5);

    a.strb(TMP5.w(), arm::Mem(TMP1).post(1));

    mov_imm(right_bit_offset, 8);
    a.sub(right_bit_offset, right_bit_offset, left_bit_offset);

    a.rev64(bitdata, bitdata);
    a.lsl(bitdata, bitdata, right_bit_offset);

    a.subs(num_bits, num_bits, right_bit_offset);
    a.b_le(done);

    a.bind(loop);
    a.ror(bitdata, bitdata, imm(56));
    a.strb(bitdata.w(), arm::Mem(TMP1).post(1));
    a.subs(num_bits, num_bits, imm(8));
    a.b_gt(loop);

    a.bind(done);
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_i_bs_create_bin(const ArgLabel &Fail,
                                               const ArgWord &Alloc,
                                               const ArgWord &Live0,
                                               const ArgRegister &Dst,
                                               const Span<ArgVal> &args) {
    Uint num_bits = 0;
    Uint estimated_num_bits = 0;
    std::size_t n = args.size();
    std::vector<BscSegment> segments;
    Label error; /* Intentionally uninitialized */
    ArgWord Live = Live0;
    a64::Gp sizeReg;
    Sint allocated_size = -1;
    bool need_error_handler = false;

    /*
     * Collect information about each segment and calculate sizes of
     * fixed segments.
     */
    for (std::size_t i = 0; i < n; i += 6) {
        BscSegment seg;
        JitBSCOp bsc_op;
        Uint bsc_segment;

        seg.type = args[i].as<ArgImmed>().get();
        bsc_segment = args[i + 1].as<ArgWord>().get();
        seg.unit = args[i + 2].as<ArgWord>().get();
        seg.flags = args[i + 3].as<ArgWord>().get();
        seg.src = args[i + 4];
        seg.size = args[i + 5];

        switch (seg.type) {
        case am_float:
            bsc_op = BSC_OP_FLOAT;
            break;
        case am_integer:
            bsc_op = BSC_OP_INTEGER;
            break;
        case am_utf8:
            bsc_op = BSC_OP_UTF8;
            break;
        case am_utf16:
            bsc_op = BSC_OP_UTF16;
            break;
        case am_utf32:
            bsc_op = BSC_OP_UTF32;
            break;
        default:
            bsc_op = BSC_OP_BITSTRING;
            break;
        }

        /*
         * Save segment number and operation for use in extended
         * error information.
         */
        seg.error_info = beam_jit_set_bsc_segment_op(bsc_segment, bsc_op);

        /*
         * Test whether we can omit the code for the error handler.
         */
        switch (seg.type) {
        case am_append:
            if (!(exact_type<BeamTypeId::Bitstring>(seg.src) &&
                  std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit)) {
                need_error_handler = true;
            }
            break;
        case am_binary:
            if (!(seg.size.isAtom() && seg.size.as<ArgAtom>().get() == am_all &&
                  exact_type<BeamTypeId::Bitstring>(seg.src) &&
                  std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit)) {
                need_error_handler = true;
            }
            break;
        case am_integer:
            if (!exact_type<BeamTypeId::Integer>(seg.src)) {
                need_error_handler = true;
            }
            break;
        case am_private_append:
        case am_string:
            break;
        default:
            need_error_handler = true;
            break;
        }

        /*
         * Attempt to calculate the effective size of this segment.
         * Give up if variable or invalid.
         */
        if (seg.size.isSmall() && seg.unit != 0) {
            Uint unsigned_size = seg.size.as<ArgSmall>().getUnsigned();

            if ((unsigned_size >> (sizeof(Eterm) - 1) * 8) != 0) {
                /* Suppress creation of heap bitstring. */
                estimated_num_bits += ERL_ONHEAP_BITS_LIMIT + 8;
            } else {
                /* This multiplication cannot overflow. */
                Uint seg_size = seg.unit * unsigned_size;
                seg.effectiveSize = seg_size;
                num_bits += seg_size;
                estimated_num_bits += seg_size;
            }
        } else if (seg.unit > 0) {
            if ((seg.unit % 8) == 0) {
                auto max = std::min(std::get<1>(getClampedRange(seg.size)),
                                    Sint(ERL_ONHEAP_BITS_LIMIT + 8));
                estimated_num_bits += max * seg.unit;
            } else {
                /* May create a non-binary bitstring in some cases, suppress
                 * creation of heap bitstring. */
                estimated_num_bits += ERL_ONHEAP_BITS_LIMIT + 8;
            }
        } else {
            switch (seg.type) {
            case am_utf8:
            case am_utf16:
            case am_utf32:
                estimated_num_bits += 32;
                break;
            default:
                /* Suppress creation of heap bitstring. */
                estimated_num_bits += ERL_ONHEAP_BITS_LIMIT + 8;
                break;
            }
        }

        if (seg.effectiveSize < 0 && seg.type != am_append &&
            seg.type != am_private_append) {
            /* At least one segment will need a dynamic size
             * calculation. */
            sizeReg = ARG8;
            need_error_handler = true;
        }

        segments.insert(segments.end(), seg);
    }

    if (need_error_handler && Fail.get() != 0) {
        error = resolve_beam_label(Fail, dispUnknown);
    } else if (need_error_handler) {
        Label past_error = a.newLabel();

        a.b(past_error);

        error = a.newLabel();
        a.bind(error);
        {
            /*
             * ARG1 = optional bad size value; valid if BSC_VALUE_ARG1 is set in
             * ARG4
             *
             * ARG3 = optional bad size value; valid if BSC_VALUE_ARG3 is
             * set in ARG4
             *
             * ARG4 = packed error information
             */
            comment("handle error");
            fragment_call(ga->get_bs_create_bin_error_shared());
            last_error_offset = a.offset();
        }

        a.bind(past_error);
    } else {
        comment("(cannot fail)");
    }

    /* We count the total number of bits in an unsigned integer. To
     * avoid having to check for overflow when adding to the counter,
     * we ensure that the signed size of each segment fits in a
     * word. */
    if (sizeReg.isValid()) {
        comment("calculate sizes");
        mov_imm(sizeReg, num_bits);
    }

    /* Generate code for calculating the size of the binary to be
     * created. */
    for (auto seg : segments) {
        if (seg.effectiveSize >= 0) {
            continue;
        }

        if (seg.type == am_append || seg.type == am_private_append) {
            continue;
        }

        if (seg.size.isAtom() && seg.size.as<ArgAtom>().get() == am_all &&
            seg.type == am_binary) {
            comment("size of an entire bitstring");

            mov_arg(ARG1, seg.src);

            if (!exact_type<BeamTypeId::Bitstring>(seg.src)) {
                /* Note: ARG1 must remain equal to seg.src here for error
                 * reporting to work. */
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                }

                emit_is_boxed(resolve_label(error, dispUnknown), seg.src, ARG1);
            }

            emit_untag_ptr(TMP4, ARG1);

            ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
            a.ldp(TMP1, TMP2, arm::Mem(TMP4));

            if (masked_types<BeamTypeId::MaybeBoxed>(seg.src) ==
                BeamTypeId::Bitstring) {
                comment("optimized size code because the value is always a "
                        "bitstring when boxed");
            } else {
                const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
                ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
                ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                               (_TAG_HEADER_HEAP_BITS & mask));

                a.and_(TMP3, TMP1, imm(mask));
                a.cmp(TMP3, imm(_TAG_HEADER_HEAP_BITS));
                a.b_ne(resolve_label(error, disp1MB));
            }

            Label not_sub_bits = a.newLabel();
            a.cmp(TMP1, imm(HEADER_SUB_BITS));
            a.b_ne(not_sub_bits);
            {
                ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
                a.ldp(TMP2, TMP3, arm::Mem(TMP4, offsetof(ErlSubBits, start)));
                a.sub(TMP2, TMP3, TMP2);
            }
            a.bind(not_sub_bits);

            a.add(sizeReg, sizeReg, TMP2);
        } else if (seg.unit != 0) {
            bool can_fail = true;
            comment("size binary/integer/float/string");

            if (always_small(seg.size)) {
                auto min = std::get<0>(getClampedRange(seg.size));
                if (min >= 0) {
                    can_fail = false;
                }
            }

            mov_arg(ARG3, seg.size);

            if (can_fail && Fail.get() == 0) {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_DEPENDS,
                                                        BSC_INFO_SIZE,
                                                        BSC_VALUE_ARG3));
            }

            if (always_small(seg.size)) {
                comment("skipped test for small size since it is always small");
            } else if (always_one_of<BeamTypeId::Number>(seg.size)) {
                comment("simplified test for small size since it is a number");
                emit_is_not_boxed(error, ARG3);
            } else {
                a.and_(TMP2, ARG3, imm(_TAG_IMMED1_MASK));
                a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
                a.b_ne(resolve_label(error, disp1MB));
            }
            if (can_fail) {
                a.tbnz(ARG3, 63, resolve_label(error, disp32K));
            }
            if (seg.unit == 1) {
                a.add(sizeReg, sizeReg, ARG3, arm::asr(_TAG_IMMED1_SIZE));
            } else {
                a.asr(TMP1, ARG3, imm(_TAG_IMMED1_SIZE));
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(
                                    seg.error_info,
                                    BSC_REASON_SYSTEM_LIMIT,
                                    BSC_INFO_SIZE,
                                    BSC_VALUE_ARG3));
                }
                a.tst(TMP1, imm(0xffful << (SMALL_BITS - ERL_UNIT_BITS)));
                a.b_ne(resolve_label(error, disp1MB));
                mov_imm(TMP2, seg.unit);
                a.madd(sizeReg, TMP1, TMP2, sizeReg);
            }
        } else {
            switch (seg.type) {
            case am_utf8: {
                comment("size utf8");
                Label next = a.newLabel();

                mov_arg(ARG3, seg.src);

                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG3));
                }

                if (always_small(seg.src)) {
                    comment("skipped test for small value since it is always "
                            "small");
                } else if (always_one_of<BeamTypeId::Integer,
                                         BeamTypeId::AlwaysBoxed>(seg.src)) {
                    comment("simplified test for small operand since other "
                            "types are boxed");
                    emit_is_not_boxed(resolve_label(error, dispUnknown), ARG3);
                } else {
                    a.and_(TMP1, ARG3, imm(_TAG_IMMED1_MASK));
                    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
                    a.b_ne(resolve_label(error, disp1MB));
                }

                a.asr(TMP1, ARG3, imm(_TAG_IMMED1_SIZE));
                mov_imm(TMP2, 1);
                a.cmp(TMP1, imm(0x7F));
                a.b_ls(next);

                mov_imm(TMP2, 2);
                a.cmp(TMP1, imm(0x7FFUL));
                a.b_ls(next);

                /* Ensure that the value is not in the invalid range
                 * 0xD800 through 0xDFFF. */
                a.lsr(TMP3, TMP1, imm(11));
                a.cmp(TMP3, 0x1b);
                a.b_eq(resolve_label(error, disp1MB));

                a.cmp(TMP1, imm(0x10000UL));
                a.cset(TMP2, arm::CondCode::kHS);
                a.add(TMP2, TMP2, imm(3));

                auto [min, max] = getClampedRange(seg.src);
                if (0 <= min && max < 0x110000) {
                    comment("skipped range check for unicode code point");
                } else {
                    a.cmp(TMP1, 0x110000);
                    a.b_hs(resolve_label(error, disp1MB));
                }

                a.bind(next);
                a.add(sizeReg, sizeReg, TMP2, arm::lsl(3));
                break;
            }
            case am_utf16: {
                /* erts_bs_put_utf16 errors out whenever something's
                 * fishy, so we can return garbage (16 or 32) if our
                 * input is not a small. */
                comment("size utf16");
                auto src_reg = load_source(seg.src, TMP1);

                a.asr(TMP1, src_reg.reg, imm(_TAG_IMMED1_SIZE));
                a.cmp(TMP1, imm(0x10000UL));
                mov_imm(TMP1, 2 * 8);
                mov_imm(TMP2, 4 * 8);
                a.csel(TMP1, TMP1, TMP2, arm::CondCode::kLO);
                a.add(sizeReg, sizeReg, TMP1);
                break;
            }
            case am_utf32: {
                Label next = a.newLabel();

                comment("size utf32");
                mov_arg(ARG3, seg.src);

                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG3));
                }

                a.add(sizeReg, sizeReg, imm(4 * 8));

                a.and_(TMP2, ARG3, imm(_TAG_IMMED1_MASK));
                a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
                a.b_ne(error);

                mov_imm(TMP2, make_small(0xD800UL));
                a.cmp(ARG3, TMP2);
                a.b_lo(next);

                mov_imm(TMP2, make_small(0xDFFFUL));
                a.cmp(ARG3, TMP2);
                a.b_ls(error);

                mov_imm(TMP2, make_small(0x10FFFFUL));
                a.cmp(ARG3, TMP2);
                a.b_hi(error);

                a.bind(next);
                break;
            }
            default:
                ASSERT(0);
                break;
            }
        }
    }

    /* Allocate the binary. */
    if (segments[0].type == am_append) {
        BscSegment seg = segments[0];
        comment("append to binary");
        mov_arg(ARG3, Live);
        if (sizeReg.isValid()) {
            a.mov(ARG4, sizeReg);
        } else {
            mov_imm(ARG4, num_bits);
        }
        mov_arg(ARG5, Alloc);
        mov_imm(ARG6, seg.unit);
        mov_arg(ArgXRegister(Live.get()), seg.src);
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);

        emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                           Update::eReductions>(Live.get() + 1);
        runtime_call<6>(erts_bs_append_checked);
        emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                           Update::eReductions>(Live.get() + 1);

        if (exact_type<BeamTypeId::Bitstring>(seg.src) &&
            std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit) {
            /* There is no way the call can fail with a system_limit
             * exception on a 64-bit architecture. */
            comment("skipped test for success because units are compatible");
        } else {
            if (Fail.get() == 0) {
                mov_arg(ARG3, ArgXRegister(Live.get()));
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_FVALUE,
                                                        BSC_VALUE_ARG3));
            }
            emit_branch_if_not_value(ARG1, resolve_label(error, dispUnknown));
        }
    } else if (segments[0].type == am_private_append) {
        BscSegment seg = segments[0];
        comment("private append to binary");
        ASSERT(Alloc.get() == 0);
        mov_arg(ARG2, seg.src);
        if (sizeReg.isValid()) {
            a.mov(ARG3, sizeReg);
        } else {
            mov_imm(ARG3, num_bits);
        }
        a.mov(ARG4, seg.unit);
        a.mov(ARG1, c_p);
        emit_enter_runtime(Live.get());
        runtime_call<4>(erts_bs_private_append_checked);
        emit_leave_runtime(Live.get());
        /* There is no way the call can fail on a 64-bit architecture. */
    } else if (estimated_num_bits <= ERL_ONHEAP_BITS_LIMIT) {
        static constexpr auto cur_bin_offset =
                offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state) +
                offsetof(struct erl_bits_state, erts_current_bin_);
        Uint need;

        arm::Mem mem_bin_base = arm::Mem(scheduler_registers, cur_bin_offset);

        if (sizeReg.isValid()) {
            Label after_gc_check = a.newLabel();

            comment("allocate heap bitstring of dynamic size (=< %ld bits)",
                    estimated_num_bits);

            /* Calculate number of bits to allocate, rounded up to term
             * alignment as it'll be allocated on the process heap. */
            need = (heap_bits_size(0) + Alloc.get() + S_RESERVED);
            a.add(TMP3, sizeReg, imm(sizeof(Eterm) * 8 - 1));
            a.and_(TMP3, TMP3, imm(~(sizeof(Eterm) * 8 - 1)));
            add(TMP1, TMP3, need * sizeof(Eterm) * 8);

            /* Do a GC test. Note that TMP1 is in bits. */
            a.add(ARG3, HTOP, TMP1, arm::lsr(3));
            a.cmp(ARG3, E);
            a.b_ls(after_gc_check);

            a.stp(sizeReg, TMP3, TMP_MEM1q);

            mov_imm(ARG4, Live.get());
            fragment_call(ga->get_garbage_collect());

            a.ldp(sizeReg, TMP3, TMP_MEM1q);

            a.bind(after_gc_check);

            /* As TMP3 is the number of bits rounded up to term alignment,
             * the 6 lowest bits will be clear, and since _HEADER_ARITY_OFFS
             * happens to be 6 bits, simply adding the header constant is
             * enough to build the header word. */
            ERTS_CT_ASSERT(sizeof(Eterm) * 8 == (1 << _HEADER_ARITY_OFFS));
            a.add(TMP1, TMP3, imm(header_heap_bits(0)));

            /* Create the heap bitstring. */
            a.add(ARG1, HTOP, imm(TAG_PRIMARY_BOXED));
            a.stp(TMP1, sizeReg, arm::Mem(HTOP).post(sizeof(Eterm[2])));

            /* Initialize the erl_bin_state struct. */
            a.stp(HTOP, ZERO, mem_bin_base);

            /* Update HTOP, note that TMP3 is in bits. */
            a.add(HTOP, HTOP, TMP3, arm::lsr(3));
        } else {
            Uint heap_size = heap_bits_size(num_bits);

            comment("allocate heap bitstring of static size");
            ERTS_CT_ASSERT(offsetof(ErlHeapBits, data) ==
                           (heap_bits_size(0) * sizeof(Eterm)));
            allocated_size = (heap_size - heap_bits_size(0)) * sizeof(Eterm);

            /* Ensure that there is sufficient room on the heap. */
            emit_gc_test(ArgWord(0), ArgWord(heap_size + Alloc.get()), Live);

            mov_imm(TMP1, header_heap_bits(num_bits));
            mov_imm(TMP2, num_bits);

            /* Create the heap bitstring. */
            a.add(ARG1, HTOP, imm(TAG_PRIMARY_BOXED));
            a.stp(TMP1, TMP2, arm::Mem(HTOP).post(sizeof(Eterm[2])));

            /* Initialize the erl_bin_state struct. */
            ERTS_CT_ASSERT_FIELD_PAIR(struct erl_bits_state,
                                      erts_current_bin_,
                                      erts_bin_offset_);
            a.stp(HTOP, ZERO, mem_bin_base);

            /* Update HTOP. */
            a.add(HTOP, HTOP, imm(allocated_size));
        }
    } else {
        comment("allocate binary");
        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);
        load_erl_bits_state(ARG3);
        load_x_reg_array(ARG2);
        a.mov(ARG1, c_p);
        emit_enter_runtime<Update::eReductions | Update::eHeapAlloc |
                           Update::eXRegs>(Live.get());
        if (sizeReg.isValid()) {
            comment("(size in bits)");
            a.mov(ARG4, sizeReg);
            runtime_call<6>(beam_jit_bs_init_bits);
        } else {
            allocated_size = NBYTES(num_bits);
            if (num_bits <= ERL_ONHEAP_BITS_LIMIT) {
                /* On-heap bitstring allocations are rounded up to multiples of
                 * sizeof(Eterm). */
                allocated_size = (allocated_size + (sizeof(Eterm) - 1)) &
                                 ~((sizeof(Eterm) - 1));
            }
            mov_imm(ARG4, num_bits);
            runtime_call<6>(beam_jit_bs_init_bits);
        }
        emit_leave_runtime<Update::eReductions | Update::eHeapAlloc |
                           Update::eXRegs>(Live.get());
    }
    a.str(ARG1, TMP_MEM1q);

    segments = bs_combine_segments(segments);

    /* Keep track of the bit offset from the being of the binary.
     * Set to -1 if offset is not known (when a segment of unknown
     * size has been seen). */
    Sint bit_offset = 0;

    /* Keep track of whether the current segment is byte-aligned.  (A
     * segment can be known to be byte-aligned even if the bit offset
     * is unknown.) */
    bool is_byte_aligned = true;

    /* Build each segment of the binary. */
    for (auto seg : segments) {
        switch (seg.type) {
        case am_append:
        case am_private_append:
            bit_offset = -1;
            break;
        case am_binary: {
            Uint error_info;
            bool can_fail = true;

            comment("construct a binary segment");
            if (seg.effectiveSize >= 0) {
                /* The segment has a literal size. */
                mov_imm(ARG3, seg.effectiveSize);
                mov_arg(ARG2, seg.src);
                a.mov(ARG1, c_p);
                emit_enter_runtime<Update::eReductions>(Live.get());
                runtime_call<3>(erts_new_bs_put_binary);
                emit_leave_runtime<Update::eReductions>(Live.get());
                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_DEPENDS,
                                                             BSC_VALUE_FVALUE);
            } else if (seg.size.isAtom() &&
                       seg.size.as<ArgAtom>().get() == am_all) {
                /* Include the entire binary/bitstring in the
                 * resulting binary. */
                a.mov(ARG3, seg.unit);
                mov_arg(ARG2, seg.src);
                a.mov(ARG1, c_p);

                emit_enter_runtime<Update::eReductions>(Live.get());
                runtime_call<3>(erts_new_bs_put_binary_all);
                emit_leave_runtime<Update::eReductions>(Live.get());

                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_UNIT,
                                                             BSC_VALUE_FVALUE);
                if (exact_type<BeamTypeId::Bitstring>(seg.src) &&
                    std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit) {
                    comment("skipped test for success because units are "
                            "compatible");
                    can_fail = false;
                }
            } else {
                /* The size is a variable. We have verified that
                 * the value is a non-negative small in the
                 * appropriate range. Multiply the size with the
                 * unit. */
                auto r = load_source(seg.size, ARG3);
                a.asr(ARG3, r.reg, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(TMP1, seg.unit);
                    a.mul(ARG3, ARG3, TMP1);
                }
                mov_arg(ARG2, seg.src);
                a.mov(ARG1, c_p);

                emit_enter_runtime<Update::eReductions>(Live.get());
                runtime_call<3>(erts_new_bs_put_binary);
                emit_leave_runtime<Update::eReductions>(Live.get());

                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_DEPENDS,
                                                             BSC_VALUE_FVALUE);
            }

            if (can_fail) {
                if (Fail.get() == 0) {
                    mov_imm(ARG4, error_info);
                }
                a.cbz(ARG1, resolve_label(error, disp1MB));
            }
            break;
        }
        case am_float:
            comment("construct float segment");
            if (seg.effectiveSize >= 0) {
                mov_imm(ARG3, seg.effectiveSize);
            } else {
                auto r = load_source(seg.size, ARG3);
                a.asr(ARG3, r.reg, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(TMP1, seg.unit);
                    a.mul(ARG3, ARG3, TMP1);
                }
            }
            mov_arg(ARG2, seg.src);
            mov_imm(ARG4, seg.flags);
            a.mov(ARG1, c_p);

            emit_enter_runtime(Live.get());
            runtime_call<4>(erts_new_bs_put_float);
            emit_leave_runtime(Live.get());

            if (Fail.get() == 0) {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_FVALUE,
                                                        BSC_VALUE_ARG1));
            }
            emit_branch_if_value(ARG1, resolve_label(error, dispUnknown));
            break;
        case am_integer:
            switch (seg.action) {
            case BscSegment::action::ACCUMULATE: {
                /* Shift an integer of known size (no more than 64 bits)
                 * into a word-size accumulator. */
                Label value_is_small = a.newLabel();
                Label done = a.newLabel();
                auto offset = seg.offsetInAccumulator;

                comment("accumulate value for integer segment at offset %ld",
                        offset);

                auto src = load_source(seg.src, ARG1);

                if (!always_small(seg.src)) {
                    if (always_one_of<BeamTypeId::Integer,
                                      BeamTypeId::AlwaysBoxed>(seg.src)) {
                        comment("simplified small test since all other types "
                                "are boxed");
                        emit_is_boxed(value_is_small, seg.src, src.reg);
                    } else {
                        a.and_(TMP1, src.reg, imm(_TAG_IMMED1_MASK));
                        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
                        a.b_eq(value_is_small);
                    }

                    /* The value is boxed. If it is a bignum, extract the
                     * least significant 64 bits. */
                    mov_var(ARG1, src);
                    fragment_call(ga->get_get_sint64_shared());
                    if (seg.effectiveSize == 64) {
                        a.mov(ARG8, ARG1);
                    } else {
                        a.bfi(ARG8,
                              ARG1,
                              arm::lsr(offset),
                              imm(seg.effectiveSize));
                    }

                    if (exact_type<BeamTypeId::Integer>(seg.src)) {
                        a.b(done);
                    } else {
                        a.b_ne(done);

                        /* Not a bignum. Signal error. */
                        if (Fail.get() == 0) {
                            mov_imm(ARG4,
                                    beam_jit_update_bsc_reason_info(
                                            seg.error_info,
                                            BSC_REASON_BADARG,
                                            BSC_INFO_TYPE,
                                            BSC_VALUE_ARG1));
                        }
                        a.b(resolve_label(error, disp128MB));
                    }
                }

                a.bind(value_is_small);
                if (seg.effectiveSize == 64) {
                    a.asr(ARG8, src.reg, imm(_TAG_IMMED1_SIZE));
                } else if (offset >= _TAG_IMMED1_SIZE) {
                    a.bfi(ARG8,
                          src.reg,
                          arm::lsr(offset - _TAG_IMMED1_SIZE),
                          imm(seg.effectiveSize + _TAG_IMMED1_SIZE));
                } else if (offset == 0 && seg.effectiveSize <= SMALL_BITS) {
                    a.bfxil(ARG8,
                            src.reg,
                            imm(_TAG_IMMED1_SIZE),
                            imm(seg.effectiveSize));
                } else {
                    a.asr(TMP1, src.reg, imm(_TAG_IMMED1_SIZE));
                    a.bfi(ARG8, TMP1, imm(offset), imm(seg.effectiveSize));
                }

                a.bind(done);
                break;
            }
            case BscSegment::action::STORE: {
                /* The accumulator is now full or the next segment is
                 * not possible to accumulate, so it's time to store
                 * the accumulator to the current position in the
                 * binary. */
                Label store = a.newLabel();
                Label done = a.newLabel();

                comment("construct integer segment from accumulator");

                /* First we'll need to ensure that the value in the
                 * accumulator is in little endian format. */
                ASSERT(seg.effectiveSize >= 0);
                if ((seg.flags & BSF_LITTLE) == 0) {
                    a.rev64(ARG8, ARG8);
                } else {
                    Uint complete_bytes = 8 * (seg.effectiveSize / 8);
                    Uint num_partial = seg.effectiveSize % 8;

                    if (seg.effectiveSize < 64) {
                        a.lsr(ARG8, ARG8, imm(64 - seg.effectiveSize));
                    }

                    if ((seg.effectiveSize % 8) != 0) {
                        a.ubfx(TMP1,
                               ARG8,
                               imm(complete_bytes),
                               imm(num_partial));
                        a.bfi(ARG8,
                              TMP1,
                              imm(complete_bytes + 8 - num_partial),
                              imm(num_partial));
                    }
                }

                a64::Gp bin_offset = ARG3;
                a64::Gp bin_data = ARG8;

                update_bin_state(bin_offset,
                                 bit_offset,
                                 seg.effectiveSize,
                                 a64::Gp());

                if (!is_byte_aligned) {
                    if (bit_offset < 0) {
                        /* Bit offset is unknown. Must test alignment. */
                        a.ands(bin_offset, bin_offset, imm(7));
                        a.b_eq(store);
                    } else if (bit_offset >= 0) {
                        /* Alignment is known to be unaligned. */
                        mov_imm(bin_offset, bit_offset & 7);
                    }

                    /* Bit offset is tested or known to be unaligned. */
                    mov_imm(ARG4, seg.effectiveSize);
                    fragment_call(ga->get_store_unaligned());

                    if (bit_offset < 0) {
                        /* The bit offset is unknown, which implies that
                         * there exists store code that we will need to
                         * branch past. */
                        a.b(done);
                    }
                }

                a.bind(store);

                if (bit_offset < 0 || is_byte_aligned) {
                    /* Bit offset is tested or known to be
                     * byte-aligned. Emit inline code to store the
                     * value of the accumulator into the binary. */
                    int num_bytes = (seg.effectiveSize + 7) / 8;

                    /* If more than one instruction is required for
                     * doing the store, test whether it would be safe
                     * to do a single 32 or 64 bit store. */
                    switch (num_bytes) {
                    case 3:
                        if (bit_offset >= 0 &&
                            allocated_size * 8 - bit_offset >= 32) {
                            comment("simplified complicated store");
                            num_bytes = 4;
                        }
                        break;
                    case 5:
                    case 6:
                    case 7:
                        if (bit_offset >= 0 &&
                            allocated_size * 8 - bit_offset >= 64) {
                            comment("simplified complicated store");
                            num_bytes = 8;
                        }
                        break;
                    }

                    do {
                        switch (num_bytes) {
                        case 1:
                            a.strb(bin_data.w(), arm::Mem(TMP1));
                            break;
                        case 2:
                            a.strh(bin_data.w(), arm::Mem(TMP1));
                            break;
                        case 3:
                            a.strh(bin_data.w(), arm::Mem(TMP1));
                            a.lsr(bin_data, bin_data, imm(16));
                            a.strb(bin_data.w(), arm::Mem(TMP1, 2));
                            break;
                        case 4:
                            a.str(bin_data.w(), arm::Mem(TMP1));
                            break;
                        case 5:
                        case 6:
                        case 7:
                            a.str(bin_data.w(), arm::Mem(TMP1).post(4));
                            a.lsr(bin_data, bin_data, imm(32));
                            break;
                        case 8:
                            a.str(bin_data, arm::Mem(TMP1));
                            num_bytes = 0;
                            break;
                        }
                        num_bytes -= 4;
                    } while (num_bytes > 0);
                }

                a.bind(done);
                break;
            }
            case BscSegment::action::DIRECT:
                /* This segment either has a size exceeding the maximum
                 * accumulator size of 64 bits or has a variable size.
                 *
                 * First load the effective size (size * unit) into ARG3.
                 */
                comment("construct integer segment");
                if (seg.effectiveSize >= 0) {
                    mov_imm(ARG3, seg.effectiveSize);
                } else {
                    auto size = load_source(seg.size, TMP1);
                    a.lsr(ARG3, size.reg, imm(_TAG_IMMED1_SIZE));
                    if (Support::isPowerOf2(seg.unit)) {
                        Uint trailing_bits = Support::ctz<Eterm>(seg.unit);
                        if (trailing_bits) {
                            a.lsl(ARG3, ARG3, imm(trailing_bits));
                        }
                    } else {
                        mov_imm(TMP1, seg.unit);
                        a.mul(ARG3, ARG3, TMP1);
                    }
                }

                if (is_byte_aligned && seg.src.isSmall() &&
                    seg.src.as<ArgSmall>().getSigned() == 0) {
                    /* Optimize the special case of setting a known
                     * byte-aligned segment to zero. */
                    comment("optimized setting segment to 0");
                    set_zero(seg.effectiveSize);
                } else {
                    /* Call the helper function to fetch and store the
                     * integer into the binary. */
                    mov_arg(ARG2, seg.src);
                    mov_imm(ARG4, seg.flags);
                    load_erl_bits_state(ARG1);

                    emit_enter_runtime(Live.get());
                    runtime_call<4>(erts_new_bs_put_integer);
                    emit_leave_runtime(Live.get());

                    if (exact_type<BeamTypeId::Integer>(seg.src)) {
                        comment("skipped test for success because construction "
                                "can't fail");
                    } else {
                        if (Fail.get() == 0) {
                            mov_arg(ARG3, seg.src);
                            mov_imm(ARG4,
                                    beam_jit_update_bsc_reason_info(
                                            seg.error_info,
                                            BSC_REASON_BADARG,
                                            BSC_INFO_TYPE,
                                            BSC_VALUE_ARG3));
                        }
                        a.cbz(ARG1, resolve_label(error, disp1MB));
                    }
                }
            }
            break;
        case am_string: {
            ArgBytePtr string_ptr(
                    ArgVal(ArgVal::BytePtr, seg.src.as<ArgWord>().get()));

            comment("insert string");
            ASSERT(seg.effectiveSize >= 0);
            mov_imm(ARG3, seg.effectiveSize / 8);
            mov_arg(ARG2, string_ptr);
            load_erl_bits_state(ARG1);

            emit_enter_runtime(Live.get());
            runtime_call<3>(erts_new_bs_put_string);
            emit_leave_runtime(Live.get());
            break;
        }
        case am_utf8: {
            emit_construct_utf8(seg.src, bit_offset, is_byte_aligned);
            break;
        }
        case am_utf16:
            comment("construct utf16 segment");
            mov_arg(ARG2, seg.src);
            a.mov(ARG3, seg.flags);
            load_erl_bits_state(ARG1);

            emit_enter_runtime(Live.get());
            runtime_call<3>(erts_bs_put_utf16);
            emit_leave_runtime(Live.get());

            if (Fail.get() == 0) {
                mov_arg(ARG3, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG3));
            }
            a.cbz(ARG1, resolve_label(error, disp1MB));
            break;
        case am_utf32:
            mov_arg(ARG2, seg.src);
            mov_imm(ARG3, 4 * 8);
            a.mov(ARG4, seg.flags);
            load_erl_bits_state(ARG1);

            emit_enter_runtime(Live.get());
            runtime_call<4>(erts_new_bs_put_integer);
            emit_leave_runtime(Live.get());

            if (Fail.get() == 0) {
                mov_arg(ARG3, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG3));
            }
            a.cbz(ARG1, resolve_label(error, disp1MB));
            break;
        default:
            ASSERT(0);
            break;
        }

        /* Try to keep track of the bit offset. */
        if (bit_offset >= 0 && (seg.action == BscSegment::action::DIRECT ||
                                seg.action == BscSegment::action::STORE)) {
            if (seg.effectiveSize >= 0) {
                bit_offset += seg.effectiveSize;
            } else {
                bit_offset = -1;
            }
        }

        /* Try to keep track whether the next segment is byte
         * aligned. */
        if (seg.type == am_append || seg.type == am_private_append) {
            if (!exact_type<BeamTypeId::Bitstring>(seg.src) ||
                std::gcd(getSizeUnit(seg.src), 8) != 8) {
                is_byte_aligned = false;
            }
        } else if (bit_offset % 8 == 0) {
            is_byte_aligned = true;
        } else if (seg.effectiveSize >= 0) {
            if (seg.effectiveSize % 8 != 0) {
                is_byte_aligned = false;
            }
        } else if (std::gcd(seg.unit, 8) != 8) {
            is_byte_aligned = false;
        }
    }

    comment("done");
    mov_arg(Dst, TMP_MEM1q);
}

/*
 * Here follows the bs_match instruction and friends.
 */

struct BsmSegment {
    BsmSegment()
            : action(action::TEST_HEAP), live(ArgNil()), size(0), unit(1),
              flags(0), dst(ArgXRegister(0)){};

    enum class action {
        TEST_HEAP,
        ENSURE_AT_LEAST,
        ENSURE_EXACTLY,
        READ,
        EXTRACT_BITSTRING,
        EXTRACT_INTEGER,
        GET_INTEGER,
        GET_BITSTRING,
        SKIP,
        DROP,
        GET_TAIL,
        EQ
    } action;
    ArgVal live;
    Uint size;
    Uint unit;
    Uint flags;
    ArgRegister dst;
};

void BeamModuleAssembler::emit_read_bits(Uint bits,
                                         const a64::Gp bin_base,
                                         const a64::Gp bin_offset,
                                         const a64::Gp bitdata) {
    Label handle_partial = a.newLabel();
    Label rev64 = a.newLabel();
    Label shift = a.newLabel();
    Label read_done = a.newLabel();

    bool need_rev64 = false;

    const a64::Gp bin_byte_ptr = TMP2;
    const a64::Gp bit_offset = TMP4;
    const a64::Gp tmp = TMP5;

    auto num_partial = bits % 8;

    ASSERT(1 <= bits && bits <= 64);

    a.add(bin_byte_ptr, bin_base, bin_offset, arm::lsr(3));

    if (bits <= 8) {
        a.ands(bit_offset, bin_offset, imm(7));

        if (num_partial == 0) {
            /* Byte-sized segment. If bit_offset is not byte-aligned,
             * this segment always spans two bytes. */
            a.b_ne(handle_partial);
        } else if (num_partial > 1) {
            /* The segment is smaller than one byte but more than one
             * bit. Test whether it fits within the current byte. */
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_gt(handle_partial);
        }

        /* The segment fits in the current byte. */
        a.ldrb(bitdata.w(), arm::Mem(bin_byte_ptr));
        if (num_partial == 0) {
            a.rev64(bitdata, bitdata);
            a.b(read_done);
        } else if (num_partial > 1) {
            a.b(rev64);
        }

        /* The segment is unaligned and spans two bytes. */
        a.bind(handle_partial);
        if (num_partial != 1) {
            a.ldrh(bitdata.w(), arm::Mem(bin_byte_ptr));
        }
        need_rev64 = true;
    } else if (bits <= 16) {
        a.ands(bit_offset, bin_offset, imm(7));

        /* We always need to read at least two bytes. */
        a.ldrh(bitdata.w(), arm::Mem(bin_byte_ptr));
        a.rev64(bitdata, bitdata);
        a.b_eq(read_done); /* Done if segment is byte-aligned. */

        /* The segment is unaligned. If its size is 9, it always fits
         * in two bytes and we fall through to the shift instruction. */
        a.bind(handle_partial);
        if (num_partial > 1) {
            /* If segment size is less than 15 bits or less, it is
             * possible that it fits into two bytes. */
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_le(shift);
        }

        if (num_partial != 1) {
            /* The segment spans three bytes. Read an additional byte and
             * shift into place (right below the already read two bytes a
             * the top of the word). */
            a.ldrb(tmp.w(), arm::Mem(bin_byte_ptr, 2));
            a.orr(bitdata, bitdata, tmp, arm::lsl(40));
        }
    } else if (bits <= 24) {
        a.ands(bit_offset, bin_offset, imm(7));

        if (num_partial == 0) {
            /* Byte-sized segment. If bit_offset is not byte-aligned,
             * this segment always spans four bytes. */
            a.b_ne(handle_partial);
        } else if (num_partial > 1) {
            /* The segment is smaller than three bytes. Test whether
             * it spans three or four bytes. */
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_gt(handle_partial);
        }

        /* This segment spans three bytes. */
        a.ldrh(bitdata.w(), arm::Mem(bin_byte_ptr));
        a.ldrb(tmp.w(), arm::Mem(bin_byte_ptr, 2));
        a.orr(bitdata, bitdata, tmp, arm::lsl(16));
        if (num_partial == 0) {
            a.rev64(bitdata, bitdata);
            a.b(read_done);
        } else if (num_partial > 1) {
            a.b(rev64);
        }

        /* This segment spans four bytes. */
        a.bind(handle_partial);
        if (num_partial != 1) {
            a.ldr(bitdata.w(), arm::Mem(bin_byte_ptr));
        }
        need_rev64 = true;
    } else if (bits <= 32) {
        a.ands(bit_offset, bin_offset, imm(7));

        /* We always need to read at least four bytes. */
        a.ldr(bitdata.w(), arm::Mem(bin_byte_ptr));
        a.rev64(bitdata, bitdata);
        a.b_eq(read_done);

        a.bind(handle_partial);
        if (num_partial > 0) {
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_le(shift);
        }

        if (num_partial != 1) {
            /* The segment spans five bytes. Read an additional byte and
             * shift into place. */
            a.ldrb(tmp.w(), arm::Mem(bin_byte_ptr, 4));
            a.orr(bitdata, bitdata, tmp, arm::lsl(24));
        }
    } else if (bits <= 40) {
        a.ands(bit_offset, bin_offset, imm(7));

        /* We always need to read four bytes. */
        a.ldr(bitdata.w(), arm::Mem(bin_byte_ptr));
        a.rev64(bitdata, bitdata);

        if (num_partial == 0) {
            /* Byte-sized segment. If bit_offset is not byte-aligned,
             * this segment always spans six bytes. */
            a.b_ne(handle_partial);
        } else if (num_partial > 1) {
            /* The segment is smaller than five bytes. Test whether it
             * spans five or six bytes. */
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_gt(handle_partial);
        }

        /* This segment spans five bytes. Read an additional byte. */
        a.ldrb(tmp.w(), arm::Mem(bin_byte_ptr, 4));
        a.orr(bitdata, bitdata, tmp, arm::lsl(24));
        if (num_partial == 0) {
            a.b(read_done);
        } else if (num_partial > 1) {
            a.b(shift);
        }

        a.bind(handle_partial);
        if (num_partial != 1) {
            /* This segment spans six bytes. Read two additional bytes. */
            a.ldrh(tmp.w(), arm::Mem(bin_byte_ptr, 4));
            a.rev16(tmp.w(), tmp.w());
            a.orr(bitdata, bitdata, tmp, arm::lsl(16));
        }
    } else if (bits <= 48) {
        a.ands(bit_offset, bin_offset, imm(7));
        a.ldr(bitdata.w(), arm::Mem(bin_byte_ptr));
        a.ldrh(tmp.w(), arm::Mem(bin_byte_ptr, 4));
        a.orr(bitdata, bitdata, tmp, arm::lsl(32));
        a.rev64(bitdata, bitdata);
        a.b_eq(read_done);

        a.bind(handle_partial);
        if (num_partial > 1) {
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_le(shift);
        }

        if (num_partial != 1) {
            a.ldrb(tmp.w(), arm::Mem(bin_byte_ptr, 6));
            a.orr(bitdata, bitdata, tmp, arm::lsl(8));
        }
    } else if (bits <= 56) {
        a.ands(bit_offset, bin_offset, imm(7));

        if (num_partial == 0) {
            /* Byte-sized segment. If bit_offset is not byte-aligned,
             * this segment always spans 8 bytes. */
            a.b_ne(handle_partial);
        } else if (num_partial > 1) {
            /* The segment is smaller than 8 bytes. Test whether it
             * spans 7 or 8 bytes. */
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_gt(handle_partial);
        }

        /* This segment spans 7 bytes. */
        a.ldr(bitdata, arm::Mem(bin_byte_ptr, -1));
        a.lsr(bitdata, bitdata, imm(8));
        a.b(rev64);

        /* This segment spans 8 bytes. */
        a.bind(handle_partial);
        if (num_partial != 1) {
            a.ldr(bitdata, arm::Mem(bin_byte_ptr));
        }
        need_rev64 = true;
    } else if (bits <= 64) {
        a.ands(bit_offset, bin_offset, imm(7));
        a.ldr(bitdata, arm::Mem(bin_byte_ptr));
        a.rev64(bitdata, bitdata);

        if (num_partial == 0) {
            /* Byte-sized segment. If it is aligned it spans 8 bytes
             * and we are done. */
            a.b_eq(read_done);
        } else if (num_partial == 1) {
            /* This segment is 57 bits wide. It always spans 8 bytes. */
            a.b(shift);
        } else {
            /* The segment is smaller than 8 bytes. Test whether it
             * spans 8 or 9 bytes. */
            a.cmp(bit_offset, imm(8 - num_partial));
            a.b_le(shift);
        }

        /* This segments spans 9 bytes. Read an additional byte. */
        a.bind(handle_partial);
        if (num_partial != 1) {
            a.ldrb(tmp.w(), arm::Mem(bin_byte_ptr, 8));
            a.lsl(bitdata, bitdata, bit_offset);
            a.lsl(tmp, tmp, bit_offset);
            a.orr(bitdata, bitdata, tmp, arm::lsr(8));
            a.b(read_done);
        }
    }

    a.bind(rev64);
    if (need_rev64) {
        a.rev64(bitdata, bitdata);
    }

    /* Shift the read data into the most significant bits of the
     * word. */
    a.bind(shift);
    a.lsl(bitdata, bitdata, bit_offset);

    a.bind(read_done);
}

void BeamModuleAssembler::emit_extract_integer(const a64::Gp &bitdata,
                                               const a64::Gp &small_tag,
                                               Uint flags,
                                               Uint position,
                                               Uint bits,
                                               const ArgRegister &Dst) {
    a64::Gp data_reg = bitdata;
    auto dst = init_destination(Dst, TMP1);

    if (bits <= 8) {
        /* Endian does not matter for values that fit in a byte. */
        flags &= ~BSF_LITTLE;
    }

    /* Optimize extraction of the first segment after a read. Saves
     * one instruction. */
    if (bits > 0 && bits < SMALL_BITS && position + bits == 64 &&
        (flags & (BSF_LITTLE | BSF_SIGNED)) == 0) {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.orr(dst.reg,
              small_tag,
              data_reg,
              arm::lsr(position - _TAG_IMMED1_SIZE));
        flush_var(dst);
        return;
    }

    Label big = a.newLabel();
    Label done = a.newLabel();
    Uint num_partial = bits % 8;
    Uint num_complete = 8 * (bits / 8);

    switch (bits) {
    case 0:
        data_reg = ZERO;
        break;
    case 64:
        data_reg = bitdata;
        break;
    default:
        data_reg = TMP2;
        switch (flags & (BSF_SIGNED | BSF_LITTLE)) {
        case BSF_SIGNED: /* Signed and big-endian */
            a.sbfx(TMP2, bitdata, position, bits);
            break;
        default:
            a.ubfx(TMP2, bitdata, position, bits);
            break;
        }
    }

    /* If this segment is little-endian, reverse endianness. */
    if ((flags & BSF_LITTLE) != 0) {
        comment("reverse endian for a little-endian segment");
        if (bits == 16) {
            a.rev16(TMP2, data_reg);
        } else if (bits == 32) {
            a.rev32(TMP2, data_reg);
        } else if (num_partial == 0) {
            a.rev64(TMP2, data_reg);
            a.lsr(TMP2, TMP2, arm::lsr(64 - bits));
        } else {
            a.ubfiz(TMP3, data_reg, imm(num_complete), imm(num_partial));
            a.ubfx(TMP2, data_reg, imm(num_partial), imm(num_complete));
            a.rev64(TMP2, TMP2);
            a.orr(TMP2, TMP3, TMP2, arm::lsr(64 - num_complete));
        }
        data_reg = TMP2;
    }

    /* Sign-extend the number if the segment is signed and little-endian. */
    if ((flags & (BSF_SIGNED | BSF_LITTLE)) == (BSF_SIGNED | BSF_LITTLE)) {
        if (0 < bits && bits < 64) {
            comment("sign extend extracted value");
            a.sbfx(TMP2, data_reg, 0, bits);
            data_reg = TMP2;
        }
    }

    /* Handle segments whose values might not fit in a small integer. */
    if (bits >= SMALL_BITS) {
        comment("test whether it fits in a small");
        if ((flags & BSF_SIGNED) != 0) {
            /* Signed segment. */
            a.adds(TMP3, ZERO, data_reg, arm::lsr(SMALL_BITS - 1));
            a.ccmp(TMP3,
                   imm(_TAG_IMMED1_MASK << 1 | 1),
                   imm(NZCV::kEqual),
                   imm(arm::CondCode::kNE));
            a.b_ne(big);
        } else {
            /* Unsigned segment. */
            a.lsr(TMP3, data_reg, imm(SMALL_BITS - 1));
            a.cbnz(TMP3, big);
        }
    }

    /* Tag and store the extracted small integer. */
    a.orr(dst.reg, small_tag, data_reg, arm::lsl(_TAG_IMMED1_SIZE));

    if (bits >= SMALL_BITS) {
        a.b(done);
    }

    /* Handle a bignum (up to 64 bits). */
    a.bind(big);
    if (bits >= SMALL_BITS) {
        comment("store extracted integer as a bignum");
        a.add(dst.reg, HTOP, imm(TAG_PRIMARY_BOXED));
        mov_imm(TMP3, make_pos_bignum_header(1));
        if ((flags & BSF_SIGNED) == 0) {
            /* Unsigned. */
            a.stp(TMP3, data_reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        } else {
            /* Signed. */
            Label store = a.newLabel();
            a.adds(TMP2, data_reg, ZERO);
            a.b_pl(store);

            mov_imm(TMP3, make_neg_bignum_header(1));
            a.neg(TMP2, TMP2);

            a.bind(store);
            a.stp(TMP3, TMP2, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        }
    }

    a.bind(done);
    flush_var(dst);
}

void BeamModuleAssembler::emit_extract_bitstring(const a64::Gp bitdata,
                                                 Uint position,
                                                 Uint bits,
                                                 const ArgRegister &Dst) {
    auto dst = init_destination(Dst, TMP1);

    switch (position) {
    case 0:
        mov_imm(TMP4, 0);
        break;
    case 64:
        a.mov(TMP4, bitdata);
        break;
    default:
        a.ror(TMP4, bitdata, imm(position));
        break;
    }
    a.add(dst.reg, HTOP, imm(TAG_PRIMARY_BOXED));
    mov_imm(TMP2, header_heap_bits(bits));
    mov_imm(TMP3, bits);
    a.stp(TMP2, TMP3, arm::Mem(HTOP).post(sizeof(Eterm[2])));
    if (bits > 0) {
        a.rev64(TMP4, TMP4);
        a.str(TMP4, arm::Mem(HTOP).post(sizeof(Eterm[1])));
    }

    flush_var(dst);
}

static std::vector<BsmSegment> opt_bsm_segments(
        const std::vector<BsmSegment> segments,
        const ArgWord &Need,
        const ArgWord &Live) {
    std::vector<BsmSegment> segs;

    Uint heap_need = Need.get();

    /*
     * First calculate the total number of heap words needed for
     * bignums and binaries.
     */
    for (auto seg : segments) {
        switch (seg.action) {
        case BsmSegment::action::GET_INTEGER:
            if (seg.size >= SMALL_BITS) {
                heap_need += BIG_NEED_FOR_BITS(seg.size);
            }
            break;
        case BsmSegment::action::GET_BITSTRING:
            heap_need += erts_extracted_bitstring_size(seg.size);
            break;
        case BsmSegment::action::GET_TAIL:
            heap_need += BUILD_SUB_BITSTRING_HEAP_NEED;
            break;
        default:
            break;
        }
    }

    int index = 0;
    int read_action_pos = -1;

    index = 0;
    for (auto seg : segments) {
        if (heap_need != 0 && seg.live.isWord()) {
            BsmSegment s = seg;

            read_action_pos = -1;
            s.action = BsmSegment::action::TEST_HEAP;
            s.size = heap_need;
            segs.push_back(s);
            index++;
            heap_need = 0;
        }

        switch (seg.action) {
        case BsmSegment::action::GET_INTEGER:
        case BsmSegment::action::GET_BITSTRING:
            if (seg.size > 64) {
                read_action_pos = -1;
            } else {
                if ((seg.flags & BSF_LITTLE) != 0 || read_action_pos < 0 ||
                    seg.size + segs.at(read_action_pos).size > 64) {
                    BsmSegment s;

                    /* Create a new READ action. */
                    read_action_pos = index;
                    s.action = BsmSegment::action::READ;
                    s.size = seg.size;
                    segs.push_back(s);
                    index++;
                } else {
                    /* Reuse previous READ action. */
                    segs.at(read_action_pos).size += seg.size;
                }
                switch (seg.action) {
                case BsmSegment::action::GET_INTEGER:
                    seg.action = BsmSegment::action::EXTRACT_INTEGER;
                    break;
                case BsmSegment::action::GET_BITSTRING:
                    seg.action = BsmSegment::action::EXTRACT_BITSTRING;
                    break;
                default:
                    break;
                }
            }
            segs.push_back(seg);
            break;
        case BsmSegment::action::EQ: {
            if (read_action_pos < 0 ||
                seg.size + segs.at(read_action_pos).size > 64) {
                BsmSegment s;

                /* Create a new READ action. */
                read_action_pos = index;
                s.action = BsmSegment::action::READ;
                s.size = seg.size;
                segs.push_back(s);
                index++;
            } else {
                /* Reuse previous READ action. */
                segs.at(read_action_pos).size += seg.size;
            }
            auto &prev = segs.back();
            if (prev.action == BsmSegment::action::EQ &&
                prev.size + seg.size <= 64) {
                /* Coalesce with the previous EQ instruction. */
                prev.size += seg.size;
                prev.unit = prev.unit << seg.size | seg.unit;
                index--;
            } else {
                segs.push_back(seg);
            }
            break;
        }
        case BsmSegment::action::SKIP:
            if (read_action_pos >= 0 &&
                seg.size + segs.at(read_action_pos).size <= 64) {
                segs.at(read_action_pos).size += seg.size;
                seg.action = BsmSegment::action::DROP;
            } else {
                read_action_pos = -1;
            }
            segs.push_back(seg);
            break;
        default:
            read_action_pos = -1;
            segs.push_back(seg);
            break;
        }
        index++;
    }

    /* Handle a trailing test_heap instruction (for the
     * i_bs_match_test_heap instruction). */
    if (heap_need) {
        BsmSegment seg;

        seg.action = BsmSegment::action::TEST_HEAP;
        seg.size = heap_need;
        seg.live = Live;
        segs.push_back(seg);
    }
    return segs;
}

UWord BeamModuleAssembler::bs_get_flags(const ArgVal &val) {
    if (val.isNil()) {
        return 0;
    } else if (val.isLiteral()) {
        Eterm term = beamfile_get_literal(beam, val.as<ArgLiteral>().get());
        UWord flags = 0;

        while (is_list(term)) {
            Eterm *consp = list_val(term);
            Eterm elem = CAR(consp);
            switch (elem) {
            case am_little:
            case am_native:
                flags |= BSF_LITTLE;
                break;
            case am_signed:
                flags |= BSF_SIGNED;
                break;
            }
            term = CDR(consp);
        }
        ASSERT(is_nil(term));
        return flags;
    } else if (val.isWord()) {
        /* Originates from bs_get_integer2 instruction. */
        return val.as<ArgWord>().get();
    } else {
        ASSERT(0); /* Should not happen. */
        return 0;
    }
}

void BeamModuleAssembler::emit_i_bs_match(ArgLabel const &Fail,
                                          ArgRegister const &Ctx,
                                          Span<ArgVal> const &List) {
    emit_i_bs_match_test_heap(Fail, Ctx, ArgWord(0), ArgWord(0), List);
}

void BeamModuleAssembler::emit_i_bs_match_test_heap(ArgLabel const &Fail,
                                                    ArgRegister const &Ctx,
                                                    ArgWord const &Need,
                                                    ArgWord const &Live,
                                                    Span<ArgVal> const &List) {
    const int orig_offset = offsetof(ErlSubBits, orig);
    const int base_offset = offsetof(ErlSubBits, base_flags);
    const int start_offset = offsetof(ErlSubBits, start);
    const int end_offset = offsetof(ErlSubBits, end);

    std::vector<BsmSegment> segments;

    auto current = List.begin();
    auto end = List.begin() + List.size();

    while (current < end) {
        auto cmd = current++->as<ArgImmed>().get();
        BsmSegment seg;

        switch (cmd) {
        case am_ensure_at_least: {
            seg.action = BsmSegment::action::ENSURE_AT_LEAST;
            seg.size = current[0].as<ArgWord>().get();
            seg.unit = current[1].as<ArgWord>().get();
            current += 2;
            break;
        }
        case am_ensure_exactly: {
            seg.action = BsmSegment::action::ENSURE_EXACTLY;
            seg.size = current[0].as<ArgWord>().get();
            current += 1;
            break;
        }
        case am_binary:
        case am_integer: {
            auto size = current[2].as<ArgWord>().get();
            auto unit = current[3].as<ArgWord>().get();

            switch (cmd) {
            case am_integer:
                seg.action = BsmSegment::action::GET_INTEGER;
                break;
            case am_binary:
                seg.action = BsmSegment::action::GET_BITSTRING;
                break;
            }

            seg.live = current[0];
            seg.size = size * unit;
            seg.unit = unit;
            seg.flags = bs_get_flags(current[1]);
            seg.dst = current[4].as<ArgRegister>();
            current += 5;
            break;
        }
        case am_get_tail: {
            seg.action = BsmSegment::action::GET_TAIL;
            seg.live = current[0].as<ArgWord>();
            seg.dst = current[2].as<ArgRegister>();
            current += 3;
            break;
        }
        case am_skip: {
            seg.action = BsmSegment::action::SKIP;
            seg.size = current[0].as<ArgWord>().get();
            seg.flags = 0;
            current += 1;
            break;
        }
        case am_Eq: {
            seg.action = BsmSegment::action::EQ;
            seg.live = current[0];
            seg.size = current[1].as<ArgWord>().get();
            seg.unit = current[2].as<ArgWord>().get();
            current += 3;
            break;
        }
        default:
            abort();
            break;
        }
        segments.push_back(seg);
    }

    segments = opt_bsm_segments(segments, Need, Live);

    const a64::Gp bin_base = ARG2;
    const a64::Gp bin_position = ARG3;
    const a64::Gp bin_size = ARG4;
    const a64::Gp small_tag = ARG5;
    const a64::Gp bitdata = ARG8;
    bool position_is_valid = false;
    bool small_tag_valid = false;
    Uint offset_in_bitdata = 0;

    for (auto seg : segments) {
        switch (seg.action) {
        case BsmSegment::action::ENSURE_AT_LEAST: {
            comment("ensure_at_least %ld %ld", seg.size, seg.unit);
            auto ctx_reg = load_source(Ctx, TMP1);
            auto stride = seg.size;
            auto unit = seg.unit;

            a.ldur(bin_position, emit_boxed_val(ctx_reg.reg, start_offset));
            a.ldur(bin_size, emit_boxed_val(ctx_reg.reg, end_offset));
            a.sub(TMP5, bin_size, bin_position);
            if (stride != 0) {
                cmp(TMP5, stride);
                a.b_lo(resolve_beam_label(Fail, disp1MB));
            }

            if (unit != 1) {
                if (stride % unit != 0) {
                    sub(TMP5, TMP5, stride);
                }

                if ((unit & (unit - 1)) != 0) {
                    mov_imm(TMP4, unit);

                    a.udiv(TMP3, TMP5, TMP4);
                    a.msub(TMP5, TMP3, TMP4, TMP5);

                    a.cbnz(TMP5, resolve_beam_label(Fail, disp1MB));
                } else {
                    a.tst(TMP5, imm(unit - 1));
                    a.b_ne(resolve_beam_label(Fail, disp1MB));
                }
            }

            position_is_valid = true;
            break;
        }
        case BsmSegment::action::ENSURE_EXACTLY: {
            comment("ensure_exactly %ld", seg.size);
            auto ctx_reg = load_source(Ctx, TMP1);
            auto size = seg.size;

            a.ldur(bin_position, emit_boxed_val(ctx_reg.reg, start_offset));
            a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, end_offset));
            if (size != 0) {
                a.sub(TMP1, TMP3, bin_position);
                cmp(TMP1, size);
            } else {
                a.subs(TMP1, TMP3, bin_position);
            }
            a.b_ne(resolve_beam_label(Fail, disp1MB));
            position_is_valid = true;
            break;
        }
        case BsmSegment::action::EQ: {
            a64::Gp cmp_reg = TMP1;
            comment("=:= %ld %ld", seg.size, seg.unit);

            offset_in_bitdata -= seg.size;

            if (seg.size == 0) {
                cmp_reg = ZERO;
            } else if (seg.size == 64) {
                cmp_reg = bitdata;
            } else {
                a.ubfx(cmp_reg, bitdata, offset_in_bitdata, seg.size);
            }

            if (seg.size == 32) {
                cmp(cmp_reg.w(), seg.unit);
            } else {
                cmp(cmp_reg, seg.unit);
            }
            a.b_ne(resolve_beam_label(Fail, disp1MB));
            break;
        }
        case BsmSegment::action::TEST_HEAP: {
            comment("test_heap %ld", seg.size);
            emit_gc_test(ArgWord(0), ArgWord(seg.size), seg.live);
            position_is_valid = false;
            small_tag_valid = false;
            break;
        }
        case BsmSegment::action::READ: {
            comment("read %ld", seg.size);
            offset_in_bitdata = 64;
            if (seg.size == 0) {
                comment("(nothing to do)");
            } else {
                auto ctx = load_source(Ctx, ARG1);

                if (!position_is_valid) {
                    a.ldur(bin_position, emit_boxed_val(ctx.reg, start_offset));
                    position_is_valid = true;
                }

                a.ldur(bin_base, emit_boxed_val(ctx.reg, base_offset));
                a.and_(bin_base, bin_base, imm(~ERL_SUB_BITS_FLAG_MASK));

                emit_read_bits(seg.size, bin_base, bin_position, bitdata);

                a.add(bin_position, bin_position, imm(seg.size));
                a.stur(bin_position, emit_boxed_val(ctx.reg, start_offset));
            }
            break;
        }
        case BsmSegment::action::EXTRACT_BITSTRING: {
            auto bits = seg.size;
            auto Dst = seg.dst;

            comment("extract binary %ld", bits);
            emit_extract_bitstring(bitdata, offset_in_bitdata, bits, Dst);
            offset_in_bitdata -= bits;
            break;
        }
        case BsmSegment::action::EXTRACT_INTEGER: {
            auto bits = seg.size;
            auto flags = seg.flags;
            auto Dst = seg.dst;

            comment("extract integer %ld", bits);
            if (!small_tag_valid) {
                small_tag_valid = true;
                mov_imm(small_tag, _TAG_IMMED1_SMALL);
            }
            offset_in_bitdata -= bits;
            emit_extract_integer(bitdata,
                                 small_tag,
                                 flags,
                                 offset_in_bitdata,
                                 bits,
                                 Dst);
            break;
        }
        case BsmSegment::action::GET_INTEGER: {
            Uint live = seg.live.as<ArgWord>().get();
            Uint flags = seg.flags;
            auto bits = seg.size;
            auto Dst = seg.dst;

            comment("get integer %ld", bits);
            auto ctx = load_source(Ctx, TMP1);

            a.mov(ARG1, c_p);
            a.mov(ARG2, bits);
            a.mov(ARG3, flags);
            emit_untag_ptr(ARG4, ctx.reg);

            if (bits >= SMALL_BITS) {
                emit_enter_runtime<Update::eHeapOnlyAlloc>(live);
            } else {
                emit_enter_runtime(live);
            }

            runtime_call<4>(erts_bs_get_integer_2);

            if (bits >= SMALL_BITS) {
                emit_leave_runtime<Update::eHeapOnlyAlloc>(live);
            } else {
                emit_leave_runtime(live);
            }

            mov_arg(Dst, ARG1);

            position_is_valid = false;
            small_tag_valid = false;
            break;
        }
        case BsmSegment::action::GET_BITSTRING: {
            auto Live = seg.live;
            ERTS_ASSERT(seg.size > 64);
            comment("get binary %ld", seg.size);
            auto ctx = load_source(Ctx, TMP1);

            if (position_is_valid) {
                a.mov(ARG5, bin_position);
            } else {
                a.ldur(ARG5, emit_boxed_val(ctx.reg, start_offset));
            }
            lea(ARG1, arm::Mem(c_p, offsetof(Process, htop)));
            if (seg.size <= ERL_ONHEAP_BITS_LIMIT) {
                comment("skipped setting registers not used for heap binary");
            } else {
                a.ldur(ARG2, emit_boxed_val(ctx.reg, orig_offset));
                a.and_(ARG3, ARG2, imm(~TAG_PTR_MASK__));
                a.and_(ARG2, ARG2, imm(TAG_PTR_MASK__));
            }
            a.ldur(ARG4, emit_boxed_val(ctx.reg, base_offset));
            a.and_(ARG4, ARG4, imm(~ERL_SUB_BITS_FLAG_MASK));
            mov_imm(ARG6, seg.size);
            a.add(TMP2, ARG5, ARG6);
            a.stur(TMP2, emit_boxed_val(ctx.reg, start_offset));

            emit_enter_runtime<Update::eHeapOnlyAlloc>(
                    Live.as<ArgWord>().get());

            runtime_call<6>(erts_build_sub_bitstring);

            emit_leave_runtime<Update::eHeapOnlyAlloc>(
                    Live.as<ArgWord>().get());

            mov_arg(seg.dst, ARG1);
            position_is_valid = false;
            small_tag_valid = false;
            break;
        }
        case BsmSegment::action::GET_TAIL: {
            comment("get_tail");

            mov_arg(ARG1, Ctx);
            fragment_call(ga->get_bs_get_tail_shared());
            mov_arg(seg.dst, ARG1);
            position_is_valid = false;
            small_tag_valid = false;
            break;
        }
        case BsmSegment::action::SKIP: {
            comment("skip %ld", seg.size);
            auto ctx = load_source(Ctx, TMP1);
            if (!position_is_valid) {
                a.ldur(bin_position, emit_boxed_val(ctx.reg, start_offset));
                position_is_valid = true;
            }
            add(bin_position, bin_position, seg.size);
            a.stur(bin_position, emit_boxed_val(ctx.reg, start_offset));
            break;
        }
        case BsmSegment::action::DROP:
            auto bits = seg.size;
            comment("drop %ld", bits);
            offset_in_bitdata -= bits;
            break;
        }
    }
}
