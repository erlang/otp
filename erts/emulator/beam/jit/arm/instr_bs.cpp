/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2022. All Rights Reserved.
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
#include "erl_binary.h"
#include "erl_bits.h"
#include "beam_common.h"
}

/* Clobbers TMP1+TMP2
 *
 * If max_size > 0, we jump to the fail label when Size > max_size
 *
 * Returns -1 when the field check always fails, 1 if it may fail, and 0 if it
 * never fails. */
int BeamModuleAssembler::emit_bs_get_field_size(const ArgSource &Size,
                                                int unit,
                                                Label fail,
                                                const arm::Gp &out) {
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

        /* Negating the tag bits lets us guard against non-smalls, negative
         * numbers, and overflow with a single `tst` instruction. */
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        ASSERT(unit <= 1024);

        a.eor(out, size_reg.reg, imm(_TAG_IMMED1_SMALL));
        a.tst(out, imm(0xFFF0000000000000UL | _TAG_IMMED1_MASK));

        if (unit == 0) {
            /* Silly but legal.*/
            mov_imm(out, 0);
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

        a.b_ne(fail);

        return 1;
    }
}

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgWord &Size,
                                              const ArgWord &Heap,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    mov_arg(ARG4, Size);
    mov_arg(ARG5, Heap);
    mov_arg(ARG6, Live);

    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>(Live.get());

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
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

        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                           Update::eReductions>(Live.get());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init);

        emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
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

    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>(Live.get());

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init_bits);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
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

        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                           Update::eReductions>(Live.get());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init_bits);

        emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
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
    Label is_binary = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, Src);

    if (Fail.get() != 0) {
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, ARG2);
    } else {
        /* bs_start_match3 may not throw, and the compiler will only emit {f,0}
         * when it knows that the source is a match state or binary, so we're
         * free to skip the binary tests. */
    }

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, ARG2);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr, 0));

    a.and_(TMP1, TMP1, imm(_HEADER_SUBTAG_MASK));
    a.cmp(TMP1, imm(BIN_MATCHSTATE_SUBTAG));
    a.b_eq(next);

    if (Fail.get() != 0) {
        comment("is_binary_header");
        a.cmp(TMP1, _TAG_HEADER_SUB_BIN);
        a.b_eq(is_binary);
        ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
        a.and_(TMP1, TMP1, imm(~4));
        a.cmp(TMP1, imm(_TAG_HEADER_REFC_BIN));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(is_binary);
    {
        emit_gc_test_preserve(ArgWord(ERL_BIN_MATCHSTATE_SIZE(0)),
                              Live,
                              Src,
                              ARG2);

        emit_enter_runtime<Update::eStack | Update::eHeap>(Live.get());

        a.mov(ARG1, c_p);
        /* ARG2 was set above */
        runtime_call<2>(erts_bs_start_match_3);

        emit_leave_runtime<Update::eStack | Update::eHeap>(Live.get());

        a.add(ARG2, ARG1, imm(TAG_PRIMARY_BOXED));
    }

    a.bind(next);
    mov_arg(Dst, ARG2);
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgRegister &Ctx,
                                                 const ArgLabel &Fail,
                                                 const ArgWord &Bits,
                                                 const ArgBytePtr &Ptr) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    const int size_offset = offsetof(ErlBinMatchState, mb.size);
    const int base_offset = offsetof(ErlBinMatchState, mb.base);

    const auto size = Bits.get();

    {
        auto ctx_reg = load_source(Ctx, TMP1);

        a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
        a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, size_offset));
        add(TMP4, TMP2, size);
        a.cmp(TMP4, TMP3);
        a.b_hi(resolve_beam_label(Fail, disp1MB));

        /* ARG4 = mb->offset & 7 */
        a.and_(ARG4, TMP2, imm(7));

        /* ARG3 = mb->base + (mb->offset >> 3) */
        a.lsr(TMP2, TMP2, imm(3));
        a.ldur(TMP1, emit_boxed_val(ctx_reg.reg, base_offset));
        a.add(ARG3, TMP1, TMP2);
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

        a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
        add(TMP2, TMP2, size);
        a.stur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
    }
}

void BeamModuleAssembler::emit_i_bs_get_position(const ArgRegister &Ctx,
                                                 const ArgRegister &Dst) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    auto ctx_reg = load_source(Ctx, TMP1);
    auto dst_reg = init_destination(Dst, TMP2);

    /* Match contexts can never be literals, so we can skip clearing literal
     * tags. */
    a.ldur(dst_reg.reg, emit_boxed_val(ctx_reg.reg, position_offset));
    a.lsl(dst_reg.reg, dst_reg.reg, imm(_TAG_IMMED1_SIZE));
    a.orr(dst_reg.reg, dst_reg.reg, imm(_TAG_IMMED1_SMALL));

    flush_var(dst_reg);
}

void BeamModuleAssembler::emit_i_bs_get_fixed_integer(const ArgRegister &Ctx,
                                                      const ArgLabel &Fail,
                                                      const ArgWord &Live,
                                                      const ArgWord &Flags,
                                                      const ArgWord &Bits,
                                                      const ArgRegister &Dst) {
    auto ctx = load_source(Ctx, TMP1);
    int flags, bits;

    flags = Flags.get();
    bits = Bits.get();

    if (bits >= SMALL_BITS) {
        emit_gc_test_preserve(ArgWord(BIG_NEED_FOR_BITS(bits)),
                              Live,
                              Ctx,
                              ctx.reg);
    }

    lea(ARG4, emit_boxed_val(ctx.reg, offsetof(ErlBinMatchState, mb)));

    if (bits >= SMALL_BITS) {
        emit_enter_runtime<Update::eHeap>(Live.get());
    } else {
        emit_enter_runtime(Live.get());
    }

    a.mov(ARG1, c_p);
    a.mov(ARG2, bits);
    a.mov(ARG3, flags);
    /* ARG4 set above. */
    runtime_call<4>(erts_bs_get_integer_2);

    if (bits >= SMALL_BITS) {
        emit_leave_runtime<Update::eHeap>(Live.get());
    } else {
        emit_leave_runtime(Live.get());
    }

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_get_integer(const ArgRegister &Ctx,
                                                const ArgLabel &Fail,
                                                const ArgWord &Live,
                                                const ArgWord &FlagsAndUnit,
                                                const ArgSource &Sz,
                                                const ArgRegister &Dst) {
    Label fail;
    int unit;

    fail = resolve_beam_label(Fail, dispUnknown);
    unit = FlagsAndUnit.get() >> 3;

    if (emit_bs_get_field_size(Sz, unit, fail, ARG5) >= 0) {
        mov_arg(ARG3, Ctx);
        mov_arg(ARG4, FlagsAndUnit);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                           Update::eReductions>(Live.get());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<6>(beam_jit_bs_get_integer);

        emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                           Update::eReductions>(Live.get());

        emit_branch_if_not_value(ARG1, fail);
        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgLabel &Fail,
                                             const ArgRegister &Ctx,
                                             const ArgWord &Offset) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    const int size_offset = offsetof(ErlBinMatchState, mb.size);

    auto ctx_reg = load_source(Ctx, TMP1);

    ASSERT(Offset.isWord());

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, size_offset));
    a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, position_offset));
    a.sub(TMP2, TMP2, TMP3);

    if (Offset.get() != 0) {
        a.cmp(TMP2, imm(Offset.get()));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    } else {
        a.cbnz(TMP2, resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_bs_set_position(const ArgRegister &Ctx,
                                               const ArgRegister &Pos) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    auto [ctx, pos] = load_sources(Ctx, TMP1, Pos, TMP2);

    a.lsr(TMP2, pos.reg, imm(_TAG_IMMED1_SIZE));
    a.stur(TMP2, emit_boxed_val(ctx.reg, position_offset));
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgRegister &Ctx,
                                                    const ArgLabel &Fail,
                                                    const ArgWord &Live,
                                                    const ArgWord &Unit,
                                                    const ArgRegister &Dst) {
    unsigned unit = Unit.get();

    mov_arg(ARG1, Ctx);

    emit_gc_test_preserve(ArgWord(EXTRACT_SUB_BIN_HEAP_NEED), Live, Ctx, ARG1);

    /* Make field fetching slightly more compact by pre-loading the match
     * buffer into the right argument slot for `erts_bs_get_binary_all_2`. */
    lea(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));

    ERTS_CT_ASSERT_FIELD_PAIR(ErlBinMatchBuffer, offset, size);
    a.ldp(TMP2, TMP3, arm::Mem(ARG2, offsetof(ErlBinMatchBuffer, offset)));

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

    emit_enter_runtime<Update::eHeap>(Live.get());

    a.mov(ARG1, c_p);
    /* ARG2 was set above. */
    runtime_call<2>(erts_bs_get_binary_all_2);

    emit_leave_runtime<Update::eHeap>(Live.get());

    mov_arg(Dst, ARG1);
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    lea(TMP1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));

    ERTS_CT_ASSERT_FIELD_PAIR(ErlBinMatchBuffer, orig, base);
    a.ldp(ARG2, ARG3, arm::Mem(TMP1, offsetof(ErlBinMatchBuffer, orig)));

    ERTS_CT_ASSERT_FIELD_PAIR(ErlBinMatchBuffer, offset, size);
    a.ldp(ARG4, TMP1, arm::Mem(TMP1, offsetof(ErlBinMatchBuffer, offset)));

    lea(ARG1, arm::Mem(c_p, offsetof(Process, htop)));

    /* Extracted size = mb->size - mb->offset */
    a.sub(ARG5, TMP1, ARG4);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeap>();

    runtime_call<5>(erts_extract_sub_binary);

    emit_leave_runtime<Update::eHeap>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgRegister &Ctx,
                                           const ArgRegister &Dst,
                                           const ArgWord &Live) {
    mov_arg(ARG1, Ctx);

    emit_gc_test_preserve(ArgWord(EXTRACT_SUB_BIN_HEAP_NEED), Live, Ctx, ARG1);

    fragment_call(ga->get_bs_get_tail_shared());

    mov_arg(Dst, ARG1);
}

/* Bits to skip are passed in ARG1 */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgLabel &Fail,
                                            const ArgRegister &Ctx) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    const int size_offset = offsetof(ErlBinMatchState, mb.size);

    auto ctx_reg = load_source(Ctx, TMP1);

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
    a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, size_offset));

    a.add(TMP2, TMP2, ARG1);
    a.cmp(TMP2, TMP3);
    a.b_hi(resolve_beam_label(Fail, disp1MB));

    a.stur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgRegister &Ctx,
                                               const ArgRegister &Bits,
                                               const ArgLabel &Fail,
                                               const ArgWord &Unit) {
    Label fail = resolve_beam_label(Fail, dispUnknown);

    if (emit_bs_get_field_size(Bits, Unit.get(), fail, ARG1) >= 0) {
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

        emit_gc_test_preserve(ArgWord(EXTRACT_SUB_BIN_HEAP_NEED),
                              Live,
                              Ctx,
                              ARG4);

        lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));

        emit_enter_runtime<Update::eHeap>(Live.get());

        a.mov(ARG1, c_p);
        a.ldr(ARG2, TMP_MEM1q);
        mov_imm(ARG3, Flags.get());
        runtime_call<4>(erts_bs_get_binary_2);

        emit_leave_runtime<Update::eHeap>(Live.get());

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
        lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));

        emit_enter_runtime<Update::eHeap>(Live.get());

        a.mov(ARG1, c_p);
        mov_imm(ARG3, Flags.get());
        runtime_call<4>(erts_bs_get_float_2);

        emit_leave_runtime<Update::eHeap>(Live.get());

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

void BeamModuleAssembler::emit_bs_get_utf8(const ArgRegister &Ctx,
                                           const ArgLabel &Fail) {
    mov_arg(ARG1, Ctx);
    lea(ARG1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));

    emit_enter_runtime();

    runtime_call<1>(erts_bs_get_utf8);

    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
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

    lea(ARG1, emit_boxed_val(ctx_reg.reg, offsetof(ErlBinMatchState, mb)));

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
                                                arm::Gp value) {
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
        const int position_offset = offsetof(ErlBinMatchState, mb.offset);
        auto ctx_reg = load_source(Ms, TMP2);

        a.ldur(TMP1, emit_boxed_val(ctx_reg.reg, position_offset));
        a.sub(TMP1, TMP1, imm(32));
        a.stur(TMP1, emit_boxed_val(ctx_reg.reg, position_offset));

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

    a.ldur(TMP2,
           emit_boxed_val(ctx_reg.reg, offsetof(ErlBinMatchState, mb.size)));
    a.ldur(TMP3,
           emit_boxed_val(ctx_reg.reg, offsetof(ErlBinMatchState, mb.offset)));

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

    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>(Live.get() + 1);

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<6>(erts_bs_append);

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
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
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>(0);

    runtime_call<2>(erts_bs_init_writable);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>(0);

    a.mov(XREG0, ARG1);
}

void BeamGlobalAssembler::emit_bs_create_bin_error_shared() {
    a.mov(XREG0, a64::x30);

    emit_enter_runtime<Update::eStack | Update::eHeap>(0);

    /* ARG3 is already set by the caller */
    a.mov(ARG2, ARG4);
    a.mov(ARG4, ARG1);
    a.mov(ARG1, c_p);
    runtime_call<4>(beam_jit_bs_construct_fail_info);

    emit_leave_runtime<Update::eStack | Update::eHeap>(0);

    a.mov(ARG4, ZERO);
    a.mov(ARG2, XREG0);
    a.b(labels[raise_exception_shared]);
}

/*
 * ARG1 = term
 *
 * If the term in ARG1 is a binary on entry, on return
 * ARG1 will contain the size of the binary in bits and
 * sign flag will be cleared.
 *
 * If the term is not a binary, the sign flag will be set.
 */
void BeamGlobalAssembler::emit_bs_bit_size_shared() {
    Label not_sub_bin = a.newLabel();
    Label fail = a.newLabel();

    arm::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);

    emit_is_boxed(fail, boxed_ptr);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_SUB_BIN));
    a.b_ne(not_sub_bin);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.ldurb(TMP2.w(), emit_boxed_val(boxed_ptr, offsetof(ErlSubBin, bitsize)));

    a.adds(ARG1, TMP2, TMP1, arm::lsl(3));
    a.ret(a64::x30);

    a.bind(not_sub_bin);
    ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
    a.and_(TMP1, TMP1, imm(~4));
    a.cmp(TMP1, imm(_TAG_HEADER_REFC_BIN));
    a.b_ne(fail);

    a.ldur(TMP1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.lsl(ARG1, TMP1, imm(3));
    a.tst(ARG1, ARG1);

    a.ret(a64::x30);

    a.bind(fail);
    mov_imm(ARG1, -1);
    a.tst(ARG1, ARG1);

    a.ret(a64::x30);
}

struct BscSegment {
    BscSegment()
            : type(am_false), unit(1), flags(0), src(ArgNil()), size(ArgNil()),
              error_info(0), effectiveSize(-1) {
    }

    Eterm type;
    Uint unit;
    Uint flags;
    ArgVal src;
    ArgVal size;

    Uint error_info;
    Sint effectiveSize;
};

void BeamModuleAssembler::emit_i_bs_create_bin(const ArgLabel &Fail,
                                               const ArgWord &Alloc,
                                               const ArgWord &Live0,
                                               const ArgRegister &Dst,
                                               const Span<ArgVal> &args) {
    Uint num_bits = 0;
    std::size_t n = args.size();
    std::vector<BscSegment> segments;
    Label error;
    ArgWord Live = Live0;
    arm::Gp sizeReg;

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
            bsc_op = BSC_OP_BINARY;
            break;
        }

        /*
         * Save segment number and operation for use in extended
         * error information.
         */
        seg.error_info = beam_jit_set_bsc_segment_op(bsc_segment, bsc_op);

        /*
         * Attempt to calculate the effective size of this segment.
         * Give up is variable or invalid.
         */
        if (seg.size.isSmall() && seg.unit != 0) {
            Uint unsigned_size = seg.size.as<ArgSmall>().getUnsigned();

            if ((unsigned_size >> (sizeof(Eterm) - 1) * 8) == 0) {
                /* This multiplication cannot overflow. */
                Uint seg_size = seg.unit * unsigned_size;
                seg.effectiveSize = seg_size;
                num_bits += seg_size;
            }
        }

        if (seg.effectiveSize < 0 && seg.type != am_append &&
            seg.type != am_private_append) {
            /* At least one segment will need a dynamic size
             * calculation. */
            sizeReg = ARG8;
        }

        segments.insert(segments.end(), seg);
    }

    if (Fail.get() != 0) {
        error = resolve_beam_label(Fail, dispUnknown);
    } else {
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
            comment("size of an entire binary");
            mov_arg(ARG1, seg.src);
            a.mov(ARG3, ARG1);
            fragment_call(ga->get_bs_bit_size_shared());
            if (exact_type(seg.src, BEAM_TYPE_BITSTRING)) {
                comment("skipped check for success since the source "
                        "is always a bit string");
            } else {
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG3));
                }
                a.b_mi(resolve_label(error, disp1MB));
            }
            a.add(sizeReg, sizeReg, ARG1);
        } else if (seg.unit != 0) {
            bool can_fail = true;
            comment("size binary/integer/float/string");

            if (always_small(seg.size)) {
                auto [min, _] = getIntRange(seg.size);
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
            } else if (always_one_of(seg.size,
                                     BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER)) {
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
            a.asr(TMP1, ARG3, imm(_TAG_IMMED1_SIZE));
            if (seg.unit == 1) {
                a.add(sizeReg, sizeReg, TMP1);
            } else {
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(
                                    seg.error_info,
                                    BSC_REASON_SYSTEM_LIMIT,
                                    BSC_INFO_SIZE,
                                    BSC_VALUE_ARG3));
                }
                a.tst(TMP1, imm(0xffful << 52));
                a.b_ne(resolve_label(error, disp1MB));
                mov_imm(TMP2, seg.unit);
                a.madd(sizeReg, TMP1, TMP2, sizeReg);
            }
        } else {
            switch (seg.type) {
            case am_utf8: {
                comment("size utf8");
                Label next = a.newLabel();
                auto src_reg = load_source(seg.src, TMP1);

                a.lsr(TMP1, src_reg.reg, imm(_TAG_IMMED1_SIZE));
                mov_imm(TMP2, 1 * 8);
                a.cmp(TMP1, imm(0x7F));
                a.b_ls(next);

                mov_imm(TMP2, 2 * 8);
                a.cmp(TMP1, imm(0x7FFUL));
                a.b_ls(next);

                a.cmp(TMP1, imm(0x10000UL));
                mov_imm(TMP2, 3 * 8);
                mov_imm(TMP3, 4 * 8);
                a.csel(TMP2, TMP2, TMP3, arm::CondCode::kLO);

                a.bind(next);
                a.add(sizeReg, sizeReg, TMP2);
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

        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                           Update::eReductions>(Live.get() + 1);
        runtime_call<6>(erts_bs_append_checked);
        emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                           Update::eReductions>(Live.get() + 1);

        if (Fail.get() == 0) {
            mov_arg(ARG3, ArgXRegister(Live.get()));
            mov_imm(ARG4,
                    beam_jit_update_bsc_reason_info(seg.error_info,
                                                    BSC_REASON_BADARG,
                                                    BSC_INFO_FVALUE,
                                                    BSC_VALUE_ARG3));
        }
        emit_branch_if_not_value(ARG1, resolve_label(error, dispUnknown));
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
    } else {
        comment("allocate binary");
        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);
        load_erl_bits_state(ARG3);
        load_x_reg_array(ARG2);
        a.mov(ARG1, c_p);
        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap | Update::eXRegs>(Live.get());
        if (sizeReg.isValid()) {
            comment("(size in bits)");
            a.mov(ARG4, sizeReg);
            runtime_call<6>(beam_jit_bs_init_bits);
        } else if (num_bits % 8 == 0) {
            comment("(size in bytes)");
            mov_imm(ARG4, num_bits / 8);
            runtime_call<6>(beam_jit_bs_init);
        } else {
            mov_imm(ARG4, num_bits);
            runtime_call<6>(beam_jit_bs_init_bits);
        }
        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap | Update::eXRegs>(Live.get());
    }
    a.str(ARG1, TMP_MEM1q);

    /* Build each segment of the binary. */
    for (auto seg : segments) {
        switch (seg.type) {
        case am_append:
        case am_private_append:
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
                if (seg.unit == 1) {
                    comment("skipped test for success because unit =:= 1");
                    can_fail = false;
                }
            } else {
                /* The size is a variable. We have verified that
                 * the value is a non-negative small in the
                 * appropriate range. Multiply the size with the
                 * unit. */
                mov_arg(ARG3, seg.size);
                a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
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
                mov_arg(ARG3, seg.size);
                a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
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
            comment("construct integer segment");
            if (seg.effectiveSize >= 0) {
                mov_imm(ARG3, seg.effectiveSize);
            } else {
                mov_arg(ARG3, seg.size);
                a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(TMP1, seg.unit);
                    a.mul(ARG3, ARG3, TMP1);
                }
            }
            mov_arg(ARG2, seg.src);
            mov_imm(ARG4, seg.flags);
            load_erl_bits_state(ARG1);

            emit_enter_runtime(Live.get());
            runtime_call<4>(erts_new_bs_put_integer);
            emit_leave_runtime(Live.get());

            if (exact_type(seg.src, BEAM_TYPE_INTEGER)) {
                comment("skipped test for success because construction can't "
                        "fail");
            } else {
                if (Fail.get() == 0) {
                    mov_arg(ARG3, seg.src);
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG3));
                }
                a.cbz(ARG1, resolve_label(error, disp1MB));
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
        case am_utf8:
            comment("construct utf8 segment");
            mov_arg(ARG2, seg.src);
            load_erl_bits_state(ARG1);

            emit_enter_runtime(Live.get());
            runtime_call<2>(erts_bs_put_utf8);

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
        case am_utf16:
            comment("construct utf8 segment");
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
    }

    comment("done");
    mov_arg(Dst, TMP_MEM1q);
}
