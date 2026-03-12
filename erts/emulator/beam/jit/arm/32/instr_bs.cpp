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
                                                const a32::Gp &out) {
    if (Size.isImmed()) {
        if (Size.isSmall()) {
            Sint sval = Size.as<ArgSmall>().getSigned();

            if (sval >= 0 && sval <= (MAX_SMALL / unit)) {
                mov_imm(out, sval * unit);
                return 0;
            }
        }

        a.b(fail);
        return -1;
    } else {
        bool can_fail = true;
        auto size_reg = load_source(Size, out);

        if (always_small(Size)) {
            auto [min, max] = getClampedRange(Size);
            can_fail =
                    !(0 <= min && (max >> (SMALL_BITS - ERL_UNIT_BITS)) == 0);
        }

        if (!can_fail) {
            comment("simplified segment size checks because "
                    "the types are known");
        }

        if (unit == 1 && !can_fail) {
            a.lsr(out, size_reg.reg, imm(_TAG_IMMED1_SIZE));
        } else {
            if (can_fail) {
                a.and_(TMP, size_reg.reg, imm(_TAG_IMMED1_MASK));
                a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
                a.b_ne(fail);
            }

            if (unit == 0) {
                mov_imm(out, 0);
            } else if (Support::isPowerOf2(unit)) {
                int trailing_bits = Support::ctz<Eterm>(unit);

                a.bic(out, size_reg.reg, imm(_TAG_IMMED1_MASK));
                if (can_fail) {
                    a.cmp(out, imm(0));
                    a.b_lt(fail);
                }

                if (trailing_bits < _TAG_IMMED1_SIZE) {
                    a.lsr(out, out, imm(_TAG_IMMED1_SIZE - trailing_bits));
                } else if (trailing_bits > _TAG_IMMED1_SIZE) {
                    a.lsl(out, out, imm(trailing_bits - _TAG_IMMED1_SIZE));
                }
            } else {
                a.bic(out, size_reg.reg, imm(_TAG_IMMED1_MASK));
                if (can_fail) {
                    a.cmp(out, imm(0));
                    a.b_lt(fail);
                }

                mov_imm(TMP, unit);
                a.mul(out, out, TMP);
                a.lsr(out, out, imm(_TAG_IMMED1_SIZE));
            }
        }

        return can_fail;
    }
}

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgWord &Size,
                                              const ArgWord &Heap,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init_heap");
}

/* Set the error reason when a size check has failed. */
void BeamGlobalAssembler::emit_bs_size_check_shared() {
    // TODO
    emit_nyi("emit_bs_size_check_shared");
}

void BeamModuleAssembler::emit_i_bs_init_fail_heap(const ArgSource &Size,
                                                   const ArgWord &Heap,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init_fail_heap");
}

void BeamModuleAssembler::emit_i_bs_init(const ArgWord &Size,
                                         const ArgWord &Live,
                                         const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init");
}

void BeamModuleAssembler::emit_i_bs_init_fail(const ArgRegister &Size,
                                              const ArgLabel &Fail,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init_fail");
}

void BeamModuleAssembler::emit_i_bs_init_bits(const ArgWord &NumBits,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init_bits");
}

void BeamModuleAssembler::emit_i_bs_init_bits_heap(const ArgWord &NumBits,
                                                   const ArgWord &Alloc,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init_bits_heap");
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail(const ArgRegister &NumBits,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init_bits_fail");
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail_heap(
        const ArgSource &NumBits,
        const ArgWord &Alloc,
        const ArgLabel &Fail,
        const ArgWord &Live,
        const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_init_bits_fail_heap");
}

void BeamModuleAssembler::emit_bs_put_string(const ArgWord &Size,
                                             const ArgBytePtr &Ptr) {
    // TODO
    emit_nyi("emit_bs_put_string");
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(const ArgSource &Src,
                                                        const ArgLabel &Fail,
                                                        const ArgWord &Sz,
                                                        const ArgWord &Flags) {
    // TODO
    emit_nyi("emit_i_new_bs_put_integer_imm");
}

void BeamModuleAssembler::emit_i_new_bs_put_integer(const ArgLabel &Fail,
                                                    const ArgRegister &Sz,
                                                    const ArgWord &Flags,
                                                    const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_new_bs_put_integer");
}

void BeamModuleAssembler::emit_i_new_bs_put_binary(const ArgLabel &Fail,
                                                   const ArgSource &Sz,
                                                   const ArgWord &Flags,
                                                   const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_new_bs_put_binary");
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(const ArgSource &Src,
                                                       const ArgLabel &Fail,
                                                       const ArgWord &Unit) {
    // TODO
    emit_nyi("emit_i_new_bs_put_binary_all");
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(const ArgLabel &Fail,
                                                       const ArgWord &Sz,
                                                       const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_new_bs_put_binary_imm");
}

void BeamModuleAssembler::emit_i_new_bs_put_float(const ArgLabel &Fail,
                                                  const ArgRegister &Sz,
                                                  const ArgWord &Flags,
                                                  const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_new_bs_put_float");
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(const ArgLabel &Fail,
                                                      const ArgWord &Sz,
                                                      const ArgWord &Flags,
                                                      const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_new_bs_put_float_imm");
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
        /* bs_start_match3 may not throw, and the compiler will only emit
         * {f,0} when it knows that the source is a match state or binary. */
    }

    a32::Gp boxed_ptr = emit_ptr_val(ARG1, ARG2);

    /* Speculatively test for an existing match context.
     * ARM64 merges primary tag bits from base_flags into thing_word before
     * comparing against HEADER_SUB_BITS|MATCH_CONTEXT; do the same here. */
    ERTS_CT_ASSERT(offsetof(ErlSubBits, thing_word) == 0 &&
                   offsetof(ErlSubBits, base_flags) == sizeof(Eterm));
    a.ldr(TMP, emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, thing_word)));
    a.ldr(VAR, emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, base_flags)));
    a.and_(VAR, VAR, imm(ERL_SUB_BITS_FLAG_MASK));
    a.bic(TMP, TMP, imm(_TAG_PRIMARY_MASK));
    a.orr(TMP, TMP, VAR);
    mov_imm(VAR, HEADER_SUB_BITS | ERL_SUB_BITS_FLAGS_MATCH_CONTEXT);
    a.cmp(TMP, VAR);
    a.b_eq(next);

    {
        if (Fail.get() != 0) {
            const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
            ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
            ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                           (_TAG_HEADER_HEAP_BITS & mask));

            a.and_(TMP, TMP, imm(mask));
            mov_imm(VAR, _TAG_HEADER_HEAP_BITS);
            a.cmp(TMP, VAR);
            a.b_ne(resolve_beam_label(Fail, disp32MB));
        }

        emit_gc_test_preserve(ArgWord(ERL_SUB_BITS_SIZE), Live, Src, ARG2);

        /* erts_bs_start_match_3 allocates a fresh match context as needed. */
        emit_enter_runtime<Update::eHeapOnlyAlloc>();

        a.mov(ARG1, c_p);
        /* ARG2 was set above. */
        runtime_call<2>(erts_bs_start_match_3);

        emit_leave_runtime<Update::eHeapOnlyAlloc>();

        add(ARG2, ARG1, TAG_PRIMARY_BOXED);
    }

    a.bind(next);
    mov_arg(Dst, ARG2);
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgRegister &Ctx,
                                                 const ArgLabel &Fail,
                                                 const ArgWord &Bits,
                                                 const ArgBytePtr &Ptr) {
    const UWord size = Bits.get();
    Label fail = resolve_beam_label(Fail, disp32MB);

    mov_arg(ARG1, Ctx);

    a.ldr(ARG2, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));
    add(ARG3, ARG2, size);
    a.ldr(TMP, emit_boxed_val(ARG1, offsetof(ErlSubBits, end)));
    a.cmp(ARG3, TMP);
    a.b_hi(fail);

    a.str(ARG1, TMP_MEM1q);

    /* ARG4 = sb->start & 7 */
    a.and_(ARG4, ARG2, imm(7));

    /* ARG3 = (sb->base_flags & ~mask) + (sb->start >> 3) */
    a.ldr(TMP, emit_boxed_val(ARG1, offsetof(ErlSubBits, base_flags)));
    mov_imm(VAR, ~ERL_SUB_BITS_FLAG_MASK);
    a.and_(TMP, TMP, VAR);
    a.add(ARG3, TMP, ARG2, arm::lsr(3));

    emit_enter_runtime();

    mov_arg(ARG1, Ptr);
    mov_imm(ARG2, 0);
    a.sub(a32::sp, a32::sp, imm(8)); /* keep AAPCS alignment */
    mov_imm(TMP, size);
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
    runtime_call<5>(erts_cmp_bits);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime();

    a.tst(ARG1, ARG1);
    a.b_ne(fail);

    a.ldr(ARG1, TMP_MEM1q);
    a.ldr(TMP, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));
    add(TMP, TMP, size);
    a.str(TMP, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));
}

void BeamModuleAssembler::emit_bs_get_position(const ArgRegister &Ctx,
                                               const ArgRegister &Dst,
                                               const ArgWord &Live) {
    const int start_offset = offsetof(ErlSubBits, start);
    (void)Live;

    mov_arg(ARG1, Ctx);

    /* Match contexts can never be literals, so we can skip clearing literal
     * tags. */
    a.ldr(ARG1, emit_boxed_val(ARG1, start_offset));
    a.lsl(ARG1, ARG1, imm(_TAG_IMMED1_SIZE));
    a.orr(ARG1, ARG1, imm(_TAG_IMMED1_SMALL));

    mov_arg(Dst, ARG1);
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
        Label fail = resolve_beam_label(Fail, disp32MB);
        int unit = Unit.get();

        if (emit_bs_get_field_size(Sz, unit, fail, VAR) >= 0) {
            a.str(VAR, TMP_MEM1q); /* size */

            /* If there cannot possibly be a GC in the code that follows, we
             * can avoid loading registers that will never be used. */
            auto max = std::get<1>(getClampedRange(Sz));
            bool potential_gc =
                    max >= SMALL_BITS || (max * Unit.get()) >= SMALL_BITS;

            mov_arg(ARG3, Ctx);
            mov_imm(ARG4, flags);

            if (potential_gc) {
                emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();
            } else {
                comment("simplified entering runtime because result is always "
                        "small");
                emit_enter_runtime();
            }

            a.mov(ARG1, c_p);
            if (potential_gc) {
                load_x_reg_array(ARG2);
            } else {
#ifdef DEBUG
                mov_imm(ARG2, 0);
#endif
            }

            a.sub(a32::sp, a32::sp, imm(8)); /* arg5/arg6 */
            a.ldr(TMP, TMP_MEM1q);
            a.str(TMP, arm::Mem(a32::sp, 0)); /* size */
            mov_imm(TMP, Live.get());
            a.str(TMP, arm::Mem(a32::sp, 4)); /* live */
            runtime_call<6>(beam_jit_bs_get_integer);
            a.add(a32::sp, a32::sp, imm(8));

            if (potential_gc) {
                emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();
            } else {
                emit_leave_runtime();
            }

            emit_branch_if_not_value(ARG1, fail);
            if (potential_gc) {
                /* Test for max heap size exceeded. */
                emit_is_not_cons(
                        resolve_fragment(ga->get_do_schedule(), disp32MB),
                        ARG1);
            }

            mov_arg(Dst, ARG1);
        }
    }
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgLabel &Fail,
                                             const ArgRegister &Ctx,
                                             const ArgWord &Offset) {
    // TODO
    emit_nyi("emit_bs_test_tail2");
}

void BeamModuleAssembler::emit_bs_set_position(const ArgRegister &Ctx,
                                               const ArgRegister &Pos) {
    const int start_offset = offsetof(ErlSubBits, start);

    mov_arg(ARG1, Ctx);
    mov_arg(ARG2, Pos);

    a.lsr(TMP, ARG2, imm(_TAG_IMMED1_SIZE));
    a.str(TMP, emit_boxed_val(ARG1, start_offset));
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgRegister &Ctx,
                                                    const ArgLabel &Fail,
                                                    const ArgWord &Live,
                                                    const ArgWord &Unit,
                                                    const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_get_binary_all2");
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeapOnlyAlloc>();

    a.ldr(ARG2, emit_boxed_val(ARG1, offsetof(ErlSubBits, orig)));
    a.mov(ARG3, ARG2);

    /* ARG2 = tag bits of sb->orig, ARG3 = sb->orig without tag bits. */
    mov_imm(TMP, TAG_PTR_MASK__);
    a.and_(ARG2, ARG2, TMP);
    mov_imm(TMP, ~TAG_PTR_MASK__);
    a.and_(ARG3, ARG3, TMP);

    a.ldr(ARG4, emit_boxed_val(ARG1, offsetof(ErlSubBits, base_flags)));
    mov_imm(TMP, ~ERL_SUB_BITS_FLAG_MASK);
    a.and_(ARG4, ARG4, TMP);

    /* Extracted size = end - start. */
    a.ldr(TMP, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));
    a.ldr(VAR, emit_boxed_val(ARG1, offsetof(ErlSubBits, end)));
    a.sub(VAR, VAR, TMP);

    lea(ARG1, arm::Mem(c_p, offsetof(Process, htop)));

    a.sub(a32::sp, a32::sp, imm(8)); /* keep AAPCS alignment */
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5: offset */
    a.str(VAR, arm::Mem(a32::sp, 4)); /* arg6: size */
    runtime_call<6>(erts_build_sub_bitstring);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime<Update::eHeapOnlyAlloc>();
    emit_leave_runtime_frame();

    a.bx(a32::lr);
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
    // TODO
    emit_nyi("emit_bs_skip_bits");
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgRegister &Ctx,
                                               const ArgRegister &Size,
                                               const ArgLabel &Fail,
                                               const ArgWord &Unit) {
    // TODO
    emit_nyi("emit_i_bs_skip_bits2");
}

void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(const ArgLabel &Fail,
                                                   const ArgRegister &Ctx,
                                                   const ArgWord &Bits) {
    // TODO
    emit_nyi("emit_i_bs_skip_bits_imm2");
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgRegister &Ctx,
                                                const ArgLabel &Fail,
                                                const ArgWord &Live,
                                                const ArgSource &Size,
                                                const ArgWord &Flags,
                                                const ArgRegister &Dst) {
    Label fail = resolve_beam_label(Fail, disp32MB);
    const int unit = Flags.get() >> 3;

    if (emit_bs_get_field_size(Size, unit, fail, ARG2) >= 0) {
        a.str(ARG2, TMP_MEM1q);

        mov_arg(ARG4, Ctx);

        emit_gc_test_preserve(ArgWord(BUILD_SUB_BITSTRING_HEAP_NEED),
                              Live,
                              Ctx,
                              ARG4);

        emit_untag_ptr(ARG4, ARG4);

        emit_enter_runtime<Update::eHeapOnlyAlloc>();

        a.mov(ARG1, c_p);
        a.ldr(ARG2, TMP_MEM1q);
        mov_imm(ARG3, Flags.get());
        runtime_call<4>(erts_bs_get_binary_2);

        emit_leave_runtime<Update::eHeapOnlyAlloc>();

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
    Label fail = resolve_beam_label(Fail, disp32MB);
    const Sint unit = Flags.get() >> 3;

    emit_gc_test_preserve(ArgWord(FLOAT_SIZE_OBJECT), Live, Ctx, ARG4);

    if (emit_bs_get_field_size(Sz, unit, fail, ARG2) >= 0) {
        emit_enter_runtime<Update::eHeapOnlyAlloc>();

        a.mov(ARG1, c_p);
        mov_imm(ARG3, Flags.get());
        mov_arg(ARG4, Ctx);
        emit_untag_ptr(ARG4, ARG4);
        runtime_call<4>(erts_bs_get_float_2);

        emit_leave_runtime<Update::eHeapOnlyAlloc>();

        emit_branch_if_not_value(ARG1, fail);
        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_bs_utf8_size(const ArgSource &Src,
                                              const ArgXRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_utf8_size");
}

void BeamModuleAssembler::emit_i_bs_put_utf8(const ArgLabel &Fail,
                                             const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_bs_put_utf8");
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
    // TODO
    emit_nyi("emit_bs_get_utf8_short_shared");
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
    // TODO
    emit_nyi("emit_bs_get_utf8_shared");
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgRegister &Ctx,
                                           const ArgLabel &Fail) {
    Label fail = resolve_beam_label(Fail, disp32MB);

    mov_arg(ARG1, Ctx);
    emit_untag_ptr(ARG1, ARG1);

    emit_enter_runtime();
    runtime_call<1>(erts_bs_get_utf8);
    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, fail);
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
    // TODO
    emit_nyi("emit_i_bs_utf16_size");
}

void BeamModuleAssembler::emit_i_bs_put_utf16(const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_bs_put_utf16");
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgRegister &Ctx,
                                            const ArgLabel &Fail,
                                            const ArgWord &Flags) {
    auto ctx_reg = load_source(Ctx, VAR);

    emit_untag_ptr(ARG1, ctx_reg.reg);

    emit_enter_runtime();

    mov_imm(ARG2, Flags.get());
    runtime_call<2>(erts_bs_get_utf16);

    emit_leave_runtime();

    emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, disp32MB));
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
                                                a32::Gp value) {
    ASSERT(value.id() != TMP.id());

    a.and_(TMP, value, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
    a.b_ne(fail);

    mov_imm(TMP, make_small(0xD800UL));
    a.cmp(value, TMP);
    a.b_lo(next);

    mov_imm(TMP, make_small(0xDFFFUL));
    a.cmp(value, TMP);
    a.b_ls(fail);

    mov_imm(TMP, make_small(0x10FFFFUL));
    a.cmp(value, TMP);
    a.b_hi(fail);

    a.b(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgLabel &Fail,
                                                     const ArgSource &Src) {
    auto src_reg = load_source(Src, VAR);
    Label fail, next = a.newLabel();

    if (Fail.get() != 0) {
        fail = resolve_beam_label(Fail, disp32MB);
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
    auto src_reg = load_source(Src, VAR);

    emit_validate_unicode(next, fail, src_reg.reg);

    a.bind(fail);
    {
        const int start_offset = offsetof(ErlSubBits, start);
        auto ctx_reg = load_source(Ms, TMP);

        a.ldr(VAR, emit_boxed_val(ctx_reg.reg, start_offset));
        sub(VAR, VAR, 32);
        a.str(VAR, emit_boxed_val(ctx_reg.reg, start_offset));

        if (Fail.get() != 0) {
            a.b(resolve_beam_label(Fail, disp32MB));
        } else {
            emit_error(BADARG);
        }
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgLabel &Fail,
                                            const ArgRegister &Ctx,
                                            const ArgWord &Unit) {
    // TODO
    emit_nyi("emit_bs_test_unit");
}

/* ARG2 = current `Size`,
 * ARG3 = elements to `Add`,
 * ARG4 = element `Unit`
 *
 * Error is indicated through cond_ne() */
void BeamGlobalAssembler::emit_bs_add_guard_shared() {
    // TODO
    emit_nyi("emit_bs_add_guard_shared");
}

/* ARG2 = current `Size`,
 * ARG3 = elements to `Add`,
 * ARG4 = element `Unit` */
void BeamGlobalAssembler::emit_bs_add_body_shared() {
    // TODO
    emit_nyi("emit_bs_add_body_shared");
}

void BeamModuleAssembler::emit_bs_add(const ArgLabel &Fail,
                                      const ArgSource &Size,
                                      const ArgSource &Add,
                                      const ArgWord &Unit,
                                      const ArgXRegister &Dst) {
    // TODO
    emit_nyi("emit_bs_add");
}

void BeamModuleAssembler::emit_i_bs_append(const ArgLabel &Fail,
                                           const ArgWord &ExtraHeap,
                                           const ArgWord &Live,
                                           const ArgWord &Unit,
                                           const ArgSource &Size,
                                           const ArgSource &Bin,
                                           const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_append");
}

void BeamModuleAssembler::emit_i_bs_private_append(const ArgLabel &Fail,
                                                   const ArgWord &Unit,
                                                   const ArgSource &Size,
                                                   const ArgRegister &Src,
                                                   const ArgXRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_private_append");
}

void BeamModuleAssembler::emit_bs_init_writable() {
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);

    /* Implicit liveness is 0, so no X-reg stashing needed. */
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();
    runtime_call<2>(erts_bs_init_writable);
    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    mov_arg(ArgXRegister(0), ARG1);
}

void BeamGlobalAssembler::emit_bs_create_bin_error_shared() {
    a.str(a32::lr, TMP_MEM5q);

    emit_enter_runtime<Update::eHeapAlloc>();

    /* ARG3 is already set by the caller */
    a.mov(ARG2, ARG4);
    a.mov(ARG4, ARG1);
    a.mov(ARG1, c_p);
    runtime_call<4>(beam_jit_bs_construct_fail_info);

    emit_leave_runtime<Update::eHeapAlloc>();

    mov_imm(ARG4, 0);
    a.ldr(ARG2, TMP_MEM5q);
    a.b(labels[raise_exception_shared]);
}

/*
 * ARG1 = tagged bignum term
 */
void BeamGlobalAssembler::emit_get_sint64_shared() {
    Label success = a.newLabel();
    Label fail = a.newLabel();

    emit_is_boxed(fail, ARG1);
    emit_ptr_val(TMP, ARG1);
    a.ldr(ARG2, emit_boxed_val(TMP));
    a.ldr(ARG3, emit_boxed_val(TMP, sizeof(Eterm)));
    mov_imm(TMP, _TAG_HEADER_MASK);
    a.and_(ARG2, ARG2, TMP);
    mov_imm(TMP, POS_BIG_SUBTAG);
    a.cmp(ARG2, TMP);
    a.b_eq(success);

    mov_imm(TMP, NEG_BIG_SUBTAG);
    a.cmp(ARG2, TMP);
    a.b_ne(fail);

    a.rsb(ARG3, ARG3, 0);

    a.bind(success);
    a.mov(ARG1, ARG3);
    a.tst(ARG2, ARG2); /* Clear Z for success. */
    a.bx(a32::lr);

    a.bind(fail);
    mov_imm(ARG2, 0);
    a.tst(ARG2, ARG2); /* Set Z for failure. */
    a.bx(a32::lr);
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
                segs.push_back(seg);
                continue;
            }

            if (seg.flags & BSF_LITTLE || segs.empty() ||
                segs.back().action == BscSegment::action::DIRECT) {
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            auto prev = segs.back();
            if (prev.flags & BSF_LITTLE) {
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            if (prev.effectiveSize + seg.effectiveSize <= 64) {
                segs.pop_back();
                prev.effectiveSize += seg.effectiveSize;
                seg.action = BscSegment::action::ACCUMULATE;
                segs.push_back(seg);
                segs.push_back(prev);
            } else {
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
void BeamModuleAssembler::update_bin_state(a32::Gp bin_offset,
                                           Sint bit_offset,
                                           Sint size,
                                           a32::Gp size_reg) {
    int cur_bin_offset = offsetof(ErtsSchedulerRegisters,
                                  aux_regs.d.erl_bits_state.erts_current_bin_);
    arm::Mem mem_bin_base = arm::Mem(scheduler_registers, cur_bin_offset);
    arm::Mem mem_bin_offset =
            arm::Mem(scheduler_registers, cur_bin_offset + sizeof(Eterm));

    if (bit_offset % 8 != 0) {
        a.ldr(VAR, mem_bin_base);
        a.ldr(bin_offset, mem_bin_offset);

        if (size_reg.isValid()) {
            a.add(TMP, bin_offset, size_reg);
        } else {
            add(TMP, bin_offset, size);
        }
        a.str(TMP, mem_bin_offset);

        a.lsr(TMP, bin_offset, imm(3));
        a.add(VAR, VAR, TMP);
    } else {
        comment("optimized updating of binary construction state");
        ASSERT(size >= 0 || size_reg.isValid());
        ASSERT(bit_offset % 8 == 0);

        a.ldr(VAR, mem_bin_base);
        if (size_reg.isValid()) {
            if (bit_offset == 0) {
                a.str(size_reg, mem_bin_offset);
            } else {
                add(TMP, size_reg, bit_offset);
                a.str(TMP, mem_bin_offset);
            }
        } else {
            mov_imm(TMP, bit_offset + size);
            a.str(TMP, mem_bin_offset);
        }
        if (bit_offset != 0) {
            add(VAR, VAR, bit_offset >> 3);
        }
    }

    a.str(VAR, TMP_MEM4q);
}

/*
 * The size of the segment is assumed to be in ARG3.
 */
void BeamModuleAssembler::set_zero(Sint effectiveSize) {
    Label loop_words = a.newLabel();
    Label after_words = a.newLabel();
    Label byte_loop = a.newLabel();
    Label done = a.newLabel();

    update_bin_state(ARG2, -1, -1, ARG3);
    a.ldr(VAR, TMP_MEM4q);
    mov_imm(TMP, 0);

    if (effectiveSize < 0 || effectiveSize > 128) {
        a.tst(ARG3, ARG3);
        a.b_eq(done);

        a.bind(loop_words);
        mov_imm(ARG2, 32);
        a.cmp(ARG3, ARG2);
        a.b_lt(after_words);
        a.str(TMP, arm::Mem(VAR).post(4));
        a.sub(ARG3, ARG3, imm(32));
        a.b(loop_words);
    } else {
        Sint rem = effectiveSize;
        while (rem >= 32) {
            a.str(TMP, arm::Mem(VAR).post(4));
            rem -= 32;
        }
        if (rem >= 16) {
            a.strh(TMP, arm::Mem(VAR).post(2));
            rem -= 16;
        }
        if (rem >= 8) {
            a.strb(TMP, arm::Mem(VAR).post(1));
            rem -= 8;
        }
        if (rem > 0) {
            a.strb(TMP, arm::Mem(VAR));
        }
        a.b(done);
    }

    a.bind(after_words);
    a.tst(ARG3, ARG3);
    a.b_eq(done);

    a.bind(byte_loop);
    a.strb(TMP, arm::Mem(VAR).post(1));
    a.subs(ARG3, ARG3, imm(8));
    a.b_gt(byte_loop);

    a.bind(done);
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

    mov_imm(TMP, 0x800);
    a.cmp(ARG1, TMP);
    a.b_hs(more_than_two_bytes);

    a.and_(VAR, ARG1, imm(0x3f));
    a.lsl(VAR, VAR, imm(8));
    a.lsr(ARG1, ARG1, imm(6));
    a.orr(ARG1, ARG1, VAR);
    mov_imm(TMP, 0x80c0);
    a.orr(ARG1, ARG1, TMP);
    mov_imm(ARG4, 16);
    a.bx(a32::lr);

    a.bind(more_than_two_bytes);
    mov_imm(TMP, 0x10000);
    a.cmp(ARG1, TMP);
    a.b_hs(four_bytes);

    a.and_(VAR, ARG1, imm(0x3f));
    a.lsl(VAR, VAR, imm(16));
    a.add(TMP, ARG1, ARG1, arm::lsl(1));
    a.and_(TMP, TMP, imm(0x3f00));
    a.lsr(ARG1, ARG1, imm(12));
    a.orr(ARG1, ARG1, TMP);
    a.orr(ARG1, ARG1, VAR);
    mov_imm(TMP, 0x8080e0);
    a.orr(ARG1, ARG1, TMP);
    mov_imm(ARG4, 24);
    a.bx(a32::lr);

    a.bind(four_bytes);
    a.and_(VAR, ARG1, imm(0x3f));
    a.lsl(VAR, VAR, imm(24));

    a.mov(TMP, ARG1);
    a.lsl(TMP, TMP, imm(10));
    mov_imm(ARG2, 0x3f0000);
    a.and_(TMP, TMP, ARG2);

    a.mov(ARG2, ARG1);
    a.lsr(ARG2, ARG2, imm(4));
    a.and_(ARG2, ARG2, imm(0x3f00));

    a.lsr(ARG1, ARG1, imm(18));
    a.orr(ARG1, ARG1, VAR);
    a.orr(ARG1, ARG1, TMP);
    a.orr(ARG1, ARG1, ARG2);
    mov_imm(TMP, 0x808080f0);
    a.orr(ARG1, ARG1, TMP);
    mov_imm(ARG4, 32);
    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_construct_utf8(const ArgVal &Src,
                                              Sint bit_offset,
                                              bool is_byte_aligned) {
    Label store = a.newLabel();
    Label next = a.newLabel();

    comment("construct utf8 segment");
    auto src = load_source(Src, ARG1);
    a.asr(ARG1, src.reg, imm(_TAG_IMMED1_SIZE));
    mov_imm(ARG4, 8);
    mov_imm(TMP, 0x80);
    a.cmp(ARG1, TMP);
    a.b_lo(store);

    fragment_call(ga->get_construct_utf8_shared());

    a.bind(store);
    update_bin_state(ARG3, bit_offset, -1, ARG4);
    a.ldr(VAR, TMP_MEM4q);

    if (!is_byte_aligned) {
        Label aligned = a.newLabel();

        a.and_(TMP, ARG3, imm(7));
        a.b_eq(aligned);

        a.mov(ARG2, ARG1);
        a.mov(ARG3, TMP);
        fragment_call(ga->get_store_unaligned());
        a.b(next);

        a.bind(aligned);
    }

    Label do_store_1 = a.newLabel();
    Label do_store_2 = a.newLabel();
    mov_imm(TMP, 8);
    a.cmp(ARG4, TMP);
    a.b_ne(do_store_1);
    a.strb(ARG1, arm::Mem(VAR));
    a.b(next);

    a.bind(do_store_1);
    mov_imm(TMP, 24);
    a.cmp(ARG4, TMP);
    a.b_hi(do_store_2);
    a.strh(ARG1, arm::Mem(VAR));
    mov_imm(TMP, 16);
    a.cmp(ARG4, TMP);
    a.b_eq(next);
    a.lsr(ARG1, ARG1, imm(16));
    a.strb(ARG1, arm::Mem(VAR, 2));
    a.b(next);

    a.bind(do_store_2);
    a.str(ARG1, arm::Mem(VAR));

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

    a.ldr(ARG1, TMP_MEM4q);
    a.str(ARG1, TMP_MEM4q);
    a.ldrb(VAR, arm::Mem(ARG1));

    a.and_(TMP, ARG2, imm(0xff));
    a.lsr(TMP, TMP, ARG3);

    a.lsl(VAR, VAR, ARG3);
    mov_imm(ARG1, 0xff);
    a.bic(VAR, VAR, ARG1);
    a.ldr(ARG1, TMP_MEM4q);
    a.lsr(VAR, VAR, ARG3);

    a.orr(VAR, VAR, TMP);
    a.strb(VAR, arm::Mem(ARG1).post(1));

    mov_imm(TMP, 8);
    a.sub(TMP, TMP, ARG3);

    a.rev(ARG2, ARG2);
    a.lsl(ARG2, ARG2, TMP);

    a.subs(ARG4, ARG4, TMP);
    a.b_le(done);

    a.bind(loop);
    a.ror(ARG2, ARG2, imm(24));
    a.strb(ARG2, arm::Mem(ARG1).post(1));
    a.subs(ARG4, ARG4, imm(8));
    a.b_gt(loop);

    a.bind(done);
    a.bx(a32::lr);
}

/*
 * In:
 *   ARG4 = Size of binary in bits.
 *   ARG5 = Extra words to allocate.
 *   ARG6 = Number of live X registers.
 *
 * Out:
 *   ARG1 = Allocated binary object.
 */

void BeamGlobalAssembler::emit_bs_init_bits_shared() {
    emit_enter_runtime_frame();

    lea(ARG3,
        getSchedulerRegRef(offsetof(ErtsSchedulerRegisters,
                                    aux_regs.d.erl_bits_state)));
    load_x_reg_array(ARG2);
    a.mov(ARG1, c_p);

    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.sub(a32::sp, a32::sp, imm(8));
    a.ldr(TMP, TMP_MEM2q);
    a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5 */
    a.ldr(TMP, TMP_MEM3q);
    a.str(TMP, arm::Mem(a32::sp, 4)); /* arg6 */
    runtime_call<6>(beam_jit_bs_init_bits);
    a.add(a32::sp, a32::sp, imm(8));

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

    emit_leave_runtime_frame();

    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, state.value)));
    mov_imm(VAR, ERTS_PSFLG_EXITING);
    a.tst(TMP, VAR);
    a.b_ne(labels[do_schedule]);

    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_i_bs_create_bin(const ArgLabel &Fail,
                                               const ArgWord &Alloc,
                                               const ArgWord &Live0,
                                               const ArgRegister &Dst,
                                               const Span<ArgVal> &args) {
    Uint num_bits = 0;
    std::size_t n = args.size();
    std::vector<BscSegment> segments;
    Label error; /* Intentionally uninitialized */
    bool dynamic_size = false;
    ArgWord Live = Live0;

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
        seg.error_info = beam_jit_set_bsc_segment_op(bsc_segment, bsc_op);

        if (seg.size.isSmall() && seg.unit != 0) {
            Uint unsigned_size = seg.size.as<ArgSmall>().getUnsigned();
            if ((unsigned_size >> (sizeof(Eterm) - 1) * 8) == 0) {
                Uint seg_size = seg.unit * unsigned_size;
                seg.effectiveSize = seg_size;
                num_bits += seg_size;
            }
        } else if (seg.type == am_binary && seg.size.isAtom() &&
                   seg.size.as<ArgAtom>().get() == am_all) {
            dynamic_size = true;
        } else if (seg.type == am_append || seg.type == am_private_append) {
            dynamic_size = true;
        } else {
            dynamic_size = true;
        }

        segments.push_back(seg);
    }

    if (Fail.get() != 0) {
        error = resolve_beam_label(Fail, dispUnknown);
    } else {
        Label past_error = a.newLabel();
        a.b(past_error);
        error = a.newLabel();
        a.bind(error);
        fragment_call(ga->get_bs_create_bin_error_shared());
        last_error_offset = a.offset();
        a.bind(past_error);
    }

    if (dynamic_size) {
        mov_imm(TMP, num_bits);
        a.str(TMP, TMP_MEM5q);

        for (auto seg : segments) {
            if (seg.effectiveSize >= 0) {
                continue;
            }

            if (seg.type == am_append || seg.type == am_private_append ||
                (seg.type == am_binary && seg.size.isAtom() &&
                 seg.size.as<ArgAtom>().get() == am_all)) {
                Label not_sub_bits = a.newLabel();

                mov_arg(ARG1, seg.src);

                if (!exact_type<BeamTypeId::Bitstring>(seg.src)) {
                    if (Fail.get() == 0) {
                        mov_imm(ARG4,
                                beam_jit_update_bsc_reason_info(
                                        seg.error_info,
                                        BSC_REASON_BADARG,
                                        BSC_INFO_TYPE,
                                        BSC_VALUE_ARG1));
                    }

                    emit_is_boxed(resolve_label(error, dispUnknown), seg.src, ARG1);
                }

                emit_untag_ptr(TMP, ARG1);

                ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
                a.ldr(ARG2, arm::Mem(TMP));
                a.ldr(ARG3, arm::Mem(TMP, sizeof(Eterm)));

                if (masked_types<BeamTypeId::MaybeBoxed>(seg.src) !=
                    BeamTypeId::Bitstring) {
                    const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
                    ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
                    ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                                   (_TAG_HEADER_HEAP_BITS & mask));

                    a.and_(ARG1, ARG2, imm(mask));
                    mov_imm(VAR, _TAG_HEADER_HEAP_BITS);
                    a.cmp(ARG1, VAR);
                    a.b_ne(resolve_label(error, dispUnknown));
                }

                mov_imm(ARG1, HEADER_SUB_BITS);
                a.cmp(ARG2, ARG1);
                a.b_ne(not_sub_bits);
                ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
                a.ldr(ARG2, arm::Mem(TMP, offsetof(ErlSubBits, start)));
                a.ldr(ARG3, arm::Mem(TMP, offsetof(ErlSubBits, end)));
                a.sub(ARG3, ARG3, ARG2);
                a.bind(not_sub_bits);

                a.ldr(TMP, TMP_MEM5q);
                a.add(TMP, TMP, ARG3);
                a.str(TMP, TMP_MEM5q);
                continue;
            }

            if (seg.unit != 0) {
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_DEPENDS,
                                                            BSC_INFO_SIZE,
                                                            BSC_VALUE_ARG3));
                }

                mov_arg(ARG3, seg.size);
                a.and_(ARG2, ARG3, imm(_TAG_IMMED1_MASK));
                mov_imm(ARG1, _TAG_IMMED1_SMALL);
                a.cmp(ARG2, ARG1);
                a.b_ne(resolve_label(error, dispUnknown));
                a.tst(ARG3, imm(0x80000000u));
                a.b_ne(resolve_label(error, dispUnknown));

                a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(ARG2, seg.unit);
                    a.mul(ARG3, ARG3, ARG2);
                }
                a.ldr(TMP, TMP_MEM5q);
                a.add(TMP, TMP, ARG3);
                a.str(TMP, TMP_MEM5q);
                continue;
            }

            switch (seg.type) {
            case am_utf8: {
                Label next = a.newLabel();
                Label one_byte = a.newLabel();
                Label two_bytes = a.newLabel();
                Label three_or_four = a.newLabel();

                mov_arg(ARG3, seg.src);
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG3));
                }

                a.and_(ARG2, ARG3, imm(_TAG_IMMED1_MASK));
                mov_imm(ARG1, _TAG_IMMED1_SMALL);
                a.cmp(ARG2, ARG1);
                a.b_ne(resolve_label(error, dispUnknown));

                a.asr(ARG1, ARG3, imm(_TAG_IMMED1_SIZE));
                mov_imm(ARG2, 8);
                mov_imm(TMP, 0x7f);
                a.cmp(ARG1, TMP);
                a.b_ls(one_byte);

                mov_imm(ARG2, 16);
                mov_imm(TMP, 0x7ff);
                a.cmp(ARG1, TMP);
                a.b_ls(two_bytes);

                a.b(three_or_four);

                a.bind(one_byte);
                a.b(next);

                a.bind(two_bytes);
                a.b(next);

                a.bind(three_or_four);
                a.lsr(TMP, ARG1, imm(11));
                mov_imm(ARG3, 0x1b);
                a.cmp(TMP, ARG3);
                a.b_eq(resolve_label(error, dispUnknown));

                mov_imm(ARG2, 24);
                mov_imm(TMP, 0x10000);
                a.cmp(ARG1, TMP);
                a.b_lo(next);
                mov_imm(ARG2, 32);
                mov_imm(TMP, 0x110000);
                a.cmp(ARG1, TMP);
                a.b_hs(resolve_label(error, dispUnknown));

                a.bind(next);
                a.ldr(TMP, TMP_MEM5q);
                a.add(TMP, TMP, ARG2);
                a.str(TMP, TMP_MEM5q);
                continue;
            }
            case am_utf16: {
                /* Mirrors ARM64 behavior: non-small values are handled by
                 * runtime helper later; size prepass uses 16/32 estimate. */
                Label utf16_add = a.newLabel();
                mov_arg(ARG3, seg.src);
                a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
                mov_imm(ARG2, 16);
                mov_imm(TMP, 0x10000);
                a.cmp(ARG3, TMP);
                a.b_lo(utf16_add);
                mov_imm(ARG2, 32);
                a.bind(utf16_add);
                a.ldr(TMP, TMP_MEM5q);
                a.add(TMP, TMP, ARG2);
                a.str(TMP, TMP_MEM5q);
                continue;
            }
            case am_utf32: {
                Label utf32_next = a.newLabel();
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG3));
                }

                mov_arg(ARG3, seg.src);
                a.ldr(TMP, TMP_MEM5q);
                add(TMP, TMP, 32);
                a.str(TMP, TMP_MEM5q);

                a.and_(ARG2, ARG3, imm(_TAG_IMMED1_MASK));
                mov_imm(ARG1, _TAG_IMMED1_SMALL);
                a.cmp(ARG2, ARG1);
                a.b_ne(resolve_label(error, dispUnknown));

                mov_imm(ARG2, make_small(0xD800));
                a.cmp(ARG3, ARG2);
                a.b_lo(utf32_next);
                mov_imm(ARG2, make_small(0xDFFF));
                a.cmp(ARG3, ARG2);
                a.b_ls(resolve_label(error, dispUnknown));
                mov_imm(ARG2, make_small(0x10FFFF));
                a.cmp(ARG3, ARG2);
                a.b_hi(resolve_label(error, dispUnknown));
                a.bind(utf32_next);
                continue;
            }
            default:
                break;
            }

            if (Fail.get() != 0) {
                a.b(resolve_beam_label(Fail, dispUnknown));
            } else {
                mov_imm(ARG1, NIL);
                mov_imm(ARG4, 0);
                mov_imm(ARG3, BADARG);
                a.b(resolve_label(error, dispUnknown));
            }
            return;
        }
    }

    if (!dynamic_size && num_bits <= ERL_ONHEAP_BITS_LIMIT) {
        static constexpr auto cur_bin_offset =
                offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state) +
                offsetof(struct erl_bits_state, erts_current_bin_);
        arm::Mem mem_bin_base = arm::Mem(scheduler_registers, cur_bin_offset);
        Uint heap_size = heap_bits_size(num_bits);
        Uint allocated_size = (heap_size - heap_bits_size(0)) * sizeof(Eterm);

        emit_gc_test(ArgWord(0), ArgWord(heap_size + Alloc.get()), Live);
        mov_imm(TMP, header_heap_bits(num_bits));
        mov_imm(VAR, num_bits);
        a.add(ARG1, HTOP, imm(TAG_PRIMARY_BOXED));
        a.str(TMP, arm::Mem(HTOP).post(sizeof(Eterm)));
        a.str(VAR, arm::Mem(HTOP).post(sizeof(Eterm)));
        a.str(HTOP, mem_bin_base);
        mov_imm(TMP, 0);
        a.str(TMP, arm::Mem(scheduler_registers, cur_bin_offset + sizeof(Eterm)));
        add(HTOP, HTOP, allocated_size);
    } else if (!dynamic_size) {
        mov_imm(ARG4, num_bits);
        mov_imm(TMP, Alloc.get());
        a.str(TMP, TMP_MEM2q);
        mov_imm(TMP, Live.get());
        a.str(TMP, TMP_MEM3q);
        fragment_call(ga->get_bs_init_bits_shared());
    } else {
        a.ldr(ARG4, TMP_MEM5q);
        mov_imm(TMP, Alloc.get());
        a.str(TMP, TMP_MEM2q);
        mov_imm(TMP, Live.get());
        a.str(TMP, TMP_MEM3q);
        fragment_call(ga->get_bs_init_bits_shared());
    }

    a.str(ARG1, TMP_MEM1q);

    for (auto seg : segments) {
        switch (seg.type) {
        case am_append:
        case am_private_append:
        case am_binary:
            {
                Label ok = a.newLabel();
                if (seg.type == am_append || seg.type == am_private_append) {
                    mov_imm(ARG3, seg.unit);
                    mov_arg(ARG2, seg.src);
                    a.mov(ARG1, c_p);
                    emit_enter_runtime<Update::eReductions>();
                    runtime_call<3>(erts_new_bs_put_binary_all);
                    emit_leave_runtime<Update::eReductions>();
                } else if (seg.effectiveSize >= 0) {
                    mov_imm(ARG3, seg.effectiveSize);
                    mov_arg(ARG2, seg.src);
                    a.mov(ARG1, c_p);
                    emit_enter_runtime<Update::eReductions>();
                    runtime_call<3>(erts_new_bs_put_binary);
                    emit_leave_runtime<Update::eReductions>();
                } else if (seg.size.isAtom() &&
                           seg.size.as<ArgAtom>().get() == am_all) {
                    mov_imm(ARG3, seg.unit);
                    mov_arg(ARG2, seg.src);
                    a.mov(ARG1, c_p);
                    emit_enter_runtime<Update::eReductions>();
                    runtime_call<3>(erts_new_bs_put_binary_all);
                    emit_leave_runtime<Update::eReductions>();
                } else {
                    mov_arg(ARG3, seg.size);
                    a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
                    if (seg.unit != 1) {
                        mov_imm(ARG2, seg.unit);
                        a.mul(ARG3, ARG3, ARG2);
                    }
                    mov_arg(ARG2, seg.src);
                    a.mov(ARG1, c_p);
                    emit_enter_runtime<Update::eReductions>();
                    runtime_call<3>(erts_new_bs_put_binary);
                    emit_leave_runtime<Update::eReductions>();
                }

                a.tst(ARG1, ARG1);
                a.b_ne(ok);
                if (Fail.get() == 0) {
                    Uint error_info =
                            (seg.type == am_binary && seg.size.isAtom() &&
                             seg.size.as<ArgAtom>().get() == am_all) ||
                                            seg.type == am_append ||
                                            seg.type == am_private_append
                                    ? beam_jit_update_bsc_reason_info(
                                              seg.error_info,
                                              BSC_REASON_BADARG,
                                              BSC_INFO_UNIT,
                                              BSC_VALUE_FVALUE)
                                    : beam_jit_update_bsc_reason_info(
                                              seg.error_info,
                                              BSC_REASON_BADARG,
                                              BSC_INFO_DEPENDS,
                                              BSC_VALUE_FVALUE);
                    mov_imm(ARG4,
                            error_info);
                    mov_arg(ARG1, seg.src);
                }
                a.b(resolve_label(error, dispUnknown));
                a.bind(ok);
            }
            break;
        case am_float:
            {
                if (seg.effectiveSize >= 0) {
                    mov_imm(ARG3, seg.effectiveSize);
                } else {
                    mov_arg(ARG3, seg.size);
                    a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
                    if (seg.unit != 1) {
                        mov_imm(ARG2, seg.unit);
                        a.mul(ARG3, ARG3, ARG2);
                    }
                }
                mov_arg(ARG2, seg.src);
                mov_imm(ARG4, seg.flags);
                a.mov(ARG1, c_p);
                emit_enter_runtime();
                runtime_call<4>(erts_new_bs_put_float);
                emit_leave_runtime();
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_FVALUE,
                                                            BSC_VALUE_ARG1));
                }
                emit_branch_if_value(ARG1, resolve_label(error, dispUnknown));
            }
            break;
        case am_integer:
            if (seg.effectiveSize >= 0) {
                mov_imm(ARG3, seg.effectiveSize);
            } else {
                mov_arg(ARG3, seg.size);
                a.asr(ARG3, ARG3, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(ARG2, seg.unit);
                    a.mul(ARG3, ARG3, ARG2);
                }
            }

            if (seg.effectiveSize >= 0 && seg.src.isSmall() &&
                seg.src.as<ArgSmall>().getSigned() == 0 &&
                (seg.effectiveSize % 8) == 0) {
                set_zero(seg.effectiveSize);
            } else {
                Label ok = a.newLabel();
                mov_arg(ARG2, seg.src);
                mov_imm(ARG4, seg.flags);
                lea(ARG1,
                    getSchedulerRegRef(offsetof(ErtsSchedulerRegisters,
                                                aux_regs.d.erl_bits_state)));
                emit_enter_runtime();
                runtime_call<4>(erts_new_bs_put_integer);
                emit_leave_runtime();
                a.tst(ARG1, ARG1);
                a.b_ne(ok);
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                    mov_arg(ARG1, seg.src);
                }
                a.b(resolve_label(error, dispUnknown));
                a.bind(ok);
            }
            break;
        case am_string: {
            ArgBytePtr string_ptr(
                    ArgVal(ArgVal::BytePtr, seg.src.as<ArgWord>().get()));
            Label no_prefix = a.newLabel();
            mov_imm(ARG3, seg.effectiveSize / 8);
            mov_arg(ARG2, string_ptr);

            /* Some short string operands can be emitted as length-prefixed
             * blobs on ARM32. Detect that form and skip the prefix byte. */
            a.ldrb(TMP, arm::Mem(ARG2));
            add(VAR, ARG3, 1);
            a.cmp(TMP, VAR);
            a.b_ne(no_prefix);
            add(ARG2, ARG2, 1);
            a.bind(no_prefix);

            lea(ARG1,
                getSchedulerRegRef(offsetof(ErtsSchedulerRegisters,
                                            aux_regs.d.erl_bits_state)));
            emit_enter_runtime();
            runtime_call<3>(erts_new_bs_put_string);
            emit_leave_runtime();
            break;
        }
        case am_utf8:
            mov_arg(ARG2, seg.src);
            lea(ARG1,
                getSchedulerRegRef(offsetof(ErtsSchedulerRegisters,
                                            aux_regs.d.erl_bits_state)));
            emit_enter_runtime();
            runtime_call<2>(erts_bs_put_utf8);
            emit_leave_runtime();
            if (Fail.get() == 0) {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
                mov_arg(ARG1, seg.src);
            }
            a.tst(ARG1, ARG1);
            a.b_eq(resolve_label(error, dispUnknown));
            break;
        case am_utf16:
            {
                Label ok = a.newLabel();
                mov_arg(ARG2, seg.src);
                mov_imm(ARG3, seg.flags);
                lea(ARG1,
                    getSchedulerRegRef(offsetof(ErtsSchedulerRegisters,
                                                aux_regs.d.erl_bits_state)));
                emit_enter_runtime();
                runtime_call<3>(erts_bs_put_utf16);
                emit_leave_runtime();
                a.tst(ARG1, ARG1);
                a.b_ne(ok);
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                    mov_arg(ARG1, seg.src);
                }
                a.b(resolve_label(error, dispUnknown));
                a.bind(ok);
            }
            break;
        case am_utf32:
            {
                Label ok = a.newLabel();
                mov_arg(ARG2, seg.src);
                mov_imm(ARG3, 4 * 8);
                mov_imm(ARG4, seg.flags);
                lea(ARG1,
                    getSchedulerRegRef(offsetof(ErtsSchedulerRegisters,
                                                aux_regs.d.erl_bits_state)));
                emit_enter_runtime();
                runtime_call<4>(erts_new_bs_put_integer);
                emit_leave_runtime();
                a.tst(ARG1, ARG1);
                a.b_ne(ok);
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                    mov_arg(ARG1, seg.src);
                }
                a.b(resolve_label(error, dispUnknown));
                a.bind(ok);
            }
            break;
        default:
            if (Fail.get() != 0) {
                a.b(resolve_beam_label(Fail, dispUnknown));
            } else {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
                mov_arg(ARG1, seg.src);
                mov_imm(ARG3, BADARG);
                a.b(resolve_label(error, dispUnknown));
            }
            return;
        }
    }

    a.ldr(ARG1, TMP_MEM1q);
    mov_arg(Dst, ARG1);
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
                                         const a32::Gp bin_base,
                                         const a32::Gp bin_offset,
                                         const a32::Gp bitdata) {
    // TODO
    emit_nyi("emit_read_bits");
}

void BeamModuleAssembler::emit_extract_integer(const a32::Gp &bitdata,
                                               const a32::Gp &small_tag,
                                               Uint flags,
                                               Uint position,
                                               Uint bits,
                                               const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_extract_integer");
}

void BeamModuleAssembler::emit_extract_bitstring(const a32::Gp bitdata,
                                                 Uint position,
                                                 Uint bits,
                                                 const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_extract_bitstring");
}

static std::vector<BsmSegment> opt_bsm_segments(
        const std::vector<BsmSegment> segments,
        const ArgWord &Need,
        const ArgWord &Live) {
    std::vector<BsmSegment> segs;
    Uint heap_need = Need.get();

    /* Keep optimization conservative on ARM32 for now: account heap needs and
     * inject test_heap barriers, but don't rewrite into READ/EXTRACT actions. */
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

    for (auto seg : segments) {
        if (heap_need != 0 && seg.live.isWord()) {
            BsmSegment s = seg;
            s.action = BsmSegment::action::TEST_HEAP;
            s.size = heap_need;
            segs.push_back(s);
            heap_need = 0;
        }
        segs.push_back(seg);
    }

    if (heap_need) {
        BsmSegment seg;
        seg.action = BsmSegment::action::TEST_HEAP;
        seg.size = heap_need;
        seg.live = Live;
        segs.push_back(seg);
    }

    return segs;
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
        case am_ensure_at_least:
            seg.action = BsmSegment::action::ENSURE_AT_LEAST;
            seg.size = current[0].as<ArgWord>().get();
            seg.unit = current[1].as<ArgWord>().get();
            current += 2;
            break;
        case am_ensure_exactly:
            seg.action = BsmSegment::action::ENSURE_EXACTLY;
            seg.size = current[0].as<ArgWord>().get();
            current += 1;
            break;
        case am_binary:
        case am_integer: {
            auto size = current[2].as<ArgWord>().get();
            auto unit = current[3].as<ArgWord>().get();

            seg.action = (cmd == am_integer) ? BsmSegment::action::GET_INTEGER
                                             : BsmSegment::action::GET_BITSTRING;
            seg.live = current[0];
            seg.size = size * unit;
            seg.unit = unit;
            seg.flags = bs_get_flags(current[1]);
            seg.dst = current[4].as<ArgRegister>();
            current += 5;
            break;
        }
        case am_get_tail:
            seg.action = BsmSegment::action::GET_TAIL;
            seg.live = current[0].as<ArgWord>();
            seg.dst = current[2].as<ArgRegister>();
            current += 3;
            break;
        case am_skip:
            seg.action = BsmSegment::action::SKIP;
            seg.size = current[0].as<ArgWord>().get();
            seg.flags = 0;
            current += 1;
            break;
        case am_Eq:
            seg.action = BsmSegment::action::EQ;
            seg.live = current[0];
            seg.size = current[1].as<ArgWord>().get();
            seg.unit = current[2].as<ArgWord>().get();
            current += 3;

            /* Keep EQ comparisons in small chunks so we can compare against a
             * small literal term directly on ARM32.
             *
             * This mirrors string/literal matching semantics without relying on
             * large integer term construction for wider segments. */
            if (seg.size > 8) {
                Uint remaining = seg.size;
                Uint value = seg.unit;

                while (remaining > 0) {
                    BsmSegment piece = seg;
                    Uint chunk = remaining >= 8 ? 8 : remaining;
                    Uint shift = remaining - chunk;
                    Uint mask = (chunk == 32) ? ~0u : ((1u << chunk) - 1);

                    piece.size = chunk;
                    piece.unit = (value >> shift) & mask;
                    segments.push_back(piece);
                    remaining -= chunk;
                }

                continue;
            }
            break;
        default:
            abort();
            break;
        }

        segments.push_back(seg);
    }

    segments = opt_bsm_segments(segments, Need, Live);
    Label fail = resolve_beam_label(Fail, disp32MB);

    for (auto seg : segments) {
        switch (seg.action) {
        case BsmSegment::action::ENSURE_AT_LEAST: {
            const auto size = seg.size;
            const auto unit = seg.unit;

            mov_arg(ARG1, Ctx);
            a.ldr(ARG2, emit_boxed_val(ARG1, start_offset));
            a.ldr(ARG3, emit_boxed_val(ARG1, end_offset));
            a.sub(VAR, ARG3, ARG2); /* remaining bits */

            if (size != 0) {
                mov_imm(TMP, size);
                a.cmp(VAR, TMP);
                a.b_lo(fail);
            }

            if (unit != 1) {
                if (size % unit != 0) {
                    sub(VAR, VAR, size);
                }

                if ((unit & (unit - 1)) == 0) {
                    a.tst(VAR, imm(unit - 1));
                    a.b_ne(fail);
                } else {
                    Label mod_loop = a.newLabel(), mod_done = a.newLabel();
                    mov_imm(TMP, unit);
                    a.bind(mod_loop);
                    a.cmp(VAR, TMP);
                    a.b_lo(mod_done);
                    a.sub(VAR, VAR, TMP);
                    a.b(mod_loop);
                    a.bind(mod_done);
                    a.tst(VAR, VAR);
                    a.b_ne(fail);
                }
            }
            break;
        }
        case BsmSegment::action::ENSURE_EXACTLY: {
            mov_arg(ARG1, Ctx);
            a.ldr(ARG2, emit_boxed_val(ARG1, start_offset));
            a.ldr(ARG3, emit_boxed_val(ARG1, end_offset));
            a.sub(VAR, ARG3, ARG2);
            if (seg.size != 0) {
                mov_imm(TMP, seg.size);
                a.cmp(VAR, TMP);
            } else {
                a.tst(VAR, VAR);
            }
            a.b_ne(fail);
            break;
        }
        case BsmSegment::action::TEST_HEAP:
            emit_gc_test(ArgWord(0), ArgWord(seg.size), seg.live.as<ArgWord>());
            break;
        case BsmSegment::action::GET_INTEGER: {
            const Uint flags = seg.flags;
            const Uint bits = seg.size;
            const auto Dst = seg.dst;

            mov_arg(ARG4, Ctx);
            emit_untag_ptr(ARG4, ARG4);

            if (bits >= SMALL_BITS) {
                emit_enter_runtime<Update::eHeapOnlyAlloc>();
            } else {
                emit_enter_runtime();
            }

            a.mov(ARG1, c_p);
            mov_imm(ARG2, bits);
            mov_imm(ARG3, flags);
            runtime_call<4>(erts_bs_get_integer_2);

            if (bits >= SMALL_BITS) {
                emit_leave_runtime<Update::eHeapOnlyAlloc>();
            } else {
                emit_leave_runtime();
            }

            emit_branch_if_not_value(ARG1, fail);
            mov_arg(Dst, ARG1);
            break;
        }
        case BsmSegment::action::GET_BITSTRING: {
            const Uint bits = seg.size;
            const auto Dst = seg.dst;

            if (bits > 64) {
                mov_arg(VAR, Ctx);
                a.ldr(TMP, emit_boxed_val(VAR, start_offset));
                a.str(TMP, TMP_MEM1q);

                lea(ARG1, arm::Mem(c_p, offsetof(Process, htop)));
                if (bits <= ERL_ONHEAP_BITS_LIMIT) {
                    comment("skipped setting registers not used for heap binary");
                } else {
                    a.ldr(ARG2, emit_boxed_val(VAR, orig_offset));
                    a.mov(ARG3, ARG2);
                    mov_imm(TMP, TAG_PTR_MASK__);
                    a.and_(ARG2, ARG2, TMP);   /* br_flags */
                    mov_imm(TMP, ~TAG_PTR_MASK__);
                    a.and_(ARG3, ARG3, TMP);   /* br */
                }

                a.ldr(ARG4, emit_boxed_val(VAR, base_offset));
                mov_imm(TMP, ~ERL_SUB_BITS_FLAG_MASK);
                a.and_(ARG4, ARG4, TMP);
                a.ldr(TMP, TMP_MEM1q);
                add(TMP, TMP, bits);
                a.str(TMP, emit_boxed_val(VAR, start_offset));

                emit_enter_runtime<Update::eHeapOnlyAlloc>();
                a.sub(a32::sp, a32::sp, imm(8));
                a.ldr(TMP, TMP_MEM1q);
                a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5: offset */
                mov_imm(TMP, bits);
                a.str(TMP, arm::Mem(a32::sp, 4)); /* arg6: size */
                runtime_call<6>(erts_build_sub_bitstring);
                a.add(a32::sp, a32::sp, imm(8));
                emit_leave_runtime<Update::eHeapOnlyAlloc>();

                mov_arg(Dst, ARG1);
            } else {
                mov_arg(ARG4, Ctx);
                emit_untag_ptr(ARG4, ARG4);

                emit_enter_runtime<Update::eHeapOnlyAlloc>();
                a.mov(ARG1, c_p);
                mov_imm(ARG2, bits);
                mov_imm(ARG3, 0);
                runtime_call<4>(erts_bs_get_binary_2);
                emit_leave_runtime<Update::eHeapOnlyAlloc>();

                emit_branch_if_not_value(ARG1, fail);
                mov_arg(Dst, ARG1);
            }
            break;
        }
        case BsmSegment::action::GET_TAIL: {
            const auto Dst = seg.dst;

            mov_arg(VAR, Ctx);
            a.ldr(TMP, emit_boxed_val(VAR, start_offset));
            a.str(TMP, TMP_MEM1q);
            a.ldr(ARG4, emit_boxed_val(VAR, end_offset));
            a.sub(TMP, ARG4, TMP); /* bits left */
            a.str(TMP, TMP_MEM2q);

            lea(ARG1, arm::Mem(c_p, offsetof(Process, htop)));
            a.ldr(ARG2, emit_boxed_val(VAR, orig_offset));
            a.mov(ARG3, ARG2);
            mov_imm(TMP, TAG_PTR_MASK__);
            a.and_(ARG2, ARG2, TMP);   /* br_flags */
            mov_imm(TMP, ~TAG_PTR_MASK__);
            a.and_(ARG3, ARG3, TMP);   /* br */

            a.ldr(ARG4, emit_boxed_val(VAR, base_offset));
            mov_imm(TMP, ~ERL_SUB_BITS_FLAG_MASK);
            a.and_(ARG4, ARG4, TMP);

            emit_enter_runtime<Update::eHeapOnlyAlloc>();
            a.sub(a32::sp, a32::sp, imm(8));
            a.ldr(TMP, TMP_MEM1q);
            a.str(TMP, arm::Mem(a32::sp, 0)); /* arg5: offset */
            a.ldr(TMP, TMP_MEM2q);
            a.str(TMP, arm::Mem(a32::sp, 4)); /* arg6: size */
            runtime_call<6>(erts_build_sub_bitstring);
            a.add(a32::sp, a32::sp, imm(8));
            emit_leave_runtime<Update::eHeapOnlyAlloc>();
            mov_arg(Dst, ARG1);
            break;
        }
        case BsmSegment::action::SKIP:
            mov_arg(ARG1, Ctx);
            a.ldr(TMP, emit_boxed_val(ARG1, start_offset));
            add(TMP, TMP, seg.size);
            a.str(TMP, emit_boxed_val(ARG1, start_offset));
            break;
        case BsmSegment::action::EQ: {
            /* Conservative path: decode as integer and compare to expected
             * small value. */
            if (seg.size >= SMALL_BITS) {
                a.b(fail);
                break;
            }

            mov_arg(ARG4, Ctx);
            emit_untag_ptr(ARG4, ARG4);
            emit_enter_runtime();
            a.mov(ARG1, c_p);
            mov_imm(ARG2, seg.size);
            mov_imm(ARG3, 0);
            runtime_call<4>(erts_bs_get_integer_2);
            emit_leave_runtime();

            emit_branch_if_not_value(ARG1, fail);
            mov_imm(TMP, make_small(seg.unit));
            a.cmp(ARG1, TMP);
            a.b_ne(fail);
            break;
        }
        case BsmSegment::action::READ:
        case BsmSegment::action::EXTRACT_BITSTRING:
        case BsmSegment::action::EXTRACT_INTEGER:
        case BsmSegment::action::DROP:
            a.b(fail);
            break;
        }
    }
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
        return val.as<ArgWord>().get();
    } else {
        ASSERT(0);
        return 0;
    }
}
