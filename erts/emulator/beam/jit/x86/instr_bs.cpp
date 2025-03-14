/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

/* Clobbers RET + ARG3
 *
 * If max_size > 0, we jump to the fail label when Size > max_size
 *
 * Returns -1 when the field check always fails, 1 if it may fail, and 0 if it
 * never fails. */
int BeamModuleAssembler::emit_bs_get_field_size(const ArgSource &Size,
                                                int unit,
                                                Label fail,
                                                const x86::Gp &out,
                                                unsigned max_size) {
    if (Size.isImmed()) {
        if (Size.isSmall()) {
            Sint sval = Size.as<ArgSmall>().getSigned();

            if (sval < 0) {
                /* badarg */
            } else if (max_size && sval > max_size) {
                /* badarg */
            } else if (sval > (MAX_SMALL / unit)) {
                /* system_limit */
            } else {
                mov_imm(out, sval * unit);
                return 0;
            }
        }

        a.jmp(fail);
        return -1;
    } else {
        bool can_fail = true;

        mov_arg(RET, Size);

        if (always_small(Size)) {
            auto [min, max] = getClampedRange(Size);
            can_fail =
                    !(0 <= min && (max >> (SMALL_BITS - ERL_UNIT_BITS)) == 0);
            comment("simplified segment size checks because "
                    "the types are known");
        } else {
            a.mov(ARG3d, RETd);
            a.and_(ARG3d, imm(_TAG_IMMED1_MASK));
            a.cmp(ARG3d, imm(_TAG_IMMED1_SMALL));
            a.jne(fail);
        }

        if (max_size) {
            ASSERT(Support::isInt32((Sint)make_small(max_size)));
            a.cmp(RET, imm(make_small(max_size)));
            a.ja(fail);
        }

        if (unit == 0) {
            mov_imm(RET, 0);
        } else if (unit == 1) {
            a.sar(RET, imm(_TAG_IMMED1_SIZE));
            if (can_fail) {
                a.js(fail);
            }
        } else if (!can_fail && Support::isPowerOf2(unit)) {
            int trailing_bits = Support::ctz<Eterm>(unit);
            a.and_(RET, imm(~_TAG_IMMED1_MASK));
            if (trailing_bits < _TAG_IMMED1_SIZE) {
                a.sar(RET, imm(_TAG_IMMED1_SIZE - trailing_bits));
            } else if (trailing_bits > _TAG_IMMED1_SIZE) {
                a.shl(RET, imm(trailing_bits - _TAG_IMMED1_SIZE));
            }
        } else {
            /* Untag the size but don't shift it just yet, we want to fail on
             * overflow if the final result doesn't fit into a small. */
            a.and_(RET, imm(~_TAG_IMMED1_MASK));
            if (can_fail) {
                a.js(fail);
            }

            /* Size = (Size) * (Unit) */
            mov_imm(ARG3, unit);
            a.mul(ARG3); /* CLOBBERS ARG3! */
            if (can_fail) {
                a.jo(fail);
            }

            a.sar(RET, imm(_TAG_IMMED1_SIZE));
        }

        if (out != RET) {
            a.mov(out, RET);
        }

        return 1;
    }
}

/* Set the error reason when a size check has failed. */
void BeamGlobalAssembler::emit_bs_size_check_shared() {
    emit_enter_runtime();
    a.mov(ARG1, c_p);
    runtime_call<void (*)(Process *, Eterm),
                 beam_jit_bs_field_size_argument_error>();
    emit_leave_runtime();
    a.ret();
}

void BeamModuleAssembler::emit_i_bs_start_match3(const ArgRegister &Src,
                                                 const ArgWord &Live,
                                                 const ArgLabel &Fail,
                                                 const ArgRegister &Dst) {
    Label next = a.newLabel();

    mov_arg(ARG2, Src);

    if (Fail.get() != 0) {
        emit_is_boxed(resolve_beam_label(Fail), Src, ARG2);
    } else {
        /* bs_start_match3 may not throw, and the compiler will only emit {f,0}
         * when it knows that the source is a match state or binary, so we're
         * free to skip the binary tests. */
    }

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG2);

    /* Boxed terms have at least one word past the header, so we can
     * speculatively load base_flags together with the thing_word. */
    ERTS_CT_ASSERT(offsetof(ErlSubBits, thing_word) == 0 &&
                   offsetof(ErlSubBits, base_flags) == sizeof(Eterm));
    a.mov(RETd,
          emit_boxed_val(boxed_ptr,
                         offsetof(ErlSubBits, base_flags),
                         sizeof(Uint32)));

    ERTS_CT_ASSERT((HEADER_SUB_BITS & _TAG_PRIMARY_MASK) == 0 &&
                   (ERL_SUB_BITS_FLAG_MASK == _TAG_PRIMARY_MASK));
    a.and_(RETd, imm(ERL_SUB_BITS_FLAG_MASK));
    a.xor_(RETd, imm(HEADER_SUB_BITS | ERL_SUB_BITS_FLAGS_MATCH_CONTEXT));
    a.xor_(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
#if defined(HARD_DEBUG) || defined(ERTS_CCONV_DEBUG)
    a.jz(next);
#else
    a.short_().jz(next);
#endif

    {
        /* RETd now contains the thing_word and flag bits that differ. The
         * latter can be ignored, and we can check whether this is a bitstring
         * by testing whether all bits within the bitstring mask are zero. */
        if (Fail.get() != 0) {
            a.and_(RETd, imm(_BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK));
            a.jnz(resolve_beam_label(Fail));
        }

        emit_gc_test_preserve(ArgWord(ERL_SUB_BITS_SIZE), Live, Src, ARG2);

        emit_enter_runtime<Update::eHeapOnlyAlloc>();

        a.mov(ARG1, c_p);
        /* ARG2 was set above */
        runtime_call<ErlSubBits *(*)(Process *, Eterm),
                     erts_bs_start_match_3>();

        emit_leave_runtime<Update::eHeapOnlyAlloc>();

        a.lea(ARG2, x86::qword_ptr(RET, TAG_PRIMARY_BOXED));
    }

    a.bind(next);
    mov_arg(Dst, ARG2);
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgRegister &Ctx,
                                                 const ArgLabel &Fail,
                                                 const ArgWord &Bits,
                                                 const ArgBytePtr &Ptr) {
    const UWord size = Bits.get();
    Label fail = resolve_beam_label(Fail);

    mov_arg(ARG1, Ctx);

    a.mov(ARG2, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));
    a.lea(ARG3, x86::qword_ptr(ARG2, size));
    a.cmp(ARG3, emit_boxed_val(ARG1, offsetof(ErlSubBits, end)));
    a.ja(fail);

    a.mov(TMP_MEM1q, ARG1);

    /* ARG4 = sb->start & 7 */
    a.mov(ARG4, ARG2);
    a.and_(ARG4, imm(7));

    /* ARG3 = sb->base_flags + (sb->start >> 3) - match_ctx_flag */
    a.shr(ARG2, imm(3));
    a.mov(ARG3, emit_boxed_val(ARG1, offsetof(ErlSubBits, base_flags)));
    a.lea(ARG3,
          x86::qword_ptr(ARG3,
                         ARG2,
                         0,
                         -(Sint)ERL_SUB_BITS_FLAGS_MATCH_CONTEXT));

    emit_enter_runtime();

    mov_arg(ARG1, Ptr);
    mov_imm(ARG2, 0);
    mov_imm(ARG5, size);
    runtime_call<int (*)(const byte *, Uint, const byte *, Uint, Uint),
                 erts_cmp_bits>();

    emit_leave_runtime();

    a.test(RET, RET);
    a.jne(fail);

    a.mov(ARG1, TMP_MEM1q);
    a.add(emit_boxed_val(ARG1, offsetof(ErlSubBits, start)), imm(size));
}

void BeamModuleAssembler::emit_i_bs_get_position(const ArgRegister &Ctx,
                                                 const ArgRegister &Dst) {
    x86::Gp tmp_reg = alloc_temp_reg();

    mov_arg(ARG1, Ctx);

    /* Match contexts can never be literals, so we can skip clearing literal
     * tags. */
    mov_preserve_cache(tmp_reg,
                       emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));
    preserve_cache(
            [&]() {
                a.sal(tmp_reg, imm(_TAG_IMMED1_SIZE));
                a.or_(tmp_reg, imm(_TAG_IMMED1_SMALL));
            },
            tmp_reg);

    mov_arg(Dst, tmp_reg);
}

void BeamModuleAssembler::emit_bs_get_small(const Label &fail,
                                            const ArgRegister &Ctx,
                                            const ArgWord &Live,
                                            const ArgSource &Sz,
                                            Uint unit,
                                            Uint flags) {
    /* Clobbers RET + ARG3, returns a negative result if we always
     * fail and further work is redundant. */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG2) >= 0) {
        comment("simplified helper call because the result is a known small");

        mov_imm(ARG3, flags);
        mov_arg(ARG4, Ctx);
        a.sub(ARG4, imm(TAG_PRIMARY_BOXED));

        emit_enter_runtime();

        /* We KNOW that the process argument is never actually used. */
#ifdef DEBUG
        mov_imm(ARG1, 0);
#endif
        runtime_call<Eterm (*)(Process *, Uint, unsigned, ErlSubBits *),
                     erts_bs_get_integer_2>();

        emit_leave_runtime();

        emit_test_the_non_value(RET);
        a.je(fail);
    }
}

void BeamModuleAssembler::emit_bs_get_any_int(const Label &fail,
                                              const ArgRegister &Ctx,
                                              const ArgWord &Live,
                                              const ArgSource &Sz,
                                              Uint unit,
                                              Uint flags) {
    /* Clobbers RET + ARG3, returns a negative result if we always
     * fail and further work is redundant. */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG5) >= 0) {
        mov_arg(ARG3, Ctx);
        mov_imm(ARG4, flags);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<Eterm (*)(Process *, Eterm *, Eterm, Uint, Uint, Uint),
                     beam_jit_bs_get_integer>();

        emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

        emit_test_the_non_value(RET);
        a.je(fail);

        /* Test for max heap size exceeded. */
        emit_is_not_cons(resolve_fragment(ga->get_do_schedule()), RET);
    }
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
        Label fail = resolve_beam_label(Fail);
        int unit = Unit.get();
        auto max = std::get<1>(getClampedRange(Sz));
        bool potential_gc =
                max >= SMALL_BITS || (max * Unit.get()) >= SMALL_BITS;

        if (potential_gc) {
            emit_bs_get_any_int(fail, Ctx, Live, Sz, unit, flags);
        } else {
            emit_bs_get_small(fail, Ctx, Live, Sz, unit, flags);
        }

        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgLabel &Fail,
                                             const ArgRegister &Ctx,
                                             const ArgWord &Offset) {
    /* This instruction is only found in unoptimized code and in code
     * compiled for Erlang/OTP 25 and earlier. */
    const ArgVal match[] = {ArgAtom(am_ensure_exactly), Offset};
    const Span<ArgVal> args(match, sizeof(match) / sizeof(match[0]));

    emit_i_bs_match(Fail, Ctx, args);
}

void BeamModuleAssembler::emit_bs_set_position(const ArgRegister &Ctx,
                                               const ArgRegister &Pos) {
    mov_arg(ARG1, Ctx);
    mov_arg(ARG2, Pos);

    preserve_cache(
            [&]() {
                a.sar(ARG2, imm(_TAG_IMMED1_SIZE));
            },
            ARG2);
    mov_preserve_cache(emit_boxed_val(ARG1, offsetof(ErlSubBits, start)), ARG2);
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgRegister &Ctx,
                                                    const ArgLabel &Fail,
                                                    const ArgWord &Live,
                                                    const ArgWord &Unit,
                                                    const ArgRegister &Dst) {
    /* This instruction is only found in unoptimized code and in code
     * compiled for Erlang/OTP 25 and earlier. */
    unsigned unit = Unit.get();
    const ArgVal match[] = {ArgAtom(am_ensure_at_least),
                            ArgWord(0),
                            ArgWord(unit),

                            ArgAtom(am_get_tail),
                            ArgWord(Live),
                            ArgWord(unit),
                            Dst};
    const Span<ArgVal> args(match, sizeof(match) / sizeof(match[0]));

    emit_i_bs_match(Fail, Ctx, args);
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    emit_enter_runtime<Update::eHeapOnlyAlloc>();

    a.mov(ARG2, emit_boxed_val(ARG1, offsetof(ErlSubBits, orig)));
    a.mov(ARG3, ARG2);

    /* ARG2 = tag bits of mb.orig, ARG3 = mb.orig without tag bits  */
    a.and_(ARG2, imm(TAG_PTR_MASK__));
    a.and_(ARG3, imm(~TAG_PTR_MASK__));

    a.mov(ARG4, emit_boxed_val(ARG1, offsetof(ErlSubBits, base_flags)));
    a.and_(ARG4, imm(~ERL_SUB_BITS_FLAG_MASK));
    a.mov(ARG5, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));

    /* Extracted size = end - start */
    a.mov(ARG6, emit_boxed_val(ARG1, offsetof(ErlSubBits, end)));
    a.sub(ARG6, ARG5);

    a.lea(ARG1, x86::qword_ptr(c_p, offsetof(Process, htop)));
    runtime_call<Eterm (*)(Eterm **,
                           Eterm,
                           const BinRef *,
                           const byte *,
                           Uint,
                           Uint),
                 erts_build_sub_bitstring>();

    emit_leave_runtime<Update::eHeapOnlyAlloc>();

    a.ret();
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgRegister &Ctx,
                                           const ArgRegister &Dst,
                                           const ArgWord &Live) {
    mov_arg(ARG1, Ctx);

    emit_gc_test_preserve(ArgWord(BUILD_SUB_BITSTRING_HEAP_NEED),
                          Live,
                          Ctx,
                          ARG1);

    safe_fragment_call(ga->get_bs_get_tail_shared());

    mov_arg(Dst, RET);
}

/* Bits to skip are passed in RET */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgLabel &Fail,
                                            const ArgRegister &Ctx) {
    mov_arg(ARG1, Ctx);

    a.add(RET, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));
    a.cmp(RET, emit_boxed_val(ARG1, offsetof(ErlSubBits, end)));
    a.ja(resolve_beam_label(Fail));

    a.mov(emit_boxed_val(ARG1, offsetof(ErlSubBits, start)), RET);
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgRegister &Ctx,
                                               const ArgSource &Bits,
                                               const ArgLabel &Fail,
                                               const ArgWord &Unit) {
    Label fail;

    fail = resolve_beam_label(Fail);
    if (emit_bs_get_field_size(Bits, Unit.get(), fail, RET) >= 0) {
        emit_bs_skip_bits(Fail, Ctx);
    }
}

void BeamModuleAssembler::emit_bs_get_binary(const ArgWord heap_need,
                                             const ArgRegister &Ctx,
                                             const ArgLabel &Fail,
                                             const ArgWord &Live,
                                             const ArgSource &Size,
                                             const ArgWord &Unit,
                                             const ArgRegister &Dst) {
    Label fail;
    int unit;

    fail = resolve_beam_label(Fail);
    unit = Unit.get();

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Size, unit, fail, ARG2) >= 0) {
        a.mov(TMP_MEM1q, ARG2);

        mov_arg(ARG4, Ctx);

        emit_gc_test_preserve(heap_need, Live, Ctx, ARG4);

        emit_enter_runtime<Update::eHeapOnlyAlloc>();

        a.mov(ARG1, c_p);
        a.mov(ARG2, TMP_MEM1q);
        a.lea(ARG3, x86::qword_ptr(ARG4, -TAG_PRIMARY_BOXED));
        runtime_call<Eterm (*)(Process *, Uint, ErlSubBits *),
                     erts_bs_get_binary_2>();

        emit_leave_runtime<Update::eHeapOnlyAlloc>();

        emit_test_the_non_value(RET);
        a.je(fail);

        mov_arg(Dst, RET);
    }
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgRegister &Ctx,
                                                const ArgLabel &Fail,
                                                const ArgWord &Live,
                                                const ArgSource &Size,
                                                const ArgWord &Unit,
                                                const ArgRegister &Dst) {
    emit_bs_get_binary(ArgWord(BUILD_SUB_BITSTRING_HEAP_NEED),
                       Ctx,
                       Fail,
                       Live,
                       Size,
                       Unit,
                       Dst);
}

void BeamModuleAssembler::emit_i_bs_get_bin_and_tail(const ArgRegister &Ctx,
                                                     const ArgLabel &Fail,
                                                     const ArgWord &Live,
                                                     const ArgRegister &Size,
                                                     const ArgWord &Unit,
                                                     const ArgRegister &Dst1,
                                                     const ArgRegister &Dst2) {
    emit_bs_get_binary(ArgWord(2 * BUILD_SUB_BITSTRING_HEAP_NEED),
                       Ctx,
                       Fail,
                       Live,
                       Size,
                       Unit,
                       Dst1);

    mov_arg(ARG1, Ctx);
    safe_fragment_call(ga->get_bs_get_tail_shared());
    mov_arg(Dst2, RET);
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    Label fail;
    Sint unit;

    fail = resolve_beam_label(Fail);
    unit = Flags.get() >> 3;

    mov_arg(ARG4, Ctx);

    emit_gc_test_preserve(ArgWord(FLOAT_SIZE_OBJECT), Live, Ctx, ARG4);

    if (emit_bs_get_field_size(Sz, unit, fail, ARG2, 64) >= 0) {
        emit_enter_runtime<Update::eHeapOnlyAlloc>();

        a.mov(ARG1, c_p);
        /* ARG2 set above */
        mov_imm(ARG3, Flags.get());
        a.sub(ARG4, imm(TAG_PRIMARY_BOXED));
        runtime_call<Eterm (*)(Process *, Uint, unsigned, ErlSubBits *),
                     erts_bs_get_float_2>();

        emit_leave_runtime<Update::eHeapOnlyAlloc>();

        emit_test_the_non_value(RET);
        a.je(fail);

        mov_arg(Dst, RET);
    }
}

/*
 * ARG1 = pointer to match state
 * ARG2 = position in binary in bits
 * ARG3 = base pointer to binary data
 * RET = number of bits left in binary
 *
 * This fragment is called if the binary is unaligned and/or the number
 * of remaining bits is less than 32.
 *
 * See the comment for emit_bs_get_utf8_shared() for details about the
 * return value.
 */
void BeamGlobalAssembler::emit_bs_get_utf8_short_shared() {
    const int start_offset = offsetof(ErlSubBits, start);

    const x86::Gp ctx = ARG1;
    const x86::Gp bin_position = ARG2;
    const x86::Gp bin_base = ARG3;

    Label at_least_one = a.newLabel();
    Label two = a.newLabel();
    Label three_or_more = a.newLabel();
    Label four = a.newLabel();
    Label five = a.newLabel();
    Label read_done = a.newLabel();
    Label no_masking = a.newLabel();
    Label ascii = a.newLabel();

    /* Calculate the number of bytes remaining in the binary and error
     * out if less than one. */
    a.shr(RET, imm(3));
    a.test(RET, RET);
    a.short_().jne(at_least_one);

    /* ZF is is already set. */
    a.ret();

    a.bind(at_least_one);

    /* Save number of bytes remaining in binary. */
    a.mov(ARG5, RET);

    /* If the position in the binary is not byte-aligned, we'll need
     * to read one more byte. */
    a.test(bin_position, imm(7));
    a.setne(ARG4.r8());
    a.movzx(ARG4d, ARG4.r8());
    a.add(RET, ARG4);

    /* Save original position in bits and set up byte offset for
     * reading. */
    a.push(bin_position);
    a.shr(bin_position, imm(3));

    a.cmp(RET, imm(2));
    a.short_().je(two);
    a.short_().ja(three_or_more);

    /* Read one byte (always byte-aligned). */
    a.mov(RETb, x86::byte_ptr(bin_base, bin_position));
    a.movzx(RETd, RETb);
    a.short_().jmp(read_done);

    /* Read two bytes. */
    a.bind(two);
    a.mov(RET.r16(), x86::word_ptr(bin_base, bin_position));
    a.movzx(RETd, RET.r16());
    a.short_().jmp(read_done);

    a.bind(three_or_more);
    a.cmp(RET, imm(4));
    a.short_().je(four);
    a.short_().ja(five);

    /* Read three bytes. */
    a.mov(RET.r8(), x86::byte_ptr(bin_base, bin_position, 0, 2));
    a.movzx(RETd, RETb);
    a.shl(RETd, imm(16));
    a.mov(RET.r16(), x86::word_ptr(bin_base, bin_position));
    a.short_().jmp(read_done);

    /* Read four bytes (always unaligned). */
    a.bind(four);
    a.mov(RETd, x86::dword_ptr(bin_base, bin_position));
    a.short_().jmp(read_done);

    /* Read five bytes (always unaligned). */
    a.bind(five);
    a.mov(RETd, x86::dword_ptr(bin_base, bin_position));
    a.mov(ARG4.r8(), x86::byte_ptr(bin_base, bin_position, 0, 4));
    a.movzx(ARG4d, ARG4.r8());
    a.shl(ARG4, imm(32));
    a.or_(RET, ARG4);

    /* Handle the bytes read. */
    a.bind(read_done);
    a.pop(bin_position);
    a.bswap(RET);

    if (x86::rcx == ctx) {
        a.push(x86::rcx);
    }
    a.mov(x86::ecx, bin_position.r32());
    a.and_(x86::cl, imm(7));
    a.shl(RET, x86::cl);

    /* Check whether we will need to clear out trailing
     * garbage not part of the binary. */
    a.mov(x86::cl, 64);
    a.cmp(ARG5, imm(3));
    a.short_().ja(no_masking);

    /* Calculate a byte mask and zero out trailing garbage. */
    a.shl(ARG5d, imm(3));
    a.sub(x86::cl, ARG5.r8());
    mov_imm(ARG5, -1);
    a.shl(ARG5, x86::cl);
    a.and_(RET, ARG5);

    a.bind(no_masking);
    if (x86::rcx == ctx) {
        a.pop(x86::rcx);
    }

    /* `test rax, rax` is a shorter instruction but can cause a warning
     * in valgrind if there are any uninitialized bits in rax. */
    a.bt(RET, imm(63));
    a.short_().jnc(ascii);

    /* The bs_get_utf8_shared fragment expects the contents in RETd. */
    a.shr(RET, imm(32));
    a.jmp(labels[bs_get_utf8_shared]);

    /* Handle plain old ASCII (code point < 128). */
    a.bind(ascii);
    a.add(x86::qword_ptr(ctx, start_offset), imm(8));
    a.shr(RET, imm(56 - _TAG_IMMED1_SIZE));
    a.or_(RET, imm(_TAG_IMMED1_SMALL)); /* Always clears ZF. */
    a.ret();
}

/*
 * ARG1 = pointer to match state
 * ARG2 = position in binary in bits
 * RETd = 4 bytes read from the binary in big-endian order
 *
 * On successful return, the extracted code point is in RET, the
 * position in the match state has been updated, and the ZF is clear.
 * On failure, the ZF is set.
 */
void BeamGlobalAssembler::emit_bs_get_utf8_shared() {
    Label error = a.newLabel();

    x86::Gp shift_q = ARG4, shift_d = ARG4d, shift_b = ARG4.r8();
    x86::Gp original_value_d = RETd;

    x86::Gp byte_count_q = ARG2, byte_count_d = ARG2d;
    x86::Gp extracted_value_d = ARG3d, extracted_value_b = ARG3.r8();
    x86::Gp control_mask_d = ARG5d;
    x86::Gp error_mask_d = ARG6d;

    ASSERT(extracted_value_d != shift_d);
    ASSERT(control_mask_d != shift_d);
    ASSERT(error_mask_d != shift_d);
    ASSERT(byte_count_d != shift_d);

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

    /* The PEXT instruction has poor latency on some processors, so we try to
     * hide that by extracting early on. Should this be a problem, it's not
     * much slower to hand-roll it with shifts or BEXTR.
     *
     * The mask covers data bits from all variants. This includes the 23rd bit
     * to support the 2-byte case, which is set on all well-formed 4-byte
     * codepoints, so it must be cleared before range testing .*/
    a.mov(extracted_value_d, imm(0x1F3F3F3F));
    a.pext(extracted_value_d, original_value_d, extracted_value_d);

    /* Preserve current match buffer and bit offset. */
    a.push(ARG1);
    a.push(ARG2);

    /* Byte count = leading bit count. */
    a.mov(byte_count_d, original_value_d);
    a.not_(byte_count_d);
    a.lzcnt(byte_count_d, byte_count_d);

    /* Mask shift = (4 - byte count) * 8 */
    a.mov(shift_d, imm(4));
    a.sub(shift_d, byte_count_d);
    a.lea(shift_d, x86::qword_ptr(0, shift_q, 3));

    /* Shift the original value and masks into place. */
    a.shrx(original_value_d, original_value_d, shift_d);

    /* Matches the '10xxxxxx' components, leaving the header byte alone. */
    a.mov(control_mask_d, imm(0x00C0C0C0));
    a.shrx(control_mask_d, control_mask_d, shift_d);
    a.mov(error_mask_d, imm(0x00808080));
    a.shrx(error_mask_d, error_mask_d, shift_d);

    /* Extracted value shift = (4 - byte count) * 6, as the leading '10' on
     * every byte has been removed through PEXT.
     *
     * We calculate the shift here to avoid depending on byte_count_d later on
     * when it may have changed. */
    a.mov(shift_d, imm(4));
    a.sub(shift_d, byte_count_d);
    a.add(shift_d, shift_d);
    a.lea(shift_d, x86::qword_ptr(shift_q, shift_q, 1));

    /* Assert that the header bits of each '10xxxxxx' component is correct,
     * signalling errors by trashing the byte count with a guaranteed-illegal
     * value. */
    a.and_(original_value_d, control_mask_d);
    a.cmp(original_value_d, error_mask_d);
    a.cmovne(byte_count_d, error_mask_d);

    /* Shift the extracted value into place. */
    a.shrx(RETd, extracted_value_d, shift_d);

    /* The extraction mask is a bit too wide, see above for details. */
    a.and_(RETd, imm(~(1 << 22)));

    /* Check for too large code point. */
    a.cmp(RETd, imm(0x10FFFF));
    a.cmova(byte_count_d, error_mask_d);

    /* Check for the illegal range 16#D800 - 16#DFFF. */
    a.mov(shift_d, RETd);
    a.and_(shift_d, imm(-0x800));
    a.cmp(shift_d, imm(0xD800));
    a.cmove(byte_count_d, error_mask_d);

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
    a.lea(shift_d, x86::qword_ptr(0, byte_count_q, 2));
    a.cmp(byte_count_d, imm(4));
    a.setne(extracted_value_b);
    a.sub(shift_b, extracted_value_b);
    a.movzx(shift_q, shift_b);

    /* Now isolate the y bits and compare to zero. */
    a.shrx(extracted_value_d, RETd, shift_d);
    a.test(extracted_value_d, extracted_value_d);
    a.cmove(byte_count_d, error_mask_d);

    /* Restore current bit offset and match buffer. */
    ASSERT(ARG1 != byte_count_q && ARG3 != byte_count_q);
    a.pop(ARG3);
    a.pop(ARG1);

    /* Advance our current position. */
    a.lea(ARG3, x86::qword_ptr(ARG3, byte_count_q, 3));

    /* Byte count must be 2, 3, or 4. */
    a.sub(byte_count_d, imm(2));
    a.cmp(byte_count_d, imm(2));
    a.ja(error);

    a.mov(x86::qword_ptr(ARG1, offsetof(ErlSubBits, start)), ARG3);

    a.shl(RETd, imm(_TAG_IMMED1_SIZE));
    a.or_(RETd, imm(_TAG_IMMED1_SMALL)); /* Always clears ZF. */

    a.ret();

    a.bind(error);
    {
        /* Signal error by setting ZF. */
        a.xor_(RET, RET);
        a.ret();
    }
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgRegister &Ctx,
                                           const ArgLabel &Fail) {
    const int base_offset = offsetof(ErlSubBits, base_flags);
    const int start_offset = offsetof(ErlSubBits, start);
    const int end_offset = offsetof(ErlSubBits, end);

    const x86::Gp ctx = ARG1;
    const x86::Gp bin_position = ARG2;
    const x86::Gp bin_base = ARG3;

    Label multi_byte = a.newLabel(), fallback = a.newLabel(),
          check = a.newLabel(), done = a.newLabel();

    mov_arg(ctx, Ctx);

    a.sub(ctx, imm(TAG_PRIMARY_BOXED));
    a.mov(bin_position, x86::qword_ptr(ctx, start_offset));
    a.mov(RET, x86::qword_ptr(ctx, end_offset));
    a.mov(bin_base, x86::qword_ptr(ctx, base_offset));
    a.and_(bin_base, imm(~ERL_SUB_BITS_FLAG_MASK));
    a.sub(RET, bin_position);
    a.cmp(RET, imm(32));
    a.short_().jb(fallback);

    a.test(bin_position, imm(7));
    a.short_().jnz(fallback);

    /* We're byte-aligned and can read at least 32 bits. */
    a.mov(RET, bin_position);
    a.shr(RET, 3);

    /* The most significant bits come first, so we'll read the the next four
     * bytes as big-endian so we won't have to reorder them later. */
    if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
        a.movbe(RETd, x86::dword_ptr(bin_base, RET));
    } else {
        a.mov(RETd, x86::dword_ptr(bin_base, RET));
        a.bswap(RETd);
    }
    a.test(RETd, RETd);
    a.short_().js(multi_byte);

    /* Handle plain old ASCII (code point < 128). */
    a.add(x86::qword_ptr(ctx, start_offset), imm(8));
    a.shr(RETd, imm(24 - _TAG_IMMED1_SIZE));
    a.or_(RETd, imm(_TAG_IMMED1_SMALL));
    a.short_().jmp(done);

    a.bind(multi_byte);

    if (hasCpuFeature(CpuFeatures::X86::kBMI2)) {
        /* This CPU supports the PEXT and SHRX instructions. */
        safe_fragment_call(ga->get_bs_get_utf8_shared());
        a.short_().jmp(check);
    }

    /* Take care of unaligned binaries and binaries with less than 32
     * bits left. */
    a.bind(fallback);
    if (hasCpuFeature(CpuFeatures::X86::kBMI2)) {
        /* This CPU supports the PEXT and SHRX instructions. */
        safe_fragment_call(ga->get_bs_get_utf8_short_shared());
    } else {
        emit_enter_runtime();

        runtime_call<Eterm (*)(ErlSubBits *), erts_bs_get_utf8>();

        emit_leave_runtime();

        emit_test_the_non_value(RET);
    }

    a.bind(check);
    a.je(resolve_beam_label(Fail));

    a.bind(done);
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgRegister &Ctx,
                                             const ArgLabel &Fail,
                                             const ArgRegister &Dst) {
    emit_bs_get_utf8(Ctx, Fail);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgRegister &Ctx,
                                              const ArgLabel &Fail) {
    emit_bs_get_utf8(Ctx, Fail);
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgRegister &Ctx,
                                            const ArgLabel &Fail,
                                            const ArgWord &Flags) {
    mov_arg(ARG1, Ctx);

    emit_enter_runtime();

    a.sub(ARG1, imm(TAG_PRIMARY_BOXED));
    mov_imm(ARG2, Flags.get());
    runtime_call<Eterm (*)(ErlSubBits *, Uint), erts_bs_get_utf16>();

    emit_leave_runtime();

    emit_test_the_non_value(RET);
    a.je(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgRegister &Ctx,
                                              const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgRegister &Dst) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Flags) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
}

void BeamModuleAssembler::emit_validate_unicode(Label next,
                                                Label fail,
                                                x86::Gp value) {
    a.mov(ARG3d, value.r32());
    a.and_(ARG3d.r8(), imm(_TAG_IMMED1_MASK));
    a.cmp(ARG3d.r8(), imm(_TAG_IMMED1_SMALL));
    a.jne(fail);

    a.cmp(value, imm(make_small(0xD800UL)));
    a.jb(next);
    a.cmp(value, imm(make_small(0xDFFFUL)));
    a.jbe(fail);
    a.cmp(value, imm(make_small(0x10FFFFUL)));
    a.ja(fail);

    a.jmp(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(
        const ArgLabel &Fail,
        const ArgSource &Src,
        const ArgRegister &Ms) {
    Label fail = a.newLabel(), next = a.newLabel();

    mov_arg(ARG1, Src);

    emit_validate_unicode(next, fail, ARG1);

    a.bind(fail);
    {
        mov_arg(ARG1, Ms);

        a.sub(emit_boxed_val(ARG1, offsetof(ErlSubBits, start)), imm(32));

        ASSERT(Fail.get() != 0);
        a.jmp(resolve_beam_label(Fail));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgLabel &Fail,
                                            const ArgRegister &Ctx,
                                            const ArgWord &Unit) {
    unsigned int unit = Unit.get();

    mov_arg(ARG1, Ctx);

    a.mov(RET, emit_boxed_val(ARG1, offsetof(ErlSubBits, end)));
    a.sub(RET, emit_boxed_val(ARG1, offsetof(ErlSubBits, start)));

    if ((unit & (unit - 1))) {
        /* Clobbers ARG3 */
        a.cqo();
        mov_imm(ARG1, unit);
        a.div(ARG1);
        a.test(x86::rdx, x86::rdx);
    } else {
        a.test(RETb, imm(unit - 1));
    }

    a.jnz(resolve_beam_label(Fail));
}

void BeamModuleAssembler::emit_bs_init_writable() {
    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    a.mov(ARG2, getXRef(0));
    runtime_call<Eterm (*)(Process *, Eterm), erts_bs_init_writable>();
    a.mov(getXRef(0), RET);

    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
}

void BeamGlobalAssembler::emit_bs_create_bin_error_shared() {
    emit_enter_runtime<Update::eHeapAlloc>();

    /* ARG3 is already set by the caller */
    a.mov(ARG2, ARG4);
    a.mov(ARG4, ARG1);
    a.mov(ARG1, c_p);
    runtime_call<void (*)(Process *, Uint, Eterm, Eterm),
                 beam_jit_bs_construct_fail_info>();

    emit_leave_runtime<Update::eHeapAlloc>();

    /* We must align the return address to make it a proper tagged CP, in case
     * we were called with `safe_fragment_call`. This is safe because we will
     * never actually return to the return address. */
    a.pop(ARG2);
    a.and_(ARG2, imm(-8));

#ifdef NATIVE_ERLANG_STACK
    a.push(ARG2);

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
#    ifdef ERLANG_FRAME_POINTERS
        a.push(frame_pointer);
#    endif
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
    }
#endif

    mov_imm(ARG4, nullptr);
    a.jmp(labels[raise_exception_shared]);
}

/*
 * ARG1 = tagged bignum term
 *
 * On return, Z is set if ARG1 is not a bignum. Otherwise, Z is clear and
 * ARG1 is the 64 least significant bits of the bignum.
 */
void BeamGlobalAssembler::emit_get_sint64_shared() {
    Label success = a.newLabel();
    Label fail = a.newLabel();

    emit_is_boxed(fail, ARG1);
    x86::Gp boxed_ptr = emit_ptr_val(ARG4, ARG1);
    a.mov(ARG2, emit_boxed_val(boxed_ptr));
    a.mov(ARG3, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.and_(ARG2, imm(_TAG_HEADER_MASK));
    a.cmp(ARG2, imm(POS_BIG_SUBTAG));
    a.je(success);

    a.cmp(ARG2, imm(NEG_BIG_SUBTAG));
    a.jne(fail);

    a.neg(ARG3);

    a.bind(success);
    {
        a.mov(ARG1, ARG3);
        /* Clear Z flag.
         *
         * ARG2 is known to be POS_BIG_SUBTAG or NEG_BIG_SUBTAG at this point.
         */
        ERTS_CT_ASSERT(POS_BIG_SUBTAG != 0 && NEG_BIG_SUBTAG != 0);
        a.test(ARG2, ARG2);
        a.ret();
    }

    a.bind(fail);
    {
        a.xor_(ARG2, ARG2); /* Set Z flag */
        a.ret();
    }
}

struct BscSegment {
    BscSegment()
            : type(am_false), unit(1), flags(0), src(ArgNil()), size(ArgNil()),
              error_info(0), effectiveSize(-1), action(action::DIRECT) {
    }

    Eterm type;
    Uint unit;
    Uint flags;
    ArgVal src;
    ArgVal size;

    Uint error_info;
    Sint effectiveSize;

    /* Here are sub actions for storing integer segments.
     *
     * We use the ACCUMULATE_FIRST and ACCUMULATE actions to shift the
     * values of segments with known, small sizes (no more than 64 bits)
     * into an accumulator register.
     *
     * When no more segments can be accumulated, the STORE action is
     * used to store the value of the accumulator into the binary.
     *
     * The DIRECT action is used when it is not possible to use the
     * accumulator (for unknown or too large sizes).
     */
    enum class action { DIRECT, ACCUMULATE_FIRST, ACCUMULATE, STORE } action;
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
                seg.action = BscSegment::action::ACCUMULATE_FIRST;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            auto prev = segs.back();
            if (prev.flags & BSF_LITTLE) {
                /* Little-endian segments cannot be combined with other
                 * segments. Create new ACCUMULATE_FIRST / STORE actions. */
                seg.action = BscSegment::action::ACCUMULATE_FIRST;
                segs.push_back(seg);
                seg.action = BscSegment::action::STORE;
                segs.push_back(seg);
                continue;
            }

            /* The current segment is compatible with the previous
             * segment. Try combining them. */
            if (prev.effectiveSize + seg.effectiveSize <= 64) {
                /* The combined values of the segments fits in the
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
                seg.action = BscSegment::action::ACCUMULATE_FIRST;
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
    return segs;
}

/*
 * In:
 *    bin_offset = if valid, register to store the lower 32 bits
 *           of the bit offset into the binary
 *    bin_ptr = register to store pointer to current byte in
 *    bit_offset = current bit offset into binary, or -1 if unknown
 *    size = size of segment to be constructed
 *           (ignored if size_reg is valid register)
 *    size_reg = if a valid register, it contains the size of
 *               the segment to be constructed
 *
 * Out:
 *    bin_offset register = the lower 32 bits of the bit offset
 *          into the binary
 *    bin_ptr register = pointer to current byte
 *
 *    Preserves all other registers except RET.
 */
void BeamModuleAssembler::update_bin_state(x86::Gp bin_offset,
                                           x86::Gp current_byte,
                                           Sint bit_offset,
                                           Sint size,
                                           x86::Gp size_reg) {
    const int x_reg_offset = offsetof(ErtsSchedulerRegisters, x_reg_array.d);
    const int cur_bin_base =
            offsetof(ErtsSchedulerRegisters,
                     aux_regs.d.erl_bits_state.erts_current_bin);
    const int cur_bin_offset =
            offsetof(ErtsSchedulerRegisters,
                     aux_regs.d.erl_bits_state.erts_bin_offset);

    x86::Mem mem_bin_base =
            x86::Mem(registers, cur_bin_base - x_reg_offset, sizeof(UWord));
    x86::Mem mem_bin_offset =
            x86::Mem(registers, cur_bin_offset - x_reg_offset, sizeof(UWord));

    if (bit_offset % 8 != 0 || !Support::isInt32(bit_offset + size)) {
        /* The bit offset is unknown or not byte-aligned. Alternatively,
         * the sum of bit_offset and size does not fit in an immediate. */
        a.mov(current_byte, mem_bin_offset);
        a.mov(RET, mem_bin_base);

        if (bin_offset.isValid()) {
            a.mov(bin_offset.r32(), current_byte.r32());
        }
        if (size_reg.isValid()) {
            a.add(mem_bin_offset, size_reg);
        } else {
            a.add(mem_bin_offset, imm(size));
        }
        a.shr(current_byte, imm(3));
        a.add(current_byte, RET);
    } else {
        ASSERT(size >= 0 || size_reg.isValid());
        ASSERT(bit_offset % 8 == 0);

        comment("optimized updating of binary construction state");
        a.mov(current_byte, mem_bin_base);
        if (bit_offset) {
            a.add(current_byte, imm(bit_offset >> 3));
        }
        if (size_reg.isValid()) {
            a.add(mem_bin_offset, size_reg);
        } else {
            a.mov(mem_bin_offset, imm(bit_offset + size));
        }
    }
}

bool BeamModuleAssembler::need_mask(const ArgVal Val, Sint size) {
    if (size == 64) {
        return false;
    } else {
        auto [min, max] = getClampedRange(Val);
        return !(0 <= min && max >> size == 0);
    }
}

/*
 * The size of the segment is assumed to be in ARG3.
 */
void BeamModuleAssembler::set_zero(Sint effectiveSize) {
    update_bin_state(ARG2, ARG1, -1, -1, ARG3);

    mov_imm(RET, 0);

    if (effectiveSize < 0 || effectiveSize > 128) {
        /* Size is unknown or greater than 128. Modern CPUs have an
         * enhanced "rep stosb" instruction that in most circumstances
         * is the fastest way to clear blocks of more than 128
         * bytes. */
        Label done = a.newLabel();

        if (effectiveSize < 0) {
            a.test(ARG3, ARG3);
            a.short_().jz(done);
        }

        if (ARG1 != x86::rdi) {
            a.mov(x86::rdi, ARG1);
        }
        a.mov(x86::rcx, ARG3);
        a.add(x86::rcx, imm(7));
        a.shr(x86::rcx, imm(3));
        a.rep().stosb();

        a.bind(done);
    } else {
        /* The size is known and it is at most 128 bits. */
        Uint offset = 0;

        ASSERT(0 <= effectiveSize && effectiveSize <= 128);

        if (effectiveSize == 128) {
            a.mov(x86::Mem(ARG1, offset, 8), RET);
            offset += 8;
        }

        if (effectiveSize >= 64) {
            a.mov(x86::Mem(ARG1, offset, 8), RET);
            offset += 8;
        }

        if ((effectiveSize & 63) >= 32) {
            a.mov(x86::Mem(ARG1, offset, 4), RETd);
            offset += 4;
        }

        if ((effectiveSize & 31) >= 16) {
            a.mov(x86::Mem(ARG1, offset, 2), RET.r16());
            offset += 2;
        }

        if ((effectiveSize & 15) >= 8) {
            a.mov(x86::Mem(ARG1, offset, 1), RET.r8());
            offset += 1;
        }

        if ((effectiveSize & 7) > 0) {
            a.mov(x86::Mem(ARG1, offset, 1), RET.r8());
        }
    }
}

/*
 * Efficiently accumulate a value for a binary segment,
 * using the smallest possible instructions.
 */
void BeamModuleAssembler::emit_accumulate(ArgVal src,
                                          Sint effectiveSize,
                                          x86::Gp bin_data,
                                          x86::Gp tmp,
                                          x86::Gp value,
                                          bool isFirst) {
    if (isFirst) {
        /* There is no need to mask the first value being
         * accumulated. */
        if (effectiveSize > 32) {
            a.mov(bin_data, value);
        } else {
            a.mov(bin_data.r32(), value.r32());
        }
        return;
    }

    ASSERT(effectiveSize < 64);

    if (!need_mask(src, effectiveSize)) {
        comment("skipped masking because the value always fits");
    } else if (effectiveSize == 32) {
        a.mov(value.r32(), value.r32());
    } else if (effectiveSize == 16) {
        a.movzx(value.r32(), value.r16());
    } else if (effectiveSize == 8) {
        a.movzx(value.r32(), value.r8());
    } else if (effectiveSize < 32) {
        a.and_(value.r32(), (1ULL << effectiveSize) - 1);
    } else {
        mov_imm(tmp, (1ULL << effectiveSize) - 1);
        a.and_(value, tmp);
    }

    a.or_(bin_data, value);
}

/*
 * In:
 *
 *   ARG3 = valid unicode code point (=> 0x80) to encode
 *
 * Out:
 *
 *   ARG3d = the code point encoded in UTF-8.
 *   ARG2 = number of bits of result (16, 24, or 32)
 *
 *   Clobbers RET and the other ARG* registers.
 */
void BeamGlobalAssembler::emit_construct_utf8_shared() {
    Label more_than_two_bytes = a.newLabel();
    Label four_bytes = a.newLabel();
    const x86::Gp tmp1 = ARG1;
    const x86::Gp tmp2 = ARG2;
    const x86::Gp value = ARG3;
    const x86::Gp num_bits = ARG2;

    a.mov(RETd, value.r32());
    a.and_(RETd, imm(0x3f));

    a.cmp(value.r32(), imm(0x800));
    a.jae(more_than_two_bytes);

    a.shl(RETd, imm(8));

    a.shr(value, imm(6));

    a.or_(value.r32(), RETd);
    a.or_(value.r32(), imm(0x80c0));

    mov_imm(num_bits, 16);
    a.ret();

    /* Test whether the value should be encoded in four bytes. */
    a.bind(more_than_two_bytes);
    a.cmp(value.r32(), imm(0x10000));
    a.jae(four_bytes);

    /* Encode Unicode code point in three bytes. */
    a.shl(RETd, imm(16));

    a.lea(tmp1.r32(), x86::Mem(0ULL, ARG3, 2, 0));
    a.and_(tmp1.r32(), imm(0x3f00));

    a.shr(value.r32(), imm(12));
    a.or_(value.r32(), tmp1.r32());
    a.or_(value.r32(), RETd);
    a.or_(value.r32(), imm(0x8080e0));

    mov_imm(num_bits, 24);
    a.ret();

    /* Encode Unicode code point in four bytes. */
    a.bind(four_bytes);
    a.shl(RETd, imm(24));

    a.mov(tmp1.r32(), value.r32());
    a.shl(tmp1.r32(), imm(10));
    a.and_(tmp1.r32(), imm(0x3f0000));

    a.mov(tmp2.r32(), value.r32());
    a.shr(tmp2.r32(), imm(4));
    a.and_(tmp2.r32(), imm(0x3f00));

    a.shr(value.r32(), imm(18));

    a.or_(value.r32(), RETd);
    a.or_(value.r32(), tmp1.r32());
    a.or_(value.r32(), tmp2.r32());
    a.or_(value.r32(), imm(0xffffffff808080f0));

    mov_imm(num_bits, 32);
    a.ret();
}

void BeamModuleAssembler::emit_construct_utf8(const ArgVal &Src,
                                              Sint bit_offset,
                                              bool is_byte_aligned) {
    Label prepare_store = a.newLabel();
    Label store = a.newLabel();
    Label next = a.newLabel();

#ifdef ERTS_JIT_ABI_WIN32
    const x86::Gp bin_ptr = ARG4;
    const x86::Gp bin_offset = is_byte_aligned ? x86::Gp() : ARG1;
#else
    const x86::Gp bin_ptr = ARG1;
    const x86::Gp bin_offset = is_byte_aligned ? x86::Gp() : ARG4;
#endif

    ASSERT(!bin_offset.isValid() || bin_offset == x86::rcx);

    /* The following two registers must be the same as
     * emit_construct_utf8_shared() expects. */
    const x86::Gp code_point = ARG3;
    const x86::Gp size_reg = ARG2;

    comment("construct utf8 segment");

    mov_arg(code_point, Src);
    a.shr(code_point.r32(), imm(_TAG_IMMED1_SIZE));
    mov_imm(size_reg, 8);
    a.cmp(code_point, imm(0x80));
    a.jb(prepare_store);

    safe_fragment_call(ga->get_construct_utf8_shared());

    a.bind(prepare_store);

    update_bin_state(bin_offset, bin_ptr, bit_offset, -1, size_reg);

    if (!is_byte_aligned) {
        /* Bit offset is unknown and is not known to be
         * byte aligned. Must test alignment. */
        a.and_(bin_offset.r32(), imm(7));
        a.je(store);

        /* We must combine the last partial byte with the UTF-8
         * encoded code point. */

        a.movzx(RETd, x86::byte_ptr(bin_ptr));

        a.bswap(code_point);
        a.shr(code_point, bin_offset.r8());
        a.bswap(code_point);

        a.shl(RETd, bin_offset.r8());
        a.and_(RETd, imm(~0xff));
        a.shr(RETd, bin_offset.r8());

        a.or_(code_point, RET);

        a.add(size_reg.r32(), imm(8));
    }

    a.bind(store);
    if (bit_offset % (4 * 8) == 0) {
        /* This segment is aligned on a 4-byte boundary. This implies
         * that a 4-byte write will be inside the allocated binary. */
        a.mov(x86::dword_ptr(bin_ptr), code_point.r32());
    } else {
        Label do_store_1 = a.newLabel();
        Label do_store_2 = a.newLabel();

        /* Unsuitable or unknown alignment. We must be careful not
         * to write beyound the allocated end of the binary. */
        a.cmp(size_reg.r8(), imm(8));
        a.short_().jne(do_store_1);

        a.mov(x86::byte_ptr(bin_ptr), code_point.r8());
        a.short_().jmp(next);

        a.bind(do_store_1);
        a.cmp(size_reg.r8(), imm(24));
        a.ja(do_store_2);

        a.mov(x86::word_ptr(bin_ptr), code_point.r16());
        a.cmp(size_reg.r8(), imm(16));
        a.short_().je(next);

        a.shr(code_point.r32(), imm(16));
        a.mov(x86::byte_ptr(bin_ptr, 2), code_point.r8());
        a.short_().jmp(next);

        a.bind(do_store_2);
        a.mov(x86::dword_ptr(bin_ptr), code_point.r32());

        if (!is_byte_aligned) {
            a.cmp(size_reg.r8(), imm(32));
            a.je(next);

            a.shr(code_point, imm(32));
            a.mov(x86::byte_ptr(bin_ptr, 4), code_point.r8());
        }
    }

    a.bind(next);
}
/*
 * In:
 *   ARG1 = pointer to current byte
 *   ARG3 = bit offset
 *   ARG4 = number of bits to write
 *   ARG5 = data to write
 */
void BeamGlobalAssembler::emit_store_unaligned() {
    Label loop = a.newLabel();
    Label done = a.newLabel();
    const x86::Gp bin_ptr = ARG1;
    const x86::Gp left_bit_offset = ARG3;
    const x86::Gp right_bit_offset = ARG2;
    const x86::Gp num_bits = ARG4;
    const x86::Gp bitdata = ARG5;

    a.movzx(RETd, x86::byte_ptr(bin_ptr));

    a.xchg(left_bit_offset, x86::rcx);

    a.mov(right_bit_offset, bitdata);
    a.and_(right_bit_offset, imm(0xff));
    a.shr(right_bit_offset, x86::cl);

    a.shl(RETd, x86::cl);
    a.and_(RETd, imm(~0xff));
    a.shr(RETd, x86::cl);

    a.xchg(left_bit_offset, x86::rcx);

    a.or_(RETd, ARG2d);
    a.mov(byte_ptr(ARG1), RETb);
    a.add(ARG1, imm(1));

    mov_imm(right_bit_offset, 8);
    a.sub(right_bit_offset, left_bit_offset);

    a.xchg(right_bit_offset, x86::rcx);
    a.bswap(bitdata);
    a.shl(bitdata, x86::cl);
    a.xchg(right_bit_offset, x86::rcx);

    a.sub(ARG4, right_bit_offset);
    a.jle(done);

    a.bind(loop);
    a.rol(bitdata, imm(8));
    a.mov(byte_ptr(ARG1), bitdata.r8());
    a.add(ARG1, imm(1));
    a.sub(num_bits, imm(8));
    a.jg(loop);

    a.bind(done);
    a.ret();
}

bool BeamModuleAssembler::bs_maybe_enter_runtime(bool entered) {
    if (!entered) {
        comment("enter runtime");
        emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();
    }
    return true;
}

void BeamModuleAssembler::bs_maybe_leave_runtime(bool entered) {
    if (entered) {
        comment("leave runtime");
        emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
    }
}

/*
 * In:
 *   ARG4 = Size of binary in bits.
 *   ARG5 = Extra words to allocate.
 *   ARG6 = Number of live X registers.
 *
 * Out:
 *   RET = Allocated binary object.
 */

void BeamGlobalAssembler::emit_bs_init_bits_shared() {
    Label exiting = a.newLabel();

    load_erl_bits_state(ARG3);
    load_x_reg_array(ARG2);
    a.mov(ARG1, c_p);

    /* Because bs_create_bin() has already entered runtime mode and
     * called this fragment, the stack is now unaligned. We must take
     * care to align it before calling anything. */
    a.push(x86::rbp);
    a.mov(x86::rbp, x86::rsp);
    runtime_call<Eterm (*)(Process *,
                           Eterm *,
                           struct erl_bits_state *,
                           Uint,
                           Uint,
                           unsigned),
                 beam_jit_bs_init_bits>();
    a.leave();

#ifdef WIN32
    a.mov(ARG1d, x86::dword_ptr(c_p, offsetof(Process, state.value)));
#else
    a.mov(ARG1d, x86::dword_ptr(c_p, offsetof(Process, state.counter)));
#endif
    a.test(ARG1d, imm(ERTS_PSFLG_EXITING));
    a.short_().jne(exiting);

    a.ret();

    a.bind(exiting);
    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
    a.jmp(labels[do_schedule]);
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
    ArgWord Live = Live0;
    x86::Gp sizeReg;
    Sint allocated_size = -1;
    bool need_error_handler = false;
    bool runtime_entered = false;

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
         * As soon as we have entered runtime mode, Y registers can no
         * longer be accessed in the usual way. Therefore, if the source
         * and/or size are in Y registers, copy them to X registers. Be
         * careful to preserve any associated type information.
         */
        if (seg.src.isYRegister()) {
            auto reg =
                    seg.src.as<ArgYRegister>().copy<ArgXRegister>(Live.get());
            ASSERT(reg.typeIndex() == seg.src.as<ArgYRegister>().typeIndex());
            mov_arg(reg, seg.src);

            Live = Live + 1;
            seg.src = reg;
        }

        if (seg.size.isYRegister()) {
            auto reg =
                    seg.size.as<ArgYRegister>().copy<ArgXRegister>(Live.get());
            ASSERT(reg.typeIndex() == seg.size.as<ArgYRegister>().typeIndex());
            mov_arg(reg, seg.size);

            Live = Live + 1;
            seg.size = reg;
        }

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
            /* We need a callee-save register for the size. We'll pick the
             * active code index register because it's not used in any capacity
             * here. Note that we have to spill it since `save_calls` may be
             * enabled and we'll lose that information if we blindly re-read
             * the index. */
            sizeReg = active_code_ix;
            need_error_handler = true;
        }

        segments.insert(segments.end(), seg);
    }

    /*
     * Test whether a heap binary of fixed size will result from the
     * construction. If so, allocate and construct the binary now
     * before entering the runtime mode.
     */
    if (!sizeReg.isValid() && num_bits <= ERL_ONHEAP_BITS_LIMIT &&
        segments[0].type != am_append &&
        segments[0].type != am_private_append) {
        const int x_reg_offset =
                offsetof(ErtsSchedulerRegisters, x_reg_array.d);
        const int cur_bin_base =
                offsetof(ErtsSchedulerRegisters,
                         aux_regs.d.erl_bits_state.erts_current_bin);
        const int cur_bin_offset =
                offsetof(ErtsSchedulerRegisters,
                         aux_regs.d.erl_bits_state.erts_bin_offset);
        x86::Mem mem_bin_base =
                x86::qword_ptr(registers, cur_bin_base - x_reg_offset);
        x86::Mem mem_bin_offset =
                x86::qword_ptr(registers, cur_bin_offset - x_reg_offset);
        Uint heap_size = heap_bits_size(num_bits);

        comment("allocate heap bitstring");
        allocated_size = (heap_size - heap_bits_size(0)) * sizeof(Eterm);

        /* Ensure that there is enough room on the heap. */
        emit_gc_test(ArgWord(0), ArgWord(heap_size + Alloc.get()), Live);

        /* Create the heap binary. */
        a.lea(RET, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
        a.mov(TMP_MEM1q, RET);
        a.mov(x86::qword_ptr(HTOP, 0), imm(header_heap_bits(num_bits)));
        a.mov(x86::qword_ptr(HTOP, sizeof(Eterm)), imm(num_bits));

        /* Initialize the erl_bin_state struct. */
        a.add(HTOP, imm(sizeof(Eterm[2])));
        a.mov(mem_bin_base, HTOP);
        a.mov(mem_bin_offset, imm(0));

        /* Update HTOP. */
        a.add(HTOP, imm(allocated_size));
    }

    if (!need_error_handler) {
        comment("(cannot fail)");
    } else {
        Label past_error = a.newLabel();

        runtime_entered = bs_maybe_enter_runtime(false);
        a.short_().jmp(past_error);

        /*
         * ARG1 = optional bad size value; valid if BSC_VALUE_ARG1 is set in
         * ARG4
         *
         * ARG3 = optional bad size value; valid if BSC_VALUE_ARG3 is set
         * in ARG4
         *
         * ARG4 = packed error information
         */
        error = a.newLabel();
        a.bind(error);
        bs_maybe_leave_runtime(runtime_entered);

        if (sizeReg.isValid()) {
            a.mov(sizeReg, TMP_MEM5q);
        }

        comment("handle error");
        if (Fail.get() != 0) {
            a.jmp(resolve_beam_label(Fail));
        } else {
            safe_fragment_call(ga->get_bs_create_bin_error_shared());
        }

        a.bind(past_error);
    }

    /* We count the total number of bits in an unsigned integer. To
     * avoid having to check for overflow when adding to the counter,
     * we ensure that the signed size of each segment fits in a
     * word. */
    if (sizeReg.isValid()) {
        comment("calculate sizes");
        a.mov(TMP_MEM5q, sizeReg);
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
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
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

                emit_is_boxed(error, seg.src, ARG1);
            }

            Label next = a.newLabel(), not_sub_bits = a.newLabel();

            x86::Gp boxed_ptr = emit_ptr_val(ARG3, ARG1);
            a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));

            /* Speculatively load heap bits size, this is safe since all boxed
             * terms have at least one data word. */
            ERTS_CT_ASSERT(offsetof(ErlHeapBits, size) == sizeof(Eterm));
            a.mov(ARG2d,
                  emit_boxed_val(boxed_ptr,
                                 offsetof(ErlHeapBits, size),
                                 sizeof(Uint32)));

            a.cmp(RETd, imm(HEADER_SUB_BITS));
            a.short_().jne(not_sub_bits);
            {
                a.mov(ARG2,
                      emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, end)));
                a.sub(ARG2,
                      emit_boxed_val(boxed_ptr, offsetof(ErlSubBits, start)));

                if (masked_types<BeamTypeId::MaybeBoxed>(seg.src) ==
                    BeamTypeId::Bitstring) {
                    comment("optimized size code because the value is always "
                            "a bitstring when boxed");
                } else {
                    a.short_().jmp(next);
                }
            }
            a.bind(not_sub_bits);

            if (masked_types<BeamTypeId::MaybeBoxed>(seg.src) !=
                BeamTypeId::Bitstring) {
                a.and_(RETb, imm(_BITSTRING_TAG_MASK));
                a.cmp(RETb, imm(_TAG_HEADER_HEAP_BITS));
                a.jne(error);
            }

            a.bind(next);
            a.add(sizeReg, ARG2);
        } else if (seg.unit != 0) {
            bool can_fail = true;
            comment("size binary/integer/float/string");

            if (always_small(seg.size)) {
                auto min = std::get<0>(getClampedRange(seg.size));
                if (min >= 0) {
                    can_fail = false;
                }
            }

            if (can_fail && Fail.get() == 0) {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_DEPENDS,
                                                        BSC_INFO_SIZE,
                                                        BSC_VALUE_ARG1));
            }

            mov_arg(ARG1, seg.size);

            if (always_small(seg.size)) {
                comment("skipped test for small size since it is always small");
            } else if (always_one_of<BeamTypeId::Number>(seg.size)) {
                comment("simplified test for small size since it is a number");
                a.test(ARG1.r8(), imm(TAG_PRIMARY_LIST));
                a.je(error);
            } else {
                a.mov(RETd, ARG1d);
                a.and_(RETb, imm(_TAG_IMMED1_MASK));
                a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
                a.jne(error);
            }

            a.mov(RET, ARG1);
            a.sar(RET, imm(_TAG_IMMED1_SIZE));
            if (can_fail) {
                a.js(error);
            }
            if (seg.unit != 1) {
                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(
                                    seg.error_info,
                                    BSC_REASON_SYSTEM_LIMIT,
                                    BSC_INFO_SIZE,
                                    BSC_VALUE_ARG1));
                }
                a.imul(RET, RET, imm(seg.unit));
                a.jo(error);
            }
            a.add(sizeReg, RET);
        } else {
            switch (seg.type) {
            case am_utf8: {
                Label next = a.newLabel();

                comment("size utf8");
                mov_arg(ARG1, seg.src);

                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                }

                if (always_small(seg.src)) {
                    comment("skipped test for small value since it is always "
                            "small");
                } else if (always_one_of<BeamTypeId::Integer,
                                         BeamTypeId::AlwaysBoxed>(seg.src)) {
                    comment("simplified test for small operand since other "
                            "types are boxed");
                    emit_is_not_boxed(error, ARG1);
                } else {
                    a.mov(RETd, ARG1d);
                    a.and_(RETb, imm(_TAG_IMMED1_MASK));
                    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
                    a.jne(error);
                }

                mov_imm(RET, 0);
                a.mov(RETb, imm(1));
                a.cmp(ARG1, imm(make_small(0x80UL)));
                a.short_().jb(next);

                a.mov(RETb, imm(2));
                a.cmp(ARG1, imm(make_small(0x800UL)));
                a.short_().jb(next);

                /* Ensure that the value is not in the invalid range
                 * 0xD800 through 0xDFFF. */
                a.mov(ARG2, ARG1);
                a.sar(ARG2, imm(11 + _TAG_IMMED1_SIZE));
                a.cmp(ARG2, imm(0x1b));
                a.je(error);

                a.cmp(ARG1, imm(make_small(0x10000UL)));
                a.setae(RETb);
                a.add(RETb, imm(3));

                auto [min, max] = getClampedRange(seg.src);
                if (0 <= min && max < 0x110000) {
                    comment("skipped range check for unicode code point");
                } else {
                    a.cmp(ARG1, imm(make_small(0x110000)));
                    a.jae(error);
                }

                a.bind(next);
                a.lea(sizeReg, x86::Mem(sizeReg, RET, 3, 0, 1));
                break;
            }
            case am_utf16: {
                mov_arg(ARG1, seg.src);
                mov_imm(RET, 2 * 8);
                mov_imm(ARG2, 4 * 8);
                a.cmp(ARG1, imm(make_small(0x10000UL)));
                a.cmovae(RET, ARG2);
                a.add(sizeReg, RET);
                break;
            }
            case am_utf32: {
                Label next = a.newLabel();

                mov_arg(ARG1, seg.src);

                if (Fail.get() == 0) {
                    mov_imm(ARG4,
                            beam_jit_update_bsc_reason_info(seg.error_info,
                                                            BSC_REASON_BADARG,
                                                            BSC_INFO_TYPE,
                                                            BSC_VALUE_ARG1));
                }

                a.add(sizeReg, imm(4 * 8));

                a.mov(RETd, ARG1d);
                a.and_(RETb, imm(_TAG_IMMED1_MASK));
                a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
                a.jne(error);

                a.cmp(ARG1, imm(make_small(0xD800UL)));
                a.short_().jb(next);
                a.cmp(ARG1, imm(make_small(0xDFFFUL)));
                a.jbe(error);
                a.cmp(ARG1, imm(make_small(0x10FFFFUL)));
                a.ja(error);

                a.bind(next);
                break;
            }
            default:
                ASSERT(0);
            }
        }
    }

    segments = bs_combine_segments(segments);

    /* Allocate the binary. */
    if (segments[0].type == am_append) {
        Label all_good = a.newLabel();
        Label schedule = resolve_fragment(ga->get_do_schedule());

        BscSegment seg = segments[0];
        runtime_entered = bs_maybe_enter_runtime(runtime_entered);
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
        runtime_call<Eterm (*)(Process *, Eterm *, Uint, Uint, Uint, Uint),
                     erts_bs_append_checked>();

        if (exact_type<BeamTypeId::Bitstring>(seg.src) &&
            std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit) {
            /* There is no way the call can fail with a system_limit
             * exception on a 64-bit architecture. However, it can
             * fail because the max_heap_size limit has been
             * exceeded. */
            comment("skipped test for success because units are compatible");
            emit_test_the_non_value(RET);
            a.short_().jne(all_good);

            bs_maybe_leave_runtime(runtime_entered);
            a.jmp(schedule);
        } else {
            emit_test_the_non_value(RET);
            a.short_().jne(all_good);

            if (Fail.get() == 0) {
                mov_arg(ARG1, ArgXRegister(Live.get()));
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_FVALUE,
                                                        BSC_VALUE_ARG1));
            }
#ifdef WIN32
            a.mov(ARG2d, x86::dword_ptr(c_p, offsetof(Process, state.value)));
#else
            a.mov(ARG2d, x86::dword_ptr(c_p, offsetof(Process, state.counter)));
#endif
            a.test(ARG2d, imm(ERTS_PSFLG_EXITING));
            a.je(error);

            bs_maybe_leave_runtime(runtime_entered);
            a.jmp(schedule);
        }

        a.bind(all_good);
        a.mov(TMP_MEM1q, RET);
    } else if (segments[0].type == am_private_append) {
        BscSegment seg = segments[0];
        runtime_entered = bs_maybe_enter_runtime(runtime_entered);
        comment("private append to binary");
        ASSERT(Alloc.get() == 0);
        mov_arg(ARG3, seg.src);
        if (sizeReg.isValid()) {
            a.mov(ARG4, sizeReg);
        } else {
            mov_imm(ARG4, num_bits);
        }
        a.mov(ARG2, c_p);
        load_erl_bits_state(ARG1);
        runtime_call<Eterm (*)(ErlBitsState *, Process *, Eterm, Uint),
                     erts_bs_private_append_checked>();
        /* There is no way the call can fail on a 64-bit architecture. */
        a.mov(TMP_MEM1q, RET);
    } else if (allocated_size >= 0) {
        /* The binary has already been allocated. */
    } else {
        comment("allocate binary");
        runtime_entered = bs_maybe_enter_runtime(runtime_entered);

        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);

        if (sizeReg.isValid()) {
            a.mov(ARG4, sizeReg);
        } else {
            /* Small static bitstrings should have been heap-allocated above. */
            ASSERT(num_bits > ERL_ONHEAP_BITS_LIMIT);
            mov_imm(ARG4, num_bits);
        }
        fragment_call(ga->get_bs_init_bits_shared());
        a.mov(TMP_MEM1q, RET);
    }

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

            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            comment("construct a binary segment");
            if (seg.effectiveSize >= 0) {
                /* The segment has a literal size. */
                mov_imm(ARG4, seg.effectiveSize);
                mov_arg(ARG3, seg.src);
                a.mov(ARG2, c_p);
                load_erl_bits_state(ARG1);
                runtime_call<int (*)(ErlBitsState *, Process *, Eterm, Uint),
                             erts_bs_put_binary>();
                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_DEPENDS,
                                                             BSC_VALUE_FVALUE);
            } else if (seg.size.isAtom() &&
                       seg.size.as<ArgAtom>().get() == am_all) {
                /* Include the entire binary/bitstring in the
                 * resulting binary. */
                can_fail =
                        !(exact_type<BeamTypeId::Bitstring>(seg.src) &&
                          std::gcd(seg.unit, getSizeUnit(seg.src)) == seg.unit);

                if (can_fail) {
                    mov_imm(ARG4, seg.unit);
                }
                mov_arg(ARG3, seg.src);
                a.mov(ARG2, c_p);
                load_erl_bits_state(ARG1);
                if (can_fail) {
                    runtime_call<
                            int (*)(ErlBitsState *, Process *, Eterm, Uint),
                            erts_bs_put_binary_all>();
                } else {
                    runtime_call<void (*)(ErlBitsState *, Process *, Eterm),
                                 beam_jit_bs_put_binary_all>();
                }
                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_UNIT,
                                                             BSC_VALUE_FVALUE);
            } else {
                /* The size is a variable. We have verified that
                 * the value is a non-negative small in the
                 * appropriate range. Multiply the size with the
                 * unit. */
                mov_arg(ARG4, seg.size);
                a.sar(ARG4, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(RET, seg.unit);
                    a.mul(ARG4); /* CLOBBERS RDX = ARG3! */
                    a.mov(ARG4, RET);
                }
                mov_arg(ARG3, seg.src);
                a.mov(ARG2, c_p);
                load_erl_bits_state(ARG1);
                runtime_call<int (*)(ErlBitsState *, Process *, Eterm, Uint),
                             erts_bs_put_binary>();
                error_info = beam_jit_update_bsc_reason_info(seg.error_info,
                                                             BSC_REASON_BADARG,
                                                             BSC_INFO_DEPENDS,
                                                             BSC_VALUE_FVALUE);
            }

            if (!can_fail) {
                comment("skipped test for success because units are "
                        "compatible");
            } else {
                if (Fail.get() == 0) {
                    mov_imm(ARG4, error_info);
                }
                a.test(RETd, RETd);
                a.je(error);
            }
            break;
        }
        case am_float:
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            comment("construct float segment");
            if (seg.effectiveSize >= 0) {
                mov_imm(ARG4, seg.effectiveSize);
            } else {
                mov_arg(ARG4, seg.size);
                a.sar(ARG4, imm(_TAG_IMMED1_SIZE));
                if (seg.unit != 1) {
                    mov_imm(RET, seg.unit);
                    a.mul(ARG4); /* CLOBBERS RDX = ARG3! */
                    a.mov(ARG4, RET);
                }
            }
            mov_arg(ARG3, seg.src);
            mov_imm(ARG5, seg.flags);
            a.mov(ARG2, c_p);
            load_erl_bits_state(ARG1);
            runtime_call<Eterm (*)(ErlBitsState *, Process *, Eterm, Uint, int),
                         erts_bs_put_float>();
            if (Fail.get() == 0) {
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_FVALUE,
                                                        BSC_VALUE_ARG1));
            }
            a.mov(ARG1, RET);
            emit_test_the_non_value(RET);
            a.jne(error);
            break;
        case am_integer:
            switch (seg.action) {
            case BscSegment::action::ACCUMULATE_FIRST:
            case BscSegment::action::ACCUMULATE: {
                /* Shift an integer of known size (no more than 64 bits)
                 * into a word-size accumulator. */
                Label accumulate = a.newLabel();
                Label value_is_small = a.newLabel();
                x86::Gp tmp = ARG4;
                x86::Gp bin_data = ARG5;

                comment("accumulate value for integer segment");
                if (seg.action != BscSegment::action::ACCUMULATE_FIRST &&
                    seg.effectiveSize < 64) {
                    a.shl(bin_data, imm(seg.effectiveSize));
                }
                if (!seg.src.isSmall()) {
                    mov_arg(ARG1, seg.src);
                }

                if (!always_small(seg.src)) {
                    if (always_one_of<BeamTypeId::Integer,
                                      BeamTypeId::AlwaysBoxed>(seg.src)) {
                        comment("simplified small test since all other types "
                                "are boxed");
                        emit_is_boxed(value_is_small, seg.src, ARG1);
                    } else {
                        a.mov(ARG2d, ARG1d);
                        a.and_(ARG2d, imm(_TAG_IMMED1_MASK));
                        a.cmp(ARG2d, imm(_TAG_IMMED1_SMALL));
                        a.short_().je(value_is_small);
                    }

                    /* The value is boxed. If it is a bignum, extract the
                     * least significant 64 bits. */
                    safe_fragment_call(ga->get_get_sint64_shared());
                    if (exact_type<BeamTypeId::Integer>(seg.src)) {
                        a.short_().jmp(accumulate);
                    } else {
                        a.short_().jne(accumulate);

                        /* Not a bignum. Signal error. */
                        if (Fail.get() == 0) {
                            mov_imm(ARG4,
                                    beam_jit_update_bsc_reason_info(
                                            seg.error_info,
                                            BSC_REASON_BADARG,
                                            BSC_INFO_TYPE,
                                            BSC_VALUE_ARG1));
                        }
                        a.jmp(error);
                    }
                }

                a.bind(value_is_small);
                if (seg.src.isSmall()) {
                    Sint val = signed_val(seg.src.as<ArgSmall>().get());
                    mov_imm(ARG1, val);
                } else if (seg.effectiveSize + _TAG_IMMED1_SIZE <= 32) {
                    a.shr(ARG1d, imm(_TAG_IMMED1_SIZE));
                } else {
                    a.sar(ARG1, imm(_TAG_IMMED1_SIZE));
                }

                /* Mask (if needed) and accumulate. */
                a.bind(accumulate);
                emit_accumulate(seg.src,
                                seg.effectiveSize,
                                bin_data,
                                tmp,
                                ARG1,
                                seg.action ==
                                        BscSegment::action::ACCUMULATE_FIRST);
                break;
            }
            case BscSegment::action::STORE: {
                /* The accumulator is now full or the next segment is
                 * not possible to accumulate, so it's time to store
                 * the accumulator to the current position in the
                 * binary. */
                Label store = a.newLabel();
                Label done = a.newLabel();
                x86::Gp bin_ptr = ARG1;
                x86::Gp bin_offset = ARG3;
                x86::Gp tmp = ARG4;
                x86::Gp bin_data = ARG5;

                comment("construct integer segment from accumulator");

                /* First we'll need to ensure that the value in the
                 * accumulator is in little endian format. */
                if (seg.effectiveSize % 8 != 0) {
                    Uint complete_bytes = 8 * (seg.effectiveSize / 8);
                    Uint num_partial = seg.effectiveSize % 8;
                    if ((seg.flags & BSF_LITTLE) == 0) {
                        a.shl(bin_data, imm(64 - seg.effectiveSize));
                        a.bswap(bin_data);
                    } else {
                        Sint mask = (1ll << complete_bytes) - 1;
                        a.mov(RET, bin_data);
                        a.shr(RET, imm(complete_bytes));
                        a.and_(RETd, imm((1ull << num_partial) - 1));
                        a.shl(RET, imm(complete_bytes + 8 - num_partial));
                        if (Support::isInt32(mask)) {
                            a.and_(bin_data, imm(mask));
                        } else {
                            mov_imm(tmp, mask);
                            a.and_(bin_data, tmp);
                        }
                        a.or_(bin_data, RET);
                    }
                } else if ((seg.flags & BSF_LITTLE) == 0) {
                    switch (seg.effectiveSize) {
                    case 8:
                        break;
                    case 32:
                        a.bswap(bin_data.r32());
                        break;
                    case 64:
                        a.bswap(bin_data);
                        break;
                    default:
                        a.bswap(bin_data);
                        a.shr(bin_data, imm(64 - seg.effectiveSize));
                        break;
                    }
                }

                update_bin_state(bin_offset,
                                 bin_ptr,
                                 bit_offset,
                                 seg.effectiveSize,
                                 x86::Gp());

                if (!is_byte_aligned) {
                    if (bit_offset < 0) {
                        /* Bit offset is unknown. Must test alignment. */
                        a.and_(bin_offset, imm(7));
                        a.short_().je(store);
                    } else if (bit_offset >= 0) {
                        /* Alignment is known to be unaligned. */
                        mov_imm(bin_offset, bit_offset & 7);
                    }

                    /* Bit offset is tested or known to be unaligned. */
                    mov_imm(ARG4, seg.effectiveSize);
                    safe_fragment_call(ga->get_store_unaligned());

                    if (bit_offset < 0) {
                        /* The bit offset is unknown, which implies
                         * that there exists store code that we will
                         * need to branch past. */
                        a.short_().jmp(done);
                    }
                }

                a.bind(store);

                if (bit_offset <= 0 || is_byte_aligned) {
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
                            a.mov(x86::Mem(bin_ptr, 0, 1), bin_data.r8());
                            break;
                        case 2:
                            a.mov(x86::Mem(bin_ptr, 0, 2), bin_data.r16());
                            break;
                        case 3:
                            a.mov(x86::Mem(bin_ptr, 0, 2), bin_data.r16());
                            a.shr(bin_data, imm(16));
                            a.mov(x86::Mem(bin_ptr, 2, 1), bin_data.r8());
                            break;
                        case 4:
                            a.mov(x86::Mem(bin_ptr, 0, 4), bin_data.r32());
                            break;
                        case 5:
                        case 6:
                        case 7:
                            a.mov(x86::Mem(bin_ptr, 0, 4), bin_data.r32());
                            a.add(bin_ptr, imm(4));
                            a.shr(bin_data, imm(32));
                            break;
                        case 8:
                            a.mov(x86::Mem(bin_ptr, 0, 8), bin_data);
                            num_bytes = 0;
                            break;
                        default:
                            ASSERT(0);
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
                    mov_arg(ARG3, seg.size);
                    a.sar(ARG3, imm(_TAG_IMMED1_SIZE));
                    if (Support::isPowerOf2(seg.unit)) {
                        Uint trailing_bits = Support::ctz<Eterm>(seg.unit);
                        if (trailing_bits) {
                            a.shl(ARG3, imm(trailing_bits));
                        }
                    } else {
                        mov_imm(RET, seg.unit);
                        a.mul(ARG3); /* CLOBBERS RDX = ARG3! */
                        a.mov(ARG3, RET);
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
                    runtime_entered = bs_maybe_enter_runtime(runtime_entered);
                    mov_arg(ARG2, seg.src);
                    load_erl_bits_state(ARG1);
                    if (seg.flags & BSF_LITTLE) {
                        runtime_call<int (*)(ErlBitsState *, Eterm, Uint),
                                     erts_bs_put_integer_le>();
                    } else {
                        runtime_call<int (*)(ErlBitsState *, Eterm, Uint),
                                     erts_bs_put_integer_be>();
                    }
                    if (exact_type<BeamTypeId::Integer>(seg.src)) {
                        comment("skipped test for success because construction "
                                "can't fail");
                    } else {
                        if (Fail.get() == 0) {
                            mov_arg(ARG1, seg.src);
                            mov_imm(ARG4,
                                    beam_jit_update_bsc_reason_info(
                                            seg.error_info,
                                            BSC_REASON_BADARG,
                                            BSC_INFO_TYPE,
                                            BSC_VALUE_ARG1));
                        }
                        a.test(RETd, RETd);
                        a.je(error);
                    }
                }
                break;
            }
            break;
        case am_string: {
            ArgBytePtr string_ptr(
                    ArgVal(ArgVal::Type::BytePtr, seg.src.as<ArgWord>().get()));

            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            comment("insert string");
            ASSERT(seg.effectiveSize >= 0);
            mov_imm(ARG3, seg.effectiveSize / 8);
            mov_arg(ARG2, string_ptr);
            load_erl_bits_state(ARG1);
            runtime_call<void (*)(ErlBitsState *, byte *, Uint),
                         erts_bs_put_string>();
        } break;
        case am_utf8: {
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            emit_construct_utf8(seg.src, bit_offset, is_byte_aligned);
            break;
        }
        case am_utf16:
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            mov_arg(ARG2, seg.src);
            a.mov(ARG3, seg.flags);
            load_erl_bits_state(ARG1);
            runtime_call<int (*)(struct erl_bits_state *, Eterm, Uint),
                         erts_bs_put_utf16>();
            if (Fail.get() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
            }
            a.test(RETd, RETd);
            a.je(error);
            break;
        case am_utf32:
            runtime_entered = bs_maybe_enter_runtime(runtime_entered);
            mov_arg(ARG2, seg.src);
            mov_imm(ARG3, 4 * 8);
            load_erl_bits_state(ARG1);
            if (seg.flags & BSF_LITTLE) {
                runtime_call<int (*)(ErlBitsState *, Eterm, Uint),
                             erts_bs_put_integer_le>();
            } else {
                runtime_call<int (*)(ErlBitsState *, Eterm, Uint),
                             erts_bs_put_integer_be>();
            }
            if (Fail.get() == 0) {
                mov_arg(ARG1, seg.src);
                mov_imm(ARG4,
                        beam_jit_update_bsc_reason_info(seg.error_info,
                                                        BSC_REASON_BADARG,
                                                        BSC_INFO_TYPE,
                                                        BSC_VALUE_ARG1));
            }
            a.test(RETd, RETd);
            a.je(error);
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

    bs_maybe_leave_runtime(runtime_entered);

    if (sizeReg.isValid()) {
        a.mov(sizeReg, TMP_MEM5q);
    }

    comment("done");
    a.mov(RET, TMP_MEM1q);
    mov_arg(Dst, RET);
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
        READ_INTEGER,
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
                                         const x86::Gp bin_base,
                                         const x86::Gp bin_offset,
                                         const x86::Gp bitdata) {
    Label read_done = a.newLabel();
    auto num_partial = bits % 8;
    auto num_bytes_to_read = (bits + 7) / 8;

    ASSERT(bin_offset == x86::rcx);

    a.mov(RET, bin_offset);
    a.shr(RET, imm(3));
    if (num_bytes_to_read != 1) {
        a.add(bin_base, RET);
    }
    a.and_(bin_offset.r32(), imm(7));

    /*
     * Special-case handling of reading 8 or 9 bytes.
     */
    if (num_bytes_to_read == 8) {
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
            a.movbe(bitdata, x86::qword_ptr(bin_base, num_bytes_to_read - 8));
        } else {
            a.mov(bitdata, x86::qword_ptr(bin_base, num_bytes_to_read - 8));
            a.bswap(bitdata);
        }

        a.shl(bitdata, bin_offset.r8());

        a.test(x86::cl, imm(7));
        if (num_partial == 0) {
            /* Byte-sized segment. If bit_offset is not byte-aligned, this
             * segment always needs an additional byte. */
            a.jz(read_done);
        } else if (num_partial > 1) {
            /* Non-byte-sized segment. Test whether we will need an
             * additional byte. */
            a.cmp(bin_offset.r32(), imm(8 - num_partial));
            a.jle(read_done);
        }

        if (num_partial != 1) {
            /* Read an extra byte. */
            a.movzx(RETd, x86::byte_ptr(bin_base, 8));
            a.shl(RETd, bin_offset.r8());
            a.shr(RETd, imm(8));
            a.or_(bitdata, RET);
        }

        a.bind(read_done);

        return;
    }

    /*
     * Handle reading of up to 7 bytes.
     */
    Label handle_partial = a.newLabel();
    Label swap = a.newLabel();
    Label shift = a.newLabel();

    if (num_partial == 0) {
        /* Byte-sized segment. If bit_offset is not byte-aligned, this
         * segment always needs an additional byte. */
        a.jnz(handle_partial);
    } else if (num_partial > 1) {
        /* Non-byte-sized segment. Test whether we will need an
         * additional byte. */
        a.cmp(bin_offset.r32(), imm(8 - num_partial));
        a.jg(handle_partial);
    }

    /* We don't need an extra byte. */
    if (num_bytes_to_read == 1) {
        a.movzx(bitdata.r32(), x86::byte_ptr(bin_base, RET));
        if (num_partial == 0) {
            a.bswap(bitdata);
            a.short_().jmp(read_done);
        } else if (num_partial > 1) {
            a.short_().jmp(swap);
        }
    } else if (num_bytes_to_read <= 4) {
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
            a.movbe(bitdata.r32(),
                    x86::dword_ptr(bin_base, num_bytes_to_read - 4));
        } else {
            a.mov(bitdata.r32(),
                  x86::dword_ptr(bin_base, num_bytes_to_read - 4));
            a.bswap(bitdata.r32());
        }
        a.add(bin_offset.r32(), imm(64 - 8 * num_bytes_to_read));
        a.short_().jmp(shift);
    } else {
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
            a.movbe(bitdata, x86::qword_ptr(bin_base, num_bytes_to_read - 8));
        } else {
            a.mov(bitdata, x86::qword_ptr(bin_base, num_bytes_to_read - 8));
            a.bswap(bitdata);
        }
        ASSERT(num_bytes_to_read < 8);
        a.add(bin_offset.r32(), imm(64 - 8 * num_bytes_to_read));
        a.short_().jmp(shift);
    }

    /* We'll need an extra byte and we will need to shift. */
    a.bind(handle_partial);
    if (num_partial != 1) {
        if (num_bytes_to_read == 1) {
            a.mov(bitdata.r16(), x86::word_ptr(bin_base, RET));
        } else {
            ASSERT(num_bytes_to_read < 8);
            a.mov(bitdata, x86::qword_ptr(bin_base, num_bytes_to_read - 7));
            a.shr(bitdata, imm(64 - 8 * (num_bytes_to_read + 1)));
        }
    }

    a.bind(swap);
    a.bswap(bitdata);

    /* Shift the read data into the most significant bits of the
     * word. */
    a.bind(shift);
    a.shl(bitdata, bin_offset.r8());

    a.bind(read_done);
}

/*
 * Read an integer and store as a term. This function only handles
 * integers of certain common sizes. This is a special optimization
 * when only one integer is to be extracted from a binary.
 *
 * Input: bin_base, bin_offset
 *
 * Clobbers: bin_base, bin_offset, tmp, RET
 */
void BeamModuleAssembler::emit_read_integer(const x86::Gp bin_base,
                                            const x86::Gp bin_offset,
                                            const x86::Gp tmp,
                                            Uint flags,
                                            Uint bits,
                                            const ArgRegister &Dst) {
    Label handle_unaligned = a.newLabel();
    Label store = a.newLabel();
    x86::Mem address;

    a.mov(tmp, bin_offset);
    a.shr(tmp, imm(3));
    a.and_(bin_offset.r32(), imm(7));

    switch (bits) {
    case 8:
        address = x86::Mem(bin_base, tmp, 0, 0, 1);
        if ((flags & BSF_SIGNED) == 0) {
            a.movzx(RETd, address);
        } else {
            a.movsx(RET, address);
        }

        a.short_().jz(store);

        a.bind(handle_unaligned);
        address = x86::Mem(bin_base, tmp, 0, 0, 2);
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
            a.movbe(RET.r16(), address);
        } else {
            a.mov(RET.r16(), address);
            a.xchg(x86::al, x86::ah);
        }
        ASSERT(bin_offset == x86::rcx);
        a.shl(RETd, bin_offset.r8());
        a.mov(x86::al, x86::ah);
        if ((flags & BSF_SIGNED) == 0) {
            a.movzx(RETd, RETb);
        } else {
            a.movsx(RET, RETb);
        }
        break;
    case 16:
        address = x86::Mem(bin_base, tmp, 0, 0, 2);
        if ((flags & BSF_LITTLE) != 0) {
            if ((flags & BSF_SIGNED) == 0) {
                a.movzx(RETd, address);
            } else {
                a.movsx(RET, address);
            }
        } else {
            /* Big-endian segment. */
            if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
                a.movbe(RET.r16(), address);
            } else {
                a.mov(RET.r16(), address);
                a.xchg(x86::al, x86::ah);
            }

            if ((flags & BSF_SIGNED) != 0) {
                a.movsx(RET, RET.r16());
            } else {
                a.movzx(RET, RET.r16());
            }
        }

        a.short_().jz(store);

        a.bind(handle_unaligned);
        a.add(bin_base, tmp);
        address = x86::Mem(bin_base, -1, 4);
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
            a.movbe(RETd, address);
        } else {
            a.mov(RETd, address);
            a.bswap(RETd);
        }
        ASSERT(bin_offset == x86::rcx);
        a.shl(RETd, bin_offset.r8());
        a.shr(RETd, imm(8));

        if ((flags & BSF_LITTLE) != 0) {
            a.xchg(x86::al, x86::ah);
        }

        if ((flags & BSF_SIGNED) == 0) {
            a.movzx(RETd, RET.r16());
        } else {
            a.movsx(RET, RET.r16());
        }
        break;
    case 32:
        address = x86::Mem(bin_base, tmp, 0, 0, 4);
        if ((flags & BSF_LITTLE) != 0) {
            /* Little-endian segment. */
            if ((flags & BSF_SIGNED) == 0) {
                a.mov(RETd, address);
            } else {
                a.movsxd(RET, address);
            }
        } else {
            /* Big-endian segment. */
            if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
                a.movbe(RETd, address);
            } else {
                a.mov(RETd, address);
                a.bswap(RETd);
            }

            if ((flags & BSF_SIGNED) != 0) {
                a.movsxd(RET, RETd);
            }
        }

        a.short_().jz(store);

        a.bind(handle_unaligned);
        a.add(bin_base, tmp);
        address = x86::Mem(bin_base, -3, 8);
        if (hasCpuFeature(CpuFeatures::X86::kMOVBE)) {
            a.movbe(RET, address);
        } else {
            a.mov(RET, address);
            a.bswap(RET);
        }
        ASSERT(bin_offset == x86::rcx);
        a.shl(RET, bin_offset.r8());
        a.shr(RET, imm(8));

        if ((flags & BSF_LITTLE) != 0) {
            a.bswap(RETd);
        }

        if ((flags & BSF_SIGNED) == 0) {
            a.mov(RETd, RETd);
        } else {
            a.movsxd(RET, RETd);
        }
        break;
    default:
        ASSERT(0);
        break;
    }

    a.bind(store);
    a.shl(RET, imm(_TAG_IMMED1_SIZE));
    a.or_(RET, imm(_TAG_IMMED1_SMALL));
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_extract_integer(const x86::Gp bitdata,
                                               const x86::Gp tmp,
                                               Uint flags,
                                               Uint bits,
                                               const ArgRegister &Dst) {
    if (bits == 0) {
        /* Necessary for correctness when matching a zero-size
         * signed segment.
         */
        mov_arg(Dst, make_small(0));
        return;
    }

    Label big = a.newLabel();
    Label done = a.newLabel();
    Uint num_partial = bits % 8;
    Uint num_complete = 8 * (bits / 8);

    if (bits <= 8) {
        /* Endian does not matter for values that fit in a byte. */
        flags &= ~BSF_LITTLE;
    }

    if ((flags & BSF_LITTLE) == 0) {
        /* Big-endian segment. */
        a.mov(RET, bitdata);
    } else if ((flags & BSF_LITTLE) != 0) {
        /* Reverse endianness for this little-endian segment. */
        if (num_partial == 0) {
            a.mov(RET, bitdata);
            a.bswap(RET);
            if (bits < 64) {
                a.shl(RET, imm(64 - num_complete));
            }
        } else {
            Uint shifted_mask = ((1 << num_partial) - 1) << (8 - num_partial);
            a.mov(tmp, bitdata);
            a.shr(tmp, imm(64 - num_complete));
            a.bswap(tmp);
            a.shr(tmp, imm(num_partial));

            a.mov(RET, bitdata);
            a.rol(RET, imm(num_complete + 8));
            a.and_(RETd, imm(shifted_mask));
            a.ror(RET, imm(8));
            a.or_(RET, tmp);
        }
    }

    /* Now the extracted data is in RET. */
    if (bits >= SMALL_BITS) {
        /* Handle segments whose values might not fit in a small
         * integer. */
        Label small = a.newLabel();
        comment("test whether this integer is a small");
        if (bits < 64) {
            if ((flags & BSF_SIGNED) == 0) {
                /* Unsigned segment. */
                a.shr(RET, imm(64 - bits));
            } else {
                /* Signed segment. */
                a.sar(RET, imm(64 - bits));
            }
        }
        a.mov(tmp, RET);
        a.shr(tmp, imm(SMALL_BITS - 1));
        if ((flags & BSF_SIGNED) == 0) {
            /* Unsigned segment. */
            a.jnz(big);
        } else {
            /* Signed segment. */
            a.jz(small);
            a.cmp(tmp.r32(), imm(_TAG_IMMED1_MASK << 1 | 1));
            a.jnz(big);
        }

        comment("store extracted integer as a small");
        a.bind(small);
        a.shl(RET, imm(_TAG_IMMED1_SIZE));
        a.or_(RET, imm(_TAG_IMMED1_SMALL));
        a.short_().jmp(done);
    } else {
        /* This segment always fits in a small. */
        comment("store extracted integer as a small");
        if ((flags & BSF_SIGNED) == 0) {
            /* Unsigned segment. */
            a.shr(RET, imm(64 - bits - _TAG_IMMED1_SIZE));
        } else {
            /* Signed segment. */
            a.sar(RET, imm(64 - bits - _TAG_IMMED1_SIZE));
        }
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == (1 << _TAG_IMMED1_SIZE) - 1);
        a.or_(RET, imm(_TAG_IMMED1_SMALL));
    }

    a.bind(big);
    if (bits >= SMALL_BITS) {
        comment("store extracted integer as a bignum");
        if ((flags & BSF_SIGNED) == 0) {
            /* Unsigned segment. */
            a.mov(x86::qword_ptr(HTOP), make_pos_bignum_header(1));
            a.mov(x86::qword_ptr(HTOP, sizeof(Eterm)), RET);
        } else {
            Label negative = a.newLabel();
            Label sign_done = a.newLabel();

            /* Signed segment. */
            a.test(RET, RET);
            a.short_().jl(negative);

            a.mov(x86::qword_ptr(HTOP), make_pos_bignum_header(1));
            a.short_().jmp(sign_done);

            a.bind(negative);
            a.mov(x86::qword_ptr(HTOP), make_neg_bignum_header(1));
            a.neg(RET);

            a.bind(sign_done);
            a.mov(x86::qword_ptr(HTOP, sizeof(Eterm)), RET);
        }
        a.lea(RET, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
        a.add(HTOP, imm(sizeof(Eterm[2])));
    }

    a.bind(done);
    mov_arg(Dst, RET);
}

/* Clobbers: RET */
void BeamModuleAssembler::emit_extract_bitstring(const x86::Gp bitdata,
                                                 Uint bits,
                                                 const ArgRegister &Dst) {
    a.lea(RET, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
    mov_arg(Dst, RET);

    a.mov(x86::qword_ptr(HTOP), header_heap_bits(bits));
    a.mov(x86::qword_ptr(HTOP, sizeof(Eterm)), imm(bits));

    if (bits > 0) {
        a.mov(RET, bitdata);
        a.bswap(RET);
        a.mov(x86::qword_ptr(HTOP, 2 * sizeof(Eterm)), RET);
        a.add(HTOP, imm(sizeof(Eterm[3])));
    } else {
        a.add(HTOP, imm(sizeof(Eterm[2])));
    }
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

    int read_action_pos = -1;
    int seg_index = 0;
    int count = segments.size();

    for (int i = 0; i < count; i++) {
        auto seg = segments[i];
        if (heap_need != 0 && seg.live.isWord()) {
            BsmSegment s = seg;

            read_action_pos = -1;
            s.action = BsmSegment::action::TEST_HEAP;
            s.size = heap_need;
            segs.push_back(s);
            heap_need = 0;
            seg_index++;
        }

        switch (seg.action) {
        case BsmSegment::action::GET_INTEGER:
        case BsmSegment::action::GET_BITSTRING: {
            bool is_common_size;
            switch (seg.size) {
            case 8:
            case 16:
            case 32:
                is_common_size = true;
                break;
            default:
                is_common_size = false;
                break;
            }

            if (seg.size > 64) {
                read_action_pos = -1;
            } else if ((seg.flags & BSF_LITTLE) != 0 && is_common_size) {
                seg.action = BsmSegment::action::READ_INTEGER;
                read_action_pos = -1;
            } else if (read_action_pos < 0 &&
                       seg.action == BsmSegment::action::GET_INTEGER &&
                       is_common_size && i + 1 == count) {
                seg.action = BsmSegment::action::READ_INTEGER;
                read_action_pos = -1;
            } else {
                if ((seg.flags & BSF_LITTLE) != 0 || read_action_pos < 0 ||
                    seg.size + segs.at(read_action_pos).size > 64) {
                    BsmSegment s;

                    /* Create a new READ action. */
                    read_action_pos = seg_index;
                    s.action = BsmSegment::action::READ;
                    s.size = seg.size;
                    segs.push_back(s);
                    seg_index++;
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
        }
        case BsmSegment::action::EQ: {
            if (read_action_pos < 0 ||
                seg.size + segs.at(read_action_pos).size > 64) {
                BsmSegment s;

                /* Create a new READ action. */
                read_action_pos = seg_index;
                s.action = BsmSegment::action::READ;
                s.size = seg.size;
                segs.push_back(s);
                seg_index++;
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
                seg_index--;
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
        seg_index++;
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

    /* Constraints:
     *
     * bin_position must be RCX because only CL can be used for
     * a variable shift without using the SHLX instruction from BMI2.
     */

#ifdef ERTS_JIT_ABI_WIN32
    const x86::Gp bin_position = ARG1;
    const x86::Gp bitdata = ARG2;
    const x86::Gp bin_base = ARG3;
    const x86::Gp ctx = ARG4;
#else
    const x86::Gp bin_position = ARG4;
    const x86::Gp bitdata = ARG3;
    const x86::Gp bin_base = ARG1;
    const x86::Gp ctx = ARG2;
#endif

    ASSERT(bin_position == x86::rcx);
    const x86::Gp tmp = ARG5;

    bool is_ctx_valid = false;
    bool is_position_valid = false;
    bool next_instr_clobbers = false;
    int count = segments.size();

    for (int i = 0; i < count; i++) {
        auto seg = segments[i];

        /* Find out whether the next sub instruction clobbers
         * registers or is the last. */
        next_instr_clobbers =
                i == count - 1 ||
                (i < count - 1 &&
                 segments[i + 1].action == BsmSegment::action::TEST_HEAP);

        switch (seg.action) {
        case BsmSegment::action::ENSURE_AT_LEAST: {
            auto size = seg.size;
            auto unit = seg.unit;
            comment("ensure_at_least %ld %ld", size, seg.unit);
            mov_arg(ctx, Ctx);
            if (unit == 1) {
                a.mov(bin_position, emit_boxed_val(ctx, start_offset));
                a.lea(RET, qword_ptr(bin_position, size));
                a.cmp(RET, emit_boxed_val(ctx, end_offset));
                a.ja(resolve_beam_label(Fail));
            } else if (size == 0 && next_instr_clobbers) {
                a.mov(RET, emit_boxed_val(ctx, end_offset));
                a.sub(RET, emit_boxed_val(ctx, start_offset));
                is_ctx_valid = is_position_valid = false;
            } else {
                a.mov(RET, emit_boxed_val(ctx, end_offset));
                a.mov(bin_position, emit_boxed_val(ctx, start_offset));
                a.sub(RET, bin_position);
                cmp(RET, size, tmp);
                a.jl(resolve_beam_label(Fail));
            }

            is_ctx_valid = is_position_valid = true;

            if (unit != 1) {
                if (size % unit != 0) {
                    sub(RET, size, tmp);
                }

                if ((unit & (unit - 1))) {
                    /* Clobbers ARG3 */
                    a.cqo();
                    mov_imm(tmp, unit);
                    a.div(tmp);
                    a.test(x86::rdx, x86::rdx);
                    is_ctx_valid = is_position_valid = false;
                } else {
                    a.test(RETb, imm(unit - 1));
                }
                a.jnz(resolve_beam_label(Fail));
            }
            break;
        }
        case BsmSegment::action::ENSURE_EXACTLY: {
            auto size = seg.size;
            comment("ensure_exactly %ld", size);

            mov_arg(ctx, Ctx);
            a.mov(RET, emit_boxed_val(ctx, end_offset));
            if (next_instr_clobbers) {
                a.sub(RET, emit_boxed_val(ctx, start_offset));
                is_ctx_valid = is_position_valid = false;
            } else {
                a.mov(bin_position, emit_boxed_val(ctx, start_offset));
                a.sub(RET, bin_position);
                is_ctx_valid = is_position_valid = true;
            }
            if (size != 0) {
                cmp(RET, size, tmp);
            }
            a.jne(resolve_beam_label(Fail));
            break;
        }
        case BsmSegment::action::EQ: {
            comment("=:= %ld %ld", seg.size, seg.unit);
            auto bits = seg.size;
            x86::Gp extract_reg;

            if (next_instr_clobbers) {
                extract_reg = bitdata;
            } else {
                extract_reg = RET;
                a.mov(extract_reg, bitdata);
            }
            if (bits != 0 && bits != 64) {
                a.shr(extract_reg, imm(64 - bits));
            }

            if (seg.size <= 32) {
                cmp(extract_reg.r32(), seg.unit, tmp);
            } else {
                cmp(extract_reg, seg.unit, tmp);
            }

            a.jne(resolve_beam_label(Fail));

            if (!next_instr_clobbers && bits != 0 && bits != 64) {
                a.shl(bitdata, imm(bits));
            }

            /* bin_position is clobbered. */
            is_position_valid = false;
            break;
        }
        case BsmSegment::action::TEST_HEAP: {
            comment("test_heap %ld", seg.size);
            emit_gc_test(ArgWord(0), ArgWord(seg.size), seg.live);
            is_ctx_valid = is_position_valid = false;
            break;
        }
        case BsmSegment::action::READ: {
            comment("read %ld", seg.size);
            if (seg.size == 0) {
                comment("(nothing to do)");
            } else {
                if (!is_ctx_valid) {
                    mov_arg(ctx, Ctx);
                    is_ctx_valid = true;
                }
                if (!is_position_valid) {
                    a.mov(bin_position, emit_boxed_val(ctx, start_offset));
                    is_position_valid = true;
                }
                a.mov(bin_base, emit_boxed_val(ctx, base_offset));
                a.and_(bin_base, imm(~ERL_SUB_BITS_FLAG_MASK));
                a.add(emit_boxed_val(ctx, start_offset), imm(seg.size));

                emit_read_bits(seg.size, bin_base, bin_position, bitdata);
            }

            is_position_valid = false;
            break;
        }
        case BsmSegment::action::EXTRACT_BITSTRING: {
            auto bits = seg.size;
            auto Dst = seg.dst;

            comment("extract binary %ld", bits);
            emit_extract_bitstring(bitdata, bits, Dst);
            if (!next_instr_clobbers && bits != 0 && bits != 64) {
                a.shl(bitdata, imm(bits));
            }
            break;
        }
        case BsmSegment::action::EXTRACT_INTEGER: {
            auto bits = seg.size;
            auto flags = seg.flags;
            auto Dst = seg.dst;

            comment("extract integer %ld", bits);
            if (next_instr_clobbers && flags == 0 && bits < SMALL_BITS) {
                a.shr(bitdata, imm(64 - bits - _TAG_IMMED1_SIZE));
                a.or_(bitdata, imm(_TAG_IMMED1_SMALL));
                mov_arg(Dst, bitdata);
            } else {
                emit_extract_integer(bitdata, tmp, flags, bits, Dst);
                if (!next_instr_clobbers && bits != 0 && bits != 64) {
                    a.shl(bitdata, imm(bits));
                }
            }

            /* bin_position is clobbered. */
            is_position_valid = false;
            break;
        }
        case BsmSegment::action::READ_INTEGER: {
            auto bits = seg.size;
            auto flags = seg.flags;
            auto Dst = seg.dst;

            comment("read integer %ld", bits);
            if (!is_ctx_valid) {
                mov_arg(ctx, Ctx);
                is_ctx_valid = true;
            }
            if (!is_position_valid) {
                a.mov(bin_position, emit_boxed_val(ctx, start_offset));
                is_position_valid = true;
            }

            a.mov(bin_base, emit_boxed_val(ctx, base_offset));
            a.and_(bin_base, imm(~ERL_SUB_BITS_FLAG_MASK));
            a.add(emit_boxed_val(ctx, start_offset), imm(seg.size));
            emit_read_integer(bin_base, bin_position, tmp, flags, bits, Dst);

            is_position_valid = false;
            break;
        }
        case BsmSegment::action::GET_INTEGER: {
            /* Match integer segments with more than 64 bits. */
            Uint flags = seg.flags;
            auto bits = seg.size;
            auto Dst = seg.dst;

            comment("get integer %ld", bits);
            if (!is_ctx_valid) {
                mov_arg(ARG4, Ctx);
            } else if (ctx != ARG4) {
                a.mov(ARG4, ctx);
            }

            emit_enter_runtime<Update::eReductions | Update::eHeapOnlyAlloc>();

            a.mov(ARG1, c_p);
            a.mov(ARG2, bits);
            a.mov(ARG3, flags);
            a.sub(ARG4, imm(TAG_PRIMARY_BOXED));
            runtime_call<Eterm (*)(Process *, Uint, unsigned, ErlSubBits *),
                         erts_bs_get_integer_2>();

            emit_leave_runtime<Update::eReductions | Update::eHeapOnlyAlloc>();

            mov_arg(Dst, RET);

            is_ctx_valid = is_position_valid = false;
            break;
        }
        case BsmSegment::action::GET_BITSTRING: {
            ERTS_ASSERT(seg.size > 64);
            comment("get binary %ld", seg.size);
            if (is_ctx_valid) {
                a.mov(RET, ctx);
            } else {
                mov_arg(RET, Ctx);
            }
            emit_enter_runtime<Update::eHeapOnlyAlloc>();
            a.mov(ARG5, emit_boxed_val(RET, start_offset));
            a.lea(ARG1, x86::qword_ptr(c_p, offsetof(Process, htop)));
            if (seg.size <= ERL_ONHEAP_BITS_LIMIT) {
                comment("skipped setting registers not used for heap binary");
            } else {
                a.mov(ARG2, emit_boxed_val(RET, orig_offset));
                a.mov(ARG3, ARG2);
                a.and_(ARG2, imm(TAG_PTR_MASK__));
                a.and_(ARG3, imm(~TAG_PTR_MASK__));
            }
            a.mov(ARG4, emit_boxed_val(RET, base_offset));
            a.and_(ARG4, imm(~ERL_SUB_BITS_FLAG_MASK));
            mov_imm(ARG6, seg.size);
            a.add(emit_boxed_val(RET, start_offset), ARG6);

            runtime_call<Eterm (*)(Eterm **,
                                   Eterm,
                                   const BinRef *,
                                   const byte *,
                                   Uint,
                                   Uint),
                         erts_build_sub_bitstring>();

            emit_leave_runtime<Update::eHeapOnlyAlloc>();
            mov_arg(seg.dst, RET);

            is_ctx_valid = is_position_valid = false;
            break;
        }
        case BsmSegment::action::GET_TAIL: {
            comment("get_tail");
            if (is_ctx_valid) {
                a.mov(ARG1, ctx);
            } else {
                mov_arg(ARG1, Ctx);
            }
            safe_fragment_call(ga->get_bs_get_tail_shared());
            mov_arg(seg.dst, RET);
            is_ctx_valid = is_position_valid = false;
            break;
        }
        case BsmSegment::action::SKIP: {
            comment("skip %ld", seg.size);
            if (!is_ctx_valid) {
                mov_arg(ctx, Ctx);
                is_ctx_valid = true;
            }
            /* The compiler limits the size of any segment in a bs_match
             * instruction to 24 bits. */
            ASSERT((seg.size >> 24) == 0);
            a.add(emit_boxed_val(ctx, start_offset), imm(seg.size));
            is_position_valid = false;
            break;
        }
        case BsmSegment::action::DROP:
            auto bits = seg.size;
            comment("drop %ld", bits);
            if (bits != 0 && bits != 64) {
                a.shl(bitdata, imm(bits));
            }
            break;
        }
    }
}
