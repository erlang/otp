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
    // TODO
    emit_nyi("emit_bs_get_field_size");
    return -1;
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
    // TODO
    emit_nyi("emit_bs_get_position");
}

void BeamModuleAssembler::emit_bs_get_integer2(const ArgLabel &Fail,
                                               const ArgRegister &Ctx,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Unit,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_bs_get_integer2");
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgLabel &Fail,
                                             const ArgRegister &Ctx,
                                             const ArgWord &Offset) {
    // TODO
    emit_nyi("emit_bs_test_tail2");
}

void BeamModuleAssembler::emit_bs_set_position(const ArgRegister &Ctx,
                                               const ArgRegister &Pos) {
    // TODO
    emit_nyi("emit_bs_set_position");
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
    // TODO
    emit_nyi("emit_bs_get_tail_shared");
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgRegister &Ctx,
                                           const ArgRegister &Dst,
                                           const ArgWord &Live) {
    // TODO
    emit_nyi("emit_bs_get_tail");
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
    // TODO
    emit_nyi("emit_i_bs_get_binary2");
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_get_float2");
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
    // TODO
    emit_nyi("emit_bs_get_utf8");
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgRegister &Ctx,
                                             const ArgLabel &Fail,
                                             const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_get_utf8");
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgRegister &Ctx,
                                              const ArgLabel &Fail) {
    // TODO
    emit_nyi("emit_i_bs_skip_utf8");
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
    // TODO
    emit_nyi("emit_bs_get_utf16");
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgRegister &Ctx,
                                              const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_bs_get_utf16");
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Flags) {
    // TODO
    emit_nyi("emit_i_bs_skip_utf16");
}

void BeamModuleAssembler::emit_validate_unicode(Label next,
                                                Label fail,
                                                a32::Gp value) {
    // TODO
    emit_nyi("emit_validate_unicode");
}

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgLabel &Fail,
                                                     const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_bs_validate_unicode");
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(
        const ArgLabel &Fail,
        const ArgSource &Src,
        const ArgRegister &Ms) {
    // TODO
    emit_nyi("emit_i_bs_validate_unicode_retract");
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
    // TODO
    emit_nyi("emit_bs_init_writable");
}

void BeamGlobalAssembler::emit_bs_create_bin_error_shared() {
    // TODO
    emit_nyi("emit_bs_create_bin_error_shared");
}

/*
 * ARG1 = tagged bignum term
 */
void BeamGlobalAssembler::emit_get_sint64_shared() {
    // TODO
    emit_nyi("emit_get_sint64_shared");
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
    // TODO
    ASSERT(false);
    return std::vector<BscSegment>();
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
    // TODO
    emit_nyi("update_bin_state");
}

/*
 * The size of the segment is assumed to be in ARG3.
 */
void BeamModuleAssembler::set_zero(Sint effectiveSize) {
    // TODO
    emit_nyi("set_zero");
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
    // TODO
    emit_nyi("emit_bs_init_bits_shared");
}

void BeamModuleAssembler::emit_construct_utf8(const ArgVal &Src,
                                              Sint bit_offset,
                                              bool is_byte_aligned) {
    // TODO
    emit_nyi("emit_construct_utf8");
}

/*
 * In:
 *   TMP1 = pointer to current byte
 *   ARG3 = bit offset
 *   ARG4 = number of bits to write
 *   ARG8 = data to write
 */
void BeamGlobalAssembler::emit_store_unaligned() {
    // TODO
    emit_nyi("emit_store_unaligned");
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
    // TODO
    emit_nyi("emit_bs_get_utf8_short_shared");
}

void BeamModuleAssembler::emit_i_bs_create_bin(const ArgLabel &Fail,
                                               const ArgWord &Alloc,
                                               const ArgWord &Live0,
                                               const ArgRegister &Dst,
                                               const Span<ArgVal> &args) {
    // TODO
    emit_nyi("emit_i_bs_create_bin");
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
                    mov_imm(TMP, TAG_PTR_MASK__);
                    a.and_(ARG3, ARG2, TMP);
                    mov_imm(TMP, ~TAG_PTR_MASK__);
                    a.and_(ARG2, ARG2, TMP);
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
            mov_imm(TMP, TAG_PTR_MASK__);
            a.and_(ARG3, ARG2, TMP);
            mov_imm(TMP, ~TAG_PTR_MASK__);
            a.and_(ARG2, ARG2, TMP);

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
