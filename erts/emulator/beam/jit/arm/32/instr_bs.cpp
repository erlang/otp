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
    return -1;
}

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgWord &Size,
                                              const ArgWord &Heap,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    // TODO
}

/* Set the error reason when a size check has failed. */
void BeamGlobalAssembler::emit_bs_size_check_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_init_fail_heap(const ArgSource &Size,
                                                   const ArgWord &Heap,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_init(const ArgWord &Size,
                                         const ArgWord &Live,
                                         const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_init_fail(const ArgRegister &Size,
                                              const ArgLabel &Fail,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_init_bits(const ArgWord &NumBits,
                                              const ArgWord &Live,
                                              const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_init_bits_heap(const ArgWord &NumBits,
                                                   const ArgWord &Alloc,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail(const ArgRegister &NumBits,
                                                   const ArgLabel &Fail,
                                                   const ArgWord &Live,
                                                   const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail_heap(
        const ArgSource &NumBits,
        const ArgWord &Alloc,
        const ArgLabel &Fail,
        const ArgWord &Live,
        const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bs_put_string(const ArgWord &Size,
                                             const ArgBytePtr &Ptr) {
    // TODO
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(const ArgSource &Src,
                                                        const ArgLabel &Fail,
                                                        const ArgWord &Sz,
                                                        const ArgWord &Flags) {
    // TODO
}

void BeamModuleAssembler::emit_i_new_bs_put_integer(const ArgLabel &Fail,
                                                    const ArgRegister &Sz,
                                                    const ArgWord &Flags,
                                                    const ArgSource &Src) {
    // TODO
}

void BeamModuleAssembler::emit_i_new_bs_put_binary(const ArgLabel &Fail,
                                                   const ArgSource &Sz,
                                                   const ArgWord &Flags,
                                                   const ArgSource &Src) {
    // TODO
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(const ArgSource &Src,
                                                       const ArgLabel &Fail,
                                                       const ArgWord &Unit) {
    // TODO
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(const ArgLabel &Fail,
                                                       const ArgWord &Sz,
                                                       const ArgSource &Src) {
    // TODO
}

void BeamModuleAssembler::emit_i_new_bs_put_float(const ArgLabel &Fail,
                                                  const ArgRegister &Sz,
                                                  const ArgWord &Flags,
                                                  const ArgSource &Src) {
    // TODO
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(const ArgLabel &Fail,
                                                      const ArgWord &Sz,
                                                      const ArgWord &Flags,
                                                      const ArgSource &Src) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_start_match3(const ArgRegister &Src,
                                                 const ArgWord &Live,
                                                 const ArgLabel &Fail,
                                                 const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgRegister &Ctx,
                                                 const ArgLabel &Fail,
                                                 const ArgWord &Bits,
                                                 const ArgBytePtr &Ptr) {
    // TODO
}

void BeamModuleAssembler::emit_bs_get_position(const ArgRegister &Ctx,
                                               const ArgRegister &Dst,
                                               const ArgWord &Live) {
    // TODO
}

void BeamModuleAssembler::emit_bs_get_integer2(const ArgLabel &Fail,
                                               const ArgRegister &Ctx,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Unit,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgLabel &Fail,
                                             const ArgRegister &Ctx,
                                             const ArgWord &Offset) {
    // TODO
}

void BeamModuleAssembler::emit_bs_set_position(const ArgRegister &Ctx,
                                               const ArgRegister &Pos) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgRegister &Ctx,
                                                    const ArgLabel &Fail,
                                                    const ArgWord &Live,
                                                    const ArgWord &Unit,
                                                    const ArgRegister &Dst) {
    // TODO
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    // TODO
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgRegister &Ctx,
                                           const ArgRegister &Dst,
                                           const ArgWord &Live) {
    // TODO
}

/* Bits to skip are passed in ARG1 */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgLabel &Fail,
                                            const ArgRegister &Ctx) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgRegister &Ctx,
                                               const ArgRegister &Size,
                                               const ArgLabel &Fail,
                                               const ArgWord &Unit) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(const ArgLabel &Fail,
                                                   const ArgRegister &Ctx,
                                                   const ArgWord &Bits) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgRegister &Ctx,
                                                const ArgLabel &Fail,
                                                const ArgWord &Live,
                                                const ArgSource &Size,
                                                const ArgWord &Flags,
                                                const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Live,
                                               const ArgSource &Sz,
                                               const ArgWord &Flags,
                                               const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_utf8_size(const ArgSource &Src,
                                              const ArgXRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_put_utf8(const ArgLabel &Fail,
                                             const ArgSource &Src) {
    // TODO
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
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgRegister &Ctx,
                                           const ArgLabel &Fail) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgRegister &Ctx,
                                             const ArgLabel &Fail,
                                             const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgRegister &Ctx,
                                              const ArgLabel &Fail) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_utf16_size(const ArgSource &Src,
                                               const ArgXRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_put_utf16(const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgSource &Src) {
    // TODO
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgRegister &Ctx,
                                            const ArgLabel &Fail,
                                            const ArgWord &Flags) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgRegister &Ctx,
                                              const ArgLabel &Fail,
                                              const ArgWord &Flags,
                                              const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgRegister &Ctx,
                                               const ArgLabel &Fail,
                                               const ArgWord &Flags) {
    // TODO
}

void BeamModuleAssembler::emit_validate_unicode(Label next,
                                                Label fail,
                                                a32::Gp value) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgLabel &Fail,
                                                     const ArgSource &Src) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(
        const ArgLabel &Fail,
        const ArgSource &Src,
        const ArgRegister &Ms) {
    // TODO
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgLabel &Fail,
                                            const ArgRegister &Ctx,
                                            const ArgWord &Unit) {
    // TODO
}

/* ARG2 = current `Size`,
 * ARG3 = elements to `Add`,
 * ARG4 = element `Unit`
 *
 * Error is indicated through cond_ne() */
void BeamGlobalAssembler::emit_bs_add_guard_shared() {
    // TODO
}

/* ARG2 = current `Size`,
 * ARG3 = elements to `Add`,
 * ARG4 = element `Unit` */
void BeamGlobalAssembler::emit_bs_add_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_bs_add(const ArgLabel &Fail,
                                      const ArgSource &Size,
                                      const ArgSource &Add,
                                      const ArgWord &Unit,
                                      const ArgXRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_append(const ArgLabel &Fail,
                                           const ArgWord &ExtraHeap,
                                           const ArgWord &Live,
                                           const ArgWord &Unit,
                                           const ArgSource &Size,
                                           const ArgSource &Bin,
                                           const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_private_append(const ArgLabel &Fail,
                                                   const ArgWord &Unit,
                                                   const ArgSource &Size,
                                                   const ArgRegister &Src,
                                                   const ArgXRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bs_init_writable() {
    // TODO
}

void BeamGlobalAssembler::emit_bs_create_bin_error_shared() {
    // TODO
}

/*
 * ARG1 = tagged bignum term
 */
void BeamGlobalAssembler::emit_get_sint64_shared() {
    // TODO
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
}

/*
 * The size of the segment is assumed to be in ARG3.
 */
void BeamModuleAssembler::set_zero(Sint effectiveSize) {
    // TODO
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
}

void BeamModuleAssembler::emit_construct_utf8(const ArgVal &Src,
                                              Sint bit_offset,
                                              bool is_byte_aligned) {
    // TODO
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
}

void BeamModuleAssembler::emit_i_bs_create_bin(const ArgLabel &Fail,
                                               const ArgWord &Alloc,
                                               const ArgWord &Live0,
                                               const ArgRegister &Dst,
                                               const Span<ArgVal> &args) {
    // TODO
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
}

void BeamModuleAssembler::emit_extract_integer(const a32::Gp &bitdata,
                                               const a32::Gp &small_tag,
                                               Uint flags,
                                               Uint position,
                                               Uint bits,
                                               const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_extract_bitstring(const a32::Gp bitdata,
                                                 Uint position,
                                                 Uint bits,
                                                 const ArgRegister &Dst) {
    // TODO
}

static std::vector<BsmSegment> opt_bsm_segments(
        const std::vector<BsmSegment> segments,
        const ArgWord &Need,
        const ArgWord &Live) {
    // TODO
    return std::vector<BsmSegment>();
}

void BeamModuleAssembler::emit_i_bs_match(ArgLabel const &Fail,
                                          ArgRegister const &Ctx,
                                          Span<ArgVal> const &List) {
    // TODO
}

void BeamModuleAssembler::emit_i_bs_match_test_heap(ArgLabel const &Fail,
                                                    ArgRegister const &Ctx,
                                                    ArgWord const &Need,
                                                    ArgWord const &Live,
                                                    Span<ArgVal> const &List) {
    // TODO
}
