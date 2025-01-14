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

/*
 * Notes.
 *
 * The immediate operand for the and, orr, eor, and tst instructions
 * follow special rules.
 *
 * For our purposes, only bit patterns consisting of 1 through 63 ones
 * at any position in a word are possible to encode as an
 * immediate. Other patterns must be loaded into a tempoary register.
 *
 * Here are some examples of possible immediate values:
 *
 *    0b00000011
 *    0b00001111
 *    0b00111100
 *
 *    0xFFFFFFFFFFFFFFF0
 *    0x100000000000000F
 *
 * The last one is possible because it is the pattern 0x1F
 * (0b00011111) rotated right one position.
 *
 * Here is an example of mask that is not a possible to encode as an
 * immediate:
 *
 *    0b111011
 *
 * For more about the encoding rules, see:
 *
 * https://stackoverflow.com/questions/30904718/range-of-immediate-values-in-armv8-a64-assembly
 *
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
#include "erl_binary.h"
#include "erl_map.h"
}

using namespace asmjit;

/* Helpers */

void BeamModuleAssembler::emit_error(int reason) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_error(int reason, const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_gc_test_preserve(const ArgWord &Need,
                                                const ArgWord &Live,
                                                const ArgSource &Preserve,
                                                a32::Gp preserve_reg) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_gc_test(const ArgWord &Ns,
                                       const ArgWord &Nh,
                                       const ArgWord &Live) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_validate(const ArgWord &Arity) {
    // TODO
    ASSERT(false);
}

/* Instrs */

void BeamModuleAssembler::emit_i_validate(const ArgWord &Arity) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_allocate_heap(const ArgWord &NeedStack,
                                             const ArgWord &NeedHeap,
                                             const ArgWord &Live) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_allocate(const ArgWord &NeedStack,
                                        const ArgWord &Live) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_deallocate(const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_test_heap(const ArgWord &Nh,
                                         const ArgWord &Live) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_normal_exit() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_continue_exit() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_get_list(const ArgRegister &Src,
                                        const ArgRegister &Hd,
                                        const ArgRegister &Tl) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_get_hd(const ArgRegister &Src,
                                      const ArgRegister &Hd) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_get_tl(const ArgRegister &Src,
                                      const ArgRegister &Tl) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_get(const ArgSource &Src,
                                     const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_get_hash(const ArgConstant &Src,
                                          const ArgWord &Hash,
                                          const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

/* Store the untagged pointer to a tuple in ARG1. */
void BeamModuleAssembler::emit_load_tuple_ptr(const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

#ifdef DEBUG
/* Emit an assertion to ensure that tuple_reg points into the same
 * tuple as Src. */
void BeamModuleAssembler::emit_tuple_assertion(const ArgSource &Src,
                                               a32::Gp tuple_reg) {
    // TODO
    ASSERT(false);
}
#endif

/* Fetch an element from the tuple pointed to by the untagged pointer
 * in ARG1. */
void BeamModuleAssembler::emit_i_get_tuple_element(const ArgSource &Src,
                                                   const ArgWord &Element,
                                                   const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_get_tuple_element_swap(
        const ArgSource &Src,
        const ArgWord &Element,
        const ArgRegister &Dst,
        const ArgRegister &OtherDst) {
    // TODO
    ASSERT(false);
}

/* Fetch two consecutive tuple elements from the tuple pointed to by
 * the boxed pointer in ARG1. */
void BeamModuleAssembler::emit_get_two_tuple_elements(const ArgSource &Src,
                                                      const ArgWord &Element,
                                                      const ArgRegister &Dst1,
                                                      const ArgRegister &Dst2) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_init_yregs(const ArgWord &Size,
                                          const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_trim(const ArgWord &Words,
                                    const ArgWord &Remaining) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_move(const ArgSource &Src,
                                      const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_move_two_trim(const ArgYRegister &Src1,
                                             const ArgRegister &Dst1,
                                             const ArgYRegister &Src2,
                                             const ArgRegister &Dst2,
                                             const ArgWord &Words) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_move_trim(const ArgSource &Src,
                                         const ArgRegister &Dst,
                                         const ArgWord &Words) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_store_two_values(const ArgSource &Src1,
                                                const ArgRegister &Dst1,
                                                const ArgSource &Src2,
                                                const ArgRegister &Dst2) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_load_two_xregs(const ArgRegister &Src1,
                                              const ArgXRegister &Dst1,
                                              const ArgRegister &Src2,
                                              const ArgXRegister &Dst2) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_swap(const ArgRegister &R1,
                                    const ArgRegister &R2) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_swap2(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_swap3(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_swap4(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4,
                                     const ArgRegister &R5) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_node(const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_put_list(const ArgSource &Hd,
                                        const ArgSource &Tl,
                                        const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_put_list_deallocate(const ArgSource &Hd,
                                                   const ArgSource &Tl,
                                                   const ArgRegister &Dst,
                                                   const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_put_list2(const ArgSource &Hd1,
                                         const ArgSource &Hd2,
                                         const ArgSource &Tl,
                                         const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_put_tuple2(const ArgRegister &Dst,
                                          const ArgWord &Arity,
                                          const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_self(const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_copy_words_increment(a32::Gp from,
                                                    a32::Gp to,
                                                    size_t count) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_update_record(const ArgAtom &Hint,
                                             const ArgWord &TupleSize,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst,
                                             const ArgWord &UpdateCount,
                                             const Span<ArgVal> &updates) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_update_record_in_place(
        const ArgWord &TupleSize,
        const ArgSource &Src,
        const ArgRegister &Dst,
        const ArgWord &UpdateCount,
        const Span<ArgVal> &updates) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_set_tuple_element(const ArgSource &Element,
                                                 const ArgRegister &Tuple,
                                                 const ArgWord &Offset) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_nonempty_list(const ArgLabel &Fail,
                                                const ArgRegister &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_jump(const ArgLabel &Fail) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_atom(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_boolean(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_bitstring(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_binary(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_float(const ArgLabel &Fail,
                                        const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_function(const ArgLabel &Fail,
                                           const ArgRegister &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_function2(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgSource &Arity) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_integer(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_list(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_map(const ArgLabel &Fail,
                                      const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_nil(const ArgLabel &Fail,
                                      const ArgRegister &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_number(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_pid(const ArgLabel &Fail,
                                      const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_port(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_reference(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tagged_tuple(const ArgLabel &Fail,
                                                 const ArgSource &Src,
                                                 const ArgWord &Arity,
                                                 const ArgAtom &Tag) {
    // TODO
    ASSERT(false);
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple_ff(const ArgLabel &NotTuple,
                                                    const ArgLabel &NotRecord,
                                                    const ArgSource &Src,
                                                    const ArgWord &Arity,
                                                    const ArgAtom &Tag) {
    // TODO
    ASSERT(false);
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity(const ArgLabel &Fail,
                                                   const ArgSource &Src,
                                                   const ArgWord &Arity) {
    // TODO
    ASSERT(false);
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity_ff(const ArgLabel &NotTuple,
                                                      const ArgLabel &BadArity,
                                                      const ArgSource &Src,
                                                      const ArgWord &Arity) {
    // TODO
    ASSERT(false);
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_test_arity(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgWord &Arity) {
    // TODO
    ASSERT(false);
}

/*
 * ARG1 = First operand
 * ARG2 = Literal list
 *
 * The result is returned in the Z flag.
 */
void BeamGlobalAssembler::emit_is_eq_exact_list_shared() {
    // TODO
    ASSERT(false);
}

/*
 * ARG1 = LHS
 * ARG2 = RHS
 *
 * The result is returned in the Z flag.
 */
void BeamGlobalAssembler::emit_is_eq_exact_shallow_boxed_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_eq_exact(const ArgLabel &Fail,
                                           const ArgSource &X,
                                           const ArgSource &Y) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_ne_exact(const ArgLabel &Fail,
                                           const ArgSource &X,
                                           const ArgSource &Y) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_eq(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_ne(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    // TODO
    ASSERT(false);
}

/*
 * ARG1 = LHS
 * ARG2 = RHS
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_arith_compare_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_lt(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_is_ge(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    // TODO
    ASSERT(false);
}

/*
 * ARG1 = Src
 * ARG2 = Min
 * ARG3 = Max
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_is_in_range_shared() {
    // TODO
    ASSERT(false);
}

/*
 * 1121 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_in_range(ArgLabel const &Small,
                                           ArgLabel const &Large,
                                           ArgRegister const &Src,
                                           ArgConstant const &Min,
                                           ArgConstant const &Max) {
    // TODO
    ASSERT(false);
}

/*
 * ARG1 = Src
 * ARG2 = A
 * ARG3 = B
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_is_ge_lt_shared() {
    // TODO
    ASSERT(false);
}

/*
 * The instruction sequence:
 *
 *   is_ge Fail1 Src A
 *   is_lt Fail1 B Src
 *
 * is common (1841 occurrences in OTP at the time of writing).
 *
 * is_ge + is_lt is 18 instructions, while is_ge_lt is
 * 14 instructions.
 */
void BeamModuleAssembler::emit_is_ge_lt(ArgLabel const &Fail1,
                                        ArgLabel const &Fail2,
                                        ArgRegister const &Src,
                                        ArgConstant const &A,
                                        ArgConstant const &B) {
    // TODO
    ASSERT(false);
}

/*
 * 1190 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_ge_ge(ArgLabel const &Fail1,
                                        ArgLabel const &Fail2,
                                        ArgRegister const &Src,
                                        ArgConstant const &A,
                                        ArgConstant const &B) {
    // TODO
    ASSERT(false);
}

/*
 * 60 occurrences in OTP at the time of writing. Seems to be common in
 * Elixir code.
 *
 * Currently not very frequent in OTP but very nice reduction in code
 * size when it happens. We expect this combination of instructions
 * to become more common in the future.
 */
void BeamModuleAssembler::emit_is_int_in_range(ArgLabel const &Fail,
                                               ArgRegister const &Src,
                                               ArgConstant const &Min,
                                               ArgConstant const &Max) {
    // TODO
    ASSERT(false);
}

/*
 * 428 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_int_ge(ArgLabel const &Fail,
                                         ArgRegister const &Src,
                                         ArgConstant const &Min) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_badmatch(const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_case_end(const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_system_limit_body() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_if_end() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_badrecord(const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_catch(const ArgYRegister &Y,
                                     const ArgCatch &Handler) {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_catch_end_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_catch_end(const ArgYRegister &CatchTag) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_try_end(const ArgYRegister &CatchTag) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_try_end_deallocate(const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_try_end_move_deallocate(
        const ArgSource &Src,
        const ArgRegister &Dst,
        const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_try_case(const ArgYRegister &CatchTag) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_try_case_end(const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_raise_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_raise(const ArgSource &Trace,
                                     const ArgSource &Value) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_build_stacktrace() {
    // TODO
    ASSERT(false);
}

/* This instruction has the same semantics as the erlang:raise/3 BIF,
 * except that it can rethrow a raw stack backtrace. */
void BeamModuleAssembler::emit_raw_raise() {
    // TODO
    ASSERT(false);
}

#define TEST_YIELD_RETURN_OFFSET                                               \
    (BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(Uint32[3]) +                         \
     (erts_alcu_enable_code_atags ? sizeof(Uint32) : 0))

/* ARG3 = current_label */
void BeamGlobalAssembler::emit_i_test_yield_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_test_yield() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_yield() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_perf_counter() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_mark_unreachable() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_coverage(void *coverage, Uint index, Uint size) {
    // TODO
    ASSERT(false);
}
