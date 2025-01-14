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

using namespace asmjit;

template<typename T>
static constexpr bool isInt13(T value) {
    // TODO
    ASSERT(false);
    return false;
}

/* The `cmp`/`cmn` instructions in AArch64 only accept 12-bit unsigned immediate
 * values (`cmn` negating said immediate, giving us an effective range of 13
 * bit signed). That means that to compare most atoms, the atom number to be
 * compared must be loaded into a temporary register.
 *
 * We can use the immediate form of `cmp`/`cmn` for more values if we untag
 * both the source value and the values to be compared.
 *
 * This function finds the `base` and `shift` that result in the most number
 * of elements fitting in a 13-bit immediate. */
static std::pair<UWord, int> plan_untag(const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
    return std::pair<UWord, int>();
}

const std::vector<ArgVal> BeamModuleAssembler::emit_select_untag(
        const ArgSource &Src,
        const Span<ArgVal> &args,
        a32::Gp comparand,
        Label fail,
        UWord base,
        int shift) {
    // TODO
    ASSERT(false);
    return std::vector<ArgVal>();
}

void BeamModuleAssembler::emit_linear_search(a32::Gp comparand,
                                             Label fail,
                                             const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_select_tuple_arity(const ArgRegister &Src,
                                                    const ArgLabel &Fail,
                                                    const ArgWord &Size,
                                                    const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_select_val_lins(const ArgSource &Src,
                                                 const ArgVal &Fail,
                                                 const ArgWord &Size,
                                                 const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_select_val_bins(const ArgSource &Src,
                                                 const ArgVal &Fail,
                                                 const ArgWord &Size,
                                                 const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

/*
 * Emit code for a binary search through an interval Left <= Right of
 * the i_select_val argument vector `args`.
 */
void BeamModuleAssembler::emit_binsearch_nodes(a32::Gp reg,
                                               size_t Left,
                                               size_t Right,
                                               Label fail,
                                               const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_jump_on_val(const ArgSource &Src,
                                             const ArgVal &Fail,
                                             const ArgWord &Base,
                                             const ArgWord &Size,
                                             const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

/*
 * Optimize the case when a select_val has exactly two values that
 * both branch to the same label.
 *
 * If the values only differ by one bit, the optimization makes use of
 * the observation that (V == X || V == Y) is equivalent to (V | (X ^
 * Y)) == (X | Y) when (X ^ Y) has only one bit set.
 *
 * If more than one bit differ, one conditional branch instruction can
 * still be eliminated by using the CCMP instruction.
 *
 * Return true if the optimization was possible.
 */
void BeamModuleAssembler::emit_optimized_two_way_select(a32::Gp reg,
                                                        const ArgVal &value1,
                                                        const ArgVal &value2,
                                                        const ArgVal &label) {
    // TODO
    ASSERT(false);
}
