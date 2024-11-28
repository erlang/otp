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
 * Guard BIF calls using the generic bif1, bif2, and bif3 instructions
 * are expensive. Not only are there two indirect calls (one to the
 * fragment, one to the BIF itself), but the caller-saved X registers
 * must also be saved and restored, and the BIF operands that are
 * usually in CPU registers must be written out to memory.
 *
 * Therefore, guard BIFs that are used fairly frequently and can
 * be implemented entirely in assembly language without any calls to
 * C function are implemented in this source file.
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

/* Raise a badarg exception for the given MFA. */
void BeamGlobalAssembler::emit_raise_badarg(const ErtsCodeMFA *mfa) {
    // TODO
}

/* ================================================================
 *  '=:='/2
 *  '=/='/2
 *  '>='/2
 *  '<'/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_is_eq_exact_shared() {
    // TODO
}

void BeamGlobalAssembler::emit_bif_is_ne_exact_shared() {
    // TODO
}

void BeamModuleAssembler::emit_cond_to_bool(arm::CondCode cc,
                                            const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_cmp_immed_to_bool(arm::CondCode cc,
                                                 const ArgSource &LHS,
                                                 const ArgSource &RHS,
                                                 const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bif_is_eq_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bif_is_ne_exact(const ArgRegister &LHS,
                                               const ArgSource &RHS,
                                               const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bif_is_ge_lt(arm::CondCode cc,
                                            const ArgSource &LHS,
                                            const ArgSource &RHS,
                                            const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bif_is_ge(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bif_is_lt(const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  and/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_and_error() {
    // TODO
}

void BeamModuleAssembler::emit_bif_and(const ArgLabel &Fail,
                                       const ArgSource &Src1,
                                       const ArgSource &Src2,
                                       const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  bit_size/1
 * ================================================================
 */
void BeamGlobalAssembler::emit_bif_bit_size_helper(Label error) {
    // TODO
}

void BeamGlobalAssembler::emit_bif_bit_size_body() {
    // TODO
}

void BeamModuleAssembler::emit_bif_bit_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  byte_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_byte_size_body() {
    // TODO
}

void BeamModuleAssembler::emit_bif_byte_size(const ArgLabel &Fail,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  element/2
 * ================================================================
 */

/* ARG1 = Position (1-based)
 * ARG2 = Tuple
 *
 * Will return the result in ARG1, or jump to the label `fail` if
 * the operation fails.
 */
void BeamGlobalAssembler::emit_bif_element_helper(Label fail) {
    // TODO
}

void BeamGlobalAssembler::emit_bif_element_body_shared() {
    // TODO
}

void BeamGlobalAssembler::emit_bif_element_guard_shared() {
    // TODO
}

void BeamGlobalAssembler::emit_handle_element_error_shared() {
    // TODO
}

void BeamModuleAssembler::emit_bif_element(const ArgLabel &Fail,
                                           const ArgSource &Pos,
                                           const ArgSource &Tuple,
                                           const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  hd/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_hd_error() {
    // TODO
}

void BeamModuleAssembler::emit_bif_hd(const ArgSource &Src,
                                      const ArgRegister &Hd) {
    // TODO
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
    // TODO
}

/* ================================================================
 *  map_get/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_get_badmap() {
    // TODO
}

void BeamGlobalAssembler::emit_handle_map_get_badkey() {
    // TODO
}

void BeamModuleAssembler::emit_bif_map_get(const ArgLabel &Fail,
                                           const ArgSource &Key,
                                           const ArgSource &Src,
                                           const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  map_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_map_size_error() {
    // TODO
}

void BeamModuleAssembler::emit_bif_map_size(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  min/2
 *  max/2
 * ================================================================
 */

void BeamModuleAssembler::emit_bif_min_max(arm::CondCode cc,
                                           const ArgSource &LHS,
                                           const ArgSource &RHS,
                                           const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bif_max(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_bif_min(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  node/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_node_error() {
    // TODO
}

void BeamModuleAssembler::emit_bif_node(const ArgLabel &Fail,
                                        const ArgRegister &Src,
                                        const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  not/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_not_error() {
    // TODO
}

void BeamModuleAssembler::emit_bif_not(const ArgLabel &Fail,
                                       const ArgRegister &Src,
                                       const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  or/2
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_or_error() {
    // TODO
}

void BeamModuleAssembler::emit_bif_or(const ArgLabel &Fail,
                                      const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgRegister &Dst) {
    // TODO
}

/* ================================================================
 *  tl/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_handle_tl_error() {
    // TODO
}

void BeamModuleAssembler::emit_bif_tl(const ArgSource &Src,
                                      const ArgRegister &Tl) {
    // TODO
}

/* ================================================================
 *  tuple_size/1
 * ================================================================
 */

void BeamGlobalAssembler::emit_bif_tuple_size_helper(Label fail) {
    // TODO
}

void BeamGlobalAssembler::emit_bif_tuple_size_body() {
    // TODO
}

void BeamGlobalAssembler::emit_bif_tuple_size_guard() {
    // TODO
}

void BeamModuleAssembler::emit_bif_tuple_size(const ArgLabel &Fail,
                                              const ArgRegister &Src,
                                              const ArgRegister &Dst) {
    // TODO
}
