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

extern "C"
{
#include "erl_bif_table.h"
#include "big.h"
}

void BeamModuleAssembler::emit_add_sub_types(bool is_small_result,
                                             const ArgSource &LHS,
                                             const a32::Gp lhs_reg,
                                             const ArgSource &RHS,
                                             const a32::Gp rhs_reg,
                                             const Label next) {
    // TODO
}

void BeamModuleAssembler::emit_are_both_small(const ArgSource &LHS,
                                              const a32::Gp lhs_reg,
                                              const ArgSource &RHS,
                                              const a32::Gp rhs_reg,
                                              const Label next) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_plus_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_plus(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = Src
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_unary_minus_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_unary_minus(const ArgLabel &Fail,
                                             const ArgWord &Live,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_minus_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_minus(const ArgLabel &Fail,
                                       const ArgWord &Live,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    // TODO
}

/*
 * Create a bignum from a the 128-bit product of two smalls shifted
 * left _TAG_IMMED1_SIZE bits.
 *
 * ARG1 = low 64 bits
 * TMP2 = high 64 bits
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_int128_to_big_shared() {
    // TODO
}

/* ARG2 = Src1
 * ARG3 = Src2
 * ARG4 = Src4
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_mul_add_body_shared() {
    // TODO
}

/* ARG2 = Src1
 * ARG3 = Src2
 * ARG4 = Src4
 *
 * The result is returned in ARG1 (set to THE_NON_VALUE if
 * the call failed).
 */
void BeamGlobalAssembler::emit_mul_add_guard_shared() {
    // TODO
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_mul_body_shared() {
    // TODO
}

/* ARG2 = Src1
 * ARG3 = Src2
 *
 * The result is returned in ARG1 (set to THE_NON_VALUE if
 * the call failed).
 */
void BeamGlobalAssembler::emit_mul_guard_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_mul_add(const ArgLabel &Fail,
                                         const ArgSource &Src1,
                                         const ArgSource &Src2,
                                         const ArgSource &Src3,
                                         const ArgSource &Src4,
                                         const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * Quotient is returned in ARG1, remainder in ARG2.
 * Error is indicated by the Z flag.
 */
void BeamGlobalAssembler::emit_int_div_rem_guard_shared() {
    // TODO
}

/* ARG2 = LHS
 * ARG3 = RHS
 * ARG4 = error MFA
 *
 * Quotient is returned in ARG1, remainder in ARG2.
 */
void BeamGlobalAssembler::emit_int_div_rem_body_shared() {
}

void BeamModuleAssembler::emit_div_rem_literal(Sint divisor,
                                               const ArgSource &Dividend,
                                               a32::Gp dividend,
                                               a32::Gp quotient,
                                               a32::Gp remainder,
                                               const Label &generic,
                                               bool need_div,
                                               bool need_rem) {
    // TODO
}

void BeamModuleAssembler::emit_div_rem(const ArgLabel &Fail,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ErtsCodeMFA *error_mfa,
                                       const ArgRegister &Quotient,
                                       const ArgRegister &Remainder,
                                       bool need_div,
                                       bool need_rem) {
    // TODO
}

void BeamModuleAssembler::emit_i_rem_div(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Remainder,
                                         const ArgRegister &Quotient) {
    // TODO
}

void BeamModuleAssembler::emit_i_div_rem(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Quotient,
                                         const ArgRegister &Remainder) {
    // TODO
}

void BeamModuleAssembler::emit_i_int_div(const ArgLabel &Fail,
                                         const ArgWord &Live,
                                         const ArgSource &LHS,
                                         const ArgSource &RHS,
                                         const ArgRegister &Quotient) {
    // TODO
}

void BeamModuleAssembler::emit_i_rem(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Remainder) {
    // TODO
}

void BeamModuleAssembler::emit_i_m_div(const ArgLabel &Fail,
                                       const ArgWord &Live,
                                       const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
template<typename T>
void BeamGlobalAssembler::emit_bitwise_fallback_body(T(*func_ptr),
                                                     const ErtsCodeMFA *mfa) {
    // TODO
}

void BeamGlobalAssembler::emit_i_band_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_band(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * Result is returned in RET.
 */
void BeamGlobalAssembler::emit_i_bor_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_bor(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bxor_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_bxor(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &LHS,
                                      const ArgSource &RHS,
                                      const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG1 = Src
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1. Error is indicated by
 * THE_NON_VALUE.
 */
void BeamGlobalAssembler::emit_i_bnot_guard_shared() {
    // TODO
}

/*
 * ARG1 = Src
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bnot_body_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_bnot(const ArgLabel &Fail,
                                      const ArgWord &Live,
                                      const ArgSource &Src,
                                      const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bsr_body_shared() {
    static const ErtsCodeMFA bif_mfa = {am_erlang, am_bsr, 2};
    emit_bitwise_fallback_body(erts_bsr, &bif_mfa);
}

void BeamModuleAssembler::emit_i_bsr(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    // TODO
}

/*
 * ARG2 = LHS
 * ARG3 = RHS
 *
 * The module code must have executed emit_enter_runtime()
 * before calling this function.
 *
 * The result is returned in ARG1.
 */
void BeamGlobalAssembler::emit_i_bsl_body_shared() {
    // TODO
}

static int count_leading_zeroes(UWord value) {
    const int word_bits = sizeof(value) * CHAR_BIT;

    if (value == 0) {
        return word_bits;
    }

    return Support::clz(value);
}

void BeamModuleAssembler::emit_i_bsl(const ArgLabel &Fail,
                                     const ArgWord &Live,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS,
                                     const ArgRegister &Dst) {
    // TODO
}
