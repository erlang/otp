/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
#include "beam_common.h"
#include "code_ix.h"
#include "erl_bif_table.h"
#include "erl_nfunc_sched.h"
#include "bif.h"
#include "erl_msacc.h"
}

void BeamModuleAssembler::ubif_comment(const ArgWord &Bif) {
    // TODO
    ASSERT(false);
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in ARG1 (will be THE_NON_VALUE if the BIF call failed). */
void BeamGlobalAssembler::emit_i_bif_guard_shared() {
    // TODO
    ASSERT(false);
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bif_body_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_bif1(const ArgSource &Src1,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_bif2(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_bif3(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgSource &Src3,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_bif(const ArgLabel &Fail,
                                     const ArgWord &Bif,
                                     const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

/*
 * Emit code for guard BIFs that can't fail (e.g. is_list/1).  We
 * don't need to test for failure.
 */

void BeamModuleAssembler::emit_nofail_bif1(const ArgSource &Src1,
                                           const ArgWord &Bif,
                                           const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_nofail_bif2(const ArgSource &Src1,
                                           const ArgSource &Src2,
                                           const ArgWord &Bif,
                                           const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_length_setup(const ArgLabel &Fail,
                                              const ArgWord &Live,
                                              const ArgSource &Src) {
    // TODO
    ASSERT(false);
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_common(Label fail, int state_size) {
    // TODO
    ASSERT(false);
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_body_shared() {
    // TODO
    ASSERT(false);
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in ARG. Error is indicated by THE_NON_VALUE. */
void BeamGlobalAssembler::emit_i_length_guard_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_length(const ArgLabel &Fail,
                                        const ArgWord &Live,
                                        const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)

static Eterm debug_call_light_bif(Process *c_p,
                                  Eterm *reg,
                                  ErtsCodePtr I,
                                  ErtsBifFunc vbf) {
    Eterm result;
    // TODO
    ASSERT(false);
    return result;
}
#endif

/* It is important that the below code is as optimized as possible.
 * When doing any changes, make sure to look at the estone bif_dispatch
 * benchmark to make sure you don't introduce any regressions.
 *
 * ARG3 = entry
 * ARG4 = export entry
 * ARG8 = BIF pointer
 */
void BeamGlobalAssembler::emit_call_light_bif_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_call_light_bif(const ArgWord &Bif,
                                              const ArgExport &Exp) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_send() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_nif_start() {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_bif_nif_epilogue(void) {
    // TODO
    ASSERT(false);
}

/* Used by call_bif, dispatch_bif, and export_trampoline.
 *
 * Note that we don't check reductions here as we may have jumped here through
 * interpreted code (e.g. an ErtsNativeFunc or export entry) and it's very
 * tricky to yield back. Reductions are checked in module code instead.
 *
 * ARG2 = BIF MFA
 * ARG3 = I (rip), doesn't need to point past an MFA
 * ARG4 = function to be called */
void BeamGlobalAssembler::emit_call_bif_shared(void) {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_dispatch_bif(void) {
    // TODO
    ASSERT(false);
}

/* This is only used for opcode compatibility with the interpreter, it's never
 * actually called. */
void BeamModuleAssembler::emit_call_bif(const ArgWord &Func) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_call_bif_mfa(const ArgAtom &M,
                                            const ArgAtom &F,
                                            const ArgWord &A) {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_call_nif_early() {
    // TODO
    ASSERT(false);
}

/* Used by call_nif, call_nif_early, and dispatch_nif.
 *
 * Note that we don't check reductions here as we may have jumped here through
 * interpreted code (e.g. an ErtsNativeFunc or export entry) and it's very
 * tricky to yield back. Reductions are checked in module code instead.
 *
 * ARG3 = current I, just past the end of an ErtsCodeInfo. */
void BeamGlobalAssembler::emit_call_nif_shared(void) {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_call_nif_yield_helper() {
    // TODO
    ASSERT(false);
}

/* WARNING: This stub is memcpy'd, so all code herein must be explicitly
 * position-independent. */
void BeamModuleAssembler::emit_call_nif(const ArgWord &Func,
                                        const ArgWord &NifMod,
                                        const ArgWord &DirtyFunc) {
    // TODO
    ASSERT(false);
}

static ErtsCodePtr get_on_load_address(Process *c_p, Eterm module) {
    // TODO
    ASSERT(false);
    return NULL;
}

/* Implements the internal and undocumented erlang:call_on_load_function/1,
 * which is very tricky to implement as a BIF. */
void BeamModuleAssembler::emit_i_call_on_load_function() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_load_nif() {
    // TODO
    ASSERT(false);
}
