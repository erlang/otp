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
#include "beam_common.h"
}

void BeamGlobalAssembler::emit_dispatch_return() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_dispatch_return() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_return() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_move_deallocate_return() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_call(const ArgLabel &CallTarget) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_call_last(const ArgLabel &CallTarget,
                                           const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_move_call_last(const ArgYRegister &Src,
                                              const ArgRegister &Dst,
                                              const ArgLabel &CallTarget,
                                              const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_call_only(const ArgLabel &CallTarget) {
    // TODO
    ASSERT(false);
}

/* Handles save_calls for remote calls. When the active code index is
 * ERTS_SAVE_CALLS_CODE_IX, all remote calls will land here.
 *
 * Export entry is in ARG1, return address is in LR (x30). Both of these must
 * be preserved since this runs between caller and callee. */
void BeamGlobalAssembler::emit_dispatch_save_calls_export() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_call_ext(const ArgExport &Exp) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_call_ext_only(const ArgExport &Exp) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_call_ext_last(const ArgExport &Exp,
                                               const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_move_call_ext_last(const ArgYRegister &Src,
                                                  const ArgRegister &Dst,
                                                  const ArgExport &Exp,
                                                  const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

static ErtsCodeMFA apply3_mfa = {am_erlang, am_apply, 3};

arm::Mem BeamModuleAssembler::emit_variable_apply(bool includeI) {
    // TODO
    ASSERT(false);
    arm::Mem m;
    return m;
}

void BeamModuleAssembler::emit_i_apply() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_apply_last(const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_apply_only() {
    // TODO
    ASSERT(false);
}

arm::Mem BeamModuleAssembler::emit_fixed_apply(const ArgWord &Arity,
                                               bool includeI) {
    // TODO
    ASSERT(false);
    arm::Mem m;
    return m;
}

void BeamModuleAssembler::emit_apply(const ArgWord &Arity) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_apply_last(const ArgWord &Arity,
                                          const ArgWord &Deallocate) {
    // TODO
    ASSERT(false);
}
