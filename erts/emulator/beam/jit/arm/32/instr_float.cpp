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
#include "big.h"
}

/* Checks whether d0 contains a finite value.
 *
 * Clobbers d30 and d31. */
void BeamGlobalAssembler::emit_check_float_error() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_float_instr(uint32_t instId,
                                           const ArgFRegister &LHS,
                                           const ArgFRegister &RHS,
                                           const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}

/* * * * */

void BeamModuleAssembler::emit_fload(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_fstore(const ArgFRegister &Src,
                                      const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

/* ARG1 = source term */
void BeamGlobalAssembler::emit_fconv_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_fconv(const ArgSource &Src,
                                     const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_fadd(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_fsub(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_fmul(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_fdiv(const ArgFRegister &LHS,
                                      const ArgFRegister &RHS,
                                      const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_fnegate(const ArgFRegister &Src,
                                         const ArgFRegister &Dst) {
    // TODO
    ASSERT(false);
}
