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
#include "erl_bif_table.h"
#include "beam_bp.h"
};

/* This function is jumped to from the export entry of a function.
 *
 * ARG1 = export entry */
void BeamGlobalAssembler::emit_generic_bp_global() {
    // TODO
    ASSERT(false);
}

/* This function is called from the module header, which is in turn called from
 * the prologue of the traced function. As such, the real return address is at
 * SP+8 rather than LR (x30).
 *
 * See beam_asm.h for more details */
void BeamGlobalAssembler::emit_generic_bp_local() {
    // TODO
    ASSERT(false);
}

/* This function is called from the module header which is called from the
 * prologue of the function to trace. See beam_asm.h for more details
 *
 * The only place that we can come to here is from generic_bp_local */
void BeamGlobalAssembler::emit_debug_bp() {
    // TODO
    ASSERT(false);
}

static void return_trace(Process *c_p,
                         ErtsCodeMFA *mfa,
                         Eterm val,
                         ErtsTracer tracer,
                         Eterm session_id) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_return_trace() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_call_trace_return() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_return_to_trace() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_hibernate() {
    // TODO
    ASSERT(false);
}
