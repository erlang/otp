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
#include "bif.h"
#include "code_ix.h"
#include "erl_proc_sig_queue.h"
#ifdef USE_VM_PROBES
#    include "dtrace-wrapper.h"
#endif
}

void BeamModuleAssembler::emit_recv_marker_reserve(const ArgRegister &Dst) {
    // TODO
}

void BeamModuleAssembler::emit_recv_marker_bind(const ArgRegister &Marker,
                                                const ArgRegister &Reference) {
    // TODO
}

void BeamModuleAssembler::emit_recv_marker_clear(const ArgRegister &Reference) {
    // TODO
}

void BeamModuleAssembler::emit_recv_marker_use(const ArgRegister &Reference) {
    // TODO
}

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_lc_proc_sig_receive_helper(Process *c_p,
                                    int fcalls,
                                    int neg_o_reds,
                                    ErtsMessage **msgpp,
                                    int *get_outp) {
    int res;
    // TODO
    return res;
}
#endif

void BeamGlobalAssembler::emit_i_loop_rec_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_loop_rec(const ArgLabel &Wait) {
    // TODO
}

void BeamModuleAssembler::emit_remove_message() {
    // TODO
}

void BeamModuleAssembler::emit_loop_rec_end(const ArgLabel &Dest) {
    // TODO
}

void BeamModuleAssembler::emit_wait_unlocked(const ArgLabel &Dest) {
    // TODO
}

void BeamModuleAssembler::emit_wait_locked(const ArgLabel &Dest) {
    // TODO
}

void BeamModuleAssembler::emit_wait_timeout_unlocked(const ArgSource &Src,
                                                     const ArgLabel &Dest) {
    // TODO
}

void BeamModuleAssembler::emit_wait_timeout_locked(const ArgSource &Src,
                                                   const ArgLabel &Dest) {
    // TODO
}

void BeamModuleAssembler::emit_timeout_locked() {
    // TODO
}

void BeamModuleAssembler::emit_timeout() {
    // TODO
}
