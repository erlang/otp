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
#include "bif.h"
#include "beam_common.h"
#include "code_ix.h"
#include "export.h"
}

#undef x

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
static Process *erts_debug_schedule(ErtsSchedulerData *esdp,
                                    Process *c_p,
                                    int calls) {
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = erts_schedule(esdp, c_p, calls);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);
    return c_p;
}
#endif

/* void process_main(ErtsSchedulerData *esdp); */
void BeamGlobalAssembler::emit_process_main() {
    Label context_switch_local = a.newLabel(),
          context_switch_simplified_local = a.newLabel(),
          do_schedule_local = a.newLabel(), schedule_next = a.newLabel();

    const arm::Mem start_time_i =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time_i));
    const arm::Mem start_time =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time));

    // TODO
    ASSERT(false);

    a.bind(do_schedule_local);
    a.bind(context_switch_local);
    comment("Context switch, unknown arity/MFA");
    a.bind(context_switch_simplified_local);
    comment("Context switch, known arity and MFA");
    a.bind(schedule_next);
    comment("schedule_next");
    /* Processes may jump to the exported entry points below, executing on the
     * Erlang stack when entering. These are separate from the `_local` labels
     * above as we don't want to worry about which stack we're on when the
     * cases overlap. */

    /* `ga->get_context_switch()`
     *
     * The *next* instruction pointer is provided in ARG3, and must be preceded
     * by an ErtsCodeMFA.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[context_switch]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

        a.b(context_switch_local);
    }

    /* `ga->get_context_switch_simplified()`
     *
     * The next instruction pointer is provided in ARG3, which does not need to
     * point past an ErtsCodeMFA as the process structure has already been
     * updated.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[context_switch_simplified]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

        a.b(context_switch_simplified_local);
    }

    /* `ga->get_do_schedule()`
     *
     * `c_p->i` must be set prior to jumping here.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[do_schedule]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

        a.b(do_schedule_local);
    }
}
