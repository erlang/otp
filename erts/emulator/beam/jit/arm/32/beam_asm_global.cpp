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

#define ERTS_BEAM_ASM_GLOBAL_WANT_STATIC_DEFS
#include "beam_asm.hpp"
#undef ERTS_BEAM_ASM_GLOBAL_WANT_STATIC_DEFS

using namespace asmjit;

extern "C"
{
#include "bif.h"
#include "beam_common.h"
}

BeamGlobalAssembler::BeamGlobalAssembler(JitAllocator *allocator)
        : BeamAssembler("beam_asm_global") {
    labels.reserve(emitPtrs.size());

    /* These labels are defined up-front so global functions can refer to each
     * other freely without any order dependencies. */
    for (auto val : labelNames) {
        std::string name = "global::" + val.second;
        labels[val.first] = a.newNamedLabel(name.c_str());
    }

    /* Emit all of the code and bind all of the labels */
    for (auto val : emitPtrs) {
        a.align(AlignMode::kCode, 8);
        a.bind(labels[val.first]);
        /* This funky syntax calls the function pointer within this instance
         * of BeamGlobalAssembler */
        (this->*val.second)();
    }

    {
        const void *executable_region;
        void *writable_region;

        BeamAssembler::codegen(allocator, &executable_region, &writable_region);
        VirtMem::flushInstructionCache((void *)executable_region,
                                       code.codeSize());
        VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadExecute);
    }

    std::vector<AsmRange> ranges;

    ranges.reserve(emitPtrs.size());

    for (auto val : emitPtrs) {
        ErtsCodePtr start = (ErtsCodePtr)getCode(labels[val.first]);
        ErtsCodePtr stop;

        if (val.first + 1 < emitPtrs.size()) {
            stop = (ErtsCodePtr)getCode(labels[(GlobalLabels)(val.first + 1)]);
        } else {
            stop = (ErtsCodePtr)((char *)getBaseAddress() + code.codeSize());
        }

        ranges.push_back({.start = start,
                          .stop = stop,
                          .name = code.labelEntry(labels[val.first])->name()});
    }

    (void)beamasm_metadata_insert("global",
                                  (ErtsCodePtr)getBaseAddress(),
                                  code.codeSize(),
                                  ranges);

    /* `this->get_xxx` are populated last to ensure that we crash if we use
     * them instead of labels in global code. */
    for (auto val : labelNames) {
        ptrs[val.first] = (fptr)getCode(labels[val.first]);
    }
}

/* ARG3 = (HTOP + S_RESERVED + bytes needed) !!
 * ARG4 = Live registers */
void BeamGlobalAssembler::emit_garbage_collect() {
    // TODO
}

/* Handles trapping to exports from C code, setting registers up in the same
 * manner a normal call_ext instruction would so that save_calls, tracing, and
 * so on will work.
 *
 * Our return address is on the stack as we always come here from a BIF, so we
 * must pop it into LR (x30) to convert this into an ordinary call. The
 * callee will then push LR to the stack in its prologue, cancelling this out.
 *
 * Assumes that c_p->current points into the MFA of an export entry. */
void BeamGlobalAssembler::emit_bif_export_trap() {
    // TODO
}

/* Handles export breakpoints, error handler, jump tracing, and so on.
 *
 * We must be careful with LR (x30) and the stack as this runs between the
 * caller and callee, and the latter pushes LR to the stack as part of its
 * prologue.
 *
 * ARG1 = export entry
 */
void BeamGlobalAssembler::emit_export_trampoline() {
    // TODO
}

/*
 * Get the error address implicitly by calling the shared fragment and using
 * the return address as the error address.
 */
void BeamModuleAssembler::emit_raise_exception() {
    // TODO
}

void BeamModuleAssembler::emit_raise_exception(const ErtsCodeMFA *exp) {
    // TODO
}

void BeamModuleAssembler::emit_raise_exception(Label I,
                                               const ErtsCodeMFA *exp) {
    // TODO
}

void BeamGlobalAssembler::emit_process_exit() {
    // TODO
}

/* You must have already done emit_leave_runtime_frame()! */
void BeamGlobalAssembler::emit_raise_exception_null_exp() {
    // TODO
}

/* You must have already done emit_leave_runtime_frame()! */
void BeamGlobalAssembler::emit_raise_exception() {
    // TODO
}

void BeamGlobalAssembler::emit_raise_exception_shared() {
    // TODO
}

void BeamModuleAssembler::emit_proc_lc_unrequire(void) {
    // TODO
}

void BeamModuleAssembler::emit_proc_lc_require(void) {
    // TODO
}

extern "C"
{
    /* GDB puts a breakpoint in this function.
     *
     * Has to be on another file than the caller as otherwise gcc may
     * optimize away the call. */
    void ERTS_NOINLINE __jit_debug_register_code(void);
    void ERTS_NOINLINE __jit_debug_register_code(void) {
    }
}
