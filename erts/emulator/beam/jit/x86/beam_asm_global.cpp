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

#ifndef WIN32
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
#endif

    /* `this->get_xxx` are populated last to ensure that we crash if we use them
     * instead of labels in global code. */

    for (auto val : labelNames) {
        ptrs[val.first] = (fptr)getCode(labels[val.first]);
    }
}

/* ARG3 = (HTOP + S_RESERVED + bytes needed) !!
 * ARG4 = Live registers */
void BeamGlobalAssembler::emit_garbage_collect() {
    Label exiting = a.newLabel();

    emit_enter_frame();

    /* Convert ARG3 to words needed and move it to the correct argument slot.
     *
     * Note that we cancel out the S_RESERVED that we added in the GC check, as
     * the GC routines handle that separately and we don't want it to be added
     * twice. */
    a.sub(ARG3, HTOP);
    a.shr(ARG3, imm(3));
    a.lea(ARG2, x86::qword_ptr(ARG3, -S_RESERVED));

    /* Save our return address in c_p->i so we can tell where we crashed if we
     * do so during GC. */
    if (erts_frame_layout == ERTS_FRAME_LAYOUT_RA) {
        a.mov(RET, x86::qword_ptr(x86::rsp));
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);
        a.mov(RET, x86::qword_ptr(x86::rsp, 8));
    }

    a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), RET);

    emit_enter_runtime<Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG3);
    a.mov(ARG5d, FCALLS);
    runtime_call<5>(erts_garbage_collect_nobump);
    a.sub(FCALLS, RETd);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

#ifdef WIN32
    a.mov(ARG1d, x86::dword_ptr(c_p, offsetof(Process, state.value)));
#else
    a.mov(ARG1d, x86::dword_ptr(c_p, offsetof(Process, state.counter)));
#endif
    a.test(ARG1d, imm(ERTS_PSFLG_EXITING));
    a.short_().jne(exiting);

    emit_leave_frame();
    a.ret();

    a.bind(exiting);
    emit_unwind_frame();
    a.jmp(labels[do_schedule]);
}

/* Handles trapping to exports from C code, setting registers up in the same
 * manner a normal call_ext instruction would so that save_calls, tracing, and
 * so on will work.
 *
 * Assumes that c_p->current points into the MFA of an export entry. */
void BeamGlobalAssembler::emit_bif_export_trap() {
    a.mov(RET, x86::qword_ptr(c_p, offsetof(Process, current)));
    a.sub(RET, imm(offsetof(Export, info.mfa)));

    emit_leave_frame();
    a.jmp(emit_setup_dispatchable_call(RET));
}

/* Handles export breakpoints, error handler, jump tracing, and so on.
 *
 * RET = export entry */
void BeamGlobalAssembler::emit_export_trampoline() {
    Label call_bif = a.newLabel(), error_handler = a.newLabel();

    /* What are we supposed to do? */
    a.mov(ARG1, x86::qword_ptr(RET, offsetof(Export, trampoline.common.op)));

    /* We test the generic bp first as it is most likely to be triggered in a
     * loop. */
    a.cmp(ARG1, imm(op_i_generic_breakpoint));
    a.je(labels[generic_bp_global]);

    a.cmp(ARG1, imm(op_call_bif_W));
    a.je(call_bif);

    a.cmp(ARG1, imm(op_call_error_handler));
    a.je(error_handler);

    /* Must never happen. */
    comment("Unexpected export trampoline op");
    a.ud2();

    a.bind(call_bif);
    {
        /* Emulate a `call_bif` instruction.
         *
         * Note that we don't check reductions: yielding here is very tricky
         * and error-prone, and there's little point in doing so as we can only
         * land here directly after being scheduled in. */
        ssize_t func_offset = offsetof(Export, trampoline.bif.address);

        a.lea(ARG2, x86::qword_ptr(RET, offsetof(Export, info.mfa)));
        a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
        a.mov(ARG4, x86::qword_ptr(RET, func_offset));

        emit_enter_frame();
        a.jmp(labels[call_bif_shared]);
    }

    a.bind(error_handler);
    {
        emit_enter_frame();
        emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();

        a.mov(ARG1, c_p);
        a.lea(ARG2, x86::qword_ptr(RET, offsetof(Export, info.mfa)));
        load_x_reg_array(ARG3);
        mov_imm(ARG4, am_undefined_function);
        runtime_call<4>(call_error_handler);

        emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();

        a.test(RET, RET);
        a.je(labels[process_exit]);

        emit_leave_frame();
        a.jmp(emit_setup_dispatchable_call(RET));
    }
}

/*
 * Get the error address implicitly by calling the shared fragment and using
 * the return address as the error address.
 */
void BeamModuleAssembler::emit_raise_exception() {
    safe_fragment_call(ga->get_raise_exception_null_exp());

    /* `line` instructions need to know the latest offset that may throw an
     * exception. See the `line` instruction for details. */
    last_error_offset = a.offset();
}

void BeamModuleAssembler::emit_raise_exception(const ErtsCodeMFA *exp) {
    mov_imm(ARG4, exp);
    safe_fragment_call(ga->get_raise_exception());

    /* `line` instructions need to know the latest offset that may throw an
     * exception. See the `line` instruction for details. */
    last_error_offset = a.offset();
}

void BeamModuleAssembler::emit_raise_exception(Label I,
                                               const ErtsCodeMFA *exp) {
    a.lea(ARG2, x86::qword_ptr(I));
    emit_raise_exception(ARG2, exp);
}

void BeamModuleAssembler::emit_raise_exception(x86::Gp I,
                                               const ErtsCodeMFA *exp) {
    if (I != ARG2) {
        a.mov(ARG2, I);
    }

    mov_imm(ARG4, exp);

#ifdef NATIVE_ERLANG_STACK
    /* The CP must be reserved for try/catch to work, so we'll fake a call with
     * the return address set to the error address. */
    a.push(ARG2);

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
#    ifdef ERLANG_FRAME_POINTERS
        a.push(frame_pointer);
#    endif
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
    }
#endif

    a.jmp(resolve_fragment(ga->get_raise_exception_shared()));
}

void BeamGlobalAssembler::emit_process_exit() {
    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);
    load_x_reg_array(ARG3);
    runtime_call<4>(handle_error);

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.test(RET, RET);
    a.je(labels[do_schedule]);
    comment("End of process");
    a.ud2();
}

void BeamGlobalAssembler::emit_raise_exception_null_exp() {
    mov_imm(ARG4, 0);
    a.jmp(labels[raise_exception]);
}

/* Helper function for throwing exceptions from global fragments.
 *
 * Assumes that the next item on the _machine stack_ is a return address: we
 * must not jump here while in a frame. */
void BeamGlobalAssembler::emit_raise_exception() {
    /* We must align the return address to make it a proper tagged CP, in case
     * we were called with `safe_fragment_call`. This is safe because we will
     * never actually return to the return address. */
    a.pop(ARG2);
    a.and_(ARG2, imm(~_CPMASK));

#ifdef NATIVE_ERLANG_STACK
    a.push(ARG2);

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
#    ifdef ERLANG_FRAME_POINTERS
        a.push(frame_pointer);
#    endif
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
    }
#endif

    a.jmp(labels[raise_exception_shared]);
}

void BeamGlobalAssembler::emit_raise_exception_shared() {
    Label crash = a.newLabel();

    emit_enter_runtime<Update::eHeapAlloc>();

    /* The error address must be a valid CP or NULL. */
    a.test(ARG2d, imm(_CPMASK));
    a.short_().jne(crash);

    /* ARG2 and ARG4 must be set prior to jumping here! */
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG3);
    runtime_call<4>(handle_error);

    emit_leave_runtime<Update::eHeapAlloc>();

    a.test(RET, RET);
    a.je(labels[do_schedule]);

    a.jmp(RET);

    a.bind(crash);
    comment("Error address is not a CP or NULL or ARG2 and ARG4 are unset");
    a.ud2();
}

void BeamModuleAssembler::emit_proc_lc_unrequire(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    emit_assert_runtime_stack();

    a.mov(ARG1, c_p);
    a.mov(ARG2, imm(ERTS_PROC_LOCK_MAIN));
    a.mov(TMP_MEM1q, RET);
    runtime_call<2>(erts_proc_lc_unrequire_lock);
    a.mov(RET, TMP_MEM1q);
#endif
}

void BeamModuleAssembler::emit_proc_lc_require(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    emit_assert_runtime_stack();

    a.mov(ARG1, c_p);
    a.mov(ARG2, imm(ERTS_PROC_LOCK_MAIN));
    a.mov(TMP_MEM1q, RET);
    runtime_call<4>(erts_proc_lc_require_lock);
    a.mov(RET, TMP_MEM1q);
#endif
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
