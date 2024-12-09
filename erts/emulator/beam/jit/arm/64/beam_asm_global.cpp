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
    emit_enter_runtime_frame();

    /* Convert ARG3 to words needed and move it to the correct argument slot.
     *
     * Note that we cancel out the S_RESERVED that we added in the GC check, as
     * the GC routines handle that separately and we don't want it to be added
     * twice. */
    a.sub(ARG2, ARG3, HTOP);
    a.lsr(ARG2, ARG2, imm(3));
    a.sub(ARG2, ARG2, imm(S_RESERVED));

    /* Save our return address in c_p->i so we can tell where we crashed if we
     * did so during GC. */
    a.str(a64::x30, arm::Mem(c_p, offsetof(Process, i)));

    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();

    a.mov(ARG1, c_p);
    /* ARG2 is already loaded. */
    load_x_reg_array(ARG3);
    /* ARG4 (live registers) is already loaded. */
    a.mov(ARG5.w(), FCALLS);
    runtime_call<5>(erts_garbage_collect_nobump);
    a.sub(FCALLS, FCALLS, ARG1.w());

    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs>();
    emit_leave_runtime_frame();

    a.ldr(TMP1.w(), arm::Mem(c_p, offsetof(Process, state.value)));
    a.tst(TMP1, imm(ERTS_PSFLG_EXITING));
    a.b_ne(labels[do_schedule]);

    a.ret(a64::x30);
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
    int export_offset = offsetof(Export, info.mfa);

    a.ldr(ARG1, arm::Mem(c_p, offsetof(Process, current)));
    a.sub(ARG1, ARG1, export_offset);

    emit_leave_erlang_frame();

    branch(emit_setup_dispatchable_call(ARG1));
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
    Label call_bif = a.newLabel(), error_handler = a.newLabel();

    /* What are we supposed to do? */
    a.ldr(TMP1, arm::Mem(ARG1, offsetof(Export, trampoline.common.op)));

    /* We test the generic bp first as it is most likely to be triggered in a
     * loop. */
    a.cmp(TMP1, imm(op_i_generic_breakpoint));
    a.b_eq(labels[generic_bp_global]);

    a.cmp(TMP1, imm(op_call_bif_W));
    a.b_eq(call_bif);

    a.cmp(TMP1, imm(op_call_error_handler));
    a.b_eq(error_handler);

    /* Must never happen. */
    a.udf(0xffff);

    a.bind(call_bif);
    {
        /* Emulate a `call_bif` instruction.
         *
         * Note that we don't check reductions: yielding here is very tricky
         * and error-prone, and there's little point in doing so as we can only
         * land here directly after being scheduled in. */
        ssize_t func_offset = offsetof(Export, trampoline.bif.address);

        lea(ARG2, arm::Mem(ARG1, offsetof(Export, info.mfa)));
        a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
        a.ldr(ARG4, arm::Mem(ARG1, func_offset));

        /* `call_bif_shared` assumes that the return address has been pushed to
         * the stack as part of the prologue, so we have to do that manually
         * now. */
        emit_enter_erlang_frame();
        a.b(labels[call_bif_shared]);
    }

    a.bind(error_handler);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime<Update::eReductions | Update::eHeapAlloc |
                           Update::eXRegs>();

        lea(ARG2, arm::Mem(ARG1, offsetof(Export, info.mfa)));
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG3);
        mov_imm(ARG4, am_undefined_function);
        runtime_call<4>(call_error_handler);

        /* If there is no error_handler, any number of X registers
         * can be live. */
        emit_leave_runtime<Update::eReductions | Update::eHeapAlloc |
                           Update::eXRegs>();
        emit_leave_runtime_frame();

        a.cbz(ARG1, labels[process_exit]);

        branch(emit_setup_dispatchable_call(ARG1));
    }
}

/*
 * Get the error address implicitly by calling the shared fragment and using
 * the return address as the error address.
 */
void BeamModuleAssembler::emit_raise_exception() {
    emit_raise_exception(nullptr);
}

void BeamModuleAssembler::emit_raise_exception(const ErtsCodeMFA *exp) {
    if (exp) {
        a.ldr(ARG4, embed_constant(exp, disp32K));
        fragment_call(ga->get_raise_exception());
    } else {
        fragment_call(ga->get_raise_exception_null_exp());
    }

    /* `line` instructions need to know the latest offset that may throw an
     * exception. See the `line` instruction for details. */
    last_error_offset = a.offset();
}

void BeamModuleAssembler::emit_raise_exception(Label I,
                                               const ErtsCodeMFA *exp) {
    a.adr(ARG2, I);

    if (exp) {
        a.ldr(ARG4, embed_constant(exp, disp32K));
        a.b(resolve_fragment(ga->get_raise_exception_shared(), disp128MB));
    } else {
        a.b(resolve_fragment(ga->get_raise_exception_null_exp(), disp128MB));
    }
}

void BeamGlobalAssembler::emit_process_exit() {
    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.mov(ARG1, c_p);
    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);
    load_x_reg_array(ARG3);
    runtime_call<4>(handle_error);

    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.cbz(ARG1, labels[do_schedule]);
    a.udf(0xdead);
}

/* You must have already done emit_leave_runtime_frame()! */
void BeamGlobalAssembler::emit_raise_exception_null_exp() {
    a.mov(ARG4, ZERO);
    a.mov(ARG2, a64::x30);
    a.b(labels[raise_exception_shared]);
}

/* You must have already done emit_leave_runtime_frame()! */
void BeamGlobalAssembler::emit_raise_exception() {
    a.mov(ARG2, a64::x30);
    a.b(labels[raise_exception_shared]);
}

void BeamGlobalAssembler::emit_raise_exception_shared() {
    Label crash = a.newLabel();

    /* Push a fake CP to ensure that we can handle a topmost frame
     * with `catch` and an instruction raising and exception.
     *
     * The fake CP is discarded by handle_error() before jumping to
     * a catch handler, and is ignored as a duplicate in stack
     * traces because it's equal to the error address. */
    a.str(ARG2, arm::Mem(E, -8).pre());

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs>();

    /* The error address must be a valid CP or NULL. */
    a.tst(ARG2, imm(_CPMASK));
    a.b_ne(crash);

    /* ARG2 and ARG4 must be set prior to jumping here! */
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG3);
    runtime_call<4>(handle_error);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs>();

    a.cbz(ARG1, labels[do_schedule]);

    /* XREG0 = THE_NON_VALUE
     * XREG1 = class
     * XREG2 = error reason/thrown value
     * XREG3 = raw stacktrace. */
    a.br(ARG1);

    a.bind(crash);
    a.udf(0xbad);
}

void BeamModuleAssembler::emit_proc_lc_unrequire(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    a.mov(ARG1, c_p);
    mov_imm(ARG2, ERTS_PROC_LOCK_MAIN);
    runtime_call<2>(erts_proc_lc_unrequire_lock);
#endif
}

void BeamModuleAssembler::emit_proc_lc_require(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    a.mov(ARG1, c_p);
    mov_imm(ARG2, ERTS_PROC_LOCK_MAIN);
    runtime_call<4>(erts_proc_lc_require_lock);
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
