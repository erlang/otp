/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

using namespace asmjit;

extern "C"
{
#include "bif.h"
#include "beam_common.h"
}

#define STRINGIFY_(X) #X
#define STRINGIFY(X) STRINGIFY_(X)

#define DECL_EMIT(NAME) {NAME, &BeamGlobalAssembler::emit_##NAME},
const std::map<BeamGlobalAssembler::GlobalLabels, BeamGlobalAssembler::emitFptr>
        BeamGlobalAssembler::emitPtrs = {BEAM_GLOBAL_FUNCS(DECL_EMIT)};
#undef DECL_EMIT

#define DECL_LABEL_NAME(NAME) {NAME, STRINGIFY(NAME)},

const std::map<BeamGlobalAssembler::GlobalLabels, std::string>
        BeamGlobalAssembler::labelNames = {BEAM_GLOBAL_FUNCS(
                DECL_LABEL_NAME) PROCESS_MAIN_LABELS(DECL_LABEL_NAME)};
#undef DECL_LABEL_NAME

BeamGlobalAssembler::BeamGlobalAssembler() : BeamAssembler("beam_asm_global") {
    std::vector<AsmRange> ranges;

    labels.reserve(emitPtrs.size());

    /* These labels are defined up-front so global functions can refer to each
     * other freely without any order dependencies. */
    for (auto val : labelNames) {
        std::string name = "global::" + val.second;
        labels[val.first] = a.newNamedLabel(name.c_str());
    }

    /* Emit all of the code and bind all of the labels */
    for (auto val : emitPtrs) {
        a.bind(labels[val.first]);
        /* This funky syntax calls the function pointer within this instance
         * of BeamGlobalAssembler */
        (this->*val.second)();
    }

    _codegen();

    /* `this->get_xxx` are populated last to ensure that we crash if we use them
     * instead of labels in global code. */

    for (auto val : labelNames) {
        ptrs[val.first] = (fptr)getCode(labels[val.first]);
    }

#ifndef WIN32
    ranges.reserve(emitPtrs.size());

    for (auto val : emitPtrs) {
        BeamInstr *start = (BeamInstr *)getCode(labels[val.first]);
        BeamInstr *stop;
        if (val.first + 1 < emitPtrs.size())
            stop = (BeamInstr *)getCode(labels[(GlobalLabels)(val.first + 1)]);
        else {
            stop = (BeamInstr *)((char *)getBaseAddress() + code.codeSize());
        }

        ranges.push_back({.start = start,
                          .stop = stop,
                          .name = code.labelEntry(labels[val.first])->name()});
    }

    update_gdb_jit_info("global", ranges);
    update_perf_info("global", ranges);
#endif
}

void BeamGlobalAssembler::emit_handle_error(int pops) {
    emit_function_postamble(pops);

    a.pop(ARG2);
    a.jmp(labels[handle_error_shared]);
}

/* ARG3 = (HTOP + bytes needed) !!
 * ARG4 = Live registers */
void BeamGlobalAssembler::emit_garbage_collect() {
    emit_function_preamble();

    /* Convert ARG3 to words needed and move it to the correct argument slot */
    a.sub(ARG3, HTOP);
    a.shr(ARG3, 3);
    a.mov(ARG2, ARG3);

    /* Save our return address in c_p->i so we can tell where we crashed if we
     * do so during GC. */
    a.mov(RET, x86::qword_ptr(frame_pointer, sizeof(UWord)));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), RET);

    emit_swapout();
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG3);
    a.mov(ARG5, FCALLS);
    abs_call<5>(erts_garbage_collect_nobump);
    a.sub(FCALLS, RET);
    emit_swapin();

    emit_function_postamble();
    a.ret();
}

/* ARG1 = op address, ARG2 = entry address */
void BeamGlobalAssembler::emit_call_error_handler_shared() {
    Label error_handler = a.newLabel();

    a.mov(ARG3, x86::qword_ptr(ARG1));

    /* We test the generic bp first as it is most likely to be triggered in a
     * loop. */
    a.cmp(ARG3, imm(op_i_generic_breakpoint));
    a.je(labels[generic_bp_global]);

    a.cmp(ARG3, imm(op_call_error_handler));
    a.je(error_handler);

    /* Jump tracing. */
    a.mov(RET, x86::qword_ptr(ARG1, sizeof(UWord)));
    a.jmp(RET);

    a.bind(error_handler);
    {
        emit_heavy_swapout();

        a.mov(ARG1, c_p);
        /* ARG2 is set in module assembler */
        load_x_reg_array(ARG3);
        mov_imm(ARG4, am_undefined_function);
        abs_call<4>(call_error_handler);

        emit_heavy_swapin();

        a.test(RET, RET);
        a.je(labels[error_action_code]);
        a.jmp(RET);
    }
}

/* WARNING: This stub is memcpy'd for performance reasons, so all code herein
 * must be explicitly position-independent. */
void BeamModuleAssembler::emit_call_error_handler() {
    static const BeamInstr ops[2] = {op_call_error_handler, 0};

    Label entry = a.newLabel(), dispatch = a.newLabel(), op = a.newLabel();

    a.bind(entry);
    a.short_().jmp(dispatch);

    a.align(kAlignCode, 8);
    a.bind(op);
    a.embed(ops, sizeof(ops));

    a.bind(dispatch);
    {
        a.lea(ARG1, x86::qword_ptr(op));
        a.lea(ARG2, x86::qword_ptr(entry));
        pic_jmp(ga->get_call_error_handler_shared());
    }
}

void BeamModuleAssembler::emit_handle_error(Label I, const ErtsCodeMFA *exp) {
    a.lea(ARG2, x86::qword_ptr(I));
    mov_imm(ARG4, (Uint)exp);

    abs_jmp(ga->get_handle_error_shared());
}

/* This is an alias for handle_error */
void BeamGlobalAssembler::emit_error_action_code() {
    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);

    a.jmp(labels[handle_error_shared]);
}

void BeamGlobalAssembler::emit_handle_error_shared() {
    Label crash = a.newLabel();

    emit_swapout();

    /* The error address must be a valid CP or NULL. The check is done here
     * rather than in handle_error since the compiler is free to assume that any
     * BeamInstr* is properly aligned. */
    a.test(ARG2, imm(_CPMASK));
    a.jne(crash);

    /* ARG2 and ARG4 must be set prior to jumping here! */
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG3);
    abs_call<4>(handle_error);

    a.test(RET, RET);
    a.je(labels[do_schedule]);

    emit_swapin();
    a.jmp(RET);

    a.bind(crash);
    a.hlt();
}

void BeamModuleAssembler::emit_proc_lc_unrequire(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    a.mov(ARG1, c_p);
    a.mov(ARG2, imm(ERTS_PROC_LOCK_MAIN));
    a.mov(TMP_MEM1q, RET);
    abs_call<2>(erts_proc_lc_unrequire_lock);
    a.mov(RET, TMP_MEM1q);
#endif
}

void BeamModuleAssembler::emit_proc_lc_require(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    a.mov(ARG1, c_p);
    a.mov(ARG2, imm(ERTS_PROC_LOCK_MAIN));
    a.mov(TMP_MEM1q, RET);
    abs_call<4>(erts_proc_lc_require_lock);
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
