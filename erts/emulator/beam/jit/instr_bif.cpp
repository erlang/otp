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

extern "C"
{
#include "beam_common.h"
#include "code_ix.h"
#include "erl_bif_table.h"
#include "erl_nfunc_sched.h"
#include "bif.h"
#include "erl_msacc.h"
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_bif_guard_shared() {
    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    /* ARG2 has been set by caller; ARG3 is never used by guard BIFs. */
    mov_imm(ARG3, 0);
    runtime_call(ARG4, 3);

    emit_leave_runtime<Update::eReductions>();

    a.cmp(RET, imm(THE_NON_VALUE));
    a.ret();
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bif_body_shared() {
    Label error = a.newLabel();

    emit_enter_runtime<Update::eReductions>();

    /* Save current BIF and argument vector for the error path. */
    a.mov(TMP_MEM1q, ARG2);
    a.mov(TMP_MEM2q, ARG4);

    a.mov(ARG1, c_p);
    /* ARG2 has been set by caller; ARG3 is never used by guard BIFs. */
    mov_imm(ARG3, 0);
    runtime_call(ARG4, 3);

    a.cmp(RET, imm(THE_NON_VALUE));
    a.je(error);

    emit_leave_runtime<Update::eReductions>();

    a.ret();

    a.bind(error);
    {
        /* Copy arguments into x-registers from the argument vector. We don't
         * need to care about the actual arity since all x-registers are
         * clobbered on exceptions. */
        a.mov(ARG2, TMP_MEM1q);
        for (int i = 0; i < GUARD_BIF_ARGV_LENGTH; i++) {
            a.mov(ARG1, x86::qword_ptr(ARG2, i * sizeof(Eterm)));
            a.mov(getXRef(i), ARG1);
        }

        /* Find the correct MFA from the BIF's function address. */
        a.mov(ARG1, TMP_MEM2q);
        runtime_call<1>(ubif2mfa);

        emit_leave_runtime<Update::eReductions>();

        a.mov(ARG4, RET);
        emit_handle_error();
    }
}

void BeamModuleAssembler::emit_setup_guard_bif(const std::vector<ArgVal> &args,
                                               const ArgVal &bif) {
    bool is_contiguous_mem = false;

    ASSERT(args.size() > 0 && args.size() <= 3);

    /* If the guard BIF's arguments are in memory and continuous, for example
     * `map_get(x0, x1)`, then we can pass the address of the first argument
     * instead of filling in the argument vector. */
    is_contiguous_mem = args.size() && args[0].isMem();
    for (size_t i = 1; i < args.size() && is_contiguous_mem; i++) {
        const ArgVal &curr = args[i], &prev = args[i - 1];

        is_contiguous_mem = curr.getType() == prev.getType() &&
                            curr.getValue() == prev.getValue() + 1;
    }

    if (is_contiguous_mem) {
        a.lea(ARG2, getArgRef(args[0]));
    } else {
        a.lea(ARG2, guard_bif_argv);

        for (size_t i = 0; i < args.size(); i++) {
            mov_arg(x86::qword_ptr(ARG2, i * sizeof(Eterm)), args[i]);
        }
    }

    mov_arg(ARG4, bif);
}

void BeamModuleAssembler::emit_i_bif1(const ArgVal &Src1,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    emit_setup_guard_bif({Src1}, Bif);

    if (Fail.getValue() != 0) {
        safe_fragment_call(ga->get_i_bif_guard_shared());
        a.je(labels[Fail.getValue()]);
    } else {
        fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bif2(const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    emit_setup_guard_bif({Src1, Src2}, Bif);

    if (Fail.getValue() != 0) {
        safe_fragment_call(ga->get_i_bif_guard_shared());
        a.je(labels[Fail.getValue()]);
    } else {
        fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bif3(const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Src3,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    emit_setup_guard_bif({Src1, Src2, Src3}, Bif);

    if (Fail.getValue() != 0) {
        safe_fragment_call(ga->get_i_bif_guard_shared());
        a.je(labels[Fail.getValue()]);
    } else {
        fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_length_setup(const ArgVal &Live,
                                              const ArgVal &Src) {
    const ArgVal slot(ArgVal::TYPE::x, Live.getValue());

    mov_arg(slot + 0, Src);
    mov_arg(slot + 1, make_small(0));
    mov_arg(slot + 2, Src);
}

void BeamModuleAssembler::emit_i_length(const ArgVal &Fail,
                                        const ArgVal &Live,
                                        const ArgVal &Dst) {
    Label entry, next, trap, error;

    entry = a.newLabel();
    next = a.newLabel();
    trap = a.newLabel();

    if (Fail.getValue() != 0) {
        error = labels[Fail.getValue()];
    } else {
        error = a.newLabel();
    }

    a.align(kAlignCode, 8);
    a.bind(entry);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    a.lea(ARG2, getXRef(Live.getValue()));
    runtime_call<2>(erts_trapping_length_1);

    emit_leave_runtime<Update::eReductions>();

    a.cmp(RET, imm(THE_NON_VALUE));
    a.je(trap);

    mov_arg(Dst, RET);
    a.jmp(next);

    comment("test trap");
    a.bind(trap);
    {
        a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, freason)));
        a.cmp(ARG1, imm(TRAP));
        a.jne(error);
        comment("do trap");
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), imm(0));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)),
              imm(Live.getValue() + 3));
        a.lea(ARG3, x86::qword_ptr(entry));
        abs_jmp(ga->get_context_switch_simplified());
    }

    if (Fail.getValue() == 0) {
        a.bind(error);
        emit_bif_arg_error({ArgVal(ArgVal::x, Live.getValue() + 2)},
                           entry,
                           &BIF_TRAP_EXPORT(BIF_length_1)->info.mfa);
    }

    a.bind(next);
}

static Eterm call_light_bif(Process *c_p,
                            Eterm *reg,
                            BeamInstr *I,
                            Export *exp,
                            ErtsBifFunc vbf,
                            UWord active_code_ix) {
    ErlHeapFragment *live_hf_end = c_p->mbuf;
    ErtsCodeMFA *codemfa = &exp->info.mfa;
    Eterm result;

    if (active_code_ix == ERTS_SAVE_CALLS_CODE_IX) {
        save_calls(c_p, exp);
    }

    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    {
        ERTS_CHK_MBUF_SZ(c_p);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
        result = vbf(c_p, reg, I);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
        ERTS_CHK_MBUF_SZ(c_p);

        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ERTS_HOLE_CHECK(c_p);
    }
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    if (ERTS_IS_GC_DESIRED(c_p)) {
        result = erts_gc_after_bif_call_lhf(c_p,
                                            live_hf_end,
                                            result,
                                            reg,
                                            codemfa->arity);
    }

    return result;
}

void BeamGlobalAssembler::emit_call_light_bif_shared() {
    /* ARG3 = entry
     * ARG4 = export entry
     * ARG5 = BIF pointer */
    Label error = a.newLabel(), execute = a.newLabel(), trace = a.newLabel(),
          trap = a.newLabel(), yield = a.newLabel();

    a.cmp(x86::dword_ptr(ARG4, offsetof(Export, is_bif_traced)), imm(0));
    a.jne(trace);

    a.dec(FCALLS);
    a.jle(yield);

    a.bind(execute);
    {
        /* Spill the arguments we may need on the error path. */
        a.mov(TMP_MEM1q, ARG3);
        a.mov(TMP_MEM2q, ARG4);

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.cmp(erts_msacc_cache, imm(0));
            a.je(skip_msacc);

            a.mov(TMP_MEM3q, ARG5);

            a.mov(ARG1, erts_msacc_cache);
            a.mov(ARG2,
                  x86::qword_ptr(ARG4, offsetof(Export, info.mfa.module)));
            a.mov(ARG3, ARG5);
            runtime_call<3>(erts_msacc_set_bif_state);

            a.mov(ARG3, TMP_MEM1q);
            a.mov(ARG4, TMP_MEM2q);
            a.mov(ARG5, TMP_MEM3q);
            a.bind(skip_msacc);
        }
#endif

        /* ARG3, ARG4, and ARG5 have been set earlier. */
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        a.mov(ARG6, active_code_ix);
        runtime_call<6>(call_light_bif);

        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        /* We must update the active code index in case another process has
         * loaded new code, as the result of this BIF may be observable on both
         * ends.
         *
         * It doesn't matter whether the BIF modifies anything; if process A
         * loads new code and calls erlang:monotonic_time/0 soon after, we'd
         * break the illusion of atomic upgrades if process B still ran old code
         * after seeing a later timestamp from its own call to
         * erlang:monotonic_time/0. */
        emit_update_code_index();

        a.cmp(RET, imm(THE_NON_VALUE));
        a.je(trap);

        a.mov(getXRef(0), RET);
        a.ret();

        a.bind(trap);
        {
            a.cmp(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(TRAP));
            a.jne(error);

#if !defined(NATIVE_ERLANG_STACK)
            a.pop(getCPRef());
#endif

            /* Trap out, our return address is on the Erlang stack. */

            a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
            a.jmp(labels[context_switch]);
        }

        a.bind(error);
        {
#if !defined(NATIVE_ERLANG_STACK)
            /* Discard the continuation pointer as it will never be used. */
            emit_discard_cp();
#endif

            /* get_handle_error expects current PC in ARG2 and MFA in ARG4. */
            a.mov(ARG2, TMP_MEM1q);
            a.mov(ARG4, TMP_MEM2q);
            a.lea(ARG4, x86::qword_ptr(ARG4, offsetof(Export, info.mfa)));

            /* Overwrite the return address with the entry address to ensure
             * that only the entry address ends up in the stack trace. */
            a.mov(x86::qword_ptr(E), ARG2);

            a.jmp(labels[handle_error_shared]);
        }
    }

    a.bind(trace);
    {
        /* Call the export entry instead of the BIF. If we use the
         * native stack as the Erlang stack our return address is
         * already on the Erlang stack. Otherwise we will have to move
         * the return address from the native stack to the Erlang
         * stack.
         *
         * The export entry must be moved to ARG2 for save_calls to work, see
         * `BeamGlobalAssembler::emit_dispatch_save_calls()` for details.
         */

#if !defined(NATIVE_ERLANG_STACK)
        /* The return address must be on the Erlang stack. */
        a.pop(getCPRef());
#endif

        a.mov(ARG1, active_code_ix);
        a.mov(ARG2, ARG4);
        a.jmp(x86::qword_ptr(ARG2, ARG1, 3, offsetof(Export, addressv)));
    }

    a.bind(yield);
    {
        a.mov(ARG2, x86::qword_ptr(ARG4, offsetof(Export, info.mfa.arity)));
        a.lea(ARG4, x86::qword_ptr(ARG4, offsetof(Export, info.mfa)));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), ARG2);
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG4);

        /* We'll find our way back through ARG3 (entry address). */
        emit_discard_cp();

        a.jmp(labels[context_switch_simplified]);
    }
}

void BeamModuleAssembler::emit_call_light_bif(const ArgVal &Bif,
                                              const ArgVal &Exp) {
    Label entry = a.newLabel();

    a.align(kAlignCode, 8);
    a.bind(entry);

    a.lea(ARG3, x86::qword_ptr(entry));
    make_move_patch(ARG4, imports[Exp.getValue()].patches);
    mov_arg(ARG5, Bif);
    fragment_call(ga->get_call_light_bif_shared());
}

void BeamModuleAssembler::emit_send() {
    Label entry = a.newLabel();

    /* This is essentially a mirror of call_light_bif, there's no point to
     * specializing send/2 anymore.
     *
     * FIXME: Rewrite this to an ordinary BIF in the loader instead. */
    a.align(kAlignCode, 8);
    a.bind(entry);

    a.lea(ARG3, x86::qword_ptr(entry));
    a.mov(ARG4, imm(BIF_TRAP_EXPORT(BIF_send_2)));
    a.mov(ARG5, imm(send_2));
    fragment_call(ga->get_call_light_bif_shared());
}

static Eterm call_bif(Process *c_p, Eterm *reg, BeamInstr *I, ErtsBifFunc vbf) {
    ErlHeapFragment *live_hf_end;
    ErtsCodeMFA *codemfa;
    Eterm result;

    codemfa = erts_code_to_codemfa(I);

    /* In case we apply process_info/1,2 or load_nif/1 */
    c_p->current = codemfa;

    /* In case we apply check_process_code/2. */
    c_p->i = I;

    /* To allow garbage collection on ourselves
     * (check_process_code/2, put/2, etc). */
    c_p->arity = 0;

    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    {
        live_hf_end = c_p->mbuf;

        ERTS_CHK_MBUF_SZ(c_p);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
        result = vbf(c_p, reg, I);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
        ERTS_CHK_MBUF_SZ(c_p);

        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ERTS_HOLE_CHECK(c_p);
    }
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    if (ERTS_IS_GC_DESIRED(c_p)) {
        result = erts_gc_after_bif_call_lhf(c_p,
                                            live_hf_end,
                                            result,
                                            reg,
                                            codemfa->arity);
    }

    return result;
}

typedef Eterm NifF(struct enif_environment_t *, int argc, Eterm argv[]);

static Eterm call_nif(Process *c_p,
                      BeamInstr *I,
                      Eterm *reg,
                      NifF *fp,
                      struct erl_module_nif *NifMod) {
    Eterm nif_bif_result;
    Eterm bif_nif_arity;
    ErlHeapFragment *live_hf_end;
    ErtsCodeMFA *codemfa;

    codemfa = erts_code_to_codemfa(I);

    c_p->current = codemfa; /* current and vbf set to please handle_error */

    bif_nif_arity = codemfa->arity;
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);

    {
        struct enif_environment_t env;
        ASSERT(c_p->scheduler_data);
        live_hf_end = c_p->mbuf;
        ERTS_CHK_MBUF_SZ(c_p);
        erts_pre_nif(&env, c_p, NifMod, NULL);

        ASSERT((c_p->scheduler_data)->current_nif == NULL);
        (c_p->scheduler_data)->current_nif = &env;

        nif_bif_result = (*fp)(&env, bif_nif_arity, reg);
        if (env.exception_thrown)
            nif_bif_result = THE_NON_VALUE;

        ASSERT((c_p->scheduler_data)->current_nif == &env);
        (c_p->scheduler_data)->current_nif = NULL;

        erts_post_nif(&env);
        ERTS_CHK_MBUF_SZ(c_p);

        PROCESS_MAIN_CHK_LOCKS(c_p);
        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ASSERT(!env.exiting);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    }
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    ERTS_HOLE_CHECK(c_p);

    if (ERTS_IS_GC_DESIRED(c_p)) {
        nif_bif_result = erts_gc_after_bif_call_lhf(c_p,
                                                    live_hf_end,
                                                    nif_bif_result,
                                                    reg,
                                                    bif_nif_arity);
    }

    return nif_bif_result;
}

void BeamGlobalAssembler::emit_bif_nif_epilogue(void) {
    Label check_trap = a.newLabel(), trap = a.newLabel(), error = a.newLabel();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.je(skip_msacc);
        a.mov(TMP_MEM1q, RET);
        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, imm(ERTS_MSACC_STATE_EMULATOR));
        a.mov(ARG3, imm(1));
        runtime_call<3>(erts_msacc_set_state_m__);
        a.mov(RET, TMP_MEM1q);
        a.bind(skip_msacc);
    }
#endif

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    /* Another process may have loaded new code and somehow notified us through
     * this call, so we must update the active code index. */
    emit_update_code_index();

    a.cmp(RET, imm(THE_NON_VALUE));
    a.je(check_trap);

    comment("Do return and dispatch to it");
    a.mov(getXRef(0), RET);
#ifdef NATIVE_ERLANG_STACK
    a.ret();
#else
    a.mov(RET, getCPRef());
    a.mov(getCPRef(), imm(NIL));
    a.jmp(RET);
#endif

    a.bind(check_trap);
    a.cmp(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(TRAP));
    a.jne(error);
    {
        comment("yield");

        comment("test trap to hibernate");
        a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, flags)));
        a.mov(ARG2, ARG1);
        a.and_(ARG2, imm(F_HIBERNATE_SCHED));
        a.je(trap);

        comment("do hibernate trap");
        a.and_(ARG1, imm(~F_HIBERNATE_SCHED));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, flags)), ARG1);
        a.jmp(labels[do_schedule]);
    }

    a.bind(trap);
    {
        comment("do normal trap");
        a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
        a.jmp(labels[context_switch]);
    }

    a.bind(error);
    {
        a.mov(ARG2, E);

        emit_enter_runtime<Update::eStack>();

        a.mov(ARG1, c_p);
        runtime_call<2>(erts_printable_return_address);

        emit_leave_runtime<Update::eStack>();

        a.mov(ARG2, RET);
        a.mov(ARG4, x86::qword_ptr(c_p, offsetof(Process, current)));
        a.jmp(labels[handle_error_shared]);
    }
}

void BeamGlobalAssembler::emit_call_bif_shared(void) {
    a.dec(FCALLS);
    a.jle(labels[context_switch]);

    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();
        int module_offset;

        module_offset = -(Sint)sizeof(ErtsCodeInfo) +
                        offsetof(ErtsCodeInfo, mfa.module);

        a.cmp(erts_msacc_cache, 0);
        a.je(skip_msacc);

        a.mov(TMP_MEM1q, ARG3);
        a.mov(TMP_MEM2q, ARG4);

        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, x86::qword_ptr(ARG3, module_offset));
        a.mov(ARG3, ARG4);
        runtime_call<3>(erts_msacc_set_bif_state);

        a.mov(ARG3, TMP_MEM1q);
        a.mov(ARG4, TMP_MEM2q);
        a.bind(skip_msacc);
    }
#endif

    /* These arguments have already been provided:
     *
     *    ARG3 = I (rip)
     *    ARG4 = function to be called
     */

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    a.mov(RET, (uint64_t)call_bif);
    a.call(RET);

#ifdef ERTS_MSACC_EXTENDED_STATES
    a.mov(TMP_MEM1q, RET);
    a.lea(ARG1, erts_msacc_cache);
    runtime_call<1>(erts_msacc_update_cache);
    a.mov(RET, TMP_MEM1q);
#endif

    emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_bif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure. */
    ssize_t dfunc_offset;

    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.trace) ==
                   sizeof(ErtsCodeInfo));
    dfunc_offset = offsetof(ErtsNativeFunc, trampoline.dfunc) -
                   offsetof(ErtsNativeFunc, trampoline.trace);

    a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
    a.mov(ARG4, x86::qword_ptr(ARG3, dfunc_offset));
    a.jmp(labels[call_bif_shared]);
}

void BeamModuleAssembler::emit_call_bif(const ArgVal &Func) {
    a.lea(ARG3, x86::qword_ptr(currLabel));
    mov_arg(ARG4, Func);
    abs_jmp(ga->get_call_bif_shared());
}

void BeamModuleAssembler::emit_call_bif_mfa(const ArgVal &M,
                                            const ArgVal &F,
                                            const ArgVal &A) {
    BeamInstr func;
    Export *e;

    e = erts_active_export_entry(M.getValue(), F.getValue(), A.getValue());
    ASSERT(e != NULL && e->bif_number != -1);

    func = (BeamInstr)bif_table[e->bif_number].f;
    emit_call_bif(ArgVal(ArgVal::i, func));
}

void BeamGlobalAssembler::emit_call_nif_early() {
    /* Fetch and align the return address so we can tell where we came from. It
     * points just after the trampoline word so we'll need to skip that to find
     * our ErtsCodeInfo. */
    a.mov(ARG2, x86::qword_ptr(x86::rsp));
    a.sub(ARG2, imm(sizeof(UWord) + sizeof(ErtsCodeInfo)));

#ifdef DEBUG
    {
        Label next = a.newLabel();

        /* Crash if our return address isn't word-aligned. */
        a.test(ARG2, imm(sizeof(UWord) - 1));
        a.je(next);

        a.ud2();

        a.bind(next);
    }
#endif

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_call_nif_early);

    emit_leave_runtime();

    /* We patch the return address to jump to the correct place. */
    a.mov(x86::qword_ptr(x86::rsp), RET);
    a.ret();
}

/* Both dispatch_nif and call_nif will end up in this function
 *
 * ARG2 = current I, just past the end of an ErtsCodeInfo */
void BeamGlobalAssembler::emit_call_nif_shared(void) {
    Label execute = a.newLabel(), yield = a.newLabel();

    a.dec(FCALLS);
    a.jl(yield);

    a.bind(execute);
    {
        /* The corresponding leave can be found in the epilogue. */
        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.cmp(erts_msacc_cache, 0);
            a.je(skip_msacc);
            a.mov(TMP_MEM1q, ARG2);
            a.mov(ARG1, erts_msacc_cache);
            a.mov(ARG2, imm(ERTS_MSACC_STATE_NIF));
            a.mov(ARG3, imm(1));
            runtime_call<3>(erts_msacc_set_state_m__);
            a.mov(ARG2, TMP_MEM1q);
            a.bind(skip_msacc);
        }
#endif

        a.mov(ARG1, c_p);
        /* ARG2 set in caller */
        load_x_reg_array(ARG3);
        a.mov(ARG4, x86::qword_ptr(ARG2, 8 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
        a.mov(ARG5, x86::qword_ptr(ARG2, 16 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
        a.mov(ARG6, x86::qword_ptr(ARG2, 24 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
        runtime_call<5>(call_nif);

        emit_bif_nif_epilogue();
    }

    a.bind(yield);
    {
        a.mov(ARG3, ARG2);
        a.jmp(labels[context_switch]);
    }
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure.
     *
     * ErtsNativeFunc already follows the NIF call layout, so we don't need to
     * do anything beyond loading the address. */
    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.trace) ==
                   sizeof(ErtsCodeInfo));
    a.mov(ARG2, x86::qword_ptr(c_p, offsetof(Process, i)));
    a.jmp(labels[call_nif_shared]);
}

/* WARNING: This stub is memcpy'd, so all code herein must be explicitly
 * position-independent. */
void BeamModuleAssembler::emit_call_nif(const ArgVal &Func,
                                        const ArgVal &NifMod,
                                        const ArgVal &DirtyFunc) {
    Label entry = a.newLabel();
    uint64_t val;

    /* The start of this function has to mimic the layout of ErtsNativeFunc...
     */
    a.jmp(entry); /* call_op */

    a.align(kAlignCode, 8);
    /* ErtsNativeFunc.dfunc */
    val = Func.getValue();
    a.embed(&val, sizeof(val));
    /* ErtsNativeFunc.m */
    val = NifMod.getValue();
    a.embed(&val, sizeof(val));
    /* ErtsNativeFunc.func */
    val = DirtyFunc.getValue();
    a.embed(&val, sizeof(val));

    /* The real code starts here */
    a.bind(entry);
    a.lea(ARG2, x86::qword_ptr(currLabel));

    pic_jmp(ga->get_call_nif_shared());
}

enum nif_load_ret { RET_NIF_success, RET_NIF_error, RET_NIF_yield };

static enum nif_load_ret load_nif(Process *c_p, BeamInstr *I, Eterm *reg) {
    if (erts_try_seize_code_write_permission(c_p)) {
        Eterm result;

        PROCESS_MAIN_CHK_LOCKS((c_p));
        ERTS_UNREQ_PROC_MAIN_LOCK((c_p));
        result = erts_load_nif(c_p, I, reg[0], reg[1]);
        erts_release_code_write_permission();
        ERTS_REQ_PROC_MAIN_LOCK(c_p);

        if (ERTS_LIKELY(is_value(result))) {
            reg[0] = result;
            return RET_NIF_success;
        } else {
            c_p->freason = BADARG;
            return RET_NIF_error;
        }
    } else {
        /* Yield and try again. */
        c_p->current = NULL;
        c_p->arity = 2;
        return RET_NIF_yield;
    }
}

/* ARG2 = entry address. */
void BeamGlobalAssembler::emit_i_load_nif_shared() {
    static ErtsCodeMFA bif_mfa = {am_erlang, am_load_nif, 2};

    Label yield = a.newLabel(), error = a.newLabel();

    a.mov(TMP_MEM1q, ARG2);

    emit_enter_runtime<Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    /* ARG2 has already been set by caller */
    load_x_reg_array(ARG3);
    runtime_call<3>(load_nif);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

    a.cmp(RET, RET_NIF_yield);
    a.je(yield);
    a.cmp(RET, RET_NIF_success);
    a.jne(error);

    a.ret();

    a.bind(error);
    {
        a.mov(ARG4, imm(&bif_mfa));
        emit_handle_error();
    }

    a.bind(yield);
    {
        a.mov(ARG3, TMP_MEM1q);
        a.jmp(labels[context_switch_simplified]);
    }
}

#ifdef NATIVE_ERLANG_STACK

void BeamModuleAssembler::emit_i_load_nif() {
    Label entry = a.newLabel(), next = a.newLabel();

    /* i_load_nif is a rewrite of a call_ext instruction, so we'll body-call
     * ourselves to ensure the stack is consistent with that. This greatly
     * simplifies yielding and error handling. */
    fragment_call(entry);
    a.short_().jmp(next);

    a.align(kAlignCode, 8);
    a.bind(entry);
    {
        a.lea(ARG2, x86::qword_ptr(entry));
        abs_jmp(ga->get_i_load_nif_shared());
    }

    a.bind(next);
}

#else

void BeamModuleAssembler::emit_i_load_nif() {
    static ErtsCodeMFA mfa = {am_erlang, am_load_nif, 2};

    Label entry = a.newLabel(), next = a.newLabel(), schedule = a.newLabel();

    a.align(kAlignCode, 8);
    a.bind(entry);

    emit_enter_runtime<Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(currLabel));
    load_x_reg_array(ARG3);
    runtime_call<3>(load_nif);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

    a.cmp(RET, imm(RET_NIF_yield));
    a.je(schedule);
    a.cmp(RET, imm(RET_NIF_success));
    a.je(next);

    emit_handle_error(currLabel, &mfa);

    a.bind(schedule);
    {
        a.lea(ARG3, x86::qword_ptr(entry));
        abs_jmp(ga->get_context_switch_simplified());
    }

    a.bind(next);
}

#endif
