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

    emit_test_the_non_value(RET);
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

    emit_test_the_non_value(RET);
    a.short_().je(error);

    emit_leave_runtime<Update::eReductions>();

    a.ret();

    a.bind(error);
    {
        /* Copy arguments into x-registers from the argument vector. We don't
         * need to care about the actual arity since all x-registers are
         * clobbered on exceptions. */
        a.mov(ARG2, TMP_MEM1q);
        for (int i = 0; i < 3; i++) {
            a.mov(ARG1, x86::qword_ptr(ARG2, i * sizeof(Eterm)));
            a.mov(getXRef(i), ARG1);
        }

        /* Find the correct MFA from the BIF's function address. */
        a.mov(ARG1, TMP_MEM2q);
        runtime_call<1>(ubif2mfa);

        emit_leave_runtime<Update::eReductions>();

        a.mov(ARG4, RET);
        a.jmp(labels[handle_error_shared_prologue]);
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
        a.lea(ARG2, TMP_MEM3q);

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
        safe_fragment_call(ga->get_i_bif_body_shared());
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
        safe_fragment_call(ga->get_i_bif_body_shared());
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
        safe_fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, RET);
}

/*
 * Emit code for guard BIFs that can't fail (e.g. is_list/1).  We
 * don't need to test for failure and even in a body there is no need
 * to align the call targeting the shared fragment.
 */

void BeamModuleAssembler::emit_nofail_bif1(const ArgVal &Src1,
                                           const ArgVal &Bif,
                                           const ArgVal &Dst) {
    emit_setup_guard_bif({Src1}, Bif);
    safe_fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_nofail_bif2(const ArgVal &Src1,
                                           const ArgVal &Src2,
                                           const ArgVal &Bif,
                                           const ArgVal &Dst) {
    emit_setup_guard_bif({Src1, Src2}, Bif);
    safe_fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_length_setup(const ArgVal &Fail,
                                              const ArgVal &Live,
                                              const ArgVal &Src) {
    x86::Mem trap_state;

    /* Store trap state after the currently live registers. There's an extra 3
     * registers beyond the ordinary ones that we're free to use for whatever
     * purpose. */
    ERTS_CT_ASSERT(ERTS_X_REGS_ALLOCATED - MAX_REG >= 3);
    ASSERT(Live.getValue() <= MAX_REG);
    trap_state = getXRef(Live.getValue());

    /* Remainder of the list. */
    mov_arg(trap_state, Src);

    /* Accumulated length. */
    a.mov(trap_state.cloneAdjusted(1 * sizeof(Eterm)), imm(make_small(0)));

    /* Original argument. This is only needed for exceptions and can be safely
     * skipped in guards. */
    if (Fail.getValue() == 0) {
        x86::Mem original_argument;

        original_argument = trap_state.cloneAdjusted(2 * sizeof(Eterm));
        mov_arg(original_argument, Src);
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
x86::Mem BeamGlobalAssembler::emit_i_length_common(Label fail, int state_size) {
    Label trap = a.newLabel();
    x86::Mem trap_state;

    ASSERT(state_size >= 2 && state_size <= ERTS_X_REGS_ALLOCATED - MAX_REG);

    /* getXRef(Live) */
    trap_state = getXRef(0);
    trap_state.setIndex(ARG2, 3);

    /* Save arguments for error/trapping path. */
    a.mov(TMP_MEM1q, ARG2);
    a.mov(TMP_MEM2q, ARG3);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    a.lea(ARG2, trap_state);
    runtime_call<2>(erts_trapping_length_1);

    emit_leave_runtime<Update::eReductions>();

    emit_test_the_non_value(RET);
    a.short_().je(trap);

    a.ret();

    a.bind(trap);
    {
        a.mov(ARG2, TMP_MEM1q);
        a.mov(ARG3, TMP_MEM2q);

        a.cmp(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(TRAP));
        a.jne(fail);

        /* The trap state is stored in the registers above the current live
         * ones, so we add the state size (in words) to keep it alive. */
        a.add(ARG2, imm(state_size));

        /* We'll find our way back through the entry address (ARG3). */
        emit_discard_cp();

        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), imm(0));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), ARG2);
        a.jmp(labels[context_switch_simplified]);
    }

    return trap_state;
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_body_shared() {
    Label error = a.newLabel();
    x86::Mem trap_state;

    /* `state_size = 3` to include the original argument. */
    trap_state = emit_i_length_common(error, 3);

    a.bind(error);
    {
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_length, 1};

        /* Move the original argument to x0. It's stored in the third word of
         * the trap state. */
        a.mov(ARG1, trap_state.cloneAdjusted(2 * sizeof(Eterm)));
        a.mov(getXRef(0), ARG1);

        a.mov(ARG4, imm(&bif_mfa));
        emit_handle_error();
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_length_guard_shared() {
    Label error = a.newLabel();

    emit_i_length_common(error, 2);

    a.bind(error);
    {
        mov_imm(RET, 0);
        a.ret();
    }
}

void BeamModuleAssembler::emit_i_length(const ArgVal &Fail,
                                        const ArgVal &Live,
                                        const ArgVal &Dst) {
    Label entry = a.newLabel();

    align_erlang_cp();
    a.bind(entry);

    mov_arg(ARG2, Live);
    a.lea(ARG3, x86::qword_ptr(entry));

    if (Fail.getValue() != 0) {
        /* The return address is discarded when yielding, so it doesn't need to
         * be aligned. */
        safe_fragment_call(ga->get_i_length_guard_shared());
        a.je(labels[Fail.getValue()]);
    } else {
        fragment_call(ga->get_i_length_body_shared());
    }

    mov_arg(Dst, RET);
}

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)

static Eterm debug_call_light_bif(Process *c_p,
                                  Eterm *reg,
                                  ErtsCodePtr I,
                                  ErtsBifFunc vbf) {
    Eterm result;

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

    return result;
}
#endif

/* It is important that the below code is as optimized as possible.
 * When doing any changes, make sure to look at the estone bif_dispatch
 * benchmark to make sure you don't introduce any regressions.
 *
 * ARG3 = entry
 * ARG4 = export entry
 * RET  = BIF pointer
 */
void BeamGlobalAssembler::emit_call_light_bif_shared() {
    /* We use the HTOP and FCALLS registers as they are
       not used on the runtime-stack and are caller save. */

    x86::Gp I = HTOP, exp = FCALLS;

    Label error = a.newLabel(), trace = a.newLabel(), trap = a.newLabel(),
          yield = a.newLabel(), call_save_calls = a.newLabel(),
          call_bif = a.newLabel(), gc_after_bif_call = a.newLabel(),
          check_bif_return = a.newLabel();

    /* Check if we should trace this bif call */
    a.cmp(x86::dword_ptr(ARG4, offsetof(Export, is_bif_traced)), imm(0));
    a.jne(trace);

    a.dec(FCALLS);
    a.jle(yield);

    {
        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

        /* Spill the arguments we may need on the error path. */
        a.mov(I, ARG3);
        a.mov(exp, ARG4);

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.cmp(erts_msacc_cache, imm(0));
            a.short_().je(skip_msacc);

            a.mov(TMP_MEM1q, RET);

            a.mov(ARG1, erts_msacc_cache);
            a.mov(ARG2,
                  x86::qword_ptr(ARG4, offsetof(Export, info.mfa.module)));
            a.mov(ARG3, RET);
            runtime_call<3>(erts_msacc_set_bif_state);

            a.mov(ARG3, I);
            a.mov(RET, TMP_MEM1q);
            a.bind(skip_msacc);
        }
#endif
        /* Check if we need to call save_calls */
        a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
        a.je(call_save_calls);
        a.bind(call_bif);

        a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, mbuf)));
        a.mov(TMP_MEM1q, ARG1);

        /* ARG3 and RET have been set earlier. */
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
        a.mov(ARG4, RET);
        runtime_call<4>(debug_call_light_bif);
#else
        runtime_call(RET, 3);
#endif

        /* ERTS_IS_GC_DESIRED_INTERNAL */
        {
            a.mov(ARG2, x86::qword_ptr(c_p, offsetof(Process, stop)));
            a.mov(ARG3, RET);
            a.mov(ARG5, x86::qword_ptr(c_p, offsetof(Process, htop)));

            /* Test if binary heap size should trigger gc */
            a.mov(RET, x86::qword_ptr(c_p, offsetof(Process, bin_vheap_sz)));
            a.cmp(x86::qword_ptr(c_p, offsetof(Process, off_heap.overhead)),
                  RET);
            a.mov(RETd, x86::dword_ptr(c_p, offsetof(Process, flags)));
            a.seta(x86::cl); /* Clobber ARG1 on windows and ARG4 on Linux */
            a.and_(RETd, imm(F_FORCE_GC));
            a.or_(x86::cl, RETb);
            a.jne(gc_after_bif_call);

            /* Test if heap fragment size is larger than remaining heap size. */
            a.mov(RET, ARG2);
            a.sub(RET, ARG5);
            a.sar(RET, imm(3));
            a.cmp(RET, x86::qword_ptr(c_p, offsetof(Process, mbuf_sz)));
            a.jl(gc_after_bif_call);
        }

        /*
           ARG2 is set to E
           ARG3 is set to bif return
           ARG5 is set to HTOP

           HTOP is exp
           E_saved|E is I
        */
        a.bind(check_bif_return);
        emit_test_the_non_value(ARG3);

        a.short_().je(trap);

        a.mov(HTOP, ARG5);
#ifdef NATIVE_ERLANG_STACK
        a.mov(E_saved, ARG2);
#else
        a.mov(E, ARG2);
#endif

        /* We must update the active code index in case another process has
         * loaded new code, as the result of this BIF may be observable on both
         * ends.
         *
         * It doesn't matter whether the BIF modifies anything; if process A
         * loads new code and calls erlang:monotonic_time/0 soon after, we'd
         * break the illusion of atomic upgrades if process B still ran old code
         * after seeing a later timestamp from its own call to
         * erlang:monotonic_time/0. */

        emit_leave_runtime<Update::eReductions | Update::eCodeIndex>();

        a.mov(getXRef(0), ARG3);
        a.ret();

        a.bind(call_save_calls);
        {
            /* Stash the bif function pointer */
            a.mov(TMP_MEM1q, RET);

            /* Setup the arguments to call */
            a.mov(ARG1, c_p);
            a.mov(ARG2, exp);
            runtime_call<2>(save_calls);

            /* Restore RET and ARG3 to the values expected
               by the bif call */
            a.mov(RET, TMP_MEM1q);
            a.mov(ARG3, I);
            a.jmp(call_bif);
        }

        a.bind(trap);
        {
            a.cmp(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(TRAP));
            a.short_().jne(error);

            emit_leave_runtime<Update::eHeap | Update::eStack |
                               Update::eReductions | Update::eCodeIndex>();

#if !defined(NATIVE_ERLANG_STACK)
            a.pop(getCPRef());
#endif

            /* Trap out, our return address is on the Erlang stack.
             *
             * The BIF_TRAP macros all set up c_p->arity and c_p->current, so
             * we can use a simplified context switch. */
            a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
            a.jmp(labels[context_switch_simplified]);
        }

        a.bind(error);
        {
            a.mov(ARG4, exp);
            a.mov(RET, I);

            /* Update::eCodeIndex clobbers ARG1 + ARG2 */
            emit_leave_runtime<Update::eHeap | Update::eStack |
                               Update::eReductions | Update::eCodeIndex>();

            /* handle_error_shared needs the entry address in ARG2 */
            a.mov(ARG2, RET);

#if !defined(NATIVE_ERLANG_STACK)
            /* Discard the continuation pointer as it will never be used. */
            emit_discard_cp();
#endif

            /* get_handle_error expects current PC in ARG2 and MFA in ARG4. */
            a.lea(ARG4, x86::qword_ptr(ARG4, offsetof(Export, info.mfa)));

            /* Overwrite the return address with the entry address to ensure
             * that only the entry address ends up in the stack trace. */
            a.mov(x86::qword_ptr(E), ARG2);

            a.jmp(labels[handle_error_shared]);
        }

        a.bind(gc_after_bif_call);
        {
            a.mov(ARG1, c_p);
            a.mov(ARG2, TMP_MEM1q);
            /* ARG3 already contains result */
            load_x_reg_array(ARG4);
            a.mov(ARG5, x86::qword_ptr(exp, offsetof(Export, info.mfa.arity)));
            runtime_call<5>(erts_gc_after_bif_call_lhf);
            a.mov(ARG3, RET);
            a.mov(ARG5, x86::qword_ptr(c_p, offsetof(Process, htop)));
            a.mov(ARG2, x86::qword_ptr(c_p, offsetof(Process, stop)));
            a.jmp(check_bif_return);
        }
    }

    a.bind(trace);
    {
        /* Call the export entry instead of the BIF. If we use the
         * native stack as the Erlang stack our return address is
         * already on the Erlang stack. Otherwise we will have to move
         * the return address from the native stack to the Erlang
         * stack. */

#if !defined(NATIVE_ERLANG_STACK)
        /* The return address must be on the Erlang stack. */
        a.pop(getCPRef());
#endif

        x86::Mem destination = emit_setup_export_call(ARG4);
        a.jmp(destination);
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

    align_erlang_cp();
    a.bind(entry);

    make_move_patch(ARG4, imports[Exp.getValue()].patches);
    a.mov(RET, imm(Bif.getValue()));
    a.lea(ARG3, x86::qword_ptr(entry));

    fragment_call(ga->get_call_light_bif_shared());
}

void BeamModuleAssembler::emit_send() {
    Label entry = a.newLabel();

    /* This is essentially a mirror of call_light_bif, there's no point to
     * specializing send/2 anymore.
     *
     * FIXME: Rewrite this to an ordinary BIF in the loader instead. */
    align_erlang_cp();
    a.bind(entry);

    a.mov(ARG4, imm(BIF_TRAP_EXPORT(BIF_send_2)));
    a.mov(RET, imm(send_2));
    a.lea(ARG3, x86::qword_ptr(entry));

    fragment_call(ga->get_call_light_bif_shared());
}

void BeamGlobalAssembler::emit_bif_nif_epilogue(void) {
    Label check_trap = a.newLabel(), trap = a.newLabel(), error = a.newLabel();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.short_().je(skip_msacc);
        a.mov(TMP_MEM1q, RET);
        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, imm(ERTS_MSACC_STATE_EMULATOR));
        a.mov(ARG3, imm(1));
        runtime_call<3>(erts_msacc_set_state_m__);
        a.mov(RET, TMP_MEM1q);
        a.bind(skip_msacc);
    }
#endif

    /* Another process may have loaded new code and somehow notified us through
     * this call, so we must update the active code index. */
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap |
                       Update::eCodeIndex>();

    emit_test_the_non_value(RET);
    a.short_().je(check_trap);

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
        a.short_().je(trap);

        comment("do hibernate trap");
        a.and_(ARG1, imm(~F_HIBERNATE_SCHED));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, flags)), ARG1);
        a.jmp(labels[do_schedule]);
    }

    a.bind(trap);
    {
        comment("do normal trap");

        /* The BIF_TRAP macros all set up c_p->arity and c_p->current, so we
         * can use a simplified context switch. */
        a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
        a.jmp(labels[context_switch_simplified]);
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

/* Used by call_bif, dispatch_bif, and export_trampoline.
 *
 * Note that we don't check reductions here as we may have jumped here through
 * interpreted code (e.g. an ErtsNativeFunc or export entry) and it's very
 * tricky to yield back. Reductions are checked in module code instead.
 *
 * ARG2 = BIF MFA
 * ARG3 = I (rip), doesn't need to point past an MFA
 * ARG4 = function to be called */
void BeamGlobalAssembler::emit_call_bif_shared(void) {
    /* "Heavy" BIFs need up-to-date values for `c_p->i`, `c_p->current`, and
     * `c_p->arity`. */

    a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG2);
    /* `call_bif` wants arity in ARG5. */
    a.mov(ARG5, x86::qword_ptr(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), ARG5);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), ARG3);

    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.short_().je(skip_msacc);

        a.mov(TMP_MEM1q, ARG3);
        a.mov(TMP_MEM2q, ARG4);
        a.mov(TMP_MEM3q, ARG5);

        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, x86::qword_ptr(ARG2, offsetof(ErtsCodeMFA, module)));
        a.mov(ARG3, ARG4);
        runtime_call<3>(erts_msacc_set_bif_state);

        a.mov(ARG3, TMP_MEM1q);
        a.mov(ARG4, TMP_MEM2q);
        a.mov(ARG5, TMP_MEM3q);
        a.bind(skip_msacc);
    }
#endif

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    /* ARG3 (I), ARG4 (func), and ARG5 (arity) have already been provided. */
    runtime_call<5>(beam_jit_call_bif);

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
    a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));

    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.trace) ==
                   sizeof(ErtsCodeInfo));

    ssize_t mfa_offset = offsetof(ErtsNativeFunc, trampoline.info.mfa) -
                         offsetof(ErtsNativeFunc, trampoline.trace);
    a.lea(ARG2, x86::qword_ptr(ARG3, mfa_offset));

    ssize_t dfunc_offset = offsetof(ErtsNativeFunc, trampoline.dfunc) -
                           offsetof(ErtsNativeFunc, trampoline.trace);
    a.mov(ARG4, x86::qword_ptr(ARG3, dfunc_offset));

    a.jmp(labels[call_bif_shared]);
}

void BeamModuleAssembler::emit_call_bif(const ArgVal &Func) {
    int mfa_offset = -(int)sizeof(ErtsCodeMFA);

    a.lea(ARG2, x86::qword_ptr(currLabel, mfa_offset));
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
        a.short_().je(next);

        a.ud2();

        a.bind(next);
    }
#endif

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_call_nif_early);

    emit_leave_runtime();

    /* We won't return to the original code. */
    emit_discard_cp();

    /* Emulate `emit_call_nif`, loading the current (phony) instruction
     * pointer into ARG2. */
    a.mov(ARG3, RET);
    a.jmp(labels[call_nif_shared]);
}

/* Used by call_nif, call_nif_early, and dispatch_nif.
 *
 * Note that we don't check reductions here as we may have jumped here through
 * interpreted code (e.g. an ErtsNativeFunc or export entry) and it's very
 * tricky to yield back. Reductions are checked in module code instead.
 *
 * ARG3 = current I, just past the end of an ErtsCodeInfo. */
void BeamGlobalAssembler::emit_call_nif_shared(void) {
    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.short_().je(skip_msacc);
        a.mov(TMP_MEM1q, ARG3);
        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, imm(ERTS_MSACC_STATE_NIF));
        a.mov(ARG3, imm(1));
        runtime_call<3>(erts_msacc_set_state_m__);
        a.mov(ARG3, TMP_MEM1q);
        a.bind(skip_msacc);
    }
#endif

    a.mov(ARG1, c_p);
    a.mov(ARG2, ARG3);
    load_x_reg_array(ARG3);
    a.mov(ARG4, x86::qword_ptr(ARG2, 8 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    a.mov(ARG5, x86::qword_ptr(ARG2, 16 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    a.mov(ARG6, x86::qword_ptr(ARG2, 24 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    runtime_call<5>(beam_jit_call_nif);

    emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure.
     *
     * ErtsNativeFunc already follows the NIF call layout, so we don't need to
     * do anything beyond loading the address. */
    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.trace) ==
                   sizeof(ErtsCodeInfo));
    a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
    a.jmp(labels[call_nif_shared]);
}

/* WARNING: This stub is memcpy'd, so all code herein must be explicitly
 * position-independent. */
void BeamModuleAssembler::emit_call_nif(const ArgVal &Func,
                                        const ArgVal &NifMod,
                                        const ArgVal &DirtyFunc) {
    Label dispatch = a.newLabel();
    uint64_t val;

    /* The start of this function has to mimic the layout of ErtsNativeFunc. */
    a.jmp(dispatch); /* call_op */

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
    a.bind(dispatch);
    {
        Label yield = a.newLabel();

        a.lea(ARG3, x86::qword_ptr(currLabel));

        a.dec(FCALLS);
        a.jl(yield);

        pic_jmp(ga->get_call_nif_shared());

        a.bind(yield);
        pic_jmp(ga->get_context_switch());
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
    runtime_call<3>(beam_jit_load_nif);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

    a.cmp(RET, RET_NIF_yield);
    a.short_().je(yield);
    a.cmp(RET, RET_NIF_success);
    a.short_().jne(error);

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

    align_erlang_cp();
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

    align_erlang_cp();
    a.bind(entry);

    emit_enter_runtime<Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(currLabel));
    load_x_reg_array(ARG3);
    runtime_call<3>(beam_jit_load_nif);

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
