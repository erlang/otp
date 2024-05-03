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
    emit_enter_frame();
    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    /* ARG2 has been set by caller; ARG3 is never used by guard BIFs. */
    mov_imm(ARG3, 0);
    runtime_call(ARG4, 3);

    emit_leave_runtime<Update::eReductions>();
    emit_leave_frame();

    emit_test_the_non_value(RET);
    a.ret();
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bif_body_shared() {
    Label error = a.newLabel();

    emit_enter_frame();
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
    emit_leave_frame();

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
        emit_leave_frame();

        a.mov(ARG4, RET);
        a.jmp(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_setup_guard_bif(const std::vector<ArgVal> &args,
                                               const ArgWord &Bif) {
    bool is_contiguous_mem = false;

    ASSERT(args.size() > 0 && args.size() <= 3);

    /* If the guard BIF's arguments are in memory and continuous, for example
     * `map_get(x0, x1)`, then we can pass the address of the first argument
     * instead of filling in the argument vector. */
    is_contiguous_mem = args.size() && args[0].isRegister();
    for (size_t i = 1; i < args.size() && is_contiguous_mem; i++) {
        const ArgSource &curr = args[i], &prev = args[i - 1];

        is_contiguous_mem =
                ArgVal::memory_relation(prev, curr) == ArgVal::consecutive;
    }

    if (is_contiguous_mem) {
        a.lea(ARG2, getArgRef(args[0]));
    } else {
        a.lea(ARG2, TMP_MEM3q);

        for (size_t i = 0; i < args.size(); i++) {
            mov_arg(x86::qword_ptr(ARG2, i * sizeof(Eterm)), args[i]);
        }
    }

    if (logger.file()) {
        ErtsCodeMFA *mfa = ubif2mfa((void *)Bif.get());
        if (mfa) {
            comment("UBIF: %T/%d", mfa->function, mfa->arity);
        }
    }
    mov_arg(ARG4, Bif);
}

void BeamModuleAssembler::emit_i_bif1(const ArgSource &Src1,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    emit_setup_guard_bif({Src1}, Bif);

    if (Fail.get() != 0) {
        safe_fragment_call(ga->get_i_bif_guard_shared());
        a.je(resolve_beam_label(Fail));
    } else {
        safe_fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bif2(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    emit_setup_guard_bif({Src1, Src2}, Bif);

    if (Fail.get() != 0) {
        safe_fragment_call(ga->get_i_bif_guard_shared());
        a.je(resolve_beam_label(Fail));
    } else {
        safe_fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_bif3(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgSource &Src3,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    emit_setup_guard_bif({Src1, Src2, Src3}, Bif);

    if (Fail.get() != 0) {
        safe_fragment_call(ga->get_i_bif_guard_shared());
        a.je(resolve_beam_label(Fail));
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

void BeamModuleAssembler::emit_nofail_bif1(const ArgSource &Src1,
                                           const ArgWord &Bif,
                                           const ArgRegister &Dst) {
    emit_setup_guard_bif({Src1}, Bif);
    safe_fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_nofail_bif2(const ArgSource &Src1,
                                           const ArgSource &Src2,
                                           const ArgWord &Bif,
                                           const ArgRegister &Dst) {
    emit_setup_guard_bif({Src1, Src2}, Bif);
    safe_fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_length_setup(const ArgLabel &Fail,
                                              const ArgWord &Live,
                                              const ArgSource &Src) {
    x86::Mem trap_state;

    /* Store trap state after the currently live registers. There's an extra 3
     * registers beyond the ordinary ones that we're free to use for whatever
     * purpose. */
    ERTS_CT_ASSERT(ERTS_X_REGS_ALLOCATED - MAX_REG >= 3);
    ASSERT(Live.get() <= MAX_REG);
    trap_state = getXRef(Live.get());

    /* Remainder of the list. */
    mov_arg(trap_state, Src);

    /* Accumulated length. */
    a.mov(trap_state.cloneAdjusted(1 * sizeof(Eterm)), imm(make_small(0)));

    /* Original argument. This is only needed for exceptions and can be safely
     * skipped in guards. */
    if (Fail.get() == 0) {
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

    emit_enter_frame();

    /* Save arguments for error/trapping path. */
    a.mov(TMP_MEM1q, ARG2);
    a.mov(TMP_MEM2q, ARG3);

    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    a.lea(ARG2, trap_state);
    runtime_call<2>(erts_trapping_length_1);

    emit_leave_runtime<Update::eReductions>();
    emit_leave_frame();

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
        a.add(x86::rsp, imm(sizeof(UWord)));

        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), imm(0));
        a.mov(x86::byte_ptr(c_p, offsetof(Process, arity)), ARG2.r8());
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
        a.jmp(labels[raise_exception]);
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
        a.sub(RET, RET);
        a.ret();
    }
}

void BeamModuleAssembler::emit_i_length(const ArgLabel &Fail,
                                        const ArgWord &Live,
                                        const ArgRegister &Dst) {
    Label entry = a.newLabel();

    align_erlang_cp();
    a.bind(entry);

    mov_arg(ARG2, Live);
    a.lea(ARG3, x86::qword_ptr(entry));

    if (Fail.get() != 0) {
        /* The return address is discarded when yielding, so it doesn't need to
         * be aligned. */
        safe_fragment_call(ga->get_i_length_guard_shared());
        a.je(resolve_beam_label(Fail));
    } else {
        safe_fragment_call(ga->get_i_length_body_shared());
    }

    mov_arg(Dst, RET);
}

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)

static Eterm debug_call_light_bif(Process *c_p,
                                  Eterm *reg,
                                  ErtsCodePtr I,
                                  ErtsBifFunc vbf) {
    Eterm result;

    ERTS_ASSERT_TRACER_REFS(&c_p->common);
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
    ERTS_ASSERT_TRACER_REFS(&c_p->common);

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
    x86::Mem entry_mem = TMP_MEM1q, export_mem = TMP_MEM2q,
             mbuf_mem = TMP_MEM3q;

    Label trace = a.newLabel(), yield = a.newLabel();

    emit_enter_frame();

    /* Spill everything we may need on the error and GC paths. */
    a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, mbuf)));
    a.mov(entry_mem, ARG3);
    a.mov(export_mem, ARG4);
    a.mov(mbuf_mem, ARG1);

    /* Check if we should trace this bif call or handle save_calls. Both
     * variants dispatch through the export entry. */
    a.cmp(x86::dword_ptr(ARG4, offsetof(Export, is_bif_traced)), imm(0));
    a.jne(trace);
    a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
    a.je(trace);

    a.dec(FCALLS);
    a.jle(yield);

    {
        Label check_bif_return = a.newLabel(), gc_after_bif_call = a.newLabel();

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.cmp(erts_msacc_cache, imm(0));
            a.short_().je(skip_msacc);

            a.mov(TMP_MEM4q, ARG3);

            a.mov(ARG1, erts_msacc_cache);
            a.mov(ARG2,
                  x86::qword_ptr(ARG4, offsetof(Export, info.mfa.module)));
            a.mov(ARG3, RET);
            runtime_call<3>(erts_msacc_set_bif_state);

            a.mov(ARG3, TMP_MEM4q);
            a.bind(skip_msacc);
        }
#endif

        {
            /* Call the BIF proper. ARG3 and RET have been set earlier. */
            a.mov(ARG1, c_p);
            load_x_reg_array(ARG2);

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
            a.mov(ARG4, RET);
            runtime_call<4>(debug_call_light_bif);
#else
            runtime_call(RET, 3);
#endif
        }

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.cmp(erts_msacc_cache, imm(0));
            a.short_().je(skip_msacc);
            {
                /* Update cache if it was changed in the BIF, stashing the
                 * return value in TMP_MEM4q. */
                a.mov(TMP_MEM4q, RET);
                a.lea(ARG1, erts_msacc_cache);
                runtime_call<1>(erts_msacc_update_cache);
                a.mov(RET, TMP_MEM4q);

                /* set state to emulator if msacc has been enabled */
                a.cmp(erts_msacc_cache, imm(0));
                a.short_().je(skip_msacc);

                a.mov(ARG1, erts_msacc_cache);
                a.mov(ARG2, imm(ERTS_MSACC_STATE_EMULATOR));
                a.mov(ARG3, imm(1));
                runtime_call<3>(erts_msacc_set_state_m__);
                a.mov(RET, TMP_MEM4q);
            }
            a.bind(skip_msacc);
        }
#endif

        /* We must update the active code index in case another process has
         * loaded new code, as the result of this BIF may be observable on
         * both ends.
         *
         * It doesn't matter whether the BIF modifies anything; if process
         * A loads new code and calls erlang:monotonic_time/0 soon after,
         * we'd break the illusion of atomic upgrades if process B still
         * ran old code after seeing a later timestamp from its own call to
         * erlang:monotonic_time/0. */
        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap | Update::eCodeIndex>();

        /* ERTS_IS_GC_DESIRED_INTERNAL */
        {
            /* Test whether GC is forced. */
            a.test(x86::dword_ptr(c_p, offsetof(Process, flags)),
                   imm(F_FORCE_GC | F_DISABLE_GC));
            a.jne(gc_after_bif_call);

            /* Test if binary heap size should trigger GC. */
            a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, bin_vheap_sz)));
            a.cmp(x86::qword_ptr(c_p, offsetof(Process, off_heap.overhead)),
                  ARG1);
            a.ja(gc_after_bif_call);

            /* Test if heap fragment size is larger than remaining heap size. */
            a.mov(ARG2, x86::qword_ptr(c_p, offsetof(Process, mbuf_sz)));
            a.lea(ARG1, x86::qword_ptr(HTOP, ARG2, 0, 3));
            a.cmp(E, ARG1);
            a.jl(gc_after_bif_call);
        }

        /* ! FALL THROUGH ! */
        a.bind(check_bif_return);
        {
            Label trap = a.newLabel(), error = a.newLabel();

            emit_test_the_non_value(RET);
            a.short_().je(trap);

            a.mov(getXRef(0), RET);

            emit_leave_frame();
            a.ret();

            a.bind(trap);
            {
                a.cmp(x86::qword_ptr(c_p, offsetof(Process, freason)),
                      imm(TRAP));
                a.short_().jne(error);

#if !defined(NATIVE_ERLANG_STACK)
                a.pop(getCPRef());
#endif

                /* Trap out, our return address is on the Erlang stack.
                 *
                 * The BIF_TRAP macros all set up c_p->arity and c_p->current,
                 * so we can use a simplified context switch. */
                a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
                a.jmp(labels[context_switch_simplified]);
            }

            a.bind(error);
            {
                a.mov(ARG2, entry_mem);
                a.mov(ARG4, export_mem);
                a.add(ARG4, imm(offsetof(Export, info.mfa)));

#if !defined(NATIVE_ERLANG_STACK)
                /* Discard the continuation pointer as it will never be
                 * used. */
                emit_unwind_frame();
#endif

                /* Overwrite the return address with the entry address to
                 * ensure that only the entry address ends up in the stack
                 * trace. */
                if (erts_frame_layout == ERTS_FRAME_LAYOUT_RA) {
                    a.mov(x86::qword_ptr(E), ARG2);
                } else {
                    ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);
                    a.mov(x86::qword_ptr(E, 8), ARG2);
                }

                a.jmp(labels[raise_exception_shared]);
            }
        }

        a.bind(gc_after_bif_call);
        {
            a.mov(ARG2, mbuf_mem);
            a.mov(ARG5, export_mem);
            a.movzx(ARG5d,
                    x86::byte_ptr(ARG5, offsetof(Export, info.mfa.arity)));

            emit_enter_runtime<Update::eReductions | Update::eStack |
                               Update::eHeap>();

            a.mov(ARG1, c_p);
            a.mov(ARG3, RET);
            load_x_reg_array(ARG4);
            runtime_call<5>(erts_gc_after_bif_call_lhf);

            emit_leave_runtime<Update::eReductions | Update::eStack |
                               Update::eHeap>();

            a.jmp(check_bif_return);
        }
    }

    a.bind(trace);
    {
        /* Tail call the export entry instead of the BIF. If we use the native
         * stack as the Erlang stack our return address is already on the
         * Erlang stack. Otherwise we will have to move the return address from
         * the native stack to the Erlang stack. */

        emit_leave_frame();

#if !defined(NATIVE_ERLANG_STACK)
        /* The return address must be on the Erlang stack. */
        a.pop(getCPRef());
#endif

        x86::Mem destination = emit_setup_dispatchable_call(ARG4);
        a.jmp(destination);
    }

    a.bind(yield);
    {
        a.movzx(ARG2d, x86::byte_ptr(ARG4, offsetof(Export, info.mfa.arity)));
        a.lea(ARG4, x86::qword_ptr(ARG4, offsetof(Export, info.mfa)));
        a.mov(x86::byte_ptr(c_p, offsetof(Process, arity)), ARG2.r8());
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG4);

        /* We'll find our way back through ARG3 (entry address). */
        emit_unwind_frame();

        a.jmp(labels[context_switch_simplified]);
    }
}

void BeamModuleAssembler::emit_call_light_bif(const ArgWord &Bif,
                                              const ArgExport &Exp) {
    Label entry = a.newLabel();

    align_erlang_cp();
    a.bind(entry);

    mov_arg(ARG4, Exp);
    a.mov(RET, imm(Bif.get()));
    a.lea(ARG3, x86::qword_ptr(entry));

    if (logger.file()) {
        BeamFile_ImportEntry *e = &beam->imports.entries[Exp.get()];
        comment("BIF: %T:%T/%d", e->module, e->function, e->arity);
    }
    fragment_call(ga->get_call_light_bif_shared());
}

void BeamModuleAssembler::emit_send() {
    Label entry = a.newLabel();

    /* This is essentially a mirror of call_light_bif, there's no point to
     * specializing send/2 anymore. We do it here because it's far more work to
     * do it in the loader. */
    align_erlang_cp();
    a.bind(entry);

    a.mov(ARG4, imm(BIF_TRAP_EXPORT(BIF_send_2)));
    a.mov(RET, imm(send_2));
    a.lea(ARG3, x86::qword_ptr(entry));

    fragment_call(ga->get_call_light_bif_shared());
}

void BeamModuleAssembler::emit_nif_start() {
    /* load time only instruction */
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

    emit_leave_frame();

#ifdef NATIVE_ERLANG_STACK
    if (erts_alcu_enable_code_atags) {
        /* See emit_i_test_yield. */
        a.mov(RET, x86::qword_ptr(E));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), RET);
    }

    a.ret();
#else
    a.mov(RET, getCPRef());
    a.mov(getCPRef(), imm(NIL));

    if (erts_alcu_enable_code_atags) {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), RET);
    }

    a.jmp(RET);
#endif

    a.bind(check_trap);
    a.cmp(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(TRAP));
    a.jne(error);
    {
        comment("yield");

        comment("test trap to hibernate");
        a.mov(ARG1d, x86::dword_ptr(c_p, offsetof(Process, flags)));
        a.mov(ARG2d, ARG1d);
        a.and_(ARG2d, imm(F_HIBERNATE_SCHED));
        a.short_().je(trap);

        comment("do hibernate trap");
        a.and_(ARG1d, imm(~F_HIBERNATE_SCHED));
        a.mov(x86::dword_ptr(c_p, offsetof(Process, flags)), ARG1d);
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
        a.jmp(labels[raise_exception_shared]);
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
    a.movzx(ARG5d, x86::byte_ptr(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.mov(x86::byte_ptr(c_p, offsetof(Process, arity)), ARG5.r8());
    a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), ARG3);

    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.short_().je(skip_msacc);

        a.mov(TMP_MEM1q, ARG3);
        a.mov(TMP_MEM2q, ARG5);

        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, x86::qword_ptr(ARG2, offsetof(ErtsCodeMFA, module)));
        a.mov(ARG3, ARG4);
        runtime_call<3>(erts_msacc_set_bif_state);
        a.mov(ARG4, RET);

        a.mov(ARG3, TMP_MEM1q);
        a.mov(ARG5, TMP_MEM2q);
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

    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.call_bif_nif) ==
                   sizeof(ErtsCodeInfo));

    ssize_t mfa_offset = offsetof(ErtsNativeFunc, trampoline.info.mfa) -
                         offsetof(ErtsNativeFunc, trampoline.call_bif_nif);
    a.lea(ARG2, x86::qword_ptr(ARG3, mfa_offset));

    ssize_t dfunc_offset = offsetof(ErtsNativeFunc, trampoline.dfunc) -
                           offsetof(ErtsNativeFunc, trampoline.call_bif_nif);
    a.mov(ARG4, x86::qword_ptr(ARG3, dfunc_offset));

    a.jmp(labels[call_bif_shared]);
}

void BeamModuleAssembler::emit_call_bif(const ArgWord &Func) {
    int mfa_offset = -(int)sizeof(ErtsCodeMFA);

    Label entry = a.newLabel();

    /* This is _always_ the first instruction in a function and replaces the
     * yield test that would otherwise add a frame, so we must add a frame
     * here. */
    emit_enter_frame();

    /* Yield entry point; must be after entering frame. */
    a.bind(entry);
    {
        a.lea(ARG2, x86::qword_ptr(current_label, mfa_offset));
        a.lea(ARG3, x86::qword_ptr(entry));
        mov_arg(ARG4, Func);

        a.jmp(resolve_fragment(ga->get_call_bif_shared()));
    }
}

void BeamModuleAssembler::emit_call_bif_mfa(const ArgAtom &M,
                                            const ArgAtom &F,
                                            const ArgWord &A) {
    BeamInstr func;
    Export *e;

    e = erts_active_export_entry(M.get(), F.get(), A.get());
    ASSERT(e != NULL && e->bif_number != -1);

    comment("HBIF: %T:%T/%d",
            e->info.mfa.module,
            e->info.mfa.function,
            A.get());
    func = (BeamInstr)bif_table[e->bif_number].f;
    emit_call_bif(ArgWord(func));
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

        comment("# Return address isn't word-aligned");
        a.ud2();

        a.bind(next);
    }
#endif

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_call_nif_early);

    emit_leave_runtime();

    /* We won't return to the original code. We KNOW that the stack points at
     * a return address. */
    a.add(x86::rsp, imm(8));

    /* Emulate `emit_call_nif`, loading the current (phony) instruction
     * pointer into ARG2. We push a (redundant) frame pointer to match the
     * corresponding `emit_leave_frame` in `call_nif_shared`. */
    emit_enter_frame();
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
    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.call_bif_nif) ==
                   sizeof(ErtsCodeInfo));
    a.mov(ARG3, x86::qword_ptr(c_p, offsetof(Process, i)));
    a.jmp(labels[call_nif_shared]);
}

void BeamGlobalAssembler::emit_call_nif_yield_helper() {
    Label yield = a.newLabel();

    if (erts_alcu_enable_code_atags) {
        /* See emit_i_test_yield. */
        a.mov(x86::qword_ptr(c_p, offsetof(Process, i)), ARG3);
    }

    a.dec(FCALLS);
    a.short_().jl(yield);
    a.jmp(labels[call_nif_shared]);

    a.bind(yield);
    {
        int mfa_offset = -(int)sizeof(ErtsCodeMFA);
        int arity_offset = mfa_offset + (int)offsetof(ErtsCodeMFA, arity);

        a.movzx(ARG1d, x86::byte_ptr(ARG3, arity_offset));
        a.mov(x86::byte_ptr(c_p, offsetof(Process, arity)), ARG1.r8());

        a.lea(ARG1, x86::qword_ptr(ARG3, mfa_offset));
        a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG1);

        /* Yield to `dispatch` rather than `entry` to avoid pushing too many
         * frames to the stack. See `emit_call_nif` for details. */
        a.add(ARG3, imm(BEAM_ASM_NFUNC_SIZE + sizeof(UWord[3])));
        a.jmp(labels[context_switch_simplified]);
    }
}

/* WARNING: This stub is memcpy'd, so all code herein must be explicitly
 * position-independent. */
void BeamModuleAssembler::emit_call_nif(const ArgWord &Func,
                                        const ArgWord &NifMod,
                                        const ArgWord &DirtyFunc) {
    Label entry = a.newLabel(), dispatch = a.newLabel();

    /* The start of this function must mimic the layout of ErtsNativeFunc.
     *
     * We jump here on the very first entry, pushing a stack frame if
     * applicable. */
    a.bind(entry);
    {
        emit_enter_frame();
        a.short_().jmp(dispatch); /* call_op */

        a.align(AlignMode::kCode, 8);
        /* ErtsNativeFunc.dfunc */
        a.embedUInt64(Func.get());
        /* ErtsNativeFunc.m */
        a.embedUInt64(NifMod.get());
        /* ErtsNativeFunc.func */
        a.embedUInt64(DirtyFunc.get());
    }

    /* `emit_call_nif_yield_helper` relies on this to compute the address of
     * `dispatch` */
    ASSERT((a.offset() - code.labelOffsetFromBase(current_label)) ==
           BEAM_ASM_NFUNC_SIZE + sizeof(UWord[3]));

    a.bind(dispatch);
    {
        a.lea(ARG3, x86::qword_ptr(current_label));
        pic_jmp(ga->get_call_nif_yield_helper());
    }
}

/* ARG2 = entry address. */
void BeamGlobalAssembler::emit_i_load_nif_shared() {
    static ErtsCodeMFA bif_mfa = {am_erlang, am_load_nif, 2};

    Label yield = a.newLabel(), error = a.newLabel();

    a.mov(TMP_MEM1q, ARG2);

    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    /* ARG2 has already been set by caller */
    load_x_reg_array(ARG3);
    runtime_call<3>(beam_jit_load_nif);

    emit_leave_runtime<Update::eHeapAlloc>();

    a.cmp(RET, RET_NIF_yield);
    a.short_().je(yield);

    /* We entered the frame in module code. */
    emit_leave_frame();

    a.cmp(RET, RET_NIF_success);
    a.short_().jne(error);
    a.ret();

    a.bind(error);
    {
        a.mov(ARG4, imm(&bif_mfa));
        a.jmp(labels[raise_exception]);
    }

    a.bind(yield);
    {
        a.mov(ARG3, TMP_MEM1q);
        a.jmp(labels[context_switch_simplified]);
    }
}

static ErtsCodePtr get_on_load_address(Process *c_p, Eterm module) {
    const Module *modp = erts_get_module(module, erts_active_code_ix());

    if (modp && modp->on_load) {
        const BeamCodeHeader *hdr = (modp->on_load)->code_hdr;

        if (hdr) {
            return erts_codeinfo_to_code(hdr->on_load);
        }
    }

    c_p->freason = BADARG;

    return NULL;
}

/* Implements the internal and undocumented erlang:call_on_load_function/1,
 * which is tricky to implement in the face of frame pointers. */
void BeamModuleAssembler::emit_i_call_on_load_function() {
    static ErtsCodeMFA mfa = {am_erlang, am_call_on_load_function, 1};
    Label next = a.newLabel();

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.mov(ARG2, getXRef(0));
    runtime_call<2>(get_on_load_address);

    emit_leave_runtime();

    a.test(RET, RET);
    a.jne(next);

    emit_raise_exception(&mfa);

    a.bind(next);
    erlang_call(RET, ARG1);
}

#ifdef NATIVE_ERLANG_STACK

void BeamModuleAssembler::emit_i_load_nif() {
    Label entry = a.newLabel(), yield = a.newLabel(), next = a.newLabel();

    /* i_load_nif is a rewrite of a call_ext instruction, so we'll body-call
     * ourselves to ensure the stack is consistent with that. This greatly
     * simplifies yielding and error handling. */
    fragment_call(entry);
    a.short_().jmp(next);

    align_erlang_cp();
    a.bind(entry);
    {
        emit_enter_frame();

        a.bind(yield);
        {
            a.lea(ARG2, x86::qword_ptr(yield));
            a.jmp(resolve_fragment(ga->get_i_load_nif_shared()));
        }
    }

    a.bind(next);
}

#else

void BeamModuleAssembler::emit_i_load_nif() {
    static ErtsCodeMFA mfa = {am_erlang, am_load_nif, 2};

    Label entry = a.newLabel(), next = a.newLabel(), schedule = a.newLabel();

    align_erlang_cp();
    a.bind(entry);

    emit_enter_runtime<Update::eHeapAlloc>();

    a.mov(ARG1, c_p);
    a.lea(ARG2, x86::qword_ptr(current_label));
    load_x_reg_array(ARG3);
    runtime_call<3>(beam_jit_load_nif);

    emit_leave_runtime<Update::eHeapAlloc>();

    a.cmp(RET, imm(RET_NIF_yield));
    a.je(schedule);
    a.cmp(RET, imm(RET_NIF_success));
    a.je(next);

    emit_raise_exception(current_label, &mfa);

    a.bind(schedule);
    {
        a.lea(ARG3, x86::qword_ptr(entry));
        a.jmp(resolve_fragment(ga->get_context_switch_simplified()));
    }

    a.bind(next);
}

#endif
