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

void BeamModuleAssembler::ubif_comment(const ArgWord &Bif) {
    if (logger.file()) {
        ErtsCodeMFA *mfa = ubif2mfa((void *)Bif.get());
        if (mfa) {
            comment("UBIF: %T/%d", mfa->function, mfa->arity);
        }
    }
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in ARG1 (will be THE_NON_VALUE if the BIF call failed). */
void BeamGlobalAssembler::emit_i_bif_guard_shared() {
    /* We use the X register array for the arguments for the BIF. The
     * actual contents of the first three X registers are kept safe in
     * callee-saved machine registers (XREG0 through XREG2).
     */
    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 2);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    lea(ARG2, getXRef(0));
    mov_imm(ARG3, 0);
    runtime_call(ARG4, 3); /* ARG3 is never used by guard BIFs. */

    emit_leave_runtime<Update::eReductions>();
    emit_leave_runtime_frame();
    a.ret(a64::x30);
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bif_body_shared() {
    Label error = a.newLabel();

    /* See comment in emit_i_bif_guard_shared. */
    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 2);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions>();

    /* Save current BIF for the error path. */
    a.mov(ARG1, c_p);
    lea(ARG2, getXRef(0));
    a.str(ARG4, TMP_MEM1q);
    mov_imm(ARG3, 0); /* ARG3 is never used by guard BIFs. */

    runtime_call(ARG4, 3);
    emit_branch_if_not_value(ARG1, error);

    emit_leave_runtime<Update::eReductions>();

    emit_leave_runtime_frame();
    a.ret(a64::x30);

    a.bind(error);
    {
        /* Find the correct MFA from the BIF's function address. */
        a.ldr(ARG1, TMP_MEM1q);
        runtime_call<1>(ubif2mfa);

        /* The argument registers must be reloaded on error, as the machine
         * registers may contain garbage, which will later be swapped into the
         * register array in the `raise_exception` fragment. */
        emit_leave_runtime<Update::eReductions | Update::eXRegs>(3);
        emit_leave_runtime_frame();

        a.mov(ARG4, ARG1);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_bif1(const ArgSource &Src1,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    auto src1 = load_source(Src1);

    a.str(src1.reg, getXRef(0));

    ubif_comment(Bif);
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif2(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);

    a.stp(src1.reg, src2.reg, getXRef(0));

    ubif_comment(Bif);
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif3(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgSource &Src3,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);
    auto src3 = load_source(Src3, TMP3);

    a.stp(src1.reg, src2.reg, getXRef(0));
    a.str(src3.reg, getXRef(2));

    ubif_comment(Bif);
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif(const ArgLabel &Fail,
                                     const ArgWord &Bif,
                                     const ArgRegister &Dst) {
    mov_arg(ARG4, Bif);

    if (Fail.get() != 0) {
        fragment_call(ga->get_i_bif_guard_shared());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, ARG1);
}

/*
 * Emit code for guard BIFs that can't fail (e.g. is_list/1).  We
 * don't need to test for failure.
 */

void BeamModuleAssembler::emit_nofail_bif1(const ArgSource &Src1,
                                           const ArgWord &Bif,
                                           const ArgRegister &Dst) {
    auto src1 = load_source(Src1);

    a.str(src1.reg, getXRef(0));

    ubif_comment(Bif);
    mov_arg(ARG4, Bif);
    fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_nofail_bif2(const ArgSource &Src1,
                                           const ArgSource &Src2,
                                           const ArgWord &Bif,
                                           const ArgRegister &Dst) {
    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);

    a.stp(src1.reg, src2.reg, getXRef(0));

    ubif_comment(Bif);
    mov_arg(ARG4, Bif);
    fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_length_setup(const ArgLabel &Fail,
                                              const ArgWord &Live,
                                              const ArgSource &Src) {
    /* Store trap state after the currently live registers. There are
     * 3 extra registers beyond the ordinary ones that we're free to
     * use for whatever purpose. */
    ERTS_CT_ASSERT(ERTS_X_REGS_ALLOCATED - MAX_REG >= 3);
    auto trap_reg1 = ArgXRegister(Live.get() + 0);
    auto trap_reg2 = ArgXRegister(Live.get() + 1);
    auto trap_reg3 = ArgXRegister(Live.get() + 2);

    auto src = load_source(Src, TMP1);
    auto dst1 = init_destination(trap_reg1, src.reg);
    auto dst2 = init_destination(trap_reg2, TMP2);

    mov_imm(dst2.reg, make_small(0));
    mov_var(dst1, src);

    /* Store original argument. This is only needed for exceptions and
     * can be safely skipped in guards. */
    if (Fail.get() != 0) {
        flush_vars(dst1, dst2);
    } else {
        auto dst3 = init_destination(trap_reg3, src.reg);
        mov_var(dst3, src);
        flush_vars(dst1, dst2, dst3);
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_common(Label fail, int state_size) {
    Label trap_or_error = a.newLabel();

    ASSERT(state_size >= 2 && state_size <= ERTS_X_REGS_ALLOCATED - MAX_REG);

    /* Save arguments for error/trapping path. */
    a.stp(ARG2, ARG3, TMP_MEM1q);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions | Update::eXRegs>();

    a.mov(ARG1, c_p);
    lea(TMP1, getXRef(0));
    a.add(ARG2, TMP1, ARG2, arm::lsl(3));
    runtime_call<2>(erts_trapping_length_1);

    emit_branch_if_not_value(ARG1, trap_or_error);

    emit_leave_runtime<Update::eReductions | Update::eXRegs>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);

    a.bind(trap_or_error);
    {
        a.ldp(ARG2, ARG3, TMP_MEM1q);
        a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.cmp(TMP1, imm(TRAP));
        a.b_ne(fail);

        emit_leave_runtime<Update::eReductions | Update::eXRegs>();
        emit_leave_runtime_frame();

        /* The trap state is stored in the registers above the current live
         * ones, so we add the state size (in words) to keep it alive. */
        a.add(ARG2, ARG2, imm(state_size));

        a.str(ZERO, arm::Mem(c_p, offsetof(Process, current)));
        a.strb(ARG2.w(), arm::Mem(c_p, offsetof(Process, arity)));

        /* We'll find our way back through the entry address (ARG3). */
        a.b(labels[context_switch_simplified]);
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_body_shared() {
    Label error = a.newLabel();
    /* `state_size = 3` to include the original argument. */
    emit_i_length_common(error, 3);

    a.bind(error);
    {
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_length, 1};

        /* Move the original argument to x0. It's stored in the third word of
         * the trap state. */
        lea(TMP1, getXRef(0));
        a.add(ARG2, TMP1, ARG2, arm::lsl(3));
        a.ldr(TMP1, arm::Mem(ARG2, sizeof(Eterm[2])));

        emit_leave_runtime<Update::eReductions | Update::eXRegs>();
        emit_leave_runtime_frame();

        a.mov(XREG0, TMP1);

        mov_imm(ARG4, &bif_mfa);
        emit_raise_exception();
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in ARG. Error is indicated by THE_NON_VALUE. */
void BeamGlobalAssembler::emit_i_length_guard_shared() {
    Label error = a.newLabel();

    emit_i_length_common(error, 2);

    a.bind(error);
    {
        emit_leave_runtime<Update::eReductions | Update::eXRegs>();
        emit_leave_runtime_frame();

        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_i_length(const ArgLabel &Fail,
                                        const ArgWord &Live,
                                        const ArgRegister &Dst) {
    Label entry = a.newLabel();

    a.bind(entry);

    mov_arg(ARG2, Live);
    a.adr(ARG3, entry);
    if (Fail.get() != 0) {
        fragment_call(ga->get_i_length_guard_shared());
        emit_branch_if_not_value(ARG1, resolve_beam_label(Fail, dispUnknown));
    } else {
        fragment_call(ga->get_i_length_body_shared());
    }

    mov_arg(Dst, ARG1);
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
 * ARG8 = BIF pointer
 */
void BeamGlobalAssembler::emit_call_light_bif_shared() {
    arm::Mem entry_mem = TMP_MEM1q, export_mem = TMP_MEM2q,
             mbuf_mem = TMP_MEM3q;

    Label trace = a.newLabel(), yield = a.newLabel();

    /* Spill everything we may need on the error and GC paths. */
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, mbuf)));
    a.stp(ARG3, ARG4, TMP_MEM1q);
    a.str(TMP1, mbuf_mem);

    /* Check if we should trace this bif call or handle save_calls. Both
     * variants dispatch through the export entry. */
    a.ldr(TMP1.w(), arm::Mem(ARG4, offsetof(Export, is_bif_traced)));
    a.cmp(TMP1, imm(0));
    a.ccmp(active_code_ix,
           imm(ERTS_SAVE_CALLS_CODE_IX),
           imm(NZCV::kZF),
           imm(arm::CondCode::kEQ));
    a.b_eq(trace);

    a.subs(FCALLS, FCALLS, imm(1));
    a.b_le(yield);
    {
        Label check_bif_return = a.newLabel(), gc_after_bif_call = a.newLabel();

        emit_enter_runtime_frame();
        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap | Update::eXRegs>(MAX_BIF_ARITY);

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.ldr(TMP1, erts_msacc_cache);
            a.cbz(TMP1, skip_msacc);

            /* The values of the X registers are in the X register array, so we
             * use XREG0 to save the entry pointer (ARG3) over this call. */
            a.mov(XREG0, ARG3);

            a.ldr(ARG1, erts_msacc_cache);
            a.ldr(ARG2, arm::Mem(ARG4, offsetof(Export, info.mfa.module)));
            a.mov(ARG3, ARG8);
            runtime_call<3>(erts_msacc_set_bif_state);

            a.mov(ARG8, ARG1);
            a.mov(ARG3, XREG0);

            a.bind(skip_msacc);
        }
#endif

        {
            /* Call the BIF proper. ARG3 and ARG8 have been set earlier. */
            a.mov(ARG1, c_p);
            load_x_reg_array(ARG2);

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
            a.mov(ARG4, ARG8);
            runtime_call<4>(debug_call_light_bif);
#else
            runtime_call(ARG8, 3);
#endif
        }

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.mov(XREG0, ARG1);

            a.ldr(TMP1, erts_msacc_cache);
            a.cbz(TMP1, skip_msacc);

            lea(ARG1, erts_msacc_cache);
            runtime_call<1>(erts_msacc_update_cache);

            /* Set state to emulator if msacc has been enabled */
            a.ldr(ARG1, erts_msacc_cache);
            a.cbz(ARG1, skip_msacc);

            mov_imm(ARG2, ERTS_MSACC_STATE_EMULATOR);
            mov_imm(ARG3, 1);
            runtime_call<3>(erts_msacc_set_state_m__);

            a.bind(skip_msacc);
            a.mov(ARG1, XREG0);
        }
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
        emit_leave_runtime<Update::eReductions | Update::eCodeIndex |
                           Update::eHeap | Update::eStack | Update::eXRegs>(
                MAX_BIF_ARITY);
        emit_leave_runtime_frame();

        /* ERTS_IS_GC_DESIRED_INTERNAL */
        {
            a.ldr(TMP1.w(), arm::Mem(c_p, offsetof(Process, flags)));
            a.tst(TMP1, imm(F_FORCE_GC | F_DISABLE_GC));

            a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, bin_vheap_sz)));
            a.ldr(TMP2, arm::Mem(c_p, offsetof(Process, off_heap.overhead)));

            /* If neither F_FORCE_GC nor F_DISABLE_GC were set,
             * test whether binary heap size should trigger GC.
             *
             * Otherwise, set the flags as if `off_heap.overhead > bin_vheap_sz`
             * to force a GC. */
            a.ccmp(TMP2, TMP1, imm(NZCV::kCF), imm(arm::CondCode::kEQ));

            a.sub(TMP1, E, HTOP);
            a.asr(TMP1, TMP1, imm(3));
            a.ldr(TMP2, arm::Mem(c_p, offsetof(Process, mbuf_sz)));

            /* If our binary heap size was small enough not to need a GC, check
             * whether the heap fragment size is larger than the remaining heap
             * size.
             *
             * Otherwise, set the flags as if it is to force a GC. */
            a.ccmp(TMP1, TMP2, imm(NZCV::kVF), imm(arm::CondCode::kLS));
            a.b_lt(gc_after_bif_call);
        }

        a.bind(check_bif_return);
        {
            Label error = a.newLabel(), trap = a.newLabel();

            emit_branch_if_not_value(ARG1, trap);

            a.mov(XREG0, ARG1);
            a.ret(a64::x30);

            a.bind(trap);
            {
                a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
                emit_branch_if_ne(TMP1, TRAP, error);

                /* Push our return address to the Erlang stack and trap out.
                 *
                 * The BIF_TRAP macros all set up c_p->arity and c_p->current,
                 * so we can use a simplified context switch. */
                emit_enter_erlang_frame();
                a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
                a.b(labels[context_switch_simplified]);
            }

            a.bind(error);
            {
                /* raise_exception_shared expects current PC in ARG2 and MFA in
                 * ARG4. */
                a.ldp(ARG2, ARG4, entry_mem);
                add(ARG4, ARG4, offsetof(Export, info.mfa));
                a.b(labels[raise_exception_shared]);
            }
        }

        a.bind(gc_after_bif_call);
        {
            emit_enter_runtime_frame();
            emit_enter_runtime<Update::eReductions | Update::eStack |
                               Update::eHeap | Update::eXRegs>(MAX_BIF_ARITY);

            a.mov(ARG3, ARG1);

            a.mov(ARG1, c_p);
            a.ldr(ARG2, mbuf_mem);
            load_x_reg_array(ARG4);
            a.ldr(ARG5, export_mem);
            a.ldrb(ARG5.w(), arm::Mem(ARG5, offsetof(Export, info.mfa.arity)));
            runtime_call<5>(erts_gc_after_bif_call_lhf);

            emit_leave_runtime<Update::eReductions | Update::eStack |
                               Update::eHeap | Update::eXRegs>(MAX_BIF_ARITY);
            emit_leave_runtime_frame();

            a.b(check_bif_return);
        }
    }

    a.bind(trace);
    {
        /* Call the export entry instead of the BIF. */
        branch(emit_setup_dispatchable_call(ARG4));
    }

    a.bind(yield);
    {
        a.ldrb(ARG2.w(), arm::Mem(ARG4, offsetof(Export, info.mfa.arity)));
        lea(ARG4, arm::Mem(ARG4, offsetof(Export, info.mfa)));
        a.strb(ARG2.w(), arm::Mem(c_p, offsetof(Process, arity)));
        a.str(ARG4, arm::Mem(c_p, offsetof(Process, current)));

        /* We'll find our way back through ARG3 (entry address). */
        a.b(labels[context_switch_simplified]);
    }
}

void BeamModuleAssembler::emit_call_light_bif(const ArgWord &Bif,
                                              const ArgExport &Exp) {
    Label entry = a.newLabel();
    BeamFile_ImportEntry *e = &beam->imports.entries[Exp.get()];

    a.bind(entry);

    mov_arg(ARG4, Exp);
    mov_arg(ARG8, Bif);
    a.adr(ARG3, entry);

    if (logger.file()) {
        comment("BIF: %T:%T/%d", e->module, e->function, e->arity);
    }
    fragment_call(ga->get_call_light_bif_shared());
}

void BeamModuleAssembler::emit_send() {
    Label entry = a.newLabel();

    /* This is essentially a mirror of call_light_bif, there's no point to
     * specializing send/2 anymore. We do it here because it's far more work to
     * do it in the loader. */
    a.bind(entry);

    a.ldr(ARG4, embed_constant(BIF_TRAP_EXPORT(BIF_send_2), disp32K));
    a.ldr(ARG8, embed_constant(send_2, disp32K));
    a.adr(ARG3, entry);

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

        a.ldr(TMP1, erts_msacc_cache);
        a.cbz(TMP1, skip_msacc);

        /* The values of the X registers are in the X register array,
         * so we can use XREG0 to save the contents of ARG1 during the
         * call. */
        a.mov(XREG0, ARG1);
        a.ldr(ARG1, erts_msacc_cache);
        mov_imm(ARG2, ERTS_MSACC_STATE_EMULATOR);
        mov_imm(ARG3, 1);
        runtime_call<3>(erts_msacc_set_state_m__);
        a.mov(ARG1, XREG0);

        a.bind(skip_msacc);
    }
#endif

    /* Another process may have loaded new code and somehow notified us through
     * this call, so we must update the active code index. */
    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions | Update::eCodeIndex>();

    emit_branch_if_not_value(ARG1, check_trap);

    comment("Do return and dispatch to it");
    a.mov(XREG0, ARG1);

    emit_leave_erlang_frame();

    if (erts_alcu_enable_code_atags) {
        /* See emit_i_test_yield. */
        a.str(a64::x30, arm::Mem(c_p, offsetof(Process, i)));
    }

    a.ret(a64::x30);

    a.bind(check_trap);
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    a.cmp(TMP1, imm(TRAP));
    a.b_ne(error);
    {
        comment("yield");

        comment("test trap to hibernate");
        a.ldr(TMP1.w(), arm::Mem(c_p, offsetof(Process, flags)));
        a.tbz(TMP1, imm(Support::ctz(F_HIBERNATE_SCHED)), trap);

        comment("do hibernate trap");
        a.and_(TMP1, TMP1, imm(~F_HIBERNATE_SCHED));
        a.str(TMP1.w(), arm::Mem(c_p, offsetof(Process, flags)));
        a.b(labels[do_schedule]);
    }

    a.bind(trap);
    {
        comment("do normal trap");

        /* The BIF_TRAP macros all set up c_p->arity and c_p->current, so we
         * can use a simplified context switch. */
        a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
        a.b(labels[context_switch_simplified]);
    }

    a.bind(error);
    {
        a.mov(ARG2, E);

        emit_enter_runtime();

        a.mov(ARG1, c_p);
        runtime_call<2>(erts_printable_return_address);

        emit_leave_runtime();

        a.mov(ARG2, ARG1);
        a.ldr(ARG4, arm::Mem(c_p, offsetof(Process, current)));
        a.b(labels[raise_exception_shared]);
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

    emit_enter_runtime_frame();
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, current)));
    /* `call_bif` wants arity in ARG5. */
    a.ldr(ARG5.w(), arm::Mem(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.strb(ARG5.w(), arm::Mem(c_p, offsetof(Process, arity)));
    a.str(ARG3, arm::Mem(c_p, offsetof(Process, i)));

    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.ldr(TMP1, erts_msacc_cache);
        a.cbz(TMP1, skip_msacc);

        /* The values of the X registers are in the X register array, so we can
         * use XREG0 and XREG1 to save the contents of the ARG* registers
         * during the call. */
        a.mov(XREG0, ARG3);
        a.mov(XREG1, ARG5);

        a.ldr(ARG1, erts_msacc_cache);
        a.ldr(ARG2, arm::Mem(ARG2, offsetof(ErtsCodeMFA, module)));
        a.mov(ARG3, ARG4);
        runtime_call<3>(erts_msacc_set_bif_state);
        a.mov(ARG4, ARG1);

        a.mov(ARG3, XREG0);
        a.mov(ARG5, XREG1);

        a.bind(skip_msacc);
    }
#endif

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    /* ARG3 (I), ARG4 (func), and ARG5 (arity) have already been provided. */
    runtime_call<5>(beam_jit_call_bif);

#ifdef ERTS_MSACC_EXTENDED_STATES
    /* The values of the X registers are in the X register array, so we can use
     * XREG0 to save the contents of ARG1 during the call. */
    a.mov(XREG0, ARG1);
    lea(ARG1, erts_msacc_cache);
    runtime_call<1>(erts_msacc_update_cache);
    a.mov(ARG1, XREG0);
#endif

    emit_leave_runtime_frame();
    emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_bif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure. */
    a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));

    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.call_bif_nif) ==
                   sizeof(ErtsCodeInfo));

    ssize_t mfa_offset = offsetof(ErtsNativeFunc, trampoline.call_bif_nif) -
                         offsetof(ErtsNativeFunc, trampoline.info.mfa);

    a.sub(ARG2, ARG3, imm(mfa_offset));

    ssize_t dfunc_offset = offsetof(ErtsNativeFunc, trampoline.dfunc) -
                           offsetof(ErtsNativeFunc, trampoline.call_bif_nif);
    a.ldr(ARG4, arm::Mem(ARG3, dfunc_offset));

    a.b(labels[call_bif_shared]);
}

/* This is only used for opcode compatibility with the interpreter, it's never
 * actually called. */
void BeamModuleAssembler::emit_call_bif(const ArgWord &Func) {
    (void)Func;

    emit_nyi("emit_call_bif");
}

void BeamModuleAssembler::emit_call_bif_mfa(const ArgAtom &M,
                                            const ArgAtom &F,
                                            const ArgWord &A) {
    BeamInstr func;
    Export *e;

    e = erts_active_export_entry(M.get(), F.get(), A.get());
    ASSERT(e != NULL && e->bif_number != -1);

    func = (BeamInstr)bif_table[e->bif_number].f;

    a.adr(ARG3, current_label);
    a.sub(ARG2, ARG3, imm(sizeof(ErtsCodeMFA)));
    comment("HBIF: %T:%T/%d",
            e->info.mfa.module,
            e->info.mfa.function,
            A.get());
    a.mov(ARG4, imm(func));

    a.b(resolve_fragment(ga->get_call_bif_shared(), disp128MB));
}

void BeamGlobalAssembler::emit_call_nif_early() {
    a.mov(ARG2, a64::x30);
    a.sub(ARG2, ARG2, imm(BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(ErtsCodeInfo)));

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_call_nif_early);

    emit_leave_runtime();

    /* Emulate `emit_call_nif`, loading the current (phony) instruction
     * pointer into ARG3.
     *
     * Note that we "inherit" the frame that was pushed to the stack prior to
     * running the breakpoint instruction, discarding the current content of
     * LR (x30). */
    a.mov(ARG3, ARG1);
    a.b(labels[call_nif_shared]);
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
    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.ldr(TMP1, erts_msacc_cache);
        a.cbz(TMP1, skip_msacc);

        /* The values of the X registers are in the X register array,
         * so we can use XREG0 to save the contents of ARG3 during the
         * call. */
        a.mov(XREG0, ARG3);
        a.ldr(ARG1, erts_msacc_cache);
        mov_imm(ARG2, ERTS_MSACC_STATE_NIF);
        mov_imm(ARG3, 1);
        runtime_call<3>(erts_msacc_set_state_m__);
        a.mov(ARG3, XREG0);

        a.bind(skip_msacc);
    }
#endif

    a.mov(ARG1, c_p);
    a.mov(ARG2, ARG3);
    load_x_reg_array(ARG3);
    ERTS_CT_ASSERT((4 + BEAM_ASM_FUNC_PROLOGUE_SIZE) % sizeof(UWord) == 0);
    a.ldr(ARG4, arm::Mem(ARG2, 4 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    a.ldr(ARG5, arm::Mem(ARG2, 12 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    a.ldr(ARG6, arm::Mem(ARG2, 16 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    runtime_call<5>(beam_jit_call_nif);

    emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure.
     *
     * ErtsNativeFunc already follows the NIF call layout, so we don't need to
     * do anything beyond loading the address. */
    a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
    a.b(labels[call_nif_shared]);
}

void BeamGlobalAssembler::emit_call_nif_yield_helper() {
    Label yield = a.newLabel();

    if (erts_alcu_enable_code_atags) {
        /* See emit_i_test_yield. */
        a.str(ARG3, arm::Mem(c_p, offsetof(Process, i)));
    }

    a.subs(FCALLS, FCALLS, imm(1));
    a.b_le(yield);
    a.b(labels[call_nif_shared]);

    a.bind(yield);
    {
        int mfa_offset = sizeof(ErtsCodeMFA);
        int arity_offset = offsetof(ErtsCodeMFA, arity) - mfa_offset;

        a.ldur(TMP1.w(), arm::Mem(ARG3, arity_offset));
        a.strb(TMP1.w(), arm::Mem(c_p, offsetof(Process, arity)));

        a.sub(TMP1, ARG3, imm(mfa_offset));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, current)));

        /* Yield to `dispatch` rather than `entry` to avoid pushing too many
         * frames to the stack. See `emit_call_nif` for details. */
        a.add(ARG3, ARG3, imm(BEAM_ASM_NFUNC_SIZE + sizeof(UWord[3])));
        a.b(labels[context_switch_simplified]);
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
     * We jump here on the very first entry. */
    a.bind(entry);
    {
        a.b(dispatch);

        /* Everything prior to this, including the breakpoint, is part of the
         * `call_bif_nif` field. */
        ASSERT(a.offset() % sizeof(UWord) == 0);

        /* ErtsNativeFunc.func */
        a.embedUInt64(Func.get());

        /* ErtsNativeFunc.m */
        a.embedUInt64(NifMod.get());

        /* ErtsNativeFunc.dfunc */
        a.embedUInt64(DirtyFunc.get());
    }

    /* `emit_call_nif_yield_helper` relies on this to compute the address of
     * `dispatch` */
    ASSERT((a.offset() - code.labelOffsetFromBase(current_label)) ==
           BEAM_ASM_NFUNC_SIZE + sizeof(UWord[3]));

    a.bind(dispatch);
    {
        a.adr(ARG3, current_label);
        pic_jmp(ga->get_call_nif_yield_helper());
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
 * which is very tricky to implement as a BIF. */
void BeamModuleAssembler::emit_i_call_on_load_function() {
    static ErtsCodeMFA mfa = {am_erlang, am_call_on_load_function, 1};
    Label next = a.newLabel();

    a.mov(ARG2, XREG0);

    /* The first X register must be preserved for the error path. */
    emit_enter_runtime(1);

    a.mov(ARG1, c_p);
    runtime_call<2>(get_on_load_address);

    emit_leave_runtime(1);

    a.cbnz(ARG1, next);
    emit_raise_exception(&mfa);

    a.bind(next);
    erlang_call(ARG1);
}

void BeamModuleAssembler::emit_i_load_nif() {
    static ErtsCodeMFA mfa = {am_erlang, am_load_nif, 2};

    Label entry = a.newLabel(), next = a.newLabel(), schedule = a.newLabel();

    a.bind(entry);

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs>(2);

    a.mov(ARG1, c_p);
    a.adr(ARG2, current_label);
    load_x_reg_array(ARG3);
    runtime_call<3>(beam_jit_load_nif);

    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs>(2);

    a.cmp(ARG1, imm(RET_NIF_yield));
    a.b_eq(schedule);

    a.cmp(ARG1, imm(RET_NIF_success));
    a.b_eq(next);

    emit_raise_exception(current_label, &mfa);

    a.bind(schedule);
    {
        a.adr(ARG3, entry);
        a.b(resolve_fragment(ga->get_context_switch_simplified(), disp128MB));
    }

    a.bind(next);
}
