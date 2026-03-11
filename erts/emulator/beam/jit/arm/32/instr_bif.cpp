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

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    mov_imm(ARG3, 0);
    runtime_call(ARG4, 3); /* ARG3 is never used by guard BIFs. */

    emit_leave_runtime<Update::eReductions>();
    emit_leave_runtime_frame();
    a.bx(a32::lr);
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bif_body_shared() {
    Label error = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions>();

    /* Save current argument vector and BIF for the error path. */
    a.mov(ARG1, c_p);
    a.str(ARG2, TMP_MEM1q);
    a.str(ARG4, TMP_MEM2q);
    mov_imm(ARG3, 0); /* ARG3 is never used by guard BIFs. */

    runtime_call(ARG4, 3);
    emit_branch_if_not_value(ARG1, error);

    emit_leave_runtime<Update::eReductions>();

    emit_leave_runtime_frame();
    a.bx(a32::lr);

    a.bind(error);
    {
        /* Copy arguments into x-registers from the argument vector. We don't
         * need to care about actual arity since x-registers are clobbered
         * on exceptions. */
        a.ldr(ARG2, TMP_MEM1q);
        a.ldr(ARG1, arm::Mem(ARG2, 0 * sizeof(Eterm)));
        a.str(ARG1, getXRef(0));
        a.ldr(ARG1, arm::Mem(ARG2, 1 * sizeof(Eterm)));
        a.str(ARG1, getXRef(1));
        a.ldr(ARG1, arm::Mem(ARG2, 2 * sizeof(Eterm)));
        a.str(ARG1, getXRef(2));

        /* Find the correct MFA from the BIF's function address. */
        a.ldr(ARG1, TMP_MEM2q);
        runtime_call<1>(ubif2mfa);

        /* The argument registers must be reloaded on error, as the machine
         * registers may contain garbage, which will later be swapped into the
         * register array in the `raise_exception` fragment. */
        emit_leave_runtime<Update::eReductions>();
        emit_leave_runtime_frame();

        a.mov(ARG4, ARG1);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_bif1(const ArgSource &Src1,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    auto src1 = load_source(Src1, ARG1);
    a.str(src1.reg, TMP_MEM3q);
    lea(ARG2, TMP_MEM3q);

    ubif_comment(Bif);
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif2(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    auto [src1, src2] = load_sources(Src1, ARG1, Src2, ARG2);

    a.str(src1.reg, TMP_MEM3q);
    a.str(src2.reg, TMP_MEM4q);
    lea(ARG2, TMP_MEM3q);

    ubif_comment(Bif);
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif3(const ArgSource &Src1,
                                      const ArgSource &Src2,
                                      const ArgSource &Src3,
                                      const ArgLabel &Fail,
                                      const ArgWord &Bif,
                                      const ArgRegister &Dst) {
    auto [src1, src2] = load_sources(Src1, ARG1, Src2, ARG2);
    auto src3 = load_source(Src3, ARG3);

    // TMP_MEM1q, TMP_MEM2q are used by the error path in emit_i_bif
    a.str(src1.reg, TMP_MEM3q);
    a.str(src2.reg, TMP_MEM4q);
    a.str(src3.reg, TMP_MEM5q);
    lea(ARG2, TMP_MEM3q);

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
    auto src1 = load_source(Src1, ARG1);

    a.str(src1.reg, TMP_MEM3q);
    lea(ARG2, TMP_MEM3q);

    ubif_comment(Bif);
    mov_arg(ARG4, Bif);
    fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_nofail_bif2(const ArgSource &Src1,
                                           const ArgSource &Src2,
                                           const ArgWord &Bif,
                                           const ArgRegister &Dst) {
    auto [src1, src2] = load_sources(Src1, ARG1, Src2, ARG2);

    a.str(src1.reg, TMP_MEM3q);
    a.str(src2.reg, TMP_MEM4q);
    lea(ARG2, TMP_MEM3q);

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

    auto src = load_source(Src, ARG1);
    auto dst1 = init_destination(trap_reg1, src.reg);
    auto dst2 = init_destination(trap_reg2, ARG2);

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
    a.str(ARG2, TMP_MEM1q);
    a.str(ARG3, TMP_MEM2q);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions>();

    a.mov(ARG1, c_p);
    lea(TMP, getXRef(0));
    a.add(ARG2, TMP, ARG2, arm::lsl(2));
    runtime_call<2>(erts_trapping_length_1);

    emit_branch_if_not_value(ARG1, trap_or_error);

    emit_leave_runtime<Update::eReductions>();
    emit_leave_runtime_frame();

    a.bx(a32::lr);

    a.bind(trap_or_error);
    {
        a.ldr(ARG2, TMP_MEM1q);
        a.ldr(ARG3, TMP_MEM2q);
        a.ldr(TMP, arm::Mem(c_p, offsetof(Process, freason)));
        a.cmp(TMP, imm(TRAP));
        a.b_ne(fail);

        emit_leave_runtime<Update::eReductions>();
        emit_leave_runtime_frame();

        /* The trap state is stored in the registers above the current live
         * ones, so we add the state size (in words) to keep it alive. */
        a.add(ARG2, ARG2, imm(state_size));

        mov_imm(TMP, 0);
        a.str(TMP, arm::Mem(c_p, offsetof(Process, current)));
        a.strb(ARG2, arm::Mem(c_p, offsetof(Process, arity)));

        /* We'll find our way back through the entry address (ARG3). */
        a.b(labels[context_switch_simplified]);
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in ARG1. */
void BeamGlobalAssembler::emit_i_length_body_shared() {
    Label error = a.newLabel();
    /* `state_size = 3` to include the original argument. */
    emit_i_length_common(error, 3);

    a.bind(error);
    {
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_length, 1};

        /* Move the original argument to x0. It's stored in the third word of
         * the trap state. */
        lea(TMP, getXRef(0));
        a.add(ARG2, TMP, ARG2, arm::lsl(2));
        a.ldr(TMP, arm::Mem(ARG2, sizeof(Eterm[2])));

        emit_leave_runtime<Update::eReductions>();
        emit_leave_runtime_frame();

        a.str(TMP, getXRef(0));

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
        emit_leave_runtime<Update::eReductions>();
        emit_leave_runtime_frame();

        a.bx(a32::lr);
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
 * ARG2 = BIF pointer
 * ARG3 = entry
 * ARG4 = export entry
 */
void BeamGlobalAssembler::emit_call_light_bif_shared() {
    arm::Mem entry_mem = TMP_MEM1q, export_mem = TMP_MEM2q,
             mbuf_mem = TMP_MEM3q;

    Label trace = a.newLabel(), yield = a.newLabel(), skip_trace = a.newLabel();

    /* Spill everything we may need on the error and GC paths. */
    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, mbuf)));
    a.str(TMP, mbuf_mem);
    lea(TMP, TMP_MEM1q);
    a.stmia(arm::Mem(TMP), a32::GpList({ARG3, ARG4}));

    /* Check if we should trace this bif call or handle save_calls. Both
     * variants dispatch through the export entry. */
    a.ldr(TMP, arm::Mem(ARG4, offsetof(Export, is_bif_traced)));
    a.tst(TMP, TMP);
    a.b_ne(skip_trace);
    a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
    a.b_eq(trace);
    a.bind(skip_trace);

    a.subs(FCALLS, FCALLS, imm(1));
    a.b_le(yield);
    {
        Label check_bif_return = a.newLabel(), gc_after_bif_call = a.newLabel();

        emit_enter_runtime_frame();
        emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

#ifdef ERTS_MSACC_EXTENDED_STATES
        //TODO
        ASSERT(false);
#endif

        {
            // ARG2 has been set to the BIF pointer, save it to TMP
            a.mov(TMP, ARG2);

            /* Call the BIF proper. ARG3 has been set earlier. */
            a.mov(ARG1, c_p);
            load_x_reg_array(ARG2);

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
            a.mov(ARG4, TMP);
            runtime_call<4>(debug_call_light_bif);
#else
            runtime_call(TMP, 3);
#endif
        }

#ifdef ERTS_MSACC_EXTENDED_STATES
        //TOOD
        ASSERT(false);
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
                           Update::eHeap | Update::eStack>();
        emit_leave_runtime_frame();

        /* ERTS_IS_GC_DESIRED_INTERNAL */
        {
            Label check_fragments = a.newLabel();

            /* Test whether GC is forced. */
            a.ldr(TMP, arm::Mem(c_p, offsetof(Process, flags)));
            a.tst(TMP, imm(F_FORCE_GC | F_DISABLE_GC));
            a.b_ne(gc_after_bif_call);

            /* Test if binary heap size should trigger GC. */
            a.ldr(TMP, arm::Mem(c_p, offsetof(Process, bin_vheap_sz)));
            a.ldr(VAR, arm::Mem(c_p, offsetof(Process, off_heap.overhead)));
            a.cmp(VAR, TMP);
            a.b_ls(check_fragments);
            a.b(gc_after_bif_call);

            /* Test if heap fragment size is larger than remaining heap size. */
            a.bind(check_fragments);
            a.sub(TMP, E, HTOP);
            a.ldr(VAR, arm::Mem(c_p, offsetof(Process, mbuf_sz)));
            a.lsl(VAR, VAR, imm(3));
            a.cmp(TMP, VAR);
            a.b_lo(gc_after_bif_call);
        }

        a.bind(check_bif_return);
        {
            Label trap = a.newLabel(), error = a.newLabel();

            emit_branch_if_not_value(ARG1, trap);

            a.str(ARG1, getXRef(0));
            a.bx(a32::lr);

            a.bind(trap);
            {
                a.ldr(TMP, arm::Mem(c_p, offsetof(Process, freason)));
                a.cmp(TMP, imm(TRAP));
                a.b_ne(error);

                /* Trap out, preserving our continuation on the Erlang stack. */
                emit_enter_erlang_frame();
                a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
                a.b(labels[context_switch_simplified]);
            }

            a.bind(error);
            {
                a.ldr(ARG2, entry_mem);
                a.ldr(ARG4, export_mem);
                add(ARG4, ARG4, offsetof(Export, info.mfa));
                a.b(labels[raise_exception_shared]);
            }
        }

        a.bind(gc_after_bif_call);
        {
            emit_enter_runtime_frame();
            emit_enter_runtime<Update::eReductions | Update::eStack |
                               Update::eHeap>();

            a.mov(ARG3, ARG1);
            a.mov(ARG1, c_p);
            a.ldr(ARG2, mbuf_mem);
            load_x_reg_array(ARG4);
            a.ldr(TMP, export_mem);
            a.ldrb(TMP, arm::Mem(TMP, offsetof(Export, info.mfa.arity)));
            a.sub(a32::sp, a32::sp, imm(8));
            a.str(TMP, arm::Mem(a32::sp, 0));
            runtime_call<5>(erts_gc_after_bif_call_lhf);
            a.add(a32::sp, a32::sp, imm(8));

            emit_leave_runtime<Update::eReductions | Update::eStack |
                               Update::eHeap>();
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
        a.ldrb(ARG2, arm::Mem(ARG4, offsetof(Export, info.mfa.arity)));
        add(ARG4, ARG4, offsetof(Export, info.mfa));
        a.strb(ARG2, arm::Mem(c_p, offsetof(Process, arity)));
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
    mov_arg(ARG2, Bif);
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

    a.ldr(ARG4, embed_constant(BIF_TRAP_EXPORT(BIF_send_2), disp4KB));
    a.ldr(ARG2, embed_constant(send_2, disp4KB));
    a.adr(ARG3, entry);

    fragment_call(ga->get_call_light_bif_shared());
}

void BeamModuleAssembler::emit_nif_start() {
    // TODO
    emit_nyi("emit_nif_start");
}

void BeamGlobalAssembler::emit_bif_nif_epilogue(void) {
    Label check_trap = a.newLabel(), trap = a.newLabel(), error = a.newLabel();

#ifdef ERTS_MSACC_EXTENDED_STATES
    ASSERT(false);
#endif

    /* Another process may have loaded new code and somehow notified us through
     * this call, so we must update the active code index. */
    emit_leave_runtime<Update::eStack | Update::eHeap |
                       Update::eReductions | Update::eCodeIndex>();

    emit_branch_if_not_value(ARG1, check_trap);

    comment("Do return and dispatch to it");
    a.str(ARG1, getXRef(0));

    emit_leave_erlang_frame();

    if (erts_alcu_enable_code_atags) {
        /* See emit_i_test_yield. */
        a.str(a32::lr, arm::Mem(c_p, offsetof(Process, i)));
    }
    
    a.bx(a32::lr);

    a.bind(check_trap);
    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, freason)));
    a.cmp(TMP, imm(TRAP));
    a.b_ne(error);
    {
        comment("yield");

        comment("test trap to hibernate");
        a.ldr(TMP, arm::Mem(c_p, offsetof(Process, flags)));
        a.tst(TMP, imm(F_HIBERNATE_SCHED));
        a.b_eq(trap);

        comment("do hibernate trap");
        mov_imm(VAR, ~F_HIBERNATE_SCHED);
        a.and_(TMP, TMP, VAR);
        a.str(TMP, arm::Mem(c_p, offsetof(Process, flags)));
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

    a.ldr(TMP, arm::Mem(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.strb(TMP, arm::Mem(c_p, offsetof(Process, arity)));
    a.str(ARG3, arm::Mem(c_p, offsetof(Process, i)));
 
    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eStack | Update::eHeap |
                       Update::eReductions>();
 
 #ifdef ERTS_MSACC_EXTENDED_STATES
    {
        ASSERT(false);
    }
 #endif
 
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    /* ARG3 (I), ARG4 (func) have already been provided.
     * `call_bif` wants arity as fifth argument. 
     * This requires us to allocate it on the stack.
     */
    a.sub(a32::sp, a32::sp, imm(8)); // keep 8-byte alignment
    a.str(TMP, arm::Mem(a32::sp, 0)); // store arity on the stack
    runtime_call<5>(beam_jit_call_bif);
    a.add(a32::sp, a32::sp, imm(8)); // delete arity from the stack
 
 #ifdef ERTS_MSACC_EXTENDED_STATES
    ASSERT(false); 
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
    // TODO
    emit_nyi("emit_call_bif");
}

void BeamModuleAssembler::emit_call_bif_mfa(const ArgAtom &M,
                                            const ArgAtom &F,
                                            const ArgWord &A) {
    // TODO
    emit_nyi("emit_call_bif_mfa");
}

void BeamGlobalAssembler::emit_call_nif_early() {
    // TODO
    emit_nyi("emit_call_nif_early");
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
    emit_enter_runtime<Update::eStack | Update::eHeap |
                       Update::eReductions>();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        ASSERT(false);
    }
#endif

    a.mov(ARG1, c_p);
    a.mov(ARG2, ARG3);
    load_x_reg_array(ARG3);
    ERTS_CT_ASSERT((4 + BEAM_ASM_FUNC_PROLOGUE_SIZE) % sizeof(UWord) == 0);
    a.ldr(ARG4, arm::Mem(ARG2, 4 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    // Loading NifMod as ARG5
    a.ldr(TMP, arm::Mem(ARG2, 12 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    
    a.sub(a32::sp, a32::sp, imm(8));        // keep AAPCS alignment
    a.str(TMP, arm::Mem(a32::sp, 0));       // arg5 at [sp]
    runtime_call<5>(beam_jit_call_nif);
    a.add(a32::sp, a32::sp, imm(8));

    emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
    // TODO
    emit_nyi("emit_dispatch_nif");
}

void BeamGlobalAssembler::emit_call_nif_yield_helper() {
    // TODO
    emit_nyi("emit_call_nif_yield_helper");
}

/* WARNING: This stub is memcpy'd, so all code herein must be explicitly
 * position-independent. */
void BeamModuleAssembler::emit_call_nif(const ArgWord &Func,
                                        const ArgWord &NifMod,
                                        const ArgWord &DirtyFunc) {
    // TODO
    emit_nyi("emit_call_nif");
}

static ErtsCodePtr get_on_load_address(Process *c_p, Eterm module) {
    // TODO
    ASSERT(false);
    return NULL;
}

/* Implements the internal and undocumented erlang:call_on_load_function/1,
 * which is very tricky to implement as a BIF. */
void BeamModuleAssembler::emit_i_call_on_load_function() {
    // TODO
    emit_nyi("emit_i_call_on_load_function");
}

void BeamModuleAssembler::emit_i_load_nif() {
    // TODO
    emit_nyi("emit_i_load_nif");
}
