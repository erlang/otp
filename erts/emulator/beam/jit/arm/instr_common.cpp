/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

/*
 * Notes.
 *
 * The immediate operand for the and, orr, eor, and tst instructions
 * follow special rules.
 *
 * For our purposes, only bit patterns consisting of 1 through 63 ones
 * at any position in a word are possible to encode as an
 * immediate. Other patterns must be loaded into a tempoary register.
 *
 * Here are some examples of possible immediate values:
 *
 *    0b00000011
 *    0b00001111
 *    0b00111100
 *
 *    0xFFFFFFFFFFFFFFF0
 *    0x100000000000000F
 *
 * The last one is possible because it is the pattern 0x1F
 * (0b00011111) rotated right one position.
 *
 * Here is an example of mask that is not a possible to encode as an
 * immediate:
 *
 *    0b111011
 *
 * For more about the encoding rules, see:
 *
 * https://stackoverflow.com/questions/30904718/range-of-immediate-values-in-armv8-a64-assembly
 *
 */

#include <algorithm>
#include <numeric>
#include "beam_asm.hpp"

extern "C"
{
#include "erl_bif_table.h"
#include "big.h"
#include "beam_catches.h"
#include "beam_common.h"
#include "code_ix.h"
#include "erl_binary.h"
#include "erl_map.h"
}

using namespace asmjit;

/* Helpers */

void BeamModuleAssembler::emit_error(int reason) {
    mov_imm(TMP1, reason);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    emit_raise_exception();
}

void BeamModuleAssembler::emit_error(int reason, const ArgSource &Src) {
    auto src = load_source(Src, TMP2);

    mov_imm(TMP1, reason);
    ERTS_CT_ASSERT_FIELD_PAIR(Process, freason, fvalue);
    a.stp(TMP1, src.reg, arm::Mem(c_p, offsetof(Process, freason)));
    emit_raise_exception();
}

void BeamModuleAssembler::emit_gc_test_preserve(const ArgWord &Need,
                                                const ArgWord &Live,
                                                const ArgSource &Preserve,
                                                a64::Gp preserve_reg) {
    const int32_t bytes_needed = (Need.get() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

#ifdef DEBUG
    comment("(debug: fill dead X registers with garbage)");
    const a64::Gp garbage_reg = preserve_reg == ARG4 ? ARG3 : ARG4;
    mov_imm(garbage_reg, ERTS_HOLE_MARKER);
    if (!(Preserve.isXRegister() &&
          Preserve.as<ArgXRegister>().get() >= Live.get())) {
        mov_arg(ArgXRegister(Live.get()), garbage_reg);
        mov_arg(ArgXRegister(Live.get() + 1), garbage_reg);
    } else {
        mov_imm(garbage_reg, ERTS_HOLE_MARKER);
        mov_arg(ArgXRegister(Live.get() + 1), garbage_reg);
        mov_arg(ArgXRegister(Live.get() + 2), garbage_reg);
    }
#endif

    add(ARG3, HTOP, bytes_needed);
    a.cmp(ARG3, E);
    a.b_ls(after_gc_check);

    ASSERT(Live.get() < ERTS_X_REGS_ALLOCATED);

    /* We don't need to stash the preserved term if it's currently live, making
     * the code slightly shorter. */
    if (!(Preserve.isXRegister() &&
          Preserve.as<ArgXRegister>().get() >= Live.get())) {
        mov_imm(ARG4, Live.get());
        fragment_call(ga->get_garbage_collect());
        mov_arg(preserve_reg, Preserve);
    } else {
        mov_arg(ArgXRegister(Live.get()), preserve_reg);

        mov_imm(ARG4, Live.get() + 1);
        fragment_call(ga->get_garbage_collect());

        mov_arg(preserve_reg, ArgXRegister(Live.get()));
    }

    a.bind(after_gc_check);
}

void BeamModuleAssembler::emit_gc_test(const ArgWord &Ns,
                                       const ArgWord &Nh,
                                       const ArgWord &Live) {
    int32_t bytes_needed = (Ns.get() + Nh.get() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

#ifdef DEBUG
    comment("(debug: fill dead X registers with garbage)");
    mov_imm(ARG4, ERTS_HOLE_MARKER);
    mov_arg(ArgXRegister(Live.get()), ARG4);
    mov_arg(ArgXRegister(Live.get() + 1), ARG4);
#endif

    add(ARG3, HTOP, bytes_needed);
    a.cmp(ARG3, E);
    a.b_ls(after_gc_check);

    mov_imm(ARG4, Live.get());
    fragment_call(ga->get_garbage_collect());

    a.bind(after_gc_check);
}

void BeamModuleAssembler::emit_validate(const ArgWord &Arity) {
#ifdef DEBUG
    Label next = a.newLabel(), crash = a.newLabel();

    /* Crash if the Erlang heap is not word-aligned */
    a.tst(HTOP, imm(sizeof(Eterm) - 1));
    a.b_ne(crash);

    /* Crash if the Erlang stack is not word-aligned */
    a.tst(E, imm(sizeof(Eterm) - 1));
    a.b_ne(crash);

    /* Crash if we've overrun the stack */
    lea(TMP1, arm::Mem(E, -(int32_t)(S_REDZONE * sizeof(Eterm))));
    a.cmp(HTOP, TMP1);
    a.b_hi(crash);

    a.b(next);

    a.bind(crash);
    a.udf(0xbad);
    a.bind(next);

#    ifdef JIT_HARD_DEBUG
    emit_enter_runtime_frame();

    for (unsigned i = 0; i < Arity.get(); i++) {
        mov_arg(ARG1, ArgVal(ArgVal::Type::XReg, i));

        emit_enter_runtime();
        runtime_call<void (*)(Eterm), beam_jit_validate_term>();
        emit_leave_runtime();
    }

    emit_leave_runtime_frame();
#    endif

#endif
}

/* Instrs */

void BeamModuleAssembler::emit_i_validate(const ArgWord &Arity) {
    emit_validate(Arity);
}

void BeamModuleAssembler::emit_allocate_heap(const ArgWord &NeedStack,
                                             const ArgWord &NeedHeap,
                                             const ArgWord &Live) {
    ASSERT(NeedStack.get() <= MAX_REG);

    emit_gc_test(NeedStack, NeedHeap, Live);

    if (NeedStack.get() > 0) {
        sub(E, E, NeedStack.get() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_allocate(const ArgWord &NeedStack,
                                        const ArgWord &Live) {
    emit_allocate_heap(NeedStack, ArgWord(0), Live);
}

void BeamModuleAssembler::emit_deallocate(const ArgWord &Deallocate) {
    ASSERT(Deallocate.get() <= 1023);

    if (Deallocate.get() > 0) {
        add(E, E, Deallocate.get() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_test_heap(const ArgWord &Nh,
                                         const ArgWord &Live) {
    emit_gc_test(ArgWord(0), Nh, Live);
}

void BeamModuleAssembler::emit_normal_exit() {
    /* This is implicitly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();
    emit_proc_lc_unrequire();

    mov_imm(TMP1, EXC_NORMAL);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    a.strb(ZERO.w(), arm::Mem(c_p, offsetof(Process, arity)));
    a.mov(ARG1, c_p);
    mov_imm(ARG2, am_normal);
    runtime_call<void (*)(Process *, Eterm), erts_do_exit_process>();

    emit_proc_lc_require();
    emit_leave_runtime<Update::eHeapAlloc | Update::eXRegs |
                       Update::eReductions>();

    a.b(resolve_fragment(ga->get_do_schedule(), disp128MB));
}

void BeamModuleAssembler::emit_continue_exit() {
    /* This is implicitly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>(0);
    emit_proc_lc_unrequire();

    a.mov(ARG1, c_p);
    runtime_call<void (*)(Process *), erts_continue_exit_process>();

    emit_proc_lc_require();
    emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>(0);

    a.b(resolve_fragment(ga->get_do_schedule(), disp128MB));
}

void BeamModuleAssembler::emit_get_list(const ArgRegister &Src,
                                        const ArgRegister &Hd,
                                        const ArgRegister &Tl) {
    auto src = load_source(Src);

    /* The `ldp` instruction does not accept a negative offset, so we
     * need to get rid of tag bits beforehand. */
    untag_ptr_preserve_cache(TMP1, src.reg);

    auto hd = init_destination(Hd, TMP2);
    auto tl = init_destination(Tl, TMP3);

    if (hd.reg == tl.reg) {
        /* ldp with two identical registers is an illegal
         * instruction. Produce the same result as the interpreter. */
        a.ldr(tl.reg, arm::Mem(TMP1, sizeof(Eterm)));
        flush_var(tl);
    } else {
        preserve_cache([&]() {
            a.ldp(hd.reg, tl.reg, arm::Mem(TMP1));
        });
        flush_vars(hd, tl);
    }
}

void BeamModuleAssembler::emit_get_hd(const ArgRegister &Src,
                                      const ArgRegister &Hd) {
    auto src = load_source(Src);
    auto hd = init_destination(Hd, TMP2);
    a64::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(hd.reg, getCARRef(cons_ptr));
    flush_var(hd);
}

void BeamModuleAssembler::emit_get_tl(const ArgRegister &Src,
                                      const ArgRegister &Tl) {
    auto src = load_source(Src);
    auto tl = init_destination(Tl, TMP2);
    a64::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(tl.reg, getCDRRef(cons_ptr));
    flush_var(tl);
}

void BeamModuleAssembler::emit_i_get(const ArgSource &Src,
                                     const ArgRegister &Dst) {
    mov_arg(ARG2, Src);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, Eterm), erts_pd_hash_get>();

    emit_leave_runtime();

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_get_hash(const ArgConstant &Src,
                                          const ArgWord &Hash,
                                          const ArgRegister &Dst) {
    mov_arg(ARG2, Hash);
    mov_arg(ARG3, Src);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, erts_ihash_t, Eterm),
                 erts_pd_hash_get_with_hx>();

    emit_leave_runtime();

    mov_arg(Dst, ARG1);
}

/* Store the untagged pointer to a tuple in ARG1. */
void BeamModuleAssembler::emit_load_tuple_ptr(const ArgSource &Src) {
    auto src = load_source(Src);

    untag_ptr_preserve_cache(ARG1, src.reg);
}

#ifdef DEBUG
/* Emit an assertion to ensure that tuple_reg points into the same
 * tuple as Src. */
void BeamModuleAssembler::emit_tuple_assertion(const ArgSource &Src,
                                               a64::Gp tuple_reg) {
    Label ok = a.newLabel(), fatal = a.newLabel();
    ASSERT(tuple_reg != TMP1);
    mov_arg(TMP1, Src);
    emit_is_boxed(fatal, TMP1);
    emit_untag_ptr(TMP1, TMP1);
    a.cmp(TMP1, tuple_reg);
    a.b_eq(ok);

    a.bind(fatal);
    a.udf(0xaaaa);
    a.bind(ok);
}
#endif

/* Fetch an element from the tuple pointed to by the untagged pointer
 * in ARG1. */
void BeamModuleAssembler::emit_i_get_tuple_element(const ArgSource &Src,
                                                   const ArgWord &Element,
                                                   const ArgRegister &Dst) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG1);
#endif

    auto dst = init_destination(Dst, TMP1);
    safe_ldr(dst.reg, arm::Mem(ARG1, Element.get()));
    flush_var(dst);
}

void BeamModuleAssembler::emit_get_tuple_element_swap(
        const ArgSource &Src,
        const ArgWord &Element,
        const ArgRegister &Dst,
        const ArgRegister &OtherDst) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG1);
#endif

    mov_arg(Dst, OtherDst);

    auto dst = init_destination(OtherDst, TMP1);
    safe_ldr(dst.reg, arm::Mem(ARG1, Element.get()));
    flush_var(dst);
}

/* Fetch two consecutive tuple elements from the tuple pointed to by
 * the boxed pointer in ARG1. */
void BeamModuleAssembler::emit_get_two_tuple_elements(const ArgSource &Src,
                                                      const ArgWord &Element,
                                                      const ArgRegister &Dst1,
                                                      const ArgRegister &Dst2) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG1);
#endif

    auto dst1 = init_destination(Dst1, TMP1);
    auto dst2 = init_destination(Dst2, TMP2);

    arm::Mem element_ptr = arm::Mem(ARG1, Element.get());
    safe_ldp(dst1.reg, dst2.reg, element_ptr);
    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_init_yregs(const ArgWord &Size,
                                          const Span<ArgVal> &args) {
    unsigned count = Size.get();
    ASSERT(count == args.size());
    unsigned i = 0;
    bool x_initialized = false;
    bool q_initialized = false;

    while (i < count) {
        unsigned first_y = args[i].as<ArgYRegister>().get();
        unsigned slots = 1;

        while (i + slots < count) {
            unsigned current_y = args[i + slots].as<ArgYRegister>().get();

            if (first_y + slots != current_y) {
                break;
            }
            slots++;
        }

        i += slots;

        /* Now first_y is the number of the first y register to be initialized
         * and slots is the number of y registers to be initialized. */

        while (slots >= 4 && first_y % 2 == 0 &&
               first_y <= 2 * MAX_LDP_STP_DISPLACEMENT) {
            /* `stp` (with vector registers) can only address the
             *  first 128 Y registers. */
            if (!q_initialized) {
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == 0x0f);
                a.movi(a64::v0.d2(), imm(-1));
                q_initialized = true;
            }
            a.stp(a64::q0, a64::q0, getYRef(first_y));
            first_y += 4;
            slots -= 4;
        }

        while (slots >= 2 && q_initialized &&
               (first_y % 2 == 0 ||
                first_y * sizeof(Eterm) <= MAX_LDUR_STUR_DISPLACEMENT)) {
            /* Note that the STR instruction for a vector register
             * requires the offset to be 16-byte aligned. If it is
             * not, the STUR instruction must be used. AsmJit
             * automatically turns STR into STUR when necessary. */
            a.str(a64::v0, getYRef(first_y));
            first_y += 2;
            slots -= 2;
        }

        while (slots >= 2) {
            /* Either the vector register is not initialized, or first_y
             * is either not 16-byte aligned or it is out of reach for the
             * STUR instruction. */
            if (!x_initialized) {
                mov_imm(TMP1, NIL);
                x_initialized = true;
            }

            /* `stp` can only address the first 64 Y registers. */
            if (first_y <= MAX_LDP_STP_DISPLACEMENT) {
                a.stp(TMP1, TMP1, getYRef(first_y));
                first_y += 2;
                slots -= 2;
            } else {
                a.str(TMP1, getYRef(first_y));
                first_y += 1;
                slots -= 1;
            }
        }

        if (slots == 1) {
            if (q_initialized) {
                a.str(a64::d0, getYRef(first_y));
            } else {
                if (!x_initialized) {
                    mov_imm(TMP1, NIL);
                    x_initialized = true;
                }
                a.str(TMP1, getYRef(first_y));
            }
        }
    }
}

void BeamModuleAssembler::emit_trim(const ArgWord &Words,
                                    const ArgWord &Remaining) {
    trim_preserve_cache(Words);
}

void BeamModuleAssembler::emit_i_move(const ArgSource &Src,
                                      const ArgRegister &Dst) {
    mov_arg(Dst, Src);
}

void BeamModuleAssembler::emit_move_two_trim(const ArgYRegister &Src1,
                                             const ArgRegister &Dst1,
                                             const ArgYRegister &Src2,
                                             const ArgRegister &Dst2,
                                             const ArgWord &Words) {
    auto dst1 = init_destination(Dst1, TMP1);
    auto dst2 = init_destination(Dst2, TMP2);
    arm::Mem mem = getArgRef(Src1);
    Sint trim = Words.get() * sizeof(Eterm);
    auto src_index = Src1.as<ArgYRegister>().get();

    ASSERT(ArgVal::memory_relation(Src1, Src2) ==
           ArgVal::Relation::consecutive);

    if (src_index == 0 && Support::isInt9(trim)) {
        /* Combine fetching of y0 and y1 with trimming. */
        mem = arm::Mem(E).post(trim);
        a.ldp(dst1.reg, dst2.reg, mem);
        dst1 = init_destination(Dst1.trimmed(Words.get()), TMP1);
        dst2 = init_destination(Dst2.trimmed(Words.get()), TMP2);
        flush_vars(dst1, dst2);
    } else {
        safe_ldp(dst1.reg, dst2.reg, Src1, Src2);

        /* Try to combine trimming with storing to one of destination
         * registers. */

        if (Dst1.isYRegister() &&
            Dst1.as<ArgYRegister>().get() == Words.get() &&
            Support::isInt9(trim)) {
            const arm::Mem dst_ref = arm::Mem(E, trim).pre();
            flush_var(dst2);
            a.str(dst1.reg, dst_ref);
        } else if (Dst2.isYRegister() &&
                   Dst2.as<ArgYRegister>().get() == Words.get() &&
                   Support::isInt9(trim)) {
            const arm::Mem dst_ref = arm::Mem(E, trim).pre();
            flush_var(dst1);
            a.str(dst2.reg, dst_ref);
        } else {
            flush_vars(dst1, dst2);

            trim_preserve_cache(Words);
        }
    }
}

void BeamModuleAssembler::emit_move_trim(const ArgSource &Src,
                                         const ArgRegister &Dst,
                                         const ArgWord &Words) {
    Sint trim = Words.get() * sizeof(Eterm);
    ASSERT(Words.get() <= 1023);

    if (Src.isYRegister()) {
        auto src_index = Src.as<ArgYRegister>().get();
        if (src_index == 0 && Support::isInt9(trim)) {
            const arm::Mem src_ref = arm::Mem(E).post(trim);
            auto dst = init_destination(Dst.trimmed(Words.get()), TMP1);
            a.ldr(dst.reg, src_ref);
            flush_var(dst);

            return;
        }
    }

    if (Dst.isYRegister()) {
        auto dst_index = Dst.as<ArgYRegister>().get();
        if (dst_index == Words.get() && Support::isInt9(trim)) {
            auto src = load_source(Src, TMP1);
            const arm::Mem dst_ref = arm::Mem(E, trim).pre();
            a.str(src.reg, dst_ref);

            return;
        }
    }

    /* Fallback. */
    mov_arg(Dst, Src);
    trim_preserve_cache(Words);
}

void BeamModuleAssembler::emit_store_two_values(const ArgSource &Src1,
                                                const ArgRegister &Dst1,
                                                const ArgSource &Src2,
                                                const ArgRegister &Dst2) {
    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);
    auto dst1 = init_destination(Dst1, src1.reg);
    auto dst2 = init_destination(Dst2, src2.reg);

    ASSERT(!isRegisterBacked(Dst1));
    ASSERT(!isRegisterBacked(Dst2));

    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_load_two_xregs(const ArgRegister &Src1,
                                              const ArgXRegister &Dst1,
                                              const ArgRegister &Src2,
                                              const ArgXRegister &Dst2) {
    ASSERT(ArgVal::memory_relation(Src1, Src2) ==
           ArgVal::Relation::consecutive);
    auto dst1 = init_destination(Dst1, TMP1);
    auto dst2 = init_destination(Dst2, TMP2);

    ASSERT(!isRegisterBacked(Src1));
    ASSERT(!isRegisterBacked(Src2));

    safe_ldp(dst1.reg, dst2.reg, Src1, Src2);
    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_swap(const ArgRegister &R1,
                                    const ArgRegister &R2) {
    if (isRegisterBacked(R1)) {
        auto r1 = load_source(R1, ZERO);
        mov_arg(TMP1, R2);
        mov_arg(R2, R1);
        a.mov(r1.reg, TMP1);
    } else if (isRegisterBacked(R2)) {
        return emit_swap(R2, R1);
    } else {
        /* Both BEAM registers are stored in memory. */
        auto [r1, r2] = load_sources(R1, TMP1, R2, TMP2);
        auto dst1 = init_destination(R2, r1.reg);
        auto dst2 = init_destination(R1, r2.reg);
        flush_vars(dst1, dst2);
    }
}

void BeamModuleAssembler::emit_swap2(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3) {
    auto [arg1, arg2] = load_sources(R1, TMP1, R2, TMP2);
    auto arg3 = load_source(R3, TMP3);

    mov_var(TMP4, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, TMP4);

    flush_vars(arg1, arg2, arg3);
}

void BeamModuleAssembler::emit_swap3(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4) {
    auto [arg1, arg2] = load_sources(R1, TMP1, R2, TMP2);
    auto [arg3, arg4] = load_sources(R3, TMP3, R4, TMP4);

    mov_var(TMP5, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, arg4);
    mov_var(arg4, TMP5);

    flush_vars(arg1, arg2, arg3);
    flush_var(arg4);
}

void BeamModuleAssembler::emit_swap4(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4,
                                     const ArgRegister &R5) {
    auto [arg1, arg2] = load_sources(R1, TMP1, R2, TMP2);
    auto [arg3, arg4] = load_sources(R3, TMP3, R4, TMP4);
    auto arg5 = load_source(R5, TMP5);

    mov_var(TMP6, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, arg4);
    mov_var(arg4, arg5);
    mov_var(arg5, TMP6);

    flush_vars(arg1, arg2, arg3);
    flush_vars(arg4, arg5);
}

void BeamModuleAssembler::emit_node(const ArgRegister &Dst) {
    a.ldr(TMP1, embed_constant(&erts_this_node, disp32K));
    a.ldr(TMP1, arm::Mem(TMP1));
    mov_arg(Dst, arm::Mem(TMP1, offsetof(ErlNode, sysname)));
}

void BeamModuleAssembler::emit_put_list(const ArgSource &Hd,
                                        const ArgSource &Tl,
                                        const ArgRegister &Dst) {
    auto [hd, tl] = load_sources(Hd, TMP1, Tl, TMP2);
    auto hd_reg = hd.reg;
    auto tl_reg = tl.reg;
    auto dst = init_destination(Dst, TMP3);

    preserve_cache([&]() {
        a.stp(hd_reg, tl_reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        a.sub(dst.reg, HTOP, imm(sizeof(Eterm[2]) - TAG_PRIMARY_LIST));
    });

    flush_var(dst);
}

void BeamModuleAssembler::emit_put_list_deallocate(const ArgSource &Hd,
                                                   const ArgSource &Tl,
                                                   const ArgRegister &Dst,
                                                   const ArgWord &Deallocate) {
    Sint dealloc = Deallocate.get() * sizeof(Eterm);
    a64::Gp hd_reg, tl_reg;
    auto dst = init_destination(Dst, TMP3);

    ASSERT(dealloc < MAX_REG * sizeof(Eterm));

    if (Hd.isYRegister() && !Tl.isYRegister() && dealloc > 0) {
        auto hd_index = Hd.as<ArgYRegister>().get();

        if (hd_index == 0) {
            arm::Mem mem = getArgRef(Hd);
            mem = arm::Mem(E).post(dealloc);
            hd_reg = TMP1;
            a.ldr(hd_reg, mem);
            tl_reg = load_source(Tl, TMP2).reg;
            dealloc = 0;
        }
    } else if (!Hd.isYRegister() && Tl.isYRegister() && dealloc > 0) {
        auto tl_index = Tl.as<ArgYRegister>().get();

        if (tl_index == 0) {
            arm::Mem mem = getArgRef(Tl);
            mem = arm::Mem(E).post(dealloc);
            tl_reg = TMP2;
            a.ldr(tl_reg, mem);
            hd_reg = load_source(Hd, TMP1).reg;
            dealloc = 0;
        }
    }

    if (!hd_reg.isValid()) {
        auto [hd, tl] = load_sources(Hd, TMP1, Tl, TMP2);
        hd_reg = hd.reg;
        tl_reg = tl.reg;
    }

    a.stp(hd_reg, tl_reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
    a.sub(dst.reg, HTOP, imm(sizeof(Eterm[2]) - TAG_PRIMARY_LIST));

    flush_var(dst);

    if (dealloc > 0) {
        add(E, E, Deallocate.get() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_put_list2(const ArgSource &Hd1,
                                         const ArgSource &Hd2,
                                         const ArgSource &Tl,
                                         const ArgRegister &Dst) {
    const arm::Mem put_cons = arm::Mem(HTOP).post(sizeof(Eterm[2]));

    auto [hd1, hd2] = load_sources(Hd1, TMP1, Hd2, TMP2);
    auto tl = load_source(Tl, TMP3);
    auto dst = init_destination(Dst, TMP4);

    a.stp(hd1.reg, tl.reg, put_cons);
    a.sub(dst.reg, HTOP, imm(sizeof(Eterm[2]) - TAG_PRIMARY_LIST));

    a.stp(hd2.reg, dst.reg, put_cons);
    a.sub(dst.reg, HTOP, imm(sizeof(Eterm[2]) - TAG_PRIMARY_LIST));

    flush_var(dst);
}

void BeamModuleAssembler::emit_put_tuple2(const ArgRegister &Dst,
                                          const ArgWord &Arity,
                                          const Span<ArgVal> &args) {
    ASSERT(arityval(Arity.get()) == args.size());

    std::vector<ArgVal> data;
    data.reserve(args.size() + 1);
    data.push_back(Arity);
    data.insert(data.end(), std::begin(args), std::end(args));

    size_t size = data.size();
    unsigned i;
    ArgVal value = ArgWord(0);
    for (i = 0; i < size - 1; i += 2) {
        if ((i % 128) == 0) {
            check_pending_stubs();
        }

        if (!data[i].isRegister() && data[i] == data[i + 1]) {
            if (data[i] != value) {
                value = data[i];
                mov_arg(TMP1, value);
            }
            a.stp(TMP1, TMP1, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        } else if (data[i] == value) {
            auto second = load_source(data[i + 1], TMP3);
            a.stp(TMP1, second.reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        } else if (data[i + 1] == value) {
            auto first = load_source(data[i], TMP2);
            a.stp(first.reg, TMP1, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        } else {
            auto [first, second] =
                    load_sources(data[i], TMP2, data[i + 1], TMP3);
            a.stp(first.reg, second.reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        }
    }

    if (i < size) {
        if (data[i] == value) {
            a.str(TMP1, arm::Mem(HTOP).post(sizeof(Eterm)));
        } else {
            mov_arg(arm::Mem(HTOP).post(sizeof(Eterm)), data[i]);
        }
    }

    auto ptr = init_destination(Dst, TMP1);
    sub(ptr.reg, HTOP, size * sizeof(Eterm) - TAG_PRIMARY_BOXED);
    flush_var(ptr);
}

void BeamModuleAssembler::emit_self(const ArgRegister &Dst) {
    mov_arg(Dst, arm::Mem(c_p, offsetof(Process, common.id)));
}

void BeamModuleAssembler::emit_copy_words_increment(a64::Gp from,
                                                    a64::Gp to,
                                                    size_t count) {
    check_pending_stubs();

    /* Copy the words inline if we can, otherwise use a loop with the largest
     * vector size we're capable of. */
    if (count <= 16) {
        while (count >= 4) {
            a.ldp(a64::q30, a64::q31, arm::Mem(from).post(sizeof(UWord[4])));
            a.stp(a64::q30, a64::q31, arm::Mem(to).post(sizeof(UWord[4])));
            count -= 4;
        }
    } else {
        Label copy_next = a.newLabel();

        ASSERT(Support::isUInt16(count / 4));
        mov_imm(SUPER_TMP, count / 4);
        a.bind(copy_next);
        {
            a.ldp(a64::q30, a64::q31, arm::Mem(from).post(sizeof(UWord[4])));
            a.stp(a64::q30, a64::q31, arm::Mem(to).post(sizeof(UWord[4])));
            a.subs(SUPER_TMP, SUPER_TMP, imm(1));
            a.b_ne(copy_next);
        }

        count = count % 4;
    }

    if (count >= 2) {
        a.ldr(a64::q30, arm::Mem(from).post(sizeof(UWord[2])));
        a.str(a64::q30, arm::Mem(to).post(sizeof(UWord[2])));
        count -= 2;
    }

    if (count == 1) {
        a.ldr(SUPER_TMP, arm::Mem(from).post(sizeof(UWord)));
        a.str(SUPER_TMP, arm::Mem(to).post(sizeof(UWord)));
        count -= 1;
    }

    ASSERT(count == 0);
    (void)count;
}

void BeamModuleAssembler::emit_update_record(const ArgAtom &Hint,
                                             const ArgWord &TupleSize,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst,
                                             const ArgWord &UpdateCount,
                                             const Span<ArgVal> &updates) {
    const size_t size_on_heap = TupleSize.get() + 1;
    Label next = a.newLabel();

    ASSERT(UpdateCount.get() == updates.size());
    ASSERT((UpdateCount.get() % 2) == 0);

    ASSERT(size_on_heap > 2);

    auto destination = init_destination(Dst, ARG1);
    auto src = load_source(Src, ARG2);

    a64::Gp untagged_src = ARG3;
    emit_untag_ptr(untagged_src, src.reg);

    /* Setting a field to the same value is pretty common, so we'll check for
     * that since it's vastly cheaper than copying if we're right, and doesn't
     * cost much if we're wrong. */
    if (Hint.get() == am_reuse && updates.size() == 2) {
        const auto next_index = updates[0].as<ArgWord>().get();
        const auto &next_value = updates[1].as<ArgSource>();

        safe_ldr(TMP1, arm::Mem(untagged_src, next_index * sizeof(Eterm)));
        cmp_arg(TMP1, next_value);

        if (destination.reg != src.reg) {
            a.csel(destination.reg,
                   destination.reg,
                   src.reg,
                   imm(arm::CondCode::kNE));
        }
        a.b_eq(next);
    }

    size_t copy_index = 0;

    for (size_t i = 0; i < updates.size(); i += 2) {
        const auto next_index = updates[i].as<ArgWord>().get();
        const auto &next_value = updates[i + 1].as<ArgSource>();
        bool odd_copy;

        ASSERT(next_index > 0 && next_index >= copy_index);

        /* If we need to copy an odd number of elements, we'll do the last one
         * ourselves to save us from having to increment `untagged_src`
         * separately. */
        odd_copy = (next_index - copy_index) & 1;
        emit_copy_words_increment(untagged_src,
                                  HTOP,
                                  (next_index - copy_index) & ~1);

        if ((i + 2) < updates.size()) {
            const auto adjacent_index = updates[i + 2].as<ArgWord>().get();
            const auto &adjacent_value = updates[i + 3].as<ArgSource>();

            if (adjacent_index == next_index + 1) {
                auto [first, second] =
                        load_sources(next_value, TMP1, adjacent_value, TMP2);

                if (odd_copy) {
                    a.ldr(TMP3, arm::Mem(untagged_src).post(sizeof(Eterm[3])));
                    a.stp(TMP3,
                          first.reg,
                          arm::Mem(HTOP).post(sizeof(Eterm[2])));
                    a.str(second.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
                } else {
                    a.add(untagged_src, untagged_src, imm(sizeof(Eterm[2])));
                    a.stp(first.reg,
                          second.reg,
                          arm::Mem(HTOP).post(sizeof(Eterm[2])));
                }

                copy_index = next_index + 2;
                i += 2;
                continue;
            }
        }

        auto value = load_source(next_value, TMP1);

        if ((next_index - copy_index) & 1) {
            a.ldr(TMP2, arm::Mem(untagged_src).post(sizeof(Eterm[2])));
            a.stp(TMP2, value.reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        } else {
            a.add(untagged_src, untagged_src, imm(sizeof(Eterm)));
            a.str(value.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
        }

        copy_index = next_index + 1;
    }

    emit_copy_words_increment(untagged_src, HTOP, size_on_heap - copy_index);

    sub(destination.reg,
        HTOP,
        (size_on_heap * sizeof(Eterm)) - TAG_PRIMARY_BOXED);

    a.bind(next);
    flush_var(destination);
}

void BeamModuleAssembler::emit_update_record_in_place(
        const ArgWord &TupleSize,
        const ArgSource &Src,
        const ArgRegister &Dst,
        const ArgWord &UpdateCount,
        const Span<ArgVal> &updates) {
    bool all_safe = true;
    ArgSource maybe_immediate = ArgNil();
    const size_t size_on_heap = TupleSize.get() + 1;

    ASSERT(UpdateCount.get() == updates.size());
    ASSERT((UpdateCount.get() % 2) == 0);

    ASSERT(size_on_heap > 2);

    auto destination = init_destination(Dst, ARG1);
    auto src = load_source(Src, ARG2);

    a64::Gp untagged_src = ARG3;
    emit_untag_ptr(untagged_src, src.reg);

    for (size_t i = 0; i < updates.size(); i += 2) {
        const auto &value = updates[i + 1].as<ArgSource>();
        if (!(always_immediate(value) || value.isLiteral())) {
            all_safe = false;
            if (maybe_immediate.isNil() &&
                always_one_of<BeamTypeId::MaybeImmediate>(value)) {
                maybe_immediate = value;
            } else {
                maybe_immediate = ArgNil();
                break;
            }
        }
    }

    if (all_safe) {
        comment("skipped copy fallback because all new values are safe");
    } else {
        Label update = a.newLabel();

        if (!maybe_immediate.isNil()) {
            auto value = load_source(maybe_immediate, ARG5);
            emit_is_boxed(update, value.reg);
        }

        a.ldr(ARG4, arm::Mem(c_p, offsetof(Process, high_water)));
        a.cmp(untagged_src, HTOP);
        a.ccmp(untagged_src, ARG4, imm(NZCV::kNone), imm(arm::CondCode::kLO));
        a.b_hs(update);

        emit_copy_words_increment(untagged_src, HTOP, size_on_heap);
        sub(untagged_src, HTOP, size_on_heap * sizeof(Eterm));

        a.bind(update);
    }

    for (size_t i = 0; i < updates.size(); i += 2) {
        const auto next_index = updates[i].as<ArgWord>().get();
        const auto &next_value = updates[i + 1].as<ArgSource>();
        arm::Mem mem(untagged_src, next_index * sizeof(Eterm));

        if (i + 2 < updates.size()) {
            const auto adjacent_index = updates[i + 2].as<ArgWord>().get();
            const auto &adjacent_value = updates[i + 3].as<ArgSource>();

            if (adjacent_index == next_index + 1) {
                auto [first, second] =
                        load_sources(next_value, TMP1, adjacent_value, TMP2);
                safe_stp(first.reg, second.reg, mem);
                i += 2;
                continue;
            }
        }

        auto value = load_source(next_value, TMP1);
        safe_str(value.reg, mem);
    }

    a.add(destination.reg, untagged_src, TAG_PRIMARY_BOXED);
    flush_var(destination);

#ifdef DEBUG
    if (!all_safe && maybe_immediate.isNil()) {
        Label pointer_ok = a.newLabel();

        /* If p->high_water contained a garbage value, a tuple not in
         * the safe part of the new heap could have been destructively
         * updated. */
        comment("sanity-checking tuple pointer");
        mov_arg(ARG2, Dst);
        a.ldr(ARG4, arm::Mem(c_p, offsetof(Process, heap)));
        a.cmp(ARG2, HTOP);
        a.ccmp(ARG2, ARG4, imm(NZCV::kNone), imm(arm::CondCode::kLO));
        a.b_hs(pointer_ok);

        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<void (*)(Process *, Eterm), beam_jit_invalid_heap_ptr>();
        emit_leave_runtime();

        a.bind(pointer_ok);
    }
#endif
}

void BeamModuleAssembler::emit_set_tuple_element(const ArgSource &Element,
                                                 const ArgRegister &Tuple,
                                                 const ArgWord &Offset) {
    auto tuple = load_source(Tuple, TMP1);
    auto element = load_source(Element, TMP2);
    a64::Gp boxed_ptr = emit_ptr_val(TMP1, tuple.reg);
    arm::Mem boxed_val = emit_boxed_val(boxed_ptr, Offset.get());

    stur(element.reg, boxed_val);
}

void BeamModuleAssembler::emit_is_nonempty_list(const ArgLabel &Fail,
                                                const ArgRegister &Src) {
    auto list_ptr = load_source(Src);
    emit_is_cons(resolve_beam_label(Fail, dispUnknown), list_ptr.reg);
}

void BeamModuleAssembler::emit_jump(const ArgLabel &Fail) {
    a.b(resolve_beam_label(Fail, disp128MB));
    mark_unreachable();
}

void BeamModuleAssembler::emit_is_atom(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    auto src = load_source(Src);

    preserve_cache(
            [&]() {
                a.and_(TMP1, src.reg, imm(_TAG_IMMED2_MASK));
                a.cmp(TMP1, imm(_TAG_IMMED2_ATOM));
                a.b_ne(resolve_beam_label(Fail, disp1MB));
            },
            TMP1);
}

void BeamModuleAssembler::emit_is_boolean(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    /* Since am_true and am_false differ by a single bit, we can simplify the
     * check by clearing said bit and comparing against the lesser one. */
    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    auto src = load_source(Src, TMP1);
    a.and_(TMP1, src.reg, imm(~(am_true & ~_TAG_IMMED2_MASK)));
    a.cmp(TMP1, imm(am_false));
    a.b_ne(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_bitstring(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Bitstring) {
        comment("skipped header test since we know it's a bitstring when "
                "boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(ARG1, src.reg);
        a.ldur(TMP1, emit_boxed_val(boxed_ptr));

        /* The header mask with the binary sub tag bits removed (0b110011)
         * is not possible to use as an immediate operand for 'and'. (See
         * the note at the beginning of the file.) Therefore, use a simpler
         * mask (0b110000) that will also clear the primary tag bits. That
         * works because we KNOW that a boxed pointer always points to a header
         * word and that the primary tag for a header is 0. */
        const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
        ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
        ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS == (_TAG_HEADER_HEAP_BITS & mask));
        a.and_(TMP1, TMP1, imm(mask));
        a.cmp(TMP1, imm(_TAG_HEADER_HEAP_BITS));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_is_binary(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
    emit_untag_ptr(ARG1, src.reg);

    ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
    a.ldp(TMP1, TMP2, arm::Mem(ARG1));

    Label not_sub_bits = a.newLabel();
    a.cmp(TMP1, imm(HEADER_SUB_BITS));
    a.b_ne(not_sub_bits);
    {
        ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
        a.ldp(TMP2, TMP3, arm::Mem(ARG1, offsetof(ErlSubBits, start)));
        a.sub(TMP2, TMP3, TMP2);
    }
    a.bind(not_sub_bits);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Bitstring) {
        comment("skipped header test since we know it's a bitstring when "
                "boxed");
        a.tst(TMP2, imm(7));
    } else {
        const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
        ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
        ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS == (_TAG_HEADER_HEAP_BITS & mask));
        a.and_(TMP1, TMP1, imm(mask));

        /* Shift out all but the lowest three bits in the size, leaving a
         * non-zero value if the size is not evenly divisible by 8.
         *
         * Thus, OR-ing this value into the header word forces the check to
         * fail when we have a non-binary bitstring. */
        ERTS_CT_ASSERT((UWORD_CONSTANT(7) << (64 - 3)) > _BITSTRING_TAG_MASK);
        a.orr(TMP1, TMP1, TMP2, arm::lsl(64 - 3));
        a.cmp(TMP1, imm(_TAG_HEADER_HEAP_BITS));
    }

    a.b_ne(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_float(const ArgLabel &Fail,
                                        const ArgSource &Src) {
    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Float) {
        comment("skipped header test since we know it's a float when boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(TMP1, emit_boxed_val(boxed_ptr));

        a.cmp(TMP1, imm(HEADER_FLONUM));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_is_function(const ArgLabel &Fail,
                                           const ArgRegister &Src) {
    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Fun) {
        comment("skipped header test since we know it's a fun when boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldurb(TMP1.w(), emit_boxed_val(boxed_ptr));
        a.cmp(TMP1, imm(FUN_SUBTAG));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_is_function2(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgSource &Arity) {
    if (!Arity.isSmall()) {
        /*
         * Non-literal arity - extremely uncommon. Generate simple code.
         */
        mov_arg(ARG2, Src);
        mov_arg(ARG3, Arity);

        emit_enter_runtime();

        a.mov(ARG1, c_p);
        runtime_call<Eterm (*)(Process *, Eterm, Eterm), erl_is_function>();

        emit_leave_runtime();

        a.cmp(ARG1, imm(am_true));
        a.b_ne(resolve_beam_label(Fail, disp1MB));

        return;
    }

    unsigned arity = Arity.as<ArgSmall>().getUnsigned();
    if (arity > MAX_ARG) {
        /* Arity is negative or too large. */
        a.b(resolve_beam_label(Fail, disp128MB));
        mark_unreachable();

        return;
    }

    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldurh(TMP2.w(), emit_boxed_val(boxed_ptr));
    cmp(TMP2, MAKE_FUN_HEADER(arity, 0, 0) & 0xFFFF);
    a.b_ne(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_integer(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    auto src = load_source(Src, TMP1);

    if (always_immediate(Src)) {
        comment("skipped test for boxed since the value is always immediate");
        a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
        a.b_ne(resolve_beam_label(Fail, disp1MB));

        return;
    }

    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified small test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);

        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, TMP2);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Integer) {
        comment("skipped header test since we know it's a bignum when "
                "boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(TMP1, emit_boxed_val(boxed_ptr));

        /* The header mask with the sign bit removed (0b111011) is not
         * possible to use as an immediate operand for 'and'. (See the
         * note at the beginning of the file.) Therefore, use a
         * simpler mask (0b111000) that will also clear the primary
         * tag bits. That works because we KNOW that a boxed pointer
         * always points to a header word and that the primary tag for
         * a header is 0.
         */
        auto mask = _HEADER_SUBTAG_MASK - _BIG_SIGN_BIT;
        ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
        a.and_(TMP1, TMP1, imm(mask));
        a.cmp(TMP1, imm(_TAG_HEADER_POS_BIG));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_list(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    auto src = load_source(Src);

    emit_is_list(resolve_beam_label(Fail, dispUnknown), src.reg);
}

void BeamModuleAssembler::emit_is_map(const ArgLabel &Fail,
                                      const ArgSource &Src) {
    auto src = load_source(Src);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    /* As an optimization for the `error | #{}` case, skip checking the header
     * word when we know that the only possible boxed type is a map. */
    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Map) {
        comment("skipped header test since we know it's a map when boxed");
    } else {
        preserve_cache(
                [&]() {
                    a64::Gp boxed_ptr = emit_ptr_val(TMP3, src.reg);
                    a.ldur(TMP3, emit_boxed_val(boxed_ptr));
                    a.and_(TMP3, TMP3, imm(_TAG_HEADER_MASK));
                    a.cmp(TMP3, imm(_TAG_HEADER_MAP));
                    a.b_ne(resolve_beam_label(Fail, disp1MB));
                },
                TMP3);
    }
}

void BeamModuleAssembler::emit_is_nil(const ArgLabel &Fail,
                                      const ArgRegister &Src) {
    auto src = load_source(Src);

    if (always_one_of<BeamTypeId::List>(Src)) {
        emit_is_not_cons(resolve_beam_label(Fail, dispUnknown), src.reg);
    } else {
        preserve_cache([&]() {
            a.cmp(src.reg, imm(NIL));
            a.b_ne(resolve_beam_label(Fail, disp1MB));
        });
    }
}

void BeamModuleAssembler::emit_is_number(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    auto src = load_source(Src, TMP1);
    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified small test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);

        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Number) {
        comment("skipped header test since we know it's a number when boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(TMP1, emit_boxed_val(boxed_ptr));

        /* The header mask with the sign bit removed (0b111011) is not
         * possible to use as an immediate operand for 'and'. (See the
         * note at the beginning of the file.) Therefore, use a
         * simpler mask (0b111000) that will also clear the primary
         * tag bits. That works because we KNOW that a boxed pointer
         * always points to a header word and that the primary tag for
         * a header is 0.
         */
        auto mask = _HEADER_SUBTAG_MASK - _BIG_SIGN_BIT;
        ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
        a.and_(TMP2, TMP1, imm(mask));
        a.cmp(TMP2, imm(_TAG_HEADER_POS_BIG));

        a.mov(TMP3, imm(HEADER_FLONUM));
        a.ccmp(TMP1, TMP3, imm(NZCV::kEqual), imm(arm::CondCode::kNE));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_pid(const ArgLabel &Fail,
                                      const ArgSource &Src) {
    auto src = load_source(Src, TMP1);
    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Pid, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified local pid test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP2, imm(_TAG_IMMED1_PID));
        a.b_eq(next);

        /* Reuse TMP2 as the important bits are still available. */
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, TMP2);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Pid) {
        comment("skipped header test since we know it's a pid when boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(TMP2, emit_boxed_val(boxed_ptr));
        a.and_(TMP2, TMP2, imm(_TAG_HEADER_MASK));
        a.cmp(TMP2, imm(_TAG_HEADER_EXTERNAL_PID));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_port(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    auto src = load_source(Src, TMP1);
    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Port, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified local port test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP2, imm(_TAG_IMMED1_PORT));
        a.b_eq(next);

        /* Reuse TMP2 as the important bits are still available. */
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, TMP2);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Port) {
        comment("skipped header test since we know it's a port when boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(TMP2, emit_boxed_val(boxed_ptr));
        a.and_(TMP2, TMP2, imm(_TAG_HEADER_MASK));
        a.cmp(TMP2, imm(_TAG_HEADER_EXTERNAL_PORT));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_reference(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Reference) {
        comment("skipped header test since we know it's a ref when boxed");
    } else {
        a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
        a.ldur(TMP1, emit_boxed_val(boxed_ptr));
        a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
        a.cmp(TMP1, imm(_TAG_HEADER_EXTERNAL_REF));
        a.ccmp(TMP1,
               imm(_TAG_HEADER_REF),
               imm(NZCV::kEqual),
               imm(arm::CondCode::kNE));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tagged_tuple(const ArgLabel &Fail,
                                                 const ArgSource &Src,
                                                 const ArgWord &Arity,
                                                 const ArgAtom &Tag) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    emit_untag_ptr(ARG1, src.reg);

    /* It is safe to fetch the both the header word and the first
     * element of the tuple with ldp because the empty tuple is always
     * a literal that is padded so that the word after arity is
     * allocated. */
    a.ldp(TMP1, TMP2, arm::Mem(ARG1));

    cmp_arg(TMP2, Tag);
    mov_imm(TMP3, Arity.get());
    a.ccmp(TMP1, TMP3, imm(NZCV::kNone), imm(arm::CondCode::kEQ));

    a.b_ne(resolve_beam_label(Fail, disp1MB));
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple_ff(const ArgLabel &NotTuple,
                                                    const ArgLabel &NotRecord,
                                                    const ArgSource &Src,
                                                    const ArgWord &Arity,
                                                    const ArgAtom &Tag) {
    Label correct_arity = a.newLabel();
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(NotTuple, dispUnknown), Src, src.reg);

    emit_untag_ptr(ARG1, src.reg);

    /* It is safe to fetch the both the header word and the first
     * element of the tuple with ldp because the empty tuple is always
     * a literal that is padded so that the word after arity is
     * allocated. */
    a.ldp(TMP1, TMP2, arm::Mem(ARG1));

    cmp_arg(TMP1, Arity);
    a.b_eq(correct_arity);

    /* Not a tuple or the wrong arity. Decide which. */
    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.b_eq(resolve_beam_label(NotRecord, disp1MB));
    a.b(resolve_beam_label(NotTuple, disp128MB));

    a.bind(correct_arity);
    {
        cmp_arg(TMP2, Tag);
        a.b_ne(resolve_beam_label(NotRecord, disp1MB));
    }
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
    emit_untag_ptr(ARG1, src.reg);

    /* As an optimization for the `error | {ok, Value}` case, skip checking the
     * header word when we know that the only possible boxed type is a tuple. */
    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Tuple) {
        comment("skipped header test since we know it's a tuple when boxed");
    } else {
        a.ldr(TMP1, arm::Mem(ARG1));
        ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
        a.tst(TMP1, imm(_TAG_HEADER_MASK));
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    }
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity(const ArgLabel &Fail,
                                                   const ArgSource &Src,
                                                   const ArgWord &Arity) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    emit_untag_ptr(ARG1, src.reg);

    a.ldr(TMP1, arm::Mem(ARG1));
    cmp_arg(TMP1, Arity);
    a.b_ne(resolve_beam_label(Fail, disp1MB));
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity_ff(const ArgLabel &NotTuple,
                                                      const ArgLabel &BadArity,
                                                      const ArgSource &Src,
                                                      const ArgWord &Arity) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(NotTuple, dispUnknown), Src, src.reg);

    emit_untag_ptr(ARG1, src.reg);

    a.ldr(TMP1, arm::Mem(ARG1));

    /* As an optimization for the `error | {ok, Value}` case, skip checking the
     * header word when we know that the only possible boxed type is a tuple. */
    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Tuple) {
        comment("skipped header test since we know it's a tuple when boxed");
    } else {
        ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
        a.tst(TMP1, imm(_TAG_HEADER_MASK));
        a.b_ne(resolve_beam_label(NotTuple, disp1MB));
    }

    cmp_arg(TMP1, Arity);
    a.b_ne(resolve_beam_label(BadArity, disp1MB));
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_test_arity(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgWord &Arity) {
    auto src = load_source(Src, ARG1);
    emit_untag_ptr(ARG1, src.reg);

    a.ldr(TMP1, arm::Mem(ARG1));
    cmp_arg(TMP1, Arity);
    a.b_ne(resolve_beam_label(Fail, disp1MB));
}

/*
 * ARG1 = First operand
 * ARG2 = Literal list
 *
 * The result is returned in the Z flag.
 */
void BeamGlobalAssembler::emit_is_eq_exact_list_shared() {
    Label loop = a.newLabel(), mid = a.newLabel(), done = a.newLabel();

    a.b(mid);

    a.bind(loop);
    emit_untag_ptr(ARG1, ARG1);
    emit_untag_ptr(ARG2, ARG2);
    a.ldp(TMP1, ARG1, arm::Mem(ARG1));
    a.ldp(TMP2, ARG2, arm::Mem(ARG2));
    a.cmp(TMP1, TMP2);
    a.b_ne(done);

    a.bind(mid);
    a.cmp(ARG1, ARG2);
    a.b_eq(done);

    /* If not equal, both terms must be CONSes. */
#if !defined(DEBUG)
    ERTS_CT_ASSERT(!is_list(make_small(0) | make_list(0)));
    ERTS_CT_ASSERT(!is_list(make_boxed(0) | make_list(0)));
#endif
    a.orr(TMP1, ARG1, ARG2);
    emit_is_not_cons(loop, TMP1);

    /* Not equal. Clear Z flag. */
    a.cmp(TMP1, imm(0));

    a.bind(done);
    a.ret(a64::x30);
}

/*
 * ARG1 = LHS
 * ARG2 = RHS
 *
 * The result is returned in the Z flag.
 */
void BeamGlobalAssembler::emit_is_eq_exact_shallow_boxed_shared() {
    Label loop = a.newLabel();
    Label done = a.newLabel();
    Label not_equal = a.newLabel();

    a.orr(TMP1, ARG1, ARG2);
    emit_is_boxed(not_equal, TMP1);

    emit_untag_ptr(TMP1, ARG1);
    a.ldr(TMP3, arm::Mem(TMP1));
    emit_untag_ptr(TMP2, ARG2);
    a.lsr(ARG3, TMP3, imm(_HEADER_ARITY_OFFS));
    a.sub(ARG3, ARG3, imm(1));

    a.bind(loop);
    {
        a.ldp(TMP3, TMP4, arm::Mem(TMP1).post(16));
        a.ldp(TMP5, TMP6, arm::Mem(TMP2).post(16));
        a.cmp(TMP3, TMP5);
        a.ccmp(TMP4, TMP6, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
        a.b_ne(done);

        a.subs(ARG3, ARG3, imm(2));
        a.b_pl(loop);
    }

    a.cmn(ARG3, imm(2));
    a.b_eq(done);

    a.ldr(TMP3, arm::Mem(TMP1));
    a.ldr(TMP5, arm::Mem(TMP2));
    a.cmp(TMP3, TMP5);

    a.bind(done);
    a.ret(a64::x30);

    a.bind(not_equal);
    a.cmp(TMP1, 0);
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_is_eq_exact(const ArgLabel &Fail,
                                           const ArgSource &X,
                                           const ArgSource &Y) {
    auto x = load_source(X, ARG1);

    if (Y.isLiteral()) {
        Eterm literal = beamfile_get_literal(beam, Y.as<ArgLiteral>().get());

        if (is_list(literal) && is_immed(CAR(list_val(literal))) &&
            is_nil(CDR(list_val(literal)))) {
            /* Inline the equality test if the RHS argument is a list
             * of one immediate value such as `[42]` or `[a]`. */
            a64::Gp cons_ptr;

            comment("inlined equality test with %T", literal);
            if (!exact_type<BeamTypeId::Cons>(X)) {
                emit_is_cons(resolve_beam_label(Fail, dispUnknown), x.reg);
            }
            cons_ptr = emit_ptr_val(TMP1, x.reg);
            a.sub(TMP1, cons_ptr, imm(TAG_PRIMARY_LIST));
            a.ldp(TMP2, TMP3, arm::Mem(TMP1));
            cmp(TMP2, CAR(list_val(literal)));
            mov_imm(TMP4, NIL);
            a.ccmp(TMP3, TMP4, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
            a.b_ne(resolve_beam_label(Fail, disp1MB));

            return;
        } else if (beam_jit_is_list_of_immediates(literal)) {
            comment("optimized equality test with %T", literal);
            mov_var(ARG1, x);
            mov_arg(ARG2, Y);
            fragment_call(ga->get_is_eq_exact_list_shared());
            a.b_ne(resolve_beam_label(Fail, disp1MB));

            return;
        } else if (beam_jit_is_shallow_boxed(literal)) {
            comment("optimized equality test with %T", literal);
            mov_var(ARG1, x);
            mov_arg(ARG2, Y);
            fragment_call(ga->get_is_eq_exact_shallow_boxed_shared());
            a.b_ne(resolve_beam_label(Fail, disp1MB));

            return;
        } else if (is_bitstring(literal) && bitstring_size(literal) == 0) {
            Label not_sub_bits = a.newLabel();

            comment("simplified non-equality test with empty binary");
            emit_is_boxed(resolve_beam_label(Fail, dispUnknown), X, x.reg);
            emit_untag_ptr(ARG1, x.reg);

            ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
            a.ldp(TMP1, TMP2, arm::Mem(ARG1));

            a.cmp(TMP1, imm(HEADER_SUB_BITS));
            a.b_ne(not_sub_bits);
            {
                ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
                a.ldp(TMP2, TMP3, arm::Mem(ARG1, offsetof(ErlSubBits, start)));
                a.sub(TMP2, TMP3, TMP2);
            }
            a.bind(not_sub_bits);

            if (masked_types<BeamTypeId::MaybeBoxed>(X) ==
                BeamTypeId::Bitstring) {
                comment("skipped header test since we know it's a bitstring "
                        "when boxed");
                a.cbnz(TMP2, resolve_beam_label(Fail, disp1MB));
            } else {
                const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
                ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
                ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                               (_TAG_HEADER_HEAP_BITS & mask));
                a.and_(TMP1, TMP1, imm(mask));
                a.cmp(TMP1, imm(_TAG_HEADER_HEAP_BITS));
                a.ccmp(TMP2, imm(0), imm(NZCV::kNone), imm(arm::CondCode::kEQ));
                a.b_ne(resolve_beam_label(Fail, disp1MB));
            }

            return;
        } else if (is_map(literal) && erts_map_size(literal) == 0) {
            comment("optimized equality test with empty map", literal);
            emit_is_boxed(resolve_beam_label(Fail, dispUnknown), X, x.reg);
            emit_untag_ptr(ARG1, x.reg);
            a.ldp(TMP1, TMP2, arm::Mem(ARG1));
            cmp(TMP1, MAP_HEADER_FLATMAP);
            a.ccmp(TMP2, imm(0), imm(NZCV::kNone), imm(arm::CondCode::kEQ));
            a.b_ne(resolve_beam_label(Fail, disp1MB));

            return;
        }
    }

    /* If either argument is known to be an immediate, we can fail immediately
     * if they're not equal. */
    if (always_immediate(X) || always_immediate(Y)) {
        if (!X.isImmed() && !Y.isImmed()) {
            comment("simplified check since one argument is an immediate");
        }

        preserve_cache([&]() {
            cmp_arg(x.reg, Y);
            a.b_ne(resolve_beam_label(Fail, disp1MB));
        });

        return;
    }

    /* Both operands are registers or literals. */
    Label next = a.newLabel();
    auto y = load_source(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.b_eq(next);

    if (exact_type<BeamTypeId::Integer>(X) &&
        exact_type<BeamTypeId::Integer>(Y)) {
        /* Fail immediately if one of the operands is a small. */
        a.orr(TMP1, x.reg, y.reg);
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), TMP1);
    } else if (always_same_types(X, Y)) {
        comment("skipped tag test since they are always equal");
    } else {
        /* Fail immediately if the pointer tags are not equal. */
        emit_is_unequal_based_on_tags(resolve_beam_label(Fail, dispUnknown),
                                      X,
                                      x.reg,
                                      Y,
                                      y.reg);
    }

    /* Both operands are pointers having the same tag. Must do a
     * deeper comparison. */
    mov_var(ARG1, x);
    mov_var(ARG2, y);

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::Float>(X) ||
        always_one_of<BeamTypeId::Integer, BeamTypeId::Float>(Y)) {
        fragment_call(ga->get_is_eq_exact_shallow_boxed_shared());
        a.b_ne(resolve_beam_label(Fail, disp1MB));
    } else {
        emit_enter_runtime();
        runtime_call<int (*)(Eterm, Eterm), eq>();
        emit_leave_runtime();
        a.cbz(ARG1.w(), resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne_exact(const ArgLabel &Fail,
                                           const ArgSource &X,
                                           const ArgSource &Y) {
    auto x = load_source(X, ARG1);

    if (Y.isLiteral()) {
        Eterm literal = beamfile_get_literal(beam, Y.as<ArgLiteral>().get());
        bool imm_list = beam_jit_is_list_of_immediates(literal);

        if (imm_list && erts_list_length(literal) == 1) {
            a64::Gp cons_ptr;
            Label next = a.newLabel();

            /* Inline the equality test if the RHS argument is a list
             * of one immediate value such as `[42]` or `[a]`. */
            comment("inlined non-equality test with %T", literal);
            if (!exact_type<BeamTypeId::Cons>(X)) {
                emit_is_cons(next, x.reg);
            }
            cons_ptr = emit_ptr_val(TMP1, x.reg);
            a.sub(TMP1, cons_ptr, imm(TAG_PRIMARY_LIST));
            a.ldp(TMP2, TMP3, arm::Mem(TMP1));
            cmp(TMP2, CAR(list_val(literal)));
            mov_imm(TMP4, NIL);
            a.ccmp(TMP3, TMP4, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
            a.b_eq(resolve_beam_label(Fail, disp1MB));

            a.bind(next);

            return;
        } else if (imm_list) {
            comment("optimized non-equality test with %T", literal);
            mov_var(ARG1, x);
            mov_arg(ARG2, Y);
            fragment_call(ga->get_is_eq_exact_list_shared());
            a.b_eq(resolve_beam_label(Fail, disp1MB));

            return;
        } else if (beam_jit_is_shallow_boxed(literal)) {
            comment("optimized non-equality test with %T", literal);
            mov_var(ARG1, x);
            mov_arg(ARG2, Y);
            fragment_call(ga->get_is_eq_exact_shallow_boxed_shared());
            a.b_eq(resolve_beam_label(Fail, disp1MB));

            return;
        } else if (is_bitstring(literal) && bitstring_size(literal) == 0) {
            Label next = a.newLabel(), not_sub_bits = a.newLabel();

            comment("simplified non-equality test with empty binary");
            emit_is_boxed(next, X, x.reg);
            emit_untag_ptr(ARG1, x.reg);

            ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
            a.ldp(TMP1, TMP2, arm::Mem(ARG1));

            a.cmp(TMP1, imm(HEADER_SUB_BITS));
            a.b_ne(not_sub_bits);
            {
                ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
                a.ldp(TMP2, TMP3, arm::Mem(ARG1, offsetof(ErlSubBits, start)));
                a.sub(TMP2, TMP3, TMP2);
            }
            a.bind(not_sub_bits);

            if (masked_types<BeamTypeId::MaybeBoxed>(X) ==
                BeamTypeId::Bitstring) {
                comment("skipped header test since we know it's a bitstring "
                        "when boxed");
                a.cbz(TMP2, resolve_beam_label(Fail, disp1MB));
            } else {
                const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
                ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
                ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                               (_TAG_HEADER_HEAP_BITS & mask));
                a.and_(TMP1, TMP1, imm(mask));
                a.cmp(TMP1, imm(_TAG_HEADER_HEAP_BITS));
                a.ccmp(TMP2, imm(0), imm(NZCV::kNone), imm(arm::CondCode::kEQ));
                a.b_eq(resolve_beam_label(Fail, disp1MB));
            }

            a.bind(next);

            return;
        } else if (is_map(literal) && erts_map_size(literal) == 0) {
            Label next = a.newLabel();

            comment("optimized non-equality test with empty map", literal);
            emit_is_boxed(next, X, x.reg);
            emit_untag_ptr(ARG1, x.reg);
            a.ldp(TMP1, TMP2, arm::Mem(ARG1));
            cmp(TMP1, MAP_HEADER_FLATMAP);
            a.ccmp(TMP2, imm(0), imm(NZCV::kNone), imm(arm::CondCode::kEQ));
            a.b_eq(resolve_beam_label(Fail, disp1MB));

            a.bind(next);

            return;
        }
    }

    /* If either argument is known to be an immediate, we can fail immediately
     * if they're equal. */
    if (always_immediate(X) || always_immediate(Y)) {
        if (!X.isImmed() && !Y.isImmed()) {
            comment("simplified check since one argument is an immediate");
        }

        preserve_cache([&]() {
            cmp_arg(x.reg, Y);
            a.b_eq(resolve_beam_label(Fail, disp1MB));
        });

        return;
    }

    /* Both operands are registers or literals. */
    Label next = a.newLabel();
    auto y = load_source(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.b_eq(resolve_beam_label(Fail, disp1MB));

    if (exact_type<BeamTypeId::Integer>(X) &&
        exact_type<BeamTypeId::Integer>(Y)) {
        /* Succeed immediately if one of the operands is a small. */
        a.orr(TMP1, x.reg, y.reg);
        emit_is_boxed(next, TMP1);
    } else if (always_same_types(X, Y)) {
        comment("skipped tag test since they are always equal");
    } else {
        /* Test whether the terms are definitely unequal based on the tags
         * alone. */
        emit_is_unequal_based_on_tags(next, X, x.reg, Y, y.reg);
    }

    /* Both operands are pointers having the same tag. Must do a
     * deeper comparison. */
    mov_var(ARG1, x);
    mov_var(ARG2, y);

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::Float>(X) ||
        always_one_of<BeamTypeId::Integer, BeamTypeId::Float>(Y)) {
        fragment_call(ga->get_is_eq_exact_shallow_boxed_shared());
        a.b_eq(resolve_beam_label(Fail, disp1MB));
    } else {
        emit_enter_runtime();
        runtime_call<int (*)(Eterm, Eterm), eq>();
        emit_leave_runtime();
        a.cbnz(ARG1.w(), resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_eq(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    Label next = a.newLabel();
    auto [x, y] = load_sources(X, ARG1, Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.b_eq(next);

    if (always_one_of<BeamTypeId::Cons, BeamTypeId::AlwaysBoxed>(X) ||
        always_one_of<BeamTypeId::Cons, BeamTypeId::AlwaysBoxed>(X)) {
        comment("skipped test for immediate because one operand never is");
    } else {
        /* We can skip deep comparisons when both args are immediates. */
        emit_are_both_immediate(X, x.reg, Y, y.reg);
        a.b_eq(resolve_beam_label(Fail, disp1MB));
    }

    mov_var(ARG1, x);
    mov_var(ARG2, y);
    fragment_call(ga->get_arith_compare_shared());
    a.b_ne(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    Label next = a.newLabel();
    auto [x, y] = load_sources(X, ARG1, Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.b_eq(resolve_beam_label(Fail, disp1MB));

    if (always_one_of<BeamTypeId::Cons, BeamTypeId::AlwaysBoxed>(X) ||
        always_one_of<BeamTypeId::Cons, BeamTypeId::AlwaysBoxed>(X)) {
        comment("skipped test for immediate because one operand never is");
    } else {
        /* We can skip deep comparisons when both args are immediates. */
        emit_are_both_immediate(X, x.reg, Y, y.reg);
        a.b_eq(next);
    }

    mov_var(ARG1, x);
    mov_var(ARG2, y);
    fragment_call(ga->get_arith_compare_shared());
    a.b_eq(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

/*
 * ARG1 = LHS
 * ARG2 = RHS
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_arith_compare_shared() {
    Label atom_compare = a.newLabel(), generic_compare = a.newLabel();

    /* Are both floats?
     *
     * This is done first as relative comparisons on atoms doesn't make much
     * sense. */
    a.orr(TMP1, ARG1, ARG2);
    emit_is_boxed(atom_compare, TMP1);

    a64::Gp boxed_ptr1 = emit_ptr_val(TMP1, ARG1);
    a.ldur(TMP3, emit_boxed_val(boxed_ptr1));
    a64::Gp boxed_ptr2 = emit_ptr_val(TMP2, ARG2);
    a.ldur(TMP4, emit_boxed_val(boxed_ptr2));

    mov_imm(TMP5, HEADER_FLONUM);
    a.cmp(TMP3, TMP5);
    a.ccmp(TMP4, TMP5, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
    a.b_ne(generic_compare);

    a.ldur(a64::d0, emit_boxed_val(boxed_ptr1, sizeof(Eterm)));
    a.ldur(a64::d1, emit_boxed_val(boxed_ptr2, sizeof(Eterm)));
    a.fcmpe(a64::d0, a64::d1);
    a.ret(a64::x30);

    a.bind(atom_compare);
    {
        a.and_(TMP1, ARG1, imm(_TAG_IMMED2_MASK));
        a.and_(TMP2, ARG2, imm(_TAG_IMMED2_MASK));
        a.sub(TMP1, TMP1, imm(_TAG_IMMED2_ATOM));
        a.sub(TMP2, TMP2, imm(_TAG_IMMED2_ATOM));
        a.orr(TMP1, TMP1, TMP2);
        a.cbnz(TMP1, generic_compare);

        emit_enter_runtime_frame();
        emit_enter_runtime();

        runtime_call<int (*)(Eterm, Eterm), erts_cmp_atoms>();

        emit_leave_runtime();
        emit_leave_runtime_frame();

        /* Note: erts_cmp_atoms() returns int, not Sint. */
        a.tst(ARG1.w(), ARG1.w());
        a.ret(a64::x30);
    }

    a.bind(generic_compare);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime();

        comment("erts_cmp_compound(X, Y, 0, 0);");
        mov_imm(ARG3, 0);
        mov_imm(ARG4, 0);
        runtime_call<Sint (*)(Eterm, Eterm, int, int), erts_cmp_compound>();

        emit_leave_runtime();
        emit_leave_runtime_frame();

        a.tst(ARG1, ARG1);

        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_is_lt(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    auto [lhs, rhs] = load_sources(LHS, ARG1, RHS, ARG2);

    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");
        a.cmp(lhs.reg, rhs.reg);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
    } else if (always_small(LHS) && exact_type<BeamTypeId::Integer>(RHS) &&
               hasLowerBound(RHS)) {
        Label next = a.newLabel();
        comment("simplified test because it always succeeds when RHS is a "
                "bignum");
        emit_is_not_boxed(next, rhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
        a.bind(next);
    } else if (always_small(LHS) && exact_type<BeamTypeId::Integer>(RHS) &&
               hasUpperBound(RHS)) {
        comment("simplified test because it always fails when RHS is a bignum");
        emit_is_not_boxed(resolve_beam_label(Fail, dispUnknown), rhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
    } else if (exact_type<BeamTypeId::Integer>(LHS) && hasLowerBound(LHS) &&
               always_small(RHS)) {
        comment("simplified test because it always fails when LHS is a bignum");
        emit_is_not_boxed(resolve_beam_label(Fail, dispUnknown), lhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
    } else if (exact_type<BeamTypeId::Integer>(LHS) && hasUpperBound(LHS) &&
               always_small(RHS)) {
        Label next = a.newLabel();
        comment("simplified test because it always succeeds when LHS is a "
                "bignum");
        emit_is_not_boxed(next, rhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
        a.bind(next);
    } else if (LHS.isLiteral() || RHS.isLiteral()) {
        Label next = a.newLabel();
        comment("skipped test for small because one operand is never small");
        a.cmp(lhs.reg, rhs.reg);
        a.b_eq(next);

        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());

        a.bind(next);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        Label branch_compare = a.newLabel();

        a.cmp(lhs.reg, rhs.reg);

        /* The only possible kind of immediate is a small and all other values
         * are boxed, so we can test for smalls by testing boxed. */
        comment("simplified small test since all other types are boxed");
        if (always_small(LHS)) {
            emit_is_boxed(branch_compare, rhs.reg);
        } else if (always_small(RHS)) {
            emit_is_boxed(branch_compare, lhs.reg);
        } else {
            a.and_(TMP1, lhs.reg, rhs.reg);
            emit_is_boxed(branch_compare, TMP1);
        }

        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());

        /* The flags will either be from the initial comparison, or from the
         * shared fragment. */
        a.bind(branch_compare);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
    } else {
        Label generic = a.newLabel(), next = a.newLabel();

        /* Relative comparisons are overwhelmingly likely to be used on smalls,
         * so we'll specialize those and keep the rest in a shared fragment. */
        if (always_small(RHS)) {
            a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
        } else {
            /* Avoid the expensive generic comparison for equal terms. */
            a.cmp(lhs.reg, rhs.reg);
            a.b_eq(next);

            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP1, lhs.reg, rhs.reg);
            a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
        }

        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);

        a.cmp(lhs.reg, rhs.reg);
        a.b(next);

        a.bind(generic);
        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());

        a.bind(next);
        a.b_ge(resolve_beam_label(Fail, disp1MB));
    }
}

void BeamModuleAssembler::emit_is_ge(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    if (always_small(LHS) && RHS.isSmall() && RHS.isImmed()) {
        auto lhs = load_source(LHS, ARG1);
        comment("simplified compare because one operand is an immediate small");
        cmp(lhs.reg, RHS.as<ArgImmed>().get());
        a.b_lt(resolve_beam_label(Fail, disp1MB));
        return;
    } else if (LHS.isSmall() && LHS.isImmed() && always_small(RHS)) {
        auto rhs = load_source(RHS, ARG1);
        comment("simplified compare because one operand is an immediate small");
        cmp(rhs.reg, LHS.as<ArgImmed>().get());
        a.b_gt(resolve_beam_label(Fail, disp1MB));
        return;
    }

    auto [lhs, rhs] = load_sources(LHS, ARG1, RHS, ARG2);

    if (always_small(LHS) && always_small(RHS)) {
        comment("skipped test for small operands since they are always small");
        a.cmp(lhs.reg, rhs.reg);
        a.b_lt(resolve_beam_label(Fail, disp1MB));
    } else if (always_small(LHS) && exact_type<BeamTypeId::Integer>(RHS) &&
               hasLowerBound(RHS)) {
        comment("simplified test because it always fails when RHS is a bignum");
        emit_is_not_boxed(resolve_beam_label(Fail, dispUnknown), rhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_lt(resolve_beam_label(Fail, disp1MB));
    } else if (always_small(LHS) && exact_type<BeamTypeId::Integer>(RHS) &&
               hasUpperBound(RHS)) {
        Label next = a.newLabel();
        comment("simplified test because it always succeeds when RHS is a "
                "bignum");
        emit_is_not_boxed(next, rhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_lt(resolve_beam_label(Fail, disp1MB));
        a.bind(next);
    } else if (exact_type<BeamTypeId::Integer>(LHS) && hasUpperBound(LHS) &&
               always_small(RHS)) {
        comment("simplified test because it always fails when LHS is a bignum");
        emit_is_not_boxed(resolve_beam_label(Fail, dispUnknown), lhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_lt(resolve_beam_label(Fail, disp1MB));
    } else if (exact_type<BeamTypeId::Integer>(LHS) && hasLowerBound(LHS) &&
               always_small(RHS)) {
        Label next = a.newLabel();
        comment("simplified test because it always succeeds when LHS is a "
                "bignum");
        emit_is_not_boxed(next, lhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_lt(resolve_beam_label(Fail, disp1MB));
        a.bind(next);
    } else if (exact_type<BeamTypeId::Integer>(LHS) && always_small(RHS)) {
        Label big = a.newLabel(), next = a.newLabel();
        comment("simplified small test for known integer");
        emit_is_not_boxed(big, lhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_ge(next);
        a.b(resolve_beam_label(Fail, disp128MB));

        a.bind(big);
        {
            a64::Gp boxed_ptr = emit_ptr_val(TMP1, lhs.reg);
            const int bitNumber = 2;
            const int bitValue = NEG_BIG_SUBTAG - POS_BIG_SUBTAG;
            a.ldur(TMP1, emit_boxed_val(boxed_ptr));
            ERTS_CT_ASSERT((1 << bitNumber) == bitValue);
            /* Fail if the bignum is negative. */
            a.tbnz(TMP1, imm(bitNumber), resolve_beam_label(Fail, disp32K));
        }
        a.bind(next);
    } else if (always_small(LHS) && exact_type<BeamTypeId::Integer>(RHS)) {
        Label big = a.newLabel(), next = a.newLabel();
        comment("simplified small test for known integer");
        emit_is_not_boxed(big, rhs.reg);
        a.cmp(lhs.reg, rhs.reg);
        a.b_ge(next);
        a.b(resolve_beam_label(Fail, disp128MB));

        a.bind(big);
        {
            a64::Gp boxed_ptr = emit_ptr_val(TMP1, rhs.reg);
            const int bitNumber = 2;
            const int bitValue = NEG_BIG_SUBTAG - POS_BIG_SUBTAG;
            a.ldur(TMP1, emit_boxed_val(boxed_ptr));
            ERTS_CT_ASSERT((1 << bitNumber) == bitValue);
            /* Fail if the bignum is positive. */
            a.tbz(TMP1, imm(bitNumber), resolve_beam_label(Fail, disp32K));
        }
        a.bind(next);
    } else if (LHS.isLiteral() || RHS.isLiteral()) {
        comment("skipped test for small because one operand is never small");
        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());
        a.b_lt(resolve_beam_label(Fail, disp1MB));
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       LHS) &&
               always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       RHS)) {
        Label branch_compare = a.newLabel();

        a.cmp(lhs.reg, rhs.reg);

        /* The only possible kind of immediate is a small and all other values
         * are boxed, so we can test for smalls by testing boxed. */
        comment("simplified small test since all other types are boxed");
        if (always_small(LHS)) {
            emit_is_boxed(branch_compare, rhs.reg);
        } else if (always_small(RHS)) {
            emit_is_boxed(branch_compare, lhs.reg);
        } else {
            a.and_(TMP1, lhs.reg, rhs.reg);
            emit_is_boxed(branch_compare, TMP1);
        }

        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());

        /* The flags will either be from the initial comparison, or from the
         * shared fragment. */
        a.bind(branch_compare);
        a.b_lt(resolve_beam_label(Fail, disp1MB));
    } else {
        Label generic = a.newLabel(), next = a.newLabel();

        /* Relative comparisons are overwhelmingly likely to be used on smalls,
         * so we'll specialize those and keep the rest in a shared fragment. */
        if (always_small(RHS)) {
            a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
        } else if (always_small(LHS)) {
            a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
        } else {
            /* Avoid the expensive generic comparison for equal terms. */
            a.cmp(lhs.reg, rhs.reg);
            a.b_eq(next);

            ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
            a.and_(TMP1, lhs.reg, rhs.reg);
            a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
        }

        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);

        a.cmp(lhs.reg, rhs.reg);
        a.b(next);

        a.bind(generic);
        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());

        a.bind(next);
        a.b_lt(resolve_beam_label(Fail, disp1MB));
    }
}

/*
 * ARG1 = Src
 * ARG2 = Min
 * ARG3 = Max
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_is_in_range_shared() {
    Label immediate = a.newLabel(), generic_compare = a.newLabel(),
          float_done = a.newLabel(), done = a.newLabel();

    /* Is the source a float? */
    emit_is_boxed(immediate, ARG1);

    a64::Gp boxed_ptr = emit_ptr_val(TMP1, ARG1);
    a.ldur(TMP2, emit_boxed_val(boxed_ptr));

    mov_imm(TMP3, HEADER_FLONUM);
    a.cmp(TMP2, TMP3);
    a.b_ne(generic_compare);

    a.ldur(a64::d0, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.asr(TMP1, ARG2, imm(_TAG_IMMED1_SIZE));
    a.scvtf(a64::d1, TMP1);

    a.fcmpe(a64::d0, a64::d1);
    a.b_mi(float_done);

    a.asr(TMP1, ARG3, imm(_TAG_IMMED1_SIZE));
    a.scvtf(a64::d1, TMP1);
    a.fcmpe(a64::d0, a64::d1);
    a.b_gt(float_done);
    a.tst(ZERO, ZERO);

    a.bind(float_done);
    a.ret(a64::x30);

    a.bind(immediate);
    {
        /*
         * Src is an immediate (such as ATOM) but not SMALL.
         * That means that Src must be greater than the upper
         * limit.
         */
        mov_imm(TMP1, 1);
        a.cmp(TMP1, imm(0));
        a.ret(a64::x30);
    }

    a.bind(generic_compare);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime();

        a.stp(ARG1, ARG3, TMP_MEM1q);

        comment("erts_cmp_compound(X, Y, 0, 0);");
        mov_imm(ARG3, 0);
        mov_imm(ARG4, 0);
        runtime_call<Sint (*)(Eterm, Eterm, int, int), erts_cmp_compound>();
        ;
        a.tst(ARG1, ARG1);
        a.b_mi(done);

        a.ldp(ARG1, ARG2, TMP_MEM1q);

        comment("erts_cmp_compound(X, Y, 0, 0);");
        mov_imm(ARG3, 0);
        mov_imm(ARG4, 0);
        runtime_call<Sint (*)(Eterm, Eterm, int, int), erts_cmp_compound>();
        ;
        a.tst(ARG1, ARG1);

        a.bind(done);
        emit_leave_runtime();
        emit_leave_runtime_frame();

        a.ret(a64::x30);
    }
}

/*
 * 1121 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_in_range(ArgLabel const &Small,
                                           ArgLabel const &Large,
                                           ArgRegister const &Src,
                                           ArgConstant const &Min,
                                           ArgConstant const &Max) {
    Label next = a.newLabel(), generic = a.newLabel();
    bool need_generic = true;
    auto src = load_source(Src, ARG1);

    if (always_small(Src)) {
        need_generic = false;
        comment("skipped test for small operand since it always small");
    } else if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(
                       Src)) {
        /* The only possible kind of immediate is a small and all
         * other values are boxed, so we can test for smalls by
         * testing boxed. */
        comment("simplified small test since all other types are boxed");
        ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED == (1 << 0));
        if (Small == Large && never_one_of<BeamTypeId::Float>(Src)) {
            /* Src is never a float and the failure labels are
             * equal. Therefore, since a bignum will never be within
             * the range, we can fail immediately if Src is not a
             * small. */
            need_generic = false;
            a.tbz(src.reg, imm(0), resolve_beam_label(Small, disp32K));
        } else {
            /* Src can be a float or the failures labels are distinct.
             * We need to call the generic routine if Src is not a small. */
            a.tbz(src.reg, imm(0), generic);
        }
    } else if (Small == Large) {
        /* We can save one instruction if we incorporate the test for
         * small into the range check. */
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        comment("simplified small & range tests since failure labels are "
                "equal");
        sub(TMP1, src.reg, Min.as<ArgImmed>().get());

        /* Since we have subtracted the (tagged) lower bound, the
         * tag bits of the difference is 0 if and only if Src is
         * a small. Testing for a tag of 0 can be done in two
         * instructions. */
        a.tst(TMP1, imm(_TAG_IMMED1_MASK));
        a.b_ne(generic);

        /* Now do the range check. */
        cmp(TMP1, Max.as<ArgImmed>().get() - Min.as<ArgImmed>().get());
        a.b_hi(resolve_beam_label(Small, disp1MB));

        /* Bypass the test code. */
        goto test_done;
    } else {
        /* We have no applicable type information and the failure
         * labels are distinct. Emit the standard test for small
         * and call the generic routine if Src is not a small. */
        a.and_(TMP1, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);
    }

    /* We have now established that the operand is small. */
    if (Small == Large) {
        comment("simplified range test since failure labels are equal");
        sub(TMP1, src.reg, Min.as<ArgImmed>().get());
        cmp(TMP1, Max.as<ArgImmed>().get() - Min.as<ArgImmed>().get());
        a.b_hi(resolve_beam_label(Small, disp1MB));
    } else {
        cmp(src.reg, Min.as<ArgImmed>().get());
        a.b_lt(resolve_beam_label(Small, disp1MB));
        cmp(src.reg, Max.as<ArgImmed>().get());
        a.b_gt(resolve_beam_label(Large, disp1MB));
    }

test_done:
    if (need_generic) {
        a.b(next);
    }

    a.bind(generic);
    if (!need_generic) {
        comment("skipped generic comparison because it is not needed");
    } else {
        mov_var(ARG1, src);
        mov_arg(ARG2, Min);
        mov_arg(ARG3, Max);
        fragment_call(ga->get_is_in_range_shared());
        if (Small == Large) {
            a.b_ne(resolve_beam_label(Small, disp1MB));
        } else {
            a.b_lt(resolve_beam_label(Small, disp1MB));
            a.b_gt(resolve_beam_label(Large, disp1MB));
        }
    }

    a.bind(next);
}

/*
 * ARG1 = Src
 * ARG2 = A
 * ARG3 = B
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_is_ge_lt_shared() {
    Label done = a.newLabel();

    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.stp(ARG1, ARG3, TMP_MEM1q);

    comment("erts_cmp_compound(Src, A, 0, 0);");
    mov_imm(ARG3, 0);
    mov_imm(ARG4, 0);
    runtime_call<Sint (*)(Eterm, Eterm, int, int), erts_cmp_compound>();
    ;
    a.tst(ARG1, ARG1);
    a.b_mi(done);

    comment("erts_cmp_compound(B, Src, 0, 0);");
    a.ldp(ARG2, ARG1, TMP_MEM1q);
    mov_imm(ARG3, 0);
    mov_imm(ARG4, 0);
    runtime_call<Sint (*)(Eterm, Eterm, int, int), erts_cmp_compound>();
    ;
    a.cmp(ARG1, imm(0));

    /* Make sure that ARG1 is -1, 0, or 1. */
    a.cset(ARG1, imm(arm::CondCode::kNE));
    a.csinv(ARG1, ARG1, ZERO, imm(arm::CondCode::kGE));

    /* Prepare return value and flags. */
    a.adds(ARG1, ARG1, imm(1));

    /* We now have:
     *   ARG1 == 0 if B < SRC
     *   ARG1 > 0 if B => SRC
     * and flags set accordingly. */

    a.bind(done);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

/*
 * The instruction sequence:
 *
 *   is_ge Fail1 Src A
 *   is_lt Fail1 B Src
 *
 * is common (1841 occurrences in OTP at the time of writing).
 *
 * is_ge + is_lt is 18 instructions, while is_ge_lt is
 * 14 instructions.
 */
void BeamModuleAssembler::emit_is_ge_lt(ArgLabel const &Fail1,
                                        ArgLabel const &Fail2,
                                        ArgRegister const &Src,
                                        ArgConstant const &A,
                                        ArgConstant const &B) {
    Label generic = a.newLabel(), next = a.newLabel();
    auto src = load_source(Src, ARG1);

    mov_arg(ARG2, A);
    mov_arg(ARG3, B);

    preserve_cache(
            [&]() {
                a.and_(TMP1, src.reg, imm(_TAG_IMMED1_MASK));
                a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
                a.b_ne(generic);

                a.cmp(src.reg, ARG2);
                a.b_lt(resolve_beam_label(Fail1, disp1MB));
                a.cmp(ARG3, src.reg);
                a.b_ge(resolve_beam_label(Fail2, disp1MB));
                a.b(next);

                a.bind(generic);
                mov_var(ARG1, src);
                fragment_call(ga->get_is_ge_lt_shared());
                a.b_lt(resolve_beam_label(Fail1, disp1MB));
                a.b_gt(resolve_beam_label(Fail2, disp1MB));

                a.bind(next);
            },
            TMP1);
}

/*
 * 1190 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_ge_ge(ArgLabel const &Fail1,
                                        ArgLabel const &Fail2,
                                        ArgRegister const &Src,
                                        ArgConstant const &A,
                                        ArgConstant const &B) {
    if (!always_small(Src)) {
        /* In practice, it is uncommon that Src is not a known small
         * integer, so we will not bother optimizing that case. */
        emit_is_ge(Fail1, Src, A);
        emit_is_ge(Fail2, Src, B);
        return;
    }

    auto src = load_source(Src, ARG1);

    preserve_cache(
            [&]() {
                subs(TMP1, src.reg, A.as<ArgImmed>().get());
                a.b_lt(resolve_beam_label(Fail1, disp1MB));
                cmp(TMP1, B.as<ArgImmed>().get() - A.as<ArgImmed>().get());
                a.b_lo(resolve_beam_label(Fail2, disp1MB));
            },
            TMP1);
}

/*
 * 60 occurrences in OTP at the time of writing. Seems to be common in
 * Elixir code.
 *
 * Currently not very frequent in OTP but very nice reduction in code
 * size when it happens. We expect this combination of instructions
 * to become more common in the future.
 */
void BeamModuleAssembler::emit_is_int_in_range(ArgLabel const &Fail,
                                               ArgRegister const &Src,
                                               ArgConstant const &Min,
                                               ArgConstant const &Max) {
    auto src = load_source(Src, ARG1);

    preserve_cache(
            [&]() {
                sub(TMP1, src.reg, Min.as<ArgImmed>().get());

                /* Since we have subtracted the (tagged) lower bound, the tag
                 * bits of the difference is 0 if and only if Src is a
                 * small. */
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
                a.tst(TMP1, imm(_TAG_IMMED1_MASK));
                a.b_ne(resolve_beam_label(Fail, disp1MB));
                cmp(TMP1, Max.as<ArgImmed>().get() - Min.as<ArgImmed>().get());
                a.b_hi(resolve_beam_label(Fail, disp1MB));
            },
            TMP1);
}

/*
 * 428 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_int_ge(ArgLabel const &Fail,
                                         ArgRegister const &Src,
                                         ArgConstant const &Min) {
    auto src = load_source(Src, ARG1);
    Label small = a.newLabel(), next = a.newLabel();

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified small test since all other types are boxed");
        emit_is_boxed(small, Src, src.reg);
    } else {
        preserve_cache(
                [&]() {
                    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
                    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
                    a.b_eq(small);
                },
                TMP2);

        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, TMP2);
    }

    preserve_cache(
            [&]() {
                a64::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
                a.ldur(TMP1, emit_boxed_val(boxed_ptr));
                a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
                a.cmp(TMP1, imm(_TAG_HEADER_POS_BIG));
                a.b_ne(resolve_beam_label(Fail, disp1MB));
                a.b(next);

                a.bind(small);
                cmp(src.reg, Min.as<ArgImmed>().get());
                a.b_lt(resolve_beam_label(Fail, disp1MB));

                a.bind(next);
            },
            TMP1);
}

void BeamModuleAssembler::emit_badmatch(const ArgSource &Src) {
    emit_error(BADMATCH, Src);
    mark_unreachable();
}

void BeamModuleAssembler::emit_case_end(const ArgSource &Src) {
    emit_error(EXC_CASE_CLAUSE, Src);
    mark_unreachable();
}

void BeamModuleAssembler::emit_system_limit_body() {
    emit_error(SYSTEM_LIMIT);
    mark_unreachable();
}

void BeamModuleAssembler::emit_if_end() {
    emit_error(EXC_IF_CLAUSE);
    mark_unreachable();
}

void BeamModuleAssembler::emit_badrecord(const ArgSource &Src) {
    emit_error(EXC_BADRECORD, Src);
    mark_unreachable();
}

void BeamModuleAssembler::emit_catch(const ArgYRegister &Y,
                                     const ArgCatch &Handler) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.add(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));

    mov_arg(Y, Handler);
}

void BeamGlobalAssembler::emit_catch_end_shared() {
    Label not_throw = a.newLabel(), not_error = a.newLabel(),
          after_gc = a.newLabel();

    /* XREG0 = THE_NON_VALUE
     * XREG1 = error reason/thrown value
     * XREG2 = raw stacktrace.
     * XREG3 = class
     */

    a.mov(XREG0, XREG1);
    a.cmp(XREG3, imm(am_throw));

    a.b_ne(not_throw);

    /* Return thrown value. */
    a.ret(a64::x30);

    a.bind(not_throw);
    {
        emit_enter_runtime_frame();

        a.mov(ARG1, XREG0);

        a.cmp(XREG3, imm(am_error));
        a.b_ne(not_error);

        a.mov(ARG2, XREG0);
        a.mov(ARG3, XREG2);

        /* This is an error, attach a stacktrace to the reason. */
        ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 2);
        emit_enter_runtime<Update::eHeapAlloc>(2);

        a.mov(ARG1, c_p);
        runtime_call<Eterm (*)(Process *, Eterm, Eterm), add_stacktrace>();

        emit_leave_runtime<Update::eHeapAlloc>(2);

        /* Fall through! */
    }

    /* Error term from exit/1 or stack backtrace from error/{1,2,3} is
     * now in ARG1. */
    a.bind(not_error);
    {
        const int32_t bytes_needed = (3 + S_RESERVED) * sizeof(Eterm);
        add(ARG3, HTOP, bytes_needed);
        a.cmp(ARG3, E);

        a.b_ls(after_gc);
        {
            /* Preserve stacktrace / reason. */
            a.mov(XREG0, ARG1);

            mov_imm(ARG4, 1);
            a.bl(labels[garbage_collect]);

            a.mov(ARG1, XREG0);
        }
        a.bind(after_gc);

        a.add(XREG0, HTOP, imm(TAG_PRIMARY_BOXED));
        mov_imm(TMP1, make_arityval(2));
        mov_imm(TMP2, am_EXIT);
        a.stp(TMP1, TMP2, arm::Mem(HTOP).post(2 * sizeof(Eterm)));
        a.str(ARG1, arm::Mem(HTOP).post(sizeof(Eterm)));
    }

    emit_leave_runtime_frame();
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_catch_end(const ArgYRegister &CatchTag) {
    Label next = a.newLabel();

    /* XREG0 = THE_NON_VALUE
     * XREG1 = class
     * XREG2 = error reason/thrown value
     * XREG3 = raw stacktrace. */

    emit_try_end(CatchTag);
    emit_branch_if_value(XREG0, next);
    fragment_call(ga->get_catch_end_shared());
    a.bind(next);
}

void BeamModuleAssembler::emit_try_end(const ArgYRegister &CatchTag) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    mov_imm(TMP1, NIL);
    a.str(TMP1, getArgRef(CatchTag));
}

void BeamModuleAssembler::emit_try_end_deallocate(const ArgWord &Deallocate) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    if (Deallocate.get() > 0) {
        add(E, E, Deallocate.get() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_try_end_move_deallocate(
        const ArgSource &Src,
        const ArgRegister &Dst,
        const ArgWord &Deallocate) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    mov_arg(Dst, Src);
    if (Deallocate.get() > 0) {
        add(E, E, Deallocate.get() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_try_case(const ArgYRegister &CatchTag) {
    /* XREG0 = THE_NON_VALUE
     * XREG1 = error reason/thrown value
     * XREG2 = raw stacktrace.
     * XREG3 = class */

    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.mov(XREG0, XREG3);
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));

    /* The try_tag in the Y slot in the stack frame has already been
     * cleared. */

#ifdef DEBUG
    Label ok = a.newLabel();
    comment("Start of assertion code");
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.ldr(TMP2, arm::Mem(c_p, offsetof(Process, ftrace)));
    mov_imm(TMP3, NIL);
    a.cmp(TMP1, TMP3);
    a.ccmp(TMP2, TMP3, imm(NZCV::kNone), imm(arm::CondCode::kEQ));
    a.b_eq(ok);

    comment("Assertion c_p->fvalue == NIL && c_p->ftrace == NIL failed");
    a.udf(0x42);

    a.bind(ok);
#endif
}

void BeamModuleAssembler::emit_try_case_end(const ArgSource &Src) {
    emit_error(EXC_TRY_CLAUSE, Src);
}

void BeamGlobalAssembler::emit_raise_shared() {
    a.str(ARG1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, ftrace)));

    emit_enter_runtime(0);

    a.mov(ARG1, c_p);
    runtime_call<void (*)(Process *, Eterm), erts_sanitize_freason>();

    emit_leave_runtime(0);

    a.mov(ARG4, ZERO);
    a.mov(ARG2, a64::x30);
    a.b(labels[raise_exception_shared]);
}

void BeamModuleAssembler::emit_raise(const ArgSource &Trace,
                                     const ArgSource &Value) {
    auto [value, trace] = load_sources(Value, ARG1, Trace, ARG2);
    mov_var(ARG1, value);
    mov_var(ARG2, trace);
    fragment_call(ga->get_raise_shared());

    mark_unreachable();

    /* `line` instructions need to know the latest offset that may throw an
     * exception. See the `line` instruction for details. */
    last_error_offset = a.offset();
}

void BeamModuleAssembler::emit_build_stacktrace() {
    a.mov(ARG2, XREG0);

    emit_enter_runtime<Update::eHeapAlloc>(0);

    a.mov(ARG1, c_p);
    runtime_call<Eterm (*)(Process *, Eterm), build_stacktrace>();

    emit_leave_runtime<Update::eHeapAlloc>(0);

    a.mov(XREG0, ARG1);
}

/* This instruction has the same semantics as the erlang:raise/3 BIF,
 * except that it can rethrow a raw stack backtrace. */
void BeamModuleAssembler::emit_raw_raise() {
    Label next = a.newLabel();

    a.mov(ARG1, XREG2);
    a.mov(ARG2, XREG0);
    a.mov(ARG3, XREG1);
    a.mov(ARG4, c_p);

    emit_enter_runtime(0);
    runtime_call<int (*)(Eterm, Eterm, Eterm, Process *), raw_raise>();
    emit_leave_runtime(0);

    a.cbnz(ARG1, next);

    emit_raise_exception();

    a.bind(next);
    mov_imm(XREG0, am_badarg);
}

#define TEST_YIELD_RETURN_OFFSET                                               \
    (BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(Uint32[3]) +                         \
     (erts_alcu_enable_code_atags ? sizeof(Uint32) : 0))

/* ARG3 = current_label */
void BeamGlobalAssembler::emit_i_test_yield_shared() {
    a.sub(ARG2, ARG3, imm(sizeof(ErtsCodeMFA)));
    a.add(ARG3, ARG3, imm(TEST_YIELD_RETURN_OFFSET));

    a.str(ARG2, arm::Mem(c_p, offsetof(Process, current)));
    a.ldr(ARG2.w(), arm::Mem(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.strb(ARG2.w(), arm::Mem(c_p, offsetof(Process, arity)));

    a.b(labels[context_switch_simplified]);
}

void BeamModuleAssembler::emit_i_test_yield() {
    /* When present, this is guaranteed to be the first instruction after the
     * breakpoint trampoline. */
    ASSERT((a.offset() - code.labelOffsetFromBase(current_label)) ==
           BEAM_ASM_FUNC_PROLOGUE_SIZE);

    a.adr(ARG3, current_label);

    if (erts_alcu_enable_code_atags) {
        /* The point-of-origin allocation tags are vastly improved when the
         * instruction pointer is updated frequently. This has a relatively low
         * impact on performance but there's little point in doing this unless
         * the user has requested it -- it's an undocumented feature for
         * now. */
        a.str(ARG3, arm::Mem(c_p, offsetof(Process, i)));
    }

    a.subs(FCALLS, FCALLS, imm(1));
    a.b_le(resolve_fragment(ga->get_i_test_yield_shared(), disp1MB));

    ASSERT((a.offset() - code.labelOffsetFromBase(current_label)) ==
           TEST_YIELD_RETURN_OFFSET);
}

void BeamModuleAssembler::emit_i_yield() {
    mov_imm(XREG0, am_true);
    fragment_call(ga->get_dispatch_return());
}

void BeamModuleAssembler::emit_i_perf_counter() {
    Label next = a.newLabel(), small = a.newLabel();

    emit_enter_runtime_frame();

    /* Call the function pointer used by erts_sys_perf_counter */
#ifdef WIN32
    mov_imm(TMP1, erts_sys_time_data__.r.o.sys_hrtime);
#else
    mov_imm(TMP1, erts_sys_time_data__.r.o.perf_counter);
#endif
    dynamic_runtime_call<0>(TMP1);

    emit_leave_runtime_frame();

    a.asr(TMP1, ARG1, imm(SMALL_BITS - 1));
    a.add(TMP1, TMP1, imm(1));
    a.cmp(TMP1, imm(1));
    a.b_ls(small);

    {
        a.mov(XREG0, ARG1);

        emit_gc_test(ArgWord(0),
                     ArgWord(ERTS_MAX_UINT64_HEAP_SIZE),
                     ArgWord(0));

        mov_imm(TMP1, make_pos_bignum_header(1));
        a.stp(TMP1, XREG0, arm::Mem(HTOP).post(sizeof(Eterm[2])));
        a.sub(XREG0, HTOP, imm(sizeof(Eterm[2]) - TAG_PRIMARY_BOXED));
        a.b(next);
    }

    a.bind(small);
    {
        a.lsl(ARG1, ARG1, imm(_TAG_IMMED1_SIZE));
        a.orr(XREG0, ARG1, imm(_TAG_IMMED1_SMALL));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_mark_unreachable() {
    mark_unreachable();
}

void BeamModuleAssembler::emit_coverage(void *coverage, Uint index, Uint size) {
    Uint address = Uint(coverage) + index * size;
    comment("coverage index = %d", index);

    mov_imm(TMP1, address);
    if (size == sizeof(Uint)) {
        if (hasCpuFeature(CpuFeatures::ARM::kLSE)) {
            mov_imm(TMP2, 1);
            a.ldaddal(TMP2, TMP2, arm::Mem(TMP1));
        } else {
            Label again = a.newLabel();
            a.bind(again);
            {
                a.ldaxr(TMP2, arm::Mem(TMP1));
                a.add(TMP2, TMP2, imm(1));
                a.stlxr(TMP2, TMP2, arm::Mem(TMP1));
                a.cbnz(TMP2, again);
            }
        }
    } else if (size == sizeof(byte)) {
        if ((address & 0xff) != 0) {
            a.strb(TMP1.w(), arm::Mem(TMP1));
        } else {
            mov_imm(TMP2, 1);
            a.strb(TMP2.w(), arm::Mem(TMP1));
        }
    } else {
        ASSERT(0);
    }
}

void BeamModuleAssembler::emit_debug_line(const ArgWord &Loc,
                                          const ArgWord &Index,
                                          const ArgWord &Live) {
    emit_validate(Live);
}
