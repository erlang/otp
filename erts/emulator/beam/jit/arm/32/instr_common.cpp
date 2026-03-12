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
    mov_imm(TMP, reason);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
    emit_raise_exception();
}

void BeamModuleAssembler::emit_error(int reason, const ArgSource &Src) {
    mov_imm(TMP, reason);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));    
    auto src = load_source(Src, TMP);
    a.str(src.reg, arm::Mem(c_p, offsetof(Process, fvalue)));
    emit_raise_exception();
}

void BeamModuleAssembler::emit_gc_test_preserve(const ArgWord &Need,
                                                const ArgWord &Live,
                                                const ArgSource &Preserve,
                                                a32::Gp preserve_reg) {
    const int32_t bytes_needed = (Need.get() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

    ASSERT(preserve_reg != ARG3);

#ifdef DEBUG
    comment("(debug: fill dead X registers with garbage)");
    const a32::Gp garbage_reg = preserve_reg == ARG4 ? ARG3 : ARG4;
    mov_imm(garbage_reg, ERTS_HOLE_MARKER);
    if (!(Preserve.isXRegister() &&
          Preserve.as<ArgXRegister>().get() >= Live.get())) {
        mov_arg(ArgXRegister(Live.get()), garbage_reg);
        mov_arg(ArgXRegister(Live.get() + 1), garbage_reg);
    } else {
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
    lea(TMP, arm::Mem(E, -(int32_t)(S_REDZONE * sizeof(Eterm))));
    a.cmp(HTOP, TMP);
    a.b_hi(crash);

    a.b(next);

    a.bind(crash);
    a.udf(0xbad);
    a.bind(next);

#    ifdef JIT_HARD_DEBUG
    emit_enter_runtime_frame();

    for (unsigned i = 0; i < Arity.get(); i++) {
        mov_arg(ARG1, ArgVal(ArgVal::XReg, i));

        emit_enter_runtime();
        runtime_call<1>(beam_jit_validate_term);
        emit_leave_runtime();
    }

    emit_leave_runtime_frame();
#    endif

#endif
}

/* Instrs */

void BeamModuleAssembler::emit_i_validate(const ArgWord &Arity) {
    // TODO
    emit_nyi("emit_i_validate");
}

/*
 * This does not allocate the heap, it prepares it to be allocated.
 * It tests the heap and the stack, and if necessary, it will call the
 * garbage collector.
 * Only the Stack is immediatly adjusted.
 */
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

    emit_enter_runtime<Update::eHeapAlloc | Update::eReductions>();
    emit_proc_lc_unrequire();

    mov_imm(TMP, EXC_NORMAL);
    a.str(TMP, arm::Mem(c_p, offsetof(Process, freason)));
    mov_imm(TMP, 0);
    a.strb(TMP, arm::Mem(c_p, offsetof(Process, arity)));
    a.mov(ARG1, c_p);
    mov_imm(ARG2, am_normal);
    runtime_call<2>(erts_do_exit_process);

    emit_proc_lc_require();
    emit_leave_runtime<Update::eHeapAlloc | Update::eReductions>();

    a.b(resolve_fragment(ga->get_do_schedule(), disp32MB));
}

void BeamModuleAssembler::emit_continue_exit() {
    /* This is implicitly global; it does not normally appear in modules and
     * doesn't require size optimization. */

     emit_enter_runtime<Update::eReductions | Update::eHeapAlloc>();
     emit_proc_lc_unrequire();
 
     a.mov(ARG1, c_p);
     runtime_call<1>(erts_continue_exit_process);
 
     emit_proc_lc_require();
     emit_leave_runtime<Update::eReductions | Update::eHeapAlloc>();
 
     a.b(resolve_fragment(ga->get_do_schedule(), disp32MB));
 }

void BeamModuleAssembler::emit_get_list(const ArgRegister &Src,
                                        const ArgRegister &Hd,
                                        const ArgRegister &Tl) {
    auto src = load_source(Src);

    auto hd = init_destination(Hd, ARG1);
    auto tl = init_destination(Tl, ARG2);

    /* We need to get rid of tag bits before using the source register. */
    untag_ptr_preserve_cache(TMP, src.reg);

    if (hd.reg == tl.reg) {
        /* ldmia with two identical registers is an illegal
         * instruction. Produce the same result as the interpreter. */
        a.ldr(tl.reg, arm::Mem(TMP, sizeof(Eterm)));
        flush_var(tl);
    } else {
        preserve_cache([&]() {
            safe_ldmia(arm::Mem(TMP), hd.reg, tl.reg);
        });
        flush_vars(hd, tl);
    }
}

void BeamModuleAssembler::emit_get_hd(const ArgRegister &Src,
                                      const ArgRegister &Hd) {
    auto src = load_source(Src);
    auto hd = init_destination(Hd, VAR);
    a32::Gp cons_ptr = emit_ptr_val(TMP, src.reg);

    a.ldr(hd.reg, getCARRef(cons_ptr));
    flush_var(hd);
}

void BeamModuleAssembler::emit_get_tl(const ArgRegister &Src,
                                      const ArgRegister &Tl) {
    auto src = load_source(Src);
    auto tl = init_destination(Tl, VAR);
    a32::Gp cons_ptr = emit_ptr_val(TMP, src.reg);

    a.ldr(tl.reg, getCDRRef(cons_ptr));
    flush_var(tl);
}

void BeamModuleAssembler::emit_i_get(const ArgSource &Src,
                                     const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_get");
}

void BeamModuleAssembler::emit_i_get_hash(const ArgConstant &Src,
                                          const ArgWord &Hash,
                                          const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_get_hash");
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
                                               a32::Gp tuple_reg) {
    Label ok = a.newLabel(), fatal = a.newLabel();
    ASSERT(tuple_reg != TMP);
    mov_arg(TMP, Src);
    emit_is_boxed(fatal, TMP);
    emit_untag_ptr(TMP, TMP);
    a.cmp(TMP, tuple_reg);
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
    // We use VAR instead of TMP because safe_ldr may make use of TMP
    auto dst = init_destination(Dst, VAR);
    safe_ldr(dst.reg, arm::Mem(ARG1, Element.get()));
    flush_var(dst);
}

void BeamModuleAssembler::emit_get_tuple_element_swap(
        const ArgSource &Src,
        const ArgWord &Element,
        const ArgRegister &Dst,
        const ArgRegister &OtherDst) {
    // TODO
    emit_nyi("emit_get_tuple_element_swap");
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

    auto dst1 = init_destination(Dst1, ARG2);
    auto dst2 = init_destination(Dst2, ARG3);

    arm::Mem element_ptr = arm::Mem(ARG1, Element.get());
    safe_ldmia(element_ptr, dst1.reg, dst2.reg);
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

        while (slots >= 2 && first_y * sizeof(Eterm) <= disp1KB) {
            // We can only store 64-bit values at a time, 
            // so we initialize a double word with -1.
            if (!q_initialized) {
                ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == 0x0f);
                a.vmov_s64(a32::d0, imm(-1));
                q_initialized = true;
            }
            // store the double word at the appropriate pair of Y registers
            a.vstr_64(a32::d0, getYRef(first_y));
            first_y += 2;
            slots -= 2;
        }

        while (slots >= 1) {
            if (q_initialized && first_y * sizeof(Eterm) <= disp1KB) {
                a.vstr_32(a32::s0, getYRef(first_y));
            } else {
                if (!x_initialized) {
                    mov_imm(TMP, NIL);
                    x_initialized = true;
                }
                a.str(TMP, getYRef(first_y));
            }
            first_y += 1;
            slots -= 1;
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
    auto dst1 = init_destination(Dst1, ARG1);
    auto dst2 = init_destination(Dst2, ARG2);
    Sint trim = Words.get() * sizeof(Eterm);
    auto src_index = Src1.get();

    ASSERT(ArgVal::memory_relation(Src1, Src2) ==
           ArgVal::Relation::consecutive);

    if (src_index == 0 && Support::isInt9(trim)) {
        /* Equivalent to ARM64's post-index pair load path:
         * fetch y0/y1 from current E, then trim E once. */
        a.ldr(dst1.reg, arm::Mem(E, 0));
        a.ldr(dst2.reg, arm::Mem(E, sizeof(Eterm)));
        add(E, E, trim);

        dst1 = init_destination(Dst1.trimmed(Words.get()), ARG1);
        dst2 = init_destination(Dst2.trimmed(Words.get()), ARG2);
        flush_vars(dst1, dst2);
    } else {
        safe_ldmia(dst1.reg, dst2.reg, Src1, Src2);

        /* Try to combine trimming with storing to one of destination
         * registers. */
        if (Dst1.isYRegister() && Dst1.as<ArgYRegister>().get() == Words.get() &&
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
            auto dst = init_destination(Dst.trimmed(Words.get()), TMP);
            a.ldr(dst.reg, src_ref);
            flush_var(dst);

            return;
        }
    }

    if (Dst.isYRegister()) {
        auto dst_index = Dst.as<ArgYRegister>().get();
        if (dst_index == Words.get() && Support::isInt9(trim)) {
            auto src = load_source(Src, TMP);
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
    auto [src1, src2] = load_sources(Src1, ARG1, Src2, ARG2);
    auto dst1 = init_destination(Dst1, src1.reg);
    auto dst2 = init_destination(Dst2, src2.reg);

    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_load_two_xregs(const ArgRegister &Src1,
                                              const ArgXRegister &Dst1,
                                              const ArgRegister &Src2,
                                              const ArgXRegister &Dst2) {
    ASSERT(ArgVal::memory_relation(Src1, Src2) ==
           ArgVal::Relation::consecutive);
    auto dst1 = init_destination(Dst1, ARG1);
    auto dst2 = init_destination(Dst2, ARG2);

    safe_ldmia(dst1.reg, dst2.reg, Src1, Src2);
    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_swap(const ArgRegister &R1,
                                    const ArgRegister &R2) {
    auto [arg1, arg2] = load_sources(R1, ARG1, R2, ARG2);
    auto dst1 = init_destination(R2, arg1.reg);
    auto dst2 = init_destination(R1, arg2.reg);
    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_swap2(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3) {
    auto [arg1, arg2] = load_sources(R1, ARG1, R2, ARG2);
    auto arg3 = load_source(R3, ARG3);

    mov_var(TMP, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, TMP);

    flush_vars(arg1, arg2, arg3);
}

void BeamModuleAssembler::emit_swap3(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4) {
    auto [arg1, arg2] = load_sources(R1, ARG1, R2, ARG2);
    auto [arg3, arg4] = load_sources(R3, ARG3, R4, ARG4);

    mov_var(TMP, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, arg4);
    mov_var(arg4, TMP);

    flush_vars(arg1, arg2, arg3);
    flush_var(arg4);
}

void BeamModuleAssembler::emit_swap4(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4,
                                     const ArgRegister &R5) {
    auto [arg1, arg2] = load_sources(R1, ARG1, R2, ARG2);
    auto [arg3, arg4] = load_sources(R3, ARG3, R4, ARG4);
    auto arg5 = load_source(R5, VAR);

    mov_var(TMP, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, arg4);
    mov_var(arg4, arg5);
    mov_var(arg5, TMP);

    flush_vars(arg1, arg2, arg3);
    flush_vars(arg4, arg5);
}

void BeamModuleAssembler::emit_node(const ArgRegister &Dst) {
    a.ldr(TMP, embed_constant(&erts_this_node, disp4KB));
    a.ldr(TMP, arm::Mem(TMP));
    mov_arg(Dst, arm::Mem(TMP, offsetof(ErlNode, sysname)));
}

void BeamModuleAssembler::emit_put_list(const ArgSource &Hd,
                                        const ArgSource &Tl,
                                        const ArgRegister &Dst) {
    auto [hd, tl] = load_sources(Hd, ARG1, Tl, ARG2);
    auto hd_reg = hd.reg;
    auto tl_reg = tl.reg;
    auto dst = init_destination(Dst, TMP);

    preserve_cache([&]() {
        safe_stmia(arm::Mem(HTOP), hd_reg, tl_reg);
        a.add(HTOP, HTOP, imm(sizeof(Eterm[2])));
        a.sub(dst.reg, HTOP, imm(sizeof(Eterm[2]) - TAG_PRIMARY_LIST));
    });

    flush_var(dst);
}

void BeamModuleAssembler::emit_put_list_deallocate(const ArgSource &Hd,
                                                   const ArgSource &Tl,
                                                   const ArgRegister &Dst,
                                                   const ArgWord &Deallocate) {
    // TODO
    emit_nyi("emit_put_list_deallocate");
}

void BeamModuleAssembler::emit_put_list2(const ArgSource &Hd1,
                                         const ArgSource &Hd2,
                                         const ArgSource &Tl,
                                         const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_put_list2");
}

void BeamModuleAssembler::emit_put_tuple2(const ArgRegister &Dst,
                                          const ArgWord &Arity,
                                          const Span<ArgVal> &args) {
    ASSERT(arityval(Arity.get()) == args.size());
    // TODO: check arm64 implementation to optimize this

    const size_t size = args.size() + 1;

    mov_arg(TMP, Arity);
    a.str(TMP, arm::Mem(HTOP));
    add(HTOP, HTOP, sizeof(Eterm));

    for (size_t i = 0; i < args.size(); i++) {
        auto src = load_source(args[i], TMP);
        a.str(src.reg, arm::Mem(HTOP));
        add(HTOP, HTOP, sizeof(Eterm));
    }

    auto ptr = init_destination(Dst, TMP);
    sub(ptr.reg, HTOP, size * sizeof(Eterm) - TAG_PRIMARY_BOXED);
    flush_var(ptr);
}

void BeamModuleAssembler::emit_self(const ArgRegister &Dst) {
    mov_arg(Dst, arm::Mem(c_p, offsetof(Process, common.id)));
}

void BeamModuleAssembler::emit_copy_words_increment(a32::Gp from,
                                                    a32::Gp to,
                                                    size_t count) {
    check_pending_stubs();

    if (count == 0) {
        return;
    }

    /* Inline small copies to avoid loop overhead. */
    if (count <= 16) {
        while (count--) {
            a.ldr(TMP, arm::Mem(from).post(sizeof(Eterm)));
            a.str(TMP, arm::Mem(to).post(sizeof(Eterm)));
        }
        return;
    }

    /* Larger copies use a tiny runtime loop to keep code size reasonable. */
    Label copy_next = a.newLabel();
    ASSERT(Support::isUInt16(count));
    mov_imm(VAR, count);

    a.bind(copy_next);
    a.ldr(TMP, arm::Mem(from).post(sizeof(Eterm)));
    a.str(TMP, arm::Mem(to).post(sizeof(Eterm)));
    a.subs(VAR, VAR, imm(1));
    a.b_ne(copy_next);
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

    a32::Gp untagged_src = ARG3;
    emit_untag_ptr(untagged_src, src.reg);

    /* Reuse fast-path for single-field updates. */
    if (Hint.get() == am_reuse && updates.size() == 2) {
        const auto next_index = updates[0].as<ArgWord>().get();
        const auto &next_value = updates[1].as<ArgSource>();

        a.ldr(TMP, arm::Mem(untagged_src, next_index * sizeof(Eterm)));
        cmp_arg(TMP, next_value);

        if (destination.reg != src.reg) {
            Label keep_new = a.newLabel();
            a.b_ne(keep_new);
            a.mov(destination.reg, src.reg);
            a.bind(keep_new);
        }
        a.b_eq(next);
    }

    size_t copy_index = 0;

    for (size_t i = 0; i < updates.size(); i += 2) {
        const auto next_index = updates[i].as<ArgWord>().get();
        const auto &next_value = updates[i + 1].as<ArgSource>();
        bool odd_copy;

        ASSERT(next_index > 0 && next_index >= copy_index);

        odd_copy = (next_index - copy_index) & 1;
        emit_copy_words_increment(untagged_src,
                                  HTOP,
                                  (next_index - copy_index) & ~1);

        if ((i + 2) < updates.size()) {
            const auto adjacent_index = updates[i + 2].as<ArgWord>().get();
            const auto &adjacent_value = updates[i + 3].as<ArgSource>();

            if (adjacent_index == next_index + 1) {
                auto [first, second] =
                        load_sources(next_value, ARG1, adjacent_value, ARG2);

                if (odd_copy) {
                    a.ldr(TMP, arm::Mem(untagged_src).post(sizeof(Eterm)));
                    a.str(TMP, arm::Mem(HTOP).post(sizeof(Eterm)));
                    a.str(first.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
                    a.str(second.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
                    add(untagged_src, untagged_src, sizeof(Eterm[2]));
                } else {
                    add(untagged_src, untagged_src, sizeof(Eterm[2]));
                    a.str(first.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
                    a.str(second.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
                }

                copy_index = next_index + 2;
                i += 2;
                continue;
            }
        }

        auto value = load_source(next_value, ARG1);

        if ((next_index - copy_index) & 1) {
            a.ldr(ARG2, arm::Mem(untagged_src).post(sizeof(Eterm)));
            a.str(ARG2, arm::Mem(HTOP).post(sizeof(Eterm)));
            a.str(value.reg, arm::Mem(HTOP).post(sizeof(Eterm)));
            add(untagged_src, untagged_src, sizeof(Eterm));
        } else {
            add(untagged_src, untagged_src, sizeof(Eterm));
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
    // TODO
    emit_nyi("emit_update_record_in_place");
}

void BeamModuleAssembler::emit_set_tuple_element(const ArgSource &Element,
                                                 const ArgRegister &Tuple,
                                                 const ArgWord &Offset) {
    // TODO
    emit_nyi("emit_set_tuple_element");
}

void BeamModuleAssembler::emit_is_nonempty_list(const ArgLabel &Fail,
                                                const ArgRegister &Src) {
    auto list_ptr = load_source(Src);
    emit_is_cons(resolve_beam_label(Fail, dispUnknown), list_ptr.reg);
}

void BeamModuleAssembler::emit_jump(const ArgLabel &Fail) {
    a.b(resolve_beam_label(Fail, disp32MB));
    mark_unreachable();
}

void BeamModuleAssembler::emit_is_atom(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    auto src = load_source(Src);

    preserve_cache(
            [&]() {
                a.and_(TMP, src.reg, imm(_TAG_IMMED2_MASK));
                a.cmp(TMP, imm(_TAG_IMMED2_ATOM));
                a.b_ne(resolve_beam_label(Fail, disp32MB));
            },
            TMP);
}

void BeamModuleAssembler::emit_is_boolean(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    /* Since am_true and am_false differ by a single bit, we can simplify the
     * check by clearing said bit and comparing against the lesser one. */
    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    auto src = load_source(Src, TMP);
    mov_imm(VAR, ~(am_true & ~_TAG_IMMED2_MASK));
    a.and_(TMP, src.reg, VAR);
    a.cmp(TMP, imm(am_false));
    a.b_ne(resolve_beam_label(Fail, disp32MB));
}

void BeamModuleAssembler::emit_is_bitstring(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Bitstring) {
        comment("skipped header test since we know it's a bitstring when "
                "boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(ARG1, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));

        /* The header mask with the binary sub tag bits removed (0b110011)
         * is not possible to use as an immediate operand for 'and'. (See
         * the note at the beginning of the file.) Therefore, use a simpler
         * mask (0b110000) that will also clear the primary tag bits. That
         * works because we KNOW that a boxed pointer always points to a header
         * word and that the primary tag for a header is 0. */
        const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
        ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
        ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS == (_TAG_HEADER_HEAP_BITS & mask));
        a.and_(TMP, TMP, imm(mask));
        a.cmp(TMP, imm(_TAG_HEADER_HEAP_BITS));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
    }
}

void BeamModuleAssembler::emit_is_binary(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
    emit_untag_ptr(ARG1, src.reg);

    ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
    a.ldmia(arm::Mem(ARG1), a32::GpList({ARG2, ARG3}));

    Label not_sub_bits = a.newLabel();
    a.cmp(ARG2, imm(HEADER_SUB_BITS));
    a.b_ne(not_sub_bits);
    {
        ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
        safe_ldmia(arm::Mem(ARG1, offsetof(ErlSubBits, start)), ARG3, ARG4);
        a.sub(ARG3, ARG4, ARG3);
    }
    a.bind(not_sub_bits);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Bitstring) {
        comment("skipped header test since we know it's a bitstring when "
                "boxed");
        a.tst(ARG3, imm(7));
    } else {
        const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
        ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
        ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS == (_TAG_HEADER_HEAP_BITS & mask));
        a.and_(ARG2, ARG2, imm(mask));

        /* Shift out all but the lowest three bits in the size, leaving a
         * non-zero value if the size is not evenly divisible by 8.
         *
         * Thus, OR-ing this value into the header word forces the check to
         * fail when we have a non-binary bitstring. */
        ERTS_CT_ASSERT((UWORD_CONSTANT(7) << (32 - 3)) > _BITSTRING_TAG_MASK);
        a.orr(ARG2, ARG2, ARG3, arm::lsl(32 - 3));
        a.cmp(ARG2, imm(_TAG_HEADER_HEAP_BITS));
    }

    a.b_ne(resolve_beam_label(Fail, disp32MB));
}

void BeamModuleAssembler::emit_is_float(const ArgLabel &Fail,
                                        const ArgSource &Src) {
    auto src = load_source(Src, TMP);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Float) {
        comment("skipped header test since we know it's a float when boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));

        a.cmp(TMP, imm(HEADER_FLONUM));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
    }
}

void BeamModuleAssembler::emit_is_function(const ArgLabel &Fail,
                                           const ArgRegister &Src) {
    auto src = load_source(Src, TMP);
    auto fail = resolve_beam_label(Fail, disp32MB);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Fun) {
        comment("skipped header test since we know it's a fun when boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(VAR, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));
        mov_imm(VAR, 0xFF);
        a.and_(TMP, TMP, VAR);
        a.cmp(TMP, imm(FUN_SUBTAG));
        a.b_ne(fail);
    }
}

void BeamModuleAssembler::emit_is_function2(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgSource &Arity) {
    auto fail = resolve_beam_label(Fail, disp32MB);

    if (!Arity.isSmall()) {
        /* Non-small arity - uncommon; defer to runtime helper. */
        mov_arg(ARG2, Src);
        mov_arg(ARG3, Arity);

        emit_enter_runtime();
        a.mov(ARG1, c_p);
        runtime_call<3>(erl_is_function);
        emit_leave_runtime();

        mov_imm(TMP, am_true);
        a.cmp(ARG1, TMP);
        a.b_ne(fail);
        return;
    }

    unsigned arity = Arity.as<ArgSmall>().getUnsigned();
    if (arity > MAX_ARG) {
        a.b(fail);
        return;
    }

    auto src = load_source(Src, ARG1);
    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    a32::Gp boxed_ptr = emit_ptr_val(VAR, src.reg);

    /* Combined header word and arity check (lowest 16 bits). */
    a.ldr(TMP, emit_boxed_val(boxed_ptr));
    mov_imm(VAR, 0xFFFF);
    a.and_(TMP, TMP, VAR);
    mov_imm(VAR, MAKE_FUN_HEADER(arity, 0, 0) & 0xFFFF);
    a.cmp(TMP, VAR);
    a.b_ne(fail);
}

void BeamModuleAssembler::emit_is_integer(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    auto src = load_source(Src, VAR);

    if (always_immediate(Src)) {
        comment("skipped test for boxed since the value is always immediate");
        a.and_(TMP, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
        a.b_ne(resolve_beam_label(Fail, disp32MB));

        return;
    }

    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified small test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(TMP, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);

        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, TMP);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Integer) {
        comment("skipped header test since we know it's a bignum when "
                "boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));

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
        a.and_(TMP, TMP, imm(mask));
        a.cmp(TMP, imm(_TAG_HEADER_POS_BIG));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
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
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));
        a.and_(TMP, TMP, imm(_TAG_HEADER_MASK));
        a.cmp(TMP, imm(_TAG_HEADER_MAP));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
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
            a.b_ne(resolve_beam_label(Fail, disp32MB));
        });
    }
}

void BeamModuleAssembler::emit_is_number(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    auto src = load_source(Src, TMP);
    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified small test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(VAR, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(VAR, imm(_TAG_IMMED1_SMALL));
        a.b_eq(next);

        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Number) {
        comment("skipped header test since we know it's a number when boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));

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
        a.and_(VAR, TMP, imm(mask));
        a.cmp(VAR, imm(_TAG_HEADER_POS_BIG));
        a.b_eq(next); // accept positive bignum

        mov_imm(VAR, HEADER_FLONUM);
        a.cmp(TMP, VAR);
        a.b_ne(resolve_beam_label(Fail, disp32MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_pid(const ArgLabel &Fail,
                                      const ArgSource &Src) {
    auto src = load_source(Src, TMP);
    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Pid, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified local pid test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(VAR, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(VAR, imm(_TAG_IMMED1_PID));
        a.b_eq(next);

        /* Reuse VAR as the important bits are still available. */
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, VAR);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Pid) {
        comment("skipped header test since we know it's a pid when boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(VAR, emit_boxed_val(boxed_ptr));
        a.and_(VAR, VAR, imm(_TAG_HEADER_MASK));
        a.cmp(VAR, imm(_TAG_HEADER_EXTERNAL_PID));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_port(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    auto src = load_source(Src, TMP);
    Label next = a.newLabel();

    if (always_one_of<BeamTypeId::Port, BeamTypeId::AlwaysBoxed>(Src)) {
        comment("simplified local port test since all other types are boxed");
        emit_is_boxed(next, Src, src.reg);
    } else {
        a.and_(VAR, src.reg, imm(_TAG_IMMED1_MASK));
        a.cmp(VAR, imm(_TAG_IMMED1_PORT));
        a.b_eq(next);

        /* Reuse VAR as the important bits are still available. */
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, VAR);
    }

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Port) {
        comment("skipped header test since we know it's a port when boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(VAR, emit_boxed_val(boxed_ptr));
        a.and_(VAR, VAR, imm(_TAG_HEADER_MASK));
        a.cmp(VAR, imm(_TAG_HEADER_EXTERNAL_PORT));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_reference(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    auto src = load_source(Src, TMP);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), Src, src.reg);

    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Reference) {
        comment("skipped header test since we know it's a ref when boxed");
    } else {
        a32::Gp boxed_ptr = emit_ptr_val(TMP, src.reg);
        a.ldr(TMP, emit_boxed_val(boxed_ptr));
        a.and_(TMP, TMP, imm(_TAG_HEADER_MASK));
        Label is_ref = a.newLabel();
        a.cmp(TMP, imm(_TAG_HEADER_EXTERNAL_REF));
        a.b_eq(is_ref);
        a.cmp(TMP, imm(_TAG_HEADER_REF));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
        a.bind(is_ref);
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
    a.ldmia(arm::Mem(ARG1), a32::GpList({ARG2, ARG3}));

    cmp_arg(ARG3, Tag);
    a.b_ne(resolve_beam_label(Fail, disp32MB));
    mov_imm(TMP, Arity.get());
    a.cmp(ARG2, TMP);
    a.b_ne(resolve_beam_label(Fail, disp32MB));
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
    a.ldmia(arm::Mem(ARG1), a32::GpList({ARG3, ARG4}));

    cmp_arg(ARG3, Arity);
    a.b_eq(correct_arity);

    /* Not a tuple or the wrong arity. Decide which. */
    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.tst(ARG3, imm(_TAG_HEADER_MASK));
    a.b_eq(resolve_beam_label(NotRecord, disp32MB));
    a.b(resolve_beam_label(NotTuple, disp32MB));

    a.bind(correct_arity);
    {
        cmp_arg(ARG4, Tag);
        a.b_ne(resolve_beam_label(NotRecord, disp32MB));
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
        a.ldr(TMP, arm::Mem(ARG1));
        ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
        a.tst(TMP, imm(_TAG_HEADER_MASK));
        a.b_ne(resolve_beam_label(Fail, disp32MB));
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

    a.ldr(TMP, arm::Mem(ARG1));
    cmp_arg(TMP, Arity);
    a.b_ne(resolve_beam_label(Fail, disp32MB));
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

    a.ldr(TMP, arm::Mem(ARG1));

    /* As an optimization for the `error | {ok, Value}` case, skip checking the
     * header word when we know that the only possible boxed type is a tuple. */
    if (masked_types<BeamTypeId::MaybeBoxed>(Src) == BeamTypeId::Tuple) {
        comment("skipped header test since we know it's a tuple when boxed");
    } else {
        ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
        a.tst(TMP, imm(_TAG_HEADER_MASK));
        a.b_ne(resolve_beam_label(NotTuple, disp32MB));
    }

    cmp_arg(TMP, Arity);
    a.b_ne(resolve_beam_label(BadArity, disp32MB));
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_test_arity(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgWord &Arity) {
    auto src = load_source(Src, ARG1);
    emit_untag_ptr(ARG1, src.reg);

    a.ldr(TMP, arm::Mem(ARG1));
    cmp_arg(TMP, Arity);
    a.b_ne(resolve_beam_label(Fail, disp32MB));
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
    a.ldmia(arm::Mem(ARG1), a32::GpList({TMP, ARG1}));
    a.ldmia(arm::Mem(ARG2), a32::GpList({ARG3, ARG2}));
    a.cmp(TMP, ARG3);
    a.b_ne(done);

    a.bind(mid);
    a.cmp(ARG1, ARG2);
    a.b_eq(done);

    /* If not equal, both terms must be CONSes. */
#if !defined(DEBUG)
    ERTS_CT_ASSERT(!is_list(make_small(0) | make_list(0)));
    ERTS_CT_ASSERT(!is_list(make_boxed(0) | make_list(0)));
#endif
    a.orr(TMP, ARG1, ARG2);
    emit_is_not_cons(loop, TMP);

    /* Not equal. Clear Z flag. */
    a.cmp(TMP, imm(0));

    a.bind(done);
    a.bx(a32::lr);
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

    a.orr(TMP, ARG1, ARG2);
    emit_is_boxed(not_equal, TMP);

    emit_untag_ptr(ARG1, ARG1);
    a.ldr(TMP, arm::Mem(ARG1));
    emit_untag_ptr(ARG2, ARG2);
    a.lsr(ARG3, TMP, imm(_HEADER_ARITY_OFFS));
    a.sub(ARG3, ARG3, imm(1));

    a.bind(loop);
    {
        a.ldr(TMP, arm::Mem(ARG1).post(sizeof(Eterm)));
        a.ldr(ARG4, arm::Mem(ARG1).post(sizeof(Eterm)));
        a.ldr(VAR, arm::Mem(ARG2).post(sizeof(Eterm)));
        a.cmp(TMP, VAR);
        a.b_ne(done);
        a.ldr(VAR, arm::Mem(ARG2).post(sizeof(Eterm)));
        a.cmp(ARG4, VAR);
        a.b_ne(done);

        a.subs(ARG3, ARG3, imm(2));
        a.b_pl(loop);
    }

    a.cmn(ARG3, imm(2));
    a.b_eq(done);

    a.ldr(TMP, arm::Mem(ARG1));
    a.ldr(VAR, arm::Mem(ARG2));
    a.cmp(TMP, VAR);

    a.bind(done);
    a.bx(a32::lr);

    a.bind(not_equal);
    a.cmp(TMP, imm(0));
    a.bx(a32::lr);
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
            a32::Gp cons_ptr;
             
            comment("inlined equality test with %T", literal);
            if (!exact_type<BeamTypeId::Cons>(X)) {
                emit_is_cons(resolve_beam_label(Fail, dispUnknown), x.reg);
            }
            cons_ptr = emit_ptr_val(TMP, x.reg);
            a.sub(TMP, cons_ptr, imm(TAG_PRIMARY_LIST));
            a.ldmia(arm::Mem(TMP), a32::GpList({ARG1, ARG2}));
            mov_imm(TMP, CAR(list_val(literal)));
            a.cmp(ARG1, TMP);
            a.b_ne(resolve_beam_label(Fail, disp32MB));
            a.cmp(ARG2, imm(NIL));
            a.b_ne(resolve_beam_label(Fail, disp32MB));

            return;
        } else if (beam_jit_is_list_of_immediates(literal)) {
            comment("optimized equality test with %T", literal);
            mov_var(ARG1, x);
            mov_arg(ARG2, Y);
            fragment_call(ga->get_is_eq_exact_list_shared());
            a.b_ne(resolve_beam_label(Fail, disp32MB));

            return;
        } else if (beam_jit_is_shallow_boxed(literal)) {
            comment("optimized equality test with %T", literal);
            mov_var(ARG1, x);
            mov_arg(ARG2, Y);
            fragment_call(ga->get_is_eq_exact_shallow_boxed_shared());
            a.b_ne(resolve_beam_label(Fail, disp32MB));

            return;
        } else if (is_bitstring(literal) && bitstring_size(literal) == 0) {
            Label not_sub_bits = a.newLabel();

            comment("simplified non-equality test with empty binary");
            emit_is_boxed(resolve_beam_label(Fail, dispUnknown), x.reg);
            emit_untag_ptr(ARG1, x.reg);

            ERTS_CT_ASSERT_FIELD_PAIR(ErlHeapBits, thing_word, size);
            a.ldr(TMP, arm::Mem(ARG1, 0));
            a.ldr(ARG2, arm::Mem(ARG1, sizeof(Eterm)));

            a.cmp(TMP, imm(HEADER_SUB_BITS));
            a.b_ne(not_sub_bits);
            {
                ERTS_CT_ASSERT_FIELD_PAIR(ErlSubBits, start, end);
                a.ldr(ARG2, arm::Mem(ARG1, offsetof(ErlSubBits, start)));
                a.ldr(ARG3, arm::Mem(ARG1, offsetof(ErlSubBits, end)));
                a.sub(ARG2, ARG3, ARG2);
            }
            a.bind(not_sub_bits);

            if (masked_types<BeamTypeId::MaybeBoxed>(X) ==
                BeamTypeId::Bitstring) {
                comment("skipped header test since we know it's a bitstring "
                        "when boxed");
                mov_imm(TMP, 0);
                a.cmp(ARG2, TMP);
                a.b_ne(resolve_beam_label(Fail, disp32MB));
            } else {
                const auto mask = _BITSTRING_TAG_MASK & ~_TAG_PRIMARY_MASK;
                ERTS_CT_ASSERT(TAG_PRIMARY_HEADER == 0);
                ERTS_CT_ASSERT(_TAG_HEADER_HEAP_BITS ==
                               (_TAG_HEADER_HEAP_BITS & mask));
                a.and_(TMP, TMP, imm(mask));
                a.cmp(TMP, imm(_TAG_HEADER_HEAP_BITS));
                a.b_ne(resolve_beam_label(Fail, disp32MB));
                a.cmp(ARG2, imm(0));
                a.b_ne(resolve_beam_label(Fail, disp32MB));
            }

            return;
        } else if (is_map(literal) && erts_map_size(literal) == 0) {
            comment("optimized equality test with empty map");
            emit_is_boxed(resolve_beam_label(Fail, dispUnknown), x.reg);
            emit_untag_ptr(ARG1, x.reg);
            a.ldr(TMP, arm::Mem(ARG1, 0));
            a.ldr(ARG2, arm::Mem(ARG1, sizeof(Eterm)));
            mov_imm(ARG3, MAP_HEADER_FLATMAP);
            a.cmp(TMP, ARG3);
            a.b_ne(resolve_beam_label(Fail, disp32MB));
            a.cmp(ARG2, imm(0));
            a.b_ne(resolve_beam_label(Fail, disp32MB));

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
            a.b_ne(resolve_beam_label(Fail, disp32MB));
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
        a.orr(TMP, x.reg, y.reg);
        emit_is_boxed(resolve_beam_label(Fail, dispUnknown), TMP);
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
        a.b_ne(resolve_beam_label(Fail, disp32MB));
    } else {
        emit_enter_runtime();
        runtime_call<2>(eq);
        emit_leave_runtime();
        mov_imm(TMP, 0);
        a.cmp(ARG1, TMP);
        a.b_eq(resolve_beam_label(Fail, disp32MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne_exact(const ArgLabel &Fail,
                                           const ArgSource &X,
                                           const ArgSource &Y) {
    auto x = load_source(X, ARG1);

    /* If either argument is known to be an immediate, a direct term compare is
     * sufficient for exact non-equality. */
    if (always_immediate(X) || always_immediate(Y)) {
        if (!X.isImmed() && !Y.isImmed()) {
            comment("simplified check since one argument is an immediate");
        }

        preserve_cache([&]() {
            cmp_arg(x.reg, Y);
            a.b_eq(resolve_beam_label(Fail, disp32MB));
        });

        return;
    }

    auto y = load_source(Y, ARG2);

    /* Pointer/term identity implies exact equality. */
    a.cmp(x.reg, y.reg);
    a.b_eq(resolve_beam_label(Fail, disp32MB));

    /* Values differ by identity; use deep exact comparison for boxed terms. */
    mov_var(ARG1, x);
    mov_var(ARG2, y);

    if (always_one_of<BeamTypeId::Integer, BeamTypeId::Float>(X) ||
        always_one_of<BeamTypeId::Integer, BeamTypeId::Float>(Y)) {
        fragment_call(ga->get_is_eq_exact_shallow_boxed_shared());
        a.b_eq(resolve_beam_label(Fail, disp32MB));
    } else {
        emit_enter_runtime();
        runtime_call<2>(eq);
        emit_leave_runtime();
        mov_imm(TMP, 0);
        a.cmp(ARG1, TMP);
        a.b_ne(resolve_beam_label(Fail, disp32MB));
    }
}

void BeamModuleAssembler::emit_is_eq(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    Label next = a.newLabel();

    mov_arg(ARG2, Y); /* May clobber ARG1. */
    mov_arg(ARG1, X);

    /* Pointer/term identity implies equality for '=='. */
    a.cmp(ARG1, ARG2);
    a.b_eq(next);

    emit_enter_runtime();
    /* Arithmetic equality compare (==): 0 means equal. */
    mov_imm(ARG3, 0);
    mov_imm(ARG4, 1);
    runtime_call<4>(erts_cmp_compound);
    emit_leave_runtime();

    /* erts_cmp_compound(..., eq_only=1) returns 0 when equal. */
    mov_imm(TMP, 0);
    a.cmp(ARG1, TMP);
    a.b_ne(resolve_beam_label(Fail, disp32MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    mov_arg(ARG2, Y); /* May clobber ARG1. */
    mov_arg(ARG1, X);

    /* Pointer/term identity implies equality, so '/=' fails. */
    a.cmp(ARG1, ARG2);
    a.b_eq(resolve_beam_label(Fail, disp32MB));

    emit_enter_runtime();
    /* Arithmetic equality compare (/=): 0 means equal and must fail. */
    mov_imm(ARG3, 0);
    mov_imm(ARG4, 1);
    runtime_call<4>(erts_cmp_compound);
    emit_leave_runtime();

    mov_imm(TMP, 0);
    a.cmp(ARG1, TMP);
    a.b_eq(resolve_beam_label(Fail, disp32MB));
}

/*
 * ARG1 = LHS
 * ARG2 = RHS
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_arith_compare_shared() {
    Label generic_compare = a.newLabel();

    emit_enter_runtime_frame();

    /* Are both atoms? */
    a.and_(TMP, ARG1, imm(_TAG_IMMED2_MASK));
    a.cmp(TMP, imm(_TAG_IMMED2_ATOM));
    a.b_ne(generic_compare);
    a.and_(TMP, ARG2, imm(_TAG_IMMED2_MASK));
    a.cmp(TMP, imm(_TAG_IMMED2_ATOM));
    a.b_ne(generic_compare);

    emit_enter_runtime();
    runtime_call<2>(erts_cmp_atoms);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    /* erts_cmp_atoms returns int. */
    a.cmp(ARG1, imm(0));
    a.bx(a32::lr);

    a.bind(generic_compare);
    emit_enter_runtime();
    comment("erts_cmp_compound(X, Y, 0, 0);");
    mov_imm(ARG3, 0);
    mov_imm(ARG4, 0);
    runtime_call<4>(erts_cmp_compound);
    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.cmp(ARG1, imm(0));
    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_is_lt(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    Label generic = a.newLabel(), done = a.newLabel();
    const bool both_small = always_small(LHS) && always_small(RHS);

    mov_arg(ARG2, RHS); /* May clobber ARG1. */
    mov_arg(ARG1, LHS);

    if (!both_small) {
        /* Fast path: tagged compare is valid only when both are small. */
        a.and_(TMP, ARG1, ARG2);
        a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);
    }

    /* LHS < RHS succeeds, so fail on LHS >= RHS. */
    a.cmp(ARG1, ARG2);
    a.b_ge(resolve_beam_label(Fail, disp32MB));
    a.b(done);

    if (!both_small) {
        a.bind(generic);
        fragment_call(ga->get_arith_compare_shared());
        a.b_ge(resolve_beam_label(Fail, disp32MB));
    } else {
        a.bind(generic);
    }

    a.bind(done);
}

void BeamModuleAssembler::emit_is_ge(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    Label generic = a.newLabel(), done = a.newLabel();
    const bool both_small = always_small(LHS) && always_small(RHS);

    mov_arg(ARG2, RHS); /* May clobber ARG1. */
    mov_arg(ARG1, LHS);

    if (!both_small) {
        /* Fast path: tagged compare is valid only when both are small. */
        a.and_(TMP, ARG1, ARG2);
        a.and_(TMP, TMP, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP, imm(_TAG_IMMED1_SMALL));
        a.b_ne(generic);
    }

    /* LHS >= RHS succeeds, so fail on LHS < RHS. */
    a.cmp(ARG1, ARG2);
    a.b_lt(resolve_beam_label(Fail, disp32MB));
    a.b(done);

    if (!both_small) {
        a.bind(generic);
        fragment_call(ga->get_arith_compare_shared());
        a.b_lt(resolve_beam_label(Fail, disp32MB));
    } else {
        a.bind(generic);
    }

    a.bind(done);
}

/*
 * ARG1 = Src
 * ARG2 = Min
 * ARG3 = Max
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_is_in_range_shared() {
    // TODO
    emit_nyi("emit_is_in_range_shared");
}

/*
 * 1121 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_in_range(ArgLabel const &Small,
                                           ArgLabel const &Large,
                                           ArgRegister const &Src,
                                           ArgConstant const &Min,
                                           ArgConstant const &Max) {
    // TODO
    emit_nyi("emit_is_in_range");
}

/*
 * ARG1 = Src
 * ARG2 = A
 * ARG3 = B
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_is_ge_lt_shared() {
    // TODO
    emit_nyi("emit_is_ge_lt_shared");
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
    // TODO
    emit_nyi("emit_is_ge_lt");
}

/*
 * 1190 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_ge_ge(ArgLabel const &Fail1,
                                        ArgLabel const &Fail2,
                                        ArgRegister const &Src,
                                        ArgConstant const &A,
                                        ArgConstant const &B) {
    // TODO
    emit_nyi("emit_is_ge_ge");
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
    // TODO
    emit_nyi("emit_is_int_in_range");
}

/*
 * 428 occurrences in OTP at the time of writing.
 */
void BeamModuleAssembler::emit_is_int_ge(ArgLabel const &Fail,
                                         ArgRegister const &Src,
                                         ArgConstant const &Min) {
    // TODO
    emit_nyi("emit_is_int_ge");
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
    // TODO
    emit_nyi("emit_system_limit_body");
}

void BeamModuleAssembler::emit_if_end() {
    emit_error(EXC_IF_CLAUSE);
    mark_unreachable();
}

void BeamModuleAssembler::emit_badrecord(const ArgSource &Src) {
    // TODO
    emit_nyi("emit_badrecord");
}

void BeamModuleAssembler::emit_catch(const ArgYRegister &Y,
                                     const ArgCatch &Handler) {
    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, catches)));
    a.add(TMP, TMP, imm(1));
    a.str(TMP, arm::Mem(c_p, offsetof(Process, catches)));

    mov_arg(Y, Handler);
}

void BeamGlobalAssembler::emit_catch_end_shared() {
    Label not_throw = a.newLabel(), not_error = a.newLabel(),
          after_gc = a.newLabel();

    /* X0 = THE_NON_VALUE
     * X1 = error reason/thrown value
     * X2 = raw stacktrace.
     * X3 = class
     */
    a.ldr(ARG1, getXRef(1));
    a.ldr(TMP, getXRef(3));
    mov_imm(ARG2, am_throw);
    a.cmp(TMP, ARG2);
    a.b_ne(not_throw);

    /* Thrown value: return it in X0. */
    a.str(ARG1, getXRef(0));
    a.bx(a32::lr);

    a.bind(not_throw);
    {
        emit_enter_runtime_frame();

        a.ldr(TMP, getXRef(3));
        mov_imm(ARG2, am_error);
        a.cmp(TMP, ARG2);
        a.b_ne(not_error);

        /* Attach stacktrace for error class. */
        a.mov(ARG2, ARG1);
        a.ldr(ARG3, getXRef(2));

        emit_enter_runtime<Update::eHeapAlloc>();
        a.mov(ARG1, c_p);
        runtime_call<3>(add_stacktrace);
        emit_leave_runtime<Update::eHeapAlloc>();
    }

    /* Error term from exit/1 or stacktrace-attached reason from error. */
    a.bind(not_error);
    {
        const int32_t bytes_needed = (3 + S_RESERVED) * sizeof(Eterm);
        add(ARG3, HTOP, bytes_needed);
        a.cmp(ARG3, E);
        a.b_ls(after_gc);
        {
            /* Preserve reason/stacktrace term across GC. */
            a.str(ARG1, TMP_MEM1q);
            mov_imm(ARG4, 1);
            a.bl(labels[garbage_collect]);
            a.ldr(ARG1, TMP_MEM1q);
        }
        a.bind(after_gc);

        a.add(ARG3, HTOP, imm(TAG_PRIMARY_BOXED));
        mov_imm(TMP, make_arityval(2));
        a.str(TMP, arm::Mem(HTOP).post(sizeof(Eterm)));
        mov_imm(TMP, am_EXIT);
        a.str(TMP, arm::Mem(HTOP).post(sizeof(Eterm)));
        a.str(ARG1, arm::Mem(HTOP).post(sizeof(Eterm)));

        a.mov(ARG1, ARG3);
        a.str(ARG1, getXRef(0));
    }

    emit_leave_runtime_frame();
    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_catch_end(const ArgYRegister &CatchTag) {
    Label next = a.newLabel();

    emit_try_end(CatchTag);
    a.ldr(TMP, getXRef(0));
    emit_branch_if_value(TMP, next);
    fragment_call(ga->get_catch_end_shared());
    a.bind(next);
}

void BeamModuleAssembler::emit_try_end(const ArgYRegister &CatchTag) {
    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP, TMP, imm(1));
    a.str(TMP, arm::Mem(c_p, offsetof(Process, catches)));
    mov_imm(TMP, NIL);
    a.str(TMP, getArgRef(CatchTag));
}

void BeamModuleAssembler::emit_try_end_deallocate(const ArgWord &Deallocate) {
    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP, TMP, imm(1));
    a.str(TMP, arm::Mem(c_p, offsetof(Process, catches)));
    if (Deallocate.get() > 0) {
        add(E, E, Deallocate.get() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_try_end_move_deallocate(
        const ArgSource &Src,
        const ArgRegister &Dst,
        const ArgWord &Deallocate) {
    // TODO
    emit_nyi("emit_try_end_move_deallocate");
}

void BeamModuleAssembler::emit_try_case(const ArgYRegister &CatchTag) {
    (void)CatchTag;
    /* x0 = THE_NON_VALUE
     * x1 = error reason/thrown value
     * x2 = raw stacktrace
     * x3 = class */
    a.ldr(TMP, arm::Mem(c_p, offsetof(Process, catches)));
    a.ldr(ARG1, getXRef(3));
    a.str(ARG1, getXRef(0));
    a.sub(TMP, TMP, imm(1));
    a.str(TMP, arm::Mem(c_p, offsetof(Process, catches)));

    /* The try_tag in the Y slot in the stack frame has already been
     * cleared. */

#ifdef DEBUG
    {
        Label ok = a.newLabel();
        Label bad = a.newLabel();
        comment("Start of assertion code");
        a.ldr(ARG1, arm::Mem(c_p, offsetof(Process, fvalue)));
        a.ldr(ARG2, arm::Mem(c_p, offsetof(Process, ftrace)));
        mov_imm(TMP, NIL);

        a.cmp(ARG1, TMP);
        a.b_ne(bad);
        a.cmp(ARG2, TMP);
        a.b_eq(ok);

        a.bind(bad);
        comment("Assertion c_p->fvalue == NIL && c_p->ftrace == NIL failed");
        a.udf(0x42);
        a.bind(ok);
    }
#endif
}

void BeamModuleAssembler::emit_try_case_end(const ArgSource &Src) {
    emit_error(EXC_TRY_CLAUSE, Src);
}

void BeamGlobalAssembler::emit_raise_shared() {
    a.str(ARG1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, ftrace)));

    emit_enter_runtime();
    a.mov(ARG1, c_p);
    runtime_call<2>(erts_sanitize_freason);
    emit_leave_runtime();

    mov_imm(ARG4, 0);
    a.mov(ARG2, a32::lr);
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
    a.ldr(ARG2, getXRef(0));

    emit_enter_runtime<Update::eHeapAlloc>();
    a.mov(ARG1, c_p);
    runtime_call<2>(build_stacktrace);
    emit_leave_runtime<Update::eHeapAlloc>();

    a.str(ARG1, getXRef(0));
}

/* This instruction has the same semantics as the erlang:raise/3 BIF,
 * except that it can rethrow a raw stack backtrace. */
void BeamModuleAssembler::emit_raw_raise() {
    Label next = a.newLabel();

    a.ldr(ARG1, getXRef(2));
    a.ldr(ARG2, getXRef(0));
    a.ldr(ARG3, getXRef(1));
    a.mov(ARG4, c_p);

    emit_enter_runtime();
    runtime_call<4>(raw_raise);
    emit_leave_runtime();

    a.tst(ARG1, ARG1);
    a.b_ne(next);

    emit_raise_exception();

    a.bind(next);
    mov_imm(ARG1, am_badarg);
    a.str(ARG1, getXRef(0));
}

#define TEST_YIELD_RETURN_OFFSET                                               \
    (BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(Uint32[3]) +                         \
     (erts_alcu_enable_code_atags ? sizeof(Uint32) : 0))

/* ARG3 = current_label */
void BeamGlobalAssembler::emit_i_test_yield_shared() {
    a.sub(ARG2, ARG3, imm(sizeof(ErtsCodeMFA)));
    a.add(ARG3, ARG3, imm(TEST_YIELD_RETURN_OFFSET));

    a.str(ARG2, arm::Mem(c_p, offsetof(Process, current)));
    a.ldr(ARG2, arm::Mem(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.strb(ARG2, arm::Mem(c_p, offsetof(Process, arity)));

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
    a.b_le(resolve_fragment(ga->get_i_test_yield_shared(), disp32MB));

    ASSERT((a.offset() - code.labelOffsetFromBase(current_label)) ==
           TEST_YIELD_RETURN_OFFSET);
}

void BeamModuleAssembler::emit_i_yield() {
    mov_imm(ARG1, am_true);
    mov_arg(ArgXRegister(0), ARG1);
    fragment_call(ga->get_dispatch_return());
}

void BeamModuleAssembler::emit_i_perf_counter() {
    // TODO
    emit_nyi("emit_i_perf_counter");
}

void BeamModuleAssembler::emit_mark_unreachable() {
    // TODO
    emit_nyi("emit_mark_unreachable");
}

void BeamModuleAssembler::emit_coverage(void *coverage, Uint index, Uint size) {
    // TODO
    emit_nyi("emit_coverage");
}
