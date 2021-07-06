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

/*
 * Some notes on how to minimize the code size.
 *
 * Instructions that use 32-bit registers (e.g. eax) are generally
 * one byte shorter than instructions that use 64-bits registers
 * (e.g. rax). This does not apply to registers r8-r15 beacuse they'll
 * always need a rex prefix. The `and`, `or`, and `cmp` instructions
 * are even shorter than operating on the RETb (al) register. The
 * `test` instruction with an immediate second operand is shorter
 * when operating on an 8-bit register.
 *
 * On both Unix and Windows, instructions can be shortened by using
 * RETd, ARG1d, or ARG2d instead of RET, ARG1, or ARG2, respectively.
 * On Unix, but not on Windows, ARG3d and ARG4d will also result in
 * shorter instructions.
 *
 * Here are some examples. If we know that the higher 32 bits of
 * a register is uninteresting or should be zeroed, we can write:
 *
 *   a.mov(RETd, ARG1d)
 *
 * (When writing to the lower 32 bits of a register, the high 32
 * bits are zeroed.)
 *
 * Here is a tag test on the contents of ARG1:
 *
 *   a.and_(ARG1d, 15)
 *   a.cmp(ARG1d, 15)
 *
 * The same tag test on RET can be even shorter if written like this:
 *
 *   a.and_(RETb, 15)
 *   a.cmp(RETb, 15)
 *
 * An alignment test can be written like this (when unit <= 256):
 *
 *   a.test(RETb, imm(unit - 1));
 *   a.test(ARG1.r8(), imm(unit -1));
 *
 * ASMJIT will automatically encode backward jumps (jumps to bound
 * labels) in the shortest form possible. However, forward jumps
 * (jumps to unbound labels) will by default be encoded in the long
 * form (using a 32-bit relative address).
 *
 * Within a single BEAM instruction, a `short_()` prefix can be used
 * to emit short forward jumps (using a signed byte as an offset,
 * limiting the distance to about 128 bytes).
 *
 * Example:
 *
 *   a.short_().je(next);
 *       .
 *       .
 *       .
 *   a.bind(next);
 */

#include <algorithm>
#include "beam_asm.hpp"

extern "C"
{
#include "erl_bif_table.h"
#include "big.h"
#include "beam_catches.h"
#include "beam_common.h"
#include "code_ix.h"
}

using namespace asmjit;

/* Helpers */

void BeamModuleAssembler::emit_error(int reason) {
    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(reason));
    emit_raise_exception();
}

void BeamModuleAssembler::emit_gc_test_preserve(const ArgVal &Need,
                                                const ArgVal &Live,
                                                x86::Gp term) {
    const int32_t bytes_needed = (Need.getValue() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

    ASSERT(term != ARG3);

    a.lea(ARG3, x86::qword_ptr(HTOP, bytes_needed));
    a.cmp(ARG3, E);
    a.short_().jbe(after_gc_check);

    a.mov(getXRef(Live.getValue()), term);
    mov_imm(ARG4, Live.getValue() + 1);
    fragment_call(ga->get_garbage_collect());
    a.mov(term, getXRef(Live.getValue()));

    a.bind(after_gc_check);
}

void BeamModuleAssembler::emit_gc_test(const ArgVal &Ns,
                                       const ArgVal &Nh,
                                       const ArgVal &Live) {
    const int32_t bytes_needed =
            (Ns.getValue() + Nh.getValue() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

    a.lea(ARG3, x86::qword_ptr(HTOP, bytes_needed));
    a.cmp(ARG3, E);
    a.short_().jbe(after_gc_check);

    mov_imm(ARG4, Live.getValue());

    fragment_call(ga->get_garbage_collect());
    a.bind(after_gc_check);
}

void BeamModuleAssembler::emit_validate(const ArgVal &arity) {
#ifdef DEBUG
    Label next = a.newLabel(), crash = a.newLabel();

    /* Crash if the Erlang heap is not word-aligned */
    a.test(HTOP, imm(sizeof(Eterm) - 1));
    a.jne(crash);

    /* Crash if the Erlang stack is not word-aligned */
    a.test(E, imm(sizeof(Eterm) - 1));
    a.jne(crash);

    /* Crash if we've overrun the stack */
    a.lea(ARG1, x86::qword_ptr(E, -(int32_t)(S_REDZONE * sizeof(Eterm))));
    a.cmp(HTOP, ARG1);
    a.ja(crash);

    a.jmp(next);
    a.bind(crash);
    a.hlt();
    a.bind(next);

#    ifdef JIT_HARD_DEBUG
    emit_enter_runtime();

    for (unsigned i = 0; i < arity.getValue(); i++) {
        a.mov(ARG1, getXRef(i));
        runtime_call<1>(beam_jit_validate_term);
    }

    emit_leave_runtime();
#    endif

#endif
}

/* Instrs */

void BeamModuleAssembler::emit_i_validate(const ArgVal &Arity) {
    emit_validate(Arity);
}

void BeamModuleAssembler::emit_allocate_heap(const ArgVal &NeedStack,
                                             const ArgVal &NeedHeap,
                                             const ArgVal &Live) {
    ASSERT(NeedStack.isWord() && NeedStack.getValue() <= MAX_REG);
    ArgVal needed = NeedStack;

#if !defined(NATIVE_ERLANG_STACK)
    needed = needed + CP_SIZE;
#endif

    emit_gc_test(needed, NeedHeap, Live);

    if (needed.getValue() > 0) {
        a.sub(E, imm(needed.getValue() * sizeof(Eterm)));
    }
#if !defined(NATIVE_ERLANG_STACK)
    a.mov(getCPRef(), imm(NIL));
#endif
}

void BeamModuleAssembler::emit_allocate(const ArgVal &NeedStack,
                                        const ArgVal &Live) {
    emit_allocate_heap(NeedStack, ArgVal(ArgVal::Word, 0), Live);
}

void BeamModuleAssembler::emit_deallocate(const ArgVal &Deallocate) {
    ASSERT(Deallocate.isWord() && Deallocate.getValue() <= 1023);

    if (ERTS_LIKELY(erts_frame_layout == ERTS_FRAME_LAYOUT_RA)) {
        ArgVal dealloc = Deallocate;

#if !defined(NATIVE_ERLANG_STACK)
        dealloc = dealloc + CP_SIZE;
#endif

        if (dealloc.getValue() > 0) {
            a.add(E, imm(dealloc.getValue() * sizeof(Eterm)));
        }
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);
    }
}

void BeamModuleAssembler::emit_test_heap(const ArgVal &Nh, const ArgVal &Live) {
    emit_gc_test(ArgVal(ArgVal::Word, 0), Nh, Live);
}

void BeamModuleAssembler::emit_normal_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_proc_lc_unrequire();

    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), imm(EXC_NORMAL));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), imm(0));
    a.mov(ARG1, c_p);
    mov_imm(ARG2, am_normal);
    runtime_call<2>(erts_do_exit_process);

    emit_proc_lc_require();
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    abs_jmp(ga->get_do_schedule());
}

void BeamModuleAssembler::emit_continue_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_proc_lc_unrequire();

    a.mov(ARG1, c_p);
    runtime_call<1>(erts_continue_exit_process);

    emit_proc_lc_require();
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    abs_jmp(ga->get_do_schedule());
}

void BeamModuleAssembler::emit_get_list(const x86::Gp src,
                                        const ArgVal &Hd,
                                        const ArgVal &Tl) {
    x86::Gp boxed_ptr = emit_ptr_val(src, src);

    switch (ArgVal::register_relation(Hd, Tl)) {
    case ArgVal::Relation::consecutive: {
        comment("(moving head and tail together)");
        x86::Mem dst_ptr = getArgRef(Hd, 16);
        x86::Mem src_ptr = getCARRef(boxed_ptr, 16);
        a.movups(x86::xmm0, src_ptr);
        a.movups(dst_ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::reverse_consecutive: {
        if (!hasCpuFeature(x86::Features::kAVX)) {
            goto fallback;
        }

        comment("(moving and swapping head and tail together)");
        x86::Mem dst_ptr = getArgRef(Tl, 16);
        x86::Mem src_ptr = getCARRef(boxed_ptr, 16);
        a.vpermilpd(x86::xmm0, src_ptr, 1); /* Load and swap */
        a.vmovups(dst_ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::none:
    fallback:
        a.mov(ARG2, getCARRef(boxed_ptr));
        a.mov(ARG3, getCDRRef(boxed_ptr));
        mov_arg(Hd, ARG2);
        mov_arg(Tl, ARG3);
        break;
    }
}

void BeamModuleAssembler::emit_get_list(const ArgVal &Src,
                                        const ArgVal &Hd,
                                        const ArgVal &Tl) {
    mov_arg(ARG1, Src);
    emit_get_list(ARG1, Hd, Tl);
}

void BeamModuleAssembler::emit_get_hd(const ArgVal &Src, const ArgVal &Hd) {
    mov_arg(ARG1, Src);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);

    a.mov(ARG2, getCARRef(boxed_ptr));

    mov_arg(Hd, ARG2);
}

void BeamModuleAssembler::emit_get_tl(const ArgVal &Src, const ArgVal &Tl) {
    mov_arg(ARG1, Src);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);

    a.mov(ARG2, getCDRRef(boxed_ptr));

    mov_arg(Tl, ARG2);
}

void BeamModuleAssembler::emit_is_nonempty_list_get_list(const ArgVal &Fail,
                                                         const ArgVal &Src,
                                                         const ArgVal &Hd,
                                                         const ArgVal &Tl) {
    mov_arg(RET, Src);
    a.test(RETb, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
    a.jne(labels[Fail.getValue()]);
    emit_get_list(RET, Hd, Tl);
}

void BeamModuleAssembler::emit_is_nonempty_list_get_hd(const ArgVal &Fail,
                                                       const ArgVal &Src,
                                                       const ArgVal &Hd) {
    mov_arg(RET, Src);
    a.test(RETb, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
    a.jne(labels[Fail.getValue()]);

    x86::Gp boxed_ptr = emit_ptr_val(RET, RET);

    a.mov(ARG2, getCARRef(boxed_ptr));

    mov_arg(Hd, ARG2);
}

void BeamModuleAssembler::emit_is_nonempty_list_get_tl(const ArgVal &Fail,
                                                       const ArgVal &Src,
                                                       const ArgVal &Tl) {
    mov_arg(RET, Src);
    a.test(RETb, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
    a.jne(labels[Fail.getValue()]);

    x86::Gp boxed_ptr = emit_ptr_val(RET, RET);

    a.mov(ARG2, getCDRRef(boxed_ptr));

    mov_arg(Tl, ARG2);
}

void BeamModuleAssembler::emit_i_get(const ArgVal &Src, const ArgVal &Dst) {
    mov_arg(ARG2, Src);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_pd_hash_get);

    emit_leave_runtime();

    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_i_get_hash(const ArgVal &Src,
                                          const ArgVal &Hash,
                                          const ArgVal &Dst) {
    mov_arg(ARG2, Hash);
    mov_arg(ARG3, Src);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_pd_hash_get_with_hx);

    emit_leave_runtime();

    mov_arg(Dst, RET);
}

/* Store the pointer to a tuple in ARG2. Remove any LITERAL_PTR tag. */
void BeamModuleAssembler::emit_load_tuple_ptr(const ArgVal &Term) {
    mov_arg(ARG2, Term);
    (void)emit_ptr_val(ARG2, ARG2);
}

#ifdef DEBUG
/* Emit an assertion to ensure that tuple_reg points into the same
 * tuple as Src. */
void BeamModuleAssembler::emit_tuple_assertion(const ArgVal &Src,
                                               x86::Gp tuple_reg) {
    Label ok = a.newLabel(), fatal = a.newLabel();
    ASSERT(tuple_reg != RET);
    mov_arg(RET, Src);
    emit_is_boxed(fatal, RET, dShort);
    (void)emit_ptr_val(RET, RET);
    a.cmp(RET, tuple_reg);
    a.short_().je(ok);

    a.bind(fatal);
    {
        a.comment("# Tuple assertion failure");
        a.ud2();
    }
    a.bind(ok);
}
#endif

/* Fetch an element from the tuple pointed to by the boxed pointer
 * in ARG2. */
void BeamModuleAssembler::emit_i_get_tuple_element(const ArgVal &Src,
                                                   const ArgVal &Element,
                                                   const ArgVal &Dst) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG2);
#endif

    a.mov(ARG1, emit_boxed_val(ARG2, Element.getValue()));
    mov_arg(Dst, ARG1);
}

/* Fetch two consecutive tuple elements from the tuple pointed to by
 * the boxed pointer in ARG2. */
void BeamModuleAssembler::emit_get_two_tuple_elements(const ArgVal &Src,
                                                      const ArgVal &Element,
                                                      const ArgVal &Dst1,
                                                      const ArgVal &Dst2) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG2);
#endif

    x86::Mem element_ptr =
            emit_boxed_val(ARG2, Element.getValue(), 2 * sizeof(Eterm));

    switch (ArgVal::register_relation(Dst1, Dst2)) {
    case ArgVal::Relation::consecutive: {
        x86::Mem dst_ptr = getArgRef(Dst1, 16);
        a.movups(x86::xmm0, element_ptr);
        a.movups(dst_ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::reverse_consecutive: {
        if (!hasCpuFeature(x86::Features::kAVX)) {
            goto fallback;
        } else {
            x86::Mem dst_ptr = getArgRef(Dst2, 16);
            a.vpermilpd(x86::xmm0, element_ptr, 1); /* Load and swap */
            a.vmovups(dst_ptr, x86::xmm0);
            break;
        }
    }
    case ArgVal::Relation::none:
    fallback:
        a.mov(ARG1, emit_boxed_val(ARG2, Element.getValue()));
        a.mov(ARG3, emit_boxed_val(ARG2, (Element + sizeof(Eterm)).getValue()));
        mov_arg(Dst1, ARG1);
        mov_arg(Dst2, ARG3);
        break;
    }
}

void BeamModuleAssembler::emit_init(const ArgVal &Y) {
    mov_arg(Y, NIL);
}

void BeamModuleAssembler::emit_init_yregs(const ArgVal &Size,
                                          const Span<ArgVal> &args) {
    unsigned count = Size.getValue();
    ASSERT(count == args.size());

    if (count == 1) {
        mov_arg(args.front(), NIL);
        return;
    }

    /* There at least two slots. */
    unsigned i = 0;
    int y_ptr = -1;

    mov_imm(x86::rax, NIL);

    while (i < count) {
        unsigned slots = 1;
        unsigned first_y = args[i].getValue();

        while (i + slots < count) {
            ArgVal current_y = args[i + slots];
            if (first_y + slots != current_y.getValue()) {
                break;
            }
            slots++;
        }

        /*
         * Now first_y is the number of the first y register to be initialized
         * and slots is the number of y registers to be initialized.
         */

        if (slots == 1) {
            a.mov(getYRef(first_y), x86::rax);
        } else {
            /*
             * There are at least two consecutive y registers to be initialized.
             * Use `stosq` with or without `rep`.
             */
            if (first_y == 0) {
#ifdef NATIVE_ERLANG_STACK
                /* `mov` is two bytes shorter than `lea`. */
                a.mov(x86::rdi, E);
#else
                /* y(0) is at E+8. Must use `lea` here. */
                a.lea(x86::rdi, getYRef(0));
#endif
                y_ptr = 0;
            } else if (y_ptr < 0) {
                /* Initialize rdi for the first time. */
                y_ptr = first_y;
                a.lea(x86::rdi, getYRef(y_ptr));
            } else {
                /* Update rdi using `add`. This is one byte shorter than using
                 * `lea`. */
                unsigned offset = (first_y - y_ptr) * sizeof(Eterm);
                a.add(x86::rdi, imm(offset));
                y_ptr = first_y;
            }

            if (slots <= 4) {
                /* Slightly more compact than `rep stosq`. */
                for (unsigned j = 0; j < slots; j++) {
                    a.stosq();
                }
            } else {
                mov_imm(x86::rcx, slots);
                a.rep().stosq();
            }

            /* Update y_ptr to account for the incrementing done by `stosq`. */
            y_ptr += slots;
        }

        i += slots;
    }
}

void BeamModuleAssembler::emit_i_trim(const ArgVal &Words) {
    ASSERT(Words.isWord() && Words.getValue() <= 1023);

    if (Words.getValue() > 0) {
        a.add(E, imm(Words.getValue() * sizeof(Eterm)));
    }
}

void BeamModuleAssembler::emit_i_move(const ArgVal &Src, const ArgVal &Dst) {
    mov_arg(Dst, Src);
}

/* Move two words at consecutive addresses to consecutive or reverse
 * consecutive destinations. */
void BeamModuleAssembler::emit_move_two_words(const ArgVal &Src1,
                                              const ArgVal &Dst1,
                                              const ArgVal &Src2,
                                              const ArgVal &Dst2) {
    x86::Mem src_ptr = getArgRef(Src1, 16);

    ASSERT(ArgVal::register_relation(Src1, Src2) ==
           ArgVal::Relation::consecutive);

    switch (ArgVal::register_relation(Dst1, Dst2)) {
    case ArgVal::Relation::consecutive: {
        x86::Mem dst_ptr = getArgRef(Dst1, 16);
        a.movups(x86::xmm0, src_ptr);
        a.movups(dst_ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::reverse_consecutive: {
        x86::Mem dst_ptr = getArgRef(Dst2, 16);
        comment("(moving and swapping)");
        if (hasCpuFeature(x86::Features::kAVX)) {
            a.vpermilpd(x86::xmm0, src_ptr, 1); /* Load and swap */
            a.vmovups(dst_ptr, x86::xmm0);
        } else {
            mov_arg(ARG1, Src1);
            mov_arg(ARG2, Src2);
            mov_arg(Dst1, ARG1);
            mov_arg(Dst2, ARG2);
        }
        break;
    }
    case ArgVal::Relation::none:
        ASSERT(0);
        break;
    }
}

void BeamModuleAssembler::emit_swap(const ArgVal &R1, const ArgVal &R2) {
    if (!hasCpuFeature(x86::Features::kAVX)) {
        goto fallback;
    }

    switch (ArgVal::register_relation(R1, R2)) {
    case ArgVal::Relation::consecutive: {
        x86::Mem ptr = getArgRef(R1, 16);
        comment("(swapping using AVX)");
        a.vpermilpd(x86::xmm0, ptr, 1); /* Load and swap */
        a.vmovups(ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::reverse_consecutive: {
        x86::Mem ptr = getArgRef(R2, 16);
        comment("(swapping using AVX)");
        a.vpermilpd(x86::xmm0, ptr, 1); /* Load and swap */
        a.vmovups(ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::none:
    fallback:
        mov_arg(ARG1, R1);
        mov_arg(ARG2, R2);
        mov_arg(R2, ARG1);
        mov_arg(R1, ARG2);
        break;
    }
}

void BeamModuleAssembler::emit_node(const ArgVal &Dst) {
    a.mov(ARG1, imm(&erts_this_node));
    a.mov(ARG1, x86::qword_ptr(ARG1));
    a.mov(ARG1, x86::qword_ptr(ARG1, offsetof(ErlNode, sysname)));
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_put_cons(const ArgVal &Hd, const ArgVal &Tl) {
    switch (ArgVal::register_relation(Hd, Tl)) {
    case ArgVal::Relation::consecutive: {
        x86::Mem src_ptr = getArgRef(Hd, 16);
        x86::Mem dst_ptr = x86::xmmword_ptr(HTOP, 0);
        comment("(put head and tail together)");
        a.movups(x86::xmm0, src_ptr);
        a.movups(dst_ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::reverse_consecutive: {
        if (!hasCpuFeature(x86::Features::kAVX)) {
            goto fallback;
        }

        x86::Mem src_ptr = getArgRef(Tl, 16);
        x86::Mem dst_ptr = x86::xmmword_ptr(HTOP, 0);
        comment("(putting and swapping head and tail together)");
        a.vpermilpd(x86::xmm0, src_ptr, 1); /* Load and swap */
        a.vmovups(dst_ptr, x86::xmm0);
        break;
    }
    case ArgVal::Relation::none:
    fallback:
        mov_arg(x86::qword_ptr(HTOP, 0), Hd);
        mov_arg(x86::qword_ptr(HTOP, 1 * sizeof(Eterm)), Tl);
        break;
    }
    a.lea(ARG2, x86::qword_ptr(HTOP, TAG_PRIMARY_LIST));
}

void BeamModuleAssembler::emit_append_cons(const ArgVal &index,
                                           const ArgVal &Hd) {
    size_t offset = 2 * index.getValue() * sizeof(Eterm);
    mov_arg(x86::qword_ptr(HTOP, offset), Hd);
    a.mov(x86::qword_ptr(HTOP, offset + sizeof(Eterm)), ARG2);
    a.lea(ARG2, x86::qword_ptr(HTOP, offset + TAG_PRIMARY_LIST));
}

void BeamModuleAssembler::emit_store_cons(const ArgVal &len,
                                          const ArgVal &Dst) {
    a.add(HTOP, imm(len.getValue() * 2 * sizeof(Eterm)));
    mov_arg(Dst, ARG2);
}

void BeamModuleAssembler::emit_put_tuple2(const ArgVal &Dst,
                                          const ArgVal &Arity,
                                          const Span<ArgVal> &args) {
    size_t size = args.size();
    ASSERT(arityval(Arity.getValue()) == size);

    comment("Move arity word");
    mov_arg(x86::qword_ptr(HTOP, 0), Arity);

    comment("Move tuple data");
    for (unsigned i = 0; i < size; i++) {
        x86::Mem dst_ptr = x86::qword_ptr(HTOP, (i + 1) * sizeof(Eterm));

        if (i + 1 == size) {
            mov_arg(dst_ptr, args[i]);
        } else {
            switch (ArgVal::register_relation(args[i], args[i + 1])) {
            case ArgVal::consecutive: {
                x86::Mem src_ptr = getArgRef(args[i], 16);

                comment("(moving two elements at once)");
                dst_ptr.setSize(16);
                a.movups(x86::xmm0, src_ptr);
                a.movups(dst_ptr, x86::xmm0);
                i++;
                break;
            }
            case ArgVal::reverse_consecutive: {
                if (!hasCpuFeature(x86::Features::kAVX)) {
                    mov_arg(dst_ptr, args[i]);
                } else {
                    x86::Mem src_ptr = getArgRef(args[i + 1], 16);

                    comment("(moving and swapping two elements at once)");
                    dst_ptr.setSize(16);
                    a.vpermilpd(x86::xmm0, src_ptr, 1); /* Load and swap */
                    a.vmovups(dst_ptr, x86::xmm0);
                    i++;
                }
                break;
            }
            case ArgVal::none:
                mov_arg(dst_ptr, args[i]);
                break;
            }
        }
    }

    comment("Create boxed ptr");
    a.lea(ARG1, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
    a.add(HTOP, imm((size + 1) * sizeof(Eterm)));

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_self(const ArgVal &Dst) {
    a.mov(ARG1, x86::qword_ptr(c_p, offsetof(Process, common.id)));

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_set_tuple_element(const ArgVal &Element,
                                                 const ArgVal &Tuple,
                                                 const ArgVal &Offset) {
    mov_arg(ARG1, Tuple);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    mov_arg(emit_boxed_val(boxed_ptr, Offset.getValue()), Element, ARG2);
}

void BeamModuleAssembler::emit_is_nonempty_list(const ArgVal &Fail,
                                                const ArgVal &Src) {
    x86::Mem list_ptr = getArgRef(Src, 1);

    a.test(list_ptr, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_jump(const ArgVal &Fail) {
    a.jmp(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_atom(const ArgVal &Fail, const ArgVal &Src) {
    mov_arg(RET, Src);
    ERTS_CT_ASSERT(_TAG_IMMED2_MASK < 256);
    a.and_(RETb, imm(_TAG_IMMED2_MASK));
    a.cmp(RETb, imm(_TAG_IMMED2_ATOM));
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_boolean(const ArgVal &Fail,
                                          const ArgVal &Src) {
    /* Since am_true and am_false differ by a single bit, we can simplify the
     * check by clearing said bit and comparing against the lesser one. */
    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    mov_arg(ARG1, Src);

    a.and_(ARG1, imm(~(am_true & ~_TAG_IMMED1_MASK)));
    a.cmp(ARG1, imm(am_false));
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_binary(Label fail,
                                         x86::Gp src,
                                         Label next,
                                         Label subbin) {
    ASSERT(src != RET && src != ARG2);

    emit_is_boxed(fail, src);

    x86::Gp boxed_ptr = emit_ptr_val(src, src);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));

    a.and_(RETb, imm(_TAG_HEADER_MASK));
    a.cmp(RETb, imm(_TAG_HEADER_SUB_BIN));
    a.short_().je(subbin);
    ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
    a.and_(RETb, imm(~4));
    a.cmp(RETb, imm(_TAG_HEADER_REFC_BIN));
    a.short_().je(next);
    a.jmp(fail);
}

void BeamModuleAssembler::emit_is_binary(const ArgVal &Fail,
                                         const ArgVal &Src) {
    Label next = a.newLabel(), subbin = a.newLabel();

    mov_arg(ARG1, Src);

    emit_is_binary(labels[Fail.getValue()], ARG1, next, subbin);

    a.bind(subbin);
    {
        /* emit_is_binary has already removed the literal tag from Src, if
         * applicable. */
        a.cmp(emit_boxed_val(ARG1, offsetof(ErlSubBin, bitsize), sizeof(byte)),
              imm(0));
        a.jne(labels[Fail.getValue()]);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_bitstring(const ArgVal &Fail,
                                            const ArgVal &Src) {
    Label next = a.newLabel();

    mov_arg(ARG1, Src);

    emit_is_binary(labels[Fail.getValue()], ARG1, next, next);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_float(const ArgVal &Fail, const ArgVal &Src) {
    mov_arg(ARG1, Src);

    emit_is_boxed(labels[Fail.getValue()], ARG1);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.cmp(emit_boxed_val(boxed_ptr), imm(HEADER_FLONUM));
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_function(const ArgVal &Fail,
                                           const ArgVal &Src) {
    Label next = a.newLabel();

    mov_arg(RET, Src);

    emit_is_boxed(labels[Fail.getValue()], RET);

    x86::Gp boxed_ptr = emit_ptr_val(RET, RET);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
    a.cmp(RET, imm(HEADER_FUN));
    a.short_().je(next);
    ERTS_CT_ASSERT(HEADER_EXPORT < 256);
    a.cmp(RETb, imm(HEADER_EXPORT));
    a.jne(labels[Fail.getValue()]);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_function2(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Arity) {
    if (Arity.getType() != ArgVal::Immediate) {
        /*
         * Non-literal arity - extremely uncommon. Generate simple code.
         */
        mov_arg(ARG2, Src);
        mov_arg(ARG3, Arity);

        emit_enter_runtime();

        a.mov(ARG1, c_p);
        runtime_call<3>(erl_is_function);

        emit_leave_runtime();

        a.cmp(RET, imm(am_true));
        a.jne(labels[Fail.getValue()]);
        return;
    }

    unsigned arity = unsigned_val(Arity.getValue());
    if (arity > MAX_ARG) {
        /* Arity is negative or too large. */
        a.jmp(labels[Fail.getValue()]);
        return;
    }

    Label next = a.newLabel(), fun = a.newLabel();

    mov_arg(ARG1, Src);

    emit_is_boxed(labels[Fail.getValue()], ARG1);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
    a.cmp(RETd, imm(HEADER_FUN));
    a.short_().je(fun);
    ERTS_CT_ASSERT(HEADER_EXPORT < 256);
    a.cmp(RETb, imm(HEADER_EXPORT));
    a.jne(labels[Fail.getValue()]);

    comment("Check arity of export fun");
    a.mov(ARG2, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.cmp(x86::qword_ptr(ARG2, offsetof(Export, info.mfa.arity)), imm(arity));
    a.jne(labels[Fail.getValue()]);
    a.short_().jmp(next);

    comment("Check arity of fun");
    a.bind(fun);
    {
        a.cmp(emit_boxed_val(boxed_ptr, offsetof(ErlFunThing, arity)),
              imm(arity));
        a.jne(labels[Fail.getValue()]);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_integer(const ArgVal &Fail,
                                          const ArgVal &Src) {
    Label next = a.newLabel();
    Label fail = labels[Fail.getValue()];

    mov_arg(ARG1, Src);

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.short_().je(next);

    emit_is_boxed(fail, RET);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));

    a.and_(RETb, imm(_TAG_HEADER_MASK - _BIG_SIGN_BIT));
    a.cmp(RETb, imm(_TAG_HEADER_POS_BIG));
    a.jne(fail);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_list(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();

    mov_arg(RET, Src);

    a.cmp(RET, imm(NIL));
    a.short_().je(next);
    a.test(RETb, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
    a.jne(labels[Fail.getValue()]);
    a.bind(next);
}

void BeamModuleAssembler::emit_is_map(const ArgVal &Fail, const ArgVal &Src) {
    mov_arg(RET, Src);

    emit_is_boxed(labels[Fail.getValue()], RET);

    x86::Gp boxed_ptr = emit_ptr_val(RET, RET);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
    a.and_(RETb, imm(_TAG_HEADER_MASK));
    a.cmp(RETb, imm(_TAG_HEADER_MAP));
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_nil(const ArgVal &Fail, const ArgVal &Src) {
    a.cmp(getArgRef(Src), imm(NIL));
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_number(const ArgVal &Fail,
                                         const ArgVal &Src) {
    Label next = a.newLabel();
    Label fail = labels[Fail.getValue()];

    mov_arg(ARG1, Src);

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.short_().je(next);

    emit_is_boxed(fail, RET);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.mov(ARG1, emit_boxed_val(boxed_ptr));

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_HEADER_MASK - _BIG_SIGN_BIT));
    a.cmp(RETb, imm(_TAG_HEADER_POS_BIG));
    a.short_().je(next);

    a.cmp(ARG1d, imm(HEADER_FLONUM));
    a.jne(fail);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_pid(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();

    mov_arg(ARG1, Src);

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_PID));
    a.short_().je(next);

    /* Reuse RET as the important bits are still available. */
    emit_is_boxed(labels[Fail.getValue()], RET);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
    a.and_(RETb, _TAG_HEADER_MASK);
    a.cmp(RETb, _TAG_HEADER_EXTERNAL_PID);
    a.jne(labels[Fail.getValue()]);
    a.bind(next);
}

void BeamModuleAssembler::emit_is_port(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();
    mov_arg(ARG1, Src);

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_PORT));
    a.short_().je(next);

    /* Reuse RET as the important bits are still available. */
    emit_is_boxed(labels[Fail.getValue()], RET);

    x86::Gp boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
    a.and_(RETb, imm(_TAG_HEADER_MASK));
    a.cmp(RETb, imm(_TAG_HEADER_EXTERNAL_PORT));
    a.jne(labels[Fail.getValue()]);
    a.bind(next);
}

void BeamModuleAssembler::emit_is_reference(const ArgVal &Fail,
                                            const ArgVal &Src) {
    Label next = a.newLabel();

    mov_arg(RET, Src);

    emit_is_boxed(labels[Fail.getValue()], RET);

    x86::Gp boxed_ptr = emit_ptr_val(RET, RET);
    a.mov(RETd, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
    a.and_(RETb, imm(_TAG_HEADER_MASK));
    a.cmp(RETb, imm(_TAG_HEADER_REF));
    a.short_().je(next);
    a.cmp(RETb, imm(_TAG_HEADER_EXTERNAL_REF));
    a.jne(labels[Fail.getValue()]);

    a.bind(next);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple(const ArgVal &Fail,
                                                 const ArgVal &Src,
                                                 const ArgVal &Arity,
                                                 const ArgVal &Tag) {
    mov_arg(ARG2, Src);

    emit_is_boxed(labels[Fail.getValue()], ARG2);

    x86::Gp boxed_ptr = emit_ptr_val(ARG2, ARG2);
    ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));
    a.cmp(emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)), imm(Arity.getValue()));
    a.jne(labels[Fail.getValue()]);

    a.cmp(emit_boxed_val(boxed_ptr, sizeof(Eterm)), imm(Tag.getValue()));
    a.jne(labels[Fail.getValue()]);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple_ff(const ArgVal &NotTuple,
                                                    const ArgVal &NotRecord,
                                                    const ArgVal &Src,
                                                    const ArgVal &Arity,
                                                    const ArgVal &Tag) {
    mov_arg(ARG2, Src);
    emit_is_boxed(labels[NotTuple.getValue()], ARG2);
    (void)emit_ptr_val(ARG2, ARG2);
    a.mov(ARG1, emit_boxed_val(ARG2));

    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.test(ARG1.r8(), imm(_TAG_HEADER_MASK));
    a.jne(labels[NotTuple.getValue()]);

    ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));
    a.cmp(ARG1d, imm(Arity.getValue()));
    a.jne(labels[NotRecord.getValue()]);

    a.cmp(emit_boxed_val(ARG2, sizeof(Eterm)), imm(Tag.getValue()));
    a.jne(labels[NotRecord.getValue()]);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tuple(const ArgVal &Fail,
                                          const ArgVal &Src) {
    mov_arg(ARG2, Src);

    emit_is_boxed(labels[Fail.getValue()], ARG2);

    (void)emit_ptr_val(ARG2, ARG2);
    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.test(emit_boxed_val(ARG2, 0, sizeof(byte)), imm(_TAG_HEADER_MASK));

    a.jne(labels[Fail.getValue()]);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity(const ArgVal &Fail,
                                                   const ArgVal &Src,
                                                   const ArgVal &Arity) {
    mov_arg(ARG2, Src);

    emit_is_boxed(labels[Fail.getValue()], ARG2);

    (void)emit_ptr_val(ARG2, ARG2);
    ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));
    a.cmp(emit_boxed_val(ARG2, 0, sizeof(Uint32)), imm(Arity.getValue()));
    a.jne(labels[Fail.getValue()]);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_test_arity(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Arity) {
    mov_arg(ARG2, Src);

    (void)emit_ptr_val(ARG2, ARG2);
    ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));
    a.cmp(emit_boxed_val(ARG2, 0, sizeof(Uint32)), imm(Arity.getValue()));
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_is_eq_exact_immed(const ArgVal &Fail,
                                                   const ArgVal &X,
                                                   const ArgVal &Y) {
    cmp_arg(getArgRef(X), Y);
    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_is_ne_exact_immed(const ArgVal &Fail,
                                                   const ArgVal &X,
                                                   const ArgVal &Y) {
    cmp_arg(getArgRef(X), Y);
    a.je(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_eq_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    Label next = a.newLabel();

    mov_arg(ARG2, Y); /* May clobber ARG1 */
    mov_arg(ARG1, X);

    a.cmp(ARG1, ARG2);
#ifdef JIT_HARD_DEBUG
    a.je(next);
#else
    a.short_().je(next);
#endif

    /* Fancy way of checking if both are immediates. */
    a.mov(RETd, ARG1d);
    a.and_(RETd, ARG2d);
    a.and_(RETb, imm(_TAG_PRIMARY_MASK));
    a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
    a.je(labels[Fail.getValue()]);

    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();

    a.test(RET, RET);
    a.je(labels[Fail.getValue()]);

    a.bind(next);
}

void BeamModuleAssembler::emit_i_is_eq_exact_literal(const ArgVal &Fail,
                                                     const ArgVal &Src,
                                                     const ArgVal &Literal,
                                                     const ArgVal &tag_test) {
    mov_arg(ARG2, Literal); /* May clobber ARG1 */
    mov_arg(ARG1, Src);

    /* Fail immediately unless Src is the same type of pointer as the literal.
     */
    a.test(ARG1.r8(), imm(tag_test.getValue()));
    a.jne(labels[Fail.getValue()]);

    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();

    a.test(RET, RET);
    a.jz(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_ne_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    Label next = a.newLabel();

    mov_arg(ARG2, Y); /* May clobber ARG1 */
    mov_arg(ARG1, X);

    a.cmp(ARG1, ARG2);
    a.je(labels[Fail.getValue()]);

    /* Fancy way of checking if both are immediates. */
    a.mov(RETd, ARG1d);
    a.and_(RETd, ARG2d);
    a.and_(RETb, imm(_TAG_PRIMARY_MASK));
    a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
#ifdef JIT_HARD_DEBUG
    a.je(next);
#else
    a.short_().je(next);
#endif

    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();

    a.test(RET, RET);
    a.jnz(labels[Fail.getValue()]);

    a.bind(next);
}

void BeamModuleAssembler::emit_i_is_ne_exact_literal(const ArgVal &Fail,
                                                     const ArgVal &Src,
                                                     const ArgVal &Literal) {
    Label next = a.newLabel();

    mov_arg(ARG2, Literal); /* May clobber ARG1 */
    mov_arg(ARG1, Src);

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
    a.short_().je(next);

    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();

    a.test(RET, RET);
    a.jnz(labels[Fail.getValue()]);

    a.bind(next);
}

void BeamGlobalAssembler::emit_arith_eq_shared() {
    Label generic_compare = a.newLabel();

    /* Are both floats? */
    a.mov(ARG3d, ARG1d);
    a.or_(ARG3d, ARG2d);
    a.and_(ARG3d, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
    a.short_().jne(generic_compare);

    x86::Gp boxed_ptr = emit_ptr_val(ARG3, ARG1);
    a.mov(ARG3, emit_boxed_val(boxed_ptr));
    boxed_ptr = emit_ptr_val(ARG5, ARG2);
    a.mov(ARG5, emit_boxed_val(boxed_ptr));
    a.and_(ARG3d, imm(_TAG_HEADER_MASK));
    a.and_(ARG5d, imm(_TAG_HEADER_MASK));
    a.sub(ARG3d, imm(_TAG_HEADER_FLOAT));
    a.sub(ARG5d, imm(_TAG_HEADER_FLOAT));
    a.or_(ARG3d, ARG5d);
    a.short_().jne(generic_compare);

    boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.movsd(x86::xmm0, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    boxed_ptr = emit_ptr_val(ARG2, ARG2);
    a.movsd(x86::xmm1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));

    /* All float terms are finite so our caller only needs to check ZF. We don't
     * need to check for errors (PF). */
    a.comisd(x86::xmm0, x86::xmm1);

    a.ret();

    a.bind(generic_compare);
    {
        emit_enter_runtime();

        /* Generic eq-only arithmetic comparison. */
        comment("erts_cmp_compound(X, Y, 0, 1);");
        mov_imm(ARG3, 0);
        mov_imm(ARG4, 1);
        runtime_call<4>(erts_cmp_compound);

        emit_leave_runtime();

        a.test(RET, RET);

        a.ret();
    }
}

void BeamModuleAssembler::emit_is_eq(const ArgVal &Fail,
                                     const ArgVal &A,
                                     const ArgVal &B) {
    Label fail = labels[Fail.getValue()], next = a.newLabel();

    mov_arg(ARG2, B); /* May clobber ARG1 */
    mov_arg(ARG1, A);

    a.cmp(ARG1, ARG2);
    a.short_().je(next);

    /* We can skip deep comparisons when both args are immediates. */
    a.mov(RETd, ARG1d);
    a.and_(RETd, ARG2d);
    a.and_(RETb, imm(_TAG_PRIMARY_MASK));
    a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
    a.je(fail);

    safe_fragment_call(ga->get_arith_eq_shared());
    a.jne(fail);
    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne(const ArgVal &Fail,
                                     const ArgVal &A,
                                     const ArgVal &B) {
    Label fail = labels[Fail.getValue()], next = a.newLabel();

    mov_arg(ARG2, B); /* May clobber ARG1 */
    mov_arg(ARG1, A);

    a.cmp(ARG1, ARG2);
    a.je(fail);

    /* We can skip deep comparisons when both args are immediates. */
    a.mov(RETd, ARG1d);
    a.and_(RETd, ARG2d);
    a.and_(RETb, imm(_TAG_PRIMARY_MASK));
    a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
    a.short_().je(next);

    safe_fragment_call(ga->get_arith_eq_shared());
    a.je(fail);
    a.bind(next);
}

void BeamGlobalAssembler::emit_arith_compare_shared() {
    Label atom_compare, generic_compare;

    atom_compare = a.newLabel();
    generic_compare = a.newLabel();

    emit_enter_frame();

    /* Are both floats?
     *
     * This is done first as relative comparisons on atoms doesn't make much
     * sense. */
    a.mov(ARG3d, ARG1d);
    a.or_(ARG3d, ARG2d);
    a.and_(ARG3d, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
    a.short_().jne(atom_compare);

    x86::Gp boxed_ptr = emit_ptr_val(ARG3, ARG1);
    a.mov(ARG3, emit_boxed_val(boxed_ptr));
    boxed_ptr = emit_ptr_val(ARG5, ARG2);
    a.mov(ARG5, emit_boxed_val(boxed_ptr));
    a.and_(ARG3d, imm(_TAG_HEADER_MASK));
    a.and_(ARG5d, imm(_TAG_HEADER_MASK));
    a.sub(ARG3d, imm(_TAG_HEADER_FLOAT));
    a.sub(ARG5d, imm(_TAG_HEADER_FLOAT));
    a.or_(ARG3d, ARG5d);

    /* NOTE: Short won't reach if JIT_HARD_DEBUG is defined. */
    a.jne(generic_compare);

    boxed_ptr = emit_ptr_val(ARG1, ARG1);
    a.movsd(x86::xmm0, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    boxed_ptr = emit_ptr_val(ARG2, ARG2);
    a.movsd(x86::xmm1, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.comisd(x86::xmm0, x86::xmm1);

    /* `comisd` doesn't set the flags the same way `test` and friends do, so
     * they need to be converted for jl/jge to work. */
    a.setae(x86::al);
    a.dec(x86::al);

    emit_leave_frame();
    a.ret();

    a.bind(atom_compare);
    {
        /* Are both atoms? */
        a.mov(ARG3d, ARG1d);
        a.mov(ARG5d, ARG2d);
        a.and_(ARG3d, imm(_TAG_IMMED2_MASK));
        a.and_(ARG5d, imm(_TAG_IMMED2_MASK));
        a.sub(ARG3d, imm(_TAG_IMMED2_ATOM));
        a.sub(ARG5d, imm(_TAG_IMMED2_ATOM));
        a.or_(ARG3d, ARG5d);
        a.jne(generic_compare);

        emit_enter_runtime();

        runtime_call<2>(erts_cmp_atoms);

        emit_leave_runtime();

        /* !! erts_cmp_atoms returns int, not Sint !! */
        a.test(RETd, RETd);

        emit_leave_frame();
        a.ret();
    }

    a.bind(generic_compare);
    {
        emit_enter_runtime();

        comment("erts_cmp_compound(X, Y, 0, 0);");
        mov_imm(ARG3, 0);
        mov_imm(ARG4, 0);
        runtime_call<4>(erts_cmp_compound);

        emit_leave_runtime();

        a.test(RET, RET);

        emit_leave_frame();
        a.ret();
    }
}

void BeamModuleAssembler::emit_is_lt(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    Label fail = labels[Fail.getValue()];
    Label generic = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, RHS); /* May clobber ARG1 */
    mov_arg(ARG1, LHS);

    a.cmp(ARG1, ARG2);
    a.je(fail);

    /* Relative comparisons are overwhelmingly likely to be used on smalls, so
     * we'll specialize those and keep the rest in a shared fragment. */

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.mov(RETd, ARG1d);
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.mov(RETd, ARG2d);
    } else {
        a.mov(RETd, ARG1d);
        a.and_(RETd, ARG2d);
    }

    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.short_().jne(generic);

    a.cmp(ARG1, ARG2);
    a.short_().jl(next);
    a.jmp(fail);

    a.bind(generic);
    {
        safe_fragment_call(ga->get_arith_compare_shared());
        a.jge(fail);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ge(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    Label fail = labels[Fail.getValue()];
    Label generic = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, RHS); /* May clobber ARG1 */
    mov_arg(ARG1, LHS);

    a.cmp(ARG1, ARG2);
    a.short_().je(next);

    /* Relative comparisons are overwhelmingly likely to be used on smalls, so
     * we'll specialize those and keep the rest in a shared fragment. */

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.mov(RETd, ARG1d);
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.mov(RETd, ARG2d);
    } else {
        a.mov(RETd, ARG1d);
        a.and_(RETd, ARG2d);
    }

    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));
    a.short_().jne(generic);

    a.cmp(ARG1, ARG2);
    a.short_().jge(next);
    a.jmp(fail);

    a.bind(generic);
    {
        safe_fragment_call(ga->get_arith_compare_shared());
        a.jl(fail);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_bif_is_eq_ne_exact_immed(const ArgVal &Src,
                                                        const ArgVal &Immed,
                                                        const ArgVal &Dst,
                                                        Eterm fail_value,
                                                        Eterm succ_value) {
    cmp_arg(getArgRef(Src), Immed);
    mov_imm(RET, fail_value);
    mov_imm(ARG1, succ_value);
    a.cmove(RET, ARG1);
    mov_arg(Dst, RET);
}

void BeamModuleAssembler::emit_bif_is_eq_exact_immed(const ArgVal &Src,
                                                     const ArgVal &Immed,
                                                     const ArgVal &Dst) {
    emit_bif_is_eq_ne_exact_immed(Src, Immed, Dst, am_false, am_true);
}

void BeamModuleAssembler::emit_bif_is_ne_exact_immed(const ArgVal &Src,
                                                     const ArgVal &Immed,
                                                     const ArgVal &Dst) {
    emit_bif_is_eq_ne_exact_immed(Src, Immed, Dst, am_true, am_false);
}

void BeamModuleAssembler::emit_badmatch(const ArgVal &Src) {
    mov_arg(x86::qword_ptr(c_p, offsetof(Process, fvalue)), Src);
    emit_error(BADMATCH);
}

void BeamModuleAssembler::emit_case_end(const ArgVal &Src) {
    mov_arg(x86::qword_ptr(c_p, offsetof(Process, fvalue)), Src);
    emit_error(EXC_CASE_CLAUSE);
}

void BeamModuleAssembler::emit_system_limit_body() {
    emit_error(SYSTEM_LIMIT);
}

void BeamModuleAssembler::emit_if_end() {
    emit_error(EXC_IF_CLAUSE);
}

void BeamModuleAssembler::emit_catch(const ArgVal &Y, const ArgVal &Fail) {
    a.inc(x86::qword_ptr(c_p, offsetof(Process, catches)));

    Label patch_addr = a.newLabel();

    /*
     * Emit the following instruction:
     *
     *     b8 ff ff ff 7f    mov    eax,0x7fffffff
     *        ^
     *        |
     *        |
     * offset to be patched
     * with the tagged catch
     */
    a.bind(patch_addr);
    a.mov(RETd, imm(0x7fffffff));

    mov_arg(Y, RET);

    /* Offset = 1 for `mov` payload */
    catches.push_back({{patch_addr, 0x1, 0}, labels[Fail.getValue()]});
}

void BeamGlobalAssembler::emit_catch_end_shared() {
    Label not_throw = a.newLabel(), not_error = a.newLabel(),
          after_gc = a.newLabel();

    emit_enter_frame();

    /* Load thrown value / reason into ARG2 for add_stacktrace */
    a.mov(ARG2, getXRef(2));

    a.cmp(getXRef(1), imm(am_throw));
    a.short_().jne(not_throw);

    /* Thrown value, return it in x0 */
    a.mov(getXRef(0), ARG2);

    emit_leave_frame();
    a.ret();

    a.bind(not_throw);
    {
        a.cmp(getXRef(1), imm(am_error));
        /* NOTE: Short won't reach if JIT_HARD_DEBUG is defined. */
        a.jne(not_error);

        /* This is an error, attach a stacktrace to the reason. */
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.mov(ARG1, c_p);
        /* ARG2 set above. */
        a.mov(ARG3, getXRef(3));
        runtime_call<3>(add_stacktrace);

        emit_leave_runtime<Update::eStack | Update::eHeap>();

        /* not_error assumes stacktrace/reason is in ARG2 */
        a.mov(ARG2, RET);
    }

    a.bind(not_error);
    {
        const int32_t bytes_needed = (3 + S_RESERVED) * sizeof(Eterm);

        a.lea(ARG3, x86::qword_ptr(HTOP, bytes_needed));
        a.cmp(ARG3, E);
        a.short_().jbe(after_gc);

        /* Preserve stacktrace / reason */
        a.mov(getXRef(0), ARG2);
        mov_imm(ARG4, 1);
        aligned_call(labels[garbage_collect]);
        a.mov(ARG2, getXRef(0));

        a.bind(after_gc);

        a.mov(x86::qword_ptr(HTOP), imm(make_arityval(2)));
        a.mov(x86::qword_ptr(HTOP, sizeof(Eterm) * 1), imm(am_EXIT));
        a.mov(x86::qword_ptr(HTOP, sizeof(Eterm) * 2), ARG2);

        a.lea(RET, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
        a.add(HTOP, imm(3 * sizeof(Eterm)));

        a.mov(getXRef(0), RET);
    }

    emit_leave_frame();
    a.ret();
}

void BeamModuleAssembler::emit_catch_end(const ArgVal &Y) {
    Label next = a.newLabel();

    emit_try_end(Y);

    a.cmp(getXRef(0), imm(THE_NON_VALUE));
    a.short_().jne(next);
    fragment_call(ga->get_catch_end_shared());
    a.bind(next);
}

void BeamModuleAssembler::emit_try_end(const ArgVal &Y) {
    a.dec(x86::qword_ptr(c_p, offsetof(Process, catches)));
    emit_init(Y);
}

void BeamModuleAssembler::emit_try_case(const ArgVal &Y) {
    a.dec(x86::qword_ptr(c_p, offsetof(Process, catches)));
    mov_arg(Y, NIL);
    a.movups(x86::xmm0, x86::xmmword_ptr(registers, 1 * sizeof(Eterm)));
    a.mov(RET, getXRef(3));
    a.movups(x86::xmmword_ptr(registers, 0 * sizeof(Eterm)), x86::xmm0);
    a.mov(getXRef(2), RET);

#ifdef DEBUG
    Label fvalue_ok = a.newLabel(), assertion_failed = a.newLabel();
    comment("Start of assertion code");
    a.cmp(x86::qword_ptr(c_p, offsetof(Process, fvalue)), NIL);
    a.short_().je(fvalue_ok);

    a.bind(assertion_failed);
    comment("Assertion c_p->fvalue == NIL && c_p->ftrace == NIL failed");
    a.ud2();

    a.bind(fvalue_ok);
    a.cmp(x86::qword_ptr(c_p, offsetof(Process, ftrace)), NIL);
    a.short_().jne(assertion_failed);
#endif
}

void BeamModuleAssembler::emit_try_case_end(const ArgVal &Src) {
    mov_arg(x86::qword_ptr(c_p, offsetof(Process, fvalue)), Src);
    emit_error(EXC_TRY_CLAUSE);
}

void BeamModuleAssembler::emit_raise(const ArgVal &Trace, const ArgVal &Value) {
    mov_arg(ARG3, Value);
    mov_arg(ARG2, Trace);

    /* This is an error, attach a stacktrace to the reason. */
    a.mov(x86::qword_ptr(c_p, offsetof(Process, fvalue)), ARG3);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, ftrace)), ARG2);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_sanitize_freason);

    emit_leave_runtime();

    emit_raise_exception();
}

void BeamModuleAssembler::emit_build_stacktrace() {
    emit_enter_runtime<Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    a.mov(ARG2, getXRef(0));
    runtime_call<2>(build_stacktrace);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

    a.mov(getXRef(0), RET);
}

void BeamModuleAssembler::emit_raw_raise() {
    Label next = a.newLabel();

    emit_enter_runtime();

    a.mov(ARG1, getXRef(2));
    a.mov(ARG2, getXRef(0));
    a.mov(ARG3, getXRef(1));
    a.mov(ARG4, c_p);
    runtime_call<4>(raw_raise);

    emit_leave_runtime();

    a.test(RET, RET);
    a.short_().jne(next);
    emit_raise_exception();
    a.bind(next);
    a.mov(getXRef(0), imm(am_badarg));
}

void BeamGlobalAssembler::emit_i_test_yield_shared() {
    int mfa_offset = -(int)sizeof(ErtsCodeMFA) - BEAM_ASM_FUNC_PROLOGUE_SIZE;

    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
        /* Subtract the size of an `emit_enter_frame` sequence. */
        mfa_offset -= 4;
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
    }

    a.add(x86::rsp, imm(8));

    /* Yield address is in ARG3. */
    a.lea(ARG2, x86::qword_ptr(ARG3, mfa_offset));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG2);
    a.mov(ARG2, x86::qword_ptr(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, arity)), ARG2);

    a.jmp(labels[context_switch_simplified]);
}

void BeamModuleAssembler::emit_i_test_yield() {
    Label next = a.newLabel(), entry = a.newLabel();

    /* When present, this is guaranteed to be the first instruction after the
     * breakpoint trampoline. */
    ASSERT(a.offset() % 8 == 0);

    emit_enter_frame();

    a.bind(entry);
    a.dec(FCALLS);
    a.short_().jg(next);
    a.lea(ARG3, x86::qword_ptr(entry));
    a.call(yieldEnter);
    a.bind(next);

#if defined(JIT_HARD_DEBUG) && defined(ERLANG_FRAME_POINTERS)
    a.mov(ARG1, c_p);
    a.mov(ARG2, x86::rbp);
    a.mov(ARG3, x86::rsp);

    emit_enter_runtime<Update::eStack>();
    runtime_call<3>(erts_validate_stack);
    emit_leave_runtime<Update::eStack>();
#endif
}

void BeamModuleAssembler::emit_i_yield() {
    a.mov(getXRef(0), imm(am_true));
#ifdef NATIVE_ERLANG_STACK
    fragment_call(yieldReturn);
#else
    Label next = a.newLabel();

    a.lea(ARG3, x86::qword_ptr(next));
    a.jmp(yieldReturn);

    a.align(kAlignCode, 8);
    a.bind(next);
#endif
}

void BeamModuleAssembler::emit_i_perf_counter() {
    Label next = a.newLabel(), small = a.newLabel();

    emit_enter_runtime();

#ifdef WIN32
    /* Call the function pointer used by erts_sys_perf_counter */
    runtime_call<0>(erts_sys_time_data__.r.o.sys_hrtime);
#else
    runtime_call<0>(erts_sys_time_data__.r.o.perf_counter);
#endif

    emit_leave_runtime();

    a.mov(ARG1, RET);
    a.sar(ARG1, imm(SMALL_BITS - 1));
    a.add(ARG1, 1);
    a.cmp(ARG1, 1);
    a.jbe(small);

    {
        a.mov(TMP_MEM1q, RET);

        emit_gc_test(ArgVal(ArgVal::Word, 0),
                     ArgVal(ArgVal::Word, ERTS_MAX_UINT64_HEAP_SIZE),
                     ArgVal(ArgVal::Word, 0));

        a.mov(ARG1, TMP_MEM1q);

        a.mov(x86::qword_ptr(HTOP, sizeof(Eterm) * 0),
              imm(make_pos_bignum_header(1)));
        a.mov(x86::qword_ptr(HTOP, sizeof(Eterm) * 1), ARG1);
        a.lea(RET, x86::qword_ptr(HTOP, TAG_PRIMARY_BOXED));
        a.add(HTOP, imm(sizeof(Eterm) * 2));
        a.short_().jmp(next);
    }

    a.bind(small);
    {
        a.shl(RET, imm(_TAG_IMMED1_SIZE));
        a.or_(RET, imm(_TAG_IMMED1_SMALL));
    }

    a.bind(next);
    a.mov(getXRef(0), RET);
}
