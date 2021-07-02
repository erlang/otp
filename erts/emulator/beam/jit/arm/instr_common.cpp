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
    mov_imm(TMP1, reason);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    emit_raise_exception();
}

void BeamModuleAssembler::emit_gc_test_preserve(const ArgVal &Need,
                                                const ArgVal &Live,
                                                arm::Gp term) {
    const int32_t bytes_needed = (Need.getValue() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

    add(ARG3, HTOP, bytes_needed);
    a.cmp(ARG3, E);
    a.cond_ls().b(after_gc_check);

    ASSERT(Live.getValue() < ERTS_X_REGS_ALLOCATED);
    mov_arg(ArgVal(ArgVal::XReg, Live.getValue()), term);

    mov_imm(ARG4, Live.getValue() + 1);
    fragment_call(ga->get_garbage_collect());

    mov_arg(term, ArgVal(ArgVal::XReg, Live.getValue()));

    a.bind(after_gc_check);
}

void BeamModuleAssembler::emit_gc_test(const ArgVal &Ns,
                                       const ArgVal &Nh,
                                       const ArgVal &Live) {
    int32_t bytes_needed =
            (Ns.getValue() + Nh.getValue() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

    add(ARG3, HTOP, bytes_needed);
    a.cmp(ARG3, E);
    a.cond_ls().b(after_gc_check);

    mov_imm(ARG4, Live.getValue());
    fragment_call(ga->get_garbage_collect());

    a.bind(after_gc_check);
}

void BeamModuleAssembler::emit_validate(const ArgVal &arity) {
#ifdef DEBUG
    Label next = a.newLabel(), crash = a.newLabel();

    /* Crash if the Erlang heap is not word-aligned */
    a.tst(HTOP, imm(sizeof(Eterm) - 1));
    a.cond_ne().b(crash);

    /* Crash if the Erlang stack is not word-aligned */
    a.tst(E, imm(sizeof(Eterm) - 1));
    a.cond_ne().b(crash);

    /* Crash if we've overrun the stack */
    lea(TMP1, arm::Mem(E, -(int32_t)(S_REDZONE * sizeof(Eterm))));
    a.cmp(HTOP, TMP1);
    a.cond_hi().b(crash);

    a.b(next);

    a.bind(crash);
    a.udf(0xbad);
    a.bind(next);

#    ifdef JIT_HARD_DEBUG
    emit_enter_runtime_frame();

    for (unsigned i = 0; i < arity.getValue(); i++) {
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

void BeamModuleAssembler::emit_i_validate(const ArgVal &Arity) {
    emit_validate(Arity);
}

void BeamModuleAssembler::emit_allocate_heap(const ArgVal &NeedStack,
                                             const ArgVal &NeedHeap,
                                             const ArgVal &Live) {
    ASSERT(NeedStack.isWord() && NeedStack.getValue() <= MAX_REG);
    ArgVal needed = NeedStack;

    emit_gc_test(needed, NeedHeap, Live);

    if (needed.getValue() > 0) {
        sub(E, E, needed.getValue() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_allocate(const ArgVal &NeedStack,
                                        const ArgVal &Live) {
    emit_allocate_heap(NeedStack, ArgVal(ArgVal::Word, 0), Live);
}

void BeamModuleAssembler::emit_deallocate(const ArgVal &Deallocate) {
    ASSERT(Deallocate.isWord() && Deallocate.getValue() <= 1023);

    if (Deallocate.getValue() > 0) {
        add(E, E, Deallocate.getValue() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_test_heap(const ArgVal &Nh, const ArgVal &Live) {
    emit_gc_test(ArgVal(ArgVal::Word, 0), Nh, Live);
}

void BeamModuleAssembler::emit_normal_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();
    emit_proc_lc_unrequire();

    mov_imm(TMP1, EXC_NORMAL);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    a.str(ZERO, arm::Mem(c_p, offsetof(Process, arity)));
    a.mov(ARG1, c_p);
    mov_imm(ARG2, am_normal);
    runtime_call<2>(erts_do_exit_process);

    emit_proc_lc_require();
    emit_leave_runtime<Update::eStack | Update::eHeap | Update::eXRegs |
                       Update::eReductions>();

    a.b(resolve_fragment(ga->get_do_schedule(), disp128MB));
}

void BeamModuleAssembler::emit_continue_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>(0);
    emit_proc_lc_unrequire();

    a.mov(ARG1, c_p);
    runtime_call<1>(erts_continue_exit_process);

    emit_proc_lc_require();
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>(0);

    a.b(resolve_fragment(ga->get_do_schedule(), disp128MB));
}

void BeamModuleAssembler::emit_get_list(const ArgVal &Src,
                                        const ArgVal &Hd,
                                        const ArgVal &Tl) {
    auto src = load_source(Src, TMP1);
    auto hd = init_destination(Hd, TMP2);
    auto tl = init_destination(Tl, TMP3);
    arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    /* The `ldp` instruction does not accept a negative offset, so we
     * will need subtract the LIST tag beforehand. (This also nicely
     * take care of the potential overwriting issue when Src == Hd.) */
    a.sub(TMP1, cons_ptr, imm(TAG_PRIMARY_LIST));
    if (hd.reg == tl.reg) {
        /* ldp with two identical registers is an illegal
         * instruction. Produce the same result at the interpreter. */
        a.ldr(tl.reg, arm::Mem(TMP1, sizeof(Eterm)));
        flush_var(tl);
    } else {
        a.ldp(hd.reg, tl.reg, arm::Mem(TMP1));
        flush_vars(hd, tl);
    }
}

void BeamModuleAssembler::emit_get_hd(const ArgVal &Src, const ArgVal &Hd) {
    auto src = load_source(Src, TMP1);
    auto hd = init_destination(Hd, TMP2);
    arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(hd.reg, getCARRef(cons_ptr));
    flush_var(hd);
}

void BeamModuleAssembler::emit_get_tl(const ArgVal &Src, const ArgVal &Tl) {
    auto src = load_source(Src, TMP1);
    auto tl = init_destination(Tl, TMP2);
    arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(tl.reg, getCDRRef(cons_ptr));
    flush_var(tl);
}

void BeamModuleAssembler::emit_i_get(const ArgVal &Src, const ArgVal &Dst) {
    mov_arg(ARG2, Src);

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_pd_hash_get);

    emit_leave_runtime();

    mov_arg(Dst, ARG1);
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

    mov_arg(Dst, ARG1);
}

/* Store the untagged pointer to a tuple in ARG1. */
void BeamModuleAssembler::emit_load_tuple_ptr(const ArgVal &Src) {
    auto src = load_source(Src, ARG1);
    emit_untag_ptr(ARG1, src.reg);
}

#ifdef DEBUG
/* Emit an assertion to ensure that tuple_reg points into the same
 * tuple as Src. */
void BeamModuleAssembler::emit_tuple_assertion(const ArgVal &Src,
                                               arm::Gp tuple_reg) {
    Label ok = a.newLabel(), fatal = a.newLabel();
    ASSERT(tuple_reg != TMP1);
    mov_arg(TMP1, Src);
    emit_is_boxed(fatal, TMP1);
    emit_untag_ptr(TMP1, TMP1);
    a.cmp(TMP1, tuple_reg);
    a.cond_eq().b(ok);

    a.bind(fatal);
    a.udf(0xaaaa);
    a.bind(ok);
}
#endif

/* Fetch an element from the tuple pointed to by the untagged pointer
 * in ARG1. */
void BeamModuleAssembler::emit_i_get_tuple_element(const ArgVal &Src,
                                                   const ArgVal &Element,
                                                   const ArgVal &Dst) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG1);
#endif

    auto dst = init_destination(Dst, TMP1);
    safe_ldr(dst.reg, arm::Mem(ARG1, Element.getValue()));
    flush_var(dst);
}

/* Fetch two consecutive tuple elements from the tuple pointed to by
 * the boxed pointer in ARG1. */
void BeamModuleAssembler::emit_get_two_tuple_elements(const ArgVal &Src,
                                                      const ArgVal &Element,
                                                      const ArgVal &Dst1,
                                                      const ArgVal &Dst2) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG1);
#endif

    auto dst1 = init_destination(Dst1, TMP1);
    auto dst2 = init_destination(Dst2, TMP2);

    arm::Mem element_ptr = arm::Mem(ARG1, Element.getValue());
    safe_ldp(dst1.reg, dst2.reg, element_ptr);
    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_init(const ArgVal &Y) {
    mov_imm(TMP1, NIL);
    a.str(TMP1, getArgRef(Y));
}

void BeamModuleAssembler::emit_init_yregs(const ArgVal &Size,
                                          const Span<ArgVal> &args) {
    unsigned count = Size.getValue();
    ASSERT(count == args.size());

    unsigned i = 0;

    mov_imm(TMP1, NIL);

    while (i < count) {
        unsigned slots = 1;
        unsigned first_y = args[i].getValue();

        while (i + slots < count) {
            const ArgVal &current_y = args[i + slots];
            if (first_y + slots != current_y.getValue()) {
                break;
            }
            slots++;
        }

        i += slots;

        /* Now first_y is the number of the first y register to be initialized
         * and slots is the number of y registers to be initialized. */
        while (slots >= 2) {
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
            a.str(TMP1, getYRef(first_y));
        }
    }
}

void BeamModuleAssembler::emit_trim(const ArgVal &Words,
                                    const ArgVal &Remaining) {
    ASSERT(Words.isWord() && Words.getValue() <= 1023);

    if (Words.getValue() > 0) {
        add(E, E, Words.getValue() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_i_move(const ArgVal &Src, const ArgVal &Dst) {
    mov_arg(Dst, Src);
}

void BeamModuleAssembler::emit_store_two_xregs(const ArgVal &Src1,
                                               const ArgVal &Dst1,
                                               const ArgVal &Src2,
                                               const ArgVal &Dst2) {
    auto [src1, src2] = load_sources(Src1, TMP1, Src2, TMP2);
    safe_stp(src1.reg, src2.reg, Dst1, Dst2);
}

void BeamModuleAssembler::emit_load_two_xregs(const ArgVal &Src1,
                                              const ArgVal &Dst1,
                                              const ArgVal &Src2,
                                              const ArgVal &Dst2) {
    ASSERT(ArgVal::register_relation(Src1, Src2) ==
           ArgVal::Relation::consecutive);
    auto dst1 = init_destination(Dst1, TMP1);
    auto dst2 = init_destination(Dst2, TMP2);

    safe_ldp(dst1.reg, dst2.reg, Src1, Src2);
    flush_vars(dst1, dst2);
}

void BeamModuleAssembler::emit_move_two_yregs(const ArgVal &Src1,
                                              const ArgVal &Dst1,
                                              const ArgVal &Src2,
                                              const ArgVal &Dst2) {
    /* Optimize fetching of source Y registers. */
    switch (ArgVal::register_relation(Src1, Src2)) {
    case ArgVal::Relation::consecutive:
        safe_ldp(TMP1, TMP2, Src1, Src2);
        break;
    case ArgVal::Relation::reverse_consecutive:
        safe_ldp(TMP2, TMP1, Src2, Src1);
        break;
    case ArgVal::Relation::none:
        a.ldr(TMP1, getArgRef(Src1));
        a.ldr(TMP2, getArgRef(Src2));
        break;
    }

    /* Destination registers are always in consecutive order. */
    safe_stp(TMP1, TMP2, Dst1, Dst2);
}

void BeamModuleAssembler::emit_swap(const ArgVal &R1, const ArgVal &R2) {
    if (isRegisterBacked(R1)) {
        auto r1 = load_source(R1, ZERO);
        mov_arg(TMP1, R2);
        mov_arg(R2, R1);
        a.mov(r1.reg, TMP1);
    } else if (isRegisterBacked(R2)) {
        return emit_swap(R2, R1);
    } else {
        switch (ArgVal::register_relation(R1, R2)) {
        case ArgVal::Relation::consecutive:
            safe_ldp(TMP1, TMP2, R1, R2);
            safe_stp(TMP2, TMP1, R1, R2);
            break;
        case ArgVal::Relation::reverse_consecutive:
            safe_ldp(TMP1, TMP2, R2, R1);
            safe_stp(TMP2, TMP1, R2, R1);
            break;
        case ArgVal::Relation::none:
            a.ldr(TMP1, getArgRef(R1));
            a.ldr(TMP2, getArgRef(R2));
            a.str(TMP1, getArgRef(R2));
            a.str(TMP2, getArgRef(R1));
            break;
        }
    }
}

void BeamModuleAssembler::emit_swap2(const ArgVal &R1,
                                     const ArgVal &R2,
                                     const ArgVal &R3) {
    auto [arg1, arg2] = load_sources(R1, TMP1, R2, TMP2);
    auto arg3 = load_source(R3, TMP3);

    mov_var(TMP4, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, TMP4);

    flush_vars(arg1, arg2);
    flush_var(arg3);
}

void BeamModuleAssembler::emit_swap3(const ArgVal &R1,
                                     const ArgVal &R2,
                                     const ArgVal &R3,
                                     const ArgVal &R4) {
    auto [arg1, arg2] = load_sources(R1, TMP1, R2, TMP2);
    auto [arg3, arg4] = load_sources(R3, TMP3, R4, TMP4);

    mov_var(TMP5, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, arg4);
    mov_var(arg4, TMP5);

    flush_vars(arg1, arg2);
    flush_vars(arg3, arg4);
}

void BeamModuleAssembler::emit_swap4(const ArgVal &R1,
                                     const ArgVal &R2,
                                     const ArgVal &R3,
                                     const ArgVal &R4,
                                     const ArgVal &R5) {
    auto [arg1, arg2] = load_sources(R1, TMP1, R2, TMP2);
    auto [arg3, arg4] = load_sources(R3, TMP3, R4, TMP4);
    auto arg5 = load_source(R5, TMP5);

    mov_var(TMP6, arg1);
    mov_var(arg1, arg2);
    mov_var(arg2, arg3);
    mov_var(arg3, arg4);
    mov_var(arg4, arg5);
    mov_var(arg5, TMP6);

    flush_vars(arg1, arg2);
    flush_vars(arg3, arg4);
    flush_var(arg5);
}

void BeamModuleAssembler::emit_node(const ArgVal &Dst) {
    a.ldr(TMP1, embed_constant(&erts_this_node, disp32K));
    a.ldr(TMP1, arm::Mem(TMP1));
    mov_arg(Dst, arm::Mem(TMP1, offsetof(ErlNode, sysname)));
}

void BeamModuleAssembler::emit_put_list(const ArgVal &Hd,
                                        const ArgVal &Tl,
                                        const ArgVal &Dst) {
    auto [hd, tl] = load_sources(Hd, TMP1, Tl, TMP2);
    auto dst = init_destination(Dst, TMP3);

    a.stp(hd.reg, tl.reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
    a.sub(dst.reg, HTOP, imm(sizeof(Eterm[2]) - TAG_PRIMARY_LIST));

    flush_var(dst);
}

void BeamModuleAssembler::emit_put_list2(const ArgVal &Hd1,
                                         const ArgVal &Hd2,
                                         const ArgVal &Tl,
                                         const ArgVal &Dst) {
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

void BeamModuleAssembler::emit_put_tuple2(const ArgVal &Dst,
                                          const ArgVal &Arity,
                                          const Span<ArgVal> &args) {
    ASSERT(arityval(Arity.getValue()) == args.size());

    std::vector<ArgVal> data;
    data.reserve(args.size() + 1);
    data.push_back(Arity);

    bool dst_is_src = false;
    for (auto arg : args) {
        data.push_back(arg);
        dst_is_src |= (arg == Dst);
    }

    if (dst_is_src) {
        a.add(TMP1, HTOP, TAG_PRIMARY_BOXED);
    } else {
        auto ptr = init_destination(Dst, TMP1);
        a.add(ptr.reg, HTOP, TAG_PRIMARY_BOXED);
        flush_var(ptr);
    }

    size_t size = data.size();
    unsigned i;
    for (i = 0; i < size - 1; i += 2) {
        if ((i % 128) == 0) {
            check_pending_stubs();
        }

        auto [first, second] = load_sources(data[i], TMP2, data[i + 1], TMP3);
        a.stp(first.reg, second.reg, arm::Mem(HTOP).post(sizeof(Eterm[2])));
    }

    if (i < size) {
        mov_arg(arm::Mem(HTOP).post(sizeof(Eterm)), data[i]);
    }

    if (dst_is_src) {
        auto ptr = init_destination(Dst, TMP1);
        mov_var(ptr, TMP1);
        flush_var(ptr);
    }
}

void BeamModuleAssembler::emit_self(const ArgVal &Dst) {
    mov_arg(Dst, arm::Mem(c_p, offsetof(Process, common.id)));
}

void BeamModuleAssembler::emit_set_tuple_element(const ArgVal &Element,
                                                 const ArgVal &Tuple,
                                                 const ArgVal &Offset) {
    auto tuple = load_source(Tuple, TMP1);
    auto element = load_source(Element, TMP2);
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, tuple.reg);
    arm::Mem boxed_val = emit_boxed_val(boxed_ptr, Offset.getValue());

    stur(element.reg, boxed_val);
}

void BeamModuleAssembler::emit_is_nonempty_list(const ArgVal &Fail,
                                                const ArgVal &Src) {
    auto list_ptr = load_source(Src, TMP1);
    const int bitNumber = 1;

    ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << bitNumber));
    a.tbnz(list_ptr.reg, bitNumber, resolve_beam_label(Fail, disp32K));
}

void BeamModuleAssembler::emit_jump(const ArgVal &Fail) {
    a.b(resolve_beam_label(Fail, disp128MB));
}

void BeamModuleAssembler::emit_is_atom(const ArgVal &Fail, const ArgVal &Src) {
    auto src = load_source(Src, TMP1);

    a.and_(TMP1, src.reg, imm(_TAG_IMMED2_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED2_ATOM));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_boolean(const ArgVal &Fail,
                                          const ArgVal &Src) {
    /* Since am_true and am_false differ by a single bit, we can simplify the
     * check by clearing said bit and comparing against the lesser one. */
    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    auto src = load_source(Src, TMP1);
    a.and_(TMP1, src.reg, imm(~(am_true & ~_TAG_IMMED1_MASK)));
    a.cmp(TMP1, imm(am_false));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

arm::Gp BeamModuleAssembler::emit_is_binary(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            Label next,
                                            Label subbin) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);

    arm::Gp boxed_ptr = emit_ptr_val(ARG1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_SUB_BIN));
    a.cond_eq().b(subbin);

    ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
    a.and_(TMP1, TMP1, imm(~4));
    a.cmp(TMP1, imm(_TAG_HEADER_REFC_BIN));
    a.cond_eq().b(next);
    a.b(resolve_beam_label(Fail, disp128MB));

    return boxed_ptr;
}

void BeamModuleAssembler::emit_is_binary(const ArgVal &Fail,
                                         const ArgVal &Src) {
    Label next = a.newLabel(), subbin = a.newLabel();
    arm::Gp boxed_ptr;

    boxed_ptr = emit_is_binary(Fail, Src, next, subbin);

    a.bind(subbin);
    {
        /* emit_is_binary() has already removed the literal tag (if
         * applicable) from the copy of Src. */
        a.ldrb(TMP1.w(),
               emit_boxed_val(boxed_ptr, offsetof(ErlSubBin, bitsize)));
        a.cbnz(TMP1, resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_bitstring(const ArgVal &Fail,
                                            const ArgVal &Src) {
    Label next = a.newLabel();

    (void)emit_is_binary(Fail, Src, next, next);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_float(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();

    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));

    a.cmp(TMP1, imm(HEADER_FLONUM));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_function(const ArgVal &Fail,
                                           const ArgVal &Src) {
    Label next = a.newLabel();

    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.cmp(TMP1, imm(HEADER_FUN));
    a.cond_eq().b(next);
    a.cmp(TMP1, imm(HEADER_EXPORT));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

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

        a.cmp(ARG1, imm(am_true));
        a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

        return;
    }

    unsigned arity = unsigned_val(Arity.getValue());
    if (arity > MAX_ARG) {
        /* Arity is negative or too large. */
        a.b(resolve_beam_label(Fail, disp128MB));

        return;
    }

    Label next = a.newLabel();
    Label fun = a.newLabel();

    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP2, emit_boxed_val(boxed_ptr));
    a.cmp(TMP2, imm(HEADER_FUN));
    a.cond_eq().b(fun);
    a.cmp(TMP2, imm(HEADER_EXPORT));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    comment("Check arity of export fun");
    a.ldur(TMP2, emit_boxed_val(boxed_ptr, sizeof(Eterm)));
    a.ldr(TMP2, arm::Mem(TMP2, offsetof(Export, info.mfa.arity)));
    emit_branch_if_ne(TMP2, arity, resolve_beam_label(Fail, dispUnknown));
    a.b(next);

    comment("Check arity of fun");
    a.bind(fun);
    {
        a.ldur(TMP2, emit_boxed_val(boxed_ptr, offsetof(ErlFunThing, arity)));
        emit_branch_if_ne(TMP2, arity, resolve_beam_label(Fail, dispUnknown));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_integer(const ArgVal &Fail,
                                          const ArgVal &Src) {
    Label next = a.newLabel();

    auto src = load_source(Src, TMP1);

    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), TMP2);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));

    /* The following value (0b111011) is not possible to use as
     * an immediate operand for 'and'. See the note at the beginning
     * of the file.
     */
    mov_imm(TMP2, _TAG_HEADER_MASK - _BIG_SIGN_BIT);
    a.and_(TMP1, TMP1, TMP2);
    a.cmp(TMP1, imm(_TAG_HEADER_POS_BIG));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_list(const ArgVal &Fail, const ArgVal &Src) {
    auto src = load_source(Src, TMP1);

    a.tst(src.reg, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
    a.mov(TMP2, NIL);
    a.ccmp(src.reg, TMP2, 4, arm::Cond::kNE);
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_map(const ArgVal &Fail, const ArgVal &Src) {
    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_MAP));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_nil(const ArgVal &Fail, const ArgVal &Src) {
    auto src = load_source(Src, TMP1);
    a.cmp(src.reg, imm(NIL));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_number(const ArgVal &Fail,
                                         const ArgVal &Src) {
    Label next = a.newLabel();

    auto src = load_source(Src, TMP1);

    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), TMP2);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));

    /* The following value (0b111011) is not possible to use as
     * an immediate operand for 'and'. See the note at the beginning
     * of the file.
     */
    mov_imm(TMP2, _TAG_HEADER_MASK - _BIG_SIGN_BIT);
    a.and_(TMP2, TMP1, TMP2);
    a.cmp(TMP2, imm(_TAG_HEADER_POS_BIG));

    a.mov(TMP3, imm(HEADER_FLONUM));
    a.ccmp(TMP1, TMP3, 4, arm::Cond::kNE);
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_pid(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();

    auto src = load_source(Src, TMP1);

    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_PID));
    a.cond_eq().b(next);

    /* Reuse TMP2 as the important bits are still available. */
    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), TMP2);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP2, emit_boxed_val(boxed_ptr));
    a.and_(TMP2, TMP2, imm(_TAG_HEADER_MASK));
    a.cmp(TMP2, imm(_TAG_HEADER_EXTERNAL_PID));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_port(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();

    auto src = load_source(Src, TMP1);

    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_PORT));
    a.cond_eq().b(next);

    /* Reuse TMP2 as the important bits are still available. */
    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), TMP2);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP2, emit_boxed_val(boxed_ptr));
    a.and_(TMP2, TMP2, imm(_TAG_HEADER_MASK));
    a.cmp(TMP2, imm(_TAG_HEADER_EXTERNAL_PORT));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_reference(const ArgVal &Fail,
                                            const ArgVal &Src) {
    Label next = a.newLabel();

    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_EXTERNAL_REF));
    a.ccmp(TMP1, imm(_TAG_HEADER_REF), 4, arm::Cond::kNE);
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tagged_tuple(const ArgVal &Fail,
                                                 const ArgVal &Src,
                                                 const ArgVal &Arity,
                                                 const ArgVal &Tag) {
    auto src = load_source(Src, ARG1);
    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);
    emit_untag_ptr(ARG1, src.reg);

    /* It is tempting to fetch the both the header word and the first
     * element of the tuple with ldp, but that is potentially unsafe
     * if Src is the empty tuple. To make ldp safe, we would have to
     * ensure that an empty tuple in a heap fragment, literal area, or
     * persistent term is always followed by one word of allocated
     * memory. */
    a.ldr(TMP1, arm::Mem(ARG1));
    cmp_arg(TMP1, Arity);
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.ldr(TMP2, arm::Mem(ARG1, sizeof(Eterm)));
    cmp_arg(TMP2, Tag);
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple_ff(const ArgVal &NotTuple,
                                                    const ArgVal &NotRecord,
                                                    const ArgVal &Src,
                                                    const ArgVal &Arity,
                                                    const ArgVal &Tag) {
    Label correct_arity = a.newLabel();
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(NotTuple, dispUnknown), src.reg);
    emit_untag_ptr(ARG1, src.reg);

    /* It is tempting to fetch the both the header word and the first
     * element of the tuple with ldp, but that is potentially unsafe
     * if Src is the empty tuple. */
    a.ldr(TMP1, arm::Mem(ARG1));

    cmp_arg(TMP1, Arity);
    a.cond_eq().b(correct_arity);

    /* Not a tuple or the wrong arity. Decide which. */
    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.cond_eq().b(resolve_beam_label(NotRecord, disp1MB));
    a.b(resolve_beam_label(NotTuple, disp128MB));

    a.bind(correct_arity);
    {
        a.ldr(TMP2, arm::Mem(ARG1, sizeof(Eterm)));
        cmp_arg(TMP2, Tag);
        a.cond_ne().b(resolve_beam_label(NotRecord, disp1MB));
    }
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple(const ArgVal &Fail,
                                          const ArgVal &Src) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);
    emit_untag_ptr(ARG1, src.reg);

    a.ldr(TMP1, arm::Mem(ARG1));
    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity(const ArgVal &Fail,
                                                   const ArgVal &Src,
                                                   const ArgVal &Arity) {
    auto src = load_source(Src, ARG1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);
    emit_untag_ptr(ARG1, src.reg);

    a.ldr(TMP1, arm::Mem(ARG1));
    cmp_arg(TMP1, Arity);
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_test_arity(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Arity) {
    auto src = load_source(Src, ARG1);
    emit_untag_ptr(ARG1, src.reg);

    a.ldr(TMP1, arm::Mem(ARG1));
    cmp_arg(TMP1, Arity);
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
}

void BeamModuleAssembler::emit_is_eq_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    auto x = load_source(X, ARG1);

    if (Y.isImmed()) {
        /* If the second operand is a known to be an immediate, we can
         * fail immediately if the operands are not equal. */
        cmp_arg(x.reg, Y);
        a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

        return;
    }

    /* Both operands are registers. */
    Label next = a.newLabel();
    auto y = load_source(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.cond_eq().b(next);

    /* The terms could still be equal if both operands are pointers
     * having the same tag. */
    emit_is_unequal_based_on_tags(x.reg, y.reg);
    a.cond_eq().b(resolve_beam_label(Fail, disp1MB));

    /* Both operands are pointers having the same tag. Must do a
     * deeper comparison. */
    mov_var(ARG1, x);
    mov_var(ARG2, y);

    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();

    a.cbz(ARG1, resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    auto x = load_source(X, ARG1);

    if (Y.isImmed()) {
        /* If the second operand is a known to be an immediate, we can
         * fail immediately if the operands are equal. */
        cmp_arg(x.reg, Y);
        a.cond_eq().b(resolve_beam_label(Fail, disp1MB));

        return;
    }

    /* Both operands are registers. */
    Label next = a.newLabel();
    auto y = load_source(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.cond_eq().b(resolve_beam_label(Fail, disp1MB));

    /* Test whether the terms are definitely unequal based on the tags
     * alone. */
    emit_is_unequal_based_on_tags(x.reg, y.reg);
    a.cond_eq().b(next);

    /* Both operands are pointers having the same tag. Must do a
     * deeper comparison. */
    mov_var(ARG1, x);
    mov_var(ARG2, y);

    emit_enter_runtime();

    runtime_call<2>(eq);

    emit_leave_runtime();

    a.cbnz(ARG1, resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_eq(const ArgVal &Fail,
                                     const ArgVal &X,
                                     const ArgVal &Y) {
    Label next = a.newLabel();

    auto x = load_source(X, ARG1);
    auto y = load_source(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.cond_eq().b(next);

    /* We can skip deep comparisons when both args are immediates. */
    emit_are_both_immediate(x.reg, y.reg);
    a.cond_eq().b(resolve_beam_label(Fail, disp1MB));

    mov_var(ARG1, x);
    mov_var(ARG2, y);
    fragment_call(ga->get_arith_compare_shared());
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne(const ArgVal &Fail,
                                     const ArgVal &X,
                                     const ArgVal &Y) {
    Label next = a.newLabel();

    auto x = load_source(X, ARG1);
    auto y = load_source(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.cond_eq().b(resolve_beam_label(Fail, disp1MB));

    /* We can skip deep comparisons when both args are immediates. */
    emit_are_both_immediate(x.reg, y.reg);
    a.cond_eq().b(next);

    mov_var(ARG1, x);
    mov_var(ARG2, y);
    fragment_call(ga->get_arith_compare_shared());
    a.cond_eq().b(resolve_beam_label(Fail, disp1MB));

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

    arm::Gp boxed_ptr1 = emit_ptr_val(TMP1, ARG1);
    a.ldur(TMP3, emit_boxed_val(boxed_ptr1));
    arm::Gp boxed_ptr2 = emit_ptr_val(TMP2, ARG2);
    a.ldur(TMP4, emit_boxed_val(boxed_ptr2));

    mov_imm(TMP5, HEADER_FLONUM);
    a.cmp(TMP3, TMP5);
    a.ccmp(TMP4, TMP5, 0, arm::Cond::kEQ);
    a.cond_ne().b(generic_compare);

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

        runtime_call<2>(erts_cmp_atoms);

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
        runtime_call<4>(erts_cmp_compound);

        emit_leave_runtime();
        emit_leave_runtime_frame();

        a.tst(ARG1, ARG1);

        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_is_lt(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    Label generic = a.newLabel(), next = a.newLabel();

    auto lhs = load_source(LHS, ARG1);
    auto rhs = load_source(RHS, ARG2);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_eq().b(resolve_beam_label(Fail, disp1MB));

    /* Relative comparisons are overwhelmingly likely to be used on smalls, so
     * we'll specialize those and keep the rest in a shared fragment. */

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_lt().b(next);
    a.b(resolve_beam_label(Fail, disp128MB));

    a.bind(generic);
    {
        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());
        a.cond_ge().b(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ge(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    Label generic = a.newLabel(), next = a.newLabel();

    auto lhs = load_source(LHS, ARG1);
    auto rhs = load_source(RHS, ARG2);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_eq().b(next);

    /* Relative comparisons are overwhelmingly likely to be used on smalls, so
     * we'll specialize those and keep the rest in a shared fragment. */

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_ge().b(next);
    a.b(resolve_beam_label(Fail, disp128MB));

    a.bind(generic);
    {
        mov_var(ARG1, lhs);
        mov_var(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());
        a.cond_lt().b(resolve_beam_label(Fail, disp1MB));
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_badmatch(const ArgVal &Src) {
    mov_arg(arm::Mem(c_p, offsetof(Process, fvalue)), Src);
    emit_error(BADMATCH);
}

void BeamModuleAssembler::emit_case_end(const ArgVal &Src) {
    mov_arg(arm::Mem(c_p, offsetof(Process, fvalue)), Src);
    emit_error(EXC_CASE_CLAUSE);
}

void BeamModuleAssembler::emit_system_limit_body() {
    emit_error(SYSTEM_LIMIT);
}

void BeamModuleAssembler::emit_if_end() {
    emit_error(EXC_IF_CLAUSE);
}

void BeamModuleAssembler::emit_catch(const ArgVal &Y, const ArgVal &Handler) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.add(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));

    mov_arg(Y, Handler);
}

void BeamGlobalAssembler::emit_catch_end_shared() {
    Label not_throw = a.newLabel(), not_error = a.newLabel(),
          after_gc = a.newLabel();

    /* XREG0 = THE_NON_VALUE
     * XREG1 = class
     * XREG2 = error reason/thrown value
     * XREG3 = raw stacktrace. */

    a.mov(XREG0, XREG2);
    a.cmp(XREG1, imm(am_throw));

    a.cond_ne().b(not_throw);

    /* Return thrown value. */
    a.ret(a64::x30);

    a.bind(not_throw);
    {
        emit_enter_runtime_frame();

        a.mov(ARG1, XREG0);

        a.cmp(XREG1, imm(am_error));
        a.cond_ne().b(not_error);

        a.mov(ARG2, XREG0);
        a.mov(ARG3, XREG3);

        /* This is an error, attach a stacktrace to the reason. */
        ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 2);
        emit_enter_runtime<Update::eStack | Update::eHeap>(2);

        a.mov(ARG1, c_p);
        runtime_call<3>(add_stacktrace);

        emit_leave_runtime<Update::eStack | Update::eHeap>(2);

        /* Fall through! */
    }

    /* Error term from exit/1 or stack backtrace from error/{1,2,3} is
     * now in ARG1. */
    a.bind(not_error);
    {
        const int32_t bytes_needed = (3 + S_RESERVED) * sizeof(Eterm);
        add(ARG3, HTOP, bytes_needed);
        a.cmp(ARG3, E);

        a.cond_ls().b(after_gc);
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

void BeamModuleAssembler::emit_catch_end(const ArgVal &Y) {
    Label next = a.newLabel();

    /* XREG0 = THE_NON_VALUE
     * XREG1 = class
     * XREG2 = error reason/thrown value
     * XREG3 = raw stacktrace. */

    emit_try_end(Y);
    emit_branch_if_value(XREG0, next);
    fragment_call(ga->get_catch_end_shared());
    a.bind(next);
}

void BeamModuleAssembler::emit_try_end(const ArgVal &Y) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    emit_init(Y);
}

void BeamModuleAssembler::emit_try_case(const ArgVal &Y) {
    /* XREG0 = THE_NON_VALUE
     * XREG1 = class
     * XREG2 = error reason/thrown value
     * XREG3 = raw stacktrace.
     *
     * These need to be shifted down one step. */
    a.mov(XREG0, XREG1);
    a.mov(XREG1, XREG2);
    a.mov(XREG2, XREG3);

    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    mov_imm(TMP1, NIL);
    mov_arg(Y, TMP1);

#ifdef DEBUG
    Label ok = a.newLabel();
    comment("Start of assertion code");
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.ldr(TMP2, arm::Mem(c_p, offsetof(Process, ftrace)));
    mov_imm(TMP3, NIL);
    a.cmp(TMP1, TMP3);
    a.ccmp(TMP2, TMP3, 0, arm::Cond::kEQ);
    a.cond_eq().b(ok);

    comment("Assertion c_p->fvalue == NIL && c_p->ftrace == NIL failed");
    a.udf(0x42);

    a.bind(ok);
#endif
}

void BeamModuleAssembler::emit_try_case_end(const ArgVal &Src) {
    mov_arg(arm::Mem(c_p, offsetof(Process, fvalue)), Src);
    emit_error(EXC_TRY_CLAUSE);
}

void BeamModuleAssembler::emit_raise(const ArgVal &Trace, const ArgVal &Value) {
    mov_arg(TMP1, Value);
    mov_arg(ARG2, Trace);

    /* This is an error, attach a stacktrace to the reason. */
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, ftrace)));

    emit_enter_runtime(0);

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_sanitize_freason);

    emit_leave_runtime(0);

    emit_raise_exception();
}

void BeamModuleAssembler::emit_build_stacktrace() {
    a.mov(ARG2, XREG0);

    emit_enter_runtime<Update::eStack | Update::eHeap>(0);

    a.mov(ARG1, c_p);
    runtime_call<2>(build_stacktrace);

    emit_leave_runtime<Update::eStack | Update::eHeap>(0);

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
    runtime_call<4>(raw_raise);
    emit_leave_runtime(0);

    a.cbnz(ARG1, next);

    emit_raise_exception();

    a.bind(next);
    mov_imm(XREG0, am_badarg);
}

static size_t TEST_YIELD_RETURN_OFFSET = sizeof(Uint32[3]);

void BeamGlobalAssembler::emit_i_test_yield_shared() {
    int mfa_offset = sizeof(ErtsCodeMFA) + BEAM_ASM_FUNC_PROLOGUE_SIZE;

    /* Yield return address is in LR (x30). */
    a.sub(ARG2, a64::x30, imm(TEST_YIELD_RETURN_OFFSET + mfa_offset));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, current)));

    a.ldr(ARG2, arm::Mem(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, arity)));

    a.mov(ARG3, a64::x30);
    a.b(labels[context_switch_simplified]);
}

void BeamModuleAssembler::emit_i_test_yield() {
    Label next = a.newLabel();

    /* When present, this is guaranteed to be the first instruction after the
     * breakpoint trampoline. */

#ifdef DEBUG
    size_t startPos = a.offset();
    ASSERT(startPos % 8 == 0);
#endif

    a.subs(FCALLS, FCALLS, imm(1));
    a.cond_gt().b(next);

    /* We avoid using the `fragment_call` helper to ensure a constant layout,
     * as it adds code in certain debug configurations. */
    a.bl(resolve_fragment(ga->get_i_test_yield_shared(), disp128MB));
    a.bind(next);

    ASSERT(a.offset() - startPos == TEST_YIELD_RETURN_OFFSET);
}

void BeamModuleAssembler::emit_i_yield() {
    mov_imm(XREG0, am_true);
    fragment_call(ga->get_dispatch_return());
}

void BeamModuleAssembler::emit_i_perf_counter() {
    Label next = a.newLabel(), small = a.newLabel();

    emit_enter_runtime_frame();
    runtime_call<0>(erts_sys_time_data__.r.o.perf_counter);
    emit_leave_runtime_frame();

    a.asr(TMP1, ARG1, imm(SMALL_BITS - 1));
    a.add(TMP1, TMP1, imm(1));
    a.cmp(TMP1, imm(1));
    a.cond_ls().b(small);

    {
        a.mov(XREG0, ARG1);

        emit_gc_test(ArgVal(ArgVal::Word, 0),
                     ArgVal(ArgVal::Word, ERTS_MAX_UINT64_HEAP_SIZE),
                     ArgVal(ArgVal::Word, 0));

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
