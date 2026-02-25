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
    // TODO
    emit_nyi("emit_error");
}

void BeamModuleAssembler::emit_gc_test_preserve(const ArgWord &Need,
                                                const ArgWord &Live,
                                                const ArgSource &Preserve,
                                                a32::Gp preserve_reg) {
    // TODO
    emit_nyi("emit_gc_test_preserve");
}

void BeamModuleAssembler::emit_gc_test(const ArgWord &Ns,
                                       const ArgWord &Nh,
                                       const ArgWord &Live) {
    // TODO
    emit_nyi("emit_gc_test");
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

void BeamModuleAssembler::emit_allocate_heap(const ArgWord &NeedStack,
                                             const ArgWord &NeedHeap,
                                             const ArgWord &Live) {
    ASSERT(NeedStack.get() <= MAX_REG);

    // TODO
    //emit_gc_test(NeedStack, NeedHeap, Live);

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
    // TODO
    emit_nyi("emit_test_heap");
}

void BeamModuleAssembler::emit_normal_exit() {
    // TODO
    emit_nyi("emit_normal_exit");
}

void BeamModuleAssembler::emit_continue_exit() {
    // TODO
    emit_nyi("emit_continue_exit");
}

void BeamModuleAssembler::emit_get_list(const ArgRegister &Src,
                                        const ArgRegister &Hd,
                                        const ArgRegister &Tl) {
    // TODO
    emit_nyi("emit_get_list");
}

void BeamModuleAssembler::emit_get_hd(const ArgRegister &Src,
                                      const ArgRegister &Hd) {
    // TODO
    emit_nyi("emit_get_hd");
}

void BeamModuleAssembler::emit_get_tl(const ArgRegister &Src,
                                      const ArgRegister &Tl) {
    // TODO
    emit_nyi("emit_get_tl");
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
    // TODO
    emit_nyi("emit_load_tuple_ptr");
}

#ifdef DEBUG
/* Emit an assertion to ensure that tuple_reg points into the same
 * tuple as Src. */
void BeamModuleAssembler::emit_tuple_assertion(const ArgSource &Src,
                                               a32::Gp tuple_reg) {
    // TODO
    emit_nyi("emit_tuple_assertion");
}
#endif

/* Fetch an element from the tuple pointed to by the untagged pointer
 * in ARG1. */
void BeamModuleAssembler::emit_i_get_tuple_element(const ArgSource &Src,
                                                   const ArgWord &Element,
                                                   const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_i_get_tuple_element");
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
    // TODO
    emit_nyi("emit_get_two_tuple_elements");
}

void BeamModuleAssembler::emit_init_yregs(const ArgWord &Size,
                                          const Span<ArgVal> &args) {
    // TODO
    emit_nyi("emit_init_yregs");
}

void BeamModuleAssembler::emit_trim(const ArgWord &Words,
                                    const ArgWord &Remaining) {
    // TODO
    emit_nyi("emit_trim");
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
    // TODO
    emit_nyi("emit_move_two_trim");
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
    // TODO
    emit_nyi("emit_store_two_values");
}

void BeamModuleAssembler::emit_load_two_xregs(const ArgRegister &Src1,
                                              const ArgXRegister &Dst1,
                                              const ArgRegister &Src2,
                                              const ArgXRegister &Dst2) {
    // TODO
    emit_nyi("emit_load_two_xregs");
}

void BeamModuleAssembler::emit_swap(const ArgRegister &R1,
                                    const ArgRegister &R2) {
    // TODO
    emit_nyi("emit_swap");
}

void BeamModuleAssembler::emit_swap2(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3) {
    // TODO
    emit_nyi("emit_swap2");
}

void BeamModuleAssembler::emit_swap3(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4) {
    // TODO
    emit_nyi("emit_swap3");
}

void BeamModuleAssembler::emit_swap4(const ArgRegister &R1,
                                     const ArgRegister &R2,
                                     const ArgRegister &R3,
                                     const ArgRegister &R4,
                                     const ArgRegister &R5) {
    // TODO
    emit_nyi("emit_swap4");
}

void BeamModuleAssembler::emit_node(const ArgRegister &Dst) {
    a.ldr(TMP, embed_constant(&erts_this_node, disp4KB));
    a.ldr(TMP, arm::Mem(TMP));
    mov_arg(Dst, arm::Mem(TMP, offsetof(ErlNode, sysname)));
}

void BeamModuleAssembler::emit_put_list(const ArgSource &Hd,
                                        const ArgSource &Tl,
                                        const ArgRegister &Dst) {
    // TODO
    emit_nyi("emit_put_list");
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
    // TODO
    emit_nyi("emit_put_tuple2");
}

void BeamModuleAssembler::emit_self(const ArgRegister &Dst) {
    mov_arg(Dst, arm::Mem(c_p, offsetof(Process, common.id)));
}

void BeamModuleAssembler::emit_copy_words_increment(a32::Gp from,
                                                    a32::Gp to,
                                                    size_t count) {
    // TODO
    emit_nyi("emit_copy_words_increment");
}

void BeamModuleAssembler::emit_update_record(const ArgAtom &Hint,
                                             const ArgWord &TupleSize,
                                             const ArgSource &Src,
                                             const ArgRegister &Dst,
                                             const ArgWord &UpdateCount,
                                             const Span<ArgVal> &updates) {
    // TODO
    emit_nyi("emit_update_record");
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
    // TODO
    emit_nyi("emit_jump");
}

void BeamModuleAssembler::emit_is_atom(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_atom");
}

void BeamModuleAssembler::emit_is_boolean(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_boolean");
}

void BeamModuleAssembler::emit_is_bitstring(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_bitstring");
}

void BeamModuleAssembler::emit_is_binary(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_binary");
}

void BeamModuleAssembler::emit_is_float(const ArgLabel &Fail,
                                        const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_float");
}

void BeamModuleAssembler::emit_is_function(const ArgLabel &Fail,
                                           const ArgRegister &Src) {
    // TODO
    emit_nyi("emit_is_function");
}

void BeamModuleAssembler::emit_is_function2(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgSource &Arity) {
    // TODO
    emit_nyi("emit_is_function2");
}

void BeamModuleAssembler::emit_is_integer(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_integer");
}

void BeamModuleAssembler::emit_is_list(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_list");
}

void BeamModuleAssembler::emit_is_map(const ArgLabel &Fail,
                                      const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_map");
}

void BeamModuleAssembler::emit_is_nil(const ArgLabel &Fail,
                                      const ArgRegister &Src) {
    // TODO
    emit_nyi("emit_is_nil");
}

void BeamModuleAssembler::emit_is_number(const ArgLabel &Fail,
                                         const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_number");
}

void BeamModuleAssembler::emit_is_pid(const ArgLabel &Fail,
                                      const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_pid");
}

void BeamModuleAssembler::emit_is_port(const ArgLabel &Fail,
                                       const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_port");
}

void BeamModuleAssembler::emit_is_reference(const ArgLabel &Fail,
                                            const ArgSource &Src) {
    // TODO
    emit_nyi("emit_is_reference");
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tagged_tuple(const ArgLabel &Fail,
                                                 const ArgSource &Src,
                                                 const ArgWord &Arity,
                                                 const ArgAtom &Tag) {
    // TODO
    emit_nyi("emit_i_is_tagged_tuple");
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple_ff(const ArgLabel &NotTuple,
                                                    const ArgLabel &NotRecord,
                                                    const ArgSource &Src,
                                                    const ArgWord &Arity,
                                                    const ArgAtom &Tag) {
    // TODO
    emit_nyi("emit_i_is_tagged_tuple_ff");
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple(const ArgLabel &Fail,
                                          const ArgSource &Src) {
    // TODO
    emit_nyi("emit_i_is_tuple");
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity(const ArgLabel &Fail,
                                                   const ArgSource &Src,
                                                   const ArgWord &Arity) {
    // TODO
    emit_nyi("emit_i_is_tuple_of_arity");
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity_ff(const ArgLabel &NotTuple,
                                                      const ArgLabel &BadArity,
                                                      const ArgSource &Src,
                                                      const ArgWord &Arity) {
    // TODO
    emit_nyi("emit_i_is_tuple_of_arity_ff");
}

/* Note: This instruction leaves the untagged pointer to the tuple in
 * ARG1. */
void BeamModuleAssembler::emit_i_test_arity(const ArgLabel &Fail,
                                            const ArgSource &Src,
                                            const ArgWord &Arity) {
    // TODO
    emit_nyi("emit_i_test_arity");
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
            a.cmp(ARG1, CAR(list_val(literal)));
            a.b_ne(resolve_beam_label(Fail, disp32MB));
            mov_imm(TMP, NIL);
            a.cmp(ARG2, TMP);
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
    // TODO
    emit_nyi("emit_is_ne_exact");
}

void BeamModuleAssembler::emit_is_eq(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    // TODO
    emit_nyi("emit_is_eq");
}

void BeamModuleAssembler::emit_is_ne(const ArgLabel &Fail,
                                     const ArgSource &X,
                                     const ArgSource &Y) {
    // TODO
    emit_nyi("emit_is_ne");
}

/*
 * ARG1 = LHS
 * ARG2 = RHS
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_arith_compare_shared() {
    // We directly call erts_cmp_compound here instead of
    // trying to use faster alternatives.
    emit_enter_runtime_frame();
    emit_enter_runtime();

    comment("erts_cmp_compound(X, Y, 0, 0);");
    mov_imm(ARG3, 0);
    mov_imm(ARG4, 0);
    runtime_call<4>(erts_cmp_compound);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    a.tst(ARG1, ARG1);

    a.bx(a32::lr);
}

void BeamModuleAssembler::emit_is_lt(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    // TODO
    emit_nyi("emit_is_lt");
}

void BeamModuleAssembler::emit_is_ge(const ArgLabel &Fail,
                                     const ArgSource &LHS,
                                     const ArgSource &RHS) {
    // TODO
    emit_nyi("emit_is_ge");
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
    // TODO
    emit_nyi("emit_case_end");
}

void BeamModuleAssembler::emit_system_limit_body() {
    // TODO
    emit_nyi("emit_system_limit_body");
}

void BeamModuleAssembler::emit_if_end() {
    // TODO
    emit_nyi("emit_if_end");
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
    // TODO
    emit_nyi("emit_catch_end_shared");
}

void BeamModuleAssembler::emit_catch_end(const ArgYRegister &CatchTag) {
    // TODO
    emit_nyi("emit_catch_end");
}

void BeamModuleAssembler::emit_try_end(const ArgYRegister &CatchTag) {
    // TODO
    emit_nyi("emit_try_end");
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
    // TODO
    emit_nyi("emit_try_case");
}

void BeamModuleAssembler::emit_try_case_end(const ArgSource &Src) {
    // TODO
    emit_nyi("emit_try_case_end");
}

void BeamGlobalAssembler::emit_raise_shared() {
    // TODO
    emit_nyi("emit_raise_shared");
}

void BeamModuleAssembler::emit_raise(const ArgSource &Trace,
                                     const ArgSource &Value) {
    // TODO
    emit_nyi("emit_raise");
}

void BeamModuleAssembler::emit_build_stacktrace() {
    // TODO
    emit_nyi("emit_build_stacktrace");
}

/* This instruction has the same semantics as the erlang:raise/3 BIF,
 * except that it can rethrow a raw stack backtrace. */
void BeamModuleAssembler::emit_raw_raise() {
    // TODO
    emit_nyi("emit_raw_raise");
}

#define TEST_YIELD_RETURN_OFFSET                                               \
    (BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(Uint32[3]) +                         \
     (erts_alcu_enable_code_atags ? sizeof(Uint32) : 0))

/* ARG3 = current_label */
void BeamGlobalAssembler::emit_i_test_yield_shared() {
    emit_nyi("emit_i_test_yield_shared");
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
    // TODO
    emit_nyi("emit_i_yield");
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
