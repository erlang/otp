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
#include <algorithm>
#include "beam_asm.hpp"

using namespace asmjit;

void BeamModuleAssembler::emit_linear_search(x86::Gp comparand,
                                             const ArgVal &Fail,
                                             const Span<ArgVal> &args) {
    int count = args.size() / 2;

    for (int i = 0; i < count; i++) {
        const ArgVal &value = args[i];
        const ArgVal &label = args[i + count];

        cmp_arg(comparand, value, ARG1);
        a.je(labels[label.getValue()]);
    }

    if (Fail.isLabel()) {
        a.jmp(labels[Fail.getValue()]);
    } else {
        /* NIL means fallthrough to the next instruction. */
        ASSERT(Fail.getType() == ArgVal::Immediate && Fail.getValue() == NIL);
    }
}

void BeamModuleAssembler::emit_i_select_tuple_arity(const ArgVal &Src,
                                                    const ArgVal &Fail,
                                                    const ArgVal &Size,
                                                    const Span<ArgVal> &args) {
    mov_arg(ARG2, Src);
    emit_is_boxed(labels[Fail.getValue()], ARG2);
    x86::Gp boxed_ptr = emit_ptr_val(ARG2, ARG2);
    ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));
    a.mov(ARG2d, emit_boxed_val(boxed_ptr, 0, sizeof(Uint32)));
    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.test(ARG2.r8(), imm(_TAG_HEADER_MASK));
    a.jne(labels[Fail.getValue()]);

    ERTS_CT_ASSERT(Support::isInt32(make_arityval(MAX_ARITYVAL)));

    int count = args.size() / 2;
    for (int i = 0; i < count; i++) {
        const ArgVal &value = args[i];
        const ArgVal &label = args[i + count];

        a.cmp(ARG2d, imm(value.getValue()));
        a.je(labels[label.getValue()]);
    }

    a.jne(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_select_val_lins(const ArgVal &Src,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Size,
                                                 const Span<ArgVal> &args) {
    ASSERT(Size.getValue() == args.size());
    mov_arg(ARG2, Src);
    if (emit_optimized_three_way_select(Fail, args))
        return;
    emit_linear_search(ARG2, Fail, args);
}

void BeamModuleAssembler::emit_i_select_val_bins(const ArgVal &Src,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Size,
                                                 const Span<ArgVal> &args) {
    ASSERT(Size.getValue() == args.size());
    int count = args.size() / 2;
    Label fail;

    if (Fail.isLabel()) {
        fail = labels[Fail.getValue()];
    } else {
        /* NIL means fallthrough to the next instruction. */
        ASSERT(Fail.getType() == ArgVal::Immediate && Fail.getValue() == NIL);
        fail = a.newLabel();
    }

    mov_arg(ARG2, Src);
    comment("Binary search in table of %lu elements", count);
    emit_binsearch_nodes(0, count - 1, Fail, args);

    if (Fail.getType() == ArgVal::Immediate) {
        a.bind(fail);
    }
}

/*
 * Emit code for a binary search through an interval Left <= Right of
 * the i_select_val argument vector `args`.
 *
 * ARG2 is the value being looked up.
 */
void BeamModuleAssembler::emit_binsearch_nodes(size_t Left,
                                               size_t Right,
                                               const ArgVal &Fail,
                                               const Span<ArgVal> &args) {
    ASSERT(Left <= Right);
    ASSERT(Right < args.size() / 2);
    size_t mid = (Left + Right) >> 1;
    ArgVal midval(ArgVal::Immediate, args[mid].getValue());
    int count = args.size() / 2;
    size_t remaining = (Right - Left + 1);

    if (remaining <= 10) {
        /* Measurements on randomly generated select_val instructions
           have shown that linear search is faster than binary search
           when there are ten or less elements.
        */
        std::vector<ArgVal> shrunk;

        comment("Linear search in [%lu..%lu], %lu elements",
                Left,
                Right,
                remaining);

        shrunk.reserve(remaining * 2);
        shrunk.insert(shrunk.end(),
                      args.begin() + Left,
                      args.begin() + Left + remaining);
        shrunk.insert(shrunk.end(),
                      args.begin() + Left + count,
                      args.begin() + count + Left + remaining);

        if (!emit_optimized_three_way_select(Fail, shrunk))
            emit_linear_search(ARG2, Fail, shrunk);
        return;
    }

    comment("Subtree [%lu..%lu], pivot %lu", Left, Right, mid);
    cmp_arg(ARG2, midval, ARG1);

    if (Left == Right) {
        a.je(labels[args[mid + count].getValue()]);
        a.jmp(labels[Fail.getValue()]);
        return;
    }

    a.je(labels[args[mid + count].getValue()]);

    if (Left == mid) {
        a.jb(labels[Fail.getValue()]);
    } else {
        Label right_tree = a.newLabel();
        a.ja(right_tree);
        emit_binsearch_nodes(Left, mid - 1, Fail, args);
        a.bind(right_tree);
    }

    emit_binsearch_nodes(mid + 1, Right, Fail, args);
}

void BeamModuleAssembler::emit_i_jump_on_val(const ArgVal &Src,
                                             const ArgVal &Fail,
                                             const ArgVal &Base,
                                             const ArgVal &Size,
                                             const Span<ArgVal> &args) {
    Label data = embed_vararg_rodata(args, 0);
    Label fail;

    ASSERT(Size.getValue() == args.size());

    mov_arg(ARG1, Src);

    a.mov(RETd, ARG1d);
    a.and_(RETb, imm(_TAG_IMMED1_MASK));
    a.cmp(RETb, imm(_TAG_IMMED1_SMALL));

    if (Fail.isLabel()) {
        a.jne(labels[Fail.getValue()]);
    } else {
        /* NIL means fallthrough to the next instruction. */
        ASSERT(Fail.getType() == ArgVal::Immediate && Fail.getValue() == NIL);
        fail = a.newLabel();
        a.short_().jne(fail);
    }

    a.sar(ARG1, imm(_TAG_IMMED1_SIZE));

    if (Base.getValue() != 0) {
        if (Support::isInt32((Sint)Base.getValue())) {
            a.sub(ARG1, imm(Base.getValue()));
        } else {
            a.mov(ARG2, imm(Base.getValue()));
            a.sub(ARG1, ARG2);
        }
    }

    a.cmp(ARG1, imm(args.size()));
    if (Fail.isLabel()) {
        a.jae(labels[Fail.getValue()]);
    } else {
        a.short_().jae(fail);
    }

    a.lea(RET, x86::qword_ptr(data));
    a.jmp(x86::qword_ptr(RET, ARG1, 3));

    if (Fail.getType() == ArgVal::Immediate) {
        a.bind(fail);
    }
}

/*
 * Attempt to optimize the case when a select_val has exactly two
 * values which only differ by one bit and they both branch to the
 * same label.
 *
 * The optimization makes use of the observation that (V == X || V ==
 * Y) is equivalent to (V | (X ^ Y)) == (X | Y) when (X ^ Y) has only
 * one bit set.
 *
 * ARG2 contains the value.
 * Return true if the optimization was possible, in
 * which case ARG1 should be considered trashed.
 */
bool BeamModuleAssembler::emit_optimized_three_way_select(
        const ArgVal &Fail,
        const Span<ArgVal> &args) {
    if (args.size() != 4 || (args[2].getValue() != args[3].getValue()))
        return false;

    uint64_t x = args[0].getValue();
    uint64_t y = args[1].getValue();
    uint64_t combined = x | y;
    uint64_t diff = x ^ y;
    ArgVal val(ArgVal::Immediate, combined);

    if ((diff & (diff - 1)) != 0)
        return false;
    comment("(Src == 0x%x || Src == 0x%x) <=> (Src | 0x%x) == 0x%x",
            x,
            y,
            diff,
            combined);

    if (Support::isInt32((Sint)diff)) {
        a.or_(ARG2, imm(diff));
    } else {
        a.mov(ARG1, imm(diff));
        a.or_(ARG2, ARG1);
    }
    cmp_arg(ARG2, val, ARG1);
    a.je(labels[args[2].getValue()]);
    if (Fail.isLabel()) {
        a.jmp(labels[Fail.getValue()]);
    } else {
        /* NIL means fallthrough to the next instruction. */
        ASSERT(Fail.getType() == ArgVal::Immediate && Fail.getValue() == NIL);
    }
    return true;
}
