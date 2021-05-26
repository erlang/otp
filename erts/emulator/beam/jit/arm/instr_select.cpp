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
#include <numeric>

#include "beam_asm.hpp"

using namespace asmjit;

/* Request Support::isInt12 ! */
template<typename T>
bool isInt12(T value) {
    typedef typename std::make_unsigned<T>::type U;
    typedef typename std::make_signed<T>::type S;

    return Support::isUInt12(U(value)) || Support::isUInt12(-S(value));
}

/* The cmp instruction for AArch accepts only a 12-bit immediate value.
 * That means that to compare most atoms, the atom number to be compared
 * must be loaded into a temporary register.
 *
 * We can use the immediate form of cmp for more values if we untag both
 * the source value and the values to be compared.
 *
 * This function finds the `base` and `shift` that result in the most number
 * of elements fitting in a 12-bit immediate. */
static std::pair<UWord, int> plan_untag(const Span<ArgVal> &args) {
    auto left = args.begin(), right = args.begin();
    auto best_left = left, best_right = right;
    int count, shift;

    count = args.size() / 2;

    ASSERT(left->isImmed() && (args.begin() + count)->isLabel());
    ASSERT(is_small(left->getValue()) || is_atom(right->getValue()));

    shift = is_small(left->getValue()) ? _TAG_IMMED1_SIZE : _TAG_IMMED2_SIZE;

    while (right < (args.begin() + count)) {
        auto distance = std::distance(left, right);
        UWord left_value, mid_value, right_value;

        left_value = left->getValue() >> shift;
        mid_value = (left + distance / 2)->getValue() >> shift;
        right_value = right->getValue() >> shift;

        if (isInt12(left_value - mid_value) &&
            isInt12(right_value - mid_value)) {
            if (distance > std::distance(best_left, best_right)) {
                best_right = right;
                best_left = left;
            }

            right++;
        } else {
            left++;
        }
    }

    auto distance = std::distance(best_left, best_right);

    /* Skip everything if the best run is too short, untagging has its costs
     * too. */
    if (distance <= 6) {
        return std::make_pair(0, 0);
    }

    /* Apply neither shift nor base if the best run doesn't need it: we're more
     * likely to lose by rebasing/shifting. */
    if (isInt12(best_left->getValue()) && isInt12(best_right->getValue())) {
        return std::make_pair(0, 0);
    }

    /* Skip rebasing if the best run doesn't need it after shifting. */
    if (isInt12(best_left->getValue() >> shift) &&
        isInt12(best_right->getValue() >> shift)) {
        return std::make_pair(0, shift);
    }

    auto mid_value = (best_left + distance / 2)->getValue();
    return std::make_pair(mid_value, shift);
}

const std::vector<ArgVal> BeamModuleAssembler::emit_select_untag(
        const Span<ArgVal> &args,
        a64::Gp comparand,
        Label fail,
        UWord base,
        int shift) {
    ASSERT(base != 0 || shift > 0);

    /* Emit code to test that the source value has the correct type and
     * untag it. */
    comment("(comparing untagged+rebased values)");
    if (is_small(args.front().getValue())) {
        a.and_(TMP1, comparand, imm(_TAG_IMMED1_MASK));
        a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    } else {
        ASSERT(is_atom(args.front().getValue()));
        a.and_(TMP1, comparand, imm(_TAG_IMMED2_MASK));
        a.cmp(TMP1, imm(_TAG_IMMED2_ATOM));
    }

    a.cond_ne().b(resolve_label(fail, disp1MB));

    if (shift != 0) {
        a.lsr(ARG1, comparand, imm(shift));
        base >>= shift;

        comparand = ARG1;
    }

    std::vector<ArgVal> result(args.begin(), args.end());
    int count = args.size() / 2;

    if (base != 0) {
        sub(ARG1, comparand, base);

        /* The values will always be ordered differently after adjusting the
         * base, so we have to sort them again.
         *
         * This is rather annoying because the labels and values can't be
         * sorted together. Perhaps we should diverge from the other platforms,
         * keeping them together just on ARM? */
        std::vector<int> sorted_indexes(count);
        std::iota(sorted_indexes.begin(), sorted_indexes.end(), 0);
        std::sort(sorted_indexes.begin(),
                  sorted_indexes.end(),
                  [&](int lhs, int rhs) {
                      auto lhs_value = (args[lhs].getValue() >> shift) - base;
                      auto rhs_value = (args[rhs].getValue() >> shift) - base;
                      return lhs_value < rhs_value;
                  });

        for (auto i = 0; i < count; i++) {
            const auto &src_value = args[sorted_indexes[i]];
            const auto &src_label = args[sorted_indexes[i] + count];
            auto &dst_value = result[i];
            auto &dst_label = result[i + count];

            dst_value = ArgVal(ArgVal::Immediate,
                               (src_value.getValue() >> shift) - base);
            dst_label = src_label;
        }
    } else {
        /* Fast-path for when a shift alone is enough, it won't affect the
         * order. */
        for (auto i = 0; i < count; i++) {
            auto &dst_value = result[i];
            auto &dst_label = result[i + count];

            dst_value = ArgVal(ArgVal::Immediate, args[i].getValue() >> shift);
            dst_label = args[i + count];
        }
    }

    ASSERT(std::is_sorted(result.begin(),
                          result.begin() + count,
                          [](const ArgVal &lhs, const ArgVal &rhs) {
                              return lhs.getValue() < rhs.getValue();
                          }));

    return result;
}

void BeamModuleAssembler::emit_linear_search(arm::Gp comparand,
                                             Label fail,
                                             const Span<ArgVal> &args) {
    int count = args.size() / 2;

    for (int i = 0; i < count; i++) {
        const ArgVal &value = args[i];
        const ArgVal &label = args[i + count];

        if ((i % 128) == 0) {
            /* Checking veneers on the first element is intentional. */
            check_pending_stubs();
        }

        cmp_arg(comparand, value);
        a.cond_eq().b(resolve_beam_label(label, disp1MB));
    }

    /* An invalid label means fallthrough to the next instruction. */
    if (fail.isValid()) {
        a.b(resolve_label(fail, disp128MB));
    }
}

void BeamModuleAssembler::emit_i_select_tuple_arity(const ArgVal &Src,
                                                    const ArgVal &Fail,
                                                    const ArgVal &Size,
                                                    const Span<ArgVal> &args) {
    auto src = load_source(Src, TMP1);

    emit_is_boxed(resolve_beam_label(Fail, dispUnknown), src.reg);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr, 0));

    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.cond_ne().b(resolve_beam_label(Fail, disp1MB));

    Label fail = rawLabels[Fail.getValue()];
    emit_linear_search(TMP1, fail, args);
}

void BeamModuleAssembler::emit_i_select_val_lins(const ArgVal &Src,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Size,
                                                 const Span<ArgVal> &args) {
    ASSERT(Size.getValue() == args.size());
    Label fail, next;

    /*
     * To keep the code simpler, we will drop down a level and
     * use rawLabels. That will allow us to use Label to represent
     * a label present in the BEAM file or a label created here.
     */

    if (Fail.isLabel()) {
        next = fail = rawLabels[Fail.getValue()];
    } else {
        /* Fail is [], meaning that if none of the values match,
         * we should fall through to the next instruction.
         *
         * We set `next` to a label that will be located after the
         * instructions for the linear search. That label is needed if
         * values are untagged and a type test is emitted to skip the
         * comparisons of the untagged values in case the type is
         * wrong.
         *
         * We intentionally do not initialize the Label `fail`
         * as an indication for emit_optimized_three_way_select() and
         * emit_linear_search() that not branch is needed at the end
         * of the linear search.
         */
        next = a.newLabel();
    }

    auto src = load_source(Src, ARG1);

    auto plan = plan_untag(args);
    auto base = plan.first;
    auto shift = plan.second;

    if (base == 0 && shift == 0) {
        if (!emit_optimized_three_way_select(src.reg, fail, args)) {
            emit_linear_search(src.reg, fail, args);
        }
    } else {
        auto untagged = emit_select_untag(args, src.reg, next, base, shift);

        if (!emit_optimized_three_way_select(ARG1, fail, untagged)) {
            emit_linear_search(ARG1, fail, untagged);
        }
    }

    if (!Fail.isLabel()) {
        bind_veneer_target(next);
    }
}

void BeamModuleAssembler::emit_i_select_val_bins(const ArgVal &Src,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Size,
                                                 const Span<ArgVal> &args) {
    ASSERT(Size.getValue() == args.size());
    int count = args.size() / 2;
    Label fail;

    /* See the comment in emit_i_select_val_lins() for an explanation
     * why we use raw labels. */
    if (Fail.isLabel()) {
        fail = rawLabels[Fail.getValue()];
    } else {
        fail = a.newLabel();
    }

    comment("Binary search in table of %lu elements", count);

    auto src = load_source(Src, ARG1);

    auto plan = plan_untag(args);
    auto base = plan.first;
    auto shift = plan.second;

    if (base == 0 && shift == 0) {
        emit_binsearch_nodes(src.reg, 0, count - 1, fail, args);
    } else {
        auto untagged = emit_select_untag(args, src.reg, fail, base, shift);
        emit_binsearch_nodes(ARG1, 0, count - 1, fail, untagged);
    }

    if (!Fail.isLabel()) {
        bind_veneer_target(fail);
    }
}

/*
 * Emit code for a binary search through an interval Left <= Right of
 * the i_select_val argument vector `args`.
 */
void BeamModuleAssembler::emit_binsearch_nodes(arm::Gp reg,
                                               size_t Left,
                                               size_t Right,
                                               Label fail,
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

        emit_linear_search(reg, fail, shrunk);

        return;
    }

    comment("Subtree [%lu..%lu], pivot %lu", Left, Right, mid);

    check_pending_stubs();

    cmp_arg(reg, midval);

    auto &lbl = args[mid + count];

    /* The search has failed if Left == Right, but that should never
     * happen since we revert to a linear search when there are
     * ten or less elements. */
    ASSERT(Left != Right);
    ASSERT(Left != mid);

    a.cond_eq().b(resolve_beam_label(lbl, disp1MB));

    Label right_tree = a.newLabel();
    a.cond_hs().b(resolve_label(right_tree, disp1MB));

    emit_binsearch_nodes(reg, Left, mid - 1, fail, args);

    bind_veneer_target(right_tree);
    emit_binsearch_nodes(reg, mid + 1, Right, fail, args);
}

void BeamModuleAssembler::emit_i_jump_on_val(const ArgVal &Src,
                                             const ArgVal &Fail,
                                             const ArgVal &Base,
                                             const ArgVal &Size,
                                             const Span<ArgVal> &args) {
    Label fail;
    auto src = load_source(Src, TMP1);

    ASSERT(Size.getValue() == args.size());

    a.and_(TMP3, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP3, imm(_TAG_IMMED1_SMALL));

    if (Fail.isLabel()) {
        a.cond_ne().b(resolve_beam_label(Fail, disp1MB));
    } else {
        /* NIL means fallthrough to the next instruction. */
        ASSERT(Fail.getType() == ArgVal::Immediate && Fail.getValue() == NIL);
        fail = a.newLabel();
        a.cond_ne().b(fail);
    }

    a.asr(TMP1, src.reg, imm(_TAG_IMMED1_SIZE));

    if (Base.getValue() != 0) {
        if (Support::isUInt12((Sint)Base.getValue())) {
            a.sub(TMP1, TMP1, imm(Base.getValue()));
        } else {
            mov_imm(TMP3, Base.getValue());
            a.sub(TMP1, TMP1, TMP3);
        }
    }

    a.cmp(TMP1, imm(args.size()));
    if (Fail.isLabel()) {
        a.cond_hs().b(resolve_beam_label(Fail, disp1MB));
    } else {
        a.cond_hs().b(fail);
    }

    embed_vararg_rodata(args, TMP2);
    a.ldr(TMP3, arm::Mem(TMP2, TMP1, arm::lsl(3)));
    a.br(TMP3);

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
 * Return true if the optimization was possible.
 */
bool BeamModuleAssembler::emit_optimized_three_way_select(
        arm::Gp reg,
        Label fail,
        const Span<ArgVal> &args) {
    if (args.size() != 4 || (args[2].getValue() != args[3].getValue()))
        return false;

    uint64_t x = args[0].getValue();
    uint64_t y = args[1].getValue();
    uint64_t combined = x | y;
    uint64_t diff = x ^ y;
    ArgVal val(ArgVal::Immediate, combined);

    if ((diff & (diff - 1)) != 0) {
        return false;
    }

    comment("(Src == 0x%x || Src == 0x%x) <=> (Src | 0x%x) == 0x%x",
            x,
            y,
            diff,
            combined);

    a.orr(TMP1, reg, imm(diff));
    cmp_arg(TMP1, val);
    a.cond_eq().b(resolve_beam_label(args[2], disp1MB));

    /* An invalid label means fallthrough to the next instruction. */
    if (fail.isValid()) {
        a.b(resolve_label(fail, disp128MB));
    }
    return true;
}
