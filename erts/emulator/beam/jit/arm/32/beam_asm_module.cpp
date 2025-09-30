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

#include <algorithm>
#include <sstream>
#include <float.h>

#include "beam_asm.hpp"
using namespace asmjit;

#ifdef BEAMASM_DUMP_SIZES
#    include <mutex>

typedef std::pair<Uint64, Uint64> op_stats;

static std::unordered_map<char *, op_stats> sizes;
static std::mutex size_lock;

extern "C" void beamasm_dump_sizes() {
    std::lock_guard<std::mutex> lock(size_lock);

    std::vector<std::pair<char *, op_stats>> flat(sizes.cbegin(), sizes.cend());
    double total_size = 0.0;

    for (const auto &op : flat) {
        total_size += op.second.second;
    }

    /* Sort instructions by total size, in descending order. */
    std::sort(
            flat.begin(),
            flat.end(),
            [](std::pair<char *, op_stats> &a, std::pair<char *, op_stats> &b) {
                return a.second.second > b.second.second;
            });

    for (const auto &op : flat) {
        fprintf(stderr,
                "%34s:\t%zu\t%f\t%zu\t%zu\r\n",
                op.first,
                op.second.second,
                op.second.second / total_size,
                op.second.first,
                op.second.first ? (op.second.second / op.second.first) : 0);
    }
}
#endif

ErtsCodePtr BeamModuleAssembler::getCode(BeamLabel label) {
    ASSERT(label < rawLabels.size() + 1);
    return (ErtsCodePtr)getCode(rawLabels[label]);
}

ErtsCodePtr BeamModuleAssembler::getLambda(unsigned index) {
    const auto &lambda = lambdas[index];
    return (ErtsCodePtr)getCode(lambda.trampoline);
}

BeamModuleAssembler::BeamModuleAssembler(BeamGlobalAssembler *ga,
                                         Eterm mod,
                                         int num_labels,
                                         int num_functions,
                                         const BeamFile *file)
        : BeamModuleAssembler(ga, mod, num_labels, file) {
    _veneers.reserve(num_labels + 1);

    code_header = a.newLabel();
    a.align(AlignMode::kCode, 8);
    a.bind(code_header);

    embed_zeros(sizeof(BeamCodeHeader) +
                sizeof(ErtsCodeInfo *) * num_functions);

#ifdef DEBUG
    last_stub_check_offset = a.offset();
#endif
}

void BeamModuleAssembler::emit_i_nif_padding() {
    // TODO
    emit_nyi("emit_i_nif_padding");
}

void BeamGlobalAssembler::emit_i_breakpoint_trampoline_shared() {
    // TODO
    emit_nyi("emit_i_breakpoint_trampoline_shared");
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    /* This little prologue is used by nif loading and tracing to insert
     * alternative instructions. */
    Label next = a.newLabel();

    emit_enter_erlang_frame();
    /* This branch is modified to jump to the BL instruction when the
     * breakpoint is enabled. */
    a.b(next);

    if (code_header.isValid()) {
        a.bl(resolve_fragment(ga->get_i_breakpoint_trampoline_shared(),
                              disp32MB));
    } else {
        /* NIF or BIF stub; we're not going to use this trampoline as-is, but
         * we need to reserve space for it. */
        a.udf(0xB1F);
    }

    a.bind(next);

    ASSERT((a.offset() - code.labelOffsetFromBase(current_label)) ==
            BEAM_ASM_FUNC_PROLOGUE_SIZE);
}

static void i_emit_nyi(char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    // skipping any preparation for the runtime call
    mov_imm(ARG1, msg);
    runtime_call<1>(i_emit_nyi);
    /* Never returns */
}

void BeamModuleAssembler::emit_nyi() {
    emit_nyi("<unspecified>");
}

bool BeamModuleAssembler::emit(unsigned specific_op, const Span<ArgVal> &args) {
    check_pending_stubs();

#ifdef BEAMASM_DUMP_SIZES
    size_t before = a.offset();
#endif

    comment(opc[specific_op].name);

#define InstrCnt()
    switch (specific_op) {
#include "beamasm_emit.h"
    default:
        ERTS_ASSERT(0 && "Invalid instruction");
        break;
    }

#ifdef BEAMASM_DUMP_SIZES
    {
        std::lock_guard<std::mutex> lock(size_lock);

        sizes[opc[specific_op].name].first++;
        sizes[opc[specific_op].name].second += a.offset() - before;
    }
#endif

    return true;
}

/*
 * Here follows meta instructions.
 */

void BeamGlobalAssembler::emit_i_func_info_shared() {
    // TODO
    emit_nyi("emit_i_func_info_shared");
}

void BeamModuleAssembler::emit_i_func_info(const ArgWord &Label,
                                           const ArgAtom &Module,
                                           const ArgAtom &Function,
                                           const ArgWord &Arity) {
    ErtsCodeInfo info = {};

    /* `op_i_func_info_IaaI` is used in various places in the emulator, so this
     * label is always encoded as a word, even though the signature ought to
     * be `op_i_func_info_LaaI`. */
    functions.push_back(Label.get());

    info.mfa.module = Module.get();
    info.mfa.function = Function.get();
    info.mfa.arity = Arity.get();

    comment("%T:%T/%d", info.mfa.module, info.mfa.function, info.mfa.arity);

    /* This is an ErtsCodeInfo structure that has a valid ARM opcode as its `op`
     * field, which *calls* the `raise_function_clause` fragment so we can trace
     * it back to this particular function.
     *
     * We also use this field to store the current breakpoint flag, as ARM is a
     * bit more strict about modifying code than x86: only branch instructions
     * can be safely modified without issuing an ISB. By storing the flag here
     * and reading it in the fragment, we don't have to change any code other
     * than the branch instruction. */
    if (code_header.isValid()) {
        /* We avoid using the `fragment_call` helper to ensure a constant
         * layout, as it adds code in certain debug configurations. */
        a.blx(resolve_fragment(ga->get_i_func_info_shared(), disp32MB));
    } else {
        a.udf(0xF1F0);
    }
    ERTS_CT_ASSERT(ERTS_ASM_BP_FLAG_NONE == 0);
    /*
      On Arm32 we do not need to add padding space here.
      The metadata struct inside ErtsCodeInfo is 4 bytes long (UInt).
      The BL instruction is already 4 bytes.
    */
    ASSERT(a.offset() % sizeof(UWord) == 0);
    a.embed(&info.gen_bp, sizeof(info.gen_bp));
    a.embed(&info.mfa, sizeof(info.mfa));
}

void BeamModuleAssembler::emit_label(const ArgLabel &Label) {
    ASSERT(Label.isLabel());

    current_label = rawLabels[Label.get()];
    bind_veneer_target(current_label);

    reg_cache.invalidate();
}

void BeamModuleAssembler::emit_aligned_label(const ArgLabel &Label,
                                             const ArgWord &Alignment) {
    a.align(AlignMode::kCode, Alignment.get());
    emit_label(Label);
}

void BeamModuleAssembler::emit_on_load() {
    // TODO
    emit_nyi("emit_on_load");
}

void BeamModuleAssembler::bind_veneer_target(const Label &target) {
    auto veneer_range = _veneers.equal_range(target.id());
    for (auto it = veneer_range.first; it != veneer_range.second; it++) {
        const Veneer &veneer = it->second;

        ASSERT(veneer.target == target);

        if (!code.isLabelBound(veneer.anchor)) {
            ASSERT((ssize_t)a.offset() <= veneer.latestOffset);
            a.bind(veneer.anchor);

            /* TODO: remove from pending stubs? */
        }
    }

    a.bind(target);
}

void BeamModuleAssembler::emit_int_code_end() {
    /* This label is used to figure out the end of the last function */
    code_end = a.newLabel();
    a.bind(code_end);

    emit_nyi("int_code_end");

    /* We emit the dispatch table before all remaining stubs to bind veneers
     * directly in the table itself, avoiding a painful extra jump.
     *
     * Since the table is potentially very large, we'll emit all stubs that are
     * due within it so we won't have to check on every iteration. */
    mark_unreachable();
    flush_pending_stubs(_dispatchTable.size() * sizeof(Uint32[8]) +
                        dispUnknown);

    for (auto pair : _dispatchTable) {
        bind_veneer_target(pair.second);

        mov_imm(TMP, pair.first);
        a.blx(TMP);
    }

    mark_unreachable();

    /* Emit all remaining stubs. */
    flush_pending_stubs(dispMax);
}

void BeamModuleAssembler::emit_line(const ArgWord &Loc) {
    /* There is no need to align the line instruction. In the loaded code, the
     * type of the pointer will be void* and that pointer will only be used in
     * comparisons.
     *
     * We only need to do something when there's a possibility of raising an
     * exception at the very end of the preceding instruction (and thus
     * pointing at the start of this one). If we were to do nothing, the error
     * would erroneously refer to this instead of the preceding line.
     *
     * Since line addresses are taken _after_ line instructions we can avoid
     * this by adding a nop when we detect this condition. */
    if (a.offset() == last_error_offset) {
        a.nop();
    }
}

void BeamModuleAssembler::emit_func_line(const ArgWord &Loc) {
    emit_line(Loc);
}

void BeamModuleAssembler::emit_empty_func_line() {
}

void BeamModuleAssembler::emit_executable_line(const ArgWord &Loc,
                                               const ArgWord &Index) {
}

/*
 * Here follows stubs for instructions that should never be called.
 */

void BeamModuleAssembler::emit_i_debug_breakpoint() {
    emit_nyi("i_debug_breakpoint should never be called");
}

void BeamModuleAssembler::emit_i_generic_breakpoint() {
    emit_nyi("i_generic_breakpoint should never be called");
}

void BeamModuleAssembler::emit_trace_jump(const ArgWord &) {
    emit_nyi("trace_jump should never be called");
}

void BeamModuleAssembler::emit_call_error_handler() {
    emit_nyi("call_error_handler should never be called");
}

const Label &BeamModuleAssembler::resolve_label(const Label &target,
                                                enum Displacement disp,
                                                const char *labelName) {
    ssize_t currOffset = a.offset();

    ssize_t minOffset = currOffset - disp;
    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);
    ASSERT(target.isValid());

    if (code.isLabelBound(target)) {
        ssize_t targetOffset = code.labelOffsetFromBase(target);

        /* Backward reference: skip veneers if it's already in range. */
        if (targetOffset >= minOffset) {
            return target;
        }
    }

    /* If a previously created veneer is reachable from this point, we can use
     * it instead of creating a new one. */
    auto range = _veneers.equal_range(target.id());
    for (auto it = range.first; it != range.second; it++) {
        const Veneer &veneer = it->second;

        if (code.isLabelBound(veneer.anchor)) {
            ssize_t veneerOffset = code.labelOffsetFromBase(veneer.anchor);

            if (veneerOffset >= minOffset && veneerOffset <= maxOffset) {
                return veneer.anchor;
            }
        } else if (veneer.latestOffset <= maxOffset) {
            return veneer.anchor;
        }
    }

    Label anchor;

    if (!labelName) {
        anchor = a.newLabel();
    } else {
        /* This is the entry label for a function. Create an unique
         * name for the anchor label. It is necessary to include a
         * sequence number in the label name because if the module is
         * huge more than one veneer can be created for each entry
         * label. */
        std::stringstream name;
        name << '@' << labelName << '-' << labelSeq++;
        anchor = a.newNamedLabel(name.str().c_str());
    }

    auto it = _veneers.emplace(target.id(),
                               Veneer{.latestOffset = maxOffset,
                                      .anchor = anchor,
                                      .target = target});

    const Veneer &veneer = it->second;
    _pending_veneers.emplace(veneer);

    return veneer.anchor;
}

const Label &BeamModuleAssembler::resolve_fragment(void (*fragment)(),
                                                   enum Displacement disp) {
    auto it = _dispatchTable.find(fragment);

    if (it == _dispatchTable.end()) {
        it = _dispatchTable.emplace(fragment, a.newLabel()).first;
    }

    return resolve_label(it->second, disp);
}

arm::Mem BeamModuleAssembler::embed_constant(const ArgVal &value,
                                             enum Displacement disp) {
    ssize_t currOffset = a.offset();

    ssize_t minOffset = currOffset - disp;
    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);
    ASSERT(!value.isRegister());

    /* If a previously embedded constant is reachable from this point, we
     * can use it instead of creating a new one. */
    auto range = _constants.equal_range(value);
    for (auto it = range.first; it != range.second; it++) {
        const Constant &constant = it->second;

        if (code.isLabelBound(constant.anchor)) {
            ssize_t constOffset = code.labelOffsetFromBase(constant.anchor);

            if (constOffset >= minOffset && constOffset <= maxOffset) {
                return arm::Mem(constant.anchor);
            }
        } else if (constant.latestOffset <= maxOffset) {
            return arm::Mem(constant.anchor);
        }
    }

    auto it = _constants.emplace(value,
                                 Constant{.latestOffset = maxOffset,
                                          .anchor = a.newLabel(),
                                          .value = value});
    const Constant &constant = it->second;
    _pending_constants.emplace(constant);

    return arm::Mem(constant.anchor);
}

void BeamModuleAssembler::emit_i_flush_stubs() {
    /* Flush all stubs that are due within the next two check intervals
     * to prevent them from being emitted inside function prologues or
     * NIF padding. */
   flush_pending_stubs(STUB_CHECK_INTERVAL * 2);
   last_stub_check_offset = a.offset();
}

void BeamModuleAssembler::check_pending_stubs() {
    size_t currOffset = a.offset();

    /* We shouldn't let too much space pass between checks. */
    if (last_stub_check_offset + STUB_CHECK_INTERVAL < currOffset ||
        (is_unreachable() &&
         last_stub_check_offset + STUB_CHECK_INTERVAL_UNREACHABLE <
                 currOffset)) {
        last_stub_check_offset = currOffset;

        flush_pending_stubs(STUB_CHECK_INTERVAL * 2);
    }

    if (is_unreachable()) {
        flush_pending_labels();
    }
}

void BeamModuleAssembler::flush_pending_stubs(size_t range) {
    ssize_t effective_offset = a.offset() + range;
    Label next;

    if (!_pending_labels.empty()) {
        next = a.newLabel();

        comment("Begin stub section");
        if (!is_unreachable()) {
            a.b(next);
        }

        flush_pending_labels();
    }

    while (!_pending_veneers.empty()) {
        const Veneer &veneer = _pending_veneers.top();

        if (veneer.latestOffset > effective_offset) {
            break;
        }

        if (!code.isLabelBound(veneer.anchor)) {
            if (!next.isValid()) {
                next = a.newLabel();

                comment("Begin stub section");
                if (!is_unreachable()) {
                    a.b(next);
                }
            }

            emit_veneer(veneer);

            effective_offset = a.offset() + range;
        }

        _pending_veneers.pop();
    }

    while (!_pending_constants.empty()) {
        const Constant &constant = _pending_constants.top();

        if (constant.latestOffset > effective_offset) {
            break;
        }

        /* Unlike veneers, we never bind constants ahead of time. */
        ASSERT(!code.isLabelBound(constant.anchor));

        if (!next.isValid()) {
            next = a.newLabel();

            comment("Begin stub section");
            if (!is_unreachable()) {
                a.b(next);
            }
        }

        emit_constant(constant);

        effective_offset = a.offset() + range;

        _pending_constants.pop();
    }

    if (next.isValid()) {
        comment("End stub section");
        a.bind(next);
    }
}

void BeamModuleAssembler::flush_pending_labels() {
    if (!_pending_labels.empty()) {
        a.align(AlignMode::kCode, 4);
    }

    while (!_pending_labels.empty()) {
        const EmbeddedLabel &embedded_label = _pending_labels.top();

        a.bind(embedded_label.anchor);
        a.embedLabel(embedded_label.label, 8);

        _pending_labels.pop();
    }
}

void BeamModuleAssembler::emit_veneer(const Veneer &veneer) {
    // TODO
    ASSERT(false);
    emit_nyi("emit_veneer");
}

void BeamModuleAssembler::emit_constant(const Constant &constant) {
    const Label &anchor = constant.anchor;
    const ArgVal &value = constant.value;

    ASSERT(!code.isLabelBound(anchor));
    a.align(AlignMode::kData, 4);
    a.bind(anchor);

    ASSERT(!value.isRegister());

    if (value.isImmed()) {
        a.embedUInt32(value.as<ArgImmed>().get());
    } else if (value.isWord()) {
        a.embedUInt32(value.as<ArgWord>().get());
    } else if (value.isLabel()) {
        a.embedLabel(rawLabels.at(value.as<ArgLabel>().get()));
    } else {
        switch (value.getType()) {
        case ArgVal::BytePtr:
            strings.push_back({anchor, 0, value.as<ArgBytePtr>().get()});
            a.embedUInt32(INT_MAX);
            break;
        case ArgVal::Catch: {
            auto handler = rawLabels[value.as<ArgCatch>().get()];
            catches.push_back({{anchor, 0, 0}, handler});
            a.embedUInt64(INT_MAX);
            break;
        }
        case ArgVal::Export: {
            auto index = value.as<ArgExport>().get();
            imports[index].patches.push_back({anchor, 0, 0});
            a.embedUInt64(INT_MAX);
            break;
        }
        case ArgVal::FunEntry: {
            auto index = value.as<ArgLambda>().get();
            lambdas[index].patches.push_back({anchor, 0, 0});
            a.embedUInt64(INT_MAX);
            break;
        }
        case ArgVal::Literal: {
            auto index = value.as<ArgLiteral>().get();
            literals[index].patches.push_back({anchor, 0, 0});
            a.embedUInt64(INT_MAX);
            break;
        }
        default:
            ASSERT(!"error");
        }
    }
}
