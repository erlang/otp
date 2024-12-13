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
}

void BeamGlobalAssembler::emit_i_breakpoint_trampoline_shared() {
    // TODO
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    // TODO
}

static void i_emit_nyi(char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    // TODO
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
}

void BeamModuleAssembler::emit_i_func_info(const ArgWord &Label,
                                           const ArgAtom &Module,
                                           const ArgAtom &Function,
                                           const ArgWord &Arity) {

    /* `op_i_func_info_IaaI` is used in various places in the emulator, so this
     * label is always encoded as a word, even though the signature ought to
     * be `op_i_func_info_LaaI`. */
    functions.push_back(Label.get());

    // TODO
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
    // TODO
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
    // TODO
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

void BeamModuleAssembler::emit_i_flush_stubs() {
        // TODO
}

void BeamModuleAssembler::check_pending_stubs() {
    size_t currOffset = a.offset();

    /* We shouldn't let too much space pass between checks. */
    ASSERT((last_stub_check_offset + dispMin) >= currOffset);

    if ((last_stub_check_offset + STUB_CHECK_INTERVAL) < currOffset) {
        last_stub_check_offset = currOffset;

        flush_pending_stubs(STUB_CHECK_INTERVAL * 2);
    }
}

void BeamModuleAssembler::flush_pending_stubs(size_t range) {
    ssize_t effective_offset = a.offset() + range;
    Label next;

    while (!_pending_veneers.empty()) {
        const Veneer &veneer = _pending_veneers.top();

        if (veneer.latestOffset > effective_offset) {
            break;
        }

        if (!code.isLabelBound(veneer.anchor)) {
            if (!next.isValid()) {
                next = a.newLabel();

                comment("Begin stub section");
                a.b(next);
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
            a.b(next);
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

void BeamModuleAssembler::emit_veneer(const Veneer &veneer) {
        // TODO
}

void BeamModuleAssembler::emit_constant(const Constant &constant) {
        // TODO
}
