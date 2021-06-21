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
#include <sstream>
#include <float.h>

#include "beam_asm.hpp"
using namespace asmjit;

static std::string getAtom(Eterm atom) {
    Atom *ap = atom_tab(atom_val(atom));
    return std::string((char *)ap->name, ap->len);
}

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
                                         unsigned num_labels,
                                         BeamFile_ExportTable *named_labels)
        : BeamAssembler(getAtom(mod)) {
    this->ga = ga;
    this->mod = mod;

    _veneers.reserve(num_labels + 1);
    rawLabels.reserve(num_labels + 1);

    if (erts_jit_asm_dump && named_labels) {
        BeamFile_ExportEntry *e = &named_labels->entries[0];
        for (unsigned i = 1; i < num_labels; i++) {
            Label lbl;
            char tmp[512]; // Large enough to hold most realistic
                           // function names. We will truncate too
                           // long names, but as the label name is not
                           // important for the functioning of asmjit
                           // and this functionality is probably only
                           // used by developers, we don't bother with
                           // dynamic allocation.
            // The named_labels are sorted, so no need for a search.
            if ((unsigned)e->label == i) {
                erts_snprintf(tmp, sizeof(tmp), "%T/%d", e->function, e->arity);
                lbl = a.newNamedLabel(tmp);
                labelNames[i] = tmp;
                e++;
            } else {
                std::string lblName = "label_" + std::to_string(i);
                lbl = a.newNamedLabel(lblName.data());
            }
            rawLabels[i] = lbl;
        }
        return;
    }
    if (erts_jit_asm_dump) {
        // There is no naming info, but dumping of the assembly code
        // has been requested, so do the best we can and number the
        // labels.
        for (unsigned i = 1; i < num_labels; i++) {
            std::string lblName = "label_" + std::to_string(i);
            rawLabels[i] = a.newNamedLabel(lblName.data());
        }
        return;
    }
    // No output is requested, go with unnamed labels
    for (unsigned i = 1; i < num_labels; i++)
        rawLabels[i] = a.newLabel();
}

BeamModuleAssembler::BeamModuleAssembler(BeamGlobalAssembler *ga,
                                         Eterm mod,
                                         unsigned num_labels,
                                         unsigned num_functions,
                                         BeamFile_ExportTable *named_labels)
        : BeamModuleAssembler(ga, mod, num_labels, named_labels) {
    codeHeader = a.newLabel();
    a.align(kAlignCode, 8);
    a.bind(codeHeader);

    embed_zeros(sizeof(BeamCodeHeader) +
                sizeof(ErtsCodeInfo *) * num_functions);

    /* Setup the early_nif/breakpoint trampoline. */
    genericBPTramp = a.newLabel();
    a.bind(genericBPTramp);
    {
        Label bp_and_nif = a.newLabel(), bp_only = a.newLabel(),
              nif_only = a.newLabel();

        a.cmp(ARG1, imm(ERTS_ASM_BP_FLAG_BP_NIF_CALL_NIF_EARLY));
        a.cond_eq().b(bp_and_nif);
        ERTS_CT_ASSERT((1 << 0) == ERTS_ASM_BP_FLAG_CALL_NIF_EARLY);
        a.tbnz(ARG1, imm(0), nif_only);
        ERTS_CT_ASSERT((1 << 1) == ERTS_ASM_BP_FLAG_BP);
        a.tbnz(ARG1, imm(1), bp_only);
        a.ret(a64::x30);

        a.bind(bp_and_nif);
        {
            emit_enter_runtime_frame();
            a.bl(resolve_fragment(ga->get_generic_bp_local(), disp128MB));
            emit_leave_runtime_frame();

            a.b(nif_only);
        }

        a.bind(bp_only);
        {
            emit_enter_runtime_frame();
            a.bl(resolve_fragment(ga->get_generic_bp_local(), disp128MB));
            emit_leave_runtime_frame();

            a.ret(a64::x30);
        }

        a.bind(nif_only);
        a.b(resolve_fragment(ga->get_call_nif_early(), disp128MB));
    }

#ifdef DEBUG
    last_stub_check_offset = a.offset();
#endif
}

void BeamModuleAssembler::embed_vararg_rodata(const Span<ArgVal> &args,
                                              a64::Gp reg) {
    /* Short sequences are inlined in the .text section for slightly better
     * speed. */
    bool inlineData = args.size() <= 6;

    Label data = a.newLabel(), next = a.newLabel();

    if (inlineData) {
        a.adr(reg, data);
        a.b(next);
    } else {
        Label pointer = a.newLabel();

        a.ldr(reg, arm::Mem(pointer));
        a.b(next);

        a.align(kAlignCode, 8);
        a.bind(pointer);
        a.embedLabel(data, 8);

        a.section(rodata);
    }

    a.align(kAlignData, 8);
    a.bind(data);

    for (const ArgVal &arg : args) {
        union {
            BeamInstr as_beam;
            char as_char[1];
        } data;

        a.align(kAlignData, 8);
        switch (arg.getType()) {
        case ArgVal::Literal: {
            auto &patches = literals[arg.getValue()].patches;
            Label patch = a.newLabel();

            a.bind(patch);
            a.embedUInt64(LLONG_MAX);
            patches.push_back({patch, 0});
            break;
        }
        case ArgVal::XReg:
            data.as_beam = make_loader_x_reg(arg.getValue());
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
        case ArgVal::YReg:
            data.as_beam = make_loader_y_reg(arg.getValue());
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
        case ArgVal::Label:
            a.embedLabel(rawLabels[arg.getValue()]);
            break;
        case ArgVal::Immediate:
        case ArgVal::Word:
            data.as_beam = arg.getValue();
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
        default:
            ERTS_ASSERT(!"error");
        }
    }

    if (!inlineData) {
        a.section(code.textSection());
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_nif_padding() {
    const size_t minimum_size = sizeof(UWord[BEAM_NATIVE_MIN_FUNC_SZ]);
    size_t prev_func_start, diff;

    prev_func_start = code.labelOffsetFromBase(rawLabels[functions.back() + 1]);
    diff = a.offset() - prev_func_start;

    if (diff < minimum_size) {
        embed_zeros(minimum_size - diff);
    }
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    /* This little prologue is used by nif loading and tracing to insert
     * alternative instructions. */
    Label next = a.newLabel();

    emit_enter_erlang_frame();

    /* This branch is modified to jump to the MOVZ instruction when the
     * breakpoint is enabled. */
    a.b(next);

    /* This instruction is updated with the current flag. */
    a.movz(ARG1, imm(ERTS_ASM_BP_FLAG_NONE));

    if (genericBPTramp.isValid()) {
        a.bl(resolve_label(genericBPTramp, disp128MB));
    } else {
        /* NIF or BIF stub; we're not going to use this trampoline as-is, but
         * we need to reserve space for it. */
        a.udf(0xB1F);
    }

    a.bind(next);

    ASSERT((a.offset() - code.labelOffsetFromBase(currLabel)) ==
           BEAM_ASM_FUNC_PROLOGUE_SIZE);
}

static void i_emit_nyi(char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    emit_enter_runtime(0);

    a.mov(ARG1, imm(msg));
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

    if (getOffset() == last_error_offset) {
        /*
         * The previous PC where an exception may occur is equal to the
         * current offset, which is also the offset of the next
         * instruction. If the next instruction happens to be a
         * line instruction, the location for the exception will
         * be that line instruction, which is probably wrong.
         * To avoid that, bump the instruction offset.
         */
        a.nop();
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
    /* a64::x30 now points 4 bytes into the ErtsCodeInfo struct for the
     * function. Put the address of the MFA into ARG1. */
    a.add(ARG1, a64::x30, offsetof(ErtsCodeInfo, mfa) - 4);
    mov_imm(TMP1, EXC_FUNCTION_CLAUSE);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    a.str(ARG1, arm::Mem(c_p, offsetof(Process, current)));

    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);

    a.b(labels[raise_exception_shared]);
}

void BeamModuleAssembler::emit_i_func_info(const ArgVal &Label,
                                           const ArgVal &Module,
                                           const ArgVal &Function,
                                           const ArgVal &Arity) {
    ErtsCodeInfo info;

    functions.push_back(Label.getValue());

    info.mfa.module = Module.getValue();
    info.mfa.function = Function.getValue();
    info.mfa.arity = Arity.getValue();
    info.u.gen_bp = NULL;

    comment("%T:%T/%d", info.mfa.module, info.mfa.function, info.mfa.arity);

    /* This is an ErtsCodeInfo structure that has a valid ARM opcode as its `op`
     * field, which *calls* the `function_clause` fragment so we can trace it
     * back to this particular function.
     *
     * We avoid using the `fragment_call` helper to ensure a constant layout,
     * as it adds code in certain debug configurations. */
    if (codeHeader.isValid()) {
        a.bl(resolve_fragment(ga->get_i_func_info_shared(), disp128MB));
    } else {
        a.nop();
    }

    a.align(kAlignCode, sizeof(UWord));
    a.embed(&info.u.gen_bp, sizeof(info.u.gen_bp));
    a.embed(&info.mfa, sizeof(info.mfa));
}

void BeamModuleAssembler::emit_label(const ArgVal &Label) {
    currLabel = rawLabels[Label.getValue()];
    bind_veneer_target(currLabel);
}

void BeamModuleAssembler::emit_aligned_label(const ArgVal &Label,
                                             const ArgVal &Alignment) {
    ASSERT(Alignment.isWord());
    a.align(kAlignCode, Alignment.getValue());
    emit_label(Label);
}

void BeamModuleAssembler::emit_on_load() {
    on_load = currLabel;
}

void BeamModuleAssembler::bind_veneer_target(const Label &target) {
    auto veneer_range = _veneers.equal_range(target.id());
    for (auto it = veneer_range.first; it != veneer_range.second; it++) {
        const Veneer &veneer = it->second;

        ASSERT(veneer.target == target);

        if (!code.isLabelBound(veneer.anchor)) {
            ASSERT(a.offset() <= veneer.latestOffset);
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
    flush_pending_stubs(_dispatchTable.size() * sizeof(Uint32[8]) +
                        dispUnknown);

    for (auto pair : _dispatchTable) {
        bind_veneer_target(pair.second);

        a.mov(SUPER_TMP, imm(pair.first));
        a.br(SUPER_TMP);
    }

    /* Emit all remaining stubs. */
    flush_pending_stubs(dispMax);
}

void BeamModuleAssembler::emit_line(const ArgVal &) {
    /*
     * There is no need to align the line instruction. In the loaded
     * code, the type of the pointer will be void* and that pointer
     * will only be used in comparisons.
     */
}

void BeamModuleAssembler::emit_func_line(const ArgVal &Loc) {
    emit_line(Loc);
}

void BeamModuleAssembler::emit_empty_func_line() {
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

void BeamModuleAssembler::emit_trace_jump(const ArgVal &) {
    emit_nyi("trace_jump should never be called");
}

void BeamModuleAssembler::emit_call_error_handler() {
    emit_nyi("call_error_handler should never be called");
}

unsigned BeamModuleAssembler::patchCatches(char *rw_base) {
    unsigned catch_no = BEAM_CATCHES_NIL;

    for (const auto &c : catches) {
        const auto &patch = c.patch;
        ErtsCodePtr handler;

        handler = (ErtsCodePtr)getCode(c.handler);
        catch_no = beam_catches_cons(handler, catch_no, nullptr);

        /* Patch the `mov` instruction with the catch tag */
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset];

        ASSERT(LLONG_MAX == *where);
        Eterm catch_term = make_catch(catch_no);

        /* With the current tag scheme, more than 33 million
         * catches can exist at once. */
        ERTS_ASSERT(catch_term >> 31 == 0);

        *where = catch_term;
    }

    return catch_no;
}

void BeamModuleAssembler::patchImport(char *rw_base,
                                      unsigned index,
                                      BeamInstr I) {
    for (const auto &patch : imports[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset];

        ASSERT(LLONG_MAX == *where);
        *where = I + patch.val_offs;
    }
}

void BeamModuleAssembler::patchLambda(char *rw_base,
                                      unsigned index,
                                      BeamInstr I) {
    for (const auto &patch : lambdas[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset];

        ASSERT(LLONG_MAX == *where);
        *where = I + patch.val_offs;
    }
}

void BeamModuleAssembler::patchLiteral(char *rw_base,
                                       unsigned index,
                                       Eterm lit) {
    for (const auto &patch : literals[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset];

        ASSERT(LLONG_MAX == *where);
        *where = lit + patch.val_offs;
    }
}

void BeamModuleAssembler::patchStrings(char *rw_base,
                                       const byte *string_table) {
    for (const auto &patch : strings) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (const byte **)&rw_base[offset];

        ASSERT(LLONG_MAX == (Eterm)*where);
        *where = string_table + patch.val_offs;
    }
}

Label BeamModuleAssembler::resolve_beam_label(const ArgVal &Lbl,
                                              enum Displacement disp) {
    ASSERT(Lbl.isLabel());
    auto it = labelNames.find(Lbl.getValue());
    if (it != labelNames.end()) {
        return resolve_label(rawLabels[Lbl.getValue()], disp, &it->second);
    } else {
        return resolve_label(rawLabels[Lbl.getValue()], disp);
    }
}

Label BeamModuleAssembler::resolve_label(Label target,
                                         enum Displacement disp,
                                         std::string *labelName) {
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
        name << '@' << *labelName << '-' << labelSeq++;
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

Label BeamModuleAssembler::resolve_fragment(void (*fragment)(),
                                            enum Displacement disp) {
    Label target;

    auto it = _dispatchTable.find(fragment);

    if (it != _dispatchTable.end()) {
        target = it->second;
    } else {
        target = a.newLabel();
        _dispatchTable.emplace(fragment, target);
    }

    return resolve_label(target, disp);
}

arm::Mem BeamModuleAssembler::embed_constant(const ArgVal &value,
                                             enum Displacement disp) {
    ssize_t currOffset = a.offset();

    ssize_t minOffset = currOffset - disp;
    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);
    ASSERT(value.isConstant());

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
    ASSERT((last_stub_check_offset + dispMin) >= currOffset);

    if ((last_stub_check_offset + STUB_CHECK_INTERVAL) < currOffset) {
        last_stub_check_offset = currOffset;

        flush_pending_stubs(STUB_CHECK_INTERVAL * 2);
    }
}

void BeamModuleAssembler::flush_pending_stubs(size_t range) {
    size_t effective_offset;
    Label next;

    effective_offset = a.offset() + range;

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
    const Label &anchor = veneer.anchor;
    const Label &target = veneer.target;
    bool directBranch;

    ASSERT(!code.isLabelBound(anchor));
    a.bind(anchor);

    /* Prefer direct branches when possible. */
    if (code.isLabelBound(target)) {
        auto targetOffset = code.labelOffsetFromBase(target);
        directBranch = (a.offset() - targetOffset) <= disp128MB;
    } else {
        directBranch = false;
    }

#ifdef DEBUG
    directBranch &= (a.offset() % 512) >= 256;
#endif

    if (ERTS_LIKELY(directBranch)) {
        a.b(target);
    } else {
        Label pointer = a.newLabel();

        a.ldr(SUPER_TMP, arm::Mem(pointer));
        a.br(SUPER_TMP);

        a.align(kAlignCode, 8);
        a.bind(pointer);
        a.embedLabel(veneer.target);
    }
}

void BeamModuleAssembler::emit_constant(const Constant &constant) {
    const Label &anchor = constant.anchor;
    const ArgVal &value = constant.value;
    auto rawValue = value.getValue();

    ASSERT(!code.isLabelBound(anchor));
    a.align(kAlignData, 8);
    a.bind(anchor);

    if (value.isImmed() || value.isWord()) {
        a.embedUInt64(rawValue);
    } else if (value.isLabel()) {
        a.embedLabel(rawLabels[rawValue]);
    } else {
        a.embedUInt64(LLONG_MAX);

        switch (value.getType()) {
        case ArgVal::BytePtr:
            strings.push_back({anchor, rawValue});
            break;
        case ArgVal::Catch: {
            auto handler = rawLabels[rawValue];
            catches.push_back({{anchor, 0}, handler});
            break;
        }
        case ArgVal::Export:
            imports[rawValue].patches.push_back({anchor, 0});
            break;
        case ArgVal::FunEntry:
            lambdas[rawValue].patches.push_back({anchor, 0});
            break;
        case ArgVal::Literal:
            literals[rawValue].patches.push_back({anchor, 0});
            break;
        default:
            ASSERT(!"error");
        }
    }
}
