/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
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

#include <algorithm>
#include <cstring>
#include <sstream>
#include <float.h>

#include "beam_asm.hpp"
extern "C"
{
#include "beam_bp.h"
}

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

    code_header = a.new_label();
    a.align(AlignMode::kCode, 8);
    a.bind(code_header);

    embed_zeros(sizeof(BeamCodeHeader) +
                sizeof(ErtsCodeInfo *) * num_functions);

#ifdef DEBUG
    last_stub_check_offset = a.offset();
#endif
}

void BeamModuleAssembler::embed_vararg_rodata(const Span<const ArgVal> &args,
                                              a64::Gp reg) {
    /* Short sequences are inlined in the .text section for slightly better
     * speed. */
    bool inlineData = args.size() <= 6;

    Label data = a.new_label(), next = a.new_label();

    if (inlineData) {
        a.adr(reg, data);
        a.b(next);
    } else {
        a.ldr(reg, embed_label(data, disp32K));

        a.section(rodata);
    }

    a.align(AlignMode::kData, 8);
    a.bind(data);

    for (const ArgVal &arg : args) {
        a.align(AlignMode::kData, 8);
        switch (arg.getType()) {
        case ArgVal::Type::Literal: {
            auto &patches = literals[arg.as<ArgLiteral>().get()].patches;
            Label patch = a.new_label();

            a.bind(patch);
            a.embed_uint64(LLONG_MAX);
            patches.push_back({patch, 0});
            break;
        }
        case ArgVal::Type::XReg:
            a.embed_uint64(make_loader_x_reg(arg.as<ArgXRegister>().get()));
            break;
        case ArgVal::Type::YReg:
            a.embed_uint64(make_loader_y_reg(arg.as<ArgYRegister>().get()));
            break;
        case ArgVal::Type::Label:
            a.embed_label(rawLabels[arg.as<ArgLabel>().get()]);
            break;
        case ArgVal::Type::Immediate:
            a.embed_uint64(arg.as<ArgImmed>().get());
            break;
        case ArgVal::Type::Word:
            a.embed_uint64(arg.as<ArgWord>().get());
            break;
        default:
            ERTS_ASSERT(!"error");
        }
    }

    if (!inlineData) {
        a.section(code.text_section());
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_nif_padding() {
    const size_t minimum_size = sizeof(UWord[BEAM_NATIVE_MIN_FUNC_SZ]);
    size_t prev_func_start, diff;

    prev_func_start =
            code.label_offset_from_base(rawLabels[functions.back() + 1]);
    diff = a.offset() - prev_func_start;

    if (diff < minimum_size) {
        embed_zeros(minimum_size - diff);
    }
}

void BeamGlobalAssembler::emit_i_breakpoint_trampoline_shared() {
    constexpr ssize_t flag_offset =
            sizeof(ErtsCodeInfo) + BEAM_ASM_FUNC_PROLOGUE_SIZE -
            offsetof(ErtsCodeInfo, u.metadata.breakpoint_flag);

    Label bp_and_nif = a.new_label(), bp_only = a.new_label(),
          nif_only = a.new_label();

    a.ldrb(ARG1.w(), a64::Mem(a64::x30, -flag_offset));

    a.cmp(ARG1, imm(ERTS_ASM_BP_FLAG_BP_NIF_CALL_NIF_EARLY));
    a.b_eq(bp_and_nif);
    ERTS_CT_ASSERT((1 << 0) == ERTS_ASM_BP_FLAG_CALL_NIF_EARLY);
    a.tbnz(ARG1, imm(0), nif_only);
    ERTS_CT_ASSERT((1 << 1) == ERTS_ASM_BP_FLAG_BP);
    a.tbnz(ARG1, imm(1), bp_only);

#ifndef DEBUG
    a.ret(a64::x30);
#else
    Label error = a.new_label();

    /* ARG1 must be a valid breakpoint flag. */
    a.cbnz(ARG1, error);
    a.ret(a64::x30);

    a.bind(error);
    a.udf(0xBC0D);
#endif

    a.bind(bp_and_nif);
    {
        emit_enter_runtime_frame();
        a.bl(labels[generic_bp_local]);
        emit_leave_runtime_frame();

        /* !! FALL THROUGH !! */
    }

    a.bind(nif_only);
    {
        /* call_nif_early returns on its own, unlike generic_bp_local. */
        a.b(labels[call_nif_early]);
    }

    a.bind(bp_only);
    {
        emit_enter_runtime_frame();
        a.bl(labels[generic_bp_local]);
        emit_leave_runtime_frame();

        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    /* This little prologue is used by nif loading and tracing to insert
     * alternative instructions. */
    Label next = a.new_label();

    emit_enter_erlang_frame();

    /* This branch is modified to jump to the BL instruction when the
     * breakpoint is enabled. */
    a.b(next);

    if (code_header.is_valid()) {
        a.bl(resolve_fragment(ga->get_i_breakpoint_trampoline_shared(),
                              disp128MB));
    } else {
        /* NIF or BIF stub; we're not going to use this trampoline as-is, but
         * we need to reserve space for it. */
        a.udf(0xB1F);
    }

    a.bind(next);

    ASSERT((a.offset() - code.label_offset_from_base(current_label)) ==
           BEAM_ASM_FUNC_PROLOGUE_SIZE);
}

void BeamGlobalAssembler::emit_i_line_breakpoint_trampoline_shared() {
    Label exit_trampoline = a.new_label();
    Label dealloc_and_exit_trampoline = a.new_label();
    Label after_gc_check = a.new_label();
    Label dispatch_call = a.new_label();

    const auto &saved_live = TMP_MEM1q;
    const auto &saved_pc = TMP_MEM2q;
    const auto &saved_stack_needed = TMP_MEM3q;

    emit_enter_erlang_frame();

    /* NB. TMP1 = live */
    a.str(TMP1, saved_live); /* stash live */

    /* Pass return address of trampoline, will be used to find current function
     * info */
    a.sub(ARG1, a64::x30, imm(8)); /* ARG1 := pc */
    a.str(ARG1, saved_pc);         /* Stash pc */

    /* START allocate live live */
    a.mov(ARG4, TMP1);         /* ARG4 := live */
    a.lsl(TMP1, TMP1, imm(3)); /* TMP1 := stack-needed = live * sizeof(Eterm) */
    a.str(TMP1, saved_stack_needed); /* stash stack-needed */
    a.add(ARG3,
          TMP1,
          imm(S_RESERVED *
              8)); /* ARG3 := stack-needed + S_RESERVED * sizeof(Eterm) */

    a.add(ARG3, ARG3, HTOP);
    a.cmp(ARG3, E);
    a.b_ls(after_gc_check);

    /* gc needed */
    aligned_call(labels[garbage_collect]);
    a.ldr(TMP1, saved_stack_needed); /* TMP1 := (stashed) stack-needed */
    a.bind(after_gc_check);

    a.sub(E, E, TMP1);
    /* END allocate live live */

    a.mov(ARG1, c_p);
    a.ldr(ARG2, saved_pc); /* pc */
    a.ldr(ARG3, saved_live);
    load_x_reg_array(ARG4);
    a.mov(ARG5, E); /* stk */

    emit_enter_runtime<Update::eXRegs>();
    runtime_call<
            const Export *(*)(Process *, ErtsCodePtr, Uint, Eterm *, UWord *),
            erts_line_breakpoint_hit__prepare_call>();
    emit_leave_runtime<Update::eXRegs>();

    /* If non-null, ARG1 points to error_handler:breakpoint/4 */
    a.cbnz(ARG1, dispatch_call);
    a.ldr(ARG1, saved_stack_needed); /* ARG1 := (stashed) stack-needed */
    a.b(dealloc_and_exit_trampoline);

    a.bind(dispatch_call);
    erlang_call(emit_setup_dispatchable_call(ARG1));

    a.bind(labels[i_line_breakpoint_cleanup]);
    load_x_reg_array(ARG1);
    a.mov(ARG2, E); /* stk */

    emit_enter_runtime<Update::eXRegs>();
    runtime_call<Uint (*)(Eterm *, UWord *),
                 erts_line_breakpoint_hit__cleanup>();
    emit_leave_runtime<Update::eXRegs>();

    a.lsl(ARG1, ARG1, imm(3)); /* ARG1 = stack-needed */

    a.bind(dealloc_and_exit_trampoline); /* ASUMES ARG1 = stack-needed */
    a.add(E, E, ARG1);

    a.bind(exit_trampoline);
    emit_leave_erlang_frame();
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_i_line_breakpoint_trampoline() {
    /* This prologue is used to implement line-breakpoints. The "b next" can
     * be replaced by nops when the breakpoint is enabled, which will instead
     * trigger the breakpoint when control goes through here */
    Label next = a.new_label();
    a.b(next);

    a.bl(resolve_fragment(ga->get_i_line_breakpoint_trampoline_shared(),
                          disp128MB));

    a.bind(next);
}

enum erts_is_line_breakpoint BeamGlobalAssembler::is_line_breakpoint_trampoline(
        ErtsCodePtr addr) {
    auto pc = static_cast<const int32_t *>(addr);
    enum erts_is_line_breakpoint line_bp_type;

    /* The b and bl opcodes take 6 bits, the remaining 26 bits are a
     * a signed offset, given in 32-bit words. */
    const auto opcode6_mask = 0xFC000000;
    const auto b_opcode = 0x14000000;
    const auto bl_opcode = 0x94000000;

    int32_t instr = *pc;
    switch (instr) {
    /* B .next .enabled: BL breakpoint_handler, .next: */
    case b_opcode | 2:
        line_bp_type = IS_DISABLED_LINE_BP;
        break;

    /* B .enabled .enabled: BL breakpoint_handler, .next: */
    case b_opcode | 1:
        line_bp_type = IS_ENABLED_LINE_BP;
        break;

    default:
        return IS_NOT_LINE_BP;
    }

    instr = *++pc;

    /* We expect a bl here. The target is a signed 26-bit offset */
    if ((instr & opcode6_mask) != bl_opcode) {
        return IS_NOT_LINE_BP;
    }
    const int32_t bl_offset = (instr << 6) >> 6;

    /* Offset is expressed in 32-bit words, not bytes */
    pc = pc + bl_offset;

    const auto expected_target = get_i_line_breakpoint_trampoline_shared();
    if (pc == (const int32_t *)expected_target)
        return line_bp_type;

    /* Now we expect to be in a veneer, that will encode a jump
     * to the actual function based on the distance to the pc
     * This can be a direct branch if close enough or branch-to-register after
     * loading the expected_address (see emit_veneer() method) */
    instr = *pc;

    if ((instr & opcode6_mask) == b_opcode) {
        /* using relative branch when expected_target is close enough */
        const int32_t b_offset = (instr << 6) >> 6;
        return (pc + b_offset == (const int32_t *)expected_target)
                       ? line_bp_type
                       : IS_NOT_LINE_BP;
    }

    const auto super_tmp_reg = SUPER_TMP.id();
    /* we expect to see up to four MOVs into SUPER_TMP to load expected_address,
     * followed by a `br SUPER_TMP` */
    auto mov_opcode =
            0xD2800000 | super_tmp_reg; /* movz SUPER_TMP, #0, lsl #0 */

    uint64_t expected_target_addr = (uint64_t)expected_target;
    for (int32_t hw = 0; hw < 4; hw++) {
        uint32_t chunk = expected_target_addr & 0xFFFF;
        expected_target_addr >>= 16;
        if (chunk == 0)
            continue;

        if ((uint32_t)instr != (mov_opcode | (hw << 21) | (chunk << 5))) {
            return IS_NOT_LINE_BP;
        }

        instr = *++pc;
        mov_opcode =
                0xF2800000 | super_tmp_reg; /* movk SUPER_TMP, #0, lsl #0 */
    };

    const int32_t expected_br_instr =
            0xd61f0000 | (super_tmp_reg << 5); /* br SUPER_TMP */
    return (instr == expected_br_instr) ? line_bp_type : IS_NOT_LINE_BP;
}

static void i_emit_nyi(const char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    emit_enter_runtime(0);

    a.mov(ARG1, imm(msg));
    runtime_call<void (*)(const char *), i_emit_nyi>();

    /* Never returns */
}

void BeamModuleAssembler::emit_nyi() {
    emit_nyi("<unspecified>");
}

bool BeamModuleAssembler::emit(unsigned specific_op,
                               const Span<const ArgVal> &args) {
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
    /* a64::x30 now points 4 bytes into the ErtsCodeInfo struct for the
     * function. Put the address of the MFA into ARG1. */
    a.add(ARG1, a64::x30, offsetof(ErtsCodeInfo, mfa) - 4);

    mov_imm(TMP1, EXC_FUNCTION_CLAUSE);
    a.str(TMP1, a64::Mem(c_p, offsetof(Process, freason)));
    a.str(ARG1, a64::Mem(c_p, offsetof(Process, current)));

    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);

    a.b(labels[raise_exception_shared]);
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
    if (code_header.is_valid()) {
        /* We avoid using the `fragment_call` helper to ensure a constant
         * layout, as it adds code in certain debug configurations. */
        a.bl(resolve_fragment(ga->get_i_func_info_shared(), disp128MB));
    } else {
        a.udf(0xF1F0);
    }

    ERTS_CT_ASSERT(ERTS_ASM_BP_FLAG_NONE == 0);
    a.embed_uint32(0);

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

void BeamModuleAssembler::emit_i_func_label(const ArgLabel &Label) {
    flush_last_error();
    emit_aligned_label(Label, ArgVal(ArgVal::Type::Word, sizeof(UWord)));
}

void BeamModuleAssembler::emit_on_load() {
    on_load = current_label;
}

void BeamModuleAssembler::bind_veneer_target(const Label &target) {
    auto veneer_range = _veneers.equal_range(target.id());
    for (auto it = veneer_range.first; it != veneer_range.second; it++) {
        const Veneer &veneer = it->second;

        ASSERT(veneer.target == target);

        if (!code.is_label_bound(veneer.anchor)) {
            ASSERT((ssize_t)a.offset() <= veneer.latestOffset);
            a.bind(veneer.anchor);

            /* TODO: remove from pending stubs? */
        }
    }

    a.bind(target);
}

void BeamModuleAssembler::emit_int_code_end() {
    /* This label is used to figure out the end of the last function */
    code_end = a.new_label();
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

        a.mov(SUPER_TMP, imm(pair.first));
        a.br(SUPER_TMP);
    }

    mark_unreachable();

    /* Emit all remaining stubs. */
    flush_pending_stubs(dispMax);
}

void BeamModuleAssembler::emit_line(const ArgWord &Loc) {
    /* There is no need to align the line instruction. In the loaded code, the
     * type of the pointer will be void* and that pointer will only be used in
     * comparisons. */

    flush_last_error();
}

void BeamModuleAssembler::emit_func_line(const ArgWord &Loc) {
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

const Label &BeamModuleAssembler::resolve_beam_label(const ArgLabel &Lbl,
                                                     enum Displacement disp) {
    ASSERT(Lbl.isLabel());

    const Label &beamLabel = rawLabels.at(Lbl.get());
    const auto &labelEntry = code.label_entry_of(beamLabel);

    if (labelEntry.has_name()) {
        return resolve_label(rawLabels.at(Lbl.get()), disp, labelEntry.name());
    } else {
        return resolve_label(rawLabels.at(Lbl.get()), disp);
    }
}

const Label &BeamModuleAssembler::resolve_label(const Label &target,
                                                enum Displacement disp,
                                                const char *labelName) {
    ssize_t currOffset = a.offset();

    ssize_t minOffset = currOffset - disp;
    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);
    ASSERT(target.is_valid());

    if (code.is_label_bound(target)) {
        ssize_t targetOffset = code.label_offset_from_base(target);

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

        if (code.is_label_bound(veneer.anchor)) {
            ssize_t veneerOffset = code.label_offset_from_base(veneer.anchor);

            if (veneerOffset >= minOffset && veneerOffset <= maxOffset) {
                return veneer.anchor;
            }
        } else if (veneer.latestOffset <= maxOffset) {
            return veneer.anchor;
        }
    }

    Label anchor;

    if (!labelName) {
        anchor = a.new_label();
    } else {
        /* This is the entry label for a function. Create an unique
         * name for the anchor label. It is necessary to include a
         * sequence number in the label name because if the module is
         * huge more than one veneer can be created for each entry
         * label. */
        std::stringstream name;
        name << '@' << labelName << '-' << labelSeq++;
        anchor = a.new_named_label(name.str().c_str());
    }

    auto it = _veneers.emplace(target.id(), Veneer{maxOffset, anchor, target});

    const Veneer &veneer = it->second;
    _pending_veneers.emplace(veneer);

    return veneer.anchor;
}

const Label &BeamModuleAssembler::resolve_fragment(void (*fragment)(),
                                                   enum Displacement disp) {
    auto it = _dispatchTable.find(fragment);

    if (it == _dispatchTable.end()) {
        it = _dispatchTable.emplace(fragment, a.new_label()).first;
    }

    return resolve_label(it->second, disp);
}

a64::Mem BeamModuleAssembler::embed_constant(const ArgVal &value,
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

        if (code.is_label_bound(constant.anchor)) {
            ssize_t constOffset = code.label_offset_from_base(constant.anchor);

            if (constOffset >= minOffset && constOffset <= maxOffset) {
                return a64::Mem(constant.anchor);
            }
        } else if (constant.latestOffset <= maxOffset) {
            return a64::Mem(constant.anchor);
        }
    }

    auto it = _constants.emplace(value,
                                 Constant{maxOffset, a.new_label(), value});
    const Constant &constant = it->second;
    _pending_constants.emplace(constant);

    return a64::Mem(constant.anchor);
}

a64::Mem BeamModuleAssembler::embed_label(const Label &label,
                                          enum Displacement disp) {
    ssize_t currOffset = a.offset();

    ssize_t maxOffset = currOffset + disp;

    ASSERT(disp >= dispMin && disp <= dispMax);

    auto it = _embedded_labels.emplace(
            label.id(),
            EmbeddedLabel{maxOffset, a.new_label(), label});
    ASSERT(it.second);
    const EmbeddedLabel &embedded_label = it.first->second;
    _pending_labels.emplace(embedded_label);

    return a64::Mem(embedded_label.anchor);
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
        next = a.new_label();

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

        if (!code.is_label_bound(veneer.anchor)) {
            if (!next.is_valid()) {
                next = a.new_label();

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
        ASSERT(!code.is_label_bound(constant.anchor));

        if (!next.is_valid()) {
            next = a.new_label();

            comment("Begin stub section");
            if (!is_unreachable()) {
                a.b(next);
            }
        }

        emit_constant(constant);

        effective_offset = a.offset() + range;

        _pending_constants.pop();
    }

    if (next.is_valid()) {
        comment("End stub section");
        a.bind(next);
    }
}

void BeamModuleAssembler::flush_pending_labels() {
    if (!_pending_labels.empty()) {
        a.align(AlignMode::kCode, 8);
    }

    while (!_pending_labels.empty()) {
        const EmbeddedLabel &embedded_label = _pending_labels.top();

        a.bind(embedded_label.anchor);
        a.embed_label(embedded_label.label, 8);

        _pending_labels.pop();
    }
}

void BeamModuleAssembler::emit_veneer(const Veneer &veneer) {
    const Label &anchor = veneer.anchor;
    const Label &target = veneer.target;
    bool directBranch;

    ASSERT(!code.is_label_bound(anchor));
    a.bind(anchor);

    /* Prefer direct branches when possible. */
    if (code.is_label_bound(target)) {
        auto targetOffset = code.label_offset_from_base(target);
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
        Label pointer = a.new_label();

        a.ldr(SUPER_TMP, a64::Mem(pointer));
        a.br(SUPER_TMP);

        a.align(AlignMode::kCode, 8);
        a.bind(pointer);
        a.embed_label(veneer.target);
    }
}

void BeamModuleAssembler::emit_constant(const Constant &constant) {
    const Label &anchor = constant.anchor;
    const ArgVal &value = constant.value;

    ASSERT(!code.is_label_bound(anchor));
    a.align(AlignMode::kData, 8);
    a.bind(anchor);

    ASSERT(!value.isRegister());

    if (value.isImmed()) {
        a.embed_uint64(value.as<ArgImmed>().get());
    } else if (value.isWord()) {
        a.embed_uint64(value.as<ArgWord>().get());
    } else if (value.isLabel()) {
        a.embed_label(rawLabels.at(value.as<ArgLabel>().get()));
    } else {
        switch (value.getType()) {
        case ArgVal::Type::BytePtr:
            strings.push_back({anchor, 0, value.as<ArgBytePtr>().get()});
            a.embed_uint64(LLONG_MAX);
            break;
        case ArgVal::Type::Catch: {
            auto handler = rawLabels[value.as<ArgCatch>().get()];
            catches.push_back({{anchor, 0, 0}, handler});

            /* Catches are limited to 32 bits, but since we don't want to load
             * 32-bit argument values due to displacement limits, we'll store
             * this as a 64-bit value with the upper bits cleared. */
            a.embed_uint64(INT_MAX);
            break;
        }
        case ArgVal::Type::Export: {
            auto index = value.as<ArgExport>().get();
            imports[index].patches.push_back({anchor, 0, 0});
            a.embed_uint64(LLONG_MAX);
            break;
        }
        case ArgVal::Type::FunEntry: {
            auto index = value.as<ArgLambda>().get();
            lambdas[index].patches.push_back({anchor, 0, 0});
            a.embed_uint64(LLONG_MAX);
            break;
        }
        case ArgVal::Type::Literal: {
            auto index = value.as<ArgLiteral>().get();
            literals[index].patches.push_back({anchor, 0, 0});
            a.embed_uint64(LLONG_MAX);
            break;
        }
        default:
            ASSERT(!"error");
        }
    }
}

void BeamModuleAssembler::flush_last_error() {
    /* When there's a possibility of raising an exception at the very end of the
     * preceding instruction (and thus pointing at the start of this one) and
     * this instruction has a new line registered, the error would erroneously
     * refer to this instead of the preceding line.
     *
     * By adding a nop when we detect this condition, the error will correctly
     * refer to the preceding line. */
    if (a.offset() == last_error_offset) {
        a.nop();
    }
}
