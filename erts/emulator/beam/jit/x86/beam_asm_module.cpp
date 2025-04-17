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
    code_header = a.newLabel();
    a.align(AlignMode::kCode, 8);
    a.bind(code_header);

    embed_zeros(sizeof(BeamCodeHeader) +
                sizeof(ErtsCodeInfo *) * num_functions);
}

Label BeamModuleAssembler::embed_vararg_rodata(const Span<ArgVal> &args,
                                               int y_offset) {
    Label label = a.newLabel();

#if !defined(NATIVE_ERLANG_STACK)
    y_offset = CP_SIZE;
#endif

    a.section(rodata);
    a.bind(label);

    for (const ArgVal &arg : args) {
        union {
            BeamInstr as_beam;
            char as_char[1];
        } data;

        a.align(AlignMode::kData, 8);
        switch (arg.getType()) {
        case ArgVal::Type::XReg: {
            auto index = arg.as<ArgXRegister>().get();
            data.as_beam = make_loader_x_reg(index);
            a.embed(&data.as_char, sizeof(data.as_beam));
        } break;
        case ArgVal::Type::YReg: {
            auto index = arg.as<ArgYRegister>().get();
            data.as_beam = make_loader_y_reg(index + y_offset);
            a.embed(&data.as_char, sizeof(data.as_beam));
        } break;
        case ArgVal::Type::Literal: {
            auto index = arg.as<ArgLiteral>().get();
            make_word_patch(literals[index].patches);
        } break;
        case ArgVal::Type::Label:
            a.embedLabel(resolve_beam_label(arg));
            break;
        case ArgVal::Type::Immediate:
            data.as_beam = arg.as<ArgImmed>().get();
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
        case ArgVal::Type::Word:
            data.as_beam = arg.as<ArgWord>().get();
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
        default:
            erts_fprintf(stderr, "tag: %li\n", arg.getType());
            ERTS_ASSERT(!"error");
        }
    }

    a.section(code.textSection());

    return label;
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

void BeamGlobalAssembler::emit_i_breakpoint_trampoline_shared() {
    constexpr ssize_t flag_offset =
            sizeof(ErtsCodeInfo) + BEAM_ASM_FUNC_PROLOGUE_SIZE -
            offsetof(ErtsCodeInfo, u.metadata.breakpoint_flag);

    Label bp_and_nif = a.newLabel(), bp_only = a.newLabel(),
          nif_only = a.newLabel();

    a.mov(RET, x86::qword_ptr(x86::rsp));
    a.movzx(RETd, x86::byte_ptr(RET, -flag_offset));

    a.cmp(RETd, imm(ERTS_ASM_BP_FLAG_BP_NIF_CALL_NIF_EARLY));
    a.short_().je(bp_and_nif);
    a.cmp(RETd, imm(ERTS_ASM_BP_FLAG_CALL_NIF_EARLY));
    a.short_().je(nif_only);
    a.cmp(RETd, imm(ERTS_ASM_BP_FLAG_BP));
    a.short_().je(bp_only);

#ifndef DEBUG
    a.ret();
#else
    Label error = a.newLabel();

    /* RET must be a valid breakpoint flag. */
    a.test(RETd, RETd);
    a.short_().jnz(error);
    a.ret();

    a.bind(error);
    a.ud2();
#endif

    a.bind(bp_and_nif);
    {
        aligned_call(labels[generic_bp_local]);
        /* FALL THROUGH */
    }

    a.bind(nif_only);
    {
        /* call_nif_early returns on its own, unlike generic_bp_local. */
        a.jmp(labels[call_nif_early]);
    }

    a.bind(bp_only);
    {
        aligned_call(labels[generic_bp_local]);
        a.ret();
    }
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    /* This little prologue is used by nif loading and tracing to insert
     * alternative instructions. The call is filled with a relative call to a
     * trampoline in the module header and then the jmp target is zeroed so that
     * it effectively becomes a nop */
    Label next = a.newLabel();

    a.short_().jmp(next);

    if (code_header.isValid()) {
        auto fragment = ga->get_i_breakpoint_trampoline_shared();
        aligned_call(resolve_fragment(fragment));
    } else {
        /* NIF or BIF stub; we're not going to use this trampoline as-is, but
         * we need to reserve space for it. */
        a.ud2();
        a.align(AlignMode::kCode, sizeof(UWord));
    }

    ASSERT(a.offset() % sizeof(UWord) == 0);
    a.bind(next);
    ASSERT((a.offset() - code.labelOffsetFromBase(current_label)) ==
           BEAM_ASM_FUNC_PROLOGUE_SIZE);
}

void BeamGlobalAssembler::emit_i_line_breakpoint_trampoline_shared() {
    Label exit_trampoline = a.newLabel();
    Label dealloc_and_exit_trampoline = a.newLabel();
    Label after_gc_check = a.newLabel();
    Label dispatch_call = a.newLabel();

    emit_enter_frame();

    const auto &saved_live = TMP_MEM1q;
    const auto &saved_pc = TMP_MEM2q;
    const auto &saved_stack_needed = TMP_MEM3q;

    /* NB. TMP1 = live */
    a.mov(saved_live, TMP1); /* stash live */

    /* Pass address of trampoline, will be used to find current function info */
    if (ERTS_LIKELY(erts_frame_layout == ERTS_FRAME_LAYOUT_RA)) {
        a.mov(TMP2, x86::qword_ptr(E)); /* TMP2 := CP */
    } else {
        ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);
        a.mov(TMP2, x86::qword_ptr(E, 8)); /* TMP2 := CP, skipping FP */
    }
    a.sub(TMP2, imm(8));   /* TMP2:= pc (CP adjusted to line of caller) */
    a.mov(saved_pc, TMP2); /* Stash pc */

/* START allocate live live */
#if !defined(NATIVE_ERLANG_STACK)
    const int cp_space = CP_SIZE;
#else
    const int cp_space = 0;
#endif

    a.mov(ARG4, TMP1); /* ARG4 := live */
    a.lea(RET, x86::ptr_abs(cp_space * 8, TMP1, 3));
    /* lea RET, [cp_space * 8 + (TMP1 << 3)]
       RET:= stack-needed = (live + cp_space) * sizeof(Eterm) */
    a.mov(saved_stack_needed, RET); /* stash stack-needed */

    a.lea(ARG3, x86::ptr(RET, S_RESERVED * 8));
    /* ARG3 := stack-needed + S_RESERVED * sizeof(Eterm); */

    a.lea(ARG3, x86::qword_ptr(HTOP, ARG3));
    a.cmp(ARG3, E);
    a.short_().jbe(after_gc_check);

    /* gc needed */
    fragment_call(labels[garbage_collect]);
    a.mov(RET, saved_stack_needed); /* RET := (stashed) stack-needed */
    a.bind(after_gc_check);

    a.sub(E, RET);

#if !defined(NATIVE_ERLANG_STACK)
    a.mov(getCPRef(), imm(NIL));
#endif
    /* END allocate live live */

    a.mov(ARG1, c_p);
    a.mov(ARG2, saved_pc);   /* pc */
    a.mov(ARG3, saved_live); /* live */
    load_x_reg_array(ARG4);  /* reg */
    a.lea(ARG5, getYRef(0)); /* stk */

    emit_enter_runtime();
    runtime_call<
            const Export *(*)(Process *, ErtsCodePtr, Uint, Eterm *, UWord *),
            erts_line_breakpoint_hit__prepare_call>();
    emit_leave_runtime();

    /* If non-null, RET points to error_handler:breakpoint/4 */
    a.test(RET, RET);
    a.jnz(dispatch_call);
    a.mov(RET, saved_stack_needed); /* RET := (stashed) stack-needed */
    a.jmp(dealloc_and_exit_trampoline);

    a.bind(dispatch_call);
    erlang_call(emit_setup_dispatchable_call(RET), ARG1);

    a.bind(labels[i_line_breakpoint_cleanup]);
    load_x_reg_array(ARG1);  /* reg */
    a.lea(ARG2, getYRef(0)); /* stk */

    emit_enter_runtime();
    runtime_call<Uint (*)(Eterm *, UWord *),
                 erts_line_breakpoint_hit__cleanup>();
    emit_leave_runtime();

    a.lea(RET, x86::ptr_abs(cp_space * 8, RET, 3)); /* RET := stack-needed */

    a.bind(dealloc_and_exit_trampoline); /* ASSUMES RET = stack-needed */
    a.add(E, RET);

    a.bind(exit_trampoline);
    emit_leave_frame();
    a.ret();
}

void BeamModuleAssembler::emit_i_line_breakpoint_trampoline() {
    /* This prologue is used to implement line-breakpoints. The "jmp next" can
     * be replaced by nops when the breakpoint is enabled, which will instead
     * trigger the breakpoint when control goes through here */
    Label next = a.newLabel();
    a.short_().jmp(next);

    auto fragment = ga->get_i_line_breakpoint_trampoline_shared();
    aligned_call(resolve_fragment(fragment));

    a.bind(next);
}

enum erts_is_line_breakpoint BeamGlobalAssembler::is_line_breakpoint_trampoline(
        ErtsCodePtr addr) {
    auto pc = static_cast<const char *>(addr);
    uint64_t word;
    enum erts_is_line_breakpoint line_bp_type;
    std::memcpy(&word, pc, sizeof(word));

    /* If addr is a trampoline, first two-bytes are either a JMP SHORT with
     * offset 1 (breakpoint enabled), or offset 6 (breakpoint disabled). */
    const auto jmp_short_opcode = 0x00EB;
    if ((word & 0xFF) != jmp_short_opcode) {
        return IS_NOT_LINE_BP;
    }
    word >>= 8;
    switch (word & 0xFF) {
    case 1:
        line_bp_type = IS_ENABLED_LINE_BP;
        break;
    case 6:
        line_bp_type = IS_DISABLED_LINE_BP;
        break;
    default:
        return IS_NOT_LINE_BP;
    }
    word >>= 8;
    pc += 2;

    /* We expect an aligned call here, because we align the trampoline to 8
     * bytes, we expect a NOP to align the call. The target is a 32-bit offset
     * from the call return address (i.e. addr + 2 + 5) */
    const auto aligned_call_opcode = 0xE890;
    if ((word & 0xFFFF) != aligned_call_opcode) {
        return IS_NOT_LINE_BP;
    }
    word >>= 16;
    const auto call_offset = (static_cast<int64_t>(word) << 32) >> 32;
    pc += 6 + call_offset;

    const auto expected_target =
            (const char *)get_i_line_breakpoint_trampoline_shared();
    if (pc == expected_target)
        return line_bp_type;

    /* The call target must be to an an entry in the dispatch-table
     * that comes at the end of the module, which contains a
     * "JMP i_line_breakpoint_trampoline_shared" */
    std::memcpy(&word, pc, sizeof(word));

    const auto jmp_opcode = 0xE940;
    if ((word & 0xFFFF) != jmp_opcode) {
        return IS_NOT_LINE_BP;
    }
    word >>= 16;
    const int32_t jmp_offset = (static_cast<int64_t>(word) << 32) >> 32;
    pc += 6 + jmp_offset;

    return pc == expected_target ? line_bp_type : IS_NOT_LINE_BP;
}

static void i_emit_nyi(char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    emit_enter_runtime();

    a.mov(ARG1, imm(msg));
    runtime_call<void (*)(char *), i_emit_nyi>();

    /* Never returns */
}

void BeamModuleAssembler::emit_nyi() {
    emit_nyi("<unspecified>");
}

bool BeamModuleAssembler::emit(unsigned specific_op, const Span<ArgVal> &args) {
    comment(opc[specific_op].name);

#ifdef BEAMASM_DUMP_SIZES
    uint64_t before = a.offset();
#endif

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
    /* Pop the ErtsCodeInfo address into ARG1 and mask out the offset added by
     * the call instruction. */
    a.pop(ARG1);
    a.and_(ARG1, imm(~0x7));

    a.add(ARG1, imm(offsetof(ErtsCodeInfo, mfa)));

    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)),
          imm(EXC_FUNCTION_CLAUSE));
    a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG1);

    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);
    a.jmp(labels[raise_exception_shared]);
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

    /* This is an ErtsCodeInfo structure that has a valid x86 opcode as its `op`
     * field, which *calls* the `_i_func_info_shared` fragment so we can trace
     * it back to this particular function.
     *
     * We also use this field to store the current breakpoint flag so that we
     * only have to modify a single branch target when changing breakpoints. */
    a.call(resolve_fragment(ga->get_i_func_info_shared()));
    a.nop();
    a.nop();
    a.embedUInt8(ERTS_ASM_BP_FLAG_NONE);

    ASSERT(a.offset() % sizeof(UWord) == 0);
    a.embed(&info.gen_bp, sizeof(info.gen_bp));
    a.embed(&info.mfa, sizeof(info.mfa));
}

void BeamModuleAssembler::emit_label(const ArgLabel &Label) {
    ASSERT(Label.isLabel());

    current_label = rawLabels[Label.get()];
    a.bind(current_label);

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

void BeamModuleAssembler::emit_int_code_end() {
    /* This label is used to figure out the end of the last function */
    code_end = a.newLabel();
    a.bind(code_end);

    emit_nyi("int_code_end");

    for (auto pair : _dispatchTable) {
        a.bind(pair.second);
        a.jmp(imm(pair.first));
    }
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

const Label &BeamModuleAssembler::resolve_fragment(void (*fragment)()) {
    auto it = _dispatchTable.find(fragment);

    if (it == _dispatchTable.end()) {
        it = _dispatchTable.emplace(fragment, a.newLabel()).first;
    }

    return it->second;
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
