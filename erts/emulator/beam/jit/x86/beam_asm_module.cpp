/*
 * %CopyrightBegin%
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
