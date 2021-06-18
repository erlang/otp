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
    ASSERT(label < labels.size() + 1);
    return (ErtsCodePtr)getCode(labels[label]);
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

    labels.reserve(num_labels + 1);

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
                e++;
            } else {
                std::string lblName = "label_" + std::to_string(i);
                lbl = a.newNamedLabel(lblName.data());
            }
            labels[i] = lbl;
        }
        return;
    }
    if (erts_jit_asm_dump) {
        // There is no naming info, but dumping of the assembly code
        // has been requested, so do the best we can and number the
        // labels.
        for (unsigned i = 1; i < num_labels; i++) {
            std::string lblName = "label_" + std::to_string(i);
            labels[i] = a.newNamedLabel(lblName.data());
        }
        return;
    }
    // No output is requested, go with unnamed labels
    for (unsigned i = 1; i < num_labels; i++)
        labels[i] = a.newLabel();
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

    /* Shared trampoline for function_clause errors, which can't jump straight
     * to `i_func_info_shared` due to size restrictions. */
    funcInfo = a.newLabel();
    a.align(kAlignCode, 8);
    a.bind(funcInfo);
    abs_jmp(ga->get_i_func_info_shared());

    /* Shared trampoline for yielding on function ingress. */
    yieldEnter = a.newLabel();
    a.align(kAlignCode, 8);
    a.bind(yieldEnter);
    abs_jmp(ga->get_i_test_yield_shared());

    /* Shared trampoline for yielding on function return. */
    yieldReturn = a.newLabel();
    a.align(kAlignCode, 8);
    a.bind(yieldReturn);
    abs_jmp(ga->get_dispatch_return());

    /* Setup the early_nif/breakpoint trampoline. */
    genericBPTramp = a.newLabel();
    a.align(kAlignCode, 16);
    a.bind(genericBPTramp);
    {
        a.ret();

        a.align(kAlignCode, 16);
        ASSERT(a.offset() - code.labelOffsetFromBase(genericBPTramp) == 16 * 1);
        abs_jmp(ga->get_call_nif_early());

        a.align(kAlignCode, 16);
        ASSERT(a.offset() - code.labelOffsetFromBase(genericBPTramp) == 16 * 2);
        aligned_call(ga->get_generic_bp_local());
        a.ret();

        a.align(kAlignCode, 16);
        ASSERT(a.offset() - code.labelOffsetFromBase(genericBPTramp) == 16 * 3);
        aligned_call(ga->get_generic_bp_local());
        abs_jmp(ga->get_call_nif_early());
    }
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

        a.align(kAlignData, 8);
        switch (arg.getType()) {
        case ArgVal::XReg:
            data.as_beam = make_loader_x_reg(arg.getValue());
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
        case ArgVal::YReg:
            data.as_beam = make_loader_y_reg(arg.getValue() + y_offset);
            a.embed(&data.as_char, sizeof(data.as_beam));
            break;
        case ArgVal::Literal:
            make_word_patch(literals[arg.getValue()].patches);
            break;
        case ArgVal::Label:
            a.embedLabel(labels[arg.getValue()]);
            break;
        case ArgVal::Immediate:
        case ArgVal::Word:
            data.as_beam = arg.getValue();
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

    prev_func_start = code.labelOffsetFromBase(labels[functions.back() + 1]);
    diff = a.offset() - prev_func_start;

    if (diff < minimum_size) {
        embed_zeros(minimum_size - diff);
    }
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    /* This little prologue is used by nif loading and tracing to insert
     * alternative instructions. The call is filled with a relative call to a
     * trampoline in the module header and then the jmp target is zeroed so that
     * it effectively becomes a nop */
    Label next = a.newLabel();

    a.short_().jmp(next);

    /* We embed a zero byte here, which is used to flag whether to make an early
     * nif call, call a breakpoint handler, or both. */
    a.embedUInt8(ERTS_ASM_BP_FLAG_NONE);

    if (genericBPTramp.isValid()) {
        a.call(genericBPTramp);
    } else {
        /* NIF or BIF stub; we're not going to use this trampoline as-is, but
         * we need to reserve space for it. */
        a.ud2();
    }

    a.align(kAlignCode, 8);
    a.bind(next);
    ASSERT((a.offset() - code.labelOffsetFromBase(currLabel)) ==
           BEAM_ASM_FUNC_PROLOGUE_SIZE);
}

static void i_emit_nyi(char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    emit_enter_runtime();

    a.mov(ARG1, imm(msg));
    runtime_call<1>(i_emit_nyi);

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
    /* Pop the ErtsCodeInfo address into ARG1 and mask out the offset added by
     * the call instruction. */
    a.pop(ARG1);
    a.and_(ARG1, ~0x7);

    a.lea(ARG1, x86::qword_ptr(ARG1, offsetof(ErtsCodeInfo, mfa)));

    a.mov(x86::qword_ptr(c_p, offsetof(Process, freason)), EXC_FUNCTION_CLAUSE);
    a.mov(x86::qword_ptr(c_p, offsetof(Process, current)), ARG1);

    mov_imm(ARG2, 0);
    mov_imm(ARG4, 0);
    a.jmp(labels[raise_exception_shared]);
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

    /* This is an ErtsCodeInfo structure that has a valid x86 opcode as its `op`
     * field, which *calls* the funcInfo trampoline so we can trace it back to
     * this particular function.
     *
     * We make a relative call to a trampoline in the module header because this
     * needs to fit into a word, and an directy call to `i_func_info_shared`
     * would be too large. */
    if (funcInfo.isValid()) {
        a.call(funcInfo);
    } else {
        a.nop();
    }

    a.align(kAlignCode, sizeof(UWord));
    a.embed(&info.u.gen_bp, sizeof(info.u.gen_bp));
    a.embed(&info.mfa, sizeof(info.mfa));
}

void BeamModuleAssembler::emit_label(const ArgVal &Label) {
    currLabel = labels[Label.getValue()];
    a.bind(currLabel);
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

void BeamModuleAssembler::emit_int_code_end() {
    /* This label is used to figure out the end of the last function */
    code_end = a.newLabel();
    a.bind(code_end);

    emit_nyi("int_code_end");
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
        auto where = (unsigned *)&rw_base[offset + patch.ptr_offs];

        ASSERT(0x7fffffff == *where);
        Eterm catch_term = make_catch(catch_no);

        /* With the current tag scheme, more than 33 million
         * catches can exist at once. */
        ERTS_ASSERT(catch_term >> 31 == 0);
        *where = (unsigned)catch_term;
    }

    return catch_no;
}

void BeamModuleAssembler::patchImport(char *rw_base,
                                      unsigned index,
                                      BeamInstr I) {
    for (const auto &patch : imports[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = I + patch.val_offs;
    }
}

void BeamModuleAssembler::patchLambda(char *rw_base,
                                      unsigned index,
                                      BeamInstr I) {
    for (const auto &patch : lambdas[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = I + patch.val_offs;
    }
}

void BeamModuleAssembler::patchLiteral(char *rw_base,
                                       unsigned index,
                                       Eterm lit) {
    for (const auto &patch : literals[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = lit + patch.val_offs;
    }
}

void BeamModuleAssembler::patchStrings(char *rw_base,
                                       const byte *string_table) {
    for (const auto &patch : strings) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (const byte **)&rw_base[offset + 2];

        ASSERT(LLONG_MAX == (Eterm)*where);
        *where = string_table + patch.val_offs;
    }
}
