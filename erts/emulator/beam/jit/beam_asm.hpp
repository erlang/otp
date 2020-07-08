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

#include <string>
#include <vector>
#include <unordered_map>
#include <map>

#include <asmjit/asmjit.h>

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "beam_catches.h"

#include "beam_asm.h"
}

class ArgVal {
    BeamOpArg gen_op;

public:
    enum TYPE {
        u = TAG_u,
        i = TAG_i,
        x = TAG_x,
        y = TAG_y,
        f = TAG_f,
        q = TAG_q,
        e = TAG_r,
        l = TAG_l /* float register */
    };

    ArgVal(const BeamOpArg &arg) {
        gen_op = arg;
    }

    ArgVal(enum TYPE t, BeamInstr val) {
        gen_op.type = t;
        gen_op.val = val;
    }

    ArgVal(unsigned t, BeamInstr val) {
#ifdef DEBUG
        switch (t) {
        case TAG_u:
            break;
        case TAG_i:
            break;
        case TAG_x:
            break;
        case TAG_y:
            break;
        case TAG_f:
            break;
        case TAG_q:
            break;
        case TAG_r:
            break;
        case TAG_l:
            break;
        default:
            ASSERT(0);
        }
#endif

        gen_op.type = t;
        gen_op.val = val;
    }

    constexpr enum TYPE getType() const {
        return (enum TYPE)gen_op.type;
    }

    constexpr uint64_t getValue() const {
        return gen_op.val;
    }

    constexpr bool isMem() const {
        return gen_op.type == x || gen_op.type == y;
    }

    constexpr bool isLiteral() const {
        return gen_op.type == q;
    }

    constexpr bool isImmed() const {
        return gen_op.type == i;
    }

    template<typename T>
    ArgVal operator+(T val) const {
        return ArgVal(gen_op.type, val + gen_op.val);
    }

    template<typename T>
    ArgVal operator*(T val) const {
        return ArgVal(gen_op.type, val * gen_op.val);
    }
};

using namespace asmjit;

class BeamAssembler : public ErrorHandler {
protected:
    /* Holds code and relocation information. */
    CodeHolder code;

    /* TODO: Want to change this to x86::Builder in order to be able to patch
     * the correct I into the code after code generation */
    x86::Assembler a;

    FileLogger logger;

    Section *rodata = nullptr;

    /* * * * * * * * * */

    const x86::Gp frame_pointer = x86::rbp;

    /* Points at x_reg_array inside an ErtsSchedulerRegisters struct, allowing
     * the aux_regs field to be addressed with an 8-bit displacement. */
    const x86::Gp registers = x86::rbx;

    /* TODO: Use r12 for the active code index once we start executing on the
     * native stack */
    const x86::Gp E = x86::r12;
    const x86::Gp c_p = x86::r13;
    const x86::Gp FCALLS = x86::r14;
    const x86::Gp HTOP = x86::r15;

    const x86::Mem active_code_ix = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.active_code_ix));

    /* Argument vector for guard BIFs, using the internal X registers after the
     * end of the array. */
    static const int GUARD_BIF_ARGV_LENGTH = 3;
    const x86::Mem guard_bif_argv = getXRef(MAX_REG);
    static_assert(ERTS_X_REGS_ALLOCATED - MAX_REG >= GUARD_BIF_ARGV_LENGTH);

#ifdef ERTS_MSACC_EXTENDED_STATES
    const x86::Mem erts_msacc_cache = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.erts_msacc_cache));
#endif

    /* * * * * * * * * */
#ifdef WIN32
    const x86::Gp ARG1 = x86::rcx;
    const x86::Gp ARG2 = x86::rdx;
    const x86::Gp ARG3 = x86::r8;
    const x86::Gp ARG4 = x86::r9;
    const x86::Gp ARG5 = x86::r10;
    const x86::Gp ARG6 = x86::r11;

    const x86::Gp ARG1d = x86::ecx;
    const x86::Gp ARG2d = x86::edx;
    const x86::Gp ARG3d = x86::r8d;
    const x86::Gp ARG4d = x86::r9d;
    const x86::Gp ARG5d = x86::r10d;
    const x86::Gp ARG6d = x86::r11d;
#else
    const x86::Gp ARG1 = x86::rdi;
    const x86::Gp ARG2 = x86::rsi;
    const x86::Gp ARG3 = x86::rdx;
    const x86::Gp ARG4 = x86::rcx;
    const x86::Gp ARG5 = x86::r8;
    const x86::Gp ARG6 = x86::r9;

    const x86::Gp ARG1d = x86::edi;
    const x86::Gp ARG2d = x86::esi;
    const x86::Gp ARG3d = x86::edx;
    const x86::Gp ARG4d = x86::ecx;
    const x86::Gp ARG5d = x86::r8d;
    const x86::Gp ARG6d = x86::r9d;
#endif

    const x86::Gp RET = x86::rax;
    const x86::Gp RETd = x86::eax;

    const x86::Mem TMP_MEM1q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[0]));
    const x86::Mem TMP_MEM2q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[1]));
    const x86::Mem TMP_MEM3q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[2]));

    const x86::Mem dTMP1_MEM = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[0]),
            sizeof(Uint32));
    const x86::Mem dTMP2_MEM = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[1]),
            sizeof(Uint32));
    const x86::Mem dTMP3_MEM = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[2]),
            sizeof(Uint32));

public:
    static bool hasCpuFeature(uint32_t featureId);

    BeamAssembler() : code() {
        /* Setup with default code info */
        Error err = code.init(hostEnvironment());
        ERTS_ASSERT(!err && "Failed to init codeHolder");

        err = code.newSection(&rodata,
                              ".rodata",
                              SIZE_MAX,
                              Section::kFlagConst,
                              8);
        ERTS_ASSERT(!err && "Failed to create .rodata section");

        err = code.attach(&a);

        ERTS_ASSERT(!err && "Failed to attach codeHolder");
#ifdef DEBUG
        a.addValidationOptions(BaseEmitter::kValidationOptionAssembler);
#endif
        code.setErrorHandler(this);
    }

    BeamAssembler(const std::string &log) : BeamAssembler() {
        if (erts_asm_dump) {
            setLogger(log + ".asm");
        }
    }

    ~BeamAssembler() {
        if (logger.file())
            fclose(logger.file());
    }

    void *getBaseAddress() {
        ASSERT(code.hasBaseAddress());
        return (void *)code.baseAddress();
    }

    size_t getOffset() {
        return a.offset();
    }

    /*
     * Generate the shortest instruction for setting a register to an immediate
     * value. May clear flags.
     */
    void mov_imm(x86::Gp to, Uint value) {
        if (value == 0) {
            /*
             * Generate the shortest instruction to set the register to zero.
             *
             *   48 c7 c0 00 00 00 00    mov    rax, 0
             *   b8 00 00 00 00          mov    eax, 0
             *   31 c0                   xor    eax, eax
             *
             * Thus, "xor eax, eax" is five bytes shorter than "mov rax, 0".
             *
             * Note: xor clears ZF and C; mov does not change any flags.
             */
            a.xor_(to.r32(), to.r32());
        } else if (Support::isInt32(value)) {
            /*
             * Generate the shortest instruction to set the register
             * to an unsigned immediate value that fits in 32 bits.
             *
             *   48 c7 c0 2a 00 00 00    mov    rax, 42
             *   b8 2a 00 00 00          mov    eax, 42
             */
            a.mov(to.r32(), imm(value));
        } else {
            a.mov(to, imm(value));
        }
    }

protected:
    void *_codegen() {
        Error err = code.flatten();
        ERTS_ASSERT(!err && "Could not flatten code");
        err = code.resolveUnresolvedLinks();
        ERTS_ASSERT(!err && "Could not resolve all links");

        /* Verify that all labels are bound */
#ifdef DEBUG
        for (auto e : code.labelEntries()) {
            if (!e->isBound()) {
                erts_exit(ERTS_ABORT_EXIT, "Label %s is not bound", e->name());
            }
        }
#endif

        /* The code needs to be 16 byte aligned, so we allocate a little extra
         * and then align it. It has to be 16 bytes aligned in order to the
         * code align functions to work. If we ever use a 32 byte align,
         * we need to align the code to 32-bytes etc etc. */
        void *module =
                (void *)erts_alloc(ERTS_ALC_T_CODE, code.codeSize() + 16);
        uint64_t aligned_module =
                (uint64_t)module + (16 - ((uint64_t)module) % 16);
        ERTS_ASSERT((uint64_t)aligned_module % 16 == 0);
        code.relocateToBase(aligned_module);
        code.copyFlattenedData((void *)aligned_module,
                               code.codeSize(),
                               CodeHolder::kCopyPadSectionBuffer);

#ifdef WIN32
        DWORD old;
        if (!VirtualProtect((void *)aligned_module,
                            code.codeSize(),
                            PAGE_EXECUTE_READWRITE,
                            &old)) {
            erts_exit(-2, "Could not change memory protection");
        }
#endif

        return module;
    }

    void *getCode(Label label) {
        ASSERT(label.isValid());
        return (char *)getBaseAddress() + code.labelOffsetFromBase(label);
    }

    byte *getCode(char *labelName) {
        return (byte *)getCode(code.labelByName(labelName, strlen(labelName)));
    }

    void handleError(Error err, const char *message, BaseEmitter *origin) {
        comment(message);
        fflush(logger.file());
        ASSERT(0 && "Fault instruction encode");
    }

    constexpr x86::Mem getCPRef() const {
        return x86::qword_ptr(E);
    }

    constexpr x86::Mem getSchedulerRegRef(int offset,
                                          size_t size = sizeof(UWord)) const {
        const int x_reg_offset =
                offsetof(ErtsSchedulerRegisters, x_reg_array.d);

        /* The entire aux_reg field should be addressable with an 8-bit
         * displacement. */
        ERTS_CT_ASSERT(x_reg_offset <= 128);

        return x86::Mem(registers, offset - x_reg_offset, size);
    }

    constexpr x86::Mem getFRef(int index) const {
        int base = offsetof(ErtsSchedulerRegisters, f_reg_array.d);
        int offset = index * sizeof(FloatDef);

        ASSERT(index >= 0 && index <= 1023);
        return getSchedulerRegRef(base + offset);
    }

    constexpr x86::Mem getXRef(int index) const {
        int base = offsetof(ErtsSchedulerRegisters, x_reg_array.d);
        int offset = index * sizeof(Eterm);

        ASSERT(index >= 0 && index < ERTS_X_REGS_ALLOCATED);
        return getSchedulerRegRef(base + offset);
    }

    constexpr x86::Mem getYRef(int index) const {
        ASSERT(index >= 0 && index <= 1023);

        return x86::qword_ptr(E, (index + CP_SIZE) * sizeof(Eterm));
    }

    void load_x_reg_array(x86::Gp reg) {
        /* By definition. */
        a.mov(reg, registers);
    }

    void load_erl_bits_state(x86::Gp reg) {
        int offset =
                offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state);

        a.lea(reg, getSchedulerRegRef(offset));
    }

    /* Calls the given label, ensuring that the return address forms a valid
     * CP. */
    void aligned_call(Label label) {
        /* The return address must be 8-byte aligned to form a valid CP.
         * Short-form calls are 5 bytes long, so we'll align up to the nearest
         * 8-byte boundary after that. */
        ssize_t next_address = (a.offset() + 5);

        if (next_address % 8) {
            ssize_t nop_count = 8 - next_address % 8;

            for (int i = 0; i < nop_count; i++) {
                a.nop();
            }
        }

        a.call(label);
        ASSERT((a.offset() % 8) == 0);
    }

    /* Calls the given address, ensuring that the return address forms a valid
     * CP. */
    template<typename T>
    void aligned_call(T(*func)) {
        /* Long-form calls are 6 bytes long. */
        ssize_t next_address = (a.offset() + 6);

        if (next_address % 8) {
            ssize_t nop_count = 8 - next_address % 8;

            for (int i = 0; i < nop_count; i++) {
                a.nop();
            }
        }

        a.call(imm(func));
        ASSERT((a.offset() % 8) == 0);
    }

    void abs_call(x86::Gp func, unsigned args) {
        ASSERT(args < 5);
        emit_stackcheck();
#ifdef WIN32
        a.sub(x86::rsp, imm(4 * sizeof(UWord)));
        a.call(func);
        a.add(x86::rsp, imm(4 * sizeof(UWord)));
#else
        a.call(func);
#endif
    }

    template<typename T>
    struct function_arity;
    template<typename T, typename... Args>
    struct function_arity<T(Args...)>
            : std::integral_constant<int, sizeof...(Args)> {};

    template<int expected_arity, typename T>
    void abs_call(T(*func)) {
        static_assert(expected_arity == function_arity<T>());
        emit_stackcheck();
#ifdef WIN32
        unsigned pushed;
        switch (expected_arity) {
        case 6:
        case 5:
            /* We push ARG6 to keep the stack aligned even when we only have 5
             * arguments. It does no harm, and is slightly more compact than
             * sub/push/sub. */
            a.push(ARG6);
            a.push(ARG5);
            a.sub(x86::rsp, imm(4 * sizeof(UWord)));
            pushed = 6;
            break;
        default:
            a.sub(x86::rsp, imm(4 * sizeof(UWord)));
            pushed = 4;
        }

        a.call(imm(func));

        a.add(x86::rsp, imm(pushed * sizeof(UWord)));
#else
        a.call(imm(func));
#endif
    }

    template<typename T>
    void abs_jmp(T(*addr)) {
        a.jmp(imm(addr));
    }

    /* Explicitly position-independent absolute jump, for use in fragments that
     * need to be memcpy'd for performance reasons (e.g. export entries) */
    template<typename T>
    void pic_jmp(T(*addr)) {
        a.mov(ARG6, imm(addr));
        a.jmp(ARG6);
    }

    constexpr x86::Mem getArgRef(const ArgVal &val) const {
        switch (val.getType()) {
        case ArgVal::TYPE::l:
            return getFRef(val.getValue());
        case ArgVal::TYPE::x:
            return getXRef(val.getValue());
        case ArgVal::TYPE::y:
            return getYRef(val.getValue());
        default:
            ERTS_ASSERT(!"NYI");
            return x86::Mem();
        }
    }

    /* Updates the local copy of the active code index, retaining save_calls if
     * active.
     *
     * Clobbers ARG1 and ARG2. */
    void emit_update_code_index() {
        a.mov(ARG2, imm(&the_active_code_index));
        a.mov(ARG2d, x86::dword_ptr(ARG2));
        a.mov(ARG1, active_code_ix);
        a.cmp(ARG1, imm(ERTS_SAVE_CALLS_CODE_IX));
        a.cmovne(ARG1, ARG2);
        a.mov(active_code_ix, ARG1);
    }

    void emit_light_swapin() {
        a.mov(HTOP, x86::qword_ptr(c_p, offsetof(Process, htop)));
    }

    void emit_light_swapout() {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
    }

    void emit_swapin() {
        a.mov(E, x86::qword_ptr(c_p, offsetof(Process, stop)));
        a.mov(HTOP, x86::qword_ptr(c_p, offsetof(Process, htop)));
    }

    void emit_swapout() {
        a.mov(x86::qword_ptr(c_p, offsetof(Process, stop)), E);
        a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
    }

    void emit_heavy_swapin() {
        emit_swapin();

        a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process, fcalls)));
    }

    void emit_heavy_swapout() {
        emit_swapout();

        a.mov(x86::qword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);
    }

    void emit_is_boxed(Label Fail, x86::Gp Src) {
        a.test(Src, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
        a.jne(Fail);
    }

    x86::Gp emit_ptr_val(x86::Gp Dst, x86::Gp Src) {
#if !defined(TAG_LITERAL_PTR)
        return Src;
#else
        if (Dst != Src) {
            a.mov(Dst, Src);
        }

        /* We intentionally skip TAG_PTR_MASK__ here, as we want to use plain
         * `emit_boxed_val` when we know the argument can't be a literal, such
         * as in bit-syntax matching.
         *
         * This comes at very little cost as `emit_boxed_val` nearly always has
         * a displacement. */
        a.and_(Dst, imm(~TAG_LITERAL_PTR));
        return Dst;
#endif
    }

    template<typename FieldType = UWord>
    constexpr x86::Mem emit_boxed_val(x86::Gp Src, int32_t bytes = 0) const {
        ASSERT(bytes % sizeof(Eterm) == 0);
        return x86::Mem(Src, bytes - TAG_PRIMARY_BOXED, sizeof(FieldType));
    }

public:
    void embed_rodata(char *labelName, const char *buff, size_t size);
    void embed_bss(char *labelName, size_t size);

    void embed_zeros(size_t size);

    void setLogger(std::string log) {
        FILE *f = fopen(log.data(), "w+");

        /* FIXME: Don't crash when loading multiple modules with the same name.
         *
         * setLogger(nullptr) disables logging. */
        if (f) {
            setvbuf(f, NULL, _IONBF, 0);
        }

        setLogger(f);
    }

    void setLogger(FILE *log) {
        logger.setFile(log);
        logger.setIndentation(FormatOptions::kIndentationCode, 4);
        code.setLogger(&logger);
    }

    template<typename... Ts>
    void comment(const char *format, Ts... args) {
        if (logger.file()) {
            char buff[1024];
            erts_snprintf(buff, sizeof(buff), format, args...);
            a.commentf("# %s", buff);
        }
    }

    struct AsmRange {
        BeamInstr *start;
        BeamInstr *stop;
        std::string name;

        /* Not used yet */
        std::string file;
        unsigned line;
    };

    void update_gdb_jit_info(std::string modulename,
                             std::vector<AsmRange> &functions);
    void update_perf_info(std::string modulename,
                          std::vector<AsmRange> &functions);

    unsigned emit_function_preamble(unsigned pushes = 0) {
        /* Push rbp to stack */
        a.push(frame_pointer);
        a.mov(frame_pointer, x86::rsp);

        /* Make sure the stack is 16-byte aligned. We're off by 16 because of
         * our return address + rbp push, so if our stack frame is "uneven" we
         * need to make it "even." */
        if (pushes % 2 == 1) {
            pushes++;
        }

        if (pushes)
            a.sub(x86::rsp, imm(8 * pushes));

        emit_stackcheck();

        return pushes;
    }

    /* This function is not allowed to modify any x86 CPU flags */
    void emit_function_postamble(unsigned pops = 0) {
        a.leave();
    }

    void emit_stackcheck(void) {
#ifdef DEBUG
        Label next = a.newLabel();
        /* Assert that we didn't missalign up the stack. */
        a.test(x86::rsp, imm(0xF));
        a.je(next);
        a.hlt();
        a.bind(next);
#endif
    }

    void embed(void *data, uint32_t size) {
        a.embed((char *)data, size);
    }
};

class BeamGlobalAssembler : public BeamAssembler {
    typedef void (BeamGlobalAssembler::*emitFptr)(void);
    typedef void (*fptr)(void);

    /* Please keep this in alphabetical order. */
#define BEAM_GLOBAL_FUNCS(_)                                                   \
    _(arith_compare_shared)                                                    \
    _(arith_eq_shared)                                                         \
    _(bif_nif_epilogue)                                                        \
    _(bs_fixed_integer_shared)                                                 \
    _(bs_get_tail_shared)                                                      \
    _(call_bif_shared)                                                         \
    _(call_error_handler_shared)                                               \
    _(call_light_bif_shared)                                                   \
    _(call_nif_early)                                                          \
    _(call_nif_shared)                                                         \
    _(catch_end_shared)                                                        \
    _(dispatch_bif)                                                            \
    _(dispatch_nif)                                                            \
    _(dispatch_return)                                                         \
    _(dispatch_save_calls)                                                     \
    _(error_action_code)                                                       \
    _(garbage_collect)                                                         \
    _(generic_bp_global)                                                       \
    _(generic_bp_local)                                                        \
    _(debug_bp)                                                                \
    _(handle_error_shared)                                                     \
    _(i_band_body_shared)                                                      \
    _(i_band_guard_shared)                                                     \
    _(i_bif_body_shared)                                                       \
    _(i_bif_guard_shared)                                                      \
    _(i_bor_body_shared)                                                       \
    _(i_bor_guard_shared)                                                      \
    _(i_bnot_body_shared)                                                      \
    _(i_bnot_guard_shared)                                                     \
    _(i_bsl_guard_shared)                                                      \
    _(i_bsl_body_shared)                                                       \
    _(i_bsr_guard_shared)                                                      \
    _(i_bsr_body_shared)                                                       \
    _(i_bxor_body_shared)                                                      \
    _(i_bxor_guard_shared)                                                     \
    _(i_func_info_shared)                                                      \
    _(i_loop_rec_shared)                                                       \
    _(i_new_small_map_lit_shared)                                              \
    _(i_test_yield_shared)                                                     \
    _(increment_body_shared)                                                   \
    _(int_div_rem_body_shared)                                                 \
    _(int_div_rem_guard_shared)                                                \
    _(minus_body_shared)                                                       \
    _(new_map_shared)                                                          \
    _(plus_body_shared)                                                        \
    _(process_main)                                                            \
    _(times_body_shared)                                                       \
    _(times_guard_shared)                                                      \
    _(update_map_assoc_shared)                                                 \
    _(update_map_exact_guard_shared)                                           \
    _(update_map_exact_body_shared)

/* Labels exported from within process_main */
#define PROCESS_MAIN_LABELS(_)                                                 \
    _(context_switch)                                                          \
    _(context_switch_simplified)                                               \
    _(do_schedule)

#define DECL_ENUM(NAME) NAME,

    enum GlobalLabels : uint32_t {
        BEAM_GLOBAL_FUNCS(DECL_ENUM) PROCESS_MAIN_LABELS(DECL_ENUM)
    };
#undef DECL_ENUM

    static const std::map<GlobalLabels, emitFptr> emitPtrs;
    static const std::map<GlobalLabels, std::string> labelNames;
    std::unordered_map<GlobalLabels, Label> labels;
    std::unordered_map<GlobalLabels, fptr> ptrs;

#define DECL_FUNC(NAME) void emit_##NAME(void);

    BEAM_GLOBAL_FUNCS(DECL_FUNC);
#undef DECL_FUNC

    template<typename T>
    void emit_bitwise_fallback_body(T(*func_ptr), const ErtsCodeMFA *mfa);

    template<typename T>
    void emit_bitwise_fallback_guard(T(*func_ptr));

    void emit_handle_error(int pops = 0);

public:
    BeamGlobalAssembler();

    void (*get(GlobalLabels lbl))(void) {
        ASSERT(ptrs[lbl]);
        return ptrs[lbl];
    }

#define GET_CODE(NAME)                                                         \
    void (*get_##NAME(void))() {                                               \
        return get(NAME);                                                      \
    }

    BEAM_GLOBAL_FUNCS(GET_CODE)
    PROCESS_MAIN_LABELS(GET_CODE)
#undef GET_CODE
};

class BeamModuleAssembler : public BeamAssembler {
    typedef unsigned BeamLabel;

    /* Map of label number to asmjit Label */
    typedef std::unordered_map<BeamLabel, Label> LabelMap;
    LabelMap labels;

    struct patch {
        Label where;
        int64_t ptr_offs;
        int64_t val_offs;
    };

    struct patch_catch {
        struct patch patch;
        Label handler;
    };
    std::vector<struct patch_catch> catches;

    /* Map of import entry to patch labels and mfa */
    struct patch_import {
        std::vector<struct patch> patches;
        ErtsCodeMFA mfa;
    };
    typedef std::unordered_map<unsigned, struct patch_import> ImportMap;
    ImportMap imports;

    /* Map of fun entry to patch labels */
    struct patch_lambda {
        std::vector<struct patch> patches;
        ErlFunEntry fe;
    };
    typedef std::unordered_map<unsigned, struct patch_lambda> LambdaMap;
    LambdaMap lambdas;

    /* Map of literals to patch labels */
    struct patch_literal {
        std::vector<struct patch> patches;
    };
    typedef std::unordered_map<unsigned, struct patch_literal> LiteralMap;
    LiteralMap literals;

    /* All string patches */
    std::vector<struct patch> strings;

    /* All functions that have been seen so far */
    std::vector<BeamLabel> functions;

    BeamGlobalAssembler *ga;

    /* Used by emit to populate the labelToMFA map */
    Label currLabel;
    unsigned prev_op = 0;
    Label codeHeader;
    Label funcInfo;
    Label funcYield;
    Label genericBPTramp;
    Label on_load;

    Label floatMax;
    Label floatSignMask;

    Eterm mod;

public:
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        unsigned num_labels);
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        unsigned num_labels,
                        unsigned num_functions);

    bool emit(unsigned op, const std::vector<ArgVal> &args);

    void *codegen(BeamCodeHeader *in_hdr, BeamCodeHeader **out_hdr);
    void *codegen(void);

    void codegen(char *buff, size_t len);

    BeamInstr *getCode(unsigned label);
    void *getCode(Label label) {
        return BeamAssembler::getCode(label);
    }
    byte *getCode(char *labelName) {
        return BeamAssembler::getCode(labelName);
    }

    Label embed_vararg_rodata(const std::vector<ArgVal> &args);

    unsigned getCodeSize() {
        ASSERT(code.hasBaseAddress());
        return code.codeSize();
    }

    void copyCodeHeader(BeamCodeHeader *hdr);
    BeamCodeHeader *getCodeHeader(void);
    BeamInstr *getOnLoad(void);

    unsigned patchCatches();
    void patchLambda(unsigned index, BeamInstr I);
    void patchLiteral(unsigned index, Eterm lit);
    void patchImport(unsigned index, BeamInstr I);
    void patchStrings(byte *string);
    void emit_call_bif_export(void *fptr);

private:
    /* Helpers */
    void emit_gc_test(const ArgVal &Stack,
                      const ArgVal &Heap,
                      const ArgVal &Live);
    void emit_gc_test_preserve(const ArgVal &Need,
                               const ArgVal &Live,
                               x86::Gp term);
    void emit_dispatch_export(const ArgVal &Exp);
    x86::Gp emit_apply(uint64_t deallocate, bool includeI);
    x86::Gp emit_apply(const ArgVal &arity, uint64_t deallocate);
    x86::Gp emit_call_fun(const ArgVal &Fun);
    x86::Gp emit_apply_fun(void);
    void emit_setup_return(x86::Gp dest);
    void emit_is_binary(Label Fail, x86::Gp Src, Label next, Label subbin);
    void emit_is_integer(Label Fail, Label next, Label BigFail, x86::Gp Src);

    void emit_div_rem(const ArgVal &Fail,
                      const ArgVal &LHS,
                      const ArgVal &RHS,
                      const ErtsCodeMFA *error_mfa);

    void emit_arith_compare(x86::Inst::Id succJmpOp,
                            x86::Inst::Id failJmpOp,
                            Label fail,
                            Label eq,
                            Label succ,
                            const ArgVal &LHS,
                            const ArgVal &RHS);

    void emit_setup_guard_bif(const std::vector<ArgVal> &args,
                              const ArgVal &bif);

    void emit_bif_arg_error(std::vector<ArgVal> args,
                            Label entry,
                            const ErtsCodeMFA *mfa);
    void emit_error(Label entry, int code);

    x86::Mem emit_bs_get_integer_prologue(Label next,
                                          Label fail,
                                          int flags,
                                          int size);

    int emit_bs_get_field_size(const ArgVal &Size,
                               int unit,
                               Label Fail,
                               const x86::Gp &out,
                               unsigned max_size = 0);

    void emit_handle_error(Label I, const ErtsCodeMFA *exp);
    void emit_validate(const ArgVal &arity);
    void emit_bs_skip_bits(const ArgVal &Fail, const ArgVal &Ctx);

    void emit_linear_search(x86::Gp val,
                            Label fail,
                            const std::vector<ArgVal> &args);

    void emit_check_float(Label entry, Label next, x86::Xmm value);

    void emit_is_both_small(Label fail, x86::Gp A, x86::Gp B);

    void emit_validate_unicode(Label next, Label fail, x86::Gp value);

    void emit_proc_lc_unrequire(void);
    void emit_proc_lc_require(void);

    void emit_nyi(const char *msg);
    void emit_nyi(void);

#include "beamasm_protos.h"

    void alloc(Uint slots) {
        a.sub(E, imm(slots * sizeof(Eterm)));
    }

    void alloc(const ArgVal &slots) {
        /* slots gives the value in bytes */
        ASSERT(slots.getValue() % sizeof(Eterm) == 0);
        alloc(slots.getValue() / sizeof(Eterm));
    }

    void dealloc(Uint slots) {
        a.add(E, imm(slots * sizeof(Eterm)));
    }

    void dealloc(const ArgVal &slots) {
        /* slots the value in bytes */
        ASSERT(slots.getValue() % sizeof(Eterm) == 0);
        dealloc(slots.getValue() / sizeof(Eterm));
    }

    void make_move_patch(x86::Gp to,
                         std::vector<struct patch> &patches,
                         int64_t offset = 0) {
        const int MOV_IMM64_PAYLOAD_OFFSET = 2;
        Label lbl = a.newLabel();

        a.bind(lbl);
        a.mov(to, imm(LLONG_MAX));

        patches.push_back({lbl, MOV_IMM64_PAYLOAD_OFFSET, offset});
    }

    void make_word_patch(std::vector<struct patch> &patches) {
        Label lbl = a.newLabel();
        UWord word = LLONG_MAX;

        a.bind(lbl);
        a.embed(reinterpret_cast<char *>(&word), sizeof(word));

        patches.push_back({lbl, 0, 0});
    }

    template<typename A, typename B>
    void mov_arg(A to, B from) {
        mov_arg(to, from, ARG1);
    }

    template<typename T>
    void cmp_arg(T oper, const ArgVal &val) {
        cmp_arg(oper, val, ARG1);
    }

    void cmp_arg(x86::Mem mem, const ArgVal &val, const x86::Gp &spill) {
        /* Note that the cast to Sint is necessary to handle negative numbers
         * such as NIL. */
        if (val.isImmed() && Support::isInt32((Sint)val.getValue())) {
            a.cmp(mem, imm(val.getValue()));
        } else {
            mov_arg(spill, val);
            a.cmp(mem, spill);
        }
    }

    void cmp_arg(x86::Gp gp, const ArgVal &val, const x86::Gp &spill) {
        if (val.isImmed() && Support::isInt32((Sint)val.getValue())) {
            a.cmp(gp, imm(val.getValue()));
        } else {
            mov_arg(spill, val);
            a.cmp(gp, spill);
        }
    }

    /* Note: May clear flags. */
    void mov_arg(x86::Gp to, const ArgVal &from, const x86::Gp &spill) {
        if (from.isMem()) {
            a.mov(to, getArgRef(from));
        } else if (from.isLiteral()) {
            make_move_patch(to, literals[from.getValue()].patches);
        } else {
            mov_imm(to, from.getValue());
        }
    }

    void mov_arg(x86::Mem to, const ArgVal &from, const x86::Gp &spill) {
        if (from.isImmed()) {
            if (Support::isInt32((Sint)from.getValue())) {
                a.mov(to, imm(from.getValue()));
            } else {
                a.mov(spill, imm(from.getValue()));
                a.mov(to, spill);
            }
        } else {
            mov_arg(spill, from);
            a.mov(to, spill);
        }
    }

    void mov_arg(const ArgVal &to, x86::Gp from, const x86::Gp &spill) {
        (void)spill;

        a.mov(getArgRef(to), from);
    }

    void mov_arg(const ArgVal &to, BeamInstr from, const x86::Gp &spill) {
        if (Support::isInt32((Sint)from)) {
            a.mov(getArgRef(to), imm(from));
        } else {
            a.mov(spill, imm(from));
            mov_arg(to, spill);
        }
    }

    void mov_arg(const ArgVal &to, const ArgVal &from, const x86::Gp &spill) {
        if (from.isMem()) {
            mov_arg(spill, from);
            mov_arg(to, spill);
        } else {
            mov_arg(getArgRef(to), from);
        }
    }
};
