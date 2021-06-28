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

#ifndef ASMJIT_ASMJIT_H_INCLUDED
#    include <asmjit/asmjit.hpp>
#endif

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

#include "beam_jit_common.hpp"

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

    /* Points at x_reg_array inside an ErtsSchedulerRegisters struct, allowing
     * the aux_regs field to be addressed with an 8-bit displacement. */
    const x86::Gp registers = x86::rbx;

#ifdef NATIVE_ERLANG_STACK
    /* The Erlang stack pointer, note that it uses RSP and is therefore invalid
     * when running on the runtime stack. */
    const x86::Gp E = x86::rsp;

#    ifdef ERLANG_FRAME_POINTERS
    /* Current frame pointer, used when we emit native stack frames (e.g. to
     * better support `perf`). */
    const x86::Gp frame_pointer = x86::rbp;
#    endif

    /* When we're not using frame pointers, we can keep the Erlang stack in
     * RBP when running on the runtime stack, which is slightly faster than
     * reading and writing from c_p->stop. */
    const x86::Gp E_saved = x86::rbp;
#else
    const x86::Gp E = x86::rbp;
#endif

    const x86::Gp c_p = x86::r13;
    const x86::Gp FCALLS = x86::r14;
    const x86::Gp HTOP = x86::r15;

    /* Local copy of the active code index.
     *
     * This is set to ERTS_SAVE_CALLS_CODE_IX when save_calls is active, which
     * routes us to a common handler routine that calls save_calls before
     * jumping to the actual code. */
    const x86::Gp active_code_ix = x86::r12;

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
    const x86::Gp RETb = x86::al;

    const x86::Mem TMP_MEM1q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[0]));
    const x86::Mem TMP_MEM2q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[1]));
    const x86::Mem TMP_MEM3q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[2]));
    const x86::Mem TMP_MEM4q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[3]));
    const x86::Mem TMP_MEM5q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[4]));

    const x86::Mem TMP_MEM1d = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[0]),
            sizeof(Uint32));
    const x86::Mem TMP_MEM2d = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[1]),
            sizeof(Uint32));
    const x86::Mem TMP_MEM3d = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[2]),
            sizeof(Uint32));
    const x86::Mem TMP_MEM4d = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[3]),
            sizeof(Uint32));
    const x86::Mem TMP_MEM5d = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[4]),
            sizeof(Uint32));

    enum Distance { dShort, dLong };

public:
    static bool hasCpuFeature(uint32_t featureId);

    BeamAssembler();
    BeamAssembler(const std::string &log);

    ~BeamAssembler();

    void *getBaseAddress();
    size_t getOffset();

protected:
    void _codegen(JitAllocator *allocator,
                  const void **executable_ptr,
                  void **writable_ptr);

    void *getCode(Label label);
    byte *getCode(char *labelName);

    void handleError(Error err, const char *message, BaseEmitter *origin);

    constexpr x86::Mem getRuntimeStackRef() const {
        int base = offsetof(ErtsSchedulerRegisters, aux_regs.d.runtime_stack);

        return getSchedulerRegRef(base);
    }

#if !defined(NATIVE_ERLANG_STACK)
#    ifdef JIT_HARD_DEBUG
    constexpr x86::Mem getInitialSPRef() const {
        int base = offsetof(ErtsSchedulerRegisters, initial_sp);

        return getSchedulerRegRef(base);
    }
#    endif

    constexpr x86::Mem getCPRef() const {
        return x86::qword_ptr(E);
    }
#endif

    constexpr x86::Mem getSchedulerRegRef(int offset,
                                          size_t size = sizeof(UWord)) const {
        const int x_reg_offset =
                offsetof(ErtsSchedulerRegisters, x_reg_array.d);

        /* The entire aux_reg field should be addressable with an 8-bit
         * displacement. */
        ERTS_CT_ASSERT(x_reg_offset <= 128);

        return x86::Mem(registers, offset - x_reg_offset, size);
    }

    constexpr x86::Mem getFRef(int index, size_t size = sizeof(UWord)) const {
        int base = offsetof(ErtsSchedulerRegisters, f_reg_array.d);
        int offset = index * sizeof(FloatDef);

        ASSERT(index >= 0 && index <= 1023);
        return getSchedulerRegRef(base + offset, size);
    }

    constexpr x86::Mem getXRef(int index, size_t size = sizeof(UWord)) const {
        int base = offsetof(ErtsSchedulerRegisters, x_reg_array.d);
        int offset = index * sizeof(Eterm);

        ASSERT(index >= 0 && index < ERTS_X_REGS_ALLOCATED);
        return getSchedulerRegRef(base + offset, size);
    }

    constexpr x86::Mem getYRef(int index, size_t size = sizeof(UWord)) const {
        ASSERT(index >= 0 && index <= 1023);

#ifdef NATIVE_ERLANG_STACK
        return x86::Mem(E, index * sizeof(Eterm), size);
#else
        return x86::Mem(E, (index + CP_SIZE) * sizeof(Eterm), size);
#endif
    }

    constexpr x86::Mem getCARRef(x86::Gp Src,
                                 size_t size = sizeof(UWord)) const {
        return x86::Mem(Src, -TAG_PRIMARY_LIST, size);
    }

    constexpr x86::Mem getCDRRef(x86::Gp Src,
                                 size_t size = sizeof(UWord)) const {
        return x86::Mem(Src, -TAG_PRIMARY_LIST + sizeof(Eterm), size);
    }

    void align_erlang_cp() {
        /* Align so that the current address forms a valid CP. */
        ERTS_CT_ASSERT(_CPMASK == 3);
        a.align(kAlignCode, 4);
        ASSERT(is_CP(a.offset()));
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

    /* Ensure that the Erlang stack is used and the redzone is unused.
     * We combine those test to minimize the number of instructions.
     */
    void emit_assert_redzone_unused() {
#ifdef JIT_HARD_DEBUG
        const int REDZONE_BYTES = S_REDZONE * sizeof(Eterm);
        Label ok = a.newLabel(), crash = a.newLabel();

        /* We modify the stack pointer to avoid spilling into a register,
         * TMP_MEM, or using the stack. */
        a.sub(E, imm(REDZONE_BYTES));
        a.cmp(HTOP, E);
        a.short_().ja(crash);
        a.cmp(E, x86::qword_ptr(c_p, offsetof(Process, hend)));
        a.short_().jle(ok);

        a.bind(crash);
        a.comment("# Redzone touched");
        a.ud2();

        a.bind(ok);
        a.add(E, imm(REDZONE_BYTES));
#endif
    }

    /*
     * Calls an Erlang function.
     */
    template<typename Any>
    void erlang_call(Any Target, const x86::Gp &spill) {
#ifdef NATIVE_ERLANG_STACK
        /* We use the Erlang stack as the native stack. We can use a
         * native `call` instruction. */
        emit_assert_redzone_unused();
        aligned_call(Target);
#else
        Label next = a.newLabel();

        /* Save the return CP on the stack. */
        a.lea(spill, x86::qword_ptr(next));
        a.mov(getCPRef(), spill);

        a.jmp(Target);

        /* Need to align this label in order for it to be recognized as
         * is_CP. */
        align_erlang_cp();
        a.bind(next);
#endif
    }

    /*
     * Calls the given address in shared fragment, ensuring that the
     * redzone is unused and that the return address forms a valid
     * CP.
     */
    template<typename Any>
    void fragment_call(Any Target) {
        emit_assert_redzone_unused();

#if defined(JIT_HARD_DEBUG) && !defined(NATIVE_ERLANG_STACK)
        /* Verify that the stack has not grown. */
        Label next = a.newLabel();
        a.cmp(x86::rsp, getInitialSPRef());
        a.short_().je(next);
        a.comment("# The stack has grown");
        a.ud2();
        a.bind(next);
#endif

        aligned_call(Target);
    }

    /*
     * Calls the given function pointer. In a debug build with
     * JIT_HARD_DEBUG defined, it will be enforced that the redzone is
     * unused.
     *
     * The return will NOT be aligned, and thus will not form a valid
     * CP. That means that call code must not scan the stack in any
     * way. That means, for example, that the called code must not
     * throw an exception, do a garbage collection, or cause a context
     * switch.
     */
    void safe_fragment_call(void (*Target)()) {
        emit_assert_redzone_unused();
        a.call(imm(Target));
    }

    template<typename FuncPtr>
    void aligned_call(FuncPtr(*target)) {
        /* Calls to absolute addresses (encoded in the address table) are
         * always 6 bytes long. */
        aligned_call(imm(target), 6);
    }

    void aligned_call(Label target) {
        /* Relative calls are always 5 bytes long. */
        aligned_call(target, 5);
    }

    template<typename OperandType>
    void aligned_call(OperandType target) {
        /* Other calls are variable size. While it would be nice to use this
         * method for pointer/label calls too, `asmjit` writes relocations into
         * the code buffer itself and overwriting them causes all kinds of
         * havoc. */
        size_t call_offset, call_size;

        call_offset = a.offset();
        a.call(target);

        call_size = a.offset() - call_offset;
        a.setOffset(call_offset);

        aligned_call(target, call_size);
    }

    /* Calls the given address, ensuring that the return address forms a valid
     * CP. */
    template<typename OperandType>
    void aligned_call(OperandType target, size_t size) {
        /* The return address must be 4-byte aligned to form a valid CP, so
         * we'll align according to the size of the call instruction. */
        ssize_t next_address = (a.offset() + size);

        ERTS_CT_ASSERT(_CPMASK == 3);
        if (next_address % 4) {
            ssize_t nop_count = 4 - next_address % 4;

            a.embed(nops[nop_count - 1], nop_count);
        }

#ifdef JIT_HARD_DEBUG
        /* TODO: When frame pointers are in place, assert (at runtime) that the
         * destination has a `push rbp; mov rbp, rsp` sequence. */
#endif

        a.call(target);
        ASSERT(is_CP(a.offset()));
    }

    /* Canned instruction sequences for multi-byte NOPs */
    static const uint8_t *nops[3];
    static const uint8_t nop1[1];
    static const uint8_t nop2[2];
    static const uint8_t nop3[3];

    void runtime_call(x86::Gp func, unsigned args) {
        ASSERT(args < 5);

        emit_assert_runtime_stack();

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
    void runtime_call(T(*func)) {
        static_assert(expected_arity == function_arity<T>());

        emit_assert_runtime_stack();

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

#endif

        a.call(imm(func));

#ifdef WIN32
        a.add(x86::rsp, imm(pushed * sizeof(UWord)));
#endif
    }

    template<typename T>
    void abs_jmp(T(*addr)) {
        a.jmp(imm(addr));
    }

    /* Explicitly position-independent absolute jump, for use in fragments that
     * need to be memcpy'd for performance reasons (e.g. NIF stubs) */
    template<typename T>
    void pic_jmp(T(*addr)) {
        a.mov(ARG6, imm(addr));
        a.jmp(ARG6);
    }

    constexpr x86::Mem getArgRef(const ArgVal &val,
                                 size_t size = sizeof(UWord)) const {
        switch (val.getType()) {
        case ArgVal::FReg:
            return getFRef(val.getValue(), size);
        case ArgVal::XReg:
            return getXRef(val.getValue(), size);
        case ArgVal::YReg:
            return getYRef(val.getValue(), size);
        default:
            ERTS_ASSERT(!"NYI");
            return x86::Mem();
        }
    }

    /* Returns the current code address for the export entry in `Src`
     *
     * Export tracing, save_calls, etc is implemented by shared fragments that
     * assume that the export entry is in RET, so we have to copy it over if it
     * isn't already. */
    x86::Mem emit_setup_export_call(const x86::Gp &Src) {
        return emit_setup_export_call(Src, active_code_ix);
    }

    x86::Mem emit_setup_export_call(const x86::Gp &Src,
                                    const x86::Gp &CodeIndex) {
        if (RET != Src) {
            a.mov(RET, Src);
        }

        return x86::qword_ptr(RET, CodeIndex, 3, offsetof(Export, addresses));
    }

    void emit_assert_runtime_stack() {
#ifdef JIT_HARD_DEBUG
        Label crash = a.newLabel(), next = a.newLabel();

#    ifdef NATIVE_ERLANG_STACK
        /* Ensure that we are using the runtime stack. */
        int end_offs, start_offs;

        end_offs = offsetof(ErtsSchedulerRegisters, runtime_stack_end);
        start_offs = offsetof(ErtsSchedulerRegisters, runtime_stack_start);

        a.cmp(E, getSchedulerRegRef(end_offs));
        a.short_().jbe(crash);
        a.cmp(E, getSchedulerRegRef(start_offs));
        a.short_().ja(crash);
#    endif

        /* Are we 16-byte aligned? */
        a.test(x86::rsp, (16 - 1));
        a.short_().je(next);

        a.bind(crash);
        a.comment("# Runtime stack is corrupt");
        a.ud2();

        a.bind(next);
#endif
    }

    void emit_assert_erlang_stack() {
#ifdef JIT_HARD_DEBUG
        Label crash = a.newLabel(), next = a.newLabel();

        /* Are we term-aligned? */
        a.test(E, imm(sizeof(Eterm) - 1));
        a.short_().jne(crash);

        a.cmp(E, x86::qword_ptr(c_p, offsetof(Process, heap)));
        a.short_().jl(crash);
        a.cmp(E, x86::qword_ptr(c_p, offsetof(Process, hend)));
        a.short_().jle(next);

        a.bind(crash);
        a.comment("Erlang stack is corrupt");
        a.ud2();
        a.bind(next);
#endif
    }

    enum Update : int {
        eStack = (1 << 0),
        eHeap = (1 << 1),
        eReductions = (1 << 2),
        eCodeIndex = (1 << 3)
    };

    void emit_enter_frame() {
#ifdef NATIVE_ERLANG_STACK
        if (ERTS_UNLIKELY(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA)) {
#    ifdef ERLANG_FRAME_POINTERS
            a.push(frame_pointer);
            a.mov(frame_pointer, E);
#    endif
        } else {
            ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
        }
#endif
    }

    void emit_leave_frame() {
#ifdef NATIVE_ERLANG_STACK
        if (ERTS_UNLIKELY(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA)) {
            a.leave();
        } else {
            ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
        }
#endif
    }

    void emit_unwind_frame() {
        emit_assert_erlang_stack();

        emit_leave_frame();
        a.add(x86::rsp, imm(sizeof(UWord)));
    }

    template<int Spec = 0>
    void emit_enter_runtime() {
        emit_assert_erlang_stack();

        ERTS_CT_ASSERT((Spec & (Update::eReductions | Update::eStack |
                                Update::eHeap)) == Spec);

        if (ERTS_LIKELY(erts_frame_layout == ERTS_FRAME_LAYOUT_RA)) {
            if ((Spec & (Update::eHeap | Update::eStack)) ==
                (Update::eHeap | Update::eStack)) {
                /* To update both heap and stack we use sse instructions like
                 * gcc -O3 does. Basically it is this function run through
                 * gcc -O3:
                 *
                 *    struct a { long a; long b; long c; };
                 *    void test(long a, long b, long c, struct a *s) {
                 *      s->a = a;
                 *      s->b = b;
                 *      s->c = c;
                 *    } */
                ERTS_CT_ASSERT((offsetof(Process, stop) -
                                offsetof(Process, htop)) == sizeof(Eterm *));
                a.movq(x86::xmm0, HTOP);
                a.movq(x86::xmm1, E);
                a.punpcklqdq(x86::xmm0, x86::xmm1);
                a.movups(x86::xmmword_ptr(c_p, offsetof(Process, htop)),
                         x86::xmm0);
            } else if (Spec & Update::eHeap) {
                a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
            } else if (Spec & Update::eStack) {
                a.mov(x86::qword_ptr(c_p, offsetof(Process, stop)), E);
            }

#ifdef NATIVE_ERLANG_STACK
            if (!(Spec & Update::eStack)) {
                a.mov(E_saved, E);
            }
#endif
        } else {
#ifdef ERLANG_FRAME_POINTERS
            ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);

            if (Spec & Update::eStack) {
                ERTS_CT_ASSERT((offsetof(Process, frame_pointer) -
                                offsetof(Process, stop)) == sizeof(Eterm *));
                a.movq(x86::xmm0, E);
                a.movq(x86::xmm1, frame_pointer);
                a.punpcklqdq(x86::xmm0, x86::xmm1);
                a.movups(x86::xmmword_ptr(c_p, offsetof(Process, stop)),
                         x86::xmm0);
            } else {
                /* We can skip updating the frame pointer whenever the process
                 * doesn't have to inspect the stack. We still need to update
                 * the stack pointer to switch stacks, though, since we don't
                 * have enough spare callee-save registers. */
                a.mov(x86::qword_ptr(c_p, offsetof(Process, stop)), E);
            }

            if (Spec & Update::eHeap) {
                a.mov(x86::qword_ptr(c_p, offsetof(Process, htop)), HTOP);
            }
#endif
        }

        if (Spec & Update::eReductions) {
            a.mov(x86::qword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);
        }

#ifdef NATIVE_ERLANG_STACK
        a.lea(E, getRuntimeStackRef());
#else
        /* Keeping track of stack alignment across shared fragments would be
         * too much of a maintenance burden, so we stash and align the stack
         * pointer at runtime instead. */
        a.mov(getRuntimeStackRef(), x86::rsp);

        a.sub(x86::rsp, imm(15));
        a.and_(x86::rsp, imm(-16));
#endif
    }

    template<int Spec = 0>
    void emit_leave_runtime() {
        emit_assert_runtime_stack();

        ERTS_CT_ASSERT((Spec & (Update::eReductions | Update::eStack |
                                Update::eHeap | Update::eCodeIndex)) == Spec);

        if (ERTS_LIKELY(erts_frame_layout == ERTS_FRAME_LAYOUT_RA)) {
            if (Spec & Update::eStack) {
                a.mov(E, x86::qword_ptr(c_p, offsetof(Process, stop)));
            } else {
#ifdef NATIVE_ERLANG_STACK
                a.mov(E, E_saved);
#endif
            }
        } else {
#ifdef ERLANG_FRAME_POINTERS
            ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA);

            a.mov(E, x86::qword_ptr(c_p, offsetof(Process, stop)));

            if (Spec & Update::eStack) {
                a.mov(frame_pointer,
                      x86::qword_ptr(c_p, offsetof(Process, frame_pointer)));
            }
#endif
        }

        if (Spec & Update::eHeap) {
            a.mov(HTOP, x86::qword_ptr(c_p, offsetof(Process, htop)));
        }

        if (Spec & Update::eReductions) {
            a.mov(FCALLS, x86::qword_ptr(c_p, offsetof(Process, fcalls)));
        }

        if (Spec & Update::eCodeIndex) {
            /* Updates the local copy of the active code index, retaining
             * save_calls if active. */
            a.mov(ARG1, imm(&the_active_code_index));
            a.mov(ARG1d, x86::dword_ptr(ARG1));

            a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
            a.cmovne(active_code_ix, ARG1);
        }

#if !defined(NATIVE_ERLANG_STACK)
        /* Restore the unaligned stack pointer we saved on enter. */
        a.mov(x86::rsp, getRuntimeStackRef());
#endif
    }

    void emit_is_boxed(Label Fail, x86::Gp Src, Distance dist = dLong) {
        /* Use the shortest possible instruction depending on the source
         * register. */
        if (Src == x86::rax || Src == x86::rdi || Src == x86::rsi ||
            Src == x86::rcx || Src == x86::rdx) {
            a.test(Src.r8(), imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
        } else {
            a.test(Src.r32(), imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED));
        }
        if (dist == dShort) {
            a.short_().jne(Fail);
        } else {
            a.jne(Fail);
        }
    }

    x86::Gp emit_ptr_val(x86::Gp Dst, x86::Gp Src) {
#if !defined(TAG_LITERAL_PTR)
        return Src;
#else
        if (Dst != Src) {
            a.mov(Dst, Src);
        }

        /* We intentionally skip TAG_PTR_MASK__ here, as we want to use
         * plain `emit_boxed_val` when we know the argument can't be a literal,
         * such as in bit-syntax matching.
         *
         * This comes at very little cost as `emit_boxed_val` nearly always has
         * a displacement. */
        a.and_(Dst, imm(~TAG_LITERAL_PTR));
        return Dst;
#endif
    }

    constexpr x86::Mem emit_boxed_val(x86::Gp Src,
                                      int32_t bytes = 0,
                                      size_t size = sizeof(UWord)) const {
        ASSERT(bytes % sizeof(Eterm) == 0);
        return x86::Mem(Src, bytes - TAG_PRIMARY_BOXED, size);
    }

    void emit_test_the_non_value(x86::Gp Reg) {
        if (THE_NON_VALUE == 0) {
            a.test(Reg.r32(), Reg.r32());
        } else {
            a.cmp(Reg, imm(THE_NON_VALUE));
        }
    }

    /*
     * Generate the shortest instruction for setting a register to an immediate
     * value. May clear flags.
     */
    template<typename T>
    void mov_imm(x86::Gp to, T value) {
        static_assert(std::is_integral<T>::value || std::is_pointer<T>::value);
        if (value) {
            a.mov(to, imm(value));
        } else {
            /*
             * Generate the shortest instruction to set the register to zero.
             *
             *   48 c7 c0 00 00 00 00    mov    rax, 0
             *   b8 00 00 00 00          mov    eax, 0
             *   31 c0                   xor    eax, eax
             *
             * Thus, "xor eax, eax" is five bytes shorter than "mov rax, 0".
             *
             * Note: xor clears ZF and CF; mov does not change any flags.
             */
            a.xor_(to.r32(), to.r32());
        }
    }

    void mov_imm(x86::Gp to, std::nullptr_t value) {
        (void)value;
        mov_imm(to, 0);
    }

public:
    void embed_rodata(const char *labelName, const char *buff, size_t size);
    void embed_bss(const char *labelName, size_t size);
    void embed_zeros(size_t size);

    void setLogger(std::string log);
    void setLogger(FILE *log);

    template<typename... Ts>
    void comment(const char *format, Ts... args) {
        if (logger.file()) {
            char buff[1024];
            erts_snprintf(buff, sizeof(buff), format, args...);
            a.commentf("# %s", buff);
        }
    }

    struct AsmRange {
        ErtsCodePtr start;
        ErtsCodePtr stop;
        std::string name;

        /* Not used yet */
        std::string file;
        unsigned line;
    };

    void update_gdb_jit_info(std::string modulename,
                             std::vector<AsmRange> &functions);
};

class BeamGlobalAssembler : public BeamAssembler {
    typedef void (BeamGlobalAssembler::*emitFptr)(void);
    typedef void (*fptr)(void);

    /* Please keep this in alphabetical order. */
#define BEAM_GLOBAL_FUNCS(_)                                                   \
    _(apply_fun_shared)                                                        \
    _(arith_compare_shared)                                                    \
    _(arith_eq_shared)                                                         \
    _(bif_nif_epilogue)                                                        \
    _(bif_element_shared)                                                      \
    _(bif_export_trap)                                                         \
    _(bs_add_shared)                                                           \
    _(bs_size_check_shared)                                                    \
    _(bs_fixed_integer_shared)                                                 \
    _(bs_get_tail_shared)                                                      \
    _(call_bif_shared)                                                         \
    _(call_light_bif_shared)                                                   \
    _(call_nif_early)                                                          \
    _(call_nif_shared)                                                         \
    _(call_nif_yield_helper)                                                   \
    _(catch_end_shared)                                                        \
    _(check_float_error)                                                       \
    _(dispatch_bif)                                                            \
    _(dispatch_nif)                                                            \
    _(dispatch_return)                                                         \
    _(dispatch_save_calls)                                                     \
    _(export_trampoline)                                                       \
    _(garbage_collect)                                                         \
    _(generic_bp_global)                                                       \
    _(generic_bp_local)                                                        \
    _(debug_bp)                                                                \
    _(fconv_shared)                                                            \
    _(handle_call_fun_error)                                                   \
    _(handle_element_error)                                                    \
    _(handle_hd_error)                                                         \
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
    _(i_load_nif_shared)                                                       \
    _(i_length_guard_shared)                                                   \
    _(i_length_body_shared)                                                    \
    _(i_loop_rec_shared)                                                       \
    _(i_new_small_map_lit_shared)                                              \
    _(i_test_yield_shared)                                                     \
    _(increment_body_shared)                                                   \
    _(int_div_rem_body_shared)                                                 \
    _(int_div_rem_guard_shared)                                                \
    _(minus_body_shared)                                                       \
    _(minus_guard_shared)                                                      \
    _(new_map_shared)                                                          \
    _(plus_body_shared)                                                        \
    _(plus_guard_shared)                                                       \
    _(process_exit)                                                            \
    _(process_main)                                                            \
    _(raise_exception)                                                         \
    _(raise_exception_shared)                                                  \
    _(times_body_shared)                                                       \
    _(times_guard_shared)                                                      \
    _(unary_minus_body_shared)                                                 \
    _(unary_minus_guard_shared)                                                \
    _(unloaded_fun)                                                            \
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

    x86::Mem emit_i_length_common(Label fail, int state_size);

public:
    BeamGlobalAssembler(JitAllocator *allocator);

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

    /* Map of fun entry to trampoline labels and patches */
    struct patch_lambda {
        std::vector<struct patch> patches;
        Label trampoline;
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

    Label codeHeader;

    /* Used by emit to populate the labelToMFA map */
    Label currLabel;

    /* Special shared fragments that must reside in each module. */
    Label funcInfo;
    Label genericBPTramp;
    Label yieldReturn;
    Label yieldEnter;

    /* The module's on_load function, if any. */
    Label on_load;

    /* The end of the last function. */
    Label code_end;

    Eterm mod;

    /* Save the last PC for an error. */
    size_t last_error_offset = 0;

public:
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        unsigned num_labels,
                        BeamFile_ExportTable *named_labels = NULL);
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        unsigned num_labels,
                        unsigned num_functions,
                        BeamFile_ExportTable *named_labels = NULL);

    bool emit(unsigned op, const Span<ArgVal> &args);

    void codegen(JitAllocator *allocator,
                 const void **executable_ptr,
                 void **writable_ptr,
                 const BeamCodeHeader *in_hdr,
                 const BeamCodeHeader **out_exec_hdr,
                 BeamCodeHeader **out_rw_hdr);

    void codegen(JitAllocator *allocator,
                 const void **executable_ptr,
                 void **writable_ptr);

    void codegen(char *buff, size_t len);

    ErtsCodePtr getCode(unsigned label);
    ErtsCodePtr getLambda(unsigned index);

    void *getCode(Label label) {
        return BeamAssembler::getCode(label);
    }
    byte *getCode(char *labelName) {
        return BeamAssembler::getCode(labelName);
    }

    Label embed_vararg_rodata(const Span<ArgVal> &args, int y_offset);

    unsigned getCodeSize() {
        ASSERT(code.hasBaseAddress());
        return code.codeSize();
    }

    void copyCodeHeader(BeamCodeHeader *hdr);
    BeamCodeHeader *getCodeHeader(void);
    const ErtsCodeInfo *getOnLoad(void);

    unsigned patchCatches(char *rw_base);
    void patchLambda(char *rw_base, unsigned index, BeamInstr I);
    void patchLiteral(char *rw_base, unsigned index, Eterm lit);
    void patchImport(char *rw_base, unsigned index, BeamInstr I);
    void patchStrings(char *rw_base, const byte *string);

protected:
    /* Helpers */
    void emit_gc_test(const ArgVal &Stack,
                      const ArgVal &Heap,
                      const ArgVal &Live);
    void emit_gc_test_preserve(const ArgVal &Need,
                               const ArgVal &Live,
                               x86::Gp term);

    x86::Mem emit_variable_apply(bool includeI);
    x86::Mem emit_fixed_apply(const ArgVal &arity, bool includeI);

    x86::Gp emit_call_fun(void);

    void emit_is_binary(Label Fail, x86::Gp Src, Label next, Label subbin);

    void emit_get_list(const x86::Gp boxed_ptr,
                       const ArgVal &Hd,
                       const ArgVal &Tl);

    void emit_div_rem(const ArgVal &Fail,
                      const ArgVal &LHS,
                      const ArgVal &RHS,
                      const ErtsCodeMFA *error_mfa);

    void emit_setup_guard_bif(const std::vector<ArgVal> &args,
                              const ArgVal &bif);

    void emit_error(int code);

    x86::Mem emit_bs_get_integer_prologue(Label next,
                                          Label fail,
                                          int flags,
                                          int size);

    int emit_bs_get_field_size(const ArgVal &Size,
                               int unit,
                               Label Fail,
                               const x86::Gp &out,
                               unsigned max_size = 0);

    void emit_bs_get_utf8(const ArgVal &Ctx, const ArgVal &Fail);
    void emit_bs_get_utf16(const ArgVal &Ctx,
                           const ArgVal &Fail,
                           const ArgVal &Flags);

    void emit_raise_exception();
    void emit_raise_exception(const ErtsCodeMFA *exp);
    void emit_raise_exception(Label I, const ErtsCodeMFA *exp);
    void emit_raise_exception(x86::Gp I, const ErtsCodeMFA *exp);

    void emit_validate(const ArgVal &arity);
    void emit_bs_skip_bits(const ArgVal &Fail, const ArgVal &Ctx);

    void emit_linear_search(x86::Gp val,
                            const ArgVal &Fail,
                            const Span<ArgVal> &args);

    void emit_float_instr(uint32_t instId,
                          const ArgVal &LHS,
                          const ArgVal &RHS,
                          const ArgVal &Dst);

    void emit_is_small(Label fail, x86::Gp Reg);
    void emit_is_both_small(Label fail, x86::Gp A, x86::Gp B);

    void emit_validate_unicode(Label next, Label fail, x86::Gp value);

    void emit_bif_is_eq_ne_exact_immed(const ArgVal &Src,
                                       const ArgVal &Immed,
                                       const ArgVal &Dst,
                                       Eterm fail_value,
                                       Eterm succ_value);

    void emit_proc_lc_unrequire(void);
    void emit_proc_lc_require(void);

    void emit_nyi(const char *msg);
    void emit_nyi(void);

    void emit_binsearch_nodes(size_t Left,
                              size_t Right,
                              const ArgVal &Fail,
                              const Span<ArgVal> &args);

    bool emit_optimized_three_way_select(const ArgVal &Fail,
                                         const Span<ArgVal> &args);

#ifdef DEBUG
    void emit_tuple_assertion(const ArgVal &Src, x86::Gp tuple_reg);
#endif

#include "beamasm_protos.h"

    void make_move_patch(x86::Gp to,
                         std::vector<struct patch> &patches,
                         int64_t offset = 0) {
        const int MOV_IMM64_PAYLOAD_OFFSET = 2;
        Label lbl = a.newLabel();

        a.bind(lbl);
        a.long_().mov(to, imm(LLONG_MAX));

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
        /* We can't move to or from Y registers when we're on the runtime
         * stack, so we'll conservatively disallow all mov_args in the hopes of
         * finding such bugs sooner. */
        emit_assert_erlang_stack();

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
        if (from.isRegister()) {
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
        if (from.isRegister()) {
            mov_arg(spill, from);
            mov_arg(to, spill);
        } else {
            mov_arg(getArgRef(to), from);
        }
    }
};

void beamasm_update_perf_info(std::string modulename,
                              std::vector<BeamAssembler::AsmRange> &ranges);
