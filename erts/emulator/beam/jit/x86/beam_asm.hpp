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

#include <string>
#include <vector>
#include <unordered_map>
#include <map>
#include <functional>
#include <algorithm>
#include <cmath>

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
#include "big.h"

#include "beam_asm.h"
}

#include "beam_jit_common.hpp"

/* On Windows, the min and max macros may be defined. */
#undef min
#undef max

using namespace asmjit;

struct BeamAssembler : public BeamAssemblerCommon {
    BeamAssembler() : BeamAssemblerCommon(a) {
        Error err = code.attach(&a);
        ERTS_ASSERT(!err && "Failed to attach codeHolder");
    }

    BeamAssembler(const std::string &log) : BeamAssembler() {
        if (erts_jit_asm_dump) {
            setLogger(log + ".asm");
        }
    }

protected:
    x86::Assembler a;

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
    const x86::Gp FCALLS = x86::r14d;
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

    const x86::Gp TMP1 = x86::rdi;
    const x86::Gp TMP2 = x86::rsi;

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

    const x86::Gp TMP1 = x86::r10;
    const x86::Gp TMP2 = x86::r11;

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
        a.align(AlignMode::kCode, 4);
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
        comment("Redzone touched");
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
        comment("The stack has grown");
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

    constexpr x86::Mem getArgRef(const ArgRegister &arg,
                                 size_t size = sizeof(UWord)) const {
        if (arg.isXRegister()) {
            return getXRef(arg.as<ArgXRegister>().get(), size);
        } else if (arg.isYRegister()) {
            return getYRef(arg.as<ArgYRegister>().get(), size);
        }

        return getFRef(arg.as<ArgFRegister>().get(), size);
    }

    /* Returns the current code address for the `Export` or `ErlFunEntry` in
     * `Src`.
     *
     * Export tracing, save_calls, etc are implemented by shared fragments that
     * assume that the respective entry is in RET, so we have to copy it over
     * if it isn't already. */
    x86::Mem emit_setup_dispatchable_call(const x86::Gp &Src) {
        return emit_setup_dispatchable_call(Src, active_code_ix);
    }

    x86::Mem emit_setup_dispatchable_call(const x86::Gp &Src,
                                          const x86::Gp &CodeIndex) {
        if (RET != Src) {
            a.mov(RET, Src);
        }

        ERTS_CT_ASSERT(offsetof(ErlFunEntry, dispatch) == 0);
        ERTS_CT_ASSERT(offsetof(Export, dispatch) == 0);

        return x86::qword_ptr(RET,
                              CodeIndex,
                              3,
                              offsetof(ErtsDispatchable, addresses));
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
        comment("Runtime stack is corrupt");
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
        comment("Erlang stack is corrupt");
        a.ud2();
        a.bind(next);
#endif
    }

    /* Prefer `eHeapAlloc` over `eStack | eHeap` when calling
     * functions in the runtime system that allocate heap
     * memory (`HAlloc`, heap factories, etc).
     *
     * Prefer `eHeapOnlyAlloc` over `eHeapAlloc` for functions
     * that assume there's already a certain amount of free
     * space on the heap, such as those using `HeapOnlyAlloc`
     * or similar. It's slightly cheaper in release builds,
     * and in debug builds it updates `eStack` to ensure that
     * we can make heap size assertions. */
    enum Update : int {
        eStack = (1 << 0),
        eHeap = (1 << 1),
        eReductions = (1 << 2),
        eCodeIndex = (1 << 3),
        eHeapAlloc = Update::eHeap | Update::eStack,
#ifndef DEBUG
        eHeapOnlyAlloc = Update::eHeap,
#else
        eHeapOnlyAlloc = Update::eHeapAlloc
#endif
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
                /* To update both heap and stack we use SSE/AVX
                 * instructions like gcc -O3 does. Basically it is
                 * this function run through gcc -O3:
                 *
                 *    struct a { long a; long b; long c; };
                 *    void test(long a, long b, long c, struct a *s) {
                 *      s->a = a;
                 *      s->b = b;
                 *      s->c = c;
                 *    } */
                ERTS_CT_ASSERT((offsetof(Process, stop) -
                                offsetof(Process, htop)) == sizeof(Eterm *));
                if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
                    a.vmovq(x86::xmm1, HTOP);
                    a.vpinsrq(x86::xmm0, x86::xmm1, E, 1);
                    a.vmovdqu(x86::xmmword_ptr(c_p, offsetof(Process, htop)),
                              x86::xmm0);
                } else {
                    a.movq(x86::xmm0, HTOP);
                    a.movq(x86::xmm1, E);
                    a.punpcklqdq(x86::xmm0, x86::xmm1);
                    a.movups(x86::xmmword_ptr(c_p, offsetof(Process, htop)),
                             x86::xmm0);
                }
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
                if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
                    a.vmovq(x86::xmm1, E);
                    a.vpinsrq(x86::xmm0, x86::xmm1, frame_pointer, 1);
                    a.vmovdqu(x86::xmmword_ptr(c_p, offsetof(Process, stop)),
                              x86::xmm0);
                } else {
                    a.movq(x86::xmm0, E);
                    a.movq(x86::xmm1, frame_pointer);
                    a.punpcklqdq(x86::xmm0, x86::xmm1);
                    a.movups(x86::xmmword_ptr(c_p, offsetof(Process, stop)),
                             x86::xmm0);
                }
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
            a.mov(x86::dword_ptr(c_p, offsetof(Process, fcalls)), FCALLS);
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
        /* If the emulator has not been compiled with AVX support (which stops
         * it from using legacy SSE instructions), we'll need to clear the upper
         * bits of all AVX registers to avoid AVX/SSE transition penalties.  */
#if !defined(__AVX__)
        if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
            a.vzeroupper();
        }
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
            a.mov(FCALLS, x86::dword_ptr(c_p, offsetof(Process, fcalls)));
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

    void emit_test(x86::Gp Src, byte mask) {
        /* Use the shortest possible instruction depending on the source
         * register. */
        if (Src == x86::rax || Src == x86::rdi || Src == x86::rsi ||
            Src == x86::rcx || Src == x86::rdx) {
            a.test(Src.r8(), imm(mask));
        } else {
            a.test(Src.r32(), imm(mask));
        }
    }

    void emit_test_cons(x86::Gp Src) {
        emit_test(Src, _TAG_PRIMARY_MASK - TAG_PRIMARY_LIST);
    }

    void emit_is_cons(Label Fail, x86::Gp Src, Distance dist = dLong) {
        emit_test_cons(Src);
        if (dist == dShort) {
            a.short_().jne(Fail);
        } else {
            a.jne(Fail);
        }
    }

    void emit_is_not_cons(Label Fail, x86::Gp Src, Distance dist = dLong) {
        emit_test_cons(Src);
        if (dist == dShort) {
            a.short_().je(Fail);
        } else {
            a.je(Fail);
        }
    }

    void emit_test_boxed(x86::Gp Src) {
        emit_test(Src, _TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED);
    }

    void emit_is_boxed(Label Fail, x86::Gp Src, Distance dist = dLong) {
        emit_test_boxed(Src);
        if (dist == dShort) {
            a.short_().jne(Fail);
        } else {
            a.jne(Fail);
        }
    }

    void emit_is_not_boxed(Label Fail, x86::Gp Src, Distance dist = dLong) {
        emit_test_boxed(Src);
        if (dist == dShort) {
            a.short_().je(Fail);
        } else {
            a.je(Fail);
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
            /* Generate the shortest instruction to set the register to an
             * immediate.
             *
             *   48 c7 c0 2a 00 00 00    mov    rax, 42
             *   b8 2a 00 00 00          mov    eax, 42
             *
             *   49 c7 c0 2a 00 00 00    mov    r8, 42
             *   41 b8 2a 00 00 00       mov    r8d, 42
             */
            if (Support::isUInt32((Uint)value)) {
                a.mov(to.r32(), imm(value));
            } else {
                a.mov(to, imm(value));
            }
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

    template<typename Dst, typename Src>
    void vmovups(Dst dst, Src src) {
        if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
            a.vmovups(dst, src);
        } else {
            a.movups(dst, src);
        }
    }

    template<typename Dst, typename Src>
    void vmovsd(Dst dst, Src src) {
        if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
            a.vmovsd(dst, src);
        } else {
            a.movsd(dst, src);
        }
    }

    template<typename Dst, typename Src>
    void vucomisd(Dst dst, Src src) {
        if (hasCpuFeature(CpuFeatures::X86::kAVX)) {
            a.vucomisd(dst, src);
        } else {
            a.ucomisd(dst, src);
        }
    }

    /* Copies `count` words from `from` to `to`.
     *
     * Clobbers `spill` and the first vector register (xmm0, ymm0 etc). */
    void emit_copy_words(x86::Mem from,
                         x86::Mem to,
                         Sint32 count,
                         x86::Gp spill) {
        ASSERT(!from.hasIndex() && !to.hasIndex());
        ASSERT(count >= 0 && count < (ERTS_SINT32_MAX / (Sint32)sizeof(UWord)));
        ASSERT(from.offset() < ERTS_SINT32_MAX - count * (Sint32)sizeof(UWord));
        ASSERT(to.offset() < ERTS_SINT32_MAX - count * (Sint32)sizeof(UWord));

        /* We're going to mix sizes pretty wildly below, so it's easiest to
         * turn off size validation. */
        from.setSize(0);
        to.setSize(0);

        using vectors = std::initializer_list<std::tuple<x86::Vec,
                                                         Sint32,
                                                         x86::Inst::Id,
                                                         CpuFeatures::X86::Id>>;
        for (const auto &spec : vectors{{x86::zmm0,
                                         8,
                                         x86::Inst::kIdVmovups,
                                         CpuFeatures::X86::kAVX512_VL},
                                        {x86::zmm0,
                                         8,
                                         x86::Inst::kIdVmovups,
                                         CpuFeatures::X86::kAVX512_F},
                                        {x86::ymm0,
                                         4,
                                         x86::Inst::kIdVmovups,
                                         CpuFeatures::X86::kAVX},
                                        {x86::xmm0,
                                         2,
                                         x86::Inst::kIdVmovups,
                                         CpuFeatures::X86::kAVX},
                                        {x86::xmm0,
                                         2,
                                         x86::Inst::kIdMovups,
                                         CpuFeatures::X86::kSSE}}) {
            const auto &[vector_reg, vector_size, vector_inst, feature] = spec;

            if (!hasCpuFeature(feature)) {
                continue;
            }

            /* Copy the words inline if we can, otherwise use a loop with the
             * largest vector size we're capable of. */
            if (count <= vector_size * 4) {
                while (count >= vector_size) {
                    a.emit(vector_inst, vector_reg, from);
                    a.emit(vector_inst, to, vector_reg);

                    from.addOffset(sizeof(UWord) * vector_size);
                    to.addOffset(sizeof(UWord) * vector_size);
                    count -= vector_size;
                }
            } else {
                Sint32 loop_iterations, loop_size;
                Label copy_next = a.newLabel();

                loop_iterations = count / vector_size;
                loop_size = loop_iterations * vector_size * sizeof(UWord);

                from.addOffset(loop_size);
                to.addOffset(loop_size);
                from.setIndex(spill);
                to.setIndex(spill);

                mov_imm(spill, -loop_size);
                a.bind(copy_next);
                {
                    a.emit(vector_inst, vector_reg, from);
                    a.emit(vector_inst, to, vector_reg);

                    a.add(spill, imm(vector_size * sizeof(UWord)));
                    a.short_().jne(copy_next);
                }

                from.resetIndex();
                to.resetIndex();

                count %= vector_size;
            }
        }

        if (count == 1) {
            a.mov(spill, from);
            a.mov(to, spill);

            count -= 1;
        }

        ASSERT(count == 0);
        (void)count;
    }
};

#include "beam_asm_global.hpp"

class BeamModuleAssembler : public BeamAssembler,
                            public BeamModuleAssemblerCommon {
    BeamGlobalAssembler *ga;

    /* Save the last PC for an error. */
    size_t last_error_offset = 0;

    /* ARG2 is excluded as it is used to point to the currently active tuple. */
    RegisterCache<20, x86::Mem, x86::Gp> reg_cache =
            RegisterCache<20, x86::Mem, x86::Gp>(
                    registers,
                    E,
                    {TMP1, TMP2, ARG3, ARG4, ARG5, ARG6, ARG1, RET});

    x86::Gp find_cache(x86::Mem mem) {
        return reg_cache.find(a.offset(), mem);
    }

    /* Store CPU register into memory and update the cache. */
    void store_cache(x86::Gp src, x86::Mem mem_dst) {
        reg_cache.consolidate(a.offset());
        a.mov(mem_dst, src);
        reg_cache.put(mem_dst, src);
        reg_cache.update(a.offset());
    }

    void load_cached(x86::Gp dst, x86::Mem mem) {
        x86::Gp cached_reg = find_cache(mem);

        if (cached_reg.isValid()) {
            /* This memory location is cached. */
            if (cached_reg == dst) {
                comment("skipped fetching of BEAM register");
            } else {
                comment("simplified fetching of BEAM register");
                a.mov(dst, cached_reg);
                reg_cache.invalidate(dst);
                reg_cache.update(a.offset());
            }
        } else {
            /* Not cached. Load and update cache. */
            a.mov(dst, mem);
            reg_cache.invalidate(dst);
            reg_cache.put(mem, dst);
            reg_cache.update(a.offset());
        }
    }

    template<typename L, typename... Any>
    void preserve_cache(L generate, Any... clobber) {
        bool valid = reg_cache.validAt(a.offset());

        generate();

        if (valid) {
            if (sizeof...(clobber) > 0) {
                reg_cache.invalidate(clobber...);
            }

            reg_cache.update(a.offset());
        }
    }

    void trim_preserve_cache(const ArgWord &Words) {
        if (Words.get() > 0) {
            ASSERT(Words.get() <= 1023);
            preserve_cache([&]() {
                auto offset = Words.get() * sizeof(Eterm);
                a.add(E, imm(offset));
                reg_cache.trim_yregs(-offset);
            });
        }
    }

    void mov_preserve_cache(x86::Mem dst, x86::Gp src) {
        preserve_cache(
                [&]() {
                    a.mov(dst, src);
                },
                dst);
    }

    void mov_preserve_cache(x86::Gp dst, x86::Gp src) {
        preserve_cache(
                [&]() {
                    a.mov(dst, src);
                },
                dst);
    }

    void mov_preserve_cache(x86::Gp dst, x86::Mem src) {
        preserve_cache(
                [&]() {
                    a.mov(dst, src);
                },
                dst);
    }

    void cmp_preserve_cache(x86::Gp reg1, x86::Gp reg2) {
        preserve_cache([&]() {
            a.cmp(reg1, reg2);
        });
    }

    void cmp_preserve_cache(x86::Mem mem, x86::Gp reg) {
        preserve_cache([&]() {
            a.cmp(mem, reg);
        });
    }

    /* Pick a temporary register, preferring a register not present in
     * the cache. */
    x86::Gp alloc_temp_reg() {
        return reg_cache.allocate(a.offset());
    }

    /* Maps code pointers to thunks that jump to them, letting us treat global
     * fragments as if they were local. */
    std::unordered_map<void (*)(), Label> _dispatchTable;

public:
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        int num_labels,
                        const BeamFile *file = NULL);
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        int num_labels,
                        int num_functions,
                        const BeamFile *file = NULL);

    bool emit(unsigned op, const Span<ArgVal> &args);

    void emit_coverage(void *coverage, Uint index, Uint size);

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

    void *register_metadata(const BeamCodeHeader *header);

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
    void patchLambda(char *rw_base, unsigned index, const ErlFunEntry *fe);
    void patchLiteral(char *rw_base, unsigned index, Eterm lit);
    void patchImport(char *rw_base, unsigned index, const Export *import);
    void patchStrings(char *rw_base, const byte *string);

protected:
    void emit_gc_test(const ArgWord &Stack,
                      const ArgWord &Heap,
                      const ArgWord &Live);
    void emit_gc_test_preserve(const ArgWord &Need,
                               const ArgWord &Live,
                               const ArgSource &Preserve,
                               x86::Gp preserve_reg);

    x86::Mem emit_variable_apply(bool includeI);
    x86::Mem emit_fixed_apply(const ArgWord &arity, bool includeI);

    x86::Gp emit_call_fun(bool skip_box_test = false,
                          bool skip_header_test = false);

    void emit_is_boxed(Label Fail, x86::Gp Src, Distance dist = dLong) {
        preserve_cache([&]() {
            BeamAssembler::emit_is_boxed(Fail, Src, dist);
        });
    }

    void emit_is_boxed(Label Fail,
                       const ArgVal &Arg,
                       x86::Gp Src,
                       Distance dist = dLong) {
        if (always_one_of<BeamTypeId::AlwaysBoxed>(Arg)) {
            comment("skipped box test since argument is always boxed");
            return;
        }

        preserve_cache([&]() {
            BeamAssembler::emit_is_boxed(Fail, Src, dist);
        });
    }

    void emit_is_cons(Label Fail, x86::Gp Src, Distance dist = dLong) {
        preserve_cache([&]() {
            emit_test_cons(Src);
            if (dist == dShort) {
                a.short_().jne(Fail);
            } else {
                a.jne(Fail);
            }
        });
    }

    void emit_is_not_cons(Label Fail, x86::Gp Src, Distance dist = dLong) {
        preserve_cache([&]() {
            emit_test_cons(Src);
            if (dist == dShort) {
                a.short_().je(Fail);
            } else {
                a.je(Fail);
            }
        });
    }

    void emit_get_list(const x86::Gp boxed_ptr,
                       const ArgRegister &Hd,
                       const ArgRegister &Tl);

    void emit_div_rem(const ArgLabel &Fail,
                      const ArgSource &LHS,
                      const ArgSource &RHS,
                      const ErtsCodeMFA *error_mfa,
                      bool need_div = true,
                      bool need_rem = true);

    void emit_setup_guard_bif(const std::vector<ArgVal> &args,
                              const ArgWord &bif);

    void emit_error(int code);

    void emit_bs_get_integer(const ArgRegister &Ctx,
                             const ArgLabel &Fail,
                             const ArgWord &Live,
                             const ArgWord Flags,
                             int bits,
                             const ArgRegister &Dst);

    int emit_bs_get_field_size(const ArgSource &Size,
                               int unit,
                               Label Fail,
                               const x86::Gp &out,
                               unsigned max_size = 0);

    void emit_bs_get_utf8(const ArgRegister &Ctx, const ArgLabel &Fail);
    void emit_bs_get_utf16(const ArgRegister &Ctx,
                           const ArgLabel &Fail,
                           const ArgWord &Flags);
    void update_bin_state(x86::Gp bin_offset,
                          x86::Gp current_byte,
                          Sint bit_offset,
                          Sint size,
                          x86::Gp size_reg);
    bool need_mask(const ArgVal Val, Sint size);
    void set_zero(Sint effectiveSize);
    bool bs_maybe_enter_runtime(bool entered);
    void bs_maybe_leave_runtime(bool entered);
    void emit_construct_utf8_shared();
    void emit_construct_utf8(const ArgVal &Src,
                             Sint bit_offset,
                             bool is_byte_aligned);

    void emit_read_bits(Uint bits,
                        const x86::Gp bin_base,
                        const x86::Gp bin_offset,
                        const x86::Gp bitdata);
    void emit_extract_integer(const x86::Gp bitdata,
                              const x86::Gp tmp,
                              Uint flags,
                              Uint bits,
                              const ArgRegister &Dst);
    void emit_extract_bitstring(const x86::Gp bitdata,
                                Uint bits,
                                const ArgRegister &Dst);
    void emit_read_integer(const x86::Gp bin_base,
                           const x86::Gp bin_position,
                           const x86::Gp tmp,
                           Uint flags,
                           Uint bits,
                           const ArgRegister &Dst);

    UWord bs_get_flags(const ArgVal &val);

    void emit_raise_exception();
    void emit_raise_exception(const ErtsCodeMFA *exp);
    void emit_raise_exception(Label I, const ErtsCodeMFA *exp);
    void emit_raise_exception(x86::Gp I, const ErtsCodeMFA *exp);

    void emit_validate(const ArgWord &arity);
    void emit_bs_skip_bits(const ArgLabel &Fail, const ArgRegister &Ctx);

    void emit_linear_search(x86::Gp val,
                            const ArgVal &Fail,
                            const Span<ArgVal> &args);

    void emit_float_instr(uint32_t instIdSSE,
                          uint32_t instIdAVX,
                          const ArgFRegister &LHS,
                          const ArgFRegister &RHS,
                          const ArgFRegister &Dst);

    void emit_is_small(Label fail, const ArgSource &Arg, x86::Gp Reg);
    void emit_are_both_small(Label fail,
                             const ArgSource &LHS,
                             x86::Gp A,
                             const ArgSource &RHS,
                             x86::Gp B);

    void emit_validate_unicode(Label next, Label fail, x86::Gp value);

    void emit_bif_is_eq_ne_exact(const ArgSource &LHS,
                                 const ArgSource &RHS,
                                 const ArgRegister &Dst,
                                 Eterm fail_value,
                                 Eterm succ_value);

    void emit_cond_to_bool(uint32_t instId, const ArgRegister &Dst);
    void emit_bif_is_ge_lt(uint32_t instId,
                           const ArgSource &LHS,
                           const ArgSource &RHS,
                           const ArgRegister &Dst);
    void emit_bif_min_max(uint32_t instId,
                          const ArgSource &LHS,
                          const ArgSource &RHS,
                          const ArgRegister &Dst);

    void emit_proc_lc_unrequire(void);
    void emit_proc_lc_require(void);

    void emit_nyi(const char *msg);
    void emit_nyi(void);

    void emit_binsearch_nodes(size_t Left,
                              size_t Right,
                              const ArgVal &Fail,
                              const Span<ArgVal> &args);

    bool emit_optimized_two_way_select(bool destructive,
                                       const ArgVal &value1,
                                       const ArgVal &value2,
                                       const ArgVal &label);

#ifdef DEBUG
    void emit_tuple_assertion(const ArgSource &Src, x86::Gp tuple_reg);
#endif

#include "beamasm_protos.h"

    const Label &resolve_beam_label(const ArgLabel &Lbl) const {
        return rawLabels.at(Lbl.get());
    }

    /* Resolves a shared fragment, creating a trampoline that loads the
     * appropriate address before jumping there. */
    const Label &resolve_fragment(void (*fragment)());

    void safe_fragment_call(void (*fragment)()) {
        emit_assert_redzone_unused();
        a.call(resolve_fragment(fragment));
    }

    template<typename FuncPtr>
    void aligned_call(FuncPtr(*target)) {
        BeamAssembler::aligned_call(resolve_fragment(target));
    }

    void aligned_call(Label target) {
        BeamAssembler::aligned_call(target);
    }

    void make_move_patch(x86::Gp to,
                         std::vector<struct patch> &patches,
                         size_t offset = 0) {
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
        x86::Gp reg = find_cache(mem);

        if (reg.isValid()) {
            /* Note that the cast to Sint is necessary to handle
             * negative numbers such as NIL. */
            if (val.isImmed() &&
                Support::isInt32((Sint)val.as<ArgImmed>().get())) {
                comment("simplified compare of BEAM register");
                preserve_cache([&]() {
                    a.cmp(reg, imm(val.as<ArgImmed>().get()));
                });
            } else if (reg != spill) {
                comment("simplified compare of BEAM register");
                mov_arg(spill, val);
                cmp_preserve_cache(reg, spill);
            } else {
                mov_arg(spill, val);
                cmp_preserve_cache(mem, spill);
            }
        } else {
            /* Note that the cast to Sint is necessary to handle
             * negative numbers such as NIL. */
            if (val.isImmed() &&
                Support::isInt32((Sint)val.as<ArgImmed>().get())) {
                preserve_cache([&]() {
                    a.cmp(mem, imm(val.as<ArgImmed>().get()));
                });
            } else {
                mov_arg(spill, val);
                cmp_preserve_cache(mem, spill);
            }
        }
    }

    void cmp_arg(x86::Gp gp, const ArgVal &val, const x86::Gp &spill) {
        if (val.isImmed() && Support::isInt32((Sint)val.as<ArgImmed>().get())) {
            preserve_cache([&]() {
                a.cmp(gp, imm(val.as<ArgImmed>().get()));
            });
        } else {
            mov_arg(spill, val);
            cmp_preserve_cache(gp, spill);
        }
    }

    void cmp(x86::Gp gp, int64_t val, const x86::Gp &spill) {
        if (Support::isInt32(val)) {
            preserve_cache([&]() {
                a.cmp(gp, imm(val));
            });
        } else if (gp.isGpd()) {
            mov_imm(spill, val);
            preserve_cache([&]() {
                a.cmp(gp, spill.r32());
            });
        } else {
            mov_imm(spill, val);
            cmp_preserve_cache(gp, spill);
        }
    }

    void sub(x86::Gp gp, int64_t val, const x86::Gp &spill) {
        if (Support::isInt32(val)) {
            preserve_cache(
                    [&]() {
                        a.sub(gp, imm(val));
                    },
                    gp);
        } else {
            preserve_cache(
                    [&]() {
                        mov_imm(spill, val);
                        a.sub(gp, spill);
                    },
                    gp,
                    spill);
        }
    }

    /* Note: May clear flags. */
    void mov_arg(x86::Gp to, const ArgVal &from, const x86::Gp &spill) {
        if (from.isBytePtr()) {
            make_move_patch(to, strings, from.as<ArgBytePtr>().get());
        } else if (from.isExport()) {
            make_move_patch(to, imports[from.as<ArgExport>().get()].patches);
        } else if (from.isImmed()) {
            preserve_cache(
                    [&]() {
                        mov_imm(to, from.as<ArgImmed>().get());
                    },
                    to);
        } else if (from.isLambda()) {
            preserve_cache(
                    [&]() {
                        make_move_patch(
                                to,
                                lambdas[from.as<ArgLambda>().get()].patches);
                    },
                    to);
        } else if (from.isLiteral()) {
            preserve_cache(
                    [&]() {
                        make_move_patch(
                                to,
                                literals[from.as<ArgLiteral>().get()].patches);
                    },
                    to);
        } else if (from.isRegister()) {
            auto mem = getArgRef(from.as<ArgRegister>());
            load_cached(to, mem);
        } else if (from.isWord()) {
            preserve_cache(
                    [&]() {
                        mov_imm(to, from.as<ArgWord>().get());
                    },
                    to);
        } else {
            ASSERT(!"mov_arg with incompatible type");
        }

#ifdef DEBUG
        /* Explicitly clear flags to catch bugs quicker, it may be very rare
         * for a certain instruction to load values that would otherwise cause
         * flags to be cleared. */
        a.test(to, to);
#endif
    }

    void mov_arg(x86::Mem to, const ArgVal &from, const x86::Gp &spill) {
        if (from.isImmed()) {
            auto val = from.as<ArgImmed>().get();

            if (Support::isInt32((Sint)val)) {
                preserve_cache(
                        [&]() {
                            a.mov(to, imm(val));
                        },
                        to);
            } else {
                preserve_cache(
                        [&]() {
                            a.mov(spill, imm(val));
                            a.mov(to, spill);
                        },
                        to,
                        spill);
            }
        } else if (from.isWord()) {
            auto val = from.as<ArgWord>().get();

            if (Support::isInt32((Sint)val)) {
                preserve_cache(
                        [&]() {
                            a.mov(to, imm(val));
                        },
                        to);
            } else {
                preserve_cache(
                        [&]() {
                            a.mov(spill, imm(val));
                            a.mov(to, spill);
                        },
                        to,
                        spill);
            }
        } else {
            mov_arg(spill, from);
            mov_preserve_cache(to, spill);
        }
    }

    void mov_arg(const ArgRegister &to, x86::Gp from, const x86::Gp &spill) {
        (void)spill;

        auto mem = getArgRef(to);
        store_cache(from, mem);
    }

    void mov_arg(const ArgRegister &to, x86::Mem from, const x86::Gp &spill) {
        a.mov(spill, from);
        a.mov(getArgRef(to), spill);
    }

    void mov_arg(const ArgRegister &to, BeamInstr from, const x86::Gp &spill) {
        preserve_cache(
                [&]() {
                    if (Support::isInt32((Sint)from)) {
                        a.mov(getArgRef(to), imm(from));
                    } else {
                        a.mov(spill, imm(from));
                        mov_arg(to, spill);
                    }
                },
                getArgRef(to),
                spill);
    }

    void mov_arg(const ArgRegister &to,
                 const ArgVal &from,
                 const x86::Gp &spill) {
        if (!from.isRegister()) {
            mov_arg(getArgRef(to), from);
        } else {
            x86::Gp from_reg = find_cache(getArgRef(from));

            if (from_reg.isValid()) {
                comment("skipped fetching of BEAM register");
            } else {
                from_reg = spill;
                mov_arg(from_reg, from);
            }
            mov_arg(to, from_reg);
        }
    }

    /* Set the Z flag if Reg1 and Reg2 are definitely not equal based
     * on their tags alone. (They may still be equal if both are
     * immediates and all other bits are equal too.)
     *
     * Clobbers RET.
     */
    void emit_is_unequal_based_on_tags(Label Unequal,
                                       const ArgVal &Src1,
                                       x86::Gp Reg1,
                                       const ArgVal &Src2,
                                       x86::Gp Reg2,
                                       Distance dist = dLong) {
        ERTS_CT_ASSERT(TAG_PRIMARY_IMMED1 == _TAG_PRIMARY_MASK);
        ERTS_CT_ASSERT((TAG_PRIMARY_LIST | TAG_PRIMARY_BOXED) ==
                       TAG_PRIMARY_IMMED1);

        if (always_one_of<BeamTypeId::AlwaysBoxed>(Src1)) {
            emit_is_boxed(Unequal, Reg2, dist);
        } else if (always_one_of<BeamTypeId::AlwaysBoxed>(Src2)) {
            emit_is_boxed(Unequal, Reg1, dist);
        } else if (exact_type<BeamTypeId::Cons>(Src1)) {
            emit_is_cons(Unequal, Reg2, dist);
        } else if (exact_type<BeamTypeId::Cons>(Src2)) {
            emit_is_cons(Unequal, Reg1, dist);
        } else {
            a.mov(RETd, Reg1.r32());
            a.or_(RETd, Reg2.r32());

            if (never_one_of<BeamTypeId::Cons>(Src1) ||
                never_one_of<BeamTypeId::Cons>(Src2)) {
                emit_is_boxed(Unequal, RET, dist);
            } else if (never_one_of<BeamTypeId::AlwaysBoxed>(Src1) ||
                       never_one_of<BeamTypeId::AlwaysBoxed>(Src2)) {
                emit_is_cons(Unequal, RET, dist);
            } else {
                a.and_(RETb, imm(_TAG_PRIMARY_MASK));

                /* RET will now be TAG_PRIMARY_IMMED1 if either one or
                 * both registers are immediates, or if one register
                 * is a list and the other a boxed. */
                a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
                if (dist == dShort) {
                    a.short_().je(Unequal);
                } else {
                    a.je(Unequal);
                }
            }
        }
    }

    /* Set the Z flag if Reg1 and Reg2 are both immediates. */
    void emit_are_both_immediate(const ArgVal &Src1,
                                 x86::Gp Reg1,
                                 const ArgVal &Src2,
                                 x86::Gp Reg2) {
        ERTS_CT_ASSERT(TAG_PRIMARY_IMMED1 == _TAG_PRIMARY_MASK);
        if (always_immediate(Src1)) {
            a.mov(RETd, Reg2.r32());
        } else if (always_immediate(Src2)) {
            a.mov(RETd, Reg1.r32());
        } else {
            a.mov(RETd, Reg1.r32());
            a.and_(RETd, Reg2.r32());
        }
        a.and_(RETb, imm(_TAG_PRIMARY_MASK));
        a.cmp(RETb, imm(TAG_PRIMARY_IMMED1));
    }
};

void *beamasm_metadata_insert(std::string module_name,
                              ErtsCodePtr base_address,
                              size_t code_size,
                              const std::vector<AsmRange> &ranges);
void beamasm_metadata_early_init();
void beamasm_metadata_late_init();
