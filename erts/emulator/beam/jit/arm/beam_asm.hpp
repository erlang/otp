/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2022. All Rights Reserved.
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
#include <queue>
#include <map>
#include <functional>
#include <algorithm>
#include <cmath>

#ifndef ASMJIT_ASMJIT_H_INCLUDED
#    include <asmjit/asmjit.hpp>
#endif

#include <asmjit/a64.h>

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

/* Is it safe to STP or LDP `Struct->Field1` and `Struct->Field2`? */
#define ERTS_CT_ASSERT_FIELD_PAIR(Struct, Field1, Field2)                      \
    static_assert(std::is_standard_layout<Struct>::value &&                    \
                  (offsetof(Struct, Field2) - offsetof(Struct, Field1) ==      \
                   sizeof(((Struct *)nullptr)->Field1)) &&                     \
                  (sizeof(((Struct *)nullptr)->Field1) ==                      \
                   sizeof(((Struct *)nullptr)->Field2)))

using namespace asmjit;

class BeamAssembler : public ErrorHandler {
protected:
    /* Holds code and relocation information. */
    CodeHolder code;

    a64::Assembler a;

    FileLogger logger;

    Section *rodata = nullptr;

    /* * * * * * * * * */

    /* Points at x_reg_array inside an ErtsSchedulerRegisters struct, allowing
     * the aux_regs field to be addressed with an 8-bit displacement. */
    const arm::Gp scheduler_registers = a64::x19;

    const arm::Gp E = a64::x20;
    const arm::Gp c_p = a64::x21;
    const arm::Gp FCALLS = a64::x22;
    const arm::Gp HTOP = a64::x23;

    /* Local copy of the active code index.
     *
     * This is set to ERTS_SAVE_CALLS_CODE_IX when save_calls is active, which
     * routes us to a common handler routine that calls save_calls before
     * jumping to the actual code. */
    const arm::Gp active_code_ix = a64::x24;

    /* X registers */
#if defined(DEBUG)
    /*
     * To ensure that we thoroughly test flushing of caller-save X
     * registers, define more caller-save X registers in a DEBUG
     * build.
     */
#    define ERTS_HIGHEST_CALLEE_SAVE_XREG 2
#    define ERTS_HIGHEST_CALLER_SAVE_XREG 5
    const arm::Gp XREG0 = a64::x25;
    const arm::Gp XREG1 = a64::x26;
    const arm::Gp XREG2 = a64::x27;

    /*
     * Caller-save X registers. Must be flushed before calling C
     * code.
     */
    const arm::Gp XREG3 = a64::x15;
    const arm::Gp XREG4 = a64::x16;
    const arm::Gp XREG5 = a64::x17;
#else
#    define ERTS_HIGHEST_CALLEE_SAVE_XREG 3
#    define ERTS_HIGHEST_CALLER_SAVE_XREG 5
    const arm::Gp XREG0 = a64::x25;
    const arm::Gp XREG1 = a64::x26;
    const arm::Gp XREG2 = a64::x27;
    const arm::Gp XREG3 = a64::x28;

    /*
     * Caller-save X registers. Must be flushed before calling C
     * code.
     */
    const arm::Gp XREG4 = a64::x15;
    const arm::Gp XREG5 = a64::x16;
#endif

#define ERTS_LOWEST_CALLEE_SAVE_XREG (0)
#define ERTS_LOWEST_CALLER_SAVE_XREG (ERTS_HIGHEST_CALLEE_SAVE_XREG + 1)

    static const int num_register_backed_xregs = 6;
    const arm::Gp register_backed_xregs[num_register_backed_xregs] =
            {XREG0, XREG1, XREG2, XREG3, XREG4, XREG5};

#ifdef ERTS_MSACC_EXTENDED_STATES
    const arm::Mem erts_msacc_cache = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.erts_msacc_cache));
#endif

    /* * * * * * * * * */
    const arm::Gp ZERO = a64::xzr;

    /*
     * All of the following registers are caller-save.
     *
     * Note that ARG1 is also the register for the return value.
     */
    const arm::Gp ARG1 = a64::x0;
    const arm::Gp ARG2 = a64::x1;
    const arm::Gp ARG3 = a64::x2;
    const arm::Gp ARG4 = a64::x3;
    const arm::Gp ARG5 = a64::x4;
    const arm::Gp ARG6 = a64::x5;
    const arm::Gp ARG7 = a64::x6;
    const arm::Gp ARG8 = a64::x7;

    const arm::Gp TMP1 = a64::x8;
    const arm::Gp TMP2 = a64::x9;
    const arm::Gp TMP3 = a64::x10;
    const arm::Gp TMP4 = a64::x11;
    const arm::Gp TMP5 = a64::x12;
    const arm::Gp TMP6 = a64::x13;

    /*
     * Assume that SUPER_TMP will be destroyed by any helper function.
     */
    const arm::Gp SUPER_TMP = a64::x14;

    /*
     * Note that x18 is reserved on Apple platforms and must not be used.
     */

    /* Callee-saved floating-point registers.
     *
     * Note that only the bottom 64 bits of these (128-bit) registers are
     * callee-save, so we cannot pack two floats into each register. */
    const arm::VecD FREG0 = a64::d8;
    const arm::VecD FREG1 = a64::d9;
    const arm::VecD FREG2 = a64::d10;
    const arm::VecD FREG3 = a64::d11;
    const arm::VecD FREG4 = a64::d12;
    const arm::VecD FREG5 = a64::d13;
    const arm::VecD FREG6 = a64::d14;
    const arm::VecD FREG7 = a64::d15;
    static const int num_register_backed_fregs = 8;
    const arm::VecD register_backed_fregs[num_register_backed_fregs] =
            {FREG0, FREG1, FREG2, FREG3, FREG4, FREG5, FREG6, FREG7};

    const arm::Mem TMP_MEM1q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[0]));
    const arm::Mem TMP_MEM2q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[1]));
    const arm::Mem TMP_MEM3q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[2]));
    const arm::Mem TMP_MEM4q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[3]));
    const arm::Mem TMP_MEM5q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[4]));

    /* Fill registers with undefined contents to find bugs faster.
     * A boxed value is most likely to cause noticeable trouble. */
    static const Uint64 bad_boxed_ptr = 0xcafebad0000002UL;

    /* Number of highest element displacement for L/SDP and L/STR. */
    static const size_t MAX_LDP_STP_DISPLACEMENT = 0x3F;
    static const size_t MAX_LDR_STR_DISPLACEMENT = 0xFFF;

    /* Constants for "alternate flag state" operands, which are distinct from
     * `arm::CondCode::xyz`. Mainly used in `CCMP` instructions. */
    enum NZCV : int {
        kNF = 8,
        kSigned = kNF,

        kZF = 4,
        kEqual = kZF,

        kCF = 2,
        kCarry = kCF,

        kVF = 1,
        kOverflow = kVF,

        kNone = 0
    };

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

#ifdef JIT_HARD_DEBUG
    constexpr arm::Mem getInitialSPRef() const {
        int base = offsetof(ErtsSchedulerRegisters, initial_sp);

        return getSchedulerRegRef(base);
    }
#endif

    constexpr arm::Mem getSchedulerRegRef(int offset) const {
        ASSERT((offset & (sizeof(Eterm) - 1)) == 0);
        return arm::Mem(scheduler_registers, offset);
    }

    constexpr arm::Mem getFRef(int index, size_t size = sizeof(UWord)) const {
        int base = offsetof(ErtsSchedulerRegisters, f_reg_array.d);
        int offset = index * sizeof(FloatDef);

        ASSERT(0 <= index && index <= 1023);
        return getSchedulerRegRef(base + offset);
    }

    constexpr arm::Mem getXRef(int index) const {
        int base = offsetof(ErtsSchedulerRegisters, x_reg_array.d);
        int offset = index * sizeof(Eterm);

        ASSERT(0 <= index && index < ERTS_X_REGS_ALLOCATED);
        return getSchedulerRegRef(base + offset);
    }

    constexpr arm::Mem getYRef(int index) const {
        ASSERT(0 <= index && index <= 1023);

        return arm::Mem(E, index * sizeof(Eterm));
    }

    constexpr arm::Mem getCARRef(arm::Gp Src) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST);
    }

    constexpr arm::Mem getCDRRef(arm::Gp Src,
                                 size_t size = sizeof(UWord)) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST + sizeof(Eterm));
    }

    /* Loads the X register array into `to`. Remember to sync the registers in
     * `emit_enter_runtime`. */
    void load_x_reg_array(arm::Gp to) {
        int offset = offsetof(ErtsSchedulerRegisters, x_reg_array.d);

        lea(to, getSchedulerRegRef(offset));
    }

    void load_erl_bits_state(arm::Gp to) {
        int offset =
                offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state);

        lea(to, getSchedulerRegRef(offset));
    }

    void emit_assert_redzone_unused() {
#ifdef JIT_HARD_DEBUG
        const int REDZONE_BYTES = S_REDZONE * sizeof(Eterm);
        Label next = a.newLabel();

        a.sub(SUPER_TMP, E, imm(REDZONE_BYTES));
        a.cmp(HTOP, SUPER_TMP);

        a.b_ls(next);
        a.udf(0xbeef);

        a.bind(next);
#endif
    }

    /*
     * Calls an Erlang function.
     */
    template<typename Any>
    void erlang_call(Any Target) {
        emit_assert_redzone_unused();
        aligned_call(Target);
    }

    void branch(arm::Mem target) {
        a.ldr(SUPER_TMP, target);
        a.br(SUPER_TMP);
    }

    template<typename FuncPtr>
    void aligned_call(FuncPtr(*target)) {
        mov_imm(SUPER_TMP, target);
        a.blr(SUPER_TMP);
    }

    void aligned_call(Label target) {
        a.bl(target);
    }

    void aligned_call(arm::Gp target) {
        a.blr(target);
    }

    /* Calls the given address. In DEBUG builds, make
     * sure that the CP is aligned. */
    template<typename OperandType>
    void aligned_call(OperandType target) {
        ERTS_CT_ASSERT(_CPMASK == 3);
        ASSERT(is_CP(a.offset()));
        a.ldr(TMP1, target);
        a.blr(TMP1);
    }

    void runtime_call(arm::Gp func, unsigned args) {
        ASSERT(args < 5);
        a.blr(func);
    }

    template<typename T>
    struct function_arity;
    template<typename T, typename... Args>
    struct function_arity<T(Args...)>
            : std::integral_constant<int, sizeof...(Args)> {};

    template<int expected_arity, typename T>
    void runtime_call(T(*func)) {
        static_assert(expected_arity == function_arity<T>());

        a.mov(TMP1, func);
        a.blr(TMP1);
    }

    /* Explicitly position-independent absolute jump, for use in fragments that
     * need to be memcpy'd for performance reasons (e.g. NIF stubs) */
    template<typename T>
    void pic_jmp(T(*addr)) {
        a.mov(SUPER_TMP, addr);
        a.br(SUPER_TMP);
    }

    constexpr arm::Mem getArgRef(const ArgRegister &arg) const {
        if (arg.isXRegister()) {
            return getXRef(arg.as<ArgXRegister>().get());
        } else if (arg.isYRegister()) {
            return getYRef(arg.as<ArgYRegister>().get());
        }

        return getFRef(arg.as<ArgFRegister>().get());
    }

    /* Returns the current code address for the `Export` or `ErlFunEntry` in
     * `Src`.
     *
     * Export tracing, save_calls, etc are implemented by shared fragments that
     * assume that the respective entry is in ARG1, so we have to copy it over
     * if it isn't already. */
    arm::Mem emit_setup_dispatchable_call(const arm::Gp &Src) {
        return emit_setup_dispatchable_call(Src, active_code_ix);
    }

    arm::Mem emit_setup_dispatchable_call(const arm::Gp &Src,
                                          const arm::Gp &CodeIndex) {
        if (ARG1 != Src) {
            a.mov(ARG1, Src);
        }

        ERTS_CT_ASSERT(offsetof(ErlFunEntry, dispatch) == 0);
        ERTS_CT_ASSERT(offsetof(Export, dispatch) == 0);
        ERTS_CT_ASSERT(offsetof(ErtsDispatchable, addresses) == 0);

        return arm::Mem(ARG1, CodeIndex, arm::lsl(3));
    }

    enum Update : int {
        eStack = (1 << 0),
        eHeap = (1 << 1),
        eReductions = (1 << 2),
        eCodeIndex = (1 << 3),
        eXRegs = (1 << 4)
    };

    void emit_enter_erlang_frame() {
        a.str(a64::x30, arm::Mem(E, -8).pre());
    }

    void emit_leave_erlang_frame() {
        a.ldr(a64::x30, arm::Mem(E).post(8));
    }

    void emit_enter_runtime_frame() {
        a.stp(a64::x29, a64::x30, arm::Mem(a64::sp, -16).pre());
        a.mov(a64::x29, a64::sp);
    }

    void emit_leave_runtime_frame() {
        a.mov(a64::sp, a64::x29);
        a.ldp(a64::x29, a64::x30, arm::Mem(a64::sp).post(16));
    }

    /* We keep the first six X registers in machine registers. Some of those
     * registers are callee-saved and some are caller-saved.
     *
     * We ignore the ones above `live` to reduce the save/restore traffic on
     * these registers. It's enough for this figure to be at least as high as
     * the number of actually live registers, and we default to all six
     * registers when we don't know the exact number.
     *
     * Furthermore, we only save the callee-save registers when told to sync
     * all registers with the `Update::eXRegs` flag, as this is very rarely
     * needed. */

    template<int Spec = 0>
    void emit_enter_runtime(int live = num_register_backed_xregs) {
        ERTS_CT_ASSERT((Spec & (Update::eReductions | Update::eStack |
                                Update::eHeap | Update::eXRegs)) == Spec);

        if ((Spec & Update::eStack) && (Spec & Update::eHeap)) {
            /* Store HTOP and E in one go. */
            ERTS_CT_ASSERT_FIELD_PAIR(Process, htop, stop);
            a.stp(HTOP, E, arm::Mem(c_p, offsetof(Process, htop)));
        } else if (Spec & Update::eStack) {
            a.str(E, arm::Mem(c_p, offsetof(Process, stop)));
        } else if (Spec & Update::eHeap) {
            a.str(HTOP, arm::Mem(c_p, offsetof(Process, htop)));
        }

        if (Spec & Update::eReductions) {
            a.str(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        }

        /* Save register-backed X registers to the X register array when
         * needed. The backing registers must NOT be used afterwards.
         *
         * In a DEBUG build, the backing X registers will be overwritten with
         * garbage values. */
        if (live > 0) {
            int num_to_save = MIN(live, ERTS_HIGHEST_CALLER_SAVE_XREG + 1);
            int i;

            if (Spec & Update::eXRegs) {
                i = ERTS_LOWEST_CALLEE_SAVE_XREG;
            } else {
                /* If we don't need to sync the X register array, then we can
                 * get away with saving only the fragile X registers. */
                i = ERTS_LOWEST_CALLER_SAVE_XREG;
            }

#ifdef DEBUG
            /* Destroy the saved X registers to find bugs sooner.*/
            if (i < num_to_save) {
                mov_imm(SUPER_TMP, bad_boxed_ptr + 0x20 + (Spec << 8));
            }
#endif

            while (i < num_to_save - 1) {
                a.stp(register_backed_xregs[i + 0],
                      register_backed_xregs[i + 1],
                      getXRef(i));

#ifdef DEBUG
                a.mov(register_backed_xregs[i + 0], SUPER_TMP);
                a.mov(register_backed_xregs[i + 1], SUPER_TMP);
#endif

                i += 2;
            }

            if (i < num_to_save) {
                a.str(register_backed_xregs[i], getXRef(i));

#ifdef DEBUG
                a.mov(register_backed_xregs[i], SUPER_TMP);
#endif
            }
        }
    }

    template<int Spec = 0>
    void emit_leave_runtime(int live = num_register_backed_xregs) {
        ERTS_CT_ASSERT(
                (Spec & (Update::eReductions | Update::eStack | Update::eHeap |
                         Update::eXRegs | Update::eCodeIndex)) == Spec);

        if ((Spec & Update::eStack) && (Spec & Update::eHeap)) {
            /* Load HTOP and E in one go. */
            ERTS_CT_ASSERT_FIELD_PAIR(Process, htop, stop);
            a.ldp(HTOP, E, arm::Mem(c_p, offsetof(Process, htop)));
        } else if (Spec & Update::eHeap) {
            a.ldr(HTOP, arm::Mem(c_p, offsetof(Process, htop)));
        } else if (Spec & Update::eStack) {
            a.ldr(E, arm::Mem(c_p, offsetof(Process, stop)));
        }

        if (Spec & Update::eReductions) {
            a.ldr(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        }

        if (Spec & Update::eCodeIndex) {
            /* Updates the local copy of the active code index, retaining
             * save_calls if active. */
            mov_imm(SUPER_TMP, &the_active_code_index);
            a.ldr(SUPER_TMP.w(), arm::Mem(SUPER_TMP));
            a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
            a.csel(active_code_ix,
                   active_code_ix,
                   SUPER_TMP,
                   arm::CondCode::kEQ);
        }

        /* Restore register-backed X registers from the X register array when
         * needed. The register array must NOT be used afterwards.
         *
         * In a DEBUG build, the register array will be overwritten with
         * garbage values. */
        if (live > 0) {
            int num_to_restore = MIN(live, ERTS_HIGHEST_CALLER_SAVE_XREG + 1);
            int i;

            if (Spec & Update::eXRegs) {
                i = ERTS_LOWEST_CALLEE_SAVE_XREG;
            } else {
                /* If we don't need to sync the X register array, then we can
                 * get away with loading only the fragile X registers. */
                i = ERTS_LOWEST_CALLER_SAVE_XREG;
            }

#ifdef DEBUG
            /* Destroy the restored X registers to find bugs sooner.*/
            if (i < num_to_restore) {
                mov_imm(SUPER_TMP, bad_boxed_ptr + 0x80 + (Spec << 8));
            }
#endif

            while (i < num_to_restore - 1) {
                a.ldp(register_backed_xregs[i],
                      register_backed_xregs[i + 1],
                      getXRef(i));

#ifdef DEBUG
                a.stp(SUPER_TMP, SUPER_TMP, getXRef(i));
#endif

                i += 2;
            }

            if (i < num_to_restore) {
                a.ldr(register_backed_xregs[i], getXRef(i));

#ifdef DEBUG
                a.str(SUPER_TMP, getXRef(i));
#endif
            }
        }
    }

    void emit_is_boxed(Label Fail, arm::Gp Src) {
        const int bitNumber = 0;
        ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED ==
                       (1 << bitNumber));
        a.tbnz(Src, imm(bitNumber), Fail);
    }

    void emit_is_not_boxed(Label Fail, arm::Gp Src) {
        const int bitNumber = 0;
        ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED ==
                       (1 << bitNumber));
        a.tbz(Src, imm(bitNumber), Fail);
    }

    arm::Gp emit_ptr_val(arm::Gp Dst, arm::Gp Src) {
#if !defined(TAG_LITERAL_PTR)
        return Src;
#else
        /* We intentionally skip TAG_PTR_MASK__ here, as we want to use
         * plain `emit_boxed_val` when we know the argument can't be a literal,
         * such as in bit-syntax matching.
         *
         * This comes at very little cost as `emit_boxed_val` nearly always has
         * a displacement. */
        a.and_(Dst, Src, imm(~TAG_LITERAL_PTR));
        return Dst;
#endif
    }

    void emit_untag_ptr(arm::Gp Dst, arm::Gp Src) {
        a.and_(Dst, Src, imm(~TAG_PTR_MASK__));
    }

    constexpr arm::Mem emit_boxed_val(arm::Gp Src, int32_t bytes = 0) const {
        ASSERT(bytes % sizeof(Eterm) == 0);
        return arm::Mem(Src, bytes - TAG_PRIMARY_BOXED);
    }

    void emit_branch_if_not_value(arm::Gp reg, Label lbl) {
        emit_branch_if_eq(reg, THE_NON_VALUE, lbl);
    }

    void emit_branch_if_value(arm::Gp reg, Label lbl) {
        emit_branch_if_ne(reg, THE_NON_VALUE, lbl);
    }

    void emit_branch_if_eq(arm::Gp reg, Uint value, Label lbl) {
        if (value == 0) {
            a.cbz(reg, lbl);
        } else {
            a.cmp(reg, imm(value));
            a.b_eq(lbl);
        }
    }

    void emit_branch_if_ne(arm::Gp reg, Uint value, Label lbl) {
        if (value == 0) {
            a.cbnz(reg, lbl);
        } else {
            a.cmp(reg, imm(value));
            a.b_ne(lbl);
        }
    }

    /* Set the Z flag if Reg1 and Reg2 are both immediates. */
    void emit_are_both_immediate(arm::Gp Reg1, arm::Gp Reg2) {
        ERTS_CT_ASSERT(TAG_PRIMARY_IMMED1 == _TAG_PRIMARY_MASK);
        a.and_(SUPER_TMP, Reg1, Reg2);
        a.and_(SUPER_TMP, SUPER_TMP, imm(_TAG_PRIMARY_MASK));
        a.cmp(SUPER_TMP, imm(TAG_PRIMARY_IMMED1));
    }

    /* Set the Z flag if Reg1 and Reg2 are definitely not equal based
     * on their tags alone. (They may still be equal if both are
     * immediates and all other bits are equal too.) */
    void emit_is_unequal_based_on_tags(arm::Gp Reg1, arm::Gp Reg2) {
        ERTS_CT_ASSERT(TAG_PRIMARY_IMMED1 == _TAG_PRIMARY_MASK);
        ERTS_CT_ASSERT((TAG_PRIMARY_LIST | TAG_PRIMARY_BOXED) ==
                       TAG_PRIMARY_IMMED1);
        a.orr(SUPER_TMP, Reg1, Reg2);
        a.and_(SUPER_TMP, SUPER_TMP, imm(_TAG_PRIMARY_MASK));

        /*
         * SUPER_TMP will be now be TAG_PRIMARY_IMMED1 if either
         * one or both registers are immediates, or if one register
         * is a list and the other a boxed.
         */
        a.cmp(SUPER_TMP, imm(TAG_PRIMARY_IMMED1));
    }

    template<typename T>
    void mov_imm(arm::Gp to, T value) {
        static_assert(std::is_integral<T>::value || std::is_pointer<T>::value);
        if (value) {
            a.mov(to, imm(value));
        } else {
            a.mov(to, ZERO);
        }
    }

    void mov_imm(arm::Gp to, std::nullptr_t value) {
        (void)value;
        mov_imm(to, 0);
    }

    void sub(arm::Gp to, arm::Gp src, int64_t val) {
        if (val < 0) {
            add(to, src, -val);
        } else if (val < (1 << 24)) {
            if (val & 0xFFF) {
                a.sub(to, src, imm(val & 0xFFF));
                src = to;
            }

            if (val & 0xFFF000) {
                a.sub(to, src, imm(val & 0xFFF000));
            }
        } else {
            mov_imm(SUPER_TMP, val);
            a.sub(to, src, SUPER_TMP);
        }
    }

    void add(arm::Gp to, arm::Gp src, int64_t val) {
        if (val < 0) {
            sub(to, src, -val);
        } else if (val < (1 << 24)) {
            if (val & 0xFFF) {
                a.add(to, src, imm(val & 0xFFF));
                src = to;
            }

            if (val & 0xFFF000) {
                a.add(to, src, imm(val & 0xFFF000));
            }
        } else {
            mov_imm(SUPER_TMP, val);
            a.add(to, src, SUPER_TMP);
        }
    }

    void cmp(arm::Gp src, int64_t val) {
        if (Support::isUInt12(val)) {
            a.cmp(src, imm(val));
        } else if (Support::isUInt12(-val)) {
            a.cmn(src, imm(-val));
        } else {
            mov_imm(SUPER_TMP, val);
            a.cmp(src, SUPER_TMP);
        }
    }

    void ldur(arm::Gp reg, arm::Mem mem) {
        safe_9bit_imm(a64::Inst::kIdLdur, reg, mem);
    }

    void stur(arm::Gp reg, arm::Mem mem) {
        safe_9bit_imm(a64::Inst::kIdStur, reg, mem);
    }

    void safe_9bit_imm(uint32_t instId, arm::Gp reg, arm::Mem mem) {
        int64_t offset = mem.offset();

        ASSERT(mem.hasBaseReg() && !mem.hasIndex());

        if (Support::isInt9(offset)) {
            a.emit(instId, reg, mem);
        } else {
            lea(SUPER_TMP, mem);
            a.emit(instId, reg, arm::Mem(SUPER_TMP));
        }
    }

    /*
     * ARM has no LEA instruction. Implement our own to enable us
     * to use helpers based on getSchedulerRegRef() that return an
     * arm::Mem class.
     */
    void lea(arm::Gp to, arm::Mem mem) {
        int64_t offset = mem.offset();

        ASSERT(mem.hasBaseReg() && !mem.hasIndex());

        if (offset == 0) {
            a.mov(to, arm::GpX(mem.baseId()));
        } else {
            add(to, arm::GpX(mem.baseId()), offset);
        }
    }

public:
    void embed_rodata(const char *labelName, const char *buff, size_t size);
    void embed_bss(const char *labelName, size_t size);
    void embed_zeros(size_t size);

    void setLogger(std::string log);
    void setLogger(FILE *log);

    void comment(const char *format) {
        if (logger.file()) {
            a.commentf("# %s", format);
        }
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
        ErtsCodePtr start;
        ErtsCodePtr stop;
        const std::string name;

        struct LineData {
            ErtsCodePtr start;
            const std::string file;
            unsigned line;
        };

        const std::vector<LineData> lines;
    };

    void embed(void *data, uint32_t size) {
        a.embed((char *)data, size);
    }
};

#include "beam_asm_global.hpp"

class BeamModuleAssembler : public BeamAssembler {
    typedef unsigned BeamLabel;

    /* Map of BEAM label number to asmjit Label. These should not be used
     * directly by most instructions because of displacement limits, use
     * `resolve_beam_label` instead. */
    typedef std::unordered_map<BeamLabel, const Label> LabelMap;
    LabelMap rawLabels;

    /* Sequence number used to create unique named labels by
     * resolve_label(). Only used when assembly output has been
     * requested. */
    long labelSeq = 0;

    struct patch {
        Label where;
        uint64_t val_offs;
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

    /* The BEAM file we've been loaded from, if any. */
    const BeamFile *beam;

    BeamGlobalAssembler *ga;

    /* Used by emit to populate the labelToMFA map */
    Label currLabel;

    /* The offset of our BeamCodeHeader, if any. */
    Label codeHeader;

    /* The module's on_load function, if any. */
    Label on_load;

    /* The end of the last function. Note that the dispatch table, constants,
     * and veneers may follow. */
    Label code_end;

    Eterm mod;

    /* Save the last PC for an error. */
    size_t last_error_offset = 0;

    static constexpr ptrdiff_t STUB_CHECK_INTERVAL = 4 << 10;
    size_t last_stub_check_offset = 0;

    enum Displacement : size_t {
        /* Pessimistic estimate for helper functions, where we don't know the
         * branch displacement or whether it will be used near label
         * resolution.
         *
         * Note that we subtract the size of one instruction to handle
         * backward displacements. */
        dispUnknown = (32 << 10) - sizeof(Uint32) - STUB_CHECK_INTERVAL,

        /* +- 32KB: `tbz`, `tbnz`, `ldr` of 8-byte literal. */
        disp32K = (32 << 10) - sizeof(Uint32),

        /* +- 1MB: `adr`, `b.cond`, `cb.cond` */
        disp1MB = (1 << 20) - sizeof(Uint32),

        /* +- 128MB: `b`, `blr` */
        disp128MB = (128 << 20) - sizeof(Uint32),

        dispMin = dispUnknown,
        dispMax = disp128MB
    };

    static_assert(dispMin <= dispUnknown && dispMax >= disp128MB);
    static_assert(STUB_CHECK_INTERVAL < dispMin / 2);

    struct Veneer {
        ssize_t latestOffset;
        Label anchor;

        Label target;

        constexpr bool operator>(const Veneer &other) const {
            return latestOffset > other.latestOffset;
        }
    };

    struct Constant {
        ssize_t latestOffset;
        Label anchor;

        ArgVal value;

        constexpr bool operator>(const Constant &other) const {
            return latestOffset > other.latestOffset;
        }
    };

    /* ArgVal -> Constant
     *
     * `_pending_constants` points directly into this container, which is
     * documented to be safe as long as we only insert elements. */
    std::unordered_multimap<ArgVal, const Constant, ArgVal::Hash> _constants;

    /* Label::id() -> Veneer
     *
     * `_pending_veneers` points directly into this container. */
    std::unordered_multimap<uint32_t, const Veneer> _veneers;

    template<typename T>
    using PendingStubs =
            std::priority_queue<std::reference_wrapper<const T>,
                                std::deque<std::reference_wrapper<const T>>,
                                std::greater<const T &>>;

    /* All pending stubs, segregated by type and sorted by `latestOffset` in
     * ascending order.
     *
     * We use separate queues to avoid interleaving them, as they have
     * different sizes and alignment requirements. */
    PendingStubs<Constant> _pending_constants;
    PendingStubs<Veneer> _pending_veneers;

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

    void register_metadata(const BeamCodeHeader *header);

    ErtsCodePtr getCode(unsigned label);
    ErtsCodePtr getLambda(unsigned index);

    void *getCode(Label label) {
        return BeamAssembler::getCode(label);
    }

    byte *getCode(char *labelName) {
        return BeamAssembler::getCode(labelName);
    }

    void embed_vararg_rodata(const Span<ArgVal> &args, a64::Gp reg);

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
    int getTypeUnion(const ArgSource &arg) const {
        auto typeIndex =
                arg.isRegister() ? arg.as<ArgRegister>().typeIndex() : 0;

        ASSERT(typeIndex < beam->types.count);
        return beam->types.entries[typeIndex].type_union;
    }

    auto getClampedRange(const ArgSource &arg) const {
        if (arg.isSmall()) {
            Sint value = arg.as<ArgSmall>().getSigned();
            return std::make_pair(value, value);
        } else {
            auto typeIndex =
                    arg.isRegister() ? arg.as<ArgRegister>().typeIndex() : 0;

            ASSERT(typeIndex < beam->types.count);
            const auto &entry = beam->types.entries[typeIndex];
            if (entry.min <= entry.max) {
                return std::make_pair(entry.min, entry.max);
            } else if (IS_SSMALL(entry.min) && !IS_SSMALL(entry.max)) {
                return std::make_pair(entry.min, MAX_SMALL);
            } else if (!IS_SSMALL(entry.min) && IS_SSMALL(entry.max)) {
                return std::make_pair(MIN_SMALL, entry.max);
            } else {
                return std::make_pair(MIN_SMALL, MAX_SMALL);
            }
        }
    }

    int getSizeUnit(const ArgSource &arg) const {
        auto typeIndex =
                arg.isRegister() ? arg.as<ArgRegister>().typeIndex() : 0;

        ASSERT(typeIndex < beam->types.count);
        return beam->types.entries[typeIndex].size_unit;
    }

    bool hasLowerBound(const ArgSource &arg) const {
        auto typeIndex =
                arg.isRegister() ? arg.as<ArgRegister>().typeIndex() : 0;
        ASSERT(typeIndex < beam->types.count);
        const auto &entry = beam->types.entries[typeIndex];
        return IS_SSMALL(entry.min) && !IS_SSMALL(entry.max);
    }

    bool hasUpperBound(const ArgSource &arg) const {
        auto typeIndex =
                arg.isRegister() ? arg.as<ArgRegister>().typeIndex() : 0;
        ASSERT(typeIndex < beam->types.count);
        const auto &entry = beam->types.entries[typeIndex];
        return !IS_SSMALL(entry.min) && IS_SSMALL(entry.max);
    }

    bool always_small(const ArgSource &arg) const {
        if (arg.isSmall()) {
            return true;
        }

        auto typeIndex =
                arg.isRegister() ? arg.as<ArgRegister>().typeIndex() : 0;
        ASSERT(typeIndex < beam->types.count);
        const auto &entry = beam->types.entries[typeIndex];
        return entry.type_union == BEAM_TYPE_INTEGER && entry.min <= entry.max;
    }

    bool always_immediate(const ArgSource &arg) const {
        if (arg.isImmed() || always_small(arg)) {
            return true;
        }

        int type_union = getTypeUnion(arg);
        return (type_union & BEAM_TYPE_MASK_ALWAYS_IMMEDIATE) == type_union;
    }

    bool always_same_types(const ArgSource &lhs, const ArgSource &rhs) const {
        int lhs_types = getTypeUnion(lhs);
        int rhs_types = getTypeUnion(rhs);

        /* We can only be certain that the types are the same when there's
         * one possible type. For example, if one is a number and the other
         * is an integer, they could differ if the former is a float. */
        if ((lhs_types & (lhs_types - 1)) == 0) {
            return lhs_types == rhs_types;
        }

        return false;
    }

    bool always_one_of(const ArgSource &arg, int types) const {
        if (arg.isImmed()) {
            if (arg.isSmall()) {
                return !!(types & BEAM_TYPE_INTEGER);
            } else if (arg.isAtom()) {
                return !!(types & BEAM_TYPE_ATOM);
            } else if (arg.isNil()) {
                return !!(types & BEAM_TYPE_NIL);
            }

            return false;
        } else {
            int type_union = getTypeUnion(arg);
            return type_union == (type_union & types);
        }
    }

    int masked_types(const ArgSource &arg, int mask) const {
        if (arg.isImmed()) {
            if (arg.isSmall()) {
                return mask & BEAM_TYPE_INTEGER;
            } else if (arg.isAtom()) {
                return mask & BEAM_TYPE_ATOM;
            } else if (arg.isNil()) {
                return mask & BEAM_TYPE_NIL;
            }

            return BEAM_TYPE_NONE;
        } else {
            return getTypeUnion(arg) & mask;
        }
    }

    bool exact_type(const ArgSource &arg, int type_id) const {
        return always_one_of(arg, type_id);
    }

    bool is_sum_small_if_args_are_small(const ArgSource &LHS,
                                        const ArgSource &RHS) {
        Sint min, max;
        auto [min1, max1] = getClampedRange(LHS);
        auto [min2, max2] = getClampedRange(RHS);
        min = min1 + min2;
        max = max1 + max2;
        return IS_SSMALL(min) && IS_SSMALL(max);
    }

    bool is_diff_small_if_args_are_small(const ArgSource &LHS,
                                         const ArgSource &RHS) {
        Sint min, max;
        auto [min1, max1] = getClampedRange(LHS);
        auto [min2, max2] = getClampedRange(RHS);
        min = min1 - max2;
        max = max1 - min2;
        return IS_SSMALL(min) && IS_SSMALL(max);
    }

    bool is_product_small_if_args_are_small(const ArgSource &LHS,
                                            const ArgSource &RHS) {
        auto [min1, max1] = getClampedRange(LHS);
        auto [min2, max2] = getClampedRange(RHS);
        auto mag1 = std::max(std::abs(min1), std::abs(max1));
        auto mag2 = std::max(std::abs(min2), std::abs(max2));

        /*
         * mag1 * mag2 <= MAX_SMALL
         * mag1 <= MAX_SMALL / mag2   (when mag2 != 0)
         */
        ERTS_CT_ASSERT(MAX_SMALL < -MIN_SMALL);
        return mag2 == 0 || mag1 <= MAX_SMALL / mag2;
    }

    bool is_bsl_small(const ArgSource &LHS, const ArgSource &RHS) {
        if (!(always_small(LHS) && always_small(RHS))) {
            return false;
        } else {
            auto [min1, max1] = getClampedRange(LHS);
            auto [min2, max2] = getClampedRange(RHS);

            if (min1 < 0 || max1 == 0 || min2 < 0) {
                return false;
            }

            return max2 < Support::clz(max1) - _TAG_IMMED1_SIZE;
        }
    }

    /* Helpers */
    void emit_gc_test(const ArgWord &Stack,
                      const ArgWord &Heap,
                      const ArgWord &Live);
    void emit_gc_test_preserve(const ArgWord &Need,
                               const ArgWord &Live,
                               const ArgSource &Preserve,
                               arm::Gp preserve_reg);

    arm::Mem emit_variable_apply(bool includeI);
    arm::Mem emit_fixed_apply(const ArgWord &arity, bool includeI);

    arm::Gp emit_call_fun(bool skip_box_test = false,
                          bool skip_fun_test = false,
                          bool skip_arity_test = false);

    arm::Gp emit_is_binary(const ArgLabel &Fail,
                           const ArgSource &Src,
                           Label next,
                           Label subbin);

    void emit_is_boxed(Label Fail, arm::Gp Src) {
        BeamAssembler::emit_is_boxed(Fail, Src);
    }

    void emit_is_boxed(Label Fail, const ArgVal &Arg, arm::Gp Src) {
        if (always_one_of(Arg, BEAM_TYPE_MASK_ALWAYS_BOXED)) {
            comment("skipped box test since argument is always boxed");
            return;
        }

        BeamAssembler::emit_is_boxed(Fail, Src);
    }

    /* Copies `count` words from the address at `from`, to the address at `to`.
     *
     * Clobbers v30 and v31. */
    void emit_copy_words_increment(arm::Gp from, arm::Gp to, size_t count);

    void emit_get_list(const arm::Gp boxed_ptr,
                       const ArgRegister &Hd,
                       const ArgRegister &Tl);

    void emit_add_sub_types(bool is_small_result,
                            const ArgSource &LHS,
                            const a64::Gp lhs_reg,
                            const ArgSource &RHS,
                            const a64::Gp rhs_reg,
                            const Label next);

    void emit_are_both_small(const ArgSource &LHS,
                             const a64::Gp lhs_reg,
                             const ArgSource &RHS,
                             const a64::Gp rhs_reg,
                             const Label next);

    void emit_div_rem(const ArgLabel &Fail,
                      const ArgSource &LHS,
                      const ArgSource &RHS,
                      const ErtsCodeMFA *error_mfa,
                      const ArgRegister &Quotient,
                      const ArgRegister &Remainder,
                      bool need_div,
                      bool need_rem);

    void emit_i_bif(const ArgLabel &Fail,
                    const ArgWord &Bif,
                    const ArgRegister &Dst);

    void emit_error(int code);

    int emit_bs_get_field_size(const ArgSource &Size,
                               int unit,
                               Label Fail,
                               const arm::Gp &out);

    void emit_bs_get_utf8(const ArgRegister &Ctx, const ArgLabel &Fail);
    void emit_bs_get_utf16(const ArgRegister &Ctx,
                           const ArgLabel &Fail,
                           const ArgWord &Flags);
    void update_bin_state(arm::Gp bin_base,
                          arm::Gp bin_offset,
                          Sint bit_offset,
                          Sint size,
                          arm::Gp size_reg);
    void set_zero(Sint effectiveSize);

    void emit_raise_exception();
    void emit_raise_exception(const ErtsCodeMFA *exp);
    void emit_raise_exception(Label I, const ErtsCodeMFA *exp);

    void emit_validate(const ArgWord &Arity);
    void emit_bs_skip_bits(const ArgLabel &Fail, const ArgRegister &Ctx);

    void emit_linear_search(arm::Gp val, Label fail, const Span<ArgVal> &args);

    void emit_float_instr(uint32_t instId,
                          const ArgFRegister &LHS,
                          const ArgFRegister &RHS,
                          const ArgFRegister &Dst);

    void emit_validate_unicode(Label next, Label fail, arm::Gp value);

    void emit_bif_is_eq_ne_exact_immed(const ArgSource &LHS,
                                       const ArgSource &RHS,
                                       const ArgRegister &Dst,
                                       Eterm fail_value,
                                       Eterm succ_value);

    void emit_proc_lc_unrequire(void);
    void emit_proc_lc_require(void);

    void emit_nyi(const char *msg);
    void emit_nyi(void);

    /* Returns a vector of the untagged and rebased `args`. The adjusted
     * `comparand` is stored in ARG1. */
    const std::vector<ArgVal> emit_select_untag(const Span<ArgVal> &args,
                                                a64::Gp comparand,
                                                Label fail,
                                                UWord base,
                                                int shift);

    void emit_binsearch_nodes(arm::Gp reg,
                              size_t Left,
                              size_t Right,
                              Label fail,
                              const Span<ArgVal> &args);

    bool emit_optimized_three_way_select(arm::Gp reg,
                                         Label fail,
                                         const Span<ArgVal> &args);

#ifdef DEBUG
    void emit_tuple_assertion(const ArgSource &Src, arm::Gp tuple_reg);
#endif

#include "beamasm_protos.h"

    /* Resolves a BEAM label.
     *
     * When the branch type is not `dispUnknown`, this must be used
     * _IMMEDIATELY BEFORE_ the instruction that the label is used in. */
    const Label &resolve_beam_label(const ArgLabel &Label,
                                    enum Displacement disp);
    const Label &resolve_label(const Label &target,
                               enum Displacement disp,
                               const char *name = nullptr);

    /* Resolves a shared fragment, creating a trampoline that loads the
     * appropriate address before jumping there.
     *
     * When the branch type is not `dispUnknown`, this must be used
     * _IMMEDIATELY BEFORE_ the instruction that the label is used in. */
    const Label &resolve_fragment(void (*fragment)(), enum Displacement disp);

    /* Embeds a constant argument and returns its address. All kinds of
     * constants are accepted, including labels and export entries.
     *
     * When the branch type is not `dispUnknown`, this must be used
     * _IMMEDIATELY BEFORE_ the instruction that the label is used in. */
    arm::Mem embed_constant(const ArgVal &value, enum Displacement disp);

    /* Convenience wrapper for embedding raw pointers or immediates. */
    template<typename T,
             std::enable_if_t<std::is_integral<T>::value ||
                                      std::is_pointer<T>::value,
                              bool> = true>
    arm::Mem embed_constant(T data, enum Displacement disp) {
        return embed_constant(ArgWord((UWord)data), disp);
    }

    /* Binds a label and all related veneers that are within reach of it. */
    void bind_veneer_target(const Label &target);

    void emit_constant(const Constant &constant);
    void emit_veneer(const Veneer &veneer);

    /* Unconditionally emits all veneers and constants that are due within
     * `range` bytes. */
    void flush_pending_stubs(size_t range);

    /* Emits pending veneers when appropriate. Must be called at least once
     * every `STUB_CHECK_INTERVAL` bytes for veneers and constants to work. */
    void check_pending_stubs();

    /* Calls the given shared fragment, ensuring that the redzone is unused and
     * that the return address forms a valid CP. */
    template<typename Any>
    void fragment_call(Any target) {
        emit_assert_redzone_unused();

#if defined(JIT_HARD_DEBUG)
        /* Verify that the stack has not grown. */
        Label next = a.newLabel();
        a.ldr(SUPER_TMP, getInitialSPRef());
        a.cmp(a64::sp, SUPER_TMP);
        a.b_eq(next);
        a.udf(0xdead);
        a.bind(next);
#endif

        a.bl(resolve_fragment((void (*)())target, disp128MB));
    }

    template<typename T>
    struct function_arity;
    template<typename T, typename... Args>
    struct function_arity<T(Args...)>
            : std::integral_constant<int, sizeof...(Args)> {};

    template<int expected_arity, typename T>
    void runtime_call(T(*func)) {
        static_assert(expected_arity == function_arity<T>());

        a.bl(resolve_fragment((void (*)())func, disp128MB));
    }

    bool isRegisterBacked(const ArgVal &arg) {
        if (arg.isXRegister()) {
            return arg.as<ArgXRegister>().get() < num_register_backed_xregs;
        } else if (arg.isFRegister()) {
            return arg.as<ArgFRegister>().get() < num_register_backed_fregs;
        }

        return false;
    }

    template<typename RegType = arm::Gp>
    struct Variable {
        RegType reg;
        arm::Mem mem;

        Variable(RegType _r) : Variable(_r, arm::Mem()) {
        }
        Variable(RegType _r, arm::Mem _mem) : reg(_r), mem(_mem) {
        }
    };

    Variable<arm::Gp> init_destination(const ArgVal &arg, arm::Gp tmp) {
        /* We don't support storing into GpW since their maximum displacement
         * is 16K, which means we have to check stubs far more often. */
        ASSERT(tmp.isGpX());

        if (isRegisterBacked(arg)) {
            auto index = arg.as<ArgXRegister>().get();
            return Variable(register_backed_xregs[index]);
        } else {
            return Variable(tmp, getArgRef(arg));
        }
    }

    Variable<arm::VecD> init_destination(const ArgVal &arg, arm::VecD tmp) {
        if (isRegisterBacked(arg)) {
            auto index = arg.as<ArgFRegister>().get();
            return Variable(register_backed_fregs[index]);
        } else {
            return Variable(tmp, getArgRef(arg));
        }
    }

    Variable<arm::Gp> load_source(const ArgVal &arg, arm::Gp tmp) {
        /* We don't support loading into GpW since their maximum displacement
         * is 16K, which means we have to check stubs far more often. */
        ASSERT(tmp.isGpX());

        if (arg.isLiteral()) {
            a.ldr(tmp, embed_constant(arg, disp32K));
            return Variable(tmp);
        } else if (arg.isRegister()) {
            if (isRegisterBacked(arg)) {
                auto index = arg.as<ArgXRegister>().get();
                return Variable(register_backed_xregs[index]);
            }

            auto ref = getArgRef(arg);
            a.ldr(tmp, ref);
            return Variable(tmp, ref);
        } else {
            if (arg.isImmed() || arg.isWord()) {
                auto val = arg.isImmed() ? arg.as<ArgImmed>().get()
                                         : arg.as<ArgWord>().get();

                if (Support::isIntOrUInt32(val)) {
                    mov_imm(tmp, val);
                    return Variable(tmp);
                }
            }

            a.ldr(tmp, embed_constant(arg, disp32K));
            return Variable(tmp);
        }
    }

    auto load_sources(const ArgVal &Src1,
                      arm::Gp tmp1,
                      const ArgVal &Src2,
                      arm::Gp tmp2) {
        if (!isRegisterBacked(Src1) && !isRegisterBacked(Src2)) {
            switch (ArgVal::memory_relation(Src1, Src2)) {
            case ArgVal::Relation::consecutive:
                safe_ldp(tmp1, tmp2, Src1, Src2);
                return std::make_pair(Variable(tmp1, getArgRef(Src1)),
                                      Variable(tmp2, getArgRef(Src2)));
            case ArgVal::Relation::reverse_consecutive:
                safe_ldp(tmp2, tmp1, Src2, Src1);
                return std::make_pair(Variable(tmp1, getArgRef(Src1)),
                                      Variable(tmp2, getArgRef(Src2)));
            case ArgVal::Relation::none:
                break;
            }
        }

        return std::make_pair(load_source(Src1, tmp1), load_source(Src2, tmp2));
    }

    Variable<arm::VecD> load_source(const ArgVal &arg, arm::VecD tmp) {
        if (isRegisterBacked(arg)) {
            auto index = arg.as<ArgFRegister>().get();
            return Variable<arm::VecD>(register_backed_fregs[index]);
        }

        a.ldr(tmp, getArgRef(arg));
        return Variable<arm::VecD>(tmp);
    }

    template<typename Reg>
    void mov_var(const Variable<Reg> &to, const Variable<Reg> &from) {
        mov_var(to.reg, from);
    }

    template<typename Reg>
    void mov_var(const Variable<Reg> &to, Reg from) {
        if (to.reg != from) {
            a.mov(to.reg, from);
        }
    }

    template<typename Reg>
    void mov_var(Reg to, const Variable<Reg> &from) {
        if (to != from.reg) {
            a.mov(to, from.reg);
        }
    }

    template<typename Reg>
    void flush_var(const Variable<Reg> &to) {
        if (to.mem.hasBase()) {
            a.str(to.reg, to.mem);
        }
    }

    void flush_vars(const Variable<arm::Gp> &to1,
                    const Variable<arm::Gp> &to2) {
        const arm::Mem &mem1 = to1.mem;
        const arm::Mem &mem2 = to2.mem;

        if (mem1.hasBaseReg() && mem2.hasBaseReg() &&
            mem1.baseId() == mem2.baseId()) {
            if (mem1.offset() + 8 == mem2.offset()) {
                safe_stp(to1.reg, to2.reg, mem1);
                return;
            } else if (mem1.offset() == mem2.offset() + 8) {
                safe_stp(to2.reg, to1.reg, mem2);
                return;
            }
        }

        /* Not possible to optimize with stp. */
        flush_var(to1);
        flush_var(to2);
    }

    void mov_arg(const ArgVal &To, const ArgVal &From) {
        if (isRegisterBacked(To)) {
            auto to = init_destination(To, SUPER_TMP);
            auto from = load_source(From, to.reg);
            mov_var(to, from);
            flush_var(to);
        } else {
            auto from = load_source(From, SUPER_TMP);
            auto to = init_destination(To, from.reg);
            mov_var(to, from);
            flush_var(to);
        }
    }

    void mov_arg(const ArgVal &To, arm::Mem From) {
        auto to = init_destination(To, SUPER_TMP);
        a.ldr(to.reg, From);
        flush_var(to);
    }

    void mov_arg(arm::Mem To, const ArgVal &From) {
        auto from = load_source(From, SUPER_TMP);
        auto to = Variable(from.reg, To);
        flush_var(to);
    }

    void mov_arg(arm::Gp to, const ArgVal &from) {
        auto r = load_source(from, to);
        if (r.reg != to) {
            a.mov(to, r.reg);
        }
    }

    void mov_arg(const ArgVal &to, arm::Gp from) {
        auto r = init_destination(to, from);
        if (r.reg != from) {
            a.mov(r.reg, from);
        }
        flush_var(r);
    }

    void cmp_arg(arm::Gp gp, const ArgVal &arg) {
        if (arg.isImmed() || arg.isWord()) {
            Sint val = arg.isImmed() ? arg.as<ArgImmed>().get()
                                     : arg.as<ArgWord>().get();
            cmp(gp, val);
            return;
        }

        auto tmp = load_source(arg, SUPER_TMP);
        a.cmp(gp, tmp.reg);
    }

    void safe_stp(arm::Gp gp1,
                  arm::Gp gp2,
                  const ArgVal &Dst1,
                  const ArgVal &Dst2) {
        ASSERT(ArgVal::memory_relation(Dst1, Dst2) ==
               ArgVal::Relation::consecutive);
        safe_stp(gp1, gp2, getArgRef(Dst1));
    }

    void safe_stp(arm::Gp gp1, arm::Gp gp2, arm::Mem mem) {
        auto offset = mem.offset();

        ASSERT(gp1.isGpX() && gp2.isGpX());

        if (std::abs(offset) <= sizeof(Eterm) * MAX_LDP_STP_DISPLACEMENT) {
            a.stp(gp1, gp2, mem);
        } else if (std::abs(offset) <
                   sizeof(Eterm) * MAX_LDR_STR_DISPLACEMENT) {
            /* Note that we used `<` instead of `<=`, as we're loading two
             * elements rather than one. */
            a.str(gp1, mem);
            a.str(gp2, mem.cloneAdjusted(sizeof(Eterm)));
        } else {
            add(SUPER_TMP, arm::GpX(mem.baseId()), offset);
            a.stp(gp1, gp2, arm::Mem(SUPER_TMP));
        }
    }

    void safe_ldr(arm::Gp gp, arm::Mem mem) {
        auto offset = mem.offset();

        ASSERT(mem.hasBaseReg() && !mem.hasIndex());
        ASSERT(gp.isGpX());

        if (std::abs(offset) <= sizeof(Eterm) * MAX_LDR_STR_DISPLACEMENT) {
            a.ldr(gp, mem);
        } else {
            add(SUPER_TMP, arm::GpX(mem.baseId()), offset);
            a.ldr(gp, arm::Mem(SUPER_TMP));
        }
    }

    void safe_ldp(arm::Gp gp1,
                  arm::Gp gp2,
                  const ArgVal &Src1,
                  const ArgVal &Src2) {
        ASSERT(ArgVal::memory_relation(Src1, Src2) ==
               ArgVal::Relation::consecutive);

        safe_ldp(gp1, gp2, getArgRef(Src1));
    }

    void safe_ldp(arm::Gp gp1, arm::Gp gp2, arm::Mem mem) {
        auto offset = mem.offset();

        ASSERT(gp1.isGpX() && gp2.isGpX());
        ASSERT(gp1 != gp2);

        if (std::abs(offset) <= sizeof(Eterm) * MAX_LDP_STP_DISPLACEMENT) {
            a.ldp(gp1, gp2, mem);
        } else if (std::abs(offset) <
                   sizeof(Eterm) * MAX_LDR_STR_DISPLACEMENT) {
            /* Note that we used `<` instead of `<=`, as we're loading two
             * elements rather than one. */
            a.ldr(gp1, mem);
            a.ldr(gp2, mem.cloneAdjusted(sizeof(Eterm)));
        } else {
            add(SUPER_TMP, arm::GpX(mem.baseId()), offset);
            a.ldp(gp1, gp2, arm::Mem(SUPER_TMP));
        }
    }
};

void beamasm_metadata_update(
        std::string module_name,
        ErtsCodePtr base_address,
        size_t code_size,
        const std::vector<BeamAssembler::AsmRange> &ranges);
void beamasm_metadata_early_init();
void beamasm_metadata_late_init();
