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
#include <queue>
#include <map>
#include <functional>

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

    /*
     * Note that x18 is reserved on Apple platforms and must not be used.
     */

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
     * A boxed value is most likely to cause noticeable trouble.
     */
    static const Uint64 bad_boxed_ptr = 0xcafebad0000002UL;

    /* Number of highest element displacement for stp/ldp. */
    static const int MAX_LDP_STP_DISPLACEMENT = 0x3F;

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

        a.cond_ls().b(next);
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

    constexpr arm::Mem getArgRef(const ArgVal &val) const {
        switch (val.getType()) {
        case ArgVal::FReg:
            return getFRef(val.getValue());
        case ArgVal::XReg:
            return getXRef(val.getValue());
        case ArgVal::YReg:
            return getYRef(val.getValue());
        default:
            ERTS_ASSERT(!"NYI");
            return arm::Mem();
        }
    }

    /* Returns the current code address for the export entry in `Src`
     *
     * Export tracing, save_calls, etc is implemented by shared fragments that
     * assume that the export entry is in ARG1, so we have to copy it over if it
     * isn't already. */
    arm::Mem emit_setup_export_call(const arm::Gp &Src) {
        return emit_setup_export_call(Src, active_code_ix);
    }

    arm::Mem emit_setup_export_call(const arm::Gp &Src,
                                    const arm::Gp &CodeIndex) {
        if (ARG1 != Src) {
            a.mov(ARG1, Src);
        }
        ERTS_CT_ASSERT(offsetof(Export, addresses) == 0);
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
     * sync all registers with the `Update::eXRegs` flag, as this is very
     * rarely needed. */

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
            a.csel(active_code_ix, active_code_ix, SUPER_TMP, arm::Cond::kEQ);
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
            a.cond_eq().b(lbl);
        }
    }

    void emit_branch_if_ne(arm::Gp reg, Uint value, Label lbl) {
        if (value == 0) {
            a.cbnz(reg, lbl);
        } else {
            a.cmp(reg, imm(value));
            a.cond_ne().b(lbl);
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
                a.sub(to, src, val & 0xFFF);
            }

            if (val & 0xFFF000) {
                a.sub(to, src, val & 0xFFF000);
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
                a.add(to, src, val & 0xFFF);
            }

            if (val & 0xFFF000) {
                a.add(to, to, val & 0xFFF000);
            }
        } else {
            mov_imm(SUPER_TMP, val);
            a.add(to, src, SUPER_TMP);
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

    void embed(void *data, uint32_t size) {
        a.embed((char *)data, size);
    }
};

class BeamGlobalAssembler : public BeamAssembler {
    typedef void (BeamGlobalAssembler::*emitFptr)(void);
    typedef void (*fptr)(void);

    /* Please keep this in alphabetical order. */
#define BEAM_GLOBAL_FUNCS(_)                                                   \
    _(apply_fun_shared)                                                        \
    _(arith_compare_shared)                                                    \
    _(bif_nif_epilogue)                                                        \
    _(bif_export_trap)                                                         \
    _(bif_bit_size_body)                                                       \
    _(bif_bit_size_guard)                                                      \
    _(bif_byte_size_body)                                                      \
    _(bif_byte_size_guard)                                                     \
    _(bif_element_body_shared)                                                 \
    _(bif_element_guard_shared)                                                \
    _(bif_is_eq_exact_shared)                                                  \
    _(bif_is_ne_exact_shared)                                                  \
    _(bif_tuple_size_body)                                                     \
    _(bif_tuple_size_guard)                                                    \
    _(bs_add_guard_shared)                                                     \
    _(bs_add_body_shared)                                                      \
    _(bs_get_tail_shared)                                                      \
    _(bs_size_check_shared)                                                    \
    _(call_bif_shared)                                                         \
    _(call_light_bif_shared)                                                   \
    _(call_nif_yield_helper)                                                   \
    _(catch_end_shared)                                                        \
    _(call_nif_early)                                                          \
    _(call_nif_shared)                                                         \
    _(check_float_error)                                                       \
    _(debug_bp)                                                                \
    _(dispatch_bif)                                                            \
    _(dispatch_nif)                                                            \
    _(dispatch_return)                                                         \
    _(dispatch_save_calls)                                                     \
    _(export_trampoline)                                                       \
    _(fconv_shared)                                                            \
    _(handle_and_error)                                                        \
    _(handle_call_fun_error)                                                   \
    _(handle_hd_error)                                                         \
    _(handle_map_size_error)                                                   \
    _(handle_not_error)                                                        \
    _(handle_or_error)                                                         \
    _(handle_tl_error)                                                         \
    _(garbage_collect)                                                         \
    _(generic_bp_global)                                                       \
    _(generic_bp_local)                                                        \
    _(i_band_body_shared)                                                      \
    _(i_bnot_body_shared)                                                      \
    _(i_bnot_guard_shared)                                                     \
    _(i_bor_body_shared)                                                       \
    _(i_bif_body_shared)                                                       \
    _(i_bif_guard_shared)                                                      \
    _(i_bsr_body_shared)                                                       \
    _(i_bsl_body_shared)                                                       \
    _(i_func_info_shared)                                                      \
    _(i_length_guard_shared)                                                   \
    _(i_length_body_shared)                                                    \
    _(i_loop_rec_shared)                                                       \
    _(i_new_small_map_lit_shared)                                              \
    _(i_test_yield_shared)                                                     \
    _(i_bxor_body_shared)                                                      \
    _(int_div_rem_body_shared)                                                 \
    _(int_div_rem_guard_shared)                                                \
    _(minus_body_shared)                                                       \
    _(new_map_shared)                                                          \
    _(update_map_assoc_shared)                                                 \
    _(unloaded_fun)                                                            \
    _(plus_body_shared)                                                        \
    _(process_exit)                                                            \
    _(process_main)                                                            \
    _(raise_exception)                                                         \
    _(raise_exception_shared)                                                  \
    _(times_body_shared)                                                       \
    _(times_guard_shared)                                                      \
    _(unary_minus_body_shared)                                                 \
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

    void emit_i_length_common(Label fail, int state_size);

    void emit_raise_badarg(const ErtsCodeMFA *mfa);

    void emit_bif_bit_size_helper(Label fail);
    void emit_bif_byte_size_helper(Label fail);
    void emit_bif_element_helper(Label fail);
    void emit_bif_tuple_size_helper(Label fail);

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

    /* Map of BEAM label number to asmjit Label. These should not be used
     * directly by most instructions because of displacement limits, use
     * `resolve_beam_label` instead. */
    typedef std::unordered_map<BeamLabel, Label> LabelMap;
    LabelMap rawLabels;

    /* Map of label number to function name. Only defined for the
     * entry label of a function. This map will be populated and
     * used only when assembly output has been requested. */
    typedef std::unordered_map<BeamLabel, std::string> LabelNames;
    LabelNames labelNames;

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

    BeamGlobalAssembler *ga;

    /* Used by emit to populate the labelToMFA map */
    Label currLabel;

    /* Special shared fragments that must reside in each module. */
    Label codeHeader;
    Label genericBPTramp;

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
    /* Helpers */
    void emit_gc_test(const ArgVal &Stack,
                      const ArgVal &Heap,
                      const ArgVal &Live);
    void emit_gc_test_preserve(const ArgVal &Need,
                               const ArgVal &Live,
                               arm::Gp term);

    arm::Mem emit_variable_apply(bool includeI);
    arm::Mem emit_fixed_apply(const ArgVal &arity, bool includeI);

    arm::Gp emit_call_fun();

    arm::Gp emit_is_binary(const ArgVal &Fail,
                           const ArgVal &Src,
                           Label next,
                           Label subbin);

    void emit_get_list(const arm::Gp boxed_ptr,
                       const ArgVal &Hd,
                       const ArgVal &Tl);

    void emit_div_rem(const ArgVal &Fail,
                      const ArgVal &LHS,
                      const ArgVal &RHS,
                      const ErtsCodeMFA *error_mfa);

    void emit_i_bif(const ArgVal &Fail, const ArgVal &Bif, const ArgVal &Dst);

    void emit_error(int code);

    int emit_bs_get_field_size(const ArgVal &Size,
                               int unit,
                               Label Fail,
                               const arm::Gp &out);

    void emit_bs_get_utf8(const ArgVal &Ctx, const ArgVal &Fail);
    void emit_bs_get_utf16(const ArgVal &Ctx,
                           const ArgVal &Fail,
                           const ArgVal &Flags);

    void emit_raise_exception();
    void emit_raise_exception(const ErtsCodeMFA *exp);
    void emit_raise_exception(Label I, const ErtsCodeMFA *exp);

    void emit_validate(const ArgVal &arity);
    void emit_bs_skip_bits(const ArgVal &Fail, const ArgVal &Ctx);

    void emit_linear_search(arm::Gp val, Label fail, const Span<ArgVal> &args);

    void emit_float_instr(uint32_t instId,
                          const ArgVal &LHS,
                          const ArgVal &RHS,
                          const ArgVal &Dst);

    void emit_validate_unicode(Label next, Label fail, arm::Gp value);

    void emit_bif_is_eq_ne_exact_immed(const ArgVal &Src,
                                       const ArgVal &Immed,
                                       const ArgVal &Dst,
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
    void emit_tuple_assertion(const ArgVal &Src, arm::Gp tuple_reg);
#endif

#include "beamasm_protos.h"

    /* Resolves a BEAM label.
     *
     * When the branch type is not `dispUnknown`, this must be used
     * _IMMEDIATELY BEFORE_ the instruction that the label is used in. */
    Label resolve_beam_label(const ArgVal &Label, enum Displacement disp);
    Label resolve_label(Label target,
                        enum Displacement disp,
                        std::string *name = nullptr);

    /* Resolves a shared fragment, creating a trampoline that loads the
     * appropriate address before jumping there.
     *
     * When the branch type is not `dispUnknown`, this must be used
     * _IMMEDIATELY BEFORE_ the instruction that the label is used in. */
    Label resolve_fragment(void (*fragment)(), enum Displacement disp);

    /* Embeds a constant argument and returns its address. All kinds of
     * constants are accepted, including labels and export entries.
     *
     * When the branch type is not `dispUnknown`, this must be used
     * _IMMEDIATELY BEFORE_ the instruction that the label is used in. */
    arm::Mem embed_constant(const ArgVal &value, enum Displacement disp);

    /* Convenience wrapper for embedding raw pointers or immediates. */
    template<typename T>
    arm::Mem embed_constant(T data, enum Displacement disp) {
        static_assert(std::is_integral<T>::value || std::is_pointer<T>::value);
        return embed_constant(ArgVal(ArgVal::Word, (UWord)data), disp);
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
    void fragment_call(Any Target) {
        emit_assert_redzone_unused();

#if defined(JIT_HARD_DEBUG)
        /* Verify that the stack has not grown. */
        Label next = a.newLabel();
        a.ldr(SUPER_TMP, getInitialSPRef());
        a.cmp(a64::sp, SUPER_TMP);
        a.cond_eq().b(next);
        a.udf(0xdead);
        a.bind(next);
#endif

        a.bl(resolve_fragment((void (*)())Target, disp128MB));
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
        switch (arg.getType()) {
        case ArgVal::XReg:
            return arg.getValue() < num_register_backed_xregs;
        case ArgVal::FReg:
            return arg.getValue() < num_register_backed_fregs;
        default:
            return false;
        }
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
            arm::Gp reg = register_backed_xregs[arg.getValue()];
            return Variable(reg);
        } else {
            return Variable(tmp, getArgRef(arg));
        }
    }

    Variable<arm::VecD> init_destination(const ArgVal &arg, arm::VecD tmp) {
        if (isRegisterBacked(arg)) {
            return Variable(register_backed_fregs[arg.getValue()]);
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
        } else if (isRegisterBacked(arg)) {
            arm::Gp xreg = register_backed_xregs[arg.getValue()];
            return Variable(xreg);
        } else if (arg.isConstant()) {
            if ((arg.isImmed() || arg.isWord()) &&
                Support::isIntOrUInt32(arg.getValue())) {
                mov_imm(tmp, arg.getValue());
            } else {
                a.ldr(tmp, embed_constant(arg, disp32K));
            }

            return Variable(tmp);
        } else {
            /* Register */
            a.ldr(tmp, getArgRef(arg));
            return Variable(tmp, getArgRef(arg));
        }
    }

    auto load_sources(const ArgVal &Src1,
                      arm::Gp tmp1,
                      const ArgVal &Src2,
                      arm::Gp tmp2) {
        if (Src1.isRegister() && Src2.isRegister() && !isRegisterBacked(Src1) &&
            !isRegisterBacked(Src2)) {
            switch (ArgVal::register_relation(Src1, Src2)) {
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
            return Variable<arm::VecD>(register_backed_fregs[arg.getValue()]);
        } else {
            /* Register */
            a.ldr(tmp, getArgRef(arg));
            return Variable<arm::VecD>(tmp);
        }
    }

    void mov_var(const Variable<arm::Gp> &to, const Variable<arm::Gp> &from) {
        mov_var(to.reg, from);
    }

    void mov_var(const Variable<arm::Gp> &to, arm::Gp from) {
        if (to.reg != from) {
            a.mov(to.reg, from);
        }
    }

    void mov_var(arm::Gp to, const Variable<arm::Gp> &from) {
        if (to != from.reg) {
            a.mov(to, from.reg);
        }
    }

    void flush_var(const Variable<arm::Gp> &to) {
        if (to.mem.hasBase()) {
            a.str(to.reg, to.mem);
        }
    }

    void flush_var(const Variable<arm::VecD> &to) {
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

    void cmp_arg(arm::Gp gp, const ArgVal &val) {
        if ((val.isImmed() || val.isWord())) {
            if (Support::isUInt12((Sint)val.getValue())) {
                a.cmp(gp, imm(val.getValue()));
            } else if (Support::isUInt12(-(Sint)val.getValue())) {
                a.adds(ZERO, gp, imm(-(Sint)val.getValue()));
            } else {
                mov_arg(SUPER_TMP, val);
                a.cmp(gp, SUPER_TMP);
            }
        } else {
            mov_arg(SUPER_TMP, val);
            a.cmp(gp, SUPER_TMP);
        }
    }

    void safe_stp(arm::Gp gp1,
                  arm::Gp gp2,
                  const ArgVal &Dst1,
                  const ArgVal &Dst2) {
        ASSERT(ArgVal::register_relation(Dst1, Dst2) ==
               ArgVal::Relation::consecutive);
        safe_stp(gp1, gp2, getArgRef(Dst1));
    }

    void safe_stp(arm::Gp gp1, arm::Gp gp2, arm::Mem mem) {
        int64_t offset = mem.offset();

        ASSERT(gp1.isGpX() && gp2.isGpX());

        if (offset <= sizeof(Eterm) * MAX_LDP_STP_DISPLACEMENT) {
            a.stp(gp1, gp2, mem);
        } else {
            a.str(gp1, mem);
            a.str(gp2, mem.cloneAdjusted(sizeof(Eterm)));
        }
    }

    void safe_ldr(arm::Gp gp, arm::Mem mem) {
        int64_t offset = mem.offset();

        ASSERT(mem.hasBaseReg() && !mem.hasIndex());
        ASSERT(gp.isGpX());

        if (offset < sizeof(Eterm) * 4096) {
            a.ldr(gp, mem);
        } else {
            mov_imm(SUPER_TMP, offset);
            a.add(SUPER_TMP, arm::GpX(mem.baseId()), SUPER_TMP);
            a.ldr(gp, arm::Mem(SUPER_TMP));
        }
    }

    void safe_ldp(arm::Gp gp1,
                  arm::Gp gp2,
                  const ArgVal &Src1,
                  const ArgVal &Src2) {
        ASSERT(ArgVal::register_relation(Src1, Src2) ==
               ArgVal::Relation::consecutive);

        safe_ldp(gp1, gp2, getArgRef(Src1));
    }

    void safe_ldp(arm::Gp gp1, arm::Gp gp2, arm::Mem mem) {
        int64_t offset = mem.offset();

        ASSERT(gp1.isGpX() && gp2.isGpX());
        ASSERT(gp1 != gp2);

        if (offset <= sizeof(Eterm) * MAX_LDP_STP_DISPLACEMENT) {
            a.ldp(gp1, gp2, mem);
        } else {
            safe_ldr(gp1, mem);
            safe_ldr(gp2, mem.cloneAdjusted(sizeof(Eterm)));
        }
    }
};

void beamasm_update_perf_info(std::string modulename,
                              std::vector<BeamAssembler::AsmRange> &ranges);
