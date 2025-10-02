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
#include <queue>
#include <map>
#include <functional>
#include <algorithm>
#include <cmath>

#ifndef ASMJIT_ASMJIT_H_INCLUDED
#    include <asmjit/asmjit.hpp>
#endif

#include <asmjit/a32.h>

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
    a32::Assembler a;

    /* Points at x_reg_array inside an ErtsSchedulerRegisters struct, allowing
     * the aux_regs field to be addressed with an 8-bit displacement. */
    const a32::Gp scheduler_registers = a32::r4;

    const a32::Gp E = a32::r7; // Erlang Stack pointer
    const a32::Gp c_p = a32::r8; // Current Process pointer
    const a32::Gp FCALLS = a32::r9; // Function call counter (reductions)
    const a32::Gp HTOP = a32::r10; // Erlang Heap pointer

    /* Local copy of the active code index.
     *
     * This is set to ERTS_SAVE_CALLS_CODE_IX when save_calls is active, which
     * routes us to a common handler routine that calls save_calls before
     * jumping to the actual code. */
    const a32::Gp active_code_ix = a32::r5;

    /*
     * All of the following registers are caller-save.
     *
     * Note that ARG1 is also the register for the return value.
     */
    const a32::Gp ARG1 = a32::r0;
    const a32::Gp ARG2 = a32::r1;
    const a32::Gp ARG3 = a32::r2;
    const a32::Gp ARG4 = a32::r3;

    const a32::Gp TMP = a32::r12;

    const a32::Gp VAR = a32::r6;

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

    constexpr arm::Mem getCARRef(a32::Gp Src) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST);
    }

    constexpr arm::Mem getCDRRef(a32::Gp Src,
                                 size_t size = sizeof(UWord)) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST + sizeof(Eterm));
    }

    /* Loads the X register array into `to`. Remember to sync the registers in
     * `emit_enter_runtime`. */
    void load_x_reg_array(a32::Gp to) {
        int offset = offsetof(ErtsSchedulerRegisters, x_reg_array.d);

        lea(to, getSchedulerRegRef(offset));
    }

    void emit_assert_redzone_unused() {
#ifdef JIT_HARD_DEBUG
        const int REDZONE_BYTES = S_REDZONE * sizeof(Eterm);
        Label next = a.newLabel();

        a.sub(TMP, E, imm(REDZONE_BYTES));
        a.cmp(HTOP, TMP);

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
        a.ldr(TMP, target);
        a.bx(TMP);
    }
    
    template<typename FuncPtr>
    void aligned_call(FuncPtr(*target)) {
        mov_imm(TMP, target);
        a.blx(TMP);
    }
    
    void aligned_call(Label target) {
        a.blx(target);
    }

    void aligned_call(a32::Gp target) {
        a.blx(target);
    }

    /* Calls the given address. In DEBUG builds, make
     * sure that the CP is aligned. */
    template<typename OperandType>
    void aligned_call(OperandType target) {
        ERTS_CT_ASSERT(_CPMASK == 3);
        ASSERT(is_CP(a.offset()));
        a.ldr(TMP, target);
        a.blx(TMP);
    }
    
    void runtime_call(a32::Gp func, unsigned args) {
        ASSERT(false);
    }

    template<typename T>
    struct function_arity;
    template<typename T, typename... Args>
    struct function_arity<T(Args...)>
            : std::integral_constant<int, sizeof...(Args)> {};

    template<int expected_arity, typename T>
    void runtime_call(T(*func)) {
        static_assert(expected_arity == function_arity<T>());
        mov_imm(TMP, func);
        a.blx(TMP);
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
    arm::Mem emit_setup_dispatchable_call(const a32::Gp &Src) {
        return emit_setup_dispatchable_call(Src, active_code_ix);
    }

    arm::Mem emit_setup_dispatchable_call(const a32::Gp &Src,
                                          const a32::Gp &CodeIndex) {
        if (ARG1 != Src) {
            a.mov(ARG1, Src);
        }

        ERTS_CT_ASSERT(offsetof(ErlFunEntry, dispatch) == 0);
        ERTS_CT_ASSERT(offsetof(Export, dispatch) == 0);
        ERTS_CT_ASSERT(offsetof(ErtsDispatchable, addresses) == 0);

        return arm::Mem(ARG1, CodeIndex, arm::lsl(3));
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
        eXRegs = (1 << 4),
        eHeapAlloc = Update::eHeap | Update::eStack,
#ifndef DEBUG
        eHeapOnlyAlloc = Update::eHeap,
#else
        eHeapOnlyAlloc = Update::eHeapAlloc
#endif
    };

    void emit_enter_erlang_frame() {
        a.str(a32::lr, arm::Mem(E, -4).pre());
    }

    void emit_leave_erlang_frame() {
        a.ldr(a32::lr, arm::Mem(E).post(4));
    }

    void emit_enter_runtime_frame() {
        // We save the current frame pointer first
        // and then the content of theLink Register on the stack
        a.push(a32::GpList({a32::fp, a32::lr}));
        // We also update the frame pointer
        a.mov(a32::fp, a32::sp);
    }

    void emit_leave_runtime_frame() {
        // Restore the frame pointer and the return address
        // This also updates the stack pointer
        a.pop(a32::GpList({a32::fp, a32::pc}));
    }

    /*
     * We save the Erlang Stack, Erlang Heap and FCALLS registers in the
     * C structure of the current process (c_p)
    */
    template<int Spec = 0>
    void emit_enter_runtime() {
        ERTS_CT_ASSERT((Spec & (Update::eReductions | Update::eStack |
                                Update::eHeap | Update::eXRegs)) == Spec);
        if (Spec & Update::eStack) {
            a.str(E, arm::Mem(c_p, offsetof(Process, stop)));
        } else {
#ifdef DEBUG
        /* Store some garbage in the process structure to catch missing
         * updates. */
        a.str(active_code_ix, arm::Mem(c_p, offsetof(Process, stop)));
#endif
        }
        if (Spec & Update::eHeap) {
            a.str(HTOP, arm::Mem(c_p, offsetof(Process, htop)));
        } else {
#ifdef DEBUG
            a.str(active_code_ix, arm::Mem(c_p, offsetof(Process, htop)));
#endif
        }
        if (Spec & Update::eReductions) {
            a.str(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        }
        // We do not have any X register cached in machine registers
        // so nothing else needs to be saved.
    }

    template<int Spec = 0>
    void emit_leave_runtime() {
        ERTS_CT_ASSERT(
            (Spec & (Update::eReductions | Update::eStack | Update::eHeap |
                     Update::eXRegs | Update::eCodeIndex)) == Spec);
        if (Spec & Update::eStack) {
            a.ldr(E, arm::Mem(c_p, offsetof(Process, stop)));
        }
        if (Spec & Update::eHeap) {
            a.ldr(HTOP, arm::Mem(c_p, offsetof(Process, htop)));
        }
        if (Spec & Update::eReductions) {
            a.ldr(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        }

        if (Spec & Update::eCodeIndex) {
            /* Updates the local copy of the active code index, retaining
             * save_calls if active. */
            mov_imm(TMP, &the_active_code_index);
            a.ldr(TMP, arm::Mem(TMP));
            a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
            a.mov_ne(active_code_ix, TMP);
        }
    }

    void emit_is_cons(Label Fail, a32::Gp Src) {
        // TODO
        ASSERT(false);
    }

    void emit_is_not_cons(Label Fail, a32::Gp Src) {
        // TODO
        ASSERT(false);
    }

    void emit_is_boxed(Label Fail, a32::Gp Src) {
        const int bitNumber = 0;
        ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED ==
                       (1 << bitNumber));
        // TST performs a AND operation, sets the Z flag to:
        // if isZeroBit(result)
        //     Z = 1
        // else
        //     Z = 0
        a.tst(Src, imm(1 << bitNumber));
        // Branch if Z == 0
        a.b_ne(Fail);
    }

    void emit_is_not_boxed(Label Fail, a32::Gp Src) {
        // TODO
        ASSERT(false);
    }

    a32::Gp emit_ptr_val(a32::Gp Dst, a32::Gp Src) {
#if !defined(TAG_LITERAL_PTR)
        return Src;
#else
        // TODO
        // TAG_LITERAL_PTR is undefined in ARCH_32 and may be not needed
        ASSERT(false);
        return Dst;
#endif
    }

    void emit_untag_ptr(a32::Gp Dst, a32::Gp Src) {
        // TODO
        ASSERT(false);
    }

    constexpr arm::Mem emit_boxed_val(a32::Gp Src, int32_t bytes = 0) const {
        ASSERT(bytes % sizeof(Eterm) == 0);
        return arm::Mem(Src, bytes - TAG_PRIMARY_BOXED);
    }

    void emit_branch_if_not_value(a32::Gp reg, Label lbl) {
        emit_branch_if_eq(reg, THE_NON_VALUE, lbl);
    }

    void emit_branch_if_value(a32::Gp reg, Label lbl) {
        // TODO
        ASSERT(false);
    }

    void emit_branch_if_eq(a32::Gp reg, Uint value, Label lbl) {
        if (value <= 255) {
            a.cmp(reg, imm(value));
        } else {
            mov_imm(TMP, value);
            a.cmp(reg, TMP);
        }
        a.b_eq(lbl);
    }

    void emit_branch_if_ne(a32::Gp reg, Uint value, Label lbl) {
        // TODO
        ASSERT(false);
    }

    /* Set the Z flag if Reg1 and Reg2 are definitely not equal based
     * on their tags alone. (They may still be equal if both are
     * immediates and all other bits are equal too.) */
    void emit_is_unequal_based_on_tags(a32::Gp Reg1, a32::Gp Reg2) {
        // TODO
        ASSERT(false);
    }

    a32::Gp follow_size(const a32::Gp &reg, const a32::Gp &size) {
        // TODO
        ASSERT(false);
        return reg;
    }

    template<typename T>
    void mov_imm(a32::Gp to, T value) {
        static_assert(std::is_integral<T>::value || std::is_pointer<T>::value);
        uint32_t value32;
        if constexpr (std::is_pointer<T>::value) {
            auto uintptr = reinterpret_cast<uintptr_t>(value);
            value32 = static_cast<uint32_t>(uintptr);
        } else {
            value32 = static_cast<uint32_t>(value);
        }
        if (value32 == 0) {
            a.eor(to, to, to);
        } else if (value32 <= 255) {
            a.mov(to, imm(value32));
        } else if (value32 <= UINT16_MAX) {
            a.movw(to, imm(value32));
        } else {
            // move the lower 16 bits
            uint16_t lower16 = value32;
            a.movw(to, imm(lower16));
            // move the upper 16 bits
            uint16_t upper16 = (value32 >> 16);
            a.movt(to, imm(upper16));
        }
    }

    void mov_imm(a32::Gp to, std::nullptr_t value) {
        // TODO
        ASSERT(false);
    }

    void sub(a32::Gp to, a32::Gp src, int64_t val) {
        // TODO
        ASSERT(false);
    }

    void add(a32::Gp to, a32::Gp src, int32_t val) {
        if (val < 0) {
            sub(to, src, -val);
        } else if (val == 0 && to != src) {
            a.mov(to, src);
        } else if (val < (1 << 24)) {
            if (val & 0xFFF) {                  // add the lower 12 bits
                a.add(to, src, imm(val & 0xFFF));
                src = to;
            }

            if (val & 0xFFF000) {               // add the upper 12 bits
                a.add(to, src, imm(val & 0xFFF000));
            }
        } else {
            a32::Gp tmp = follow_size(TMP, to);

            mov_imm(tmp, val);
            a.add(to, src, tmp);
        }
    }

    void subs(a32::Gp to, a32::Gp src, int64_t val) {
        // TODO
        ASSERT(false);
    }

    void cmp(a32::Gp src, int64_t val) {
        // TODO
        ASSERT(false);
    }

    void ldur(a32::Gp reg, arm::Mem mem) {
        // TODO
        ASSERT(false);
    }

    void stur(a32::Gp reg, arm::Mem mem) {
        // TODO
        ASSERT(false);
    }

    void safe_9bit_imm(uint32_t instId, a32::Gp reg, arm::Mem mem) {
        // TODO
        ASSERT(false);
    }

    /*
     * ARM has no LEA instruction. Implement our own to enable us
     * to use helpers based on getSchedulerRegRef() that return an
     * arm::Mem class.
     */
    void lea(a32::Gp to, arm::Mem mem) {
        int32_t offset = mem.offset();

        ASSERT(mem.hasBaseReg() && !mem.hasIndex());
        if (offset == 0) {
            a.mov(to, a32::Gp(mem.baseId()));
        } else {
            add(to, a32::Gp(mem.baseId()), offset);
        }
    }

    /*
     * FOR DEVELOPMENT ONLY
     * NYI: Not Yet Implemented
     * This fun was not present in the global assembler,
     * but to speedup development we can use this to skip
     * global funcitons implementation and focus on the module assembler.
     */
    static void i_emit_nyi(char *msg) {
        erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
    }

    void emit_nyi(const char *msg) {
        // skipping any preparation for the runtime call
        mov_imm(ARG1, msg);
        runtime_call<1>(i_emit_nyi);
        /* Never returns */
    }

};

#include "beam_asm_global.hpp"

class BeamModuleAssembler : public BeamAssembler,
                            public BeamModuleAssemblerCommon {
    BeamGlobalAssembler *ga;

    /* Sequence number used to create unique named labels by
     * resolve_label(). Only used when assembly output has been
     * requested. */
    long labelSeq = 0;

    /* Save the last PC for an error. */
    size_t last_error_offset = 0;

    static constexpr ptrdiff_t STUB_CHECK_INTERVAL = 4 << 10;
    static constexpr ptrdiff_t STUB_CHECK_INTERVAL_UNREACHABLE =
            (4 << 10) - 128;
    size_t last_stub_check_offset = 0;

    /* Save the last known unreachable position. */
    size_t last_unreachable_offset = 0;

    /* Mark this point unreachable. This must be placed at the very end when
     * used in a BEAM instruction, and should not be used in helper
     * functions. */
    void mark_unreachable() {
        last_unreachable_offset = a.offset();
    }

    /* Use within BEAM instructions. */
    void mark_unreachable_check_pending_stubs() {
        mark_unreachable();
        check_pending_stubs();
    }

    bool is_unreachable() {
        return a.offset() == last_unreachable_offset;
    }

    enum Displacement : size_t {
        /* Pessimistic estimate for helper functions, where we don't know the
         * branch displacement or whether it will be used near label
         * resolution.
         *
         * Note that we subtract the size of one instruction to handle
         * backward displacements. */
        dispUnknown = (32 << 10) - sizeof(Uint32) - STUB_CHECK_INTERVAL,

        /* +- 32MB: `b`, `bl`, `blx`, b.cond */
        disp32MB = (32 << 20) - sizeof(Uint32),

        dispMin = dispUnknown,
        dispMax = disp32MB
    };

    static_assert(dispMin <= dispUnknown && dispMax >= disp32MB);
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

    struct EmbeddedLabel {
        ssize_t latestOffset;
        Label anchor;

        Label label;

        constexpr bool operator>(const EmbeddedLabel &other) const {
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

    /* Index of Label -> EmbeddedLabel
     *
     * `_pending_labels` points directly into this container. */
    std::unordered_map<uint32_t, EmbeddedLabel> _embedded_labels;

    /* All pending stubs, segregated by type and sorted by `latestOffset` in
     * ascending order.
     *
     * We use separate queues to avoid interleaving them, as they have
     * different sizes and alignment requirements. */
    PendingStubs<Constant> _pending_constants;
    PendingStubs<Veneer> _pending_veneers;
    PendingStubs<EmbeddedLabel> _pending_labels;

    /* Maps code pointers to thunks that jump to them, letting us treat global
     * fragments as if they were local. */
    std::unordered_map<void (*)(), Label> _dispatchTable;

    RegisterCache<16, arm::Mem, a32::Gp> reg_cache =
            RegisterCache<16, arm::Mem, a32::Gp>(scheduler_registers, E, {});

    void reg_cache_put(arm::Mem mem, a32::Gp src) {
        if (src != TMP) {
            reg_cache.put(mem, src);
        } else {
            reg_cache.invalidate(mem);
        }
    }

    a32::Gp find_cache(arm::Mem mem) {
        return reg_cache.find(a.offset(), mem);
    }

    /* Works as the STR instruction, but also updates the cache. */
    void str_cache(a32::Gp src, arm::Mem mem_dst) {
        reg_cache.consolidate(a.offset());
        reg_cache.invalidate(src);

        a.str(src, mem_dst);

        reg_cache_put(mem_dst, src);
        reg_cache.update(a.offset());
    }

    /* Works as the STP instruction, but also updates the cache. */
    void stp_cache(a32::Gp src1, a32::Gp src2, arm::Mem mem_dst) {
        // TODO
        ASSERT(false);
    }

    /* Works like LDR, but looks in the cache first. */
    void ldr_cached(a32::Gp dst, arm::Mem mem) {
        a32::Gp cached_reg = find_cache(mem);

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
            a.ldr(dst, mem);
            reg_cache.invalidate(dst);
            reg_cache_put(mem, dst);
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
        // TODO
        ASSERT(false);
    }

    void mov_preserve_cache(a32::VecD dst, a32::VecD src) {
        // TODO
        ASSERT(false);
    }

    void mov_preserve_cache(a32::Gp dst, a32::Gp src) {
        preserve_cache(
            [&]() {
                a.mov(dst, src);
            },
            dst);
    }

    void untag_ptr_preserve_cache(a32::Gp dst, a32::Gp src) {
        // TODO
        ASSERT(false);
    }

    arm::Mem embed_label(const Label &label, enum Displacement disp);

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

    void embed_vararg_rodata(const Span<ArgVal> &args, a32::Gp reg);

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
                               a32::Gp preserve_reg);

    arm::Mem emit_variable_apply(bool includeI);
    arm::Mem emit_fixed_apply(const ArgWord &arity, bool includeI);

    a32::Gp emit_call_fun(bool skip_box_test = false,
                          bool skip_header_test = false);

    void emit_is_cons(Label Fail, a32::Gp Src) {
        // TODO
        emit_nyi("emit_is_cons");
    }

    void emit_is_not_cons(Label Fail, a32::Gp Src) {
        // TODO
        emit_nyi("emit_is_not_cons");
    }

    void emit_is_list(Label Fail, a32::Gp Src) {
        // TODO
        emit_nyi("emit_is_list");
    }

    void emit_is_boxed(Label Fail, a32::Gp Src) {
        // TODO
        emit_nyi("emit_is_boxed");
    }

    void emit_is_boxed(Label Fail, const ArgVal &Arg, a32::Gp Src) {
        // TODO
        emit_nyi("emit_is_boxed");
    }

    /* Copies `count` words from the address at `from`, to the address at `to`.
     *
     * Clobbers v30 and v31. */
    void emit_copy_words_increment(a32::Gp from, a32::Gp to, size_t count);

    void emit_get_list(const a32::Gp boxed_ptr,
                       const ArgRegister &Hd,
                       const ArgRegister &Tl);

    void emit_add_sub_types(bool is_small_result,
                            const ArgSource &LHS,
                            const a32::Gp lhs_reg,
                            const ArgSource &RHS,
                            const a32::Gp rhs_reg,
                            const Label next);

    void emit_are_both_small(const ArgSource &LHS,
                             const a32::Gp lhs_reg,
                             const ArgSource &RHS,
                             const a32::Gp rhs_reg,
                             const Label next);

    void emit_div_rem_literal(Sint divisor,
                              const ArgSource &Dividend,
                              a32::Gp dividend,
                              a32::Gp quotient,
                              a32::Gp remainder,
                              const Label &generic,
                              bool need_div,
                              bool need_rem);

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
    void emit_error(int reason, const ArgSource &Src);

    int emit_bs_get_field_size(const ArgSource &Size,
                               int unit,
                               Label Fail,
                               const a32::Gp &out);

    void emit_bs_get_utf8(const ArgRegister &Ctx, const ArgLabel &Fail);
    void emit_bs_get_utf16(const ArgRegister &Ctx,
                           const ArgLabel &Fail,
                           const ArgWord &Flags);
    void update_bin_state(a32::Gp bin_offset,
                          Sint bit_offset,
                          Sint size,
                          a32::Gp size_reg);
    void set_zero(Sint effectiveSize);
    void emit_construct_utf8(const ArgVal &Src,
                             Sint bit_offset,
                             bool is_byte_aligned);

    void emit_read_bits(Uint bits,
                        const a32::Gp bin_offset,
                        const a32::Gp bin_base,
                        const a32::Gp bitdata);

    void emit_extract_integer(const a32::Gp &bitdata,
                              const a32::Gp &small_tag,
                              Uint flags,
                              Uint position,
                              Uint bits,
                              const ArgRegister &Dst);

    void emit_extract_bitstring(const a32::Gp bitdata,
                                Uint position,
                                Uint bits,
                                const ArgRegister &Dst);

    UWord bs_get_flags(const ArgVal &val);

    void emit_raise_exception();
    void emit_raise_exception(const ErtsCodeMFA *exp);
    void emit_raise_exception(Label I, const ErtsCodeMFA *exp);

    void emit_validate(const ArgWord &Arity);
    void emit_bs_skip_bits(const ArgLabel &Fail, const ArgRegister &Ctx);

    void emit_linear_search(a32::Gp val, Label fail, const Span<ArgVal> &args);

    void emit_float_instr(uint32_t instId,
                          const ArgFRegister &LHS,
                          const ArgFRegister &RHS,
                          const ArgFRegister &Dst);

    void emit_validate_unicode(Label next, Label fail, a32::Gp value);

    void ubif_comment(const ArgWord &Bif);

    void emit_cmp_immed_to_bool(arm::CondCode cc,
                                const ArgSource &LHS,
                                const ArgSource &RHS,
                                const ArgRegister &Dst);

    void emit_cond_to_bool(arm::CondCode cc, const ArgRegister &Dst);
    void emit_bif_is_ge_lt(arm::CondCode cc,
                           const ArgSource &LHS,
                           const ArgSource &RHS,
                           const ArgRegister &Dst);
    void emit_bif_min_max(arm::CondCode cc,
                          const ArgSource &LHS,
                          const ArgSource &RHS,
                          const ArgRegister &Dst);

    void emit_proc_lc_unrequire(void);
    void emit_proc_lc_require(void);

    void emit_nyi(const char *msg);
    void emit_nyi(void);

    /* Returns a vector of the untagged and rebased `args`. The adjusted
     * `comparand` is stored in ARG1. */
    const std::vector<ArgVal> emit_select_untag(const ArgSource &Src,
                                                const Span<ArgVal> &args,
                                                a32::Gp comparand,
                                                Label fail,
                                                UWord base,
                                                int shift);

    void emit_binsearch_nodes(a32::Gp reg,
                              size_t Left,
                              size_t Right,
                              Label fail,
                              const Span<ArgVal> &args);

    void emit_optimized_two_way_select(a32::Gp reg,
                                       const ArgVal &value1,
                                       const ArgVal &value2,
                                       const ArgVal &label);

#ifdef DEBUG
    void emit_tuple_assertion(const ArgSource &Src, a32::Gp tuple_reg);
#endif

    void emit_dispatch_return();

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

    /* Unconditionally emits all pending labels. Must only be called when
     * the current code position is unreachable. */
    void flush_pending_labels();

    /* Calls the given shared fragment, ensuring that the redzone is unused and
     * that the return address forms a valid CP. */
    template<typename Any>
    void fragment_call(Any target) {
        emit_assert_redzone_unused();

#if defined(JIT_HARD_DEBUG)
        /* Verify that the stack has not grown. */
        Label next = a.newLabel();

        int sp_offset = offsetof(ErtsSchedulerRegisters, initial_sp);
        mov_imm(TMP, sp_offset);
        a.add(TMP, scheduler_registers, TMP);
        a.ldr(TMP, arm::Mem(TMP));
        a.cmp(a32::sp, TMP);
        a.b_eq(next);
        a.udf(0xdead);
        a.bind(next);
#endif

        a.bl(resolve_fragment((void (*)())target, disp32MB));
    }

    template<typename T>
    struct function_arity;
    template<typename T, typename... Args>
    struct function_arity<T(Args...)>
            : std::integral_constant<int, sizeof...(Args)> {};

    template<int expected_arity, typename T>
    void runtime_call(T(*func)) {
        static_assert(expected_arity == function_arity<T>());

        a.blx(resolve_fragment((void (*)())func, disp32MB));
    }

    bool isRegisterBacked(const ArgVal &arg) {
        // TODO
        ASSERT(false);
        return false;
    }

    template<typename RegType = a32::Gp>
    struct Variable {
        RegType reg;
        arm::Mem mem;

        Variable(RegType _r) : Variable(_r, arm::Mem()) {
        }
        Variable(RegType _r, arm::Mem _mem) : reg(_r), mem(_mem) {
        }
    };

    Variable<a32::Gp> init_destination(const ArgVal &arg, a32::Gp tmp) {
        return Variable(tmp, getArgRef(arg));
    }

    Variable<a32::VecD> init_destination(const ArgVal &arg, a32::VecD tmp) {
        return Variable(tmp, getArgRef(arg));
    }

    Variable<a32::Gp> load_source(const ArgVal &arg, a32::Gp tmp) {
        if (arg.isLiteral()) {
            preserve_cache(
                    [&]() {
                        a.ldr(tmp, embed_constant(arg, disp32MB));
                    },
                    tmp);
            return Variable(tmp);
        } else if (arg.isRegister()) {
            auto ref = getArgRef(arg);
            ldr_cached(tmp, ref);
            return Variable(tmp, ref);
        } else {
            if (arg.isImmed() || arg.isWord()) {
                auto val = arg.isImmed() ? arg.as<ArgImmed>().get()
                                         : arg.as<ArgWord>().get();

                if (Support::isIntOrUInt32(val)) {
                    preserve_cache(
                            [&]() {
                                mov_imm(tmp, val);
                            },
                            tmp);
                    return Variable(tmp);
                }
            }

            preserve_cache(
                    [&]() {
                        a.ldr(tmp, embed_constant(arg, disp32MB));
                    },
                    tmp);
            return Variable(tmp);
        }
    }

    /*
     * Load the argument into ANY register, using the
     * cache to avoid reloading the value.
     *
     * Because it is not possible to predict into which register
     * the value will end up, the following code is UNSAFE:
     *
     *    auto src = load_source(Src);
     *    a.tst(src.reg, ...);
     *    a.mov(TMP2, NIL);
     *    a.ccmp(src.reg, TMP2, ..., ...);
     *
     * If the value of Src happens to end up in TMP2, it will be
     * overwritten before its second use.
     *
     * Basically, the only safe way to use this function is when the
     * register is used immediately and only once. For example:
     *
     *    a.and_(TMP1, load_source(Src), imm(...));
     *    a.cmp(TMP1, imm(...));
     */
    Variable<a32::Gp> load_source(const ArgVal &arg) {
        a32::Gp todo;
        // TODO
        ASSERT(false);
        return Variable(todo);
    }

    auto load_sources(const ArgVal &Src1,
                      a32::Gp tmp1,
                      const ArgVal &Src2,
                      a32::Gp tmp2) {
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

    Variable<a32::VecD> load_source(const ArgVal &arg, a32::VecD tmp) {
        // TODO
        ASSERT(false);
        return Variable<a32::VecD>(tmp);
    }

    void emit_load_args(const ArgSource &Src1,
                        a32::Gp src1_default,
                        const ArgSource &Src2,
                        a32::Gp src2_default,
                        const ArgSource &Src3,
                        a32::Gp src3_default) {
        //TODO
        ASSERT(false);
    }

    template<typename Reg>
    void mov_var(const Variable<Reg> &to, const Variable<Reg> &from) {
        mov_var(to.reg, from);
    }

    template<typename Reg>
    void mov_var(const Variable<Reg> &to, Reg from) {
        if (to.reg != from) {
            mov_preserve_cache(to.reg, from);
        }
    }

    template<typename Reg>
    void mov_var(Reg to, const Variable<Reg> &from) {
        if (to != from.reg) {
            mov_preserve_cache(to, from.reg);
        }
    }

    void flush_var(const Variable<a32::Gp> &to) {
        if (to.mem.hasBase()) {
            str_cache(to.reg, to.mem);
        } else {
            reg_cache.invalidate(to.reg);
        }
    }

    void flush_var(const Variable<a32::VecD> &to) {
        // TODO
        ASSERT(false);
    }

    enum Relation { none, consecutive, reverse_consecutive };

    static Relation memory_relation(const arm::Mem &mem1,
                                    const arm::Mem &mem2) {
        // TODO
        ASSERT(false);
        return none;
    }

    void flush_vars(const Variable<a32::Gp> &to1,
                    const Variable<a32::Gp> &to2) {
        // TODO
        ASSERT(false);
    }

    void flush_vars(const Variable<a32::Gp> &to1,
                    const Variable<a32::Gp> &to2,
                    const Variable<a32::Gp> &to3) {
        // TODO
        ASSERT(false);
    }

    void mov_arg(const ArgRegister &To, const ArgVal &From) {
        auto from = load_source(From, TMP);
        auto to = init_destination(To, from.reg);
        mov_var(to, from);
        flush_var(to);
    }

    void mov_arg(const ArgRegister &To, arm::Mem From) {
        // TODO
        ASSERT(false);
    }

    void mov_arg(arm::Mem To, const ArgVal &From) {
        // TODO
        ASSERT(false);
    }

    void mov_arg(a32::Gp to, const ArgVal &from) {
        auto r = load_source(from, to);
        if (r.reg != to) {
            mov_preserve_cache(to, r.reg);
        }
    }

    void mov_arg(const ArgVal &to, a32::Gp from) {
        // TODO
        ASSERT(false);
    }

    void cmp_arg(a32::Gp gp, const ArgVal &arg) {
        // TODO
        ASSERT(false);
    }

    void safe_str(a32::Gp gp, arm::Mem mem) {
        // TODO
        ASSERT(false);
    }

    void safe_stp(a32::Gp gp1,
                  a32::Gp gp2,

                  const ArgVal &Dst1,
                  const ArgVal &Dst2) {
        // TODO
        ASSERT(false);
    }

    void safe_stp(a32::Gp gp1, a32::Gp gp2, arm::Mem mem) {
        // TODO
        ASSERT(false);
    }

    void safe_ldr(a32::Gp gp, arm::Mem mem) {
        // TODO
        ASSERT(false);
    }

    void safe_ldur(a32::Gp gp, arm::Mem mem) {

        // TODO
        ASSERT(false);
    }

    void safe_ldp(a32::Gp gp1,
                  a32::Gp gp2,
                  const ArgVal &Src1,
                  const ArgVal &Src2) {
        // TODO
        ASSERT(false);
    }

    void safe_ldp(a32::Gp gp1, a32::Gp gp2, arm::Mem mem) {
        // TODO
        ASSERT(false);
    }

    /* Set the Z flag if Reg1 and Reg2 are definitely not equal based
     * on their tags alone. (They may still be equal if both are
     * immediates and all other bits are equal too.) */
    void emit_is_unequal_based_on_tags(Label Unequal,
                                       const ArgVal &Src1,
                                       a32::Gp Reg1,
                                       const ArgVal &Src2,
                                       a32::Gp Reg2) {
        // TODO
        ASSERT(false);

    }

    /* Set the Z flag if Reg1 and Reg2 are both immediates. */
    void emit_are_both_immediate(const ArgVal &Src1,
                                 a32::Gp Reg1,
                                 const ArgVal &Src2,
                                 a32::Gp Reg2) {
        // TODO
        ASSERT(false);
    }
};

void *beamasm_metadata_insert(std::string module_name,
                              ErtsCodePtr base_address,
                              size_t code_size,
                              const std::vector<AsmRange> &ranges);
void beamasm_metadata_early_init();
void beamasm_metadata_late_init();
