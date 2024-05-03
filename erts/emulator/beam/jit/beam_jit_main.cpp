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

#include "beam_asm.hpp"

extern "C"
{
#include "bif.h"
#include "beam_common.h"
#include "code_ix.h"
#include "export.h"
#include "erl_threads.h"

#if defined(__APPLE__)
#    include <libkern/OSCacheControl.h>
#elif defined(WIN32)
#    include <windows.h>
#endif
}

#ifdef ERLANG_FRAME_POINTERS
ErtsFrameLayout ERTS_WRITE_UNLIKELY(erts_frame_layout);
#endif

/* Global configuration variables (under the `+J` prefix) */
#ifdef HAVE_LINUX_PERF_SUPPORT
enum beamasm_perf_flags erts_jit_perf_support;
#endif
/* Force use of single-mapped RWX memory for JIT code */
int erts_jit_single_map = 0;

/*
 * Special Beam instructions.
 */

ErtsCodePtr beam_run_process;
ErtsCodePtr beam_normal_exit;
ErtsCodePtr beam_exit;
ErtsCodePtr beam_export_trampoline;
ErtsCodePtr beam_bif_export_trap;
ErtsCodePtr beam_continue_exit;
ErtsCodePtr beam_save_calls_export;
ErtsCodePtr beam_save_calls_fun;
ErtsCodePtr beam_unloaded_fun;

/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are for the instruction value, and sometimes
**      for the variable reference (one of these), and rogue references
**      will most likely cause chaos.
*/
ErtsCodePtr beam_return_to_trace;   /* OpCode(i_return_to_trace) */
ErtsCodePtr beam_return_trace;      /* OpCode(i_return_trace) */
ErtsCodePtr beam_exception_trace;   /* UGLY also OpCode(i_return_trace) */
ErtsCodePtr beam_call_trace_return; /* OpCode(i_call_trace_return) */

static JitAllocator *jit_allocator;

static BeamGlobalAssembler *bga;
static BeamModuleAssembler *bma;
static CpuInfo cpuinfo;

#if defined(__aarch64__) && !(defined(WIN32) || defined(__APPLE__)) &&         \
        defined(__GNUC__) && defined(ERTS_THR_INSTRUCTION_BARRIER) &&          \
        ETHR_HAVE_GCC_ASM_ARM_IC_IVAU_INSTRUCTION &&                           \
        ETHR_HAVE_GCC_ASM_ARM_DC_CVAU_INSTRUCTION
#    define BEAMASM_MANUAL_ICACHE_FLUSHING
#endif

#ifdef BEAMASM_MANUAL_ICACHE_FLUSHING
static UWord min_icache_line_size;
static UWord min_dcache_line_size;
#endif

static void init_cache_info() {
#if defined(__aarch64__) && defined(BEAMASM_MANUAL_ICACHE_FLUSHING)
    UWord ctr_el0;

    /* DC/IC operate on a cache line basis, so we need to step according to the
     * _smallest_ data and instruction cache line size.
     *
     * Query the "Cache Type Register" MSR to find out what they are. */
    __asm__ __volatile__("mrs %0, ctr_el0\n" : "=r"(ctr_el0));
    min_dcache_line_size = (4 << ((ctr_el0 >> 16) & 0xF));
    min_icache_line_size = (4 << (ctr_el0 & 0xF));
#endif
}

/*
 * Enter all BIFs into the export table.
 *
 * Note that they will all call the error_handler until their modules have been
 * loaded, which may prevent the system from booting if BIFs from non-preloaded
 * modules are apply/3'd while loading code. Ordinary BIF calls will work fine
 * however since they won't go through export entries.
 */
static void install_bifs(void) {
    typedef Eterm (*bif_func_type)(Process *, Eterm *, ErtsCodePtr);
    int i;

    ASSERT(beam_export_trampoline != NULL);
    ASSERT(beam_save_calls_export != NULL);

    for (i = 0; i < BIF_SIZE; i++) {
        BifEntry *entry;
        Export *ep;
        int j;

        entry = &bif_table[i];

        ERTS_ASSERT(entry->arity <= MAX_BIF_ARITY);

        ep = erts_export_put(entry->module, entry->name, entry->arity);

        sys_memset(&ep->info.u, 0, sizeof(ep->info.u));
        ep->info.mfa.module = entry->module;
        ep->info.mfa.function = entry->name;
        ep->info.mfa.arity = entry->arity;
        ep->bif_number = i;

        for (j = 0; j < ERTS_NUM_CODE_IX; j++) {
            erts_activate_export_trampoline(ep, j);
        }

        /* Set up a hidden export entry so we can trap to this BIF without
         * it being seen when tracing. */
        erts_init_trap_export(BIF_TRAP_EXPORT(i),
                              entry->module,
                              entry->name,
                              entry->arity,
                              (bif_func_type)entry->f);
    }
}

static auto create_allocator(const JitAllocator::CreateParams &params) {
    JitAllocator::Span test_span;
    bool single_mapped;
    Error err;

    auto *allocator = new JitAllocator(&params);

    err = allocator->alloc(test_span, 1);

    if (err == ErrorCode::kErrorOk) {
        /* We can get dual-mapped memory when asking for single-mapped memory
         * if the latter is not possible: return whether that happened. */
        single_mapped = (test_span.rx() == test_span.rw());
    }

    allocator->release(test_span.rx());

    if (err == ErrorCode::kErrorOk) {
        return std::make_pair(allocator, single_mapped);
    }

    delete allocator;
    return std::make_pair((JitAllocator *)nullptr, false);
}

static JitAllocator *pick_allocator() {
    JitAllocator::CreateParams params;
    JitAllocator *allocator;
    bool single_mapped;

#if defined(VALGRIND)
    erts_jit_single_map = 1;
#elif defined(__APPLE__) && defined(__aarch64__)
    /* Allocating dual-mapped executable memory on this platform is horribly
     * slow, and provides little security benefits over the MAP_JIT per-thread
     * permission scheme. Force single-mapped memory.
     *
     * 64-bit x86 still uses dual-mapped memory as it lacks support for per-
     * thread permissions and thus gets unprotected RWX pages with MAP_JIT. */
    erts_jit_single_map = 1;
#endif

#if defined(HAVE_LINUX_PERF_SUPPORT)
    /* `perf` has a hard time showing symbols for dual-mapped memory, so we'll
     * use single-mapped memory when enabled. */
    if (erts_jit_perf_support & BEAMASM_PERF_ENABLED) {
        erts_jit_single_map = 1;
    }
#endif

    /* Default to dual-mapped memory with separate executable and writable
     * regions of the same code. This is required for platforms that enforce
     * W^X, and we prefer it when available to catch errors sooner.
     *
     * `blockSize` is analogous to "carrier size," and we pick something
     * much larger than the default since dual-mapping implies having one
     * file descriptor per block on most platforms. The block sizes do grow
     * over time, but we don't want to waste half a dozen fds just to get to
     * the shell on platforms that are very fd-constrained. */
    params.reset();
    params.blockSize = 32 << 20;

    allocator = nullptr;
    single_mapped = false;

    if (!erts_jit_single_map) {
        params.options = JitAllocatorOptions::kUseDualMapping;
        std::tie(allocator, single_mapped) = create_allocator(params);
    }

    if (allocator == nullptr) {
        params.options &= ~JitAllocatorOptions::kUseDualMapping;
        std::tie(allocator, single_mapped) = create_allocator(params);
    }

    if (erts_jit_single_map && !single_mapped) {
        ERTS_INTERNAL_ERROR("jit: Failed to allocate executable+writable "
                            "memory. Either allow this or disable both the "
                            "'+JPperf' and '+JMsingle' options.");
    }

    if (allocator == nullptr) {
        ERTS_INTERNAL_ERROR("jit: Cannot allocate executable memory. Use the "
                            "interpreter instead.");
    }

    return allocator;
}

void beamasm_init() {
    unsigned label = 1;

    ASSERT(bga == nullptr && bma == nullptr);

    struct operands {
        Eterm name;
        int arity;
        BeamInstr operand;
        ErtsCodePtr *target;
    };

    std::vector<struct operands> operands = {
            {am_run_process, 3, op_i_apply_only, &beam_run_process},
            {am_normal_exit, 0, op_normal_exit, &beam_normal_exit},
            {am_continue_exit, 0, op_continue_exit, &beam_continue_exit},
            {am_exception_trace, 0, op_return_trace, &beam_exception_trace},
            {am_return_trace, 0, op_return_trace, &beam_return_trace},
            {am_return_to_trace,
             0,
             op_i_return_to_trace,
             &beam_return_to_trace},
            {am_call_trace_return,
             0,
             op_i_call_trace_return,
             &beam_call_trace_return}};

    Eterm mod_name;
    ERTS_DECL_AM(erts_beamasm);
    mod_name = AM_erts_beamasm;

    /* erts_frame_layout is hardcoded to ERTS_FRAME_LAYOUT_RA when Erlang
     * frame pointers are disabled or unsupported. */
#if defined(ERLANG_FRAME_POINTERS)
#    ifdef HAVE_LINUX_PERF_SUPPORT
    if (erts_jit_perf_support & BEAMASM_PERF_FP) {
        erts_frame_layout = ERTS_FRAME_LAYOUT_FP_RA;
    } else {
        erts_frame_layout = ERTS_FRAME_LAYOUT_RA;
    }
#    else
    erts_frame_layout = ERTS_FRAME_LAYOUT_RA;
#    endif
#else
    ERTS_CT_ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
#endif

    beamasm_metadata_early_init();
    init_cache_info();

    /*
     * Ensure that commonly used fields in the PCB can be accessed with
     * short instructions. Before removing any of these assertions, please
     * consider the effect it will have on code size and/or performance.
     */
    ERTS_CT_ASSERT(offsetof(Process, htop) < 128);
    ERTS_CT_ASSERT(offsetof(Process, stop) < 128);
    ERTS_CT_ASSERT(offsetof(Process, fcalls) < 128);
    ERTS_CT_ASSERT(offsetof(Process, freason) < 128);
    ERTS_CT_ASSERT(offsetof(Process, fvalue) < 128);
    ERTS_CT_ASSERT(offsetof(Process, flags) < 128);

#ifdef ERLANG_FRAME_POINTERS
    ERTS_CT_ASSERT(offsetof(Process, frame_pointer) < 128);
#endif

    cpuinfo = CpuInfo::host();

    jit_allocator = pick_allocator();

    bga = new BeamGlobalAssembler(jit_allocator);

    bma = new BeamModuleAssembler(bga,
                                  mod_name,
                                  1 + operands.size() * 2,
                                  operands.size());

    std::vector<ArgVal> args;

    for (auto &op : operands) {
        unsigned func_label, entry_label;

        func_label = label++;
        entry_label = label++;

        args = {ArgVal(ArgVal::Label, func_label),
                ArgVal(ArgVal::Word, sizeof(UWord))};
        bma->emit(op_aligned_label_Lt, args);

        args = {ArgVal(ArgVal::Word, func_label),
                ArgVal(ArgVal::Immediate, mod_name),
                ArgVal(ArgVal::Immediate, op.name),
                ArgVal(ArgVal::Word, op.arity)};
        bma->emit(op_i_func_info_IaaI, args);

        args = {ArgVal(ArgVal::Label, entry_label),
                ArgVal(ArgVal::Word, sizeof(UWord))};
        bma->emit(op_aligned_label_Lt, args);

        args = {};
        bma->emit(op.operand, args);

        op.operand = entry_label;
    }

    args = {};
    bma->emit(op_int_code_end, args);

    {
        /* We have no need of the module pointers as we use `getCode(...)`
         * for everything, and the code will live as long as the emulator
         * itself. */
        const void *_ignored_exec;
        void *_ignored_rw;

        /* Register our global code with gdb/perf so it shows up nicely in
         * stack traces. */
        BeamCodeHeader *_ignored_code_hdr_rw = NULL;
        const BeamCodeHeader *code_hdr_ro = NULL;
        BeamCodeHeader load_header = {};

        load_header.num_functions = operands.size();

        bma->codegen(jit_allocator,
                     &_ignored_exec,
                     &_ignored_rw,
                     &load_header,
                     &code_hdr_ro,
                     &_ignored_code_hdr_rw);
        bma->register_metadata(code_hdr_ro);
    }

    for (auto op : operands) {
        if (op.target) {
            *op.target = bma->getCode(op.operand);
        }
    }

    /* These instructions rely on register contents, and can only be reached
     * from a `call_ext_*`-instruction or trapping from the emulator, hence the
     * lack of wrapper functions. */
    beam_save_calls_export = (ErtsCodePtr)bga->get_dispatch_save_calls_export();
    beam_save_calls_fun = (ErtsCodePtr)bga->get_dispatch_save_calls_fun();
    beam_export_trampoline = (ErtsCodePtr)bga->get_export_trampoline();

    /* Used when trappping to Erlang code from the emulator, setting up
     * registers in the same way as call_ext so that save_calls and tracing
     * works when trapping. */
    beam_bif_export_trap = (ErtsCodePtr)bga->get_bif_export_trap();

    beam_exit = (ErtsCodePtr)bga->get_process_exit();

    beam_unloaded_fun = (ErtsCodePtr)bga->get_unloaded_fun();

    beamasm_metadata_late_init();
}

bool BeamAssemblerCommon::hasCpuFeature(uint32_t featureId) {
    return cpuinfo.hasFeature(featureId);
}

void init_emulator(void) {
    install_bifs();
}

void process_main(ErtsSchedulerData *esdp) {
    typedef void (*pmain_type)(ErtsSchedulerData *);

    pmain_type pmain = (pmain_type)bga->get_process_main();
    pmain(esdp);
}

extern "C"
{
    int erts_beam_jump_table(void) {
#if defined(NO_JUMP_TABLE)
        return 0;
#else
        return 1;
#endif
    }

    void beamasm_unseal_module(const void *executable_region,
                               void *writable_region,
                               size_t size) {
        (void)executable_region;
        (void)writable_region;
        (void)size;

        VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadWrite);
    }

    void beamasm_seal_module(const void *executable_region,
                             void *writable_region,
                             size_t size) {
        (void)executable_region;
        (void)writable_region;
        (void)size;

        VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadExecute);
    }

    void beamasm_flush_icache(const void *address, size_t size) {
#ifdef DEBUG
        erts_debug_require_code_barrier();
#endif

#if defined(__aarch64__) && defined(WIN32)
        /* Issues full memory/instruction barriers on all threads for us. */
        FlushInstructionCache(GetCurrentProcess(), address, size);
#elif defined(__aarch64__) && defined(__APPLE__)
        /* Issues full memory/instruction barriers on all threads for us. */
        sys_icache_invalidate((char *)address, size);
#elif defined(__aarch64__) && defined(BEAMASM_MANUAL_ICACHE_FLUSHING)
        /* Note that we do not issue an instruction synchronization barrier
         * here. This is on purpose as we must issue those on all schedulers
         * and not just the calling thread, and the chances of us forgetting to
         * do that is much higher if we issue one here. */
        UWord start, end, stride;

        start = reinterpret_cast<UWord>(address);
        end = start + size;

        stride = min_dcache_line_size;
        for (UWord i = start & ~(stride - 1); i < end; i += stride) {
            __asm__ __volatile__("dc cvau, %0\n" ::"r"(i) :);
        }

        /* We need a special memory barrier between clearing dcache and icache,
         * or there's a chance that the icache on another core is invalidated
         * before the dcache, which can then be repopulated with stale data. */
        __asm__ __volatile__("dsb ish\n" ::: "memory");

        stride = min_icache_line_size;
        for (UWord i = start & ~(stride - 1); i < end; i += stride) {
            __asm__ __volatile__("ic ivau, %0\n" ::"r"(i) :);
        }

        /* Ensures that all cores clear their instruction cache before moving
         * on. The usual full memory barrier (`dmb sy`) executed by the thread
         * progress mechanism is not sufficient for this.
         *
         * Note that this barrier need not be executed on other cores, it's
         * enough for them to issue an instruction synchronization barrier. */
        __asm__ __volatile__("dsb ish\n" ::: "memory");
#elif (defined(__x86_64__) || defined(_M_X64)) &&                              \
        defined(ERTS_THR_INSTRUCTION_BARRIER)
        /* We don't need to invalidate cache on this platform, but since we
         * might be modifying code with a different linear address than the one
         * we execute from (dual-mapped memory), we still need to issue an
         * instruction barrier on all schedulers to ensure that the change is
         * visible. */
        (void)address;
        (void)size;
#else
#    error "Platform lacks implementation for clearing instruction cache." \
                "Please report this bug."
#endif
    }

    void *beamasm_new_assembler(Eterm mod,
                                int num_labels,
                                int num_functions,
                                BeamFile *file) {
        return new BeamModuleAssembler(bga,
                                       mod,
                                       num_labels,
                                       num_functions,
                                       file);
    }

    int beamasm_emit(void *instance, unsigned specific_op, BeamOp *op) {
        /* The argument array must be safely convertible from `BeamOpArg*` to
         * `ArgVal*` for us to reuse it directly.
         *
         * The exact traits we need weren't introduced until C++20, but the
         * assertions below will catch just about everything that would break
         * this conversion. */
        static_assert(std::is_base_of<BeamOpArg, ArgVal>::value);
        static_assert(std::is_standard_layout<ArgVal>::value);

        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        const Span<ArgVal> args(static_cast<ArgVal *>(op->a), op->arity);
        return ba->emit(specific_op, args);
    }

    void beamasm_emit_coverage(void *instance,
                               void *coverage,
                               Uint index,
                               Uint size) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->emit_coverage(coverage, index, size);
    }

    void beamasm_emit_call_nif(const ErtsCodeInfo *info,
                               void *normal_fptr,
                               void *lib,
                               void *dirty_fptr,
                               char *buff,
                               unsigned buff_len) {
        BeamModuleAssembler ba(bga, info->mfa.module, 3);
        std::vector<ArgVal> args;

        args = {ArgVal(ArgVal::Label, 1), ArgVal(ArgVal::Word, sizeof(UWord))};
        ba.emit(op_aligned_label_Lt, args);

        args = {ArgVal(ArgVal::Word, 1),
                ArgVal(ArgVal::Immediate, info->mfa.module),
                ArgVal(ArgVal::Immediate, info->mfa.function),
                ArgVal(ArgVal::Word, info->mfa.arity)};
        ba.emit(op_i_func_info_IaaI, args);

        args = {ArgVal(ArgVal::Label, 2), ArgVal(ArgVal::Word, sizeof(UWord))};
        ba.emit(op_aligned_label_Lt, args);

        args = {};
        ba.emit(op_i_breakpoint_trampoline, args);

        args = {ArgVal(ArgVal::Word, (BeamInstr)normal_fptr),
                ArgVal(ArgVal::Word, (BeamInstr)lib),
                ArgVal(ArgVal::Word, (BeamInstr)dirty_fptr)};
        ba.emit(op_call_nif_WWW, args);

        ba.codegen(buff, buff_len);
    }

    void beamasm_delete_assembler(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        delete ba;
    }

    void beamasm_purge_module(const void *executable_region,
                              void *writable_region,
                              size_t size) {
        (void)size;
        jit_allocator->release(const_cast<void *>(executable_region));
    }

    ErtsCodePtr beamasm_get_code(void *instance, int label) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return reinterpret_cast<ErtsCodePtr>(ba->getCode(label));
    }

    ErtsCodePtr beamasm_get_lambda(void *instance, int index) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return reinterpret_cast<ErtsCodePtr>(ba->getLambda(index));
    }

    const byte *beamasm_get_rodata(void *instance, char *label) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return reinterpret_cast<const byte *>(ba->getCode(label));
    }

    void beamasm_embed_rodata(void *instance,
                              const char *labelName,
                              const char *buff,
                              size_t size) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        if (size) {
            ba->embed_rodata(labelName, buff, size);
        }
    }

    void beamasm_embed_bss(void *instance, char *labelName, size_t size) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        if (size) {
            ba->embed_bss(labelName, size);
        }
    }

    void beamasm_codegen(void *instance,
                         const void **executable_region,
                         void **writable_region,
                         const BeamCodeHeader *in_hdr,
                         const BeamCodeHeader **out_exec_hdr,
                         BeamCodeHeader **out_rw_hdr) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);

        ba->codegen(jit_allocator,
                    executable_region,
                    writable_region,
                    in_hdr,
                    out_exec_hdr,
                    out_rw_hdr);
    }

    void *beamasm_register_metadata(void *instance,
                                    const BeamCodeHeader *header) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return ba->register_metadata(header);
    }

    Uint beamasm_get_header(void *instance, const BeamCodeHeader **hdr) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);

        *hdr = ba->getCodeHeader();

        return ba->getCodeSize();
    }

    char *beamasm_get_base(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return (char *)ba->getBaseAddress();
    }

    size_t beamasm_get_offset(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return ba->getOffset();
    }

    const ErtsCodeInfo *beamasm_get_on_load(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return ba->getOnLoad();
    }

    unsigned int beamasm_patch_catches(void *instance, char *rw_base) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return ba->patchCatches(rw_base);
    }

    void beamasm_patch_import(void *instance,
                              char *rw_base,
                              int index,
                              const Export *import) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchImport(rw_base, index, import);
    }

    void beamasm_patch_literal(void *instance,
                               char *rw_base,
                               int index,
                               Eterm lit) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchLiteral(rw_base, index, lit);
    }

    void beamasm_patch_lambda(void *instance,
                              char *rw_base,
                              int index,
                              const ErlFunEntry *fe) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchLambda(rw_base, index, fe);
    }

    void beamasm_patch_strings(void *instance,
                               char *rw_base,
                               const byte *string_table) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchStrings(rw_base, string_table);
    }
}
