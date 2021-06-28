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

#include "beam_asm.hpp"

extern "C"
{
#include "bif.h"
#include "beam_common.h"
#include "code_ix.h"
#include "export.h"

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
int erts_jit_perf_support;
#endif

/*
 * Special Beam instructions.
 */

ErtsCodePtr beam_run_process;
ErtsCodePtr beam_normal_exit;
ErtsCodePtr beam_exit;
ErtsCodePtr beam_export_trampoline;
ErtsCodePtr beam_bif_export_trap;
ErtsCodePtr beam_continue_exit;
ErtsCodePtr beam_save_calls;
ErtsCodePtr beam_unloaded_fun;

/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are for the instruction value, and sometimes
**      for the variable reference (one of these), and rogue references
**      will most likely cause chaos.
*/
ErtsCodePtr beam_return_to_trace;   /* OpCode(i_return_to_trace) */
ErtsCodePtr beam_return_trace;      /* OpCode(i_return_trace) */
ErtsCodePtr beam_exception_trace;   /* UGLY also OpCode(i_return_trace) */
ErtsCodePtr beam_return_time_trace; /* OpCode(i_return_time_trace) */

static JitAllocator *jit_allocator;

static BeamGlobalAssembler *bga;
static BeamModuleAssembler *bma;
static CpuInfo cpuinfo;

static void beamasm_init_gdb_jit_info(void);

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
    ASSERT(beam_save_calls != NULL);

    for (i = 0; i < BIF_SIZE; i++) {
        BifEntry *entry;
        Export *ep;
        int j;

        entry = &bif_table[i];

        ep = erts_export_put(entry->module, entry->name, entry->arity);

        ep->info.op = op_i_func_info_IaaI;
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

static JitAllocator *create_allocator(JitAllocator::CreateParams *params) {
    void *test_ro, *test_rw;
    Error err;

    auto *allocator = new JitAllocator(params);

    err = allocator->alloc(&test_ro, &test_rw, 1);
    allocator->release(test_ro);

    if (err == ErrorCode::kErrorOk) {
        return allocator;
    }

    delete allocator;
    return nullptr;
}

static JitAllocator *pick_allocator() {
    JitAllocator::CreateParams single_params;
    single_params.reset();

#if defined(HAVE_LINUX_PERF_SUPPORT)
    /* `perf` has a hard time showing symbols for dual-mapped memory, so we'll
     * use single-mapped memory when enabled. */
    if (erts_jit_perf_support & (BEAMASM_PERF_DUMP | BEAMASM_PERF_MAP)) {
        if (auto *alloc = create_allocator(&single_params)) {
            return alloc;
        }

        ERTS_INTERNAL_ERROR("jit: Failed to allocate executable+writable "
                            "memory. Either allow this or disable the "
                            "'+JPperf' option.");
    }
#endif

#if !defined(VALGRIND)
    /* Default to dual-mapped memory with separate executable and writable
     * regions of the same code. This is required for platforms that enforce
     * W^X, and we prefer it when available to catch errors sooner.
     *
     * `blockSize` is analogous to "carrier size," and we pick something
     * much larger than the default since dual-mapping implies having one
     * file descriptor per block on most platforms. The block sizes do grow
     * over time, but we don't want to waste half a dozen fds just to get to
     * the shell on platforms that are very fd-constrained. */
    JitAllocator::CreateParams dual_params;

    dual_params.reset();
    dual_params.options = JitAllocator::kOptionUseDualMapping,
    dual_params.blockSize = 4 << 20;

    if (auto *alloc = create_allocator(&dual_params)) {
        return alloc;
    } else if (auto *alloc = create_allocator(&single_params)) {
        return alloc;
    }

    ERTS_INTERNAL_ERROR("jit: Cannot allocate executable memory. Use the "
                        "interpreter instead.");
#elif defined(VALGRIND)
    if (auto *alloc = create_allocator(&single_params)) {
        return alloc;
    }

    ERTS_INTERNAL_ERROR("jit: the valgrind emulator requires the ability to "
                        "allocate executable+writable memory.");
#endif
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
            {am_return_time_trace,
             0,
             op_i_return_time_trace,
             &beam_return_time_trace}};

    Eterm mod_name;
    ERTS_DECL_AM(erts_beamasm);
    mod_name = AM_erts_beamasm;

    /* erts_frame_layout is hardcoded to ERTS_FRAME_LAYOUT_RA when Erlang
     * frame pointers are disabled or unsupported. */
#if defined(ERLANG_FRAME_POINTERS)
    if (erts_jit_perf_support & BEAMASM_PERF_MAP) {
        erts_frame_layout = ERTS_FRAME_LAYOUT_FP_RA;
    } else {
        erts_frame_layout = ERTS_FRAME_LAYOUT_RA;
    }
#else
    ERTS_CT_ASSERT(erts_frame_layout == ERTS_FRAME_LAYOUT_RA);
#endif

    beamasm_init_perf();
    beamasm_init_gdb_jit_info();

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

        args = {ArgVal(ArgVal::Immediate, func_label),
                ArgVal(ArgVal::Word, sizeof(UWord))};
        bma->emit(op_aligned_label_Lt, args);

        args = {ArgVal(ArgVal::Immediate, func_label),
                ArgVal(ArgVal::Immediate, mod_name),
                ArgVal(ArgVal::Immediate, op.name),
                ArgVal(ArgVal::Immediate, op.arity)};
        bma->emit(op_i_func_info_IaaI, args);

        args = {ArgVal(ArgVal::Immediate, entry_label),
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
        bma->codegen(jit_allocator, &_ignored_exec, &_ignored_rw);
    }

    for (auto op : operands) {
        if (op.target) {
            *op.target = bma->getCode(op.operand);
        }
    }

    /* These instructions rely on register contents, and can only be reached
     * from a `call_ext_*`-instruction or trapping from the emulator, hence the
     * lack of wrapper functions. */
    beam_save_calls = (ErtsCodePtr)bga->get_dispatch_save_calls();
    beam_export_trampoline = (ErtsCodePtr)bga->get_export_trampoline();

    /* Used when trappping to Erlang code from the emulator, setting up
     * registers in the same way as call_ext so that save_calls and tracing
     * works when trapping. */
    beam_bif_export_trap = (ErtsCodePtr)bga->get_bif_export_trap();

    beam_exit = (ErtsCodePtr)bga->get_process_exit();

    beam_unloaded_fun = (ErtsCodePtr)bga->get_unloaded_fun();
}

bool BeamAssembler::hasCpuFeature(uint32_t featureId) {
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

enum jit_actions : uint32_t {
    JIT_NOACTION = 0,
    JIT_REGISTER_FN,
    JIT_UNREGISTER_FN,
};

struct jit_code_entry {
    struct jit_code_entry *next_entry;
    struct jit_code_entry *prev_entry;
    const char *symfile_addr;
    uint64_t symfile_size;
};

struct jit_descriptor {
    uint32_t version;
    jit_actions action_flag;
    struct jit_code_entry *relevant_entry;
    struct jit_code_entry *first_entry;
    erts_mtx_t mutex;
};

extern "C"
{
    extern void ERTS_NOINLINE __jit_debug_register_code(void);

    /* Make sure to specify the version statically, because the
     * debugger may check the version before we can set it. */
    struct jit_descriptor __jit_debug_descriptor = {1,
                                                    JIT_NOACTION,
                                                    NULL,
                                                    NULL};
} /* extern "C" */

static void beamasm_init_gdb_jit_info(void) {
    Sint symfile_size = sizeof(uint64_t) * 2;
    uint64_t *symfile = (uint64_t *)malloc(symfile_size);
    jit_code_entry *entry;

    symfile[0] = 0;
    symfile[1] = (uint64_t)beam_normal_exit;

    entry = (jit_code_entry *)malloc(sizeof(jit_code_entry));

    /* Add address description */
    entry->symfile_addr = (char *)symfile;
    entry->symfile_size = symfile_size;

    /* Insert into linked list */
    entry->next_entry = __jit_debug_descriptor.first_entry;
    if (entry->next_entry) {
        entry->next_entry->prev_entry = entry;
    } else {
        entry->prev_entry = nullptr;
    }

    /* register with dbg */
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_descriptor.first_entry = entry;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_register_code();

    erts_mtx_init(&__jit_debug_descriptor.mutex,
                  "jit_debug_descriptor",
                  NIL,
                  (ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                   ERTS_LOCK_FLAGS_CATEGORY_GENERIC));
}

void BeamAssembler::update_gdb_jit_info(std::string modulename,
                                        std::vector<AsmRange> &functions) {
    Sint symfile_size = sizeof(uint64_t) * 3 + modulename.size() + 1;

    for (auto fun : functions) {
        symfile_size += sizeof(uint64_t) * 2;
        symfile_size += fun.name.size() + 1;
    }

    char *symfile = (char *)malloc(symfile_size);
    jit_code_entry *entry;

    entry = (jit_code_entry *)malloc(sizeof(jit_code_entry));

    /* Add address description */
    entry->symfile_addr = symfile;
    entry->symfile_size = symfile_size;

    ((uint64_t *)symfile)[0] = functions.size();
    ((uint64_t *)symfile)[1] = code.baseAddress();
    ((uint64_t *)symfile)[2] = (uint64_t)code.codeSize();

    symfile += sizeof(uint64_t) * 3;

    sys_memcpy(symfile, modulename.c_str(), modulename.size() + 1);
    symfile += modulename.size() + 1;

    for (unsigned i = 0; i < functions.size(); i++) {
        ((uint64_t *)symfile)[0] = (uint64_t)functions[i].start;
        ((uint64_t *)symfile)[1] = (uint64_t)functions[i].stop;

        ASSERT(functions[i].start <= functions[i].stop);

        symfile += sizeof(uint64_t) * 2;

        sys_memcpy(symfile,
                   functions[i].name.c_str(),
                   functions[i].name.size() + 1);
        symfile += functions[i].name.size() + 1;
    }

    ASSERT(symfile_size == (symfile - entry->symfile_addr));

    /* Insert into linked list */
    erts_mtx_lock(&__jit_debug_descriptor.mutex);
    entry->next_entry = __jit_debug_descriptor.first_entry;
    if (entry->next_entry) {
        entry->next_entry->prev_entry = entry;
    } else {
        entry->prev_entry = nullptr;
    }

    /* register with dbg */
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_descriptor.first_entry = entry;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_register_code();
    erts_mtx_unlock(&__jit_debug_descriptor.mutex);
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

    void beamasm_flush_icache(const void *address, size_t size) {
#if defined(__aarch64__)
#    if defined(WIN32)
        FlushInstructionCache(GetCurrentProcess(), address, size);
#    elif defined(__APPLE__)
        sys_icache_invalidate((char *)address, size);
#    elif defined(__GNUC__)
        __builtin___clear_cache(&((char *)address)[0],
                                &((char *)address)[size]);
#    else
#        error "Platform lacks implementation for clearing instruction cache." \
                "Please report this bug."
#    endif
#else
        (void)address;
        (void)size;
#endif
    }

    void *beamasm_new_assembler(Eterm mod,
                                int num_labels,
                                int num_functions,
                                BeamFile_ExportTable *named_labels) {
        return new BeamModuleAssembler(bga,
                                       mod,
                                       num_labels,
                                       num_functions,
                                       named_labels);
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

    void beamasm_emit_call_nif(const ErtsCodeInfo *info,
                               void *normal_fptr,
                               void *lib,
                               void *dirty_fptr,
                               char *buff,
                               unsigned buff_len) {
        BeamModuleAssembler ba(bga, info->mfa.module, 3);
        std::vector<ArgVal> args;

        args = {ArgVal(ArgVal::Immediate, 1),
                ArgVal(ArgVal::Word, sizeof(UWord))};
        ba.emit(op_aligned_label_Lt, args);

        args = {ArgVal(ArgVal::Immediate, 1),
                ArgVal(ArgVal::Immediate, info->mfa.module),
                ArgVal(ArgVal::Immediate, info->mfa.function),
                ArgVal(ArgVal::Immediate, info->mfa.arity)};
        ba.emit(op_i_func_info_IaaI, args);

        args = {ArgVal(ArgVal::Immediate, 2),
                ArgVal(ArgVal::Word, sizeof(UWord))};
        ba.emit(op_aligned_label_Lt, args);

        args = {};
        ba.emit(op_i_breakpoint_trampoline, args);

        args = {ArgVal(ArgVal::Immediate, (BeamInstr)normal_fptr),
                ArgVal(ArgVal::Immediate, (BeamInstr)lib),
                ArgVal(ArgVal::Immediate, (BeamInstr)dirty_fptr)};
        ba.emit(op_call_nif_WWW, args);

        ba.codegen(buff, buff_len);
    }

    void beamasm_delete_assembler(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        delete ba;
    }

    void beamasm_purge_module(const void *native_module_exec,
                              void *native_module_rw) {
        jit_allocator->release(const_cast<void *>(native_module_exec));
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
                         const void **native_module_exec,
                         void **native_module_rw,
                         const BeamCodeHeader *in_hdr,
                         const BeamCodeHeader **out_exec_hdr,
                         BeamCodeHeader **out_rw_hdr) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);

        ba->codegen(jit_allocator,
                    native_module_exec,
                    native_module_rw,
                    in_hdr,
                    out_exec_hdr,
                    out_rw_hdr);
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
                              BeamInstr import) {
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
                              BeamInstr fe) {
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
