/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021-2024. All Rights Reserved.
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

#include "beam_jit_common.hpp"
#include "beam_asm.hpp"

extern "C"
{
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "code_ix.h"
#include "erl_proc_sig_queue.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_map.h"
#include "beam_common.h"
#ifdef USE_VM_PROBES
#    include "dtrace-wrapper.h"
#endif
}

static std::string getAtom(Eterm atom) {
    Atom *ap = atom_tab(atom_val(atom));
    return std::string((char *)ap->name, ap->len);
}

BeamAssemblerCommon::BeamAssemblerCommon(BaseAssembler &assembler_)
        : assembler(assembler_), code() {
    /* Setup with default code info */
    Error err = code.init(Environment::host());
    ERTS_ASSERT(!err && "Failed to init codeHolder");

    err = code.newSection(&rodata,
                          ".rodata",
                          SIZE_MAX,
                          SectionFlags::kReadOnly,
                          8);
    ERTS_ASSERT(!err && "Failed to create .rodata section");

#ifdef DEBUG
    assembler.addDiagnosticOptions(DiagnosticOptions::kValidateAssembler);
#endif
    assembler.addEncodingOptions(EncodingOptions::kOptimizeForSize |
                                 EncodingOptions::kOptimizedAlign);
    code.setErrorHandler(this);
}

BeamAssemblerCommon::~BeamAssemblerCommon() {
    if (logger.file()) {
        fclose(logger.file());
    }
}

void *BeamAssemblerCommon::getBaseAddress() {
    ASSERT(code.hasBaseAddress());
    return (void *)code.baseAddress();
}

size_t BeamAssemblerCommon::getOffset() {
    return assembler.offset();
}

void BeamAssemblerCommon::codegen(JitAllocator *allocator,
                                  const void **executable_ptr,
                                  void **writable_ptr) {
    Error err;

    err = code.flatten();
    ERTS_ASSERT(!err && "Could not flatten code");
    err = code.resolveUnresolvedLinks();
    ERTS_ASSERT(!err && "Could not resolve all links");

    /* Verify that all labels are bound */
#ifdef DEBUG
    for (auto e : code.labelEntries()) {
        if (!e->isBound()) {
            if (e->hasName()) {
                erts_exit(ERTS_ABORT_EXIT,
                          "Label %d with name %s is not bound\n",
                          e->id(),
                          e->name());
            } else {
                erts_exit(ERTS_ABORT_EXIT, "Label %d is not bound\n", e->id());
            }
        }
    }
#endif

    JitAllocator::Span span;

    err = allocator->alloc(span, code.codeSize() + 16);

    if (err == ErrorCode::kErrorTooManyHandles) {
        ERTS_ASSERT(!"Failed to allocate module code: "
                     "out of file descriptors");
    } else if (err) {
        ERTS_ASSERT("Failed to allocate module code");
    }

    *executable_ptr = span.rx();
    *writable_ptr = span.rw();

    VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadWrite);

    code.relocateToBase((uint64_t)*executable_ptr);
    code.copyFlattenedData(*writable_ptr,
                           code.codeSize(),
                           CopySectionFlags::kPadSectionBuffer);

#ifdef DEBUG
    if (FileLogger *l = dynamic_cast<FileLogger *>(code.logger()))
        if (FILE *f = l->file())
            fprintf(f, "; CODE_SIZE: %zd\n", code.codeSize());
#endif
}

void *BeamAssemblerCommon::getCode(Label label) {
    ASSERT(label.isValid());
    return (char *)getBaseAddress() + code.labelOffsetFromBase(label);
}

byte *BeamAssemblerCommon::getCode(char *labelName) {
    return (byte *)getCode(code.labelByName(labelName, strlen(labelName)));
}

void BeamAssemblerCommon::handleError(Error err,
                                      const char *message,
                                      BaseEmitter *origin) {
    comment(message);

    if (logger.file() != NULL) {
        fflush(logger.file());
    }

    ASSERT(0 && "Failed to encode instruction");
}

void BeamAssemblerCommon::embed_rodata(const char *labelName,
                                       const char *buff,
                                       size_t size) {
    Label label = assembler.newNamedLabel(labelName);

    assembler.section(rodata);
    assembler.bind(label);
    assembler.embed(buff, size);
    assembler.section(code.textSection());
}

void BeamAssemblerCommon::embed_bss(const char *labelName, size_t size) {
    Label label = assembler.newNamedLabel(labelName);

    /* Reuse rodata section for now */
    assembler.section(rodata);
    assembler.bind(label);
    embed_zeros(size);
    assembler.section(code.textSection());
}

void BeamAssemblerCommon::embed_zeros(size_t size) {
    static constexpr size_t buf_size = 16384;
    static const char zeros[buf_size] = {};

    while (size >= buf_size) {
        assembler.embed(zeros, buf_size);
        size -= buf_size;
    }

    if (size > 0) {
        assembler.embed(zeros, size);
    }
}

void BeamAssemblerCommon::setLogger(const std::string &log) {
    FILE *f = fopen(log.data(), "w+");

    /* FIXME: Don't crash when loading multiple modules with the same name.
     *
     * setLogger(nullptr) disables logging. */
    if (f) {
        setvbuf(f, NULL, _IONBF, 0);
    }

    setLogger(f);
}

void BeamAssemblerCommon::setLogger(FILE *log) {
    logger.setFile(log);
    logger.setIndentation(FormatIndentationGroup::kCode, 4);
    code.setLogger(&logger);
}

void BeamModuleAssembler::codegen(JitAllocator *allocator,
                                  const void **executable_ptr,
                                  void **writable_ptr,
                                  const BeamCodeHeader *in_hdr,
                                  const BeamCodeHeader **out_exec_hdr,
                                  BeamCodeHeader **out_rw_hdr) {
    const BeamCodeHeader *code_hdr_exec;
    BeamCodeHeader *code_hdr_rw;

    BeamAssembler::codegen(allocator, executable_ptr, writable_ptr);

    {
        auto offset = code.labelOffsetFromBase(code_header);

        auto base_exec = (const char *)(*executable_ptr);
        code_hdr_exec = (const BeamCodeHeader *)&base_exec[offset];

        auto base_rw = (const char *)(*writable_ptr);
        code_hdr_rw = (BeamCodeHeader *)&base_rw[offset];
    }

    sys_memcpy(code_hdr_rw, in_hdr, sizeof(BeamCodeHeader));
    code_hdr_rw->on_load = getOnLoad();

    for (unsigned i = 0; i < functions.size(); i++) {
        ErtsCodeInfo *ci = (ErtsCodeInfo *)getCode(functions[i]);
        code_hdr_rw->functions[i] = ci;
    }

    char *module_end = (char *)code.baseAddress() + a.offset();
    code_hdr_rw->functions[functions.size()] = (ErtsCodeInfo *)module_end;

    /* Note that we don't make the module executable yet since we're going to
     * patch literals et cetera and it's pointless to ping-pong the page
     * permissions. The user will call `beamasm_seal_module` to do so later
     * on. */

    *out_exec_hdr = code_hdr_exec;
    *out_rw_hdr = code_hdr_rw;
}

void BeamModuleAssembler::codegen(JitAllocator *allocator,
                                  const void **executable_ptr,
                                  void **writable_ptr) {
    BeamAssembler::codegen(allocator, executable_ptr, writable_ptr);
    VirtMem::protectJitMemory(VirtMem::ProtectJitAccess::kReadExecute);
}

void BeamModuleAssembler::codegen(char *buff, size_t len) {
    code.flatten();
    code.resolveUnresolvedLinks();
    ERTS_ASSERT(code.codeSize() <= len);
    code.relocateToBase((uint64_t)buff);
    code.copyFlattenedData(buff,
                           code.codeSize(),
                           CopySectionFlags::kPadSectionBuffer);
}

BeamModuleAssembler::BeamModuleAssembler(BeamGlobalAssembler *_ga,
                                         Eterm _mod,
                                         int num_labels,
                                         const BeamFile *file)
        : BeamAssembler(getAtom(_mod)), BeamModuleAssemblerCommon(file, _mod),
          ga(_ga) {
    rawLabels.reserve(num_labels + 1);

    if (logger.file() && beam) {
        /* Dig out all named labels from the BEAM-file and sort them on the
         * label id. */
        const int named_count = beam->exports.count + beam->locals.count;
        BeamFile_ExportEntry *entries;

        entries = (BeamFile_ExportEntry *)erts_alloc(
                ERTS_ALC_T_PREPARED_CODE,
                (named_count + 1) * sizeof(entries[0]));

        for (int i = 0; i < beam->exports.count; i++) {
            entries[i] = beam->exports.entries[i];
        }

        for (int i = 0; i < beam->locals.count; i++) {
            entries[i + beam->exports.count] = beam->locals.entries[i];
        }

        /* Place a sentinel entry with an invalid label number. */
        entries[named_count].label = 0;

        std::qsort(entries,
                   named_count,
                   sizeof(entries[0]),
                   [](const void *lhs__, const void *rhs__) {
                       auto lhs = (const BeamFile_ExportEntry *)lhs__;
                       auto rhs = (const BeamFile_ExportEntry *)rhs__;

                       if (lhs->label < rhs->label) {
                           return -1;
                       } else if (lhs->label == rhs->label) {
                           return 0;
                       } else {
                           return 1;
                       }
                   });

        BeamFile_ExportEntry *e = &entries[0];

        for (int i = 1; i < num_labels; i++) {
            /* Large enough to hold most realistic function names. We will
             * truncate too long names, but as the label name is not important
             * for the functioning of the JIT and this functionality is
             * probably only used by developers, we don't bother with dynamic
             * allocation. */
            char tmp[MAX_ATOM_SZ_LIMIT];

            /* The named_labels are sorted, so no need for a search. */
            if (e->label == i) {
                erts_snprintf(tmp, sizeof(tmp), "%T/%d", e->function, e->arity);
                rawLabels.emplace(i, a.newNamedLabel(tmp));
                e++;
            } else {
                std::string lblName = "label_" + std::to_string(i);
                rawLabels.emplace(i, a.newNamedLabel(lblName.data()));
            }
        }

        erts_free(ERTS_ALC_T_PREPARED_CODE, entries);
    } else if (logger.file()) {
        /* There is no naming info, but dumping of the assembly code
         * has been requested, so do the best we can and number the
         * labels. */
        for (int i = 1; i < num_labels; i++) {
            std::string lblName = "label_" + std::to_string(i);
            rawLabels.emplace(i, a.newNamedLabel(lblName.data()));
        }
    } else {
        /* No output is requested, go with unnamed labels */
        for (int i = 1; i < num_labels; i++) {
            rawLabels.emplace(i, a.newLabel());
        }
    }

    if (beam) {
        /* Create labels for the trampolines that unpack fun environments.
         *
         * Note that `num_free == 0` short-circuits directly to the target. */
        for (int i = 0; i < beam->lambdas.count; i++) {
            const auto &lambda = beam->lambdas.entries[i];
            lambdas[i].trampoline = (lambda.num_free > 0)
                                            ? a.newLabel()
                                            : rawLabels.at(lambda.label);
        }
    }
}

void *BeamModuleAssembler::register_metadata(const BeamCodeHeader *header) {
#ifndef WIN32
    const BeamCodeLineTab *line_table = header->line_table;

    char name_buffer[MAX_ATOM_SZ_LIMIT];
    std::string module_name = getAtom(mod);
    std::vector<AsmRange> ranges;
    ERTS_DECL_AM(erts_beamasm);

    ranges.reserve(functions.size() + 2);

    ASSERT((ErtsCodePtr)getBaseAddress() == (ErtsCodePtr)header);
    ASSERT(functions.size() == header->num_functions);

    /* Push info about the header */
    ranges.push_back({.start = (ErtsCodePtr)getBaseAddress(),
                      .stop = (ErtsCodePtr)&header->functions[functions.size()],
                      .name = module_name + "::codeHeader"});

    for (unsigned i = 0; i < functions.size(); i++) {
        std::vector<AsmRange::LineData> lines;
        ErtsCodePtr start, stop;
        const ErtsCodeInfo *ci;
        Sint n;

        start = getCode(functions[i]);
        ci = (const ErtsCodeInfo *)start;

        stop = ((const char *)erts_codeinfo_to_code(ci));

        if (ci->mfa.module != AM_erts_beamasm) {
            /* All modules (except erts_beamasm, which is a JIT internal module)
               have a prologue that should be counted as part of the CodeInfo */
            stop = ((const char *)stop) + BEAM_ASM_FUNC_PROLOGUE_SIZE;
        }

        n = erts_snprintf(name_buffer,
                          1024,
                          "%T:%T/%d",
                          ci->mfa.module,
                          ci->mfa.function,
                          ci->mfa.arity);

        /* We use a different symbol for CodeInfo and the Prologue
         * in order for the perf disassembly to be better. */
        std::string function_name(name_buffer, n);
        ranges.push_back({.start = start,
                          .stop = stop,
                          .name = function_name + "-CodeInfoPrologue"});

        /* The actual code */
        start = stop;
        if (i + 1 < functions.size()) {
            stop = getCode(functions[i + 1]);
        } else {
            stop = getCode(code_end);
        }

        if (line_table) {
            const void **line_cursor = line_table->func_tab[i];
            const int loc_size = line_table->loc_size;

            /* Register all lines belonging to this function. */
            while ((intptr_t)line_cursor[0] < (intptr_t)stop) {
                ptrdiff_t line_index;
                Uint32 loc;

                line_index = line_cursor - line_table->func_tab[0];

                if (loc_size == 2) {
                    loc = line_table->loc_tab.p2[line_index];
                } else {
                    ASSERT(loc_size == 4);
                    loc = line_table->loc_tab.p4[line_index];
                }

                if (loc != LINE_INVALID_LOCATION) {
                    Uint32 file;
                    Eterm fname;
                    int res;

                    file = LOC_FILE(loc);
                    fname = line_table->fname_ptr[file];

                    ERTS_ASSERT(is_nil(fname) || is_list(fname));

                    res = erts_unicode_list_to_buf(fname,
                                                   (byte *)name_buffer,
                                                   sizeof(name_buffer),
                                                   sizeof(name_buffer) / 4,
                                                   &n);

                    ERTS_ASSERT(res != -1);

                    lines.push_back({.start = line_cursor[0],
                                     .file = std::string(name_buffer, n),
                                     .line = LOC_LINE(loc)});
                }

                line_cursor++;
            }
        }

        ranges.push_back({.start = start,
                          .stop = stop,
                          .name = function_name,
                          .lines = lines});
    }

    /* Push info about the footer */
    ranges.push_back(
            {.start = ranges.back().stop,
             .stop = (ErtsCodePtr)(code.baseAddress() + code.codeSize()),
             .name = module_name + "::codeFooter"});

    return beamasm_metadata_insert(module_name,
                                   (ErtsCodePtr)code.baseAddress(),
                                   code.codeSize(),
                                   ranges);
#else
    return NULL;
#endif
}

BeamCodeHeader *BeamModuleAssembler::getCodeHeader() {
    return (BeamCodeHeader *)getCode(code_header);
}

const ErtsCodeInfo *BeamModuleAssembler::getOnLoad() {
    if (on_load.isValid()) {
        return erts_code_to_codeinfo((ErtsCodePtr)getCode(on_load));
    } else {
        return 0;
    }
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
        auto where = (int32_t *)&rw_base[offset + patch.ptr_offs];

        ASSERT(INT_MAX == *where);
        Eterm catch_term = make_catch(catch_no);

        /* With the current tag scheme, more than 33 million
         * catches can exist at once. */
        ERTS_ASSERT(catch_term >> 31 == 0);

        *where = catch_term;
    }

    return catch_no;
}

void BeamModuleAssembler::patchImport(char *rw_base,
                                      unsigned index,
                                      const Export *import) {
    for (const auto &patch : imports[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = reinterpret_cast<Eterm>(import) + patch.val_offs;
    }
}

void BeamModuleAssembler::patchLambda(char *rw_base,
                                      unsigned index,
                                      const ErlFunEntry *fe) {
    for (const auto &patch : lambdas[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = reinterpret_cast<Eterm>(fe) + patch.val_offs;
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
        auto where = (const byte **)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == (Eterm)*where);
        *where = string_table + patch.val_offs;
    }
}

/* ** */

#if defined(DEBUG) && defined(JIT_HARD_DEBUG)
void beam_jit_validate_term(Eterm term) {
    size_object_x(term, NULL);
}
#endif

Eterm beam_jit_call_bif(Process *c_p,
                        Eterm *reg,
                        ErtsCodePtr I,
                        ErtsBifFunc vbf,
                        Uint arity) {
    ErlHeapFragment *live_hf_end;
    Eterm result;

    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    {
        live_hf_end = c_p->mbuf;

        ERTS_CHK_MBUF_SZ(c_p);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
        result = vbf(c_p, reg, I);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
        ERTS_CHK_MBUF_SZ(c_p);

        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ERTS_HOLE_CHECK(c_p);
    }
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    if (ERTS_IS_GC_AFTER_BIF_DESIRED(c_p)) {
        result = erts_gc_after_bif_call_lhf(c_p,
                                            live_hf_end,
                                            result,
                                            reg,
                                            arity);
    }

    return result;
}

Eterm beam_jit_call_nif(Process *c_p,
                        ErtsCodePtr I,
                        Eterm *reg,
                        BeamJitNifF *fp,
                        struct erl_module_nif *NifMod) {
    Eterm nif_bif_result;
    Eterm bif_nif_arity;
    ErlHeapFragment *live_hf_end;
    const ErtsCodeMFA *codemfa;

    codemfa = erts_code_to_codemfa(I);

    c_p->current = codemfa; /* current and vbf set to please handle_error */

    bif_nif_arity = codemfa->arity;
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);

    {
        struct enif_environment_t env;
        ASSERT(c_p->scheduler_data);
        live_hf_end = c_p->mbuf;
        ERTS_CHK_MBUF_SZ(c_p);
        erts_pre_nif(&env, c_p, NifMod, NULL);

        ASSERT((c_p->scheduler_data)->current_nif == NULL);
        (c_p->scheduler_data)->current_nif = &env;

        nif_bif_result = (*fp)(&env, bif_nif_arity, reg);
        if (env.exception_thrown)
            nif_bif_result = THE_NON_VALUE;

        ASSERT((c_p->scheduler_data)->current_nif == &env);
        (c_p->scheduler_data)->current_nif = NULL;

        erts_post_nif(&env);
        ERTS_CHK_MBUF_SZ(c_p);

        PROCESS_MAIN_CHK_LOCKS(c_p);
        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ASSERT(!env.exiting);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    }
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    ERTS_HOLE_CHECK(c_p);

    if (ERTS_IS_GC_AFTER_BIF_DESIRED(c_p)) {
        nif_bif_result = erts_gc_after_bif_call_lhf(c_p,
                                                    live_hf_end,
                                                    nif_bif_result,
                                                    reg,
                                                    bif_nif_arity);
    }

    return nif_bif_result;
}

enum beam_jit_nif_load_ret beam_jit_load_nif(Process *c_p,
                                             ErtsCodePtr I,
                                             Eterm *reg) {
    if (erts_try_seize_code_mod_permission(c_p)) {
        Eterm result;

        PROCESS_MAIN_CHK_LOCKS((c_p));
        ERTS_UNREQ_PROC_MAIN_LOCK((c_p));
        result = erts_load_nif(c_p, I, reg[0], reg[1]);
        erts_release_code_mod_permission();
        ERTS_REQ_PROC_MAIN_LOCK(c_p);

        if (ERTS_LIKELY(is_value(result))) {
            reg[0] = result;
            return RET_NIF_success;
        } else {
            c_p->freason = BADARG;
            return RET_NIF_error;
        }
    } else {
        /* Yield and try again. */
        c_p->current = NULL;
        c_p->arity = 2;
        return RET_NIF_yield;
    }
}

Uint beam_jit_get_map_elements(Eterm map,
                               Eterm *reg,
                               Eterm *E,
                               Uint n,
                               Eterm *fs) {
    Uint sz;

    /* This instruction assumes Arg1 is a map, i.e. that it follows a test
     * is_map if needed. */

    if (is_flatmap(map)) {
        flatmap_t *mp;
        Eterm *ks;
        Eterm *vs;

        mp = (flatmap_t *)flatmap_val(map);
        sz = flatmap_get_size(mp);

        if (sz == 0) {
            return 0;
        }

        ks = flatmap_get_keys(mp);
        vs = flatmap_get_values(mp);

        while (sz) {
            if (EQ(fs[0], *ks)) {
                PUT_TERM_REG(*vs, fs[1]);

                n--;
                fs += 3;

                /* no more values to fetch, we are done */
                if (n == 0) {
                    return 1;
                }
            }

            ks++, sz--, vs++;
        }
        return 0;
    } else {
        ASSERT(is_hashmap(map));

        while (n--) {
            erts_ihash_t hx;
            const Eterm *v;

            hx = fs[2];
            ASSERT(hx == hashmap_make_hash(fs[0]));

            if ((v = erts_hashmap_get(hx, fs[0], map)) == NULL) {
                return 0;
            }

            PUT_TERM_REG(*v, fs[1]);
            fs += 3;
        }

        return 1;
    }
}

static void test_bin_vheap(Process *c_p,
                           Eterm *reg,
                           Uint VNh,
                           Uint Nh,
                           Uint Live) {
    int need = Nh;

    if (c_p->stop - c_p->htop < (need + S_RESERVED) ||
        MSO(c_p).overhead + VNh >= c_p->bin_vheap_sz) {
        c_p->fcalls -=
                erts_garbage_collect_nobump(c_p, need, reg, Live, c_p->fcalls);
    }
}

static void gc_test(Process *c_p, Eterm *reg, Uint Ns, Uint Nh, Uint Live) {
    int need = Nh + Ns;

    if (ERTS_UNLIKELY(c_p->stop - c_p->htop < (need + S_RESERVED))) {
        c_p->fcalls -=
                erts_garbage_collect_nobump(c_p, need, reg, Live, c_p->fcalls);
    }
}

void beam_jit_bs_field_size_argument_error(Process *c_p, Eterm size) {
    if (((is_small(size) && signed_val(size) >= 0) ||
         (is_big(size) && !big_sign(size)))) {
        /* If the argument is a positive integer, we must've had a system_limit
         * error. */
        c_p->freason = SYSTEM_LIMIT;
    } else {
        c_p->freason = BADARG;
    }
}

/* Set the exception code for bs_add argument errors after the fact, which is
 * much easier and more compact than discriminating within module code. */
void beam_jit_bs_add_argument_error(Process *c_p, Eterm A, Eterm B) {
    if (((is_small(A) && signed_val(A) >= 0) || (is_big(A) && !big_sign(A))) &&
        ((is_small(B) && signed_val(B) >= 0) || (is_big(B) && !big_sign(B)))) {
        /* If all arguments are positive integers, we must've had a system_limit
         * error. */
        c_p->freason = SYSTEM_LIMIT;
    } else {
        c_p->freason = BADARG;
    }
}

Eterm beam_jit_bs_init_bits(Process *c_p,
                            Eterm *reg,
                            ERL_BITS_DECLARE_STATEP,
                            Uint num_bits,
                            Uint alloc,
                            unsigned Live) {
    if (num_bits <= ERL_ONHEAP_BITS_LIMIT) {
        alloc += heap_bits_size(num_bits);
    } else {
        alloc += ERL_REFC_BITS_SIZE;
    }

    erts_bin_offset = 0;

    if (num_bits <= ERL_ONHEAP_BITS_LIMIT) {
        ErlHeapBits *hb;

        gc_test(c_p, reg, 0, alloc, Live);
        hb = (ErlHeapBits *)c_p->htop;

        c_p->htop += heap_bits_size(num_bits);
        hb->thing_word = header_heap_bits(num_bits);
        hb->size = num_bits;

        erts_current_bin = (byte *)hb->data;
        return make_bitstring(hb);
    } else {
        const Uint num_bytes = NBYTES(num_bits);
        Binary *new_binary;

        test_bin_vheap(c_p, reg, num_bytes / sizeof(Eterm), alloc, Live);

        new_binary = erts_bin_nrml_alloc(num_bytes);
        erts_current_bin = (byte *)new_binary->orig_bytes;

        return erts_wrap_refc_bitstring(&MSO(c_p).first,
                                        &MSO(c_p).overhead,
                                        &HEAP_TOP(c_p),
                                        new_binary,
                                        erts_current_bin,
                                        0,
                                        num_bits);
    }
}

/*
 * This function can return one of the following:
 *
 * - THE_NON_VALUE if the extraction failed (for example, if the
 *   binary is shorter than the number of bits requested). The caller
 *   must raise an exception.
 *
 * - A nonempty list (cons) term. That means that the max_heap_size
 *   limit was exceeded. The caller must transfer control to the
 *   scheduler.
 *
 * - A tagged integer (small or big). The operation was successful.
 */

Eterm beam_jit_bs_get_integer(Process *c_p,
                              Eterm *reg,
                              Eterm context,
                              Uint flags,
                              Uint size,
                              Uint Live) {
    ErlSubBits *sb;

    if (size >= SMALL_BITS) {
        Uint wordsneeded;

        /* Check bits size before potential gc.
         * We do not want a gc and then realize we don't need
         * the allocated space (i.e. if the op fails).
         *
         * Remember to re-acquire the matchbuffer after gc.
         */
        sb = (ErlSubBits *)bitstring_val(context);
        if (sb->end - sb->start < size) {
            return THE_NON_VALUE;
        }

        wordsneeded = 1 + WSIZE(NBYTES((Uint)size));
        reg[Live] = context;
        gc_test(c_p, reg, 0, wordsneeded, Live + 1);
        if (ERTS_PROC_IS_EXITING(c_p)) {
            return make_list(0);
        }

        context = reg[Live];
    }

    sb = (ErlSubBits *)bitstring_val(context);
    return erts_bs_get_integer_2(c_p, size, flags, sb);
}

void beam_jit_bs_construct_fail_info(Process *c_p,
                                     Uint packed_error_info,
                                     Eterm arg3,
                                     Eterm arg1) {
    Eterm *hp;
    Eterm cause_tuple;
    Eterm error_info;
    Uint segment = beam_jit_get_bsc_segment(packed_error_info);
    JitBSCOp op = beam_jit_get_bsc_op(packed_error_info);
    JitBSCInfo info = beam_jit_get_bsc_info(packed_error_info);
    JitBSCReason reason = beam_jit_get_bsc_reason(packed_error_info);
    JitBSCValue value_location = beam_jit_get_bsc_value(packed_error_info);
    Eterm Op = am_none;
    Uint Reason = BADARG;
    Eterm Info = am_none;
    Eterm value = am_undefined;

    switch (op) {
    case BSC_OP_BITSTRING:
        Op = am_binary;
        break;
    case BSC_OP_FLOAT:
        Op = am_float;
        break;
    case BSC_OP_INTEGER:
        Op = am_integer;
        break;
    case BSC_OP_UTF8:
        Op = am_utf8;
        break;
    case BSC_OP_UTF16:
        Op = am_utf16;
        break;
    case BSC_OP_UTF32:
        Op = am_utf32;
        break;
    }

    switch (value_location) {
    case BSC_VALUE_ARG1:
        value = arg1;
        break;
    case BSC_VALUE_ARG3:
        value = arg3;
        break;
    case BSC_VALUE_FVALUE:
        value = c_p->fvalue;
        break;
    }

    switch (reason) {
    case BSC_REASON_BADARG:
        Reason = BADARG;
        break;
    case BSC_REASON_SYSTEM_LIMIT:
        Reason = SYSTEM_LIMIT;
        break;
    case BSC_REASON_DEPENDS:
        if ((is_small(value) && signed_val(value) >= 0) ||
            (is_big(value) && !big_sign(value))) {
            Reason = SYSTEM_LIMIT;
        } else {
            Reason = BADARG;
        }
        break;
    }

    switch (info) {
    case BSC_INFO_FVALUE:
        Info = c_p->fvalue;
        break;
    case BSC_INFO_TYPE:
        Info = am_type;
        break;
    case BSC_INFO_SIZE:
        Info = am_size;
        break;
    case BSC_INFO_UNIT:
        Info = am_unit;
        break;
    case BSC_INFO_DEPENDS:
        ASSERT(op == BSC_OP_BITSTRING);
        Info = is_bitstring(value) ? am_short : am_type;
        break;
    }

    hp = HAlloc(c_p, Sint(MAP3_SZ + 5));
    cause_tuple = TUPLE4(hp, make_small(segment), Op, Info, value);
    hp += 5;
    error_info = MAP3(hp,
                      am_cause,
                      cause_tuple,
                      am_function,
                      am_format_bs_fail,
                      am_module,
                      am_erl_erts_errors);
    c_p->fvalue = error_info;
    c_p->freason = Reason | EXF_HAS_EXT_INFO;
}

Sint beam_jit_bs_bit_size(Eterm term) {
    if (is_bitstring(term)) {
        Uint size = bitstring_size(term);

        ASSERT(sizeof(Uint) == 8); /* Only support 64-bit machines. */
        ASSERT(size <= ERTS_SINT_MAX);

        return (Sint)size;
    }

    /* Signal error */
    return (Sint)-1;
}

Eterm beam_jit_int128_to_big(Process *p, Uint sign, Uint low, Uint high) {
    Eterm *hp;
    Uint arity;

    arity = high ? 2 : 1;
    hp = HeapFragOnlyAlloc(p, BIG_NEED_SIZE(arity));
    if (sign) {
        hp[0] = make_neg_bignum_header(arity);
    } else {
        hp[0] = make_pos_bignum_header(arity);
    }
    BIG_DIGIT(hp, 0) = low;
    if (arity == 2) {
        BIG_DIGIT(hp, 1) = high;
    }
    return make_big(hp);
}

ErtsMessage *beam_jit_decode_dist(Process *c_p, ErtsMessage *msgp) {
    if (!erts_proc_sig_decode_dist(c_p, ERTS_PROC_LOCK_MAIN, msgp, 0)) {
        /*
         * A corrupt distribution message that we weren't able to decode;
         * remove it...
         */

        /* TODO: Add DTrace probe for this bad message situation? */
        erts_msgq_unlink_msg(c_p, msgp);
        msgp->next = NULL;
        erts_cleanup_messages(msgp);

        return NULL;
    }

    return msgp;
}

/* Remove a (matched) message from the message queue. */
Sint32 beam_jit_remove_message(Process *c_p,
                               Sint32 FCALLS,
                               Eterm *HTOP,
                               Eterm *E,
                               Uint32 active_code_ix) {
    ErtsMessage *msgp;

    ERTS_CHK_MBUF_SZ(c_p);

    if (active_code_ix == ERTS_SAVE_CALLS_CODE_IX) {
        save_calls(c_p, &exp_receive);
    }

    msgp = erts_msgq_peek_msg(c_p);

    if (ERL_MESSAGE_TOKEN(msgp) == NIL) {
#ifdef USE_VM_PROBES
        if (DT_UTAG(c_p) != NIL) {
            if (DT_UTAG_FLAGS(c_p) & DT_UTAG_PERMANENT) {
                SEQ_TRACE_TOKEN(c_p) = am_have_dt_utag;
            } else {
                DT_UTAG(c_p) = NIL;
                SEQ_TRACE_TOKEN(c_p) = NIL;
            }
        } else {
#endif
            SEQ_TRACE_TOKEN(c_p) = NIL;
#ifdef USE_VM_PROBES
        }
        DT_UTAG_FLAGS(c_p) &= ~DT_UTAG_SPREADING;
#endif
    } else if (ERL_MESSAGE_TOKEN(msgp) != am_undefined) {
        Eterm msg;
        SEQ_TRACE_TOKEN(c_p) = ERL_MESSAGE_TOKEN(msgp);
#ifdef USE_VM_PROBES
        if (ERL_MESSAGE_TOKEN(msgp) == am_have_dt_utag) {
            if (DT_UTAG(c_p) == NIL) {
                DT_UTAG(c_p) = ERL_MESSAGE_DT_UTAG(msgp);
            }
            DT_UTAG_FLAGS(c_p) |= DT_UTAG_SPREADING;
        } else {
#endif
            ASSERT(is_tuple(SEQ_TRACE_TOKEN(c_p)));
            ASSERT(SEQ_TRACE_TOKEN_ARITY(c_p) == 5);
            ASSERT(is_small(SEQ_TRACE_TOKEN_SERIAL(c_p)));
            ASSERT(is_small(SEQ_TRACE_TOKEN_LASTCNT(c_p)));
            ASSERT(is_small(SEQ_TRACE_TOKEN_FLAGS(c_p)));
            ASSERT(is_pid(SEQ_TRACE_TOKEN_SENDER(c_p)) ||
                   is_atom(SEQ_TRACE_TOKEN_SENDER(c_p)));
            c_p->seq_trace_lastcnt = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
            if (c_p->seq_trace_clock <
                unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p))) {
                c_p->seq_trace_clock =
                        unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
            }
            msg = ERL_MESSAGE_TERM(msgp);
            seq_trace_output(SEQ_TRACE_TOKEN(c_p),
                             msg,
                             SEQ_TRACE_RECEIVE,
                             c_p->common.id,
                             c_p);
#ifdef USE_VM_PROBES
        }
#endif
    }
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(message_receive)) {
        Eterm token2 = NIL;
        DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);
        Sint tok_label = 0;
        Sint tok_lastcnt = 0;
        Sint tok_serial = 0;

        dtrace_proc_str(c_p, receiver_name);
        token2 = SEQ_TRACE_TOKEN(c_p);
        if (have_seqtrace(token2)) {
            tok_label = SEQ_TRACE_T_DTRACE_LABEL(token2);
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(token2));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(token2));
        }
        DTRACE6(message_receive,
                receiver_name,
                size_object(ERL_MESSAGE_TERM(msgp)),
                c_p->sig_qs.mq_len,
                tok_label,
                tok_lastcnt,
                tok_serial);
    }
#endif
    erts_msgq_unlink_msg_set_save_first(c_p, msgp);
    CANCEL_TIMER(c_p);

    erts_save_message_in_proc(c_p, msgp);
    c_p->flags &= ~F_DELAY_GC;

    if (ERTS_IS_GC_DESIRED_INTERNAL(c_p, HTOP, E, 0)) {
        /*
         * We want to GC soon but we leave a few
         * reductions giving the message some time
         * to turn into garbage.
         */
        ERTS_VBUMP_LEAVE_REDS_INTERNAL(c_p, 5, FCALLS);
    }

    ERTS_CHK_MBUF_SZ(c_p);

    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    return FCALLS;
}

void beam_jit_take_receive_lock(Process *c_p) {
    erts_proc_lock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
}

void beam_jit_wait_locked(Process *c_p, ErtsCodePtr cp) {
    c_p->arity = 0;
    if (!ERTS_PTMR_IS_TIMED_OUT(c_p)) {
        erts_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
    }
    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    c_p->current = NULL;
    c_p->i = cp;
}

void beam_jit_wait_unlocked(Process *c_p, ErtsCodePtr cp) {
    beam_jit_take_receive_lock(c_p);
    beam_jit_wait_locked(c_p, cp);
}

enum beam_jit_tmo_ret beam_jit_wait_timeout(Process *c_p,
                                            Eterm timeout_value,
                                            ErtsCodePtr next) {
    /*
     * If we have already set the timer, we must NOT set it again.  Therefore,
     * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
     */
    if ((c_p->flags & (F_INSLPQUEUE | F_TIMO)) == 0) {
        if (timeout_value == make_small(0)) {
            erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
            return RET_next;
        } else if (timeout_value == am_infinity) {
            c_p->flags |= F_TIMO;
        } else {
            int tres = erts_set_proc_timer_term(c_p, timeout_value);
            if (tres == 0) {
                /*
                 * The timer routiner will set c_p->i to the value in
                 * c_p->def_arg_reg[0].  Note that it is safe to use this
                 * location because there are no living x registers in
                 * a receive statement.
                 */
                c_p->def_arg_reg[0] = (Eterm)next;
            } else { /* Wrong time */
                erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
                c_p->freason = EXC_TIMEOUT_VALUE;
                erts_msgq_set_save_first(c_p);
                return RET_badarg;
            }
        }
    }
    return RET_wait;
}

void beam_jit_timeout(Process *c_p) {
    if (ERTS_IS_P_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
        trace_receive(c_p, am_clock_service, am_timeout);
    }
    if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)) {
        save_calls(c_p, &exp_timeout);
    }
    c_p->flags &= ~F_TIMO;
    erts_msgq_set_save_first(c_p);
}

void beam_jit_timeout_locked(Process *c_p) {
    erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    beam_jit_timeout(c_p);
}

void beam_jit_return_to_trace(Process *c_p,
                              Eterm session_weak_id,
                              Eterm *frame) {
    if (ERTS_IS_P_TRACED_FL(c_p, F_TRACE_RETURN_TO)) {
        ErtsCodePtr return_to_address;
        Uint *cpp;

        cpp = (Uint *)frame;
        ASSERT(is_CP(cpp[0]));

        for (;;) {
            erts_inspect_frame(cpp, &return_to_address);

            if (BeamIsReturnTrace(return_to_address)) {
                cpp += CP_SIZE + BEAM_RETURN_TRACE_FRAME_SZ;
            } else if (BeamIsReturnCallAccTrace(return_to_address)) {
                cpp += CP_SIZE + BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
            } else if (BeamIsReturnToTrace(return_to_address)) {
                cpp += CP_SIZE + BEAM_RETURN_TO_TRACE_FRAME_SZ;
            } else {
                break;
            }
        }

        ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
        erts_trace_return_to(c_p, return_to_address, session_weak_id);
        ERTS_REQ_PROC_MAIN_LOCK(c_p);
    }
}

Eterm beam_jit_build_argument_list(Process *c_p, const Eterm *regs, int arity) {
    Eterm *hp;
    Eterm res;

    hp = HAlloc(c_p, arity * 2);
    res = NIL;

    for (int i = arity - 1; i >= 0; i--) {
        res = CONS(hp, regs[i], res);
        hp += 2;
    }

    return res;
}

Export *beam_jit_handle_unloaded_fun(Process *c_p,
                                     Eterm *reg,
                                     int arity,
                                     Eterm fun_thing) {
    ErtsCodeIndex code_ix = erts_active_code_ix();
    Eterm module, args;
    ErlFunThing *funp;
    ErlFunEntry *fe;
    Module *modp;
    Export *ep;

    funp = (ErlFunThing *)fun_val(fun_thing);
    ASSERT(is_local_fun(funp));

    fe = funp->entry.fun;
    module = fe->module;

    ERTS_THR_READ_MEMORY_BARRIER;

    if (fe->pend_purge_address) {
        /* The system is currently trying to purge the module containing this
         * fun. Suspend the process and let it try again when the purge
         * operation is done (may succeed or not). */
        ep = erts_suspend_process_on_pending_purge_lambda(c_p, fe);
    } else {
        if ((modp = erts_get_module(module, code_ix)) != NULL &&
            modp->curr.code_hdr != NULL) {
            /* There is a module loaded, but obviously the fun is not defined
             * in it. We must not call the error_handler (or we will get into
             * an infinite loop). */
            c_p->current = NULL;
            c_p->freason = EXC_BADFUN;
            c_p->fvalue = fun_thing;
            return NULL;
        }

        /* No current code for this module. Call the error_handler module to
         * attempt loading the module. */
        ep = erts_find_function(erts_proc_get_error_handler(c_p),
                                am_undefined_lambda,
                                3,
                                code_ix);
        if (ERTS_UNLIKELY(ep == NULL)) {
            /* No error handler, crash out. */
            c_p->current = NULL;
            c_p->freason = EXC_UNDEF;
            return NULL;
        }
    }

    args = beam_jit_build_argument_list(c_p, reg, arity);

    reg[0] = module;
    reg[1] = fun_thing;
    reg[2] = args;
    reg[3] = NIL;

    return ep;
}

bool beam_jit_is_list_of_immediates(Eterm term) {
    while (is_list(term)) {
        Eterm *cons = list_val(term);
        if (!is_immed(CAR(cons))) {
            return false;
        }
        term = CDR(cons);
    }
    return is_nil(term);
}

bool beam_jit_is_shallow_boxed(Eterm term) {
    if (is_tuple(term)) {
        Eterm *tuple_ptr = tuple_val(term);
        for (unsigned i = 1; i <= arityval(*tuple_ptr); i++) {
            if (!is_immed(tuple_ptr[i])) {
                return false;
            }
        }
        return true;
    } else if (is_big(term)) {
        return true;
    } else if (is_float(term)) {
        return true;
    } else {
        return false;
    }
}
