/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021. All Rights Reserved.
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

BeamAssembler::BeamAssembler() : code() {
    /* Setup with default code info */
    Error err = code.init(hostEnvironment());
    ERTS_ASSERT(!err && "Failed to init codeHolder");

    err = code.newSection(&rodata, ".rodata", SIZE_MAX, Section::kFlagConst, 8);
    ERTS_ASSERT(!err && "Failed to create .rodata section");

    err = code.attach(&a);

    ERTS_ASSERT(!err && "Failed to attach codeHolder");
#ifdef DEBUG
    a.addValidationOptions(BaseEmitter::kValidationOptionAssembler);
#endif
    a.addEncodingOptions(BaseEmitter::kEncodingOptionOptimizeForSize);
    code.setErrorHandler(this);
}

BeamAssembler::BeamAssembler(const std::string &log) : BeamAssembler() {
    if (erts_jit_asm_dump) {
        setLogger(log + ".asm");
    }
}

BeamAssembler::~BeamAssembler() {
    if (logger.file()) {
        fclose(logger.file());
    }
}

void *BeamAssembler::getBaseAddress() {
    ASSERT(code.hasBaseAddress());
    return (void *)code.baseAddress();
}

size_t BeamAssembler::getOffset() {
    return a.offset();
}

void BeamAssembler::_codegen(JitAllocator *allocator,
                             const void **executable_ptr,
                             void **writable_ptr) {
    Error err = code.flatten();
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

    err = allocator->alloc(const_cast<void **>(executable_ptr),
                           writable_ptr,
                           code.codeSize() + 16);

    if (err == ErrorCode::kErrorTooManyHandles) {
        ERTS_ASSERT(!"Failed to allocate module code: "
                     "out of file descriptors");
    } else if (err) {
        ERTS_ASSERT("Failed to allocate module code");
    }

    code.relocateToBase((uint64_t)*executable_ptr);
    code.copyFlattenedData(*writable_ptr,
                           code.codeSize(),
                           CodeHolder::kCopyPadSectionBuffer);

    beamasm_flush_icache(*executable_ptr, code.codeSize());

#ifdef DEBUG
    if (FileLogger *l = dynamic_cast<FileLogger *>(code.logger()))
        if (FILE *f = l->file())
            fprintf(f, "; CODE_SIZE: %zd\n", code.codeSize());
#endif
}

void *BeamAssembler::getCode(Label label) {
    ASSERT(label.isValid());
    return (char *)getBaseAddress() + code.labelOffsetFromBase(label);
}

byte *BeamAssembler::getCode(char *labelName) {
    return (byte *)getCode(code.labelByName(labelName, strlen(labelName)));
}

void BeamAssembler::handleError(Error err,
                                const char *message,
                                BaseEmitter *origin) {
    comment(message);
    fflush(logger.file());
    ASSERT(0 && "Failed to encode instruction");
}

void BeamAssembler::embed_rodata(const char *labelName,
                                 const char *buff,
                                 size_t size) {
    Label label = a.newNamedLabel(labelName);

    a.section(rodata);
    a.bind(label);
    a.embed(buff, size);
    a.section(code.textSection());
}

void BeamAssembler::embed_bss(const char *labelName, size_t size) {
    Label label = a.newNamedLabel(labelName);

    /* Reuse rodata section for now */
    a.section(rodata);
    a.bind(label);
    embed_zeros(size);
    a.section(code.textSection());
}

void BeamAssembler::embed_zeros(size_t size) {
    static constexpr size_t buf_size = 16384;
    static const char zeros[buf_size] = {};

    while (size >= buf_size) {
        a.embed(zeros, buf_size);
        size -= buf_size;
    }

    if (size > 0) {
        a.embed(zeros, size);
    }
}

void BeamAssembler::setLogger(std::string log) {
    FILE *f = fopen(log.data(), "w+");

    /* FIXME: Don't crash when loading multiple modules with the same name.
     *
     * setLogger(nullptr) disables logging. */
    if (f) {
        setvbuf(f, NULL, _IONBF, 0);
    }

    setLogger(f);
}

void BeamAssembler::setLogger(FILE *log) {
    logger.setFile(log);
    logger.setIndentation(FormatOptions::kIndentationCode, 4);
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

    codegen(allocator, executable_ptr, writable_ptr);

    {
        auto offset = code.labelOffsetFromBase(codeHeader);

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

    *out_exec_hdr = code_hdr_exec;
    *out_rw_hdr = code_hdr_rw;
}

void BeamModuleAssembler::codegen(JitAllocator *allocator,
                                  const void **executable_ptr,
                                  void **writable_ptr) {
    _codegen(allocator, executable_ptr, writable_ptr);

#if !(defined(WIN32) || defined(__APPLE__) || defined(__MACH__) ||             \
      defined(__DARWIN__))
    if (functions.size()) {
        char *buff = (char *)erts_alloc(ERTS_ALC_T_TMP, 1024);
        std::vector<AsmRange> ranges;
        std::string name = getAtom(mod);
        ranges.reserve(functions.size() + 2);

        /* Push info about the header */
        ranges.push_back({.start = (ErtsCodePtr)getBaseAddress(),
                          .stop = getCode(functions[0]),
                          .name = name + "::codeHeader"});

        for (unsigned i = 0; i < functions.size(); i++) {
            ErtsCodePtr start, stop;
            const ErtsCodeInfo *ci;
            int n;

            start = getCode(functions[i]);
            ci = (const ErtsCodeInfo *)start;

            n = erts_snprintf(buff,
                              1024,
                              "%T:%T/%d",
                              ci->mfa.module,
                              ci->mfa.function,
                              ci->mfa.arity);
            stop = ((const char *)erts_codeinfo_to_code(ci)) +
                   BEAM_ASM_FUNC_PROLOGUE_SIZE;

            /* We use a different symbol for CodeInfo and the Prologue
               in order for the perf disassembly to be better. */
            std::string name(buff, n);
            ranges.push_back({.start = start,
                              .stop = stop,
                              .name = name + "-CodeInfoPrologue"});

            /* The actual code */
            start = stop;
            if (i + 1 < functions.size()) {
                stop = getCode(functions[i + 1]);
            } else {
                stop = getCode(code_end);
            }

            ranges.push_back({.start = start, .stop = stop, .name = name});
        }

        /* Push info about the footer */
        ranges.push_back(
                {.start = ranges.back().stop,
                 .stop = (ErtsCodePtr)(code.baseAddress() + code.codeSize()),
                 .name = name + "::codeFooter"});

        update_gdb_jit_info(name, ranges);
        beamasm_update_perf_info(name, ranges);
        erts_free(ERTS_ALC_T_TMP, buff);
    }
#endif
}

void BeamModuleAssembler::codegen(char *buff, size_t len) {
    code.flatten();
    code.resolveUnresolvedLinks();
    ERTS_ASSERT(code.codeSize() <= len);
    code.relocateToBase((uint64_t)buff);
    code.copyFlattenedData(buff,
                           code.codeSize(),
                           CodeHolder::kCopyPadSectionBuffer);
}

BeamCodeHeader *BeamModuleAssembler::getCodeHeader() {
    return (BeamCodeHeader *)getCode(codeHeader);
}

const ErtsCodeInfo *BeamModuleAssembler::getOnLoad() {
    if (on_load.isValid()) {
        return erts_code_to_codeinfo((ErtsCodePtr)getCode(on_load));
    } else {
        return 0;
    }
}

/* ** */

#if defined(DEBUG) && defined(JIT_HARD_DEBUG)
void beam_jit_validate_term(Eterm term) {
    if (is_boxed(term)) {
        Eterm header = *boxed_val(term);

        if (header_is_bin_matchstate(header)) {
            return;
        }
    }

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

    if (ERTS_IS_GC_DESIRED(c_p)) {
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

    if (ERTS_IS_GC_DESIRED(c_p)) {
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
    if (erts_try_seize_code_write_permission(c_p)) {
        Eterm result;

        PROCESS_MAIN_CHK_LOCKS((c_p));
        ERTS_UNREQ_PROC_MAIN_LOCK((c_p));
        result = erts_load_nif(c_p, I, reg[0], reg[1]);
        erts_release_code_write_permission();
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
            const Eterm *v;
            Uint32 hx;

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
        MSO(c_p).overhead + VNh >= BIN_VHEAP_SZ(c_p)) {
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

static Eterm i_bs_start_match2_gc_test_preserve(Process *c_p,
                                                Eterm *reg,
                                                Uint need,
                                                Uint live,
                                                Eterm preserve) {
    Uint words_left = (Uint)(STACK_TOP(c_p) - HEAP_TOP(c_p));

    if (ERTS_UNLIKELY(words_left < need + S_RESERVED)) {
        reg[live] = preserve;
        PROCESS_MAIN_CHK_LOCKS(c_p);
        c_p->fcalls -= erts_garbage_collect_nobump(c_p,
                                                   need,
                                                   reg,
                                                   live + 1,
                                                   c_p->fcalls);
        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        PROCESS_MAIN_CHK_LOCKS(c_p);
        preserve = reg[live];
    }

    return preserve;
}

Eterm beam_jit_bs_start_match2(Eterm context,
                               Uint live,
                               Uint slots,
                               Process *c_p,
                               Eterm *reg) {
    Eterm header;
    if (!is_boxed(context)) {
        return THE_NON_VALUE;
    }
    header = *boxed_val(context);

    slots++;

    if (header_is_bin_matchstate(header)) {
        ErlBinMatchState *ms = (ErlBinMatchState *)boxed_val(context);
        Uint actual_slots = HEADER_NUM_SLOTS(header);

        /* We're not compatible with contexts created by bs_start_match3. */
        ASSERT(actual_slots >= 1);

        ms->save_offset[0] = ms->mb.offset;
        if (ERTS_UNLIKELY(actual_slots < slots)) {
            ErlBinMatchState *expanded;
            Uint wordsneeded = ERL_BIN_MATCHSTATE_SIZE(slots);
            context = i_bs_start_match2_gc_test_preserve(c_p,
                                                         reg,
                                                         wordsneeded,
                                                         live,
                                                         context);
            ms = (ErlBinMatchState *)boxed_val(context);
            expanded = (ErlBinMatchState *)HEAP_TOP(c_p);
            *expanded = *ms;
            *HEAP_TOP(c_p) = HEADER_BIN_MATCHSTATE(slots);
            HEAP_TOP(c_p) += wordsneeded;
            context = make_matchstate(expanded);
        }
        return context;
    } else if (is_binary_header(header)) {
        Uint wordsneeded = ERL_BIN_MATCHSTATE_SIZE(slots);
        context = i_bs_start_match2_gc_test_preserve(c_p,
                                                     reg,
                                                     wordsneeded,
                                                     live,
                                                     context);
        return erts_bs_start_match_2(c_p, context, slots);
    } else {
        return THE_NON_VALUE;
    }
}

Eterm beam_jit_bs_init(Process *c_p,
                       Eterm *reg,
                       ERL_BITS_DECLARE_STATEP,
                       Eterm BsOp1,
                       Eterm BsOp2,
                       unsigned Live) {
    if (BsOp1 <= ERL_ONHEAP_BIN_LIMIT) {
        ErlHeapBin *hb;
        Uint bin_need;

        bin_need = heap_bin_size(BsOp1);
        erts_bin_offset = 0;
        erts_writable_bin = 0;
        gc_test(c_p, reg, 0, bin_need + BsOp2 + ERL_SUB_BIN_SIZE, Live);
        hb = (ErlHeapBin *)c_p->htop;
        c_p->htop += bin_need;
        hb->thing_word = header_heap_bin(BsOp1);
        hb->size = BsOp1;
        erts_current_bin = (byte *)hb->data;
        return make_binary(hb);
    } else {
        Binary *bptr;
        ProcBin *pb;

        erts_bin_offset = 0;
        erts_writable_bin = 0;
        test_bin_vheap(c_p,
                       reg,
                       BsOp1 / sizeof(Eterm),
                       BsOp2 + PROC_BIN_SIZE + ERL_SUB_BIN_SIZE,
                       Live);

        /*
         * Allocate the binary struct itself.
         */
        bptr = erts_bin_nrml_alloc(BsOp1);
        erts_current_bin = (byte *)bptr->orig_bytes;

        /*
         * Now allocate the ProcBin on the heap.
         */
        pb = (ProcBin *)c_p->htop;
        c_p->htop += PROC_BIN_SIZE;
        pb->thing_word = HEADER_PROC_BIN;
        pb->size = BsOp1;
        pb->next = MSO(c_p).first;
        MSO(c_p).first = (struct erl_off_heap_header *)pb;
        pb->val = bptr;
        pb->bytes = (byte *)bptr->orig_bytes;
        pb->flags = 0;

        OH_OVERHEAD(&(MSO(c_p)), BsOp1 / sizeof(Eterm));

        return make_binary(pb);
    }
}

Eterm beam_jit_bs_init_bits(Process *c_p,
                            Eterm *reg,
                            ERL_BITS_DECLARE_STATEP,
                            Uint num_bits,
                            Uint alloc,
                            unsigned Live) {
    Eterm new_binary;
    Uint num_bytes = ((Uint64)num_bits + (Uint64)7) >> 3;

    if (num_bits & 7) {
        alloc += ERL_SUB_BIN_SIZE;
    }
    if (num_bytes <= ERL_ONHEAP_BIN_LIMIT) {
        alloc += heap_bin_size(num_bytes);
    } else {
        alloc += PROC_BIN_SIZE;
    }
    gc_test(c_p, reg, 0, alloc, Live);

    /* num_bits = Number of bits to build
     * num_bytes = Number of bytes to allocate in the binary
     * alloc = Total number of words to allocate on heap
     * Operands: NotUsed NotUsed Dst
     */
    if (num_bytes <= ERL_ONHEAP_BIN_LIMIT) {
        ErlHeapBin *hb;

        erts_bin_offset = 0;
        erts_writable_bin = 0;
        hb = (ErlHeapBin *)c_p->htop;
        c_p->htop += heap_bin_size(num_bytes);
        hb->thing_word = header_heap_bin(num_bytes);
        hb->size = num_bytes;
        erts_current_bin = (byte *)hb->data;
        new_binary = make_binary(hb);

    do_bits_sub_bin:
        if (num_bits & 7) {
            ErlSubBin *sb;

            sb = (ErlSubBin *)c_p->htop;
            c_p->htop += ERL_SUB_BIN_SIZE;
            sb->thing_word = HEADER_SUB_BIN;
            sb->size = num_bytes - 1;
            sb->bitsize = num_bits & 7;
            sb->offs = 0;
            sb->bitoffs = 0;
            sb->is_writable = 0;
            sb->orig = new_binary;
            new_binary = make_binary(sb);
        }
        /*    HEAP_SPACE_VERIFIED(0); */
        return new_binary;
    } else {
        Binary *bptr;
        ProcBin *pb;

        erts_bin_offset = 0;
        erts_writable_bin = 0;

        /*
         * Allocate the binary struct itself.
         */
        bptr = erts_bin_nrml_alloc(num_bytes);
        erts_current_bin = (byte *)bptr->orig_bytes;

        /*
         * Now allocate the ProcBin on the heap.
         */
        pb = (ProcBin *)c_p->htop;
        c_p->htop += PROC_BIN_SIZE;
        pb->thing_word = HEADER_PROC_BIN;
        pb->size = num_bytes;
        pb->next = MSO(c_p).first;
        MSO(c_p).first = (struct erl_off_heap_header *)pb;
        pb->val = bptr;
        pb->bytes = (byte *)bptr->orig_bytes;
        pb->flags = 0;
        OH_OVERHEAD(&(MSO(c_p)), pb->size / sizeof(Eterm));
        new_binary = make_binary(pb);
        goto do_bits_sub_bin;
    }
}

Eterm beam_jit_bs_get_integer(Process *c_p,
                              Eterm *reg,
                              Eterm context,
                              Uint flags,
                              Uint size,
                              Uint Live) {
    ErlBinMatchBuffer *mb;

    if (size >= SMALL_BITS) {
        Uint wordsneeded;

        /* Check bits size before potential gc.
         * We do not want a gc and then realize we don't need
         * the allocated space (i.e. if the op fails).
         *
         * Remember to re-acquire the matchbuffer after gc.
         */
        mb = ms_matchbuffer(context);
        if (mb->size - mb->offset < size) {
            return THE_NON_VALUE;
        }

        wordsneeded = 1 + WSIZE(NBYTES((Uint)size));
        reg[Live] = context;
        gc_test(c_p, reg, 0, wordsneeded, Live + 1);
        context = reg[Live];
    }

    mb = ms_matchbuffer(context);
    return erts_bs_get_integer_2(c_p, size, flags, mb);
}

void beam_jit_bs_context_to_binary(Eterm context) {
    if (is_boxed(context) && header_is_bin_matchstate(*boxed_val(context))) {
        Uint orig, size, offs, hole_size;
        ErlBinMatchBuffer *mb;
        ErlBinMatchState *ms;
        ErlSubBin *sb;
        ms = (ErlBinMatchState *)boxed_val(context);
        mb = &ms->mb;
        offs = ms->save_offset[0];
        size = mb->size - offs;
        orig = mb->orig;
        sb = (ErlSubBin *)boxed_val(context);
        /* Since we're going to overwrite the match state with the result, an
         * ErlBinMatchState must be at least as large as an ErlSubBin. */
        ERTS_CT_ASSERT(sizeof(ErlSubBin) <= sizeof(ErlBinMatchState));
        hole_size = 1 + header_arity(sb->thing_word) - ERL_SUB_BIN_SIZE;
        sb->thing_word = HEADER_SUB_BIN;
        sb->size = BYTE_OFFSET(size);
        sb->bitsize = BIT_OFFSET(size);
        sb->offs = BYTE_OFFSET(offs);
        sb->bitoffs = BIT_OFFSET(offs);
        sb->is_writable = 0;
        sb->orig = orig;
        if (hole_size) {
            sb[1].thing_word = make_pos_bignum_header(hole_size - 1);
        }
    }
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
Sint beam_jit_remove_message(Process *c_p,
                             Sint FCALLS,
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
        Sint len = erts_proc_sig_privqs_len(c_p);

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
                len, /* This is NOT message queue len, but its something... */
                tok_label,
                tok_lastcnt,
                tok_serial);
    }
#endif
    erts_msgq_unlink_msg(c_p, msgp);
    erts_msgq_set_save_first(c_p);
    CANCEL_TIMER(c_p);

    erts_save_message_in_proc(c_p, msgp);
    c_p->flags &= ~F_DELAY_GC;

    if (ERTS_IS_GC_DESIRED_INTERNAL(c_p, HTOP, E)) {
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
                return RET_badarg;
            }
        }
    }
    return RET_wait;
}

void beam_jit_timeout(Process *c_p) {
    if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
        trace_receive(c_p, am_clock_service, am_timeout, NULL);
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

void beam_jit_return_to_trace(Process *c_p) {
    if (IS_TRACED_FL(c_p, F_TRACE_RETURN_TO)) {
        ErtsCodePtr return_to_address;
        Uint *cpp;

        cpp = (Uint *)c_p->stop;
        ASSERT(is_CP(cpp[0]));

        for (;;) {
            erts_inspect_frame(cpp, &return_to_address);

            if (BeamIsReturnTrace(return_to_address)) {
                cpp += CP_SIZE + 2;
            } else if (BeamIsReturnTimeTrace(return_to_address)) {
                cpp += CP_SIZE + 1;
            } else if (BeamIsReturnToTrace(return_to_address)) {
                cpp += CP_SIZE;
            } else {
                break;
            }
        }

        ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
        erts_trace_return_to(c_p, return_to_address);
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
    fe = funp->fe;
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
