/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_binary.h"
#include "beam_catches.h"
#include "beam_load.h"
#include "erl_version.h"
#include "beam_bp.h"

#define CodeNeed(w) do {                                                \
    ASSERT(ci <= codev_size);                                           \
    if (codev_size < ci+(w)) {                                          \
        codev_size = 2*ci+(w);                                          \
        stp->code_hdr = (BeamCodeHeader*) erts_realloc(ERTS_ALC_T_CODE, \
            (void *) stp->code_hdr,                                     \
            (offsetof(BeamCodeHeader,functions)                         \
             + codev_size * sizeof(BeamInstr)));                        \
        code = stp->codev = (BeamInstr*) &stp->code_hdr->functions;     \
    }                                                                   \
} while (0)

/* #define LOAD_MEMORY_HARD_DEBUG 1*/

#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
/* Requires allocators ERTS_ALLOC_UTIL_HARD_DEBUG also set in erl_alloc_util.h */
extern void check_allocators(void);
extern void check_allocated_block(Uint type, void *blk);
#define CHKALLOC() check_allocators()
#define CHKBLK(TYPE,BLK) if ((BLK) != NULL) check_allocated_block((TYPE),(BLK))
#else
#define CHKALLOC() /* nothing */
#define CHKBLK(TYPE,BLK) /* nothing */
#endif

static void init_label(Label* lp);

int beam_load_prepare_emit(LoaderState *stp) {
    BeamCodeHeader *hdr;
    int i;

    /* Initialize code area and header. */
    stp->codev_size = 2048 + stp->beam.code.function_count;
    hdr = erts_alloc(ERTS_ALC_T_CODE,
                     (offsetof(BeamCodeHeader,functions)
                      + sizeof(BeamInstr) * stp->codev_size));

    hdr->num_functions = stp->beam.code.function_count;
    hdr->attr_ptr = NULL;
    hdr->attr_size = 0;
    hdr->attr_size_on_heap = 0;
    hdr->compile_ptr = NULL;
    hdr->compile_size = 0;
    hdr->compile_size_on_heap = 0;
    hdr->literal_area = NULL;
    hdr->md5_ptr = NULL;
    hdr->are_nifs = NULL;

    stp->code_hdr = hdr;

    /* Let the codev array start at functions[0] in order to index
     * both function pointers and the loaded code itself that follows. */
    stp->codev = (BeamInstr*)&hdr->functions;
    stp->ci = stp->beam.code.function_count + 1;

    stp->catches = 0;

    stp->labels = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                             stp->beam.code.label_count * sizeof(Label));
    for (i = 0; i < stp->beam.code.label_count; i++) {
        init_label(&stp->labels[i]);
    }

    stp->fun_refs = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                               stp->beam.lambdas.count * sizeof(SWord));
    for (i = 0; i < stp->beam.lambdas.count; i++) {
        stp->fun_refs[i] = ERTS_SWORD_MAX;
    }

    stp->import_patches =
        erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                   stp->beam.imports.count * sizeof(BeamInstr));

    sys_memzero(stp->import_patches,
                stp->beam.imports.count * sizeof(BeamInstr));

    stp->bif_imports = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                  stp->beam.imports.count * sizeof(BifEntry**));

    for (i = 0; i < stp->beam.imports.count; i++) {
        BeamFile_ImportEntry *import;
        Export *export;
        int bif_number;

        import = &stp->beam.imports.entries[i];
        export = erts_active_export_entry(import->module,
                                          import->function,
                                          import->arity);

        stp->bif_imports[i] = NULL;

        if (export) {
            bif_number = export->bif_number;

            if (bif_number >= 0) {
                if (bif_number == BIF_load_nif_2) {
                    stp->may_load_nif = 1;
                }

                stp->bif_imports[i] = &bif_table[bif_number];
            }
        }
    }

    stp->current_li = 0;
    if (stp->beam.lines.item_count > 0) {
        stp->line_instr = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                     stp->beam.lines.instruction_count *
                                         sizeof(LineInstr));
        stp->func_line = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                    stp->beam.code.function_count *
                                        sizeof(unsigned int));
    }

    return 1;
}

void beam_load_prepared_free(Binary* magic)
{
    beam_load_prepared_dtor(magic);
    erts_bin_release(magic);
}

/* This destructor function can safely be called multiple times. */
int beam_load_prepared_dtor(Binary* magic)
{
    LoaderState* stp = ERTS_MAGIC_BIN_DATA(magic);

    /* This should have been freed earlier! */
    ASSERT(stp->op_allocator.beamop_blocks == NULL);

    beamfile_free(&stp->beam);
    beamopallocator_dtor(&stp->op_allocator);

    if (stp->code_hdr) {
        BeamCodeHeader *hdr = stp->code_hdr;

        if (hdr->literal_area) {
            erts_release_literal_area(hdr->literal_area);
            hdr->literal_area = NULL;
        }
        if (hdr->are_nifs) {
            erts_free(ERTS_ALC_T_PREPARED_CODE, hdr->are_nifs);
            hdr->are_nifs = NULL;
        }

        erts_free(ERTS_ALC_T_CODE, hdr);
        stp->code_hdr = NULL;
        stp->codev = NULL;
    }

    if (stp->labels) {
        Uint num;
        for (num = 0; num < stp->beam.code.label_count; num++) {
            erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->labels[num].patches);
        }
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->labels);
        stp->labels = NULL;
    }

    if (stp->fun_refs != NULL) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *)stp->fun_refs);
        stp->fun_refs = NULL;
    }

    if (stp->import_patches != NULL) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void*)stp->import_patches);
        stp->import_patches = NULL;
    }

    if (stp->bif_imports != NULL) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void*)stp->bif_imports);
        stp->bif_imports = NULL;
    }

    while (stp->lambda_patches) {
        LambdaPatch* next = stp->lambda_patches->next;
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->lambda_patches);
        stp->lambda_patches = next;
    }

    while (stp->literal_patches) {
        LiteralPatch* next = stp->literal_patches->next;
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->literal_patches);
        stp->literal_patches = next;
    }

    while (stp->string_patches) {
        StringPatch* next = stp->string_patches->next;
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->string_patches);
        stp->string_patches = next;
    }

    if (stp->line_instr) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->line_instr);
        stp->line_instr = NULL;
    }

    if (stp->func_line) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->func_line);
        stp->func_line = NULL;
    }

    return 1;
}

int beam_load_finish_emit(LoaderState *stp) {
    BeamCodeHeader* code_hdr = stp->code_hdr;
    BeamInstr* codev = (BeamInstr*) &stp->code_hdr->functions;
    int i;
    byte* str_table;
    unsigned strtab_size = stp->beam.strings.size;
    unsigned attr_size = stp->beam.attributes.size;
    unsigned compile_size = stp->beam.compile_info.size;
    Uint size;
    Sint decoded_size;
    Uint line_size;

    /*
     * Calculate the final size of the code.
     */
    if (stp->line_instr == 0) {
        line_size = 0;
    } else {
        line_size = offsetof(BeamCodeLineTab,func_tab);

        /* func_tab */
        line_size += (stp->beam.code.function_count + 1) * sizeof(BeamInstr**);

        /* line items */
        line_size += (stp->current_li + 1) * sizeof(BeamInstr*);

        /* name table, may be empty */
        line_size += stp->beam.lines.name_count * sizeof(Eterm);

        /* location table */
        line_size += (stp->current_li + 1) * stp->beam.lines.location_size;
    }
    size = offsetof(BeamCodeHeader,functions) + (stp->ci * sizeof(BeamInstr)) +
        strtab_size + attr_size + compile_size + MD5_SIZE + line_size;

    /* Move the code to its final location. */
    code_hdr = (BeamCodeHeader*)erts_realloc(ERTS_ALC_T_CODE, (void *) code_hdr, size);
    codev = (BeamInstr*)&code_hdr->functions;
    stp->code_hdr = code_hdr;
    stp->codev = codev;
    CHKBLK(ERTS_ALC_T_CODE, code_hdr);

    /*
     * Place a pointer to the op_int_code_end instruction in the
     * function table in the beginning of the file.
     */

    code_hdr->functions[stp->beam.code.function_count] = (ErtsCodeInfo*)(codev + stp->ci - 1);
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Store the pointer to the on_load function.
     */

    if (stp->on_load) {
        code_hdr->on_load = erts_code_to_codeinfo(codev + stp->on_load);
    } else {
        code_hdr->on_load = NULL;
    }
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Place the literals in their own allocated heap (for fast range check)
     * and fix up all instructions that refer to it.
     */
    {
        Eterm* ptr;
        LiteralPatch* lp;
        ErlOffHeap code_off_heap;
        ErtsLiteralArea *literal_area;
        Uint tot_lit_size;
        Uint lit_asize;

        tot_lit_size =
            stp->beam.static_literals.heap_size +
            stp->beam.dynamic_literals.heap_size;

        ERTS_INIT_OFF_HEAP(&code_off_heap);

        lit_asize = ERTS_LITERAL_AREA_ALLOC_SIZE(tot_lit_size);
        literal_area = erts_alloc(ERTS_ALC_T_LITERAL, lit_asize);
        ptr = &literal_area->start[0];
        literal_area->end = ptr + tot_lit_size;

        beamfile_move_literals(&stp->beam, &ptr, &code_off_heap);

        literal_area->off_heap = code_off_heap.first;
        lp = stp->literal_patches;
        while (lp != 0) {
            BeamInstr* op_ptr;
            op_ptr = codev + lp->pos;
            op_ptr[0] = beamfile_get_literal(&stp->beam, op_ptr[0]);
            lp = lp->next;
        }
        code_hdr->literal_area = literal_area;
    }

    /*
     * If there is line information, place it here.
     */
    if (stp->line_instr == 0) {
        code_hdr->line_table = NULL;
        str_table = (byte *) (codev + stp->ci);
    } else {
        BeamCodeLineTab* const line_tab = (BeamCodeLineTab *) (codev+stp->ci);
        const unsigned int ftab_size = stp->beam.code.function_count;
        const unsigned int num_instrs = stp->current_li;
        const unsigned int num_names = stp->beam.lines.name_count;
        const void** const line_items =
            (const void**) &line_tab->func_tab[ftab_size + 1];
        const void *locp_base;

        code_hdr->line_table = line_tab;

        for (i = 0; i < ftab_size; i++) {
            line_tab->func_tab[i] = line_items + stp->func_line[i];
        }
        line_tab->func_tab[i] = line_items + num_instrs;

        for (i = 0; i < num_instrs; i++) {
            line_items[i] = codev + stp->line_instr[i].pos;
        }
        line_items[i] = codev + stp->ci - 1;

        line_tab->fname_ptr = (Eterm*)&line_items[i + 1];
        for (i = 0; i < num_names; i++) {
            Eterm *fname = (Eterm*)&line_tab->fname_ptr[i];
            *fname = beamfile_get_literal(&stp->beam, stp->beam.lines.names[i]);
        }

        locp_base = &line_tab->fname_ptr[stp->beam.lines.name_count];
        line_tab->loc_size = stp->beam.lines.location_size;

        if (stp->beam.lines.location_size == sizeof(Uint16)) {
            Uint16* locp = (Uint16*)locp_base;
            line_tab->loc_tab.p2 = locp;

            for (i = 0; i < num_instrs; i++) {
                BeamFile_LineEntry *entry;
                int idx;

                idx = stp->line_instr[i].loc;
                entry = &stp->beam.lines.items[idx];
                *locp++ = MAKE_LOCATION(entry->name_index, entry->location);
            }

            *locp++ = LINE_INVALID_LOCATION;
            str_table = (byte *) locp;
        } else {
            Uint32* locp = (Uint32*)locp_base;
            line_tab->loc_tab.p4 = locp;

            ASSERT(stp->beam.lines.location_size == sizeof(Uint32));

            for (i = 0; i < num_instrs; i++) {
                BeamFile_LineEntry *entry;
                int idx;

                idx = stp->line_instr[i].loc;
                entry = &stp->beam.lines.items[idx];
                *locp++ = MAKE_LOCATION(entry->name_index, entry->location);
            }

            *locp++ = LINE_INVALID_LOCATION;
            str_table = (byte *) locp;
        }
        CHKBLK(ERTS_ALC_T_CODE,code);
    }

    /*
     * Place the string table and, optionally, attributes here.
     */
    sys_memcpy(str_table, stp->beam.strings.data, strtab_size);
    CHKBLK(ERTS_ALC_T_CODE,code);
    if (attr_size) {
        byte* attr = str_table + strtab_size;
        sys_memcpy(attr, stp->beam.attributes.data, stp->beam.attributes.size);
        code_hdr->attr_ptr = attr;
        code_hdr->attr_size = (BeamInstr) stp->beam.attributes.size;
        decoded_size = erts_decode_ext_size(attr, attr_size);
        if (decoded_size < 0) {
             BeamLoadError0(stp,
                            "bad external term representation of module "
                            "attributes");
         }
        code_hdr->attr_size_on_heap = decoded_size;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);
    if (compile_size) {
        byte* compile_info = str_table + strtab_size + attr_size;
        CHKBLK(ERTS_ALC_T_CODE,code);
        sys_memcpy(compile_info, stp->beam.compile_info.data,
               stp->beam.compile_info.size);

        CHKBLK(ERTS_ALC_T_CODE,code);
        code_hdr->compile_ptr = compile_info;
        CHKBLK(ERTS_ALC_T_CODE,code);
        code_hdr->compile_size = (BeamInstr) stp->beam.compile_info.size;
        CHKBLK(ERTS_ALC_T_CODE,code);
        decoded_size = erts_decode_ext_size(compile_info, compile_size);
        CHKBLK(ERTS_ALC_T_CODE,code);
        if (decoded_size < 0) {
             BeamLoadError0(stp,
                            "bad external term representation of compilation "
                            "information");
         }
        CHKBLK(ERTS_ALC_T_CODE,code);
        code_hdr->compile_size_on_heap = decoded_size;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);
    {
        byte* md5_sum = str_table + strtab_size + attr_size + compile_size;
        CHKBLK(ERTS_ALC_T_CODE,code);
        sys_memcpy(md5_sum, stp->beam.checksum, MD5_SIZE);
        CHKBLK(ERTS_ALC_T_CODE,code);
        code_hdr->md5_ptr = md5_sum;
        CHKBLK(ERTS_ALC_T_CODE,code);
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * Make sure that we have not overflowed the allocated code space.
     */
    ASSERT(str_table + strtab_size + attr_size + compile_size + MD5_SIZE ==
           ((byte *) code_hdr) + size);

    /*
     * Patch all instructions that refer to the string table.
     */
    {
        StringPatch* sp = stp->string_patches;

        while (sp != 0) {
            BeamInstr* op_ptr;
            byte* strp;

            op_ptr = codev + sp->pos;
            strp = str_table + op_ptr[0];
            op_ptr[0] = (BeamInstr) strp;
            sp = sp->next;
        }
    }
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Resolve all labels.
     */

    for (i = 0; i < stp->beam.code.label_count; i++) {
        Uint patch;
        Uint value = stp->labels[i].value;

        if (value == 0 && stp->labels[i].num_patches != 0) {
            BeamLoadError1(stp, "label %d not resolved", i);
        }
        ASSERT(value < stp->ci);
        for (patch = 0; patch < stp->labels[i].num_patches; patch++) {
            LabelPatch* lp = &stp->labels[i].patches[patch];
            Uint pos = lp->pos;
            ASSERT(pos < stp->ci);
            if (pos < stp->beam.code.function_count) {
                /*
                 * This is the array of pointers to the beginning of
                 * each function. The pointers must remain absolute.
                 */
                codev[pos] = (BeamInstr) (codev + value);
            } else {
#if defined(DEBUG) && defined(BEAM_WIDE_MASK)
                Uint w;
#endif
                Sint32 rel = lp->offset + value;
                switch (lp->packed) {
                case 0:         /* Not packed */
                    ASSERT(codev[pos] == i);
                    codev[pos] = rel;
                    break;
#ifdef BEAM_WIDE_MASK
                case 1:         /* Least significant word. */
#ifdef DEBUG
                    w = codev[pos] & BEAM_WIDE_MASK;
                    /* Correct label in least significant word? */
                    ASSERT(w == i);
#endif
                    codev[pos] = (codev[pos] & ~BEAM_WIDE_MASK) |
                        (rel & BEAM_WIDE_MASK);
                    break;
                case 2:         /* Most significant word */
#ifdef DEBUG
                    w = (codev[pos] >> BEAM_WIDE_SHIFT) & BEAM_WIDE_MASK;
                    /* Correct label in most significant word? */
                    ASSERT(w == i);
#endif
                    codev[pos] = ((Uint)rel << BEAM_WIDE_SHIFT) |
                        (codev[pos] & BEAM_WIDE_MASK);
                    break;
#endif
                default:
                    ASSERT(0);
                }
            }
        }
    }
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Save the updated code size.
     */
    stp->loaded_size = size;

    CHKBLK(ERTS_ALC_T_CODE,code_hdr);
    return 1;

 load_error:
    return 0;
}

void beam_load_finalize_code(LoaderState* stp, struct erl_module_instance* inst_p)
{
    ErtsCodeIndex staging_ix;
    unsigned int i;
    int on_load = stp->on_load;
    unsigned catches;
    Uint index, size;
    BeamInstr* codev = stp->codev;

    size = stp->loaded_size;
    erts_total_code_size += size;

    inst_p->code_hdr = stp->code_hdr;
    inst_p->code_length = size;
    inst_p->writable_region = (void*)inst_p->code_hdr;
    inst_p->executable_region = inst_p->writable_region;

    staging_ix = erts_staging_code_ix();

    ERTS_LC_ASSERT(erts_initialized == 0 || erts_has_code_load_permission() ||
                   erts_thr_progress_is_blocking());

    /* Update ranges (used for finding a function from a PC value). */
    erts_update_ranges(inst_p->code_hdr, size);

    /*
     * Allocate catch indices and fix up all catch_yf instructions.
     */

    index = stp->catches;
    catches = BEAM_CATCHES_NIL;
    while (index != 0) {
        BeamInstr next = codev[index];
        BeamInstr* abs_addr;
        codev[index] = BeamOpCodeAddr(op_catch_yf);
        /* We must make the address of the label absolute again. */
        abs_addr = (BeamInstr *)codev + index + codev[index+2];
        catches = beam_catches_cons(abs_addr, catches, NULL);
        codev[index+2] = make_catch(catches);
        index = next;
    }
    inst_p->catches = catches;

    /*
     * Import functions and patch all callers.
     */
    for (i = 0; i < stp->beam.imports.count; i++) {
        BeamFile_ImportEntry *import;
        Export *export;
        Uint current;
        Uint next;

        import = &stp->beam.imports.entries[i];
        export = erts_export_put(import->module,
                                 import->function,
                                 import->arity);

        current = stp->import_patches[i];
        while (current != 0) {
            ASSERT(current < stp->ci);
            next = stp->codev[current];
            stp->codev[current] = (BeamInstr)export;
            current = next;
        }
    }

    /*
     * Patch all instructions that refer to the fun table.
     */
    if (stp->beam.lambdas.count) {
        BeamFile_LambdaTable *lambda_table;
        ErlFunEntry **fun_entries;
        LambdaPatch *lp;

        lambda_table = &stp->beam.lambdas;

        fun_entries = erts_alloc(ERTS_ALC_T_LOADER_TMP,
                                 sizeof(ErlFunEntry*) * lambda_table->count);

        for (int i = 0; i < lambda_table->count; i++) {
            BeamFile_LambdaEntry *lambda;
            ErlFunEntry *fun_entry;
            FunRef *fun_refp;
            Eterm fun_ref;

            lambda = &lambda_table->entries[i];

            fun_entry = erts_put_fun_entry2(stp->module,
                                            lambda->old_uniq,
                                            i,
                                            stp->beam.checksum,
                                            lambda->index,
                                            lambda->arity - lambda->num_free);
            fun_entries[i] = fun_entry;

            fun_ref = beamfile_get_literal(&stp->beam, stp->fun_refs[i]);

            /* If there are no free variables, the literal refers to an
             * ErlFunThing that needs to be fixed up before we process the
             * FunRef. */
            if (lambda->num_free == 0) {
                ErlFunThing *funp = (ErlFunThing*)boxed_val(fun_ref);
                ASSERT(funp->entry.fun == NULL);
                funp->entry.fun = fun_entry;
                fun_ref = funp->env[0];
            }

            /* Patch up the fun reference literal. */
            fun_refp = (FunRef*)boxed_val(fun_ref);
            fun_refp->entry = fun_entry;

            /* Bump the reference count: this could not be done when copying
             * the literal as we had no idea which entry it belonged to.
             *
             * We also need to parry an annoying wrinkle: when reloading a
             * module over itself, we inherit the old instance's fun entries,
             * and thus have to cancel the reference bump in
             * `erts_put_fun_entry2` to make fun purging work. */
            if (!erts_is_fun_loaded(fun_entry, staging_ix)) {
                erts_refc_inctest(&fun_entry->refc, 1);
            }

            erts_set_fun_code(fun_entry,
                              staging_ix,
                              stp->codev + stp->labels[lambda->label].value);
        }

        lp = stp->lambda_patches;
        while (lp != 0) {
            ErlFunEntry *entry;
            BeamInstr* op_ptr;

            op_ptr = codev + lp->pos;

            entry = fun_entries[op_ptr[0]];
            op_ptr[0] = (BeamInstr)entry;

            lp = lp->next;
        }

        erts_free(ERTS_ALC_T_LOADER_TMP, fun_entries);
    }

    /*
     * Export functions
     *
     * This is done last so the BIF stubs don't clobber patch chains used for
     * patches and imports.
     */
    for (i = 0; i < stp->beam.exports.count; i++) {
        BeamFile_ExportEntry *entry;
        BeamInstr* address;
        Export* ep;

        entry = &stp->beam.exports.entries[i];
        address = stp->codev + stp->labels[entry->label].value;

        ep = erts_export_put(stp->module,
                             entry->function,
                             entry->arity);

        /* Fill in BIF stubs with a proper call to said BIF. */
        if (ep->bif_number != -1) {
            BifEntry *entry;

            ASSERT(ep->bif_number >= 0 && ep->bif_number < BIF_SIZE);
            entry = &bif_table[ep->bif_number];

            address[0] = BeamOpCodeAddr(op_call_bif_W);
            address[1] = (BeamInstr)entry->f;
        }

        if (on_load) {
            /*
             * on_load: Don't make any of the exported functions
             * callable yet. Keep any function in the current
             * code callable.
             */
            ep->trampoline.not_loaded.deferred = (BeamInstr) address;
        } else {
            ep->dispatch.addresses[staging_ix] = address;
        }
    }

#ifdef DEBUG
    /* Ensure that we've loaded stubs for all BIFs in this module. */
    for (i = 0; i < BIF_SIZE; i++) {
        BifEntry *entry = &bif_table[i];

        if (stp->module == entry->module) {
            Export *ep = erts_export_put(entry->module,
                                         entry->name,
                                         entry->arity);
            const BeamInstr *addr =
                ep->dispatch.addresses[staging_ix];

            if (!ErtsInArea(addr, stp->codev, stp->ci * sizeof(BeamInstr))) {
                erts_exit(ERTS_ABORT_EXIT,
                          "Module %T doesn't export BIF %T/%i\n",
                          entry->module,
                          entry->name,
                          entry->arity);
            }
        }
    }
#endif

    /* Prevent code from being freed. */
    stp->code_hdr = NULL;
}

static void init_label(Label* lp)
{
    lp->value = 0;
    lp->looprec_targeted = 0;
    lp->num_patches = 0;
    lp->num_allocated = 4;
    lp->patches = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                             lp->num_allocated * sizeof(LabelPatch));
}

int
beam_load_new_label(LoaderState* stp)
{
    unsigned int num = stp->beam.code.label_count;

    stp->beam.code.label_count++;
    stp->labels = (Label *) erts_realloc(ERTS_ALC_T_PREPARED_CODE,
                                         (void *) stp->labels,
                                         stp->beam.code.label_count * sizeof(Label));
    init_label(&stp->labels[num]);
    return num;
}

static void
register_label_patch(LoaderState* stp, Uint label, Uint ci, Uint offset)
{
    Label* lp;

    ASSERT(label < stp->beam.code.label_count);
    lp = &stp->labels[label];
    if (lp->num_allocated <= lp->num_patches) {
        lp->num_allocated *= 2;
        lp->patches = erts_realloc(ERTS_ALC_T_PREPARED_CODE,
                                   (void *) lp->patches,
                                   lp->num_allocated * sizeof(LabelPatch));
    }
    lp->patches[lp->num_patches].pos = ci;
    lp->patches[lp->num_patches].offset = offset;
    lp->patches[lp->num_patches].packed = 0;
    lp->num_patches++;
    stp->codev[ci] = label;
}

static void
new_lambda_patch(LoaderState* stp, int pos) {
    LambdaPatch* p = erts_alloc(ERTS_ALC_T_PREPARED_CODE, sizeof(LambdaPatch));
    p->pos = pos;
    p->next = stp->lambda_patches;
    stp->lambda_patches = p;
}

static void
new_literal_patch(LoaderState* stp, int pos)
{
    LiteralPatch* p = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                 sizeof(LiteralPatch));
    p->pos = pos;
    p->next = stp->literal_patches;
    stp->literal_patches = p;
}

static void
new_string_patch(LoaderState* stp, int pos)
{
    StringPatch* p = erts_alloc(ERTS_ALC_T_PREPARED_CODE, sizeof(StringPatch));
    p->pos = pos;
    p->next = stp->string_patches;
    stp->string_patches = p;
}

int beam_load_emit_op(LoaderState *stp, BeamOp *tmp_op) {
    /* The size of the loaded func_info instruction is needed by both the nif
     * functionality and line instructions. */
    enum {
        FUNC_INFO_SZ = sizeof(ErtsCodeInfo) / sizeof(Eterm)
    };

    const char *sign;
    int arg;

    Uint last_instr_start;
    int codev_size, ci;
    BeamInstr *code;

#if defined(BEAM_WIDE_SHIFT)
    int num_trailing_f;     /* Number of extra 'f' arguments in a list */
#endif

    codev_size = stp->codev_size;
    code = stp->codev;
    ci = stp->ci;

    ASSERT(ci <= codev_size);

    CodeNeed(opc[stp->specific_op].sz + 16); /* Extra margin for packing */
    last_instr_start = ci + opc[stp->specific_op].adjust;

    /* Load the found specific operation. */
    code[ci++] = BeamOpCodeAddr(stp->specific_op);
    sign = opc[stp->specific_op].sign;

    ASSERT(sign != NULL);
    arg = 0;
    while (*sign) {
        Uint tag;

        ASSERT(arg < stp->genop->arity);
        tag = stp->genop->a[arg].type;
        switch (*sign) {
        case 'r':        /* x(0) */
        case 'n':        /* Nil */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            break;
        case 'x':        /* x(N) */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            code[ci++] = (tmp_op->a[arg].val & REG_MASK) * sizeof(Eterm);
            break;
        case 'y':        /* y(N) */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            code[ci++] = ((tmp_op->a[arg].val & REG_MASK) + CP_SIZE) * sizeof(Eterm);
            break;
        case 'a':                /* Tagged atom */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            code[ci++] = tmp_op->a[arg].val;
            break;
        case 'c':                /* Tagged constant */
            switch (tag) {
            case TAG_i:
                code[ci++] = (BeamInstr) make_small((Uint) tmp_op->a[arg].val);
                break;
            case TAG_a:
                code[ci++] = tmp_op->a[arg].val;
                break;
            case TAG_n:
                code[ci++] = NIL;
                break;
            case TAG_q:
                new_literal_patch(stp, ci);
                code[ci++] = tmp_op->a[arg].val;
                break;
            default:
                BeamLoadError1(stp, "bad tag %d for tagged constant",
                               tmp_op->a[arg].type);
                break;
            }
            break;
        case 's':        /* Any source (tagged constant or register) */
            switch (tag) {
            case TAG_x:
                code[ci++] = make_loader_x_reg(tmp_op->a[arg].val & REG_MASK);
                break;
            case TAG_y:
                code[ci++] = make_loader_y_reg((tmp_op->a[arg].val & REG_MASK) + CP_SIZE);
                break;
            case TAG_i:
                code[ci++] = (BeamInstr) make_small((Uint)tmp_op->a[arg].val);
                break;
            case TAG_n:
                ASSERT(tmp_op->a[arg].val == NIL);
                /* ! Fall through ! */
            case TAG_a:
                code[ci++] = tmp_op->a[arg].val;
                break;
            case TAG_q:
                {
                    BeamInstr val = tmp_op->a[arg].val;
                    Eterm term = beamfile_get_literal(&stp->beam, val);
                    new_literal_patch(stp, ci);
                    code[ci++] = val;
                    switch (loader_tag(term)) {
                    case LOADER_X_REG:
                    case LOADER_Y_REG:
                        BeamLoadError1(stp, "the term '%T' would be confused "
                                       "with a register", term);
                    }
                }
                break;
            default:
                BeamLoadError1(stp, "bad tag %d for general source",
                               tmp_op->a[arg].type);
                break;
            }
            break;
        case 'd':        /* Destination (x(N), y(N) */
        case 'S':   /* Source (x(N), y(N)) */
            switch (tag) {
            case TAG_x:
                code[ci++] = (tmp_op->a[arg].val & REG_MASK) * sizeof(Eterm);
                break;
            case TAG_y:
                code[ci++] = ((tmp_op->a[arg].val & REG_MASK) + CP_SIZE) * sizeof(Eterm) + 1;
                break;
            default:
                BeamLoadError1(stp, "bad tag %d for destination",
                               tmp_op->a[arg].type);
                break;
            }
            break;
        case 't':        /* Small untagged integer (16 bits) -- can be packed. */
        case 'I':        /* Untagged integer (32 bits) -- can be packed.  */
        case 'W':        /* Untagged integer or pointer (machine word). */
#ifdef DEBUG
        switch (*sign) {
        case 't':
            /* 't'-typed values must fit in 16 bits. */
            ASSERT((tmp_op->a[arg].val >> 16) == 0);
            break;
#ifdef ARCH_64
        case 'I':
        case 'V':
            /* 'I'- and 'V'-typed values must fit in 32 bits. */
            ASSERT((tmp_op->a[arg].val >> 32) == 0);
            break;
#endif
        }
#endif
            BeamLoadVerifyTag(stp, tag, TAG_u);
            code[ci++] = tmp_op->a[arg].val;
            break;
        case 'A':        /* Arity value. */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            code[ci++] = make_arityval_unchecked(tmp_op->a[arg].val);
            break;
        case 'f':                /* Destination label */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            register_label_patch(stp, tmp_op->a[arg].val, ci,
                                 -last_instr_start);
            ci++;
            break;
        case 'j':                /* 'f' or 'p' */
            if (tag == TAG_p) {
                code[ci] = 0;
            } else if (tag == TAG_f) {
                register_label_patch(stp, tmp_op->a[arg].val, ci,
                                     -last_instr_start);
            } else {
                BeamLoadError3(stp, "bad tag %d; expected %d or %d",
                               tag, TAG_f, TAG_p);
            }
            ci++;
            break;
        case 'L':                /* Define label */
            ci--;                /* Remove label from loaded code */
            ASSERT(stp->specific_op == op_label_L);
            BeamLoadVerifyTag(stp, tag, TAG_u);
            stp->last_label = tmp_op->a[arg].val;
            if (stp->last_label < 0 ||
                stp->last_label >= stp->beam.code.label_count) {
                BeamLoadError2(stp, "invalid label num %u (0 < label < %u)",
                               stp->last_label, stp->beam.code.label_count);
            }
            if (stp->labels[stp->last_label].value != 0) {
                BeamLoadError1(stp, "label %d defined more than once",
                               stp->last_label);
            }
            stp->labels[stp->last_label].value = ci;
            break;
        case 'e':                /* Export entry */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            if (tmp_op->a[arg].val >= stp->beam.imports.count) {
                BeamLoadError1(stp, "invalid import table index %d",
                               tmp_op->a[arg].val);
            } else {
                code[ci] = stp->import_patches[tmp_op->a[arg].val];
                stp->import_patches[tmp_op->a[arg].val] = ci;

                ci++;
            }

            break;
        case 'b':
            {
                int i = tmp_op->a[arg].val;
                BeamLoadVerifyTag(stp, tag, TAG_u);
                if (i >= stp->beam.imports.count) {
                    BeamLoadError1(stp, "invalid import table index %d", i);
                } else if (stp->bif_imports[i] == NULL) {
                    BeamLoadError1(stp, "import %d not a BIF", i);
                } else {
                    code[ci++] = (BeamInstr)stp->bif_imports[i]->f;
                }
            }
            break;
        case 'P':                /* Byte offset into tuple or stack */
        case 'Q':                /* Like 'P', but packable */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            code[ci++] = (BeamInstr) ((tmp_op->a[arg].val+1) * sizeof(Eterm));
            break;
        case 'l':                /* Floating point register. */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            code[ci++] = tmp_op->a[arg].val * sizeof(FloatDef);
            break;
        case 'q':                /* Literal */
            new_literal_patch(stp, ci);
            code[ci++] = tmp_op->a[arg].val;
            break;
        case 'F':                /* Fun entry */
            new_lambda_patch(stp, ci);
            code[ci++] = tmp_op->a[arg].val;
            break;
        default:
            BeamLoadError1(stp, "bad argument tag: %d", *sign);
        }
        sign++;
        arg++;
    }

    /* The packing engine. */
    if (opc[stp->specific_op].pack[0]) {
        char* prog;            /* Program for packing engine. */
        struct pack_stack {
            BeamInstr instr;
            Uint* patch_pos;
        } stack[8];            /* Stack. */
        struct pack_stack* sp = stack; /* Points to next free position. */
        BeamInstr packed = 0; /* Accumulator for packed operations. */
        LabelPatch* packed_label = 0;

        for (prog = opc[stp->specific_op].pack; *prog; prog++) {
            switch (*prog) {
            case 'g':        /* Get operand and push on stack. */
                ci--;
                sp->instr = code[ci];
                sp->patch_pos = 0;
                sp++;
                break;
            case 'f':        /* Get possible 'f' operand and push on stack. */
                {
                    Uint w = code[--ci];
                    sp->instr = w;
                    sp->patch_pos = 0;

                    if (w != 0) {
                        LabelPatch* lbl_p;
                        int num_patches;
                        int patch;

                        ASSERT(w < stp->beam.code.label_count);
                        lbl_p = stp->labels[w].patches;
                        num_patches = stp->labels[w].num_patches;
                        for (patch = num_patches - 1; patch >= 0; patch--) {
                            if (lbl_p[patch].pos == ci) {
                                sp->patch_pos = &lbl_p[patch].pos;
                                break;
                            }
                        }
                        ASSERT(sp->patch_pos);
                    }
                    sp++;
                }
                break;
            case 'q':        /* Get possible 'q' operand and push on stack. */
                {
                    LiteralPatch* lp;

                    ci--;
                    sp->instr = code[ci];
                    sp->patch_pos = 0;

                    for (lp = stp->literal_patches;
                            lp && lp->pos > ci - ERTS_BEAM_MAX_OPARGS;
                            lp = lp->next) {
                        if (lp->pos == ci) {
                            sp->patch_pos = &lp->pos;
                            break;
                        }
                    }
                    sp++;
                }
                break;
#ifdef ARCH_64
            case '1':        /* Tightest shift (always 10 bits) */
                ci--;
                ASSERT((code[ci] & ~0x1FF8ull) == 0); /* Fits in 10 bits */
                packed = (packed << BEAM_TIGHTEST_SHIFT);
                packed |= code[ci] >> 3;
                if (packed_label) {
                    packed_label->packed++;
                }
                break;
#endif
            case '2':        /* Tight shift (10 or 16 bits) */
                packed = (packed << BEAM_TIGHT_SHIFT) | code[--ci];
                if (packed_label) {
                    packed_label->packed++;
                }
                break;
            case '3':        /* Loose shift (16 bits) */
                packed = (packed << BEAM_LOOSE_SHIFT) | code[--ci];
                if (packed_label) {
                    packed_label->packed++;
                }
                break;
#ifdef ARCH_64
            case '4':        /* Wide shift (32 bits) */
                {
                    Uint w = code[--ci];

                    if (packed_label) {
                        packed_label->packed++;
                    }

                    /*
		     * 'w' can handle both labels ('f' and 'j'), as well
		     * as 'I'. Test whether this is a label.
		     */

                    if (w < stp->beam.code.label_count) {
                        /*
			 * Probably a label. Look for patch pointing to this
			 * position.
			 */
                        LabelPatch* lp = stp->labels[w].patches;
                        int num_patches = stp->labels[w].num_patches;
                        int patch;
                        for (patch = num_patches - 1; patch >= 0; patch--) {
                            if (lp[patch].pos == ci) {
                                lp[patch].packed = 1;
                                packed_label = &lp[patch];
                                break;
                            }
                        }
                    }
                    packed = (packed << BEAM_WIDE_SHIFT) |
                        (code[ci] & BEAM_WIDE_MASK);
                }
                break;
#endif
            case 'p':        /* Put instruction (from stack). */
                --sp;
                code[ci] = sp->instr;
                if (sp->patch_pos) {
                    *sp->patch_pos = ci;
                }
                ci++;
                break;
            case 'P':        /* Put packed operands (on the stack). */
                sp->instr = packed;
                sp->patch_pos = 0;
                if (packed_label) {
                    sp->patch_pos = &packed_label->pos;
                    packed_label = 0;
                }
                sp++;
                packed = 0;
                break;
#if defined(ARCH_64) && defined(CODE_MODEL_SMALL)
            case '#':       /* -1 */
            case '$':       /* -2 */
            case '%':       /* -3 */
            case '&':       /* -4 */
            case '\'':      /* -5 */
            case '(':       /* -6 */
                /* Pack accumulator contents into instruction word. */
                {
                    Sint pos = ci - (*prog - '#' + 1);
                    /* Are the high 32 bits of the instruction word zero? */
                    ASSERT((code[pos] & ~((1ull << BEAM_WIDE_SHIFT)-1)) == 0);
                    code[pos] |= packed << BEAM_WIDE_SHIFT;
                    if (packed_label) {
                        ASSERT(packed_label->packed == 1);
                        packed_label->pos = pos;
                        packed_label->packed = 2;
                        packed_label = 0;
                    }
                    packed >>= BEAM_WIDE_SHIFT;
                }
                break;
#endif
            default:
                erts_exit(ERTS_ERROR_EXIT, "beam_load: invalid packing op: %c\n", *prog);
            }
        }
        ASSERT(sp == stack); /* Incorrect program? */
    }

    /* Load any list arguments using the primitive tags. */

#if defined(BEAM_WIDE_SHIFT)
    num_trailing_f = 0;
#endif
    for ( ; arg < tmp_op->arity; arg++) {
#if defined(BEAM_WIDE_SHIFT)
        if (tmp_op->a[arg].type == TAG_f) {
            num_trailing_f++;
        } else {
            num_trailing_f = 0;
        }
#endif
        CodeNeed(1);
        switch (tmp_op->a[arg].type) {
        case TAG_i:
            code[ci++] = make_small(tmp_op->a[arg].val);
            break;
        case TAG_u:
        case TAG_a:
        case TAG_v:
            code[ci++] = tmp_op->a[arg].val;
            break;
        case TAG_f:
            register_label_patch(stp, tmp_op->a[arg].val, ci,
                                 -last_instr_start);
            ci++;
            break;
        case TAG_x:
            code[ci++] = make_loader_x_reg(tmp_op->a[arg].val & REG_MASK);
            break;
        case TAG_y:
            code[ci++] = make_loader_y_reg((tmp_op->a[arg].val & REG_MASK) + CP_SIZE);
            break;
        case TAG_n:
            code[ci++] = NIL;
            break;
        case TAG_q:
            new_literal_patch(stp, ci);
            code[ci++] = tmp_op->a[arg].val;
            break;
        default:
            BeamLoadError1(stp, "unsupported primitive type '%c'",
                           tag_to_letter[tmp_op->a[arg].type]);
        }
    }

    /* If all the extra arguments were 'f' operands, and the wordsize is 64
     * bits, pack two 'f' operands into each word. */

#if defined(BEAM_WIDE_SHIFT)
    if (num_trailing_f >= 1) {
        Uint src_index = ci - num_trailing_f;
        Uint src_limit = ci;
        Uint dst_limit = src_index + (num_trailing_f+1)/2;

        ci = src_index;
        while (ci < dst_limit) {
            Uint w[2];
            BeamInstr packed = 0;
            int wi;

            w[0] = code[src_index];
            if (src_index+1 < src_limit) {
                w[1] = code[src_index+1];
            } else {
                w[1] = 0;
            }
            for (wi = 0; wi < 2; wi++) {
                Uint lbl = w[wi];
                LabelPatch* lp = stp->labels[lbl].patches;
                int num_patches = stp->labels[lbl].num_patches;

#if defined(WORDS_BIGENDIAN)
                packed <<= BEAM_WIDE_SHIFT;
                packed |= lbl & BEAM_WIDE_MASK;
#else
                packed >>= BEAM_WIDE_SHIFT;
                packed |= lbl << BEAM_WIDE_SHIFT;
#endif
                while (num_patches-- > 0) {
                    if (lp->pos == src_index + wi) {
                        lp->pos = ci;
#if defined(WORDS_BIGENDIAN)
                        lp->packed = 2 - wi;
#else
                        lp->packed = wi + 1;
#endif
                        break;
                    }
                    lp++;
                }
            }
            code[ci++] = packed;
            src_index += 2;
        }
    }
#endif

    /*
     * Handle a few special cases.
     */
    switch (stp->specific_op) {
    case op_i_func_info_IaaI:
        {
            Sint offset;

            if (stp->function_number >= stp->beam.code.function_count) {
                BeamLoadError1(stp,
                               "too many functions in module (header said %u)",
                               stp->beam.code.function_count);
            }

            /*
	     * Save context for error messages.
	     */
            stp->function = code[ci-2];
            stp->arity = code[ci-1];

            /*
	     * Save current offset of into the line instruction array.
	     */
            if (stp->func_line) {
                stp->func_line[stp->function_number] = stp->current_li;
            }

            stp->last_func_start = ci;

            /* When this assert is triggered, it is normally a sign that the
             * size of the ops.tab i_func_info instruction is not the same as
             * FUNC_INFO_SZ.
	     */
            ASSERT(stp->labels[stp->last_label].value == ci - FUNC_INFO_SZ);
            offset = stp->function_number;
            register_label_patch(stp, stp->last_label, offset, 0);
            stp->function_number++;
            if (stp->arity > MAX_ARG) {
                BeamLoadError1(stp, "too many arguments: %d", stp->arity);
            }

#ifdef DEBUG
            {
                int i;
                 /* Should not be referenced. */
                ASSERT(stp->labels[0].num_patches == 0);
                for (i = 1; i < stp->beam.code.label_count; i++) {
                    ASSERT(stp->labels[i].num_patches <= stp->labels[i].num_allocated);
                }
            }
#endif
        }
        break;
    case op_nif_start:
        if (!stp->code_hdr->are_nifs) {
            int bytes = stp->beam.code.function_count * sizeof(byte);
            stp->code_hdr->are_nifs = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                                 bytes);
            sys_memzero(stp->code_hdr->are_nifs, bytes);
        }
        ASSERT(stp->function_number > 0);
        ASSERT(stp->function_number <= stp->beam.code.function_count);
        stp->code_hdr->are_nifs[stp->function_number-1] = 1;

        ci -= 1;         /* Get rid of the instruction */
        break;
    case op_i_nif_padding:
        {
            /* Native function calls may be larger than their stubs, so we'll
             * need to make sure any potentially-native function stub is padded
             * with enough room. */
            Sint pad;

            ci -= 1;         /* Get rid of the instruction */

            ASSERT(stp->last_func_start);
            pad = BEAM_NATIVE_MIN_FUNC_SZ - (ci - stp->last_func_start);

            if (pad > 0) {
                ASSERT(pad < BEAM_NATIVE_MIN_FUNC_SZ);
                CodeNeed(pad);

                while (pad-- > 0) {
                /* Filling with actual instructions (instead of zeroes) will
                 * look nicer in a disassembly listing. */
                    code[ci++] = BeamOpCodeAddr(op_padding);
                }
            }
        }
        break;
    case op_on_load:
        ci--;                /* Get rid of the instruction */

        /* Remember offset for the on_load function. */
        stp->on_load = ci;
        break;
    case op_bs_put_string_WW:
    case op_i_bs_match_string_xfWW:
    case op_i_bs_match_string_yfWW:
        new_string_patch(stp, ci-1);
        break;
    case op_i_bs_create_bin_jIWdW:
        {
            int n = tmp_op->arity;
            BeamInstr* p = &code[ci-n];
            BeamInstr* end_p = &code[ci];
            while (p < end_p) {
                switch (*p) {
                case BSC_STRING:
                    new_string_patch(stp, p+3-code);
                    break;
                }
                p += 5;
            }
        }
        break;
    case op_catch_yf:
        /* code[ci-3]        &&lb_catch_yf
	 * code[ci-2]        y-register offset in E
	 * code[ci-1]        label; index tagged as CATCH at runtime
	 */
        code[ci-3] = stp->catches;
        stp->catches = ci-3;
        break;

    case op_line_I:
        if (stp->line_instr) {
            BeamInstr item = code[ci-1];
            unsigned int li;
            if (item >= stp->beam.lines.item_count) {
                BeamLoadError2(stp, "line instruction index overflow (%u/%u)",
                               item, stp->beam.lines.item_count);
            }
            li = stp->current_li;
            if (li >= stp->beam.lines.instruction_count) {
                BeamLoadError2(stp, "line instruction table overflow (%u/%u)",
                               li, stp->beam.lines.instruction_count);
            }

            if (ci - 2 == stp->last_func_start) {
                /*
		 * This line instruction directly follows the func_info
		 * instruction. Its address must be adjusted to point to
		 * func_info instruction.
		 */
                stp->line_instr[li].pos = stp->last_func_start - FUNC_INFO_SZ;
                stp->line_instr[li].loc = item;
                stp->current_li++;
            } else if (li <= stp->func_line[stp->function_number - 1] ||
		       stp->line_instr[li-1].loc != item) {
                /*
		 * Only store the location if it is different
		 * from the previous location in the same function.
		 */
                stp->line_instr[li].pos = ci - 2;
                stp->line_instr[li].loc = item;
                stp->current_li++;
            }
        }
        ci -= 2;                /* Get rid of the instruction */
        break;

        /* End of code found. */
    case op_int_code_end:
        if (stp->function_number != stp->beam.code.function_count) {
            BeamLoadError2(stp,
                           "too few functions (%u) in module (header said %u)",
                           stp->function_number,
                           stp->beam.code.function_count);
        }

        stp->function = THE_NON_VALUE;
        stp->genop = NULL;
        stp->specific_op = -1;
    }

    stp->codev_size = codev_size;
    stp->ci = ci;

    return 1;

load_error:
    return 0;
}

void beam_load_purge_aux(const BeamCodeHeader *hdr)
{
}
