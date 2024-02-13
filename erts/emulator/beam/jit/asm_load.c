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

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_binary.h"
#include "beam_catches.h"
#include "beam_load.h"
#include "erl_version.h"
#include "beam_bp.h"

#include "beam_asm.h"

#ifdef ADDRESS_SANITIZER
#    include <sanitizer/lsan_interface.h>
#endif

#define INVALID_LAMBDA_INDEX -1

static void init_label(Label *lp);

int beam_load_prepare_emit(LoaderState *stp) {
    BeamCodeHeader *hdr;
    int i;

    stp->ba = beamasm_new_assembler(stp->module,
                                    stp->beam.code.label_count,
                                    stp->beam.code.function_count,
                                    &stp->beam);

    /* Initialize code header */
    stp->codev_size = stp->beam.code.function_count + 1;
    hdr = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                     (offsetof(BeamCodeHeader, functions) +
                      sizeof(BeamInstr) * stp->codev_size));

    hdr->num_functions = stp->beam.code.function_count;

    stp->ci = hdr->num_functions + 1;

    hdr->attr_ptr = NULL;
    hdr->attr_size = 0;
    hdr->attr_size_on_heap = 0;
    hdr->compile_ptr = NULL;
    hdr->compile_size = 0;
    hdr->compile_size_on_heap = 0;
    hdr->literal_area = NULL;
    hdr->md5_ptr = NULL;
    hdr->are_nifs = NULL;

    stp->coverage = hdr->coverage = NULL;
    stp->line_coverage_valid = hdr->line_coverage_valid = NULL;

    hdr->line_coverage_len = 0;

    if (stp->beam.lines.flags & BEAMFILE_FORCE_LINE_COUNTERS) {
        hdr->coverage_mode = ERTS_COV_LINE_COUNTERS;
    } else if ((stp->beam.lines.flags & BEAMFILE_EXECUTABLE_LINE) == 0 &&
               (erts_coverage_mode == ERTS_COV_LINE ||
                erts_coverage_mode == ERTS_COV_LINE_COUNTERS)) {
        /* A line coverage mode is enabled, but there are no
         * executable_line instructions in this module; therefore,
         * turn off coverage for this module. */
        hdr->coverage_mode = ERTS_COV_NONE;
    } else {
        /* Use the system default coverage mode for this module. */
        hdr->coverage_mode = erts_coverage_mode;
    }

    switch (hdr->coverage_mode) {
    case ERTS_COV_FUNCTION:
    case ERTS_COV_FUNCTION_COUNTERS: {
        size_t alloc_size = hdr->num_functions;
        if (hdr->coverage_mode == ERTS_COV_FUNCTION_COUNTERS) {
            alloc_size *= sizeof(Uint);
        }
        stp->coverage = erts_alloc(ERTS_ALC_T_CODE_COVERAGE, alloc_size);
        sys_memset(stp->coverage, 0, alloc_size);
        break;
    }
    case ERTS_COV_LINE:
    case ERTS_COV_LINE_COUNTERS: {
        size_t alloc_size = stp->beam.lines.instruction_count;
        Uint coverage_size;

        if (hdr->coverage_mode == ERTS_COV_LINE) {
            coverage_size = sizeof(byte);
        } else {
            coverage_size = sizeof(Uint);
        }
        stp->coverage = erts_alloc(ERTS_ALC_T_CODE_COVERAGE,
                                   alloc_size * coverage_size);
        sys_memset(stp->coverage, 0, alloc_size * coverage_size);
        stp->line_coverage_valid =
                erts_alloc(ERTS_ALC_T_CODE_COVERAGE, alloc_size);
        sys_memset(stp->line_coverage_valid, 0, alloc_size);
        hdr->line_coverage_len = alloc_size;
        break;
    }
    }

    stp->load_hdr = hdr;

    stp->labels = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                             stp->beam.code.label_count * sizeof(Label));
    for (i = 0; i < stp->beam.code.label_count; i++) {
        init_label(&stp->labels[i]);
    }

    stp->fun_refs = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                               stp->beam.lambdas.count * sizeof(SWord));

    for (i = 0; i < stp->beam.lambdas.count; i++) {
        BeamFile_LambdaEntry *lambda = &stp->beam.lambdas.entries[i];

        if (stp->labels[lambda->label].lambda_index == INVALID_LAMBDA_INDEX) {
            stp->labels[lambda->label].lambda_index = i;
            stp->fun_refs[i] = ERTS_SWORD_MAX;
        } else {
            beam_load_report_error(__LINE__,
                                   stp,
                                   "lambda already defined for label %i. To "
                                   "fix this, please recompile this module "
                                   "with an OTP " ERLANG_OTP_RELEASE
                                   " compiler.",
                                   lambda->label);
            return 0;
        }
    }

    stp->bif_imports =
            erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                       stp->beam.imports.count * sizeof(BifEntry **));

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

static void init_label(Label *lp) {
    sys_memset(lp, 0, sizeof(*lp));
    lp->lambda_index = INVALID_LAMBDA_INDEX;
}

void beam_load_prepared_free(Binary *magic) {
    beam_load_prepared_dtor(magic);
    erts_bin_release(magic);
}

/* This destructor function can safely be called multiple times. */
int beam_load_prepared_dtor(Binary *magic) {
    LoaderState *stp = ERTS_MAGIC_BIN_DATA(magic);

    /* This should have been freed earlier! */
    ASSERT(stp->op_allocator.beamop_blocks == NULL);

    beamfile_free(&stp->beam);
    beamopallocator_dtor(&stp->op_allocator);

    if (stp->load_hdr) {
        BeamCodeHeader *hdr = stp->load_hdr;

        if (hdr->literal_area) {
            erts_release_literal_area(hdr->literal_area);
            hdr->literal_area = NULL;
        }
        if (hdr->are_nifs) {
            erts_free(ERTS_ALC_T_PREPARED_CODE, hdr->are_nifs);
            hdr->are_nifs = NULL;
        }
        if (hdr->coverage) {
            erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->coverage);
            hdr->coverage = NULL;
        }
        if (hdr->line_coverage_valid) {
            erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->line_coverage_valid);
            hdr->line_coverage_valid = NULL;
        }

        erts_free(ERTS_ALC_T_PREPARED_CODE, hdr);
        stp->load_hdr = NULL;
    }

    if (stp->labels) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *)stp->labels);
        stp->labels = NULL;
    }

    if (stp->fun_refs) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, (void *)stp->fun_refs);
        stp->fun_refs = NULL;
    }

    if (stp->bif_imports) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->bif_imports);
        stp->bif_imports = NULL;
    }

    if (stp->line_instr) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->line_instr);
        stp->line_instr = NULL;
    }

    if (stp->func_line) {
        erts_free(ERTS_ALC_T_PREPARED_CODE, stp->func_line);
        stp->func_line = NULL;
    }

    if (stp->coverage) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, stp->coverage);
        stp->coverage = NULL;
    }

    if (stp->line_coverage_valid) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, stp->line_coverage_valid);
        stp->line_coverage_valid = NULL;
    }

    if (stp->ba) {
        beamasm_delete_assembler(stp->ba);
        stp->ba = NULL;
    }

    if (stp->executable_region) {
        ASSERT(stp->writable_region != NULL);

        beamasm_purge_module(stp->executable_region,
                             stp->writable_region,
                             stp->loaded_size);

        stp->executable_region = NULL;
        stp->writable_region = NULL;
    }

    return 1;
}

static int add_line_entry(LoaderState *stp,
                          BeamInstr item,
                          int insert_duplicates) {
    int is_duplicate;
    unsigned int li;

    if (!stp->beam.lines.item_count) {
        return 0;
    }

    if (item >= stp->beam.lines.item_count) {
        BeamLoadError2(stp,
                       "line instruction index overflow (%u/%u)",
                       item,
                       stp->beam.lines.item_count);
    }

    li = stp->current_li;
    if (li >= stp->beam.lines.instruction_count) {
        BeamLoadError2(stp,
                       "line instruction table overflow (%u/%u)",
                       li,
                       stp->beam.lines.instruction_count);
    }

    is_duplicate = li && (stp->line_instr[li - 1].loc == item);

    if (insert_duplicates || !is_duplicate) {
        stp->line_instr[li].pos = beamasm_get_offset(stp->ba);
        stp->line_instr[li].loc = item;
        stp->current_li++;
    }

    return 0;

load_error:
    return -1;
}

int beam_load_emit_op(LoaderState *stp, BeamOp *tmp_op) {
    const char *sign;
    int arg;

    /*
     * Verify and massage the operands for the specific instruction.
     *
     * After the massaging, TAG_i denotes a tagged immediate value,
     * either NIL, an atom, or a small integer. The tags TAG_n and
     * TAG_a are no longer used. TAG_f can either be a non-zero label
     * number (in guards) or 0 (in function bodies) to indicate that
     * an exception should be generated on failure. TAG_j is no longer
     * used.
     */
    sign = opc[stp->specific_op].sign;
    arg = 0;
    ASSERT(sign != NULL);
    while (*sign) {
        Uint tag;
        BeamOpArg *curr = &stp->genop->a[arg];

        ASSERT(arg < stp->genop->arity);
        tag = curr->type;

        switch (*sign) {
        case 'n': /* Nil */
            ASSERT(tag != TAG_r);
            curr->type = 'I';
            curr->val = NIL;
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            break;
        case 'x': /* x(N) */
        case 'y': /* y(N) */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            break;
        case 'a': /* Tagged atom */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            curr->type = 'I';
            break;
        case 'c': /* Tagged constant */
            switch (tag) {
            case TAG_i:
                curr->val = make_small((Uint)curr->val);
                curr->type = 'I';
                break;
            case TAG_a:
                curr->type = 'I';
                break;
            case TAG_n:
                curr->val = NIL;
                curr->type = 'I';
                break;
            case TAG_q:
                break;
            default:
                BeamLoadError1(stp,
                               "bad tag %d for tagged constant",
                               curr->type);
                break;
            }
            break;
        case 's':
            /* Any source (tagged constant or register) */
            switch (tag) {
            case TAG_x:
                break;
            case TAG_y:
                break;
            case TAG_i:
                curr->val = (BeamInstr)make_small(curr->val);
                curr->type = 'I';
                break;
            case TAG_a:
                curr->type = 'I';
                break;
            case TAG_n:
                curr->type = 'I';
                curr->val = NIL;
                break;
            case TAG_q: {
                Eterm term = beamfile_get_literal(&stp->beam, curr->val);
                switch (loader_tag(term)) {
                case LOADER_X_REG:
                case LOADER_Y_REG:
                    BeamLoadError1(stp,
                                   "the term '%T' would be confused "
                                   "with a register",
                                   term);
                }
            } break;
            default:
                BeamLoadError1(stp,
                               "bad tag %d for general source",
                               curr->type);
                break;
            }
            break;
        case 'd': /* Destination (x(N), y(N) */
        case 'S': /* Source (x(N), y(N)) */
            switch (tag) {
            case TAG_x:
                break;
            case TAG_y:
                break;
            default:
                BeamLoadError1(stp, "bad tag %d for destination", curr->type);
                break;
            }
            break;
        case 't': /* Small untagged integer (16 bits) -- can be packed. */
        case 'I': /* Untagged integer (32 bits) -- can be packed.  */
        case 'W': /* Untagged integer or pointer (machine word). */
#ifdef DEBUG
            switch (*sign) {
            case 't':
                /* 't'-typed values must fit in 16 bits. */
                ASSERT((curr->val >> 16) == 0);
                break;
#    ifdef ARCH_64
            case 'I':
            case 'V':
                /* 'I'- and 'V'-typed values must fit in 32 bits. */
                ASSERT((curr->val >> 32) == 0);
                break;
#    endif
            }
#endif
            BeamLoadVerifyTag(stp, tag, TAG_u);
            break;
        case 'A': /* Arity value. */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            curr->val = curr->val == 0 ? make_arityval_zero()
                                       : make_arityval(curr->val);
            break;
        case 'f': /* Destination label */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            break;
        case 'j': /* 'f' or 'p' */
            switch (tag) {
            case TAG_p:
                curr->type = TAG_f;
                curr->val = 0;
                break;
            case TAG_f:
                break;
            default:
                BeamLoadError3(stp,
                               "bad tag %d; expected %d or %d",
                               tag,
                               TAG_f,
                               TAG_p);
            }

            break;
        case 'L': /* Define label */
            ASSERT(stp->specific_op == op_label_L ||
                   stp->specific_op == op_aligned_label_Lt);
            BeamLoadVerifyTag(stp, tag, TAG_u);
            stp->last_label = curr->val;
            if (stp->last_label < 0 ||
                stp->last_label >= stp->beam.code.label_count) {
                BeamLoadError2(stp,
                               "invalid label num %u (0 < label < %u)",
                               curr->val,
                               stp->beam.code.label_count);
            }
            if (stp->labels[stp->last_label].value != 0) {
                BeamLoadError1(stp,
                               "label %d defined more than once",
                               stp->last_label);
            }
            stp->labels[stp->last_label].value = 1;
            curr->type = TAG_f;
            break;
        case 'e': /* Export entry */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            if (curr->val >= stp->beam.imports.count) {
                BeamLoadError1(stp, "invalid import table index %d", curr->val);
            }
            curr->type = 'E';
            break;
        case 'b': {
            int i = tmp_op->a[arg].val;
            BeamLoadVerifyTag(stp, tag, TAG_u);
            if (i >= stp->beam.imports.count) {
                BeamLoadError1(stp, "invalid import table index %d", i);
            } else if (stp->bif_imports[i] == NULL) {
                BeamLoadError1(stp, "import %d not a BIF", i);
            } else {
                curr->val = (BeamInstr)stp->bif_imports[i]->f;
            }
        } break;
        case 'P': /* Byte offset into tuple */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            curr->val = (BeamInstr)((curr->val + 1) * sizeof(Eterm));
            break;
        case 'l': /* Floating point register. */
            BeamLoadVerifyTag(stp, tag_to_letter[tag], *sign);
            curr->val = curr->val * sizeof(FloatDef);
            break;
        case 'q': /* Literal */
            BeamLoadVerifyTag(stp, tag, TAG_q);
            break;
        case 'F': /* Fun entry */
            BeamLoadVerifyTag(stp, tag, TAG_u);
            curr->type = 'F';
            break;
        case 'H': /* Exception handler */
            BeamLoadVerifyTag(stp, tag, TAG_f);
            curr->type = 'H';
            break;
        case 'M':
            curr->type = 'M';
            break;
        case 'i':
            curr->type = 'I';
            break;
        default:
            BeamLoadError1(stp, "bad argument tag: %d", *sign);
        }

        /* These types must have been translated to 'I' */
        ASSERT(curr->type != TAG_i && curr->type != TAG_n &&
               curr->type != TAG_a && curr->type != TAG_v);

        sign++;
        arg++;
    }

    /*
     * Verify and massage any list arguments according to the primitive tags.
     *
     * 'I' will denote a tagged immediate value (NIL, small integer,
     * atom, or tuple arity). TAG_n, TAG_a, and TAG_v will no longer be used.
     */
    for (; arg < tmp_op->arity; arg++) {
        BeamOpArg *curr = &tmp_op->a[arg];

        switch (tmp_op->a[arg].type) {
        case TAG_i:
            curr->val = make_small(tmp_op->a[arg].val);
            curr->type = 'I';
            break;
        case TAG_n:
            curr->val = NIL;
            curr->type = 'I';
            break;
        case TAG_a:
        case TAG_v:
            curr->type = 'I';
            break;
        case TAG_u:
        case TAG_f:
        case TAG_x:
        case TAG_y:
        case TAG_q:
            break;
        default:
            BeamLoadError1(stp,
                           "unsupported primitive type '%c'",
                           tag_to_letter[tmp_op->a[arg].type]);
        }
    }

    /* Handle a few special cases. */
    switch (stp->specific_op) {
    case op_i_func_info_IaaI:
        if (stp->function_number >= stp->beam.code.function_count) {
            BeamLoadError1(stp,
                           "too many functions in module (header said %u)",
                           stp->beam.code.function_count);
        }

        stp->function_number++;

        /* Save context for error messages. */
        stp->function = tmp_op->a[2].val;
        stp->arity = tmp_op->a[3].val;

        if (stp->arity > MAX_ARG) {
            BeamLoadError1(stp, "too many arguments: %d", stp->arity);
        }

        break;
    case op_empty_func_line:
    case op_func_line_I:
        /* This is the first line instruction of a function, preceding
         * the func_info instruction. */
        if (stp->func_line) {
            stp->func_line[stp->function_number] = stp->current_li;
        }

        break;
    case op_nif_start:
        if (!stp->load_hdr->are_nifs) {
            int bytes = stp->beam.code.function_count * sizeof(byte);
            stp->load_hdr->are_nifs =
                    erts_alloc(ERTS_ALC_T_PREPARED_CODE, bytes);
            sys_memzero(stp->load_hdr->are_nifs, bytes);
        }
        ASSERT(stp->function_number > 0);
        ASSERT(stp->function_number <= stp->beam.code.function_count);
        stp->load_hdr->are_nifs[stp->function_number - 1] = 1;
        break;
    }

    /* Generate assembly code for the specific instruction. */
    if (beamasm_emit(stp->ba, stp->specific_op, tmp_op) == 0) {
        BeamLoadError1(stp, "failed to emit asm for %d", stp->specific_op);
    }

    switch (stp->specific_op) {
    case op_func_line_I:
        /* Since this is the beginning of a new function, force insertion
         * of the line entry even if it happens to be a duplicate of the
         * previous one. */
        if (add_line_entry(stp, tmp_op->a[0].val, 1)) {
            goto load_error;
        }
        break;
    case op_line_I:
        /* We'll save some memory by not inserting a line entry that
         * is equal to the previous one. */
        if (add_line_entry(stp, tmp_op->a[0].val, 0)) {
            goto load_error;
        }
        break;
    case op_executable_line_I: {
        byte coverage_size = 0;

        /* We'll save some memory by not inserting a line entry that
         * is equal to the previous one. */
        if (add_line_entry(stp, tmp_op->a[0].val, 0)) {
            goto load_error;
        }
        if (stp->load_hdr->coverage_mode == ERTS_COV_LINE) {
            coverage_size = sizeof(byte);
        } else if (stp->load_hdr->coverage_mode == ERTS_COV_LINE_COUNTERS) {
            coverage_size = sizeof(Uint);
        }
        if (coverage_size) {
            unsigned loc_index = stp->current_li - 1;
            ASSERT(stp->beam.lines.item_count > 0);
            stp->line_coverage_valid[loc_index] = 1;
            beamasm_emit_coverage(stp->ba,
                                  stp->coverage,
                                  loc_index,
                                  coverage_size);
        }
        break;
    }
    case op_int_code_end:
        /* End of code found. */
        if (stp->function_number != stp->beam.code.function_count) {
            BeamLoadError2(stp,
                           "too few functions (%u) in module (header said %u)",
                           stp->function_number,
                           stp->beam.code.function_count);
        }

        stp->function = THE_NON_VALUE;
        stp->genop = NULL;
        stp->specific_op = -1;

        if (stp->load_hdr->coverage_mode == ERTS_COV_LINE ||
            stp->load_hdr->coverage_mode == ERTS_COV_LINE_COUNTERS) {
            stp->load_hdr->line_coverage_len = stp->current_li;
        }
        break;
    case op_i_test_yield:
        if (stp->load_hdr->coverage_mode == ERTS_COV_FUNCTION) {
            ASSERT(stp->function_number != 0);
            beamasm_emit_coverage(stp->ba,
                                  stp->coverage,
                                  stp->function_number - 1,
                                  sizeof(byte));
        } else if (stp->load_hdr->coverage_mode == ERTS_COV_FUNCTION_COUNTERS) {
            ASSERT(stp->function_number != 0);
            beamasm_emit_coverage(stp->ba,
                                  stp->coverage,
                                  stp->function_number - 1,
                                  sizeof(Uint));
        }
        break;
    }

    return 1;

load_error:
    return 0;
}

static void *get_writable_ptr(const void *executable,
                              void *writable,
                              const void *ptr) {
    const char *exec_raw;
    const char *ptr_raw;
    char *rw_raw;

    exec_raw = (const char *)executable;
    rw_raw = (char *)writable;

    ptr_raw = (const char *)ptr;

    return (void *)(&rw_raw[ptr_raw - exec_raw]);
}

static const BeamCodeLineTab *finish_line_table(LoaderState *stp,
                                                char *module_base,
                                                size_t module_size) {
    const unsigned int ftab_size = stp->beam.code.function_count;
    const unsigned int num_instrs = stp->current_li;

    const BeamCodeLineTab *line_tab_ro;
    BeamCodeLineTab *line_tab_rw;

    const void **line_items_ro;
    void **line_items_rw;

    const unsigned int num_names = stp->beam.lines.name_count;
    const Eterm *fname_base_ro;
    Eterm *fname_base_rw;

    const void *locp_base_ro;
    void *locp_base_rw;

    int i;

    if (stp->line_instr == 0) {
        return NULL;
    }

    line_tab_ro = (const BeamCodeLineTab *)beamasm_get_rodata(stp->ba, "line");
    line_items_ro = (const void **)&line_tab_ro->func_tab[ftab_size + 1];
    fname_base_ro = (Eterm *)&line_items_ro[num_instrs + 1];
    locp_base_ro = &fname_base_ro[stp->beam.lines.name_count];

    line_tab_rw = get_writable_ptr(stp->executable_region,
                                   stp->writable_region,
                                   line_tab_ro);
    line_items_rw = get_writable_ptr(stp->executable_region,
                                     stp->writable_region,
                                     line_items_ro);
    locp_base_rw = get_writable_ptr(stp->executable_region,
                                    stp->writable_region,
                                    locp_base_ro);
    fname_base_rw = get_writable_ptr(stp->executable_region,
                                     stp->writable_region,
                                     fname_base_ro);

    line_tab_rw->loc_size = stp->beam.lines.location_size;
    line_tab_rw->fname_ptr = fname_base_ro;

    for (i = 0; i < ftab_size; i++) {
        line_tab_rw->func_tab[i] = line_items_ro + stp->func_line[i];
    }
    line_tab_rw->func_tab[i] = line_items_ro + num_instrs;

    for (i = 0; i < num_instrs; i++) {
        line_items_rw[i] = (void *)&module_base[stp->line_instr[i].pos];
    }

    line_items_rw[i] = (void *)&module_base[module_size];

    for (i = 0; i < num_names; i++) {
        fname_base_rw[i] =
                beamfile_get_literal(&stp->beam, stp->beam.lines.names[i]);
    }

    if (stp->beam.lines.location_size == sizeof(Uint16)) {
        Uint16 *locp = (Uint16 *)locp_base_rw;
        line_tab_rw->loc_tab.p2 = (Uint16 *)locp_base_ro;

        for (i = 0; i < num_instrs; i++) {
            BeamFile_LineEntry *entry;
            int idx;

            idx = stp->line_instr[i].loc;
            entry = &stp->beam.lines.items[idx];
            *locp++ = MAKE_LOCATION(entry->name_index, entry->location);
        }

        *locp++ = LINE_INVALID_LOCATION;
    } else {
        Uint32 *locp = (Uint32 *)locp_base_rw;
        line_tab_rw->loc_tab.p4 = (Uint32 *)locp_base_ro;

        ASSERT(stp->beam.lines.location_size == sizeof(Uint32));

        for (i = 0; i < num_instrs; i++) {
            BeamFile_LineEntry *entry;
            int idx;

            idx = stp->line_instr[i].loc;
            entry = &stp->beam.lines.items[idx];
            *locp++ = MAKE_LOCATION(entry->name_index, entry->location);
        }

        *locp++ = LINE_INVALID_LOCATION;
    }

    return line_tab_ro;
}

int beam_load_finish_emit(LoaderState *stp) {
    const BeamCodeHeader *code_hdr_ro = NULL;
    BeamCodeHeader *code_hdr_rw = NULL;
    size_t module_size;
    char *module_base;
    Sint decoded_size;
    int ret;

    ret = 0;

    if (stp->line_instr != 0) {
        Uint line_size = offsetof(BeamCodeLineTab, func_tab);

        /* func_tab */
        line_size +=
                (stp->beam.code.function_count + 1) * sizeof(ErtsCodePtr *);

        /* line items */
        line_size += (stp->current_li + 1) * sizeof(ErtsCodePtr);

        /* fname table */
        line_size += stp->beam.lines.name_count * sizeof(Eterm);

        /* loc_tab */;
        line_size += (stp->current_li + 1) * stp->beam.lines.location_size;

        beamasm_embed_bss(stp->ba, "line", line_size);
    }

    /* Place the string table and, optionally, attributes here. */
    beamasm_embed_rodata(stp->ba,
                         "str",
                         (const char *)stp->beam.strings.data,
                         stp->beam.strings.size);
    beamasm_embed_rodata(stp->ba,
                         "attr",
                         (const char *)stp->beam.attributes.data,
                         stp->beam.attributes.size);
    beamasm_embed_rodata(stp->ba,
                         "compile",
                         (const char *)stp->beam.compile_info.data,
                         stp->beam.compile_info.size);
    beamasm_embed_rodata(stp->ba,
                         "md5",
                         (const char *)stp->beam.checksum,
                         sizeof(stp->beam.checksum));

    /* Transfer ownership of the coverage tables to the prepared code. */
    stp->load_hdr->coverage = stp->coverage;
    stp->load_hdr->line_coverage_valid = stp->line_coverage_valid;
    stp->coverage = NULL;
    stp->line_coverage_valid = NULL;

    /* Move the code to its final location. */
    beamasm_codegen(stp->ba,
                    &stp->executable_region,
                    &stp->writable_region,
                    stp->load_hdr,
                    &code_hdr_ro,
                    &code_hdr_rw);

    stp->on_load = beamasm_get_on_load(stp->ba);
    module_base = beamasm_get_base(stp->ba);
    module_size = beamasm_get_offset(stp->ba);

    /* Save the updated code pointer and code size. */
    stp->code_hdr = code_hdr_ro;
    stp->loaded_size = module_size;

    /*
     * Place the literals in their own allocated heap (for fast range check)
     * and fix up all instructions that refer to it.
     */
    {
        Eterm *ptr;
        ErlOffHeap code_off_heap;
        ErtsLiteralArea *literal_area;
        Uint tot_lit_size;
        Uint lit_asize;
        int i;

        tot_lit_size = stp->beam.static_literals.heap_size +
                       stp->beam.dynamic_literals.heap_size;

        ERTS_INIT_OFF_HEAP(&code_off_heap);

        lit_asize = ERTS_LITERAL_AREA_ALLOC_SIZE(tot_lit_size);
        literal_area = erts_alloc(ERTS_ALC_T_LITERAL, lit_asize);
        ptr = &literal_area->start[0];
        literal_area->end = ptr + tot_lit_size;

        beamfile_move_literals(&stp->beam, &ptr, &code_off_heap);

        /* MAJOR HACK: literal identifiers are opaque at the moment. Make it
         * more like string patching. */
        for (i = 0; i < stp->beam.static_literals.count; i++) {
            Eterm lit = beamfile_get_literal(&stp->beam, i);
            beamasm_patch_literal(stp->ba, stp->writable_region, i, lit);
        }

        for (i = 0; i < stp->beam.dynamic_literals.count; i++) {
            Eterm lit = beamfile_get_literal(&stp->beam, ~i);
            beamasm_patch_literal(stp->ba, stp->writable_region, ~i, lit);
        }

        literal_area->off_heap = code_off_heap.first;
        code_hdr_rw->literal_area = literal_area;

        /* Ensure deallocation of literals in case the prepared code is
         * deallocated (without calling erlang:finish_loading/1). */
        (stp->load_hdr)->literal_area = literal_area;
    }

    /* Line information must be added after moving literals, since source file
     * names are literal lists. */
    code_hdr_rw->line_table = finish_line_table(stp, module_base, module_size);

    if (stp->beam.attributes.size) {
        const byte *attr = beamasm_get_rodata(stp->ba, "attr");

        code_hdr_rw->attr_ptr = attr;
        code_hdr_rw->attr_size = stp->beam.attributes.size;

        decoded_size = erts_decode_ext_size(attr, code_hdr_rw->attr_size);
        if (decoded_size < 0) {
            BeamLoadError0(stp,
                           "bad external term representation of module "
                           "attributes");
        }

        code_hdr_rw->attr_size_on_heap = decoded_size;
    }

    if (stp->beam.compile_info.size) {
        const byte *compile_info = beamasm_get_rodata(stp->ba, "compile");

        code_hdr_rw->compile_ptr = compile_info;
        code_hdr_rw->compile_size = stp->beam.compile_info.size;

        decoded_size =
                erts_decode_ext_size(compile_info, stp->beam.compile_info.size);
        if (decoded_size < 0) {
            BeamLoadError0(stp,
                           "bad external term representation of compilation "
                           "information");
        }

        code_hdr_rw->compile_size_on_heap = decoded_size;
    }

    {
        const byte *md5_sum = beamasm_get_rodata(stp->ba, "md5");
        code_hdr_rw->md5_ptr = md5_sum;
    }

    /* Patch all instructions that refer to the string table. */
    if (stp->beam.strings.size) {
        const byte *string_table = beamasm_get_rodata(stp->ba, "str");
        beamasm_patch_strings(stp->ba, stp->writable_region, string_table);
    }

    ret = 1;
load_error:

    /* Some platforms use per-thread global permissions where a thread can
     * either write to or execute _ALL_ JITed pages, so we must seal the module
     * before yielding or this thread won't be able to execute any other JITed
     * code.
     *
     * Note that we have to do this regardless of whether we've succeeded or
     * not, as the module is unsealed after code generation. */
    beamasm_seal_module(stp->executable_region,
                        stp->writable_region,
                        stp->loaded_size);

    return ret;
}

void beam_load_finalize_code(LoaderState *stp,
                             struct erl_module_instance *inst_p) {
    ErtsCodeIndex staging_ix;
    int code_size;

    ERTS_LC_ASSERT(erts_initialized == 0 || erts_has_code_load_permission() ||
                   erts_thr_progress_is_blocking());

    code_size = beamasm_get_header(stp->ba, &stp->code_hdr);
    erts_total_code_size += code_size;

    inst_p->executable_region = stp->executable_region;
    inst_p->writable_region = stp->writable_region;
    inst_p->code_hdr = stp->code_hdr;
    inst_p->code_length = code_size;
    inst_p->unsealed = 0;

    erts_unseal_module(inst_p);

#ifdef ADDRESS_SANITIZER
    /*
     * LeakSanitizer ignores directly mmap'ed memory by default. This causes
     * false positive leak reports of literal areas if only pointed to by
     * mmap'ed jit code. To suprress this we tell leak sanitizer that
     * the jit code is part of the "root set" when doing leak analysis.
     * Module purge should do a corresponding "unregister".
     */
    __lsan_register_root_region(inst_p->code_hdr, inst_p->code_length);
#endif

    /* Update ranges (used for finding a function from a PC value). */
    erts_update_ranges(inst_p->code_hdr, code_size);

    /* Allocate catch indices and fix up all catch_yf instructions. */
    inst_p->catches = beamasm_patch_catches(stp->ba, stp->writable_region);

    /* Exported functions */
    staging_ix = erts_staging_code_ix();

    for (int i = 0; i < stp->beam.exports.count; i++) {
        BeamFile_ExportEntry *entry = &stp->beam.exports.entries[i];
        ErtsCodePtr address;
        Export *ep;

        address = beamasm_get_code(stp->ba, entry->label);
        ep = erts_export_put(stp->module, entry->function, entry->arity);

        if (stp->on_load) {
            /* on_load: Don't make any of the exported functions
             * callable yet. Keep any function in the current
             * code callable. */
            ep->trampoline.not_loaded.deferred = (BeamInstr)address;
        } else {
            ep->dispatch.addresses[staging_ix] = address;
        }
    }

    /* Patch external function calls, this is done after exporting functions as
     * the module may remote-call itself*/
    for (int i = 0; i < stp->beam.imports.count; i++) {
        BeamFile_ImportEntry *entry = &stp->beam.imports.entries[i];
        Export *import;

        import = erts_export_put(entry->module, entry->function, entry->arity);

        beamasm_patch_import(stp->ba, stp->writable_region, i, import);
    }

    /* Patch fun creation. */
    if (stp->beam.lambdas.count) {
        BeamFile_LambdaTable *lambda_table = &stp->beam.lambdas;

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

            ASSERT(stp->fun_refs[i] != ERTS_SWORD_MAX);
            fun_ref = beamfile_get_literal(&stp->beam, stp->fun_refs[i]);

            /* If there are no free variables, the literal refers to an
             * ErlFunThing that needs to be fixed up before we process the
             * FunRef. */
            if (lambda->num_free == 0) {
                ErlFunThing *funp = (ErlFunThing *)boxed_val(fun_ref);
                ASSERT(fun_env_size(funp) == 1 && funp->entry.fun == NULL);
                funp->entry.fun = fun_entry;
                fun_ref = funp->env[0];
            }

            fun_refp = (FunRef *)boxed_val(fun_ref);
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
                              beamasm_get_lambda(stp->ba, i));

            beamasm_patch_lambda(stp->ba, stp->writable_region, i, fun_entry);
        }
    }

    /* Register debug / profiling info with external tools. */
    inst_p->metadata = beamasm_register_metadata(stp->ba, stp->code_hdr);

    erts_seal_module(inst_p);

    /* Prevent literals and code from being freed. */
    (stp->load_hdr)->literal_area = NULL;
    stp->load_hdr->are_nifs = NULL;
    stp->load_hdr->coverage = NULL;
    stp->load_hdr->line_coverage_valid = NULL;
    stp->executable_region = NULL;
    stp->writable_region = NULL;
    stp->code_hdr = NULL;
}

void beam_load_purge_aux(const BeamCodeHeader *hdr) {
    if (hdr->coverage) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->coverage);
    }

    if (hdr->line_coverage_valid) {
        erts_free(ERTS_ALC_T_CODE_COVERAGE, hdr->line_coverage_valid);
    }
}
