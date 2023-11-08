/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
#include "erl_vm.h"
#include "global.h"
#include "erl_version.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "external.h"
#include "beam_load.h"
#include "beam_bp.h"
#include "big.h"
#include "erl_bits.h"
#include "beam_catches.h"
#include "erl_binary.h"
#include "erl_zlib.h"
#include "erl_map.h"
#include "erl_process_dict.h"
#include "erl_unicode.h"
#include "beam_file.h"

Uint erts_total_code_size;

static int load_code(LoaderState *stp);

#define PLEASE_RECOMPILE "please re-compile this module with an Erlang/OTP " ERLANG_OTP_RELEASE " compiler or update your Erlang/OTP version"

/**********************************************************************/

void init_load(void)
{
    erts_total_code_size = 0;
    beam_catches_init();
    erts_init_ranges();

#ifdef DEBUG
    {
        int i;

        for (i = 1; i < NUM_GENERIC_OPS; i++) {
            const GenOpEntry *op = &gen_opc[i];

            ASSERT(op->name && op->name[0] != '\0');
            ASSERT(op->arity <= ERTS_BEAM_MAX_OPARGS);
            ASSERT(op->num_specific <= 1 || op->arity <= 6);
        }
    }
#endif
}

Binary *erts_alloc_loader_state(void) {
    LoaderState* stp;
    Binary* magic;

    magic = erts_create_magic_binary(sizeof(LoaderState),
                                     beam_load_prepared_dtor);
    erts_refc_inc(&magic->intern.refc, 1);

    stp = ERTS_MAGIC_BIN_DATA(magic);

    sys_memset(stp, 0, sizeof(*stp));

    /* Function not known yet */
    stp->function = THE_NON_VALUE;
    stp->specific_op = -1;

    beamopallocator_init(&stp->op_allocator);

    return magic;
}

Eterm
erts_preload_module(Process *c_p,
                    ErtsProcLocks c_p_locks,
                    Eterm group_leader, /* Group leader or NIL if none. */
                    Eterm *modp,        /*
                                         * Module name as an atom (NIL to not
                                         * check). On return, contains the
                                         * actual module name.
                                         */
                    const byte* code,   /* Points to the code to load */
                    Uint size)          /* Size of code to load. */
{
    Binary* magic = erts_alloc_loader_state();
    Eterm retval;

    ASSERT(!erts_initialized);
    retval = erts_prepare_loading(magic, c_p, group_leader, modp,
                                  code, size);

    if (retval != NIL) {
        return retval;
    }

    return erts_finish_loading(magic, c_p, c_p_locks, modp);
}

Eterm
erts_prepare_loading(Binary* magic, Process *c_p, Eterm group_leader,
                     Eterm* modp, const byte *code, Uint unloaded_size)
{
    enum beamfile_read_result read_result;
    Eterm retval = am_badfile;
    LoaderState* stp;

    stp = ERTS_MAGIC_BIN_DATA(magic);
    stp->module = *modp;
    stp->group_leader = group_leader;

#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
    erts_fprintf(stderr,"Loading a module\n");
#endif

    read_result = beamfile_read(code,
                                unloaded_size,
                                &stp->beam);

    switch(read_result) {
    case BEAMFILE_READ_CORRUPT_FILE_HEADER:
        BeamLoadError0(stp, "corrupt file header");
    case BEAMFILE_READ_MISSING_ATOM_TABLE:
        BeamLoadError0(stp, "missing atom table");
    case BEAMFILE_READ_OBSOLETE_ATOM_TABLE:
        BeamLoadError0(stp, PLEASE_RECOMPILE);
    case BEAMFILE_READ_CORRUPT_ATOM_TABLE:
        BeamLoadError0(stp, "corrupt atom table");
    case BEAMFILE_READ_MISSING_CODE_CHUNK:
        BeamLoadError0(stp, "missing code chunk");
    case BEAMFILE_READ_CORRUPT_CODE_CHUNK:
        BeamLoadError0(stp, "corrupt code chunk");
    case BEAMFILE_READ_MISSING_EXPORT_TABLE:
        BeamLoadError0(stp, "missing export table");
    case BEAMFILE_READ_CORRUPT_EXPORT_TABLE:
        BeamLoadError0(stp, "corrupt export table");
    case BEAMFILE_READ_MISSING_IMPORT_TABLE:
        BeamLoadError0(stp, "missing import table");
    case BEAMFILE_READ_CORRUPT_IMPORT_TABLE:
        BeamLoadError0(stp, "corrupt import table");
    case BEAMFILE_READ_CORRUPT_LAMBDA_TABLE:
        BeamLoadError0(stp, "corrupt lambda table");
    case BEAMFILE_READ_CORRUPT_LINE_TABLE:
        BeamLoadError0(stp, "corrupt line table");
    case BEAMFILE_READ_CORRUPT_LITERAL_TABLE:
        BeamLoadError0(stp, "corrupt literal table");
    case BEAMFILE_READ_CORRUPT_LOCALS_TABLE:
        BeamLoadError0(stp, "corrupt locals table");
    case BEAMFILE_READ_CORRUPT_TYPE_TABLE:
        BeamLoadError0(stp, "corrupt type table");
    case BEAMFILE_READ_SUCCESS:
        break;
    }

    ERTS_ASSERT(read_result == BEAMFILE_READ_SUCCESS);

    if (stp->module != stp->beam.module) {
        BeamLoadError1(stp, "module name in object code is %T", stp->beam.module);
    }

    if (stp->beam.code.max_opcode > MAX_GENERIC_OPCODE) {
        BeamLoadError2(stp,
                       "This BEAM file was compiled for a later version"
                       " of the runtime system than the current (Erlang/OTP " ERLANG_OTP_RELEASE ").\n"
                       "  To fix this, " PLEASE_RECOMPILE ".\n"
                       "  (Use of opcode %d; this emulator supports "
                       "only up to %d.)",
                       stp->beam.code.max_opcode, MAX_GENERIC_OPCODE);
    } else if (stp->beam.code.max_opcode < genop_swap_2) {
        /*
         * This BEAM file was produced by OTP 22 or earlier.
         *
         * We know that because OTP 23/24/25/26 artifically set the
         * highest used op code to the op code for the `swap`
         * instruction introduced in OTP 23. (OTP 27 artificially sets
         * the highest op code to `make_fun3` introduced in OTP 24.)
         *
         * Old BEAM files produced by OTP R12 and earlier may be
         * incompatible with the current runtime system. We used to
         * reject such BEAM files using transformation rules that
         * specifically targeted the known problematic constructs, but
         * rejecting them this way is much easier.
         */
        BeamLoadError0(stp,
                       "This BEAM file was compiled for an old version of "
                       "the runtime system.\n"
                       "  To fix this, please re-compile this module with "
                       "Erlang/OTP 24 or later.\n");
    }

    if (!load_code(stp)) {
        goto load_error;
    }

    /* Good so far */
    retval = NIL;

 load_error:
    if (retval != NIL) {
        beam_load_prepared_free(magic);
    }

    return retval;
}

Eterm
erts_finish_loading(Binary* magic, Process* c_p,
                    ErtsProcLocks c_p_locks, Eterm* modp)
{
    Eterm retval = NIL;
    LoaderState* stp = ERTS_MAGIC_BIN_DATA(magic);

    struct erl_module_instance* inst_p;
    Module* mod_tab_p;

    ERTS_LC_ASSERT(erts_initialized == 0 || erts_has_code_load_permission() ||
                   erts_thr_progress_is_blocking());

    /* Make current code for the module old and insert the new code
     * as current. This will fail if there already exists old code
     * for the module. */
    mod_tab_p = erts_put_module(stp->module);
    if (!stp->on_load) {
        /* Normal case -- no -on_load() function. */
        retval = beam_make_current_old(c_p, c_p_locks, stp->module);
        ASSERT(retval == NIL);
    } else {
        ErtsCodeIndex code_ix = erts_staging_code_ix();
        Eterm module = stp->module;
        int i, num_exps;

        /*
         * There is an -on_load() function. We will keep the current
         * code, but we must turn off any tracing.
         */
        num_exps = export_list_size(code_ix);
        for (i = 0; i < num_exps; i++) {
            Export *ep = export_list(i, code_ix);

            if (ep == NULL || ep->info.mfa.module != module) {
                continue;
            }

            DBG_CHECK_EXPORT(ep, code_ix);

            if (erts_is_export_trampoline_active(ep, code_ix)) {
                if (BeamIsOpCode(ep->trampoline.common.op, op_i_generic_breakpoint)) {
                    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
                    ASSERT(mod_tab_p->curr.num_traced_exports > 0);

                    erts_clear_all_export_break(mod_tab_p, ep);

                    ep->dispatch.addresses[code_ix] =
                        (ErtsCodePtr)ep->trampoline.breakpoint.address;
                    ep->trampoline.breakpoint.address = 0;

                    ASSERT(!erts_is_export_trampoline_active(ep, code_ix));
                }

                ASSERT(ep->trampoline.breakpoint.address == 0);
            }
        }
        ASSERT(mod_tab_p->curr.num_breakpoints == 0);
        ASSERT(mod_tab_p->curr.num_traced_exports == 0);
    }

    /* Update module table. */
    if (!stp->on_load) {
        inst_p = &mod_tab_p->curr;
    } else {
        mod_tab_p->on_load = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                                        sizeof(struct erl_module_instance));
        inst_p = mod_tab_p->on_load;
        erts_module_instance_init(inst_p);
    }

    beam_load_finalize_code(stp, inst_p);

#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
    erts_fprintf(stderr,"Loaded %T\n",*modp);
#if 0
    debug_dump_code(stp->code,stp->ci);
#endif
#endif

    *modp = stp->module;

    /* If there is an on_load function, signal an error to
     * indicate that the on_load function must be run. */
    if (stp->on_load) {
        retval = am_on_load;
    }

    beam_load_prepared_free(magic);
    return retval;
}

/* Return a non-zero value if the module has an on_load function,
 * or 0 if it does not. */
Eterm erts_has_code_on_load(Binary* magic)
{
    LoaderState* stp;

    if (ERTS_MAGIC_BIN_DESTRUCTOR(magic) != beam_load_prepared_dtor) {
        return NIL;
    }

    stp = ERTS_MAGIC_BIN_DATA(magic);

    return stp->on_load ? am_true : am_false;
}

/* Return the module name (a tagged atom) for the prepared code
 * in the magic binary, or NIL if the binary does not contain
 * prepared code. */
Eterm
erts_module_for_prepared_code(Binary* magic)
{
    LoaderState* stp;

    if (ERTS_MAGIC_BIN_DESTRUCTOR(magic) != beam_load_prepared_dtor) {
        return NIL;
    }

    stp = ERTS_MAGIC_BIN_DATA(magic);

    if (stp->code_hdr) {
        return stp->module;
    } else {
        return NIL;
    }
}

void beam_load_report_error(int line, LoaderState* context, char *fmt,...)
{
    erts_dsprintf_buf_t *dsbufp;
    va_list va;

    if (is_non_value(context->module)) {
        /* Suppressed by code:get_chunk/2 */
        return;
    }

    dsbufp = erts_create_logger_dsbuf();

    erts_dsprintf(dsbufp, "%s(%d): Error loading ", __FILE__, line);

    if (is_atom(context->function))
        erts_dsprintf(dsbufp, "function %T:%T/%d", context->module,
                      context->function, context->arity);
    else
        erts_dsprintf(dsbufp, "module %T", context->module);

    if (context->genop)
        erts_dsprintf(dsbufp, ": op %s", gen_opc[context->genop->op].name);

    if (context->specific_op != -1)
        erts_dsprintf(dsbufp, ": %s", opc[context->specific_op].sign);
    else if (context->genop) {
        int i;
        for (i = 0; i < context->genop->arity; i++)
            erts_dsprintf(dsbufp, " %c",
                          tag_to_letter[context->genop->a[i].type]);
    }

    erts_dsprintf(dsbufp, ":\n  ");

    va_start(va, fmt);
    erts_vdsprintf(dsbufp, fmt, va);
    va_end(va);

    erts_dsprintf(dsbufp, "\n");
#ifdef DEBUG
    erts_fprintf(stderr, "%s", dsbufp->str);
#endif
    erts_send_error_to_logger(context->group_leader, dsbufp);
}

static int load_code(LoaderState* stp)
{
    BeamOp* last_op = NULL;
    BeamOp** last_op_next = NULL;

    BeamCodeReader *op_reader;
    BeamOp *tmp_op;

    int num_specific;

    op_reader = beamfile_get_code(&stp->beam, &stp->op_allocator);

    if (!beam_load_prepare_emit(stp)) {
        goto load_error;
    }

    for (;;) {
    get_next_instr:
        if (!beamcodereader_next(op_reader, &last_op)) {
            BeamLoadError0(stp, "invalid opcode");
        }

        /*
         * Create a new generic operation and put it last in the chain.
         */
        if (last_op_next == NULL) {
            last_op_next = &(stp->genop);
            while (*last_op_next != NULL) {
                last_op_next = &(*last_op_next)->next;
            }
        }

        last_op->next = NULL;
        *last_op_next = last_op;
        last_op_next = &(last_op->next);
        stp->specific_op = -1;

    do_transform:
        ASSERT(stp->genop != NULL);
        if (gen_opc[stp->genop->op].transform) {
            if (stp->genop->next == NULL) {
                /*
                 * Simple heuristic: Most transformations requires
                 * at least two instructions, so make sure that
                 * there are. That will reduce the number of
                 * TE_SHORT_WINDOWs.
                 */
                goto get_next_instr;
            }
            switch (erts_transform_engine(stp)) {
            case TE_FAIL:
                /*
                 * No transformation found. stp->genop != NULL and
                 * last_op_next is still valid. Go ahead and load
                 * the instruction.
                 */
                break;
            case TE_OK:
                /*
                 * Some transformation was applied. last_op_next is
                 * no longer valid and stp->genop may be NULL.
                 * Try to transform again.
                 */
                if (stp->genop == NULL) {
                    last_op_next = &stp->genop;
                    goto get_next_instr;
                }
                last_op_next = NULL;
                goto do_transform;
            case TE_SHORT_WINDOW:
                /*
                 * No transformation applied. stp->genop != NULL and
                 * last_op_next is still valid. Fetch a new instruction
                 * before trying the transformation again.
                 */
                goto get_next_instr;
            }
        }

        /* From the collected generic instruction, find the specific
         * instruction. */
	tmp_op = stp->genop;
	num_specific = gen_opc[tmp_op->op].num_specific;
	if (num_specific == 1) {
	    /*
	     * There is only one possible specific instruction. Note that
	     * the operands for the instruction will be verified later.
	     */
	    stp->specific_op = gen_opc[tmp_op->op].specific;
	} else {
	    /*
	     * Use bit masks to quickly find the most specific of the
	     * the possible specific instructions associated with this
	     * specific instruction.
	     */
            int specific, arity, arg, i;
            Uint32 mask[3] = {0, 0, 0};

            arity = gen_opc[tmp_op->op].arity;

            for (arg = 0; arg < arity; arg++) {
                int type = tmp_op->a[arg].type;

                mask[arg / 2] |= (1u << type) << ((arg % 2) << 4);
            }

            specific = gen_opc[tmp_op->op].specific;

            for (i = 0; i < num_specific; i++) {
                if (((opc[specific].mask[0] & mask[0]) == mask[0]) &&
                    ((opc[specific].mask[1] & mask[1]) == mask[1]) &&
                    ((opc[specific].mask[2] & mask[2]) == mask[2])) {
#ifdef BEAMASM
		    /* Go ahead and match. */
		    break;
#else
                    if (!opc[specific].involves_r) {
                        /* No complications - match */
                        break;
                    }

                    /*
                     * The specific operation uses the 'r' operand,
                     * which is shorthand for x(0). Now things
                     * get complicated. First we must check whether
                     * all operands that should be of type 'r' use
                     * x(0) (as opposed to some other X register).
                     */
                    for (arg = 0; arg < arity; arg++) {
                        if (opc[specific].involves_r & (1 << arg) &&
                            tmp_op->a[arg].type == TAG_x) {
                            if (tmp_op->a[arg].val != 0) {
                                break; /* Other X register than 0 */
                            }
                        }
                    }

                    if (arg == arity) {
                        /*
                         * All 'r' operands use x(0) in the generic
                         * operation. That means a match. Now we
                         * will need to rewrite the generic instruction
                         * to actually use 'r' instead of 'x(0)'.
                         */
                        for (arg = 0; arg < arity; arg++) {
                            if (opc[specific].involves_r & (1 << arg) &&
                                tmp_op->a[arg].type == TAG_x) {
                                tmp_op->a[arg].type = TAG_r;
                            }
                        }

                        /* Match */
                        break;
                    }
#endif
                }
                specific++;
            }

            /*
             * No specific operation found.
             */
            if (ERTS_UNLIKELY(i == num_specific)) {
                stp->specific_op = -1;

                /*
                 * No specific operations and no transformations means that
                 * the instruction is obsolete.
                 */
                if (num_specific == 0 && gen_opc[tmp_op->op].transform == -1) {
                    BeamLoadError0(stp, PLEASE_RECOMPILE);
                }

                /*
                 * Some generic instructions should have a special
                 * error message.
                 */
                switch (stp->genop->op) {
                case genop_unsupported_guard_bif_3:
                    {
                        Eterm Mod = (Eterm) stp->genop->a[0].val;
                        Eterm Name = (Eterm) stp->genop->a[1].val;
                        Uint arity = (Uint) stp->genop->a[2].val;

                        beamopallocator_free_op(&stp->op_allocator,
                                                stp->genop);

                        stp->genop = NULL;
                        BeamLoadError3(stp, "unsupported guard BIF: %T:%T/%d\n",
                                       Mod, Name, arity);
                    }
                case genop_bad_bs_match_1:
                    {
                        BeamOpArg sub_command = stp->genop->a[0];
                        beamopallocator_free_op(&stp->op_allocator,
                                                stp->genop);
                        stp->genop = NULL;
                        if (sub_command.type == TAG_a) {
                            BeamLoadError1(stp, "bs_match: invalid sub instruction '%T' "
                                           "(is this BEAM file generated by a future version of Erlang/OTP?)\n",
                                           sub_command.val);
                        } else {
                            BeamLoadError0(stp, "bs_match: invalid operands (corrupt BEAM file?)\n");
                        }
                    }
                default:
                    BeamLoadError0(stp, "no specific operation found");
                }
            }

            stp->specific_op = specific;
        }

        if (!beam_load_emit_op(stp, tmp_op)) {
            goto load_error;
        }

        if (stp->specific_op != -1) {
            /* Delete the generic instruction just loaded. */
            BeamOp* next = (stp->genop)->next;

            beamopallocator_free_op(&stp->op_allocator, stp->genop);

            stp->genop = next;
            if (next != NULL) {
                goto do_transform;
            }

            last_op_next = &stp->genop;
        } else {
            /* End of code found */
            break;
        }
    }

    beamcodereader_close(op_reader);
    beamopallocator_dtor(&stp->op_allocator);

    return beam_load_finish_emit(stp);

load_error:
    beamcodereader_close(op_reader);
    beamopallocator_dtor(&stp->op_allocator);
    return 0;
}

void
erts_release_literal_area(ErtsLiteralArea* literal_area)
{
    struct erl_off_heap_header* oh;

    if (!literal_area)
        return;

    oh = literal_area->off_heap;

    while (oh) {
        switch (thing_subtag(oh->thing_word)) {
        case BIN_REF_SUBTAG:
            {
                Binary *bin = ((BinRef*)oh)->val;
                erts_bin_release(bin);
                break;
            }
        case FUN_REF_SUBTAG:
            {
                ErlFunEntry* fe = ((FunRef*)oh)->entry;

                /* All fun entries are NULL during module loading, before the
                 * code is finalized, so we need to tolerate it to avoid
                 * crashing in the prepared code destructor.
                 *
                 * Strictly speaking it would be nice to crash when we see this
                 * outside of loading, but it's too complicated to keep track
                 * of whether we are. */
                if (fe != NULL) {
                    if (erts_refc_dectest(&fe->refc, 0) == 0) {
                        erts_erase_fun_entry(fe);
                    }
                }
                break;
            }
        case REF_SUBTAG:
            {
                ErtsMagicBinary *bptr;
                ASSERT(is_magic_ref_thing(oh));
                bptr = ((ErtsMRefThing *) oh)->mb;
                erts_bin_release((Binary *) bptr);
                break;
            }
        default:
            ASSERT(is_external_header(oh->thing_word));
            erts_deref_node_entry(((ExternalThing*)oh)->node,
                                  make_boxed(&oh->thing_word));
        }
        oh = oh->next;
    }
    erts_free(ERTS_ALC_T_LITERAL, literal_area);
}

#ifdef ENABLE_DBG_TRACE_MFA

#define MFA_MAX 10
Eterm dbg_trace_m[MFA_MAX];
Eterm dbg_trace_f[MFA_MAX];
Uint  dbg_trace_a[MFA_MAX];
unsigned int dbg_trace_ix = 0;

void dbg_set_traced_mfa(const char* m, const char* f, Uint a)
{
    unsigned i = dbg_trace_ix++;
    ASSERT(i < MFA_MAX);
    dbg_trace_m[i] = am_atom_put(m, sys_strlen(m));
    dbg_trace_f[i] = am_atom_put(f, sys_strlen(f));
    dbg_trace_a[i] = a;
}

int dbg_is_traced_mfa(Eterm m, Eterm f, Uint a)
{
    unsigned int i;
    for (i = 0; i < dbg_trace_ix; ++i) {
        if (m == dbg_trace_m[i] &&
            (!f || (f == dbg_trace_f[i] && a == dbg_trace_a[i]))) {

            return i+1;
        }
    }
    return 0;
}

void dbg_vtrace_mfa(unsigned ix, const char* format, ...)
{
    va_list arglist;
    va_start(arglist, format);
    ASSERT(--ix < MFA_MAX);
    erts_fprintf(stderr, "MFA TRACE %T:%T/%u: ",
                 dbg_trace_m[ix], dbg_trace_f[ix], (int)dbg_trace_a[ix]);

    erts_vfprintf(stderr, format, arglist);
    va_end(arglist);
}

#endif /* ENABLE_DBG_TRACE_MFA */
