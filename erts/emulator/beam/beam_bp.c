/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2024. All Rights Reserved.
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
#include "erl_process.h"
#include "beam_code.h"
#include "bif.h"
#include "error.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_term.h"
#include "erl_nfunc_sched.h"

#include "beam_common.h"
#include "jit/beam_asm.h"


/* *************************************************************************
** Macros
*/

/*
** Memory allocation macros
*/
/* Breakpoint data */
#define Alloc(SZ)		erts_alloc(ERTS_ALC_T_BPD, (SZ))
#define ReAlloc(P, SIZ)		erts_realloc(ERTS_ALC_T_BPD, (P), (SZ))
#define Free(P)			erts_free(ERTS_ALC_T_BPD, (P))

#define ERTS_BPF_LOCAL_TRACE       0x01
#define ERTS_BPF_META_TRACE        0x02
#define ERTS_BPF_COUNT             0x04
#define ERTS_BPF_COUNT_ACTIVE      0x08
#define ERTS_BPF_DEBUG             0x10
#define ERTS_BPF_TIME_TRACE        0x20
#define ERTS_BPF_TIME_TRACE_ACTIVE 0x40
#define ERTS_BPF_GLOBAL_TRACE      0x80
#define ERTS_BPF_MEM_TRACE        0x100
#define ERTS_BPF_MEM_TRACE_ACTIVE 0x200

#define ERTS_BPF_ALL              0x3FF

erts_atomic32_t erts_active_bp_index;
erts_atomic32_t erts_staging_bp_index;
erts_mtx_t erts_dirty_bp_ix_mtx;

ErtsTraceSession* erts_staging_trace_session;
GenericBp* breakpoint_free_list;

/*
 * Inlined helpers
 */

static ERTS_INLINE ErtsMonotonicTime
get_mtime(Process *c_p)
{
    return erts_get_monotonic_time(erts_proc_sched_data(c_p));
}

static ERTS_INLINE Uint32
acquire_bp_sched_ix(Process *c_p)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    ASSERT(esdp);
    if (ERTS_SCHEDULER_IS_DIRTY(esdp)) {
	erts_mtx_lock(&erts_dirty_bp_ix_mtx);
        return (Uint32) erts_no_schedulers;
    }
    return (Uint32) esdp->no - 1;
}

static ERTS_INLINE void
release_bp_sched_ix(Uint32 ix)
{
    if (ix == (Uint32) erts_no_schedulers)
        erts_mtx_unlock(&erts_dirty_bp_ix_mtx);
}



/* *************************************************************************
** Local prototypes
*/

/*
** Helpers
*/
const ErtsCodeInfo* erts_trace_call_acc(Process* c_p,
                                        ErtsTraceSession*,
                                        process_breakpoint_trace_t *pbt,
                                        const ErtsCodeInfo *ci,
                                        BpDataAccumulator accum,
                                        int psd_ix,
                                        BpDataCallTrace* bdt);

static ErtsTracer do_call_trace(Process* c_p, ErtsCodeInfo *info, Eterm* reg,
                                int local, Binary* ms,
                                ErtsTraceSession*,
                                ErtsTracerRef*,
                                ErtsTracer tracer);
static void set_break(BpFunctions* f, Binary *match_spec, Uint break_flags,
		      enum erts_break_op count_op, ErtsTracer tracer);
static void set_function_break(ErtsCodeInfo *ci,
			       Binary *match_spec,
			       Uint break_flags,
			       enum erts_break_op count_op,
			       ErtsTracer tracer);

static void clear_break(BpFunctions* f, Uint break_flags);
static void clear_function_break(const ErtsCodeInfo *ci, Uint break_flags);
static void clear_all_sessions_function_break(const ErtsCodeInfo *ci);
static void clear_function_break_session(GenericBp*, Uint break_flags);

static BpDataCallTrace* get_time_break(ErtsTraceSession*, const ErtsCodeInfo *ci);
static BpDataCallTrace* get_memory_break(ErtsTraceSession*, const ErtsCodeInfo *ci);
static GenericBpData* check_break(ErtsTraceSession *session,
                                  const ErtsCodeInfo *ci, Uint break_flags);

static void bp_meta_unref(BpMetaTracer *bmt);
static void bp_count_unref(BpCount *bcp);
static void bp_calltrace_unref(BpDataCallTrace *bdt);
static void consolidate_bp_data(struct erl_module_instance *mi,
                                ErtsCodeInfo *ci, int local);
static void consolidate_bp_data_session(GenericBp* g);
static void uninstall_breakpoint(ErtsCodeInfo *ci_rw,
                                 const ErtsCodeInfo *ci_exec);
static Uint do_session_breakpoint(Process *c_p, ErtsCodeInfo *info, Eterm *reg,
                                  GenericBp* g);

/* bp_hash */
#define BP_ACCUMULATE(pi0, pi1)                         \
    do {                                                \
	(pi0)->count   += (pi1)->count;                 \
	(pi0)->accumulator  += (pi1)->accumulator;      \
    } while(0)

static void bp_hash_init(bp_trace_hash_t *hash, Uint n);
static void bp_hash_rehash(bp_trace_hash_t *hash, Uint n);
static ERTS_INLINE bp_data_trace_item_t * bp_hash_get(bp_trace_hash_t *hash, bp_data_trace_item_t *sitem);
static ERTS_INLINE bp_data_trace_item_t * bp_hash_put(bp_trace_hash_t *hash, bp_data_trace_item_t *sitem);
static void bp_hash_delete(bp_trace_hash_t *hash);

/* *************************************************************************
** External interfaces
*/

void 
erts_bp_init(void) {
    erts_atomic32_init_nob(&erts_active_bp_index, 0);
    erts_atomic32_init_nob(&erts_staging_bp_index, 1);
    erts_mtx_init(&erts_dirty_bp_ix_mtx, "dirty_break_point_index", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DEBUG);
}


void
erts_bp_match_functions(BpFunctions* f, ErtsCodeMFA *mfa, int specified)
{
    ErtsCodeIndex code_ix = erts_active_code_ix();
    Uint max_funcs = 0;
    int current;
    int max_modules = module_code_size(code_ix);
    int num_modules = 0;
    Module* modp;
    Module** module;
    Uint i;

    module = (Module **) Alloc(max_modules*sizeof(Module *));
    num_modules = 0;
    for (current = 0; current < max_modules; current++) {
	modp = module_code(current, code_ix);
	if (modp->curr.code_hdr) {
	    max_funcs += modp->curr.code_hdr->num_functions;
	    module[num_modules++] = modp;
	}
    }

    f->matching = (BpFunction *) Alloc(max_funcs*sizeof(BpFunction));
    i = 0;
    for (current = 0; current < num_modules; current++) {
        const BeamCodeHeader* code_hdr = module[current]->curr.code_hdr;
	Uint num_functions = (Uint)(UWord) code_hdr->num_functions;
	Uint fi;

	if (specified > 0) {
	    if (mfa->module != make_atom(module[current]->module)) {
		/* Wrong module name */
		continue;
	    }
	}

        for (fi = 0; fi < num_functions; fi++) {
            const ErtsCodeInfo* ci = code_hdr->functions[fi];

#ifndef BEAMASM
            ASSERT(BeamIsOpCode(ci->u.op, op_i_func_info_IaaI));
#endif
            switch (specified) {
            case 3:
                if (ci->mfa.arity != mfa->arity)
                    continue;
            case 2:
                if (ci->mfa.function != mfa->function)
                    continue;
            case 1:
                if (ci->mfa.module != mfa->module)
                    continue;
            case 0:
                break;
            }
            /* Store match */
            f->matching[i].code_info = ci;
            f->matching[i].mod = module[current];
            i++;
        }
    }
    f->matched = i;
    Free(module);
}

void
erts_bp_match_export(BpFunctions* f, ErtsCodeMFA *mfa, int specified)
{
    ErtsCodeIndex code_ix = erts_active_code_ix();
    int i;
    int num_exps = export_list_size(code_ix);
    int ne;

    f->matching = (BpFunction *) Alloc(num_exps*sizeof(BpFunction));
    ne = 0;
    for (i = 0; i < num_exps; i++) {
        Export* ep;

        ep = export_list(i, code_ix);

        switch (specified) {
        case 3:
            if (mfa->arity != ep->info.mfa.arity)
                continue;
        case 2:
            if (mfa->function != ep->info.mfa.function)
                continue;
        case 1:
            if (mfa->module != ep->info.mfa.module)
                continue;
        case 0:
            break;
        default:
            ASSERT(0);
        }

        if (erts_is_export_trampoline_active(ep, code_ix)) {
            if (BeamIsOpCode(ep->trampoline.common.op, op_call_error_handler)) {
                continue;
            }

            ASSERT(BeamIsOpCode(ep->trampoline.common.op, op_i_generic_breakpoint));
        }

        f->matching[ne].code_info = &ep->info;
        f->matching[ne].mod = erts_get_module(ep->info.mfa.module, code_ix);

        ne++;
    }

    f->matched = ne;
}

void
erts_bp_free_matched_functions(BpFunctions* f)
{
    if (f->matching) {
	Free(f->matching);
    }
    else ASSERT(f->matched == 0);
}

void
erts_consolidate_export_bp_data(BpFunctions* f)
{
    BpFunction* fs = f->matching;
    Uint i, n;

    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    n = f->matched;

    for (i = 0; i < n; i++) {
        struct erl_module_instance *mi;
        ErtsCodeInfo *ci_rw;

        mi = fs[i].mod ? &fs[i].mod->curr : NULL;

        /* Export entries are always writable, discard const. */
        ci_rw = (ErtsCodeInfo*)fs[i].code_info;

        ASSERT(mi == NULL ||
               !ErtsInArea(ci_rw,
                           mi->executable_region,
                           mi->code_length));

        consolidate_bp_data(mi, ci_rw, 0);
    }
}

void
erts_consolidate_local_bp_data(BpFunctions* f)
{
    struct erl_module_instance *prev_mi;
    BpFunction* fs = f->matching;
    Uint i, n;

    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    n = f->matched;
    prev_mi = NULL;

    for (i = 0; i < n; i++) {
        struct erl_module_instance *mi;
        ErtsCodeInfo *ci_rw;

        ASSERT(fs[i].mod);
        mi = &fs[i].mod->curr;

        if (prev_mi != mi) {
            if (prev_mi != NULL) {
                erts_seal_module(prev_mi);
            }

            erts_unseal_module(mi);
            prev_mi = mi;
        }

        ci_rw = (ErtsCodeInfo*)erts_writable_code_ptr(mi, fs[i].code_info);

        consolidate_bp_data(mi, ci_rw, 1);
    }

    if (prev_mi != NULL) {
        erts_seal_module(prev_mi);
    }
}

void
erts_free_breakpoints(void)
{
    while (breakpoint_free_list) {
        GenericBp* free_me = breakpoint_free_list;
        breakpoint_free_list = breakpoint_free_list->next_to_free;
        Free(free_me);
    }
}

static void
consolidate_bp_data(struct erl_module_instance *mi,
                    ErtsCodeInfo *ci_rw, int local)
{
    GenericBp* g;
    GenericBp*volatile *prev_p;

    g = ci_rw->gen_bp;
    if (!g) {
	return;
    }

    prev_p = &ci_rw->gen_bp;
    do {
        consolidate_bp_data_session(g);

        if (g->data[erts_active_bp_ix()].flags == 0) {
            // unlink disabled breakpoint
            *prev_p = g->next; // Warning: Assumes atomic word write

            // and link into free list
            g->next_to_free = breakpoint_free_list;
            breakpoint_free_list = g;
        }
        else {
            prev_p = &g->next;
        }
        g = g->next;
    } while (g);

    if (ci_rw->gen_bp == NULL && mi) {
        if (local) {
            mi->num_breakpoints--;
        } else {
            mi->num_traced_exports--;
        }
        ASSERT(mi->num_breakpoints >= 0);
        ASSERT(mi->num_traced_exports >= 0);
#if !defined(BEAMASM) && defined(DEBUG)
        {
            BeamInstr instr = *(const BeamInstr*)erts_codeinfo_to_code(ci_rw);
            ASSERT(!BeamIsOpCode(instr, op_i_generic_breakpoint));
        }
#endif
    }

}


static void
consolidate_bp_data_session(GenericBp* g)
{
    GenericBpData* src;
    GenericBpData* dst;
    Uint flags;

    src = &g->data[erts_active_bp_ix()];
    dst = &g->data[erts_staging_bp_ix()];
    ASSERT(src != dst);

    /*
     * The contents of the staging area may be out of date.
     * Decrement all reference pointers.
     */

    flags = dst->flags;
    if (flags & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_GLOBAL_TRACE)) {
	MatchSetUnref(dst->local_ms);
    }
    if (flags & ERTS_BPF_META_TRACE) {
	bp_meta_unref(dst->meta_tracer);
	MatchSetUnref(dst->meta_ms);
    }
    if (flags & ERTS_BPF_COUNT) {
	bp_count_unref(dst->count);
    }
    if (flags & ERTS_BPF_TIME_TRACE) {
	bp_calltrace_unref(dst->time);
    }
    if (flags & ERTS_BPF_MEM_TRACE) {
	bp_calltrace_unref(dst->memory);
    }

    flags = dst->flags = src->flags;
    if (flags == 0) {
        // Breakpoint disabled, will be unlinked and deallocated.
        return;
    }

    /*
     * Copy the active data to the staging area (making it ready
     * for the next time it will be used).
     */

    if (flags & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_GLOBAL_TRACE)) {
	dst->local_ms = src->local_ms;
	MatchSetRef(dst->local_ms);
    }
    if (flags & ERTS_BPF_META_TRACE) {
	dst->meta_tracer = src->meta_tracer;
	erts_refc_inc(&dst->meta_tracer->refc, 1);
	dst->meta_ms = src->meta_ms;
	MatchSetRef(dst->meta_ms);
    }
    if (flags & ERTS_BPF_COUNT) {
	dst->count = src->count;
	erts_refc_inc(&dst->count->refc, 1);
    }
    if (flags & ERTS_BPF_TIME_TRACE) {
	dst->time = src->time;
	erts_refc_inc(&dst->time->refc, 1);
	ASSERT(dst->time->hash);
    }
    if (flags & ERTS_BPF_MEM_TRACE) {
	dst->memory = src->memory;
	erts_refc_inc(&dst->memory->refc, 1);
	ASSERT(dst->memory->hash);
    }
}

void
erts_commit_staged_bp(void)
{
    ErtsBpIndex staging = erts_staging_bp_ix();
    ErtsBpIndex active = erts_active_bp_ix();

    erts_atomic32_set_nob(&erts_active_bp_index, staging);
    erts_atomic32_set_nob(&erts_staging_bp_index, active);
}

void
erts_install_additional_session_bp(ErtsCodeInfo* ci_rw)
{
    GenericBp *g = ci_rw->gen_bp;

    if (g->to_insert) {
        ASSERT(g->to_insert->data[erts_active_bp_ix()].flags == 0);
        ASSERT(g->to_insert->data[erts_staging_bp_ix()].flags != 0);
        ASSERT(g->to_insert->next == g);
        ci_rw->gen_bp = g->to_insert;  // Warning: Atomic word write
        g->to_insert = NULL;
    }
}

void
erts_install_breakpoints(BpFunctions* f)
{
    struct erl_module_instance *prev_mi;
    Uint i, n;

    n = f->matched;
    prev_mi = NULL;

    for (i = 0; i < n; i++) {
        struct erl_module_instance *mi;
        const ErtsCodeInfo *ci_exec;
        ErtsCodeInfo *ci_rw;
        GenericBp *g;
        Module *modp;

        modp = f->matching[i].mod;
        mi = &modp->curr;

        if (prev_mi != mi) {
            if (prev_mi != NULL) {
                erts_seal_module(prev_mi);
            }

            erts_unseal_module(mi);
            prev_mi = mi;
        }

        ci_exec = f->matching[i].code_info;
        ci_rw = (ErtsCodeInfo*)erts_writable_code_ptr(mi, ci_exec);

        g = ci_rw->gen_bp;
        if (!g)
            continue;

#ifdef BEAMASM
        if ((erts_asm_bp_get_flags(ci_exec) & ERTS_ASM_BP_FLAG_BP) == 0) {
	    /*
	     * The breakpoint must be disabled in the active data
	     * (it will enabled later by switching bp indices),
	     * and enabled in the staging data.
	     */
	    ASSERT(g->data[erts_active_bp_ix()].flags == 0);
	    ASSERT(g->data[erts_staging_bp_ix()].flags != 0);
            ASSERT(g->to_insert == NULL);

            erts_asm_bp_set_flag(ci_rw, ci_exec, ERTS_ASM_BP_FLAG_BP);
            mi->num_breakpoints++;
        }
#else
        {
            BeamInstr volatile *pc = (BeamInstr*)erts_codeinfo_to_code(ci_rw);
            BeamInstr instr = *pc;

            ASSERT(ci_exec == ci_rw);
            (void)ci_exec;

            if (!BeamIsOpCode(instr, op_i_generic_breakpoint)) {
                BeamInstr br = BeamOpCodeAddr(op_i_generic_breakpoint);

                /* The breakpoint must be disabled in the active data
                 * (it will enabled later by switching bp indices),
                 * and enabled in the staging data. */
                ASSERT(g->data[erts_active_bp_ix()].flags == 0);
                ASSERT(g->data[erts_staging_bp_ix()].flags != 0);
                ASSERT(g->to_insert == NULL);

                /* The following write is not protected by any lock. We
                 * assume that the hardware guarantees that a write of an
                 * aligned word-size writes is atomic (i.e. that other
                 * processes executing this code will not see a half
                 * pointer).
                 *
                 * The contents of *pc is marked 'volatile' to ensure that
                 * the compiler will do a single full-word write, and not
                 * try any fancy optimizations to write a half word.
                 */
                instr = BeamSetCodeAddr(instr, br);
                *pc = instr;

                mi->num_breakpoints++;
            }
        }
#endif
        erts_install_additional_session_bp(ci_rw);
    }

    if (prev_mi != NULL) {
        erts_seal_module(prev_mi);
    }
}

void
erts_uninstall_breakpoints(BpFunctions* f)
{
    struct erl_module_instance *prev_mi;
    Uint i, n;

    n = f->matched;
    prev_mi = NULL;

    for (i = 0; i < n; i++) {
        struct erl_module_instance *mi = &f->matching[i].mod->curr;
        const ErtsCodeInfo *ci_exec;
        ErtsCodeInfo *ci_rw;

        mi = &f->matching[i].mod->curr;

        if (prev_mi != mi) {
            if (prev_mi != NULL) {
                erts_seal_module(prev_mi);
            }

            erts_unseal_module(mi);
            prev_mi = mi;
        }

        ci_exec = f->matching[i].code_info;
        ci_rw = erts_writable_code_ptr(mi, ci_exec);

        uninstall_breakpoint(ci_rw, ci_exec);
    }

    if (prev_mi != NULL) {
        erts_seal_module(prev_mi);
    }
}

Uint
erts_sum_all_session_flags(ErtsCodeInfo *ci_rw)
{
    const ErtsBpIndex ix = erts_active_bp_ix();
    GenericBp* g = ci_rw->gen_bp;
    Uint all_flags = 0;
    
    for (g = ci_rw->gen_bp; g; g = g->next)
        all_flags |= g->data[ix].flags;
    return all_flags;
}

#ifdef BEAMASM
static void
uninstall_breakpoint(ErtsCodeInfo *ci_rw, const ErtsCodeInfo *ci_exec)
{
    if (erts_asm_bp_get_flags(ci_rw) & ERTS_ASM_BP_FLAG_BP) {
        if (erts_sum_all_session_flags(ci_rw) == 0) {
            erts_asm_bp_unset_flag(ci_rw, ci_exec, ERTS_ASM_BP_FLAG_BP);
        }
    }
}
#else
static void
uninstall_breakpoint(ErtsCodeInfo *ci_rw, const ErtsCodeInfo *ci_exec)
{
    BeamInstr *pc = (BeamInstr*)erts_codeinfo_to_code(ci_rw);

    ASSERT(ci_rw == ci_exec);
    (void)ci_exec;

    if (BeamIsOpCode(*pc, op_i_generic_breakpoint)) {

        if (erts_sum_all_session_flags(ci_rw) == 0) {
            GenericBp* g = ci_rw->gen_bp;
            /*
             * The following write is not protected by any lock. We
             * assume that the hardware guarantees that a write of an
             * aligned word-size (or half-word) writes is atomic
             * (i.e. that other processes executing this code will not
             * see a half pointer).
             */
            *pc = g->orig_instr;
        }
    }
}
#endif

void
erts_set_trace_break(BpFunctions* f, Binary *match_spec)
{
    set_break(f, match_spec, ERTS_BPF_LOCAL_TRACE, 0, erts_tracer_true);
}

void
erts_set_mtrace_break(BpFunctions* f, Binary *match_spec, ErtsTracer tracer)
{
    set_break(f, match_spec, ERTS_BPF_META_TRACE, 0, tracer);
}

void
erts_set_export_trace(ErtsCodeInfo *ci, Binary *match_spec)
{
    set_function_break(ci, match_spec, ERTS_BPF_GLOBAL_TRACE, 0, erts_tracer_nil);
}

void
erts_set_debug_break(BpFunctions* f) {
    set_break(f, NULL, ERTS_BPF_DEBUG, 0, erts_tracer_nil);
}

void
erts_set_count_break(BpFunctions* f, enum erts_break_op count_op)
{
    set_break(f, 0, ERTS_BPF_COUNT|ERTS_BPF_COUNT_ACTIVE,
	      count_op, erts_tracer_nil);
}

void
erts_set_time_break(BpFunctions* f, enum erts_break_op count_op)
{
    set_break(f, 0, ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE,
	      count_op, erts_tracer_nil);
}

void
erts_set_memory_break(BpFunctions* f, enum erts_break_op count_op)
{
    set_break(f, 0, ERTS_BPF_MEM_TRACE|ERTS_BPF_MEM_TRACE_ACTIVE,
	      count_op, erts_tracer_nil);
}

void
erts_clear_trace_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_LOCAL_TRACE);
}

void
erts_clear_export_trace(ErtsCodeInfo *ci)
{
    clear_function_break(ci, ERTS_BPF_GLOBAL_TRACE);
}

void
erts_clear_mtrace_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_META_TRACE);
}

void
erts_clear_debug_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_DEBUG);
}

void
erts_clear_count_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_COUNT|ERTS_BPF_COUNT_ACTIVE);
}

void
erts_clear_time_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE);
}

void
erts_clear_memory_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_MEM_TRACE|ERTS_BPF_MEM_TRACE_ACTIVE);
}
 
void
erts_clear_all_breaks(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_ALL);
}

int
erts_clear_module_break(Module *modp) {
    struct erl_module_instance *mi;
    const BeamCodeHeader* code_hdr;
    Uint n;
    Uint i;

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
    ASSERT(modp);

    mi = &modp->curr;

    code_hdr = mi->code_hdr;
    if (!code_hdr) {
        return 0;
    }

    n = (Uint)code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
        const ErtsCodeInfo *ci = code_hdr->functions[i];

        clear_all_sessions_function_break(ci);
    }

    erts_commit_staged_bp();

    erts_unseal_module(mi);

    for (i = 0; i < n; ++i) {
        const ErtsCodeInfo *ci_exec;
        ErtsCodeInfo *ci_rw;

        ci_exec = code_hdr->functions[i];
        ci_rw = (ErtsCodeInfo*)erts_writable_code_ptr(mi, ci_exec);

        uninstall_breakpoint(ci_rw, ci_exec);
        consolidate_bp_data(mi, ci_rw, 1);

        ASSERT(ci_rw->gen_bp == NULL);
    }

    erts_seal_module(mi);

    erts_free_breakpoints();

    return n;
}

void
erts_clear_all_export_break(Module* modp, Export *ep)
{
    ErtsCodeInfo *ci;
    GenericBp *g;

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());

    ci = &ep->info;

    ASSERT(erts_codeinfo_to_code(ci) == (ErtsCodePtr)&ep->trampoline);
#ifndef BEAMASM
    ASSERT(BeamIsOpCode(ep->trampoline.common.op, op_i_generic_breakpoint));
#endif
    ep->trampoline.common.op = 0;

    for (g = ci->gen_bp; g; g = g->next) {
        clear_function_break_session(g, ERTS_BPF_ALL);
    }
    erts_commit_staged_bp();

    consolidate_bp_data(&modp->curr, ci, 0);
    erts_free_breakpoints();
    ASSERT(ci->gen_bp == NULL);
}

/*
 * If the topmost continuation pointer on the stack is a trace return
 * instruction, we modify it to be the place where we again start to
 * execute code.
 *
 * This continuation pointer is used by match spec {caller} to get the
 * calling function, and if we don't do this fixup it will be
 * 'undefined'. This has the odd side effect of {caller} not really
 * being the function which is the caller, but rather the function
 * which we are about to return to.
 */
static void fixup_cp_before_trace(Process *c_p,
                                  Eterm cp_save[2],
                                  int *return_to_trace)
{
    const ErtsFrameLayout frame_layout = erts_frame_layout;
    Eterm *cpp = c_p->stop;

    if (frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
        ASSERT(is_CP(cpp[1]));
        cp_save[1] = cpp[1];
    }

    ASSERT(is_CP(cpp[0]));
    cp_save[0] = cpp[0];

    for (;;) {
        ErtsCodePtr w;

        erts_inspect_frame(cpp, &w);

        if (BeamIsReturnTrace(w)) {
            cpp += CP_SIZE + BEAM_RETURN_TRACE_FRAME_SZ;
        } else if (BeamIsReturnCallAccTrace(w)) {
            cpp += CP_SIZE + BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
        } else if (BeamIsReturnToTrace(w)) {
            *return_to_trace = 1;
            cpp += CP_SIZE + BEAM_RETURN_TO_TRACE_FRAME_SZ;
        } else {
            if (frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
                ASSERT(is_CP(cpp[1]));
                c_p->stop[1] = cpp[1];
            }

            ASSERT(is_CP(cpp[0]));
            c_p->stop[0] = cpp[0];

            return;
        }
    }
}

static void restore_cp_after_trace(Process *c_p, const Eterm cp_save[2]) {
    if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
        c_p->stop[1] = cp_save[1];
    }

    c_p->stop[0] = cp_save[0];
}

static ERTS_INLINE Uint get_allocated_words(Process *c_p, Sint allocated) {
    if (c_p->abandoned_heap)
        return allocated + c_p->htop - c_p->heap + c_p->mbuf_sz;
    return allocated + c_p->htop - c_p->high_water + c_p->mbuf_sz;
}

BeamInstr
erts_generic_breakpoint(Process* c_p, ErtsCodeInfo *info, Eterm* reg)
{
    GenericBp* g;
    Uint bp_flags = 0;

    ASSERT(info->gen_bp);
#ifndef BEAMASM
    ASSERT(BeamIsOpCode(info->u.op, op_i_func_info_IaaI));
#endif

    if (!ERTS_IS_PROC_SENSITIVE(c_p)) {
        for (g = info->gen_bp; g; g = g->next) {
            bp_flags |= do_session_breakpoint(c_p, info, reg, g);
        }

        if (bp_flags & ERTS_BPF_DEBUG) {
            return BeamOpCodeAddr(op_i_debug_breakpoint);
        }
    }
    return info->gen_bp->orig_instr;
}

static Uint
do_session_breakpoint(Process *c_p, ErtsCodeInfo *info, Eterm *reg,
                      GenericBp* g)
{
    GenericBpData* bp;
    ErtsTracerRef* ref;
    Uint bp_flags;

    ref = get_tracer_ref(&c_p->common, g->session);

    bp = &g->data[erts_active_bp_ix()];
    bp_flags = bp->flags;
    ASSERT((bp_flags & ~ERTS_BPF_ALL) == 0);
    if (bp_flags & (ERTS_BPF_LOCAL_TRACE|
		    ERTS_BPF_GLOBAL_TRACE|
		    ERTS_BPF_TIME_TRACE_ACTIVE|
                    ERTS_BPF_MEM_TRACE_ACTIVE)
        && (!ref || !IS_SESSION_TRACED_FL(ref, F_TRACE_CALLS))) {

	bp_flags &= ~(ERTS_BPF_LOCAL_TRACE|
		      ERTS_BPF_GLOBAL_TRACE|
		      ERTS_BPF_TIME_TRACE|
		      ERTS_BPF_TIME_TRACE_ACTIVE|
                      ERTS_BPF_MEM_TRACE|
                      ERTS_BPF_MEM_TRACE_ACTIVE);
    }
    else if (bp_flags & ERTS_BPF_LOCAL_TRACE) {
	ASSERT((bp_flags & ERTS_BPF_GLOBAL_TRACE) == 0);
        ASSERT(ref);
	(void) do_call_trace(c_p, info, reg, 1, bp->local_ms, g->session, ref,
                             erts_tracer_true);
    } else if (bp_flags & ERTS_BPF_GLOBAL_TRACE) {
        ASSERT((bp_flags & ERTS_BPF_LOCAL_TRACE) == 0);
        ASSERT(ref);
	(void) do_call_trace(c_p, info, reg, 0, bp->local_ms, g->session, ref,
                             erts_tracer_true);
    }

    if (bp_flags & ERTS_BPF_META_TRACE) {
	ErtsTracer old_tracer, new_tracer;

	old_tracer = erts_atomic_read_nob(&bp->meta_tracer->tracer);

	new_tracer = do_call_trace(c_p, info, reg, 1, bp->meta_ms, g->session,
                                   NULL, old_tracer);

	if (!ERTS_TRACER_COMPARE(new_tracer, old_tracer)) {
            if ((erts_aint_t)old_tracer == erts_atomic_cmpxchg_acqb(
                    &bp->meta_tracer->tracer,
                    (erts_aint_t)new_tracer,
                    (erts_aint_t)old_tracer)) {
                ERTS_TRACER_CLEAR(&old_tracer);
            } else {
                ERTS_TRACER_CLEAR(&new_tracer);
            }
	}
    }

    if (bp_flags & ERTS_BPF_COUNT_ACTIVE) {
	erts_atomic_inc_nob(&bp->count->acount);
    }

    if ((bp_flags & (ERTS_BPF_TIME_TRACE_ACTIVE |
                     ERTS_BPF_MEM_TRACE_ACTIVE))) {
        process_breakpoint_trace_t *pbt;
        const ErtsCodeInfo* prev_info = 0;
        ErtsCodePtr w;
        Eterm* E;

        if (bp_flags & ERTS_BPF_TIME_TRACE_ACTIVE) {
            BpDataAccumulator time = get_mtime(c_p);

            for (pbt = ERTS_PROC_GET_CALL_TIME(c_p); pbt; pbt = pbt->next)
                if (pbt->session == g->session)
                    break;

            prev_info = erts_trace_call_acc(c_p, g->session, pbt, info, time,
                                            ERTS_PSD_CALL_TIME_BP, bp->time);
        }

        if (bp_flags & ERTS_BPF_MEM_TRACE_ACTIVE) {
            BpDataAccumulator allocated;

            for (pbt = ERTS_PROC_GET_CALL_MEMORY(c_p); pbt; pbt = pbt->next)
                if (pbt->session == g->session)
                    break;

            /* if this is initial call, ignore 'allocated' */
            if (!pbt &&
                c_p->u.initial.function == info->mfa.function &&
                c_p->u.initial.module == info->mfa.module &&
                c_p->u.initial.arity == info->mfa.arity)
                allocated = 0;
            else {
                allocated = get_allocated_words(c_p, pbt ? pbt->allocated : 0);
            }
            prev_info = erts_trace_call_acc(c_p, g->session, pbt, info, allocated,
                                            ERTS_PSD_CALL_MEMORY_BP, bp->memory);
        }

        E = c_p->stop;

        erts_inspect_frame(E, &w);

        if (!(BeamIsReturnTrace(w) ||
              BeamIsReturnToTrace(w) ||
              BeamIsReturnCallAccTrace(w))) {
            int need = CP_SIZE + BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;

            ASSERT(c_p->htop <= E && E <= c_p->hend);

            if (HeapWordsLeft(c_p) < need) {
                (void) erts_garbage_collect(c_p, need,
                                            reg, info->mfa.arity);
                ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
            }

            E = c_p->stop;

            ASSERT(c_p->htop <= E && E <= c_p->hend);
            ERTS_CT_ASSERT(BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ == 3);
            E -= 1 + BEAM_RETURN_CALL_ACC_TRACE_FRAME_SZ;
            E[3] = g->session->weak_id;
            E[2] = make_small(bp_flags);
            E[1] = prev_info ? make_cp(erts_codeinfo_to_code(prev_info)) : NIL;
            E[0] = make_cp(beam_call_trace_return);

            if (erts_frame_layout == ERTS_FRAME_LAYOUT_FP_RA) {
                E -= 1;
                E[0] = make_cp(FRAME_POINTER(c_p));
                FRAME_POINTER(c_p) = E;
            }

            c_p->stop = E;
        }
    }
    return bp_flags;
}

static ErtsTracer
do_call_trace(Process* c_p, ErtsCodeInfo* info, Eterm* reg,
	      int local, Binary* ms,
              ErtsTraceSession* session,
              ErtsTracerRef* ref, ErtsTracer tracer)
{
    Eterm cp_save[2] = {0, 0};
    int return_to_trace = 0;
    Uint32 flags;
    Uint need = 0;
    Eterm* E;

    fixup_cp_before_trace(c_p, cp_save, &return_to_trace);

    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    flags = erts_call_trace(c_p, info, ms, reg, local, ref, &tracer);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    restore_cp_after_trace(c_p, cp_save);

    E = c_p->stop;

    ASSERT(!ERTS_PROC_IS_EXITING(c_p));

    if ((flags & MATCH_SET_RETURN_TO_TRACE) && !return_to_trace) {
        need += CP_SIZE + BEAM_RETURN_TO_TRACE_FRAME_SZ;
    }

    if (flags & MATCH_SET_RX_TRACE) {
        need += CP_SIZE + BEAM_RETURN_TRACE_FRAME_SZ + size_object(tracer);
    }

    if (need) {
        ASSERT(c_p->htop <= E && E <= c_p->hend);

        if (HeapWordsLeft(c_p) < need) {
            (void) erts_garbage_collect(c_p, need, reg, info->mfa.arity);
            ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
            E = c_p->stop;
        }

        if ((flags & MATCH_SET_RETURN_TO_TRACE) && !return_to_trace) {
            ERTS_CT_ASSERT(BEAM_RETURN_TO_TRACE_FRAME_SZ == 1);
            E -= 1;
            E[0] = session->weak_id;

            E -= CP_SIZE;
            if (erts_frame_layout == ERTS_FRAME_LAYOUT_RA) {
                E[0] = make_cp(beam_return_to_trace);
            } else {
                E[1] = make_cp(beam_return_to_trace);
                E[0] = make_cp(FRAME_POINTER(c_p));
                FRAME_POINTER(c_p) = E;
            }

            ASSERT(c_p->htop <= E && E <= c_p->hend);

            c_p->stop = E;
        }

        if (flags & MATCH_SET_RX_TRACE) {
            ErtsCodePtr trace_cp;

            if (flags & MATCH_SET_EXCEPTION_TRACE) {
                trace_cp = beam_exception_trace;
            } else {
                trace_cp = beam_return_trace;
            }

            ERTS_CT_ASSERT(BEAM_RETURN_TRACE_FRAME_SZ == 3);
            E -= 3;
            E[2] = session->weak_id;
            E[1] = copy_object(tracer, c_p);
            E[0] = make_cp(&info->mfa.module);

            E -= CP_SIZE;
            if (erts_frame_layout == ERTS_FRAME_LAYOUT_RA) {
                E[0] = make_cp(trace_cp);
            } else {
                E[1] = make_cp(trace_cp);
                E[0] = make_cp(FRAME_POINTER(c_p));
                FRAME_POINTER(c_p) = E;
            }

            ASSERT(c_p->htop <= E && E <= c_p->hend);
            ASSERT(is_CP((Eterm)(&info->mfa.module)));
            ASSERT(IS_TRACER_VALID(tracer));

            c_p->stop = E;
            c_p->return_trace_frames++;
        }
    }

    return tracer;
}

const ErtsCodeInfo*
erts_trace_call_acc(Process* c_p,
                    ErtsTraceSession *session,
                    process_breakpoint_trace_t *pbt,
                    const ErtsCodeInfo *info, BpDataAccumulator accum,
                    int psd_ix, BpDataCallTrace* bdt)
{
    bp_data_trace_item_t sitem, *item = NULL;
    bp_trace_hash_t *h = NULL;
    BpDataCallTrace *pbdt = NULL;
    Uint32 six = acquire_bp_sched_ix(c_p);
    const ErtsCodeInfo* prev_info;

    ASSERT(c_p);
    ASSERT(erts_atomic32_read_acqb(&c_p->state) & (ERTS_PSFLG_RUNNING
                                                   | ERTS_PSFLG_DIRTY_RUNNING));
    
    if (pbt == 0) {
	/* First call of process to instrumented function */
	pbt = Alloc(sizeof(process_breakpoint_trace_t));
        pbt->session = session;
        pbt->allocated = 0;
        pbt->ci = NULL;
        pbt->next = erts_psd_set(c_p, psd_ix, pbt);
    }
    else if (pbt->ci) {
	/* add time/allocation to previous code */
	sitem.accumulator = accum - pbt->accumulator;
	sitem.pid = c_p->common.id;
	sitem.count = 0;

	/* previous breakpoint */
	pbdt = ((psd_ix == ERTS_PSD_CALL_TIME_BP)
                ? get_time_break(pbt->session, pbt->ci)
                : get_memory_break(pbt->session, pbt->ci));

	/* if null then the breakpoint was removed */
	if (pbdt) {
	    h = &(pbdt->hash[six]);

	    ASSERT(h);
	    ASSERT(h->item);

	    item = bp_hash_get(h, &sitem);
	    if (!item) {
		item = bp_hash_put(h, &sitem);
	    } else {
		BP_ACCUMULATE(item, &sitem);
	    }
	}
    }
    /*else caller is not call_time traced */

    /* Add count to this code */
    sitem.pid     = c_p->common.id;
    sitem.count   = 1;
    sitem.accumulator = 0;

    /* this breakpoint */
    ASSERT(bdt);
    h = &(bdt->hash[six]);

    ASSERT(h);
    ASSERT(h->item);

    item = bp_hash_get(h, &sitem);
    if (!item) {
	item = bp_hash_put(h, &sitem);
    } else {
	BP_ACCUMULATE(item, &sitem);
    }

    prev_info = pbt->ci;
    pbt->ci = info;
    pbt->accumulator = accum;

    release_bp_sched_ix(six);
    return prev_info;
}


static void
call_trace_add(Process *p, BpDataCallTrace *pbdt, Uint32 six,
               BpDataAccumulator accum, BpDataAccumulator prev_accum)
{
    bp_data_trace_item_t sitem, *item = NULL;
    bp_trace_hash_t *h = NULL;

    sitem.accumulator = accum - prev_accum;
    sitem.pid   = p->common.id;
    sitem.count = 0;

    /* beware, the trace_pattern might have been removed */
    if (pbdt) {

        h = &(pbdt->hash[six]);

        ASSERT(h);
        ASSERT(h->item);

        item = bp_hash_get(h, &sitem);
        if (!item) {
            item = bp_hash_put(h, &sitem);
        } else {
            BP_ACCUMULATE(item, &sitem);
        }
    }
}

void
erts_call_trace_return(Process *p, const ErtsCodeInfo *prev_info,
                       Eterm bp_flags_term, Eterm session_weak_id)
{
    process_breakpoint_trace_t *pbt = NULL;
    BpDataCallTrace *pbdt;
    Uint32 six;
    const Uint bp_flags = unsigned_val(bp_flags_term);
    ErtsTracerRef* ref;

    ASSERT(p);
    ASSERT(erts_atomic32_read_acqb(&p->state) & (ERTS_PSFLG_RUNNING
						     | ERTS_PSFLG_DIRTY_RUNNING));

    ref = get_tracer_ref_from_weak_id(&p->common, session_weak_id);
    if (!ref)
        return;

    six = acquire_bp_sched_ix(p);

    /* get pbt
    * lookup bdt from code
    * timestamp/alloc = t1
    * get ts0/alloc from pbt
    * get item from bdt->hash[bp_hash(p->id)]
    * add diff (t1, t0) to item
    */
    if (bp_flags & ERTS_BPF_TIME_TRACE_ACTIVE) {
        /* get previous timestamp and breakpoint
        * from the process psd  */
        for (pbt = ERTS_PROC_GET_CALL_TIME(p); pbt; pbt = pbt->next) {
            if (pbt->session == ref->session) {
                const ErtsMonotonicTime time = get_mtime(p);

                /* might have been removed due to
                * trace_pattern(false)
                */
                ASSERT(pbt->ci);
                /* previous breakpoint */
                pbdt = get_time_break(ref->session, pbt->ci);
                call_trace_add(p, pbdt, six, time, pbt->accumulator);
                pbt->ci = prev_info;
                pbt->accumulator = time;
                break;
            }
        }
    }

    if (bp_flags & ERTS_BPF_MEM_TRACE_ACTIVE) {
        for (pbt = ERTS_PROC_GET_CALL_MEMORY(p); pbt; pbt = pbt->next) {
            if (pbt->session == ref->session) {
                Sint allocated = get_allocated_words(p, pbt->allocated);
                /* previous breakpoint */
                ASSERT(pbt->ci);
                pbdt = get_memory_break(ref->session, pbt->ci);
                call_trace_add(p, pbdt, six, allocated, pbt->accumulator);
                pbt->ci = prev_info;
                pbt->accumulator = allocated;
                break;
            }
        }
    }

    release_bp_sched_ix(six);
}

int 
erts_is_trace_break(ErtsTraceSession *session,
                    const ErtsCodeInfo *ci, Binary **match_spec_ret, int local)
{
    Uint flags = local ? ERTS_BPF_LOCAL_TRACE : ERTS_BPF_GLOBAL_TRACE;
    GenericBpData* bp = check_break(session, ci, flags);

    if (bp) {
	if (match_spec_ret) {
	    *match_spec_ret = bp->local_ms;
	}
	return 1;
    }
    return 0;
}

int
erts_is_mtrace_break(ErtsTraceSession *session, const ErtsCodeInfo *ci,
                     Binary **match_spec_ret, ErtsTracer *tracer_ret)
{
    GenericBpData* bp = check_break(session, ci, ERTS_BPF_META_TRACE);
    
    if (bp) {
	if (match_spec_ret) {
	    *match_spec_ret = bp->meta_ms;
	}
	if (tracer_ret) {
            *tracer_ret = erts_atomic_read_nob(&bp->meta_tracer->tracer);
	}
	return 1;
    }
    return 0;
}

int 
erts_is_count_break(ErtsTraceSession *session, const ErtsCodeInfo *ci,
                    Uint *count_ret)
{
    GenericBpData* bp = check_break(session, ci, ERTS_BPF_COUNT);
    
    if (bp) {
	if (count_ret) {
	    *count_ret = (Uint) erts_atomic_read_nob(&bp->count->acount);
	}
	return 1;
    }
    return 0;
}

int erts_is_call_break(Process *p, ErtsTraceSession *session, int is_time,
                       const ErtsCodeInfo *ci, Eterm *retval)
{
    Uint i, ix;
    bp_trace_hash_t hash;
    bp_data_trace_item_t *item = NULL;
    BpDataCallTrace *bdt = is_time ? get_time_break(session, ci)
                                   : get_memory_break(session, ci);

    if (!bdt)
        return 0;

    ASSERT(retval);
    /* collect all hashes to one hash */
    bp_hash_init(&hash, 64);
    /* foreach threadspecific hash */
    for (i = 0; i < bdt->n; i++) {
        bp_data_trace_item_t *sitem;

        /* foreach hash bucket not NIL*/
        for(ix = 0; ix < bdt->hash[i].n; ix++) {
            item = &(bdt->hash[i].item[ix]);
            if (item->pid != NIL) {
                sitem = bp_hash_get(&hash, item);
                if (sitem) {
                    BP_ACCUMULATE(sitem, item);
                } else {
                    bp_hash_put(&hash, item);
                }
            }
        }
    }
    /* *retval should be NIL or term from previous bif in export entry */

    if (hash.used > 0) {
        Uint size;
        Eterm *hp, *hp_end, t;

        size = hash.used * (is_time ? (2+5) : (2+4+ERTS_MAX_SINT64_HEAP_SIZE));
        hp   = HAlloc(p, size);
        hp_end = hp + size;

        for(ix = 0; ix < hash.n; ix++) {
            item = &(hash.item[ix]);
            if (item->pid != NIL) {
                if (is_time) {
                    BpDataAccumulator sec, usec;
                    usec = ERTS_MONOTONIC_TO_USEC(item->accumulator);
                    sec = usec / 1000000;
                    usec = usec - sec*1000000;
                    t = TUPLE4(hp, item->pid,
                               make_small(item->count),
                               make_small((Uint) sec),
                               make_small((Uint) usec));
                    hp += 5;
                }
                else {
                    Eterm words = erts_bld_sint64(&hp, NULL, item->accumulator);
                    t = TUPLE3(hp, item->pid,
                               make_small(item->count),
                               words);
                    hp += 4;
                }
                *retval = CONS(hp, t, *retval); hp += 2;
            }
        }
        ASSERT(hp <= hp_end);
        HRelease(p, hp_end, hp);
    }
    bp_hash_delete(&hash);
    return 1;
}

const ErtsCodeInfo *
erts_find_local_func(const ErtsCodeMFA *mfa) {
    const BeamCodeHeader *code_hdr;
    const ErtsCodeInfo *ci;
    Module *modp;
    Uint i,n;

    if ((modp = erts_get_module(mfa->module, erts_active_code_ix())) == NULL)
	return NULL;
    if ((code_hdr = modp->curr.code_hdr) == NULL)
	return NULL;
    n = (BeamInstr) code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
	ci = code_hdr->functions[i];
#ifndef BEAMASM
	ASSERT(BeamIsOpCode(ci->u.op, op_i_func_info_IaaI));
#endif
	ASSERT(mfa->module == ci->mfa.module || is_nil(ci->mfa.module));
	if (mfa->function == ci->mfa.function &&
	    mfa->arity == ci->mfa.arity) {
	    return ci;
	}
    }
    return NULL;
}

static void bp_hash_init(bp_trace_hash_t *hash, Uint n) {
    Uint size = sizeof(bp_data_trace_item_t)*n;
    Uint i;

    hash->n    = n;
    hash->used = 0;

    hash->item = (bp_data_trace_item_t *)Alloc(size);
    sys_memzero(hash->item, size);

    for(i = 0; i < n; ++i) {
	hash->item[i].pid = NIL;
    }
}

static void bp_hash_rehash(bp_trace_hash_t *hash, Uint n) {
    bp_data_trace_item_t *item = NULL;
    Uint size = sizeof(bp_data_trace_item_t)*n;
    Uint ix;
    Uint hval;

    ASSERT(n > 0);

    item = (bp_data_trace_item_t *)Alloc(size);
    sys_memzero(item, size);

    for( ix = 0; ix < n; ++ix) {
	item[ix].pid = NIL;
    }


    /* rehash, old hash -> new hash */

    for( ix = 0; ix < hash->n; ix++) {
	if (hash->item[ix].pid != NIL) {

	    hval = ((hash->item[ix].pid) >> 4) % n; /* new n */

	    while (item[hval].pid != NIL) {
		hval = (hval + 1) % n;
	    }
	    item[hval].pid     = hash->item[ix].pid;
	    item[hval].count   = hash->item[ix].count;
	    item[hval].accumulator = hash->item[ix].accumulator;
	}
    }

    Free(hash->item);
    hash->n = n;
    hash->item = item;
}
static ERTS_INLINE bp_data_trace_item_t * bp_hash_get(bp_trace_hash_t *hash, bp_data_trace_item_t *sitem) {
    Eterm pid = sitem->pid;
    Uint hval = (pid >> 4) % hash->n;
    bp_data_trace_item_t *item = NULL;

    item = hash->item;

    while (item[hval].pid != pid) {
	if (item[hval].pid == NIL) return NULL;
	hval = (hval + 1) % hash->n;
    }

    return &(item[hval]);
}

static ERTS_INLINE bp_data_trace_item_t * bp_hash_put(bp_trace_hash_t *hash, bp_data_trace_item_t* sitem) {
    Uint hval;
    float r = 0.0;
    bp_data_trace_item_t *item;

    /* make sure that the hash is not saturated */
    /* if saturated, rehash it */

    r = hash->used / (float) hash->n;

    if (r > 0.7f) {
	bp_hash_rehash(hash, hash->n * 2);
    }
    /* Do hval after rehash */
    hval = (sitem->pid >> 4) % hash->n;

    /* find free slot */
    item = hash->item;

    while (item[hval].pid != NIL) {
	hval = (hval + 1) % hash->n;
    }
    item = &(hash->item[hval]);

    item->pid     = sitem->pid;
    item->accumulator = sitem->accumulator;
    item->count   = sitem->count;
    hash->used++;

    return item;
}

static void bp_hash_delete(bp_trace_hash_t *hash) {
    hash->n = 0;
    hash->used = 0;
    Free(hash->item);
    hash->item = NULL;
}

static void bp_hash_reset(BpDataCallTrace* bdt) {
    Uint i;
    for (i = 0; i < bdt->n; i++) {
        bp_hash_delete(&(bdt->hash[i]));
        bp_hash_init(&(bdt->hash[i]), 32);
    }
}

void erts_schedule_time_break(Process *p, Uint schedule) {
    process_breakpoint_trace_t *pbt = NULL;
    bp_data_trace_item_t sitem, *item = NULL;
    bp_trace_hash_t *h = NULL;
    BpDataCallTrace *pbdt = NULL;
    Uint32 six = acquire_bp_sched_ix(p);

    ASSERT(p);
    
    switch(schedule) {
    case ERTS_BP_CALL_TIME_SCHEDULE_EXITING :
        break;
    case ERTS_BP_CALL_TIME_SCHEDULE_OUT:
        /* 
         * When a process is scheduled _out_,
	 * timestamp it and add its delta to
	 * the previous breakpoint.
	 */   
        for (pbt = ERTS_PROC_GET_CALL_TIME(p); pbt; pbt = pbt->next) {        
            if (pbt->ci) {
                pbdt = get_time_break(pbt->session, pbt->ci);
                if (pbdt) {
                    sitem.accumulator = get_mtime(p) - pbt->accumulator;
                    sitem.pid   = p->common.id;
                    sitem.count = 0;
                    
                    h = &(pbdt->hash[six]);
                    
                    ASSERT(h);
                    ASSERT(h->item);
                    
                    item = bp_hash_get(h, &sitem);
                    if (!item) {
                        item = bp_hash_put(h, &sitem);
                    } else {
                        BP_ACCUMULATE(item, &sitem);
                    }
                }
            }
        }
        break;
    case ERTS_BP_CALL_TIME_SCHEDULE_IN: {
        ErtsMonotonicTime time = 0;
        /* 
         * When a process is scheduled _in_,
         * timestamp it and remove the previous
         * timestamp in the psd.
         */
        for (pbt = ERTS_PROC_GET_CALL_TIME(p); pbt; pbt = pbt->next) {
            if (!time)
                time = get_mtime(p);
            pbt->accumulator = time;
        }
        break;
    }
    default :
        ASSERT(0);
        /* will never happen */
        break;
    }

    release_bp_sched_ix(six);
}

/* *************************************************************************
** Local helpers
*/


static void
set_break(BpFunctions* f, Binary *match_spec, Uint break_flags,
	  enum erts_break_op count_op, ErtsTracer tracer)
{
    struct erl_module_instance *prev_mi = NULL;
    BpFunction* fs = f->matching;
    Uint i, n;

    n = f->matched;
    prev_mi = NULL;

    for (i = 0; i < n; i++) {
        struct erl_module_instance *mi = &fs[i].mod->curr;
        ErtsCodeInfo *ci_rw;

        if (prev_mi != mi) {
            if (prev_mi != NULL) {
                erts_seal_module(prev_mi);
            }

            erts_unseal_module(mi);
            prev_mi = mi;
        }

        ci_rw = (ErtsCodeInfo *)erts_writable_code_ptr(mi, fs[i].code_info);

        set_function_break(ci_rw,
                           match_spec, break_flags,
                           count_op, tracer);
    }

    if (prev_mi != NULL) {
        erts_seal_module(prev_mi);
    }
}


static GenericBp*
get_bp_session(ErtsTraceSession *session, const ErtsCodeInfo *ci)
{
    GenericBp *g = ci->gen_bp;

    ASSERT(session);
    if (!g)
        return NULL;

    if (g->to_insert) {
        ASSERT(session == erts_staging_trace_session);
        ASSERT(g->to_insert->next == g);
        g = g->to_insert;
    }

    for ( ; g; g = g->next) {
        if (g->session == session)
            return g;
    }
    return NULL;
}

static GenericBp*
get_staging_bp_session(const ErtsCodeInfo *ci)
{
    return get_bp_session(erts_staging_trace_session, ci);
}


static void
set_function_break(ErtsCodeInfo *ci,
                   Binary *match_spec, Uint break_flags,
		   enum erts_break_op count_op, ErtsTracer tracer)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint common;
    ErtsBpIndex ix = erts_staging_bp_ix();

    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    g = get_staging_bp_session(ci);
    if (g == 0) {
	int i;
	if (count_op == ERTS_BREAK_RESTART || count_op == ERTS_BREAK_PAUSE) {
	    /* Do not insert a new breakpoint */
	    return;
	}

	g = Alloc(sizeof(GenericBp));

	for (i = 0; i < ERTS_NUM_BP_IX; i++) {
	    g->data[i].flags = 0;
	}
        g->session = erts_staging_trace_session;
        g->next_to_free = NULL;
        g->to_insert = NULL;

        if (!ci->gen_bp) {
            const UWord *instr_word = (const UWord *)erts_codeinfo_to_code(ci);

#ifdef BEAMASM
            /* The orig_instr is only used in global tracing for BEAMASM and
             * there the address i located within the trampoline in the export
             * entry so we read it from there.
             *
             * For local tracing this value is patched in
             * erts_set_trace_pattern. */
            g->orig_instr = instr_word[2];
#else
            ERTS_CT_ASSERT(sizeof(UWord) == sizeof(BeamInstr));
            if (break_flags == ERTS_BPF_GLOBAL_TRACE) {
                g->orig_instr = BeamOpCodeAddr(op_trace_jump_W);
            }
            else {
                g->orig_instr = instr_word[0];
            }
#endif
            g->next = NULL;
            ci->gen_bp = g;
        }
        else {
            /* Add additional session GenericBp to existing breakpoint.
             * We can't link it yet, must wait for thread progress
             * for readers to see consistent view.
             * Prepare GenericBp to be linked first in list.
             */
            ASSERT(ci->gen_bp->to_insert == NULL);
            g->next = ci->gen_bp;
            ci->gen_bp->to_insert = g;

            g->orig_instr = ci->gen_bp->orig_instr;
        }
    }
    bp = &g->data[ix];

    /*
     * If we are changing an existing breakpoint, clean up old data.
     */

    common = break_flags & bp->flags;
    if (common & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_GLOBAL_TRACE)) {
	MatchSetUnref(bp->local_ms);
    } else if (common & ERTS_BPF_META_TRACE) {
	MatchSetUnref(bp->meta_ms);
	bp_meta_unref(bp->meta_tracer);
    } else if (common & ERTS_BPF_COUNT) {
	if (count_op == ERTS_BREAK_PAUSE) {
	    bp->flags &= ~ERTS_BPF_COUNT_ACTIVE;
	} else {
	    bp->flags |= ERTS_BPF_COUNT_ACTIVE;
	    erts_atomic_set_nob(&bp->count->acount, 0);
	}
	ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
	return;
    } else if (common & ERTS_BPF_TIME_TRACE) {
	if (count_op == ERTS_BREAK_PAUSE) {
	    bp->flags &= ~ERTS_BPF_TIME_TRACE_ACTIVE;
	} else {
	    bp->flags |= ERTS_BPF_TIME_TRACE_ACTIVE;
	    bp_hash_reset(bp->time);
	}
	ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
	return;
    } else if (common & ERTS_BPF_MEM_TRACE) {
	if (count_op == ERTS_BREAK_PAUSE) {
	    bp->flags &= ~ERTS_BPF_MEM_TRACE_ACTIVE;
	} else {
	    bp->flags |= ERTS_BPF_MEM_TRACE_ACTIVE;
	    bp_hash_reset(bp->memory);
	}
	ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
	return;
    }

    /*
     * Initialize the new breakpoint data.
     */

    if (break_flags & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_GLOBAL_TRACE)) {
	MatchSetRef(match_spec);
	bp->local_ms = match_spec;
    } else if (break_flags & ERTS_BPF_META_TRACE) {
	BpMetaTracer* bmt;
        ErtsTracer meta_tracer = erts_tracer_nil;
	MatchSetRef(match_spec);
	bp->meta_ms = match_spec;
	bmt = Alloc(sizeof(BpMetaTracer));
	erts_refc_init(&bmt->refc, 1);
        erts_tracer_update(&meta_tracer, tracer); /* copy tracer */
	erts_atomic_init_nob(&bmt->tracer, (erts_aint_t)meta_tracer);
	bp->meta_tracer = bmt;
    } else if (break_flags & ERTS_BPF_COUNT) {
	BpCount* bcp;

	ASSERT((bp->flags & ERTS_BPF_COUNT) == 0);
	bcp = Alloc(sizeof(BpCount));
	erts_refc_init(&bcp->refc, 1);
	erts_atomic_init_nob(&bcp->acount, 0);
	bp->count = bcp;
    } else if (break_flags & (ERTS_BPF_TIME_TRACE | ERTS_BPF_MEM_TRACE)) {
	BpDataCallTrace* bdt;
	Uint i;

	ASSERT((break_flags & bp->flags & ERTS_BPF_TIME_TRACE) == 0);
        ASSERT((break_flags & bp->flags & ERTS_BPF_MEM_TRACE) == 0);
	bdt = Alloc(sizeof(BpDataCallTrace));
	erts_refc_init(&bdt->refc, 1);
	bdt->n = erts_no_schedulers + 1;
	bdt->hash = Alloc(sizeof(bp_trace_hash_t)*(bdt->n));
	for (i = 0; i < bdt->n; i++) {
	    bp_hash_init(&(bdt->hash[i]), 32);
	}
        if (break_flags & ERTS_BPF_TIME_TRACE)
            bp->time = bdt;
        else
            bp->memory = bdt;
    }

    bp->flags |= break_flags;
    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
}

static void
clear_break(BpFunctions* f, Uint break_flags)
{
    Uint i;
    Uint n;

    n = f->matched;
    for (i = 0; i < n; i++) {
        clear_function_break(f->matching[i].code_info, break_flags);
    }
}

static void
clear_function_break(const ErtsCodeInfo *ci, Uint break_flags)
{
    GenericBp* g;

    ERTS_LC_ASSERT(erts_has_code_mod_permission());

    g = get_staging_bp_session(ci);
    if (g) {
        clear_function_break_session(g, break_flags);
    }
}

static void
clear_all_sessions_function_break(const ErtsCodeInfo *ci)
{
    struct GenericBp *g;

    for (g = ci->gen_bp; g; g = g->next) {
        clear_function_break_session(g, ERTS_BPF_ALL);
    }
}

static void
clear_function_break_session(GenericBp* g, Uint break_flags)
{
    GenericBpData* bp;
    Uint common;

    bp = &g->data[erts_staging_bp_ix()];
    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
    common = bp->flags & break_flags;
    if (!common)
        return;

    bp->flags &= ~break_flags;
    if (common & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_GLOBAL_TRACE)) {
	MatchSetUnref(bp->local_ms);
    }
    if (common & ERTS_BPF_META_TRACE) {
	MatchSetUnref(bp->meta_ms);
	bp_meta_unref(bp->meta_tracer);
    }
    if (common & ERTS_BPF_COUNT) {
	ASSERT((bp->flags & ERTS_BPF_COUNT_ACTIVE) == 0);
	bp_count_unref(bp->count);
    }
    if (common & ERTS_BPF_TIME_TRACE) {
	ASSERT((bp->flags & ERTS_BPF_TIME_TRACE_ACTIVE) == 0);
	bp_calltrace_unref(bp->time);
    }
    if (common & ERTS_BPF_MEM_TRACE) {
        ASSERT((bp->flags & ERTS_BPF_MEM_TRACE_ACTIVE) == 0);
	bp_calltrace_unref(bp->memory);
    }
    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
}

static void
bp_meta_unref(BpMetaTracer* bmt)
{
    if (erts_refc_dectest(&bmt->refc, 0) <= 0) {
        ErtsTracer trc = erts_atomic_read_nob(&bmt->tracer);
        ERTS_TRACER_CLEAR(&trc);
	Free(bmt);
    }
}

static void
bp_count_unref(BpCount* bcp)
{
    if (erts_refc_dectest(&bcp->refc, 0) <= 0) {
	Free(bcp);
    }
}

static void
bp_calltrace_unref(BpDataCallTrace* bdt)
{
    if (erts_refc_dectest(&bdt->refc, 0) <= 0) {
	Uint i = 0;

	for (i = 0; i < bdt->n; ++i) {
	    bp_hash_delete(&(bdt->hash[i]));
	}
	Free(bdt->hash);
	Free(bdt);
    }
}

static BpDataCallTrace*
get_time_break(ErtsTraceSession *session, const ErtsCodeInfo *ci)
{
    GenericBpData* bp = check_break(session, ci, ERTS_BPF_TIME_TRACE);
    return bp ? bp->time : 0;
}

static BpDataCallTrace*
get_memory_break(ErtsTraceSession *session, const ErtsCodeInfo *ci)
{
    GenericBpData* bp = check_break(session, ci, ERTS_BPF_MEM_TRACE);
    return bp ? bp->memory : 0;
}

static GenericBpData*
check_break(ErtsTraceSession *session, const ErtsCodeInfo *ci, Uint break_flags)
{
    GenericBp* g = get_bp_session(session, ci);

#ifndef BEAMASM
    ASSERT(BeamIsOpCode(ci->u.op, op_i_func_info_IaaI));
#endif

    if (g) {
        GenericBpData* bp = &g->data[erts_active_bp_ix()];

        ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
        if (bp->flags & break_flags) {
            return bp;
        }
    }

    return 0;
}
