/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2017. All Rights Reserved.
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
#include "beam_load.h"
#include "bif.h"
#include "error.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_term.h"
#include "erl_nfunc_sched.h"

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

#if defined(ERTS_ENABLE_LOCK_CHECK)
#  define ERTS_REQ_PROC_MAIN_LOCK(P) \
      if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,\
					 __FILE__, __LINE__)
#  define ERTS_UNREQ_PROC_MAIN_LOCK(P) \
      if ((P)) erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN)
#else
#  define ERTS_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_UNREQ_PROC_MAIN_LOCK(P)
#endif

#define ERTS_BPF_LOCAL_TRACE       0x01
#define ERTS_BPF_META_TRACE        0x02
#define ERTS_BPF_COUNT             0x04
#define ERTS_BPF_COUNT_ACTIVE      0x08
#define ERTS_BPF_DEBUG             0x10
#define ERTS_BPF_TIME_TRACE        0x20
#define ERTS_BPF_TIME_TRACE_ACTIVE 0x40
#define ERTS_BPF_GLOBAL_TRACE      0x80

#define ERTS_BPF_ALL               0xFF

extern BeamInstr beam_return_to_trace[1];   /* OpCode(i_return_to_trace) */
extern BeamInstr beam_return_trace[1];      /* OpCode(i_return_trace) */
extern BeamInstr beam_exception_trace[1];   /* OpCode(i_exception_trace) */
extern BeamInstr beam_return_time_trace[1]; /* OpCode(i_return_time_trace) */

erts_atomic32_t erts_active_bp_index;
erts_atomic32_t erts_staging_bp_index;
erts_mtx_t erts_dirty_bp_ix_mtx;

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
static ErtsTracer do_call_trace(Process* c_p, ErtsCodeInfo *info, Eterm* reg,
                                int local, Binary* ms, ErtsTracer tracer);
static void set_break(BpFunctions* f, Binary *match_spec, Uint break_flags,
		      enum erts_break_op count_op, ErtsTracer tracer);
static void set_function_break(ErtsCodeInfo *ci,
			       Binary *match_spec,
			       Uint break_flags,
			       enum erts_break_op count_op,
			       ErtsTracer tracer);

static void clear_break(BpFunctions* f, Uint break_flags);
static int clear_function_break(ErtsCodeInfo *ci, Uint break_flags);

static BpDataTime* get_time_break(ErtsCodeInfo *ci);
static GenericBpData* check_break(ErtsCodeInfo *ci, Uint break_flags);

static void bp_meta_unref(BpMetaTracer *bmt);
static void bp_count_unref(BpCount *bcp);
static void bp_time_unref(BpDataTime *bdt);
static void consolidate_bp_data(Module *modp, ErtsCodeInfo *ci, int local);
static void uninstall_breakpoint(ErtsCodeInfo *ci);

/* bp_hash */
#define BP_TIME_ADD(pi0, pi1)                       \
    do {                                            \
	(pi0)->count   += (pi1)->count;             \
	(pi0)->time    += (pi1)->time;              \
    } while(0)

static void bp_hash_init(bp_time_hash_t *hash, Uint n);
static void bp_hash_rehash(bp_time_hash_t *hash, Uint n);
static ERTS_INLINE bp_data_time_item_t * bp_hash_get(bp_time_hash_t *hash, bp_data_time_item_t *sitem);
static ERTS_INLINE bp_data_time_item_t * bp_hash_put(bp_time_hash_t *hash, bp_data_time_item_t *sitem);
static void bp_hash_delete(bp_time_hash_t *hash);

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
	BeamCodeHeader* code_hdr = module[current]->curr.code_hdr;
	ErtsCodeInfo* ci;
	Uint num_functions = (Uint)(UWord) code_hdr->num_functions;
	Uint fi;

	if (specified > 0) {
	    if (mfa->module != make_atom(module[current]->module)) {
		/* Wrong module name */
		continue;
	    }
	}

	for (fi = 0; fi < num_functions; fi++) {

	    ci = code_hdr->functions[fi];
	    ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
	    if (erts_is_function_native(ci)) {
		continue;
	    }
	    if (is_nil(ci->mfa.module)) { /* Ignore BIF stub */
		continue;
	    }
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
            f->matching[i].ci = ci;
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
	Export* ep = export_list(i, code_ix);
	BeamInstr* pc;

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

	pc = ep->beam;
	if (ep->addressv[code_ix] == pc) {
	    if (BeamIsOpCode(*pc, op_apply_bif) ||
                BeamIsOpCode(*pc, op_call_error_handler)) {
                continue;
	    }
	    ASSERT(BeamIsOpCode(*pc, op_i_generic_breakpoint));
	} else if (erts_is_function_native(erts_code_to_codeinfo(ep->addressv[code_ix]))) {
	    continue;
	}

	f->matching[ne].ci = &ep->info;
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
erts_consolidate_bp_data(BpFunctions* f, int local)
{
    BpFunction* fs = f->matching;
    Uint i;
    Uint n = f->matched;

    ERTS_LC_ASSERT(erts_has_code_write_permission());

    for (i = 0; i < n; i++) {
	consolidate_bp_data(fs[i].mod, fs[i].ci, local);
    }
}

void
erts_consolidate_bif_bp_data(void)
{
    int i;

    ERTS_LC_ASSERT(erts_has_code_write_permission());
    for (i = 0; i < BIF_SIZE; i++) {
	Export *ep = bif_export[i];
	consolidate_bp_data(0, &ep->info, 0);
    }
}

static void
consolidate_bp_data(Module* modp, ErtsCodeInfo *ci, int local)
{
    GenericBp* g = ci->u.gen_bp;
    GenericBpData* src;
    GenericBpData* dst;
    Uint flags;

    if (g == 0) {
	return;
    }

    src = &g->data[erts_active_bp_ix()];
    dst = &g->data[erts_staging_bp_ix()];

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
	bp_time_unref(dst->time);
    }

    /*
     * If all flags are zero, deallocate all breakpoint data.
     */

    flags = dst->flags = src->flags;
    if (flags == 0) {
	if (modp) {
	    if (local) {
		modp->curr.num_breakpoints--;
	    } else {
		modp->curr.num_traced_exports--;
	    }
	    ASSERT(modp->curr.num_breakpoints >= 0);
	    ASSERT(modp->curr.num_traced_exports >= 0);
	    ASSERT(! BeamIsOpCode(*erts_codeinfo_to_code(ci),
                                  op_i_generic_breakpoint));
	}
	ci->u.gen_bp = NULL;
	Free(g);
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
erts_install_breakpoints(BpFunctions* f)
{
    Uint i;
    Uint n = f->matched;
    BeamInstr br = BeamOpCodeAddr(op_i_generic_breakpoint);

    for (i = 0; i < n; i++) {
	ErtsCodeInfo* ci = f->matching[i].ci;
	GenericBp* g = ci->u.gen_bp;
        BeamInstr volatile *pc = erts_codeinfo_to_code(ci);
        BeamInstr instr = *pc;

	if (!BeamIsOpCode(instr, op_i_generic_breakpoint) && g) {
	    Module* modp = f->matching[i].mod;

	    /*
	     * The breakpoint must be disabled in the active data
	     * (it will enabled later by switching bp indices),
	     * and enabled in the staging data.
	     */
	    ASSERT(g->data[erts_active_bp_ix()].flags == 0);
	    ASSERT(g->data[erts_staging_bp_ix()].flags != 0);

	    /*
	     * The following write is not protected by any lock. We
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
	    modp->curr.num_breakpoints++;
	}
    }
}

void
erts_uninstall_breakpoints(BpFunctions* f)
{
    Uint i;
    Uint n = f->matched;

    for (i = 0; i < n; i++) {
	uninstall_breakpoint(f->matching[i].ci);
    }
}

static void
uninstall_breakpoint(ErtsCodeInfo *ci)
{
    BeamInstr *pc = erts_codeinfo_to_code(ci);
    if (BeamIsOpCode(*pc, op_i_generic_breakpoint)) {
	GenericBp* g = ci->u.gen_bp;
	if (g->data[erts_active_bp_ix()].flags == 0) {
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
erts_set_call_trace_bif(ErtsCodeInfo *ci, Binary *match_spec, int local)
{
    Uint flags = local ? ERTS_BPF_LOCAL_TRACE : ERTS_BPF_GLOBAL_TRACE;

    set_function_break(ci, match_spec, flags, 0, erts_tracer_nil);
}

void
erts_set_mtrace_bif(ErtsCodeInfo *ci, Binary *match_spec, ErtsTracer tracer)
{
    set_function_break(ci, match_spec, ERTS_BPF_META_TRACE, 0, tracer);
}

void
erts_set_time_trace_bif(ErtsCodeInfo *ci, enum erts_break_op count_op)
{
    set_function_break(ci, NULL,
		       ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE,
		       count_op, erts_tracer_nil);
}

void
erts_clear_time_trace_bif(ErtsCodeInfo *ci) {
    clear_function_break(ci, ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE);
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
erts_clear_trace_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_LOCAL_TRACE);
}

void
erts_clear_call_trace_bif(ErtsCodeInfo *ci, int local)
{
    GenericBp* g = ci->u.gen_bp;

    if (g) {
	Uint flags = local ? ERTS_BPF_LOCAL_TRACE : ERTS_BPF_GLOBAL_TRACE;
	if (g->data[erts_staging_bp_ix()].flags & flags) {
	    clear_function_break(ci, flags);
	}
    }
}

void
erts_clear_mtrace_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_META_TRACE);
}

void
erts_clear_mtrace_bif(ErtsCodeInfo *ci)
{
    clear_function_break(ci, ERTS_BPF_META_TRACE);
}

void
erts_clear_debug_break(BpFunctions* f)
{
    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
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
erts_clear_all_breaks(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_ALL);
}

int 
erts_clear_module_break(Module *modp) {
    BeamCodeHeader* code_hdr;
    Uint n;
    Uint i;

    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
    ASSERT(modp);
    code_hdr = modp->curr.code_hdr;
    if (!code_hdr) {
	return 0;
    }
    n = (Uint)(UWord) code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
	ErtsCodeInfo *ci = code_hdr->functions[i];
	if (erts_is_function_native(ci))
	    continue;
	clear_function_break(ci, ERTS_BPF_ALL);
    }

    erts_commit_staged_bp();

    for (i = 0; i < n; ++i) {
	ErtsCodeInfo *ci = code_hdr->functions[i];
	if (erts_is_function_native(ci))
	    continue;
	uninstall_breakpoint(ci);
	consolidate_bp_data(modp, ci, 1);
	ASSERT(ci->u.gen_bp == NULL);
    }
    return n;
}

void
erts_clear_export_break(Module* modp, ErtsCodeInfo *ci)
{
    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());

    clear_function_break(ci, ERTS_BPF_ALL);
    erts_commit_staged_bp();
    *erts_codeinfo_to_code(ci) = (BeamInstr) 0;
    consolidate_bp_data(modp, ci, 0);
    ASSERT(ci->u.gen_bp == NULL);
}

/*
 * If c_p->cp is a trace return instruction, we set cp
 * to be the place where we again start to execute code.
 *
 * cp is used by match spec {caller} to get the calling
 * function, and if we don't do this fixup it will be
 * 'undefined'. This has the odd side effect of {caller}
 * not really being which function is the caller, but
 * rather which function we are about to return to.
 */
static void fixup_cp_before_trace(Process *c_p, int *return_to_trace)
{
    Eterm *cpp, *E = c_p->stop;
    BeamInstr w = *c_p->cp;
    if (BeamIsOpCode(w, op_return_trace)) {
        cpp = &E[2];
    } else if (BeamIsOpCode(w, op_i_return_to_trace)) {
        *return_to_trace = 1;
        cpp = &E[0];
    } else if (BeamIsOpCode(w, op_i_return_time_trace)) {
        cpp = &E[0];
    } else {
        cpp = NULL;
    }
    if (cpp) {
        for (;;) {
            BeamInstr w = *cp_val(*cpp);
            if (BeamIsOpCode(w, op_return_trace)) {
                cpp += 3;
            } else if (BeamIsOpCode(w, op_i_return_to_trace)) {
                *return_to_trace = 1;
                cpp += 1;
            } else if (BeamIsOpCode(w, op_i_return_time_trace)) {
                cpp += 2;
            } else {
                break;
            }
        }
        c_p->cp = (BeamInstr *) cp_val(*cpp);
        ASSERT(is_CP(*cpp));
    }
}

BeamInstr
erts_generic_breakpoint(Process* c_p, ErtsCodeInfo *info, Eterm* reg)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint bp_flags;
    ErtsBpIndex ix = erts_active_bp_ix();

    ASSERT(BeamIsOpCode(info->op, op_i_func_info_IaaI));

    g = info->u.gen_bp;
    bp = &g->data[ix];
    bp_flags = bp->flags;
    ASSERT((bp_flags & ~ERTS_BPF_ALL) == 0);
    if (bp_flags & (ERTS_BPF_LOCAL_TRACE|
		    ERTS_BPF_GLOBAL_TRACE|
		    ERTS_BPF_TIME_TRACE_ACTIVE) &&
	!IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	bp_flags &= ~(ERTS_BPF_LOCAL_TRACE|
		      ERTS_BPF_GLOBAL_TRACE|
		      ERTS_BPF_TIME_TRACE|
		      ERTS_BPF_TIME_TRACE_ACTIVE);
	if (bp_flags == 0) {	/* Quick exit */
	    return g->orig_instr;
	}
    }

    if (bp_flags & ERTS_BPF_LOCAL_TRACE) {
	ASSERT((bp_flags & ERTS_BPF_GLOBAL_TRACE) == 0);
	(void) do_call_trace(c_p, info, reg, 1, bp->local_ms, erts_tracer_true);
    } else if (bp_flags & ERTS_BPF_GLOBAL_TRACE) {
	(void) do_call_trace(c_p, info, reg, 0, bp->local_ms, erts_tracer_true);
    }

    if (bp_flags & ERTS_BPF_META_TRACE) {
	ErtsTracer old_tracer, new_tracer;

	old_tracer = erts_atomic_read_nob(&bp->meta_tracer->tracer);

	new_tracer = do_call_trace(c_p, info, reg, 1, bp->meta_ms, old_tracer);

	if (!ERTS_TRACER_COMPARE(new_tracer, old_tracer)) {
            if (old_tracer == erts_atomic_cmpxchg_acqb(
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

    if (bp_flags & ERTS_BPF_TIME_TRACE_ACTIVE) {
	Eterm w;
	ErtsCodeInfo* prev_info = erts_trace_time_call(c_p, info, bp->time);
	w = (BeamInstr) *c_p->cp;
	if (! (BeamIsOpCode(w, op_i_return_time_trace) ||
	       BeamIsOpCode(w, op_return_trace) ||
               BeamIsOpCode(w, op_i_return_to_trace)) ) {
	    Eterm* E = c_p->stop;
	    ASSERT(c_p->htop <= E && E <= c_p->hend);
	    if (E - 2 < c_p->htop) {
		(void) erts_garbage_collect(c_p, 2, reg, info->mfa.arity);
		ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    }
	    E = c_p->stop;

	    ASSERT(c_p->htop <= E && E <= c_p->hend);

	    E -= 2;
	    E[0] = prev_info ? make_cp(erts_codeinfo_to_code(prev_info)) : NIL;
	    E[1] = make_cp(c_p->cp);     /* original return address */
	    c_p->cp = beam_return_time_trace;
	    c_p->stop = E;
	}
    }

    if (bp_flags & ERTS_BPF_DEBUG) {
	return BeamOpCodeAddr(op_i_debug_breakpoint);
    } else {
	return g->orig_instr;
    }
}

/*
 * Entry point called by the trace wrap functions in erl_bif_wrap.c
 *
 * The trace wrap functions are themselves called through the export
 * entries instead of the original BIF functions.
 */
Eterm
erts_bif_trace(int bif_index, Process* p, Eterm* args, BeamInstr* I)
{
    Eterm result;
    Eterm (*func)(Process*, Eterm*, BeamInstr*);
    Export* ep = bif_export[bif_index];
    Uint32 flags = 0, flags_meta = 0;
    ErtsTracer meta_tracer = erts_tracer_nil;
    int applying = (I == ep->beam); /* Yup, the apply code for a bif
                                      * is actually in the
                                      * export entry */
    BeamInstr *cp = p->cp;
    GenericBp* g;
    GenericBpData* bp = NULL;
    Uint bp_flags = 0;
    int return_to_trace = 0;

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);

    g = ep->info.u.gen_bp;
    if (g) {
	bp = &g->data[erts_active_bp_ix()];
	bp_flags = bp->flags;
    }

    /*
     * Make continuation pointer OK, it is not during direct BIF calls,
     * but it is correct during apply of bif.
     */
    if (!applying) {
	p->cp = I;
    } else {
        fixup_cp_before_trace(p, &return_to_trace);
    }
    if (bp_flags & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_GLOBAL_TRACE) &&
	IS_TRACED_FL(p, F_TRACE_CALLS)) {
	int local = !!(bp_flags & ERTS_BPF_LOCAL_TRACE);
	flags = erts_call_trace(p, &ep->info, bp->local_ms, args,
				local, &ERTS_TRACER(p));
    }
    if (bp_flags & ERTS_BPF_META_TRACE) {
	ErtsTracer old_tracer;

        meta_tracer = erts_atomic_read_nob(&bp->meta_tracer->tracer);
        old_tracer = meta_tracer;
	flags_meta = erts_call_trace(p, &ep->info, bp->meta_ms, args,
				     0, &meta_tracer);

	if (!ERTS_TRACER_COMPARE(old_tracer, meta_tracer)) {
            ErtsTracer new_tracer = erts_tracer_nil;
            erts_tracer_update(&new_tracer, meta_tracer);
	    if (old_tracer == erts_atomic_cmpxchg_acqb(
                    &bp->meta_tracer->tracer,
                    (erts_aint_t)new_tracer,
                    (erts_aint_t)old_tracer)) {
                ERTS_TRACER_CLEAR(&old_tracer);
            } else {
                ERTS_TRACER_CLEAR(&new_tracer);
            }
	}
    }
    if (bp_flags & ERTS_BPF_TIME_TRACE_ACTIVE &&
	IS_TRACED_FL(p, F_TRACE_CALLS)) {
	erts_trace_time_call(p, &ep->info, bp->time);
    }

    /* Restore original continuation pointer (if changed). */
    p->cp = cp;

    func = bif_table[bif_index].f;

    result = func(p, args, I);

    if (erts_nif_export_check_save_trace(p, result,
					 applying, ep,
					 cp, flags,
					 flags_meta, I,
					 meta_tracer)) {
	/*
	 * erts_bif_trace_epilogue() will be called
	 * later when appropriate via the NIF export
	 * scheduling functionality...
	 */
	return result;
    }

    return erts_bif_trace_epilogue(p, result, applying, ep, cp,
				   flags, flags_meta, I,
				   meta_tracer);
}

Eterm
erts_bif_trace_epilogue(Process *p, Eterm result, int applying,
			Export* ep, BeamInstr *cp, Uint32 flags,
			Uint32 flags_meta, BeamInstr* I,
			ErtsTracer meta_tracer)
{
    if (applying && (flags & MATCH_SET_RETURN_TO_TRACE)) {
	BeamInstr i_return_trace      = beam_return_trace[0];
	BeamInstr i_return_to_trace   = beam_return_to_trace[0];
	BeamInstr i_return_time_trace = beam_return_time_trace[0];
	Eterm *cpp;
	/* Maybe advance cp to skip trace stack frames */
	for (cpp = p->stop;  ;  cp = cp_val(*cpp++)) {
	    if (*cp == i_return_trace) {
		/* Skip stack frame variables */
		while (is_not_CP(*cpp)) cpp++;
		cpp += 2; /* Skip return_trace parameters */
	    } else if (*cp == i_return_time_trace) {
		/* Skip stack frame variables */
		while (is_not_CP(*cpp)) cpp++;
		cpp += 1; /* Skip return_time_trace parameters */
	    } else if (*cp == i_return_to_trace) {
		/* A return_to trace message is going to be generated
		 * by normal means, so we do not have to.
		 */
		cp = NULL;
		break;
	    } else break;
	}
    }

    /* Try to get these in the order
     * they usually appear in normal code... */
    if (is_non_value(result)) {
	Uint reason = p->freason;
	if (reason != TRAP) {
	    Eterm class;
	    Eterm value = p->fvalue;
	    /* Expand error value like in handle_error() */
	    if (reason & EXF_ARGLIST) {
		Eterm *tp;
		ASSERT(is_tuple(value));
		tp = tuple_val(value);
		value = tp[1];
	    }
	    if ((reason & EXF_THROWN) && (p->catches <= 0)) {
                Eterm *hp = HAlloc(p, 3);
		value = TUPLE2(hp, am_nocatch, value);
		reason = EXC_ERROR;
	    }
	    /* Note: expand_error_value() could theoretically
	     * allocate on the heap, but not for any error
	     * returned by a BIF, and it would do no harm,
	     * just be annoying.
	     */
	    value = expand_error_value(p, reason, value);
	    class = exception_tag[GET_EXC_CLASS(reason)];

	    if (flags_meta & MATCH_SET_EXCEPTION_TRACE) {
		erts_trace_exception(p, &ep->info.mfa, class, value,
				     &meta_tracer);
	    }
	    if (flags & MATCH_SET_EXCEPTION_TRACE) {
		erts_trace_exception(p, &ep->info.mfa, class, value,
				     &ERTS_TRACER(p));
	    }
	    if ((flags & MATCH_SET_RETURN_TO_TRACE) && p->catches > 0) {
		/* can only happen if(local)*/
		Eterm *ptr = p->stop;
		ASSERT(is_CP(*ptr));
		ASSERT(ptr <= STACK_START(p));
		/* Search the nearest stack frame for a catch */
		while (++ptr < STACK_START(p)) {
		    if (is_CP(*ptr)) break;
		    if (is_catch(*ptr)) {
			if (applying) {
			    /* Apply of BIF, cp is in calling function */
			    if (cp) erts_trace_return_to(p, cp);
			} else {
			    /* Direct bif call, I points into
			     * calling function */
			    erts_trace_return_to(p, I);
			}
		    }
		}
	    }
	    if ((flags_meta|flags) & MATCH_SET_EXCEPTION_TRACE) {
		erts_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
		ERTS_TRACE_FLAGS(p) |= F_EXCEPTION_TRACE;
		erts_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
	    }
	}
    } else {
	if (flags_meta & MATCH_SET_RX_TRACE) {
	    erts_trace_return(p, &ep->info.mfa, result, &meta_tracer);
	}
	/* MATCH_SET_RETURN_TO_TRACE cannot occur if(meta) */
	if (flags & MATCH_SET_RX_TRACE) {
	    erts_trace_return(p, &ep->info.mfa, result, &ERTS_TRACER(p));
	}
	if (flags & MATCH_SET_RETURN_TO_TRACE &&
            IS_TRACED_FL(p, F_TRACE_RETURN_TO)) {
	    /* can only happen if(local)*/
	    if (applying) {
		/* Apply of BIF, cp is in calling function */
		if (cp) erts_trace_return_to(p, cp);
	    } else {
		/* Direct bif call, I points into calling function */
		erts_trace_return_to(p, I);
	    }
	}
    }
    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
    return result;
}

static ErtsTracer
do_call_trace(Process* c_p, ErtsCodeInfo* info, Eterm* reg,
	      int local, Binary* ms, ErtsTracer tracer)
{
    int return_to_trace = 0;
    BeamInstr *cp_save = c_p->cp;
    Uint32 flags;
    Uint need = 0;
    Eterm* E = c_p->stop;

    fixup_cp_before_trace(c_p, &return_to_trace);

    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    flags = erts_call_trace(c_p, info, ms, reg, local, &tracer);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    /* restore cp after potential fixup */
    c_p->cp = cp_save;

    ASSERT(!ERTS_PROC_IS_EXITING(c_p));
    if ((flags & MATCH_SET_RETURN_TO_TRACE) && !return_to_trace) {
	need += 1;
    }
    if (flags & MATCH_SET_RX_TRACE) {
	need += 3 + size_object(tracer);
    }
    if (need) {
	ASSERT(c_p->htop <= E && E <= c_p->hend);
	if (E - need < c_p->htop) {
	    (void) erts_garbage_collect(c_p, need, reg, info->mfa.arity);
	    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    E = c_p->stop;
	}
    }
    if (flags & MATCH_SET_RETURN_TO_TRACE && !return_to_trace) {
	E -= 1;
	ASSERT(c_p->htop <= E && E <= c_p->hend);
	E[0] = make_cp(c_p->cp);
	c_p->cp = beam_return_to_trace;
    }
    if (flags & MATCH_SET_RX_TRACE)
    {
	E -= 3;
        c_p->stop = E;
	ASSERT(c_p->htop <= E && E <= c_p->hend);
	ASSERT(is_CP((Eterm) (UWord) (&info->mfa.module)));
	ASSERT(IS_TRACER_VALID(tracer));
	E[2] = make_cp(c_p->cp);
        E[1] = copy_object(tracer, c_p);
	E[0] = make_cp(&info->mfa.module);
                               /* We ARE at the beginning of an instruction,
				  the funcinfo is above i. */
	c_p->cp = (flags & MATCH_SET_EXCEPTION_TRACE) ?
	    beam_exception_trace : beam_return_trace;
	erts_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	ERTS_TRACE_FLAGS(c_p) |= F_EXCEPTION_TRACE;
	erts_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
    } else
        c_p->stop = E;
    return tracer;
}

ErtsCodeInfo*
erts_trace_time_call(Process* c_p, ErtsCodeInfo *info, BpDataTime* bdt)
{
    ErtsMonotonicTime time;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;
    Uint32 six = acquire_bp_sched_ix(c_p);
    ErtsCodeInfo* prev_info;

    ASSERT(c_p);
    ASSERT(erts_atomic32_read_acqb(&c_p->state) & (ERTS_PSFLG_RUNNING
						       | ERTS_PSFLG_DIRTY_RUNNING));

    /* get previous timestamp and breakpoint
     * from the process psd  */
    
    pbt = ERTS_PROC_GET_CALL_TIME(c_p);
    time = get_mtime(c_p);

    /* get pbt
     * timestamp = t0
     * lookup bdt from code
     * set ts0 to pbt
     * add call count here?
     */
    if (pbt == 0) {
	/* First call of process to instrumented function */
	pbt = Alloc(sizeof(process_breakpoint_time_t));
	(void) ERTS_PROC_SET_CALL_TIME(c_p, pbt);
        pbt->ci = NULL;
    }
    else if (pbt->ci) {
	/* add time to previous code */
	sitem.time = time - pbt->time;
	sitem.pid = c_p->common.id;
	sitem.count = 0;

	/* previous breakpoint */
	pbdt = get_time_break(pbt->ci);

	/* if null then the breakpoint was removed */
	if (pbdt) {
	    h = &(pbdt->hash[six]);

	    ASSERT(h);
	    ASSERT(h->item);

	    item = bp_hash_get(h, &sitem);
	    if (!item) {
		item = bp_hash_put(h, &sitem);
	    } else {
		BP_TIME_ADD(item, &sitem);
	    }
	}
    }
    /*else caller is not call_time traced */

    /* Add count to this code */
    sitem.pid     = c_p->common.id;
    sitem.count   = 1;
    sitem.time    = 0;

    /* this breakpoint */
    ASSERT(bdt);
    h = &(bdt->hash[six]);

    ASSERT(h);
    ASSERT(h->item);

    item = bp_hash_get(h, &sitem);
    if (!item) {
	item = bp_hash_put(h, &sitem);
    } else {
	BP_TIME_ADD(item, &sitem);
    }

    prev_info = pbt->ci;
    pbt->ci = info;
    pbt->time = time;

    release_bp_sched_ix(six);
    return prev_info;
}

void
erts_trace_time_return(Process *p, ErtsCodeInfo *prev_info)
{
    ErtsMonotonicTime time;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;
    Uint32 six = acquire_bp_sched_ix(p);

    ASSERT(p);
    ASSERT(erts_atomic32_read_acqb(&p->state) & (ERTS_PSFLG_RUNNING
						     | ERTS_PSFLG_DIRTY_RUNNING));

    /* get previous timestamp and breakpoint
     * from the process psd  */

    pbt = ERTS_PROC_GET_CALL_TIME(p);
    time = get_mtime(p);

    /* get pbt
     * lookup bdt from code
     * timestamp = t1
     * get ts0 from pbt
     * get item from bdt->hash[bp_hash(p->id)]
     * ack diff (t1, t0) to item
     */

    if (pbt) {

	/* might have been removed due to
	 * trace_pattern(false)
	 */
	ASSERT(pbt->ci);

	sitem.time = time - pbt->time;
	sitem.pid   = p->common.id;
	sitem.count = 0;

	/* previous breakpoint */
	pbdt = get_time_break(pbt->ci);

	/* beware, the trace_pattern might have been removed */
	if (pbdt) {

	    h = &(pbdt->hash[six]);

	    ASSERT(h);
	    ASSERT(h->item);

	    item = bp_hash_get(h, &sitem);
	    if (!item) {
		item = bp_hash_put(h, &sitem);
	    } else {
		BP_TIME_ADD(item, &sitem);
	    }

	}

	pbt->ci = prev_info;
	pbt->time = time;

    }

    release_bp_sched_ix(six);
}

int 
erts_is_trace_break(ErtsCodeInfo *ci, Binary **match_spec_ret, int local)
{
    Uint flags = local ? ERTS_BPF_LOCAL_TRACE : ERTS_BPF_GLOBAL_TRACE;
    GenericBpData* bp = check_break(ci, flags);

    if (bp) {
	if (match_spec_ret) {
	    *match_spec_ret = bp->local_ms;
	}
	return 1;
    }
    return 0;
}

int
erts_is_mtrace_break(ErtsCodeInfo *ci, Binary **match_spec_ret,
		     ErtsTracer *tracer_ret)
{
    GenericBpData* bp = check_break(ci, ERTS_BPF_META_TRACE);
    
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
erts_is_count_break(ErtsCodeInfo *ci, Uint *count_ret)
{
    GenericBpData* bp = check_break(ci, ERTS_BPF_COUNT);
    
    if (bp) {
	if (count_ret) {
	    *count_ret = (Uint) erts_atomic_read_nob(&bp->count->acount);
	}
	return 1;
    }
    return 0;
}

int erts_is_time_break(Process *p, ErtsCodeInfo *ci, Eterm *retval) {
    Uint i, ix;
    bp_time_hash_t hash;
    Uint size;
    Eterm *hp, t;
    bp_data_time_item_t *item = NULL;
    BpDataTime *bdt = get_time_break(ci);

    if (bdt) {
	if (retval) {
	    /* collect all hashes to one hash */
	    bp_hash_init(&hash, 64);
	    /* foreach threadspecific hash */
	    for (i = 0; i < bdt->n; i++) {
		bp_data_time_item_t *sitem;

	        /* foreach hash bucket not NIL*/
		for(ix = 0; ix < bdt->hash[i].n; ix++) {
		    item = &(bdt->hash[i].item[ix]);
		    if (item->pid != NIL) {
			sitem = bp_hash_get(&hash, item);
			if (sitem) {
			    BP_TIME_ADD(sitem, item);
			} else {
			    bp_hash_put(&hash, item);
			}
		    }
		}
	    }
	    /* *retval should be NIL or term from previous bif in export entry */

	    if (hash.used > 0) {
		size = (5 + 2)*hash.used;
		hp   = HAlloc(p, size);

		for(ix = 0; ix < hash.n; ix++) {
		    item = &(hash.item[ix]);
		    if (item->pid != NIL) {
			ErtsMonotonicTime sec, usec;
			usec = ERTS_MONOTONIC_TO_USEC(item->time);
			sec = usec / 1000000;
			usec = usec - sec*1000000;
			t = TUPLE4(hp, item->pid,
				make_small(item->count),
				   make_small((Uint) sec),
				   make_small((Uint) usec));
			hp += 5;
			*retval = CONS(hp, t, *retval); hp += 2;
		    }
		}
	    }
	    bp_hash_delete(&hash);
	}
	return 1;
    }

    return 0;
}


ErtsCodeInfo *
erts_find_local_func(ErtsCodeMFA *mfa) {
    Module *modp;
    BeamCodeHeader* code_hdr;
    ErtsCodeInfo* ci;
    Uint i,n;

    if ((modp = erts_get_module(mfa->module, erts_active_code_ix())) == NULL)
	return NULL;
    if ((code_hdr = modp->curr.code_hdr) == NULL)
	return NULL;
    n = (BeamInstr) code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
	ci = code_hdr->functions[i];
	ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
	ASSERT(mfa->module == ci->mfa.module || is_nil(ci->mfa.module));
	if (mfa->function == ci->mfa.function &&
	    mfa->arity == ci->mfa.arity) {
	    return ci;
	}
    }
    return NULL;
}

static void bp_hash_init(bp_time_hash_t *hash, Uint n) {
    Uint size = sizeof(bp_data_time_item_t)*n;
    Uint i;

    hash->n    = n;
    hash->used = 0;

    hash->item = (bp_data_time_item_t *)Alloc(size);
    sys_memzero(hash->item, size);

    for(i = 0; i < n; ++i) {
	hash->item[i].pid = NIL;
    }
}

static void bp_hash_rehash(bp_time_hash_t *hash, Uint n) {
    bp_data_time_item_t *item = NULL;
    Uint size = sizeof(bp_data_time_item_t)*n;
    Uint ix;
    Uint hval;

    item = (bp_data_time_item_t *)Alloc(size);
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
	    item[hval].time    = hash->item[ix].time;
	}
    }

    Free(hash->item);
    hash->n = n;
    hash->item = item;
}
static ERTS_INLINE bp_data_time_item_t * bp_hash_get(bp_time_hash_t *hash, bp_data_time_item_t *sitem) {
    Eterm pid = sitem->pid;
    Uint hval = (pid >> 4) % hash->n;
    bp_data_time_item_t *item = NULL;

    item = hash->item;

    while (item[hval].pid != pid) {
	if (item[hval].pid == NIL) return NULL;
	hval = (hval + 1) % hash->n;
    }

    return &(item[hval]);
}

static ERTS_INLINE bp_data_time_item_t * bp_hash_put(bp_time_hash_t *hash, bp_data_time_item_t* sitem) {
    Uint hval;
    float r = 0.0;
    bp_data_time_item_t *item;

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
    item->time    = sitem->time;
    item->count   = sitem->count;
    hash->used++;

    return item;
}

static void bp_hash_delete(bp_time_hash_t *hash) {
    hash->n = 0;
    hash->used = 0;
    Free(hash->item);
    hash->item = NULL;
}

void erts_schedule_time_break(Process *p, Uint schedule) {
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;
    Uint32 six = acquire_bp_sched_ix(p);

    ASSERT(p);

    pbt = ERTS_PROC_GET_CALL_TIME(p);

    if (pbt) {

	switch(schedule) {
	case ERTS_BP_CALL_TIME_SCHEDULE_EXITING :
	    break;
	case ERTS_BP_CALL_TIME_SCHEDULE_OUT :
	    /* When a process is scheduled _out_,
	     * timestamp it and add its delta to
	     * the previous breakpoint.
	     */

            if (pbt->ci) {
                pbdt = get_time_break(pbt->ci);
                if (pbdt) {
                    sitem.time = get_mtime(p) - pbt->time;
                    sitem.pid   = p->common.id;
                    sitem.count = 0;

                    h = &(pbdt->hash[six]);

                    ASSERT(h);
                    ASSERT(h->item);

                    item = bp_hash_get(h, &sitem);
                    if (!item) {
                        item = bp_hash_put(h, &sitem);
                    } else {
                        BP_TIME_ADD(item, &sitem);
                    }
                }
            }
	    break;
	case ERTS_BP_CALL_TIME_SCHEDULE_IN :
	    /* When a process is scheduled _in_,
	     * timestamp it and remove the previous
	     * timestamp in the psd.
	     */
	    pbt->time = get_mtime(p);
	    break;
	default :
	    ASSERT(0);
		/* will never happen */
	    break;
	}
    } /* pbt */

    release_bp_sched_ix(six);
}

/* *************************************************************************
** Local helpers
*/


static void
set_break(BpFunctions* f, Binary *match_spec, Uint break_flags,
	  enum erts_break_op count_op, ErtsTracer tracer)
{
    Uint i;
    Uint n;

    n = f->matched;
    for (i = 0; i < n; i++) {
	set_function_break(f->matching[i].ci,
                           match_spec, break_flags,
			   count_op, tracer);
    }
}

static void
set_function_break(ErtsCodeInfo *ci, Binary *match_spec, Uint break_flags,
		   enum erts_break_op count_op, ErtsTracer tracer)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint common;
    ErtsBpIndex ix = erts_staging_bp_ix();

    ERTS_LC_ASSERT(erts_has_code_write_permission());
    g = ci->u.gen_bp;
    if (g == 0) {
	int i;
	if (count_op == ERTS_BREAK_RESTART || count_op == ERTS_BREAK_PAUSE) {
	    /* Do not insert a new breakpoint */
	    return;
	}
	g = Alloc(sizeof(GenericBp));
	g->orig_instr = *erts_codeinfo_to_code(ci);
	for (i = 0; i < ERTS_NUM_BP_IX; i++) {
	    g->data[i].flags = 0;
	}
	ci->u.gen_bp = g;
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
	BpDataTime* bdt = bp->time;
	Uint i = 0;

	if (count_op == ERTS_BREAK_PAUSE) {
	    bp->flags &= ~ERTS_BPF_TIME_TRACE_ACTIVE;
	} else {
	    bp->flags |= ERTS_BPF_TIME_TRACE_ACTIVE;
	    for (i = 0; i < bdt->n; i++) {
		bp_hash_delete(&(bdt->hash[i]));
		bp_hash_init(&(bdt->hash[i]), 32);
	    }
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
    } else if (break_flags & ERTS_BPF_TIME_TRACE) {
	BpDataTime* bdt;
	int i;

	ASSERT((bp->flags & ERTS_BPF_TIME_TRACE) == 0);
	bdt = Alloc(sizeof(BpDataTime));
	erts_refc_init(&bdt->refc, 1);
	bdt->n = erts_no_schedulers + 1;
	bdt->hash = Alloc(sizeof(bp_time_hash_t)*(bdt->n));
	for (i = 0; i < bdt->n; i++) {
	    bp_hash_init(&(bdt->hash[i]), 32);
	}
	bp->time = bdt;
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
	clear_function_break(f->matching[i].ci, break_flags);
    }
}

static int
clear_function_break(ErtsCodeInfo *ci, Uint break_flags)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint common;
    ErtsBpIndex ix = erts_staging_bp_ix();

    ERTS_LC_ASSERT(erts_has_code_write_permission());

    if ((g = ci->u.gen_bp) == NULL) {
	return 1;
    }

    bp = &g->data[ix];
    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
    common = bp->flags & break_flags;
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
	bp_time_unref(bp->time);
    }

    ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
    return 1;
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
bp_time_unref(BpDataTime* bdt)
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

static BpDataTime*
get_time_break(ErtsCodeInfo *ci)
{
    GenericBpData* bp = check_break(ci, ERTS_BPF_TIME_TRACE);
    return bp ? bp->time : 0;
}

static GenericBpData*
check_break(ErtsCodeInfo *ci, Uint break_flags)
{
    GenericBp* g = ci->u.gen_bp;

    ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
    if (erts_is_function_native(ci)) {
	return 0;
    }
    if (g) {
	GenericBpData* bp = &g->data[erts_active_bp_ix()];
	ASSERT((bp->flags & ~ERTS_BPF_ALL) == 0);
	if (bp->flags & break_flags) {
	    return bp;
	}
    }
    return 0;
}
