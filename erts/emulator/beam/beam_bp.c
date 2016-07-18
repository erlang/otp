/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
#  define ERTS_SMP_REQ_PROC_MAIN_LOCK(P) \
      if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,\
					 __FILE__, __LINE__)
#  define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P) \
      if ((P)) erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN)
#else
#  define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)
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

erts_smp_atomic32_t erts_active_bp_index;
erts_smp_atomic32_t erts_staging_bp_index;

/*
 * Inlined helpers
 */

static ERTS_INLINE ErtsMonotonicTime
get_mtime(Process *c_p)
{
    return erts_get_monotonic_time(erts_proc_sched_data(c_p));
}

/* *************************************************************************
** Local prototypes
*/

/*
** Helpers
*/
static ErtsTracer do_call_trace(Process* c_p, BeamInstr* I, Eterm* reg,
                                int local, Binary* ms, ErtsTracer tracer);
static void set_break(BpFunctions* f, Binary *match_spec, Uint break_flags,
		      enum erts_break_op count_op, ErtsTracer tracer);
static void set_function_break(BeamInstr *pc,
			       Binary *match_spec,
			       Uint break_flags,
			       enum erts_break_op count_op,
			       ErtsTracer tracer);

static void clear_break(BpFunctions* f, Uint break_flags);
static int clear_function_break(BeamInstr *pc, Uint break_flags);

static BpDataTime* get_time_break(BeamInstr *pc);
static GenericBpData* check_break(BeamInstr *pc, Uint break_flags);

static void bp_meta_unref(BpMetaTracer* bmt);
static void bp_count_unref(BpCount* bcp);
static void bp_time_unref(BpDataTime* bdt);
static void consolidate_bp_data(Module* modp, BeamInstr* pc, int local);
static void uninstall_breakpoint(BeamInstr* pc);

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
    erts_smp_atomic32_init_nob(&erts_active_bp_index, 0);
    erts_smp_atomic32_init_nob(&erts_staging_bp_index, 1);
}


void
erts_bp_match_functions(BpFunctions* f, Eterm mfa[3], int specified)
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
	BeamInstr* code;
	Uint num_functions = (Uint)(UWord) code_hdr->num_functions;
	Uint fi;

	if (specified > 0) {
	    if (mfa[0] != make_atom(module[current]->module)) {
		/* Wrong module name */
		continue;
	    }
	}

	for (fi = 0; fi < num_functions; fi++) {
	    BeamInstr* pc;
	    int wi;

	    code = code_hdr->functions[fi];
	    ASSERT(code[0] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
	    pc = code+5;
	    if (erts_is_native_break(pc)) {
		continue;
	    }
	    if (is_nil(code[3])) { /* Ignore BIF stub */
		continue;
	    }
	    for (wi = 0;
		 wi < specified && (Eterm) code[2+wi] == mfa[wi];
		 wi++) {
		/* Empty loop body */
	    }
	    if (wi == specified) {
		/* Store match */
		f->matching[i].pc = pc;
		f->matching[i].mod = module[current];
		i++;
	    }
	}
    }
    f->matched = i;
    Free(module);
}

void
erts_bp_match_export(BpFunctions* f, Eterm mfa[3], int specified)
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
	int j;

	for (j = 0; j < specified && mfa[j] == ep->code[j]; j++) {
	    /* Empty loop body */
	}
	if (j < specified) {
	    continue;
	}
	pc = ep->code+3;
	if (ep->addressv[code_ix] == pc) {
	    if ((*pc == (BeamInstr) em_apply_bif ||
		 *pc == (BeamInstr) em_call_error_handler)) {
		continue;
	    }
	    ASSERT(*pc == (BeamInstr) BeamOp(op_i_generic_breakpoint));
	} else if (erts_is_native_break(ep->addressv[code_ix])) {
	    continue;
	}

	f->matching[ne].pc = pc;
	f->matching[ne].mod = erts_get_module(ep->code[0], code_ix);
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

    ERTS_SMP_LC_ASSERT(erts_has_code_write_permission());

    for (i = 0; i < n; i++) {
	consolidate_bp_data(fs[i].mod, fs[i].pc, local);
    }
}

void
erts_consolidate_bif_bp_data(void)
{
    int i;

    ERTS_SMP_LC_ASSERT(erts_has_code_write_permission());
    for (i = 0; i < BIF_SIZE; i++) {
	Export *ep = bif_export[i];
	consolidate_bp_data(0, ep->code+3, 0);
    }
}

static void
consolidate_bp_data(Module* modp, BeamInstr* pc, int local)
{
    GenericBp* g = (GenericBp *) pc[-4];
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
	    ASSERT(*pc != (BeamInstr) BeamOp(op_i_generic_breakpoint));
	}
	pc[-4] = 0;
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

    erts_smp_atomic32_set_nob(&erts_active_bp_index, staging);
    erts_smp_atomic32_set_nob(&erts_staging_bp_index, active);
}

void
erts_install_breakpoints(BpFunctions* f)
{
    Uint i;
    Uint n = f->matched;
    BeamInstr br = (BeamInstr) BeamOp(op_i_generic_breakpoint);

    for (i = 0; i < n; i++) {
	BeamInstr* pc = f->matching[i].pc;
	GenericBp* g = (GenericBp *) pc[-4];
	if (*pc != br && g) {
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
	     * aligned word-size (or half-word) writes is atomic
	     * (i.e. that other processes executing this code will not
	     * see a half pointer).
	     */
	    *pc = br;
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
	BeamInstr* pc = f->matching[i].pc;
	uninstall_breakpoint(pc);
    }
}

static void
uninstall_breakpoint(BeamInstr* pc)
{
    if (*pc == (BeamInstr) BeamOp(op_i_generic_breakpoint)) {
	GenericBp* g = (GenericBp *) pc[-4];
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
erts_set_call_trace_bif(BeamInstr *pc, Binary *match_spec, int local)
{
    Uint flags = local ? ERTS_BPF_LOCAL_TRACE : ERTS_BPF_GLOBAL_TRACE;

    set_function_break(pc, match_spec, flags, 0, erts_tracer_nil);
}

void
erts_set_mtrace_bif(BeamInstr *pc, Binary *match_spec, ErtsTracer tracer)
{
    set_function_break(pc, match_spec, ERTS_BPF_META_TRACE, 0, tracer);
}

void
erts_set_time_trace_bif(BeamInstr *pc, enum erts_break_op count_op)
{
    set_function_break(pc, NULL,
		       ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE,
		       count_op, erts_tracer_nil);
}

void
erts_clear_time_trace_bif(BeamInstr *pc) {
    clear_function_break(pc, ERTS_BPF_TIME_TRACE|ERTS_BPF_TIME_TRACE_ACTIVE);
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
erts_clear_call_trace_bif(BeamInstr *pc, int local)
{
    GenericBp* g = (GenericBp *) pc[-4];

    if (g) {
	Uint flags = local ? ERTS_BPF_LOCAL_TRACE : ERTS_BPF_GLOBAL_TRACE;
	if (g->data[erts_staging_bp_ix()].flags & flags) {
	    clear_function_break(pc, flags);
	}
    }
}

void
erts_clear_mtrace_break(BpFunctions* f)
{
    clear_break(f, ERTS_BPF_META_TRACE);
}

void
erts_clear_mtrace_bif(BeamInstr *pc)
{
    clear_function_break(pc, ERTS_BPF_META_TRACE);
}

void
erts_clear_debug_break(BpFunctions* f)
{
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
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

    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
    ASSERT(modp);
    code_hdr = modp->curr.code_hdr;
    if (!code_hdr) {
	return 0;
    }
    n = (Uint)(UWord) code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
	BeamInstr* pc;

	pc = code_hdr->functions[i] + 5;
	if (erts_is_native_break(pc)) {
	    continue;
	}
	clear_function_break(pc, ERTS_BPF_ALL);
    }

    erts_commit_staged_bp();

    for (i = 0; i < n; ++i) {
	BeamInstr* pc;

	pc = code_hdr->functions[i] + 5;
	if (erts_is_native_break(pc)) {
	    continue;
	}
	uninstall_breakpoint(pc);
	consolidate_bp_data(modp, pc, 1);
	ASSERT(pc[-4] == 0);
    }
    return n;
}

void
erts_clear_export_break(Module* modp, BeamInstr* pc)
{
    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());

    clear_function_break(pc, ERTS_BPF_ALL);
    erts_commit_staged_bp();
    *pc = (BeamInstr) 0;
    consolidate_bp_data(modp, pc, 0);
    ASSERT(pc[-4] == 0);
}

BeamInstr
erts_generic_breakpoint(Process* c_p, BeamInstr* I, Eterm* reg)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint bp_flags;
    ErtsBpIndex ix = erts_active_bp_ix();

    g = (GenericBp *) I[-4];
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
	(void) do_call_trace(c_p, I, reg, 1, bp->local_ms, erts_tracer_true);
    } else if (bp_flags & ERTS_BPF_GLOBAL_TRACE) {
	(void) do_call_trace(c_p, I, reg, 0, bp->local_ms, erts_tracer_true);
    }

    if (bp_flags & ERTS_BPF_META_TRACE) {
	ErtsTracer old_tracer, new_tracer;

	old_tracer = erts_smp_atomic_read_nob(&bp->meta_tracer->tracer);

	new_tracer = do_call_trace(c_p, I, reg, 1, bp->meta_ms, old_tracer);
	if (!ERTS_TRACER_COMPARE(new_tracer, old_tracer)) {
            if (old_tracer == erts_smp_atomic_cmpxchg_acqb(
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
	erts_smp_atomic_inc_nob(&bp->count->acount);
    }

    if (bp_flags & ERTS_BPF_TIME_TRACE_ACTIVE) {
	Eterm w;
	erts_trace_time_call(c_p, I, bp->time);
	w = (BeamInstr) *c_p->cp;
	if (! (w == (BeamInstr) BeamOp(op_i_return_time_trace) ||
	       w == (BeamInstr) BeamOp(op_return_trace) ||
	       w == (BeamInstr) BeamOp(op_i_return_to_trace)) ) {
	    Eterm* E = c_p->stop;
	    ASSERT(c_p->htop <= E && E <= c_p->hend);
	    if (E - 2 < c_p->htop) {
		(void) erts_garbage_collect(c_p, 2, reg, I[-1]);
		ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	    }
	    E = c_p->stop;

	    ASSERT(c_p->htop <= E && E <= c_p->hend);

	    E -= 2;
	    E[0] = make_cp(I);
	    E[1] = make_cp(c_p->cp);     /* original return address */
	    c_p->cp = beam_return_time_trace;
	    c_p->stop = E;
	}
    }

    if (bp_flags & ERTS_BPF_DEBUG) {
	return (BeamInstr) BeamOp(op_i_debug_breakpoint);
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
    int applying = (I == &(ep->code[3])); /* Yup, the apply code for a bif
					   * is actually in the
					   * export entry */
    BeamInstr *cp = p->cp;
    GenericBp* g;
    GenericBpData* bp = NULL;
    Uint bp_flags = 0;

    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);

    g = (GenericBp *) ep->fake_op_func_info_for_hipe[1];
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
    }
    if (bp_flags & (ERTS_BPF_LOCAL_TRACE|ERTS_BPF_GLOBAL_TRACE) &&
	IS_TRACED_FL(p, F_TRACE_CALLS)) {
	int local = !!(bp_flags & ERTS_BPF_LOCAL_TRACE);
	flags = erts_call_trace(p, ep->code, bp->local_ms, args,
				local, &ERTS_TRACER(p));
    }
    if (bp_flags & ERTS_BPF_META_TRACE) {
	ErtsTracer old_tracer;

        meta_tracer = erts_smp_atomic_read_nob(&bp->meta_tracer->tracer);
        old_tracer = meta_tracer;
	flags_meta = erts_call_trace(p, ep->code, bp->meta_ms, args,
				     0, &meta_tracer);

	if (!ERTS_TRACER_COMPARE(old_tracer, meta_tracer)) {
            ErtsTracer new_tracer = erts_tracer_nil;
            erts_tracer_update(&new_tracer, meta_tracer);
	    if (old_tracer == erts_smp_atomic_cmpxchg_acqb(
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
	BeamInstr *pc = (BeamInstr *)ep->code+3;
	erts_trace_time_call(p, pc, bp->time);
    }

    /* Restore original continuation pointer (if changed). */
    p->cp = cp;

    func = bif_table[bif_index].f;

    result = func(p, args, I);

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
		erts_trace_exception(p, ep->code, class, value,
				     &meta_tracer);
	    }
	    if (flags & MATCH_SET_EXCEPTION_TRACE) {
		erts_trace_exception(p, ep->code, class, value,
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
		erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
		ERTS_TRACE_FLAGS(p) |= F_EXCEPTION_TRACE;
		erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
	    }
	}
    } else {
	if (flags_meta & MATCH_SET_RX_TRACE) {
	    erts_trace_return(p, ep->code, result, &meta_tracer);
	}
	/* MATCH_SET_RETURN_TO_TRACE cannot occur if(meta) */
	if (flags & MATCH_SET_RX_TRACE) {
	    erts_trace_return(p, ep->code, result, &ERTS_TRACER(p));
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
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
    return result;
}

static ErtsTracer
do_call_trace(Process* c_p, BeamInstr* I, Eterm* reg,
	      int local, Binary* ms, ErtsTracer tracer)
{
    Eterm* cpp;
    int return_to_trace = 0;
    BeamInstr w;
    BeamInstr *cp_save = c_p->cp;
    Uint32 flags;
    Uint need = 0;
    Eterm* E = c_p->stop;

    w = *c_p->cp;
    if (w == (BeamInstr) BeamOp(op_return_trace)) {
	cpp = &E[2];
    } else if (w == (BeamInstr) BeamOp(op_i_return_to_trace)) {
	return_to_trace = 1;
	cpp = &E[0];
    } else if (w == (BeamInstr) BeamOp(op_i_return_time_trace)) {
	cpp = &E[0];
    } else {
	cpp = NULL;
    }
    if (cpp) {
	for (;;) {
	    BeamInstr w = *cp_val(*cpp);
	    if (w == (BeamInstr) BeamOp(op_return_trace)) {
		cpp += 3;
	    } else if (w == (BeamInstr) BeamOp(op_i_return_to_trace)) {
		return_to_trace = 1;
		cpp += 1;
	    } else if (w == (BeamInstr) BeamOp(op_i_return_time_trace)) {
		cpp += 2;
	    } else {
		break;
	    }
	}
	cp_save = c_p->cp;
	c_p->cp = (BeamInstr *) cp_val(*cpp);
	ASSERT(is_CP(*cpp));
    }
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
    flags = erts_call_trace(c_p, I-3, ms, reg, local, &tracer);
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    if (cpp) {
	c_p->cp = cp_save;
    }

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
	    (void) erts_garbage_collect(c_p, need, reg, I[-1]);
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
	ASSERT(is_CP((Eterm) (UWord) (I - 3)));
	ASSERT(IS_TRACER_VALID(tracer));
	E[2] = make_cp(c_p->cp);
        E[1] = copy_object(tracer, c_p);
	E[0] = make_cp(I - 3); /* We ARE at the beginning of an
				  instruction,
				  the funcinfo is above i. */
	c_p->cp = (flags & MATCH_SET_EXCEPTION_TRACE) ?
	    beam_exception_trace : beam_return_trace;
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	ERTS_TRACE_FLAGS(c_p) |= F_EXCEPTION_TRACE;
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
    } else
        c_p->stop = E;
    return tracer;
}

void
erts_trace_time_call(Process* c_p, BeamInstr* I, BpDataTime* bdt)
{
    ErtsMonotonicTime time;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;

    ASSERT(c_p);
    ASSERT(erts_smp_atomic32_read_acqb(&c_p->state) & (ERTS_PSFLG_RUNNING
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
    } else {
	ASSERT(pbt->pc);
	/* add time to previous code */
	sitem.time = time - pbt->time;
	sitem.pid = c_p->common.id;
	sitem.count = 0;

	/* previous breakpoint */
	pbdt = get_time_break(pbt->pc);

	/* if null then the breakpoint was removed */
	if (pbdt) {
	    h = &(pbdt->hash[bp_sched2ix_proc(c_p)]);

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

    /* Add count to this code */
    sitem.pid     = c_p->common.id;
    sitem.count   = 1;
    sitem.time    = 0;

    /* this breakpoint */
    ASSERT(bdt);
    h = &(bdt->hash[bp_sched2ix_proc(c_p)]);

    ASSERT(h);
    ASSERT(h->item);

    item = bp_hash_get(h, &sitem);
    if (!item) {
	item = bp_hash_put(h, &sitem);
    } else {
	BP_TIME_ADD(item, &sitem);
    }

    pbt->pc = I;
    pbt->time = time;
}

void
erts_trace_time_return(Process *p, BeamInstr *pc)
{
    ErtsMonotonicTime time;
    process_breakpoint_time_t *pbt = NULL;
    bp_data_time_item_t sitem, *item = NULL;
    bp_time_hash_t *h = NULL;
    BpDataTime *pbdt = NULL;

    ASSERT(p);
    ASSERT(erts_smp_atomic32_read_acqb(&p->state) & (ERTS_PSFLG_RUNNING
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
	ASSERT(pbt->pc);

	sitem.time = time - pbt->time;
	sitem.pid   = p->common.id;
	sitem.count = 0;

	/* previous breakpoint */
	pbdt = get_time_break(pbt->pc);

	/* beware, the trace_pattern might have been removed */
	if (pbdt) {
	    h = &(pbdt->hash[bp_sched2ix_proc(p)]);

	    ASSERT(h);
	    ASSERT(h->item);

	    item = bp_hash_get(h, &sitem);
	    if (!item) {
		item = bp_hash_put(h, &sitem);
	    } else {
		BP_TIME_ADD(item, &sitem);
	    }
	}

	pbt->pc = pc;
	pbt->time = time;
    }
}

int 
erts_is_trace_break(BeamInstr *pc, Binary **match_spec_ret, int local)
{
    Uint flags = local ? ERTS_BPF_LOCAL_TRACE : ERTS_BPF_GLOBAL_TRACE;
    GenericBpData* bp = check_break(pc, flags);

    if (bp) {
	if (match_spec_ret) {
	    *match_spec_ret = bp->local_ms;
	}
	return 1;
    }
    return 0;
}

int
erts_is_mtrace_break(BeamInstr *pc, Binary **match_spec_ret,
		     ErtsTracer *tracer_ret)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_META_TRACE);
    
    if (bp) {
	if (match_spec_ret) {
	    *match_spec_ret = bp->meta_ms;
	}
	if (tracer_ret) {
            *tracer_ret = erts_smp_atomic_read_nob(&bp->meta_tracer->tracer);
	}
	return 1;
    }
    return 0;
}

int
erts_is_native_break(BeamInstr *pc) {
#ifdef HIPE
    ASSERT(pc[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
    return pc[0]  == (BeamInstr) BeamOp(op_hipe_trap_call)
	|| pc[0]  == (BeamInstr) BeamOp(op_hipe_trap_call_closure);
#else
    return 0;
#endif
}

int 
erts_is_count_break(BeamInstr *pc, Uint *count_ret)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_COUNT);
    
    if (bp) {
	if (count_ret) {
	    *count_ret = (Uint) erts_smp_atomic_read_nob(&bp->count->acount);
	}
	return 1;
    }
    return 0;
}

int erts_is_time_break(Process *p, BeamInstr *pc, Eterm *retval) {
    Uint i, ix;
    bp_time_hash_t hash;
    Uint size;
    Eterm *hp, t;
    bp_data_time_item_t *item = NULL;
    BpDataTime *bdt = get_time_break(pc);

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


BeamInstr *
erts_find_local_func(Eterm mfa[3]) {
    Module *modp;
    BeamCodeHeader* code_hdr;
    BeamInstr* code_ptr;
    Uint i,n;

    if ((modp = erts_get_module(mfa[0], erts_active_code_ix())) == NULL)
	return NULL;
    if ((code_hdr = modp->curr.code_hdr) == NULL)
	return NULL;
    n = (BeamInstr) code_hdr->num_functions;
    for (i = 0; i < n; ++i) {
	code_ptr = code_hdr->functions[i];
	ASSERT(((BeamInstr) BeamOp(op_i_func_info_IaaI)) == code_ptr[0]);
	ASSERT(mfa[0] == ((Eterm) code_ptr[2]) ||
	       is_nil((Eterm) code_ptr[2]));
	if (mfa[1] == ((Eterm) code_ptr[3]) &&
	    ((BeamInstr) mfa[2]) == code_ptr[4]) {
	    return code_ptr + 5;
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

	    pbdt = get_time_break(pbt->pc);
	    if (pbdt) {
		sitem.time = get_mtime(p) - pbt->time;
		sitem.pid   = p->common.id;
		sitem.count = 0;

		h = &(pbdt->hash[bp_sched2ix_proc(p)]);

		ASSERT(h);
		ASSERT(h->item);

		item = bp_hash_get(h, &sitem);
		if (!item) {
		    item = bp_hash_put(h, &sitem);
		} else {
		    BP_TIME_ADD(item, &sitem);
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
	BeamInstr* pc = f->matching[i].pc;
	set_function_break(pc, match_spec, break_flags,
			   count_op, tracer);
    }
}

static void
set_function_break(BeamInstr *pc, Binary *match_spec, Uint break_flags,
		   enum erts_break_op count_op, ErtsTracer tracer)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint common;
    ErtsBpIndex ix = erts_staging_bp_ix();

    ERTS_SMP_LC_ASSERT(erts_has_code_write_permission());
    g = (GenericBp *) pc[-4];
    if (g == 0) {
	int i;
	if (count_op == ERTS_BREAK_RESTART || count_op == ERTS_BREAK_PAUSE) {
	    /* Do not insert a new breakpoint */
	    return;
	}
	g = Alloc(sizeof(GenericBp));
	g->orig_instr = *pc;
	for (i = 0; i < ERTS_NUM_BP_IX; i++) {
	    g->data[i].flags = 0;
	}
	pc[-4] = (BeamInstr) g;
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
	    erts_smp_atomic_set_nob(&bp->count->acount, 0);
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
	erts_smp_atomic_init_nob(&bmt->tracer, (erts_aint_t)meta_tracer);
	bp->meta_tracer = bmt;
    } else if (break_flags & ERTS_BPF_COUNT) {
	BpCount* bcp;

	ASSERT((bp->flags & ERTS_BPF_COUNT) == 0);
	bcp = Alloc(sizeof(BpCount));
	erts_refc_init(&bcp->refc, 1);
	erts_smp_atomic_init_nob(&bcp->acount, 0);
	bp->count = bcp;
    } else if (break_flags & ERTS_BPF_TIME_TRACE) {
	BpDataTime* bdt;
	int i;

	ASSERT((bp->flags & ERTS_BPF_TIME_TRACE) == 0);
	bdt = Alloc(sizeof(BpDataTime));
	erts_refc_init(&bdt->refc, 1);
	bdt->n = erts_no_schedulers;
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
	BeamInstr* pc = f->matching[i].pc;
	clear_function_break(pc, break_flags);
    }
}

static int
clear_function_break(BeamInstr *pc, Uint break_flags)
{
    GenericBp* g;
    GenericBpData* bp;
    Uint common;
    ErtsBpIndex ix = erts_staging_bp_ix();

    ERTS_SMP_LC_ASSERT(erts_has_code_write_permission());

    if ((g = (GenericBp *) pc[-4]) == 0) {
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
        ErtsTracer trc = erts_smp_atomic_read_nob(&bmt->tracer);
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
	Uint j = 0;
	Process *h_p = NULL;
	bp_data_time_item_t* item = NULL;
	process_breakpoint_time_t* pbt = NULL;

	/* remove all psd associated with the hash
	 * and then delete the hash.
	 * ... sigh ...
	 */

	for (i = 0; i < bdt->n; ++i) {
	    if (bdt->hash[i].used) {
		for (j = 0; j < bdt->hash[i].n; ++j) {
		    item = &(bdt->hash[i].item[j]);
		    if (item->pid != NIL) {
			h_p = erts_pid2proc(NULL, 0, item->pid,
					    ERTS_PROC_LOCK_MAIN);
			if (h_p) {
			    pbt = ERTS_PROC_SET_CALL_TIME(h_p, NULL);
			    if (pbt) {
				Free(pbt);
			    }
			    erts_smp_proc_unlock(h_p, ERTS_PROC_LOCK_MAIN);
			}
		    }
		}
	    }
	    bp_hash_delete(&(bdt->hash[i]));
	}
	Free(bdt->hash);
	Free(bdt);
    }
}

static BpDataTime*
get_time_break(BeamInstr *pc)
{
    GenericBpData* bp = check_break(pc, ERTS_BPF_TIME_TRACE);
    return bp ? bp->time : 0;
}

static GenericBpData*
check_break(BeamInstr *pc, Uint break_flags)
{
    GenericBp* g = (GenericBp *) pc[-4];

    ASSERT(pc[-5] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
    if (erts_is_native_break(pc)) {
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
