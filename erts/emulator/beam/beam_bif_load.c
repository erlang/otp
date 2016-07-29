/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
#include "error.h"
#include "bif.h"
#include "beam_load.h"
#include "big.h"
#include "beam_bp.h"
#include "beam_catches.h"
#include "erl_binary.h"
#include "erl_nif.h"
#include "erl_bits.h"
#include "erl_thr_progress.h"

#ifdef HIPE
#  include "hipe_stack.h"
#endif

static void set_default_trace_pattern(Eterm module);
static Eterm check_process_code(Process* rp, Module* modp, Uint flags, int *redsp, int fcalls);
static void delete_code(Module* modp);
static void decrement_refc(BeamCodeHeader*);
static int any_heap_ref_ptrs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);
static int any_heap_refs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);

BIF_RETTYPE code_is_module_native_1(BIF_ALIST_1)
{
    Module* modp;
    Eterm res;
    ErtsCodeIndex code_ix;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
    if ((modp = erts_get_module(BIF_ARG_1, code_ix)) == NULL) {
	return am_undefined;
    }
    erts_rlock_old_code(code_ix);
    res = (erts_is_module_native(modp->curr.code_hdr) ||
           erts_is_module_native(modp->old.code_hdr)) ?
		am_true : am_false;
    erts_runlock_old_code(code_ix);
    return res;
}

BIF_RETTYPE code_make_stub_module_3(BIF_ALIST_3)
{
    Module* modp;
    Eterm res;

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD3(bif_export[BIF_code_make_stub_module_3],
			BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    modp = erts_get_module(BIF_ARG_1, erts_active_code_ix());

    if (modp && modp->curr.num_breakpoints > 0) {
	ASSERT(modp->curr.code_hdr != NULL);
	erts_clear_module_break(modp);
	ASSERT(modp->curr.num_breakpoints == 0);
    }

    erts_start_staging_code_ix(1);

    res = erts_make_stub_module(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (res == BIF_ARG_1) {
	erts_end_staging_code_ix();
	erts_commit_staging_code_ix();
    }
    else {
	erts_abort_staging_code_ix();
    }
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_release_code_write_permission();
    return res;
}

BIF_RETTYPE
prepare_loading_2(BIF_ALIST_2)
{
    byte* temp_alloc = NULL;
    byte* code;
    Uint sz;
    Binary* magic;
    Eterm reason;
    Eterm* hp;
    Eterm res;

    if (is_not_atom(BIF_ARG_1)) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((code = erts_get_aligned_binary_bytes(BIF_ARG_2, &temp_alloc)) == NULL) {
	goto error;
    }

    magic = erts_alloc_loader_state();
    sz = binary_size(BIF_ARG_2);
    reason = erts_prepare_loading(magic, BIF_P, BIF_P->group_leader,
				  &BIF_ARG_1, code, sz);
    erts_free_aligned_binary_bytes(temp_alloc);
    if (reason != NIL) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_error, reason);
	BIF_RET(res);
    }
    hp = HAlloc(BIF_P, PROC_BIN_SIZE);
    res = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), magic);
    erts_refc_dec(&magic->refc, 1);
    BIF_RET(res);
}

BIF_RETTYPE
has_prepared_code_on_load_1(BIF_ALIST_1)
{
    Eterm res;
    ProcBin* pb;

    if (!ERTS_TERM_IS_MAGIC_BINARY(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }

    pb = (ProcBin*) binary_val(BIF_ARG_1);
    res = erts_has_code_on_load(pb->val);
    if (res == NIL) {
	goto error;
    }
    BIF_RET(res);
}

struct m {
    Binary* code;
    Eterm module;
    Module* modp;
    Uint exception;
};

static Eterm staging_epilogue(Process* c_p, int, Eterm res, int, struct m*, int);
#ifdef ERTS_SMP
static void smp_code_ix_commiter(void*);

static struct /* Protected by code_write_permission */
{
    Process* stager;
    ErtsThrPrgrLaterOp lop;
} committer_state;
#endif

static Eterm
exception_list(Process* p, Eterm tag, struct m* mp, Sint exceptions)
{
    Eterm* hp = HAlloc(p, 3 + 2*exceptions);
    Eterm res = NIL;

    while (exceptions > 0) {
	if (mp->exception) {
	    res = CONS(hp, mp->module, res);
	    hp += 2;
	    exceptions--;
	}
	mp++;
    }
    return TUPLE2(hp, tag, res);
}


BIF_RETTYPE
finish_loading_1(BIF_ALIST_1)
{
    Sint i;
    Sint n;
    struct m* p = NULL;
    Uint exceptions;
    Eterm res;
    int is_blocking = 0;
    int do_commit = 0;

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD1(bif_export[BIF_finish_loading_1], BIF_P, BIF_ARG_1);
    }

    /*
     * Validate the argument before we start loading; it must be a
     * proper list where each element is a magic binary containing
     * prepared (not previously loaded) code.
     *
     * First count the number of elements and allocate an array
     * to keep the elements in.
     */

    n = erts_list_length(BIF_ARG_1);
    if (n < 0) {
    badarg:
	if (p) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, p);
	}
	erts_release_code_write_permission();
	BIF_ERROR(BIF_P, BADARG);
    }
    p = erts_alloc(ERTS_ALC_T_LOADER_TMP, n*sizeof(struct m));

    /*
     * We now know that the argument is a proper list. Validate
     * and collect the binaries into the array.
     */

    for (i = 0; i < n; i++) {
	Eterm* cons = list_val(BIF_ARG_1);
	Eterm term = CAR(cons);
	ProcBin* pb;

	if (!ERTS_TERM_IS_MAGIC_BINARY(term)) {
	    goto badarg;
	}
	pb = (ProcBin*) binary_val(term);
	p[i].code = pb->val;
	p[i].module = erts_module_for_prepared_code(p[i].code);
	if (p[i].module == NIL) {
	    goto badarg;
	}
	BIF_ARG_1 = CDR(cons);
    }

    /*
     * Since we cannot handle atomic loading of a group of modules
     * if one or more of them uses on_load, we will only allow
     * more than one element in the list if none of the modules
     * have an on_load function.
     */

    if (n > 1) {
	for (i = 0; i < n; i++) {
	    if (erts_has_code_on_load(p[i].code) == am_true) {
		erts_free(ERTS_ALC_T_LOADER_TMP, p);
		erts_release_code_write_permission();
		BIF_ERROR(BIF_P, SYSTEM_LIMIT);
	    }
	}
    }

    /*
     * All types are correct. There cannot be a BADARG from now on.
     * Before we can start loading, we must check whether any of
     * the modules already has old code. To avoid a race, we must
     * not allow other process to initiate a code loading operation
     * from now on.
     */

    res = am_ok;
    erts_start_staging_code_ix(n);

    for (i = 0; i < n; i++) {
	p[i].modp = erts_put_module(p[i].module);
	p[i].modp->seen = 0;
    }

    exceptions = 0;
    for (i = 0; i < n; i++) {
	p[i].exception = 0;
	if (p[i].modp->seen) {
	    p[i].exception = 1;
	    exceptions++;
	}
	p[i].modp->seen = 1;
    }
    if (exceptions) {
	res = exception_list(BIF_P, am_duplicated, p, exceptions);
	goto done;
    }

    for (i = 0; i < n; i++) {
	if (p[i].modp->curr.num_breakpoints > 0 ||
	    p[i].modp->curr.num_traced_exports > 0 ||
	    erts_is_default_trace_enabled()) {
	    /* tracing involved, fallback with thread blocking */
	    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    erts_smp_thr_progress_block();
	    is_blocking = 1;
	    break;
	}
    }

    if (is_blocking) {
	for (i = 0; i < n; i++) {
	    if (p[i].modp->curr.num_breakpoints) {
		erts_clear_module_break(p[i].modp);
		ASSERT(p[i].modp->curr.num_breakpoints == 0);
	    }
	}
    }

    exceptions = 0;
    for (i = 0; i < n; i++) {
	p[i].exception = 0;
	if (p[i].modp->curr.code_hdr && p[i].modp->old.code_hdr) {
	    p[i].exception = 1;
	    exceptions++;
	}
    }

    if (exceptions) {
	res = exception_list(BIF_P, am_not_purged, p, exceptions);
    } else {
	/*
	 * Now we can load all code. This can't fail.
	 */

	exceptions = 0;
	for (i = 0; i < n; i++) {
	    Eterm mod;
	    Eterm retval;

	    erts_refc_inc(&p[i].code->refc, 1);
	    retval = erts_finish_loading(p[i].code, BIF_P, 0, &mod);
	    ASSERT(retval == NIL || retval == am_on_load);
	    if (retval == am_on_load) {
		p[i].exception = 1;
		exceptions++;
	    }
	}

	/*
	 * Check whether any module has an on_load_handler.
	 */

	if (exceptions) {
	    res = exception_list(BIF_P, am_on_load, p, exceptions);
	}
	do_commit = 1;
    }

done:
    return staging_epilogue(BIF_P, do_commit, res, is_blocking, p, n);
}

static Eterm
staging_epilogue(Process* c_p, int commit, Eterm res, int is_blocking,
		 struct m* loaded, int nloaded)
{    
#ifdef ERTS_SMP
    if (is_blocking || !commit)
#endif
    {
	if (commit) {
	    erts_end_staging_code_ix();
	    erts_commit_staging_code_ix();
	    if (loaded) {
		int i;
		for (i=0; i < nloaded; i++) {		
		    set_default_trace_pattern(loaded[i].module);
		}
	    }
	}
	else {
	    erts_abort_staging_code_ix();
	}
	if (loaded) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, loaded);
	}
	if (is_blocking) {
	    erts_smp_thr_progress_unblock();
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
	}
	erts_release_code_write_permission();
	return res;
    }
#ifdef ERTS_SMP
    else {
	ASSERT(is_value(res));

	if (loaded) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, loaded);
	}
	erts_end_staging_code_ix();
	/*
	 * Now we must wait for all schedulers to do a memory barrier before
	 * we can commit and let them access the new staged code. This allows
	 * schedulers to read active code_ix in a safe way while executing
	 * without any memory barriers at all. 
	 */
	ASSERT(committer_state.stager == NULL);
	committer_state.stager = c_p;
	erts_schedule_thr_prgr_later_op(smp_code_ix_commiter, NULL, &committer_state.lop);
	erts_proc_inc_refc(c_p);
	erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
	/*
	 * smp_code_ix_commiter() will do the rest "later"
	 * and resume this process to return 'res'.  
	 */
	ERTS_BIF_YIELD_RETURN(c_p, res);
    }
#endif
}


#ifdef ERTS_SMP
static void smp_code_ix_commiter(void* null)
{
    Process* p = committer_state.stager;

    erts_commit_staging_code_ix();
#ifdef DEBUG
    committer_state.stager = NULL;
#endif
    erts_release_code_write_permission();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (!ERTS_PROC_IS_EXITING(p)) {
	erts_resume(p, ERTS_PROC_LOCK_STATUS);
    }
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_proc_dec_refc(p);
}
#endif /* ERTS_SMP */



BIF_RETTYPE
check_old_code_1(BIF_ALIST_1)
{
    ErtsCodeIndex code_ix;
    Module* modp;
    Eterm res = am_false;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);
    if (modp != NULL) {
	erts_rlock_old_code(code_ix);
	if (modp->old.code_hdr) {
	    res = am_true;
	}
	erts_runlock_old_code(code_ix);
    }
    BIF_RET(res);
}

Eterm
erts_check_process_code(Process *c_p, Eterm module, Uint flags, int *redsp, int fcalls)
{
    Module* modp;
    Eterm res;
    ErtsCodeIndex code_ix;

    (*redsp)++;

    ASSERT(is_atom(module));

    code_ix = erts_active_code_ix();
    modp = erts_get_module(module, code_ix);
    if (!modp)
	return am_false;
    erts_rlock_old_code(code_ix);
    res = (!modp->old.code_hdr ? am_false :
           check_process_code(c_p, modp, flags, redsp, fcalls));
    erts_runlock_old_code(code_ix);

    return res;
}

BIF_RETTYPE erts_internal_check_process_code_2(BIF_ALIST_2)
{
    int reds = 0;
    Uint flags;
    Eterm res;

    if (is_not_atom(BIF_ARG_1))
	goto badarg;

    if (is_not_small(BIF_ARG_2))
        goto badarg;

    flags = unsigned_val(BIF_ARG_2);
    if (flags & ~ERTS_CPC_ALL) {
        goto badarg;
    }

    res = erts_check_process_code(BIF_P, BIF_ARG_1, flags, &reds, BIF_P->fcalls);

    ASSERT(is_value(res));

    BIF_RET2(res, reds);

badarg:
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE delete_module_1(BIF_ALIST_1)
{
    ErtsCodeIndex code_ix;
    Module* modp;
    int is_blocking = 0;
    int success = 0;
    Eterm res = NIL;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD1(bif_export[BIF_delete_module_1], BIF_P, BIF_ARG_1);
    }

    {
	erts_start_staging_code_ix(0);
	code_ix = erts_staging_code_ix();
	modp = erts_get_module(BIF_ARG_1, code_ix);
	if (!modp) {
	    res = am_undefined;
	}
	else if (modp->old.code_hdr) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Module %T must be purged before loading\n",
			  BIF_ARG_1);
	    erts_send_error_to_logger(BIF_P->group_leader, dsbufp);
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	}
	else {
	    if (modp->curr.num_breakpoints > 0 ||
		modp->curr.num_traced_exports > 0) {
		/* we have tracing, retry single threaded */
		erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		erts_smp_thr_progress_block();
		is_blocking = 1;
		if (modp->curr.num_breakpoints) {
		    erts_clear_module_break(modp);
		    ASSERT(modp->curr.num_breakpoints == 0);
		}
	    }
	    delete_code(modp);
	    res = am_true;
	    success = 1;
	}
    }
    return staging_epilogue(BIF_P, success, res, is_blocking, NULL, 0);  
}

BIF_RETTYPE module_loaded_1(BIF_ALIST_1)
{
    Module* modp;
    ErtsCodeIndex code_ix;
    Eterm res = am_false;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
    if ((modp = erts_get_module(BIF_ARG_1, code_ix)) != NULL) {
	if (modp->curr.code_hdr
	    && modp->curr.code_hdr->on_load_function_ptr == NULL) {
	    res = am_true;
	}
    }
    BIF_RET(res);
}

BIF_RETTYPE pre_loaded_0(BIF_ALIST_0)
{
    return erts_preloaded(BIF_P);
}

BIF_RETTYPE loaded_0(BIF_ALIST_0)
{
    ErtsCodeIndex code_ix = erts_active_code_ix();
    Module* modp;
    Eterm previous = NIL;
    Eterm* hp;
    int i;
    int j = 0;

    for (i = 0; i < module_code_size(code_ix); i++) {
	if ((modp = module_code(i, code_ix)) != NULL &&
	    ((modp->curr.code_length != 0) ||
	     (modp->old.code_length != 0))) {
	    j++;
	}
    }
    if (j > 0) {
	hp = HAlloc(BIF_P, j*2);

	for (i = 0; i < module_code_size(code_ix); i++) {
	    if ((modp=module_code(i,code_ix)) != NULL &&
		((modp->curr.code_length != 0) ||
		 (modp->old.code_length != 0))) {
		previous = CONS(hp, make_atom(modp->module), previous);
		hp += 2;
	    }
	}
    }
    BIF_RET(previous);
}

BIF_RETTYPE call_on_load_function_1(BIF_ALIST_1)
{
    Module* modp = erts_get_module(BIF_ARG_1, erts_active_code_ix());

    if (modp && modp->old.code_hdr) {
	BIF_TRAP_CODE_PTR_0(BIF_P, modp->old.code_hdr->on_load_function_ptr);
    }
    else {
	BIF_ERROR(BIF_P, BADARG);
    }
}

BIF_RETTYPE finish_after_on_load_2(BIF_ALIST_2)
{
    ErtsCodeIndex code_ix;
    Module* modp;

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD2(bif_export[BIF_finish_after_on_load_2],
			BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    /* ToDo: Use code_ix staging instead of thread blocking */

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);

    if (!modp || !modp->old.code_hdr) {
    error:
	erts_smp_thr_progress_unblock();
        erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_release_code_write_permission();
	BIF_ERROR(BIF_P, BADARG);
    }
    if (modp->old.code_hdr->on_load_function_ptr == NULL) {
	goto error;
    }
    if (BIF_ARG_2 != am_false && BIF_ARG_2 != am_true) {
	goto error;
    }

    if (BIF_ARG_2 == am_true) {
	int i;
	struct erl_module_instance t;

	/*
	 * Swap old and new code.
	 */
	t = modp->curr;
	modp->curr = modp->old;
	modp->old = t;

	/*
	 * The on_load function succeded. Fix up export entries.
	 */
	for (i = 0; i < export_list_size(code_ix); i++) {
	    Export *ep = export_list(i,code_ix);
	    if (ep == NULL || ep->code[0] != BIF_ARG_1) {
		continue;
	    }
	    if (ep->code[4] != 0) {
		ep->addressv[code_ix] = (void *) ep->code[4];
		ep->code[4] = 0;
	    } else {
		if (ep->addressv[code_ix] == ep->code+3 &&
		    ep->code[3] == (BeamInstr) em_apply_bif) {
		    continue;
		}
		ep->addressv[code_ix] = ep->code+3;
		ep->code[3] = (BeamInstr) em_call_error_handler;
	    }
	}
	modp->curr.code_hdr->on_load_function_ptr = NULL;
	set_default_trace_pattern(BIF_ARG_1);
    } else if (BIF_ARG_2 == am_false) {
	int i;

	/*
	 * The on_load function failed. Remove references to the
	 * code that is about to be purged from the export entries.
	 */

	for (i = 0; i < export_list_size(code_ix); i++) {
	    Export *ep = export_list(i,code_ix);
	    if (ep == NULL || ep->code[0] != BIF_ARG_1) {
		continue;
	    }
	    if (ep->code[3] == (BeamInstr) em_apply_bif) {
		continue;
	    }
	    ep->code[4] = 0;
	}
    }
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_release_code_write_permission();
    BIF_RET(am_true);
}

static void
set_default_trace_pattern(Eterm module)
{
    int trace_pattern_is_on;
    Binary *match_spec;
    Binary *meta_match_spec;
    struct trace_pattern_flags trace_pattern_flags;
    ErtsTracer meta_tracer;

    erts_get_default_trace_pattern(&trace_pattern_is_on,
				   &match_spec,
				   &meta_match_spec,
				   &trace_pattern_flags,
				   &meta_tracer);
    if (trace_pattern_is_on) {
	Eterm mfa[1];
	mfa[0] = module;
	(void) erts_set_trace_pattern(0, mfa, 1,
				      match_spec,
				      meta_match_spec,
				      1, trace_pattern_flags,
				      meta_tracer, 1);
    }
}

static ERTS_INLINE int
check_mod_funs(Process *p, ErlOffHeap *off_heap, char *area, size_t area_size)
{
    struct erl_off_heap_header* oh;
    for (oh = off_heap->first; oh; oh = oh->next) {
	if (thing_subtag(oh->thing_word) == FUN_SUBTAG) {
	    ErlFunThing* funp = (ErlFunThing*) oh;
	    if (ErtsInArea(funp->fe->address, area, area_size))
		return !0;
	}
    }
    return 0;
}

static Uint hfrag_literal_size(Eterm* start, Eterm* end,
                               char* lit_start, Uint lit_size);
static void hfrag_literal_copy(Eterm **hpp, ErlOffHeap *ohp,
                               Eterm *start, Eterm *end,
                               char *lit_start, Uint lit_size);

static Eterm
check_process_code(Process* rp, Module* modp, Uint flags, int *redsp, int fcalls)
{
    BeamInstr* start;
    char* literals;
    Uint lit_bsize;
    char* mod_start;
    Uint mod_size;
    Eterm* sp;
    int done_gc = 0;
    int need_gc = 0;
    ErtsMessage *msgp;
    ErlHeapFragment *hfrag;

#define ERTS_ORDINARY_GC__ (1 << 0)
#define ERTS_LITERAL_GC__  (1 << 1)

    /*
     * Pick up limits for the module.
     */
    start = (BeamInstr*) modp->old.code_hdr;
    mod_start = (char *) start;
    mod_size = modp->old.code_length;

    /*
     * Check if current instruction or continuation pointer points into module.
     */
    if (ErtsInArea(rp->i, mod_start, mod_size)
	|| ErtsInArea(rp->cp, mod_start, mod_size)) {
	return am_true;
    }

    /*
     * Check all continuation pointers stored on the stack.
     */
    for (sp = rp->stop; sp < STACK_START(rp); sp++) {
	if (is_CP(*sp) && ErtsInArea(cp_val(*sp), mod_start, mod_size)) {
	    return am_true;
	}
    }

    /* 
     * Check all continuation pointers stored in stackdump
     * and clear exception stackdump if there is a pointer
     * to the module.
     */
    if (rp->ftrace != NIL) {
	struct StackTrace *s;
	ASSERT(is_list(rp->ftrace));
	s = (struct StackTrace *) big_val(CDR(list_val(rp->ftrace)));
	if ((s->pc && ErtsInArea(s->pc, mod_start, mod_size)) ||
	    (s->current && ErtsInArea(s->current, mod_start, mod_size))) {
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	} else {
	    int i;
	    for (i = 0;  i < s->depth;  i++) {
		if (ErtsInArea(s->trace[i], mod_start, mod_size)) {
		    rp->freason = EXC_NULL;
		    rp->fvalue = NIL;
		    rp->ftrace = NIL;
		    break;
		}
	    }
	}
    }

    if (rp->flags & F_DISABLE_GC) {
	/*
	 * Cannot proceed. Process has disabled gc in order to
	 * safely leave inconsistent data on the heap and/or
	 * off heap lists. Need to wait for gc to be enabled
	 * again.
	 */ 
	return THE_NON_VALUE;
    }

    /*
     * Message queue can contains funs, and may contain
     * literals. If we got references to this module from the message
     * queue.
     *
     * If a literal is in the message queue we maka an explicit copy of
     * and attach it to the heap fragment. Each message needs to be
     * self contained, we cannot save the literal in the old_heap or
     * any other heap than the message it self.
     */

    erts_smp_proc_lock(rp, ERTS_PROC_LOCK_MSGQ);
    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(rp);
    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MSGQ);

    literals = (char*) modp->old.code_hdr->literals_start;
    lit_bsize = (char*) modp->old.code_hdr->literals_end - literals;

    for (msgp = rp->msg.first; msgp; msgp = msgp->next) {
	if (msgp->data.attached == ERTS_MSG_COMBINED_HFRAG)
	    hfrag = &msgp->hfrag;
	else if (is_value(ERL_MESSAGE_TERM(msgp)) && msgp->data.heap_frag)
	    hfrag = msgp->data.heap_frag;
	else
	    continue;
        {
            ErlHeapFragment *hf;
            Uint lit_sz;
            for (hf=hfrag; hf; hf = hf->next) {
                if (check_mod_funs(rp, &hfrag->off_heap, mod_start, mod_size))
                    return am_true;
                lit_sz = hfrag_literal_size(&hf->mem[0], &hf->mem[hf->used_size],
                                            literals, lit_bsize);
            }
            if (lit_sz > 0) {
                ErlHeapFragment *bp = new_message_buffer(lit_sz);
                Eterm *hp = bp->mem;

                for (hf=hfrag; hf; hf = hf->next) {
                    hfrag_literal_copy(&hp, &bp->off_heap,
                                       &hf->mem[0], &hf->mem[hf->used_size],
                                       literals, lit_bsize);
                    hfrag=hf;
                }
                /* link new hfrag last */
                ASSERT(hfrag->next == NULL);
                hfrag->next = bp;
                bp->next = NULL;
            }
        }
    }

    while (1) {

	/* Check heap, stack etc... */
	if (check_mod_funs(rp, &rp->off_heap, mod_start, mod_size))
	    goto try_gc;
        if (!(flags & ERTS_CPC_COPY_LITERALS)) {
            /* Process ok. May contain old literals but we will be called
             * again before module is purged.
             */
            return am_false;
        }
	if (any_heap_ref_ptrs(&rp->fvalue, &rp->fvalue+1, literals, lit_bsize)) {
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	}
	if (any_heap_ref_ptrs(rp->stop, rp->hend, literals, lit_bsize))
	    goto try_literal_gc;
#ifdef HIPE
	if (nstack_any_heap_ref_ptrs(rp, literals, lit_bsize))
	    goto try_literal_gc;
#endif
	if (any_heap_refs(rp->heap, rp->htop, literals, lit_bsize))
	    goto try_literal_gc;
	if (any_heap_refs(rp->old_heap, rp->old_htop, literals, lit_bsize))
	    goto try_literal_gc;

	/* Check dictionary */
	if (rp->dictionary) {
	    Eterm* start = ERTS_PD_START(rp->dictionary);
	    Eterm* end = start + ERTS_PD_SIZE(rp->dictionary);

	    if (any_heap_ref_ptrs(start, end, literals, lit_bsize))
		goto try_literal_gc;
	}

	/* Check heap fragments */
	for (hfrag = rp->mbuf; hfrag; hfrag = hfrag->next) {
	    Eterm *hp, *hp_end;
	    /* Off heap lists should already have been moved into process */
	    ASSERT(!check_mod_funs(rp, &hfrag->off_heap, mod_start, mod_size));

	    hp = &hfrag->mem[0];
	    hp_end = &hfrag->mem[hfrag->used_size];
	    if (any_heap_refs(hp, hp_end, literals, lit_bsize))
		goto try_literal_gc;
	}

	/*
	 * Message buffer fragments (matched messages) 
         *  - off heap lists should already have been moved into
         *    process off heap structure.
         *  - Check for literals
	 */
	for (msgp = rp->msg_frag; msgp; msgp = msgp->next) {
            hfrag = erts_message_to_heap_frag(msgp);
	    for (; hfrag; hfrag = hfrag->next) {
		Eterm *hp, *hp_end;
		ASSERT(!check_mod_funs(rp, &hfrag->off_heap, mod_start, mod_size));

		hp = &hfrag->mem[0];
		hp_end = &hfrag->mem[hfrag->used_size];

                if (any_heap_refs(hp, hp_end, literals, lit_bsize))
                    goto try_literal_gc;
	    }
	}

	return am_false;

    try_literal_gc:
	need_gc |= ERTS_LITERAL_GC__;

    try_gc:
	need_gc |= ERTS_ORDINARY_GC__;

	if ((done_gc & need_gc) == need_gc)
	    return am_true;

	if (!(flags & ERTS_CPC_ALLOW_GC))
	    return am_aborted;

	need_gc &= ~done_gc;

	/*
	 * Try to get rid of literals by by garbage collecting.
	 * Clear both fvalue and ftrace.
	 */

	rp->freason = EXC_NULL;
	rp->fvalue = NIL;
	rp->ftrace = NIL;

	if (need_gc & ERTS_ORDINARY_GC__) {
	    FLAGS(rp) |= F_NEED_FULLSWEEP;
	    *redsp += erts_garbage_collect_nobump(rp, 0, rp->arg_reg, rp->arity, fcalls);
	    done_gc |= ERTS_ORDINARY_GC__;
	}
	if (need_gc & ERTS_LITERAL_GC__) {
	    struct erl_off_heap_header* oh;
	    oh = modp->old.code_hdr->literals_off_heap;
	    *redsp += lit_bsize / 64; /* Need, better value... */
	    erts_garbage_collect_literals(rp, (Eterm*)literals, lit_bsize, oh);
	    done_gc |= ERTS_LITERAL_GC__;
	}
	need_gc = 0;
    }

#undef ERTS_ORDINARY_GC__
#undef ERTS_LITERAL_GC__

}

static int
any_heap_ref_ptrs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size)
{
    Eterm* p;
    Eterm val;

    for (p = start; p < end; p++) {
	val = *p;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_BOXED:
	case TAG_PRIMARY_LIST:
	    if (ErtsInArea(val, mod_start, mod_size)) {
		return 1;
	    }
	    break;
	}
    }
    return 0;
}

static int
any_heap_refs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size)
{
    Eterm* p;
    Eterm val;

    for (p = start; p < end; p++) {
	val = *p;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_BOXED:
	case TAG_PRIMARY_LIST:
	    if (ErtsInArea(val, mod_start, mod_size)) {
		return 1;
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (!header_is_transparent(val)) {
		Eterm* new_p;
                if (header_is_bin_matchstate(val)) {
                    ErlBinMatchState *ms = (ErlBinMatchState*) p;
                    ErlBinMatchBuffer *mb = &(ms->mb);
                    if (ErtsInArea(mb->orig, mod_start, mod_size)) {
                        return 1;
                    }
                }
		new_p = p + thing_arityval(val);
		ASSERT(start <= new_p && new_p < end);
		p = new_p;
	    }
	}
    }
    return 0;
}

static Uint
hfrag_literal_size(Eterm* start, Eterm* end, char* lit_start, Uint lit_size)
{
    Eterm* p;
    Eterm val;
    Uint sz = 0;

    for (p = start; p < end; p++) {
        val = *p;
        switch (primary_tag(val)) {
        case TAG_PRIMARY_BOXED:
        case TAG_PRIMARY_LIST:
            if (ErtsInArea(val, lit_start, lit_size)) {
                sz += size_object(val);
            }
            break;
        case TAG_PRIMARY_HEADER:
            if (!header_is_transparent(val)) {
                Eterm* new_p;
                if (header_is_bin_matchstate(val)) {
                    ErlBinMatchState *ms = (ErlBinMatchState*) p;
                    ErlBinMatchBuffer *mb = &(ms->mb);
                    if (ErtsInArea(mb->orig, lit_start, lit_size)) {
                        sz += size_object(mb->orig);
                    }
                }
                new_p = p + thing_arityval(val);
                ASSERT(start <= new_p && new_p < end);
                p = new_p;
            }
        }
    }
    return sz;
}

static void
hfrag_literal_copy(Eterm **hpp, ErlOffHeap *ohp,
                   Eterm *start, Eterm *end,
                   char *lit_start, Uint lit_size) {
    Eterm* p;
    Eterm val;
    Uint sz;

    for (p = start; p < end; p++) {
        val = *p;
        switch (primary_tag(val)) {
        case TAG_PRIMARY_BOXED:
        case TAG_PRIMARY_LIST:
            if (ErtsInArea(val, lit_start, lit_size)) {
                sz = size_object(val);
                val = copy_struct(val, sz, hpp, ohp);
                *p = val; 
            }
            break;
        case TAG_PRIMARY_HEADER:
            if (!header_is_transparent(val)) {
                Eterm* new_p;
                /* matchstate in message, not possible. */
                if (header_is_bin_matchstate(val)) {
                    ErlBinMatchState *ms = (ErlBinMatchState*) p;
                    ErlBinMatchBuffer *mb = &(ms->mb);
                    if (ErtsInArea(mb->orig, lit_start, lit_size)) {
                        sz = size_object(mb->orig);
                        mb->orig = copy_struct(mb->orig, sz, hpp, ohp);
                    }
                }
                new_p = p + thing_arityval(val);
                ASSERT(start <= new_p && new_p < end);
                p = new_p;
            }
        }
    }
}

#undef in_area

#ifdef ERTS_SMP
static void copy_literals_commit(void*);
#endif

copy_literals_t erts_clrange = {NULL, 0, THE_NON_VALUE};

/* copy literals
 *
 * copy_literals.ptr = LitPtr
 * copy_literals.sz  = LitSz
 * ------ THR PROG COMMIT -----
 *
 *  - check process code
 *  - check process code
 *  ...
 * copy_literals.ptr = NULL
 * copy_literals.sz  = 0
 * ------ THR PROG COMMIT -----
 *  ...
 */


BIF_RETTYPE erts_internal_copy_literals_2(BIF_ALIST_2)
{
    ErtsCodeIndex code_ix;
    Eterm res = am_true;

    if (is_not_atom(BIF_ARG_1) || (am_true != BIF_ARG_2 && am_false != BIF_ARG_2)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD2(bif_export[BIF_erts_internal_copy_literals_2],
                        BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    code_ix = erts_active_code_ix();

    if (BIF_ARG_2 == am_true) {
        Module* modp = erts_get_module(BIF_ARG_1, code_ix);
        if (!modp || !modp->old.code_hdr) {
            res = am_false;
            goto done;
        }
        if (erts_clrange.ptr != NULL
            && !(BIF_P->static_flags & ERTS_STC_FLG_SYSTEM_PROC)) {
            res = am_aborted;
            goto done;
        }
        erts_clrange.ptr = modp->old.code_hdr->literals_start;
        erts_clrange.sz  = modp->old.code_hdr->literals_end - erts_clrange.ptr;
        erts_clrange.pid = BIF_P->common.id;
    } else if (BIF_ARG_2 == am_false) {
        if (erts_clrange.pid != BIF_P->common.id) {
            res = am_false;
            goto done;
        }
        erts_clrange.ptr = NULL;
        erts_clrange.sz  = 0;
        erts_clrange.pid = THE_NON_VALUE;
    }

#ifdef ERTS_SMP
    ASSERT(committer_state.stager == NULL);
    committer_state.stager = BIF_P;
    erts_schedule_thr_prgr_later_op(copy_literals_commit, NULL, &committer_state.lop);
    erts_proc_inc_refc(BIF_P);
    erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
    ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
#endif
done:
    erts_release_code_write_permission();
    BIF_RET(res);
}

#ifdef ERTS_SMP
static void copy_literals_commit(void* null) {
    Process* p = committer_state.stager;
#ifdef DEBUG
    committer_state.stager = NULL;
#endif
    erts_release_code_write_permission();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (!ERTS_PROC_IS_EXITING(p)) {
	erts_resume(p, ERTS_PROC_LOCK_STATUS);
    }
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_proc_dec_refc(p);
}
#endif /* ERTS_SMP */


/* Do the actualy module purging and return:
 * true for success
 * false if no such old module
 * BADARG if not an atom
 */
BIF_RETTYPE erts_internal_purge_module_1(BIF_ALIST_1)
{
    ErtsCodeIndex code_ix;
    BeamInstr* code;
    BeamInstr* end;
    Module* modp;
    int is_blocking = 0;
    Eterm ret;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD1(bif_export[BIF_erts_internal_purge_module_1],
                        BIF_P, BIF_ARG_1);
    }

    code_ix = erts_active_code_ix();

    /*
     * Correct module?
     */

    if ((modp = erts_get_module(BIF_ARG_1, code_ix)) == NULL) {
        ERTS_BIF_PREP_RET(ret, am_false);
    }
    else {
	erts_rwlock_old_code(code_ix);

	/*
	 * Any code to purge?
	 */
	if (!modp->old.code_hdr) {
            ERTS_BIF_PREP_RET(ret, am_false);
	}
	else {
	    /*
	     * Unload any NIF library
	     */
	    if (modp->old.nif != NULL) {
		/* ToDo: Do unload nif without blocking */
		erts_rwunlock_old_code(code_ix);
		erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		erts_smp_thr_progress_block();
		is_blocking = 1;
		erts_rwlock_old_code(code_ix);
		erts_unload_nif(modp->old.nif);
		modp->old.nif = NULL;
	    }

	    /*
	     * Remove the old code.
	     */
	    ASSERT(erts_total_code_size >= modp->old.code_length);
	    erts_total_code_size -= modp->old.code_length;
	    code = (BeamInstr*) modp->old.code_hdr;
	    end = (BeamInstr *)((char *)code + modp->old.code_length);
	    erts_cleanup_funs_on_purge(code, end);
	    beam_catches_delmod(modp->old.catches, code, modp->old.code_length,
				code_ix);
	    decrement_refc(modp->old.code_hdr);
            if (modp->old.code_hdr->literals_start) {
                erts_free(ERTS_ALC_T_LITERAL, modp->old.code_hdr->literals_start);
            }
	    erts_free(ERTS_ALC_T_CODE, (void *) code);
	    modp->old.code_hdr = NULL;
	    modp->old.code_length = 0;
	    modp->old.catches = BEAM_CATCHES_NIL;
	    erts_remove_from_ranges(code);
	    ERTS_BIF_PREP_RET(ret, am_true);
	}
	erts_rwunlock_old_code(code_ix);
    }
    if (is_blocking) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    }
    erts_release_code_write_permission();
    return ret;
}

static void
decrement_refc(BeamCodeHeader* code_hdr)
{
    struct erl_off_heap_header* oh = code_hdr->literals_off_heap;

    while (oh) {
	Binary* bptr;
	ASSERT(thing_subtag(oh->thing_word) == REFC_BINARY_SUBTAG);
	bptr = ((ProcBin*)oh)->val;
	if (erts_refc_dectest(&bptr->refc, 0) == 0) {
	    erts_bin_free(bptr);
	}
	oh = oh->next;
    }
}

/*
 * Move code from current to old and null all export entries for the module
 */

static void
delete_code(Module* modp)
{
    ErtsCodeIndex code_ix = erts_staging_code_ix();
    Eterm module = make_atom(modp->module);
    int i;

    for (i = 0; i < export_list_size(code_ix); i++) {
	Export *ep = export_list(i, code_ix);
        if (ep != NULL && (ep->code[0] == module)) {
	    if (ep->addressv[code_ix] == ep->code+3) {
		if (ep->code[3] == (BeamInstr) em_apply_bif) {
		    continue;
		}
		else if (ep->code[3] ==
			 (BeamInstr) BeamOp(op_i_generic_breakpoint)) {
		    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
		    ASSERT(modp->curr.num_traced_exports > 0);
		    erts_clear_export_break(modp, ep->code+3);
		}
		else ASSERT(ep->code[3] == (BeamInstr) em_call_error_handler
			    || !erts_initialized);
	    }
	    ep->addressv[code_ix] = ep->code+3;
	    ep->code[3] = (BeamInstr) em_call_error_handler;
	    ep->code[4] = 0;
	}
    }

    ASSERT(modp->curr.num_breakpoints == 0);
    ASSERT(modp->curr.num_traced_exports == 0);
    modp->old = modp->curr;
    modp->curr.code_hdr = NULL;
    modp->curr.code_length = 0;
    modp->curr.catches = BEAM_CATCHES_NIL;
    modp->curr.nif = NULL;

}


Eterm
beam_make_current_old(Process *c_p, ErtsProcLocks c_p_locks, Eterm module)
{
    Module* modp = erts_put_module(module);

    /*
     * Check if the previous code has been already deleted;
     * if not, delete old code; error if old code already exists.
     */

    if (modp->curr.code_hdr && modp->old.code_hdr)  {
	return am_not_purged;
    } else if (!modp->old.code_hdr) { /* Make the current version old. */
	delete_code(modp);
    }
    return NIL;
}
