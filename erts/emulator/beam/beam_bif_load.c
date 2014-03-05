/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2013. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
#include "erl_thr_progress.h"

static void set_default_trace_pattern(Eterm module);
static Eterm check_process_code(Process* rp, Module* modp, int allow_gc, int *redsp);
static void delete_code(Module* modp);
static void decrement_refc(BeamInstr* code);
static int is_native(BeamInstr* code);
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
    res = ((modp->curr.code && is_native(modp->curr.code)) ||
	    (modp->old.code != 0 && is_native(modp->old.code))) ?
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
	ASSERT(modp->curr.code != NULL);
	erts_clear_module_break(modp);
	ASSERT(modp->curr.num_breakpoints == 0);
    }

    erts_start_staging_code_ix();

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
}commiter_state;
#endif

static Eterm
exception_list(Process* p, Eterm tag, struct m* mp, Sint exceptions)
{
    Eterm* hp = HAlloc(p, 3 + 2*exceptions);
    Eterm res = NIL;

    mp += exceptions - 1;
    while (exceptions > 0) {
	if (mp->exception) {
	    res = CONS(hp, mp->module, res);
	    hp += 2;
	    exceptions--;
	}
	mp--;
    }
    return TUPLE2(hp, tag, res);
}


BIF_RETTYPE
finish_loading_1(BIF_ALIST_1)
{
    int i;
    int n;
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
    if (n == -1) {
	ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	goto done;
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
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	    goto done;
	}
	pb = (ProcBin*) binary_val(term);
	p[i].code = pb->val;
	p[i].module = erts_module_for_prepared_code(p[i].code);
	if (p[i].module == NIL) {
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	    goto done;
	}
	BIF_ARG_1 = CDR(cons);
    }

    /*
     * Since we cannot handle atomic loading of a group of modules
     * if one or more of them uses on_load, we will only allow one
     * element in the list. This limitation is intended to be
     * lifted in the future.
     */

    if (n > 1) {
	ERTS_BIF_PREP_ERROR(res, BIF_P, SYSTEM_LIMIT);
	goto done;
    }

    /*
     * All types are correct. There cannot be a BADARG from now on.
     * Before we can start loading, we must check whether any of
     * the modules already has old code. To avoid a race, we must
     * not allow other process to initiate a code loading operation
     * from now on.
     */

    res = am_ok;
    erts_start_staging_code_ix();

    for (i = 0; i < n; i++) {
	p[i].modp = erts_put_module(p[i].module);
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
	if (p[i].modp->curr.code && p[i].modp->old.code) {
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
	ASSERT(commiter_state.stager == NULL);
	commiter_state.stager = c_p;
	erts_schedule_thr_prgr_later_op(smp_code_ix_commiter, NULL, &commiter_state.lop);
	erts_smp_proc_inc_refc(c_p);
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
    Process* p = commiter_state.stager;

    erts_commit_staging_code_ix();
#ifdef DEBUG
    commiter_state.stager = NULL;
#endif
    erts_release_code_write_permission();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (!ERTS_PROC_IS_EXITING(p)) {
	erts_resume(p, ERTS_PROC_LOCK_STATUS);
    }
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_smp_proc_dec_refc(p);
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
	if (modp->old.code != NULL) {
	    res = am_true;
	}
	erts_runlock_old_code(code_ix);
    }
    BIF_RET(res);
}

Eterm
erts_check_process_code(Process *c_p, Eterm module, int allow_gc, int *redsp)
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
    res = modp->old.code ? check_process_code(c_p, modp, allow_gc, redsp) : am_false;
    erts_runlock_old_code(code_ix);

    return res;
}

BIF_RETTYPE erts_internal_check_process_code_2(BIF_ALIST_2)
{
    int reds = 0;
    Eterm res;
    Eterm olist = BIF_ARG_2;
    int allow_gc = 1;

    if (is_not_atom(BIF_ARG_1))
	goto badarg;

    while (is_list(olist)) {
	Eterm *lp = list_val(olist);
	Eterm opt = CAR(lp);
	if (is_tuple(opt)) {
	    Eterm* tp = tuple_val(opt);
	    switch (arityval(tp[0])) {
	    case 2:
		switch (tp[1]) {
		case am_allow_gc:
		    switch (tp[2]) {
		    case am_false:
			allow_gc = 0;
			break;
		    case am_true:
			allow_gc = 1;
			break;
		    default:
			goto badarg;
		    }
		    break;
		default:
		    goto badarg;
		}
		break;
	    default:
		goto badarg;
	    }
	}
	else
	    goto badarg;
	olist = CDR(lp);
    }
    if (is_not_nil(olist))
	goto badarg;

    res = erts_check_process_code(BIF_P, BIF_ARG_1, allow_gc, &reds);

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
	erts_start_staging_code_ix();
	code_ix = erts_staging_code_ix();
	modp = erts_get_module(BIF_ARG_1, code_ix);
	if (!modp) {
	    res = am_undefined;
	}
	else if (modp->old.code != 0) {
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
	if (modp->curr.code != NULL
	    && modp->curr.code[MI_ON_LOAD_FUNCTION_PTR] == 0) {
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

    if (modp && modp->curr.code) {
	BIF_TRAP_CODE_PTR_0(BIF_P, modp->curr.code[MI_ON_LOAD_FUNCTION_PTR]);
    }
    else {
	BIF_ERROR(BIF_P, BADARG);
    }
}

BIF_RETTYPE finish_after_on_load_2(BIF_ALIST_2)
{
    ErtsCodeIndex code_ix;
    Module* modp;
    Eterm on_load;

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD2(bif_export[BIF_finish_after_on_load_2],
			BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    /* ToDo: Use code_ix staging instead of thread blocking */

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);

    if (!modp || modp->curr.code == 0) {
    error:
	erts_smp_thr_progress_unblock();
        erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_release_code_write_permission();
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((on_load = modp->curr.code[MI_ON_LOAD_FUNCTION_PTR]) == 0) {
	goto error;
    }
    if (BIF_ARG_2 != am_false && BIF_ARG_2 != am_true) {
	goto error;
    }

    if (BIF_ARG_2 == am_true) {
	int i;

	/*
	 * The on_load function succeded. Fix up export entries.
	 */
	for (i = 0; i < export_list_size(code_ix); i++) {
	    Export *ep = export_list(i,code_ix);
	    if (ep != NULL &&
		ep->code[0] == BIF_ARG_1 &&
		ep->code[4] != 0) {
		ep->addressv[code_ix] = (void *) ep->code[4];
		ep->code[4] = 0;
	    }
	}
	modp->curr.code[MI_ON_LOAD_FUNCTION_PTR] = 0;
	set_default_trace_pattern(BIF_ARG_1);
    } else if (BIF_ARG_2 == am_false) {
	BeamInstr* code;
	BeamInstr* end;

	/*
	 * The on_load function failed. Remove the loaded code.
	 * This is an combination of delete and purge. We purge
	 * the current code; the old code is not touched.
	 */
	erts_total_code_size -= modp->curr.code_length;
	code = modp->curr.code;
	end = (BeamInstr *)((char *)code + modp->curr.code_length);
	erts_cleanup_funs_on_purge(code, end);
	beam_catches_delmod(modp->curr.catches, code, modp->curr.code_length,
			    erts_active_code_ix());
	erts_free(ERTS_ALC_T_CODE, (void *) code);
	modp->curr.code = NULL;
	modp->curr.code_length = 0;
	modp->curr.catches = BEAM_CATCHES_NIL;
	erts_remove_from_ranges(code);
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
    Eterm meta_tracer_pid;

    erts_get_default_trace_pattern(&trace_pattern_is_on,
				   &match_spec,
				   &meta_match_spec,
				   &trace_pattern_flags,
				   &meta_tracer_pid);
    if (trace_pattern_is_on) {
	Eterm mfa[1];
	mfa[0] = module;
	(void) erts_set_trace_pattern(0, mfa, 1,
				      match_spec,
				      meta_match_spec,
				      1, trace_pattern_flags,
				      meta_tracer_pid, 1);
    }
}

static Eterm
check_process_code(Process* rp, Module* modp, int allow_gc, int *redsp)
{
    BeamInstr* start;
    char* mod_start;
    Uint mod_size;
    BeamInstr* end;
    Eterm* sp;
    struct erl_off_heap_header* oh;
    int done_gc = 0;

#define INSIDE(a) (start <= (a) && (a) < end)

    /*
     * Pick up limits for the module.
     */
    start = modp->old.code;
    end = (BeamInstr *)((char *)start + modp->old.code_length);
    mod_start = (char *) start;
    mod_size = modp->old.code_length;

    /*
     * Check if current instruction or continuation pointer points into module.
     */
    if (INSIDE(rp->i) || INSIDE(rp->cp)) {
	return am_true;
    }

    /*
     * Check all continuation pointers stored on the stack.
     */
    for (sp = rp->stop; sp < STACK_START(rp); sp++) {
	if (is_CP(*sp) && INSIDE(cp_val(*sp))) {
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
	if ((s->pc && INSIDE(s->pc)) ||
	    (s->current && INSIDE(s->current))) {
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	} else {
	    int i;
	    for (i = 0;  i < s->depth;  i++) {
		if (INSIDE(s->trace[i])) {
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
     * See if there are funs that refer to the old version of the module.
     */

 rescan:
    for (oh = MSO(rp).first; oh; oh = oh->next) {
	if (thing_subtag(oh->thing_word) == FUN_SUBTAG) {
	    ErlFunThing* funp = (ErlFunThing*) oh;

	    if (INSIDE((BeamInstr *) funp->fe->address)) {
		if (done_gc) {
		    return am_true;
		} else {
		    if (!allow_gc)
			return am_aborted;
		    /*
		    * Try to get rid of this fun by garbage collecting.
		    * Clear both fvalue and ftrace to make sure they
		    * don't hold any funs.
		    */
		    rp->freason = EXC_NULL;
		    rp->fvalue = NIL;
		    rp->ftrace = NIL;
		    done_gc = 1;
		    FLAGS(rp) |= F_NEED_FULLSWEEP;
		    *redsp += erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
		    goto rescan;
		}
	    }
	}
    }

    /*
     * See if there are constants inside the module referenced by the process.
     */
    done_gc = 0;
    for (;;) {
	ErlMessage* mp;

	if (any_heap_ref_ptrs(&rp->fvalue, &rp->fvalue+1, mod_start, mod_size)) {
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	}
	if (any_heap_ref_ptrs(rp->stop, rp->hend, mod_start, mod_size)) {
	    goto need_gc;
	}
	if (any_heap_refs(rp->heap, rp->htop, mod_start, mod_size)) {
	    goto need_gc;
	}

	if (any_heap_refs(rp->old_heap, rp->old_htop, mod_start, mod_size)) {
	    goto need_gc;
	}

	if (rp->dictionary != NULL) {
	    Eterm* start = rp->dictionary->data;
	    Eterm* end = start + rp->dictionary->used;

	    if (any_heap_ref_ptrs(start, end, mod_start, mod_size)) {
		goto need_gc;
	    }
	}

	for (mp = rp->msg.first; mp != NULL; mp = mp->next) {
	    if (any_heap_ref_ptrs(mp->m, mp->m+2, mod_start, mod_size)) {
		goto need_gc;
	    }
	}
	break;

    need_gc:
	if (done_gc) {
	    return am_true;
	} else {
	    Eterm* literals;
	    Uint lit_size;
	    struct erl_off_heap_header* oh;

	    if (!allow_gc)
		return am_aborted;

	    /*
	     * Try to get rid of constants by by garbage collecting.
	     * Clear both fvalue and ftrace.
	     */
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	    done_gc = 1;
	    FLAGS(rp) |= F_NEED_FULLSWEEP;
	    *redsp += erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
	    literals = (Eterm *) modp->old.code[MI_LITERALS_START];
	    lit_size = (Eterm *) modp->old.code[MI_LITERALS_END] - literals;
	    oh = (struct erl_off_heap_header *)
		modp->old.code[MI_LITERALS_OFF_HEAP];
	    *redsp += lit_size / 10; /* Need, better value... */
	    erts_garbage_collect_literals(rp, literals, lit_size, oh);
	}
    }
    return am_false;
#undef INSIDE
}

#define in_area(ptr,start,nbytes) \
    ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

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
	    if (in_area(EXPAND_POINTER(val), mod_start, mod_size)) {
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
	    if (in_area(EXPAND_POINTER(val), mod_start, mod_size)) {
		return 1;
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (!header_is_transparent(val)) {
		Eterm* new_p = p + thing_arityval(val);
		ASSERT(start <= new_p && new_p < end);
		p = new_p;
	    }
	}
    }
    return 0;
}

#undef in_area

BIF_RETTYPE purge_module_1(BIF_ALIST_1)
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
	ERTS_BIF_YIELD1(bif_export[BIF_purge_module_1], BIF_P, BIF_ARG_1);
    }

    code_ix = erts_active_code_ix();

    /*
     * Correct module?
     */

    if ((modp = erts_get_module(BIF_ARG_1, code_ix)) == NULL) {
	ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
    }
    else {
	erts_rwlock_old_code(code_ix);

	/*
	 * Any code to purge?
	 */
	if (modp->old.code == 0) {
	    ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
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
	    code = modp->old.code;
	    end = (BeamInstr *)((char *)code + modp->old.code_length);
	    erts_cleanup_funs_on_purge(code, end);
	    beam_catches_delmod(modp->old.catches, code, modp->old.code_length,
				code_ix);
	    decrement_refc(code);
	    erts_free(ERTS_ALC_T_CODE, (void *) code);
	    modp->old.code = NULL;
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
decrement_refc(BeamInstr* code)
{
    struct erl_off_heap_header* oh =
	(struct erl_off_heap_header *) code[MI_LITERALS_OFF_HEAP];

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
    modp->curr.code = NULL;
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

    if (modp->curr.code != NULL && modp->old.code != NULL)  {
	return am_not_purged;
    } else if (modp->old.code == NULL) { /* Make the current version old. */
	delete_code(modp);
    }
    return NIL;
}

static int
is_native(BeamInstr* code)
{
    Uint i, num_functions = code[MI_NUM_FUNCTIONS];

    /* Check NativeAdress of first real function in module
     */
    for (i=0; i<num_functions; i++) {
	BeamInstr* func_info = (BeamInstr *) code[MI_FUNCTIONS+i];
	Eterm name = (Eterm) func_info[3];

	if (is_atom(name)) {
	    return func_info[1] != 0;    
	}
	else ASSERT(is_nil(name)); /* ignore BIF stubs */
    }
    /* Not a single non-BIF function? */
    return 0;
}


