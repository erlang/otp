/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
#include "erl_nfunc_sched.h"
#include "erl_proc_sig_queue.h"
#ifdef HIPE
#  include "hipe_bif0.h"
#  define IF_HIPE(X) (X)
#else
#  define IF_HIPE(X) (0)
#endif

#ifdef HIPE
#  include "hipe_stack.h"
#endif

static struct {
    Eterm module;
    erts_mtx_t mtx;
    Export *pending_purge_lambda;
    Eterm *sprocs;
    Eterm def_sprocs[10];
    Uint sp_size;
    Uint sp_ix;
    ErlFunEntry **funs;
    ErlFunEntry *def_funs[10];
    Uint fe_size;
    Uint fe_ix;
    struct erl_module_instance saved_old;
} purge_state;

Process *erts_code_purger = NULL;

static void set_default_trace_pattern(Eterm module);
static Eterm check_process_code(Process* rp, Module* modp, int *redsp, int fcalls);
static void delete_code(Module* modp);
static int any_heap_ref_ptrs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);
static int any_heap_refs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);

static void
init_purge_state(void)
{
    purge_state.module = THE_NON_VALUE;

    erts_mtx_init(&purge_state.mtx, "purge_state", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    purge_state.pending_purge_lambda =
	erts_export_put(am_erts_code_purger, am_pending_purge_lambda, 3);

    purge_state.sprocs = &purge_state.def_sprocs[0];
    purge_state.sp_size = sizeof(purge_state.def_sprocs);
    purge_state.sp_size /= sizeof(purge_state.def_sprocs[0]);
    purge_state.sp_ix = 0;

    purge_state.funs = &purge_state.def_funs[0];
    purge_state.fe_size = sizeof(purge_state.def_funs);
    purge_state.fe_size /= sizeof(purge_state.def_funs[0]);
    purge_state.fe_ix = 0;

    purge_state.saved_old.code_hdr = 0;
}

static void
init_release_literal_areas(void);

void
erts_beam_bif_load_init(void)
{
    init_release_literal_areas();
    init_purge_state();
}

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
#if !defined(HIPE)
    BIF_ERROR(BIF_P, EXC_NOTSUP);
#else
    Module* modp;
    Eterm res, mod;

    if (!is_internal_magic_ref(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    mod = erts_module_for_prepared_code(erts_magic_ref2bin(BIF_ARG_1));

    if (is_not_atom(mod))
	BIF_ERROR(BIF_P, BADARG);

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD3(bif_export[BIF_code_make_stub_module_3],
			BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_thr_progress_block();

    modp = erts_get_module(mod, erts_active_code_ix());

    if (modp && modp->curr.num_breakpoints > 0) {
	ASSERT(modp->curr.code_hdr != NULL);
	erts_clear_module_break(modp);
	ASSERT(modp->curr.num_breakpoints == 0);
    }

    erts_start_staging_code_ix(1);

    res = erts_make_stub_module(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (res == mod) {
	erts_end_staging_code_ix();
	erts_commit_staging_code_ix();
        if (!modp)
	    modp = erts_get_module(mod, erts_active_code_ix());
        hipe_redirect_to_module(modp);
    }
    else {
	erts_abort_staging_code_ix();
    }
    erts_thr_progress_unblock();
    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_release_code_write_permission();
    return res;
#endif
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
    hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);
    res = erts_mk_magic_ref(&hp, &MSO(BIF_P), magic);
    erts_refc_dec(&magic->intern.refc, 1);
    BIF_RET(res);
}

BIF_RETTYPE
has_prepared_code_on_load_1(BIF_ALIST_1)
{
    Eterm res;

    if (!is_internal_magic_ref(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }

    res = erts_has_code_on_load(erts_magic_ref2bin(BIF_ARG_1));
    if (res == NIL) {
	goto error;
    }
    BIF_RET(res);
}

struct m {
    Binary* code;
    Eterm module;
    Module* modp;
    Eterm exception;
};

static Eterm staging_epilogue(Process* c_p, int, Eterm res, int, struct m*, int, int);
static void smp_code_ix_commiter(void*);

static struct /* Protected by code_write_permission */
{
    Process* stager;
    ErtsThrPrgrLaterOp lop;
} committer_state;

static Eterm
exception_list(Process* p, Eterm tag, struct m* mp, Sint exceptions)
{
    Eterm* hp = HAlloc(p, 3 + 2*exceptions);
    Eterm res = NIL;

    while (exceptions > 0) {
	if (is_value(mp->exception)) {
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

	if (!is_internal_magic_ref(term)) {
	    goto badarg;
	}
	p[i].code = erts_magic_ref2bin(term);
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
	p[i].exception = THE_NON_VALUE;
	if (p[i].modp->seen) {
	    p[i].exception = am_duplicated;
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
	    erts_is_default_trace_enabled() ||
	    IF_HIPE(hipe_need_blocking(p[i].modp))) {
	    /* tracing or hipe need thread blocking */
	    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    erts_thr_progress_block();
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
	p[i].exception = THE_NON_VALUE;
	if (p[i].modp->curr.code_hdr && p[i].modp->old.code_hdr) {
	    p[i].exception = am_not_purged;
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

	    erts_refc_inc(&p[i].code->intern.refc, 1);
	    retval = erts_finish_loading(p[i].code, BIF_P, 0, &mod);
	    ASSERT(retval == NIL || retval == am_on_load);
	    if (retval == am_on_load) {
		p[i].exception = am_on_load;
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
    return staging_epilogue(BIF_P, do_commit, res, is_blocking, p, n, 1);
}

static Eterm
staging_epilogue(Process* c_p, int commit, Eterm res, int is_blocking,
		 struct m* mods, int nmods, int free_mods)
{    
    if (is_blocking || !commit)
    {
	if (commit) {
	    int i;
	    erts_end_staging_code_ix();
	    erts_commit_staging_code_ix();

	    for (i=0; i < nmods; i++) {
		if (mods[i].modp->curr.code_hdr
                    && mods[i].exception != am_on_load) {
		    set_default_trace_pattern(mods[i].module);
		}
	      #ifdef HIPE
		hipe_redirect_to_module(mods[i].modp);
	      #endif
	    }
	}
	else {
	    erts_abort_staging_code_ix();
	}
	if (free_mods) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, mods);
	}
	if (is_blocking) {
	    erts_thr_progress_unblock();
	    erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
	}
	erts_release_code_write_permission();
	return res;
    }
    else {
	ASSERT(is_value(res));

	if (free_mods) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, mods);
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
}


static void smp_code_ix_commiter(void* null)
{
    Process* p = committer_state.stager;

    erts_commit_staging_code_ix();
#ifdef DEBUG
    committer_state.stager = NULL;
#endif
    erts_release_code_write_permission();
    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (!ERTS_PROC_IS_EXITING(p)) {
	erts_resume(p, ERTS_PROC_LOCK_STATUS);
    }
    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_proc_dec_refc(p);
}



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
erts_check_process_code(Process *c_p, Eterm module, int *redsp, int fcalls)
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
    res = (!modp->old.code_hdr
	   ? am_false
	   : check_process_code(c_p, modp, redsp, fcalls));
    erts_runlock_old_code(code_ix);

    return res;
}

BIF_RETTYPE erts_internal_check_process_code_1(BIF_ALIST_1)
{
    int reds = 0;
    Eterm res;

    if (is_not_atom(BIF_ARG_1))
	goto badarg;

    res = erts_check_process_code(BIF_P, BIF_ARG_1, &reds, BIF_P->fcalls);

    ASSERT(is_value(res));

    BIF_RET2(res, reds);

badarg:
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE erts_internal_check_dirty_process_code_2(BIF_ALIST_2)
{
    erts_aint32_t state;
    Process *rp;
    int dirty, busy, reds = 0;
    Eterm res;

    if (BIF_P != erts_dirty_process_signal_handler
        && BIF_P != erts_dirty_process_signal_handler_high
        && BIF_P != erts_dirty_process_signal_handler_max)
	BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (is_not_internal_pid(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    if (is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);

    if (BIF_ARG_1 == BIF_P->common.id)
        BIF_RET(am_normal);

    rp = erts_proc_lookup_raw(BIF_ARG_1);
    if (!rp)
        BIF_RET(am_false);

    state = erts_atomic32_read_nob(&rp->state);
    dirty = (state & ERTS_PSFLG_DIRTY_RUNNING);
    /*
     * Ignore ERTS_PSFLG_DIRTY_RUNNING_SYS (see
     * comment in erts_execute_dirty_system_task()
     * in erl_process.c).
     */
    if (!dirty)
        BIF_RET(am_normal);

    busy = erts_proc_trylock(rp, ERTS_PROC_LOCK_MAIN) == EBUSY;

    if (busy)
        BIF_RET(am_busy);

    res = erts_check_process_code(rp, BIF_ARG_2, &reds, BIF_P->fcalls);

    erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);

    ASSERT(res == am_true || res == am_false);

    BIF_RET2(res, reds);
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
	    erts_dsprintf(dsbufp, "Module %T must be purged before deleting\n",
			  BIF_ARG_1);
	    erts_send_error_to_logger(BIF_P->group_leader, dsbufp);
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	}
	else {
	    if (modp->curr.num_breakpoints > 0 ||
		modp->curr.num_traced_exports > 0 ||
		IF_HIPE(hipe_need_blocking(modp))) {
		/* tracing or hipe need to go single threaded */
		erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		erts_thr_progress_block();
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
    {
	struct m mod;
	Eterm retval;
	mod.module = BIF_ARG_1;
	mod.modp = modp;
        mod.exception = THE_NON_VALUE;
	retval = staging_epilogue(BIF_P, success, res, is_blocking, &mod, 1, 0);
	return retval;
    }
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

    if (!modp || !modp->on_load) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (modp->on_load->code_hdr) {
	BIF_TRAP_CODE_PTR_0(BIF_P,
			    modp->on_load->code_hdr->on_load_function_ptr);
    } else {
	BIF_ERROR(BIF_P, BADARG);
    }
}

BIF_RETTYPE finish_after_on_load_2(BIF_ALIST_2)
{
    ErtsCodeIndex code_ix;
    Module* modp;

    if (BIF_ARG_2 != am_false && BIF_ARG_2 != am_true) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD2(bif_export[BIF_finish_after_on_load_2],
			BIF_P, BIF_ARG_1, BIF_ARG_2);
    }

    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);

    if (!modp || !modp->on_load || !modp->on_load->code_hdr
	|| !modp->on_load->code_hdr->on_load_function_ptr) {

	erts_release_code_write_permission();
	BIF_ERROR(BIF_P, BADARG);
    }

    if (BIF_ARG_2 == am_true) {
	struct m mods[1];
	int is_blocking = 0;
	int i, num_exps;

	erts_start_staging_code_ix(0);
	code_ix = erts_staging_code_ix();
	modp = erts_get_module(BIF_ARG_1, code_ix);

	ASSERT(modp && modp->on_load && modp->on_load->code_hdr
	       && modp->on_load->code_hdr->on_load_function_ptr);

        if (erts_is_default_trace_enabled()
	    || IF_HIPE(hipe_need_blocking(modp))) {

            erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
            erts_thr_progress_block();
            is_blocking = 1;
	}

	/*
	 * Make the code with the on_load function current.
	 */

	if (modp->curr.code_hdr) {
	    modp->old = modp->curr;
	}
	modp->curr = *modp->on_load;
	erts_free(ERTS_ALC_T_PREPARED_CODE, modp->on_load);
	modp->on_load = 0;

	/*
	 * The on_load function succeded. Fix up export entries.
	 */
	num_exps = export_list_size(code_ix);
	for (i = 0; i < num_exps; i++) {
	    Export *ep = export_list(i,code_ix);
	    if (ep == NULL || ep->info.mfa.module != BIF_ARG_1) {
		continue;
	    }
	    if (ep->beam[1] != 0) {
		ep->addressv[code_ix] = (void *) ep->beam[1];
		ep->beam[1] = 0;
	    } else {
		if (ep->addressv[code_ix] == ep->beam &&
		    BeamIsOpCode(ep->beam[0], op_apply_bif)) {
		    continue;
		}
                ep->addressv[code_ix] = ep->beam;
                ep->beam[0] = BeamOpCodeAddr(op_call_error_handler);
	    }
	}
	modp->curr.code_hdr->on_load_function_ptr = NULL;

	mods[0].modp = modp;
	mods[0].module = BIF_ARG_1;
        mods[0].exception = THE_NON_VALUE;
	return staging_epilogue(BIF_P, 1, am_true, is_blocking, mods, 1, 0);
    }
    else if (BIF_ARG_2 == am_false) {
	int i, num_exps;

	/*
	 * The on_load function failed. Remove references to the
	 * code that is about to be purged from the export entries.
	 */

	num_exps = export_list_size(code_ix);
	for (i = 0; i < num_exps; i++) {
	    Export *ep = export_list(i,code_ix);
	    if (ep == NULL || ep->info.mfa.module != BIF_ARG_1) {
		continue;
	    }
	    if (BeamIsOpCode(ep->beam[0], op_apply_bif)) {
		continue;
	    }
	    ep->beam[1] = 0;
	}
    }
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
        ErtsCodeMFA mfa;
        mfa.module = module;
	(void) erts_set_trace_pattern(0, &mfa, 1,
				      match_spec,
				      meta_match_spec,
				      1, trace_pattern_flags,
				      meta_tracer, 1);
    }
}

static Uint hfrag_literal_size(Eterm* start, Eterm* end,
                               char* lit_start, Uint lit_size);
static void hfrag_literal_copy(Eterm **hpp, ErlOffHeap *ohp,
                               Eterm *start, Eterm *end,
                               char *lit_start, Uint lit_size);

static ERTS_INLINE void
msg_copy_literal_area(ErtsMessage *msgp, int *redsp,
                      char *literals, Uint lit_bsize)
{
    ErlHeapFragment *hfrag, *hf;
    Uint lit_sz = 0;

    *redsp += 1;

    if (!ERTS_SIG_IS_INTERNAL_MSG(msgp) || !msgp->data.attached)
        return;

    if (msgp->data.attached == ERTS_MSG_COMBINED_HFRAG)
        hfrag = &msgp->hfrag;
    else
        hfrag = msgp->data.heap_frag;

    for (hf = hfrag; hf; hf = hf->next) {
        lit_sz += hfrag_literal_size(&hf->mem[0],
                                     &hf->mem[hf->used_size],
                                     literals, lit_bsize);
        *redsp += 1;
    }

    *redsp += lit_sz / 16; /* Better value needed... */
    if (lit_sz > 0) {
        ErlHeapFragment *bp = new_message_buffer(lit_sz);
        Eterm *hp = bp->mem;

        for (hf = hfrag; hf; hf = hf->next) {
            hfrag_literal_copy(&hp, &bp->off_heap,
                               &hf->mem[0],
                               &hf->mem[hf->used_size],
                               literals, lit_bsize);
            hfrag = hf;
        }

        /* link new hfrag last */
        ASSERT(hfrag->next == NULL);
        hfrag->next = bp;
        bp->next = NULL;
    }
}

Eterm
erts_proc_copy_literal_area(Process *c_p, int *redsp, int fcalls, int gc_allowed)
{
    ErtsLiteralArea *la;
    struct erl_off_heap_header* oh;
    char *literals;
    Uint lit_bsize;
    ErlHeapFragment *hfrag;
    ErtsMessage *mfp;

    la = ERTS_COPY_LITERAL_AREA();
    if (!la)
        goto return_ok;

    oh = la->off_heap;
    literals = (char *) &la->start[0];
    lit_bsize = (char *) la->end - literals;

    /*
     * If a literal is in the message queue we make an explicit copy of
     * it and attach it to the heap fragment. Each message needs to be
     * self contained, we cannot save the literal in the old_heap or
     * any other heap than the message it self.
     */

    erts_proc_lock(c_p, ERTS_PROC_LOCK_MSGQ);
    erts_proc_sig_fetch(c_p);
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_MSGQ);

    ERTS_FOREACH_SIG_PRIVQS(c_p, msgp, msg_copy_literal_area(msgp,
                                                             redsp,
                                                             literals,
                                                             lit_bsize));

    if (gc_allowed) {
	/*
	 * Current implementation first tests without
	 * allowing GC, and then restarts the operation
	 * allowing GC if it is needed. It is therfore
	 * very likely that we will need the GC (although
	 * this is not completely certain). We go for
	 * the GC directly instead of scanning everything
	 * one more time...
	 *
	 * Also note that calling functions expect a
	 * major GC to be performed if gc_allowed is set
	 * to true. If you change this, you need to fix
	 * callers...
	 */
	goto literal_gc;
    }

    *redsp += 2;
    if (any_heap_ref_ptrs(&c_p->fvalue, &c_p->fvalue+1, literals, lit_bsize)) {
	c_p->freason = EXC_NULL;
	c_p->fvalue = NIL;
	c_p->ftrace = NIL;
    }

    if (any_heap_ref_ptrs(c_p->stop, c_p->hend, literals, lit_bsize))
	goto literal_gc;   
    *redsp += 1;
#ifdef HIPE
    if (nstack_any_heap_ref_ptrs(c_p, literals, lit_bsize))
	goto literal_gc;
    *redsp += 1;
#endif
    if (any_heap_refs(c_p->heap, c_p->htop, literals, lit_bsize))
	goto literal_gc;
    *redsp += 1;
    if (c_p->abandoned_heap) {
	if (any_heap_refs(c_p->abandoned_heap, c_p->abandoned_heap + c_p->heap_sz,
			  literals, lit_bsize))
	    goto literal_gc;
	*redsp += 1;
    }
    if (any_heap_refs(c_p->old_heap, c_p->old_htop, literals, lit_bsize))
	goto literal_gc;

    /* Check dictionary */
    *redsp += 1;
    if (c_p->dictionary) {
	Eterm* start = ERTS_PD_START(c_p->dictionary);
	Eterm* end = start + ERTS_PD_SIZE(c_p->dictionary);

	if (any_heap_ref_ptrs(start, end, literals, lit_bsize))
	    goto literal_gc;
    }

    /* Check heap fragments */
    for (hfrag = c_p->mbuf; hfrag; hfrag = hfrag->next) {
	Eterm *hp, *hp_end;

	*redsp += 1;

	hp = &hfrag->mem[0];
	hp_end = &hfrag->mem[hfrag->used_size];
	if (any_heap_refs(hp, hp_end, literals, lit_bsize))
	    goto literal_gc;
    }

    /*
     * Message buffer fragments (matched messages) 
     *  - off heap lists should already have been moved into
     *    process off heap structure.
     *  - Check for literals
     */
    for (mfp = c_p->msg_frag; mfp; mfp = mfp->next) {
	hfrag = erts_message_to_heap_frag(mfp);
	for (; hfrag; hfrag = hfrag->next) {
	    Eterm *hp, *hp_end;

	    *redsp += 1;

	    hp = &hfrag->mem[0];
	    hp_end = &hfrag->mem[hfrag->used_size];

	    if (any_heap_refs(hp, hp_end, literals, lit_bsize))
		goto literal_gc;
	}
    }

return_ok:

    if (ERTS_SCHEDULER_IS_DIRTY(erts_proc_sched_data(c_p)))
	c_p->flags &= ~F_DIRTY_CLA;

    return am_ok;

literal_gc:

    if (!gc_allowed)
        return am_need_gc;

    if (c_p->flags & F_DISABLE_GC)
        return THE_NON_VALUE;

    *redsp += erts_garbage_collect_literals(c_p, (Eterm *) literals, lit_bsize,
					    oh, fcalls);

    if (c_p->flags & F_DIRTY_CLA)
	return THE_NON_VALUE;

    return am_ok;
}

static Eterm
check_process_code(Process* rp, Module* modp, int *redsp, int fcalls)
{
    BeamInstr* start;
    char* mod_start;
    Uint mod_size;
    Eterm* sp;
#ifdef HIPE
    void *nat_start = NULL;
    Uint nat_size = 0;
#endif

    *redsp += 1;

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
 
    *redsp += 1;

    if (erts_check_nif_export_in_area(rp, mod_start, mod_size))
	return am_true;

    *redsp += (STACK_START(rp) - rp->stop) / 32;

    /*
     * Check all continuation pointers stored on the stack.
     */
    for (sp = rp->stop; sp < STACK_START(rp); sp++) {
	if (is_CP(*sp) && ErtsInArea(cp_val(*sp), mod_start, mod_size)) {
	    return am_true;
	}
    }

#ifdef HIPE
    /*
     * Check all continuation pointers stored on the native stack if the module
     * has native code.
     */
    if (modp->old.hipe_code) {
	nat_start = modp->old.hipe_code->text_segment;
	nat_size = modp->old.hipe_code->text_segment_size;
	if (nat_size && nstack_any_cps_in_segment(rp, nat_start, nat_size)) {
	    return am_true;
	}
    }
#endif

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
	    char *area_start = mod_start;
	    Uint area_size = mod_size;
#ifdef HIPE
	    if (rp->freason & EXF_NATIVE) {
		area_start = nat_start;
		area_size = nat_size;
	    }
#endif
	    for (i = 0;  i < s->depth;  i++) {
		if (ErtsInArea(s->trace[i], area_start, area_size)) {
		    rp->freason = EXC_NULL;
		    rp->fvalue = NIL;
		    rp->ftrace = NIL;
		    break;
		}
	    }
	}
    }

    return am_false;
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

/*
 * Release of literal areas...
 *
 * Overview over how literal areas are released.
 *
 * - A literal area to remove is placed in the release_literal_areas.first
 *   queue.
 * - The erts_literal_area_collector process is woken and calls
 *   erts_internal:release_literal_area_switch() which publishes the
 *   area to release available to the emulator
 *   (ERTS_COPY_LITERAL_AREA()).
 * - The literal area collector process gets suspended waiting thread
 *   progress in order to ensure all schedulers see the newly published
 *   area to release.
 * - When the literal area collector process is resumed after thread
 *   progress has completed, erts_internal:release_literal_area_switch()
 *   returns 'true'.
 * - The literal area collector process sends copy-literals requests
 *   to all processes in the system.
 * - Processes inspects their heap for literals in the area, if
 *   such are found do a literal-gc to make copies on the heap
 *   of all those literals, and then send replies to the
 *   literal area collector process.
 * - Processes that terminates replies even though they might need to
 *   access literal areas. When a process that might need to access a
 *   literal area terminates, it blocks release of literal areas
 *   by incrementing a counter, and later when termination has
 *   completed decrements that counter. The increment is performed
 *   before replying to the copy-literals request.
 * - When all processes has responded, the literal area collector
 *   process calls erts_internal:release_literal_area_switch() again
 *   in order to switch to the next area.
 * - erts_internal:release_literal_area_switch() changes the set of
 *   counters that blocks release of literal areas
 * - The literal area collector process gets suspended waiting thread
 *   progress in order to ensure that the change of counters is visable
 *   by all schedulers.
 * - When the literal area collector process is resumed after thread
 *   progress has completed, erts_internal:release_literal_area_switch()
 *   inspects all counters in previously used set ensuring that no
 *   terminating processes (which began termination before the change
 *   of counters) are lingering. If needed the literal area collector
 *   process will be blocked in
 *   erts_internal:release_literal_area_switch() waiting for all
 *   terminating processes to complete.
 * - When counter inspection is complete
 *   erts_internal:release_literal_area_switch() returns 'true' if
 *   a new area was set for release and 'false' if no more areas have
 *   been scheduled for release.
 *
 * When multiple literal areas have been queued for release,
 * erts_internal:release_literal_area_switch() will time the thread
 * progress waits so each wait period will be utilized both for
 * ensuring that a new area is seen by all schedulers, and ensuring
 * that a change of counters is seen by all schedulers. By this only
 * one thread progress wait will be done per literal area collected
 * until the last literal area which will need two thread progress
 * wait periods.
 */

static Export *wait_release_literal_area_switch;

ErtsThrPrgrLaterOp later_literal_area_switch;

typedef struct {
    ErtsThrPrgrLaterOp lop;
    ErtsLiteralArea *la;
} ErtsLaterReleasLiteralArea;

erts_atomic_t erts_copy_literal_area__;
#define ERTS_SET_COPY_LITERAL_AREA(LA)			\
    erts_atomic_set_nob(&erts_copy_literal_area__,	\
			    (erts_aint_t) (LA))
Process *erts_literal_area_collector = NULL;

typedef struct ErtsLiteralAreaRef_ ErtsLiteralAreaRef;
struct ErtsLiteralAreaRef_ {
    ErtsLiteralAreaRef *next;
    ErtsLiteralArea *literal_area;
};

typedef struct {
    erts_atomic_t counter[2];
} ErtsReleaseLiteralAreaBlockCounters;

typedef struct {
    union {
        ErtsReleaseLiteralAreaBlockCounters block;
        char align__[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsReleaseLiteralAreaBlockCounters))];
    } u;
} ErtsAlignedReleaseLiteralAreaBlockCounters;

typedef enum {
    ERTS_RLA_BLOCK_STATE_NONE,
    ERTS_RLA_BLOCK_STATE_SWITCHED_IX,
    ERTS_RLA_BLOCK_STATE_WAITING
} ErtsReleaseLiteralAreaBlockState;

static struct {
    erts_mtx_t mtx;
    ErtsLiteralAreaRef *first;
    ErtsLiteralAreaRef *last;
    ErtsAlignedReleaseLiteralAreaBlockCounters *bc;
    erts_atomic32_t block_ix;
    int wait_sched_ix;
    ErtsReleaseLiteralAreaBlockState block_state;
    ErtsLiteralArea *block_area;
} release_literal_areas;

static void
init_release_literal_areas(void)
{
    int i;
    erts_mtx_init(&release_literal_areas.mtx, "release_literal_areas", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    release_literal_areas.first = NULL;
    release_literal_areas.last = NULL;
    erts_atomic_init_nob(&erts_copy_literal_area__,
                         (erts_aint_t) NULL);

    erts_atomic32_init_nob(&release_literal_areas.block_ix, 0);
    release_literal_areas.wait_sched_ix = 0;
    release_literal_areas.block_state = ERTS_RLA_BLOCK_STATE_NONE;
    release_literal_areas.block_area = NULL;

    release_literal_areas.bc =
        erts_alloc_permanent_cache_aligned(ERTS_ALC_T_RLA_BLOCK_CNTRS,
                                           sizeof(ErtsAlignedReleaseLiteralAreaBlockCounters)
                                           * erts_no_schedulers);
    /*
     * The literal-area-collector has an increment in all block counters
     * which it only removes when waiting for other increments to disappear.
     */
    for (i = 0; i < erts_no_schedulers; i++) {
        erts_atomic_init_nob(&release_literal_areas.bc[i].u.block.counter[0], 1);
        erts_atomic_init_nob(&release_literal_areas.bc[i].u.block.counter[1], 1);
    }

    wait_release_literal_area_switch = erts_export_put(am_erts_internal,
                                                       am_wait_release_literal_area_switch,
                                                       1);
}

static void
rla_resume(void *literal_area)
{
    erts_resume(erts_literal_area_collector, 0);
}


static ERTS_INLINE Sint
rla_bc_read(int sched_ix, int block_ix)
{
    return (Sint) erts_atomic_read_nob(
        &release_literal_areas.bc[sched_ix].u.block.counter[block_ix]);
}

static ERTS_INLINE Sint
rla_bc_read_acqb(int sched_ix, int block_ix)
{
    return (Sint) erts_atomic_read_acqb(
        &release_literal_areas.bc[sched_ix].u.block.counter[block_ix]);
}

static ERTS_INLINE Sint
rla_bc_dec_read_acqb(int sched_ix, int block_ix)
{
    return (Sint) erts_atomic_dec_read_acqb(
        &release_literal_areas.bc[sched_ix].u.block.counter[block_ix]);
}

static ERTS_INLINE Sint
rla_bc_dec_read_relb(int sched_ix, int block_ix)
{
    return (Sint) erts_atomic_dec_read_relb(
        &release_literal_areas.bc[sched_ix].u.block.counter[block_ix]);
}

static ERTS_INLINE void
rla_bc_inc(int sched_ix, int block_ix)
{
    erts_atomic_inc_nob(
        &release_literal_areas.bc[sched_ix].u.block.counter[block_ix]);
}


Uint32
erts_block_release_literal_area(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    int sched_ix;
    int block_ix;
    
    ASSERT(esdp->type == ERTS_SCHED_NORMAL);

    sched_ix = ((int) esdp->no) - 1;
    ASSERT((sched_ix & ~0xffff) == 0);
    
    ASSERT(0 <= sched_ix && sched_ix <= erts_no_schedulers);

    block_ix = (int) erts_atomic32_read_nob(&release_literal_areas.block_ix);
    ASSERT(block_ix == 0 || block_ix == 1);
    
    rla_bc_inc(sched_ix, block_ix);

    /*
     * The returned value needs to be non-zero, so the user can
     * use zero as a marker for not having blocked.
     *
     * Both block_ix and sched_ix can be zero so we set
     * the highest (unused) bits to 0xfed00000
     */
    return (Uint32) 0xfed00000 | ((block_ix << 16) | sched_ix);
}

static void
wakeup_literal_area_collector(void *unused)
{        
    erts_queue_message(erts_literal_area_collector,
                       0,
                       erts_alloc_message(0, NULL),
                       am_copy_literals,
                       am_system);
}

void
erts_unblock_release_literal_area(Uint32 sched_block_ix)
{
    Sint block_count;
    int block_ix = (int) ((sched_block_ix >> 16) & 0xf);
    int sched_ix = (int) (sched_block_ix & 0xffff);

    ASSERT((sched_block_ix & ((Uint32) 0xfff00000))
           == (Uint32) 0xfed00000);
    
    ASSERT(block_ix == 0 || block_ix == 1);

    block_count = rla_bc_dec_read_relb(sched_ix, block_ix);

    ASSERT(block_count >= 0);

    if (!block_count) {
        /*
         * Wakeup literal collector so it can continue...
         *
         * We don't know what locks we have here, so schedule
         * the operation...
         */
        int sid = 1;
        ErtsSchedulerData *esdp = erts_get_scheduler_data();
        if (esdp && esdp->type == ERTS_SCHED_NORMAL)
            sid = (int) esdp->no;
        erts_schedule_misc_aux_work(sid,
                                    wakeup_literal_area_collector,
                                    NULL);
    }
}

static void
rla_switch_area(void)
{
    ErtsLiteralAreaRef *la_ref;
    
    erts_mtx_lock(&release_literal_areas.mtx);
    la_ref = release_literal_areas.first;
    if (la_ref) {
	release_literal_areas.first = la_ref->next;
	if (!release_literal_areas.first)
	    release_literal_areas.last = NULL;
    }

    erts_mtx_unlock(&release_literal_areas.mtx);

    if (!la_ref)
        ERTS_SET_COPY_LITERAL_AREA(NULL);
    else {
        ERTS_SET_COPY_LITERAL_AREA(la_ref->literal_area);
        erts_free(ERTS_ALC_T_LITERAL_REF, la_ref);
    }
}

BIF_RETTYPE erts_internal_release_literal_area_switch_0(BIF_ALIST_0)
{
    ErtsLiteralArea *new_area, *old_area;
    int wait_ix = 0;
    int sched_ix = 0;

    if (BIF_P != erts_literal_area_collector)
       	BIF_ERROR(BIF_P, EXC_NOTSUP);
   
    while (1) {
        int six;

        switch (release_literal_areas.block_state) {
        case ERTS_RLA_BLOCK_STATE_NONE: {

            old_area = ERTS_COPY_LITERAL_AREA();
            
            rla_switch_area();
    
            if (old_area) {
                int block_ix;
                /*
                 * Switch block index.
                 */
                block_ix = (int) erts_atomic32_read_nob(&release_literal_areas.block_ix);
                erts_atomic32_set_nob(&release_literal_areas.block_ix,
                                      (erts_aint32_t) !block_ix);
                release_literal_areas.block_state = ERTS_RLA_BLOCK_STATE_SWITCHED_IX;
                ASSERT(!release_literal_areas.block_area);
                release_literal_areas.block_area = old_area;
            }
        
            new_area = ERTS_COPY_LITERAL_AREA();

            if (!old_area && !new_area)
                BIF_RET(am_false);

        publish_new_info:
            
            /*
             * Waiting 'thread progress' will ensure that all schedulers are
             * guaranteed to see the new block index and the new area before
             * we continue...
             */
            erts_schedule_thr_prgr_later_op(rla_resume,
                                            NULL,
                                            &later_literal_area_switch);
            erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
            if (new_area) {
                /*
                 * If we also got a new block_area, we will
                 * take care of that the next time we come back
                 * after all processes has responded on
                 * copy-literals requests...
                 */
                ERTS_BIF_YIELD_RETURN(BIF_P,
                                      am_true);
            }

            ASSERT(old_area);
            ERTS_VBUMP_ALL_REDS(BIF_P);
            BIF_TRAP0(bif_export[BIF_erts_internal_release_literal_area_switch_0],
                      BIF_P);
        }

        case ERTS_RLA_BLOCK_STATE_SWITCHED_IX:
            wait_ix = !erts_atomic32_read_nob(&release_literal_areas.block_ix);
            /*
             * Now all counters in the old index will monotonically
             * decrease towards 1 (our own increment). Check that we
             * have no other increments, than our own, in all counters
             * of the old block index. Wait for other increments to
             * be decremented if necessary...
             */
            sched_ix = 0;
            break;
            
        case ERTS_RLA_BLOCK_STATE_WAITING:
            wait_ix = !erts_atomic32_read_nob(&release_literal_areas.block_ix);
            /*
             * Woken after being waiting for a counter to reach zero...
             */
            sched_ix = release_literal_areas.wait_sched_ix;
            /* restore "our own increment" */
            rla_bc_inc(sched_ix, wait_ix);
            break;
        }

        ASSERT(0 <= sched_ix && sched_ix < erts_no_schedulers);

#ifdef DEBUG
        for (six = 0; six < sched_ix; six++) {
            ASSERT(1 == rla_bc_read(six, wait_ix));
        }
#endif

        for (six = sched_ix; six < erts_no_schedulers; six++) {
            Sint block_count = rla_bc_read_acqb(six, wait_ix);
            ASSERT(block_count >= 1);
            if (block_count == 1)
                continue;
            
            block_count = rla_bc_dec_read_acqb(six, wait_ix);
            if (!block_count) {
                /* 
                 * We brought it down to zero ourselves, so no need to wait.
                 * Since the counter is guaranteed to be monotonically
                 * decreasing (disregarding our own operations) it is safe
                 * to continue. Restore "our increment" in preparation for
                 * next switch.
                 */
                rla_bc_inc(six, wait_ix);
                continue;
            }

            /*
             * Wait for counter to be brought down to zero. The one bringing
             * the counter down to zero will wake us up. We might also be
             * woken later in erts_internal:wait_release_literal_area_switch()
             * if a new area appears (handled here below).
             */
            release_literal_areas.wait_sched_ix = six;
            release_literal_areas.block_state = ERTS_RLA_BLOCK_STATE_WAITING;
            if (!ERTS_COPY_LITERAL_AREA()) {
                rla_switch_area();
                new_area = ERTS_COPY_LITERAL_AREA();
                if (new_area) {
                    /*
                     * A new area showed up. Start the work with that area
                     * and come back and check block counters when that has
                     * been handled.
                     */
                    old_area = release_literal_areas.block_area;
                    goto publish_new_info;
                }
            }

            /*
             * Wait for block_counter to reach zero or a new literal area
             * to handle...
             */
            BIF_TRAP1(wait_release_literal_area_switch, BIF_P, am_copy_literals);
        }

        /* Done checking all block counters, release the literal area... */

        release_literal_areas.block_state = ERTS_RLA_BLOCK_STATE_NONE;
        erts_release_literal_area(release_literal_areas.block_area);
        release_literal_areas.block_area = NULL;

#ifdef DEBUG
        /* All counters should be at 1; ready for next switch... */
        for (six = 0; six < erts_no_schedulers; six++) {
            ASSERT(1 == rla_bc_read(six, wait_ix));
        }
#endif
    }
}

void
erts_purge_state_add_fun(ErlFunEntry *fe)
{
    ASSERT(is_value(purge_state.module));
    if (purge_state.fe_ix >= purge_state.fe_size) {
	ErlFunEntry **funs;
	purge_state.fe_size += 100;
	funs = erts_alloc(ERTS_ALC_T_PURGE_DATA,
			  sizeof(ErlFunEntry *)*purge_state.fe_size);
	sys_memcpy((void *) funs,
		   (void *) purge_state.funs,
		   purge_state.fe_ix*sizeof(ErlFunEntry *));
	if (purge_state.funs != &purge_state.def_funs[0])
	    erts_free(ERTS_ALC_T_PURGE_DATA, purge_state.funs);
	purge_state.funs = funs;
    }
    purge_state.funs[purge_state.fe_ix++] = fe;
}

Export *
erts_suspend_process_on_pending_purge_lambda(Process *c_p, ErlFunEntry* fe)
{
    erts_mtx_lock(&purge_state.mtx);
    if (purge_state.module == fe->module) {
	/*
	 * The process c_p is about to call a fun in the code
	 * that we are trying to purge. Suspend it and call
	 * erts_code_purger:pending_purge_lambda/3. The process
	 * will be resumed when the purge completes or aborts,
	 * and will then try to do the call again.
	 */
	if (purge_state.sp_ix >= purge_state.sp_size) {
	    Eterm *sprocs;
	    purge_state.sp_size += 100;
	    sprocs = erts_alloc(ERTS_ALC_T_PURGE_DATA,
				(sizeof(ErlFunEntry *)
				 * purge_state.sp_size));
	    sys_memcpy((void *) sprocs,
		       (void *) purge_state.sprocs,
		       purge_state.sp_ix*sizeof(ErlFunEntry *));
	    if (purge_state.sprocs != &purge_state.def_sprocs[0])
		erts_free(ERTS_ALC_T_PURGE_DATA, purge_state.sprocs);
	    purge_state.sprocs = sprocs;
	}
	purge_state.sprocs[purge_state.sp_ix++] = c_p->common.id;
	erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
	ERTS_VBUMP_ALL_REDS(c_p);
    }
    erts_mtx_unlock(&purge_state.mtx);
    return purge_state.pending_purge_lambda;
}

static void
finalize_purge_operation(Process *c_p, int succeded)
{
    Uint ix;

    if (c_p)
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);

    erts_mtx_lock(&purge_state.mtx);

    ASSERT(purge_state.module != THE_NON_VALUE);

    purge_state.module = THE_NON_VALUE;

    /*
     * Resume all processes that have tried to call
     * funs in this code.
     */
    for (ix = 0; ix < purge_state.sp_ix; ix++) {
	Process *rp = erts_pid2proc(NULL, 0,
				    purge_state.sprocs[ix],
				    ERTS_PROC_LOCK_STATUS);
	if (rp) {
	    erts_resume(rp, ERTS_PROC_LOCK_STATUS);
	    erts_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	}
    }

    erts_mtx_unlock(&purge_state.mtx);

    if (c_p)
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

    if (purge_state.sprocs != &purge_state.def_sprocs[0]) {
	erts_free(ERTS_ALC_T_PURGE_DATA, purge_state.sprocs);
	purge_state.sprocs = &purge_state.def_sprocs[0];
	purge_state.sp_size = sizeof(purge_state.def_sprocs);
	purge_state.sp_size /= sizeof(purge_state.def_sprocs[0]);
    }
    purge_state.sp_ix = 0;

    if (purge_state.funs != &purge_state.def_funs[0]) {
	erts_free(ERTS_ALC_T_PURGE_DATA, purge_state.funs);
	purge_state.funs = &purge_state.def_funs[0];
	purge_state.fe_size = sizeof(purge_state.def_funs);
	purge_state.fe_size /= sizeof(purge_state.def_funs[0]);
    }
    purge_state.fe_ix = 0;
}


static ErtsThrPrgrLaterOp purger_lop_data;

static void
resume_purger(void *unused)
{
    Process *p = erts_code_purger;
    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    erts_resume(p, ERTS_PROC_LOCK_STATUS);
    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
}

static void
finalize_purge_abort(void *unused)
{
    erts_fun_purge_abort_finalize(purge_state.funs, purge_state.fe_ix);

    finalize_purge_operation(NULL, 0);

    resume_purger(NULL);
}


BIF_RETTYPE erts_internal_purge_module_2(BIF_ALIST_2)
{
    if (BIF_P != erts_code_purger)
	BIF_ERROR(BIF_P, EXC_NOTSUP);

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    switch (BIF_ARG_2) {

    case am_prepare:
    case am_prepare_on_load: {
	/*
	 * Prepare for purge by marking all fun
	 * entries referring to the code to purge
	 * with "pending purge" markers.
	 */
	ErtsCodeIndex code_ix;
	Module* modp;
	Eterm res;

	if (is_value(purge_state.module))
	    BIF_ERROR(BIF_P, BADARG);

	code_ix = erts_active_code_ix();

	/*
	 * Correct module?
	 */
	modp = erts_get_module(BIF_ARG_1, code_ix);
	if (!modp)
	    res = am_false;
	else {
	    /*
	     * Any code to purge?
	     */

	    if (BIF_ARG_2 == am_prepare_on_load) {
                erts_rwlock_old_code(code_ix);
	    } else {
		erts_rlock_old_code(code_ix);
	    }

	    if (BIF_ARG_2 == am_prepare_on_load) {
		ASSERT(modp->on_load);
		ASSERT(modp->on_load->code_hdr);
		purge_state.saved_old = modp->old;
		modp->old = *modp->on_load;
		erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) modp->on_load);
		modp->on_load = 0;
	    }

	    if (!modp->old.code_hdr)
		res = am_false;
	    else {
		BeamInstr* code;
		BeamInstr* end;
		erts_mtx_lock(&purge_state.mtx);
		purge_state.module = BIF_ARG_1;
		erts_mtx_unlock(&purge_state.mtx);
		res = am_true;
		code = (BeamInstr*) modp->old.code_hdr;
		end = (BeamInstr *)((char *)code + modp->old.code_length);
		erts_fun_purge_prepare(code, end);
	    }

            if (BIF_ARG_2 == am_prepare_on_load) {
                erts_rwunlock_old_code(code_ix);
	    } else {
                erts_runlock_old_code(code_ix);
	    }
	}
	
	if (res != am_true)
	    BIF_RET(res);
	else {
	    /*
	     * We'll be resumed when all schedulers are guaranteed
	     * to see the "pending purge" markers that we've made on
	     * all fun entries of the code that we are about to purge.
	     * Processes trying to call these funs will be suspended
	     * before calling the funs. That is we are guaranteed not
	     * to get any more direct references into the code while
	     * checking for such references...
	     */
	    erts_schedule_thr_prgr_later_op(resume_purger,
					    NULL,
					    &purger_lop_data);
	    erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
	    ERTS_BIF_YIELD_RETURN(BIF_P, am_true);
	}
    }

    case am_abort: {
	/*
	 * Soft purge that detected direct references into the code
	 * we set out to purge. Abort the purge.
	 */

	if (purge_state.module != BIF_ARG_1)
	    BIF_ERROR(BIF_P, BADARG);

	erts_fun_purge_abort_prepare(purge_state.funs, purge_state.fe_ix);

	/*
	 * We need to restore the code addresses of the funs in
	 * two stages in order to ensure that we do not get any
	 * stale suspended processes due to the purge abort.
	 * Restore address pointer (erts_fun_purge_abort_prepare);
	 * wait for thread progress; clear pending purge address
	 * pointer (erts_fun_purge_abort_finalize), and then
	 * resume processes that got suspended
	 * (finalize_purge_operation).
	 */
	erts_schedule_thr_prgr_later_op(finalize_purge_abort,
					NULL,
					&purger_lop_data);
	erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, NULL);
	ERTS_BIF_YIELD_RETURN(BIF_P, am_false);
    }

    case am_complete: {
	ErtsCodeIndex code_ix;
	BeamInstr* code;
	Module* modp;
	int is_blocking = 0;
	Eterm ret;
	ErtsLiteralArea *literals = NULL;


	/*
	 * We have no direct references into the code.
	 * Complete to purge.
	 */

	if (purge_state.module != BIF_ARG_1)
	    BIF_ERROR(BIF_P, BADARG);

	if (!erts_try_seize_code_write_permission(BIF_P)) {
	    ERTS_BIF_YIELD2(bif_export[BIF_erts_internal_purge_module_2],
			    BIF_P, BIF_ARG_1, BIF_ARG_2);
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
		if (modp->old.nif != NULL
		    || IF_HIPE(hipe_purge_need_blocking(modp))) {
		    /* ToDo: Do unload nif without blocking */
		    erts_rwunlock_old_code(code_ix);
		    erts_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
		    erts_thr_progress_block();
		    is_blocking = 1;
		    erts_rwlock_old_code(code_ix);
		    if (modp->old.nif) {
		      erts_unload_nif(modp->old.nif);
		      modp->old.nif = NULL;
		    }
		}

		/*
		 * Remove the old code.
		 */
		ASSERT(erts_total_code_size >= modp->old.code_length);
		erts_total_code_size -= modp->old.code_length;
		code = (BeamInstr*) modp->old.code_hdr;
		erts_fun_purge_complete(purge_state.funs, purge_state.fe_ix);
		beam_catches_delmod(modp->old.catches, code, modp->old.code_length,
				    code_ix);
		literals = modp->old.code_hdr->literal_area;
		modp->old.code_hdr->literal_area = NULL;
		erts_free(ERTS_ALC_T_CODE, (void *) code);
		modp->old.code_hdr = NULL;
		modp->old.code_length = 0;
		modp->old.catches = BEAM_CATCHES_NIL;
		erts_remove_from_ranges(code);
#ifdef HIPE
		hipe_purge_module(modp, is_blocking);
#endif
		ERTS_BIF_PREP_RET(ret, am_true);
	    }

	    if (purge_state.saved_old.code_hdr) {
		modp->old = purge_state.saved_old;
		purge_state.saved_old.code_hdr = 0;
	    }
	    erts_rwunlock_old_code(code_ix);
	}
	if (is_blocking) {
	    erts_thr_progress_unblock();
	    erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	}

	erts_release_code_write_permission();

	finalize_purge_operation(BIF_P, ret == am_true);

	if (literals) {
            erts_queue_release_literals(BIF_P, literals);
	}

	return ret;
    }

    default:
	BIF_ERROR(BIF_P, BADARG);

    }
}

void
erts_queue_release_literals(Process* c_p, ErtsLiteralArea* literals)
{
    ErtsLiteralAreaRef *ref;
    ErtsMessage *mp;
    ref = erts_alloc(ERTS_ALC_T_LITERAL_REF,
                     sizeof(ErtsLiteralAreaRef));
    ref->literal_area = literals;
    ref->next = NULL;
    erts_mtx_lock(&release_literal_areas.mtx);
    if (release_literal_areas.last) {
        release_literal_areas.last->next = ref;
        release_literal_areas.last = ref;
    } else {
        release_literal_areas.first = ref;
        release_literal_areas.last = ref;
    }
    erts_mtx_unlock(&release_literal_areas.mtx);
    mp = erts_alloc_message(0, NULL);
    ERL_MESSAGE_TOKEN(mp) = am_undefined;
    if (c_p == NULL) {
        erts_queue_message(erts_literal_area_collector,
                           0,
                           mp,
                           am_copy_literals,
                           am_system);
    } else {
        erts_queue_proc_message(c_p,
                                erts_literal_area_collector,
                                0,
                                mp,
                                am_copy_literals);
    }
}

void
erts_debug_foreach_release_literal_area_off_heap(void (*func)(ErlOffHeap *, void *), void *arg)
{
    int i;
    ErtsLiteralArea *lareas[2];
    ErtsLiteralArea *lap;
    ErlOffHeap oh;
    ErtsLiteralAreaRef *ref;
    erts_mtx_lock(&release_literal_areas.mtx);
    for (ref = release_literal_areas.first; ref; ref = ref->next) {
        lap = ref->literal_area;
        if (!erts_debug_have_accessed_literal_area(lap)) {
            ERTS_INIT_OFF_HEAP(&oh);
            oh.first = lap->off_heap;
            (*func)(&oh, arg);
            erts_debug_save_accessed_literal_area(lap);
        }
    }
    erts_mtx_unlock(&release_literal_areas.mtx);
    lareas[0] = ERTS_COPY_LITERAL_AREA();
    lareas[1] = release_literal_areas.block_area;
    for (i = 0; i < sizeof(lareas)/sizeof(lareas[0]); i++) {
        lap = lareas[i];
        if (lap && !erts_debug_have_accessed_literal_area(lap)) {
           ERTS_INIT_OFF_HEAP(&oh);
            oh.first = lap->off_heap;
            (*func)(&oh, arg);
            erts_debug_save_accessed_literal_area(lap);
        }
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
    int i, num_exps = export_list_size(code_ix);

    for (i = 0; i < num_exps; i++) {
	Export *ep = export_list(i, code_ix);
        if (ep != NULL && (ep->info.mfa.module == module)) {
	    if (ep->addressv[code_ix] == ep->beam) {
		if (BeamIsOpCode(ep->beam[0], op_apply_bif)) {
		    continue;
		}
		else if (BeamIsOpCode(ep->beam[0], op_i_generic_breakpoint)) {
		    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
		    ASSERT(modp->curr.num_traced_exports > 0);
		    DBG_TRACE_MFA_P(&ep->info.mfa,
				  "export trace cleared, code_ix=%d", code_ix);
		    erts_clear_export_break(modp, &ep->info);
		}
		else {
                    ASSERT(BeamIsOpCode(ep->beam[0], op_call_error_handler) ||
                           !erts_initialized);
                }
            }
	    ep->addressv[code_ix] = ep->beam;
	    ep->beam[0] = BeamOpCodeAddr(op_call_error_handler);
	    ep->beam[1] = 0;
	    DBG_TRACE_MFA_P(&ep->info.mfa,
			    "export invalidation, code_ix=%d", code_ix);
	}
    }

    ASSERT(modp->curr.num_breakpoints == 0);
    ASSERT(modp->curr.num_traced_exports == 0);
    modp->old = modp->curr;
    erts_module_instance_init(&modp->curr);
}


Eterm
beam_make_current_old(Process *c_p, ErtsProcLocks c_p_locks, Eterm module)
{
    Module* modp = erts_put_module(module);

    /*
     * Check if the previous code has been already deleted;
     * if not, delete old code; error if old code already exists.
     */

    if (modp->curr.code_hdr) {
	if (modp->old.code_hdr)  {
	    return am_not_purged;
	}
	/* Make the current version old. */
	delete_code(modp);
    }
    return NIL;
}
