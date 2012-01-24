/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2011. All Rights Reserved.
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

static void set_default_trace_pattern(Eterm module, ErtsCodeIndex);
static Eterm check_process_code(Process* rp, Module* modp);
static void ensure_no_breakpoints(Process *, ErtsProcLocks, Eterm module);
static void delete_code(Process *c_p, ErtsProcLocks c_p_locks, Module* modp);
static void delete_export_references(Eterm module);
static int purge_module(Process*, int module);
static void decrement_refc(BeamInstr* code);
static int is_native(BeamInstr* code);
static int any_heap_ref_ptrs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);
static int any_heap_refs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);

Eterm
load_module_2(BIF_ALIST_2)
{
    Eterm   reason;
    Eterm*  hp;
    int      sz;
    byte*    code;
    Eterm res;
    byte* temp_alloc = NULL;
    struct LoaderState* stp;

    if (is_not_atom(BIF_ARG_1)) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((code = erts_get_aligned_binary_bytes(BIF_ARG_2, &temp_alloc)) == NULL) {
	goto error;
    }
    hp = HAlloc(BIF_P, 3);

    /*
     * Read the BEAM file and prepare the module for loading.
     */
    stp = erts_alloc_loader_state();
    sz = binary_size(BIF_ARG_2);
    reason = erts_prepare_loading(stp, BIF_P, BIF_P->group_leader,
				  &BIF_ARG_1, code, sz);
    erts_free_aligned_binary_bytes(temp_alloc);
    if (reason != NIL) {
	res = TUPLE2(hp, am_error, reason);
	BIF_RET(res);
    }

    /*SVERK
     * Stop all other processes and finish the loading of the module.
     *
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();*/

    ensure_no_breakpoints(BIF_P, 0, BIF_ARG_1);

    erts_lock_code_ix();
    erts_start_loader_code_ix();

    reason = erts_finish_loading(stp, BIF_P, 0, &BIF_ARG_1);
    if (reason != NIL) {
	if (reason == am_on_load) {
	    erts_commit_loader_code_ix();
	}
	else {
	    erts_abort_loader_code_ix();
	}
	res = TUPLE2(hp, am_error, reason);
    } else {
	set_default_trace_pattern(BIF_ARG_1, erts_loader_code_ix());
	erts_commit_loader_code_ix();
	res = TUPLE2(hp, am_module, BIF_ARG_1);
    }

    erts_unlock_code_ix();

    /*SVERK
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);*/
    BIF_RET(res);
}

BIF_RETTYPE purge_module_1(BIF_ALIST_1)
{
    int purge_res;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    /*SVERK
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    erts_export_consolidate(erts_active_code_ix());*/

    purge_res = purge_module(BIF_P, atom_val(BIF_ARG_1));

    /*SVERK
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);*/

    if (purge_res < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
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
    res = ((modp->curr.code && is_native(modp->curr.code)) ||
	    (modp->old.code != 0 && is_native(modp->old.code))) ?
		am_true : am_false;
    erts_runlock_old_code(code_ix);
    return res;
}

BIF_RETTYPE code_make_stub_module_3(BIF_ALIST_3)
{
    Eterm res;

    /*SVERK
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    erts_export_consolidate(erts_active_code_ix());*/

    ensure_no_breakpoints(BIF_P, 0, BIF_ARG_1);

    erts_lock_code_ix();
    erts_start_loader_code_ix();

    res = erts_make_stub_module(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (res == BIF_ARG_1) {
	erts_commit_loader_code_ix();
    }
    else {
	erts_abort_loader_code_ix();
    }
    erts_unlock_code_ix();

    /*SVERK
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);*/
    return res;
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
	if (modp->old.code != NULL) {
	    res = am_true;
	}
	erts_runlock_old_code(code_ix);
    }
    BIF_RET(res);
}

Eterm
check_process_code_2(BIF_ALIST_2)
{
    Process* rp;
    Module* modp;

    if (is_not_atom(BIF_ARG_2)) {
	goto error;
    }
    if (is_internal_pid(BIF_ARG_1)) {
	Eterm res;
	ErtsCodeIndex code_ix;
	if (internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	    goto error;
	code_ix = erts_active_code_ix();
	modp = erts_get_module(BIF_ARG_2, code_ix);
	if (modp == NULL) {		/* Doesn't exist. */
	    return am_false;
	}
	erts_rlock_old_code(code_ix);
	if (modp->old.code == NULL) { /* No old code. */
	    erts_runlock_old_code(code_ix);
	    return am_false;
	}
	erts_runlock_old_code(code_ix);
	
#ifdef ERTS_SMP
	rp = erts_pid2proc_suspend(BIF_P, ERTS_PROC_LOCK_MAIN,
				   BIF_ARG_1, ERTS_PROC_LOCK_MAIN);
#else
	rp = erts_pid2proc(BIF_P, 0, BIF_ARG_1, 0);
#endif
	if (!rp) {
	    BIF_RET(am_false);
	}
	if (rp == ERTS_PROC_LOCK_BUSY) {
	    ERTS_BIF_YIELD2(bif_export[BIF_check_process_code_2], BIF_P,
			    BIF_ARG_1, BIF_ARG_2);
	}
	erts_rlock_old_code(code_ix);
	if (modp->old.code != NULL) { /* must check again */
	    res = check_process_code(rp, modp);
	}
	else {
	    res = am_false;
	}
	erts_runlock_old_code(code_ix);
#ifdef ERTS_SMP
	if (BIF_P != rp) {
	    erts_resume(rp, ERTS_PROC_LOCK_MAIN);
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
	}
#endif
	BIF_RET(res);
    }
    else if (is_external_pid(BIF_ARG_1)
	     && external_pid_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	BIF_RET(am_false);
    }

 error:
    BIF_ERROR(BIF_P, BADARG);
}


BIF_RETTYPE delete_module_1(BIF_ALIST_1)
{
    ErtsCodeIndex code_ix;
    int res;

    if (is_not_atom(BIF_ARG_1))
	goto badarg;

    /*SVERK
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();*/

    ensure_no_breakpoints(BIF_P, 0, BIF_ARG_1);

    erts_lock_code_ix();
    erts_start_loader_code_ix();
    code_ix = erts_loader_code_ix();
    {
	Module *modp = erts_get_module(BIF_ARG_1, code_ix);
	if (!modp) {
	    res = am_undefined;
	}
	else if (modp->old.code != 0) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Module %T must be purged before loading\n",
			  BIF_ARG_1);
	    erts_send_error_to_logger(BIF_P->group_leader, dsbufp);
	    res = am_badarg;
	}
	else {
	    delete_export_references(BIF_ARG_1);
	    delete_code(BIF_P, 0, modp);
	    res = am_true;
	}
    }

    if (res == am_true) {
	erts_commit_loader_code_ix();
    }
    else {
	erts_abort_loader_code_ix();
    }
    erts_unlock_code_ix();

    /*SVERK
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);*/

    if (res == am_badarg) {
    badarg:
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
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
    ErtsCodeIndex code_ix = erts_active_code_ix();  /*SVERK ?? on_load ?? */
    Module* modp = erts_get_module(BIF_ARG_1, code_ix);
    Eterm on_load = 0;

    if (modp) {
	if (modp->curr.code) {
	    on_load = modp->curr.code[MI_ON_LOAD_FUNCTION_PTR];
	}
    }
    if (on_load) {
	BIF_TRAP_CODE_PTR_0(BIF_P, on_load);
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

    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);

    if (!modp || modp->curr.code == 0) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((on_load = modp->curr.code[MI_ON_LOAD_FUNCTION_PTR]) == 0) {
	goto error;
    }
    if (BIF_ARG_2 != am_false && BIF_ARG_2 != am_true) {
	goto error;
    }

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    /*SVERK Use code_ix switch instead */

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
	set_default_trace_pattern(BIF_ARG_1, erts_active_code_ix ());
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
    BIF_RET(am_true);
}

static void
set_default_trace_pattern(Eterm module, ErtsCodeIndex code_ix)
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
	(void) erts_set_trace_pattern(mfa, 1,
				      match_spec,
				      meta_match_spec,
				      1, trace_pattern_flags,
				      meta_tracer_pid,
				      code_ix);
    }
}

static Eterm
check_process_code(Process* rp, Module* modp)
{
    BeamInstr* start;
    char* mod_start;
    Uint mod_size;
    BeamInstr* end;
    Eterm* sp;
#ifndef HYBRID /* FIND ME! */
    struct erl_off_heap_header* oh;
    int done_gc = 0;
#endif

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

    /*
     * See if there are funs that refer to the old version of the module.
     */

#ifndef HYBRID /* FIND ME! */
 rescan:
    for (oh = MSO(rp).first; oh; oh = oh->next) {
	if (thing_subtag(oh->thing_word) == FUN_SUBTAG) {
	    ErlFunThing* funp = (ErlFunThing*) oh;

	    if (INSIDE((BeamInstr *) funp->fe->address)) {
		if (done_gc) {
		    return am_true;
		} else {
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
		    (void) erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
		    goto rescan;
		}
	    }
	}
    }
#endif

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

	    /*
	     * Try to get rid of constants by by garbage collecting.
	     * Clear both fvalue and ftrace.
	     */
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	    done_gc = 1;
	    FLAGS(rp) |= F_NEED_FULLSWEEP;
	    (void) erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
	    literals = (Eterm *) modp->old.code[MI_LITERALS_START];
	    lit_size = (Eterm *) modp->old.code[MI_LITERALS_END] - literals;
	    oh = (struct erl_off_heap_header *)
		modp->old.code[MI_LITERALS_OFF_HEAP];
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


static int
purge_module(Process* c_p, int module)
{
    ErtsCodeIndex code_ix;
    BeamInstr* code;
    BeamInstr* end;
    Module* modp;
    int is_blocked = 0;
    int ret;

    erts_lock_code_ix();
retry:
    code_ix = erts_active_code_ix();

    /*
     * Correct module?
     */

    if ((modp = erts_get_module(make_atom(module), code_ix)) == NULL) {
	ret = -2;
    }
    else {
	erts_rwlock_old_code(code_ix);

	/*
	 * Any code to purge?
	 */
	if (modp->old.code == 0) {
	    ret = -1;
	}
	else {
	    /*
	     * Unload any NIF library
	     */
	    if (modp->old.nif != NULL) {
		if (!is_blocked) {
		    /*SVERK Do unload nif without blocking */
		    erts_rwunlock_old_code(code_ix);
		    erts_unlock_code_ix();
		    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
		    erts_smp_thr_progress_block();
		    is_blocked = 1;
		    goto retry;
		}

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
	    ret = 0;
	}
	erts_rwunlock_old_code(code_ix);
    }
    if (is_blocked) {
	erts_smp_thr_progress_unblock();
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
    else {
	erts_unlock_code_ix();
    }
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
 * Clear breakpoints in module if any
 */
static void ensure_no_breakpoints(Process *c_p, ErtsProcLocks c_p_locks,
				  Eterm module)
{
    ErtsCodeIndex code_ix = erts_active_code_ix();
    Module* modp = erts_get_module(module, code_ix);

    if (modp && modp->curr.code != NULL
	&& modp->curr.code[MI_NUM_BREAKPOINTS] > 0) {
#ifdef ERTS_ENABLE_LOCK_CHECK
#ifdef ERTS_SMP
	if (c_p && c_p_locks)
	    erts_proc_lc_chk_only_proc_main(c_p);
	else
#endif
	    erts_lc_check_exact(NULL, 0);
#endif
	if (c_p && c_p_locks)
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_smp_thr_progress_block();
	erts_clear_module_break(modp);
	modp->curr.code[MI_NUM_BREAKPOINTS] = 0;
	erts_smp_thr_progress_unblock();
	if (c_p && c_p_locks)
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
}

/*
 * Move code from current to old.
 */

static void
delete_code(Process *c_p, ErtsProcLocks c_p_locks, Module* modp)
{
    ASSERT(!(modp->curr.code && modp->curr.code[MI_NUM_BREAKPOINTS] > 0));
    modp->old = modp->curr;
    modp->curr.code = NULL;
    modp->curr.code_length = 0;
    modp->curr.catches = BEAM_CATCHES_NIL;
    modp->curr.nif = NULL;
}


/* null all references on the export table for the module called with the
   atom index below */

static void
delete_export_references(Eterm module)
{
    ErtsCodeIndex code_ix = erts_loader_code_ix();
    int i;

    ASSERT(is_atom(module));

    for (i = 0; i < export_list_size(code_ix); i++) {
	Export *ep = export_list(i, code_ix);
        if (ep != NULL && (ep->code[0] == module)) {
	    if (ep->addressv[code_ix] == ep->code+3 &&
		(ep->code[3] == (BeamInstr) em_apply_bif)) {
		continue;
	    }
	    ep->addressv[code_ix] = ep->code+3;
	    ASSERT(ep->code[3] != (BeamInstr) em_call_traced_function); /*SVERK What to do now? */
	    ep->code[3] = (BeamInstr) em_call_error_handler;
	    ep->code[4] = 0;
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = NULL;
	}
    }
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
	delete_code(c_p, c_p_locks, modp);
	delete_export_references(module);
    }
    return NIL;
}

static int
is_native(BeamInstr* code)
{
    return ((Eterm *)code[MI_FUNCTIONS])[1] != 0;
}


