/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

static void set_default_trace_pattern(Eterm module);
static Eterm check_process_code(Process* rp, Module* modp);
static void delete_code(Process *c_p, ErtsProcLocks c_p_locks, Module* modp);
static void delete_export_references(Eterm module);
static int purge_module(int module);
static int is_native(BeamInstr* code);
static int any_heap_ref_ptrs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);
static int any_heap_refs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);
static void remove_from_address_table(BeamInstr* code);

Eterm
load_module_2(BIF_ALIST_2)
{
    Eterm   reason;
    Eterm*  hp;
    int      i;
    int      sz;
    byte*    code;
    Eterm res;
    byte* temp_alloc = NULL;

    if (is_not_atom(BIF_ARG_1)) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((code = erts_get_aligned_binary_bytes(BIF_ARG_2, &temp_alloc)) == NULL) {
	goto error;
    }
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    erts_export_consolidate();

    hp = HAlloc(BIF_P, 3);
    sz = binary_size(BIF_ARG_2);
    if ((i = erts_load_module(BIF_P, 0,
			      BIF_P->group_leader, &BIF_ARG_1, code, sz)) < 0) { 
	switch (i) {
	case -1: reason = am_badfile; break; 
	case -2: reason = am_nofile; break;
	case -3: reason = am_not_purged; break;
	case -4:
	    reason = am_atom_put("native_code", sizeof("native_code")-1);
	    break;
	case -5:
	    {
		/*
		 * The module contains an on_load function. The loader
		 * has loaded the module as usual, except that the
		 * export entries does not point into the module, so it
		 * is not possible to call any code in the module.
		 */

		ERTS_DECL_AM(on_load);
		reason = AM_on_load;
		break;
	    }
	default: reason = am_badfile; break;
	}
	res = TUPLE2(hp, am_error, reason);
	goto done;
    }

    set_default_trace_pattern(BIF_ARG_1);
    res = TUPLE2(hp, am_module, BIF_ARG_1);

 done:
    erts_free_aligned_binary_bytes(temp_alloc);
    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    BIF_RET(res);
}

BIF_RETTYPE purge_module_1(BIF_ALIST_1)
{
    int purge_res;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    erts_export_consolidate();
    purge_res = purge_module(atom_val(BIF_ARG_1));

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    if (purge_res < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE code_is_module_native_1(BIF_ALIST_1)
{
    Module* modp;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((modp = erts_get_module(BIF_ARG_1)) == NULL) {
	return am_undefined;
    }
    return (is_native(modp->code) ||
	    (modp->old_code != 0 && is_native(modp->old_code))) ?
		am_true : am_false;
}

BIF_RETTYPE code_make_stub_module_3(BIF_ALIST_3)
{
    Eterm res;

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    erts_export_consolidate();
    res = erts_make_stub_module(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    return res;
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
	if (internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	    goto error;
	rp = erts_pid2proc_not_running(BIF_P, ERTS_PROC_LOCK_MAIN,
				       BIF_ARG_1, ERTS_PROC_LOCK_MAIN);
	if (!rp) {
	    BIF_RET(am_false);
	}
	if (rp == ERTS_PROC_LOCK_BUSY) {
	    ERTS_BIF_YIELD2(bif_export[BIF_check_process_code_2], BIF_P,
			    BIF_ARG_1, BIF_ARG_2);
	}
	modp = erts_get_module(BIF_ARG_2);
	res = check_process_code(rp, modp);
#ifdef ERTS_SMP
	if (BIF_P != rp)
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
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
    int res;

    if (is_not_atom(BIF_ARG_1))
	goto badarg;

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    {
	Module *modp = erts_get_module(BIF_ARG_1);
	if (!modp) {
	    res = am_undefined;
	}
	else if (modp->old_code != 0) {
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

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    if (res == am_badarg) {
    badarg:
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
}

BIF_RETTYPE module_loaded_1(BIF_ALIST_1)
{
    Module* modp;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((modp = erts_get_module(BIF_ARG_1)) == NULL ||
	modp->code == NULL ||
	modp->code[MI_ON_LOAD_FUNCTION_PTR] != 0) {
	BIF_RET(am_false);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE pre_loaded_0(BIF_ALIST_0)
{
    return erts_preloaded(BIF_P);
}

BIF_RETTYPE loaded_0(BIF_ALIST_0)
{
    Eterm previous = NIL;
    Eterm* hp;
    int i;
    int j = 0;
    
    for (i = 0; i < module_code_size(); i++) {
	if (module_code(i) != NULL &&
	    ((module_code(i)->code_length != 0) ||
	     (module_code(i)->old_code_length != 0))) {
	    j++;
	}
    }
    if (j > 0) {
	hp = HAlloc(BIF_P, j*2);

	for (i = 0; i < module_code_size(); i++) {
	    if (module_code(i) != NULL &&
		((module_code(i)->code_length != 0) ||
		 (module_code(i)->old_code_length != 0))) {
		previous = CONS(hp, make_atom(module_code(i)->module), 
				previous);
		hp += 2;
	    }
	}
    }
    BIF_RET(previous);
}

BIF_RETTYPE call_on_load_function_1(BIF_ALIST_1)
{
    Module* modp = erts_get_module(BIF_ARG_1);
    Eterm on_load;

    if (!modp || modp->code == 0) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((on_load = modp->code[MI_ON_LOAD_FUNCTION_PTR]) == 0) {
	goto error;
    }
    BIF_TRAP_CODE_PTR_0(BIF_P, on_load);
}

BIF_RETTYPE finish_after_on_load_2(BIF_ALIST_2)
{
    Module* modp = erts_get_module(BIF_ARG_1);
    Eterm on_load;

    if (!modp || modp->code == 0) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((on_load = modp->code[MI_ON_LOAD_FUNCTION_PTR]) == 0) {
	goto error;
    }
    if (BIF_ARG_2 != am_false && BIF_ARG_2 != am_true) {
	goto error;
    }

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    if (BIF_ARG_2 == am_true) {
	int i;

	/*
	 * The on_load function succeded. Fix up export entries.
	 */
	for (i = 0; i < export_list_size(); i++) {
	    Export *ep = export_list(i);
	    if (ep != NULL &&
		ep->code[0] == BIF_ARG_1 &&
		ep->code[4] != 0) {
		ep->address = (void *) ep->code[4];
		ep->code[4] = 0;
	    }
	}
	modp->code[MI_ON_LOAD_FUNCTION_PTR] = 0;
	set_default_trace_pattern(BIF_ARG_1);
    } else if (BIF_ARG_2 == am_false) {
	BeamInstr* code;
	BeamInstr* end;

	/*
	 * The on_load function failed. Remove the loaded code.
	 * This is an combination of delete and purge. We purge
	 * the current code; the old code is not touched.
	 */
	erts_total_code_size -= modp->code_length;
	code = modp->code;
	end = (BeamInstr *)((char *)code + modp->code_length);
	erts_cleanup_funs_on_purge(code, end);
	beam_catches_delmod(modp->catches, code, modp->code_length);
	erts_free(ERTS_ALC_T_CODE, (void *) code);
	modp->code = NULL;
	modp->code_length = 0;
	modp->catches = BEAM_CATCHES_NIL;
	remove_from_address_table(code);
    }
    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
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
	(void) erts_set_trace_pattern(mfa, 1,
				      match_spec,
				      meta_match_spec,
				      1, trace_pattern_flags,
				      meta_tracer_pid);
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
    if (modp == NULL) {		/* Doesn't exist. */
	return am_false;
    } else if (modp->old_code == NULL) { /* No old code. */
	return am_false;
    }

    /*
     * Pick up limits for the module.
     */
    start = modp->old_code;
    end = (BeamInstr *)((char *)start + modp->old_code_length);
    mod_start = (char *) start;
    mod_size = modp->old_code_length;

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
	    literals = (Eterm *) modp->old_code[MI_LITERALS_START];
	    lit_size = (Eterm *) modp->old_code[MI_LITERALS_END] - literals;
	    erts_garbage_collect_literals(rp, literals, lit_size);
	}
    }
    return am_false;
#undef INSIDE
}

#define in_area(ptr,start,nbytes) \
    ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

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
purge_module(int module)
{
    BeamInstr* code;
    BeamInstr* end;
    Module* modp;

    /*
     * Correct module?
     */

    if ((modp = erts_get_module(make_atom(module))) == NULL) {
	return -2;
    }

    /*
     * Any code to purge?
     */
    if (modp->old_code == 0) {
	if (display_loads) {
	    erts_printf("No code to purge for %T\n", make_atom(module));
	}
	return -1;
    }

    /*
     * Unload any NIF library
     */
    if (modp->old_nif != NULL) {
	erts_unload_nif(modp->old_nif);
	modp->old_nif = NULL;
    }

    /*
     * Remove the old code.
     */
    ASSERT(erts_total_code_size >= modp->old_code_length);
    erts_total_code_size -= modp->old_code_length;
    code = modp->old_code;
    end = (BeamInstr *)((char *)code + modp->old_code_length);
    erts_cleanup_funs_on_purge(code, end);
    beam_catches_delmod(modp->old_catches, code, modp->old_code_length);
    erts_free(ERTS_ALC_T_CODE, (void *) code);
    modp->old_code = NULL;
    modp->old_code_length = 0;
    modp->old_catches = BEAM_CATCHES_NIL;
    remove_from_address_table(code);
    return 0;
}

static void
remove_from_address_table(BeamInstr* code)
{
    int i;

    for (i = 0; i < num_loaded_modules; i++) {
	if (modules[i].start == code) {
	    num_loaded_modules--;
	    while (i < num_loaded_modules) {
		modules[i] = modules[i+1];
		i++;
	    }
	    mid_module = &modules[num_loaded_modules/2];
	    return;
	}
    }
    ASSERT(0);			/* Not found? */
}


/*
 * Move code from current to old.
 */

static void 
delete_code(Process *c_p, ErtsProcLocks c_p_locks, Module* modp)
{
#ifdef ERTS_ENABLE_LOCK_CHECK
#ifdef ERTS_SMP
    if (c_p && c_p_locks)
	erts_proc_lc_chk_only_proc_main(c_p);
    else
#endif
	erts_lc_check_exact(NULL, 0);
#endif

    /*
     * Clear breakpoints if any
     */
    if (modp->code != NULL && modp->code[MI_NUM_BREAKPOINTS] > 0) {
	if (c_p && c_p_locks)
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);
	erts_clear_module_break(modp);
	modp->code[MI_NUM_BREAKPOINTS] = 0;
	erts_smp_release_system();
	if (c_p && c_p_locks)
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
    modp->old_code = modp->code;
    modp->old_code_length = modp->code_length;
    modp->old_catches = modp->catches;
    modp->old_nif = modp->nif;
    modp->code = NULL;
    modp->code_length = 0;
    modp->catches = BEAM_CATCHES_NIL;
    modp->nif = NULL;
}


/* null all references on the export table for the module called with the
   atom index below */

static void
delete_export_references(Eterm module)
{
    int i;

    ASSERT(is_atom(module));

    for (i = 0; i < export_list_size(); i++) {
	Export *ep = export_list(i);
        if (ep != NULL && (ep->code[0] == module)) {
	    if (ep->address == ep->code+3 &&
		(ep->code[3] == (BeamInstr) em_apply_bif)) {
		continue;
	    }
	    ep->address = ep->code+3;
	    ep->code[3] = (BeamInstr) em_call_error_handler;
	    ep->code[4] = 0;
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = NULL;
	}
    }
}


int
beam_make_current_old(Process *c_p, ErtsProcLocks c_p_locks, Eterm module)
{
    Module* modp = erts_put_module(module);

    /*
     * Check if the previous code has been already deleted;
     * if not, delete old code; error if old code already exists.
     */

    if (modp->code != NULL && modp->old_code != NULL)  {
	return -3;
    } else if (modp->old_code == NULL) { /* Make the current version old. */
	if (display_loads) {
	    erts_printf("saving old code\n");
	}
	delete_code(c_p, c_p_locks, modp);
	delete_export_references(module);
    }
    return 0;
}

static int
is_native(BeamInstr* code)
{
    return ((Eterm *)code[MI_FUNCTIONS])[1] != 0;
}


