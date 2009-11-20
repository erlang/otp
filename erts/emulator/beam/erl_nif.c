/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009. All Rights Reserved.
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
/* Erlang Native InterFace
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_nif.h"

#include "sys.h"
#include "global.h"
#include "erl_binary.h"
#include "bif.h"
#include "error.h"
#include "big.h"
#include "beam_bp.h"

#include <limits.h>

/*
static ERTS_INLINE Eterm* alloc_heap(ErlNifEnv* env, unsigned need)
{
    return HAlloc(env->proc, need);
}
*/

#define MIN_HEAP_FRAG_SZ 200
static Eterm* alloc_heap_heavy(ErlNifEnv* env, unsigned need);

static ERTS_INLINE Eterm* alloc_heap(ErlNifEnv* env, unsigned need)
{
    Eterm* hp = env->hp;
    env->hp += need;
    if (env->hp <= env->hp_end) {
	return hp;
    }
    env->hp = hp;
    return alloc_heap_heavy(env,need);
}

static Eterm* alloc_heap_heavy(ErlNifEnv* env, unsigned need)
{
    Eterm* hp;

    if (env->heap_frag_sz == 0) {       
	ASSERT(HEAP_LIMIT(env->proc) == env->hp_end);
	HEAP_TOP(env->proc) = env->hp;	
	env->heap_frag_sz = need + MIN_HEAP_FRAG_SZ;
    }
    else {
	HRelease(env->proc, env->hp_end, env->hp);
	env->heap_frag_sz *= 2;
    }
    hp = erts_heap_alloc(env->proc, env->heap_frag_sz);
    env->hp = hp + need;
    env->hp_end = hp + env->heap_frag_sz;
    return hp;
}

void erts_pre_nif(ErlNifEnv* env, Process* p, void* nif_data)
{
    env->nif_data = nif_data;
    env->proc = p;
    env->hp = HEAP_TOP(p);
    env->hp_end = HEAP_LIMIT(p);
    env->heap_frag_sz = 0;
    env->fpe_was_unmasked = erts_block_fpe();
}

void erts_post_nif(ErlNifEnv* env)
{
    erts_unblock_fpe(env->fpe_was_unmasked);
    if (env->heap_frag_sz == 0) {
	ASSERT(env->hp_end == HEAP_LIMIT(env->proc));
	ASSERT(env->hp >= HEAP_TOP(env->proc));
	ASSERT(env->hp <= HEAP_LIMIT(env->proc));	
	HEAP_TOP(env->proc) = env->hp;
    }
    else {
	ASSERT(env->hp_end != HEAP_LIMIT(env->proc));
	ASSERT(env->hp_end - env->hp <= env->heap_frag_sz);
	HRelease(env->proc, env->hp_end, env->hp);
    }
}

void* enif_get_data(ErlNifEnv* env)
{
    return env->nif_data;
}

void* enif_alloc(ErlNifEnv* env, size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_NIF, (Uint) size);
}

void enif_free(ErlNifEnv* env, void* ptr)
{
    erts_free(ERTS_ALC_T_NIF, ptr);
}


int enif_is_binary(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_binary(term) && (binary_bitsize(term) % 8 == 0);
}
				     

int enif_inspect_binary(ErlNifEnv* env, Eterm bin_term, ErlNifBinary* bin)
{
    bin->tmp_alloc = NULL;
    bin->data = erts_get_aligned_binary_bytes(bin_term, &bin->tmp_alloc);
    if (bin->data == NULL) {
	return 0;
    }
    bin->bin_term = bin_term;
    bin->size = binary_size(bin_term);
    bin->ref_bin = NULL;
    return 1;
}


int enif_alloc_binary(ErlNifEnv* env, unsigned size, ErlNifBinary* bin)
{
    Binary* refbin;

    refbin = erts_bin_drv_alloc_fnf(size); /* BUGBUG: alloc type? */
    if (refbin == NULL) {
	return 0; /* The NIF must take action */
    }
    refbin->flags = BIN_FLAG_DRV; /* BUGBUG: Flag? */
    erts_refc_init(&refbin->refc, 1);
    refbin->orig_size = (long) size;

    bin->size = size;
    bin->data = (unsigned char*) refbin->orig_bytes;
    bin->bin_term = THE_NON_VALUE;
    bin->tmp_alloc = NULL;
    bin->ref_bin = refbin;
    return 1;
}

void enif_release_binary(ErlNifEnv* env, ErlNifBinary* bin)
{
    if (bin->ref_bin == NULL) {
	erts_free_aligned_binary_bytes(bin->tmp_alloc);
    }
    else {
	Binary* refbin = bin->ref_bin;
	ASSERT(bin->tmp_alloc == NULL);
	ASSERT(bin->bin_term == THE_NON_VALUE);
	if (erts_refc_dectest(&refbin->refc, 0) == 0) {
	    erts_bin_free(refbin);
	}
    }
#ifdef DEBUG
    bin->bin_term = THE_NON_VALUE;
    bin->tmp_alloc = NULL;
    bin->ref_bin = NULL;
#endif
}

Eterm enif_make_binary(ErlNifEnv* env, ErlNifBinary* bin)
{
    if (bin->ref_bin == NULL) {
	erts_free_aligned_binary_bytes(bin->tmp_alloc);
	return bin->bin_term;
    }
    else {
	Binary* bptr = bin->ref_bin;
	ProcBin* pb;
	ASSERT(bin->tmp_alloc == NULL);
	
	/* !! Copy-paste from new_binary() !! */
	pb = (ProcBin *) alloc_heap(env, PROC_BIN_SIZE);
	pb->thing_word = HEADER_PROC_BIN;
	pb->size = bptr->orig_size;
	pb->next = MSO(env->proc).mso;
	MSO(env->proc).mso = pb;
	pb->val = bptr;
	pb->bytes = (byte*) bptr->orig_bytes;
	pb->flags = 0;
	
	MSO(env->proc).overhead += pb->size / sizeof(Eterm);
	return make_binary(pb);
    }
}

ERL_NIF_TERM enif_make_badarg(ErlNifEnv* env)
{
    BIF_ERROR(env->proc, BADARG);
}


int enif_get_int(ErlNifEnv* env, Eterm term, int* ip)
{
#if SIZEOF_INT == SIZEOF_VOID_P
    return term_to_Sint(term, ip);
#elif SIZEOF_LONG == SIZEOF_VOID_P
    Sint i;
    if (!term_to_Sint(term, &i) || i < INT_MIN || i > INT_MAX) {
	return 0;
    }
    *ip = (int) i;
    return 1;
#else
#  error Unknown word size 
#endif     
}

int enif_get_ulong(ErlNifEnv* env, Eterm term, unsigned long* ip)
{
#if SIZEOF_LONG == SIZEOF_VOID_P
    return term_to_Uint(term, ip);
#else
#  error Unknown long word size 
#endif     
}

int enif_get_list_cell(ErlNifEnv* env, Eterm term, Eterm* head, Eterm* tail)
{
    Eterm* val;
    if (is_not_list(term)) return 0;
    val = list_val(term);
    *head = CAR(val);
    *tail = CDR(val);
    return 1;
}

ERL_NIF_TERM enif_make_int(ErlNifEnv* env, int i)
{
#if SIZEOF_INT == SIZEOF_VOID_P
    return IS_SSMALL(i) ? make_small(i) : small_to_big(i,alloc_heap(env,2));
#elif SIZEOF_LONG == SIZEOF_VOID_P
    return make_small(i);
#endif
}

ERL_NIF_TERM enif_make_ulong(ErlNifEnv* env, unsigned long i)
{
#if SIZEOF_LONG == SIZEOF_VOID_P
    Eterm* hp;
    Uint sz = 0;
    erts_bld_uint(NULL, &sz, i);
    hp = alloc_heap(env,sz);
    return erts_bld_uint(&hp, NULL, i);
#else
#  error Unknown long word size 
#endif     

}


ERL_NIF_TERM enif_make_atom(ErlNifEnv* env, const char* name)
{
    return am_atom_put(name, sys_strlen(name));
}


ERL_NIF_TERM enif_make_tuple(ErlNifEnv* env, unsigned cnt, ...)
{
    Eterm* hp = alloc_heap(env,cnt+1);
    Eterm ret = make_tuple(hp);
    va_list ap;

    *hp++ = make_arityval(cnt);
    va_start(ap,cnt);
    while (cnt--) {
	*hp++ = va_arg(ap,Eterm);	   
    }
    va_end(ap);
    return ret;
}

ERL_NIF_TERM enif_make_list_cell(ErlNifEnv* env, Eterm car, Eterm cdr)
{
    Eterm* hp = alloc_heap(env,2);
    Eterm ret = make_list(hp);

    CAR(hp) = car;
    CDR(hp) = cdr;
    return ret;
}

ERL_NIF_TERM enif_make_list(ErlNifEnv* env, unsigned cnt, ...)
{
    Eterm* hp = alloc_heap(env,cnt*2);
    Eterm ret = make_list(hp);
    Eterm* last = &ret;
    va_list ap;

    va_start(ap,cnt);
    while (cnt--) {
	*last = make_list(hp);
	*hp = va_arg(ap,Eterm);
	last = ++hp;
	++hp;
    }
    va_end(ap);
    *last = NIL;
    return ret;
}

ERL_NIF_TERM enif_make_string(ErlNifEnv* env, const char* string)
{    
    Sint n = strlen(string);    
    Eterm* hp = alloc_heap(env,n*2);
    return erts_bld_string_n(&hp,NULL,string,n); 
}




/***************************************************************************
 **                              load_nif/2                               **
 ***************************************************************************/


static Uint** get_func_pp(Eterm* mod_code, Eterm f_atom, unsigned arity)
{
    int n = (int) mod_code[MI_NUM_FUNCTIONS];
    int j;
    for (j = 0; j < n; ++j) {
	Uint* code_ptr = (Uint*) mod_code[MI_FUNCTIONS+j];
	ASSERT(code_ptr[0] == (Uint) BeamOp(op_i_func_info_IaaI));
	if (f_atom == ((Eterm) code_ptr[3])
	    && arity == ((unsigned) code_ptr[4])) {

	    return (Uint**) &mod_code[MI_FUNCTIONS+j];
	}
    }
    return NULL;
}

#define in_area(ptr,start,nbytes) \
    ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

static void refresh_cached_nif_data(Eterm* mod_code,
				    struct erl_module_nif* mod_nif)
{
    int i;
    for (i=0; i < mod_nif->entry->num_of_funcs; i++) {
	Eterm f_atom;
	ErlNifFunc* func = &mod_nif->entry->funcs[i];	
	Uint* code_ptr;
	
	erts_atom_get(func->name, strlen(func->name), &f_atom); 
	code_ptr = *get_func_pp(mod_code, f_atom, func->arity);
	code_ptr[5+2] = (Uint) mod_nif->data;
    }
}

static Eterm mkatom(const char *str)
{
    return am_atom_put(str, sys_strlen(str));
}

static struct tainted_module_t
{
    struct tainted_module_t* next;
    Eterm module_atom;
}*first_tainted_module = NULL;

static void add_taint(Eterm mod_atom)
{
    struct tainted_module_t* t;
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	if (t->module_atom == mod_atom) {
	    return;
	}
    }
    t = erts_alloc_fnf(ERTS_ALC_T_TAINT, sizeof(*t));
    if (t != NULL) {
	t->module_atom = mod_atom;
	t->next = first_tainted_module;
	first_tainted_module = t;
    }
}

Eterm erts_nif_taints(Process* p)
{
    struct tainted_module_t* t;
    unsigned cnt = 0;
    Eterm list = NIL;
    Eterm* hp;
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	cnt++;
    }
    hp = HAlloc(p,cnt*2);
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	list = CONS(hp, t->module_atom, list);
	hp += 2;
    }
    return list;
}


static Eterm load_nif_error(Process* p, const char* atom, const char* format, ...)
{
    erts_dsprintf_buf_t* dsbufp = erts_create_tmp_dsbuf(0);
    Eterm ret;
    Eterm* hp;
    Eterm** hpp = NULL;
    Uint sz = 0;
    Uint* szp = &sz;
    va_list arglist;   

    va_start(arglist, format);
    erts_vdsprintf(dsbufp, format, arglist);
    va_end(arglist);
    
    for (;;) {
	Eterm txt = erts_bld_string_n(hpp, &sz, dsbufp->str, dsbufp->str_len);
	ret = erts_bld_tuple(hpp, szp, 3, am_error, mkatom(atom), txt);
	if (hpp != NULL) {
	    break;
	}
	hp = HAlloc(p,sz);
	hpp = &hp;
	szp = NULL;
    }
    erts_destroy_tmp_dsbuf(dsbufp);
    return ret;
}

BIF_RETTYPE load_nif_2(BIF_ALIST_2)
{
    static const char bad_lib[] = "bad_lib";
    static const char reload[] = "reload";
    static const char upgrade[] = "upgrade";
    char lib_name[256]; /* BUGBUG: Max-length? */
    void* handle = NULL;
    void* init_func;
    ErlNifEntry* entry = NULL;
    ErlNifEnv env;
    int len, i, err;
    Module* mod;
    Eterm mod_atom;
    Eterm f_atom;
    Eterm* caller;
    ErtsSysDdllError errdesc = ERTS_SYS_DDLL_ERROR_INIT;
    Eterm ret = am_ok;
    int veto;

    len = intlist_to_buf(BIF_ARG_1, lib_name, sizeof(lib_name)-1);
    if (len < 1) {
	/*erts_fprintf(stderr, "Invalid library path name '%T'\r\n", BIF_ARG_1);*/
	BIF_ERROR(BIF_P, BADARG);
    }
    lib_name[len] = '\0';

    /* Block system (is this the right place to do it?) */
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    /* Find calling module */
    ASSERT(BIF_P->current != NULL);
    ASSERT(BIF_P->current[0] == am_erlang
	   && BIF_P->current[1] == am_load_nif 
	   && BIF_P->current[2] == 2);
    caller = find_function_from_pc(BIF_P->cp);
    ASSERT(caller != NULL);
    mod_atom = caller[0];
    ASSERT(is_atom(mod_atom));
    mod=erts_get_module(mod_atom);
    ASSERT(mod != NULL);

    if (!in_area(caller, mod->code, mod->code_length)) {
	ASSERT(in_area(caller, mod->old_code, mod->old_code_length));

	ret = load_nif_error(BIF_P, "old_code", "Calling load_nif from old "
			     "module '%T' not allowed", mod_atom);
    }    
    else if ((err=erts_sys_ddll_open2(lib_name, &handle, &errdesc)) != ERL_DE_NO_ERROR) {
	ret = load_nif_error(BIF_P, "load_failed", "Failed to load NIF library"
			     " %s: '%s'", lib_name, errdesc.str);
    }
    else if (erts_sys_ddll_load_nif_init(handle, &init_func, &errdesc) != ERL_DE_NO_ERROR) {
	ret  = load_nif_error(BIF_P, bad_lib, "Failed to find library init"
			      " function: '%s'", errdesc.str);
	
    }
    else if ((add_taint(mod_atom),
	      (entry = erts_sys_ddll_call_nif_init(init_func)) == NULL)) {
	ret = load_nif_error(BIF_P, bad_lib, "Library init-call unsuccessful");
    }
    else if (entry->major != ERL_NIF_MAJOR_VERSION
	     || entry->minor > ERL_NIF_MINOR_VERSION) {
	
	ret = load_nif_error(BIF_P, bad_lib, "Library version (%d.%d) not compatible (with %d.%d).",
			     entry->major, entry->minor, ERL_NIF_MAJOR_VERSION, ERL_NIF_MINOR_VERSION);
    }    
    else if (!erts_is_atom_str((char*)entry->name, mod_atom)) {
	ret = load_nif_error(BIF_P, bad_lib, "Library module name '%s' does not"
			     " match calling module '%T'", entry->name, mod_atom);
    }
    else {
	/*erts_fprintf(stderr, "Found module %T\r\n", mod_atom);*/
    
	for (i=0; i < entry->num_of_funcs && ret==am_ok; i++) {
	    Uint** code_pp;
	    ErlNifFunc* f = &entry->funcs[i];
	    if (f->arity > 3) {
		ret = load_nif_error(BIF_P,bad_lib,"Function arity too high for NIF %s/%u",
				     f->name, f->arity);
	    }
	    else if (!erts_atom_get(f->name, strlen(f->name), &f_atom)
		     || (code_pp = get_func_pp(mod->code, f_atom, f->arity))==NULL) { 
		ret = load_nif_error(BIF_P,bad_lib,"Function not found %T:%s/%u",
				     mod_atom, f->name, f->arity);
	    }    
	    else if (code_pp[1] - code_pp[0] < (5+3)) {
		ret = load_nif_error(BIF_P,bad_lib,"No explicit call to load_nif"
				     " in module (%T:%s/%u to small)",
				     mod_atom, entry->funcs[i].name, entry->funcs[i].arity);
	    }
	    /*erts_fprintf(stderr, "Found NIF %T:%s/%u\r\n",
			 mod_atom, entry->funcs[i].name, entry->funcs[i].arity);*/
	}
    }

    if (ret != am_ok) {
	goto error;
    }

    /* Call load, reload or upgrade:
     */
    if (mod->nif.handle != NULL) { /* Reload */
	int k;
	ASSERT(mod->nif.entry != NULL);
	if (entry->reload == NULL) {
	    ret = load_nif_error(BIF_P,reload,"Reload not supported by this NIF library.");
	    goto error;
	}
	/* Check that no NIF is removed */
	for (k=0; k < mod->nif.entry->num_of_funcs; k++) {
	    ErlNifFunc* old_func = &mod->nif.entry->funcs[k];
	    for (i=0; i < entry->num_of_funcs; i++) {
		if (old_func->arity == entry->funcs[i].arity
		    && sys_strcmp(old_func->name, entry->funcs[i].name) == 0) {			   
		    break;
		}
	    }
	    if (i == entry->num_of_funcs) {
		ret = load_nif_error(BIF_P,reload,"Reloaded library missing "
				     "function %T:%s/%u\r\n", mod_atom,
				     old_func->name, old_func->arity);
		goto error;
	    }
	}       
	erts_pre_nif(&env, BIF_P, mod->nif.data);
	veto = entry->reload(&env, &env.nif_data, BIF_ARG_2);
	erts_post_nif(&env);
	if (veto) {
	    ret = load_nif_error(BIF_P, reload, "Library reload-call unsuccessful.");
	}
	else {
	    erts_sys_ddll_close(mod->nif.handle);
	}
    }
    else {
	if (mod->old_nif.handle != NULL) { /* Upgrade */
	    void* prev_old_data = mod->old_nif.data;
	    if (entry->upgrade == NULL) {
		ret = load_nif_error(BIF_P, upgrade, "Upgrade not supported by this NIF library.");
		goto error;
	    }
	    erts_pre_nif(&env, BIF_P, NULL);
	    veto = entry->upgrade(&env, &env.nif_data, &mod->old_nif.data, BIF_ARG_2);
	    erts_post_nif(&env);
	    if (veto) {
		mod->old_nif.data = prev_old_data;
		ret = load_nif_error(BIF_P, upgrade, "Library upgrade-call unsuccessful.");
	    }
	    else if (mod->old_nif.data != prev_old_data) {
		refresh_cached_nif_data(mod->old_code, &mod->old_nif);
	    }
	}
	else if (entry->load != NULL) { /* Initial load */
	    erts_pre_nif(&env, BIF_P, NULL);
	    veto = entry->load(&env, &env.nif_data, BIF_ARG_2);
	    erts_post_nif(&env);
	    if (veto) {
		ret = load_nif_error(BIF_P, "load", "Library load-call unsuccessful.");
	    }
	}
    }
    if (ret == am_ok) {
	/*
	** Everything ok, patch the beam code with op_call_nif
	*/
	mod->nif.data = env.nif_data;
	mod->nif.handle = handle;
	mod->nif.entry = entry;
	for (i=0; i < entry->num_of_funcs; i++)
	{
	    Uint* code_ptr;
	    erts_atom_get(entry->funcs[i].name, strlen(entry->funcs[i].name), &f_atom); 
	    code_ptr = *get_func_pp(mod->code, f_atom, entry->funcs[i].arity); 
	    
	    if (code_ptr[1] == 0) {
		code_ptr[5+0] = (Uint) BeamOp(op_call_nif);		
	    } else { /* Function traced, patch the original instruction word */
		BpData* bp = (BpData*) code_ptr[1];
	        bp->orig_instr = (Uint) BeamOp(op_call_nif); 
	    }	    
	    code_ptr[5+1] = (Uint) entry->funcs[i].fptr;
	    code_ptr[5+2] = (Uint) mod->nif.data;
	}
    }
    else {    
    error:
	ASSERT(ret != am_ok);
	if (handle != NULL) {
	    erts_sys_ddll_close(handle);
	}
	erts_sys_ddll_free_error(&errdesc);
    }

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    BIF_RET(ret);
}

