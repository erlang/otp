/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2010. All Rights Reserved.
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
#include <stddef.h> /* offsetof */


/* Information about a loaded nif library.
 * Each successful call to erlang:load_nif will allocate an instance of
 * erl_module_nif. Two calls opening the same library will thus have the same
 * 'handle'.
 */
struct erl_module_nif {
    void* priv_data;
    void* handle;             /* "dlopen" */
    struct enif_entry_t* entry;
    erts_refc_t rt_cnt;       /* number of resource types */
    erts_refc_t rt_dtor_cnt;  /* number of resource types with destructors */
    int is_orphan;            /* if erlang module has been purged */
};

#ifdef DEBUG
#  define READONLY_CHECK
#endif
#ifdef READONLY_CHECK
#  define ADD_READONLY_CHECK(ENV,PTR,SIZE) add_readonly_check(ENV,PTR,SIZE)
static void add_readonly_check(ErlNifEnv*, unsigned char* ptr, unsigned sz);
#else
#  define ADD_READONLY_CHECK(ENV,PTR,SIZE) ((void)0)
#endif


#define MIN_HEAP_FRAG_SZ 200
static Eterm* alloc_heap_heavy(ErlNifEnv* env, unsigned need, Eterm* hp);

static ERTS_INLINE Eterm* alloc_heap(ErlNifEnv* env, unsigned need)
{
    Eterm* hp = env->hp;
    env->hp += need;
    if (env->hp <= env->hp_end) {
	return hp;
    }
    return alloc_heap_heavy(env, need, hp);
}

static Eterm* alloc_heap_heavy(ErlNifEnv* env, unsigned need, Eterm* hp)
{    
    unsigned frag_sz;
    env->hp = hp;
    if (env->heap_frag == NULL) {       
	ASSERT(HEAP_LIMIT(env->proc) == env->hp_end);
	HEAP_TOP(env->proc) = env->hp;	
    }
    else {
	HRelease(env->proc, env->hp_end, env->hp);
    }
    frag_sz = need + MIN_HEAP_FRAG_SZ;
    hp = erts_heap_alloc(env->proc, frag_sz);
    env->hp = hp + need;
    env->hp_end = hp + frag_sz;
    env->heap_frag = MBUF(env->proc);
    return hp;
}

void erts_pre_nif(ErlNifEnv* env, Process* p, struct erl_module_nif* mod_nif)
{
    env->mod_nif = mod_nif;
    env->proc = p;
    env->hp = HEAP_TOP(p);
    env->hp_end = HEAP_LIMIT(p);
    env->heap_frag = NULL;
    env->fpe_was_unmasked = erts_block_fpe();
    env->tmp_obj_list = NULL;
}

static void pre_nif_noproc(ErlNifEnv* env, struct erl_module_nif* mod_nif)
{
    env->mod_nif = mod_nif;
    env->proc = NULL;
    env->hp = NULL;
    env->hp_end = NULL;
    env->heap_frag = NULL;
    env->fpe_was_unmasked = erts_block_fpe();
    env->tmp_obj_list = NULL;
}

/* Temporary object header, auto-deallocated when NIF returns. */
struct enif_tmp_obj_t {
    struct enif_tmp_obj_t* next;
    void (*dtor)(struct enif_tmp_obj_t*);
    /*char data[];*/
};

static ERTS_INLINE void free_tmp_objs(ErlNifEnv* env)
{
    while (env->tmp_obj_list != NULL) {
	struct enif_tmp_obj_t* free_me = env->tmp_obj_list;
	env->tmp_obj_list = free_me->next;
	free_me->dtor(free_me);
    }
}

void erts_post_nif(ErlNifEnv* env)
{
    erts_unblock_fpe(env->fpe_was_unmasked);
    if (env->heap_frag == NULL) {
	ASSERT(env->hp_end == HEAP_LIMIT(env->proc));
	ASSERT(env->hp >= HEAP_TOP(env->proc));
	ASSERT(env->hp <= HEAP_LIMIT(env->proc));	
	HEAP_TOP(env->proc) = env->hp;
    }
    else {
	ASSERT(env->hp_end != HEAP_LIMIT(env->proc));
	ASSERT(env->hp_end - env->hp <= env->heap_frag->size);
	HRelease(env->proc, env->hp_end, env->hp);
    }
    free_tmp_objs(env);
}

static void post_nif_noproc(ErlNifEnv* env)
{
    erts_unblock_fpe(env->fpe_was_unmasked);
    free_tmp_objs(env);
}


/* Flush out our cached heap pointers to allow an ordinary HAlloc
*/
static void enable_halloc(ErlNifEnv* env)
{
    if (env->heap_frag == NULL) {
	ASSERT(env->hp_end == HEAP_LIMIT(env->proc));
	ASSERT(env->hp >= HEAP_TOP(env->proc));
	ASSERT(env->hp <= HEAP_LIMIT(env->proc));	
	HEAP_TOP(env->proc) = env->hp;
    }
    else {
	ASSERT(env->hp_end != HEAP_LIMIT(env->proc));
	ASSERT(env->hp_end - env->hp <= env->heap_frag->size);
	HRelease(env->proc, env->hp_end, env->hp);
    }
}

/* Restore cached heap pointers
*/
static void disable_halloc(ErlNifEnv* env)
{
    if (env->heap_frag == NULL) {
	ASSERT(env->hp_end == HEAP_LIMIT(env->proc));
	ASSERT(env->hp <= HEAP_TOP(env->proc));
	ASSERT(env->hp <= HEAP_LIMIT(env->proc));	
	env->hp = HEAP_TOP(env->proc);
    }
    else {
	ASSERT(env->hp_end != HEAP_LIMIT(env->proc));
	ASSERT(env->hp_end - env->hp <= env->heap_frag->size);       
	env->heap_frag = MBUF(env->proc);
	ASSERT(env->heap_frag != NULL);
	env->hp = env->heap_frag->mem + env->heap_frag->used_size;
	env->hp_end = env->heap_frag->mem + env->heap_frag->size;
    }
}

void* enif_priv_data(ErlNifEnv* env)
{
    return env->mod_nif->priv_data;
}

void* enif_alloc(ErlNifEnv* env, size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_NIF, (Uint) size);
}

void* enif_realloc(ErlNifEnv* env, void* ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_NIF, ptr, size);
}

void enif_free(ErlNifEnv* env, void* ptr)
{
    erts_free(ERTS_ALC_T_NIF, ptr);
}

int enif_is_atom(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_atom(term);
}

int enif_is_binary(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_binary(term) && (binary_bitsize(term) % 8 == 0);
}

int enif_is_empty_list(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_nil(term);
}

int enif_is_fun(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_fun(term);
}

int enif_is_pid(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_pid(term);
}

int enif_is_port(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_port(term);
}

int enif_is_ref(ErlNifEnv* env, ERL_NIF_TERM term)
{
    return is_ref(term);
}

static void aligned_binary_dtor(struct enif_tmp_obj_t* obj)
{
    erts_free_aligned_binary_bytes_extra((byte*)obj,ERTS_ALC_T_TMP);
}

int enif_inspect_binary(ErlNifEnv* env, Eterm bin_term, ErlNifBinary* bin)
{
    union {
	struct enif_tmp_obj_t* tmp;
	byte* raw_ptr;
    }u;
    u.tmp = NULL;
    bin->data = erts_get_aligned_binary_bytes_extra(bin_term, &u.raw_ptr, ERTS_ALC_T_TMP,
						    sizeof(struct enif_tmp_obj_t));
    if (bin->data == NULL) {
	return 0;
    }
    if (u.tmp != NULL) {
	u.tmp->next = env->tmp_obj_list;
	u.tmp->dtor = &aligned_binary_dtor;
	env->tmp_obj_list = u.tmp;
    }
    bin->bin_term = bin_term;
    bin->size = binary_size(bin_term);
    bin->ref_bin = NULL;
    ADD_READONLY_CHECK(env, bin->data, bin->size); 
    return 1;
}

static void tmp_alloc_dtor(struct enif_tmp_obj_t* obj)
{
    erts_free(ERTS_ALC_T_TMP,  obj);
}

int enif_inspect_iolist_as_binary(ErlNifEnv* env, Eterm term, ErlNifBinary* bin)
{
    struct enif_tmp_obj_t* tobj;
    int sz;
    if (is_binary(term)) {
	return enif_inspect_binary(env,term,bin);
    }
    if (is_nil(term)) {
	bin->data = (unsigned char*) &bin->data; /* dummy non-NULL */
	bin->size = 0;
	bin->bin_term = THE_NON_VALUE;
	bin->ref_bin = NULL;
	return 1;
    }
    if ((sz = io_list_len(term)) < 0) {
	return 0;
    }
    
    tobj = erts_alloc(ERTS_ALC_T_TMP, sz + sizeof(struct enif_tmp_obj_t));
    tobj->next = env->tmp_obj_list;
    tobj->dtor = &tmp_alloc_dtor;
    env->tmp_obj_list = tobj;

    bin->data = (unsigned char*) &tobj[1]; 
    bin->size = sz;
    bin->bin_term = THE_NON_VALUE;
    bin->ref_bin = NULL;
    io_list_to_buf(term, (char*) bin->data, sz);
    ADD_READONLY_CHECK(env, bin->data, bin->size); 
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
    bin->ref_bin = refbin;
    return 1;
}

int enif_realloc_binary(ErlNifEnv* env, ErlNifBinary* bin, unsigned size)
{
    if (bin->ref_bin != NULL) {
	Binary* oldbin;
	Binary* newbin;
    
	oldbin = (Binary*) bin->ref_bin; 
	newbin = (Binary *) erts_bin_realloc_fnf(oldbin, size);
	if (!newbin) {
	    return 0;
	}    
	newbin->orig_size = size;
	bin->ref_bin = newbin;
	bin->data = (unsigned char*) newbin->orig_bytes;
	bin->size = size;
    }
    else {
	unsigned char* old_data = bin->data;
	unsigned cpy_sz = (size < bin->size ? size : bin->size);  
	enif_alloc_binary(env, size, bin);
	sys_memcpy(bin->data, old_data, cpy_sz); 
    }
    return 1;
}


void enif_release_binary(ErlNifEnv* env, ErlNifBinary* bin)
{
    if (bin->ref_bin != NULL) {
	Binary* refbin = bin->ref_bin;
	ASSERT(bin->bin_term == THE_NON_VALUE);
	if (erts_refc_dectest(&refbin->refc, 0) == 0) {
	    erts_bin_free(refbin);
	}
    }
#ifdef DEBUG
    bin->data = NULL;
    bin->bin_term = THE_NON_VALUE;
    bin->ref_bin = NULL;
#endif
}

unsigned char* enif_make_new_binary(ErlNifEnv* env, unsigned size,
				    ERL_NIF_TERM* termp)
{
    enable_halloc(env);
    *termp = new_binary(env->proc, NULL, size);
    disable_halloc(env);
    return binary_bytes(*termp);
}

int enif_is_identical(ErlNifEnv* env, Eterm lhs, Eterm rhs)
{
    return EQ(lhs,rhs);
}

int enif_compare(ErlNifEnv* env, Eterm lhs, Eterm rhs)
{
    return cmp(lhs,rhs);
}

int enif_get_tuple(ErlNifEnv* env, Eterm tpl, int* arity, const Eterm** array)
{
    Eterm* ptr;
    if (is_not_tuple(tpl)) {
	return 0;
    }
    ptr = tuple_val(tpl);
    *arity = arityval(*ptr);
    *array = ptr+1;
    return 1;
}

int enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf, unsigned len,
		    ErlNifCharEncoding encoding)
{
    Eterm* listptr;
    int n = 0;

    ASSERT(encoding == ERL_NIF_LATIN1);
    if (len < 1) {
	return 0;
    }
    while (is_not_nil(list)) { 	    
	if (is_not_list(list)) {
	    buf[n] = '\0';
	    return 0;
	}
	listptr = list_val(list);
    
	if (!is_byte(*listptr)) {
	    buf[n] = '\0';
	    return 0;
	}
	buf[n++] = unsigned_val(*listptr);
	if (n >= len) {
	    buf[n-1] = '\0'; /* truncate */
	    return -len;
	}
	list = CDR(listptr);
    }
    buf[n] = '\0';
    return n + 1;
}

Eterm enif_make_binary(ErlNifEnv* env, ErlNifBinary* bin)
{
    if (bin->bin_term != THE_NON_VALUE) {
	return bin->bin_term;
    }
    else if (bin->ref_bin != NULL) {
	Binary* bptr = bin->ref_bin;
	ProcBin* pb;
	Eterm bin_term;
	
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
	bin_term = make_binary(pb);	
	if (erts_refc_read(&bptr->refc, 1) == 1) {
	    /* Total ownership transfer */
	    bin->ref_bin = NULL;
	    bin->bin_term = bin_term;
	}
	return bin_term;
    }
    else {
	enable_halloc(env);
	bin->bin_term = new_binary(env->proc, bin->data, bin->size);
	disable_halloc(env);
	return bin->bin_term;
    }
}

Eterm enif_make_sub_binary(ErlNifEnv* env, ERL_NIF_TERM bin_term,
			   unsigned pos, unsigned size)
{
    ErlSubBin* sb;
    Eterm orig;
    Uint offset, bit_offset, bit_size; 
    unsigned src_size;

    ASSERT(is_binary(bin_term));
    src_size = binary_size(bin_term);
    ASSERT(pos <= src_size);
    ASSERT(size <= src_size);
    ASSERT(pos + size <= src_size);   
    sb = (ErlSubBin*) alloc_heap(env, ERL_SUB_BIN_SIZE);
    ERTS_GET_REAL_BIN(bin_term, orig, offset, bit_offset, bit_size);
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = size;
    sb->offs = offset + pos;
    sb->orig = orig;
    sb->bitoffs = bit_offset;
    sb->bitsize = 0;
    sb->is_writable = 0;
    return make_binary(sb);
}


Eterm enif_make_badarg(ErlNifEnv* env)
{
    BIF_ERROR(env->proc, BADARG);
}

int enif_get_atom(ErlNifEnv* env, Eterm atom, char* buf, unsigned len)
{
    Atom* ap;
    if (is_not_atom(atom)) {
	return 0;
    }
    ap = atom_tab(atom_val(atom));
    if (ap->len+1 > len) {
	return 0;
    }
    sys_memcpy(buf, ap->name, ap->len);
    buf[ap->len] = '\0';
    return ap->len + 1;
}

int enif_get_int(ErlNifEnv* env, Eterm term, int* ip)
{
#if SIZEOF_INT ==  ERTS_SIZEOF_ETERM
    return term_to_Sint(term, (Sint*)ip);
#elif SIZEOF_LONG ==  ERTS_SIZEOF_ETERM
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

int enif_get_uint(ErlNifEnv* env, Eterm term, unsigned* ip)
{
#if SIZEOF_INT == ERTS_SIZEOF_ETERM
    return term_to_Uint(term, (Uint*)ip);
#elif SIZEOF_LONG == ERTS_SIZEOF_ETERM
    Uint i;
    if (!term_to_Uint(term, &i) || i > UINT_MAX) {
	return 0;
    }
    *ip = (unsigned) i;
    return 1;
#endif     
}

int enif_get_long(ErlNifEnv* env, Eterm term, long* ip)
{
#if SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return term_to_Sint(term, ip);
#elif SIZEOF_INT == ERTS_SIZEOF_ETERM
    Uint u;
    term_to_Sint(term, u);
    *ip = (long) u;
    return 1;
#else
#  error Unknown long word size 
#endif     
}

int enif_get_ulong(ErlNifEnv* env, Eterm term, unsigned long* ip)
{
#if SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return term_to_Uint(term, ip);
#elif SIZEOF_INT == ERTS_SIZEOF_ETERM
    Uint u;
    int r;
    r = term_to_Uint(term, &u);
    *ip = (unsigned long) u;
    return r;
#else
#  error Unknown long word size 
#endif     
}

int enif_get_double(ErlNifEnv* env, Eterm term, double* dp)
{
    FloatDef f;
    if (is_not_float(term)) {
	return 0;
    }
    GET_DOUBLE(term, f);
    *dp = f.fd;
    return 1;
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
#if SIZEOF_INT == ERTS_SIZEOF_ETERM
    return IS_SSMALL(i) ? make_small(i) : small_to_big(i,alloc_heap(env,2));
#elif SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return make_small(i);
#endif
}

ERL_NIF_TERM enif_make_uint(ErlNifEnv* env, unsigned i)
{
#if SIZEOF_INT == ERTS_SIZEOF_ETERM
    return IS_USMALL(0,i) ? make_small(i) : uint_to_big(i,alloc_heap(env,2));
#elif SIZEOF_LONG == ERTS_SIZEOF_ETERM
    return make_small(i);
#endif
}

ERL_NIF_TERM enif_make_long(ErlNifEnv* env, long i)
{
    return IS_SSMALL(i) ? make_small(i) : small_to_big(i, alloc_heap(env,2));
}

ERL_NIF_TERM enif_make_ulong(ErlNifEnv* env, unsigned long i)
{
    return IS_USMALL(0,i) ? make_small(i) : uint_to_big(i,alloc_heap(env,2));
}

ERL_NIF_TERM enif_make_double(ErlNifEnv* env, double d)
{
    Eterm* hp = alloc_heap(env,FLOAT_SIZE_OBJECT);
    FloatDef f;
    f.fd = d;
    PUT_DOUBLE(f, hp);
    return make_float(hp);
}

ERL_NIF_TERM enif_make_atom(ErlNifEnv* env, const char* name)
{
    return am_atom_put(name, sys_strlen(name));
}

int enif_make_existing_atom(ErlNifEnv* env, const char* name, ERL_NIF_TERM* atom)
{
    return erts_atom_get(name, sys_strlen(name), atom);
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

ERL_NIF_TERM enif_make_tuple_from_array(ErlNifEnv* env, const ERL_NIF_TERM arr[], unsigned cnt)
{
    Eterm* hp = alloc_heap(env,cnt+1);
    Eterm ret = make_tuple(hp);
    const Eterm* src = arr;

    *hp++ = make_arityval(cnt);
    while (cnt--) {
	*hp++ = *src++;	   
    }
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

ERL_NIF_TERM enif_make_list_from_array(ErlNifEnv* env, const ERL_NIF_TERM arr[], unsigned cnt)
{
    Eterm* hp = alloc_heap(env,cnt*2);
    Eterm ret = make_list(hp);
    Eterm* last = &ret;
    const Eterm* src = arr;

    while (cnt--) {
	*last = make_list(hp);
	*hp = *src++;
	last = ++hp;
	++hp;
    }
    *last = NIL;
    return ret;
}

ERL_NIF_TERM enif_make_string(ErlNifEnv* env, const char* string,
			      ErlNifCharEncoding encoding)
{    
    Sint n = sys_strlen(string);    
    Eterm* hp = alloc_heap(env,n*2);
    ASSERT(encoding == ERL_NIF_LATIN1);
    return erts_bld_string_n(&hp,NULL,string,n); 
}

ERL_NIF_TERM enif_make_ref(ErlNifEnv* env)
{
    Eterm* hp = alloc_heap(env, REF_THING_SIZE);
    return erts_make_ref_in_buffer(hp);
}

void enif_system_info(ErlNifSysInfo *sip, size_t si_size)
{
    driver_system_info(sip, si_size);
}


ErlNifMutex* enif_mutex_create(char *name) { return erl_drv_mutex_create(name); }
void enif_mutex_destroy(ErlNifMutex *mtx) {  erl_drv_mutex_destroy(mtx); }
int enif_mutex_trylock(ErlNifMutex *mtx) { return erl_drv_mutex_trylock(mtx); }
void enif_mutex_lock(ErlNifMutex *mtx) { erl_drv_mutex_lock(mtx); }
void enif_mutex_unlock(ErlNifMutex *mtx) { erl_drv_mutex_unlock(mtx); }
ErlNifCond* enif_cond_create(char *name) { return erl_drv_cond_create(name); }
void enif_cond_destroy(ErlNifCond *cnd) { erl_drv_cond_destroy(cnd); }
void enif_cond_signal(ErlNifCond *cnd) { erl_drv_cond_signal(cnd); }
void enif_cond_broadcast(ErlNifCond *cnd) { erl_drv_cond_broadcast(cnd); }
void enif_cond_wait(ErlNifCond *cnd, ErlNifMutex *mtx) { erl_drv_cond_wait(cnd,mtx); }
ErlNifRWLock* enif_rwlock_create(char *name) { return erl_drv_rwlock_create(name); }
void enif_rwlock_destroy(ErlNifRWLock *rwlck) { erl_drv_rwlock_destroy(rwlck); }
int enif_rwlock_tryrlock(ErlNifRWLock *rwlck) { return erl_drv_rwlock_tryrlock(rwlck); }
void enif_rwlock_rlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_rlock(rwlck); }
void enif_rwlock_runlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_runlock(rwlck); }
int enif_rwlock_tryrwlock(ErlNifRWLock *rwlck) { return erl_drv_rwlock_tryrwlock(rwlck); }
void enif_rwlock_rwlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_rwlock(rwlck); }
void enif_rwlock_rwunlock(ErlNifRWLock *rwlck) { erl_drv_rwlock_rwunlock(rwlck); }
int enif_tsd_key_create(char *name, ErlNifTSDKey *key) { return erl_drv_tsd_key_create(name,key); }
void enif_tsd_key_destroy(ErlNifTSDKey key) { erl_drv_tsd_key_destroy(key); }
void enif_tsd_set(ErlNifTSDKey key, void *data) { erl_drv_tsd_set(key,data); }
void* enif_tsd_get(ErlNifTSDKey key) { return erl_drv_tsd_get(key); }
ErlNifThreadOpts* enif_thread_opts_create(char *name) { return (ErlNifThreadOpts*) erl_drv_thread_opts_create(name); }
void enif_thread_opts_destroy(ErlNifThreadOpts *opts) { erl_drv_thread_opts_destroy((ErlDrvThreadOpts*)opts); }
int enif_thread_create(char *name, ErlNifTid *tid, void* (*func)(void *),
		       void *args, ErlNifThreadOpts *opts) {
    return erl_drv_thread_create(name,tid,func,args,(ErlDrvThreadOpts*)opts);
}
ErlNifTid enif_thread_self(void) { return erl_drv_thread_self(); }
int enif_equal_tids(ErlNifTid tid1, ErlNifTid tid2) { return erl_drv_equal_tids(tid1,tid2); }
void enif_thread_exit(void *resp) { erl_drv_thread_exit(resp); }
int enif_thread_join(ErlNifTid tid, void **respp) { return erl_drv_thread_join(tid,respp); }

int enif_fprintf(void* filep, const char* format, ...) 
{ 
    int ret;
    va_list arglist;
    va_start(arglist, format);
    ret = erts_vfprintf((FILE*)filep, format, arglist);
    va_end(arglist);
    return ret;
}    

/***********************************************************
 **       Memory managed (GC'ed) "resource" objects       **
 ***********************************************************/


struct enif_resource_type_t
{
    struct enif_resource_type_t* next;   /* list of all resource types */
    struct enif_resource_type_t* prev;    
    struct erl_module_nif* owner;  /* that created this type and thus implements the destructor*/
    ErlNifResourceDtor* dtor;      /* user destructor function */
    erts_refc_t refc;  /* num of resources of this type (HOTSPOT warning)
                          +1 for active erl_module_nif */
    char name[1];
};

/* dummy node in circular list */
struct enif_resource_type_t resource_type_list; 

typedef struct enif_resource_t
{
    struct enif_resource_type_t* type;
#ifdef DEBUG
    erts_refc_t nif_refc;
#endif
    char data[1];
}ErlNifResource;

#define SIZEOF_ErlNifResource(SIZE) (offsetof(ErlNifResource,data) + (SIZE))
#define DATA_TO_RESOURCE(PTR) ((ErlNifResource*)((char*)(PTR) - offsetof(ErlNifResource,data)))

static ErlNifResourceType* find_resource_type(const char* name)
{
    ErlNifResourceType* type;
    for (type = resource_type_list.next;
	 type != &resource_type_list;
	 type = type->next) {

	if (sys_strcmp(type->name, name) == 0) {
	    return type;
	}
    }
    return NULL;
}

#define in_area(ptr,start,nbytes) \
    ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))


static void close_lib(struct erl_module_nif* lib)
{
    ASSERT(lib != NULL);
    ASSERT(lib->handle != NULL);
    ASSERT(erts_refc_read(&lib->rt_dtor_cnt,0) == 0);

    if (lib->entry != NULL && lib->entry->unload != NULL) {
	ErlNifEnv env;
	pre_nif_noproc(&env, lib);
	lib->entry->unload(&env, lib->priv_data);
	post_nif_noproc(&env);
    }
    erts_sys_ddll_close(lib->handle);
    lib->handle = NULL;
}

static void steal_resource_type(ErlNifResourceType* type)
{
    struct erl_module_nif* lib = type->owner;

    if (type->dtor != NULL
	&& erts_refc_dectest(&lib->rt_dtor_cnt, 0) == 0
	&& lib->is_orphan) {
	/* last type with destructor gone, close orphan lib */

	close_lib(lib);
    }
    if (erts_refc_dectest(&lib->rt_cnt, 0) == 0
	&& lib->is_orphan) {
	erts_free(ERTS_ALC_T_NIF, lib);
    }
}

ErlNifResourceType*
enif_open_resource_type(ErlNifEnv* env, const char* type_name, 
			  ErlNifResourceDtor* dtor,
			  enum ErlNifResourceFlags flags,
			  enum ErlNifResourceFlags* tried)
{
    ErlNifResourceType* type = find_resource_type(type_name);
    enum ErlNifResourceFlags op = flags;
    ASSERT(erts_smp_is_system_blocked(0));
    if (type == NULL) {
	if (flags & ERL_NIF_RT_CREATE) {
	    type = erts_alloc(ERTS_ALC_T_NIF,
			      sizeof(struct enif_resource_type_t) 
			      + sys_strlen(type_name));
	    type->dtor = dtor;
	    sys_strcpy(type->name, type_name);
	    erts_refc_init(&type->refc, 1);
	    type->owner = env->mod_nif;
	    type->prev = &resource_type_list;
	    type->next = resource_type_list.next;
	    type->next->prev = type;
	    type->prev->next = type;
	    op = ERL_NIF_RT_CREATE;
	}
    }
    else {
	if (flags & ERL_NIF_RT_TAKEOVER) {	
	    steal_resource_type(type);
	    op = ERL_NIF_RT_TAKEOVER;
	}
	else {
	    type = NULL;
	}
    }
    if (type != NULL) {
	type->owner = env->mod_nif;
	type->dtor = dtor;
	if (type->dtor != NULL) {
	    erts_refc_inc(&type->owner->rt_dtor_cnt, 1);
	}
	erts_refc_inc(&type->owner->rt_cnt, 1);    
    }
    if (tried != NULL) {
	*tried = op;
    }
    return type;
}

static void nif_resource_dtor(Binary* bin)
{
    ErlNifResource* resource = (ErlNifResource*) ERTS_MAGIC_BIN_DATA(bin);
    ErlNifResourceType* type = resource->type;
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == &nif_resource_dtor);

    if (type->dtor != NULL) {
	ErlNifEnv env;
	pre_nif_noproc(&env, type->owner);
	type->dtor(&env,resource->data);
	post_nif_noproc(&env);
    }
    if (erts_refc_dectest(&type->refc, 0) == 0) {
	ASSERT(type->next == NULL);
	ASSERT(type->owner != NULL);
	ASSERT(type->owner->is_orphan);
	steal_resource_type(type);
	erts_free(ERTS_ALC_T_NIF, type);
    }
}

void* enif_alloc_resource(ErlNifEnv* env, ErlNifResourceType* type, unsigned size)
{
    Binary* bin = erts_create_magic_binary(SIZEOF_ErlNifResource(size), &nif_resource_dtor);
    ErlNifResource* resource = ERTS_MAGIC_BIN_DATA(bin);
    resource->type = type;
    erts_refc_inc(&bin->refc, 1);
#ifdef DEBUG
    erts_refc_init(&resource->nif_refc, 1);
#endif
    erts_refc_inc(&resource->type->refc, 2);
    return resource->data;
}

void enif_release_resource(ErlNifEnv* env, void* obj)
{
    ErlNifResource* resource = DATA_TO_RESOURCE(obj);
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_DATA(resource);

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == &nif_resource_dtor);
#ifdef DEBUG
    erts_refc_dec(&resource->nif_refc, 0);
#endif
    if (erts_refc_dectest(&bin->binary.refc, 0) == 0) {
	erts_bin_free(&bin->binary);
    }
}

ERL_NIF_TERM enif_make_resource(ErlNifEnv* env, void* obj)
{
    ErlNifResource* resource = DATA_TO_RESOURCE(obj);
    ErtsBinary* bin = ERTS_MAGIC_BIN_FROM_DATA(resource);
    Eterm* hp = alloc_heap(env,PROC_BIN_SIZE);
    return erts_mk_magic_binary_term(&hp, &MSO(env->proc), &bin->binary);
}

int enif_get_resource(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifResourceType* type,
		      void** objp)
{
    Binary* mbin;
    ErlNifResource* resource;
    if (!ERTS_TERM_IS_MAGIC_BINARY(term)) {
	return 0;
    }
    mbin = ((ProcBin*) binary_val(term))->val;
    resource = (ErlNifResource*) ERTS_MAGIC_BIN_DATA(mbin);
    if (ERTS_MAGIC_BIN_DESTRUCTOR(mbin) != &nif_resource_dtor
	|| resource->type != type) {	
	return 0;
    }
    *objp = resource->data;
    return 1;
}

unsigned enif_sizeof_resource(ErlNifEnv* env, void* obj)
{
    ErlNifResource* resource = DATA_TO_RESOURCE(obj);
    Binary* bin = &ERTS_MAGIC_BIN_FROM_DATA(resource)->binary;
    return ERTS_MAGIC_BIN_DATA_SIZE(bin) - offsetof(ErlNifResource,data);
}

/***************************************************************************
 **                              load_nif/2                               **
 ***************************************************************************/


static BeamInstr** get_func_pp(BeamInstr* mod_code, Eterm f_atom, unsigned arity)
{
    int n = (int) mod_code[MI_NUM_FUNCTIONS];
    int j;
    for (j = 0; j < n; ++j) {
	BeamInstr* code_ptr = (BeamInstr*) mod_code[MI_FUNCTIONS+j];
	ASSERT(code_ptr[0] == (BeamInstr) BeamOp(op_i_func_info_IaaI));
	if (f_atom == ((Eterm) code_ptr[3])
	    && arity == ((unsigned) code_ptr[4])) {

	    return (BeamInstr**) &mod_code[MI_FUNCTIONS+j];
	}
    }
    return NULL;
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

void erts_print_nif_taints(int to, void* to_arg)
{
    struct tainted_module_t* t;
    const char* delim = "";
    for (t=first_tainted_module ; t!=NULL; t=t->next) {
	const Atom* atom = atom_tab(atom_val(t->module_atom));
	erts_print(to,to_arg,"%s%.*s", delim, atom->len, atom->name);
	delim = ",";
    }
    erts_print(to,to_arg,"\n");
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
	ret = erts_bld_tuple(hpp, szp, 2, am_error,
			     erts_bld_tuple(hpp, szp, 2, mkatom(atom), txt));
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
    char* lib_name = NULL;
    void* handle = NULL;
    void* init_func;
    ErlNifEntry* entry = NULL;
    ErlNifEnv env;
    int len, i, err;
    Module* mod;
    Eterm mod_atom;
    Eterm f_atom;
    BeamInstr* caller;
    ErtsSysDdllError errdesc = ERTS_SYS_DDLL_ERROR_INIT;
    Eterm ret = am_ok;
    int veto;
    struct erl_module_nif* lib = NULL;

    len = list_length(BIF_ARG_1);
    if (len < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    lib_name = (char *) erts_alloc(ERTS_ALC_T_TMP, len + 1);

    if (intlist_to_buf(BIF_ARG_1, lib_name, len) != len) {
	erts_free(ERTS_ALC_T_TMP, lib_name);
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
	const char slogan[] = "Failed to load NIF library";
	if (strstr(errdesc.str, lib_name) != NULL) {
	    ret = load_nif_error(BIF_P, "load_failed", "%s: '%s'", slogan, errdesc.str);
	}
	else {
	    ret = load_nif_error(BIF_P, "load_failed", "%s %s: '%s'", slogan, lib_name, errdesc.str);
	}
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
	    BeamInstr** code_pp;
	    ErlNifFunc* f = &entry->funcs[i];
	    if (!erts_atom_get(f->name, sys_strlen(f->name), &f_atom)
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


    lib = erts_alloc(ERTS_ALC_T_NIF, sizeof(struct erl_module_nif));
    lib->handle = handle;
    lib->entry = entry;
    erts_refc_init(&lib->rt_cnt, 0);
    erts_refc_init(&lib->rt_dtor_cnt, 0);
    lib->is_orphan = 0;
    env.mod_nif = lib;
    if (mod->nif != NULL) { /* Reload */
	int k;
        lib->priv_data = mod->nif->priv_data;

	ASSERT(mod->nif->entry != NULL);
	if (entry->reload == NULL) {
	    ret = load_nif_error(BIF_P,reload,"Reload not supported by this NIF library.");
	    goto error;
	}
	/* Check that no NIF is removed */
	for (k=0; k < mod->nif->entry->num_of_funcs; k++) {
	    ErlNifFunc* old_func = &mod->nif->entry->funcs[k];
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
	erts_pre_nif(&env, BIF_P, lib);
	veto = entry->reload(&env, &lib->priv_data, BIF_ARG_2);
	erts_post_nif(&env);
	if (veto) {
	    ret = load_nif_error(BIF_P, reload, "Library reload-call unsuccessful.");
	}
	else {
	    mod->nif->entry = NULL; /* to prevent 'unload' callback */
	    erts_unload_nif(mod->nif);
	}
    }
    else {
	lib->priv_data = NULL;
	if (mod->old_nif != NULL) { /* Upgrade */
	    void* prev_old_data = mod->old_nif->priv_data;
	    if (entry->upgrade == NULL) {
		ret = load_nif_error(BIF_P, upgrade, "Upgrade not supported by this NIF library.");
		goto error;
	    }
	    erts_pre_nif(&env, BIF_P, lib);
	    veto = entry->upgrade(&env, &lib->priv_data, &mod->old_nif->priv_data, BIF_ARG_2);
	    erts_post_nif(&env);
	    if (veto) {
		mod->old_nif->priv_data = prev_old_data;
		ret = load_nif_error(BIF_P, upgrade, "Library upgrade-call unsuccessful.");
	    }
	    /*else if (mod->old_nif->priv_data != prev_old_data) {
		refresh_cached_nif_data(mod->old_code, mod->old_nif);
	    }*/
	}
	else if (entry->load != NULL) { /* Initial load */
	    erts_pre_nif(&env, BIF_P, lib);
	    veto = entry->load(&env, &lib->priv_data, BIF_ARG_2);
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
        mod->nif = lib; 
	for (i=0; i < entry->num_of_funcs; i++)
	{
	    BeamInstr* code_ptr;
	    erts_atom_get(entry->funcs[i].name, sys_strlen(entry->funcs[i].name), &f_atom); 
	    code_ptr = *get_func_pp(mod->code, f_atom, entry->funcs[i].arity); 
	    
	    if (code_ptr[1] == 0) {
		code_ptr[5+0] = (BeamInstr) BeamOp(op_call_nif);
	    }
	    else { /* Function traced, patch the original instruction word */
		BpData* bp = (BpData*) code_ptr[1];
	        bp->orig_instr = (BeamInstr) BeamOp(op_call_nif);
	    }	    
	    code_ptr[5+1] = (BeamInstr) entry->funcs[i].fptr;
	    code_ptr[5+2] = (BeamInstr) lib;
	}
    }
    else {
    error:
	ASSERT(ret != am_ok);
        if (lib != NULL) {
	    erts_free(ERTS_ALC_T_NIF, lib);
	}
	if (handle != NULL) {
	    erts_sys_ddll_close(handle);
	}
	erts_sys_ddll_free_error(&errdesc);
    }

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_free(ERTS_ALC_T_TMP, lib_name);
    BIF_RET(ret);
}


void
erts_unload_nif(struct erl_module_nif* lib)
{
    ErlNifResourceType* rt;
    ErlNifResourceType* next;
    ASSERT(erts_smp_is_system_blocked(0));
    ASSERT(lib != NULL);
    ASSERT(!lib->is_orphan);
    for (rt = resource_type_list.next;
	 rt != &resource_type_list;
	 rt = next) {

	next = rt->next;
	if (rt->owner == lib) {
	    rt->next->prev = rt->prev;
	    rt->prev->next = rt->next;
	    rt->next = NULL;
	    rt->prev = NULL;
	    if (erts_refc_dectest(&rt->refc, 0) == 0) {
		if (rt->dtor != NULL) {
		    erts_refc_dec(&lib->rt_dtor_cnt, 0);
		}
		erts_refc_dec(&lib->rt_cnt, 0);
		erts_free(ERTS_ALC_T_NIF, rt);
	    }
	}
    }
    if (erts_refc_read(&lib->rt_dtor_cnt, 0) == 0) {
	close_lib(lib);
	if (erts_refc_read(&lib->rt_cnt, 0) == 0) {
	    erts_free(ERTS_ALC_T_NIF, lib);
	    return;
	}
    }
    else {
	ASSERT(erts_refc_read(&lib->rt_cnt, 1) > 0);
    }
    lib->is_orphan = 1;
}	

void erl_nif_init()
{
    resource_type_list.next = &resource_type_list;
    resource_type_list.prev = &resource_type_list;
    resource_type_list.dtor = NULL;
    resource_type_list.owner = NULL;
    resource_type_list.name[0] = '\0';
}

#ifdef READONLY_CHECK
/* Use checksums to assert that NIFs do not write into inspected binaries
*/
static void readonly_check_dtor(struct enif_tmp_obj_t*);
static unsigned calc_checksum(unsigned char* ptr, unsigned size);

struct readonly_check_t
{
    struct enif_tmp_obj_t hdr;
    unsigned char* ptr;
    unsigned size;
    unsigned checksum;
};
static void add_readonly_check(ErlNifEnv* env, unsigned char* ptr, unsigned sz)
{
    struct readonly_check_t* obj = erts_alloc(ERTS_ALC_T_TMP, 
					      sizeof(struct readonly_check_t));
    obj->hdr.next = env->tmp_obj_list;
    env->tmp_obj_list = &obj->hdr;
    obj->hdr.dtor = &readonly_check_dtor;
    obj->ptr = ptr;
    obj->size = sz;
    obj->checksum = calc_checksum(ptr, sz);    
}
static void readonly_check_dtor(struct enif_tmp_obj_t* o)
{
    struct readonly_check_t* obj = (struct readonly_check_t*) o;
    unsigned chksum = calc_checksum(obj->ptr, obj->size);
    if (chksum != obj->checksum) { 
	fprintf(stderr, "\r\nReadonly data written by NIF, checksums differ"
		" %x != %x\r\nABORTING\r\n", chksum, obj->checksum);
	abort();
    }
    erts_free(ERTS_ALC_T_TMP,  obj);
}
static unsigned calc_checksum(unsigned char* ptr, unsigned size)
{
    unsigned i, sum = 0;
    for (i=0; i<size; i++) {
	sum ^= ptr[i] << ((i % 4)*8);
    }
    return sum;
}

#endif /* READONLY_CHECK */

