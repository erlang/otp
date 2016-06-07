/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
#include "erl_nif.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#ifndef __WIN32__
#include <unistd.h>
#endif

#include "nif_mod.h"

static int static_cntA; /* zero by default */
static int static_cntB = NIF_SUITE_LIB_VER * 100;

static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_self;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_join;
static ERL_NIF_TERM atom_binary_resource_type;
static ERL_NIF_TERM atom_seconds;
static ERL_NIF_TERM atom_milli_seconds;
static ERL_NIF_TERM atom_micro_seconds;
static ERL_NIF_TERM atom_nano_seconds;


typedef struct
{
    int ref_cnt;
    CallInfo* call_history;
    NifModPrivData* nif_mod;
    union { ErlNifResourceType* t; void* vp; } rt_arr[2];
} PrivData;

/*
 * Use a union for pointer type conversion to avoid compiler warnings
 * about strict-aliasing violations with gcc-4.1. gcc >= 4.2 does not
 * emit the warning.
 * TODO: Reconsider use of union once gcc-4.1 is obsolete?
 */
typedef union {
    void* vp;
    struct make_term_info* p;
} mti_t;

void add_call(ErlNifEnv* env, PrivData* data, const char* func_name)
{
    CallInfo* call = enif_alloc(sizeof(CallInfo)+strlen(func_name));
    strcpy(call->func_name, func_name);
    call->lib_ver = NIF_SUITE_LIB_VER;
    call->next = data->call_history;
    call->static_cntA = ++static_cntA;
    call->static_cntB = ++static_cntB;
    data->call_history = call;
    call->arg = NULL;
    call->arg_sz = 0;
}

#define ADD_CALL(FUNC_NAME) add_call(env, enif_priv_data(env),FUNC_NAME)

static void*    resource_dtor_last = NULL;
static unsigned resource_dtor_last_sz = 0;
static char     resource_dtor_last_data[20];
static int resource_dtor_cnt = 0;

static void resource_dtor(ErlNifEnv* env, void* obj)
{
    resource_dtor_last = obj;
    resource_dtor_cnt++;
    resource_dtor_last_sz = enif_sizeof_resource(obj);
    assert(resource_dtor_last_sz <= sizeof(resource_dtor_last_data));
    memcpy(resource_dtor_last_data, obj, resource_dtor_last_sz);
}

static ErlNifResourceType* msgenv_resource_type;
static void msgenv_dtor(ErlNifEnv* env, void* obj);

static ErlNifResourceType* binary_resource_type;
static void binary_resource_dtor(ErlNifEnv* env, void* obj);
struct binary_resource {
    unsigned char* data;
    unsigned size;
};

static int get_pointer(ErlNifEnv* env, ERL_NIF_TERM term, void** pp)
{
    ErlNifUInt64 i64;
    int r = enif_get_uint64(env, term, &i64);
    if (r) {
	*pp = (void*)i64;
    }
    return r;
}

static ERL_NIF_TERM make_pointer(ErlNifEnv* env, void* p)
{
    ErlNifUInt64 i64 = (ErlNifUInt64) p;
    return enif_make_uint64(env, i64);
}


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    PrivData* data = enif_alloc(sizeof(PrivData));
    assert(data != NULL);
    data->ref_cnt = 1;
    data->call_history = NULL;
    data->nif_mod = NULL;

    add_call(env, data, "load");

    data->rt_arr[0].t = enif_open_resource_type(env,NULL,"Gold",resource_dtor,
						ERL_NIF_RT_CREATE,NULL);
    data->rt_arr[1].t = enif_open_resource_type(env,NULL,"Silver",resource_dtor,
						ERL_NIF_RT_CREATE,NULL);

    binary_resource_type =  enif_open_resource_type(env,NULL,"nif_SUITE.binary",
						    binary_resource_dtor,
						    ERL_NIF_RT_CREATE, NULL);

    msgenv_resource_type =  enif_open_resource_type(env,NULL,"nif_SUITE.msgenv",
						    msgenv_dtor,
						    ERL_NIF_RT_CREATE, NULL);
    atom_false = enif_make_atom(env,"false");
    atom_true = enif_make_atom(env,"true");
    atom_self = enif_make_atom(env,"self");
    atom_ok = enif_make_atom(env,"ok");
    atom_join = enif_make_atom(env,"join");
    atom_binary_resource_type = enif_make_atom(env,"binary_resource_type");
    atom_seconds = enif_make_atom(env,"seconds");
    atom_milli_seconds = enif_make_atom(env,"milli_seconds");
    atom_micro_seconds = enif_make_atom(env,"micro_seconds");
    atom_nano_seconds = enif_make_atom(env,"nano_seconds");

    *priv_data = data;
    return 0;
}

static void resource_takeover(ErlNifEnv* env, PrivData* priv)
{
    ErlNifResourceFlags tried;
    ErlNifResourceType* rt;
    rt = enif_open_resource_type(env, NULL, "Gold", resource_dtor,
				 ERL_NIF_RT_TAKEOVER, &tried);
    assert(rt == priv->rt_arr[0].t); 
    assert(tried == ERL_NIF_RT_TAKEOVER);
    rt = enif_open_resource_type(env, NULL, "Silver", resource_dtor,
				 ERL_NIF_RT_TAKEOVER, &tried);
    assert(rt == priv->rt_arr[1].t); 
    assert(tried == ERL_NIF_RT_TAKEOVER);

    rt =  enif_open_resource_type(env, NULL, "nif_SUITE.binary", binary_resource_dtor,
				  ERL_NIF_RT_TAKEOVER, &tried);
    assert(rt != NULL);
    assert(tried == ERL_NIF_RT_TAKEOVER);
    assert(binary_resource_type==NULL || binary_resource_type == rt); 
    binary_resource_type = rt;

    rt = enif_open_resource_type(env, NULL, "nif_SUITE.msgenv", msgenv_dtor,
				  ERL_NIF_RT_TAKEOVER, &tried);
    assert(rt != NULL);
    assert(tried == ERL_NIF_RT_TAKEOVER);
    assert(msgenv_resource_type==NULL || msgenv_resource_type == rt); 
    msgenv_resource_type = rt;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    PrivData* priv = (PrivData*) *priv_data;
    add_call(env, priv, "reload");
    resource_takeover(env,priv);
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    PrivData* priv = (PrivData*) *old_priv_data;
    add_call(env, priv, "upgrade");
    priv->ref_cnt++;
    *priv_data = *old_priv_data;
    resource_takeover(env,priv);
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    PrivData* data = priv_data;
    add_call(env, data, "unload");
    if (--data->ref_cnt == 0) {
	if (data->nif_mod != NULL) {
	    NifModPrivData_release(data->nif_mod);
	}
	enif_free(priv_data);
    }
}

static ERL_NIF_TERM lib_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ADD_CALL("lib_version");
    return enif_make_int(env, NIF_SUITE_LIB_VER);
}

static ERL_NIF_TERM make_call_history(ErlNifEnv* env, CallInfo** headp)
{
    ERL_NIF_TERM list = enif_make_list(env, 0); /* NIL */

    while (*headp != NULL) {
	CallInfo* call = *headp;
	ERL_NIF_TERM func_term = enif_make_atom(env,call->func_name);
	ERL_NIF_TERM tpl;
	if (call->arg != NULL) {
	    ERL_NIF_TERM arg_bin;	    
	    memcpy(enif_make_new_binary(env, call->arg_sz, &arg_bin),
		   call->arg, call->arg_sz);
	    func_term = enif_make_tuple2(env, func_term, arg_bin);
	}
	tpl = enif_make_tuple4(env, func_term, 					    
			       enif_make_int(env,call->lib_ver),
			       enif_make_int(env,call->static_cntA),
			       enif_make_int(env,call->static_cntB));
	list = enif_make_list_cell(env, tpl, list);
	*headp = call->next;
	enif_free(call);
    }
    return list;
}

static ERL_NIF_TERM call_history(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* data = (PrivData*) enif_priv_data(env);

    return make_call_history(env,&data->call_history);
}

static ERL_NIF_TERM hold_nif_mod_priv_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* data = (PrivData*) enif_priv_data(env);
    void* ptr;
    
    if (!get_pointer(env,argv[0],&ptr)) {
	return enif_make_badarg(env);
    }
    if (data->nif_mod != NULL) {
	NifModPrivData_release(data->nif_mod);
    }
    data->nif_mod = (NifModPrivData*) ptr;    
    return enif_make_int(env,++(data->nif_mod->ref_cnt)); 
}

static ERL_NIF_TERM nif_mod_call_history(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* data = (PrivData*) enif_priv_data(env);
    ERL_NIF_TERM ret;
    if (data->nif_mod == NULL) {
	return enif_make_string(env,"nif_mod pointer is NULL", ERL_NIF_LATIN1);
    }
    enif_mutex_lock(data->nif_mod->mtx);
    ret = make_call_history(env, &data->nif_mod->call_history);
    enif_mutex_unlock(data->nif_mod->mtx);
    return ret;
}

static ERL_NIF_TERM list_seq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM list;
    int n;
    if (!enif_get_int(env, argv[0], &n)) {
	return enif_make_badarg(env);
    }
    list = enif_make_list(env, 0); /* NIL */
    while (n > 0) {
	list = enif_make_list_cell(env, enif_make_int(env,n), list);
	n--;
    }
    return list;
}

static int test_int(ErlNifEnv* env, int i1)
{
    int i2 = 0;
    ERL_NIF_TERM int_term = enif_make_int(env, i1);
    if (!enif_get_int(env,int_term, &i2) || i1 != i2) {
	fprintf(stderr, "test_int(%d) ...FAILED i2=%d\r\n", i1, i2);
	return 0;
    }
    return 1;
}

static int test_uint(ErlNifEnv* env, unsigned i1)
{
    unsigned i2 = 0;
    ERL_NIF_TERM int_term = enif_make_uint(env, i1);
    if (!enif_get_uint(env,int_term, &i2) || i1 != i2) {
	fprintf(stderr, "test_uint(%u) ...FAILED i2=%u\r\n", i1, i2);
	return 0;
    }
    return 1;
}

static int test_long(ErlNifEnv* env, long i1)
{
    long i2 = 0;
    ERL_NIF_TERM int_term = enif_make_long(env, i1);
    if (!enif_get_long(env,int_term, &i2) || i1 != i2) {
	fprintf(stderr, "test_long(%ld) ...FAILED i2=%ld\r\n", i1, i2);
	return 0;
    }
    return 1;
}

static int test_ulong(ErlNifEnv* env, unsigned long i1)
{
    unsigned long i2 = 0;
    ERL_NIF_TERM int_term = enif_make_ulong(env, i1);
    if (!enif_get_ulong(env,int_term, &i2) || i1 != i2) {
	fprintf(stderr, "test_ulong(%lu) ...FAILED i2=%lu\r\n", i1, i2);
	return 0;
    }
    return 1;
}

static int test_int64(ErlNifEnv* env, ErlNifSInt64 i1)
{
    ErlNifSInt64 i2 = 0;
    ERL_NIF_TERM int_term = enif_make_int64(env, i1);
    if (!enif_get_int64(env,int_term, &i2) || i1 != i2) {
	fprintf(stderr, "test_int64(%ld) ...FAILED i2=%ld\r\n",
		(long)i1, (long)i2);
	return 0;
    }
    return 1;
}

static int test_uint64(ErlNifEnv* env, ErlNifUInt64 i1)
{
    ErlNifUInt64 i2 = 0;
    ERL_NIF_TERM int_term = enif_make_uint64(env, i1);
    if (!enif_get_uint64(env,int_term, &i2) || i1 != i2) {
	fprintf(stderr, "test_ulong(%lu) ...FAILED i2=%lu\r\n",
		(unsigned long)i1, (unsigned long)i2);
	return 0;
    }
    return 1;
}

static int test_double(ErlNifEnv* env, double d1)
{
    double d2 = 0;
    ERL_NIF_TERM term = enif_make_double(env, d1);
    if (!enif_get_double(env,term, &d2) || d1 != d2) {
	fprintf(stderr, "test_double(%e) ...FAILED i2=%e\r\n", d1, d2);
	return 0;
    }
    return 1;
}

#define TAG_BITS        4
#define SMALL_BITS	(sizeof(void*)*8 - TAG_BITS)
#ifdef _WIN64
#define MAX_SMALL	((1LL << (SMALL_BITS-1))-1)
#define MIN_SMALL	(-(1LL << (SMALL_BITS-1)))
#else
#define MAX_SMALL	((1L << (SMALL_BITS-1))-1)
#define MIN_SMALL	(-(1L << (SMALL_BITS-1)))
#endif

static ERL_NIF_TERM type_test(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i;
    int sint;
    unsigned uint;
    long slong;
    unsigned long ulong;
    ErlNifSInt64 sint64;
    ErlNifUInt64 uint64;
    double d;
    ERL_NIF_TERM atom, ref1, ref2, term;
    size_t len;

    sint = INT_MIN;
    do {
	if (!test_int(env,sint)) {
	    goto error;
	}
	sint += ~sint / 3 + 1;
    } while (sint < 0);
    sint = INT_MAX;
    do {
	if (!test_int(env,sint)) {
	    goto error;
	}
	sint -= sint / 3 + 1;
    } while (sint >= 0);

    slong = LONG_MIN;
    do {
	if (!test_long(env,slong)) {
	    goto error;
	}
	slong += ~slong / 3 + 1;
    } while (slong < 0);
    slong = LONG_MAX;
    do {
	if (!test_long(env,slong)) {
	    goto error;
	}
	slong -= slong / 3 + 1;
    } while (slong >= 0);

    sint64 = ((ErlNifSInt64)1 << 63); /* INT64_MIN */
    do {
	if (!test_int64(env,sint64)) {
	    goto error;
	}
	sint64 += ~sint64 / 3 + 1;
    } while (sint64 < 0);
    sint64 = ((ErlNifUInt64)1 << 63) - 1; /* INT64_MAX */
    do {
	if (!test_int64(env,sint64)) {
	    goto error;
	}
	sint64 -= sint64 / 3 + 1;
    } while (sint64 >= 0);

    uint = UINT_MAX;
    for (;;) {
	if (!test_uint(env,uint)) {
	    goto error;
	}
	if (uint == 0) break;
	uint -= uint / 3 + 1;
    }
    ulong = ULONG_MAX;
    for (;;) {
	if (!test_ulong(env,ulong)) {
	    goto error;
	}
	if (ulong == 0) break;
	ulong -= ulong / 3 + 1;
    }    
    uint64 = (ErlNifUInt64)-1; /* UINT64_MAX */
    for (;;) {
	if (!test_uint64(env,uint64)) {
	    goto error;
	}
	if (uint64 == 0) break;
	uint64 -= uint64 / 3 + 1;
    }    

    if (MAX_SMALL < INT_MAX) { /* 32-bit */
	for (i=-10 ; i <= 10; i++) {
	    if (!test_int(env,MAX_SMALL+i)) {
		goto error;
	    }
	}
	for (i=-10 ; i <= 10; i++) {
	    if (!test_int(env,MIN_SMALL+i)) {
		goto error;
	    }
	}
	for (i=-10 ; i <= 10; i++) {
	    if (!test_uint(env,MAX_SMALL+i)) {
		goto error;
	    }
	}
    }
    assert((MAX_SMALL < INT_MAX) == (MIN_SMALL > INT_MIN));

    for (i=-10 ; i < 10; i++) {
	if (!test_long(env,MAX_SMALL+i) || !test_ulong(env,MAX_SMALL+i) ||
	    !test_long(env,MIN_SMALL+i) ||
	    !test_int64(env,MAX_SMALL+i) || !test_uint64(env,MAX_SMALL+i) ||
	    !test_int64(env,MIN_SMALL+i)) {
	    goto error;
	}
	if (MAX_SMALL < INT_MAX) {
	    if (!test_int(env,MAX_SMALL+i) || !test_uint(env,MAX_SMALL+i) ||
		!test_int(env,MIN_SMALL+i)) {
		goto error;
	    }
	}
    }
    for (d=3.141592e-100 ; d < 1e100 ; d *= 9.97) {
	if (!test_double(env,d) || !test_double(env,-d)) {
	    goto error;
	}	
    }

    if (!enif_make_existing_atom(env,"nif_SUITE", &atom, ERL_NIF_LATIN1)
	|| !enif_is_identical(atom,enif_make_atom(env,"nif_SUITE"))) {
	fprintf(stderr, "nif_SUITE not an atom?\r\n");
	goto error;
    }
    for (i=2; i; i--) {
	if (enif_make_existing_atom(env,"nif_SUITE_pink_unicorn", &atom, ERL_NIF_LATIN1)) {
	    fprintf(stderr, "pink unicorn exist?\r\n");
	    goto error;
	}
    }

    ref1 = enif_make_ref(env);
    ref2 = enif_make_ref(env);
    if (!enif_is_ref(env,ref1) || !enif_is_ref(env,ref2) 
	|| enif_is_identical(ref1,ref2) || enif_compare(ref1,ref2)==0) {
	fprintf(stderr, "strange refs?\r\n");
	goto error;
    }
    return enif_make_atom(env,"ok");

error:
    return enif_make_atom(env,"error");
}

static ERL_NIF_TERM echo_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int sint;
    unsigned uint;
    long slong;
    unsigned long ulong;
    ErlNifSInt64 sint64;
    ErlNifUInt64 uint64;    
    ERL_NIF_TERM sint_term = atom_false,   uint_term = atom_false;
    ERL_NIF_TERM slong_term = atom_false,  ulong_term = atom_false;
    ERL_NIF_TERM sint64_term = atom_false, uint64_term = atom_false;

    if (enif_get_int(env, argv[0], &sint)) {
	sint_term = enif_make_int(env, sint);
    }
    if (enif_get_uint(env, argv[0], &uint)) {
	uint_term = enif_make_uint(env, uint);
    }
    if (enif_get_long(env, argv[0], &slong)) {
	slong_term = enif_make_long(env, slong);
    }
    if (enif_get_ulong(env, argv[0], &ulong)) {
	ulong_term = enif_make_ulong(env, ulong);
    }
    if (enif_get_int64(env, argv[0], &sint64)) {
	sint64_term = enif_make_int64(env, sint64);
    }
    if (enif_get_uint64(env, argv[0], &uint64)) {
	uint64_term = enif_make_uint64(env, uint64);
    }
    return enif_make_list6(env, sint_term, uint_term, slong_term, ulong_term, sint64_term, uint64_term);
}

static ERL_NIF_TERM type_sizes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_tuple2(env, enif_make_int(env, sizeof(int)),
			    enif_make_int(env, sizeof(long)));
}

static ERL_NIF_TERM tuple_2_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int arity = -1;
    const ERL_NIF_TERM* ptr;
    ERL_NIF_TERM list = enif_make_list(env,0);

    if (argc!=1 || !enif_get_tuple(env,argv[0],&arity,&ptr)) {
	return enif_make_badarg(env);
    }
    while (--arity >= 0) {
	list = enif_make_list_cell(env,ptr[arity],list);
    }
    return list;
}

static ERL_NIF_TERM is_identical(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{    
    if (argc != 2) {
	return enif_make_badarg(env);
    }
    return enif_make_atom(env, (enif_is_identical(argv[0],argv[1]) ?
				"true" : "false"));
}

static ERL_NIF_TERM compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{    
    if (argc != 2) {
	return enif_make_badarg(env);
    }
    return enif_make_int(env, enif_compare(argv[0],argv[1]));
}

static ERL_NIF_TERM many_args_100(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i, k;
    if (argc == 100) {
	for (i=1; i<=100; i++) {
	    if (!enif_get_int(env,argv[i-1],&k) || k!=i) {
		goto badarg;
	    }
	}
	return enif_make_atom(env,"ok");
    }
badarg:
    return enif_make_badarg(env);
}

static ERL_NIF_TERM clone_bin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary ibin;
    if (enif_inspect_binary(env,argv[0],&ibin)) {
	ERL_NIF_TERM obin;	
	memcpy(enif_make_new_binary(env, ibin.size, &obin),
	       ibin.data, ibin.size);
	return obin;
    }
    else {
	return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM make_sub_bin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int pos, size;
    if (!enif_get_int(env,argv[1],&pos) || !enif_get_int(env,argv[2],&size)) {
	return enif_make_badarg(env);
    }
    return enif_make_sub_binary(env,argv[0],pos,size);
}

static ERL_NIF_TERM string_to_bin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary obin;
    unsigned size;
    int n;
    if (!enif_get_int(env,argv[1],(int*)&size) 
	|| !enif_alloc_binary(size,&obin)) {
	return enif_make_badarg(env);
    }
    n = enif_get_string(env, argv[0], (char*)obin.data, size, ERL_NIF_LATIN1);
    return enif_make_tuple(env, 2, enif_make_int(env,n),
			   enif_make_binary(env,&obin));
}

static ERL_NIF_TERM atom_to_bin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary obin;
    unsigned size;
    int n;
    if (!enif_get_int(env,argv[1],(int*)&size) 
	|| !enif_alloc_binary(size,&obin)) {
	return enif_make_badarg(env);
    }
    n = enif_get_atom(env, argv[0], (char*)obin.data, size, ERL_NIF_LATIN1);
    return enif_make_tuple(env, 2, enif_make_int(env,n),
			   enif_make_binary(env,&obin));
}

static ERL_NIF_TERM macros(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM* a;
    ERL_NIF_TERM lists, tuples;
    int arity;
    if (!enif_get_tuple(env, argv[0], &arity, &a) || arity != 9) {
	return enif_make_badarg(env);
    }
    
    lists = enif_make_list(env,9,
			   enif_make_list1(env,a[0]),
			   enif_make_list2(env,a[0],a[1]),
			   enif_make_list3(env,a[0],a[1],a[2]),
			   enif_make_list4(env,a[0],a[1],a[2],a[3]),
			   enif_make_list5(env,a[0],a[1],a[2],a[3],a[4]),
			   enif_make_list6(env,a[0],a[1],a[2],a[3],a[4],a[5]),
			   enif_make_list7(env,a[0],a[1],a[2],a[3],a[4],a[5],a[6]),
			   enif_make_list8(env,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]),
			   enif_make_list9(env,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
    tuples = enif_make_list(env,9,
			    enif_make_tuple1(env,a[0]),
			    enif_make_tuple2(env,a[0],a[1]),
			    enif_make_tuple3(env,a[0],a[1],a[2]),
			    enif_make_tuple4(env,a[0],a[1],a[2],a[3]),
			    enif_make_tuple5(env,a[0],a[1],a[2],a[3],a[4]),
			    enif_make_tuple6(env,a[0],a[1],a[2],a[3],a[4],a[5]),
			    enif_make_tuple7(env,a[0],a[1],a[2],a[3],a[4],a[5],a[6]),
			    enif_make_tuple8(env,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]),
			    enif_make_tuple9(env,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]));
    return enif_make_tuple2(env,lists,tuples);
}

static ERL_NIF_TERM tuple_2_list_and_tuple(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM* arr;
    int arity;
    if (!enif_get_tuple(env,argv[0],&arity,&arr)) {
	return enif_make_badarg(env);
    }
    return enif_make_tuple2(env, 
			    enif_make_list_from_array(env, arr, arity),
			    enif_make_tuple_from_array(env, arr, arity));
}

static ERL_NIF_TERM iolist_2_bin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary obin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &obin)) {
	return enif_make_badarg(env);
    }
    return enif_make_binary(env,&obin);
}

static ERL_NIF_TERM last_resource_dtor_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    if (resource_dtor_last != NULL) {
	ERL_NIF_TERM bin;	
	memcpy(enif_make_new_binary(env, resource_dtor_last_sz, &bin),
	       resource_dtor_last_data, resource_dtor_last_sz);
	ret = enif_make_tuple3(env,
				make_pointer(env, resource_dtor_last),
				bin,  
				enif_make_int(env, resource_dtor_cnt));
    }
    else {
	ret = enif_make_list(env,0);
    }
    resource_dtor_last = NULL;
    resource_dtor_last_sz = 0;
    resource_dtor_cnt = 0;
    return ret;
}

static ERL_NIF_TERM get_resource_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* data = (PrivData*) enif_priv_data(env);
    int ix;
    
    if (!enif_get_int(env, argv[0], &ix) || ix >= 2) {
	return enif_make_badarg(env);
    }
    return make_pointer(env, data->rt_arr[ix].vp);
}

static ERL_NIF_TERM alloc_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data_bin;
    union { ErlNifResourceType* t; void* vp; } type;
    void* data;
    if (!get_pointer(env, argv[0], &type.vp)
	|| !enif_inspect_binary(env, argv[1], &data_bin)
	|| (data = enif_alloc_resource(type.t, data_bin.size))==NULL) {

	return enif_make_badarg(env);
    }
    memcpy(data, data_bin.data, data_bin.size);
    return make_pointer(env, data);
}

static ERL_NIF_TERM make_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* data;
    if (!get_pointer(env, argv[0], &data)) {
	return enif_make_badarg(env);
    }
    return enif_make_resource(env, data);
}

static ERL_NIF_TERM make_new_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data_bin;
    union { ErlNifResourceType* t; void* vp; } type;
    void* data;
    ERL_NIF_TERM ret;
    if (!get_pointer(env, argv[0], &type.vp)
	|| !enif_inspect_binary(env, argv[1], &data_bin)
	|| (data = enif_alloc_resource(type.t, data_bin.size))==NULL) {

	return enif_make_badarg(env);
    }
    ret = enif_make_resource(env, data);
    memcpy(data, data_bin.data, data_bin.size);
    enif_release_resource(data);
    return ret;
}

static ERL_NIF_TERM make_new_resource_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data_bin;
    union { struct binary_resource* p; void* vp; } br;
    void* buf;
    ERL_NIF_TERM ret;
    if (!enif_inspect_binary(env, argv[0], &data_bin)
	|| (br.vp = enif_alloc_resource(binary_resource_type,
					sizeof(struct binary_resource)))==NULL
	|| (buf = enif_alloc(data_bin.size)) == NULL) {
	
	return enif_make_badarg(env);
    }    
    memset(br.vp,0xba,sizeof(struct binary_resource)); /* avoid valgrind warning */
    br.p->data = buf;
    br.p->size = data_bin.size;
    memcpy(br.p->data, data_bin.data, data_bin.size);    
    ret = enif_make_resource_binary(env, br.vp, br.p->data, br.p->size);    
    enif_release_resource(br.p);
    return enif_make_tuple2(env, make_pointer(env,br.vp), ret);
}

static void binary_resource_dtor(ErlNifEnv* env, void* obj)
{
    struct binary_resource* br = (struct binary_resource*) obj;
    resource_dtor(env,obj);
    assert(br->data != NULL);
    enif_free(br->data);
    br->data = NULL;
}

static ERL_NIF_TERM get_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data_bin;
    union { ErlNifResourceType* t; void* vp; } type;
    void* data;

    type.t = NULL;
    if (enif_is_identical(argv[0], atom_binary_resource_type)) {
	type.t = binary_resource_type;
    }
    else {
	get_pointer(env, argv[0], &type.vp);
    }
    if (type.t == NULL 
	|| !enif_get_resource(env, argv[1], type.t, &data)) {
	return enif_make_badarg(env);
    }
    enif_alloc_binary(enif_sizeof_resource(data), &data_bin);    
    memcpy(data_bin.data, data, data_bin.size);
    return enif_make_tuple2(env, make_pointer(env,data),
			    enif_make_binary(env, &data_bin));
}

static ERL_NIF_TERM release_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* data;
    if (!get_pointer(env, argv[0], &data)) {
	return enif_make_badarg(env);
    }
    enif_release_resource(data);
    return enif_make_atom(env,"ok");
}

/*
 * argv[0] an atom
 * argv[1] a binary
 * argv[2] a ref
 * argv[3] 'ok'
 * argv[4] a fun
 * argv[5] a pid
 * argv[6] a port
 * argv[7] an empty list
 * argv[8] a non-empty list
 * argv[9] a tuple
 * argv[10] a number (small, big integer or float)
 */
static ERL_NIF_TERM check_is(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ok_atom = enif_make_atom(env, "ok");

    if (!enif_is_atom(env, argv[0])) return enif_make_badarg(env);
    if (!enif_is_binary(env, argv[1])) return enif_make_badarg(env);
    if (!enif_is_ref(env, argv[2])) return enif_make_badarg(env);
    if (!enif_is_identical(argv[3], ok_atom)) return enif_make_badarg(env);
    if (!enif_is_fun(env, argv[4])) return enif_make_badarg(env);
    if (!enif_is_pid(env, argv[5])) return enif_make_badarg(env);
    if (!enif_is_port(env, argv[6])) return enif_make_badarg(env);
    if (!enif_is_empty_list(env, argv[7])) return enif_make_badarg(env);
    if (!enif_is_list(env, argv[7])) return enif_make_badarg(env);
    if (!enif_is_list(env, argv[8])) return enif_make_badarg(env);
    if (!enif_is_tuple(env, argv[9])) return enif_make_badarg(env);
    if (!enif_is_number(env, argv[10])) return enif_make_badarg(env);

    return ok_atom;
}

/*
 * no arguments
 *
 * This function is separate from check_is because it calls enif_make_badarg
 * and so it must return the badarg exception as its return value. Thus, the
 * badarg exception indicates success.
 */
static ERL_NIF_TERM check_is_exception(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM badarg, exc_term;
    ERL_NIF_TERM error_atom = enif_make_atom(env, "error");
    ERL_NIF_TERM badarg_atom = enif_make_atom(env, "badarg");
    assert(!enif_is_exception(env, error_atom));
    badarg = enif_make_badarg(env);
    assert(enif_is_exception(env, badarg));
    assert(enif_has_pending_exception(env, NULL));
    assert(enif_has_pending_exception(env, &exc_term));
    assert(enif_is_identical(exc_term, badarg_atom));
    return badarg;
}

/*
 * argv[0] atom with length of 6
 * argv[1] list with length of 6
 * argv[2] empty list
 * argv[3] not an atom
 * argv[4] not a list
 * argv[5] improper list
 */
static ERL_NIF_TERM length_test(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned len;

    if (!enif_get_atom_length(env, argv[0], &len, ERL_NIF_LATIN1) || len != 6)
	return enif_make_badarg(env);

    if (!enif_get_list_length(env, argv[1], &len) || len != 6)
	return enif_make_badarg(env);

    if (!enif_get_list_length(env, argv[2], &len) || len != 0)
	return enif_make_badarg(env);

    if (enif_get_atom_length(env, argv[3], &len, ERL_NIF_LATIN1))
	return enif_make_badarg(env);

    if (enif_get_list_length(env, argv[4], &len))
	return enif_make_badarg(env);

    if (enif_get_list_length(env, argv[5], &len))
	return enif_make_badarg(env);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM make_atoms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM arr[7];
    ERL_NIF_TERM existingatom0a, existingatom0b;
    ERL_NIF_TERM existing0atom0;
    const char * const an0atom = "an0atom";
    const char an0atom0[8] = {'a','n','\0','a','t','o','m',0};

    arr[0] = enif_make_atom(env, "an0atom");
    arr[1] = enif_make_atom_len(env, "an0atom", 7);
    arr[2] = enif_make_atom_len(env, an0atom, 7);
    arr[3] = enif_make_atom_len(env, an0atom0, 8);

    if (!enif_make_existing_atom(env, "an0atom", &existingatom0a, ERL_NIF_LATIN1))
	return enif_make_atom(env, "error");
    arr[4] = existingatom0a;

    if (!enif_make_existing_atom_len(env, an0atom, 7, &existingatom0b, ERL_NIF_LATIN1))
	return enif_make_atom(env, "error");
    arr[5] = existingatom0b;

    if (!enif_make_existing_atom_len(env, an0atom0, 8, &existing0atom0, ERL_NIF_LATIN1))
	return enif_make_atom(env, "error");
    arr[6] = existing0atom0;

    return enif_make_tuple7(env,
			    arr[0],arr[1],arr[2],arr[3],arr[4],arr[5],arr[6]);
}

static ERL_NIF_TERM make_strings(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const char a0string[8] = {'a','0','s','t','r','i','n','g'};
    const char a0string0[9] = {'a','\0','s','t','r','i','n','g',0};
    const char astringwith8bits[37] = {'E','r','l','a','n','g',' ',0xE4 /* 'ä' */,'r',' ','e','t','t',' ','g','e','n','e','r','e','l','l','t',' ','p','r','o','g','r','a','m','s','p','r', 0xE5 /* 'å' */,'k',0};

    return enif_make_tuple5(env,
			    enif_make_string(env, "a0string", ERL_NIF_LATIN1),
			    enif_make_string_len(env, "a0string", 8, ERL_NIF_LATIN1),
			    enif_make_string_len(env, a0string, 8, ERL_NIF_LATIN1),
			    enif_make_string_len(env, a0string0, 9, ERL_NIF_LATIN1),
			    enif_make_string(env, astringwith8bits, ERL_NIF_LATIN1));
}
static ERL_NIF_TERM send_list_seq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid to;
    ERL_NIF_TERM msg;
    ErlNifEnv* msg_env;
    int i, res;
    
    if (!enif_get_int(env, argv[0], &i)) {
	return enif_make_badarg(env);
    }
    if (argv[1] == atom_self) {
	enif_self(env, &to);
    }
    else if (!enif_get_local_pid(env, argv[1], &to)) {
	return enif_make_badarg(env);
    }
    msg_env = enif_alloc_env();
    msg = enif_make_list(msg_env,0);
    for ( ; i>0 ; i--) {
	msg = enif_make_list_cell(msg_env, enif_make_int(msg_env, i), msg);
    }
    res = enif_send(env, &to, msg_env, msg);
    enif_free_env(msg_env);
    return enif_make_tuple2(env, atom_ok, enif_make_int(env,res));
}

static void fill(void* dst, unsigned bytes, int seed)
{
    unsigned char* ptr = dst;
    int i;
    for (i=bytes; i>0; i--) {
	*ptr++ = seed;
	seed += 7;
    }
}

#define MAKE_TERM_REUSE_LEN 16
struct make_term_info
{
    ErlNifEnv* caller_env;
    ErlNifEnv* dst_env;
    ERL_NIF_TERM reuse[MAKE_TERM_REUSE_LEN];
    unsigned reuse_push;
    unsigned reuse_pull;
    ErlNifResourceType* resource_type;
    ERL_NIF_TERM other_term;
    ERL_NIF_TERM blob;
    ErlNifPid to_pid;
    ErlNifTid tid;
    ErlNifCond* cond;
    ErlNifMutex* mtx;
    int send_it;
    int send_res;
    unsigned n;
};


static void push_term(struct make_term_info* mti, ERL_NIF_TERM term)
{
    unsigned ix = (mti->reuse_push++) % MAKE_TERM_REUSE_LEN; 
    mti->reuse[ix] = term;
    //enif_fprintf(stderr, "push at %u: %T\r\n", ix, term);
}
static ERL_NIF_TERM pull_term(struct make_term_info* mti)
{
    unsigned ix;
    if (mti->reuse_pull >= mti->reuse_push &&
	mti->reuse_push < MAKE_TERM_REUSE_LEN) {
	mti->reuse_pull = 0;
	if (mti->reuse_push == 0) {
	    mti->reuse[0] = enif_make_list(mti->dst_env, 0);
	}
    }
    ix = (mti->reuse_pull++) % MAKE_TERM_REUSE_LEN;
    //enif_fprintf(stderr, "pull from %u: %T\r\n", ix, mti->reuse[ix]);
    return mti->reuse[ix];
}

static int make_term_n(struct make_term_info* mti, int n, ERL_NIF_TERM* res);

static ERL_NIF_TERM make_term_binary(struct make_term_info* mti, int n)
{	
    ErlNifBinary bin;
    enif_alloc_binary(100, &bin);
    fill(bin.data, bin.size, n);
    return enif_make_binary(mti->dst_env, &bin);
}

static ERL_NIF_TERM make_term_int(struct make_term_info* mti, int n)
{
    int i;
    fill(&i, sizeof(i), n);
    return enif_make_int(mti->dst_env, i);
}

static ERL_NIF_TERM make_term_ulong(struct make_term_info* mti, int n)
{
    unsigned long ul;
    fill(&ul, sizeof(ul), n);
    return enif_make_ulong(mti->dst_env, ul);
}

static ERL_NIF_TERM make_term_double(struct make_term_info* mti, int n)
{
    double d = 3.141592;
    return enif_make_double(mti->dst_env, d);    	
}
static ERL_NIF_TERM make_term_atom(struct make_term_info* mti, int n)
{
    return enif_make_atom(mti->dst_env, "make_term_n");    
} 
static ERL_NIF_TERM make_term_existing_atom(struct make_term_info* mti, int n)
{	
    ERL_NIF_TERM res;
    int exist = enif_make_existing_atom(mti->dst_env, "nif_SUITE", &res,
					ERL_NIF_LATIN1);
    assert(exist);
    return res;
}
static ERL_NIF_TERM make_term_string(struct make_term_info* mti, int n)
{
    return enif_make_string(mti->dst_env, "Hello!", ERL_NIF_LATIN1);
}
static ERL_NIF_TERM make_term_ref(struct make_term_info* mti, int n)
{
    return enif_make_ref(mti->dst_env);
}
static ERL_NIF_TERM make_term_sub_binary(struct make_term_info* mti, int n)
{
    ERL_NIF_TERM orig;
    unsigned char* ptr = enif_make_new_binary(mti->dst_env, 10, &orig);
    fill(ptr, 10, n);
    return enif_make_sub_binary(mti->dst_env, orig, 3, 5);	
}
static ERL_NIF_TERM make_term_uint(struct make_term_info* mti, int n)
{
    unsigned int ui;
    fill(&ui, sizeof(ui), n);
    return enif_make_uint(mti->dst_env, ui);	       
}
static ERL_NIF_TERM make_term_long(struct make_term_info* mti, int n)
{
    long l;
    fill(&l, sizeof(l), n);
    return enif_make_long(mti->dst_env, l);
}
static ERL_NIF_TERM make_term_tuple0(struct make_term_info* mti, int n)
{
    return enif_make_tuple(mti->dst_env, 0);
}
static ERL_NIF_TERM make_term_list0(struct make_term_info* mti, int n)
{
    return enif_make_list(mti->dst_env, 0);
}
static ERL_NIF_TERM make_term_resource(struct make_term_info* mti, int n)
{
    void* resource = enif_alloc_resource(mti->resource_type, 10);
    ERL_NIF_TERM term;
    fill(resource, 10, n); 
    term = enif_make_resource(mti->dst_env, resource);
    enif_release_resource(resource);
    return term;
}
static ERL_NIF_TERM make_term_new_binary(struct make_term_info* mti, int n)
{
    ERL_NIF_TERM res;
    unsigned char* ptr = enif_make_new_binary(mti->dst_env,20,&res);
    fill(ptr, 20, n);	
    return res;
}
static ERL_NIF_TERM make_term_caller_pid(struct make_term_info* mti, int n)
{
    ErlNifPid pid;    
    return enif_make_pid(mti->dst_env, enif_self(mti->caller_env, &pid));    		
}	

static ERL_NIF_TERM make_term_tuple(struct make_term_info* mti, int n)
{
    ERL_NIF_TERM t[3];
    t[0] = pull_term(mti);
    t[1] = pull_term(mti);
    t[2] = pull_term(mti);
    return enif_make_tuple3(mti->dst_env, t[0], t[1], t[2]);	      
}
static ERL_NIF_TERM make_term_list(struct make_term_info* mti, int n)
{
    ERL_NIF_TERM t[3];
    t[0] = pull_term(mti);
    t[1] = pull_term(mti);
    t[2] = pull_term(mti);
    return enif_make_list3(mti->dst_env, t[0], t[1], t[2]);    
}
static ERL_NIF_TERM make_term_list_cell(struct make_term_info* mti, int n)
{
    ERL_NIF_TERM t[2];
    t[0] = pull_term(mti);
    t[1] = pull_term(mti);
    return enif_make_list_cell(mti->dst_env, t[0], t[1]);
}
static ERL_NIF_TERM make_term_tuple_from_array(struct make_term_info* mti, int n)
{
    ERL_NIF_TERM t[3];
    t[0] = pull_term(mti);
    t[1] = pull_term(mti);
    t[2] = pull_term(mti);
    return enif_make_tuple_from_array(mti->dst_env, t, 3);	
}
static ERL_NIF_TERM make_term_list_from_array(struct make_term_info* mti, int n)
{
    ERL_NIF_TERM t[3];
    t[0] = pull_term(mti);
    t[1] = pull_term(mti);
    t[2] = pull_term(mti);
    return enif_make_list_from_array(mti->dst_env, t, 3);    
}
static ERL_NIF_TERM make_term_garbage(struct make_term_info* mti, int n)
{
    (void) enif_make_string(mti->dst_env, "garbage string", ERL_NIF_LATIN1);
    return pull_term(mti);
}
static ERL_NIF_TERM make_term_copy(struct make_term_info* mti, int n)
{
    return enif_make_copy(mti->dst_env, mti->other_term);
}

typedef ERL_NIF_TERM Make_term_Func(struct make_term_info*, int);
static Make_term_Func* make_funcs[] = {
    make_term_binary,
    make_term_int,
    make_term_ulong,
    make_term_double,
    make_term_atom,
    make_term_existing_atom,
    make_term_string,
    //make_term_ref,
    make_term_sub_binary,
    make_term_uint,
    make_term_long,
    make_term_tuple0,
    make_term_list0,
    make_term_resource,
    make_term_new_binary,
    make_term_caller_pid,
    make_term_tuple,
    make_term_list,
    make_term_list_cell,
    make_term_tuple_from_array,
    make_term_list_from_array,
    make_term_garbage,
    make_term_copy
};
static unsigned num_of_make_funcs()
{
    return sizeof(make_funcs)/sizeof(*make_funcs);
}
static int make_term_n(struct make_term_info* mti, int n, ERL_NIF_TERM* res)
{
    if (n < num_of_make_funcs()) {
	*res = make_funcs[n](mti, n);
	push_term(mti, *res);
	return 1;
    }
    return 0;
}

static ERL_NIF_TERM make_blob(ErlNifEnv* caller_env, ErlNifEnv* dst_env,
			      ERL_NIF_TERM other_term)
{
    PrivData* priv = (PrivData*) enif_priv_data(caller_env);
    ERL_NIF_TERM term, list;
    int n = 0;
    struct make_term_info mti;
    mti.caller_env = caller_env;
    mti.dst_env = dst_env;
    mti.reuse_push = 0;
    mti.reuse_pull = 0;
    mti.resource_type = priv->rt_arr[0].t;
    mti.other_term = other_term;

    list = enif_make_list(dst_env, 0); 
    while (make_term_n(&mti, n++, &term)) {
	list = enif_make_list_cell(dst_env, term, list);
    }
    return list;
}

static ERL_NIF_TERM send_new_blob(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid to;
    ERL_NIF_TERM msg, copy;
    ErlNifEnv* msg_env;
    int res;
    
    if (!enif_get_local_pid(env, argv[0], &to)) {
	return enif_make_badarg(env);
    }
    msg_env = enif_alloc_env();
    msg = make_blob(env,msg_env, argv[1]);
    copy = make_blob(env,env, argv[1]);
    res = enif_send(env, &to, msg_env, msg);
    enif_free_env(msg_env);
    return enif_make_tuple3(env, atom_ok, enif_make_int(env,res), copy);
}

static ERL_NIF_TERM alloc_msgenv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* priv = (PrivData*) enif_priv_data(env);
    struct make_term_info* mti;
    ERL_NIF_TERM ret;

    mti = (struct make_term_info*) enif_alloc_resource(msgenv_resource_type,
						       sizeof(*mti));
    mti->caller_env = NULL;
    mti->dst_env = enif_alloc_env();
    mti->reuse_push = 0;
    mti->reuse_pull = 0;
    mti->resource_type = priv->rt_arr[0].t;
    mti->other_term = enif_make_list(mti->dst_env, 0);
    mti->blob = enif_make_list(mti->dst_env, 0);
    mti->mtx = enif_mutex_create("nif_SUITE:mtx");
    mti->cond = enif_cond_create("nif_SUITE:cond");
    mti->send_res = 0xcafebabe;
    mti->n = 0;
    ret = enif_make_resource(env, mti);
    enif_release_resource(mti);
    return ret;
}

static void msgenv_dtor(ErlNifEnv* env, void* obj)
{
    struct make_term_info* mti = (struct make_term_info*) obj;
    if (mti->dst_env != NULL) {
	enif_free_env(mti->dst_env);
    }
    enif_mutex_destroy(mti->mtx);
    enif_cond_destroy(mti->cond);
}

static ERL_NIF_TERM clear_msgenv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)) {
	return enif_make_badarg(env);
    }
    enif_clear_env(mti.p->dst_env);
    mti.p->reuse_pull = 0;
    mti.p->reuse_push = 0;
    mti.p->blob = enif_make_list(mti.p->dst_env, 0);
    return atom_ok;
}

static ERL_NIF_TERM grow_blob(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    ERL_NIF_TERM term;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)
	|| (argc>2 && !enif_get_uint(env,argv[2], &mti.p->n))) {
	return enif_make_badarg(env);
    }
    mti.p->caller_env = env;
    mti.p->other_term = argv[1];
    mti.p->n %= num_of_make_funcs();
    make_term_n(mti.p, mti.p->n++, &term);
    mti.p->blob = enif_make_list_cell(mti.p->dst_env, term, mti.p->blob);
    return atom_ok;
}

static ERL_NIF_TERM send_blob(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    ErlNifPid to;
    ERL_NIF_TERM copy;
    int res;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)
	|| !enif_get_local_pid(env, argv[1], &to)) {
	return enif_make_badarg(env);
    }
    copy = enif_make_copy(env, mti.p->blob);
    res = enif_send(env, &to, mti.p->dst_env, mti.p->blob);
    return enif_make_tuple3(env, atom_ok, enif_make_int(env,res), copy);
}

static ERL_NIF_TERM send3_blob(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    ErlNifPid to;
    ERL_NIF_TERM copy;
    int res;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)
	|| !enif_get_local_pid(env, argv[1], &to)) {
	return enif_make_badarg(env);
    }
    mti.p->blob = enif_make_tuple2(mti.p->dst_env, 
				   enif_make_copy(mti.p->dst_env, argv[2]),
				   mti.p->blob);
    res = enif_send(env, &to, mti.p->dst_env, mti.p->blob);
    return enif_make_int(env,res);
}

void* threaded_sender(void *arg)
{

    mti_t mti;
    mti.vp = arg;

    enif_mutex_lock(mti.p->mtx);
    while (!mti.p->send_it) {
	enif_cond_wait(mti.p->cond, mti.p->mtx);
    }
    mti.p->send_it = 0;
    enif_mutex_unlock(mti.p->mtx);
    mti.p->send_res = enif_send(NULL, &mti.p->to_pid, mti.p->dst_env, mti.p->blob);
    return NULL;
}

static ERL_NIF_TERM send_blob_thread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    ERL_NIF_TERM copy;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)
	|| !enif_get_local_pid(env,argv[1], &mti.p->to_pid)) {
	return enif_make_badarg(env);
    }
    copy = enif_make_copy(env, mti.p->blob);

    mti.p->send_it = enif_is_identical(argv[2],atom_join);
    if (enif_thread_create("nif_SUITE:send_from_thread", &mti.p->tid,
			   threaded_sender, mti.p, NULL) != 0) {
	return enif_make_badarg(env);
    }
    if (enif_is_identical(argv[2],atom_join)) {
	int err = enif_thread_join(mti.p->tid, NULL);
	assert(err == 0);
	return enif_make_tuple3(env, atom_ok, enif_make_int(env, mti.p->send_res), copy);
    }
    else {
	enif_keep_resource(mti.vp);
	return enif_make_tuple2(env, atom_ok, copy);
    }
}

static ERL_NIF_TERM join_send_thread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    int err;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)) {
	return enif_make_badarg(env);
    }
    enif_mutex_lock(mti.p->mtx);
    mti.p->send_it = 1;
    enif_cond_signal(mti.p->cond);
    enif_mutex_unlock(mti.p->mtx);
    err = enif_thread_join(mti.p->tid, NULL);
    assert(err == 0);
    enif_release_resource(mti.vp);
    return enif_make_tuple2(env, atom_ok, enif_make_int(env, mti.p->send_res));    
}

static ERL_NIF_TERM copy_blob(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)) {
	return enif_make_badarg(env);
    }
    return enif_make_copy(env, mti.p->blob);
}

static ERL_NIF_TERM send_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifEnv* menv;
    ErlNifPid pid;
    int ret;
    if (!enif_get_local_pid(env, argv[0], &pid)) {
	return enif_make_badarg(env);
    }
    menv = enif_alloc_env();
    ret = enif_send(env, &pid, menv, enif_make_copy(menv, argv[1]));
    enif_free_env(menv);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM send_copy_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifEnv* menv;
    ErlNifPid pid;
    int ret;
    if (!enif_get_local_pid(env, argv[0], &pid)) {
	return enif_make_badarg(env);
    }
    ret = enif_send(env, &pid, NULL, argv[1]);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM reverse_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM rev_list;

    if(!enif_make_reverse_list(env, argv[0], &rev_list))
	return enif_make_atom(env, "badarg");
    return rev_list;
}

static ERL_NIF_TERM otp_9668_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* Inspect in process independent env */
    ErlNifEnv* myenv = enif_alloc_env();
    ERL_NIF_TERM mycopy = enif_make_copy(myenv, argv[0]);
    ErlNifBinary obin, cbin;

    if ((enif_inspect_binary(env, argv[0], &obin)
	 && enif_inspect_binary(myenv, mycopy, &cbin))
	||
	(enif_inspect_iolist_as_binary(env, argv[0], &obin)
	 && enif_inspect_iolist_as_binary(myenv, mycopy, &cbin)))
    {
	assert(obin.size == cbin.size);
	assert(memcmp(obin.data, cbin.data, obin.size) == 0);
    }	
    enif_free_env(myenv);
    return atom_ok;
}

static ERL_NIF_TERM otp_9828_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* copy a writable binary could reallocate it due to "emasculation"
       and thereby render a previous inspection invalid.
     */
    ErlNifBinary bin1;
    ErlNifEnv* myenv;

    if (!enif_inspect_binary(env, argv[0], &bin1)) {
	return enif_make_badarg(env);
    }

    myenv = enif_alloc_env();
    enif_make_copy(myenv, argv[0]);
    enif_free_env(myenv);

    return memcmp(bin1.data, "I'm alive!", 10)==0 ? atom_ok : atom_false;
}


static ERL_NIF_TERM consume_timeslice_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int percent;
    char atom[10];

    if (!enif_get_int(env, argv[0], &percent) ||
	!enif_get_atom(env, argv[1], atom, sizeof(atom), ERL_NIF_LATIN1)) {
	return enif_make_badarg(env);
    }
    if (strcmp(atom , "true") == 0) {
	int cnt = 1;
	while (enif_consume_timeslice(env, percent) == 0 && cnt < 200)
	    cnt++;
	return enif_make_int(env, cnt);
    }
    else {
	return enif_make_int(env, enif_consume_timeslice(env, percent));
    }
}

static ERL_NIF_TERM nif_sched2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char s[64];
    if (!enif_get_string(env, argv[2], s, sizeof s, ERL_NIF_LATIN1))
	return enif_make_badarg(env);
    return enif_make_tuple2(env, argv[3], argv[2]);
}

static ERL_NIF_TERM nif_sched1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM new_argv[4];
    new_argv[0] = enif_make_atom(env, "garbage0");
    new_argv[1] = enif_make_atom(env, "garbage1");
    new_argv[2] = argv[0];
    new_argv[3] = argv[1];
    return enif_schedule_nif(env, "nif_sched2", 0, nif_sched2, 4, new_argv);
}

static ERL_NIF_TERM call_nif_schedule(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM result;
    if (argc != 2)
	return enif_make_atom(env, "false");
    result = enif_schedule_nif(env, "nif_sched1", 0, nif_sched1, argc, argv);
    assert(!enif_is_exception(env, result));
    return result;
}

/*
 * If argv[0] is the integer 0, call enif_make_badarg, but don't return its
 * return value. Instead, return ok.  Result should still be a badarg
 * exception for the erlang caller.
 *
 * For any other value of argv[0], use it as an exception term and return
 * the exception.
 */
static ERL_NIF_TERM call_nif_exception(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM exc_term;
    ERL_NIF_TERM badarg_atom = enif_make_atom(env, "badarg");
    int arg;

    if (enif_get_int(env, argv[0], &arg) && arg == 0) {
	/* ignore return value */ enif_make_badarg(env);
	assert(enif_has_pending_exception(env, NULL));
	assert(enif_has_pending_exception(env, &exc_term));
	assert(enif_is_identical(badarg_atom, exc_term));
	return enif_make_atom(env, "ok");
    } else {
	ERL_NIF_TERM exc_retval = enif_raise_exception(env, argv[0]);
	assert(enif_has_pending_exception(env, NULL));
	assert(enif_has_pending_exception(env, &exc_term));
	assert(enif_is_identical(argv[0], exc_term));
	return exc_retval;
    }
}

#if !defined(NAN) || !defined(INFINITY)
double zero(void)
{
    return 0.0;
}
#endif

static ERL_NIF_TERM call_nif_nan_or_inf(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    double val;
    char arg[6];
    ERL_NIF_TERM res;

    assert(argc == 1);
    enif_get_atom(env, argv[0], arg, sizeof arg, ERL_NIF_LATIN1);
    if (strcmp(arg, "nan") == 0) {
        /* Verify that enif_make_double raises a badarg for NaN */
#ifdef NAN
        val = NAN;
#else
        val = 0.0/zero();
#endif
    } else {
        /* Verify that enif_make_double raises a badarg for NaN and infinity */
#ifdef INFINITY
        val = INFINITY;
#else
        val = 1.0/zero();
#endif
    }
    res = enif_make_double(env, val);
    assert(enif_is_exception(env, res));
    assert(enif_has_pending_exception(env, NULL));
    if (strcmp(arg, "tuple") == 0) {
        return enif_make_tuple2(env, argv[0], res);
    } else {
        return res;
    }
}

static ERL_NIF_TERM call_nif_atom_too_long(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char str[257];
    char arg[4];
    size_t len;
    int i;
    ERL_NIF_TERM res;

    assert(argc == 1);
    enif_get_atom(env, argv[0], arg, sizeof arg, ERL_NIF_LATIN1);
    /* Verify that creating an atom from a string that's too long results in a badarg */
    for (i = 0; i < sizeof str; ++i) {
        str[i] = 'a';
    }
    str[256] = '\0';
    if (strcmp(arg, "len") == 0) {
        len = strlen(str);
        res = enif_make_atom_len(env, str, len);
    } else {
        res = enif_make_atom(env, str);
    }
    assert(enif_is_exception(env, res));
    return res;
}

static ERL_NIF_TERM is_map_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, enif_is_map(env,argv[0]));
}
static ERL_NIF_TERM get_map_size_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t size = (size_t)-123;
    int ret = enif_get_map_size(env, argv[0], &size);
    return enif_make_tuple2(env, enif_make_int(env, ret), enif_make_int(env, (int)size));
}
static ERL_NIF_TERM make_new_map_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_new_map(env);
}
static ERL_NIF_TERM make_map_put_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM map_out = enif_make_atom(env, "undefined");
    int ret = enif_make_map_put(env, argv[0], argv[1], argv[2], &map_out);
    return enif_make_tuple2(env, enif_make_int(env,ret), map_out);
}
static ERL_NIF_TERM get_map_value_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM value = enif_make_atom(env, "undefined");
    int ret = enif_get_map_value(env, argv[0], argv[1], &value);
    return enif_make_tuple2(env, enif_make_int(env,ret), value);

}
static ERL_NIF_TERM make_map_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM map_out = enif_make_atom(env, "undefined");
    int ret = enif_make_map_update(env, argv[0], argv[1], argv[2], &map_out);
    return enif_make_tuple2(env, enif_make_int(env,ret), map_out);
}
static ERL_NIF_TERM make_map_remove_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM map_out = enif_make_atom(env, "undefined");
    int ret = enif_make_map_remove(env, argv[0], argv[1], &map_out);
    return enif_make_tuple2(env, enif_make_int(env,ret), map_out);
}

/* maps */
static ERL_NIF_TERM maps_from_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM cell = argv[0];
    ERL_NIF_TERM map  = enif_make_new_map(env);
    ERL_NIF_TERM tuple;
    const ERL_NIF_TERM *pair;
    int arity = -1;

    if (argc != 1 && !enif_is_list(env, cell)) return enif_make_badarg(env);

    /* assume sorted keys */

    while (!enif_is_empty_list(env,cell)) {
	if (!enif_get_list_cell(env, cell, &tuple, &cell)) return enif_make_badarg(env);
	if (enif_get_tuple(env,tuple,&arity,&pair)) {
	    enif_make_map_put(env, map, pair[0], pair[1], &map);
	}
    }

    return map;
}

static ERL_NIF_TERM sorted_list_from_maps_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ERL_NIF_TERM map = argv[0];
    ERL_NIF_TERM list_f = enif_make_list(env, 0); /* NIL */
    ERL_NIF_TERM list_b = enif_make_list(env, 0); /* NIL */
    ERL_NIF_TERM key, value, k2, v2;
    ErlNifMapIterator iter_f;
    ErlNifMapIterator iter_b;
    int cnt, next_ret, prev_ret;

    if (argc != 1 && !enif_is_map(env, map))
	return enif_make_int(env, __LINE__);

    if(!enif_map_iterator_create(env, map, &iter_f, ERL_NIF_MAP_ITERATOR_FIRST))
	return enif_make_int(env, __LINE__);

    cnt = 0;
    next_ret = 1;
    while(enif_map_iterator_get_pair(env,&iter_f,&key,&value)) {
	if (!next_ret)
	    return enif_make_int(env, __LINE__);
	list_f = enif_make_list_cell(env, enif_make_tuple2(env, key, value), list_f);
	next_ret = enif_map_iterator_next(env,&iter_f);
	cnt++;
    }
    if (cnt && next_ret)
	return enif_make_int(env, __LINE__);

    if(!enif_map_iterator_create(env, map, &iter_b, ERL_NIF_MAP_ITERATOR_LAST))
	return enif_make_int(env, __LINE__);

    cnt = 0;
    prev_ret = 1;
    while(enif_map_iterator_get_pair(env,&iter_b,&key,&value)) {
	if (!prev_ret)
	    return enif_make_int(env, __LINE__);

	/* Test that iter_f can step "backwards" */
	if (!enif_map_iterator_prev(env,&iter_f)
	    || !enif_map_iterator_get_pair(env,&iter_f,&k2,&v2)
	    || k2 != key || v2 != value) {
	    return enif_make_int(env, __LINE__);
	}

	list_b = enif_make_list_cell(env, enif_make_tuple2(env, key, value), list_b);
	prev_ret = enif_map_iterator_prev(env,&iter_b);
	cnt++;
    }

    if (cnt) {
	if (prev_ret || enif_map_iterator_prev(env,&iter_f))
	    return enif_make_int(env, __LINE__);

	/* Test that iter_b can step "backwards" one step */
	if (!enif_map_iterator_next(env, &iter_b)
	    || !enif_map_iterator_get_pair(env,&iter_b,&k2,&v2)
	    || k2 != key || v2 != value)
	    return enif_make_int(env, __LINE__);
    }

    enif_map_iterator_destroy(env, &iter_f);
    enif_map_iterator_destroy(env, &iter_b);

    return enif_make_tuple2(env, list_f, list_b);
}


static ERL_NIF_TERM monotonic_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifTimeUnit time_unit;

    if (argc != 1)
	return atom_false;

    if (enif_compare(argv[0], atom_seconds) == 0)
	time_unit = ERL_NIF_SEC;
    else if (enif_compare(argv[0], atom_milli_seconds) == 0)
	time_unit = ERL_NIF_MSEC;
    else if (enif_compare(argv[0], atom_micro_seconds) == 0)
	time_unit = ERL_NIF_USEC;
    else if (enif_compare(argv[0], atom_nano_seconds) == 0)
	time_unit = ERL_NIF_NSEC;
    else
	time_unit = 4711; /* invalid time unit */

    return enif_make_int64(env, enif_monotonic_time(time_unit));
}

static ERL_NIF_TERM time_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifTimeUnit time_unit;

    if (argc != 1)
	return atom_false;

    if (enif_compare(argv[0], atom_seconds) == 0)
	time_unit = ERL_NIF_SEC;
    else if (enif_compare(argv[0], atom_milli_seconds) == 0)
	time_unit = ERL_NIF_MSEC;
    else if (enif_compare(argv[0], atom_micro_seconds) == 0)
	time_unit = ERL_NIF_USEC;
    else if (enif_compare(argv[0], atom_nano_seconds) == 0)
	time_unit = ERL_NIF_NSEC;
    else
	time_unit = 4711; /* invalid time unit */
    return enif_make_int64(env, enif_time_offset(time_unit));
}

static ERL_NIF_TERM convert_time_unit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifSInt64 i64;
    ErlNifTime val;
    ErlNifTimeUnit from, to;

    if (argc != 3)
	return atom_false;

    if (!enif_get_int64(env, argv[0], &i64))
	return enif_make_badarg(env);

    val = (ErlNifTime) i64;

    if (enif_compare(argv[1], atom_seconds) == 0)
	from = ERL_NIF_SEC;
    else if (enif_compare(argv[1], atom_milli_seconds) == 0)
	from = ERL_NIF_MSEC;
    else if (enif_compare(argv[1], atom_micro_seconds) == 0)
	from = ERL_NIF_USEC;
    else if (enif_compare(argv[1], atom_nano_seconds) == 0)
	from = ERL_NIF_NSEC;
    else
	from = 4711; /* invalid time unit */

    if (enif_compare(argv[2], atom_seconds) == 0)
	to = ERL_NIF_SEC;
    else if (enif_compare(argv[2], atom_milli_seconds) == 0)
	to = ERL_NIF_MSEC;
    else if (enif_compare(argv[2], atom_micro_seconds) == 0)
	to = ERL_NIF_USEC;
    else if (enif_compare(argv[2], atom_nano_seconds) == 0)
	to = ERL_NIF_NSEC;
    else
	to = 4711; /* invalid time unit */

    return enif_make_int64(env, enif_convert_time_unit(val, from, to));
}

static ERL_NIF_TERM now_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_now_time(env);
}

static ERL_NIF_TERM cpu_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_cpu_time(env);
}

static ERL_NIF_TERM unique_integer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM atom_pos = enif_make_atom(env,"positive"),
        atom_mon = enif_make_atom(env,"monotonic");
    ERL_NIF_TERM opts = argv[0], opt;
    ErlNifUniqueInteger properties = 0;

    while (!enif_is_empty_list(env, opts)) {
	if (!enif_get_list_cell(env, opts, &opt, &opts))
            return enif_make_badarg(env);

        if (enif_compare(opt, atom_pos) == 0)
            properties |= ERL_NIF_UNIQUE_POSITIVE;
        if (enif_compare(opt, atom_mon) == 0)
            properties |= ERL_NIF_UNIQUE_MONOTONIC;
    }

    return enif_make_unique_integer(env, properties);
}

static ERL_NIF_TERM is_process_alive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid pid;
    if (!enif_get_local_pid(env, argv[0], &pid))
        return enif_make_badarg(env);
    if (enif_is_process_alive(env, &pid))
        return atom_true;
    return atom_false;
}

static ERL_NIF_TERM is_port_alive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPort port;
    if (!enif_get_local_port(env, argv[0], &port))
        return enif_make_badarg(env);
    if (enif_is_port_alive(env, &port))
        return atom_true;
    return atom_false;
}

static ERL_NIF_TERM term_to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    ErlNifPid pid;
    ErlNifEnv *msg_env = env;
    ERL_NIF_TERM term;

    if (enif_get_local_pid(env, argv[1], &pid))
        msg_env = enif_alloc_env();

    if (!enif_term_to_binary(msg_env, argv[0], &bin))
        return enif_make_badarg(env);

    term = enif_make_binary(msg_env, &bin);

    if (msg_env != env) {
        enif_send(env, &pid, msg_env, term);
        enif_free_env(msg_env);
        return atom_true;
    } else {
        return term;
    }
}

static ERL_NIF_TERM binary_to_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    ERL_NIF_TERM term, ret_term;
    ErlNifPid pid;
    ErlNifEnv *msg_env = env;
    unsigned int opts;
    ErlNifUInt64 ret;

    if (enif_get_local_pid(env, argv[1], &pid))
        msg_env = enif_alloc_env();

    if (!enif_inspect_binary(env, argv[0], &bin)
	|| !enif_get_uint(env, argv[2], &opts))
        return enif_make_badarg(env);

    ret = enif_binary_to_term(msg_env, bin.data, bin.size, &term,
			      (ErlNifBinaryToTerm)opts);
    if (!ret)
	return atom_false;

    ret_term = enif_make_uint64(env, ret);
    if (msg_env != env) {
        enif_send(env, &pid, msg_env, term);
        enif_free_env(msg_env);
        return ret_term;
    } else {
        return enif_make_tuple2(env, ret_term, term);
    }
}

static ERL_NIF_TERM port_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPort port;

    if (!enif_get_local_port(env, argv[0], &port))
        return enif_make_badarg(env);

    if (!enif_port_command(env, &port, NULL, argv[1]))
        return enif_make_badarg(env);
    return atom_true;
}

static ERL_NIF_TERM format_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary obin;
    unsigned int size;

    if (!enif_get_uint(env, argv[0], &size))
	return enif_make_badarg(env);
    if (!enif_alloc_binary(size,&obin))
	return enif_make_badarg(env);

    if (enif_snprintf((char*)obin.data, (size_t)size, "%T", argv[1]) < 0)
        return atom_false;

    return enif_make_binary(env,&obin);
}


static ErlNifFunc nif_funcs[] =
{
    {"lib_version", 0, lib_version},
    {"call_history", 0, call_history},
    {"hold_nif_mod_priv_data", 1, hold_nif_mod_priv_data},
    {"nif_mod_call_history", 0, nif_mod_call_history},
    {"list_seq", 1, list_seq},
    {"type_test", 0, type_test},
    {"tuple_2_list", 1, tuple_2_list},
    {"is_identical",2,is_identical},
    {"compare",2,compare},
    {"many_args_100", 100, many_args_100},
    {"clone_bin", 1, clone_bin},
    {"make_sub_bin", 3, make_sub_bin},
    {"string_to_bin", 2, string_to_bin},
    {"atom_to_bin", 2, atom_to_bin},
    {"macros", 1, macros},
    {"tuple_2_list_and_tuple",1,tuple_2_list_and_tuple},
    {"iolist_2_bin", 1, iolist_2_bin},
    {"get_resource_type", 1, get_resource_type},
    {"alloc_resource", 2, alloc_resource},
    {"make_resource", 1, make_resource},
    {"get_resource", 2, get_resource},
    {"release_resource", 1, release_resource},
    {"last_resource_dtor_call", 0, last_resource_dtor_call},
    {"make_new_resource", 2, make_new_resource},
    {"check_is", 11, check_is},
    {"check_is_exception", 0, check_is_exception},
    {"length_test", 6, length_test},
    {"make_atoms", 0, make_atoms},
    {"make_strings", 0, make_strings},
    {"make_new_resource", 2, make_new_resource},
    {"make_new_resource_binary", 1, make_new_resource_binary},
    {"send_list_seq", 2, send_list_seq},
    {"send_new_blob", 2, send_new_blob},
    {"alloc_msgenv", 0, alloc_msgenv},
    {"clear_msgenv", 1, clear_msgenv},
    {"grow_blob", 2, grow_blob},
    {"grow_blob", 3, grow_blob},
    {"send_blob", 2, send_blob},
    {"send3_blob", 3, send3_blob},
    {"send_blob_thread", 3, send_blob_thread},
    {"join_send_thread", 1, join_send_thread},
    {"copy_blob", 1, copy_blob},
    {"send_term", 2, send_term},
    {"send_copy_term", 2, send_copy_term},
    {"reverse_list",1, reverse_list},
    {"echo_int", 1, echo_int},
    {"type_sizes", 0, type_sizes},
    {"otp_9668_nif", 1, otp_9668_nif},
    {"otp_9828_nif", 1, otp_9828_nif},
    {"consume_timeslice_nif", 2, consume_timeslice_nif},
    {"call_nif_schedule", 2, call_nif_schedule},
    {"call_nif_exception", 1, call_nif_exception},
    {"call_nif_nan_or_inf", 1, call_nif_nan_or_inf},
    {"call_nif_atom_too_long", 1, call_nif_atom_too_long},
    {"is_map_nif", 1, is_map_nif},
    {"get_map_size_nif", 1, get_map_size_nif},
    {"make_new_map_nif", 0, make_new_map_nif},
    {"make_map_put_nif", 3, make_map_put_nif},
    {"get_map_value_nif", 2, get_map_value_nif},
    {"make_map_update_nif", 3, make_map_update_nif},
    {"make_map_remove_nif", 2, make_map_remove_nif},
    {"maps_from_list_nif", 1, maps_from_list_nif},
    {"sorted_list_from_maps_nif", 1, sorted_list_from_maps_nif},
    {"monotonic_time", 1, monotonic_time},
    {"time_offset", 1, time_offset},
    {"convert_time_unit", 3, convert_time_unit},
    {"now_time", 0, now_time},
    {"cpu_time", 0, cpu_time},
    {"unique_integer_nif", 1, unique_integer},
    {"is_process_alive_nif", 1, is_process_alive},
    {"is_port_alive_nif", 1, is_port_alive},
    {"term_to_binary_nif", 2, term_to_binary},
    {"binary_to_term_nif", 3, binary_to_term},
    {"port_command_nif", 2, port_command},
    {"format_term_nif", 2, format_term}
};

ERL_NIF_INIT(nif_SUITE,nif_funcs,load,reload,upgrade,unload)
