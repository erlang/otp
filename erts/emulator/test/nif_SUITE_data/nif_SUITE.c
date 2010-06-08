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
#include "erl_nif.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "nif_mod.h"

static int static_cntA; /* zero by default */
static int static_cntB = NIF_SUITE_LIB_VER * 100;

static ERL_NIF_TERM atom_self;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_join;
static ERL_NIF_TERM atom_binary_resource_type;


typedef struct
{
    int ref_cnt;
    CallInfo* call_history;
    NifModPrivData* nif_mod;
    union { ErlNifResourceType* t; long l; } rt_arr[2];
}PrivData;

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

    atom_self = enif_make_atom(env,"self");
    atom_ok = enif_make_atom(env,"ok");
    atom_join = enif_make_atom(env,"join");
    atom_binary_resource_type = enif_make_atom(env,"binary_resource_type");

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
    unsigned long ptr_as_ulong;
    
    if (!enif_get_ulong(env,argv[0],&ptr_as_ulong)) {
	return enif_make_badarg(env);
    }
    if (data->nif_mod != NULL) {
	NifModPrivData_release(data->nif_mod);
    }
    data->nif_mod = (NifModPrivData*) ptr_as_ulong;    
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
#define MAX_SMALL	((1L << (SMALL_BITS-1))-1)
#define MIN_SMALL	(-(1L << (SMALL_BITS-1)))

static ERL_NIF_TERM type_test(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i;
    int sint;
    unsigned uint;
    long slong;
    unsigned long ulong;
    double d;
    ERL_NIF_TERM atom, ref1, ref2;

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


    uint = UINT_MAX;
    for (;;) {
	if (!test_uint(env,uint)) {
	    
	}
	if (uint == 0) break;
	uint -= uint / 3 + 1;
    }
    ulong = ULONG_MAX;
    for (;;) {
	if (!test_ulong(env,ulong)) {
	    
	}
	if (ulong == 0) break;
	ulong -= ulong / 3 + 1;
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
	    !test_long(env,MIN_SMALL+i)) {
	    goto error;
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
				enif_make_long(env, (long)resource_dtor_last),
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
    return enif_make_long(env, data->rt_arr[ix].l);
}

static ERL_NIF_TERM alloc_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data_bin;
    union { ErlNifResourceType* t; long l;} type;
    union { void* p; long l;} data;
    if (!enif_get_long(env, argv[0], &type.l)
	|| !enif_inspect_binary(env, argv[1], &data_bin)
	|| (data.p = enif_alloc_resource(type.t, data_bin.size))==NULL) {

	return enif_make_badarg(env);
    }
    memcpy(data.p, data_bin.data, data_bin.size);
    return enif_make_long(env, data.l);
}

static ERL_NIF_TERM make_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    union { void* p; long l; } data;
    if (!enif_get_long(env, argv[0], &data.l)) {
	return enif_make_badarg(env);
    }
    return enif_make_resource(env, data.p);
}

static ERL_NIF_TERM make_new_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data_bin;
    union { ErlNifResourceType* t; long l;} type;
    void* data;
    ERL_NIF_TERM ret;
    if (!enif_get_long(env, argv[0], &type.l)
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
    union { struct binary_resource* p; void* vp; long l;} br;
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
    return enif_make_tuple2(env, enif_make_long(env,br.l), ret);
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
    union { ErlNifResourceType* t; long l; } type;
    union { void* p; long l; } data;

    type.t = NULL;
    if (enif_is_identical(argv[0], atom_binary_resource_type)) {
	type.t = binary_resource_type;
    }
    else {
	enif_get_long(env, argv[0], &type.l);
    }
    if (type.t == NULL 
	|| !enif_get_resource(env, argv[1], type.t, &data.p)) {
	return enif_make_badarg(env);
    }
    enif_alloc_binary(enif_sizeof_resource(data.p), &data_bin);    
    memcpy(data_bin.data, data.p, data_bin.size);
    return enif_make_tuple2(env, enif_make_long(env,data.l),
			    enif_make_binary(env, &data_bin));
}

static ERL_NIF_TERM release_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    union { void* p; long l; } data;
    if (!enif_get_long(env, argv[0], &data.l)) {
	return enif_make_badarg(env);
    }
    enif_release_resource(data.p);
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

    return ok_atom;
}

/*
 * argv[0] atom with length of 6
 * argv[1] list with length of 6
 * argv[2] empty list
 * argv[3] not an atom
 * argv[4] not a list
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
    fill(resource, 10, n); 
    return enif_make_resource(mti->dst_env, resource);
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
static int make_term_n(struct make_term_info* mti, int n, ERL_NIF_TERM* res)
{
    typedef ERL_NIF_TERM Make_term_Func(struct make_term_info*, int);
    static Make_term_Func* funcs[] = {
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
    if (n < sizeof(funcs)/sizeof(*funcs)) {
	*res = funcs[n](mti, n);
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
    union {
	void* vp;
	struct make_term_info* p;
    }mti;
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
    union { void* vp; struct make_term_info* p; }mti;
    ERL_NIF_TERM term;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)) {
	return enif_make_badarg(env);
    }
    mti.p->caller_env = env;
    mti.p->other_term = argv[1];
    while (!make_term_n(mti.p, mti.p->n++, &term)) {
	mti.p->n = 0;
    }
    mti.p->blob = enif_make_list_cell(mti.p->dst_env, term, mti.p->blob);
    return atom_ok;
}

static ERL_NIF_TERM send_blob(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    union { void* vp; struct make_term_info* p; }mti;
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

void* threaded_sender(void *arg)
{

    union { void* vp; struct make_term_info* p; }mti;
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
    union { void* vp; struct make_term_info* p; }mti;
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
    union { void* vp; struct make_term_info* p; }mti;
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
    {"check_is", 10, check_is},
    {"length_test", 5, length_test},
    {"make_atoms", 0, make_atoms},
    {"make_strings", 0, make_strings},
    {"make_new_resource", 2, make_new_resource},
    {"make_new_resource_binary", 1, make_new_resource_binary},
    {"send_list_seq", 2, send_list_seq},
    {"send_new_blob", 2, send_new_blob},
    {"alloc_msgenv", 0, alloc_msgenv},
    {"clear_msgenv", 1, clear_msgenv},
    {"grow_blob", 2, grow_blob},
    {"send_blob", 2, send_blob},
    {"send_blob_thread", 3, send_blob_thread},
    {"join_send_thread", 1, join_send_thread}
};

ERL_NIF_INIT(nif_SUITE,nif_funcs,load,reload,upgrade,unload)

