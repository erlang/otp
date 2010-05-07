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

typedef struct
{
    int ref_cnt;
    CallInfo* call_history;
    NifModPrivData* nif_mod;
    union { ErlNifResourceType* t; long l; } rt_arr[2];
}PrivData;

void add_call(ErlNifEnv* env, PrivData* data, const char* func_name)
{
    CallInfo* call = enif_alloc(env, sizeof(CallInfo)+strlen(func_name));
    strcpy(call->func_name, func_name);
    call->lib_ver = NIF_SUITE_LIB_VER;
    call->next = data->call_history;
    call->static_cntA = ++static_cntA;
    call->static_cntB = ++static_cntB;
    data->call_history = call;
    call->arg = NULL;
    call->arg_sz = 0;
}

#define ADD_CALL(FUNC_NAME) add_call(env, enif_get_data(env),FUNC_NAME)

static void*    resource_dtor_last = NULL;
static unsigned resource_dtor_last_sz = 0;
static char     resource_dtor_last_data[20];
static int resource_dtor_cnt = 0;

static void resource_dtor(ErlNifEnv* env, void* obj)
{
    resource_dtor_last = obj;
    resource_dtor_cnt++;
    resource_dtor_last_sz = enif_sizeof_resource(env, obj);
    assert(resource_dtor_last_sz <= sizeof(resource_dtor_last_data));
    memcpy(resource_dtor_last_data, obj, resource_dtor_last_sz);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    /*ERL_NIF_TERM head, tail;*/
    PrivData* data = enif_alloc(env, sizeof(PrivData));
    assert(data != NULL);
    data->ref_cnt = 1;
    data->call_history = NULL;
    data->nif_mod = NULL;

    add_call(env, data, "load");

    /*
    head = load_info;
    data->rt_cnt = 0;
    for (head=load_info; enif_get_list_cell(env,load_info,&head,&tail);
	  head=tail) {
	char buf[20];
	int n = enif_get_string(env,head,buf,sizeof(buf));
	assert(n > 0);
	assert(i < sizeof(data->rt_arr)/sizeof(*data->rt_arr));
	data->rt_arr[data->rt_cnt++].t = enif_create_resource_type(env,buf,resource_dtor,
								   ERL_NIF_RT_CREATE,NULL);	   
    }
    assert(enif_is_empty_list(env,head)); 
    */
    data->rt_arr[0].t = enif_open_resource_type(env,"Gold",resource_dtor,
						ERL_NIF_RT_CREATE,NULL);
    data->rt_arr[1].t = enif_open_resource_type(env,"Silver",resource_dtor,
						ERL_NIF_RT_CREATE,NULL);

    *priv_data = data;
    return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    add_call(env, *priv_data, "reload");
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    PrivData* data = *old_priv_data;
    add_call(env, data, "upgrade");
    data->ref_cnt++;
    *priv_data = *old_priv_data;    
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    PrivData* data = priv_data;
    add_call(env, data, "unload");
    if (--data->ref_cnt == 0) {
	enif_free(env, priv_data);
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
	enif_free(env,call);
    }
    return list;
}

static ERL_NIF_TERM call_history(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* data = (PrivData*) enif_get_data(env);

    return make_call_history(env,&data->call_history);
}

static ERL_NIF_TERM hold_nif_mod_priv_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* data = (PrivData*) enif_get_data(env);
    unsigned long ptr_as_ulong;
    
    if (!enif_get_ulong(env,argv[0],&ptr_as_ulong)) {
	return enif_make_badarg(env);
    }
    if (data->nif_mod != NULL && --(data->nif_mod->ref_cnt) == 0) {
	enif_free(env,data->nif_mod);
    }
    data->nif_mod = (NifModPrivData*) ptr_as_ulong;    
    return enif_make_int(env,++(data->nif_mod->ref_cnt)); 
}

static ERL_NIF_TERM nif_mod_call_history(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    PrivData* data = (PrivData*) enif_get_data(env);
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

    if (!enif_make_existing_atom(env,"nif_SUITE", &atom)
	|| !enif_is_identical(env,atom,enif_make_atom(env,"nif_SUITE"))) {
	fprintf(stderr, "nif_SUITE not an atom?\r\n");
	goto error;
    }
    for (i=2; i; i--) {
	if (enif_make_existing_atom(env,"nif_SUITE_pink_unicorn", &atom)) {
	    fprintf(stderr, "pink unicorn exist?\r\n");
	    goto error;
	}
    }
    ref1 = enif_make_ref(env);
    ref2 = enif_make_ref(env);
    if (!enif_is_ref(env,ref1) || !enif_is_ref(env,ref2) 
	|| enif_is_identical(env,ref1,ref2) || enif_compare(env,ref1,ref2)==0) {
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
    return enif_make_atom(env, (enif_is_identical(env,argv[0],argv[1]) ?
				"true" : "false"));
}

static ERL_NIF_TERM compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{    
    if (argc != 2) {
	return enif_make_badarg(env);
    }
    return enif_make_int(env, enif_compare(env,argv[0],argv[1]));
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
	|| !enif_alloc_binary(env,size,&obin)) {
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
	|| !enif_alloc_binary(env,size,&obin)) {
	return enif_make_badarg(env);
    }
    n = enif_get_atom(env, argv[0], (char*)obin.data, size);
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
    PrivData* data = (PrivData*) enif_get_data(env);
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
	|| (data.p = enif_alloc_resource(env, type.t, data_bin.size))==NULL) {

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
	|| (data = enif_alloc_resource(env, type.t, data_bin.size))==NULL) {

	return enif_make_badarg(env);
    }
    ret = enif_make_resource(env, data);
    memcpy(data, data_bin.data, data_bin.size);
    enif_release_resource(env, data);
    return ret;
}

static ERL_NIF_TERM get_resource(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary data_bin;
    union { ErlNifResourceType* t; long l; } type;
    union { void* p; long l; } data;

    if (!enif_get_long(env, argv[0], &type.l)
	|| !enif_get_resource(env, argv[1], type.t, &data.p)) {
	return enif_make_badarg(env);
    }

    enif_alloc_binary(env, enif_sizeof_resource(env,data.p), &data_bin);    
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
    enif_release_resource(env, data.p);
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
    if (!enif_is_identical(env, argv[3], ok_atom)) return enif_make_badarg(env);
    if (!enif_is_fun(env, argv[4])) return enif_make_badarg(env);
    if (!enif_is_pid(env, argv[5])) return enif_make_badarg(env);
    if (!enif_is_port(env, argv[6])) return enif_make_badarg(env);
    if (!enif_is_empty_list(env, argv[7])) return enif_make_badarg(env);
    if (!enif_is_list(env, argv[7])) return enif_make_badarg(env);
    if (!enif_is_list(env, argv[8])) return enif_make_badarg(env);
    if (!enif_is_tuple(env, argv[9])) return enif_make_badarg(env);

    return ok_atom;
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
    {"check_is", 10, check_is}

};

ERL_NIF_INIT(nif_SUITE,nif_funcs,load,reload,upgrade,unload)

