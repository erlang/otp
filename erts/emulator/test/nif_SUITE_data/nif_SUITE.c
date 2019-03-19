/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2018. All Rights Reserved.
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
#include <erl_nif.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#ifndef __WIN32__
#include <unistd.h>
#include <fcntl.h>
#include <sys/uio.h>
#endif

#include "nif_mod.h"

#if 0
static ErlNifMutex* dbg_trace_lock;
#define DBG_TRACE_INIT dbg_trace_lock = enif_mutex_create("nif_SUITE.DBG_TRACE")
#define DBG_TRACE_FINI enif_mutex_destroy(dbg_trace_lock)
#define DBG_TRACE_LOCK enif_mutex_lock(dbg_trace_lock)
#define DBG_TRACE_UNLOCK enif_mutex_unlock(dbg_trace_lock)
#define DBG_TRACE0(FMT) do {DBG_TRACE_LOCK; enif_fprintf(stderr, FMT); DBG_TRACE_UNLOCK; }while(0)
#define DBG_TRACE1(FMT, A) do {DBG_TRACE_LOCK; enif_fprintf(stderr, FMT, A); DBG_TRACE_UNLOCK; }while(0)
#define DBG_TRACE2(FMT, A, B) do {DBG_TRACE_LOCK; enif_fprintf(stderr, FMT, A, B); DBG_TRACE_UNLOCK; }while(0)
#define DBG_TRACE3(FMT, A, B, C) do {DBG_TRACE_LOCK; enif_fprintf(stderr, FMT, A, B, C); DBG_TRACE_UNLOCK; }while(0)
#define DBG_TRACE4(FMT, A, B, C, D) do {DBG_TRACE_LOCK; enif_fprintf(stderr, FMT, A, B, C, D); DBG_TRACE_UNLOCK; }while(0)
#else
#define DBG_TRACE_INIT
#define DBG_TRACE_FINI
#define DBG_TRACE0(FMT)
#define DBG_TRACE1(FMT, A)
#define DBG_TRACE2(FMT, A, B)
#define DBG_TRACE3(FMT, A, B, C)
#define DBG_TRACE4(FMT, A, B, C, D)
#endif

/*
 * Hack to get around this function missing from the NIF API.
 * TODO: Add this function/macro in the appropriate place, probably with
 *       enif_make_pid() in erl_nif_api_funcs.h
 */
#ifndef enif_make_port
#define enif_make_port(ENV, PORT) ((void)(ENV),(const ERL_NIF_TERM)((PORT)->port_id))
#endif

static int static_cntA; /* zero by default */
static int static_cntB = NIF_SUITE_LIB_VER * 100;

static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_self;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_join;
static ERL_NIF_TERM atom_binary_resource_type;
static ERL_NIF_TERM atom_second;
static ERL_NIF_TERM atom_millisecond;
static ERL_NIF_TERM atom_microsecond;
static ERL_NIF_TERM atom_nanosecond;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_eof;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_fd_resource_stop;
static ERL_NIF_TERM atom_monitor_resource_type;
static ERL_NIF_TERM atom_monitor_resource_down;
static ERL_NIF_TERM atom_init;
static ERL_NIF_TERM atom_stats;
static ERL_NIF_TERM atom_done;
static ERL_NIF_TERM atom_stop;
static ERL_NIF_TERM atom_null;
static ERL_NIF_TERM atom_pid;
static ERL_NIF_TERM atom_port;
static ERL_NIF_TERM atom_send;
static ERL_NIF_TERM atom_lookup;
static ERL_NIF_TERM atom_badarg;

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

static ErlNifResourceType* fd_resource_type;
static void fd_resource_dtor(ErlNifEnv* env, void* obj);
static void fd_resource_stop(ErlNifEnv* env, void* obj, ErlNifEvent, int);
static ErlNifResourceTypeInit fd_rt_init = {
    fd_resource_dtor,
    fd_resource_stop
};
struct fd_resource {
    ErlNifEvent fd;
    int was_selected;
    ErlNifPid pid;
};

static ErlNifResourceType* monitor_resource_type;
static void monitor_resource_dtor(ErlNifEnv* env, void* obj);
static void monitor_resource_down(ErlNifEnv*, void* obj, ErlNifPid*, ErlNifMonitor*);
static ErlNifResourceTypeInit monitor_rt_init = {
    monitor_resource_dtor,
    NULL,
    monitor_resource_down
};
struct monitor_resource {
    ErlNifPid receiver;
    int use_msgenv;
};

static ErlNifResourceType* frenzy_resource_type;
static void frenzy_resource_dtor(ErlNifEnv* env, void* obj);
static void frenzy_resource_down(ErlNifEnv*, void* obj, ErlNifPid*, ErlNifMonitor*);
static ErlNifResourceTypeInit frenzy_rt_init = {
    frenzy_resource_dtor,
    NULL,
    frenzy_resource_down
};

static ErlNifResourceType* whereis_resource_type;
static void whereis_thread_resource_dtor(ErlNifEnv* env, void* obj);
static ErlNifResourceType* ioq_resource_type;

static void ioq_resource_dtor(ErlNifEnv* env, void* obj);
struct ioq_resource {
    ErlNifIOQueue *q;
};

static int get_pointer(ErlNifEnv* env, ERL_NIF_TERM term, void** pp)
{
    ErlNifBinary bin;
    int r = enif_inspect_binary(env, term, &bin);
    if (r) {
	*pp = *(void**)bin.data;
    }
    return r;
}

static ERL_NIF_TERM make_pointer(ErlNifEnv* env, void* p)
{
    void** bin_data;
    ERL_NIF_TERM res;
    bin_data = (void**)enif_make_new_binary(env, sizeof(void*), &res);
    *bin_data = p;
    return res;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    PrivData* data = enif_alloc(sizeof(PrivData));
    assert(data != NULL);
    data->ref_cnt = 1;
    data->call_history = NULL;
    data->nif_mod = NULL;

    DBG_TRACE_INIT;

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
    fd_resource_type =  enif_open_resource_type_x(env, "nif_SUITE.fd",
                                                  &fd_rt_init,
                                                  ERL_NIF_RT_CREATE, NULL);
    monitor_resource_type = enif_open_resource_type_x(env, "nif_SUITE.monitor",
                                                      &monitor_rt_init,
                                                      ERL_NIF_RT_CREATE, NULL);
    frenzy_resource_type = enif_open_resource_type_x(env, "nif_SUITE.monitor_frenzy",
						      &frenzy_rt_init,
						      ERL_NIF_RT_CREATE, NULL);

    whereis_resource_type = enif_open_resource_type(env, NULL, "nif_SUITE.whereis",
                            whereis_thread_resource_dtor, ERL_NIF_RT_CREATE, NULL);

    ioq_resource_type = enif_open_resource_type(env,NULL,"ioq",
                                                ioq_resource_dtor,
                                                ERL_NIF_RT_CREATE, NULL);

    atom_false = enif_make_atom(env,"false");
    atom_true = enif_make_atom(env,"true");
    atom_self = enif_make_atom(env,"self");
    atom_ok = enif_make_atom(env,"ok");
    atom_join = enif_make_atom(env,"join");
    atom_binary_resource_type = enif_make_atom(env,"binary_resource_type");
    atom_second = enif_make_atom(env,"second");
    atom_millisecond = enif_make_atom(env,"millisecond");
    atom_microsecond = enif_make_atom(env,"microsecond");
    atom_nanosecond = enif_make_atom(env,"nanosecond");
    atom_eagain = enif_make_atom(env, "eagain");
    atom_eof = enif_make_atom(env, "eof");
    atom_error = enif_make_atom(env, "error");
    atom_fd_resource_stop = enif_make_atom(env, "fd_resource_stop");
    atom_monitor_resource_type = enif_make_atom(env, "monitor_resource_type");
    atom_monitor_resource_down = enif_make_atom(env, "monitor_resource_down");
    atom_init = enif_make_atom(env,"init");
    atom_stats = enif_make_atom(env,"stats");
    atom_done = enif_make_atom(env,"done");
    atom_stop = enif_make_atom(env,"stop");
    atom_null = enif_make_atom(env,"null");
    atom_pid = enif_make_atom(env, "pid");
    atom_port = enif_make_atom(env, "port");
    atom_send = enif_make_atom(env, "send");
    atom_lookup = enif_make_atom(env, "lookup");
    atom_badarg = enif_make_atom(env, "badarg");

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
    DBG_TRACE_FINI;
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
    assert(argc == 2);
    return enif_make_atom(env, (enif_is_identical(argv[0],argv[1]) ?
				"true" : "false"));
}

static ERL_NIF_TERM compare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{    
    assert(argc == 2);
    return enif_make_int(env, enif_compare(argv[0],argv[1]));
}

static ERL_NIF_TERM hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifHash type;
    ErlNifUInt64 salt;

    assert(argc == 3);
    if (enif_is_identical(argv[0], enif_make_atom(env, "internal"))) {
        type = ERL_NIF_INTERNAL_HASH;
    }
    else if (enif_is_identical(argv[0], enif_make_atom(env, "phash2"))) {
        type = ERL_NIF_PHASH2;
    }
    else {
        return enif_make_badarg(env);
    }

    if (! enif_get_uint64(env, argv[2], &salt)) {
        return enif_make_badarg(env);
    }

    return enif_make_uint64(env, enif_hash(type, argv[1], salt));
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

static ERL_NIF_TERM last_resource_dtor_call_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    else if (enif_is_identical(argv[0], atom_monitor_resource_type)) {
	type.t = monitor_resource_type;
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

static void* threaded_release_resource(void* resource)
{
    enif_release_resource(resource);
    return NULL;
}

static ERL_NIF_TERM release_resource_from_thread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* resource;
    ErlNifTid tid;
    int err;

    if (!get_pointer(env, argv[0], &resource)) {
        return enif_make_badarg(env);
    }
    if (enif_thread_create("nif_SUITE:release_resource_from_thread", &tid,
                           threaded_release_resource, resource, NULL) != 0) {
        return enif_make_badarg(env);
    }
    err = enif_thread_join(tid, NULL);
    assert(err == 0);
    return atom_ok;
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

/* enif_whereis_... tests */

enum {
    /* results */
    WHEREIS_SUCCESS,
    WHEREIS_ERROR_LOOKUP,
    WHEREIS_ERROR_SEND,
    /* types */
    WHEREIS_LOOKUP_PID,     /* enif_whereis_pid() */
    WHEREIS_LOOKUP_PORT     /* enif_whereis_port() */
};

typedef union {
    ErlNifPid   pid;
    ErlNifPort  port;
} whereis_term_data_t;

/* single use, no cross-thread access/serialization */
typedef struct {
    ErlNifEnv* env;
    ERL_NIF_TERM name;
    whereis_term_data_t res;
    ErlNifTid tid;
    int type;
    int rc;
    ERL_NIF_TERM dtor_msg;
} whereis_thread_resource_t;

static whereis_thread_resource_t* whereis_thread_resource_create(void)
{
    whereis_thread_resource_t* rp = (whereis_thread_resource_t*)
        enif_alloc_resource(whereis_resource_type, sizeof(*rp));
    memset(rp, 0, sizeof(*rp));
    rp->env = enif_alloc_env();

    return rp;
}

static int whereis_lookup_internal(ErlNifEnv*, int type, ERL_NIF_TERM name,
                                   whereis_term_data_t* out);
static int whereis_send_internal(ErlNifEnv*, int type, whereis_term_data_t* to,
                                 ERL_NIF_TERM msg);


static void whereis_thread_resource_dtor(ErlNifEnv* env, void* obj)
{
    whereis_thread_resource_t* rp = (whereis_thread_resource_t*) obj;
    whereis_term_data_t to;

    if (whereis_lookup_internal(env, rp->type, rp->name, &to)
        == WHEREIS_SUCCESS) {
        whereis_send_internal(env, rp->type, &to, rp->dtor_msg);
    }
    enif_free_env(rp->env);
}

static int whereis_type(ERL_NIF_TERM type_term, int* type_p)
{
    if (enif_is_identical(type_term, atom_pid)) {
        *type_p = WHEREIS_LOOKUP_PID;
        return 1;
    }
    if (enif_is_identical(type_term, atom_port)) {
        *type_p = WHEREIS_LOOKUP_PORT;
        return 1;
    }
    return 0;
}

static int whereis_lookup_internal(
    ErlNifEnv* env, int type, ERL_NIF_TERM name, whereis_term_data_t* out)
{
    if (type == WHEREIS_LOOKUP_PID)
        return enif_whereis_pid(env, name, & out->pid)
            ? WHEREIS_SUCCESS : WHEREIS_ERROR_LOOKUP;

    if (type == WHEREIS_LOOKUP_PORT)
        return enif_whereis_port(env, name, & out->port)
            ? WHEREIS_SUCCESS : WHEREIS_ERROR_LOOKUP;

    abort();
}

static int whereis_send_internal(
    ErlNifEnv* env, int type, whereis_term_data_t* to, ERL_NIF_TERM msg)
{
    if (type == WHEREIS_LOOKUP_PID)
        return enif_send(env, & to->pid, NULL, msg)
            ? WHEREIS_SUCCESS : WHEREIS_ERROR_SEND;

    if (type == WHEREIS_LOOKUP_PORT)
        return enif_port_command(env, & to->port, NULL, msg)
            ? WHEREIS_SUCCESS : WHEREIS_ERROR_SEND;

    abort();
}

static ERL_NIF_TERM whereis_resolved_term(
    ErlNifEnv* env, int type, whereis_term_data_t* res)
{
    switch (type) {
        case WHEREIS_LOOKUP_PID:
            return enif_make_pid(env, &res->pid);
        case WHEREIS_LOOKUP_PORT:
            return enif_make_port(env, &res->port);
        default:
            abort();
    }
}

static ERL_NIF_TERM whereis_result_term(ErlNifEnv* env, int result)
{
    ERL_NIF_TERM err;
    switch (result)
    {
        case WHEREIS_SUCCESS:
            return atom_ok;
        case WHEREIS_ERROR_LOOKUP:
            err = atom_lookup;
            break;
        case WHEREIS_ERROR_SEND:
            err = atom_send;
            break;
        default:
            err = enif_make_int(env, -result);
            break;
    }
    return enif_make_tuple2(env, atom_error, err);
}

static void* whereis_lookup_thread(void* arg)
{
    whereis_thread_resource_t* rp = (whereis_thread_resource_t*) arg;

    rp->rc = whereis_lookup_internal(NULL, rp->type, rp->name, &rp->res);

    return NULL;
}

/* whereis_term(Type, Name) -> pid() | port() | false */
static ERL_NIF_TERM
whereis_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    whereis_term_data_t res;
    int type, rc;

    assert(argc == 2);
    if (!whereis_type(argv[0], &type))
        return enif_make_badarg(env);

    rc = whereis_lookup_internal(env, type, argv[1], & res);
    return (rc == WHEREIS_SUCCESS ?
            whereis_resolved_term(env, type, &res) :
            atom_false);
}

/* whereis_send(Type, Name, Message) -> ok | {error, Reason} */
static ERL_NIF_TERM
whereis_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    whereis_term_data_t to;
    int type, rc;

    assert(argc == 3);
    if (!enif_is_atom(env, argv[1]))
        return enif_make_badarg(env);

    if (!whereis_type(argv[0], &type))
        return enif_make_badarg(env);

    rc = whereis_lookup_internal(env, type, argv[1], & to);
    if (rc == WHEREIS_SUCCESS)
        rc = whereis_send_internal(env, type, & to, argv[2]);

    return whereis_result_term(env, rc);
}

/* whereis_thd_lookup(Type, Name, DtorMsg) -> {ok, Resource} | {error, SysErrno} */
static ERL_NIF_TERM
whereis_thd_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    whereis_thread_resource_t* rp;
    int type, rc;
    ERL_NIF_TERM ret;

    assert(argc == 3);
    if (!enif_is_atom(env, argv[1]))
        return enif_make_badarg(env);

    if (!whereis_type(argv[0], &type))
        return enif_make_badarg(env);

    rp = whereis_thread_resource_create();
    rp->type = type;
    rp->name = enif_make_copy(rp->env, argv[1]);
    rp->dtor_msg = enif_make_copy(rp->env, argv[2]);

    rc = enif_thread_create(
        "nif_SUITE:whereis_thd", & rp->tid, whereis_lookup_thread, rp, NULL);

    if (rc == 0)
        ret = enif_make_tuple2(env, atom_ok, enif_make_resource(env, rp));
    else
        ret = enif_make_tuple2(env, atom_error, enif_make_int(env, rc));
    enif_release_resource(rp);
    return ret;
}

/* whereis_thd_result(Resource) -> {ok, pid() | port()} | {error, ErrNum} */
static ERL_NIF_TERM
whereis_thd_result(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    whereis_thread_resource_t* rp;
    ERL_NIF_TERM ret;
    int join_rc;

    assert(argc == 1);
    if (!enif_get_resource(env, argv[0], whereis_resource_type, (void**) & rp))
        return enif_make_badarg(env);

    if ((join_rc = enif_thread_join(rp->tid, NULL)) != 0)
        return enif_make_tuple2(env, atom_error, enif_make_int(env, join_rc));
    
    if (rp->rc == WHEREIS_SUCCESS) {
        ret = enif_make_tuple2(env, atom_ok,
                               whereis_resolved_term(env, rp->type, &rp->res));
    }
    else
        ret = whereis_result_term(env, rp->rc);
    
    return ret;
}

#define MAKE_TERM_REUSE_LEN 16
struct make_term_info
{
    ErlNifEnv* caller_env;
    ErlNifEnv* dst_env;
    int dst_env_valid;
    ERL_NIF_TERM reuse[MAKE_TERM_REUSE_LEN];
    unsigned reuse_push;
    unsigned reuse_pull;
    ErlNifResourceType* resource_type;
    void *resource;
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
            assert(mti->dst_env_valid);
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
    return enif_make_resource(mti->dst_env, mti->resource);
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
        assert(mti->dst_env_valid);
	*res = make_funcs[n](mti, n);
	push_term(mti, *res);
	return 1;
    }
    return 0;
}


static void
init_make_blob(struct make_term_info *mti,
	       ErlNifEnv* caller_env,
	       ERL_NIF_TERM other_term)
{
    PrivData* priv = (PrivData*) enif_priv_data(caller_env);
    mti->caller_env = caller_env;
    mti->resource_type = priv->rt_arr[0].t;
    mti->resource = enif_alloc_resource(mti->resource_type, 10);
    fill(mti->resource, 10, 17); 
    mti->other_term = other_term;
}

static void
fini_make_blob(struct make_term_info *mti)
{
    enif_release_resource(mti->resource);
}

static ERL_NIF_TERM make_blob(struct make_term_info *mti,
			      ErlNifEnv* dst_env)
{
    ERL_NIF_TERM term, list;
    int n = 0;

    mti->reuse_push = 0;
    mti->reuse_pull = 0;
    mti->dst_env = dst_env;
    mti->dst_env_valid = 1;

    list = enif_make_list(dst_env, 0); 
    while (make_term_n(mti, n++, &term)) {
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
    struct make_term_info mti;
    
    if (!enif_get_local_pid(env, argv[0], &to)) {
	return enif_make_badarg(env);
    }
    msg_env = enif_alloc_env();
    init_make_blob(&mti, env, argv[1]);
    msg = make_blob(&mti,msg_env);
    copy = make_blob(&mti,env);
    fini_make_blob(&mti);
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
    mti->dst_env_valid = 1;
    mti->reuse_push = 0;
    mti->reuse_pull = 0;
    mti->resource_type = priv->rt_arr[0].t;
    mti->resource = enif_alloc_resource(mti->resource_type, 10);
    fill(mti->resource, 10, 17); 
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
    enif_release_resource(mti->resource);
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
    mti.p->dst_env_valid = 1;
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
    if (res)
        mti.p->dst_env_valid = 0;
    return enif_make_tuple3(env, atom_ok, enif_make_int(env,res), copy);
}

static ERL_NIF_TERM send3_blob(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    mti_t mti;
    ErlNifPid to;
    int res;
    if (!enif_get_resource(env, argv[0], msgenv_resource_type, &mti.vp)
	|| !enif_get_local_pid(env, argv[1], &to)) {
	return enif_make_badarg(env);
    }
    mti.p->blob = enif_make_tuple2(mti.p->dst_env, 
				   enif_make_copy(mti.p->dst_env, argv[2]),
				   mti.p->blob);
    res = enif_send(env, &to, mti.p->dst_env, mti.p->blob);
    if (res)
        mti.p->dst_env_valid = 0;
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
    if (mti.p->send_res)
        mti.p->dst_env_valid = 0;
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

static int get_pidbin(ErlNifEnv* env, ERL_NIF_TERM pidbin, ErlNifPid* pid)
{
    ErlNifBinary bin;

    if (!enif_inspect_binary(env, pidbin, &bin) || bin.size != sizeof(ErlNifPid))
        return 0;

    memcpy(pid, bin.data, bin.size);
    return 1;
}

static ERL_NIF_TERM send_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifEnv* menv;
    ErlNifPid pid;
    int ret;
    if (!enif_get_local_pid(env, argv[0], &pid) && !get_pidbin(env, argv[0], &pid)) {
	return enif_make_badarg(env);
    }
    menv = enif_alloc_env();
    ret = enif_send(env, &pid, menv, enif_make_copy(menv, argv[1]));
    enif_free_env(menv);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM send_copy_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
    assert(argc == 2);
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
    ERL_NIF_TERM *keys, *values;
    ERL_NIF_TERM result, cell;
    unsigned count;

    assert(argc == 1);
    if (!enif_get_list_length(env, argv[0], &count)) {
        return enif_make_badarg(env);
    }

    keys = enif_alloc(sizeof(ERL_NIF_TERM) * count * 2);
    values = keys + count;

    cell = argv[0];
    count = 0;

    while (!enif_is_empty_list(env, cell)) {
        const ERL_NIF_TERM *pair;
        ERL_NIF_TERM tuple;
        int arity;

        if (!enif_get_list_cell(env, cell, &tuple, &cell)
            || !enif_get_tuple(env, tuple, &arity, &pair)
            || arity != 2) {
            enif_free(keys);
            return enif_make_badarg(env);
        }

        keys[count] = pair[0];
        values[count] = pair[1];

        count++;
    }

    if (!enif_make_map_from_arrays(env, keys, values, count, &result)) {
        result = enif_make_atom(env, "has_duplicate_keys");
    }

    enif_free(keys);

    return result;
}

static ERL_NIF_TERM sorted_list_from_maps_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ERL_NIF_TERM map = argv[0];
    ERL_NIF_TERM list_f = enif_make_list(env, 0); /* NIL */
    ERL_NIF_TERM list_b = enif_make_list(env, 0); /* NIL */
    ERL_NIF_TERM key, value, k2, v2;
    ErlNifMapIterator iter_f;
    ErlNifMapIterator iter_b;
    int cnt, next_ret, prev_ret;

    assert(argc == 1);
    if (!enif_is_map(env, map))
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

    assert(argc == 1);
    if (enif_compare(argv[0], atom_second) == 0)
	time_unit = ERL_NIF_SEC;
    else if (enif_compare(argv[0], atom_millisecond) == 0)
	time_unit = ERL_NIF_MSEC;
    else if (enif_compare(argv[0], atom_microsecond) == 0)
	time_unit = ERL_NIF_USEC;
    else if (enif_compare(argv[0], atom_nanosecond) == 0)
	time_unit = ERL_NIF_NSEC;
    else
	time_unit = 4711; /* invalid time unit */

    return enif_make_int64(env, enif_monotonic_time(time_unit));
}

static ERL_NIF_TERM time_offset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifTimeUnit time_unit;

    assert(argc == 1);
    if (enif_compare(argv[0], atom_second) == 0)
	time_unit = ERL_NIF_SEC;
    else if (enif_compare(argv[0], atom_millisecond) == 0)
	time_unit = ERL_NIF_MSEC;
    else if (enif_compare(argv[0], atom_microsecond) == 0)
	time_unit = ERL_NIF_USEC;
    else if (enif_compare(argv[0], atom_nanosecond) == 0)
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

    assert(argc == 3);
    if (!enif_get_int64(env, argv[0], &i64))
	return enif_make_badarg(env);

    val = (ErlNifTime) i64;

    if (enif_compare(argv[1], atom_second) == 0)
	from = ERL_NIF_SEC;
    else if (enif_compare(argv[1], atom_millisecond) == 0)
	from = ERL_NIF_MSEC;
    else if (enif_compare(argv[1], atom_microsecond) == 0)
	from = ERL_NIF_USEC;
    else if (enif_compare(argv[1], atom_nanosecond) == 0)
	from = ERL_NIF_NSEC;
    else
	from = 4711; /* invalid time unit */

    if (enif_compare(argv[2], atom_second) == 0)
	to = ERL_NIF_SEC;
    else if (enif_compare(argv[2], atom_millisecond) == 0)
	to = ERL_NIF_MSEC;
    else if (enif_compare(argv[2], atom_microsecond) == 0)
	to = ERL_NIF_USEC;
    else if (enif_compare(argv[2], atom_nanosecond) == 0)
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
    ERL_NIF_TERM term, dummy, ret_term;
    ErlNifPid pid;
    ErlNifEnv *msg_env = env;
    unsigned int opts;
    ErlNifUInt64 ret;

    if (enif_get_local_pid(env, argv[1], &pid))
        msg_env = enif_alloc_env();

    if (!enif_inspect_binary(env, argv[0], &bin)
	|| !enif_get_uint(env, argv[2], &opts))
        return enif_make_badarg(env);

    /* build dummy heap term first to provoke OTP-15080 */
    dummy = enif_make_list_cell(msg_env, atom_true, atom_false);

    ret = enif_binary_to_term(msg_env, bin.data, bin.size, &term,
			      (ErlNifBinaryToTerm)opts);
    if (!ret)
	return atom_false;

    ret_term = enif_make_uint64(env, ret);
    if (msg_env != env) {
        enif_send(env, &pid, msg_env,
                  enif_make_tuple2(msg_env, term, dummy));
        enif_free_env(msg_env);
        return ret_term;
    } else {
        return enif_make_tuple3(env, ret_term, term, dummy);
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

static int get_fd(ErlNifEnv* env, ERL_NIF_TERM term, struct fd_resource** rsrc)
{
    if (!enif_get_resource(env, term, fd_resource_type, (void**)rsrc)) {
        return 0;
    }
    return 1;
}

/* Returns: badarg
 *    Or an enif_select result, which is a combination of bits:
 *    ERL_NIF_SELECT_STOP_CALLED = 1
 *    ERL_NIF_SELECT_STOP_SCHEDULED = 2
 *    ERL_NIF_SELECT_INVALID_EVENT = 4
 *    ERL_NIF_SELECT_FAILED = 8
 */
static ERL_NIF_TERM select_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct fd_resource* fdr;
    enum ErlNifSelectFlags mode;
    void* obj;
    ErlNifPid nifpid, *pid = NULL;
    ERL_NIF_TERM ref_or_msg;
    ErlNifEnv* msg_env = NULL;
    int retval;

    if (!get_fd(env, argv[0], &fdr)
        || !enif_get_uint(env, argv[1], (unsigned int*)&mode)
        || !enif_get_resource(env, argv[2], fd_resource_type, &obj))
    {
        return enif_make_badarg(env);
    }

    if (argv[3] != atom_null) {
	if (!enif_get_local_pid(env, argv[3], &nifpid))
	    return enif_make_badarg(env);
	pid = &nifpid;
    }
    ref_or_msg = argv[4];
    if (argv[5] != atom_null) {
        msg_env = enif_alloc_env();
        ref_or_msg = enif_make_copy(msg_env, ref_or_msg);
    }

    fdr->was_selected = 1;
    enif_self(env, &fdr->pid);
    switch (mode) {
    case ERL_NIF_SELECT_CUSTOM_MSG | ERL_NIF_SELECT_READ:
        retval = enif_select_read(env, fdr->fd, obj, pid, ref_or_msg, msg_env);
        break;
    case ERL_NIF_SELECT_CUSTOM_MSG | ERL_NIF_SELECT_WRITE:
        retval = enif_select_write(env, fdr->fd, obj, pid, ref_or_msg, msg_env);
        break;
    default:
        retval = enif_select(env, fdr->fd, mode, obj, pid, ref_or_msg);
    }

    if (msg_env)
        enif_free_env(msg_env);

    return enif_make_int(env, retval);
}

#ifndef __WIN32__
/*
 * Create a read-write pipe with two fds (to read and to write)
 */
static ERL_NIF_TERM pipe_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct fd_resource* read_rsrc;
    struct fd_resource* write_rsrc;
    ERL_NIF_TERM read_fd, write_fd;
    int fds[2], flags;

    if (pipe(fds) < 0)
        return enif_make_string(env, "pipe failed", ERL_NIF_LATIN1);

    if ((flags = fcntl(fds[0], F_GETFL, 0)) < 0
        || fcntl(fds[0], F_SETFL, flags|O_NONBLOCK) < 0
        || (flags = fcntl(fds[1], F_GETFL, 0)) < 0
        || fcntl(fds[1], F_SETFL, flags|O_NONBLOCK) < 0) {
        close(fds[0]);
        close(fds[1]);
        return enif_make_string(env, "fcntl failed on pipe", ERL_NIF_LATIN1);
    }

    read_rsrc  = enif_alloc_resource(fd_resource_type, sizeof(struct fd_resource));
    write_rsrc = enif_alloc_resource(fd_resource_type, sizeof(struct fd_resource));
    read_rsrc->fd  = fds[0];
    read_rsrc->was_selected = 0;
    write_rsrc->fd = fds[1];
    write_rsrc->was_selected = 0;
    read_fd  = enif_make_resource(env, read_rsrc);
    write_fd = enif_make_resource(env, write_rsrc);
    enif_release_resource(read_rsrc);
    enif_release_resource(write_rsrc);

    return enif_make_tuple2(env,
               enif_make_tuple2(env, read_fd, make_pointer(env, read_rsrc)),
               enif_make_tuple2(env, write_fd, make_pointer(env, write_rsrc)));
}

/*
 * Create (dupe) of a resource with the same fd, to test stealing
 */
static ERL_NIF_TERM dupe_resource_nif(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
    struct fd_resource* orig_rsrc;

    if (!get_fd(env, argv[0], &orig_rsrc)) {
        return enif_make_badarg(env);
    } else {
        struct fd_resource* new_rsrc;
        ERL_NIF_TERM new_fd;

        new_rsrc = enif_alloc_resource(fd_resource_type,
                                       sizeof(struct fd_resource));
        new_rsrc->fd = orig_rsrc->fd;
        new_rsrc->was_selected = 0;
        new_fd = enif_make_resource(env, new_rsrc);
        enif_release_resource(new_rsrc);

        return enif_make_tuple2(env, new_fd, make_pointer(env, new_rsrc));
    }
}

static ERL_NIF_TERM write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct fd_resource* fdr;
    ErlNifBinary bin;
    int n, written = 0;

    if (!get_fd(env, argv[0], &fdr)
        || !enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    for (;;) {
        n = write(fdr->fd, bin.data + written, bin.size - written);
        if (n >= 0) {
            written += n;
            if (written == bin.size) {
                return atom_ok;
            }
        }
        else if (errno == EAGAIN) {
            return enif_make_tuple2(env, atom_eagain, enif_make_int(env, written));
        }
        else if (errno == EINTR) {
            continue;
        }
        else {
            return enif_make_tuple2(env, atom_error, enif_make_int(env, errno));
        }
    }
}

static ERL_NIF_TERM read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct fd_resource* fdr;
    unsigned char* buf;
    int n, count;
    ERL_NIF_TERM res;

    if (!get_fd(env, argv[0], &fdr)
        || !enif_get_int(env, argv[1], &count) || count < 1)
        return enif_make_badarg(env);

    buf = enif_make_new_binary(env, count, &res);

    for (;;) {
        n = read(fdr->fd, buf, count);
        if (n > 0) {
            if (n < count) {
                res = enif_make_sub_binary(env, res, 0, n);
            }
            return res;
        }
        else if (n == 0) {
            return atom_eof;
        }
        else if (errno == EAGAIN) {
            return atom_eagain;
        }
        else if (errno == EINTR) {
            continue;
        }
        else {
            return enif_make_tuple2(env, atom_error, enif_make_int(env, errno));
        }
    }
}

static ERL_NIF_TERM is_closed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct fd_resource* fdr;

    if (!get_fd(env, argv[0], &fdr))
        return enif_make_badarg(env);

    return fdr->fd < 0 ? atom_true : atom_false;
}

static ERL_NIF_TERM clear_select_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct fd_resource* fdr = NULL;

    if (!get_fd(env, argv[0], &fdr))
        return enif_make_badarg(env);

    fdr->fd = -1;
    fdr->was_selected = 0;

    return atom_ok;
}

#endif /* !__WIN32__ */


static void fd_resource_dtor(ErlNifEnv* env, void* obj)
{
    struct fd_resource* fdr = (struct fd_resource*)obj;
    resource_dtor(env, obj);
#ifdef __WIN32__
    abort();
#else
    if (fdr->fd >= 0) {
        assert(!fdr->was_selected);
        close(fdr->fd);
    }
#endif
}

static struct {
    void* obj;
    int was_direct_call;
}last_fd_stop;
int fd_stop_cnt = 0;

static void fd_resource_stop(ErlNifEnv* env, void* obj, ErlNifEvent fd,
                             int is_direct_call)
{
    struct fd_resource* fdr = (struct fd_resource*)obj;
    assert(fd == fdr->fd);
    assert(fd >= 0);

    last_fd_stop.obj = obj;
    last_fd_stop.was_direct_call = is_direct_call;
    fd_stop_cnt++;

    close(fd);
    fdr->fd = -1;   /* thread safety ? */
    fdr->was_selected = 0;

    {
        ErlNifEnv* msg_env = enif_alloc_env();
        ERL_NIF_TERM msg;
        msg = enif_make_tuple3(msg_env,
                               atom_fd_resource_stop,
                               make_pointer(msg_env, obj),
                               enif_make_int(msg_env, is_direct_call));

        enif_send(env, &fdr->pid, msg_env, msg);
        enif_free_env(msg_env);
    }
}

static ERL_NIF_TERM last_fd_stop_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM last, ret;
    last = enif_make_tuple2(env, make_pointer(env, last_fd_stop.obj),
                            enif_make_int(env, last_fd_stop.was_direct_call));
    ret = enif_make_tuple2(env, enif_make_int(env, fd_stop_cnt), last);
    fd_stop_cnt = 0;
    return ret;
}


static void monitor_resource_dtor(ErlNifEnv* env, void* obj)
{
    resource_dtor(env, obj);
}

static ERL_NIF_TERM make_monitor(ErlNifEnv* env, const ErlNifMonitor* mon)
{
    ERL_NIF_TERM mon_bin;
    memcpy(enif_make_new_binary(env, sizeof(ErlNifMonitor), &mon_bin),
           mon, sizeof(ErlNifMonitor));
    return mon_bin;
}

static int get_monitor(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifMonitor* mon)
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, term, &bin)
        || bin.size != sizeof(ErlNifMonitor))
        return 0;
    memcpy(mon, bin.data, bin.size);
    return 1;
}

static void monitor_resource_down(ErlNifEnv* env, void* obj, ErlNifPid* pid,
                                  ErlNifMonitor* mon)
{
    struct monitor_resource* rsrc = (struct monitor_resource*)obj;
    ErlNifEnv* build_env;
    ErlNifEnv* msg_env;
    ERL_NIF_TERM msg;

    if (rsrc->use_msgenv) {
        msg_env = enif_alloc_env();
        build_env = msg_env;
    }
    else {
        msg_env = NULL;
        build_env = env;
    }

    msg = enif_make_tuple4(build_env,
                           atom_monitor_resource_down,
                           make_pointer(build_env, obj),
                           enif_make_pid(build_env, pid),
                           make_monitor(build_env, mon));

    enif_send(env, &rsrc->receiver, msg_env, msg);
    if (msg_env)
        enif_free_env(msg_env);
}

static ERL_NIF_TERM alloc_monitor_resource_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct monitor_resource* rsrc;

    rsrc  = enif_alloc_resource(monitor_resource_type, sizeof(struct monitor_resource));

    return make_pointer(env,rsrc);
}

static ERL_NIF_TERM monitor_process_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct monitor_resource* rsrc;
    ErlNifPid target;
    ErlNifMonitor mon;
    int res;

    if (!get_pointer(env, argv[0], (void**)&rsrc)
        || !enif_get_local_pid(env, argv[1], &target)
        || !enif_get_local_pid(env, argv[3], &rsrc->receiver)) {
        return enif_make_badarg(env);
    }

    rsrc->use_msgenv = (argv[2] == atom_true);
    res = enif_monitor_process(env, rsrc, &target, &mon);

    return enif_make_tuple2(env, enif_make_int(env, res), make_monitor(env, &mon));
}

static ERL_NIF_TERM demonitor_process_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct monitor_resource* rsrc;
    ErlNifMonitor mon;
    int res;

    if (!get_pointer(env, argv[0], (void**)&rsrc)
        || !get_monitor(env, argv[1], &mon)) {
        return enif_make_badarg(env);
    }

    res = enif_demonitor_process(env, rsrc, &mon);

    return enif_make_int(env, res);
}

static ERL_NIF_TERM compare_monitors_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifMonitor m1, m2;
    if (!get_monitor(env, argv[0], &m1)
        || !get_monitor(env, argv[1], &m2)) {
        return enif_make_badarg(env);
    }

    return enif_make_int(env, enif_compare_monitors(&m1, &m2));
}

static ERL_NIF_TERM make_monitor_term_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifMonitor m;
    if (!get_monitor(env, argv[0], &m)) {
        return enif_make_badarg(env);
    }

    return enif_make_monitor_term(env, &m);
}


/*********** monitor_frenzy ************/

struct frenzy_rand_bits
{
    unsigned int source;
    unsigned int bits_consumed;
};

static unsigned int frenzy_rand_bits_max;

unsigned rand_bits(struct frenzy_rand_bits* rnd, unsigned int nbits)
{
    unsigned int res;

    rnd->bits_consumed += nbits;
    assert(rnd->bits_consumed <= frenzy_rand_bits_max);
    res = rnd->source & ((1 << nbits)-1);
    rnd->source >>= nbits;
    return res;
}

#define FRENZY_PROCS_MAX_BITS 4
#define FRENZY_PROCS_MAX (1 << FRENZY_PROCS_MAX_BITS)

#define FRENZY_RESOURCES_MAX_BITS 4
#define FRENZY_RESOURCES_MAX (1 << FRENZY_RESOURCES_MAX_BITS)

#define FRENZY_MONITORS_MAX_BITS 4
#define FRENZY_MONITORS_MAX (1 << FRENZY_MONITORS_MAX_BITS)

struct frenzy_monitor {
    ErlNifMutex* lock;
    volatile enum {
        MON_FREE, MON_FREE_DOWN, MON_FREE_DEMONITOR,
        MON_TRYING, MON_ACTIVE, MON_PENDING
    } state;
    ErlNifMonitor mon;
    ErlNifPid pid;
    unsigned int use_cnt;
};

struct frenzy_resource {
    unsigned int rix;
    struct frenzy_monitor monv[FRENZY_MONITORS_MAX];
};
struct frenzy_reslot {
    ErlNifMutex* lock;
    int stopped;
    struct frenzy_resource* obj;
    unsigned long alloc_cnt;
    unsigned long release_cnt;
    unsigned long dtor_cnt;
};
static struct frenzy_reslot resv[FRENZY_RESOURCES_MAX];

static ERL_NIF_TERM monitor_frenzy_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct frenzy_proc {
        ErlNifPid pid;
        int is_free;
    };
    static struct frenzy_proc procs[FRENZY_PROCS_MAX];
    static struct frenzy_proc* proc_refs[FRENZY_PROCS_MAX];
    static unsigned int nprocs, old_nprocs;
    static ErlNifMutex* procs_lock;
    static unsigned long spawn_cnt = 0;
    static unsigned long kill_cnt = 0;
    static unsigned long proc_histogram[FRENZY_PROCS_MAX];
    static int initialized = 0;

    static const unsigned int primes[] = {7, 13, 17, 19};

    struct frenzy_resource* r;
    struct frenzy_rand_bits rnd;
    unsigned int op, inc, my_nprocs;
    unsigned int mix;  /* r->monv[] index */
    unsigned int rix;  /* resv[] index */
    unsigned int pix;  /* procs[] index */
    unsigned int ref_ix; /* proc_refs[] index */
    int self_pix, rv;
    ERL_NIF_TERM retval = atom_error;
    const ERL_NIF_TERM Op = argv[0];
    const ERL_NIF_TERM Rnd = argv[1];
    const ERL_NIF_TERM SelfPix = argv[2];
    const ERL_NIF_TERM NewPid = argv[3];

    if (enif_is_atom(env, Op)) {
        if (Op == atom_init) {
            if (initialized || !enif_get_uint(env, Rnd, &frenzy_rand_bits_max))
                return enif_make_badarg(env);

            procs_lock = enif_mutex_create("nif_SUITE:monitor_frenzy.procs");
            nprocs = 0;
            old_nprocs = 0;
            for (pix = 0; pix < FRENZY_PROCS_MAX; pix++) {
                proc_refs[pix] = &procs[pix];
                procs[pix].is_free = 1;
                proc_histogram[pix] = 0;
            }
            for (rix = 0; rix < FRENZY_RESOURCES_MAX; rix++) {
                resv[rix].lock = enif_mutex_create("nif_SUITE:monitor_frenzy.resv.lock");
                resv[rix].obj = NULL;
                resv[rix].stopped = 0;
                resv[rix].alloc_cnt = 0;
                resv[rix].release_cnt = 0;
                resv[rix].dtor_cnt = 0;
            }

            /* Add self as first process */
            enif_self(env, &procs[0].pid);
            procs[0].is_free = 0;
            old_nprocs = ++nprocs;

            spawn_cnt = 1;
            kill_cnt = 0;
            initialized = 1;
            return enif_make_uint(env, 0);  /* SelfPix */
        }
        else if (Op == atom_stats) {
            ERL_NIF_TERM hist[FRENZY_PROCS_MAX];
            unsigned long res_alloc_cnt = 0;
            unsigned long res_release_cnt = 0;
            unsigned long res_dtor_cnt = 0;
            for (ref_ix = 0; ref_ix < FRENZY_PROCS_MAX; ref_ix++) {
                hist[ref_ix] = enif_make_ulong(env, proc_histogram[ref_ix]);
            }
            for (rix = 0; rix < FRENZY_RESOURCES_MAX; rix++) {
                res_alloc_cnt += resv[rix].alloc_cnt;
                res_release_cnt += resv[rix].release_cnt;
                res_dtor_cnt += resv[rix].dtor_cnt;
            }

            return
            enif_make_list4(env,
                            enif_make_tuple2(env, enif_make_string(env, "proc_histogram", ERL_NIF_LATIN1),
                                             enif_make_list_from_array(env, hist, FRENZY_PROCS_MAX)),
                            enif_make_tuple2(env, enif_make_string(env, "spawn_cnt", ERL_NIF_LATIN1),
                                             enif_make_ulong(env, spawn_cnt)),
                            enif_make_tuple2(env, enif_make_string(env, "kill_cnt", ERL_NIF_LATIN1),
                                             enif_make_ulong(env, kill_cnt)),
                            enif_make_tuple4(env, enif_make_string(env, "resource_alloc", ERL_NIF_LATIN1),
                                             enif_make_ulong(env, res_alloc_cnt),
                                             enif_make_ulong(env, res_release_cnt),
                                             enif_make_ulong(env, res_dtor_cnt)));

        }
        else if (Op == atom_stop && initialized) {  /* stop all */

            /* Release all resources */
            for (rix = 0; rix < FRENZY_RESOURCES_MAX; rix++) {
                enif_mutex_lock(resv[rix].lock);
                r = resv[rix].obj;
                if (r) {
                    resv[rix].obj =  NULL;
                    resv[rix].release_cnt++;
                }
                resv[rix].stopped = 1;
                enif_mutex_unlock(resv[rix].lock);
                if (r)
                    enif_release_resource(r);
            }

            /* Remove and return all pids */
            retval = enif_make_list(env, 0);
            enif_mutex_lock(procs_lock);
            for (ref_ix = 0; ref_ix < nprocs; ref_ix++) {
                assert(!proc_refs[ref_ix]->is_free);
                retval = enif_make_list_cell(env, enif_make_pid(env, &proc_refs[ref_ix]->pid),
                                             retval);
                proc_refs[ref_ix]->is_free = 1;
            }
            kill_cnt += nprocs;
            nprocs = 0;
            old_nprocs = 0;
            enif_mutex_unlock(procs_lock);

            return retval;
        }
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, SelfPix, &self_pix) ||
        !enif_get_uint(env, Op, &op) ||
        !enif_get_uint(env, Rnd, &rnd.source))
        return enif_make_badarg(env);

    rnd.bits_consumed = 0;
    switch (op) {
    case 0:  { /* add/remove process */
        ErlNifPid self;
        enif_self(env, &self);

        ref_ix = rand_bits(&rnd, FRENZY_PROCS_MAX_BITS) % FRENZY_PROCS_MAX;
        enif_mutex_lock(procs_lock);
        if (procs[self_pix].is_free || procs[self_pix].pid.pid != self.pid) {
            /* Some one already removed me */
            enif_mutex_unlock(procs_lock);
            return atom_done;
        }
        if (ref_ix >= nprocs || nprocs < 2) { /* add process */
            ref_ix = nprocs++;
            pix = proc_refs[ref_ix] - procs;
            assert(procs[pix].is_free);
            if (!enif_get_local_pid(env, NewPid, &procs[pix].pid))
                abort();
            procs[pix].is_free = 0;
            spawn_cnt++;
            proc_histogram[ref_ix]++;
            old_nprocs = nprocs;
            enif_mutex_unlock(procs_lock);
            DBG_TRACE2("Add pid %T, nprocs = %u\n", NewPid, nprocs);
            retval = enif_make_uint(env, pix);
        }
        else { /* remove process */
            pix = proc_refs[ref_ix] - procs;
            if (pix == self_pix) {
                ref_ix = (ref_ix + 1) % nprocs;
                pix = proc_refs[ref_ix] - procs;
            }
            assert(procs[pix].pid.pid != self.pid);
            assert(!procs[pix].is_free);
            retval = enif_make_pid(env, &procs[pix].pid);
            --nprocs;
            assert(!proc_refs[nprocs]->is_free);
            if (ref_ix != nprocs) {
                struct frenzy_proc* tmp = proc_refs[ref_ix];
                proc_refs[ref_ix] = proc_refs[nprocs];
                proc_refs[nprocs] = tmp;
            }
            procs[pix].is_free = 1;
            proc_histogram[nprocs]++;
            kill_cnt++;
            enif_mutex_unlock(procs_lock);
            DBG_TRACE2("Removed pid %T, nprocs = %u\n", retval, nprocs);
        }
	break;
    }
    case 1:
    case 2: /* create/delete/lookup resource */
	rix = rand_bits(&rnd, FRENZY_RESOURCES_MAX_BITS) % FRENZY_RESOURCES_MAX;
	inc = primes[rand_bits(&rnd, 2)];
	while (enif_mutex_trylock(resv[rix].lock) == EBUSY) {
	    rix = (rix + inc) % FRENZY_RESOURCES_MAX;
	}
        if (resv[rix].stopped) {
            retval = atom_done;
            enif_mutex_unlock(resv[rix].lock);
            break;
        }
        else if (resv[rix].obj == NULL) {
	    r = enif_alloc_resource(frenzy_resource_type,
				    sizeof(struct frenzy_resource));
	    resv[rix].obj = r;
            resv[rix].alloc_cnt++;
	    r->rix = rix;
            for (mix = 0; mix < FRENZY_MONITORS_MAX; mix++) {
                r->monv[mix].lock = enif_mutex_create("nif_SUITE:monitor_frenzy.monv.lock");
                r->monv[mix].state = MON_FREE;
                r->monv[mix].use_cnt = 0;
                r->monv[mix].pid.pid = 0; /* null-pid */
            }
            DBG_TRACE2("New resource at r=%p rix=%u\n", r, rix);
	}
        else {
            unsigned int resource_op = rand_bits(&rnd, 3);
            r = resv[rix].obj;
            if (resource_op == 0) {      /* delete resource */
                resv[rix].obj = NULL;
                resv[rix].release_cnt++;
                enif_mutex_unlock(resv[rix].lock);
                DBG_TRACE2("Delete resource at r=%p rix=%u\n", r, rix);
                enif_release_resource(r);
                retval = atom_ok;
                break;
            }
            else if (resource_op == 1) {  /* return resource */
                retval = enif_make_resource(env, r);
                enif_mutex_unlock(resv[rix].lock);
                break;
            }
        }
        enif_keep_resource(r);
        enif_mutex_unlock(resv[rix].lock);

        /* monitor/demonitor */

        mix = rand_bits(&rnd, FRENZY_MONITORS_MAX_BITS) % FRENZY_MONITORS_MAX;
        inc = primes[rand_bits(&rnd, 2)];
        while (enif_mutex_trylock(r->monv[mix].lock) == EBUSY) {
            mix = (mix + inc) % FRENZY_MONITORS_MAX;
        }
        switch (r->monv[mix].state) {
        case MON_FREE:
        case MON_FREE_DOWN:
        case MON_FREE_DEMONITOR: {   /* do monitor */
            /*
             * Use an old possibly larger value of 'nprocs', to increase
             * probability of monitoring an already terminated process
             */
            my_nprocs = old_nprocs;
            if (my_nprocs > 0) {
                int save_state = r->monv[mix].state;
                ref_ix = rand_bits(&rnd, FRENZY_PROCS_MAX_BITS) % my_nprocs;
                pix = proc_refs[ref_ix] - procs;
                r->monv[mix].pid.pid = procs[pix].pid.pid; /* "atomic" */
                r->monv[mix].state = MON_TRYING;
                rv = enif_monitor_process(env, r, &r->monv[mix].pid, &r->monv[mix].mon);
                if (rv == 0) {
                    r->monv[mix].state = MON_ACTIVE;
                    r->monv[mix].use_cnt++;
                    DBG_TRACE3("Monitor from r=%p rix=%u to %T\n",
                                 r, r->rix, r->monv[mix].pid.pid);
                }
                else {
                    r->monv[mix].state = save_state;
                    DBG_TRACE4("Monitor from r=%p rix=%u to %T FAILED with %d\n",
                               r, r->rix, r->monv[mix].pid.pid, rv);
                }
                retval = enif_make_int(env,rv);
            }
            else {
                DBG_TRACE0("No pids to monitor\n");
                retval = atom_ok;
            }
            break;
        }
        case MON_ACTIVE: /* do demonitor */
            rv = enif_demonitor_process(env, r, &r->monv[mix].mon);
            if (rv == 0) {
                DBG_TRACE3("Demonitor from r=%p rix=%u to %T\n",
                           r, r->rix, r->monv[mix].pid.pid);
                r->monv[mix].state = MON_FREE_DEMONITOR;
            }
            else {
                DBG_TRACE4("Demonitor from r=%p rix=%u to %T FAILED with %d\n",
                           r, r->rix, r->monv[mix].pid.pid, rv);
                r->monv[mix].state = MON_PENDING;
            }
            retval = enif_make_int(env,rv);
            break;

        case MON_PENDING: /* waiting for 'down' callback, do nothing */
            retval = atom_ok;
            break;
        default:
            abort();
            break;
        }
        enif_mutex_unlock(r->monv[mix].lock);
        enif_release_resource(r);
	break;

    case 3: /* no-op */
        retval = atom_ok;
        break;
    }

    {
        int percent = (rand_bits(&rnd, 6) + 1) * 2;  /* 2 to 128 */
        if (percent <= 100)
            enif_consume_timeslice(env, percent);
    }

    return retval;
}

static void frenzy_resource_dtor(ErlNifEnv* env, void* obj)
{
    struct frenzy_resource* r = (struct frenzy_resource*) obj;
    unsigned int mix;

    DBG_TRACE2("DTOR r=%p rix=%u\n", r, r->rix);

    enif_mutex_lock(resv[r->rix].lock);
    resv[r->rix].dtor_cnt++;
    enif_mutex_unlock(resv[r->rix].lock);

    for (mix = 0; mix < FRENZY_MONITORS_MAX; mix++) {
        assert(r->monv[mix].state != MON_PENDING);
        enif_mutex_destroy(r->monv[mix].lock);
        r->monv[mix].lock = NULL;
    }

}

static void frenzy_resource_down(ErlNifEnv* env, void* obj, ErlNifPid* pid,
                                 ErlNifMonitor* mon)
{
    struct frenzy_resource* r = (struct frenzy_resource*) obj;
    unsigned int mix;

    DBG_TRACE3("DOWN pid=%T, r=%p rix=%u\n", pid->pid, r, r->rix);

    for (mix = 0; mix < FRENZY_MONITORS_MAX; mix++) {
        int state1 = r->monv[mix].state;
        /* First do dirty access of pid and state without the lock */
        if (r->monv[mix].pid.pid == pid->pid && state1 >= MON_TRYING) {
            int state2;
            enif_mutex_lock(r->monv[mix].lock);
            state2 = r->monv[mix].state;
            if (state2 >= MON_ACTIVE) {
                if (enif_compare_monitors(mon, &r->monv[mix].mon) == 0) {
                    r->monv[mix].state = MON_FREE_DOWN;
                    enif_mutex_unlock(r->monv[mix].lock);
                    return;
                }
            }
            else {
                assert(state2 != MON_TRYING);
                assert(state1 == MON_TRYING ||  /* racing monitor failed */
                       state2 == MON_FREE_DEMONITOR || /* racing demonitor */
                       state2 == MON_FREE_DOWN);       /* racing down */
            }
            enif_mutex_unlock(r->monv[mix].lock);
        }
    }
    enif_fprintf(stderr, "DOWN called for unknown monitor\n");
    abort();
}

/*********** testing ioq ************/

static void ioq_resource_dtor(ErlNifEnv* env, void* obj) {

}

#ifndef __WIN32__
static int writeiovec(ErlNifEnv *env, ERL_NIF_TERM term, ERL_NIF_TERM *tail, ErlNifIOQueue *q, int fd) {
    ErlNifIOVec vec, *iovec = &vec;
    SysIOVec *sysiovec;
    int saved_errno;
    int iovcnt, n;

    if (!enif_inspect_iovec(env, 64, term, tail, &iovec))
        return -2;

    if (enif_ioq_size(q) > 0) {
        /* If the I/O queue contains data we enqueue the iovec and then
           peek the data to write out of the queue. */
        if (!enif_ioq_enqv(q, iovec, 0))
            return -3;

        sysiovec = enif_ioq_peek(q, &iovcnt);
    } else {
        /* If the I/O queue is empty we skip the trip through it. */
        iovcnt = iovec->iovcnt;
        sysiovec = iovec->iov;
    }

    /* Attempt to write the data */
    n = writev(fd, (struct iovec*)sysiovec, iovcnt);
    saved_errno = errno;

    if (enif_ioq_size(q) == 0) {
        /* If the I/O queue was initially empty we enqueue any
           remaining data into the queue for writing later. */
        if (n >= 0 && !enif_ioq_enqv(q, iovec, n))
            return -3;
    } else {
        /* Dequeue any data that was written from the queue. */
        if (n > 0 && !enif_ioq_deq(q, n, NULL))
            return -4;
    }

    /* return n, which is either number of bytes written or -1 if
       some error happened */
    errno = saved_errno;
    return n;
}
#endif

static ERL_NIF_TERM ioq(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct ioq_resource *ioq;
    ERL_NIF_TERM ret;
    if (enif_is_identical(argv[0], enif_make_atom(env, "create"))) {
        ErlNifIOQueue *q = enif_ioq_create(ERL_NIF_IOQ_NORMAL);
        ioq = (struct ioq_resource *)enif_alloc_resource(ioq_resource_type,
                                                         sizeof(*ioq));
        ioq->q = q;
        ret = enif_make_resource(env, ioq);
        enif_release_resource(ioq);
        return ret;
    } else if (enif_is_identical(argv[0], enif_make_atom(env, "inspect"))) {
        ErlNifIOVec vec, *iovec = NULL;
        int i, iovcnt;
        ERL_NIF_TERM *elems, tail, list;
        ErlNifEnv *myenv = NULL;

        if (enif_is_identical(argv[2], enif_make_atom(env, "use_stack")))
            iovec = &vec;
        if (enif_is_identical(argv[3], enif_make_atom(env, "use_env")))
            myenv = env;
        if (!enif_inspect_iovec(myenv, ~(size_t)0, argv[1], &tail, &iovec))
            return enif_make_badarg(env);

        iovcnt = iovec->iovcnt;
        elems = enif_alloc(sizeof(ERL_NIF_TERM) * iovcnt);

        for (i = 0; i < iovcnt; i++) {
            ErlNifBinary bin;
            if (!enif_alloc_binary(iovec->iov[i].iov_len, &bin)) {
                enif_free_iovec(iovec);
                enif_free(elems);
                return enif_make_badarg(env);
            }
            memcpy(bin.data, iovec->iov[i].iov_base, iovec->iov[i].iov_len);
            elems[i] = enif_make_binary(env, &bin);
        }

        if (!myenv)
            enif_free_iovec(iovec);

	list = enif_make_list_from_array(env, elems, iovcnt);
	enif_free(elems);
	return list;
    } else {
        unsigned skip;
        if (!enif_get_resource(env, argv[1], ioq_resource_type, (void**)&ioq)
            || !ioq->q)
            return enif_make_badarg(env);

        if (enif_is_identical(argv[0], enif_make_atom(env, "example"))) {
#ifndef __WIN32__
            int fd[2], res = 0, cnt = 0;
            ERL_NIF_TERM tail;
            char buff[255];
            res = pipe(fd);
            assert(res == 0);
            fcntl(fd[0], F_SETFL, fcntl(fd[0], F_GETFL) | O_NONBLOCK);
            fcntl(fd[1], F_SETFL, fcntl(fd[1], F_GETFL) | O_NONBLOCK);

            /* Write until the pipe buffer is full, which should result in data
             * being queued up. */
            for (res = 0; res >= 0; ) {
                cnt += res;
                res = writeiovec(env, argv[2], &tail, ioq->q, fd[1]);
            }

            /* Flush the queue while reading from the other end of the pipe. */
            tail = enif_make_list(env, 0);
            while (enif_ioq_size(ioq->q) > 0) {
                res = writeiovec(env, tail, &tail, ioq->q, fd[1]);

                if (res < 0 && errno != EAGAIN) {
                    break;
                } else if (res > 0) {
                    cnt += res;
                }

                for (res = 0; res >= 0; ) {
                    cnt -= res;
                    res = read(fd[0], buff, sizeof(buff));
                }
            }

            close(fd[0]);
            close(fd[1]);

            /* Check that we read as much as we wrote */
            if (cnt == 0 && enif_ioq_size(ioq->q) == 0)
                return enif_make_atom(env, "true");

            return enif_make_int(env, cnt);
#else
            return enif_make_atom(env, "true");
#endif
        }
        if (enif_is_identical(argv[0], enif_make_atom(env, "destroy"))) {
            enif_ioq_destroy(ioq->q);
            ioq->q = NULL;
            return enif_make_atom(env, "false");
        } else if (enif_is_identical(argv[0], enif_make_atom(env, "enqv"))) {
            ErlNifIOVec vec, *iovec = &vec;
            ERL_NIF_TERM tail;

            if (!enif_get_uint(env, argv[3], &skip))
                return enif_make_badarg(env);
            if (!enif_inspect_iovec(env, ~0ul, argv[2], &tail, &iovec))
                return enif_make_badarg(env);
            if (!enif_ioq_enqv(ioq->q, iovec, skip))
                return enif_make_badarg(env);

            return enif_make_atom(env, "true");
        } else if (enif_is_identical(argv[0], enif_make_atom(env, "enqb"))) {
            ErlNifBinary bin;
            if (!enif_get_uint(env, argv[3], &skip) ||
                !enif_inspect_binary(env, argv[2], &bin))
                return enif_make_badarg(env);

            if (!enif_ioq_enq_binary(ioq->q, &bin, skip))
                return enif_make_badarg(env);

            return enif_make_atom(env, "true");
        } else if (enif_is_identical(argv[0], enif_make_atom(env, "enqbraw"))) {
            ErlNifBinary bin;
            ErlNifBinary localbin;
	    int i;
            if (!enif_get_uint(env, argv[3], &skip) ||
                !enif_inspect_binary(env, argv[2], &bin) ||
                !enif_alloc_binary(bin.size, &localbin))
                return enif_make_badarg(env);

            memcpy(localbin.data, bin.data, bin.size);
            i = enif_ioq_enq_binary(ioq->q, &localbin, skip);
	    if (!i)
		return enif_make_badarg(env);
	    else
		return enif_make_atom(env, "true");
        } else if (enif_is_identical(argv[0], enif_make_atom(env, "peek_head"))) {
            ERL_NIF_TERM head_term;

            if(enif_ioq_peek_head(env, ioq->q, NULL, &head_term)) {
                return enif_make_tuple2(env,
                    enif_make_atom(env, "true"), head_term);
            }

            return enif_make_atom(env, "false");
        } else if (enif_is_identical(argv[0], enif_make_atom(env, "peek"))) {
            int iovlen, num, i, off = 0;
            SysIOVec *iov = enif_ioq_peek(ioq->q, &iovlen);
            ErlNifBinary bin;

            if (!enif_get_int(env, argv[2], &num) || !enif_alloc_binary(num, &bin))
                return enif_make_badarg(env);

            for (i = 0; i < iovlen && num > 0; i++) {
                int to_copy = num < iov[i].iov_len ? num : iov[i].iov_len;
                memcpy(bin.data + off, iov[i].iov_base, to_copy);
                num -= to_copy;
                off += to_copy;
            }

            return enif_make_binary(env, &bin);
        } else if (enif_is_identical(argv[0], enif_make_atom(env, "deq"))) {
            int num;
            size_t sz;
            ErlNifUInt64 sz64;
            if (!enif_get_int(env, argv[2], &num))
                return enif_make_badarg(env);

            if (!enif_ioq_deq(ioq->q, num, &sz))
                return enif_make_badarg(env);

            sz64 = sz;

            return enif_make_uint64(env, sz64);
        } else if (enif_is_identical(argv[0], enif_make_atom(env, "size"))) {
            ErlNifUInt64 size = enif_ioq_size(ioq->q);
            return enif_make_uint64(env, size);
        }
    }

    return enif_make_badarg(env);
}

static ERL_NIF_TERM make_bool(ErlNifEnv* env, int bool)
{
    return bool ? atom_true : atom_false;
}

static ERL_NIF_TERM get_local_pid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid pid;
    ERL_NIF_TERM pid_bin;
    int ret = enif_get_local_pid(env, argv[0], &pid);

    memcpy(enif_make_new_binary(env, sizeof(ErlNifPid), &pid_bin),
           &pid, sizeof(ErlNifPid));

    return enif_make_tuple2(env, make_bool(env, ret), pid_bin);
}

static ERL_NIF_TERM make_pid_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid pid;

    if (!get_pidbin(env, argv[0], &pid))
        return enif_make_badarg(env);

    return enif_make_pid(env, &pid);
}

static ERL_NIF_TERM set_pid_undefined_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid pid;
    ERL_NIF_TERM pid_bin;

    enif_set_pid_undefined(&pid);
    memcpy(enif_make_new_binary(env, sizeof(ErlNifPid), &pid_bin),
           &pid, sizeof(ErlNifPid));

    return pid_bin;
}

static ERL_NIF_TERM is_pid_undefined_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid pid;

    if (!get_pidbin(env, argv[0], &pid))
        return enif_make_badarg(env);

    return make_bool(env, enif_is_pid_undefined(&pid));
}

static ERL_NIF_TERM compare_pids_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid a, b;

    if (!get_pidbin(env, argv[0], &a) || !get_pidbin(env, argv[1], &b))
        return enif_make_badarg(env);

    return enif_make_int(env, enif_compare_pids(&a, &b));
}

static ERL_NIF_TERM term_type_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    switch (enif_term_type(env, argv[0])) {
    case ERL_NIF_TERM_TYPE_ATOM:
        return enif_make_atom(env, "atom");
    case ERL_NIF_TERM_TYPE_BITSTRING:
        return enif_make_atom(env, "bitstring");
    case ERL_NIF_TERM_TYPE_FLOAT:
        return enif_make_atom(env, "float");
    case ERL_NIF_TERM_TYPE_FUN:
        return enif_make_atom(env, "fun");
    case ERL_NIF_TERM_TYPE_INTEGER:
        return enif_make_atom(env, "integer");
    case ERL_NIF_TERM_TYPE_LIST:
        return enif_make_atom(env, "list");
    case ERL_NIF_TERM_TYPE_MAP:
        return enif_make_atom(env, "map");
    case ERL_NIF_TERM_TYPE_PID:
        return enif_make_atom(env, "pid");
    case ERL_NIF_TERM_TYPE_PORT:
        return enif_make_atom(env, "port");
    case ERL_NIF_TERM_TYPE_REFERENCE:
        return enif_make_atom(env, "reference");
    case ERL_NIF_TERM_TYPE_TUPLE:
        return enif_make_atom(env, "tuple");
    default:
        return enif_make_badarg(env);
    }
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
    {"hash_nif",3,hash_nif},
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
    {"release_resource_from_thread", 1, release_resource_from_thread},
    {"last_resource_dtor_call_nif", 0, last_resource_dtor_call_nif},
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
    {"format_term_nif", 2, format_term},
    {"select_nif", 6, select_nif},
#ifndef __WIN32__
    {"pipe_nif", 0, pipe_nif},
    {"write_nif", 2, write_nif},
    {"dupe_resource_nif", 1, dupe_resource_nif},
    {"read_nif", 2, read_nif},
    {"is_closed_nif", 1, is_closed_nif},
    {"clear_select_nif", 1, clear_select_nif},
#endif
    {"last_fd_stop_call", 0, last_fd_stop_call},
    {"alloc_monitor_resource_nif", 0, alloc_monitor_resource_nif},
    {"monitor_process_nif", 4, monitor_process_nif},
    {"demonitor_process_nif", 2, demonitor_process_nif},
    {"compare_monitors_nif", 2, compare_monitors_nif},
    {"make_monitor_term_nif", 1, make_monitor_term_nif},
    {"monitor_frenzy_nif", 4, monitor_frenzy_nif},
    {"whereis_send", 3, whereis_send},
    {"whereis_term", 2, whereis_term},
    {"whereis_thd_lookup", 3, whereis_thd_lookup},
    {"whereis_thd_result", 1, whereis_thd_result},
    {"ioq_nif", 1, ioq},
    {"ioq_nif", 2, ioq},
    {"ioq_nif", 3, ioq},
    {"ioq_nif", 4, ioq},
    {"get_local_pid_nif", 1, get_local_pid_nif},
    {"make_pid_nif", 1, make_pid_nif},
    {"set_pid_undefined_nif", 0, set_pid_undefined_nif},
    {"is_pid_undefined_nif", 1, is_pid_undefined_nif},
    {"compare_pids_nif", 2, compare_pids_nif},
    {"term_type_nif", 1, term_type_nif}
};

ERL_NIF_INIT(nif_SUITE,nif_funcs,load,NULL,upgrade,unload)
