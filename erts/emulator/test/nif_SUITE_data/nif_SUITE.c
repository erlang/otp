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
}

#define ADD_CALL(FUNC_NAME) add_call(env, enif_get_data(env),FUNC_NAME)

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    PrivData* data = enif_alloc(env, sizeof(PrivData));
    assert(data != NULL);
    data->ref_cnt = 1;
    data->call_history = NULL;
    data->nif_mod = NULL;

    add_call(env, data, "load");
    
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
	ERL_NIF_TERM tpl = enif_make_tuple(env, 4, 
					   enif_make_atom(env,call->func_name),
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

    if (data->nif_mod == NULL) {
	return enif_make_string(env,"nif_mod pointer is NULL");
    }
    return make_call_history(env,&data->nif_mod->call_history);
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

static int test_ulong(ErlNifEnv* env, unsigned long i1)
{
    unsigned long i2 = 0;
    ERL_NIF_TERM int_term = enif_make_ulong(env, i1);
    if (!enif_get_ulong(env,int_term, &i2) || i1 != i2) {
	fprintf(stderr, "SVERK: test_ulong(%lu) ...FAILED i2=%lu\r\n", i1, i2);
	return 0;
    }
    return 1;
}

static int test_double(ErlNifEnv* env, double d1)
{
    double d2 = 0;
    ERL_NIF_TERM term = enif_make_double(env, d1);
    if (!enif_get_double(env,term, &d2) || d1 != d2) {
	fprintf(stderr, "SVERK: test_double(%e) ...FAILED i2=%e\r\n", d1, d2);
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
    unsigned long u;
    double d;
    ERL_NIF_TERM atom, ref1, ref2;

    i = INT_MIN;
    do {
	if (!test_int(env,i)) {
	    goto error;
	}
	i += ~i / 3 + 1;
    } while (i < 0);
    i = INT_MAX;
    do {
	if (!test_int(env,i)) {
	    goto error;
	}
	i -= i / 3 + 1;
    } while (i >= 0);

    u = ULONG_MAX;
    for (;;) {
	if (!test_ulong(env,u)) {
	    
	}
	if (u == 0) break;
	u -= u / 3 + 1;
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
    }
    assert((MAX_SMALL < INT_MAX) == (MIN_SMALL > INT_MIN));

    for (u=0 ; u < 10; u++) {
	if (!test_ulong(env,MAX_SMALL+u) || !test_ulong(env,MAX_SMALL-u)) {
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
	fprintf(stderr, "SVERK: nif_SUITE not an atom?\r\n");
	goto error;
    }
    for (i=2; i; i--) {
	if (enif_make_existing_atom(env,"nif_SUITE_pink_unicorn", &atom)) {
	    fprintf(stderr, "SVERK: pink unicorn exist?\r\n");
	    goto error;
	}
    }
    ref1 = enif_make_ref(env);
    ref2 = enif_make_ref(env);
    if (!enif_is_ref(env,ref1) || !enif_is_ref(env,ref2) 
	|| enif_is_identical(env,ref1,ref2) || enif_compare(env,ref1,ref2)==0) {
	fprintf(stderr, "SVERK: strange refs?\r\n");
    }
    return enif_make_atom(env,"ok");

error:
    return enif_make_atom(env,"error");
}

static ERL_NIF_TERM tuple_2_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int arity = -1;
    ERL_NIF_TERM* ptr;
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
    {"compare",2,compare}
};

ERL_NIF_INIT(nif_SUITE,nif_funcs,load,reload,upgrade,unload)

