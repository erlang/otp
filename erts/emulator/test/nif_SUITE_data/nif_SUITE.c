#include "erl_nif.h"
#include <string.h>
#include <assert.h>

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

static ERL_NIF_TERM lib_version(ErlNifEnv* env)
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

static ERL_NIF_TERM call_history(ErlNifEnv* env)
{
    PrivData* data = (PrivData*) enif_get_data(env);

    return make_call_history(env,&data->call_history);
}

static ERL_NIF_TERM hold_nif_mod_priv_data(ErlNifEnv* env, ERL_NIF_TERM a1)
{
    PrivData* data = (PrivData*) enif_get_data(env);
    unsigned long ptr_as_ulong;
    
    if (!enif_get_ulong(env,a1,&ptr_as_ulong)) {
	return enif_make_badarg(env);
    }
    if (data->nif_mod != NULL && --(data->nif_mod->ref_cnt) == 0) {
	enif_free(env,data->nif_mod);
    }
    data->nif_mod = (NifModPrivData*) ptr_as_ulong;    
    return enif_make_int(env,++(data->nif_mod->ref_cnt)); 
}

static ERL_NIF_TERM nif_mod_call_history(ErlNifEnv* env)
{
    PrivData* data = (PrivData*) enif_get_data(env);

    if (data->nif_mod == NULL) {
	return enif_make_string(env,"nif_mod pointer is NULL");
    }
    return make_call_history(env,&data->nif_mod->call_history);
}

static ERL_NIF_TERM list_seq(ErlNifEnv* env, ERL_NIF_TERM a1)
{
    ERL_NIF_TERM list;
    int n;
    if (!enif_get_int(env, a1, &n)) {
	return enif_make_badarg(env);
    }
    list = enif_make_list(env, 0); /* NIL */
    while (n > 0) {
	list = enif_make_list_cell(env, enif_make_int(env,n), list);
	n--;
    }
    return list;
}

static ErlNifFunc nif_funcs[] =
{
    {"lib_version", 0, lib_version},
    {"call_history", 0, call_history},
    {"hold_nif_mod_priv_data", 1, hold_nif_mod_priv_data},
    {"nif_mod_call_history", 0, nif_mod_call_history},
    {"list_seq", 1, list_seq}
};

ERL_NIF_INIT(nif_SUITE,nif_funcs,load,reload,upgrade,unload)

