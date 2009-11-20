#include "erl_nif.h"
#include <string.h>
#include <assert.h>

#include "nif_mod.h"


static int static_cntA; /* zero by default */
static int static_cntB = NIF_LIB_VER * 100;

static void add_call(ErlNifEnv* env, NifModPrivData* data, const char* func_name)
{
    CallInfo* call = enif_alloc(env, sizeof(CallInfo)+strlen(func_name));
    strcpy(call->func_name, func_name);
    call->lib_ver = NIF_LIB_VER;
    call->static_cntA = ++static_cntA;
    call->static_cntB = ++static_cntB;
    call->next = data->call_history;
    data->call_history = call;
}

#define ADD_CALL(FUNC_NAME) add_call(env, enif_get_data(env),FUNC_NAME)

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    NifModPrivData* data = enif_alloc(env, sizeof(NifModPrivData));
    assert(data != NULL);
    data->ref_cnt = 1;
    data->call_history = NULL;
    add_call(env, data, "load");

    data->calls = 0;
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
    NifModPrivData* data = *old_priv_data;
    add_call(env, data, "upgrade");
    data->ref_cnt++;
    *priv_data = *old_priv_data;    
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    NifModPrivData* data = priv_data;
    add_call(env, data, "unload");
    if (--data->ref_cnt == 0) {
	enif_free(env, data);
    }
}

static ERL_NIF_TERM lib_version(ErlNifEnv* env)
{
    ADD_CALL("lib_version");
    return enif_make_int(env, NIF_LIB_VER);
}

static ERL_NIF_TERM call_history(ErlNifEnv* env)
{
    NifModPrivData* data = (NifModPrivData*) enif_get_data(env);
    ERL_NIF_TERM list = enif_make_list(env, 0); /* NIL */

    while (data->call_history != NULL) {
	CallInfo* call = data->call_history;
	ERL_NIF_TERM tpl = enif_make_tuple(env, 2, 
					   enif_make_atom(env,call->func_name),
					   enif_make_int(env,call->lib_ver));
	list = enif_make_list_cell(env, tpl, list);
	data->call_history = call->next;
	enif_free(env,call);
    }
    return list;
}

static ERL_NIF_TERM get_priv_data_ptr(ErlNifEnv* env)
{
    ADD_CALL("get_priv_data_ptr");
    return enif_make_ulong(env, (unsigned long)enif_get_data(env));
}


static ErlNifFunc nif_funcs[] =
{
    {"lib_version", 0, lib_version},
    {"call_history", 0, call_history},
    {"get_priv_data_ptr", 0, get_priv_data_ptr}
};

#if NIF_LIB_VER != 3
ERL_NIF_INIT(nif_mod,nif_funcs,load,reload,upgrade,unload)
#else
ERL_NIF_INIT_GLOB /* avoid link error on windows */
#endif

