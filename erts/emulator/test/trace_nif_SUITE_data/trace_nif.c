#include <erl_nif.h>


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
}

static ERL_NIF_TERM is_nif_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env,"true");
}

static ERL_NIF_TERM nif_0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_tuple(env,2,
		    enif_make_atom(env,"ok"),
		    enif_make_list(env,0));
}

static ERL_NIF_TERM nif_1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_tuple(env,2,
		    enif_make_atom(env,"ok"),
		    enif_make_list(env,1,argv[0]));
}



static ErlNifFunc nif_funcs[] =
{
    {"is_nif_loaded", 0, is_nif_loaded},
    {"nif", 0, nif_0},
    {"nif", 1, nif_1}
};

ERL_NIF_INIT(trace_nif_SUITE,nif_funcs,load,NULL,upgrade,unload)

