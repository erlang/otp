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

static ERL_NIF_TERM nif_dec_1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x = 0;
    enif_get_uint(env, argv[0], &x);
    return enif_make_int(env, x - 1);
}



static ErlNifFunc nif_funcs[] =
{
    {"nif_dec", 1, nif_dec_1}
};

ERL_NIF_INIT(trace_call_time_SUITE,nif_funcs,load,NULL,upgrade,unload)
