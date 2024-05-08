#ifdef __WIN32__
#include <windows.h>
#endif

#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif

#include <erl_nif.h>

static ERL_NIF_TERM slow_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
/* Sleep for 150 msec */
#ifdef __WIN32__
    Sleep(150);
#else
    usleep(150000);
#endif
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"slow_nif", 0, slow_nif}
};

ERL_NIF_INIT(trace_SUITE,nif_funcs,NULL,NULL,NULL,NULL)
