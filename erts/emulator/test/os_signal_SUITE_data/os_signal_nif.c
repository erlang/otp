#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include <erl_nif.h>

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM set_alarm(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int t;
    if (!enif_get_int(env, argv[0], &t)) {
        return enif_make_badarg(env);
    }

    alarm(t);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM fork_0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    pid_t pid;

    pid = fork();

    if (pid == 0) {
        /* child */
        exit(42);
    }

    return enif_make_tuple(env, 2,
            enif_make_atom(env, "ok"),
            enif_make_int(env, (int)pid));
}

static ERL_NIF_TERM get_exit_code(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x;
    pid_t pid;
    if (!enif_get_int(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }

    pid = (pid_t) x;

    waitpid(pid, &x, 0);

    return enif_make_tuple(env, 2,
            enif_make_atom(env, "ok"),
            enif_make_int(env, WEXITSTATUS(x)));
}


static ErlNifFunc nif_funcs[] =
{
    {"set_alarm", 1, set_alarm},
    {"fork", 0, fork_0},
    {"get_exit_code", 1, get_exit_code}
};

ERL_NIF_INIT(os_signal_SUITE,nif_funcs,load,NULL,NULL,NULL)
