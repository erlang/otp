#include <unistd.h>
#include "erl_nif.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    ErlNifSysInfo sys_info;
    enif_system_info(&sys_info, sizeof(ErlNifSysInfo));
    if (!sys_info.smp_support || !sys_info.dirty_scheduler_support)
        return 1;
    return 0;
}

static ERL_NIF_TERM
dirty_sleeper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT
    sleep(3);
#endif
    return enif_make_atom(env, "ok");
}

static ErlNifFunc funcs[] = {
#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT
    {"dirty_sleeper", 0, dirty_sleeper, ERL_NIF_DIRTY_JOB_IO_BOUND}
#else
    {"dirty_sleeper", 0, dirty_sleeper, 0}
#endif
};

ERL_NIF_INIT(scheduler_SUITE, funcs, &load, NULL, NULL, NULL);
