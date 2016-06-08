/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2014. All Rights Reserved.
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
#include "erl_nif.h"
#include <assert.h>
#ifdef __WIN32__
#include <windows.h>
#else
#include <unistd.h>
#endif

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM lib_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "true");
}

static int have_dirty_schedulers(void)
{
    ErlNifSysInfo si;
    enif_system_info(&si, sizeof(si));
    return si.dirty_scheduler_support;
}

static ERL_NIF_TERM dirty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n;
    char s[10];
    ErlNifBinary b;
    if (have_dirty_schedulers()) {
	assert(ERL_NIF_THR_DIRTY_CPU_SCHEDULER == enif_thread_type()
	       || ERL_NIF_THR_DIRTY_IO_SCHEDULER == enif_thread_type());
    }
    assert(argc == 3);
    enif_get_int(env, argv[0], &n);
    enif_get_string(env, argv[1], s, sizeof s, ERL_NIF_LATIN1);
    enif_inspect_binary(env, argv[2], &b);
    return enif_make_tuple3(env,
			    enif_make_int(env, n),
			    enif_make_string(env, s, ERL_NIF_LATIN1),
			    enif_make_binary(env, &b));
}

static ERL_NIF_TERM call_dirty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n;
    char s[10];
    ErlNifBinary b;
    assert(ERL_NIF_THR_NORMAL_SCHEDULER == enif_thread_type());
    if (argc != 3)
	return enif_make_badarg(env);
    if (have_dirty_schedulers()) {
	if (enif_get_int(env, argv[0], &n) &&
	    enif_get_string(env, argv[1], s, sizeof s, ERL_NIF_LATIN1) &&
	    enif_inspect_binary(env, argv[2], &b))
	    return enif_schedule_nif(env, "call_dirty_nif", ERL_NIF_DIRTY_JOB_CPU_BOUND, dirty_nif, argc, argv);
	else
	    return enif_make_badarg(env);
    } else {
	return dirty_nif(env, argc, argv);
    }
}

static ERL_NIF_TERM send_from_dirty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM result;
    ErlNifPid pid;
    ErlNifEnv* menv;
    int res;

    if (!enif_get_local_pid(env, argv[0], &pid))
	return enif_make_badarg(env);
    result = enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_pid(env, &pid));
    menv = enif_alloc_env();
    res = enif_send(env, &pid, menv, result);
    enif_free_env(menv);
    if (!res)
	return enif_make_badarg(env);
    else
	return result;
}

static ERL_NIF_TERM call_dirty_nif_exception(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    switch (argc) {
    case 1: {
	int arg;
	if (enif_get_int(env, argv[0], &arg) && arg < 2) {
	    ERL_NIF_TERM args[255];
	    int i;
	    args[0] = argv[0];
	    for (i = 1; i < 255; i++)
		args[i] = enif_make_int(env, i);
	    return enif_schedule_nif(env, "call_dirty_nif_exception", ERL_NIF_DIRTY_JOB_CPU_BOUND,
				     call_dirty_nif_exception, 255, args);
	} else {
	    return enif_raise_exception(env, argv[0]);
	}
    }
    case 2: {
        int return_badarg_directly;
        enif_get_int(env, argv[0], &return_badarg_directly);
        assert(return_badarg_directly == 1 || return_badarg_directly == 0);
        if (return_badarg_directly)
            return enif_make_badarg(env);
        else {
            /* ignore return value */ enif_make_badarg(env);
            return enif_make_atom(env, "ok");
        }
    }
    default:
	return enif_schedule_nif(env, "call_dirty_nif_exception", ERL_NIF_DIRTY_JOB_CPU_BOUND,
				 call_dirty_nif_exception, argc-1, argv);
    }
}

static ERL_NIF_TERM call_dirty_nif_zero_args(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int i;
    ERL_NIF_TERM result[1000];
    ERL_NIF_TERM ok = enif_make_atom(env, "ok");
    assert(argc == 0);
    for (i = 0; i < sizeof(result)/sizeof(*result); i++) {
	result[i] = ok;
    }
    return enif_make_list_from_array(env, result, i);
}

static ERL_NIF_TERM
dirty_sleeper(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid pid;
    ErlNifEnv* msg_env = NULL;

    assert(ERL_NIF_THR_DIRTY_CPU_SCHEDULER == enif_thread_type()
	   || ERL_NIF_THR_DIRTY_IO_SCHEDULER == enif_thread_type());

    /* If we get a pid argument, it indicates a process involved in the
       test wants a message from us. Prior to the sleep we send a 'ready'
       message, and then after the sleep, send a 'done' message. */
    if (argc == 1 && enif_get_local_pid(env, argv[0], &pid)) {
        msg_env = enif_alloc_env();
        enif_send(env, &pid, msg_env, enif_make_atom(msg_env, "ready"));
    }

#ifdef __WIN32__
    Sleep(6000);
#else
    sleep(6);
#endif

    if (argc == 1) {
        assert(msg_env != NULL);
        enif_send(env, &pid, msg_env, enif_make_atom(msg_env, "done"));
        enif_free_env(msg_env);
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM dirty_call_while_terminated_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid self;
    ERL_NIF_TERM result, self_term;
    ErlNifPid to;
    ErlNifEnv* menv;
    int res;

    if (!enif_get_local_pid(env, argv[0], &to))
	return enif_make_badarg(env);

    if (!enif_self(env, &self))
	return enif_make_badarg(env);

    self_term = enif_make_pid(env, &self);

    result = enif_make_tuple2(env, enif_make_atom(env, "dirty_alive"), self_term);
    menv = enif_alloc_env();
    res = enif_send(env, &to, menv, result);
    enif_free_env(menv);
    if (!res)
	return enif_make_badarg(env);

    /* Wait until we have been killed */
    while (enif_is_process_alive(env, &self))
	;

    result = enif_make_tuple2(env, enif_make_atom(env, "dirty_dead"), self_term);
    menv = enif_alloc_env();
    res = enif_send(env, &to, menv, result);
    enif_free_env(menv);

#ifdef __WIN32__
    Sleep(1000);
#else
    sleep(1);
#endif

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM dirty_heap_access_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res = enif_make_list(env, 0);
    int i;
    assert(ERL_NIF_THR_DIRTY_CPU_SCHEDULER == enif_thread_type()
	   || ERL_NIF_THR_DIRTY_IO_SCHEDULER == enif_thread_type());
    for (i = 0; i < 1000; i++)
	res = enif_make_list_cell(env, enif_make_copy(env, argv[0]), res);

    return res;
}


static ErlNifFunc nif_funcs[] =
{
    {"lib_loaded", 0, lib_loaded},
    {"call_dirty_nif", 3, call_dirty_nif},
    {"send_from_dirty_nif", 1, send_from_dirty_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"call_dirty_nif_exception", 1, call_dirty_nif_exception, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"call_dirty_nif_zero_args", 0, call_dirty_nif_zero_args, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"dirty_sleeper", 0, dirty_sleeper, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"dirty_sleeper", 1, dirty_sleeper, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"dirty_call_while_terminated_nif", 1, dirty_call_while_terminated_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"dirty_heap_access_nif", 1, dirty_heap_access_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(dirty_nif_SUITE,nif_funcs,load,NULL,NULL,NULL)
