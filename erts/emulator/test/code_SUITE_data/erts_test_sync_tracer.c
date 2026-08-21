/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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
#include <erl_internal_test.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

static ERL_NIF_TERM atom_discard;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_trace;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

#define ASSERT(expr) assert(expr)

static ethr_atomic_t the_trace_enabled; // ERL_NIF_TERM
static ethr_atomic_t the_sync; // ERL_NIF_TERM

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

    atom_discard = enif_make_atom(env, "discard");
    atom_ok = enif_make_atom(env, "ok");
    atom_trace = enif_make_atom(env, "trace");
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");

    erts_internal_test.ethr_atomic_init(&the_trace_enabled, atom_discard);
    erts_internal_test.ethr_atomic_init(&the_sync, atom_ok);

    *priv_data = NULL;

    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{

}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
                   ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}

static ERL_NIF_TERM nifs_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "true");
}

static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);

    //enif_fprintf(stderr, "erts_test_sync_tracer.enabled(%T, %T, %T)\n",
    //             argv[0], argv[1], argv[2]);

    return (ERL_NIF_TERM) erts_internal_test.ethr_atomic_read(&the_trace_enabled);
}

static bool term_to_bool(ERL_NIF_TERM term, bool *res)
{
    if (term == atom_true) {
        *res = true;
        return true;
    }
    else if (term == atom_false) {
        *res = false;
        return true;
    }
    else {
        return false;
    }
}

static ERL_NIF_TERM enable_trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    bool is_enabled;
    if (!term_to_bool(argv[0], &is_enabled)) {
        return enif_make_badarg(env);
    }
    erts_internal_test.ethr_atomic_set(&the_trace_enabled,
                                       is_enabled ? atom_trace : atom_discard);
    return atom_ok;
}


static void do_wait_for_sync(ERL_NIF_TERM wait_for)
{
    int wait_ms = 10000;
    ERL_NIF_TERM sync;

    //enif_fprintf(stderr, "Wait for the_sync=%T\n", wait_for);
    while (wait_ms > 0) {
        sync = erts_internal_test.ethr_atomic_read(&the_sync);
        if (sync == wait_for) {
            //enif_fprintf(stderr, "Got the_sync=%T\n", wait_for);
            return;
        }
        erts_internal_test.erts_milli_sleep(1);
        wait_ms--;
        if (wait_ms % 1000 == 0) {
            //enif_fprintf(stderr, "wait_ms = %d, the_sync=%T\n", wait_ms, sync);
        }
    }
    enif_fprintf(stderr, "Gave up waiting for %T, the_sync=%T\n", wait_for, sync);
}

static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const ERL_NIF_TERM tracerState = argv[1];
    ErlNifPid to;

    ASSERT(argc == 5);

    //enif_fprintf(stderr, "erts_test_sync_tracer.trace(%T, %T, %T, %T, %T)\n",
    //             argv[0], argv[1], argv[2], argv[3], argv[4]);

    if (!enif_get_local_pid(env, tracerState, &to)) {
        enif_fprintf(stderr, "erts_test_sync_tracer.trace tracerState is not a pid: %T\n",
                     tracerState);
        return atom_ok;
    }

    // Sending a message does not work as it will probably be enqueued
    // and not sent until we are scheduled out.
    //enif_send(env, &to, NULL, enif_make_tuple2(env, argv[2], argv[0]));

    erts_internal_test.ethr_atomic_set(&the_sync, atom_true);
    do_wait_for_sync(atom_false);

    return atom_ok;
}

static ERL_NIF_TERM set_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }
    //enif_fprintf(stderr, "Set sync to %T\n", argv[0]);
    erts_internal_test.ethr_atomic_set(&the_sync, argv[0]);
    return atom_ok;
}

static ERL_NIF_TERM get_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM sync = erts_internal_test.ethr_atomic_read(&the_sync);
    ASSERT(enif_is_atom(env, sync));
    return sync;
}

static ErlNifFunc nif_funcs[] = {
    {"enabled", 3, enabled},
    {"trace", 5, trace},
    {"nifs_loaded", 0, nifs_loaded},
    {"set_sync", 1, set_sync},
    {"get_sync", 0, get_sync},
    {"enable_trace", 1, enable_trace}
};

ERL_NIF_INIT(erts_test_sync_tracer, nif_funcs, load, NULL, upgrade, unload)
