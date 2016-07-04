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

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"enabled", 3, enabled},
    {"trace", 5, trace}
};

ERL_NIF_INIT(tracer_test, nif_funcs, load, NULL, upgrade, unload)

static ERL_NIF_TERM atom_discard;
static ERL_NIF_TERM atom_ok;

#define ASSERT(expr) assert(expr)

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

    atom_discard = enif_make_atom(env, "discard");
    atom_ok = enif_make_atom(env, "ok");

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

static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int state_arity;
    const ERL_NIF_TERM *state_tuple;
    ERL_NIF_TERM value;
    ASSERT(argc == 3);

    if (!enif_get_tuple(env, argv[1], &state_arity, &state_tuple))
        return atom_discard;

    if (enif_get_map_value(env, state_tuple[0], argv[0], &value)) {
        return value;
    } else {
        return atom_discard;
    }
}

static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int state_arity;
    ErlNifPid self, to;
    ERL_NIF_TERM *tuple, msg;
    const ERL_NIF_TERM *state_tuple;
    ASSERT(argc == 5);

    enif_get_tuple(env, argv[1], &state_arity, &state_tuple);

    tuple = enif_alloc(sizeof(ERL_NIF_TERM)*(argc));
    memcpy(tuple,argv,sizeof(ERL_NIF_TERM)*argc);

    msg = enif_make_tuple_from_array(env, tuple, argc);
    enif_get_local_pid(env, state_tuple[1], &to);
    enif_send(env, &to, NULL, msg);
    enif_free(tuple);

    return atom_ok;
}
