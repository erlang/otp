/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2019. All Rights Reserved.
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

#include <stdio.h>


static ErlNifResourceType* resource_type;
static void resource_dtor(ErlNifEnv* env, void* obj);

typedef struct {
    ErlNifPid to;
    ERL_NIF_TERM msg;
    ErlNifEnv* msg_env;
} DtorSender;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    resource_type = enif_open_resource_type(env,NULL,"DtorSender",resource_dtor,
                                            ERL_NIF_RT_CREATE, NULL);
    return 0;
}

static ERL_NIF_TERM is_loaded_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "true");
}

static ERL_NIF_TERM send_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    DtorSender *p;
    ErlNifPid pid;
    ERL_NIF_TERM res;

    p = enif_alloc_resource(resource_type, sizeof(DtorSender));
    
    if (!enif_get_local_pid(env, argv[0], &p->to)) {
        p->msg_env = NULL;
        enif_release_resource(p);
        return enif_make_badarg(env);
    }
    p->msg_env = enif_alloc_env();
    p->msg = enif_make_copy(p->msg_env, argv[1]);
    res = enif_make_resource(env, p);
    enif_release_resource(p);
    return res;
}

static void resource_dtor(ErlNifEnv* env, void* obj)
{
    DtorSender *p = (DtorSender*)obj;

    if (p->msg_env) {
        enif_send(env, &p->to, p->msg_env, p->msg);
        enif_free(p->msg_env);
    }
}


static ErlNifFunc nif_funcs[] =
{
    {"is_loaded", 0, is_loaded_nif},
    {"send", 2, send_nif}
};

ERL_NIF_INIT(erts_test_destructor,nif_funcs,load,NULL,NULL,NULL)
