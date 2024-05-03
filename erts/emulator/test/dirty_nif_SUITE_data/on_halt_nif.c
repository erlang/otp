/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022. All Rights Reserved.
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
#include <errno.h>
#include <assert.h>
#ifdef __WIN32__
#include <windows.h>
#else
#include <unistd.h>
#endif
#include <stdio.h>
#include <string.h>


static int fn_write_ok(char *filename)
{
    FILE *file = fopen(filename, "w");
    if (!file)
        return EINVAL;
    if (1 != fwrite("ok", 2, 1, file))
        return EINVAL;
    fclose(file);
    return 0;
}

static void on_halt(void *priv_data)
{
    int res;
#ifdef __WIN32__
    Sleep(1000);
#else
    sleep(1);
#endif
    assert(priv_data);
    res = fn_write_ok((char *) priv_data);
    assert(res == 0);
}

static void unload(ErlNifEnv *env, void *priv_data)
{
    if (priv_data)
        enif_free(priv_data);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    unsigned filename_len;
    char *filename;
    if (0 != enif_set_option(env, ERL_NIF_OPT_ON_HALT, on_halt))
        return __LINE__;
    if (!enif_get_list_length(env, load_info, &filename_len))
        return __LINE__;
    if (filename_len == 0)
        return __LINE__;
    filename_len++;
    filename = enif_alloc(filename_len);
    if (!filename)
        return __LINE__;
    if (filename_len != enif_get_string(env,
                                        load_info,
                                        filename,
                                        filename_len,
                                        ERL_NIF_LATIN1)) {
        enif_free(filename);
        return __LINE__;
    }
    *priv_data = (void *) filename;
    return 0;
}

static ERL_NIF_TERM lib_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "true");
}

static ErlNifFunc nif_funcs[] =
{
    {"lib_loaded", 0, lib_loaded}
};
