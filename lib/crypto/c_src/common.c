/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2021. All Rights Reserved.
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

#include "common.h"

ERL_NIF_TERM raise_exception(ErlNifEnv* env, ERL_NIF_TERM id, int arg_num, char* explanation, char* file, int line)
/* Ex: raise_exception(atom_badarg, 1, "Unknown cipher", "api_ng.c", 17)
 *    -> {badarg, {"api_ng.c",17}, 1, "Unknown cipher"}
 * id = atom_error | atom_notsup | atom_badarg
 * arg_num is the (zero-based) position in argv[]. -1 is to signal that it is undefined.
 */
{
    ERL_NIF_TERM file_info, exception;

    file_info = enif_make_new_map(env);
    enif_make_map_put(env, file_info,
                      enif_make_atom(env,"c_file_name"),
                      enif_make_string(env, file, (ERL_NIF_LATIN1)),
                      &file_info);
    enif_make_map_put(env, file_info,
                      enif_make_atom(env,"c_file_line_num"),
                      enif_make_int(env, line),
                      &file_info);
    enif_make_map_put(env, file_info,
                      enif_make_atom(env,"c_function_arg_num"),
                      enif_make_int(env, arg_num),
                      &file_info);
    exception =
        enif_make_tuple3(env,
                         id,
                         file_info,
                         enif_make_string(env, explanation, (ERL_NIF_LATIN1))
                         );

    return enif_raise_exception(env, exception);
}
