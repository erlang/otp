/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2024. All Rights Reserved.
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
#include <string.h>

#define MAX_CRYPTOLIB_ERR_SIZE 256
#define SEP ": "

ERL_NIF_TERM raise_exception(ErlNifEnv* env, ERL_NIF_TERM id, int arg_num, char* explanation, char* file, int line)
/* Ex: raise_exception(atom_badarg, 1, "Unknown cipher", "api_ng.c", 17)
 *    -> {badarg, {"api_ng.c",17}, 1, "Unknown cipher"}
 * id = atom_error | atom_notsup | atom_badarg
 * arg_num is the (zero-based) position in argv[]. -1 is to signal that it is undefined.
 */
{
    ERL_NIF_TERM file_info, exception;
    char *error_msg;

#ifdef CRYPTO_DEVELOP_ERRORS
    /* Set CRYPTO_DEVELOP_ERRORS to make error messages more verbose,
       that is, include the error msg from cryptolib.
       Example:
         {error,{"api_ng.c",750},"Can't copy ctx_res"}
       becomes
         {error,{"api_ng.c",750},"Can't copy ctx_res: error:030000BE:digital envelope routines::not able to copy ctx"}
       which enables the developer to locate more in detail where in the cryptolib code a test failed.
    */

    char *p;
    /* Make the error message (concat explanation, ": ", cryptolib error msg) */
    error_msg = enif_alloc(strlen(explanation) + strlen(SEP) + MAX_CRYPTOLIB_ERR_SIZE);
    p = error_msg;

    strcpy(p, explanation);
    p += strlen(explanation);

    strcpy(p, SEP);
    p += strlen(SEP);

    ERR_error_string_n(ERR_peek_last_error(), p, MAX_CRYPTOLIB_ERR_SIZE);
#else
    error_msg = explanation;
#endif

    /* Make the data for exception */
    {
        ERL_NIF_TERM keys[3], vals[3];
        int ok;
        keys[0] = enif_make_atom(env,"c_file_name");
        vals[0] = enif_make_string(env, file, ERL_NIF_LATIN1);
        keys[1] = enif_make_atom(env,"c_file_line_num");
        vals[1] = enif_make_int(env, line);
        keys[2] = enif_make_atom(env,"c_function_arg_num");
        vals[2] = enif_make_int(env, arg_num);
        ok = enif_make_map_from_arrays(env, keys, vals, 3, &file_info);
        ASSERT(ok); (void)ok;
    }
    exception =
        enif_make_tuple3(env,
                         id,
                         file_info,
                         enif_make_string(env, error_msg, (ERL_NIF_LATIN1))
                         );

#ifdef CRYPTO_DEVELOP_ERRORS
    enif_free(error_msg);
#endif

    return enif_raise_exception(env, exception);
}
