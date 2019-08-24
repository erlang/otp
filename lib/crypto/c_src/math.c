/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

#include "math.h"

ERL_NIF_TERM do_exor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Data1, Data2) */
    ErlNifBinary d1, d2;
    unsigned char* ret_ptr;
    size_t i;
    ERL_NIF_TERM ret;

    ASSERT(argc == 2);

    if (!enif_inspect_iolist_as_binary(env, argv[0], &d1))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &d2))
        goto bad_arg;
    if (d1.size != d2.size)
        goto bad_arg;

    if ((ret_ptr = enif_make_new_binary(env, d1.size, &ret)) == NULL)
        goto err;

    for (i=0; i<d1.size; i++) {
	ret_ptr[i] = d1.data[i] ^ d2.data[i];
    }

    CONSUME_REDS(env,d1);
    return ret;

 bad_arg:
 err:
    return enif_make_badarg(env);
}

