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
#include "hash_equals.h"

ERL_NIF_TERM hash_equals_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_OPENSSL_CRYPTO_MEMCMP
    ErlNifBinary s1, s2;

    ASSERT(argc == 2);

    if (!enif_inspect_binary(env, argv[0], &s1))
        goto bad_arg;
    if (!enif_inspect_binary(env, argv[1], &s2))
        goto bad_arg;

    if (s1.size != s2.size)
        goto err;

    if (CRYPTO_memcmp(s1.data, s2.data, s1.size) == 0)
        return enif_make_atom(env, "true");

    return enif_make_atom(env, "false");

 bad_arg:
 err:
    return enif_make_badarg(env);
#else
    return EXCP_NOTSUP(env, "Unsupported CRYPTO_memcmp");
#endif
}
