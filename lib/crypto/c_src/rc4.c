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

#include "rc4.h"

ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key) */
#ifndef OPENSSL_NO_RC4
    ErlNifBinary key;
    ERL_NIF_TERM ret;
    RC4_KEY *rc4_key;

    CHECK_NO_FIPS_MODE();

    ASSERT(argc == 1);

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key))
        goto bad_arg;
    if (key.size > INT_MAX)
        goto bad_arg;

    if ((rc4_key = (RC4_KEY*)enif_make_new_binary(env, sizeof(RC4_KEY), &ret)) == NULL)
        goto err;

    RC4_set_key(rc4_key, (int)key.size, key.data);
    return ret;

 bad_arg:
 err:
    return enif_make_badarg(env);

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (State, Data) */
#ifndef OPENSSL_NO_RC4
    ErlNifBinary state, data;
    RC4_KEY* rc4_key;
    ERL_NIF_TERM new_state, new_data;
    unsigned char *outp;

    CHECK_NO_FIPS_MODE();

    ASSERT(argc == 2);

    if (!enif_inspect_iolist_as_binary(env, argv[0], &state))
        goto bad_arg;
    if (state.size != sizeof(RC4_KEY))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &data))
        goto bad_arg;

    if ((rc4_key = (RC4_KEY*)enif_make_new_binary(env, sizeof(RC4_KEY), &new_state)) == NULL)
        goto err;
    if ((outp = enif_make_new_binary(env, data.size, &new_data)) == NULL)
        goto err;

    memcpy(rc4_key, state.data, sizeof(RC4_KEY));
    RC4(rc4_key, data.size, data.data, outp);

    CONSUME_REDS(env, data);
    return enif_make_tuple2(env, new_state, new_data);

 bad_arg:
 err:
    return enif_make_badarg(env);

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

