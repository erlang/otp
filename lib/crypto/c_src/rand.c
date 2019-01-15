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

#include "rand.h"
#include "bn.h"

ERL_NIF_TERM strong_rand_bytes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Bytes) */
    unsigned bytes;
    unsigned char* data;
    ERL_NIF_TERM ret;

    ASSERT(argc == 1);

    if (!enif_get_uint(env, argv[0], &bytes))
        goto bad_arg;
    if (bytes > INT_MAX)
        goto bad_arg;

    if ((data = enif_make_new_binary(env, bytes, &ret)) == NULL)
        goto err;
    if (RAND_bytes(data, (int)bytes) != 1)
        goto err;

    ERL_VALGRIND_MAKE_MEM_DEFINED(data, bytes);
    return ret;

 bad_arg:
    return enif_make_badarg(env);

 err:
    return atom_false;
}

ERL_NIF_TERM strong_rand_range_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Range) */
    BIGNUM *bn_range = NULL, *bn_rand = NULL;
    ERL_NIF_TERM ret;

    ASSERT(argc == 1);

    if (!get_bn_from_bin(env, argv[0], &bn_range))
        goto bad_arg;

    if ((bn_rand = BN_new()) == NULL)
        goto err;
    if (!BN_rand_range(bn_rand, bn_range))
        goto err;

    if ((ret = bin_from_bn(env, bn_rand)) == atom_error)
        goto err;
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = atom_false;

 done:
    if (bn_rand)
        BN_free(bn_rand);
    if (bn_range)
        BN_free(bn_range);
    return ret;
}

ERL_NIF_TERM rand_uniform_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Lo,Hi) */
    BIGNUM *bn_from = NULL, *bn_to = NULL, *bn_rand = NULL;
    unsigned char* data;
    int dlen;
    ERL_NIF_TERM ret;

    ASSERT(argc == 2);

    if (!get_bn_from_mpint(env, argv[0], &bn_from))
        goto bad_arg;
    if (!get_bn_from_mpint(env, argv[1], &bn_rand))
        goto bad_arg;

    if ((bn_to = BN_new()) == NULL)
        goto err;

    if (!BN_sub(bn_to, bn_rand, bn_from))
        goto err;
    if (!BN_pseudo_rand_range(bn_rand, bn_to))
        goto err;
    if (!BN_add(bn_rand, bn_rand, bn_from))
        goto err;

    if ((dlen = BN_num_bytes(bn_rand)) < 0)
        goto err;
    if ((data = enif_make_new_binary(env, (size_t)dlen+4, &ret)) == NULL)
        goto err;

    put_uint32(data, (unsigned int)dlen);
    BN_bn2bin(bn_rand, data+4);
    ERL_VALGRIND_MAKE_MEM_DEFINED(data+4, dlen);
    goto done;

 bad_arg:
 err:
    ret = enif_make_badarg(env);

 done:
    if (bn_rand)
        BN_free(bn_rand);
    if (bn_from)
        BN_free(bn_from);
    if (bn_to)
        BN_free(bn_to);
    return ret;
}

ERL_NIF_TERM rand_seed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Seed) */
    ErlNifBinary seed_bin;

    ASSERT(argc == 1);

    if (!enif_inspect_binary(env, argv[0], &seed_bin))
        goto bad_arg;
    if (seed_bin.size > INT_MAX)
        goto bad_arg;

    RAND_seed(seed_bin.data, (int)seed_bin.size);
    return atom_ok;

 bad_arg:
    return enif_make_badarg(env);
}
