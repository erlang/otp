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

    if (!enif_get_uint(env, argv[0], &bytes)) {
	return enif_make_badarg(env);
    }
    data = enif_make_new_binary(env, bytes, &ret);
    if ( RAND_bytes(data, bytes) != 1) {
        return atom_false;
    }
    ERL_VALGRIND_MAKE_MEM_DEFINED(data, bytes);
    return ret;
}

ERL_NIF_TERM strong_rand_range_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Range) */
    BIGNUM *bn_range, *bn_rand;
    ERL_NIF_TERM ret;

    if(!get_bn_from_bin(env, argv[0], &bn_range)) {
        return enif_make_badarg(env);
    }

    bn_rand = BN_new();
    if (BN_rand_range(bn_rand, bn_range) != 1) {
        ret = atom_false;
    }
    else {
        ret = bin_from_bn(env, bn_rand);
    }
    BN_free(bn_rand);
    BN_free(bn_range);
    return ret;
}

ERL_NIF_TERM rand_uniform_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Lo,Hi) */
    BIGNUM *bn_from = NULL, *bn_to, *bn_rand;
    unsigned char* data;
    unsigned dlen;
    ERL_NIF_TERM ret;

    if (!get_bn_from_mpint(env, argv[0], &bn_from)
	|| !get_bn_from_mpint(env, argv[1], &bn_rand)) {
	if (bn_from) BN_free(bn_from);
	return enif_make_badarg(env);
    }

    bn_to = BN_new();
    BN_sub(bn_to, bn_rand, bn_from);
    BN_pseudo_rand_range(bn_rand, bn_to);
    BN_add(bn_rand, bn_rand, bn_from);
    dlen = BN_num_bytes(bn_rand);
    data = enif_make_new_binary(env, dlen+4, &ret);
    put_int32(data, dlen);
    BN_bn2bin(bn_rand, data+4);
    ERL_VALGRIND_MAKE_MEM_DEFINED(data+4, dlen);
    BN_free(bn_rand);
    BN_free(bn_from);
    BN_free(bn_to);
    return ret;
}

ERL_NIF_TERM rand_seed_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary seed_bin;

    if (!enif_inspect_binary(env, argv[0], &seed_bin))
        return enif_make_badarg(env);
    RAND_seed(seed_bin.data,seed_bin.size);
    return atom_ok;
}

