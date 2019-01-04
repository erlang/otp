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

#include "bn.h"


int get_bn_from_mpint(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp)
{
    BIGNUM *ret;
    ErlNifBinary bin;
    int sz;

    if (!enif_inspect_binary(env, term, &bin))
        goto err;
    if (bin.size > INT_MAX - 4)
        goto err;

    ERL_VALGRIND_ASSERT_MEM_DEFINED(bin.data, bin.size);

    if (bin.size < 4)
        goto err;
    sz = (int)bin.size - 4;
    if (get_int32(bin.data) != sz)
        goto err;

    if ((ret = BN_bin2bn(bin.data+4, sz, NULL)) == NULL)
        goto err;

    *bnp = ret;
    return 1;

 err:
    return 0;
}

int get_bn_from_bin(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp)
{
    BIGNUM *ret;
    ErlNifBinary bin;

    if (!enif_inspect_binary(env, term, &bin))
        goto err;
    if (bin.size > INT_MAX)
        goto err;

    ERL_VALGRIND_ASSERT_MEM_DEFINED(bin.data, bin.size);

    if ((ret = BN_bin2bn(bin.data, (int)bin.size, NULL)) == NULL)
        goto err;

    *bnp = ret;
    return 1;

 err:
    return 0;
}

ERL_NIF_TERM bin_from_bn(ErlNifEnv* env, const BIGNUM *bn)
{
    int bn_len;
    unsigned char *bin_ptr;
    ERL_NIF_TERM term;

    /* Copy the bignum into an erlang binary. */
    bn_len = BN_num_bytes(bn);
    bin_ptr = enif_make_new_binary(env, bn_len, &term);
    BN_bn2bin(bn, bin_ptr);

    return term;
}

ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Base,Exponent,Modulo,bin_hdr) */
    BIGNUM *bn_base=NULL, *bn_exponent=NULL, *bn_modulo=NULL, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    unsigned bin_hdr; /* return type: 0=plain binary, 4: mpint */
    unsigned extra_byte;
    ERL_NIF_TERM ret;

    if (!get_bn_from_bin(env, argv[0], &bn_base)
	|| !get_bn_from_bin(env, argv[1], &bn_exponent)
	|| !get_bn_from_bin(env, argv[2], &bn_modulo)
	|| !enif_get_uint(env,argv[3],&bin_hdr) || (bin_hdr & ~4)) {

	if (bn_base) BN_free(bn_base);
	if (bn_exponent) BN_free(bn_exponent);
	if (bn_modulo) BN_free(bn_modulo);
	return enif_make_badarg(env);
    }
    bn_result = BN_new();
    bn_ctx = BN_CTX_new();
    BN_mod_exp(bn_result, bn_base, bn_exponent, bn_modulo, bn_ctx);
    dlen = BN_num_bytes(bn_result);
    extra_byte = bin_hdr && BN_is_bit_set(bn_result, dlen*8-1);
    ptr = enif_make_new_binary(env, bin_hdr+extra_byte+dlen, &ret);
    if (bin_hdr) {
	put_int32(ptr, extra_byte+dlen);
	ptr[4] = 0; /* extra zeroed byte to ensure a positive mpint */
	ptr += bin_hdr + extra_byte;
    }
    BN_bn2bin(bn_result, ptr);
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);
    BN_free(bn_modulo);
    BN_free(bn_exponent);
    BN_free(bn_base);
    return ret;
}

#ifdef HAVE_EC
ERL_NIF_TERM bn2term(ErlNifEnv* env, const BIGNUM *bn)
{
    unsigned dlen;
    unsigned char* ptr;
    ERL_NIF_TERM ret;

    if (!bn)
	    return atom_undefined;

    dlen = BN_num_bytes(bn);
    ptr = enif_make_new_binary(env, dlen, &ret);
    BN_bn2bin(bn, ptr);
    ERL_VALGRIND_MAKE_MEM_DEFINED(ptr, dlen);
    return ret;
}
#endif
