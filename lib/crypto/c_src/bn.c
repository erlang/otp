/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2023. All Rights Reserved.
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
    return get_bn_from_bin_sz(env, term, bnp, NULL);
}

int get_bn_from_bin_sz(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp, size_t* binsize)
{
    BIGNUM *ret;
    ErlNifBinary bin;

    if (!enif_inspect_binary(env, term, &bin))
        goto err;
    if (bin.size > INT_MAX)
        goto err;

    if ((ret = BN_bin2bn(bin.data, (int)bin.size, NULL)) == NULL)
        goto err;

    if (binsize != NULL)
        *binsize = bin.size;
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
    if ((bn_len = BN_num_bytes(bn)) < 0)
        goto err;
    if ((bin_ptr = enif_make_new_binary(env, (size_t)bn_len, &term)) == NULL)
        goto err;

    if (BN_bn2bin(bn, bin_ptr) < 0)
        goto err;

    return term;

 err:
    return atom_error;
}

ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Base,Exponent,Modulo,bin_hdr) */
    BIGNUM *bn_base = NULL, *bn_exponent = NULL, *bn_modulo = NULL, *bn_result = NULL;
    BN_CTX *bn_ctx = NULL;
    unsigned char* ptr;
    int dlen;
    unsigned bin_hdr; /* return type: 0=plain binary, 4: mpint */
    unsigned extra_byte;
    ERL_NIF_TERM ret;

    if (!get_bn_from_bin(env, argv[0], &bn_base))
        goto bad_arg;
    if (!get_bn_from_bin(env, argv[1], &bn_exponent))
        goto bad_arg;
    if (!get_bn_from_bin(env, argv[2], &bn_modulo))
        goto bad_arg;
    if (!enif_get_uint(env, argv[3], &bin_hdr))
        goto bad_arg;
    if (bin_hdr != 0 && bin_hdr != 4)
        goto bad_arg;

    if ((bn_result = BN_new()) == NULL)
        goto err;
    if ((bn_ctx = BN_CTX_new()) == NULL)
        goto err;

    if (!BN_mod_exp(bn_result, bn_base, bn_exponent, bn_modulo, bn_ctx))
        goto err;

    dlen = BN_num_bytes(bn_result);
    if (dlen < 0 || dlen > INT_MAX / 8)
        goto bad_arg;
    extra_byte = bin_hdr && BN_is_bit_set(bn_result, dlen * 8 - 1);

    if ((ptr = enif_make_new_binary(env, bin_hdr + extra_byte + (unsigned int)dlen, &ret)) == NULL)
        goto err;

    if (bin_hdr) {
        put_uint32(ptr, extra_byte + (unsigned int)dlen);
        ptr[4] = 0; /* extra zeroed byte to ensure a positive mpint */
        ptr += bin_hdr + extra_byte;
    }

    BN_bn2bin(bn_result, ptr);
    goto done;

 bad_arg:
 err:
    ret = enif_make_badarg(env);

 done:
    if (bn_base)
        BN_free(bn_base);
    if (bn_exponent)
        BN_free(bn_exponent);
    if (bn_modulo)
        BN_free(bn_modulo);
    if (bn_result)
        BN_free(bn_result);
    if (bn_ctx)
        BN_CTX_free(bn_ctx);
    return ret;
}

#ifdef HAVE_EC
ERL_NIF_TERM bn2term(ErlNifEnv* env, size_t size, const BIGNUM *bn)
{
    int dlen;
    unsigned char* ptr;
    ERL_NIF_TERM ret;

    if (bn == NULL)
        return atom_undefined;

    dlen = BN_num_bytes(bn);
    if (dlen < 0)
        goto err;
    if (dlen > (int)size)
        goto err;
    if ((ptr = enif_make_new_binary(env, size, &ret)) == NULL)
        goto err;

#ifdef HAS_BN_bn2binpad
    BN_bn2binpad(bn, ptr, (int) size);    
#else
    /* First, maybe pad with zeroes */
    memset(ptr, 0, (size-dlen) );
    BN_bn2bin(bn, ptr + (size-dlen));
#endif

    return ret;

 err:
    return enif_make_badarg(env);
}
#endif


#ifdef HAS_3_0_API

int get_ossl_octet_string_param_from_bin(ErlNifEnv* env, char* key, ERL_NIF_TERM bin, OSSL_PARAM *dest)
{
    ErlNifBinary tmp;

    if (!enif_inspect_binary(env, bin, &tmp)) return 0;
    
    *dest = OSSL_PARAM_construct_octet_string(key, tmp.data, tmp.size);
    return 1;
}


int get_ossl_BN_param_from_bin(ErlNifEnv* env, char* key, ERL_NIF_TERM bin, OSSL_PARAM *dest)
{
    return get_ossl_BN_param_from_bin_sz(env, key, bin, dest, NULL);
}

int get_ossl_BN_param_from_bin_sz(ErlNifEnv* env, char* key, ERL_NIF_TERM bin,
                                  OSSL_PARAM *dest, size_t *size)
{
    BIGNUM *bn = NULL;
    int ok = 0;

    if (!get_bn_from_bin_sz(env, bin, &bn, size))
        return 0;

    ok = get_ossl_BN_param_from_bn(env, key, bn, dest);
    BN_free(bn);
    return ok;
}

int get_ossl_BN_param_from_bn(ErlNifEnv* env, char* key, const BIGNUM* bn,
                              OSSL_PARAM *dest)
{
    const size_t bn_sz = BN_num_bytes(bn);
    unsigned char* tmp_buf;
    ERL_NIF_TERM dummy_term;

    /* Create a binary term just as a convenient tmp buffer */
    tmp_buf = enif_make_new_binary(env, bn_sz, &dummy_term);
    if (BN_bn2nativepad(bn, tmp_buf, bn_sz) < 0) // Fill with BN in right endianity
        return 0;

    *dest = OSSL_PARAM_construct_BN(key, tmp_buf, bn_sz);
    return 1;
}



int get_ossl_param_from_bin_in_list(ErlNifEnv* env, char* key, ERL_NIF_TERM *listcell, OSSL_PARAM *dest)
{
    ERL_NIF_TERM head;
    
    return
        enif_get_list_cell(env, *listcell, &head, listcell) &&
        get_ossl_BN_param_from_bin(env, key, head, dest);
}

#endif








