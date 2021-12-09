/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2020. All Rights Reserved.
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

#include "rsa.h"
#include "bn.h"

//#define(CHK_RSA_3_0)

static ERL_NIF_TERM rsa_generate_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#if !defined(HAS_3_0_API) || defined(CHK_RSA_3_0)
static ERL_NIF_TERM put_rsa_private_key(ErlNifEnv* env, const RSA *rsa);
#endif
#if !defined(HAS_3_0_API)
static int check_erlang_interrupt(int maj, int min, BN_GENCB *ctxt);
#endif

int get_rsa_private_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    /* key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C] */
    ERL_NIF_TERM head, tail;
    BIGNUM *e = NULL, *n = NULL, *d = NULL;
    BIGNUM *p = NULL, *q = NULL;
    BIGNUM *dmp1 = NULL, *dmq1 = NULL, *iqmp = NULL;
    RSA *rsa = NULL;

    if (!enif_get_list_cell(env, key, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &e))
        goto bad_arg;
    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &n))
        goto bad_arg;
    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &d))
        goto bad_arg;

    if ((rsa = RSA_new()) == NULL)
        goto err;

    if (!RSA_set0_key(rsa, n, e, d))
        goto err;
    /* rsa now owns n, e, and d */
    n = NULL;
    e = NULL;
    d = NULL;

    if (enif_is_empty_list(env, tail)) {
        if (EVP_PKEY_assign_RSA(*pkey, rsa) != 1)
            goto err;
        return 1;
    }

    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &p))
        goto bad_arg;
    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &q))
        goto bad_arg;
    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &dmp1))
        goto bad_arg;
    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &dmq1))
        goto bad_arg;
    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &iqmp))
        goto bad_arg;
    if (!enif_is_empty_list(env, tail))
        goto bad_arg;

    if (!RSA_set0_factors(rsa, p, q))
        goto err;

    /* rsa now owns p and q */
    p = NULL;
    q = NULL;

    if (!RSA_set0_crt_params(rsa, dmp1, dmq1, iqmp))
        goto err;
    /* rsa now owns dmp1, dmq1, and iqmp */
    dmp1 = NULL;
    dmq1 = NULL;
    iqmp = NULL;

    if (EVP_PKEY_assign_RSA(*pkey, rsa) != 1)
        goto err;

    return 1;

 bad_arg:
 err:
    if (rsa)
        RSA_free(rsa);
    if (e)
        BN_free(e);
    if (n)
        BN_free(n);
    if (d)
        BN_free(d);
    if (p)
        BN_free(p);
    if (q)
        BN_free(q);
    if (dmp1)
        BN_free(dmp1);
    if (dmq1)
        BN_free(dmq1);
    if (iqmp)
        BN_free(iqmp);

    return 0;
}

int get_rsa_public_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    /* key=[E,N] */
    ERL_NIF_TERM head, tail;
    BIGNUM *e = NULL, *n = NULL;
    RSA *rsa = NULL;

    if (!enif_get_list_cell(env, key, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &e))
        goto bad_arg;
    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &n))
        goto bad_arg;
    if (!enif_is_empty_list(env, tail))
        goto bad_arg;

    if ((rsa = RSA_new()) == NULL)
        goto err;
    if (!RSA_set0_key(rsa, n, e, NULL))
        goto err;

    /* rsa now owns n and e */
    n = NULL;
    e = NULL;

    if (EVP_PKEY_assign_RSA(*pkey, rsa) != 1)
        goto err;

    return 1;

 bad_arg:
 err:
    if (rsa)
        RSA_free(rsa);
    if (e)
        BN_free(e);
    if (n)
        BN_free(n);

    return 0;
}

#if !defined(HAS_3_0_API) || defined(CHK_RSA_3_0)
/* Creates a term which can be parsed by get_rsa_private_key(). This is a list of plain integer binaries (not mpints). */
static ERL_NIF_TERM put_rsa_private_key(ErlNifEnv* env, const RSA *rsa)
{
    ERL_NIF_TERM result[8];
    const BIGNUM *n = NULL, *e = NULL, *d = NULL, *p = NULL, *q = NULL, *dmp1 = NULL, *dmq1 = NULL, *iqmp = NULL;

    /* Return at least [E,N,D] */
    RSA_get0_key(rsa, &n, &e, &d);

    if ((result[0] = bin_from_bn(env, e)) == atom_error)  // Exponent E
        goto err;
    if ((result[1] = bin_from_bn(env, n)) == atom_error)  // Modulus N = p*q
        goto err;
    if ((result[2] = bin_from_bn(env, d)) == atom_error)  // Exponent D
        goto err;

    /* Check whether the optional additional parameters are available */
    RSA_get0_factors(rsa, &p, &q);
    RSA_get0_crt_params(rsa, &dmp1, &dmq1, &iqmp);

    if (p && q && dmp1 && dmq1 && iqmp) {
        if ((result[3] = bin_from_bn(env, p)) == atom_error)     // Factor p
            goto err;
        if ((result[4] = bin_from_bn(env, q)) == atom_error)     // Factor q
            goto err;
        if ((result[5] = bin_from_bn(env, dmp1)) == atom_error)  // D mod (p-1)
            goto err;
        if ((result[6] = bin_from_bn(env, dmq1)) == atom_error)  // D mod (q-1)
            goto err;
        if ((result[7] = bin_from_bn(env, iqmp)) == atom_error)  // (1/q) mod p
            goto err;

	return enif_make_list_from_array(env, result, 8);
    } else {
	return enif_make_list_from_array(env, result, 3);
    }

 err:
    return enif_make_badarg(env);
}
#endif

#if !defined(HAS_3_0_API)
/* Legacy API deprecated from 3.0 is used */

static int check_erlang_interrupt(int maj, int min, BN_GENCB *ctxt)
{
    ErlNifEnv *env = BN_GENCB_get_arg(ctxt);

    if (!enif_is_current_process_alive(env)) {
	return 0;
    } else {
	return 1;
    }
}

static ERL_NIF_TERM rsa_generate_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (ModulusSize, PublicExponent) */
    ERL_NIF_TERM ret;
    int modulus_bits;
    BIGNUM *pub_exp = NULL, *three = NULL;
    RSA *rsa = NULL;
    BN_GENCB *intr_cb = NULL;
#ifndef HAVE_OPAQUE_BN_GENCB
    BN_GENCB intr_cb_buf;
#endif

    if (!enif_get_int(env, argv[0], &modulus_bits))
        goto bad_arg;
    if (modulus_bits < 256)
        goto bad_arg;
    if (!get_bn_from_bin(env, argv[1], &pub_exp))
        goto bad_arg;

    /* Make sure the public exponent is large enough (at least 3).
     * Without this, RSA_generate_key_ex() can run forever. */
    if ((three = BN_new()) == NULL)
        goto err;
    if (!BN_set_word(three, 3))
        goto err;
    if (BN_cmp(pub_exp, three) < 0)
        goto err;

    /* For large keys, prime generation can take many seconds. Set up
     * the callback which we use to test whether the process has been
     * interrupted. */
#ifdef HAVE_OPAQUE_BN_GENCB
    if ((intr_cb = BN_GENCB_new()) == NULL)
        goto err;
#else
    intr_cb = &intr_cb_buf;
#endif
    BN_GENCB_set(intr_cb, check_erlang_interrupt, env);

    if ((rsa = RSA_new()) == NULL)
        goto err;

    if (!RSA_generate_key_ex(rsa, modulus_bits, pub_exp, intr_cb))
        goto err;

    ret = put_rsa_private_key(env, rsa);
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = atom_error;

 done:
    if (pub_exp)
        BN_free(pub_exp);
    if (three)
        BN_free(three);
#ifdef HAVE_OPAQUE_BN_GENCB
    if (intr_cb)
        BN_GENCB_free(intr_cb);
#endif
    if (rsa)
        RSA_free(rsa);
    return ret;
}

#else
/* New 3.0 API is used */

static ERL_NIF_TERM rsa_generate_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (ModulusSize, PublicExponent/binary, PublicExponent) */
    ERL_NIF_TERM ret;
    unsigned int msize;
    ErlNifBinary pub_exp;
    OSSL_PARAM params[3];
    EVP_PKEY *pkey = NULL;
    EVP_PKEY_CTX *pctx = NULL;

    if (!enif_get_uint(env, argv[0], &msize)) {
        ret = EXCP_BADARG_N(env, 0, "Can't get unsigned int");
        goto ret;
    }
    if (msize < 256) {
        ret = EXCP_BADARG_N(env, 0, "Can't be < 256");
        goto ret;
    }

    if (!enif_inspect_binary(env, argv[1], &pub_exp)) {
        ret = EXCP_BADARG_N(env, 1, "Can't get binary public exponent");
        goto ret;
    }

    /* https://www.openssl.org/docs/man3.0/man7/EVP_PKEY-RSA.html */
    pctx = EVP_PKEY_CTX_new_from_name(NULL, "RSA", NULL);

    if (!EVP_PKEY_keygen_init(pctx)) {
        ret = EXCP_ERROR(env, "Can't init RSA generation");
        goto ret;
    }

    params[0] = OSSL_PARAM_construct_uint("bits", &msize);
    params[1] = OSSL_PARAM_construct_BN("e", pub_exp.data, pub_exp.size);
    params[2] = OSSL_PARAM_construct_end();

    if (!EVP_PKEY_CTX_set_params(pctx, params))  {
        ret = EXCP_ERROR(env, "Can't set params");
        goto ret;
    }

    if (!EVP_PKEY_generate(pctx, &pkey)) {
        ret = EXCP_ERROR(env, "Can't generate RSA key-pair");
        goto ret;
    }

    /* get priv and pub */
    {
        BIGNUM *e = NULL, *n = NULL, *d = NULL;
        BIGNUM *p = NULL, *q = NULL;
        BIGNUM *dmp1 = NULL, *dmq1 = NULL, *iqmp = NULL;
        ERL_NIF_TERM result[8];

        /* https://www.openssl.org/docs/man3.0/man7/EVP_PKEY-RSA.html */
        if (
            !EVP_PKEY_get_bn_param(pkey, "e", &e)                       // Exponent E
            || !EVP_PKEY_get_bn_param(pkey, "n", &n)                    // Modulus N = p*q
            || !EVP_PKEY_get_bn_param(pkey, "d", &d)                    // Exponent D
            || !EVP_PKEY_get_bn_param(pkey, "rsa-factor1", &p)          // Factor p
            || !EVP_PKEY_get_bn_param(pkey, "rsa-factor2", &q)          // Factor q
            || !EVP_PKEY_get_bn_param(pkey, "rsa-exponent1", &dmp1)     // D mod (p-1)
            || !EVP_PKEY_get_bn_param(pkey, "rsa-exponent2", &dmq1)     // D mod (q-1)
            || !EVP_PKEY_get_bn_param(pkey, "rsa-coefficient1", &iqmp)  // (1/q) mod p
            || ((result[0] = bin_from_bn(env, e)) == atom_error)
            || ((result[1] = bin_from_bn(env, n)) == atom_error)
            || ((result[2] = bin_from_bn(env, d)) == atom_error)
            || ((result[3] = bin_from_bn(env, p)) == atom_error)
            || ((result[4] = bin_from_bn(env, q)) == atom_error)
            || ((result[5] = bin_from_bn(env, dmp1)) == atom_error)
            || ((result[6] = bin_from_bn(env, dmq1)) == atom_error)
            || ((result[7] = bin_from_bn(env, iqmp)) == atom_error)
            ) {
            ret = EXCP_ERROR(env, "Can't get RSA keys");
            goto ret;
        }

        ret =  enif_make_list_from_array(env, result, 8);

#ifdef CHK_RSA_3_0
        {RSA *rsa = EVP_PKEY_get1_RSA(pkey);
            if (!rsa)
                ret = enif_make_tuple2(env, ret, atom_error);
            else
                ret = enif_make_tuple2(env, ret, put_rsa_private_key(env,rsa));
        }
#endif
    }

 ret:
    if (pctx) EVP_PKEY_CTX_free(pctx);
    return ret;
}

#endif /* #else-part of #if !defined(HAS_3_0_API) */


ERL_NIF_TERM rsa_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* RSA key generation can take a long time (>1 sec for a large
     * modulus), so schedule it as a CPU-bound operation. */
    return enif_schedule_nif(env, "rsa_generate_key",
			     ERL_NIF_DIRTY_JOB_CPU_BOUND,
			     rsa_generate_key, argc, argv);
}
