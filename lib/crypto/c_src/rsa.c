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

#include "rsa.h"
#include "bn.h"

static ERL_NIF_TERM rsa_generate_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM put_rsa_private_key(ErlNifEnv* env, const RSA *rsa);
static int check_erlang_interrupt(int maj, int min, BN_GENCB *ctxt);

int get_rsa_private_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa)
{
    /* key=[E,N,D]|[E,N,D,P1,P2,E1,E2,C] */
    ERL_NIF_TERM head, tail;
    BIGNUM *e = NULL, *n = NULL, *d = NULL;
    BIGNUM *p = NULL, *q = NULL;
    BIGNUM *dmp1 = NULL, *dmq1 = NULL, *iqmp = NULL;

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

    if (!RSA_set0_key(rsa, n, e, d))
        goto err;
    /* rsa now owns n, e, and d */
    n = NULL;
    e = NULL;
    d = NULL;

    if (enif_is_empty_list(env, tail))
        return 1;

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

    return 1;

 bad_arg:
 err:
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

int get_rsa_public_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa)
{
    /* key=[E,N] */
    ERL_NIF_TERM head, tail;
    BIGNUM *e = NULL, *n = NULL;

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

    if (!RSA_set0_key(rsa, n, e, NULL))
        goto err;
    /* rsa now owns n and e */
    n = NULL;
    e = NULL;

    return 1;

 bad_arg:
 err:
    if (e)
        BN_free(e);
    if (n)
        BN_free(n);

    return 0;
}

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

    ASSERT(argc == 2);

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

ERL_NIF_TERM rsa_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* RSA key generation can take a long time (>1 sec for a large
     * modulus), so schedule it as a CPU-bound operation. */
    return enif_schedule_nif(env, "rsa_generate_key",
			     ERL_NIF_DIRTY_JOB_CPU_BOUND,
			     rsa_generate_key, argc, argv);
}
