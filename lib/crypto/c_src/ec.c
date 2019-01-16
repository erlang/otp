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

#include "ec.h"
#include "bn.h"

#ifdef HAVE_EC
static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg);
static ERL_NIF_TERM point2term(ErlNifEnv* env,
			       const EC_GROUP *group,
			       const EC_POINT *point,
			       point_conversion_form_t form);

ERL_NIF_TERM make_badarg_maybe(ErlNifEnv* env)
{
    ERL_NIF_TERM reason;
    if (enif_has_pending_exception(env, &reason))
	return reason; /* dummy return value ignored */
    else
	return enif_make_badarg(env);
}

static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg)
{
    EC_KEY *key = NULL;
    int c_arity = -1;
    const ERL_NIF_TERM* curve;
    ErlNifBinary seed;
    BIGNUM *p = NULL;
    BIGNUM *a = NULL;
    BIGNUM *b = NULL;
    BIGNUM *bn_order = NULL;
    BIGNUM *cofactor = NULL;
    EC_GROUP *group = NULL;
    EC_POINT *point = NULL;
    int f_arity = -1;
    const ERL_NIF_TERM *field;
    int p_arity = -1;
    const ERL_NIF_TERM *prime;
    long field_bits;

    /* {Field, Prime, Point, Order, CoFactor} = Curve */
    if (!enif_get_tuple(env, curve_arg, &c_arity, &curve))
        goto err;
    if (c_arity != 5)
        goto err;
    if (!get_bn_from_bin(env, curve[3], &bn_order))
        goto err;
    if (curve[4] != atom_none) {
        if (!get_bn_from_bin(env, curve[4], &cofactor))
            goto err;
    }

    /* {A, B, Seed} = Prime */
    if (!enif_get_tuple(env, curve[1], &p_arity, &prime))
        goto err;
    if (!get_bn_from_bin(env, prime[0], &a))
        goto err;
    if (!get_bn_from_bin(env, prime[1], &b))
        goto err;

    if (!enif_get_tuple(env, curve[0], &f_arity, &field))
        goto err;

    if (f_arity == 2 && field[0] == atom_prime_field) {
        /* {prime_field, Prime} */
        if (!get_bn_from_bin(env, field[1], &p))
            goto err;
        if (BN_is_negative(p))
            goto err;
        if (BN_is_zero(p))
            goto err;

        field_bits = BN_num_bits(p);
        if (field_bits > OPENSSL_ECC_MAX_FIELD_BITS)
            goto err;

        /* create the EC_GROUP structure */
        if ((group = EC_GROUP_new_curve_GFp(p, a, b, NULL)) == NULL)
            goto err;

    } else if (f_arity == 3 && field[0] == atom_characteristic_two_field) {
#if defined(OPENSSL_NO_EC2M)
        enif_raise_exception(env, atom_notsup);
        goto err;
#else
        /* {characteristic_two_field, M, Basis} */
        int b_arity = -1;
        const ERL_NIF_TERM* basis;

        if ((p = BN_new()) == NULL)
            goto err;
        if (!enif_get_long(env, field[1], &field_bits))
            goto err;
        if (field_bits > OPENSSL_ECC_MAX_FIELD_BITS || field_bits > INT_MAX)
            goto err;

        if (enif_get_tuple(env, field[2], &b_arity, &basis)) {
            if (b_arity == 2) {
                unsigned int k1;

                if (basis[0] != atom_tpbasis)
                    goto err;
                if (!enif_get_uint(env, basis[1], &k1))
                    goto err;

                /* {tpbasis, k} = Basis */
                if (field_bits <= k1 || k1 == 0 || k1 > INT_MAX)
                    goto err;

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits))
                    goto err;
                if (!BN_set_bit(p, (int)k1))
                    goto err;
                if (!BN_set_bit(p, 0))
                    goto err;

            } else if (b_arity == 4) {
                unsigned int k1, k2, k3;

                if (basis[0] != atom_ppbasis)
                    goto err;
                if (!enif_get_uint(env, basis[1], &k1))
                    goto err;
                if (!enif_get_uint(env, basis[2], &k2))
                    goto err;
                if (!enif_get_uint(env, basis[3], &k3))
                    goto err;

                /* {ppbasis, k1, k2, k3} = Basis */
                if (field_bits <= k3 || k3 <= k2 || k2 <= k1 || k1 == 0 || k3 > INT_MAX || k2 > INT_MAX || k1 > INT_MAX)
                    goto err;

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits))
                    goto err;
                if (!BN_set_bit(p, (int)k1))
                    goto err;
                if (!BN_set_bit(p, (int)k2))
                    goto err;
                if (!BN_set_bit(p, (int)k3))
                    goto err;
                if (!BN_set_bit(p, 0))
                    goto err;

            } else
                goto err;
        } else if (field[2] == atom_onbasis) {
            /* onbasis = Basis */
            /* no parameters */
            goto err;

        } else
            goto err;

        if ((group = EC_GROUP_new_curve_GF2m(p, a, b, NULL)) == NULL)
            goto err;
#endif
    } else
        goto err;

    if (enif_inspect_binary(env, prime[2], &seed)) {
        if (!EC_GROUP_set_seed(group, seed.data, seed.size))
            goto err;
    }

    if (!term2point(env, curve[2], group, &point))
        goto err;

    if (BN_is_negative(bn_order))
        goto err;
    if (BN_is_zero(bn_order))
        goto err;
    if (BN_num_bits(bn_order) > (int)field_bits + 1)
        goto err;

    if (!EC_GROUP_set_generator(group, point, bn_order, cofactor))
        goto err;

    EC_GROUP_set_asn1_flag(group, 0x0);

    if ((key = EC_KEY_new()) == NULL)
        goto err;

    if (!EC_KEY_set_group(key, group))
        goto err;

    goto done;

 err:
    if (key)
        EC_KEY_free(key);
    key = NULL;

 done:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (bn_order)
        BN_free(bn_order);
    if (cofactor)
        BN_free(cofactor);
    if (a)
        BN_free(a);
    if (b)
        BN_free(b);
    if (p)
        BN_free(p);
    if (group)
        EC_GROUP_free(group);
    if (point)
        EC_POINT_free(point);

    return key;
}

static ERL_NIF_TERM point2term(ErlNifEnv* env,
			       const EC_GROUP *group,
			       const EC_POINT *point,
			       point_conversion_form_t form)
{
    ERL_NIF_TERM ret;
    size_t dlen;
    ErlNifBinary bin;
    int bin_alloc = 0;

    if ((dlen = EC_POINT_point2oct(group, point, form, NULL, 0, NULL)) == 0)
	return atom_undefined;

    if (!enif_alloc_binary(dlen, &bin))
        goto err;
    bin_alloc = 1;

    if (!EC_POINT_point2oct(group, point, form, bin.data, bin.size, NULL))
        goto err;

    ERL_VALGRIND_MAKE_MEM_DEFINED(bin.data, bin.size);

    ret = enif_make_binary(env, &bin);
    bin_alloc = 0;
    goto done;

 err:
    if (bin_alloc)
        enif_release_binary(&bin);
    ret = enif_make_badarg(env);

 done:
    return ret;
}

int term2point(ErlNifEnv* env, ERL_NIF_TERM term, EC_GROUP *group, EC_POINT **pptr)
{
    ErlNifBinary bin;
    EC_POINT *point = NULL;

    if (!enif_inspect_binary(env, term, &bin))
        goto err;

    if ((point = EC_POINT_new(group)) == NULL)
        goto err;

    /* set the point conversion form */
    EC_GROUP_set_point_conversion_form(group, (point_conversion_form_t)(bin.data[0] & ~0x01));

    /* extract the ec point */
    if (!EC_POINT_oct2point(group, point, bin.data, bin.size, NULL))
        goto err;

    *pptr = point;
    return 1;

 err:
    if (point)
        EC_POINT_free(point);
    return 0;
}

int get_ec_key(ErlNifEnv* env,
		      ERL_NIF_TERM curve, ERL_NIF_TERM priv, ERL_NIF_TERM pub,
		      EC_KEY** res)
{
    EC_KEY *key = NULL;
    BIGNUM *priv_key = NULL;
    EC_POINT *pub_key = NULL;
    EC_GROUP *group = NULL;

    if (priv != atom_undefined) {
        if (!get_bn_from_bin(env, priv, &priv_key))
            goto err;
    }
    if (pub != atom_undefined) {
        if (!enif_is_binary(env, pub))
            goto err;
    }

    if ((key = ec_key_new(env, curve)) == NULL)
        goto err;

    if ((group = EC_GROUP_dup(EC_KEY_get0_group(key))) == NULL)
        goto err;

    if (term2point(env, pub, group, &pub_key)) {
        if (!EC_KEY_set_public_key(key, pub_key))
            goto err;
    }

    if (priv != atom_undefined && !BN_is_zero(priv_key)) {
        if (!EC_KEY_set_private_key(key, priv_key))
            goto err;

        /* calculate public key (if necessary) */
        if (EC_KEY_get0_public_key(key) == NULL) {
            /* the public key was not included in the SEC1 private
             * key => calculate the public key */
            if ((pub_key = EC_POINT_new(group)) == NULL)
                goto err;
            if (!EC_POINT_copy(pub_key, EC_GROUP_get0_generator(group)))
                goto err;
            if (!EC_POINT_mul(group, pub_key, priv_key, NULL, NULL, NULL))
                goto err;
            if (!EC_KEY_set_public_key(key, pub_key))
                goto err;
        }
    }
    goto done;

 err:
    if (key)
        EC_KEY_free(key);
    key = NULL;

 done:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (priv_key)
        BN_clear_free(priv_key);
    if (group)
        EC_GROUP_free(group);
    if (pub_key)
        EC_POINT_free(pub_key);

    if (key == NULL)
        return 0;

    *res = key;
    return 1;
}

#endif /* HAVE_EC */

ERL_NIF_TERM ec_key_generate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#if defined(HAVE_EC)
    EC_KEY *key = NULL;
    const EC_GROUP *group;
    const EC_POINT *public_key;
    ERL_NIF_TERM priv_key;
    ERL_NIF_TERM pub_key;
    ERL_NIF_TERM ret;

    if (!get_ec_key(env, argv[0], argv[1], atom_undefined, &key))
        goto bad_arg;

    if (argv[1] == atom_undefined) {
	if (!EC_KEY_generate_key(key))
            goto err;
    }

    group = EC_KEY_get0_group(key);
    public_key = EC_KEY_get0_public_key(key);

    if (group == NULL || public_key == NULL) {
        pub_key = atom_undefined;

    } else {
        pub_key = point2term(env, group, public_key,
                             EC_KEY_get_conv_form(key));
    }

    priv_key = bn2term(env, EC_KEY_get0_private_key(key));
    ret = enif_make_tuple2(env, pub_key, priv_key);
    goto done;

 err:
 bad_arg:
    ret = make_badarg_maybe(env);

 done:
    if (key)
        EC_KEY_free(key);
    return ret;

#else
    return atom_notsup;
#endif
}
