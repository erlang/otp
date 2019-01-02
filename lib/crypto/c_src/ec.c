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

    /* {Field, Prime, Point, Order, CoFactor} = Curve */
    if (enif_get_tuple(env,curve_arg,&c_arity,&curve)
	&& c_arity == 5
	&& get_bn_from_bin(env, curve[3], &bn_order)
	&& (curve[4] != atom_none && get_bn_from_bin(env, curve[4], &cofactor))) {

	int f_arity = -1;
	const ERL_NIF_TERM* field;
	int p_arity = -1;
	const ERL_NIF_TERM* prime;

	long field_bits;

	/* {A, B, Seed} = Prime */
	if (!enif_get_tuple(env,curve[1],&p_arity,&prime)
	    || !get_bn_from_bin(env, prime[0], &a)
	    || !get_bn_from_bin(env, prime[1], &b))
	    goto out_err;

	if (!enif_get_tuple(env,curve[0],&f_arity,&field))
	    goto out_err;

	if (f_arity == 2 && field[0] == atom_prime_field) {
	    /* {prime_field, Prime} */

	    if (!get_bn_from_bin(env, field[1], &p))
		goto out_err;

	    if (BN_is_negative(p) || BN_is_zero(p))
		goto out_err;

	    field_bits = BN_num_bits(p);
	    if (field_bits > OPENSSL_ECC_MAX_FIELD_BITS)
		goto out_err;

	    /* create the EC_GROUP structure */
	    group = EC_GROUP_new_curve_GFp(p, a, b, NULL);

	} else if (f_arity == 3 && field[0] == atom_characteristic_two_field) {
#if defined(OPENSSL_NO_EC2M)
	    enif_raise_exception(env, atom_notsup);
	    goto out_err;
#else
	    /* {characteristic_two_field, M, Basis} */

	    int b_arity = -1;
	    const ERL_NIF_TERM* basis;
	    unsigned int k1, k2, k3;

	    if ((p = BN_new()) == NULL)
		goto out_err;

	    if (!enif_get_long(env, field[1], &field_bits)
		|| field_bits > OPENSSL_ECC_MAX_FIELD_BITS)
		goto out_err;

	    if (enif_get_tuple(env,field[2],&b_arity,&basis)) {
		if (b_arity == 2
		    && basis[0] == atom_tpbasis
		    && enif_get_uint(env, basis[1], &k1)) {
		    /* {tpbasis, k} = Basis */

		    if (!(field_bits > k1 && k1 > 0))
			goto out_err;

		    /* create the polynomial */
		    if (!BN_set_bit(p, (int)field_bits)
                        || !BN_set_bit(p, (int)k1)
                        || !BN_set_bit(p, 0))
                                goto out_err;

		} else if (b_arity == 4
		    && basis[0] == atom_ppbasis
		    && enif_get_uint(env, basis[1], &k1)
		    && enif_get_uint(env, basis[2], &k2)
		    && enif_get_uint(env, basis[3], &k3)) {
		    /* {ppbasis, k1, k2, k3} = Basis */

		    if (!(field_bits > k3 && k3 > k2 && k2 > k1 && k1 > 0))
			goto out_err;

		    /* create the polynomial */
		    if (!BN_set_bit(p, (int)field_bits)
			|| !BN_set_bit(p, (int)k1)
			|| !BN_set_bit(p, (int)k2)
			|| !BN_set_bit(p, (int)k3)
			|| !BN_set_bit(p, 0))
			goto out_err;

		} else
		    goto out_err;
	    } else if (field[2] == atom_onbasis) {
		/* onbasis = Basis */
		/* no parameters */
		goto out_err;

	    } else
		goto out_err;

	    group = EC_GROUP_new_curve_GF2m(p, a, b, NULL);
#endif
	} else
	    goto out_err;

        if (!group)
            goto out_err;

	if (enif_inspect_binary(env, prime[2], &seed)) {
	    EC_GROUP_set_seed(group, seed.data, seed.size);
	}

	if (!term2point(env, curve[2], group, &point))
	    goto out_err;

	if (BN_is_negative(bn_order)
	    || BN_is_zero(bn_order)
	    || BN_num_bits(bn_order) > (int)field_bits + 1)
	    goto out_err;

	if (!EC_GROUP_set_generator(group, point, bn_order, cofactor))
	    goto out_err;

	EC_GROUP_set_asn1_flag(group, 0x0);

	key = EC_KEY_new();
	if (!key)
	    goto out_err;
	EC_KEY_set_group(key, group);
    }
    else {
	goto out_err;
    }


    goto out;

out_err:
    if (key) EC_KEY_free(key);
    key = NULL;

out:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (p) BN_free(p);
    if (a) BN_free(a);
    if (b) BN_free(b);
    if (bn_order) BN_free(bn_order);
    if (cofactor) BN_free(cofactor);
    if (group) EC_GROUP_free(group);
    if (point) EC_POINT_free(point);

    return key;
}

static ERL_NIF_TERM point2term(ErlNifEnv* env,
			       const EC_GROUP *group,
			       const EC_POINT *point,
			       point_conversion_form_t form)
{
    unsigned dlen;
    ErlNifBinary bin;

    dlen = EC_POINT_point2oct(group, point, form, NULL, 0, NULL);
    if (dlen == 0)
	return atom_undefined;

    if (!enif_alloc_binary(dlen, &bin))
	return enif_make_badarg(env);

    if (!EC_POINT_point2oct(group, point, form, bin.data, bin.size, NULL)) {
	enif_release_binary(&bin);
	return enif_make_badarg(env);
    }
    ERL_VALGRIND_MAKE_MEM_DEFINED(bin.data, bin.size);
    return enif_make_binary(env, &bin);
}

int term2point(ErlNifEnv* env, ERL_NIF_TERM term, EC_GROUP *group, EC_POINT **pptr)
{
    int ret = 0;
    ErlNifBinary bin;
    EC_POINT *point;

    if (!enif_inspect_binary(env,term,&bin)) {
        return 0;
    }

    if ((*pptr = point = EC_POINT_new(group)) == NULL) {
	return 0;
    }

    /* set the point conversion form */
    EC_GROUP_set_point_conversion_form(group, (point_conversion_form_t)(bin.data[0] & ~0x01));

    /* extract the ec point */
    if (!EC_POINT_oct2point(group, point, bin.data, bin.size, NULL)) {
	EC_POINT_free(point);
	*pptr = NULL;
    } else
	ret = 1;

    return ret;
}

int get_ec_key(ErlNifEnv* env,
		      ERL_NIF_TERM curve, ERL_NIF_TERM priv, ERL_NIF_TERM pub,
		      EC_KEY** res)
{
    EC_KEY *key = NULL;
    BIGNUM *priv_key = NULL;
    EC_POINT *pub_key = NULL;
    EC_GROUP *group = NULL;

    if (!(priv == atom_undefined || get_bn_from_bin(env, priv, &priv_key))
	|| !(pub == atom_undefined || enif_is_binary(env, pub))) {
	goto out_err;
    }

    key = ec_key_new(env, curve);

    if (!key) {
	goto out_err;
    }

    if (!group)
	group = EC_GROUP_dup(EC_KEY_get0_group(key));

    if (term2point(env, pub, group, &pub_key)) {
	    if (!EC_KEY_set_public_key(key, pub_key)) {
		    goto out_err;
	    }
    }
    if (priv != atom_undefined
	&& !BN_is_zero(priv_key)) {
	    if (!EC_KEY_set_private_key(key, priv_key))
		    goto out_err;

	    /* calculate public key (if necessary) */
	    if (EC_KEY_get0_public_key(key) == NULL)
	    {
		    /* the public key was not included in the SEC1 private
		     * key => calculate the public key */
		    pub_key = EC_POINT_new(group);
		    if (pub_key == NULL
			|| !EC_POINT_copy(pub_key, EC_GROUP_get0_generator(group))
			|| !EC_POINT_mul(group, pub_key, priv_key, NULL, NULL, NULL)
			|| !EC_KEY_set_public_key(key, pub_key))
			    goto out_err;
	    }
    }

    goto out;

out_err:
    if (key) EC_KEY_free(key);
    key = NULL;

out:
    /* some OpenSSL structures are mem-dup'ed into the key,
       so we have to free our copies here */
    if (priv_key) BN_clear_free(priv_key);
    if (pub_key) EC_POINT_free(pub_key);
    if (group) EC_GROUP_free(group);
    if (!key)
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
    ERL_NIF_TERM pub_key = atom_undefined;

    if (!get_ec_key(env, argv[0], argv[1], atom_undefined, &key))
	goto badarg;

    if (argv[1] == atom_undefined) {
	if (!EC_KEY_generate_key(key))
	    goto badarg;
    }

    group = EC_KEY_get0_group(key);
    public_key = EC_KEY_get0_public_key(key);

    if (group && public_key) {
	pub_key = point2term(env, group, public_key,
			     EC_KEY_get_conv_form(key));
    }
    priv_key = bn2term(env, EC_KEY_get0_private_key(key));
    EC_KEY_free(key);
    return enif_make_tuple2(env, pub_key, priv_key);

badarg:
    if (key)
	EC_KEY_free(key);
    return make_badarg_maybe(env);
#else
    return atom_notsup;
#endif
}
