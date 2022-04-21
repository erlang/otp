/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2022. All Rights Reserved.
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

# if defined(HAS_3_0_API)

int get_curve_definition(ErlNifEnv* env, ERL_NIF_TERM *ret, ERL_NIF_TERM def,
                         OSSL_PARAM params[], int *i,
                         size_t *order_size)
{
    const ERL_NIF_TERM* curve;
    int c_arity = -1;
    const ERL_NIF_TERM *prime;
    int p_arity = -1;
    const ERL_NIF_TERM *field;
    int f_arity = -1;
    BIGNUM *p = NULL;

    /* Here are two random curve definition examples, one prime_field and
       one characteristic_two_field. Both are from the crypto/src/crypto_ec_curves.erl.

        curve(secp192r1) ->
           {
            {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF:192>>}, %% Prime
            {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC:192>>,               %% A
             <<16#64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1:192>>,               %% B
             <<16#3045AE6FC8422F64ED579528D38120EAE12196D5:160>>},                      %% Seed
             <<16#04:8,
               16#188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012:192,                 %% X(p0)
               16#07192B95FFC8DA78631011ED6B24CDD573F977A11E794811:192>>,               %% Y(p0)
             <<16#FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831:192>>,               %% Order
             <<16#01:8>>                                                                %% CoFactor
           };

        curve(c2pnb176v1) ->
           {
            {characteristic_two_field, 176, {ppbasis,1,2,43}},
            {<<16#E4E6DB2995065C407D9D39B8D0967B96704BA8E9C90B:176>>,                   %% A
             <<16#5DDA470ABE6414DE8EC133AE28E9BBD7FCEC0AE0FFF2:176>>,                   %% B
             none},                                                                     %% Seed
             <<16#04:8,
               16#8D16C2866798B600F9F08BB4A8E860F3298CE04A5798:176,                     %% X(p0)
               16#6FA4539C2DADDDD6BAB5167D61B436E1D92BB16A562C:176>>,                   %% Y(p0)
             <<16#010092537397ECA4F6145799D62B0A19CE06FE26AD:168>>,                     %% Order
             <<16#FF6E:16>>                                                             %% CoFactor
           };
    */

    /* {Field, Prime, Point, Order, CoFactor} = CurveDef */
    if (!enif_get_tuple(env, def, &c_arity, &curve) ||
        c_arity != 5)
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad curve def. Expect 5-tuple."));

    if (!get_ossl_octet_string_param_from_bin(env, "generator", curve[2], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad Generator (Point)"));

    if (!get_ossl_BN_param_from_bin_sz(env, "order", curve[3], &params[(*i)++], order_size))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad order"));

    if (curve[4] == atom_none)
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Cofactor must be != none"));
                
    if (!get_ossl_BN_param_from_bin(env, "cofactor", curve[4], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad cofactor"));

    /* {A, B, Seed} = Prime = curve[1] */
    if (!enif_get_tuple(env, curve[1], &p_arity, &prime))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad Prime"));

    if (!get_ossl_BN_param_from_bin(env, "a", prime[0], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad a"));

    if (!get_ossl_BN_param_from_bin(env, "b", prime[1], &params[(*i)++]))
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad b"));

    if (enif_is_binary(env, prime[2]))
        if (!get_ossl_octet_string_param_from_bin(env, "seed", prime[2], &params[(*i)++]))
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad seed"));

    /* Field = curve[0] */
    if (!enif_get_tuple(env, curve[0], &f_arity, &field)) {
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad Field"));
    }
    else if (f_arity == 2 && field[0] == atom_prime_field) {
        /* {prime_field, Prime} */
        params[(*i)++] = OSSL_PARAM_construct_utf8_string("field-type",  "prime-field", 0);
                
        if (!get_ossl_BN_param_from_bin(env, "p", field[1], &params[(*i)++]))
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad p (Prime)"));
    }

    else if (f_arity == 3 && field[0] == atom_characteristic_two_field) {
        /* {characteristic_two_field, M, Basis} */
#  if defined(OPENSSL_NO_EC2M)
        assign_goto(*ret, err, EXCP_NOTSUP_N(env, 1, "Unsupported field-type (characteristic_two_field)"));
#  else
        int b_arity = -1;
        const ERL_NIF_TERM* basis;
        long field_bits;

        params[(*i)++] = OSSL_PARAM_construct_utf8_string("field-type",  "characteristic-two-field", 0);

        if ((p = BN_new()) == NULL)
            assign_goto(*ret, err, EXCP_ERROR(env, "Creating bignum failed"));

        if (!enif_get_long(env, field[1], &field_bits) ||
            (field_bits > OPENSSL_ECC_MAX_FIELD_BITS || field_bits > INT_MAX)
            )
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad field-bits (M)"));
                    
        if (enif_get_tuple(env, field[2], &b_arity, &basis)) {
            if (b_arity == 2) {
                unsigned int k1;

                if (basis[0] != atom_tpbasis)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad atom"));
                if (!enif_get_uint(env, basis[1], &k1))
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "uint expected (k1)"));

                /* {tpbasis, k} = Basis */
                if (field_bits <= k1 || k1 == 0 || k1 > INT_MAX)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "bad values (field_bits or k1)"));

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits) ||
                    !BN_set_bit(p, (int)k1) ||
                    !BN_set_bit(p, 0))
                    assign_goto(*ret, err, EXCP_ERROR(env, "Polynom bit setting failed"));

            } else if (b_arity == 4) {
                /* {ppbasis, k1, k2, k3} = Basis */
                unsigned int k1, k2, k3;

                if (basis[0] != atom_ppbasis)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad atom"));

                if (!enif_get_uint(env, basis[1], &k1) ||
                    !enif_get_uint(env, basis[2], &k2) ||
                    !enif_get_uint(env, basis[3], &k3))
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Expecting uint (k1,k2,k3)"));

                if (field_bits <= k3 || k3 <= k2 || k2 <= k1 || k1 == 0 ||
                    k3 > INT_MAX || k2 > INT_MAX || k1 > INT_MAX)
                    assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "bad values (field_bits, k1, k2 or k3)"));

                /* create the polynomial */
                if (!BN_set_bit(p, (int)field_bits) ||
                    !BN_set_bit(p, (int)k1) ||
                    !BN_set_bit(p, (int)k2) ||
                    !BN_set_bit(p, (int)k3) ||
                    !BN_set_bit(p, 0) )
                    assign_goto(*ret, err, EXCP_ERROR(env, "Polynom bit setting failed"));

            } else
                assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad tuple"));

        } else if (field[2] == atom_onbasis) {
            /* onbasis = Basis */
            /* no parameters */
            assign_goto(*ret, err, EXCP_NOTSUP_N(env, 1, "'onbasis' not supported"));
        } else
            assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad last field"));

        {
            ErlNifBinary tmp;
                        
            if (!enif_inspect_binary(env, bin_from_bn(env,p), &tmp) || // Allocate buf
                BN_bn2nativepad(p, tmp.data, tmp.size) < 0) {// Fill with BN in right endianity
                assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "BN padding failed"));
            }
            params[(*i)++] = OSSL_PARAM_construct_BN("p", tmp.data, tmp.size);
        }
#  endif
    }
    else
        assign_goto(*ret, err, EXCP_ERROR_N(env, 1, "Bad field-type")); 

    if (p) BN_free(p);
    return 1;
    
 err:
    if (p) BN_free(p);
    return 0;
}

# endif /* HAS_3_0_API */

static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg, size_t *size);
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

static EC_KEY* ec_key_new(ErlNifEnv* env, ERL_NIF_TERM curve_arg, size_t *size)
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
    if (!get_bn_from_bin_sz(env, curve[3], &bn_order, size))
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

int get_ec_private_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    EC_KEY *ec = NULL;

    if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms))
        goto err;
    if (tpl_arity != 2)
        goto err;
    if (!enif_is_tuple(env, tpl_terms[0]))
        goto err;
    if (!enif_is_binary(env, tpl_terms[1]))
        goto err;
    if (!get_ec_key_sz(env, tpl_terms[0], tpl_terms[1], atom_undefined, &ec, NULL))
        goto err;

    *pkey = EVP_PKEY_new();
    if (EVP_PKEY_assign_EC_KEY(*pkey, ec) != 1)
        goto err;
            /* On success, result owns ec */
    ec = NULL;
    return 1;

 err:
    if (ec)
        EC_KEY_free(ec);
    return 0;
}

int get_ec_public_key(ErlNifEnv* env, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    const ERL_NIF_TERM *tpl_terms;
    int tpl_arity;
    EC_KEY *ec = NULL;

    if (!enif_get_tuple(env, key, &tpl_arity, &tpl_terms))
        goto err;
    if (tpl_arity != 2)
        goto err;
    if (!enif_is_tuple(env, tpl_terms[0]))
        goto err;
    if (!enif_is_binary(env, tpl_terms[1]))
        goto err;
    if (!get_ec_key_sz(env, tpl_terms[0], atom_undefined, tpl_terms[1], &ec, NULL))
        goto err;

    *pkey = EVP_PKEY_new();
    if (EVP_PKEY_assign_EC_KEY(*pkey, ec) != 1)
        goto err;
            /* On success, result owns ec */
    ec = NULL;
    return 1;

 err:
    if (ec)
        EC_KEY_free(ec);
    return 0;
}


int get_ec_key_sz(ErlNifEnv* env,
                  ERL_NIF_TERM curve, ERL_NIF_TERM priv, ERL_NIF_TERM pub,
                  EC_KEY** res,
                  size_t* size)
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

    if ((key = ec_key_new(env, curve, size)) == NULL)
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


ERL_NIF_TERM ec_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Curve, PrivKey)  */
    EC_KEY *key = NULL;
    const EC_GROUP *group;
    const EC_POINT *public_key;
    ERL_NIF_TERM priv_key;
    ERL_NIF_TERM pub_key;
    ERL_NIF_TERM ret;
    size_t size;

    if (!get_ec_key_sz(env, argv[0], argv[1], atom_undefined, &key, &size))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Couldn't get EC key"));

    if (argv[1] == atom_undefined) {
	if (!EC_KEY_generate_key(key))
            assign_goto(ret, err, EXCP_ERROR(env, "Couldn't generate EC key"));
    }

    group = EC_KEY_get0_group(key);
    public_key = EC_KEY_get0_public_key(key);

    if (group == NULL || public_key == NULL) {
        pub_key = atom_undefined;

    } else {
        pub_key = point2term(env, group, public_key,
                             EC_KEY_get_conv_form(key));
    }

    priv_key = bn2term(env, size, EC_KEY_get0_private_key(key));
    ret = enif_make_tuple2(env, pub_key, priv_key);
    goto done;

 err:
 done:
    if (key)
        EC_KEY_free(key);
    return ret;
}
#endif /* HAVE_EC */


#if ! defined(HAVE_EC)
ERL_NIF_TERM ec_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{ /* (Curve, PrivKey)  */
    return EXCP_NOTSUP_N(env, 0, "EC not supported");
}
#endif
