#include "srp.h"
#include "bn.h"

ERL_NIF_TERM srp_value_B_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Multiplier, Verifier, Generator, Exponent, Prime) */
    BIGNUM *bn_verifier = NULL;
    BIGNUM *bn_exponent = NULL, *bn_generator = NULL, *bn_prime = NULL, *bn_multiplier = NULL, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!get_bn_from_bin(env, argv[0], &bn_multiplier)
	|| !get_bn_from_bin(env, argv[1], &bn_verifier)
	|| !get_bn_from_bin(env, argv[2], &bn_generator)
	|| !get_bn_from_bin(env, argv[3], &bn_exponent)
	|| !get_bn_from_bin(env, argv[4], &bn_prime)) {
	if (bn_multiplier) BN_free(bn_multiplier);
	if (bn_verifier) BN_free(bn_verifier);
	if (bn_generator) BN_free(bn_generator);
	if (bn_exponent) BN_free(bn_exponent);
	if (bn_prime) BN_free(bn_prime);
	return enif_make_badarg(env);
    }

    bn_result = BN_new();
    bn_ctx = BN_CTX_new();

    /* B = k*v + g^b % N */

    /* k * v */
    BN_mod_mul(bn_multiplier, bn_multiplier, bn_verifier, bn_prime, bn_ctx);

    /* g^b % N */
    BN_mod_exp(bn_result, bn_generator, bn_exponent, bn_prime, bn_ctx);

    /* k*v + g^b % N */
    BN_mod_add(bn_result, bn_result, bn_multiplier, bn_prime, bn_ctx);

    /* check that B % N != 0, reuse bn_multiplier */
    BN_nnmod(bn_multiplier, bn_result, bn_prime, bn_ctx);
    if (BN_is_zero(bn_multiplier)) {
	ret = atom_error;
    } else {
	dlen = BN_num_bytes(bn_result);
	ptr = enif_make_new_binary(env, dlen, &ret);
	BN_bn2bin(bn_result, ptr);
    }
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);
    BN_free(bn_prime);
    BN_free(bn_generator);
    BN_free(bn_multiplier);
    BN_free(bn_exponent);
    BN_free(bn_verifier);
    return ret;
}

ERL_NIF_TERM srp_user_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (a, u, B, Multiplier, Prime, Exponent, Generator) */
/*
        <premaster secret> = (B - (k * g^x)) ^ (a + (u * x)) % N
*/
    BIGNUM *bn_exponent = NULL, *bn_a = NULL;
    BIGNUM *bn_u = NULL, *bn_multiplier = NULL, *bn_exp2,
        *bn_base, *bn_prime = NULL, *bn_generator = NULL,
        *bn_B = NULL, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!get_bn_from_bin(env, argv[0], &bn_a)
	|| !get_bn_from_bin(env, argv[1], &bn_u)
	|| !get_bn_from_bin(env, argv[2], &bn_B)
	|| !get_bn_from_bin(env, argv[3], &bn_multiplier)
	|| !get_bn_from_bin(env, argv[4], &bn_generator)
	|| !get_bn_from_bin(env, argv[5], &bn_exponent)
	|| !get_bn_from_bin(env, argv[6], &bn_prime))
    {
	if (bn_exponent) BN_free(bn_exponent);
	if (bn_a) BN_free(bn_a);
	if (bn_u) BN_free(bn_u);
	if (bn_B) BN_free(bn_B);
	if (bn_multiplier) BN_free(bn_multiplier);
	if (bn_generator) BN_free(bn_generator);
	if (bn_prime) BN_free(bn_prime);
	return enif_make_badarg(env);
    }

    bn_ctx = BN_CTX_new();
    bn_result = BN_new();

    /* check that B % N != 0 */
    BN_nnmod(bn_result, bn_B, bn_prime, bn_ctx);
    if (BN_is_zero(bn_result)) {
	BN_free(bn_exponent);
	BN_free(bn_a);
	BN_free(bn_generator);
	BN_free(bn_prime);
	BN_free(bn_u);
	BN_free(bn_B);
	BN_CTX_free(bn_ctx);

	return atom_error;
    }

    /* (B - (k * g^x)) */
    bn_base = BN_new();
    BN_mod_exp(bn_result, bn_generator, bn_exponent, bn_prime, bn_ctx);
    BN_mod_mul(bn_result, bn_multiplier, bn_result, bn_prime, bn_ctx);
    BN_mod_sub(bn_base, bn_B, bn_result, bn_prime, bn_ctx);

    /* a + (u * x) */
    bn_exp2 = BN_new();
    BN_mul(bn_result, bn_u, bn_exponent, bn_ctx);
    BN_add(bn_exp2, bn_a, bn_result);

    /* (B - (k * g^x)) ^ (a + (u * x)) % N */
    BN_mod_exp(bn_result, bn_base, bn_exp2, bn_prime, bn_ctx);

    dlen = BN_num_bytes(bn_result);
    ptr = enif_make_new_binary(env, dlen, &ret);
    BN_bn2bin(bn_result, ptr);
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);

    BN_free(bn_multiplier);
    BN_free(bn_exp2);
    BN_free(bn_u);
    BN_free(bn_exponent);
    BN_free(bn_a);
    BN_free(bn_B);
    BN_free(bn_base);
    BN_free(bn_generator);
    BN_free(bn_prime);
    return ret;
}

ERL_NIF_TERM srp_host_secret_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Verifier, b, u, A, Prime) */
/*
        <premaster secret> = (A * v^u) ^ b % N
*/
    BIGNUM *bn_b = NULL, *bn_verifier = NULL;
    BIGNUM *bn_prime = NULL, *bn_A = NULL, *bn_u = NULL, *bn_base, *bn_result;
    BN_CTX *bn_ctx;
    unsigned char* ptr;
    unsigned dlen;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!get_bn_from_bin(env, argv[0], &bn_verifier)
	|| !get_bn_from_bin(env, argv[1], &bn_b)
	|| !get_bn_from_bin(env, argv[2], &bn_u)
	|| !get_bn_from_bin(env, argv[3], &bn_A)
	|| !get_bn_from_bin(env, argv[4], &bn_prime))
    {
	if (bn_verifier) BN_free(bn_verifier);
	if (bn_b) BN_free(bn_b);
	if (bn_u) BN_free(bn_u);
	if (bn_A) BN_free(bn_A);
	if (bn_prime) BN_free(bn_prime);
	return enif_make_badarg(env);
    }

    bn_ctx = BN_CTX_new();
    bn_result = BN_new();

    /* check that A % N != 0 */
    BN_nnmod(bn_result, bn_A, bn_prime, bn_ctx);
    if (BN_is_zero(bn_result)) {
	BN_free(bn_b);
	BN_free(bn_verifier);
	BN_free(bn_prime);
	BN_free(bn_A);
	BN_CTX_free(bn_ctx);

	return atom_error;
    }

    /* (A * v^u) */
    bn_base = BN_new();
    BN_mod_exp(bn_base, bn_verifier, bn_u, bn_prime, bn_ctx);
    BN_mod_mul(bn_base, bn_A, bn_base, bn_prime, bn_ctx);

    /* (A * v^u) ^ b % N */
    BN_mod_exp(bn_result, bn_base, bn_b, bn_prime, bn_ctx);

    dlen = BN_num_bytes(bn_result);
    ptr = enif_make_new_binary(env, dlen, &ret);
    BN_bn2bin(bn_result, ptr);
    BN_free(bn_result);
    BN_CTX_free(bn_ctx);

    BN_free(bn_u);
    BN_free(bn_base);
    BN_free(bn_verifier);
    BN_free(bn_prime);
    BN_free(bn_A);
    BN_free(bn_b);
    return ret;
}

