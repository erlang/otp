#ifndef E_BN_H__
#define E_BN_H__ 1

#include "common.h"

ERL_NIF_TERM bin_from_bn(ErlNifEnv* env, const BIGNUM *bn);
ERL_NIF_TERM mod_exp_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef HAVE_EC
ERL_NIF_TERM bn2term(ErlNifEnv* env, const BIGNUM *bn);
#endif

int get_bn_from_mpint(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp);
int get_bn_from_bin(ErlNifEnv* env, ERL_NIF_TERM term, BIGNUM** bnp);

#endif /* E_BN_H__ */
