#ifndef E_EC_H__
#define E_EC_H__ 1

#include "common.h"

#if defined(HAVE_EC)
int get_ec_key(ErlNifEnv* env, ERL_NIF_TERM curve, ERL_NIF_TERM priv, ERL_NIF_TERM pub,
               EC_KEY** res);
int term2point(ErlNifEnv* env, ERL_NIF_TERM term, EC_GROUP *group, EC_POINT **pptr);
ERL_NIF_TERM make_badarg_maybe(ErlNifEnv* env);
#endif

ERL_NIF_TERM ec_key_generate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_EC_H__ */
