#ifndef E_RSA_H__
#define E_RSA_H__ 1

#include "common.h"

int get_rsa_public_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa);
int get_rsa_private_key(ErlNifEnv* env, ERL_NIF_TERM key, RSA *rsa);

ERL_NIF_TERM rsa_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_RSA_H__ */
