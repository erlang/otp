#ifndef E_PKEY_H__
#define E_PKEY_H__ 1

#include "common.h"

ERL_NIF_TERM pkey_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM pkey_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM pkey_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM privkey_to_pubkey_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_PKEY_H__ */
