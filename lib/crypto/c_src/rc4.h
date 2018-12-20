#ifndef E_RC4_H__
#define E_RC4_H__ 1

#include "common.h"

ERL_NIF_TERM rc4_set_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM rc4_encrypt_with_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_RC4_H__ */
