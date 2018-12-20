#ifndef E_AEAD_H__
#define E_AEAD_H__ 1

#include "common.h"

ERL_NIF_TERM aead_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM aead_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_AEAD_H__ */
