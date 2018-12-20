#ifndef E_ECDH_H__
#define E_ECDH_H__ 1

#include "common.h"

ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_ECDH_H__ */
