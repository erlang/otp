#ifndef E_DH_H__
#define E_DH_H__ 1

#include "common.h"

ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_DH_H__ */
