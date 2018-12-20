#ifndef E_FIPS_H__
#define E_FIPS_H__ 1

#include "common.h"

ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM enable_fips_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_FIPS_H__ */
