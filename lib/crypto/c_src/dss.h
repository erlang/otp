#ifndef E_DSS_H__
#define E_DSS_H__ 1

#include "common.h"

int get_dss_private_key(ErlNifEnv* env, ERL_NIF_TERM key, DSA *dsa);
int get_dss_public_key(ErlNifEnv* env, ERL_NIF_TERM key, DSA *dsa);

#endif /* E_DSS_H__ */
