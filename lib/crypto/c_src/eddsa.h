#ifndef E_EDDSA_H__
#define E_EDDSA_H__ 1

#include "common.h"

#ifdef HAVE_EDDSA
int get_eddsa_key(ErlNifEnv* env, int public, ERL_NIF_TERM key, EVP_PKEY **pkey);
#endif

#endif /* E_EDDSA_H__ */
