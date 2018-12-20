#ifndef E_ALGORITHMS_H__
#define E_ALGORITHMS_H__ 1

#include "common.h"

void init_algorithms_types(ErlNifEnv* env);

ERL_NIF_TERM algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_ALGORITHMS_H__ */
