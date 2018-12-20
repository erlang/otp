#ifndef E_CHACHA20_H__
#define E_CHACHA20_H__ 1

#include "common.h"

ERL_NIF_TERM chacha20_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM chacha20_stream_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_CHACHA20_H__ */
