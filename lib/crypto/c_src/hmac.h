#ifndef E_HMAC_H__
#define E_HMAC_H__ 1

#include "common.h"

struct hmac_context
{
    ErlNifMutex* mtx;
    int alive;
    HMAC_CTX* ctx;
};

extern ErlNifResourceType* hmac_context_rtype;
void hmac_context_dtor(ErlNifEnv* env, struct hmac_context*);

ERL_NIF_TERM hmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hmac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hmac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hmac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_HMAC_H__ */
