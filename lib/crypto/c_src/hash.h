#ifndef E_HASH_H__
#define E_HASH_H__ 1

#include "common.h"

#if OPENSSL_VERSION_NUMBER >= PACKED_OPENSSL_VERSION_PLAIN(1,0,0)
struct evp_md_ctx {
    EVP_MD_CTX* ctx;
};
extern ErlNifResourceType* evp_md_ctx_rtype;

void evp_md_ctx_dtor(ErlNifEnv* env, struct evp_md_ctx *ctx);
#endif

ERL_NIF_TERM hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hash_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hash_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM hash_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_HASH_H__ */
