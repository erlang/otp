#ifndef E_CIPHER_H__
#define E_CIPHER_H__ 1

#include "common.h"

struct cipher_type_t {
    union {
	const char* str;    /* before init */
	ERL_NIF_TERM atom;  /* after init */
    }type;
    union {
	const EVP_CIPHER* (*funcp)(void); /* before init, NULL if notsup */
	const EVP_CIPHER* p;              /* after init, NULL if notsup */
    }cipher;
    const size_t key_len;      /* != 0 to also match on key_len */
};

#ifdef HAVE_EVP_AES_CTR
extern ErlNifResourceType* evp_cipher_ctx_rtype;
struct evp_cipher_ctx {
    EVP_CIPHER_CTX* ctx;
};

void evp_cipher_ctx_dtor(ErlNifEnv* env, struct evp_cipher_ctx* ctx);
#endif

void init_cipher_types(ErlNifEnv* env);
struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len);

#endif /* E_CIPHER_H__ */
