#ifndef E_DIGEST_H__
#define E_DIGEST_H__ 1

#include "common.h"

struct digest_type_t {
    union {
	const char*  str;        /* before init, NULL for end-of-table */
	ERL_NIF_TERM atom;       /* after init, 'false' for end-of-table */
    }type;
    union {
	const EVP_MD* (*funcp)(void);  /* before init, NULL if notsup */
	const EVP_MD* p;               /* after init, NULL if notsup */
    }md;
};

void init_digest_types(ErlNifEnv* env);
struct digest_type_t* get_digest_type(ERL_NIF_TERM type);

#endif /* E_DIGEST_H__ */
