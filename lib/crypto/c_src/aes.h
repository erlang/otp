#ifndef E_AES_H__
#define E_AES_H__ 1

#include "common.h"

ERL_NIF_TERM aes_cfb_8_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM aes_cfb_128_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM aes_ige_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef HAVE_GCM_EVP_DECRYPT_BUG
ERL_NIF_TERM aes_gcm_decrypt_NO_EVP(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#endif

#endif /* E_AES_H__ */
