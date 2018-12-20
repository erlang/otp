#ifndef E_INFO_H__
#define E_INFO_H__ 1

#include "common.h"

#ifdef HAVE_DYNAMIC_CRYPTO_LIB
extern char *crypto_callback_name;

int change_basename(ErlNifBinary* bin, char* buf, int bufsz, const char* newfile);
void error_handler(void* null, const char* errstr);
#endif

ERL_NIF_TERM info_lib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_INFO_H__ */
