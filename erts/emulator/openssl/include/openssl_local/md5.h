/*
 * Copyright 1995-2020 The OpenSSL Project Authors. All Rights Reserved.
 *
 * Licensed under the Apache License 2.0 (the "License").  You may not use
 * this file except in compliance with the License.  You can obtain a copy
 * in the file LICENSE in the source distribution or at
 * https://www.openssl.org/source/license.html
 */

#ifndef OPENSSL_MD5_H
# define OPENSSL_MD5_H
# pragma once

#if defined(ERLANG_OPENSSL_INTEGRATION)

#include "erl_md5.h"

#undef OPENSSL_NO_MD5
#undef OPENSSL_NO_DEPRECATED_3_0

#else  /* !defined(ERLANG_OPENSSL_INTEGRATION) */

# include <openssl/macros.h>
# ifndef OPENSSL_NO_DEPRECATED_3_0
#  define HEADER_MD5_H
# endif

# include <openssl/opensslconf.h>

#endif /* !defined(ERLANG_OPENSSL_INTEGRATION) */

# ifndef OPENSSL_NO_MD5
#if !defined(ERLANG_OPENSSL_INTEGRATION)
#  include <openssl/e_os2.h>
#endif
#  include <stddef.h>
#  ifdef  __cplusplus
extern "C" {
#  endif

#  define MD5_DIGEST_LENGTH 16

#  if !defined(OPENSSL_NO_DEPRECATED_3_0)
/*
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 * ! MD5_LONG has to be at least 32 bits wide.                     !
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 */
#   define MD5_LONG unsigned int

#   define MD5_CBLOCK      64
#   define MD5_LBLOCK      (MD5_CBLOCK/4)

typedef struct MD5state_st {
    MD5_LONG A, B, C, D;
    MD5_LONG Nl, Nh;
    MD5_LONG data[MD5_LBLOCK];
    unsigned int num;
} MD5_CTX;
#  endif
#  ifndef OPENSSL_NO_DEPRECATED_3_0
#    if defined(ERLANG_OPENSSL_INTEGRATION)
int MD5_INIT_FUNCTION_NAME(MD5_CTX *c);
int MD5_UPDATE_FUNCTION_NAME(MD5_CTX *c, const void *data, size_t len);
int MD5_FINAL_FUNCTION_NAME(unsigned char *md, MD5_CTX *c);
void MD5_TRANSFORM_FUNCTION_NAME(MD5_CTX *c, const unsigned char *b);
#    else /* !defined(ERLANG_OPENSSL_INTEGRATION) */
OSSL_DEPRECATEDIN_3_0 int MD5_Init(MD5_CTX *c);
OSSL_DEPRECATEDIN_3_0 int MD5_Update(MD5_CTX *c, const void *data, size_t len);
OSSL_DEPRECATEDIN_3_0 int MD5_Final(unsigned char *md, MD5_CTX *c);
OSSL_DEPRECATEDIN_3_0 unsigned char *MD5(const unsigned char *d, size_t n,
                                         unsigned char *md);
OSSL_DEPRECATEDIN_3_0 void MD5_Transform(MD5_CTX *c, const unsigned char *b);
#    endif /* !defined(ERLANG_OPENSSL_INTEGRATION) */
#  endif

#  ifdef  __cplusplus
}
#  endif
# endif

#endif
