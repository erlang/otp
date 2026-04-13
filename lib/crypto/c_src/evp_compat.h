/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2010-2025. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#pragma once

#include "openssl_config.h"

// This code activates under OpenSSL 1.1.0
#ifdef NEED_EVP_COMPATIBILITY_FUNCTIONS

// In OpenSSL 1.1.0, most structs are opaque. That means that the structs cannot be allocated as automatic
// variables on the C stack (because the size is unknown) and that it is necessary to use access functions. For backward
// compatibility to previous versions of OpenSSL, define on our versions of the new functions defined in 1.1.0 here,
// so that we don't have to sprinkle ifdefs throughout the code.

HMAC_CTX *HMAC_CTX_new();
void HMAC_CTX_free(HMAC_CTX *ctx);
HMAC_CTX *HMAC_CTX_new();
void HMAC_CTX_free(HMAC_CTX *ctx);

/* Renamed in 1.1.0 */
#define EVP_MD_CTX_new() EVP_MD_CTX_create()
#define EVP_MD_CTX_free(ctx) EVP_MD_CTX_destroy((ctx))

int DH_set0_key(DH *dh, BIGNUM *pub_key, BIGNUM *priv_key);
int DH_set0_pqg(DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g);
int DH_set_length(DH *dh, long length);
int DSA_set0_key(DSA *d, BIGNUM *pub_key, BIGNUM *priv_key);
int DSA_set0_pqg(DSA *d, BIGNUM *p, BIGNUM *q, BIGNUM *g);
int RSA_set0_crt_params(RSA *r, BIGNUM *dmp1, BIGNUM *dmq1, BIGNUM *iqmp);
int RSA_set0_factors(RSA *r, BIGNUM *p, BIGNUM *q);
int RSA_set0_key(RSA *r, BIGNUM *n, BIGNUM *e, BIGNUM *d);
void *BN_GENCB_get_arg(BN_GENCB *cb);
void DH_get0_key(const DH *dh, const BIGNUM **pub_key, const BIGNUM **priv_key);
void DH_get0_pqg(const DH *dh, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g);
void DSA_get0_key(const DSA *dsa, const BIGNUM **pub_key, const BIGNUM **priv_key);
void DSA_get0_pqg(const DSA *dsa, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g);
void RSA_get0_crt_params(const RSA *r, const BIGNUM **dmp1, const BIGNUM **dmq1, const BIGNUM **iqmp);
void RSA_get0_factors(const RSA *r, const BIGNUM **p, const BIGNUM **q);
void RSA_get0_key(const RSA *r, const BIGNUM **n, const BIGNUM **e, const BIGNUM **d);

#endif // NEED_EVP_COMPATIBILITY_FUNCTIONS
