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

#include "evp_compat.h"

#if OPENSSL_VERSION_NUMBER < PACKED_OPENSSL_VERSION_PLAIN(1,1,0)

extern "C" HMAC_CTX *HMAC_CTX_new() {
    const auto ctx = static_cast<HMAC_CTX *>(CRYPTO_malloc(sizeof(HMAC_CTX), __FILE__, __LINE__));
    if (!ctx)
        return nullptr;

    HMAC_CTX_init(ctx);
    return ctx;
}

extern "C" void HMAC_CTX_free(HMAC_CTX *ctx) {
    if (ctx == nullptr)
        return;

    HMAC_CTX_cleanup(ctx);
    CRYPTO_free(ctx);
}

extern "C" void *BN_GENCB_get_arg(const BN_GENCB *cb) {
    return cb->arg;
}

extern "C" int RSA_set0_key(RSA *r, BIGNUM *n, BIGNUM *e, BIGNUM *d) {
    r->n = n;
    r->e = e;
    r->d = d;
    return 1;
}

extern "C" void RSA_get0_key(const RSA *r, const BIGNUM **n, const BIGNUM **e, const BIGNUM **d) {
    *n = r->n;
    *e = r->e;
    *d = r->d;
}

extern "C" int RSA_set0_factors(RSA *r, BIGNUM *p, BIGNUM *q) {
    r->p = p;
    r->q = q;
    return 1;
}

extern "C" void RSA_get0_factors(const RSA *r, const BIGNUM **p, const BIGNUM **q) {
    *p = r->p;
    *q = r->q;
}

extern "C" int RSA_set0_crt_params(RSA *r, BIGNUM *dmp1, BIGNUM *dmq1, BIGNUM *iqmp) {
    r->dmp1 = dmp1;
    r->dmq1 = dmq1;
    r->iqmp = iqmp;
    return 1;
}

extern "C" void RSA_get0_crt_params(const RSA *r, const BIGNUM **dmp1, const BIGNUM **dmq1, const BIGNUM **iqmp) {
    *dmp1 = r->dmp1;
    *dmq1 = r->dmq1;
    *iqmp = r->iqmp;
}

extern "C" int DSA_set0_key(DSA *d, BIGNUM *pub_key, BIGNUM *priv_key) {
    d->pub_key = pub_key;
    d->priv_key = priv_key;
    return 1;
}

extern "C" int DSA_set0_pqg(DSA *d, BIGNUM *p, BIGNUM *q, BIGNUM *g) {
    d->p = p;
    d->q = q;
    d->g = g;
    return 1;
}

extern "C" void DSA_get0_pqg(const DSA *dsa, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g) {
    *p = dsa->p;
    *q = dsa->q;
    *g = dsa->g;
}

extern "C" void DSA_get0_key(const DSA *dsa, const BIGNUM **pub_key, const BIGNUM **priv_key) {
    if (pub_key)
        *pub_key = dsa->pub_key;

    if (priv_key)
        *priv_key = dsa->priv_key;
}

extern "C" int DH_set0_key(DH *dh, BIGNUM *pub_key, BIGNUM *priv_key);
extern "C" int DH_set0_pqg(DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g);
extern "C" int DH_set_length(DH *dh, long length);
extern "C" void DH_get0_pqg(const DH *dh, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g);
extern "C" void DH_get0_key(const DH *dh, const BIGNUM **pub_key, const BIGNUM **priv_key);

extern "C" int DH_set0_key(DH *dh, BIGNUM *pub_key, BIGNUM *priv_key) {
    dh->pub_key = pub_key;
    dh->priv_key = priv_key;
    return 1;
}

extern "C" int DH_set0_pqg(DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g) {
    dh->p = p;
    dh->q = q;
    dh->g = g;
    return 1;
}

extern "C" int DH_set_length(DH *dh, long length) {
    dh->length = length;
    return 1;
}

extern "C" void DH_get0_pqg(const DH *dh, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g) {
    *p = dh->p;
    *q = dh->q;
    *g = dh->g;
}

extern "C" void DH_get0_key(const DH *dh, const BIGNUM **pub_key, const BIGNUM **priv_key) {
    if (pub_key)
        *pub_key = dh->pub_key;

    if (priv_key)
        *priv_key = dh->priv_key;
}

#endif // OPENSSL VERSION < 1.0.0