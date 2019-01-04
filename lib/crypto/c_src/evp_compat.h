/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

#ifndef E_EVP_COMPAT_H__
#define E_EVP_COMPAT_H__ 1

/*
 * In OpenSSL 1.1.0, most structs are opaque. That means that
 * the structs cannot be allocated as automatic variables on the
 * C stack (because the size is unknown) and that it is necessary
 * to use access functions.
 *
 * For backward compatibility to previous versions of OpenSSL, define
 * on our versions of the new functions defined in 1.1.0 here, so that
 * we don't have to sprinkle ifdefs throughout the code.
 */

static INLINE HMAC_CTX *HMAC_CTX_new(void);
static INLINE void HMAC_CTX_free(HMAC_CTX *ctx);

static INLINE HMAC_CTX *HMAC_CTX_new()
{
    HMAC_CTX *ctx;

    if ((ctx = CRYPTO_malloc(sizeof(HMAC_CTX), __FILE__, __LINE__)) == NULL)
        return NULL;

    HMAC_CTX_init(ctx);
    return ctx;
}

static INLINE void HMAC_CTX_free(HMAC_CTX *ctx)
{
    if (ctx == NULL)
        return;

    HMAC_CTX_cleanup(ctx);
    CRYPTO_free(ctx);
}

/* Renamed in 1.1.0 */
#define EVP_MD_CTX_new() EVP_MD_CTX_create()
#define EVP_MD_CTX_free(ctx) EVP_MD_CTX_destroy((ctx))

static INLINE void *BN_GENCB_get_arg(BN_GENCB *cb);

static INLINE void *BN_GENCB_get_arg(BN_GENCB *cb)
{
    return cb->arg;
}

static INLINE int RSA_set0_key(RSA *r, BIGNUM *n, BIGNUM *e, BIGNUM *d);
static INLINE void RSA_get0_key(const RSA *r, const BIGNUM **n, const BIGNUM **e, const BIGNUM **d);
static INLINE int RSA_set0_factors(RSA *r, BIGNUM *p, BIGNUM *q);
static INLINE void RSA_get0_factors(const RSA *r, const BIGNUM **p, const BIGNUM **q);
static INLINE int RSA_set0_crt_params(RSA *r, BIGNUM *dmp1, BIGNUM *dmq1, BIGNUM *iqmp);
static INLINE void RSA_get0_crt_params(const RSA *r, const BIGNUM **dmp1, const BIGNUM **dmq1, const BIGNUM **iqmp);

static INLINE int RSA_set0_key(RSA *r, BIGNUM *n, BIGNUM *e, BIGNUM *d)
{
    r->n = n;
    r->e = e;
    r->d = d;
    return 1;
}

static INLINE void RSA_get0_key(const RSA *r, const BIGNUM **n, const BIGNUM **e, const BIGNUM **d)
{
    *n = r->n;
    *e = r->e;
    *d = r->d;
}

static INLINE int RSA_set0_factors(RSA *r, BIGNUM *p, BIGNUM *q)
{
    r->p = p;
    r->q = q;
    return 1;
}

static INLINE void RSA_get0_factors(const RSA *r, const BIGNUM **p, const BIGNUM **q)
{
    *p = r->p;
    *q = r->q;
}

static INLINE int RSA_set0_crt_params(RSA *r, BIGNUM *dmp1, BIGNUM *dmq1, BIGNUM *iqmp)
{
    r->dmp1 = dmp1;
    r->dmq1 = dmq1;
    r->iqmp = iqmp;
    return 1;
}

static INLINE void RSA_get0_crt_params(const RSA *r, const BIGNUM **dmp1, const BIGNUM **dmq1, const BIGNUM **iqmp)
{
    *dmp1 = r->dmp1;
    *dmq1 = r->dmq1;
    *iqmp = r->iqmp;
}

static INLINE int DSA_set0_key(DSA *d, BIGNUM *pub_key, BIGNUM *priv_key);
static INLINE int DSA_set0_pqg(DSA *d, BIGNUM *p, BIGNUM *q, BIGNUM *g);
static INLINE void DSA_get0_pqg(const DSA *dsa,
			       const BIGNUM **p, const BIGNUM **q, const BIGNUM **g);
static INLINE void DSA_get0_key(const DSA *dsa,
			       const BIGNUM **pub_key, const BIGNUM **priv_key);

static INLINE int DSA_set0_key(DSA *d, BIGNUM *pub_key, BIGNUM *priv_key)
{
    d->pub_key = pub_key;
    d->priv_key = priv_key;
    return 1;
}

static INLINE int DSA_set0_pqg(DSA *d, BIGNUM *p, BIGNUM *q, BIGNUM *g)
{
    d->p = p;
    d->q = q;
    d->g = g;
    return 1;
}

static INLINE void
DSA_get0_pqg(const DSA *dsa, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g)
{
    *p = dsa->p;
    *q = dsa->q;
    *g = dsa->g;
}

static INLINE void
DSA_get0_key(const DSA *dsa, const BIGNUM **pub_key, const BIGNUM **priv_key)
{
    if (pub_key)
        *pub_key = dsa->pub_key;

    if (priv_key)
        *priv_key = dsa->priv_key;
}



static INLINE int DH_set0_key(DH *dh, BIGNUM *pub_key, BIGNUM *priv_key);
static INLINE int DH_set0_pqg(DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g);
static INLINE int DH_set_length(DH *dh, long length);
static INLINE void DH_get0_pqg(const DH *dh,
			       const BIGNUM **p, const BIGNUM **q, const BIGNUM **g);
static INLINE void DH_get0_key(const DH *dh,
			       const BIGNUM **pub_key, const BIGNUM **priv_key);

static INLINE int DH_set0_key(DH *dh, BIGNUM *pub_key, BIGNUM *priv_key)
{
    dh->pub_key = pub_key;
    dh->priv_key = priv_key;
    return 1;
}

static INLINE int DH_set0_pqg(DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g)
{
    dh->p = p;
    dh->q = q;
    dh->g = g;
    return 1;
}

static INLINE int DH_set_length(DH *dh, long length)
{
    dh->length = length;
    return 1;
}



static INLINE void
DH_get0_pqg(const DH *dh, const BIGNUM **p, const BIGNUM **q, const BIGNUM **g)
{
    *p = dh->p;
    *q = dh->q;
    *g = dh->g;
}

static INLINE void
DH_get0_key(const DH *dh, const BIGNUM **pub_key, const BIGNUM **priv_key)
{
    if (pub_key)
        *pub_key = dh->pub_key;

    if (priv_key)
        *priv_key = dh->priv_key;
}

#endif /* E_EVP_COMPAT_H__ */
