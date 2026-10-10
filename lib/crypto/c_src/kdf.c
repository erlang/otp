/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright 2026 John Downey <jdowney@gmail.com>
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

#include "common.h"
#include "kdf.h"

#ifdef HAVE_KDF

#include <openssl/kdf.h>
#include <openssl/core_names.h>
#include "digest.h"

static struct kdf_type_t kdf_types[] = {
#ifdef HAVE_ARGON2
    {"argon2id", "ARGON2ID", 0, OSSL_KDF_PARAM_PASSWORD, ARGON2_kdf, 0, NULL},
    {"argon2i",  "ARGON2I",  0, OSSL_KDF_PARAM_PASSWORD, ARGON2_kdf, 0, NULL},
    {"argon2d",  "ARGON2D",  0, OSSL_KDF_PARAM_PASSWORD, ARGON2_kdf, 0, NULL},
#endif
    {"hkdf",     "HKDF",     0, OSSL_KDF_PARAM_KEY,      HKDF_kdf,   0, NULL},
    {"pbkdf2",   "PBKDF2",   0, OSSL_KDF_PARAM_PASSWORD, PBKDF2_kdf, 0, NULL},
    {"scrypt",   "SCRYPT",   0, OSSL_KDF_PARAM_PASSWORD, SCRYPT_kdf, 0, NULL},
    {"sskdf",    "SSKDF",    0, OSSL_KDF_PARAM_SECRET,   SSKDF_kdf,  0, NULL},

    /*==== End of list ==== */
    {NULL, NULL, 0, NULL, NO_kdf, 0, NULL}
};

void init_kdf_types(ErlNifEnv* env)
{
    struct kdf_type_t* kdf;

    for (kdf = kdf_types; kdf->str; kdf++) {
        kdf->atom = enif_make_atom(env, kdf->str);
        kdf->kdf = EVP_KDF_fetch(NULL, kdf->ossl_name, "");

#ifdef FIPS_SUPPORT
        {
            EVP_KDF *tmp = EVP_KDF_fetch(NULL, kdf->ossl_name, "fips=yes");
            if (tmp) {
                EVP_KDF_free(tmp);
                kdf->flags &= ~NO_FIPS_KDF;
            } else {
                kdf->flags |= NO_FIPS_KDF;
            }
        }
#endif
    }
}

static struct kdf_type_t* get_kdf_type(ERL_NIF_TERM type)
{
    struct kdf_type_t* kdf;

    for (kdf = kdf_types; kdf->str; kdf++) {
        if (type == kdf->atom) {
            return kdf;
        }
    }

    return NULL;
}

/* Reject the first option key the KDF does not recognize. */
static int check_options(ErlNifEnv* env, ERL_NIF_TERM map,
                         const ERL_NIF_TERM allowed[], size_t n_allowed,
                         ERL_NIF_TERM* return_term)
{
    ErlNifMapIterator iter;
    ERL_NIF_TERM key, value;

    if (!enif_map_iterator_create(env, map, &iter, ERL_NIF_MAP_ITERATOR_FIRST)) {
        *return_term = EXCP_BADARG_N(env, 3, "Not a map");
        return 0;
    }

    while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
        size_t i;
        int found = 0;

        for (i = 0; i < n_allowed; i++) {
            if (key == allowed[i]) {
                found = 1;
                break;
            }
        }

        if (!found) {
            char buffer[64];
            if (enif_snprintf(buffer, sizeof(buffer), "Unknown option: %T", key) > 0) {
                *return_term = EXCP_BADARG_N(env, 3, buffer);
            } else {
                *return_term = EXCP_BADARG_N(env, 3, "Unknown option");
            }

            enif_map_iterator_destroy(env, &iter);
            return 0;
        }

        enif_map_iterator_next(env, &iter);
    }

    enif_map_iterator_destroy(env, &iter);
    return 1;
}

/* Fetch a binary option from the map. */
static int get_bin_opt(ErlNifEnv* env, ERL_NIF_TERM map, ERL_NIF_TERM key,
                       const char* name, int required, ErlNifBinary* bin,
                       int* present, ERL_NIF_TERM* return_term)
{
    ERL_NIF_TERM value;

    if (!enif_get_map_value(env, map, key, &value)) {
        *present = 0;
        if (required) {
            char buf[64];
            enif_snprintf(buf, sizeof(buf), "Missing option: %s", name);
            *return_term = EXCP_BADARG_N(env, 3, buf);
            return 0;
        }

        return 1;
    }

    *present = 1;
    if (!enif_inspect_binary(env, value, bin)) {
        char buf[64];
        enif_snprintf(buf, sizeof(buf), "Option %s must be a binary", name);
        *return_term = EXCP_BADARG_N(env, 3, buf);
        return 0;
    }

    return 1;
}

/* Fetch a positive integer option from the map. A non-zero max sets an
   inclusive upper bound and 0 means no upper bound. */
static int get_uint_opt(ErlNifEnv* env, ERL_NIF_TERM map, ERL_NIF_TERM key,
                        const char* name, int required, uint64_t max,
                        uint64_t* val, int* present, ERL_NIF_TERM* return_term)
{
    ERL_NIF_TERM value;
    ErlNifUInt64 tmp;

    if (!enif_get_map_value(env, map, key, &value)) {
        *present = 0;
        if (required) {
            char buf[64];
            enif_snprintf(buf, sizeof(buf), "Missing option: %s", name);
            *return_term = EXCP_BADARG_N(env, 3, buf);
            return 0;
        }
        return 1;
    }

    *present = 1;
    if (!enif_get_uint64(env, value, &tmp) || tmp < 1) {
        char buffer[64];
        enif_snprintf(buffer, sizeof(buffer),
                      "Option %s must be a positive integer", name);
        *return_term = EXCP_BADARG_N(env, 3, buffer);
        return 0;
    }

    if (max && tmp > max) {
        char buffer[64];
        enif_snprintf(buffer, sizeof(buffer), "Option %s is too large", name);
        *return_term = EXCP_BADARG_N(env, 3, buffer);
        return 0;
    }

    *val = (uint64_t)tmp;
    return 1;
}

/* Fetch the digest option as an OSSL digest name. If md is non-NULL it
   receives the resolved EVP_MD. require_flags is a mask of digest_type_t
   flags the digest must carry (0 for no extra requirement). */
static int get_digest_opt(ErlNifEnv* env, ERL_NIF_TERM map,
                          const char** name, const EVP_MD** md,
                          unsigned require_flags, ERL_NIF_TERM* return_term)
{
    ERL_NIF_TERM value;
    struct digest_type_t* digest;

    if (!enif_get_map_value(env, map, atom_digest, &value)) {
        *return_term = EXCP_BADARG_N(env, 3, "Missing option: digest");
        return 0;
    }

    digest = get_digest_type(value);
    if (digest == NULL) {
        *return_term = EXCP_BADARG_N(env, 3, "Bad digest type");
        return 0;
    }

    if (DIGEST_FORBIDDEN_IN_FIPS(digest)) {
        *return_term = EXCP_NOTSUP_N(env, 3, "Digest type not supported in FIPS");
        return 0;
    }

    if (digest->md.p == NULL) {
        *return_term = EXCP_NOTSUP_N(env, 3, "Unsupported digest type");
        return 0;
    }

    /* XOFs (shake128/shake256) are not usable as the HMAC/hash primitive for
       these KDFs; OpenSSL otherwise fails the derive with an opaque error. */
    if (digest->xof_default_length != 0) {
        *return_term = EXCP_BADARG_N(env, 3, "XOF digest not supported");
        return 0;
    }

    if ((digest->flags & require_flags) != require_flags) {
        *return_term = EXCP_BADARG_N(env, 3, "Not eligible digest type");
        return 0;
    }

    *name = digest->str_v3;
    if (md) {
        *md = digest->md.p;
    }

    return 1;
}

static ERL_NIF_TERM kdf_derive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, KeyMaterial, KeyLen, Options) */
    struct kdf_type_t* kdf;
    OSSL_PARAM params[8];
    ErlNifBinary key_material, salt, info, out;
    int out_allocated = 0;
    ERL_NIF_TERM ret;
    uint64_t iterations, scn, scr, scp, maxmem;
    ErlNifUInt64 keylen;
    size_t n = 0;
#ifdef HAVE_ARGON2
    uint64_t memory, parallelism;
    ErlNifBinary secret, ad;
#endif

    int present;
    int mode_val = EVP_KDF_HKDF_MODE_EXTRACT_AND_EXPAND;
    const char* digest_name;
    const EVP_MD* hkdf_md = NULL;
    EVP_KDF_CTX* ctx = NULL;

    ASSERT(argc == 4);

    if ((kdf = get_kdf_type(argv[0])) == NULL) {
        return EXCP_BADARG_N(env, 0, "Unknown KDF");
    }

    if (kdf->kdf == NULL) {
        return EXCP_NOTSUP(env, "KDF not supported by the cryptolib");
    }

    if (KDF_FORBIDDEN_IN_FIPS(kdf)) {
        return EXCP_NOTSUP(env, "KDF not supported in FIPS mode");
    }

    if (!enif_inspect_binary(env, argv[1], &key_material)) {
        return EXCP_BADARG_N(env, 1, "Key material must be a binary");
    }

    if (!enif_get_uint64(env, argv[2], &keylen) || keylen < 1 || keylen > INT_MAX) {
        return EXCP_BADARG_N(env, 2, "Bad key length");
    }

    if (!enif_is_map(env, argv[3])) {
        return EXCP_BADARG_N(env, 3, "Not a map");
    }

    params[n++] =
        OSSL_PARAM_construct_octet_string(kdf->input_name, key_material.data, key_material.size);

    switch (kdf->kind) {

#ifdef HAVE_ARGON2
    case ARGON2_kdf: {
        const ERL_NIF_TERM allowed[] = {atom_salt, atom_memory, atom_iterations,
                                        atom_parallelism, atom_secret, atom_ad};
        /* RFC 9106 (Argon2) parameter bounds, matching the limits OpenSSL
           enforces, so bad input yields a clear badarg here instead of an opaque
           derive failure:
             - tag length  >= 4            (RFC 9106 3.1 T; ARGON2_MIN_OUTLEN)
             - salt        >= 8 bytes      (RFC 9106 3.1 S; ARGON2_MIN_SALT_LENGTH)
             - parallelism <= 2^24 - 1     (RFC 9106 3.1 p; ARGON2_MAX_LANES)
             - memory      >= 8 * lanes    (RFC 9106 3.1 m) */
        if (keylen < 4) {               /* RFC 9106 3.1: tag length T >= 4 */
            ret = EXCP_BADARG_N(env, 2, "Argon2 KeyLen must be at least 4");
            goto done;
        }

        if (!check_options(env, argv[3], allowed, 6, &ret)) {
            goto done;
        }

        if (!get_bin_opt(env, argv[3], atom_salt, "salt", 1, &salt, &present, &ret)) {
            goto done;
        }

        if (salt.size < 8) {            /* RFC 9106 3.1: salt S >= 8 bytes */
            ret = EXCP_BADARG_N(env, 3, "Option salt must be at least 8 bytes");
            goto done;
        }
        params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SALT,
                                                        salt.data, salt.size);

        if (!get_uint_opt(env, argv[3], atom_memory, "memory", 1, UINT32_MAX, &memory, &present, &ret)) {
            goto done;
        }
        params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_ARGON2_MEMCOST, &memory);

        if (!get_uint_opt(env, argv[3], atom_iterations, "iterations", 1, UINT32_MAX, &iterations, &present, &ret)) {
            goto done;
        }
        params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_ITER, &iterations);

        if (!get_uint_opt(env, argv[3], atom_parallelism, "parallelism", 1,
                          0xFFFFFF /* RFC 9106 3.1: p <= 2^24 - 1 (ARGON2_MAX_LANES) */,
                          &parallelism, &present, &ret)) {
            goto done;
        }
        params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_ARGON2_LANES, &parallelism);

        if (memory < 8 * parallelism) { /* RFC 9106 3.1: m >= 8 * p */
            ret = EXCP_BADARG_N(env, 3,
                                "Option memory must be at least 8 times parallelism");
            goto done;
        }

        if (!get_bin_opt(env, argv[3], atom_secret, "secret", 0, &secret, &present, &ret)) {
            goto done;
        }
        if (present) {
            if (secret.size > UINT32_MAX) { /* RFC 9106 3.1: secret K <= 2^32 - 1 */
                ret = EXCP_BADARG_N(env, 3, "Option secret is too large");
                goto done;
            }
            params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SECRET,
                                                            secret.data, secret.size);
        }

        if (!get_bin_opt(env, argv[3], atom_ad, "ad", 0, &ad, &present, &ret)) {
            goto done;
        }
        if (present) {
            if (ad.size > UINT32_MAX) {     /* RFC 9106 3.1: associated data X <= 2^32 - 1 */
                ret = EXCP_BADARG_N(env, 3, "Option ad is too large");
                goto done;
            }
            params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_ARGON2_AD,
                                                            ad.data, ad.size);
        }

        break;
    }
#endif /* HAVE_ARGON2 */

    case HKDF_kdf: {
        const ERL_NIF_TERM allowed[] = {atom_digest, atom_salt, atom_info, atom_mode};
        ERL_NIF_TERM mode;

        if (!check_options(env, argv[3], allowed, 4, &ret)) {
            goto done;
        }

        if (!get_digest_opt(env, argv[3], &digest_name, &hkdf_md, 0, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_utf8_string(OSSL_KDF_PARAM_DIGEST,
                                                       (char*)digest_name, 0);

        /* The salt is only used in the extract step, so expand_only should
         * reject it rather than silently drop it. */
        if (enif_get_map_value(env, argv[3], atom_mode, &mode)) {
            if (mode == atom_extract_and_expand) {
                mode_val = EVP_KDF_HKDF_MODE_EXTRACT_AND_EXPAND;
            } else if (mode == atom_extract_only) {
                mode_val = EVP_KDF_HKDF_MODE_EXTRACT_ONLY;
                if (keylen != (ErlNifUInt64)EVP_MD_get_size(hkdf_md)) {
                    ret = EXCP_BADARG_N(env, 2,
                        "extract_only KeyLen must equal the digest size");
                    goto done;
                }
            } else if (mode == atom_expand_only) {
                mode_val = EVP_KDF_HKDF_MODE_EXPAND_ONLY;
            } else {
                ret = EXCP_BADARG_N(env, 3, "Bad hkdf mode");
                goto done;
            }

            params[n++] = OSSL_PARAM_construct_int(OSSL_KDF_PARAM_MODE, &mode_val);
        }

        /* HKDF-Expand can emit at most 255 * HashLen bytes (RFC 5869 2.3).
           extract_only is pinned to the digest size above; every other mode
           (default == extract_and_expand, and expand_only) expands. */
        {
            int md_size = EVP_MD_get_size(hkdf_md);
            if (mode_val != EVP_KDF_HKDF_MODE_EXTRACT_ONLY
                && md_size > 0
                && keylen > (ErlNifUInt64)255 * md_size) {
                ret = EXCP_BADARG_N(env, 2,
                    "KeyLen must be at most 255 times the digest size");
                goto done;
            }
        }

        if (!get_bin_opt(env, argv[3], atom_salt, "salt", 0, &salt, &present, &ret)) {
            goto done;
        }

        if (present) {
            if (mode_val == EVP_KDF_HKDF_MODE_EXPAND_ONLY) {
                ret = EXCP_BADARG_N(env, 3,
                                    "Option salt not used with mode => expand_only");
                goto done;
            }

            params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SALT,
                                                            salt.data, salt.size);
        }

        if (!get_bin_opt(env, argv[3], atom_info, "info", 0, &info, &present, &ret)) {
            goto done;
        }

        if (present) {
            if (mode_val == EVP_KDF_HKDF_MODE_EXTRACT_ONLY) {
                ret = EXCP_BADARG_N(env, 3,
                                    "Option info not used with mode => extract_only");
                goto done;
            }

            params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_INFO,
                                                            info.data, info.size);
        }

        break;
    }

    case PBKDF2_kdf: {
        const ERL_NIF_TERM allowed[] = {atom_digest, atom_salt, atom_iterations};
        if (!check_options(env, argv[3], allowed, 3, &ret)) {
            goto done;
        }

        if (!get_digest_opt(env, argv[3], &digest_name, NULL,
                            PBKDF2_ELIGIBLE_DIGEST, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_utf8_string(OSSL_KDF_PARAM_DIGEST,
                                                       (char*)digest_name, 0);

        if (!get_bin_opt(env, argv[3], atom_salt, "salt", 1, &salt, &present, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SALT,
                                                        salt.data, salt.size);

        if (!get_uint_opt(env, argv[3], atom_iterations, "iterations", 1, 0, &iterations, &present, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_ITER, &iterations);
        break;
    }

    case SCRYPT_kdf: {
        const ERL_NIF_TERM allowed[] = {atom_salt, atom_n, atom_r, atom_p, atom_maxmem};
        if (!check_options(env, argv[3], allowed, 5, &ret)) {
            goto done;
        }

        if (!get_bin_opt(env, argv[3], atom_salt, "salt", 1, &salt, &present, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SALT, salt.data, salt.size);
        if (!get_uint_opt(env, argv[3], atom_n, "n", 1, 0, &scn, &present, &ret)) {
            goto done;
        }

        if (scn < 2 || (scn & (scn - 1)) != 0) {
            ret = EXCP_BADARG_N(env, 3,
                                "Option n must be a power of two greater than 1");
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_SCRYPT_N, &scn);

        if (!get_uint_opt(env, argv[3], atom_r, "r", 1, UINT32_MAX, &scr, &present, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_SCRYPT_R, &scr);

        if (!get_uint_opt(env, argv[3], atom_p, "p", 1, UINT32_MAX, &scp, &present, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_SCRYPT_P, &scp);

        /* Pass maxmem through only when supplied; otherwise OpenSSL applies its
           own default and enforces the n/r/p memory limit itself. */
        if (!get_uint_opt(env, argv[3], atom_maxmem, "maxmem", 0, 0, &maxmem, &present, &ret)) {
            goto done;
        }

        if (present) {
            params[n++] = OSSL_PARAM_construct_uint64(OSSL_KDF_PARAM_SCRYPT_MAXMEM, &maxmem);
        }

        break;
    }

    case SSKDF_kdf: {
        const ERL_NIF_TERM allowed[] = {atom_digest, atom_mac, atom_salt, atom_info};
        int has_mac = 0;
        ERL_NIF_TERM mac;
        if (!check_options(env, argv[3], allowed, 4, &ret)) {
            goto done;
        }

        if (!get_digest_opt(env, argv[3], &digest_name, NULL, 0, &ret)) {
            goto done;
        }

        params[n++] = OSSL_PARAM_construct_utf8_string(OSSL_KDF_PARAM_DIGEST,
                                                       (char*)digest_name, 0);

        if (enif_get_map_value(env, argv[3], atom_mac, &mac)) {
            if (mac != atom_hmac) {
                ret = EXCP_BADARG_N(env, 3, "Bad mac type");
                goto done;
            }
            has_mac = 1;
            params[n++] = OSSL_PARAM_construct_utf8_string(OSSL_KDF_PARAM_MAC,
                (char*)"HMAC", 0);
        }

        if (!get_bin_opt(env, argv[3], atom_salt, "salt", 0, &salt, &present, &ret)) {
            goto done;
        }

        if (present) {
            /* SSKDF only uses salt as the HMAC/KMAC key */
            if (!has_mac) {
                ret = EXCP_BADARG_N(env, 3, "Option salt requires mac => hmac");
                goto done;
            }

            params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_SALT,
                                                            salt.data, salt.size);
        }

        if (!get_bin_opt(env, argv[3], atom_info, "info", 0, &info, &present, &ret)) {
            goto done;
        }

        if (present) {
            params[n++] = OSSL_PARAM_construct_octet_string(OSSL_KDF_PARAM_INFO,
                                                            info.data, info.size);
        }

        break;
    }

    default:
        return EXCP_NOTSUP(env, "Unsupported KDF");
    }

    params[n] = OSSL_PARAM_construct_end();

    if ((ctx = EVP_KDF_CTX_new(kdf->kdf)) == NULL) {
        ret = EXCP_ERROR(env, "Can't create KDF context");
        goto done;
    }

    if (!enif_alloc_binary((size_t)keylen, &out)) {
        ret = EXCP_ERROR(env, "Can't allocate binary");
        goto done;
    }

    out_allocated = 1;

    if (EVP_KDF_derive(ctx, out.data, (size_t)keylen, params) <= 0) {
        ret = EXCP_ERROR(env, "Low-level call failed");
        goto done;
    }

    out_allocated = 0;
    ret = enif_make_binary(env, &out);

 done:
    if (out_allocated) {
        enif_release_binary(&out);
    }

    if (ctx) {
        EVP_KDF_CTX_free(ctx);
    }

    return ret;
}

/* True if the caller-controlled binary inputs (the key material plus every
   binary-valued option in the map) total no more than MAX_BYTES_TO_NIF. Used to
   keep long HMAC/hash passes off the normal schedulers. */
static int kdf_is_small_input(ErlNifEnv* env, ERL_NIF_TERM key_material, ERL_NIF_TERM map)
{
    ErlNifBinary bin;
    size_t total = 0, map_size;
    ErlNifMapIterator iter;

    /* A valid options map has only a handful of keys (at most 4 for any
       fast-path KDF); a larger map is invalid input and badargs anyway. Bound
       the size here so the scan below can't run unbounded on a normal
       scheduler. Oversized maps fall through to the dirty scheduler. */
    if (!enif_get_map_size(env, map, &map_size) || map_size > 8) {
        return 0;
    }

    if (enif_inspect_binary(env, key_material, &bin)) {
        total += bin.size;
    }

    if (total <= MAX_BYTES_TO_NIF
        && enif_map_iterator_create(env, map, &iter, ERL_NIF_MAP_ITERATOR_FIRST)) {
        ERL_NIF_TERM key, value;
        while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
            if (enif_inspect_binary(env, value, &bin)) {
                total += bin.size;
                if (total > MAX_BYTES_TO_NIF) {
                    break;
                }
            }

            enif_map_iterator_next(env, &iter);
        }

        enif_map_iterator_destroy(env, &iter);
    }

    return total <= MAX_BYTES_TO_NIF;
}

#else /* !HAVE_KDF */

void init_kdf_types(ErlNifEnv* env)
{
    (void)env;
}

#endif /* HAVE_KDF */

ERL_NIF_TERM kdf_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, KeyMaterial, KeyLen, Options) */
#ifdef HAVE_KDF
    struct kdf_type_t* kdfp;

    ASSERT(argc == 4);

    if ((kdfp = get_kdf_type(argv[0])) == NULL) {
        return EXCP_BADARG_N(env, 0, "Unknown KDF");
    }

    switch (kdfp->kind) {
    case HKDF_kdf:
    case SSKDF_kdf: {
        /* HKDF-Expand runs one HMAC per output block, and SSKDF re-hashes the
           full secret+info for every output block, so for both the work grows
           with keylen. Cap keylen (a few blocks at most) to keep small
           derivations on a normal scheduler; the key material and
           salt/secret/info are caller-controlled, hence the input-size cap.
           Larger outputs fall through to the dirty scheduler. */
        ErlNifUInt64 keylen;
        if (enif_get_uint64(env, argv[2], &keylen)
            && keylen <= 64
            && kdf_is_small_input(env, argv[1], argv[3])) {
            return kdf_derive(env, argc, argv);
        }

        break;
    }

    case PBKDF2_kdf: {
        /* A small iteration count, key length, and inputs keep the work short
           enough for a normal scheduler. */
        ERL_NIF_TERM iter_term;
        ErlNifUInt64 iter, keylen;

        if (enif_get_uint64(env, argv[2], &keylen)
            && keylen <= 64
            && enif_get_map_value(env, argv[3], atom_iterations, &iter_term)
            && enif_get_uint64(env, iter_term, &iter)
            && iter <= 100
            && kdf_is_small_input(env, argv[1], argv[3])) {
            return kdf_derive(env, argc, argv);
        }
        break;
    }

    default:
        break;
    }

    /* use a dirty CPU scheduler otherwise */
    return enif_schedule_nif(env, "kdf_derive",
                             ERL_NIF_DIRTY_JOB_CPU_BOUND,
                             kdf_derive, argc, argv);
#else
    return EXCP_NOTSUP(env, "Unsupported: requires OpenSSL 3.0 or later");
#endif
}

ERL_NIF_TERM kdf_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* () */
#ifdef HAVE_KDF
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    struct kdf_type_t* kdf;

    for (kdf = kdf_types; kdf->str; kdf++) {
        if (kdf->kdf == NULL) {
            continue;
        }

        if (KDF_FORBIDDEN_IN_FIPS(kdf)) {
            continue;
        }

        ret = enif_make_list_cell(env, kdf->atom, ret);
    }
    return ret;
#else

    return enif_make_list(env, 0);
#endif
}
