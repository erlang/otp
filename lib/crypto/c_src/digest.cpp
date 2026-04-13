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

#include "digest.h"

static digest_type_t digest_types[] = {
#ifdef HAVE_MD4
        digest_type_t("md4", "MD4", &EVP_md4).no_fips(),
#else
        digest_type_t("md4", "MD4").no_fips(),
#endif

#ifdef HAVE_MD5
        digest_type_t("md5", "MD5", &EVP_md5).no_fips(),
#else
        digest_type_t("md5", "MD5").no_fips(),
#endif

#ifdef HAVE_RIPEMD160
        digest_type_t("ripemd160", "RIPEMD160", &EVP_ripemd160).no_fips(),
#else
        digest_type_t("ripemd160", "RIPEMD160").no_fips(),
#endif

        digest_type_t("sha", "SHA1", &EVP_sha1).pbkdf2_eligible(),

#ifdef HAVE_SHA224
        digest_type_t("sha224", "SHA2-224", &EVP_sha224).pbkdf2_eligible(),
#else
        digest_type_t("sha224", "SHA2-224").pbkdf2_eligible(),
#endif

#ifdef HAVE_SHA256
        digest_type_t("sha256", "SHA2-256", &EVP_sha256).pbkdf2_eligible(),
#else
        digest_type_t("sha256", "SHA2-256").pbkdf2_eligible(),
#endif

#ifdef HAVE_SHA384
        digest_type_t("sha384", "SHA2-384", &EVP_sha384).pbkdf2_eligible(),
#else
        digest_type_t("sha384", "SHA2-384").pbkdf2_eligible(),
#endif

#ifdef HAVE_SHA512
        digest_type_t("sha512", "SHA2-512", &EVP_sha512).pbkdf2_eligible(),
#else
        digest_type_t("sha512", "SHA2-512").pbkdf2_eligible(),
#endif

#ifdef HAVE_SHA512_224
        digest_type_t("sha512_224", "SHA2-512/224", &EVP_sha512_224).pbkdf2_eligible(),
#else
        digest_type_t("sha512_224", "SHA2-512/224").pbkdf2_eligible(),
#endif

#ifdef HAVE_SHA512_256
        digest_type_t("sha512_256", "SHA2-512/256", &EVP_sha512_256).pbkdf2_eligible(),
#else
        digest_type_t("sha512_256", "SHA2-512/256").pbkdf2_eligible(),
#endif

#ifdef HAVE_SHA3_224
        digest_type_t("sha3_224", "SHA3-224", &EVP_sha3_224),
#else
        digest_type_t("sha3_224", "SHA3-224"),
#endif

#ifdef HAVE_SHA3_256
        digest_type_t("sha3_256", "SHA3-256", &EVP_sha3_256),
#else
        digest_type_t("sha3_256", "SHA3-256"),
#endif

#ifdef HAVE_SHA3_384
        digest_type_t("sha3_384", "SHA3-384", &EVP_sha3_384),
#else
        digest_type_t("sha3_384", "SHA3-384"),
#endif

#ifdef HAVE_SHA3_512
        digest_type_t("sha3_512", "SHA3-512", &EVP_sha3_512),
#else
        digest_type_t("sha3_512", "SHA3-512"),
#endif

#ifdef HAVE_SHAKE128
        digest_type_t("shake128", "SHAKE-128", &EVP_shake128).set_xof_default_length(16),
#else
        digest_type_t("shake128", "SHAKE-128")
#endif

#ifdef HAVE_SHAKE256
        digest_type_t("shake256", "SHAKE-256", &EVP_shake256).set_xof_default_length(32),
#else
                digest_type_t("shake256", "SHAKE-256"),
#endif

#ifdef HAVE_SM3
        digest_type_t("sm3", "SM3", &EVP_sm3),
#else
        digest_type_t("sm3", "SM3"),
#endif

#ifdef HAVE_BLAKE2
        digest_type_t("blake2b", "BLAKE2b512", &EVP_blake2b512),
#else
        digest_type_t("blake2b", "BLAKE2b512"),
#endif

#ifdef HAVE_BLAKE2
        digest_type_t("blake2s", "BLAKE2s256", &EVP_blake2s256),
#else
        digest_type_t("blake2s", "BLAKE2s256"),
#endif
};

void init_digest_types(ErlNifEnv* env)
{
    for (auto &p: digest_types) {
#ifdef HAS_3_0_API
        if (p.str_v3) {
            p.resource = EVP_MD_fetch(nullptr, p.str_v3, "");
# ifdef FIPS_SUPPORT
            /* Try if valid in FIPS */
            {
                EVP_MD *tmp = EVP_MD_fetch(nullptr, p.str_v3, "fips=yes");

                if (tmp) {
                    EVP_MD_free(tmp);
                    p.flags.fips_forbidden = false;
                } else {
                    p.flags.fips_forbidden = true;
                }
            }
# endif /* FIPS_SUPPORT and >=3.0.0 */
        }
#else
        if (p->md.funcp) {
            p->md.p = p->md.funcp();
        }
#endif
        p.atom = enif_make_atom(env, p.str);
    }
}

digest_type_t * get_digest_type(ERL_NIF_TERM type)
{
    for (auto &p: digest_types) {
        if (type == p.atom) {
            return &p;
        }
    }

    return nullptr;
}


#ifdef HAS_3_0_API
ERL_NIF_TERM digest_types_as_list(ErlNifEnv* env)
{
    ERL_NIF_TERM hd = enif_make_list(env, 0);

    for (const auto &p: digest_types) {
        if (p.is_fips_forbidden()) {
            continue;
        }
        if (p.resource != nullptr) {
            hd = enif_make_list_cell(env, p.atom, hd);
        }
    }

    return hd;
}
#endif
