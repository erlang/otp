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

#include "algorithms_digest.h"
#include <array>

static digest_probe_t digest_probes[] = {
#ifdef HAVE_MD4
        digest_probe_t("md4", "MD4", &EVP_md4),
#endif
#ifdef HAVE_MD5
        digest_probe_t("md5", "MD5", &EVP_md5),
#endif
#ifdef HAVE_RIPEMD160
        digest_probe_t("ripemd160", "RIPEMD160", &EVP_ripemd160),
#endif
        digest_probe_t("sha", "SHA1", &EVP_sha1).set_pbkdf(),
#ifdef HAVE_SHA224
        digest_probe_t("sha224", "SHA2-224", &EVP_sha224).set_pbkdf(),
#endif
#ifdef HAVE_SHA256
        digest_probe_t("sha256", "SHA2-256", &EVP_sha256).set_pbkdf(),
#endif
#ifdef HAVE_SHA384
        digest_probe_t("sha384", "SHA2-384", &EVP_sha384).set_pbkdf(),
#endif
#ifdef HAVE_SHA512
        digest_probe_t("sha512", "SHA2-512", &EVP_sha512).set_pbkdf(),
#endif
#ifdef HAVE_SHA512_224
        digest_probe_t("sha512_224", "SHA2-512/224", &EVP_sha512_224).set_pbkdf(),
#endif
#ifdef HAVE_SHA512_256
        digest_probe_t("sha512_256", "SHA2-512/256", &EVP_sha512_256).set_pbkdf(),
#endif
#ifdef HAVE_SHA3_224
        digest_probe_t("sha3_224", "SHA3-224", &EVP_sha3_224),
#endif
#ifdef HAVE_SHA3_256
        digest_probe_t("sha3_256", "SHA3-256", &EVP_sha3_256),
#endif
#ifdef HAVE_SHA3_384
        digest_probe_t("sha3_384", "SHA3-384", &EVP_sha3_384),
#endif
#ifdef HAVE_SHA3_512
        digest_probe_t("sha3_512", "SHA3-512", &EVP_sha3_512),
#endif
#ifdef HAVE_SHAKE128
        digest_probe_t("shake128", "SHAKE-128", &EVP_shake128).set_xof_default_length(6),
#endif
#ifdef HAVE_SHAKE256
        digest_probe_t("shake256", "SHAKE-256", &EVP_shake256).set_xof_default_length(32),
#endif
#ifdef HAVE_SM3
        digest_probe_t("sm3", "SM3", &EVP_sm3),
#endif
#ifdef HAVE_BLAKE2
        digest_probe_t("blake2b", "BLAKE2b512", &EVP_blake2b512),
#endif
#ifdef HAVE_BLAKE2
        digest_probe_t("blake2s", "BLAKE2s256", &EVP_blake2s256),
#endif
};

digest_collection_t digest_collection("crypto.digest.digest_collection", digest_probes,
                                      sizeof(digest_probes) / sizeof(digest_probes[0]));

//
// Implementation of Pubkey Algorithm storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM digest_types_as_list(ErlNifEnv *env, const bool fips_forbidden) {
    return digest_collection.to_list(env, fips_forbidden);
}

ERL_NIF_TERM digest_type_t::get_atom() const { return this->init->atom; }

#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
// Initialize an algorithm to check that all its dependencies are valid in FIPS
bool digest_type_t::check_valid_in_fips(const EVP_MD *md) {
    if (md) {
        const auto_md_ctx_t ctx(EVP_MD_CTX_new());
        // Try to initialize the digest algorithm for use, this will check the dependencies
        if (EVP_DigestInit_ex(ctx.pointer, md, nullptr) == 1) {
            return true;
        }
    }
    return false;
}
#endif // FIPS_SUPPORT && HAS_3_0_API

void digest_type_t::create_md_resource(const bool fips_mode) {
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    auto_md_t fetched_md(EVP_MD_fetch(nullptr, this->init->get_v3_name(), nullptr));

    // Record failed algorithm instantiation for FIPS enabled & OpenSSL API 3.0 only
    if (fips_mode && !check_valid_in_fips(fetched_md.pointer)) {
        this->flags.fips_forbidden = true;
    } else {
        this->flags.fips_forbidden = false;
        this->resource = std::move(fetched_md); // pass ownership and move data to this->md
    }
#else
    // construct from the old API, each probe has a constructor function
    this->resource.reset(const_cast<evp_md_pointer_type_t>(this->init->v1_ctor()));
#endif // HAS_3_0_API && FIPS_SUPPORT
}

digest_type_t::digest_type_t(const digest_probe_t *init_) :
    init(init_), flags(init_->flags), xof_default_length(init_->xof_default_length) {}

void digest_probe_t::probe(ErlNifEnv *env, const bool fips_mode, std::vector<digest_type_t> &output) {
    output.emplace_back(this);
    // Unavailable are skipped. Available are added. Forbidden are added, but with flags.fips_forbidden=true.
    auto &algo = output.back();
    algo.create_md_resource(fips_mode);
    this->atom = create_or_existing_atom(env, this->str, this->atom);
}

// Array lookup
extern "C" digest_type_C *get_digest_type(ErlNifEnv *env, ERL_NIF_TERM type) {
    const bool fips_enabled = FIPS_MODE();
    for (auto p = digest_collection.begin(env, fips_enabled); p != digest_collection.end(fips_enabled); ++p) {
        if (type == p->get_atom()) {
            return &*p;
        }
    }
    return nullptr;
}

extern "C" bool is_digest_forbidden_in_fips(const digest_type_C *p) {
    return p ? p->is_forbidden_in_fips() : true; // forbidden if p is null
}

extern "C" const char *get_digest_type_str_v3(const digest_type_C *p) { return p->init->get_v3_name(); }

extern "C" const EVP_MD *get_digest_type_resource(const digest_type_C *p) { return p ? p->resource.pointer : nullptr; }

extern "C" size_t get_digest_type_xof_default_length(const digest_type_C *p) { return p ? p->xof_default_length : 0; }

extern "C" bool is_digest_eligible_for_pbkdf2(const digest_type_C *p) { return p ? p->flags.pbkdf2_eligible : false; }
