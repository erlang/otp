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
        {.str = "md4", .str_v3 = "MD4", .v1_ctor = &EVP_md4},
#endif
#ifdef HAVE_MD5
        {.str = "md5", .str_v3 = "MD5", .v1_ctor = &EVP_md5},
#endif
#ifdef HAVE_RIPEMD160
        {.str = "ripemd160", .str_v3 = "RIPEMD160", .v1_ctor = &EVP_ripemd160},
#endif
        {.str = "sha", .str_v3 = "SHA1", .flags = {.pbkdf2_eligible = true}, .v1_ctor = &EVP_sha1},
#ifdef HAVE_SHA224
        {.str = "sha224", .str_v3 = "SHA2-224", .flags = {.pbkdf2_eligible = true}, .v1_ctor = &EVP_sha224},
#endif
#ifdef HAVE_SHA256
        {.str = "sha256", .str_v3 = "SHA2-256", .flags = {.pbkdf2_eligible = true}, .v1_ctor = &EVP_sha256},
#endif
#ifdef HAVE_SHA384
        {.str = "sha384", .str_v3 = "SHA2-384", .flags = {.pbkdf2_eligible = true}, .v1_ctor = &EVP_sha384},
#endif
#ifdef HAVE_SHA512
        {.str = "sha512", .str_v3 = "SHA2-512", .flags = {.pbkdf2_eligible = true}, .v1_ctor = &EVP_sha512},
#endif
#ifdef HAVE_SHA512_224
        {.str = "sha512_224", .str_v3 = "SHA2-512/224", .flags = {.pbkdf2_eligible = true}, .v1_ctor = &EVP_sha512_224},
#endif
#ifdef HAVE_SHA512_256
        {.str = "sha512_256", .str_v3 = "SHA2-512/256", .flags = {.pbkdf2_eligible = true}, .v1_ctor = &EVP_sha512_256},
#endif
#ifdef HAVE_SHA3_224
        {.str = "sha3_224", .str_v3 = "SHA3-224", .v1_ctor = &EVP_sha3_224},
#endif
#ifdef HAVE_SHA3_256
        {.str = "sha3_256", .str_v3 = "SHA3-256", .v1_ctor = &EVP_sha3_256},
#endif
#ifdef HAVE_SHA3_384
        {.str = "sha3_384", .str_v3 = "SHA3-384", .v1_ctor = &EVP_sha3_384},
#endif
#ifdef HAVE_SHA3_512
        {.str = "sha3_512", .str_v3 = "SHA3-512", .v1_ctor = &EVP_sha3_512},
#endif
#ifdef HAVE_SHAKE128
        {.str = "shake128", .str_v3 = "SHAKE-128", .v1_ctor = &EVP_shake128, .xof_default_length = 6},
#endif
#ifdef HAVE_SHAKE256
        {.str = "shake256", .str_v3 = "SHAKE-256", .v1_ctor = &EVP_shake256, .xof_default_length = 32},
#endif
#ifdef HAVE_SM3
        {.str = "sm3", .str_v3 = "SM3", .v1_ctor = &EVP_sm3},
#endif
#ifdef HAVE_BLAKE2
        {.str = "blake2b", .str_v3 = "BLAKE2b512", .v1_ctor = &EVP_blake2b512},
#endif
#ifdef HAVE_BLAKE2
        {.str = "blake2s", .str_v3 = "BLAKE2s256", .v1_ctor = &EVP_blake2s256},
#endif
};

digest_collection_t digest_collection("crypto.digest.digest_collection", digest_probes,
                                      sizeof(digest_probes) / sizeof(digest_probes[0]));

//
// Implementation of Pubkey Algorithm storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" void digest_types_lazy_init(ErlNifEnv *env, const bool fips_enabled) {
    digest_collection.lazy_init(env, fips_enabled);
}

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
    this->resource = this->init->v1_ctor();
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
extern "C" digest_type_C *get_digest_type(ERL_NIF_TERM type) {
    for (auto &p: digest_collection) {
        if (type == p.get_atom()) {
            return &p;
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
