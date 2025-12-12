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

#ifdef __cplusplus
extern "C" {
#endif

#include "common.h"

typedef struct cipher_type_t cipher_type_C;
struct cipher_type_flags_t {
    bool fips_forbidden : 1;
    bool algorithm_init_failed : 1;
    bool aes_cfbx : 1;
    bool aead_cipher : 1;
    bool non_evp_cipher : 1;
    bool aes_ctr_compat : 1;
    bool ccm_mode : 1;
    bool gcm_mode : 1;
};
struct cipher_type_aead_t {
    int ctx_ctrl_set_ivlen, ctx_ctrl_get_tag, ctx_ctrl_set_tag;
};

//
// Supported Cipher Algorithms storage C API
//
ERL_NIF_TERM cipher_algorithms_as_list(ErlNifEnv *env, bool fips_enabled);
const cipher_type_C *get_cipher_type(ErlNifEnv *env, ERL_NIF_TERM type, size_t key_len);
const cipher_type_C *get_cipher_type_no_key(ErlNifEnv *env, ERL_NIF_TERM type);
struct cipher_type_flags_t get_cipher_type_flags(const cipher_type_C *p);
struct cipher_type_aead_t get_cipher_type_aead(const cipher_type_C *p);
bool is_cipher_forbidden_in_fips(const cipher_type_C *p);
const EVP_CIPHER *get_cipher_type_resource(const cipher_type_C *p);
const char *get_cipher_type_str_v3(const cipher_type_C *p);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include "algorithms_collection.h"
#include "auto_openssl_resource.h"

struct cipher_probe_t;

// Describes a cipher algorithm added by the collection's probe function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct cipher_type_t {
    const cipher_probe_t *init = nullptr; // the cipher_probe_t used to create this record
    auto_cipher_t resource{};
    cipher_type_aead_t aead{};
    ERL_NIF_TERM atom = 0; // copy of init->atom
    size_t key_len = 0; // copy of init->key_len

    cipher_type_flags_t flags{};

    cipher_type_t(const cipher_probe_t *init_, ERL_NIF_TERM atom, const size_t key_len, cipher_type_flags_t flags_) :
        init(init_), atom(atom), key_len(key_len), flags(flags_) {}
    explicit cipher_type_t(ERL_NIF_TERM atom, const size_t key_len=0) :
        atom(atom), key_len(key_len) {}

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return this->flags.fips_forbidden && FIPS_MODE();
#else
        return false;
#endif
    }
    bool is_available() const {
        // Available if cipher could be initialized, and (has a EVP_CIPHER resource, or is AES_CTR compatible)
        return !this->flags.algorithm_init_failed;
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;
    void check_availability(bool fips_enabled);
    // Partial order compare, returns a.atom < b.atom && a.key < b.key
    static bool compare_function(const cipher_type_t &a, const cipher_type_t &b);
    // Partial order compare, returns a.atom < b.atom
    static bool compare_function_no_key(const cipher_type_t &a, const cipher_type_t &b);
    bool eq(const cipher_type_t &other) const { return this->atom == other.atom && this->key_len == other.key_len; }
    bool eq_no_key(const cipher_type_t &other) const { return this->atom == other.atom; }

private:
    void check_fips_availability(bool fips_enabled);
#if defined(HAS_3_0_API)
    bool can_cipher_be_instantiated() const;
#endif
};

#ifdef HAVE_AEAD
enum AEAD_CTRL_TYPE {
    // Writes {{0,0,0}} into cipher_availability_t::aead
    NOT_AEAD,
    // Writes {{EVP_CTRL_AEAD_SET_IVLEN, EVP_CTRL_AEAD_GET_TAG, EVP_CTRL_AEAD_SET_TAG}}
    // into cipher_availability_t::aead
    AEAD_CTRL,
    // Writes {{EVP_CTRL_GCM_SET_IVLEN, EVP_CTRL_GCM_GET_TAG, EVP_CTRL_GCM_SET_TAG}}}
    // into cipher_availability_t::aead
    AEAD_CTRL_GCM,
    // Writes {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}}
    // into cipher_availability_t::aead
    AEAD_CTRL_CCM,
};
#endif // HAVE_AEAD

// A 0-argument function returning EVP_CIPHER used to construct supported ciphers
using cipher_constructor_fn_t = const EVP_CIPHER *(*) ();

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct cipher_probe_t {
    const char *str;
    // can be null, then str is used
    const char *str_v3;
    // constructor for OpenSSL < 3.0 (can be null)
    cipher_constructor_fn_t ctor_v1;
    ERL_NIF_TERM atom = 0;
    size_t key_len = 0;
    // initial value for the flags
    cipher_type_flags_t flags = {};
#ifdef HAVE_AEAD
    // determines which value goes into cipher_availability_t::aead
    AEAD_CTRL_TYPE aead_ctrl_type = NOT_AEAD;
#endif // HAVE_AEAD

    constexpr cipher_probe_t(const char *str_, const char *str_v3_,
                             const cipher_constructor_fn_t ctor = nullptr) : str(str_), str_v3(str_v3_), ctor_v1(ctor) {
    }
#ifdef HAVE_AEAD
    constexpr cipher_probe_t set_aead(const AEAD_CTRL_TYPE aead_ctrl_type,
                       const bool ccm_mode = false, const bool gcm_mode = false) {
        this->aead_ctrl_type = aead_ctrl_type;
        this->flags.aead_cipher = true;
        this->flags.ccm_mode = ccm_mode;
        this->flags.gcm_mode = gcm_mode;
        return *this;
    }
#endif
    constexpr cipher_probe_t set_fips_forbidden() {
        this->flags.fips_forbidden = true;
        return *this;
    }
    constexpr cipher_probe_t set_aes_cfbx() {
        this->flags.aes_cfbx = true;
        return *this;
    }
    constexpr cipher_probe_t set_keylen(const size_t key_len_) {
        this->key_len = key_len_;
        return *this;
    }

    const char *get_v3_name() const { return this->str_v3 ? this->str_v3 : this->str; }
    // Attempt to add a new known Cipher algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_enabled, std::vector<cipher_type_t> &output);
    // Sort the ciphers after all probes are executed for binary search
    static void post_lazy_init(std::vector<cipher_type_t> &algorithms);
};

using cipher_collection_t = algorithm_collection_t<cipher_type_t, cipher_probe_t>;
extern cipher_collection_t cipher_collection;

#endif // __cplusplus
