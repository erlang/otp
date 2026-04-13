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

#include "common.h"

int init_mac_ctx(ErlNifEnv *env, ErlNifBinary *rt_buf);

void init_mac_types(ErlNifEnv *env);

ERL_NIF_TERM mac_types_as_list(ErlNifEnv *env);

ERL_NIF_TERM mac_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM mac_init_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM mac_update_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM mac_final_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/***************************
     MAC type declaration
***************************/

struct mac_type_t {
    const char *str = nullptr; // before init, NULL for end-of-table
    ERL_NIF_TERM atom = ERL_CRYPTO_BAD_ATOM_VALUE; // after init, 'false' for end-of-table
    int nid = 0;

    struct flags_t {
        bool fips_forbidden : 1;
    };
    flags_t flags = {};

    enum type_enum_t { NO_mac = 0, HMAC_mac = 1, CMAC_mac = 2, POLY1305_mac = 3 };
    type_enum_t type = NO_mac;

    size_t key_len = 0; /* != 0 to also match on key_len */

    const char *str_v3 = nullptr;
#if defined(HAS_3_0_API)
    EVP_MAC *evp_mac = nullptr;
#endif

    explicit constexpr mac_type_t(const char *str_v1_, const char *str_v3_, const type_enum_t type_ = NO_mac) :
        str(str_v1_), type(type_), str_v3(str_v3_) {}
    constexpr mac_type_t &no_fips() {
        flags.fips_forbidden = false;
        return *this;
    }
    constexpr mac_type_t &set_nid(const int nid_) {
        nid = nid_;
        return *this;
    }
    constexpr mac_type_t &set_key_len(const size_t key_len_) {
        key_len = key_len_;
        return *this;
    }
    constexpr bool is_fips_forbidden() const {
#ifdef FIPS_SUPPORT
        return flags.fips_forbidden && FIPS_MODE();
#else
        return false;
#endif
    }
};
