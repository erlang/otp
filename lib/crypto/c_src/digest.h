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

using digest_type_fn_t = const EVP_MD *(*) ();

struct digest_type_t {
    const char *str;
    const char *str_v3; // the algorithm name as in OpenSSL 3.x
    ERL_NIF_TERM atom = ERL_CRYPTO_BAD_ATOM_VALUE; // available after init, 'false' for end-of-table
    struct flags_t {
        bool fips_forbidden : 1;
        bool pbkdf2_eligible : 1;
    };
    flags_t flags = {};
    const EVP_MD *(*funcp)() = nullptr; // NULL if notsup
    const EVP_MD *resource = nullptr; // available after init, NULL if notsup
    // 0 or default digest length for XOF digests.
    // Type 'unsigned int' is dictated by OSSL_PARAM_construct_uint() compatibility
    unsigned int xof_default_length = 0;

    constexpr digest_type_t(const char *str_v1, const char *str_v3, const digest_type_fn_t funcp_ = nullptr) :
        str(str_v1), str_v3(str_v3), funcp(funcp_) {}
    constexpr digest_type_t &no_fips() {
        flags.fips_forbidden = true;
        return *this;
    }
    constexpr digest_type_t &pbkdf2_eligible() {
        flags.pbkdf2_eligible = true;
        return *this;
    }
    constexpr digest_type_t &set_xof_default_length(const size_t len) {
        xof_default_length = len;
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

void init_digest_types(ErlNifEnv *env);
digest_type_t *get_digest_type(ERL_NIF_TERM type);

#ifdef HAS_3_0_API
ERL_NIF_TERM digest_types_as_list(ErlNifEnv *env);
#endif
