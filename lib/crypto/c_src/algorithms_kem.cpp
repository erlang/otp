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

#include "algorithms_kem.h"
#include "auto_openssl_resource.h"

#ifdef HAVE_ML_KEM
kem_probe_t kem_probes[] = {
        kem_probe_t("mlkem512"),
        kem_probe_t("mlkem768"),
        kem_probe_t("mlkem1024"),
};

kem_collection_t kem_collection("crypto.kem_collection", kem_probes, sizeof(kem_probes)/sizeof(kem_probes[0]));
#else
// Cannot allocate array of size 0, so instead use the zero size constructor
kem_collection_t kem_collection("crypto.kem_collection", nullptr, 0);
#endif

//
// Implementation of KEM Algorithm storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM kem_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return kem_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM kem_type_t::get_atom() const { return this->init->atom; }

//
// for FIPS will attempt to initialize the KEM context to verify whether the
// algorithm is allowed, for non-FIPS the old behavior - always allow.
//
bool kem_type_t::check_kem_algorithm(bool fips_enabled) {
#ifdef HAVE_ML_KEM
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    const auto_kem_t kem(EVP_KEM_fetch(nullptr, this->init->str_v3, nullptr));
    if (!kem) {
        return false; // not available by name
    }

    const auto_pkey_ctx_t ctx(EVP_PKEY_CTX_new_from_name(nullptr, this->init->str_v3, nullptr));
    // failed: algorithm not available, do not add
    if (ctx) {
        if (EVP_PKEY_encapsulate_init(ctx.pointer, nullptr) != 1) {
            this->flags.fips_forbidden = true;
        }
    }
#endif // FIPS_SUPPORT && HAS_3_0_API
    return true;
#else
    return false;
#endif // HAVE_ML_KEM
}

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void kem_probe_t::probe(ErlNifEnv *env, const bool fips_enabled, std::vector<kem_type_t> &output) {
    // Nothing will happen if HAVE_ML_KEM is not defined, the output will remain empty
#ifdef HAVE_ML_KEM
    this->atom = create_or_existing_atom(env, this->str_v3, this->atom);
    kem_type_t algo = {.init = this};
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    if (!algo.check_kem_algorithm(fips_enabled)) {
        return; // failed to find the algorithm, do not add
    }
#endif // FIPS_SUPPORT && HAS_3_0_API
    output.push_back(algo);
#endif
}
