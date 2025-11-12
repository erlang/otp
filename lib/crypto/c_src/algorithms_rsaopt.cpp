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

#include "algorithms_rsaopt.h"

rsaopt_probe_t rsaopt_probes[] = {
#ifdef HAS_EVP_PKEY_CTX
#ifdef HAVE_RSA_PKCS1_PSS_PADDING
    {.str_v3 = "rsa_pkcs1_pss_padding"},
    {.str_v3 = "rsa_pss_saltlen"},
#endif
#ifdef HAVE_RSA_MGF1_MD
    {.str_v3 = "rsa_mgf1_md"},
#endif
#ifdef HAVE_RSA_OAEP_PADDING
    {.str_v3 = "rsa_pkcs1_oaep_padding"},
#endif
#ifdef HAVE_RSA_OAEP_MD
    {.str_v3 = "rsa_oaep_label"},
    {.str_v3 = "rsa_oaep_md"},
#endif
    {.str_v3 = "signature_md"},
#endif
    {.str_v3 = "rsa_pkcs1_padding"},
    {.str_v3 = "rsa_x931_padding"},
#ifdef HAVE_RSA_SSLV23_PADDING
    {.str_v3 = "rsa_sslv23_padding"},
#endif
    {.str_v3 = "rsa_no_padding"},
};

rsaopt_collection_t rsaopt_collection("crypto.rsaopt_collection", rsaopt_probes,
                                      sizeof(rsaopt_probes) / sizeof(rsaopt_probes[0]));

//
// Implementation of Known RSA Options storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" size_t rsaopts_lazy_init(ErlNifEnv* env, const bool fips_enabled) {
    return rsaopt_collection.lazy_init(env, fips_enabled);
}

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM rsaopt_as_list(ErlNifEnv* env, const bool fips_enabled) {
    return rsaopt_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM rsaopt_availability_t::get_atom() const { return this->init->atom; }

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void rsaopt_probe_t::probe(ErlNifEnv* env, const bool fips_enabled, std::vector<rsaopt_availability_t>& output) {
    this->atom = create_or_existing_atom(env, this->str_v3, this->atom);
    const rsaopt_availability_t algo = {.init = this};
    // No extra checks, just convert name to atom and add
    return output.push_back(algo);
}
