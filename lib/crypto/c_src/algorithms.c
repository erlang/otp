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

#include "algorithms.h"
#include "cipher.h"
#include "common.h"
#include "mac.h"

#include "algorithms_cipher.h"
#include "algorithms_curve.h"
#include "algorithms_digest.h"
#include "algorithms_kem.h"
#include "algorithms_mac.h"
#include "algorithms_pkey.h"
#include "algorithms_rsaopt.h"

//
// Supported Algorithms (filter on fips_forbidden == false)
//

ERL_NIF_TERM hash_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return digest_types_as_list(env, false);
}

ERL_NIF_TERM pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return pkey_algorithms_as_list(env, false);
}

ERL_NIF_TERM kem_algorithms_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return kem_algorithms_as_list(env, false);
}

ERL_NIF_TERM cipher_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return cipher_algorithms_as_list(env, false);
}

ERL_NIF_TERM mac_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return mac_algorithms_as_list(env, false);
}

ERL_NIF_TERM curve_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return curve_algorithms_as_list(env, false);
}

ERL_NIF_TERM rsa_opts_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return rsaopts_as_list(env, false);
}

//
// Forbidden Algorithms (filter on fips_forbidden == true)
//

ERL_NIF_TERM fips_forbidden_hash_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return digest_types_as_list(env, true);
}

ERL_NIF_TERM fips_forbidden_pubkey_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return pkey_algorithms_as_list(env, true);
}

ERL_NIF_TERM fips_forbidden_cipher_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return cipher_algorithms_as_list(env, true);
}

ERL_NIF_TERM fips_forbidden_kem_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return kem_algorithms_as_list(env, true);
}

ERL_NIF_TERM fips_forbidden_mac_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return mac_algorithms_as_list(env, true);
}

ERL_NIF_TERM fips_forbidden_curve_algorithms(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return curve_algorithms_as_list(env, true);
}
