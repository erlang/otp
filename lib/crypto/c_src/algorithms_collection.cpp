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

#include "algorithms_collection.h"
#include "algorithms_cipher.h"
#include "algorithms_curve.h"
#include "algorithms_digest.h"
#include "algorithms_kem.h"
#include "algorithms_mac.h"
#include "algorithms_pkey.h"
#include "algorithms_rsaopt.h"

#include <cstring>

extern "C" bool create_algorithm_mutexes() {
    ASSERT(enif_is_atom(CRYPTOENIF_BAD_ATOM_VALUE) != true); // Allow using 0 as safe initial value for ERL_NIF_TERM
    return cipher_collection.create_mutex() && curve_collection.create_mutex() && digest_collection.create_mutex() &&
           kem_collection.create_mutex() && mac_collection.create_mutex() && pkey_collection.create_mutex() &&
           rsaopt_collection.create_mutex();
}

extern "C" void free_algorithm_mutexes(void) {
    cipher_collection.destroy_mutex();
    curve_collection.destroy_mutex();
    digest_collection.destroy_mutex();
    kem_collection.destroy_mutex();
    mac_collection.destroy_mutex();
    pkey_collection.destroy_mutex();
    rsaopt_collection.destroy_mutex();
}

// Ensure atoms are not created repeatedly. Pass atom=0 to attempt creating an existing atom (then a new atom).
ERL_NIF_TERM create_or_existing_atom(ErlNifEnv *env, const char *atom_name, ERL_NIF_TERM atom) {
    ASSERT(atom_name != nullptr);
    ASSERT(std::strlen(atom_name) > 0);
    if (atom == CRYPTOENIF_BAD_ATOM_VALUE) {
        atom = enif_make_atom(env, atom_name);
    }
    return atom;
}
