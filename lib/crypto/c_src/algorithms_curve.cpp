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

#include "algorithms_curve.h"

curve_probe_t curve_probes[] = {};

curve_collection_t curve_collection("crypto.curve_collection", curve_probes, sizeof(curve_probes) / sizeof(curve_probes[0]));

//
// Implementation of Curve Algorithm storage API
//

extern "C" size_t curve_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled) {
    return curve_collection.lazy_init(env, fips_enabled);
}

extern "C" void curve_add_algorithm(ErlNifEnv *env, const char *str_v3, const unsigned unavail_flags) {
    const curve_availability_t curve = {
            .str_v3 = str_v3, .flags = unavail_flags, .atom = create_or_existing_atom(env, str_v3)};
    curve_collection.algorithms.push_back(curve);
}

extern "C" ERL_NIF_TERM curve_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return curve_collection.to_list(env, fips_enabled);
}
