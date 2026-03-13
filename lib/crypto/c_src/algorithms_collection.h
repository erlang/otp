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
extern "C"
{
#endif

#include "common.h"

    //
    // C API which affects all collections at once
    //

    // Creates protective mutex for each collection to allow for safe lazy init and reinit
    bool create_algorithm_mutexes(ErlNifEnv *env);
    // Deletes (and zeroes) algorithm mutexes
    void free_algorithm_mutexes(void);

#ifdef __cplusplus
}
#endif

// Use this value as a safe uninitialized atom value (a header+atom bits combination, doesn't exist in wild nature)
#define CRYPTOENIF_BAD_ATOM_VALUE 0

#ifdef __cplusplus
#    include <vector>

// RAII enif_mutex wrapper, auto releases the execution has left the scope
// {
//   mutex_lock_and_auto_release x(mutex_ptr);
//   ... protected code
// } <- auto released here
struct mutex_lock_and_auto_release {
    ErlNifMutex *mutex;

    explicit mutex_lock_and_auto_release(ErlNifMutex *m) : mutex(m) {
        enif_mutex_lock(m);
    }
    ~mutex_lock_and_auto_release() {
        enif_mutex_unlock(mutex);
    }
};

// Stores a static array of algorithms for detected algorithms of type AlgorithmT and to populate the array, a second
// type is provided: ProbeT, this is a type of struct containing logic to detect algorithm availability and create
// AlgorithmT.
//
// The collections for all types of algorithms are statically created before the crypto library is initialized, but the
// mutex must be additionally constructed (call only once).
template<typename AlgorithmT, typename ProbeT>
struct algorithm_collection_t {
    using ContainerT = std::vector<AlgorithmT>;
private:
    // Lazy init flag for fips=false and fips=true
    bool lazy_init_done[2] = {false, false};
    // each probe is executed every time we reset and repopulate algorithms list. Probes are not const, because their
    // implementations might want to cache something like found existing atoms by string
    ProbeT *probes;
    size_t probes_count;
    // Detected and supported algorithms for fips=false and fips=true
    ContainerT algorithms[2] = {};
    ErlNifMutex *mutex;
    const char *mutex_name; // Used for mutex creation

public:
    explicit algorithm_collection_t(const char *mutex_name_, ProbeT *probes_, const size_t probes_count_)
            : probes(probes_), probes_count(probes_count_), mutex(nullptr), mutex_name(mutex_name_) {
    }

    ~algorithm_collection_t() {
        destroy_mutex();
    }

    // Const pointer to start of algorithms, functions as std::cbegin() but uses the args to call lazy_init
    auto cbegin(ErlNifEnv *env, const bool fips_mode) -> typename ContainerT::const_iterator {
        lazy_init(env, fips_mode);
        return this->algorithms[fips_mode ? 1 : 0].cbegin();
    }
    // Const pointer to one after last of the algorithms
    auto cend(const bool fips_mode) const -> typename ContainerT::const_iterator {
        return this->algorithms[fips_mode ? 1 : 0].cend();
    }

    // Mutable pointer to start of algorithms, functions as std::begin() but uses the args to call lazy_init
    auto begin(ErlNifEnv *env, bool const fips_mode) -> typename ContainerT::iterator {
        lazy_init(env, fips_mode);
        return this->algorithms[fips_mode ? 1 : 0].begin();
    }
    // Mutable pointer to one after last of the algorithms
    auto end(const bool fips_mode) -> typename ContainerT::iterator {
        return this->algorithms[fips_mode ? 1 : 0].end();
    }

    bool create_mutex() {
        this->mutex = enif_mutex_create(const_cast<char *>(mutex_name));
        return this->mutex != nullptr;
    }

    void destroy_mutex() {
        if (this->mutex) {
            enif_mutex_destroy(this->mutex);
            this->mutex = nullptr;
        }
    }

    // Invokes to_list/3 with empty starting list.
    ERL_NIF_TERM to_list(ErlNifEnv *env, const bool match_fips_forbidden) {
        return to_list(env, enif_make_list(env, 0), match_fips_forbidden);
    }

    // Search for algorithms which are is_available() and is_forbidden_in_fips() equals to match_fips_forbidden.
    // Uses hd as list start (can push into existing list or use NIL for a new list)
    ERL_NIF_TERM to_list(ErlNifEnv *env, ERL_NIF_TERM hd, const bool match_fips_forbidden) {
        const bool fips_enabled = FIPS_MODE();
        lazy_init(env, fips_enabled);

        for (const auto &algo : this->algorithms[fips_enabled ? 1 : 0]) {
            // Any of the forbidden flags is not set, then something is
            // available
            if (algo.is_available() && algo.is_forbidden_in_fips() == match_fips_forbidden) {
                const auto atom = algo.get_atom();
                ASSERT(atom != 0);
                hd = enif_make_list_cell(env, atom, hd);
            }
        }
        return hd;
    }

private:
    // Checks whether the init has already been done for the array, otherwise will invoke init_fn
    void lazy_init(ErlNifEnv *env, const bool fips_enabled) {
        const size_t collection_index = fips_enabled ? 1 : 0;
        if (this->lazy_init_done[collection_index]) return;

        mutex_lock_and_auto_release critical_section(this->mutex);

        // for those who waited for mutex
        if (this->lazy_init_done[collection_index]) return;

        for (size_t i = 0; i < probes_count; ++i) {
            // For each probe, call probe() member function, in case of success the probe code will use the passed
            // 'this->algorithms' reference to add an algorithm to the collection.
            probes[i].probe(env, fips_enabled, this->algorithms[collection_index]);
        }

        ProbeT::post_lazy_init(this->algorithms[collection_index]);
        this->lazy_init_done[collection_index] = true;
    }
};

// Helper: Ensure atoms are not created repeatedly
ERL_NIF_TERM create_or_existing_atom(ErlNifEnv *env, const char *atom_name, ERL_NIF_TERM atom = 0);

#endif
