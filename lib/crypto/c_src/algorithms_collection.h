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
#include <assert.h>

#ifdef __cplusplus
extern "C"
{
#endif

#include "common.h"

    //
    // C API which affects all collections at once
    //

    // Creates protective mutex for each collection to allow for safe lazy init
    // and reinit
    bool create_algorithm_mutexes(void);
    // Deletes (and zeroes) algorithm mutexes
    void free_algorithm_mutexes(void);
    // Called on fips mode change to reset the algorithm lists. Next lazy_init
    // call to each collection will do the work again.
    void algorithms_reset_cache(void);

#ifdef __cplusplus
}
#endif

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

// Stores a static array of algorithms for detected algorithms of type
// AlgorithmT and to populate the array, a second type is provided: ProbeT, this
// is a type of struct containing logic to detect algorithm availability and
// create AlgorithmT.
//
// The collections for all types of algorithms are statically created before the
// crypto library is initialized, but the mutex must be additionally constructed
// (call only once).
template<typename AlgorithmT, typename ProbeT>
struct algorithm_collection_t {
private:
    bool lazy_init_done;
    // each probe is executed every time we reset and repopulate algorithms
    // list. Probes are not const, because their implementations might want to
    // cache something like found existing atoms by string
    ProbeT *probes;
    size_t probes_count;
    // contains detected and supported algorithms
    std::vector<AlgorithmT> algorithms;
    ErlNifMutex *mutex;
    const char *debug_name;

public:
    explicit algorithm_collection_t(const char *debug_name, ProbeT *probes_, const size_t probes_count_)
            : lazy_init_done(false), probes(probes_), probes_count(probes_count_), mutex(nullptr),
              debug_name(debug_name) {
    }

    ~algorithm_collection_t() {
        destroy_mutex();
    }

    // Const pointer to start of algorithms
    auto cbegin() const -> decltype(this->algorithms.cbegin()) {
        return this->algorithms.cbegin();
    }
    // Const pointer to one after last of the algorithms
    auto cend() const -> decltype(this->algorithms.cend()) {
        return this->algorithms.cend();
    }

    // Mutable pointer to start of algorithms
    auto begin() -> decltype(this->algorithms.begin()) {
        return this->algorithms.begin();
    }
    // Mutable pointer to one after last of the algorithms
    auto end() -> decltype(this->algorithms.end()) {
        return this->algorithms.end();
    }

    bool create_mutex() {
        this->mutex = enif_mutex_create(const_cast<char *>(debug_name));
        return this->mutex != nullptr;
    }

    void destroy_mutex() {
        if (this->mutex) {
            enif_mutex_destroy(this->mutex);
            this->mutex = nullptr;
        }
    }

    // Resets the found algorithms list and the flag for lazy init, so lazy init
    // will
    void reset() {
        mutex_lock_and_auto_release critical_section(this->mutex);
        this->lazy_init_done = false;
        this->algorithms.clear();
    }

    // Checks whether the init has already been done for the array, otherwise
    // will invoke init_fn
    size_t lazy_init(ErlNifEnv *env, const bool fips_enabled) {
        size_t result = 0;
        if (this->lazy_init_done) {
            return this->algorithms.size();
        }

        mutex_lock_and_auto_release critical_section(this->mutex);

        this->algorithms.clear();

        for (size_t i = 0; i < probes_count; ++i) {
            // For each probe, call probe() member function, in case of success
            // the probe code will use the passed 'this->algorithms' reference
            // to add an algorithm to the collection.
            probes[i].probe(env, fips_enabled, this->algorithms);
        }

        result = this->algorithms.size();
        this->lazy_init_done = true;

        return result;
    }

    // Invokes to_list/3 with empty starting list.
    ERL_NIF_TERM to_list(ErlNifEnv *env, const bool match_fips_forbidden) const {
        return to_list(env, enif_make_list(env, 0), match_fips_forbidden);
    }

    // Search for algorithms which are is_available() and is_forbidden_in_fips() equals to match_fips_forbidden.
    // Uses hd as list start (can push into existing list or use NIL for a new list)
    ERL_NIF_TERM to_list(ErlNifEnv *env, ERL_NIF_TERM hd, const bool match_fips_forbidden) const {
        for (const auto &algo : this->algorithms) {
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
};

// Helper: Ensure atoms are not created repeatedly
ERL_NIF_TERM create_or_existing_atom(ErlNifEnv *env, const char *atom_name, ERL_NIF_TERM atom = 0);

#endif
