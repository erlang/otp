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

extern "C" {
#include "algorithms_store.h"
#include "common.h"
}

#include <vector>

// RAII enif_mutex wrapper, auto releases the execution has left the scope
// {
//   mutex_lock_and_auto_release x(mutex_ptr);
//   ... protected code
// } <- auto released here
struct mutex_lock_and_auto_release {
    ErlNifMutex *mutex;

    explicit mutex_lock_and_auto_release(ErlNifMutex *m) : mutex(m) { enif_mutex_lock(m); }
    ~mutex_lock_and_auto_release() { enif_mutex_unlock(mutex); }
};

// Stores array of algorithms for T (T can be a struct for describing a
// pubkey, mac, cipher, kem, curve ...etc)
// Statically created before the crypto library is initialized, the mutex
// must be additionally constructed (call only once)
template<typename T>
struct algorithm_collection_t {
    bool lazy_init_done;
    std::vector<T> algorithms;
    ErlNifMutex *mutex;
    const char *array_name;

    explicit algorithm_collection_t(const char *array_name) :
        lazy_init_done(false), mutex(nullptr), array_name(array_name) {}

    ~algorithm_collection_t() { destroy_mutex(); }

    bool create_mutex() {
        this->mutex = enif_mutex_create(const_cast<char *>(array_name));
        return this->mutex != nullptr;
    }

    void destroy_mutex() {
        if (this->mutex) {
            enif_mutex_destroy(this->mutex);
            this->mutex = nullptr;
        }
    }

    void reset() {
        mutex_lock_and_auto_release critical_section(this->mutex);
        this->lazy_init_done = false;
        this->algorithms.clear();
    }

    // Checks whether the init has already be done for the array, otherwise will invoke init_fn
    size_t lazy_init(ErlNifEnv *env, const bool fips_enabled, const init_algorithms_fn init_fn) {
        size_t result = 0;
        if (this->lazy_init_done) {
            return this->algorithms.size();
        }

        mutex_lock_and_auto_release critical_section(this->mutex);

        this->algorithms.clear();
        init_fn(env, fips_enabled);
        result = this->algorithms.size();
        this->lazy_init_done = true;

        return result;
    }

    ERL_NIF_TERM to_list(ErlNifEnv *env, const bool fips_forbidden) const {
        ERL_NIF_TERM hd = enif_make_list(env, 0);

        for (const auto &algo: this->algorithms) {
            // Any of the forbidden flags is not set, then something is available
            if (algo.is_forbidden_in_fips() == fips_forbidden) {
                hd = enif_make_list_cell(env, algo.get_atom(), hd);
            }
        }
        return hd;
    }
};

struct pkey_availability_t {
    const char *str_v3; /* the algorithm name as in OpenSSL 3.x */
    unsigned flags; /* combination of PKEY_AVAIL_FLAGS */
    ERL_NIF_TERM atom; /* added to results when the user is querying */

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return (this->flags == FIPS_FORBIDDEN_PKEY_ALL || this->flags == FIPS_PKEY_NOT_AVAIL) && FIPS_MODE();
#else
        return false;
#endif
    }
    ERL_NIF_TERM get_atom() const { return this->atom; }
};

static algorithm_collection_t<pkey_availability_t> pkey_collection("crypto.pkey_collection");

struct curve_availability_t {
    const char *str_v3; // the algorithm name as in OpenSSL 3.x
    unsigned flags; // combination of CURVE_AVAIL_FLAGS
    ERL_NIF_TERM atom; // as returned to the library user on a query

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return this->flags != 0 && FIPS_MODE();
#else
        return false;
#endif
    }
    ERL_NIF_TERM get_atom() const { return this->atom; }
};

static algorithm_collection_t<curve_availability_t> curve_collection("crypto.curve_collection");

extern "C" bool create_algorithm_mutexes() { return pkey_collection.create_mutex() && curve_collection.create_mutex(); }

extern "C" void free_algorithm_mutexes(void) {
    pkey_collection.destroy_mutex();
    curve_collection.destroy_mutex();
}

extern "C" void algorithms_reset_cache() {
    pkey_collection.reset();
    curve_collection.reset();
}

//
// Implementation of Pubkey Algorithm storage API
//

extern "C" size_t pubkey_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled,
                                              const init_algorithms_fn delayed_init_fn) {
    return pkey_collection.lazy_init(env, fips_enabled, delayed_init_fn);
}

extern "C" ERL_NIF_TERM pubkey_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return pkey_collection.to_list(env, fips_enabled);
}

// Ensure atoms are not created repeatedly
static ERL_NIF_TERM create_or_existing_atom(ErlNifEnv *env, const char *atom_name, ERL_NIF_TERM atom = 0) {
    if (!atom) {
        enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_UTF8);
        if (!atom) {
            atom = enif_make_atom(env, atom_name);
        }
    }
    return atom;
}

extern "C" void pubkey_add_algorithm(ErlNifEnv *env, const char *str_v3, const unsigned unavailable,
                                     ERL_NIF_TERM atom) {
    const pkey_availability_t algo = {
            .str_v3 = str_v3,
            .flags = unavailable,
            .atom = create_or_existing_atom(env, str_v3, atom),
    };
    pkey_collection.algorithms.push_back(algo);
}

//
// Implementation of Curve Algorithm storage API
//

extern "C" size_t curve_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled,
                                             const init_algorithms_fn delayed_init_fn) {
    return curve_collection.lazy_init(env, fips_enabled, delayed_init_fn);
}

extern "C" void curve_add_algorithm(ErlNifEnv *env, const char *str_v3, const unsigned unavail_flags) {
    const curve_availability_t curve = {
            .str_v3 = str_v3, .flags = unavail_flags, .atom = create_or_existing_atom(env, str_v3)};
    curve_collection.algorithms.push_back(curve);
}

extern "C" ERL_NIF_TERM curve_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return curve_collection.to_list(env, fips_enabled);
}
