/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017. All Rights Reserved.
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

/* This is a replacement for getenv(3) and friends, operating on instances so
 * we can keep a common implementation for both the global and local (per-port)
 * environments.
 *
 * The instances are not thread-safe on their own but unlike getenv(3) we're
 * guaranteed to be the only user, so placing locks around all our accesses
 * will suffice.
 *
 * Use erts_sys_rwlock_global_osenv to access the global environment. */

#ifndef __ERL_OSENV_H__
#define __ERL_OSENV_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

typedef struct __erts_osenv_data_t erts_osenv_data_t;

typedef struct __erts_osenv_t {
    struct __env_rbtnode_t *tree;
    int variable_count;
    int content_size;
} erts_osenv_t;

#include "sys.h"

struct __erts_osenv_data_t {
    Sint length;
    void *data;
};

void erts_osenv_init(erts_osenv_t *env);
void erts_osenv_clear(erts_osenv_t *env);

/* @brief Merges \c with into \c env
 *
 * @param overwrite Whether to overwrite existing entries or keep them as they
 * are. */
void erts_osenv_merge(erts_osenv_t *env, const erts_osenv_t *with, int overwrite);

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* @brief Copies env[key] into \c value
 *
 * @return 1 on success, 0 if the key couldn't be found, and -1 if the input
 * was invalid. */
int erts_osenv_get_term(const erts_osenv_t *env, struct process *process,
    Eterm key, Eterm *value);

/* @brief Copies \c value into \c env[key]
 *
 * @return 1 on success, -1 if the input was invalid. */
int erts_osenv_put_term(erts_osenv_t *env, Eterm key, Eterm value);

/* @brief Removes \c env[key]
 *
 * @return 1 on success, 0 if the key couldn't be found, and -1 if the input
 * was invalid. */
int erts_osenv_unset_term(erts_osenv_t *env, Eterm key);

/* @brief Copies env[key] into \c value
 *
 * @param value [in,out] The buffer to copy the value into, may be NULL if you
 * only wish to query presence.
 *
 * @return 1 on success, 0 if the key couldn't be found, and -1 if if the value
 * didn't fit into the buffer. */
int erts_osenv_get_native(const erts_osenv_t *env, const erts_osenv_data_t *key,
    erts_osenv_data_t *value);

/* @brief Copies \c value into \c env[key]
 *
 * @return 1 on success, -1 on failure. */
int erts_osenv_put_native(erts_osenv_t *env, const erts_osenv_data_t *key,
    const erts_osenv_data_t *value);

/* @brief Removes \c key from the env.
 *
 * @return 1 on success, 0 if the key couldn't be found. */
int erts_osenv_unset_native(erts_osenv_t *env, const erts_osenv_data_t *key);

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

typedef void (*erts_osenv_foreach_term_cb_t)(struct process *process,
    void *state, Eterm key, Eterm value);

typedef void (*erts_osenv_foreach_native_cb_t)(void *state,
    const erts_osenv_data_t *key,
    const erts_osenv_data_t *value);

/* @brief Walks through all environment variables, calling \c callback for each
 * one. It's unsafe to modify \c env within the callback. */
void erts_osenv_foreach_term(const erts_osenv_t *env, struct process *process,
    void *state, erts_osenv_foreach_term_cb_t callback);

/* @copydoc erts_osenv_foreach_term */
void erts_osenv_foreach_native(const erts_osenv_t *env, void *state,
    erts_osenv_foreach_native_cb_t callback);

#endif
