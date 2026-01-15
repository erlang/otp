/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2025. All Rights Reserved.
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

/*
 *  A peephole to access selected internal symbols (functions) for testing
 *  without having to pollute the name space of exported symbols.
 */
#ifndef ERL_INTERNAL_TEST_H
#define ERL_INTERNAL_TEST_H

#include "erl_drv_nif.h"
#include "ethread.h"

typedef struct {
    int (*erts_milli_sleep)(long);
    void (*ethr_atomic_dec)(ethr_atomic_t *var);
    void (*ethr_atomic_inc)(ethr_atomic_t *var);
    void (*ethr_atomic_init)(ethr_atomic_t *var, ethr_sint_t val);
    ethr_sint_t (*ethr_atomic_read)(ethr_atomic_t *var);
    void (*ethr_atomic_set)(ethr_atomic_t *var, ethr_sint_t val);
    int (*ethr_rwmutex_destroy)(ethr_rwmutex *);
    int (*ethr_rwmutex_init_opt)(ethr_rwmutex *, ethr_rwmutex_opt *);
    void (*ethr_rwmutex_rlock)(ethr_rwmutex *);
    void (*ethr_rwmutex_runlock)(ethr_rwmutex *);
    void (*ethr_rwmutex_rwlock)(ethr_rwmutex *);
    void (*ethr_rwmutex_rwunlock)(ethr_rwmutex *);
    int (*ethr_rwmutex_tryrlock)(ethr_rwmutex *);
    int (*ethr_rwmutex_tryrwlock)(ethr_rwmutex *);
    int (*ethr_thr_create)(ethr_tid *, void * (*)(void *), void *, ethr_thr_opts *);
    int (*ethr_thr_join)(ethr_tid, void **);
}ErtsInternalTest;

#if (defined(__WIN32__) || defined(_WIN32) || defined(_WIN32_))
extern const ErtsInternalTest erts_internal_test_instance;
#  define erts_internal_test (*(const ErtsInternalTest*)(WinDynNifCallbacks.erts_internal_test_ptr))
#else
ERL_NAPI_EXPORT
extern const ErtsInternalTest erts_internal_test_instance;
#  define erts_internal_test erts_internal_test_instance
#endif


#endif // ERL_INTERNAL_TEST_H
