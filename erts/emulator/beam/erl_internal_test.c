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
#include "erl_internal_test.h"
#include "erl_misc_utils.h"

const ErtsInternalTest erts_internal_test_instance = {
    .erts_milli_sleep = &erts_milli_sleep,
    .ethr_atomic_dec = &ethr_atomic_dec,
    .ethr_atomic_inc = &ethr_atomic_inc,
    .ethr_atomic_init = &ethr_atomic_init,
    .ethr_atomic_read = &ethr_atomic_read,
    .ethr_atomic_set = &ethr_atomic_set,
    .ethr_rwmutex_destroy = &ethr_rwmutex_destroy,
    .ethr_rwmutex_init_opt = &ethr_rwmutex_init_opt,
    .ethr_rwmutex_rlock = &ethr_rwmutex_rlock,
    .ethr_rwmutex_runlock = &ethr_rwmutex_runlock,
    .ethr_rwmutex_rwlock = &ethr_rwmutex_rwlock,
    .ethr_rwmutex_rwunlock = &ethr_rwmutex_rwunlock,
    .ethr_rwmutex_tryrlock = &ethr_rwmutex_tryrlock,
    .ethr_rwmutex_tryrwlock = &ethr_rwmutex_tryrwlock,
    .ethr_thr_create = &ethr_thr_create,
    .ethr_thr_join = &ethr_thr_join
};
