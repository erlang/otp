/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
 * Native ethread atomics on TILE64/TILEPro.
 *
 */
#ifndef ETHREAD_TILE_ATOMIC_H
#define ETHREAD_TILE_ATOMIC_H

#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL "tilera"

#include <atomic.h>

/* An atomic is an aligned int accessed via locked operations.
 */
typedef struct {
    volatile ethr_sint32_t counter;
} ethr_native_atomic32_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADDR 1

static ETHR_INLINE ethr_sint32_t *
ethr_native_atomic32_addr(ethr_native_atomic32_t *var)
{
    return (ethr_sint32_t *) &var->counter;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INIT 1

static ETHR_INLINE void
ethr_native_atomic32_init(ethr_native_atomic32_t *var, ethr_sint32_t i)
{
    var->counter = i;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_read(ethr_native_atomic32_t *var)
{
    return var->counter;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_read_acqb(ethr_native_atomic32_t *var)
{
    return atomic_compare_and_exchange_val_acq(&var->counter,
					       0x81818181,
					       0x81818181);
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD 1

static ETHR_INLINE void
ethr_native_atomic32_add(ethr_native_atomic32_t *var, ethr_sint32_t incr)
{
    atomic_add(&var->counter, incr);
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC 1

static ETHR_INLINE void
ethr_native_atomic32_inc(ethr_native_atomic32_t *var)
{
    atomic_increment(&var->counter);
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC 1

static ETHR_INLINE void
ethr_native_atomic32_dec(ethr_native_atomic32_t *var)
{
    atomic_decrement(&var->counter);
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_add_return(ethr_native_atomic32_t *var, ethr_sint32_t incr)
{
    return atomic_exchange_and_add(&var->counter, incr) + incr;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_and_retold(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    return atomic_and_val(&var->counter, mask);
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_or_retold(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    return atomic_or_val(&var->counter, mask);
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_XCHG_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_xchg_acqb(ethr_native_atomic32_t *var, ethr_sint32_t val)
{   
    return atomic_exchange_acq(&var->counter, val);
} 

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_cmpxchg_acqb(ethr_native_atomic32_t *var,
				  ethr_sint32_t new,
				  ethr_sint32_t expected)
{
    return atomic_compare_and_exchange_val_acq(&var->counter, new, expected);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_TILE_ATOMIC_H */
