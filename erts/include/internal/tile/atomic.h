/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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

#include <atomic.h>

/* An atomic is an aligned int accessed via locked operations.
 */
typedef struct {
    volatile ethr_sint32_t counter;
} ethr_native_atomic32_t;

#define ETHR_MEMORY_BARRIER __insn_mf()

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

static ETHR_INLINE ethr_sint32_t *
ethr_native_atomic32_addr(ethr_native_atomic32_t *var)
{
    return (ethr_sint32_t *) &var->counter;
}

static ETHR_INLINE void
ethr_native_atomic32_init(ethr_native_atomic32_t *var, ethr_sint32_t i)
{
    var->counter = i;
}

static ETHR_INLINE void
ethr_native_atomic32_set(ethr_native_atomic32_t *var, ethr_sint32_t i)
{
    atomic_exchange_acq(&var->counter, i);
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_read(ethr_native_atomic32_t *var)
{
    return var->counter;
}

static ETHR_INLINE void
ethr_native_atomic32_add(ethr_native_atomic32_t *var, ethr_sint32_t incr)
{
    ETHR_MEMORY_BARRIER;
    atomic_add(&var->counter, incr);
    ETHR_MEMORY_BARRIER;
}      
       
static ETHR_INLINE void
ethr_native_atomic32_inc(ethr_native_atomic32_t *var)
{
    ETHR_MEMORY_BARRIER;
    atomic_increment(&var->counter);
    ETHR_MEMORY_BARRIER;
}

static ETHR_INLINE void
ethr_native_atomic32_dec(ethr_native_atomic32_t *var)
{
    ETHR_MEMORY_BARRIER;
    atomic_decrement(&var->counter);
    ETHR_MEMORY_BARRIER;
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_add_return(ethr_native_atomic32_t *var, ethr_sint32_t incr)
{
    ethr_sint32_t res;
    ETHR_MEMORY_BARRIER;
    res = atomic_exchange_and_add(&var->counter, incr) + incr;
    ETHR_MEMORY_BARRIER;
    return res;
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_inc_return(ethr_native_atomic32_t *var)
{
    return ethr_native_atomic32_add_return(var, 1);
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_dec_return(ethr_native_atomic32_t *var)
{
    return ethr_native_atomic32_add_return(var, -1);
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_and_retold(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_sint32_t res;
    ETHR_MEMORY_BARRIER;
    res = atomic_and_val(&var->counter, mask);
    ETHR_MEMORY_BARRIER;
    return res;
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_or_retold(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_sint32_t res;
    ETHR_MEMORY_BARRIER;
    res = atomic_or_val(&var->counter, mask);
    ETHR_MEMORY_BARRIER;
    return res;
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_xchg(ethr_native_atomic32_t *var, ethr_sint32_t val)
{   
    ETHR_MEMORY_BARRIER;
    return atomic_exchange_acq(&var->counter, val);
} 

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_cmpxchg(ethr_native_atomic32_t *var,
			     ethr_sint32_t new,
			     ethr_sint32_t expected)
{
    ETHR_MEMORY_BARRIER;
    return atomic_compare_and_exchange_val_acq(&var->counter, new, expected);
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_read_acqb(ethr_native_atomic32_t *var)
{
    ethr_sint32_t res = ethr_native_atomic32_read(var);
    ETHR_MEMORY_BARRIER;
    return res;
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_inc_return_acqb(ethr_native_atomic32_t *var)
{
    return ethr_native_atomic32_inc_return(var);
}

static ETHR_INLINE void
ethr_native_atomic32_set_relb(ethr_native_atomic32_t *var, ethr_sint32_t val)
{
    ETHR_MEMORY_BARRIER;
    ethr_native_atomic32_set(var, val);
}

static ETHR_INLINE void
ethr_native_atomic32_dec_relb(ethr_native_atomic32_t *var)
{
    ethr_native_atomic32_dec(var);
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_dec_return_relb(ethr_native_atomic32_t *var)
{
    return ethr_native_atomic32_dec_return(var);
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_cmpxchg_acqb(ethr_native_atomic32_t *var,
				  ethr_sint32_t new,
				  ethr_sint32_t exp)
{
    return ethr_native_atomic32_cmpxchg(var, new, exp);
}

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_cmpxchg_relb(ethr_native_atomic32_t *var,
				  ethr_sint32_t new,
				  ethr_sint32_t exp)
{
    return ethr_native_atomic32_cmpxchg(var, new, exp);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_TILE_ATOMIC_H */
