/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

#include <atomic.h>

/* An atomic is an aligned int accessed via locked operations.
 */
typedef struct {
    volatile long counter;
} ethr_native_atomic_t;

#define ETHR_MEMORY_BARRIER __insn_mf()

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_atomic_init(ethr_native_atomic_t *var, long i)
{
    var->counter = i;
}

static ETHR_INLINE void
ethr_native_atomic_set(ethr_native_atomic_t *var, long i)
{
    atomic_exchange_acq(&var->counter, i);
}

static ETHR_INLINE long
ethr_native_atomic_read(ethr_native_atomic_t *var)
{
    return var->counter;
}

static ETHR_INLINE void
ethr_native_atomic_add(ethr_native_atomic_t *var, long incr)
{
    atomic_add(&var->counter, incr);
}      
       
static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    atomic_increment(&var->counter);
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    atomic_decrement(&var->counter);
}

static ETHR_INLINE long
ethr_native_atomic_add_return(ethr_native_atomic_t *var, long incr)
{
    return atomic_exchange_and_add(&var->counter, incr) + incr;
}

static ETHR_INLINE long
ethr_native_atomic_inc_return(ethr_native_atomic_t *var)
{
    return ethr_native_atomic_add_return(var, 1);
}

static ETHR_INLINE long
ethr_native_atomic_dec_return(ethr_native_atomic_t *var)
{
    return ethr_native_atomic_add_return(var, -1);
}

static ETHR_INLINE long
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, long mask)
{
    return atomic_and_val(&var->counter, mask);
}

static ETHR_INLINE long
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, long mask)
{
    return atomic_or_val(&var->counter, mask);
}

static ETHR_INLINE long
ethr_native_atomic_xchg(ethr_native_atomic_t *var, long val)
{   
    return atomic_exchange_acq(&var->counter, val);
} 

static ETHR_INLINE long
ethr_native_atomic_cmpxchg(ethr_native_atomic_t *var, long new, long expected)
{
    return atomic_compare_and_exchange_val_acq(&var->counter, new, expected);
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE long
ethr_native_atomic_read_acqb(ethr_native_atomic_t *var)
{
    long res = ethr_native_atomic_read(var);
    ETHR_MEMORY_BARRIER;
    return res;
}

static ETHR_INLINE long
ethr_native_atomic_inc_return_acqb(ethr_native_atomic_t *var)
{
    long res = ethr_native_atomic_inc_return(var);
    ETHR_MEMORY_BARRIER;
    return res;
}

static ETHR_INLINE void
ethr_native_atomic_set_relb(ethr_native_atomic_t *var, long val)
{
    ETHR_MEMORY_BARRIER;
    ethr_native_atomic_set(var, val);
}

static ETHR_INLINE void
ethr_native_atomic_dec_relb(ethr_native_atomic_t *var)
{
    ETHR_MEMORY_BARRIER;
    ethr_native_atomic_dec(var);
}

static ETHR_INLINE long
ethr_native_atomic_dec_return_relb(ethr_native_atomic_t *var)
{
    ETHR_MEMORY_BARRIER;
    return ethr_native_atomic_dec_return(var);
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg_acqb(ethr_native_atomic_t *var, long new, long exp)
{
    return ethr_native_atomic_cmpxchg(var, new, exp);
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg_relb(ethr_native_atomic_t *var, long new, long exp)
{
    ETHR_MEMORY_BARRIER;
    return ethr_native_atomic_cmpxchg(var, new, exp);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_TILE_ATOMIC_H */
