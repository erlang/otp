/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

#ifdef ETHR_TRY_INLINE_FUNCS

static ETHR_INLINE void
ethr_native_atomic_init(ethr_native_atomic_t *var, long i)
{
    var->counter = i;
}

static ETHR_INLINE void
ethr_native_atomic_set(ethr_native_atomic_t *var, long i)
{
    __insn_mf();
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
    __insn_mf();
    atomic_add(&var->counter, incr);
}      
       
static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    __insn_mf();
    atomic_increment(&var->counter);
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    __insn_mf();
    atomic_decrement(&var->counter);
}

static ETHR_INLINE long
ethr_native_atomic_add_return(ethr_native_atomic_t *var, long incr)
{
    __insn_mf();
    return atomic_exchange_and_add(&var->counter, incr) + incr;
}

static ETHR_INLINE long
ethr_native_atomic_inc_return(ethr_native_atomic_t *var)
{
    return ethr_native_atomic_add_return(&var->counter, 1);
}

static ETHR_INLINE long
ethr_native_atomic_dec_return(ethr_native_atomic_t *var)
{
    return ethr_native_atomic_add_return(&var->counter, -1);
}

static ETHR_INLINE long
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, long mask)
{
    /* Implement a barrier suitable for a mutex unlock. */
    __insn_mf();
    return atomic_and_val(&var->counter, mask);
}

static ETHR_INLINE long
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, long mask)
{
    __insn_mf();
    return atomic_or_val(&var->counter, mask);
}

static ETHR_INLINE long
ethr_native_atomic_xchg(ethr_native_atomic_t *var, long val)
{   
    __insn_mf();
    return atomic_exchange_acq(&var->counter, val);
} 

static ETHR_INLINE long
ethr_native_atomic_cmpxchg(ethr_native_atomic_t *var, long new, long expected)
{
    /* Implement a barrier suitable for a mutex unlock. */
    __insn_mf();
    return atomic_compare_and_exchange_val_acq(&var->counter, new, expected);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_TILE_ATOMIC_H */
