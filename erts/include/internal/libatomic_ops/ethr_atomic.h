/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010. All Rights Reserved.
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
 * Description: Native atomics ethread support using libatomic_ops
 * Author: Rickard Green
 */

#ifndef ETHR_LIBATOMIC_OPS_ATOMIC_H__
#define ETHR_LIBATOMIC_OPS_ATOMIC_H__

#if !defined(ETHR_HAVE_NATIVE_ATOMICS) && defined(ETHR_HAVE_LIBATOMIC_OPS)
#define ETHR_HAVE_NATIVE_ATOMICS 1

#if (defined(__i386__) && !defined(ETHR_PRE_PENTIUM4_COMPAT)) \
    || defined(__x86_64__)
#define AO_USE_PENTIUM4_INSTRS
#endif

#include "atomic_ops.h"

/*
 * libatomic_ops can be downloaded from:
 *   http://www.hpl.hp.com/research/linux/atomic_ops/
 *
 * These operations need to be defined by libatomic_ops;
 * otherwise, we won't compile:
 * - AO_nop_full()
 * - AO_load()
 * - AO_store()
 * - AO_compare_and_swap()
 *
 * The `AO_t' type also have to be at least as large as
 * `void *' and `long' types.
 */

#if ETHR_SIZEOF_AO_T < ETHR_SIZEOF_PTR
#error The AO_t type is too small
#endif

typedef struct {
    volatile AO_t counter;
} ethr_native_atomic_t;

#define ETHR_MEMORY_BARRIER AO_nop_full()
#ifdef AO_HAVE_nop_write
# define ETHR_WRITE_MEMORY_BARRIER AO_nop_write()
#else
# define ETHR_WRITE_MEMORY_BARRIER ETHR_MEMORY_BARRIER
#endif
#ifdef AO_HAVE_nop_read
#  define ETHR_READ_MEMORY_BARRIER AO_nop_read()
#else
#  define ETHR_READ_MEMORY_BARRIER ETHR_MEMORY_BARRIER
#endif
#ifdef AO_NO_DD_ORDERING
#  define ETHR_READ_DEPEND_MEMORY_BARRIER ETHR_READ_MEMORY_BARRIER
#else
#  define ETHR_READ_DEPEND_MEMORY_BARRIER __asm__ __volatile__("":::"memory")
#endif

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_atomic_set(ethr_native_atomic_t *var, long value)
{
    AO_store(&var->counter, (AO_t) value);
}

static ETHR_INLINE void
ethr_native_atomic_init(ethr_native_atomic_t *var, long value)
{
    ethr_native_atomic_set(var, value);
}

static ETHR_INLINE long
ethr_native_atomic_read(ethr_native_atomic_t *var)
{
    return (long) AO_load(&var->counter);
}

static ETHR_INLINE long
ethr_native_atomic_add_return(ethr_native_atomic_t *var, long incr)
{
#ifdef AO_HAVE_fetch_and_add
    return ((long) AO_fetch_and_add(&var->counter, (AO_t) incr)) + incr;
#else
    while (1) {
	AO_t exp = AO_load(&var->counter);
	AO_t new = exp + (AO_t) incr;
	if (AO_compare_and_swap(&var->counter, exp, new))
	    return (long) new;
    }
#endif
}

static ETHR_INLINE void
ethr_native_atomic_add(ethr_native_atomic_t *var, long incr)
{
    (void) ethr_native_atomic_add_return(var, incr);
}

static ETHR_INLINE long
ethr_native_atomic_inc_return(ethr_native_atomic_t *var)
{
#ifdef AO_HAVE_fetch_and_add1
    return ((long) AO_fetch_and_add1(&var->counter)) + 1;
#else
    return ethr_native_atomic_add_return(var, 1);
#endif
}

static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    (void) ethr_native_atomic_inc_return(var);
}

static ETHR_INLINE long
ethr_native_atomic_dec_return(ethr_native_atomic_t *var)
{
#ifdef AO_HAVE_fetch_and_sub1
    return ((long) AO_fetch_and_sub1(&var->counter)) - 1;
#else
    return ethr_native_atomic_add_return(var, -1);
#endif
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    (void) ethr_native_atomic_dec_return(var);
}

static ETHR_INLINE long
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, long mask)
{
    while (1) {
	AO_t exp = AO_load(&var->counter);
	AO_t new = exp & ((AO_t) mask);
	if (AO_compare_and_swap(&var->counter, exp, new))
	    return (long) exp;
    }
}

static ETHR_INLINE long
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, long mask)
{
    while (1) {
	AO_t exp = AO_load(&var->counter);
	AO_t new = exp | ((AO_t) mask);
	if (AO_compare_and_swap(&var->counter, exp, new))
	    return (long) exp;
    }
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg(ethr_native_atomic_t *var, long new, long exp)
{
    long act;
    do {
	if (AO_compare_and_swap(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (long) AO_load(&var->counter);
    } while (act == exp);
    return act;
}

static ETHR_INLINE long
ethr_native_atomic_xchg(ethr_native_atomic_t *var, long new)
{
    while (1) {
	AO_t exp = AO_load(&var->counter);
	if (AO_compare_and_swap(&var->counter, exp, (AO_t) new))
	    return (long) exp;
    }
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE long
ethr_native_atomic_read_acqb(ethr_native_atomic_t *var)
{
#ifdef AO_HAVE_load_acquire
    return (long) AO_load_acquire(&var->counter);
#else
    long res = ethr_native_atomic_read(var);
    ETHR_MEMORY_BARRIER;
    return res;
#endif
}

static ETHR_INLINE long
ethr_native_atomic_inc_return_acqb(ethr_native_atomic_t *var)
{
#ifdef AO_HAVE_fetch_and_add1_acquire
    return ((long) AO_fetch_and_add1_acquire(&var->counter)) + 1;
#else
    long res = ethr_native_atomic_add_return(var, 1);
    ETHR_MEMORY_BARRIER;
    return res;
#endif
}

static ETHR_INLINE void
ethr_native_atomic_set_relb(ethr_native_atomic_t *var, long value)
{
#ifdef AO_HAVE_store_release
    AO_store_release(&var->counter, (AO_t) value);
#else
    ETHR_MEMORY_BARRIER;
    ethr_native_atomic_set(var, value);
#endif
}

static ETHR_INLINE long
ethr_native_atomic_dec_return_relb(ethr_native_atomic_t *var)
{
#ifdef AO_HAVE_fetch_and_sub1_release
    return ((long) AO_fetch_and_sub1_release(&var->counter)) - 1;
#else
    ETHR_MEMORY_BARRIER;
    return ethr_native_atomic_dec_return(var);
#endif
}

static ETHR_INLINE void
ethr_native_atomic_dec_relb(ethr_native_atomic_t *var)
{
    (void) ethr_native_atomic_dec_return_relb(var);
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg_acqb(ethr_native_atomic_t *var, long new, long exp)
{
#ifdef AO_HAVE_compare_and_swap_acquire
    long act;
    do {
	if (AO_compare_and_swap_acquire(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (long) AO_load(&var->counter);
    } while (act == exp);
    AO_nop_full();
    return act;
#else
    long act = ethr_native_atomic_cmpxchg(var, new, exp);
    ETHR_MEMORY_BARRIER;
    return act;
#endif
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg_relb(ethr_native_atomic_t *var, long new, long exp)
{
#ifdef AO_HAVE_compare_and_swap_release
    long act;
    do {
	if (AO_compare_and_swap_release(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (long) AO_load(&var->counter);
    } while (act == exp);
    return act;
#else
    ETHR_MEMORY_BARRIER;
    return ethr_native_atomic_cmpxchg(var, new, exp);
#endif
}


#endif

#endif

#endif
