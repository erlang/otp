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
 * Description: Native atomics ethread support using gcc's builtins
 * Author: Rickard Green
 */

#ifndef ETHR_GCC_ATOMIC_H__
#define ETHR_GCC_ATOMIC_H__

#if !defined(ETHR_HAVE_NATIVE_ATOMICS) && defined(ETHR_HAVE_GCC_ATOMIC_OPS)
#define ETHR_HAVE_NATIVE_ATOMICS 1

#define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 0
/* Enable immediate read/write on platforms where we know it is safe */
#if defined(__i386__) || defined(__x86_64__) || defined(__sparc__) \
    || defined(__powerpc__) || defined(__ppc__)
#  undef ETHR_IMMED_ATOMIC_SET_GET_SAFE__
#  define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 1
#endif

typedef struct {
    volatile long counter;
} ethr_native_atomic_t;


/*
 * According to the documentation this is what we want:
 *   #define ETHR_MEMORY_BARRIER __sync_synchronize()
 * However, __sync_synchronize() is known to erroneously be
 * a noop on at least some platforms with some gcc versions.
 * This has suposedly been fixed in some gcc version, but we
 * don't know from which version. Therefore, we use the
 * workaround implemented below on all gcc versions.
 */
#define ETHR_MEMORY_BARRIER \
do { \
    volatile long x___ = 0; \
    (void) __sync_val_compare_and_swap(&x___, (long) 0, (long) 1); \
} while (0)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_atomic_set(ethr_native_atomic_t *var, long value)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    var->counter = value;
#else
    /*
     * Unfortunately no __sync_store() or similar exist in the gcc atomic
     * op interface. We therefore have to simulate it this way...
     */
    long act = 0, exp;
    do {
	exp = act;
	act = __sync_val_compare_and_swap(&var->counter, exp, value);
    } while (act != exp);
#endif
}

#define ethr_native_atomic_init ethr_native_atomic_set

static ETHR_INLINE long
ethr_native_atomic_read(ethr_native_atomic_t *var)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    return var->counter;
#else
    /*
     * Unfortunately no __sync_fetch() or similar exist in the gcc atomic
     * op interface. We therefore have to simulate it this way...
     */
    return __sync_add_and_fetch(&var->counter, (long) 0);
#endif
}

static ETHR_INLINE void
ethr_native_atomic_add(ethr_native_atomic_t *var, long incr)
{
    (void) __sync_add_and_fetch(&var->counter, incr);
}

static ETHR_INLINE long
ethr_native_atomic_add_return(ethr_native_atomic_t *var, long incr)
{
    return __sync_add_and_fetch(&var->counter, incr);
}

static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    (void) __sync_add_and_fetch(&var->counter, (long) 1);
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    (void) __sync_sub_and_fetch(&var->counter, (long) 1);
}

static ETHR_INLINE long
ethr_native_atomic_inc_return(ethr_native_atomic_t *var)
{
    return __sync_add_and_fetch(&var->counter, (long) 1);
}

static ETHR_INLINE long
ethr_native_atomic_dec_return(ethr_native_atomic_t *var)
{
    return __sync_sub_and_fetch(&var->counter, (long) 1);
}

static ETHR_INLINE long
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, long mask)
{
    return __sync_fetch_and_and(&var->counter, mask);
}

static ETHR_INLINE long
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, long mask)
{
    return (long) __sync_fetch_and_or(&var->counter, mask);
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg(ethr_native_atomic_t *var, long new, long old)
{
    return __sync_val_compare_and_swap(&var->counter, old, new);
}

static ETHR_INLINE long
ethr_native_atomic_xchg(ethr_native_atomic_t *var, long new)
{
    long exp, act = 0;
    do {
	exp = act;
	act = __sync_val_compare_and_swap(&var->counter, exp, new);
    } while (act != exp);
    return act;
}

#endif

#endif

#endif
