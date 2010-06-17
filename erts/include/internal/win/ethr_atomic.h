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
 * Description: Native atomics ethread support when using VC++
 * Author: Rickard Green
 */

#ifndef ETHR_WIN_ATOMIC_H__
#define ETHR_WIN_ATOMIC_H__

#ifdef _MSC_VER
#  if _MSC_VER < 1300
#    define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 0 /* Dont trust really old compilers */
#  else
#    if defined(_M_IX86)
#      define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 1
#    else /* I.e. IA64 */
#      if _MSC_VER >= 1400
#        define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 1
#      else
#        define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 0
#      endif
#    endif
#  endif
#  if _MSC_VER >= 1400
#    include <intrin.h>
#    undef ETHR_COMPILER_BARRIER
#    define ETHR_COMPILER_BARRIER _ReadWriteBarrier()
#  endif
#pragma intrinsic(_ReadWriteBarrier)
#pragma intrinsic(_InterlockedAnd)
#pragma intrinsic(_InterlockedOr)
#else
#    define ETHR_IMMED_ATOMIC_SET_GET_SAFE__ 0
#endif

/*
 * No configure test checking for _Interlocked*_{acq,rel} and
 * Interlocked*{Acquire,Release} have been written yet...
 *
 * Note, that these are pure optimizations for the itanium
 * processor.
 */

#ifdef ETHR_HAVE_INTERLOCKEDCOMPAREEXCHANGE_ACQ
#pragma intrinsic(_InterlockedCompareExchange_acq)
#endif
#ifdef ETHR_HAVE_INTERLOCKEDCOMPAREEXCHANGE_REL
#pragma intrinsic(_InterlockedCompareExchange_rel)
#endif


typedef struct {
    volatile LONG value;
} ethr_native_atomic_t;

#define ETHR_MEMORY_BARRIER \
do { \
    volatile LONG x___ = 0; \
    _InterlockedCompareExchange(&x___, (LONG) 1, (LONG) 0); \
} while (0)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_atomic_init(ethr_native_atomic_t *var, long i)
{
    var->value = (LONG) i;
}

static ETHR_INLINE void
ethr_native_atomic_set(ethr_native_atomic_t *var, long i)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    var->value = (LONG) i;
#else
    (void) InterlockedExchange(&var->value, (LONG) i);
#endif
}

static ETHR_INLINE long
ethr_native_atomic_read(ethr_native_atomic_t *var)
{
#if ETHR_IMMED_ATOMIC_SET_GET_SAFE__
    return var->value;
#else
    return InterlockedExchangeAdd(&var->value, (LONG) 0);
#endif
}

static ETHR_INLINE void
ethr_native_atomic_add(ethr_native_atomic_t *var, long incr)
{
    (void) InterlockedExchangeAdd(&var->value, (LONG) incr);
}

static ETHR_INLINE long
ethr_native_atomic_add_return(ethr_native_atomic_t *var, long i)
{
    LONG tmp = InterlockedExchangeAdd(&var->value, (LONG) i);
    return tmp + i;
}

static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    (void) InterlockedIncrement(&var->value);
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    (void) InterlockedDecrement(&var->value);
}

static ETHR_INLINE long
ethr_native_atomic_inc_return(ethr_native_atomic_t *var)
{
    return (long) InterlockedIncrement(&var->value);
}

static ETHR_INLINE long
ethr_native_atomic_dec_return(ethr_native_atomic_t *var)
{
    return (long) InterlockedDecrement(&var->value);
}

static ETHR_INLINE long
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, long mask)
{
    return (long) _InterlockedAnd(&var->value, mask);
}

static ETHR_INLINE long
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, long mask)
{
    return (long) _InterlockedOr(&var->value, mask);
}


static ETHR_INLINE long
ethr_native_atomic_cmpxchg(ethr_native_atomic_t *var, long new, long old)
{
    return (long) _InterlockedCompareExchange(&var->value, (LONG) new, (LONG) old);
}


static ETHR_INLINE long
ethr_native_atomic_xchg(ethr_native_atomic_t *var, long new)
{
    return (long) InterlockedExchange(&var->value, (LONG) new);
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE long
ethr_native_atomic_read_acqb(ethr_native_atomic_t *var)
{
#ifdef ETHR_HAVE_INTERLOCKEDEXCHANGEADDACQUIRE
    return (long) InterlockedExchangeAddAcquire(&var->value, (LONG) 0);
#else
    return (long) InterlockedExchangeAdd(&var->value, (LONG) 0);
#endif
}

static ETHR_INLINE long
ethr_native_atomic_inc_return_acqb(ethr_native_atomic_t *var)
{
#ifdef ETHR_HAVE_INTERLOCKEDINCREMENTACQUIRE
    return (long) InterlockedIncrementAcquire(&var->value);
#else
    return (long) InterlockedIncrement(&var->value);
#endif
}

static ETHR_INLINE void
ethr_native_atomic_set_relb(ethr_native_atomic_t *var, long i)
{
    (void) InterlockedExchange(&var->value, (LONG) i);
}

static ETHR_INLINE void
ethr_native_atomic_dec_relb(ethr_native_atomic_t *var)
{
#ifdef ETHR_HAVE_INTERLOCKEDDECREMENTRELEASE
    (void) InterlockedDecrementRelease(&var->value);
#else
    (void) InterlockedDecrement(&var->value);
#endif
}

static ETHR_INLINE long
ethr_native_atomic_dec_return_relb(ethr_native_atomic_t *var)
{
#ifdef ETHR_HAVE_INTERLOCKEDDECREMENTRELEASE
    return (long) InterlockedDecrementRelease(&var->value);
#else
    return (long) InterlockedDecrement(&var->value);
#endif
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg_acqb(ethr_native_atomic_t *var, long new, long old)
{
#ifdef ETHR_HAVE_INTERLOCKEDCOMPAREEXCHANGE_ACQ
    return (long) _InterlockedCompareExchange_acq(&var->value, (LONG) new, (LONG) old);
#else
    return (long) _InterlockedCompareExchange(&var->value, (LONG) new, (LONG) old);
#endif
}

static ETHR_INLINE long
ethr_native_atomic_cmpxchg_relb(ethr_native_atomic_t *var, long new, long old)
{

#ifdef ETHR_HAVE_INTERLOCKEDCOMPAREEXCHANGE_REL
    return (long) _InterlockedCompareExchange_rel(&var->value, (LONG) new, (LONG) old);
#else
    return (long) _InterlockedCompareExchange(&var->value, (LONG) new, (LONG) old);
#endif
}

#endif

#endif
