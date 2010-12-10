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

#if defined(_MSC_VER) && _MSC_VER >= 1400
#define ETHR_HAVE_NATIVE_ATOMICS 1

#if defined(_M_IX86) || defined(_M_AMD64) || defined(_M_IA64)
#  define ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__ 1
#else
#  define ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__ 0
#endif

#if defined(_M_AMD64) || (defined(_M_IX86) \
			  && !defined(ETHR_PRE_PENTIUM4_COMPAT))
#  define ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__ 1
#else
#  define ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__ 0
#endif
/*
 * No configure test checking for interlocked acquire/release
 * versions have been written, yet. It should define
 * ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS if, and
 * only if, all used interlocked operations with barriers
 * exists.
 *
 * Note, that these are pure optimizations for the itanium
 * processor.
 */

#include <intrin.h>
#undef ETHR_COMPILER_BARRIER
#define ETHR_COMPILER_BARRIER _ReadWriteBarrier()
#pragma intrinsic(_ReadWriteBarrier)
#pragma intrinsic(_InterlockedCompareExchange)

#if defined(_M_AMD64) || (defined(_M_IX86) \
			  && !defined(ETHR_PRE_PENTIUM4_COMPAT))
#include <emmintrin.h>
#include <mmintrin.h>
#pragma intrinsic(_mm_mfence)
#define ETHR_MEMORY_BARRIER _mm_mfence()
#pragma intrinsic(_mm_sfence)
#define ETHR_WRITE_MEMORY_BARRIER _mm_sfence()
#pragma intrinsic(_mm_lfence)
#define ETHR_READ_MEMORY_BARRIER _mm_lfence()
#define ETHR_READ_DEPEND_MEMORY_BARRIER ETHR_COMPILER_BARRIER

#else

#define ETHR_MEMORY_BARRIER					\
do {								\
    volatile long x___ = 0;					\
    _InterlockedCompareExchange(&x___, (long) 1, (long) 0);	\
} while (0)

#endif

#if ETHR_SIZEOF_PTR == 4

#pragma intrinsic(_InterlockedDecrement)
#pragma intrinsic(_InterlockedIncrement)
#pragma intrinsic(_InterlockedExchangeAdd)
#pragma intrinsic(_InterlockedExchange)
#pragma intrinsic(_InterlockedAnd)
#pragma intrinsic(_InterlockedOr)
#ifdef ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS
#pragma intrinsic(_InterlockedExchangeAdd_acq)
#pragma intrinsic(_InterlockedIncrement_acq)
#pragma intrinsic(_InterlockedDecrement_rel)
#pragma intrinsic(_InterlockedCompareExchange_acq)
#pragma intrinsic(_InterlockedCompareExchange_rel)
#endif

#define ETHR_ATMC__(X) X
#ifdef ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS
#define ETHR_ATMC_ACQ__(X) X ## _acq
#define ETHR_ATMC_REL__(X) X ## _rel
#else
#define ETHR_ATMC_ACQ__(X) X
#define ETHR_ATMC_REL__(X) X
#endif

#elif ETHR_SIZEOF_PTR == 8

#pragma intrinsic(_InterlockedDecrement64)
#pragma intrinsic(_InterlockedIncrement64)
#pragma intrinsic(_InterlockedExchangeAdd64)
#pragma intrinsic(_InterlockedExchange64)
#pragma intrinsic(_InterlockedAnd64)
#pragma intrinsic(_InterlockedOr64)
#ifdef ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS
#pragma intrinsic(_InterlockedExchangeAdd64_acq)
#pragma intrinsic(_InterlockedIncrement64_acq)
#pragma intrinsic(_InterlockedDecrement64_rel)
#pragma intrinsic(_InterlockedCompareExchange64_acq)
#pragma intrinsic(_InterlockedCompareExchange64_rel)
#endif

#define ETHR_ATMC__(X) X ## 64
#ifdef ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS
#define ETHR_ATMC_ACQ__(X) X ## 64_acq
#define ETHR_ATMC_REL__(X) X ## 64_rel
#else
#define ETHR_ATMC_ACQ__(X) X ## 64
#define ETHR_ATMC_REL__(X) X ## 64
#endif

#else
#error "Unsupported word size"
#endif

#define ETHR_NATMC_FUNC__(X) ethr_native_atomic_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic_t
#define ETHR_AINT_T__ ethr_sint_t

typedef struct {
    volatile ETHR_AINT_T__ value;
} ETHR_ATMC_T__;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

static ETHR_INLINE void
ETHR_NATMC_FUNC__(init)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
    var->value = i;
#else
    (void) ETHR_ATMC__(_InterlockedExchange)(&var->value, i);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
    var->value = i;
#else
    (void) ETHR_ATMC__(_InterlockedExchange)(&var->value, i);
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
    return var->value;
#else
    return ETHR_ATMC__(_InterlockedExchangeAdd)(&var->value, (ETHR_AINT_T__) 0);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(add)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    (void) ETHR_ATMC__(_InterlockedExchangeAdd)(&var->value, incr);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    return ETHR_ATMC__(_InterlockedExchangeAdd)(&var->value, i) + i;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(inc)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_ATMC__(_InterlockedIncrement)(&var->value);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_ATMC__(_InterlockedDecrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return)(ETHR_ATMC_T__ *var)
{
    return ETHR_ATMC__(_InterlockedIncrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return)(ETHR_ATMC_T__ *var)
{
    return ETHR_ATMC__(_InterlockedDecrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return ETHR_ATMC__(_InterlockedAnd)(&var->value, mask);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return ETHR_ATMC__(_InterlockedOr)(&var->value, mask);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var,
			   ETHR_AINT_T__ new,
			   ETHR_AINT_T__ old)
{
    return ETHR_ATMC__(_InterlockedCompareExchange)(&var->value, new, old);
}


static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(xchg)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ new)
{
    return ETHR_ATMC__(_InterlockedExchange)(&var->value, new);
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_acqb)(ETHR_ATMC_T__ *var)
{
#if ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__
    ETHR_AINT_T__ val = var->value;
    ETHR_COMPILER_BARRIER;
    return val;
#else
    return ETHR_ATMC_ACQ__(_InterlockedExchangeAdd)(&var->value,
						    (ETHR_AINT_T__) 0);
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_acqb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ATMC_ACQ__(_InterlockedIncrement)(&var->value);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__
    ETHR_COMPILER_BARRIER;
    var->value = i;
#else
    (void) ETHR_ATMC_REL__(_InterlockedExchange)(&var->value, i);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec_relb)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_ATMC_REL__(_InterlockedDecrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_relb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ATMC_REL__(_InterlockedDecrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_ATMC_ACQ__(_InterlockedCompareExchange)(&var->value, new, old);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_relb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_ATMC_REL__(_InterlockedCompareExchange)(&var->value, new, old);
}

#endif

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
#undef ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__

#endif

#endif
