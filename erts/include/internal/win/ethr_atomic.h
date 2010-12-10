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

#undef ETHR_INCLUDE_ATOMIC_IMPL__
#if !defined(ETHR_WIN_ATOMIC32_H__) && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)
#define ETHR_WIN_ATOMIC32_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#undef ETHR_ATOMIC_WANT_32BIT_IMPL__
#elif !defined(ETHR_WIN_ATOMIC64_H__) && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)
#define ETHR_WIN_ATOMIC64_H__
#ifdef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64
/* _InterlockedCompareExchange64() required... */
#define ETHR_INCLUDE_ATOMIC_IMPL__ 8
#endif
#undef ETHR_ATOMIC_WANT_64BIT_IMPL__
#endif

#ifdef ETHR_INCLUDE_ATOMIC_IMPL__

#if defined(_MSC_VER) && _MSC_VER >= 1400

#ifndef ETHR_WIN_ATOMIC_COMMON__
#define ETHR_WIN_ATOMIC_COMMON__

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

#endif /* ETHR_WIN_ATOMIC_COMMON__ */

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4

#define ETHR_HAVE_NATIVE_ATOMIC32 1

/*
 * All used operations available as 32-bit intrinsics
 */

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

#define ETHR_ILCKD__(X) _Interlocked ## X
#ifdef ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS
#define ETHR_ILCKD_ACQ__(X) _Interlocked ## X ## _acq
#define ETHR_ILCKD_REL__(X) _Interlocked ## X ## _rel
#else
#define ETHR_ILCKD_ACQ__(X) _Interlocked ## X
#define ETHR_ILCKD_REL__(X) _Interlocked ## X
#endif

#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t

#elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8

#define ETHR_HAVE_NATIVE_ATOMIC64 1

/*
 * _InterlockedCompareExchange64() is required. The other may not
 * be available, but if so, we can generate them.
 */
#pragma intrinsic(_InterlockedCompareExchange64)

#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__		
#define ETHR_OWN_ILCKD_INIT_VAL__(PTR) *(PTR)
#else
#define ETHR_OWN_ILCKD_INIT_VAL__(PTR) (__int64) 0
#endif

#define ETHR_OWN_ILCKD_BODY_IMPL__(FUNC, PTR, NEW, ACT, EXP, OPS, RET)	\
{									\
    __int64 NEW, ACT, EXP;						\
    ACT = ETHR_OWN_ILCKD_INIT_VAL__(PTR);				\
    do {								\
	EXP = ACT;							\
	{ OPS; }							\
	ACT = _InterlockedCompareExchange64(PTR, NEW, EXP);		\
    } while (ACT != EXP);						\
    return RET;								\
}

#define ETHR_OWN_ILCKD_1_IMPL__(FUNC, NEW, ACT, EXP, OPS, RET)		\
static __forceinline __int64						\
FUNC(__int64 volatile *ptr)						\
ETHR_OWN_ILCKD_BODY_IMPL__(FUNC, ptr, NEW, ACT, EXP, OPS, RET)

#define ETHR_OWN_ILCKD_2_IMPL__(FUNC, NEW, ACT, EXP, OPS, ARG, RET)	\
static __forceinline __int64						\
FUNC(__int64 volatile *ptr, __int64 ARG)				\
ETHR_OWN_ILCKD_BODY_IMPL__(FUNC, ptr, NEW, ACT, EXP, OPS, RET)


#ifdef ETHR_HAVE__INTERLOCKEDDECREMENT64
#pragma intrinsic(_InterlockedDecrement64)
#else
ETHR_OWN_ILCKD_1_IMPL__(_InterlockedDecrement64, new, act, exp,
			new = act - 1, new)
#endif
#ifdef ETHR_HAVE__INTERLOCKEDINCREMENT64
#pragma intrinsic(_InterlockedIncrement64)
#else
ETHR_OWN_ILCKD_1_IMPL__(_InterlockedIncrement64, new, act, exp,
			new = act + 1, new)
#endif
#ifdef ETHR_HAVE__INTERLOCKEDEXCHANGEADD64
#pragma intrinsic(_InterlockedExchangeAdd64)
#else
ETHR_OWN_ILCKD_2_IMPL__(_InterlockedExchangeAdd64, new, act, exp,
			new = act + arg, arg, act)
#endif
#ifdef ETHR_HAVE__INTERLOCKEDEXCHANGE64
#pragma intrinsic(_InterlockedExchange64)
#else
ETHR_OWN_ILCKD_2_IMPL__(_InterlockedExchange64, new, act, exp,
			new = arg, arg, act)
#endif
#ifdef ETHR_HAVE__INTERLOCKEDAND64
#pragma intrinsic(_InterlockedAnd64)
#else
ETHR_OWN_ILCKD_2_IMPL__(_InterlockedAnd64, new, act, exp,
			new = act & arg, arg, act)
#endif
#ifdef ETHR_HAVE__INTERLOCKEDOR64
#pragma intrinsic(_InterlockedOr64)
#else
ETHR_OWN_ILCKD_2_IMPL__(_InterlockedOr64, new, act, exp,
			new = act | arg, arg, act)
#endif
#ifdef ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS
#pragma intrinsic(_InterlockedExchangeAdd64_acq)
#pragma intrinsic(_InterlockedIncrement64_acq)
#pragma intrinsic(_InterlockedDecrement64_rel)
#pragma intrinsic(_InterlockedCompareExchange64_acq)
#pragma intrinsic(_InterlockedCompareExchange64_rel)
#endif

#define ETHR_ILCKD__(X) _Interlocked ## X ## 64
#ifdef ETHR_HAVE_INTERLOCKED_ACQUIRE_RELEASE_BARRIERS
#define ETHR_ILCKD_ACQ__(X) _Interlocked ## X ## 64_acq
#define ETHR_ILCKD_REL__(X) _Interlocked ## X ## 64_rel
#else
#define ETHR_ILCKD_ACQ__(X) _Interlocked ## X ## 64
#define ETHR_ILCKD_REL__(X) _Interlocked ## X ## 64
#endif

#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t

#else
#error "Unsupported integer size"
#endif

typedef struct {
    volatile ETHR_AINT_T__ value;
} ETHR_ATMC_T__;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

static ETHR_INLINE ETHR_AINT_T__ *
ETHR_NATMC_FUNC__(addr)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__ *) &var->value;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(init)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
    var->value = i;
#else
    (void) ETHR_ILCKD__(Exchange)(&var->value, i);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
    var->value = i;
#else
    (void) ETHR_ILCKD__(Exchange)(&var->value, i);
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
    return var->value;
#else
    return ETHR_ILCKD__(ExchangeAdd)(&var->value, (ETHR_AINT_T__) 0);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(add)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    (void) ETHR_ILCKD__(ExchangeAdd)(&var->value, incr);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    return ETHR_ILCKD__(ExchangeAdd)(&var->value, i) + i;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(inc)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_ILCKD__(Increment)(&var->value);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_ILCKD__(Decrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD__(Increment)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD__(Decrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return ETHR_ILCKD__(And)(&var->value, mask);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return ETHR_ILCKD__(Or)(&var->value, mask);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var,
			   ETHR_AINT_T__ new,
			   ETHR_AINT_T__ old)
{
    return ETHR_ILCKD__(CompareExchange)(&var->value, new, old);
}


static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(xchg)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ new)
{
    return ETHR_ILCKD__(Exchange)(&var->value, new);
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
    return ETHR_ILCKD_ACQ__(ExchangeAdd)(&var->value, (ETHR_AINT_T__) 0);
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_acqb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD_ACQ__(Increment)(&var->value);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__
    ETHR_COMPILER_BARRIER;
    var->value = i;
#else
    (void) ETHR_ILCKD_REL__(Exchange)(&var->value, i);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec_relb)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_ILCKD_REL__(Decrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_relb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD_REL__(Decrement)(&var->value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_ILCKD_ACQ__(CompareExchange)(&var->value, new, old);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_relb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_ILCKD_REL__(CompareExchange)(&var->value, new, old);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_ILCKD__
#undef ETHR_ILCKD_ACQ__
#undef ETHR_ILCKD_REL__
#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
#undef ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__

#endif /* _MSC_VER */

#endif /* ETHR_INCLUDE_ATOMIC_IMPL__ */
