/* 
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2011. All Rights Reserved.
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
 * Native ethread atomics on SPARC V9.
 * Author: Mikael Pettersson.
 */

#undef ETHR_INCLUDE_ATOMIC_IMPL__
#if !defined(ETHR_SPARC_V9_ATOMIC32_H__) && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)
#define ETHR_SPARC_V9_ATOMIC32_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#undef ETHR_ATOMIC_WANT_32BIT_IMPL__
#elif !defined(ETHR_SPARC_V9_ATOMIC64_H__) && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)
#define ETHR_SPARC_V9_ATOMIC64_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 8
#undef ETHR_ATOMIC_WANT_64BIT_IMPL__
#endif

#ifdef ETHR_INCLUDE_ATOMIC_IMPL__

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL "ethread"
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#define ETHR_CAS__ "cas"
#elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATIVE_ATOMIC64_IMPL "ethread"
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t
#define ETHR_CAS__ "casx"
#else
#error "Unsupported integer size"
#endif

typedef struct {
    volatile ETHR_AINT_T__ counter;
} ETHR_ATMC_T__;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADDR 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADDR 1
#endif

static ETHR_INLINE ETHR_AINT_T__ *
ETHR_NATMC_FUNC__(addr)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__ *) &var->counter;
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    var->counter = i;
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
    return var->counter;
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ new, ETHR_AINT_T__ old)
{
    __asm__ __volatile__(
      ETHR_CAS__ " [%2], %1, %0"
      : "=&r"(new)
      : "r"(old), "r"(&var->counter), "0"(new)
      : "memory");
    return new;
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ new, ETHR_AINT_T__ old)
{
    ETHR_AINT_T__ res = ETHR_NATMC_FUNC__(cmpxchg)(var, new, old);
    ETHR_MEMBAR(ETHR_StoreLoad|ETHR_StoreStore);
    return res;
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_CAS__

#endif /* ETHR_INCLUDE_ATOMIC_IMPL__ */
