/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

#undef ETHR_INCLUDE_ATOMIC_IMPL__
#if !defined(ETHR_GCC_ATOMIC32_H__) && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)
#define ETHR_GCC_ATOMIC32_H__
#if defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP32)
#  define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#endif
#undef ETHR_ATOMIC_WANT_32BIT_IMPL__
#elif !defined(ETHR_GCC_ATOMIC64_H__) && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)
#define ETHR_GCC_ATOMIC64_H__
#if defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP64)
#  define ETHR_INCLUDE_ATOMIC_IMPL__ 8
#endif
#undef ETHR_ATOMIC_WANT_64BIT_IMPL__
#endif

#ifdef ETHR_INCLUDE_ATOMIC_IMPL__

#ifndef ETHR_GCC_ATOMIC_COMMON__
#define ETHR_GCC_ATOMIC_COMMON__

#define ETHR_READ_AND_SET_WITHOUT_SYNC_OP__ 0
#if defined(__i386__) || defined(__x86_64__) || defined(__sparc__) \
    || defined(__powerpc__) || defined(__ppc__) || defined(__mips__)
#  undef ETHR_READ_AND_SET_WITHOUT_SYNC_OP__
#  define ETHR_READ_AND_SET_WITHOUT_SYNC_OP__ 1
#endif

#endif /* ETHR_GCC_ATOMIC_COMMON__ */

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL "gcc"
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#if defined(ETHR_HAVE___SYNC_ADD_AND_FETCH32)
#  define ETHR_HAVE___SYNC_ADD_AND_FETCH
#endif
#if defined(ETHR_HAVE___SYNC_FETCH_AND_AND32)
#  define ETHR_HAVE___SYNC_FETCH_AND_AND
#endif 
#if defined(ETHR_HAVE___SYNC_FETCH_AND_OR32)
#  define ETHR_HAVE___SYNC_FETCH_AND_OR
#endif
#elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATIVE_ATOMIC64_IMPL "gcc"
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t
#if defined(ETHR_HAVE___SYNC_ADD_AND_FETCH64)
#  define ETHR_HAVE___SYNC_ADD_AND_FETCH
#endif
#if defined(ETHR_HAVE___SYNC_FETCH_AND_AND64)
#  define ETHR_HAVE___SYNC_FETCH_AND_AND
#endif 
#if defined(ETHR_HAVE___SYNC_FETCH_AND_OR64)
#  define ETHR_HAVE___SYNC_FETCH_AND_OR
#endif
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

#if ETHR_READ_AND_SET_WITHOUT_SYNC_OP__

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    var->counter = value;
}

#endif /* ETHR_READ_AND_SET_WITHOUT_SYNC_OP__ */

#if ETHR_READ_AND_SET_WITHOUT_SYNC_OP__

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

#endif /* ETHR_READ_AND_SET_WITHOUT_SYNC_OP__ */

#if defined(ETHR_HAVE___SYNC_ADD_AND_FETCH)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return __sync_add_and_fetch(&var->counter, incr);
}

#endif

#if defined(ETHR_HAVE___SYNC_FETCH_AND_AND)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_AND_RETOLD_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __sync_fetch_and_and(&var->counter, mask);
}

#endif

#if defined(ETHR_HAVE___SYNC_FETCH_AND_OR)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_OR_RETOLD_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return (ETHR_AINT_T__) __sync_fetch_and_or(&var->counter, mask);
}

#endif

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_mb)(ETHR_ATMC_T__ *var,
			      ETHR_AINT_T__ new,
			      ETHR_AINT_T__ old)
{
    return __sync_val_compare_and_swap(&var->counter, old, new);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_AINT_SUFFIX__
#undef ETHR_HAVE___SYNC_ADD_AND_FETCH
#undef ETHR_HAVE___SYNC_FETCH_AND_AND
#undef ETHR_HAVE___SYNC_FETCH_AND_OR

#endif
