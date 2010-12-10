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

#undef ETHR_INCLUDE_ATOMIC_IMPL__
#if !defined(ETHR_GCC_ATOMIC32_H__) && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)
#define ETHR_GCC_ATOMIC32_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#undef ETHR_ATOMIC_WANT_32BIT_IMPL__
#elif !defined(ETHR_GCC_ATOMIC64_H__) && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)
#define ETHR_GCC_ATOMIC64_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 8
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

#if defined(__x86_64__) || (defined(__i386__) \
			    && !defined(ETHR_PRE_PENTIUM4_COMPAT))
#  define ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__ 1
#else
#  define ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__ 0
#endif

/*
 * According to the documentation this is what we want:
 *   #define ETHR_MEMORY_BARRIER __sync_synchronize()
 * However, __sync_synchronize() is known to erroneously be
 * a noop on at least some platforms with some gcc versions.
 * This has suposedly been fixed in some gcc version, but we
 * don't know from which version. Therefore, we only use
 * it when it has been verified to work. Otherwise
 * we use a workaround.
 */
#if defined(__mips__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 2))
/* __sync_synchronize() has been verified to work here */
#define ETHR_MEMORY_BARRIER __sync_synchronize()
#define ETHR_READ_DEPEND_MEMORY_BARRIER __sync_synchronize()
#elif defined(__x86_64__) || (defined(__i386__) \
			      && !defined(ETHR_PRE_PENTIUM4_COMPAT))
/* Use fence instructions directly instead of workaround */
#define ETHR_MEMORY_BARRIER __asm__ __volatile__("mfence" : : : "memory")
#define ETHR_WRITE_MEMORY_BARRIER __asm__ __volatile__("sfence" : : : "memory")
#define ETHR_READ_MEMORY_BARRIER __asm__ __volatile__("lfence" : : : "memory")
#define ETHR_READ_DEPEND_MEMORY_BARRIER __asm__ __volatile__("" : : : "memory")
#else
/* Workaround */
#define ETHR_MEMORY_BARRIER \
do { \
    volatile ethr_sint32_t x___ = 0; \
    (void) __sync_val_compare_and_swap(&x___, (ethr_sint32_t) 0, (ethr_sint32_t) 1); \
} while (0)
#define ETHR_READ_DEPEND_MEMORY_BARRIER ETHR_MEMORY_BARRIER
#endif

#define ETHR_COMPILER_BARRIER __asm__ __volatile__("" : : : "memory")

#endif /* ETHR_GCC_ATOMIC_COMMON__ */

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t
#else
#error "Unsupported integer size"
#endif

typedef struct {
    volatile ETHR_AINT_T__ counter;
} ETHR_ATMC_T__;


#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

static ETHR_INLINE ETHR_AINT_T__ *
ETHR_NATMC_FUNC__(addr)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__ *) &var->counter;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
#if ETHR_READ_AND_SET_WITHOUT_SYNC_OP__
    var->counter = value;
#else
    /*
     * Unfortunately no __sync_store() or similar exist in the gcc atomic
     * op interface. We therefore have to simulate it this way...
     */
    ETHR_AINT_T__ act = 0, exp;
    do {
	exp = act;
	act = __sync_val_compare_and_swap(&var->counter, exp, value);
    } while (act != exp);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(init)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    ETHR_NATMC_FUNC__(set)(var, value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
#if ETHR_READ_AND_SET_WITHOUT_SYNC_OP__
    return var->counter;
#else
    /*
     * Unfortunately no __sync_fetch() or similar exist in the gcc atomic
     * op interface. We therefore have to simulate it this way...
     */
    return __sync_add_and_fetch(&var->counter, (ETHR_AINT_T__) 0);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(add)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    (void) __sync_add_and_fetch(&var->counter, incr);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return __sync_add_and_fetch(&var->counter, incr);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(inc)(ETHR_ATMC_T__ *var)
{
    (void) __sync_add_and_fetch(&var->counter, (ETHR_AINT_T__) 1);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec)(ETHR_ATMC_T__ *var)
{
    (void) __sync_sub_and_fetch(&var->counter, (ETHR_AINT_T__) 1);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return)(ETHR_ATMC_T__ *var)
{
    return __sync_add_and_fetch(&var->counter, (ETHR_AINT_T__) 1);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return)(ETHR_ATMC_T__ *var)
{
    return __sync_sub_and_fetch(&var->counter, (ETHR_AINT_T__) 1);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __sync_fetch_and_and(&var->counter, mask);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return (ETHR_AINT_T__) __sync_fetch_and_or(&var->counter, mask);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var,
			   ETHR_AINT_T__ new,
			   ETHR_AINT_T__ old)
{
    return __sync_val_compare_and_swap(&var->counter, old, new);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(xchg)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ new)
{
    ETHR_AINT_T__ exp, act = 0;
    do {
	exp = act;
	act = __sync_val_compare_and_swap(&var->counter, exp, new);
    } while (act != exp);
    return act;
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_acqb)(ETHR_ATMC_T__ *var)
{
#if ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__
    ETHR_AINT_T__ val = var->counter;
    ETHR_COMPILER_BARRIER;
    return val;
#else
    return __sync_add_and_fetch(&var->counter, (ETHR_AINT_T__) 0);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if ETHR_READ_ACQB_AND_SET_RELB_COMPILER_BARRIER_ONLY__
    ETHR_COMPILER_BARRIER;
    var->counter = i;
#else
    (void) ETHR_NATMC_FUNC__(xchg)(var, i);
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_acqb)(ETHR_ATMC_T__ *var)
{
    return ETHR_NATMC_FUNC__(inc_return)(var);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec_relb)(ETHR_ATMC_T__ *var)
{
    ETHR_NATMC_FUNC__(dec)(var);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_relb)(ETHR_ATMC_T__ *var)
{
    return ETHR_NATMC_FUNC__(dec_return)(var);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_NATMC_FUNC__(cmpxchg)(var, new, old);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_relb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_NATMC_FUNC__(cmpxchg)(var, new, old);
}

#endif

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_AINT_SUFFIX__

#endif
