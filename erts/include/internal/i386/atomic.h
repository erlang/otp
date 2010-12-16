/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2010. All Rights Reserved.
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
 * Native ethread atomics on x86/x86-64.
 * Author: Mikael Pettersson.
 *
 * This code requires a 486 or newer processor.
 */

#undef ETHR_INCLUDE_ATOMIC_IMPL__
#if !defined(ETHR_X86_ATOMIC32_H__) && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)
#define ETHR_X86_ATOMIC32_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#undef ETHR_ATOMIC_WANT_32BIT_IMPL__
#elif !defined(ETHR_X86_ATOMIC64_H__) && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)
#define ETHR_X86_ATOMIC64_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 8
#undef ETHR_ATOMIC_WANT_64BIT_IMPL__
#endif

#ifdef ETHR_INCLUDE_ATOMIC_IMPL__

#ifndef ETHR_X86_ATOMIC_COMMON__
#define ETHR_X86_ATOMIC_COMMON__

#define ETHR_ATOMIC_HAVE_INC_DEC_INSTRUCTIONS 1

#if defined(__x86_64__) || !defined(ETHR_PRE_PENTIUM4_COMPAT)
#define ETHR_MEMORY_BARRIER __asm__ __volatile__("mfence" : : : "memory")
#define ETHR_WRITE_MEMORY_BARRIER __asm__ __volatile__("sfence" : : : "memory")
#define ETHR_READ_MEMORY_BARRIER __asm__ __volatile__("lfence" : : : "memory")
#define ETHR_READ_DEPEND_MEMORY_BARRIER __asm__ __volatile__("" : : : "memory")
#else
#define ETHR_MEMORY_BARRIER \
do { \
    volatile ethr_sint32_t x___ = 0; \
    __asm__ __volatile__("lock; incl %0" : "=m"(x___) : "m"(x___) : "memory"); \
} while (0)
#endif

#endif /* ETHR_X86_ATOMIC_COMMON__ */

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#define ETHR_AINT_SUFFIX__ "l"
#elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t
#define ETHR_AINT_SUFFIX__ "q"
#else
#error "Unsupported integer size"
#endif

/* An atomic is an aligned ETHR_AINT_T__ accessed via locked operations.
 */
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
ETHR_NATMC_FUNC__(init)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    var->counter = i;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    var->counter = i;
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
    return var->counter;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(add)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    __asm__ __volatile__(
       "lock; add" ETHR_AINT_SUFFIX__ " %1, %0"
       : "=m"(var->counter)
       : "ir"(incr), "m"(var->counter));
}      
       
static ETHR_INLINE void
ETHR_NATMC_FUNC__(inc)(ETHR_ATMC_T__ *var)
{
    __asm__ __volatile__(
	"lock; inc" ETHR_AINT_SUFFIX__ " %0"
	: "=m"(var->counter)
	: "m"(var->counter));
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec)(ETHR_ATMC_T__ *var)
{
    __asm__ __volatile__(
	"lock; dec" ETHR_AINT_SUFFIX__ " %0"
	: "=m"(var->counter)
	: "m"(var->counter));
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    ETHR_AINT_T__ tmp;

    tmp = incr;
    __asm__ __volatile__(
	"lock; xadd" ETHR_AINT_SUFFIX__ " %0, %1" /* xadd didn't exist prior to the 486 */
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp));
    /* now tmp is the atomic's previous value */
    return tmp + incr;
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return)(ETHR_ATMC_T__ *var)
{
    return ETHR_NATMC_FUNC__(add_return)(var, (ETHR_AINT_T__) 1);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return)(ETHR_ATMC_T__ *var)
{
    return ETHR_NATMC_FUNC__(add_return)(var, (ETHR_AINT_T__) -1);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var,
			   ETHR_AINT_T__ new,
			   ETHR_AINT_T__ old)
{
    __asm__ __volatile__(
      "lock; cmpxchg" ETHR_AINT_SUFFIX__ " %2, %3"
      : "=a"(old), "=m"(var->counter)
      : "r"(new), "m"(var->counter), "0"(old)
      : "cc", "memory"); /* full memory clobber to make this a compiler barrier */
    return old;
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    ETHR_AINT_T__ tmp, old;

    tmp = var->counter;
    do {
	old = tmp;
        tmp = ETHR_NATMC_FUNC__(cmpxchg)(var, tmp & mask, tmp);
    } while (__builtin_expect(tmp != old, 0));
    /* now tmp is the atomic's previous value */
    return tmp;
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    ETHR_AINT_T__ tmp, old;

    tmp = var->counter;
    do {
	old = tmp;
        tmp = ETHR_NATMC_FUNC__(cmpxchg)(var, tmp | mask, tmp);
    } while (__builtin_expect(tmp != old, 0));
    /* now tmp is the atomic's previous value */
    return tmp;
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(xchg)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ val)
{   
    ETHR_AINT_T__ tmp = val;
    __asm__ __volatile__(
	"xchg" ETHR_AINT_SUFFIX__ " %0, %1"
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp));
    /* now tmp is the atomic's previous value */ 
    return tmp;
} 

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_acqb)(ETHR_ATMC_T__ *var)
{
    ETHR_AINT_T__ val;
#if defined(__x86_64__) || !defined(ETHR_PRE_PENTIUM4_COMPAT)
    val = var->counter;
#else
    val = ETHR_NATMC_FUNC__(add_return)(var, 0);
#endif
    __asm__ __volatile__("" : : : "memory");
    return val;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    __asm__ __volatile__("" : : : "memory");
#if defined(__x86_64__) || !defined(ETHR_PRE_PENTIUM4_COMPAT)
    var->counter = i;
#else
    (void) ETHR_NATMC_FUNC__(xchg)(var, i);
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_acqb)(ETHR_ATMC_T__ *var)
{
    ETHR_AINT_T__ res = ETHR_NATMC_FUNC__(inc_return)(var);
    __asm__ __volatile__("" : : : "memory");
    return res;
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec_relb)(ETHR_ATMC_T__ *var)
{
    __asm__ __volatile__("" : : : "memory");
    ETHR_NATMC_FUNC__(dec)(var);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_relb)(ETHR_ATMC_T__ *var)
{
    __asm__ __volatile__("" : : : "memory");
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

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_AINT_SUFFIX__

#endif /* ETHR_INCLUDE_ATOMIC_IMPL__ */
