/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2011. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
#if !defined(ETHR_X86_ATOMIC32_H__) \
    && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)
#  define ETHR_X86_ATOMIC32_H__
#  define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#  undef ETHR_ATOMIC_WANT_32BIT_IMPL__
#elif !defined(ETHR_X86_ATOMIC64_H__) \
      && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)
#  define ETHR_X86_ATOMIC64_H__
#  define ETHR_INCLUDE_ATOMIC_IMPL__ 8
#  undef ETHR_ATOMIC_WANT_64BIT_IMPL__
#endif

#ifdef ETHR_INCLUDE_ATOMIC_IMPL__

#  ifndef ETHR_X86_ATOMIC_COMMON__
#    define ETHR_X86_ATOMIC_COMMON__
#    define ETHR_ATOMIC_HAVE_INC_DEC_INSTRUCTIONS 1
#  endif /* ETHR_X86_ATOMIC_COMMON__ */

#  if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#    define ETHR_HAVE_NATIVE_ATOMIC32 1
#    define ETHR_NATIVE_ATOMIC32_IMPL "ethread"
#    define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#    define ETHR_ATMC_T__ ethr_native_atomic32_t
#    define ETHR_AINT_T__ ethr_sint32_t
#    define ETHR_AINT_SUFFIX__ "l"
#  elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8
#    define ETHR_HAVE_NATIVE_ATOMIC64 1
#    define ETHR_NATIVE_ATOMIC64_IMPL "ethread"
#    define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#    define ETHR_ATMC_T__ ethr_native_atomic64_t
#    define ETHR_AINT_T__ ethr_sint64_t
#    define ETHR_AINT_SUFFIX__ "q"
#  else
#    error "Unsupported integer size"
#  endif

/* An atomic is an aligned ETHR_AINT_T__ accessed via locked operations.
 */
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
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_mb)(ETHR_ATMC_T__ *var,
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

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_XCHG_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_XCHG_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(xchg_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ val)
{   
    ETHR_AINT_T__ tmp = val;
    __asm__ __volatile__(
	"xchg" ETHR_AINT_SUFFIX__ " %0, %1"
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp)
	: "memory");
    /* now tmp is the atomic's previous value */ 
    return tmp;
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
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_RELB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if defined(_M_IX86)
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	(void) ETHR_NATMC_FUNC__(xchg_mb)(var, i);
    else
#endif /* _M_IX86 */
    {
	ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);
	var->counter = i;
    }
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_MB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    (void) ETHR_NATMC_FUNC__(xchg_mb)(var, i);
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
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_MB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(add_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    __asm__ __volatile__(
       "lock; add" ETHR_AINT_SUFFIX__ " %1, %0"
       : "=m"(var->counter)
       : "ir"(incr), "m"(var->counter)
       : "memory");
}      

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_INC_MB 1
#endif
       
static ETHR_INLINE void
ETHR_NATMC_FUNC__(inc_mb)(ETHR_ATMC_T__ *var)
{
    __asm__ __volatile__(
	"lock; inc" ETHR_AINT_SUFFIX__ " %0"
	: "=m"(var->counter)
	: "m"(var->counter)
	: "memory");
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_DEC_MB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec_mb)(ETHR_ATMC_T__ *var)
{
    __asm__ __volatile__(
	"lock; dec" ETHR_AINT_SUFFIX__ " %0"
	: "=m"(var->counter)
	: "m"(var->counter)
	: "memory");
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    ETHR_AINT_T__ tmp;

    tmp = incr;
    __asm__ __volatile__(
	"lock; xadd" ETHR_AINT_SUFFIX__ " %0, %1" /* xadd didn't exist prior to the 486 */
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp)
	: "memory");
    /* now tmp is the atomic's previous value */
    return tmp + incr;
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_AINT_SUFFIX__

#endif /* ETHR_INCLUDE_ATOMIC_IMPL__ */
