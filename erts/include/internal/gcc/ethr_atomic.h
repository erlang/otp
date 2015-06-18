/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2015. All Rights Reserved.
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
 * Description: Native atomics ethread support using gcc's __atomic
 *              and __sync builtins
 * Author: Rickard Green
 *
 * Note: The C11 memory model implemented by gcc's __atomic
 *       builtins does not match the ethread API very well.
 *
 *       Due to this we cannot use the __ATOMIC_SEQ_CST
 *       memory model. For more information see the comment
 *       in the begining of ethr_membar.h in this directory.
 */

#undef ETHR_INCLUDE_ATOMIC_IMPL__
#if !defined(ETHR_GCC_ATOMIC_ATOMIC32_H__)		\
    && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)		\
    && ((ETHR_HAVE___sync_val_compare_and_swap & 4)	\
	|| (ETHR_HAVE___atomic_compare_exchange_n & 4))

#define ETHR_GCC_ATOMIC_ATOMIC32_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#undef ETHR_ATOMIC_WANT_32BIT_IMPL__

#elif !defined(ETHR_GCC_ATOMIC64_H__)			\
    && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)		\
    && ((ETHR_HAVE___sync_val_compare_and_swap & 8)	\
	|| (ETHR_HAVE___atomic_compare_exchange_n & 8))

#define ETHR_GCC_ATOMIC64_H__
#define ETHR_INCLUDE_ATOMIC_IMPL__ 8
#undef ETHR_ATOMIC_WANT_64BIT_IMPL__

#endif

#ifdef ETHR_INCLUDE_ATOMIC_IMPL__

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#if ((ETHR_HAVE___sync_val_compare_and_swap & 4) \
     && (ETHR_HAVE___atomic_compare_exchange_n & 4))
#  define ETHR_NATIVE_ATOMIC32_IMPL "gcc_atomic_and_sync_builtins"
#elif (ETHR_HAVE___atomic_compare_exchange_n & 4)
#  define ETHR_NATIVE_ATOMIC32_IMPL "gcc_atomic_builtins"
#elif (ETHR_HAVE___sync_val_compare_and_swap & 4)
#  define ETHR_NATIVE_ATOMIC32_IMPL "gcc_sync_builtins"
#else
#  error "!?"
#endif
#elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t
#if ((ETHR_HAVE___sync_val_compare_and_swap & 8) \
     && (ETHR_HAVE___atomic_compare_exchange_n & 8))
#  define ETHR_NATIVE_ATOMIC64_IMPL "gcc_atomic_and_sync_builtins"
#elif (ETHR_HAVE___atomic_compare_exchange_n & 8)
#  define ETHR_NATIVE_ATOMIC64_IMPL "gcc_atomic_builtins"
#elif (ETHR_HAVE___sync_val_compare_and_swap & 8)
#  define ETHR_NATIVE_ATOMIC64_IMPL "gcc_sync_builtins"
#else
#  error "!?"
#endif
#else
#error "Unsupported integer size"
#endif

#undef ETHR_NATIVE_ATOMIC_IMPL__

typedef struct {
    volatile ETHR_AINT_T__ value;
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
    return (ETHR_AINT_T__ *) &var->value;
}

/*
 * set()
 */
#if (ETHR_HAVE___atomic_store_n & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    __atomic_store_n(&var->value, value, __ATOMIC_RELAXED);
}

#endif /* ETHR_GCC_RELAXED_VERSIONS__ */

#if (ETHR_GCC_RELB_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_RELB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    __atomic_store_n(&var->value, value, __ATOMIC_RELEASE);
}

#endif /* ETHR_GCC_RELB_VERSIONS__ */

#elif (ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    var->value = value;
}

#endif /* ETHR_GCC_RELAXED_VERSIONS__ */

#if (ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELB_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_RELB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    var->value = value;
}

#endif /* ETHR_GCC_RELB_VERSIONS__ */

#endif /* ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__ */

#endif /* ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE__ */

/*
 * read()
 */

#if (ETHR_HAVE___atomic_load_n & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
    return __atomic_load_n(&var->value, __ATOMIC_RELAXED);
}

#endif /* ETHR_GCC_RELAXED_VERSIONS__ */

#if ((ETHR_GCC_ACQB_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__) \
     & ~ETHR___atomic_load_ACQUIRE_barrier_bug)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_acqb)(ETHR_ATMC_T__ *var)
{
    return __atomic_load_n(&var->value, __ATOMIC_ACQUIRE);
}

#endif /* ETHR_GCC_ACQB_VERSIONS__ */

#elif (ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
    return var->value;
}

#endif /* ETHR_GCC_RELAXED_VERSIONS__ */

#if (ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_ACQB_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_acqb)(ETHR_ATMC_T__ *var)
{
    return var->value;
}

#endif /* ETHR_GCC_ACQB_VERSIONS__ */

#endif /* ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__ */

#endif /* ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__ */

/*
 * add_return()
 */
#if (ETHR_HAVE___atomic_add_fetch & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return __atomic_add_fetch(&var->value, incr, __ATOMIC_RELAXED);
}

#endif /* ETHR_GCC_RELAXED_MOD_VERSIONS__ */

#if (ETHR_GCC_ACQB_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_acqb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return __atomic_add_fetch(&var->value, incr, __ATOMIC_ACQUIRE);
}

#endif /* ETHR_GCC_ACQB_MOD_VERSIONS__ */

#if (ETHR_GCC_RELB_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return __atomic_add_fetch(&var->value, incr, __ATOMIC_RELEASE);
}

#endif /* ETHR_GCC_RELB_MOD_VERSIONS__ */

#endif /* ETHR_HAVE___atomic_add_fetch */

#if ((ETHR_HAVE___sync_add_and_fetch & ETHR_INCLUDE_ATOMIC_IMPL__) \
     & ETHR_GCC_MB_MOD_VERSIONS__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return __sync_add_and_fetch(&var->value, incr);
}

#endif /* ETHR_HAVE___sync_add_and_fetch */

/*
 * and_retold()
 */
#if (ETHR_HAVE___atomic_fetch_and & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_AND_RETOLD 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __atomic_fetch_and(&var->value, mask, __ATOMIC_RELAXED);
}

#endif /* ETHR_GCC_RELAXED_MOD_VERSIONS__ */

#if (ETHR_GCC_ACQB_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_AND_RETOLD_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold_acqb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __atomic_fetch_and(&var->value, mask, __ATOMIC_ACQUIRE);
}

#endif /* ETHR_GCC_ACQB_MOD_VERSIONS__ */

#if (ETHR_GCC_RELB_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_AND_RETOLD_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __atomic_fetch_and(&var->value, mask, __ATOMIC_RELEASE);
}

#endif /* ETHR_GCC_RELB_MOD_VERSIONS__ */

#endif /* ETHR_HAVE___atomic_fetch_and */

#if ((ETHR_HAVE___sync_fetch_and_and & ETHR_INCLUDE_ATOMIC_IMPL__) \
     & ETHR_GCC_MB_MOD_VERSIONS__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_AND_RETOLD_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __sync_fetch_and_and(&var->value, mask);
}

#endif /* ETHR_HAVE___sync_fetch_and_and */

/*
 * or_retold()
 */
#if (ETHR_HAVE___atomic_fetch_or & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_OR_RETOLD 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __atomic_fetch_or(&var->value, mask, __ATOMIC_RELAXED);
}

#endif /* ETHR_GCC_RELAXED_MOD_VERSIONS__ */

#if (ETHR_GCC_ACQB_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_OR_RETOLD_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold_acqb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __atomic_fetch_or(&var->value, mask, __ATOMIC_ACQUIRE);
}

#endif /* ETHR_GCC_ACQB_MOD_VERSIONS__ */

#if (ETHR_GCC_RELB_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_OR_RETOLD_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return __atomic_fetch_or(&var->value, mask, __ATOMIC_RELEASE);
}

#endif /* ETHR_GCC_RELB_MOD_VERSIONS__ */

#endif /* ETHR_HAVE___atomic_fetch_or */

#if ((ETHR_HAVE___sync_fetch_and_or & ETHR_INCLUDE_ATOMIC_IMPL__) \
     & ETHR_GCC_MB_MOD_VERSIONS__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_OR_RETOLD_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return (ETHR_AINT_T__) __sync_fetch_and_or(&var->value, mask);
}

#endif /* ETHR_HAVE___sync_fetch_and_or */

/*
 * cmpxchg()
 */
#if (ETHR_HAVE___atomic_compare_exchange_n & ETHR_INCLUDE_ATOMIC_IMPL__)

#if (ETHR_GCC_RELAXED_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var,
			   ETHR_AINT_T__ new,
			   ETHR_AINT_T__ exp)
{
    ETHR_AINT_T__ xchg = exp;
    if (__atomic_compare_exchange_n(&var->value,
				    &xchg,
				    new,
				    0, /* No spurious failures, please */
				    __ATOMIC_RELAXED,
				    __ATOMIC_RELAXED))
	return exp;
    return xchg;
}

#endif /* ETHR_GCC_RELAXED_MOD_VERSIONS__ */

#if (ETHR_GCC_ACQB_MOD_VERSIONS__ & ETHR_INCLUDE_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ exp)
{
    ETHR_AINT_T__ xchg = exp;
    if (__atomic_compare_exchange_n(&var->value,
				    &xchg,
				    new,
				    0, /* No spurious failures, please */
				    __ATOMIC_ACQUIRE,
				    __ATOMIC_ACQUIRE))
	return exp;
    return xchg;
}

#endif /* ETHR_GCC_ACQB_MOD_VERSIONS__ */

#endif /* ETHR_HAVE___atomic_compare_exchange_n */

#if ((ETHR_HAVE___sync_val_compare_and_swap & ETHR_INCLUDE_ATOMIC_IMPL__) \
     & ETHR_GCC_MB_MOD_VERSIONS__)

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
    return __sync_val_compare_and_swap(&var->value, old, new);
}

#endif /* ETHR_HAVE___sync_val_compare_and_swap */

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_AINT_SUFFIX__

#endif
