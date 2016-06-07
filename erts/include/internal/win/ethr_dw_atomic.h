/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2016. All Rights Reserved.
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
 * Description: Native double word atomics for windows
 * Author: Rickard Green
 */

#undef ETHR_INCLUDE_DW_ATOMIC_IMPL__
#ifndef ETHR_X86_DW_ATOMIC_H__
#  define ETHR_X86_DW_ATOMIC_H__
#  if ((ETHR_SIZEOF_PTR == 4 \
        && defined(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64)) \
       || (ETHR_SIZEOF_PTR == 8 \
           && defined(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE128)))
#    define ETHR_INCLUDE_DW_ATOMIC_IMPL__
#  endif
#endif

#ifdef ETHR_INCLUDE_DW_ATOMIC_IMPL__

#  if ETHR_SIZEOF_PTR == 4
#    define ETHR_HAVE_NATIVE_SU_DW_ATOMIC
#  else
#    define ETHR_HAVE_NATIVE_DW_ATOMIC
#  endif
#  define ETHR_NATIVE_DW_ATOMIC_IMPL "windows-interlocked"

#  if defined(_M_IX86) || defined(_M_AMD64)
/*
 * If ETHR_RTCHK_USE_NATIVE_DW_ATOMIC_IMPL__ is defined, it will be used
 * at runtime in order to determine if native or fallback implementation
 * should be used.
 */
#    define ETHR_RTCHK_USE_NATIVE_DW_ATOMIC_IMPL__ \
       ETHR_X86_RUNTIME_CONF_HAVE_DW_CMPXCHG__
#  endif

#  include <intrin.h>
#  if ETHR_SIZEOF_PTR == 4
#    pragma intrinsic(_InterlockedCompareExchange64)
#    define ETHR_DW_NATMC_ALIGN_MASK__ 0x7
#    define ETHR_NATIVE_SU_DW_SINT_T ethr_sint64_t
#  else
#    pragma intrinsic(_InterlockedCompareExchange128)
#    define ETHR_DW_NATMC_ALIGN_MASK__ 0xf
#  endif

typedef volatile __int64 * ethr_native_dw_ptr_t;

/*
 * We need 16 byte aligned memory in 64-bit mode, and 8 byte aligned
 * memory in 32-bit mode. 16 byte aligned malloc in 64-bit mode is
 * not common, and at least some glibc malloc implementations
 * only 4 byte align in 32-bit mode.
 *
 * This code assumes 8 byte aligned memory in 64-bit mode, and 4 byte
 * aligned memory in 32-bit mode. A malloc implementation that does
 * not adhere to these alignment requirements is seriously broken,
 * and we wont bother trying to work around it.
 *
 * Since memory alignment may be off by one word we need to align at
 * runtime. We, therefore, need an extra word allocated.
 */
#define ETHR_DW_NATMC_MEM__(VAR) \
   (&var->c[(int) ((ethr_uint_t) &(VAR)->c[0]) & ETHR_DW_NATMC_ALIGN_MASK__])
typedef union {
#ifdef ETHR_NATIVE_SU_DW_SINT_T
    volatile ETHR_NATIVE_SU_DW_SINT_T dw_sint;
#endif
    volatile ethr_sint_t sint[3];
    volatile char c[ETHR_SIZEOF_PTR*3];
} ethr_native_dw_atomic_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#ifdef ETHR_DEBUG
#  define ETHR_DW_DBG_ALIGNED__(PTR) \
     ETHR_ASSERT((((ethr_uint_t) (PTR)) & ETHR_DW_NATMC_ALIGN_MASK__) == 0);
#else
#  define ETHR_DW_DBG_ALIGNED__(PTR)
#endif

#define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_ADDR

static ETHR_INLINE ethr_sint_t *
ethr_native_dw_atomic_addr(ethr_native_dw_atomic_t *var)
{
    ethr_sint_t *p = (ethr_sint_t *) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    return p;
}


#if ETHR_SIZEOF_PTR == 4

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_MB

static ETHR_INLINE ethr_sint64_t
ethr_native_su_dw_atomic_cmpxchg_mb(ethr_native_dw_atomic_t *var,
				    ethr_sint64_t new,
				    ethr_sint64_t exp)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    return (ethr_sint64_t) _InterlockedCompareExchange64(p, new, exp);
}

#elif ETHR_SIZEOF_PTR == 8

#define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG_MB

#ifdef ETHR_BIGENDIAN
#  define ETHR_WIN_LOW_WORD__ 1
#  define ETHR_WIN_HIGH_WORD__ 0
#else
#  define ETHR_WIN_LOW_WORD__ 0
#  define ETHR_WIN_HIGH_WORD__ 1
#endif

static ETHR_INLINE int
ethr_native_dw_atomic_cmpxchg_mb(ethr_native_dw_atomic_t *var,
				 ethr_sint_t *new,
				 ethr_sint_t *xchg)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    return (int) _InterlockedCompareExchange128(p,
						new[ETHR_WIN_HIGH_WORD__],
						new[ETHR_WIN_LOW_WORD__],
						xchg);
}

#endif

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHR_INCLUDE_DW_ATOMIC_IMPL__ */
