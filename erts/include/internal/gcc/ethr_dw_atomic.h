/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2017. All Rights Reserved.
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
 * Description: Native double word atomics using gcc's __atomic
 *              and __sync builtins
 * Author: Rickard Green
 *
 * Note: The C11 memory model implemented by gcc's __atomic
 *       builtins does not match the ethread API very well.
 *
 *       Due to this we cannot use the __ATOMIC_SEQ_CST
 *       memory model. For more information see the comment
 *       in the beginning of ethr_membar.h in this directory.
 */

#undef ETHR_INCLUDE_DW_ATOMIC_IMPL__
#if !defined(ETHR_GCC_ATOMIC_DW_ATOMIC_H__)				\
    && ((ETHR_HAVE___sync_val_compare_and_swap & (2*ETHR_SIZEOF_PTR))	\
	|| (ETHR_HAVE___atomic_compare_exchange_n & (2*ETHR_SIZEOF_PTR)))
#  define ETHR_GCC_ATOMIC_DW_ATOMIC_H__
#  define ETHR_INCLUDE_DW_ATOMIC_IMPL__
#endif

#ifdef ETHR_INCLUDE_DW_ATOMIC_IMPL__
#  define ETHR_HAVE_NATIVE_SU_DW_ATOMIC

#if ((ETHR_HAVE___sync_val_compare_and_swap & (2*ETHR_SIZEOF_PTR))	\
     && (ETHR_HAVE___atomic_compare_exchange_n & (2*ETHR_SIZEOF_PTR)))
#  define ETHR_NATIVE_DW_ATOMIC_IMPL "gcc_atomic_and_sync_builtins"
#elif (ETHR_HAVE___atomic_compare_exchange_n & (2*ETHR_SIZEOF_PTR))
#  define ETHR_NATIVE_DW_ATOMIC_IMPL "gcc_atomic_builtins"
#elif (ETHR_HAVE___sync_val_compare_and_swap & (2*ETHR_SIZEOF_PTR))
#  define ETHR_NATIVE_DW_ATOMIC_IMPL "gcc_sync_builtins"
#else
#  error "!?"
#endif

#  if ETHR_SIZEOF_PTR == 4
#    define ETHR_DW_NATMC_ALIGN_MASK__ 0x7
#    define ETHR_NATIVE_SU_DW_SINT_T ethr_sint64_t
#  elif ETHR_SIZEOF_PTR == 8
#    define ETHR_DW_NATMC_ALIGN_MASK__ 0xf
#    define ETHR_NATIVE_SU_DW_SINT_T ethr_sint128_t
#  endif

typedef volatile ETHR_NATIVE_SU_DW_SINT_T * ethr_native_dw_ptr_t;

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
    volatile ETHR_NATIVE_SU_DW_SINT_T dw_sint;
    volatile ethr_sint_t sint[3];
    volatile char c[ETHR_SIZEOF_PTR*3];
} ethr_native_dw_atomic_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#  ifdef ETHR_DEBUG
#    define ETHR_DW_DBG_ALIGNED__(PTR) \
       ETHR_ASSERT((((ethr_uint_t) (PTR)) & ETHR_DW_NATMC_ALIGN_MASK__) == 0);
#  else
#    define ETHR_DW_DBG_ALIGNED__(PTR)
#  endif


#define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_ADDR 1

static ETHR_INLINE ethr_sint_t *
ethr_native_dw_atomic_addr(ethr_native_dw_atomic_t *var)
{
    return (ethr_sint_t *) ETHR_DW_NATMC_MEM__(var);
}

#if (ETHR_HAVE___atomic_store_n & (2*ETHR_SIZEOF_PTR))

#if (ETHR_GCC_RELAXED_VERSIONS__ & (2*ETHR_SIZEOF_PTR))

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_SET 1

static ETHR_INLINE void
ethr_native_su_dw_atomic_set(ethr_native_dw_atomic_t *var,
			     ETHR_NATIVE_SU_DW_SINT_T value)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    __atomic_store_n(p, value, __ATOMIC_RELAXED);
}

#endif /* ETHR_GCC_RELAXED_VERSIONS__ */

#if (ETHR_GCC_RELB_VERSIONS__ & (2*ETHR_SIZEOF_PTR))

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_SET_RELB 1

static ETHR_INLINE void
ethr_native_su_dw_atomic_set_relb(ethr_native_dw_atomic_t *var,
				  ETHR_NATIVE_SU_DW_SINT_T value)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    __atomic_store_n(p, value, __ATOMIC_RELEASE);
}

#endif /* ETHR_GCC_RELB_VERSIONS__ */

#endif /* ETHR_HAVE___atomic_store_n */

#if (ETHR_HAVE___atomic_load_n & (2*ETHR_SIZEOF_PTR))

#if (ETHR_GCC_RELAXED_VERSIONS__ & (2*ETHR_SIZEOF_PTR))

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_READ 1

static ETHR_INLINE ETHR_NATIVE_SU_DW_SINT_T
ethr_native_su_dw_atomic_read(ethr_native_dw_atomic_t *var)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    return __atomic_load_n(p, __ATOMIC_RELAXED);
}

#endif /* ETHR_GCC_RELAXED_VERSIONS__ */

#if ((ETHR_GCC_ACQB_VERSIONS__ & (2*ETHR_SIZEOF_PTR))	\
     & ~ETHR___atomic_load_ACQUIRE_barrier_bug)

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_READ_ACQB 1

static ETHR_INLINE ETHR_NATIVE_SU_DW_SINT_T
ethr_native_su_dw_atomic_read_acqb(ethr_native_dw_atomic_t *var)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    return __atomic_load_n(p, __ATOMIC_ACQUIRE);
}

#endif /* ETHR_GCC_ACQB_VERSIONS__ */

#endif /* ETHR_HAVE___atomic_load_n */

#if (ETHR_HAVE___atomic_compare_exchange_n & (2*ETHR_SIZEOF_PTR))

#if (ETHR_GCC_RELAXED_MOD_VERSIONS__ & (2*ETHR_SIZEOF_PTR))

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG 1

static ETHR_INLINE ETHR_NATIVE_SU_DW_SINT_T
ethr_native_su_dw_atomic_cmpxchg(ethr_native_dw_atomic_t *var,
				 ETHR_NATIVE_SU_DW_SINT_T new,
				 ETHR_NATIVE_SU_DW_SINT_T exp)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_NATIVE_SU_DW_SINT_T xchg = exp;
    ETHR_DW_DBG_ALIGNED__(p);
    if (__atomic_compare_exchange_n(p,
				    &xchg,
				    new,
				    0,
				    __ATOMIC_RELAXED,
				    __ATOMIC_RELAXED))
	return exp;
    return xchg;
}

#endif /* ETHR_GCC_RELAXED_MOD_VERSIONS__ */

#if (ETHR_GCC_ACQB_MOD_VERSIONS__ & (2*ETHR_SIZEOF_PTR))

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_ACQB 1

static ETHR_INLINE ETHR_NATIVE_SU_DW_SINT_T
ethr_native_su_dw_atomic_cmpxchg_acqb(ethr_native_dw_atomic_t *var,
				      ETHR_NATIVE_SU_DW_SINT_T new,
				      ETHR_NATIVE_SU_DW_SINT_T exp)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_NATIVE_SU_DW_SINT_T xchg = exp;
    ETHR_DW_DBG_ALIGNED__(p);
    if (__atomic_compare_exchange_n(p,
				    &xchg,
				    new,
				    0,
				    __ATOMIC_ACQUIRE,
				    __ATOMIC_ACQUIRE))
	return exp;
    return xchg;
}

#endif /* ETHR_GCC_ACQB_MOD_VERSIONS__ */

#endif /* ETHR_HAVE___atomic_compare_exchange_n */

#if ((ETHR_HAVE___sync_val_compare_and_swap & (2*ETHR_SIZEOF_PTR)) \
     & ETHR_GCC_MB_MOD_VERSIONS__)

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_MB 1

static ETHR_INLINE ETHR_NATIVE_SU_DW_SINT_T
ethr_native_su_dw_atomic_cmpxchg_mb(ethr_native_dw_atomic_t *var,
				    ETHR_NATIVE_SU_DW_SINT_T new,
				    ETHR_NATIVE_SU_DW_SINT_T old)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    return __sync_val_compare_and_swap(p, old, new);
}

#endif /* ETHR_HAVE___sync_val_compare_and_swap */

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHR_INCLUDE_DW_ATOMIC_IMPL__ */
