/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011. All Rights Reserved.
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
 * Description: Native double word atomics using gcc's builtins
 * Author: Rickard Green
 */

#undef ETHR_INCLUDE_DW_ATOMIC_IMPL__
#ifndef ETHR_GCC_DW_ATOMIC_H__
#  define ETHR_GCC_DW_ATOMIC_H__
#  if ((ETHR_SIZEOF_PTR == 4 \
        && defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP64)) \
       || (ETHR_SIZEOF_PTR == 8 \
           && defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP128) \
	   && defined(ETHR_HAVE_INT128_T)))
#    define ETHR_INCLUDE_DW_ATOMIC_IMPL__
#  endif
#endif

#ifdef ETHR_INCLUDE_DW_ATOMIC_IMPL__
#  define ETHR_HAVE_NATIVE_SU_DW_ATOMIC
#  define ETHR_NATIVE_DW_ATOMIC_IMPL "gcc"

#  if defined(__i386__) || defined(__x86_64__)
/*
 * If ETHR_RTCHK_USE_NATIVE_DW_ATOMIC_IMPL__ is defined, it will be used
 * at runtime in order to determine if native or fallback implementation
 * should be used.
 */
#    define ETHR_RTCHK_USE_NATIVE_DW_ATOMIC_IMPL__ \
       ETHR_X86_RUNTIME_CONF_HAVE_DW_CMPXCHG__
#  endif

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

#define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_ADDR
static ETHR_INLINE ethr_sint_t *
ethr_native_dw_atomic_addr(ethr_native_dw_atomic_t *var)
{
    return (ethr_sint_t *) ETHR_DW_NATMC_MEM__(var);
}


#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_MB

static ETHR_INLINE ETHR_NATIVE_SU_DW_SINT_T
ethr_native_su_dw_atomic_cmpxchg_mb(ethr_native_dw_atomic_t *var,
				    ETHR_NATIVE_SU_DW_SINT_T new,
				    ETHR_NATIVE_SU_DW_SINT_T old)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    return __sync_val_compare_and_swap(p, old, new);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHR_GCC_DW_ATOMIC_H__ */

