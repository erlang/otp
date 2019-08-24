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
 * Description: Native double word atomics for x86/x86_64
 * Author: Rickard Green
 */

#ifndef ETHR_X86_DW_ATOMIC_H__
#define ETHR_X86_DW_ATOMIC_H__

#ifdef ETHR_GCC_HAVE_DW_CMPXCHG_ASM_SUPPORT

#define ETHR_HAVE_NATIVE_DW_ATOMIC
#define ETHR_NATIVE_DW_ATOMIC_IMPL "ethread"

/*
 * If ETHR_RTCHK_USE_NATIVE_DW_ATOMIC_IMPL__ is defined, it will be used
 * at runtime in order to determine if native or fallback implementation
 * should be used.
 */
#define ETHR_RTCHK_USE_NATIVE_DW_ATOMIC_IMPL__ \
  ETHR_X86_RUNTIME_CONF_HAVE_DW_CMPXCHG__

#if ETHR_SIZEOF_PTR == 4
typedef volatile ethr_sint64_t * ethr_native_dw_ptr_t;
#  define ETHR_DW_NATMC_ALIGN_MASK__ 0x7
#  define ETHR_DW_CMPXCHG_SFX__ "8b"
#  define ETHR_NATIVE_SU_DW_SINT_T ethr_sint64_t
#else
#ifdef ETHR_HAVE_INT128_T
#  define ETHR_NATIVE_SU_DW_SINT_T ethr_sint128_t
typedef volatile ethr_sint128_t * ethr_native_dw_ptr_t;
#else
typedef struct {
    ethr_sint64_t sint64[2];
} ethr_native_sint128_t__;
typedef volatile ethr_native_sint128_t__ * ethr_native_dw_ptr_t;
#endif
#  define ETHR_DW_NATMC_ALIGN_MASK__ 0xf
#  define ETHR_DW_CMPXCHG_SFX__ "16b"
#endif

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
   (&(VAR)->c[(int) ((ethr_uint_t) &(VAR)->c[0]) & ETHR_DW_NATMC_ALIGN_MASK__])
typedef union {
#ifdef ETHR_NATIVE_SU_DW_SINT_T
    volatile ETHR_NATIVE_SU_DW_SINT_T dw_sint;
#endif
    volatile ethr_sint_t sint[3];
    volatile char c[ETHR_SIZEOF_PTR*3];
} ethr_native_dw_atomic_t;


#if (defined(ETHR_TRY_INLINE_FUNCS) \
     || defined(ETHR_ATOMIC_IMPL__) \
     || defined(ETHR_X86_SSE2_ASM_C__)) \
    && ETHR_SIZEOF_PTR == 4 \
    && defined(ETHR_GCC_HAVE_SSE2_ASM_SUPPORT)
ethr_sint64_t
ethr_sse2_native_su_dw_atomic_read(ethr_native_dw_atomic_t *var);
void
ethr_sse2_native_su_dw_atomic_set(ethr_native_dw_atomic_t *var,
				  ethr_sint64_t val);
#endif

#if (defined(ETHR_TRY_INLINE_FUNCS) \
     || defined(ETHR_ATOMIC_IMPL__) \
     || defined(ETHR_X86_SSE2_ASM_C__))
#  ifdef ETHR_DEBUG
#    define ETHR_DW_DBG_ALIGNED__(PTR) \
       ETHR_ASSERT((((ethr_uint_t) (PTR)) & ETHR_DW_NATMC_ALIGN_MASK__) == 0);
#  else
#    define ETHR_DW_DBG_ALIGNED__(PTR)
#  endif
#endif

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_ADDR
static ETHR_INLINE ethr_sint_t *
ethr_native_dw_atomic_addr(ethr_native_dw_atomic_t *var)
{
    return (ethr_sint_t *) ETHR_DW_NATMC_MEM__(var);
}

#if defined(ETHR_CMPXCHG8B_PIC_NO_CLOBBER_EBX) && defined(__PIC__) && __PIC__
#if ETHR_SIZEOF_PTR != 4
#  error unexpected pic issue
#endif
/*
 * When position independent code is used in 32-bit mode, the EBX register
 * is used for storage of global offset table address. When compiling with
 * an old gcc (< vsn 5) we may not use it as input or output in an inline
 * asm. We then need to save and restore the EBX register explicitly (for
 * some reason old gcc compilers didn't provide this service to us).
 * ETHR_CMPXCHG8B_PIC_NO_CLOBBER_EBX will be defined if we need to
 * explicitly manage EBX ourselves.
 *
 */
#  define ETHR_NO_CLOBBER_EBX__ 1
#else
#  define ETHR_NO_CLOBBER_EBX__ 0
#endif

#if ETHR_NO_CLOBBER_EBX__ && !defined(ETHR_CMPXCHG8B_REGISTER_SHORTAGE)
/* When no optimization is on, we'll run into a register shortage */
#  if defined(ETHR_DEBUG) || defined(DEBUG) || defined(VALGRIND) \
      || defined(GCOV) || defined(PURIFY) || defined(PURECOV)
#    define ETHR_CMPXCHG8B_REGISTER_SHORTAGE 1
#  else
#    define ETHR_CMPXCHG8B_REGISTER_SHORTAGE 0
#  endif
#endif


#define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG_MB

static ETHR_INLINE int
ethr_native_dw_atomic_cmpxchg_mb(ethr_native_dw_atomic_t *var,
				 ethr_sint_t *new,
				 ethr_sint_t *xchg)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    char xchgd;

    ETHR_DW_DBG_ALIGNED__(p);

#if ETHR_NO_CLOBBER_EBX__ && ETHR_CMPXCHG8B_REGISTER_SHORTAGE
    /*
     * gcc wont let us use ebx as input and we
     * get a register shortage
     */

    __asm__ __volatile__(
	"pushl %%ebx\n\t"
	"movl (%7), %%ebx\n\t"
	"movl 4(%7), %%ecx\n\t"
	"lock; cmpxchg8b %0\n\t"
	"setz %3\n\t"
	"popl %%ebx\n\t"
	: "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=c"(xchgd)
	: "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "r"(new)
	: "cc", "memory");

#elif ETHR_NO_CLOBBER_EBX__
    /*
     * gcc wont let us use ebx as input
     */

    __asm__ __volatile__(
	"pushl %%ebx\n\t"
	"movl %8, %%ebx\n\t"
	"lock; cmpxchg8b %0\n\t"
	"setz %3\n\t"
	"popl %%ebx\n\t"
	: "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=q"(xchgd)
	: "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "c"(new[1]), "r"(new[0])
	: "cc", "memory");

#else
    /*
     * gcc lets us place values in the registers where
     * we want them
     */

    __asm__ __volatile__(
	"lock; cmpxchg" ETHR_DW_CMPXCHG_SFX__ " %0\n\t"
	"setz %3\n\t"
	: "=m"(*p), "=d"(xchg[1]), "=a"(xchg[0]), "=q"(xchgd)
	: "m"(*p), "1"(xchg[1]), "2"(xchg[0]), "c"(new[1]), "b"(new[0])
	: "cc", "memory");

#endif

    return (int) xchgd;
}

#undef ETHR_NO_CLOBBER_EBX__

#if ETHR_SIZEOF_PTR == 4 && defined(ETHR_GCC_HAVE_SSE2_ASM_SUPPORT)

typedef union {
    ethr_sint64_t sint64;
    ethr_sint_t sint[2];
} ethr_dw_atomic_no_sse2_convert_t;

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_READ

static ETHR_INLINE ethr_sint64_t
ethr_native_su_dw_atomic_read(ethr_native_dw_atomic_t *var)
{
    if (ETHR_X86_RUNTIME_CONF_HAVE_SSE2__)
	return ethr_sse2_native_su_dw_atomic_read(var);
    else {
	ethr_sint_t new[2];
	ethr_dw_atomic_no_sse2_convert_t xchg;
	new[0] = new[1] = xchg.sint[0] = xchg.sint[1] = 0x83838383;
	(void) ethr_native_dw_atomic_cmpxchg_mb(var, new, xchg.sint);
	return xchg.sint64;
    }
}

#define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_SET

static ETHR_INLINE void
ethr_native_su_dw_atomic_set(ethr_native_dw_atomic_t *var,
			     ethr_sint64_t val)
{
    if (ETHR_X86_RUNTIME_CONF_HAVE_SSE2__)
	ethr_sse2_native_su_dw_atomic_set(var, val);
    else {
	ethr_sint_t xchg[2] = {0, 0};
	ethr_dw_atomic_no_sse2_convert_t new;
	new.sint64 = val;
	while (!ethr_native_dw_atomic_cmpxchg_mb(var, new.sint, xchg));
    }
}

#endif /* ETHR_SIZEOF_PTR == 4 */

#endif /* ETHR_TRY_INLINE_FUNCS */

#if defined(ETHR_X86_SSE2_ASM_C__) \
    && ETHR_SIZEOF_PTR == 4 \
    && defined(ETHR_GCC_HAVE_SSE2_ASM_SUPPORT)

/*
 * 8-byte aligned loads and stores of 64-bit values are atomic from
 * pentium and forward. An ordinary volatile load or store in 32-bit
 * mode generates two 32-bit operations (at least with gcc-4.1.2 using
 * -msse2). In order to guarantee one 64-bit load/store operation
 * from/to memory we load/store via an xmm register using movq.
 *
 * Load/store can be achieved using cmpxchg8b, however, using movq is
 * much faster. Unfortunately we cannot do the same thing in 64-bit
 * mode; instead, we have to do loads and stores via cmpxchg16b.
 *
 * We do not inline these, but instead compile these into a separate
 * object file using -msse2. This since we don't want to use -msse2 for
 * the whole system. If we detect sse2 support (pentium4 and forward)
 * at runtime, we use them; otherwise, we fall back to using cmpxchg8b
 * for loads and stores. This way the binary can be moved between
 * processors with and without sse2 support.
 */

ethr_sint64_t
ethr_sse2_native_su_dw_atomic_read(ethr_native_dw_atomic_t *var)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ethr_sint64_t val;
    ETHR_DW_DBG_ALIGNED__(p);
    __asm__ __volatile__("movq %1, %0\n\t" : "=x"(val) : "m"(*p) : "memory");
    return val;
}

void
ethr_sse2_native_su_dw_atomic_set(ethr_native_dw_atomic_t *var,
				  ethr_sint64_t val)
{
    ethr_native_dw_ptr_t p = (ethr_native_dw_ptr_t) ETHR_DW_NATMC_MEM__(var);
    ETHR_DW_DBG_ALIGNED__(p);
    __asm__ __volatile__("movq %1, %0\n\t" : "=m"(*p) : "x"(val) : "memory");
}

#endif /* ETHR_X86_SSE2_ASM_C__ */

#endif /* ETHR_GCC_HAVE_DW_CMPXCHG_ASM_SUPPORT */

#endif /* ETHR_X86_DW_ATOMIC_H__ */

