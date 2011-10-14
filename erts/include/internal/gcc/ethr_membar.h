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
 * Description: Memory barriers when using gcc's builtins
 * Author: Rickard Green
 */

#ifndef ETHR_GCC_MEMBAR_H__
#define ETHR_GCC_MEMBAR_H__

#define ETHR_LoadLoad	(1 << 0)
#define ETHR_LoadStore	(1 << 1)
#define ETHR_StoreLoad	(1 << 2)
#define ETHR_StoreStore	(1 << 3)

/*
 * According to the documentation __sync_synchronize() will
 * issue a full memory barrier. However, __sync_synchronize()
 * is known to erroneously be a noop on at least some
 * platforms with some gcc versions. This has suposedly been
 * fixed in some gcc version, but we don't know from which
 * version. Therefore, we only use it when it has been
 * verified to work. Otherwise we use the workaround
 * below.
 */

#if defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP32)
#  define ETHR_MB_T__ ethr_sint32_t
#elif defined(ETHR_HAVE___SYNC_VAL_COMPARE_AND_SWAP64)
#  define ETHR_MB_T__ ethr_sint64_t
#else
#  error "No __sync_val_compare_and_swap"
#endif
#define ETHR_SYNC_SYNCHRONIZE_WORKAROUND__ \
do { \
    volatile ETHR_MB_T__ x___ = 0; \
    (void) __sync_val_compare_and_swap(&x___, (ETHR_MB_T__) 0, (ETHR_MB_T__) 1); \
} while (0)

#define ETHR_COMPILER_BARRIER __asm__ __volatile__("" : : : "memory")

#if defined(__mips__) && ETHR_AT_LEAST_GCC_VSN__(4, 2, 0)
#  define ETHR_MEMBAR(B) __sync_synchronize()
#  define ETHR_READ_DEPEND_MEMORY_BARRIER __sync_synchronize()
#elif ((defined(__powerpc__) || defined(__ppc__)) \
       && ETHR_AT_LEAST_GCC_VSN__(4, 1, 2))
#  define ETHR_MEMBAR(B) __sync_synchronize()
#else /* Use workaround */
#  define ETHR_MEMBAR(B) \
     ETHR_SYNC_SYNCHRONIZE_WORKAROUND__
#  define ETHR_READ_DEPEND_MEMORY_BARRIER \
     ETHR_SYNC_SYNCHRONIZE_WORKAROUND__
#endif


#endif /* ETHR_GCC_MEMBAR_H__ */
