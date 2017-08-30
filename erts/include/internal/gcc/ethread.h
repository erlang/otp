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
 * Description: Native atomic ethread support when using gcc's __atomic
 *              and __sync builtins
 * Author: Rickard Green
 */

#if !defined(ETHREAD_GCC_NATIVE_H__) && ETHR_GCC_COMPILER
#define ETHREAD_GCC_NATIVE_H__

#ifndef ETHR_MEMBAR
#  include "ethr_membar.h"
#endif

#define ETHR_GCC_VERSIONS_MASK__ 28

#undef ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE__
#undef ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__
#undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__
#undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__
#undef ETHR_GCC_RELAXED_VERSIONS__
#undef ETHR_GCC_RELAXED_MOD_VERSIONS__
#undef ETHR_GCC_ACQB_VERSIONS__
#undef ETHR_GCC_ACQB_MOD_VERSIONS__
#undef ETHR_GCC_RELB_VERSIONS__
#undef ETHR_GCC_RELB_MOD_VERSIONS__
#undef ETHR_GCC_MB_MOD_VERSIONS__

/*
 * True GNU GCCs before version 4.8 do not emit a memory barrier
 * after the load in the __atomic_load_n(_, __ATOMIC_ACQUIRE)
 * case (which is needed on most architectures).
 */
#undef ETHR___atomic_load_ACQUIRE_barrier_bug
#if ETHR_GCC_COMPILER != ETHR_GCC_COMPILER_TRUE
/*
 * A gcc compatible compiler. We have no information
 * about the existence of this bug, but we assume
 * that it is not impossible that it could have
 * been "inherited". Therefore, until we are certain
 * that the bug does not exist, we assume that it
 * does.
 */
#  define ETHR___atomic_load_ACQUIRE_barrier_bug ETHR_GCC_VERSIONS_MASK__
#elif !ETHR_AT_LEAST_GCC_VSN__(4, 8, 0)
/* True gcc of version < 4.8, i.e., bug exist... */
#  define ETHR___atomic_load_ACQUIRE_barrier_bug ETHR_GCC_VERSIONS_MASK__
#else /* True gcc of version >= 4.8 */
/*
 * Sizes less than or equal to word size have been fixed,
 * but double word size has not been fixed.
 */
#  if ETHR_SIZEOF_PTR == 8
#    define ETHR___atomic_load_ACQUIRE_barrier_bug \
    (~(8|4) & ETHR_GCC_VERSIONS_MASK__)
#  elif ETHR_SIZEOF_PTR == 4
#    define ETHR___atomic_load_ACQUIRE_barrier_bug \
    (~4 & ETHR_GCC_VERSIONS_MASK__)
#  else
#    error word size not supported
#  endif
#endif

#define ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE__ 0
#define ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__ 0
#define ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__ 0
#define ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__ 0
#define ETHR_GCC_RELAXED_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#define ETHR_GCC_RELAXED_MOD_VERSIONS__ ETHR_GCC_VERSIONS_MASK__

#if ETHR_TRUST_GCC_ATOMIC_BUILTINS_MEMORY_BARRIERS
#  define ETHR_GCC_ACQB_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#  define ETHR_GCC_ACQB_MOD_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#  define ETHR_GCC_RELB_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#  define ETHR_GCC_RELB_MOD_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#else
/*
 * This is currently the default (on most platforms) since
 * we've seen too many memory barrier bugs produced by gcc...
 */
#  define ETHR_GCC_ACQB_VERSIONS__ 0
#  define ETHR_GCC_ACQB_MOD_VERSIONS__ 0
#  define ETHR_GCC_RELB_VERSIONS__ 0
#  define ETHR_GCC_RELB_MOD_VERSIONS__ 0
#endif
/*
 * In the general case we do not want any full barrier versions
 * if we can implement more relaxed ones (using __atomic_* builtins).
 * This since the implementations normally need extra memory barrier
 * instructions to implement these. The x86/x86_64 implementations
 * are an exception see below.
 */
#define ETHR_GCC_MB_MOD_VERSIONS__ \
    (ETHR_GCC_VERSIONS_MASK__ & ~ETHR_HAVE___atomic_compare_exchange_n)

#if ETHR_SIZEOF_PTR == 8
#  define ETHR_GCC_VOLATILE_BIT_MASK__ 12
#elif ETHR_SIZEOF_PTR == 4
#  define ETHR_GCC_VOLATILE_BIT_MASK__ 4
#endif

#if defined(__i386__) || defined(__x86_64__) || defined(__sparc__)	\
    || defined(__powerpc__) || defined(__ppc__) || defined(__mips__)	\
    || defined(__alpha__) || defined(__ia64__)

/*
 * Aligned volatile stores and loads of data smaller
 * than or equal to word size are atomic...
 */
#  undef ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE__
#  define ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE__ ETHR_GCC_VOLATILE_BIT_MASK__
#  undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__
#  define ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__ ETHR_GCC_VOLATILE_BIT_MASK__

#elif defined(__arm__)

/* volatile stores are problematic on some machines... */
#  undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__
#  define ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__ ETHR_GCC_VOLATILE_BIT_MASK__

#endif

#if defined(__ia64__)

/* Volatile stores produce stores with release barriers. */
#  undef ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__
#  define ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__ ETHR_GCC_VOLATILE_BIT_MASK__

/* Volatile loads produce loads with acquire barrier. */
#  undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__
#  define ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__ ETHR_GCC_VOLATILE_BIT_MASK__

/*
 * We trust gcc to produce acquire/release barriers on itanium.
 * Since all atomic ops also have at least acquire or release
 * barriers (also when passed the relaxed memory model) it
 * would be very inefficient not to use these as native
 * barriers on Itanium.
 */
#  undef ETHR_GCC_ACQB_VERSIONS__
#  define ETHR_GCC_ACQB_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#  undef ETHR_GCC_ACQB_MOD_VERSIONS__
#  define ETHR_GCC_ACQB_MOD_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#  undef ETHR_GCC_RELB_VERSIONS__
#  define ETHR_GCC_RELB_VERSIONS__ ETHR_GCC_VERSIONS_MASK__
#  undef ETHR_GCC_RELB_MOD_VERSIONS__
#  define ETHR_GCC_RELB_MOD_VERSIONS__ ETHR_GCC_VERSIONS_MASK__

/*
 * Itanium is not effected by the load acquire
 * bug since the barrier is part of the instruction
 * on Itanium (ld.acq), and not a separate instruction
 * as on most platforms.
 */
#  undef ETHR___atomic_load_ACQUIRE_barrier_bug
#  define ETHR___atomic_load_ACQUIRE_barrier_bug 0

/*
 * No point exposing relaxed versions since they are
 * implemended using either acquire or release
 * barriers.
 */
#  undef ETHR_GCC_RELAXED_VERSIONS__
#  define ETHR_GCC_RELAXED_VERSIONS__ 0

/* #endif defined(__ia64__) */
#elif defined(__i386__) || defined(__x86_64__)

/*
 * Want full barrier versions of all modification
 * operations since all of these are implemented
 * using locked instructions implying full memory
 * barriers.
 */
#  undef ETHR_GCC_MB_MOD_VERSIONS__
#  define ETHR_GCC_MB_MOD_VERSIONS__ ETHR_HAVE___sync_val_compare_and_swap

/*
 * No point exposing acquire/release versions
 * when we got full memory barrier versions
 * of modification operations since all of these
 * are implemented using locked instructions
 * implying full memory barriers.
 */
#  if ETHR_GCC_ACQB_MOD_VERSIONS__
#    undef ETHR_GCC_ACQB_MOD_VERSIONS__
#    define ETHR_GCC_ACQB_MOD_VERSIONS__ \
    (ETHR_GCC_VERSIONS_MASK__ & ~ETHR_HAVE___sync_val_compare_and_swap)
#  endif
#  if ETHR_GCC_RELB_MOD_VERSIONS__
#    undef ETHR_GCC_RELB_MOD_VERSIONS__
#    define ETHR_GCC_RELB_MOD_VERSIONS__ \
    (ETHR_GCC_VERSIONS_MASK__ & ~ETHR_HAVE___sync_val_compare_and_swap)
#  endif

#  ifdef ETHR_X86_OUT_OF_ORDER

/* See above... */
#    undef ETHR_GCC_RELAXED_MOD_VERSIONS__
#    define ETHR_GCC_RELAXED_MOD_VERSIONS__ 0

#  else /* !ETHR_X86_OUT_OF_ORDER, i.e., we don't use any x86-OOO instructions... */

/*
 * Not effected by the load acquire barrier bug,
 * since no barrier at all is needed for a load
 * acquire...
 */
#    undef ETHR___atomic_load_ACQUIRE_barrier_bug
#    define ETHR___atomic_load_ACQUIRE_barrier_bug 0

/* Stores imply release barriers semantics. */
#    undef ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__
#    define ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__ ETHR_GCC_VOLATILE_BIT_MASK__

/* Loads imply acquire barrier semantics. */
#    undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__
#    define ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__ ETHR_GCC_VOLATILE_BIT_MASK__

/*
 * Trust load acquire and store release for sizes
 * where volatile operation implies these barrier
 * semantics since no barriers are needed.
 */
#    if !ETHR_GCC_ACQB_VERSIONS__
#      undef ETHR_GCC_ACQB_VERSIONS__
#      define ETHR_GCC_ACQB_VERSIONS__ ETHR_GCC_VOLATILE_BIT_MASK__
#    endif
#    if !ETHR_GCC_RELB_VERSIONS__
#      undef ETHR_GCC_RELB_VERSIONS__
#      define ETHR_GCC_RELB_VERSIONS__ ETHR_GCC_VOLATILE_BIT_MASK__
#    endif

/*
 * No point exposing relaxed versions at all since
 * all mod operations are implemented with locked
 * instructions implying full memory barriers and
 * volatile store and load imply release and
 * acquire barrier semantics.
 */
#    undef ETHR_GCC_RELAXED_VERSIONS__
#    define ETHR_GCC_RELAXED_VERSIONS__ 0

#  endif /* !ETHR_X86_OUT_OF_ORDER */

/* #endif defined(__i386__) || defined(__x86_64__) */
#elif defined(__powerpc__) || defined(__ppc__)

#  if !defined(ETHR_PPC_HAVE_LWSYNC)
/*
 * Release barriers are typically implemented using
 * the lwsync instruction. We want our runtime
 * configure test to determine if the lwsync
 * instruction is available on the system or not
 * before we use it. Therefore, do not implement any
 * native ops using the __ATOMIC_RELEASE model.
 */
#    undef ETHR_GCC_RELB_VERSIONS__
#    define ETHR_GCC_RELB_VERSIONS__ 0
#    if defined(ETHR_GCC_IMPLEMENT_ACQB_USING_LWSYNC)
/*
 * Acquire barriers are usually implemented by other means
 * than lwsync, but can be implemented using lwsync. Define
 * ETHR_GCC_IMPLEMENT_ACQB_USING_LWSYNC if acquire barriers
 * are implemented using lwsync.
 */
#      undef ETHR_GCC_ACQB_VERSIONS__
#      define ETHR_GCC_ACQB_VERSIONS__ 0
#    endif
#  endif

#endif /* defined(__powerpc__) || defined(__ppc__) */

#if !ETHR_GCC_RELAXED_VERSIONS__
#  undef ETHR_GCC_RELAXED_MOD_VERSIONS__
#  define ETHR_GCC_RELAXED_MOD_VERSIONS__ 0
#endif

#if !ETHR_GCC_ACQB_VERSIONS__
#  undef ETHR_GCC_ACQB_MOD_VERSIONS__
#  define ETHR_GCC_ACQB_MOD_VERSIONS__ 0
#endif

#if !ETHR_GCC_RELB_VERSIONS__
#  undef ETHR_GCC_RELB_MOD_VERSIONS__
#  define ETHR_GCC_RELB_MOD_VERSIONS__ 0
#endif

#if !defined(ETHR_HAVE_NATIVE_ATOMIC32)
#  define ETHR_ATOMIC_WANT_32BIT_IMPL__
#  include "ethr_atomic.h"
#endif

#if ETHR_SIZEOF_PTR == 8 && !defined(ETHR_HAVE_NATIVE_ATOMIC64)
#  define ETHR_ATOMIC_WANT_64BIT_IMPL__
#  include "ethr_atomic.h"
#endif

#if defined(__x86_64__)
/*
 * No instructions available for native implementation
 * of these for dw-atomics...
 */
#  undef ETHR_GCC_RELAXED_VERSIONS__
#  define ETHR_GCC_RELAXED_VERSIONS__ 0
#  undef ETHR_GCC_ACQB_VERSIONS__
#  define ETHR_GCC_ACQB_VERSIONS__ 0
#  undef ETHR_GCC_RELB_VERSIONS__
#  define ETHR_GCC_RELB_VERSIONS__ 0
#endif

#if !ETHR_GCC_RELAXED_VERSIONS__
#  undef ETHR_GCC_RELAXED_MOD_VERSIONS__
#  define ETHR_GCC_RELAXED_MOD_VERSIONS__ 0
#endif

#if !ETHR_GCC_ACQB_VERSIONS__
#  undef ETHR_GCC_ACQB_MOD_VERSIONS__
#  define ETHR_GCC_ACQB_MOD_VERSIONS__ 0
#endif

#if !ETHR_GCC_RELB_VERSIONS__
#  undef ETHR_GCC_RELB_MOD_VERSIONS__
#  define ETHR_GCC_RELB_MOD_VERSIONS__ 0
#endif

#if (!defined(ETHR_HAVE_NATIVE_DW_ATOMIC) \
     && !(ETHR_SIZEOF_PTR == 4 && defined(ETHR_HAVE_NATIVE_ATOMIC64)) \
     && !(ETHR_SIZEOF_PTR == 8 && defined(ETHR_HAVE_NATIVE_ATOMIC128)))
#  include "ethr_dw_atomic.h"
#endif

#undef ETHR___atomic_load_ACQUIRE_barrier_bug
#undef ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE__
#undef ETHR_GCC_VOLATILE_STORE_IS_ATOMIC_STORE_RELB__
#undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD__
#undef ETHR_GCC_VOLATILE_LOAD_IS_ATOMIC_LOAD_ACQB__
#undef ETHR_GCC_RELAXED_VERSIONS__
#undef ETHR_GCC_RELB_VERSIONS__
#undef ETHR_GCC_RELB_VERSIONS__
#undef ETHR_GCC_RELAXED_MOD_VERSIONS__
#undef ETHR_GCC_ACQB_MOD_VERSIONS__
#undef ETHR_GCC_RELB_MOD_VERSIONS__
#undef ETHR_GCC_MB_MOD_VERSIONS__

#endif /* ETHREAD_GCC_NATIVE_H__ */
