/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2011-2015. All Rights Reserved.
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
 * Description: Memory barriers when using gcc's __atomic and
 *              __sync builtins
 * Author: Rickard Green
 *
 * Note: The C11 memory model implemented by gcc's __atomic
 *       builtins does not match the ethread API very well.
 *
 *       A function with a barrier postfix in the ethread atomic
 *       API needs to ensure that all stores and loads are
 *       ordered around it according to the semantics of the
 *       barrier specified.
 *
 *       The C11 aproch is different. The __atomic builtins
 *       API takes a memory model parameter. Assuming that all
 *       memory syncronizations using the involved atomic
 *       variables are made using this API, the synchronizations
 *       will adhere to the memory models used. That is, you do
 *       *not* know how loads and stores will be ordered around
 *       a specific __atomic operation in the general case. You
 *       only know the total effect of the combination of
 *       operations issued will adhere to the model.
 *
 *       This limits how we can use the __atomic builtins. What
 *       we cannot use:
 *
 *       1. We cannot rely on __atomic_thread_fence() to issue
 *          any specific memory barriers at all. This regardless
 *          of memory model parameter passed. That is, we cannot
 *          use the __atomic_thread_fence() builtin at all.
 *
 *          Why is this? If all __atomic builtins accessing
 *          memory issue memory barriers, __atomic_thread_fence()
 *          does not have to issue memory barriers. The
 *          implementation for the Itanium architecture is an
 *          example of this. Even using the __ATOMIC_RELAXED
 *          memory model all __atomic builtins accessing memory
 *          will issue memory barriers. Due to this no memory
 *          barriers at all will be issued by
 *           __atomic_thread_fence() using either one of the
 *          __ATOMIC_CONSUME, __ATOMIC_ACQUIRE, or
 *          __ATOMIC_RELEASE memory models.
 *
 *       2. We cannot rely on any __atomic builtin with the
 *          __ATOMIC_SEQ_CST memory model parameters to
 *          issue any specific memory barriers. That is, we
 *          cannot use these memory models at all.
 *
 *          Why is this? Since all synchronizations is expected
 *          to be made using the __atomic builtins, memory
 *          barriers only have to be issued by some of them,
 *          and you do not know which ones wont issue memory
 *          barriers.
 *
 *          One can easily be fooled into believing that when
 *          using the __ATOMIC_SEQ_CST memory model on all
 *          operations, all operations will issue full memory
 *          barriers. This is however not the case. The
 *          implementation for the x86_64 architecture is an
 *          example of this. Since all operations except loads
 *          issue full memory barriers, no memory barriers at
 *          all is issued by loads. This could also be
 *          implemented by issuing a full memory barrier on
 *          loads, but no barrier at all on stores.
 *
 *       What can be used then?
 *       1. All (legacy) __sync builtins implying full memory
 *          barriers issued.
 *       2. All __atomic builtins using the __ATOMIC_RELAXED
 *          memory model can, of course, be used. This since
 *          no ordering guarantees at all are made.
 *       3. All __atomic builtins accessing memory using the
 *          __ATOMIC_ACQUIRE and __ATOMIC_RELEASE memory
 *          models. This since an __atomic builtin memory
 *          access using the __ATOMIC_ACQUIRE must at least
 *          issue an aquire memory barrier and an __atomic
 *          builtin memory acess with the __ATOMIC_RELEASE
 *          memory model must at least issue a release memory
 *          barrier. Otherwise the two can not be paired.
 *       4. All __atomic builtins accessing memory using the
 *          __ATOMIC_CONSUME builtin can be used for the same
 *          reason __ATOMIC_ACQUIRE can be used. The ethread
 *          atomic framework implementing the ethread API
 *          using native implementations does not expect the
 *          native implementations to produce versions with
 *          data dependent read barriers, so until the
 *          framework is changed we haven't got any use for
 *          for it.
 *
 *       For some architectures we have our own memory barrier
 *       implementations. We prefer to use these since they
 *       should be as fine grained as possible. For other
 *       architectures we use the __sync_synchronize() builtin
 *       which issue a full memory barrier. For these
 *       architectures we have to assume that all loads and
 *       stores can be reordered without limitation. That is,
 *       unnecessary memory barriers will be issued if such
 *       reordering actually cannot occur.
 */

/*
 * We prefer to use our own memory barrier implementation if
 * such exist instead of using __sync_synchronize()...
 */
#if defined(__i386__) || defined(__x86_64__)
#  include "../i386/ethr_membar.h"
#elif defined(__sparc__)
#  include "../sparc32/ethr_membar.h"
#elif defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#  include "../ppc32/ethr_membar.h"
#elif !defined(ETHR_GCC_ATOMIC_MEMBAR_H__)			\
    && (ETHR_HAVE_GCC_ASM_ARM_DMB_INSTRUCTION			\
	|| ETHR_HAVE___sync_synchronize				\
	|| (ETHR_HAVE___sync_val_compare_and_swap & 12))
#define ETHR_GCC_ATOMIC_MEMBAR_H__

#define ETHR_LoadLoad	(1 << 0)
#define ETHR_LoadStore	(1 << 1)
#define ETHR_StoreLoad	(1 << 2)
#define ETHR_StoreStore	(1 << 3)

#define ETHR_COMPILER_BARRIER __asm__ __volatile__("" : : : "memory")

#if ETHR_HAVE_GCC_ASM_ARM_DMB_INSTRUCTION

static __inline__ __attribute__((__always_inline__)) void
ethr_full_fence__(void)
{
    __asm__ __volatile__("dmb sy" : : : "memory");
}

static __inline__ __attribute__((__always_inline__)) void
ethr_store_fence__(void)
{
    __asm__ __volatile__("dmb st" : : : "memory");
}

#define ETHR_MEMBAR(B) \
 ETHR_CHOOSE_EXPR((B) == ETHR_StoreStore, ethr_store_fence__(), ethr_full_fence__())

#elif ETHR_HAVE___sync_synchronize

static __inline__ __attribute__((__always_inline__)) void
ethr_full_fence__(void)
{
    /*
     * The compiler barriers are here to fix missing clobbers
     * in __sync_synchronize() when using buggy LLVM
     * implementation of __sync_synchronize(). They
     * do not introduce any unnecessary overhead when used
     * here, so we use them for all systems.
     */
    ETHR_COMPILER_BARRIER;
    __sync_synchronize();
    ETHR_COMPILER_BARRIER;
}

#else /* !ETHR_HAVE___sync_synchronize */

/*
 * Buggy __sync_synchronize(); call __sync_val_compare_and_swap()
 * instead which imply a full memory barrier (and hope that one
 * isn't buggy too).
 */

#if (ETHR_HAVE___sync_val_compare_and_swap & 4)
#  define ETHR_MB_T__ ethr_sint32_t
#elif (ETHR_HAVE___sync_val_compare_and_swap & 8)
#  define ETHR_MB_T__ ethr_sint64_t
#endif

static __inline__ __attribute__((__always_inline__)) void
ethr_full_fence__(void)
{
    volatile ETHR_MB_T__ x = 0;
    (void) __sync_val_compare_and_swap(&x, (ETHR_MB_T__) 0, (ETHR_MB_T__) 1);
}

#endif /* !ETHR_HAVE___sync_synchronize */

#ifndef ETHR_MEMBAR
#  define ETHR_MEMBAR(B) ethr_full_fence__()
#endif

/*
 * Define ETHR_READ_DEPEND_MEMORY_BARRIER for all architechtures
 * not known to order data dependent loads
 */

#if !defined(__ia64__) && !defined(__arm__)
#  define ETHR_READ_DEPEND_MEMORY_BARRIER ETHR_MEMBAR(ETHR_LoadLoad)
#endif

#endif /* ETHR_GCC_ATOMIC_MEMBAR_H__ */
