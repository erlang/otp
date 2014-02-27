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
 * Description: Memory barriers for Windows
 * Author: Rickard Green
 */

#if (!defined(ETHR_WIN_MEMBAR_H__) \
     && (defined(_MSC_VER) && _MSC_VER >= 1400) \
     && (defined(_M_AMD64) \
	 || defined(_M_IA64) \
	 || defined(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE)))
#define ETHR_WIN_MEMBAR_H__

#define ETHR_LoadLoad	(1 << 0)
#define ETHR_LoadStore	(1 << 1)
#define ETHR_StoreLoad	(1 << 2)
#define ETHR_StoreStore	(1 << 3)

#include <intrin.h>
#undef ETHR_COMPILER_BARRIER
#define ETHR_COMPILER_BARRIER _ReadWriteBarrier()
#pragma intrinsic(_ReadWriteBarrier)

#pragma intrinsic(_InterlockedCompareExchange)

#define ETHR_MB_USING_INTERLOCKED__			\
do {							\
    volatile long x___ = 0;				\
    (void) _InterlockedCompareExchange(&x___, 2, 1);	\
} while (0)

#if defined(_M_IA64)

#define ETHR_MEMBAR(B) __mf()

#elif defined(_M_AMD64) || defined(_M_IX86)

#include <emmintrin.h>
#include <mmintrin.h>

#if ETHR_SIZEOF_PTR == 4
#  define ETHR_NO_SSE2_MB__ ETHR_MB_USING_INTERLOCKED__
#endif
#pragma intrinsic(_mm_mfence)
#pragma intrinsic(_mm_sfence)
#pragma intrinsic(_mm_lfence)

static ETHR_FORCE_INLINE void
ethr_cfence__(void)
{
    _ReadWriteBarrier();
}

static ETHR_FORCE_INLINE void
ethr_mfence__(void)
{
#if ETHR_SIZEOF_PTR == 4
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	ETHR_NO_SSE2_MB__;
    else
#endif
	_mm_mfence();
}

static ETHR_FORCE_INLINE void
ethr_sfence__(void)
{
#if ETHR_SIZEOF_PTR == 4
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	ETHR_NO_SSE2_MB__;
    else
#endif
	_mm_sfence();
}

static ETHR_FORCE_INLINE void
ethr_lfence__(void)
{
#if ETHR_SIZEOF_PTR == 4
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	ETHR_NO_SSE2_MB__;
    else
#endif
	_mm_lfence();
}

#define ETHR_X86_OUT_OF_ORDER_MEMBAR(B)				\
  ETHR_CHOOSE_EXPR((B) == ETHR_StoreStore,			\
		   ethr_sfence__(),				\
		   ETHR_CHOOSE_EXPR((B) == ETHR_LoadLoad,	\
				    ethr_lfence__(),		\
				    ethr_mfence__()))

#ifdef ETHR_X86_OUT_OF_ORDER

#define ETHR_MEMBAR(B) \
  ETHR_X86_OUT_OF_ORDER_MEMBAR((B))

#else /* !ETHR_X86_OUT_OF_ORDER (the default) */

/*
 * We assume that only stores before loads may be reordered. That is,
 * we assume that *no* instructions like these are used:
 * - CLFLUSH,
 * - streaming stores executed with non-temporal move,
 * - string operations, or
 * - other instructions which aren't LoadLoad, LoadStore, and StoreStore
 *   ordered by themselves
 * If such instructions are used, either insert memory barriers
 * using ETHR_X86_OUT_OF_ORDER_MEMBAR() at appropriate places, or
 * define ETHR_X86_OUT_OF_ORDER. For more info see Intel 64 and IA-32
 * Architectures Software Developer's Manual; Vol 3A; Chapter 8.2.2.
 */

#define ETHR_MEMBAR(B) \
  ETHR_CHOOSE_EXPR((B) & ETHR_StoreLoad, ethr_mfence__(), ethr_cfence__())

#endif

#else /* No knowledge about platform; use interlocked fallback */

#define ETHR_MEMBAR(B) ETHR_MB_USING_INTERLOCKED__
#define ETHR_READ_DEPEND_MEMORY_BARRIER ETHR_MB_USING_INTERLOCKED__

#endif

#endif /* ETHR_WIN_MEMBAR_H__ */
