/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
 * Description: Native atomics ethread support using libatomic_ops
 * Author: Rickard Green
 */

#ifndef ETHR_LIBATOMIC_OPS_ATOMIC_H__
#define ETHR_LIBATOMIC_OPS_ATOMIC_H__

#if !defined(ETHR_HAVE_NATIVE_ATOMICS) && defined(ETHR_HAVE_LIBATOMIC_OPS)
#define ETHR_HAVE_NATIVE_ATOMICS 1

#if (defined(__i386__) && !defined(ETHR_PRE_PENTIUM4_COMPAT)) \
    || defined(__x86_64__)
#define AO_USE_PENTIUM4_INSTRS
#endif

#include "atomic_ops.h"

/*
 * libatomic_ops can be downloaded from:
 *   http://www.hpl.hp.com/research/linux/atomic_ops/
 *
 * These operations need to be defined by libatomic_ops;
 * otherwise, we won't compile:
 * - AO_nop_full()
 * - AO_load()
 * - AO_store()
 * - AO_compare_and_swap()
 *
 * The `AO_t' type also have to be at least as large as the `void *' type.
 */

#if ETHR_SIZEOF_AO_T < ETHR_SIZEOF_PTR
#error The AO_t type is too small
#endif

#if ETHR_SIZEOF_AO_T == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#define ETHR_AINT_SUFFIX__ "l"
#elif ETHR_SIZEOF_AO_T == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t
#define ETHR_AINT_SUFFIX__ "q"
#else
#error "Unsupported integer size"
#endif

#if ETHR_SIZEOF_AO_T == 8
typedef union {
    volatile AO_t counter;
    ethr_sint32_t sint32[2];
} ETHR_ATMC_T__;
#else
typedef struct {
    volatile AO_t counter;
} ETHR_ATMC_T__;
#endif

#define ETHR_MEMORY_BARRIER AO_nop_full()
#ifdef AO_HAVE_nop_write
# define ETHR_WRITE_MEMORY_BARRIER AO_nop_write()
#else
# define ETHR_WRITE_MEMORY_BARRIER ETHR_MEMORY_BARRIER
#endif
#ifdef AO_HAVE_nop_read
#  define ETHR_READ_MEMORY_BARRIER AO_nop_read()
#else
#  define ETHR_READ_MEMORY_BARRIER ETHR_MEMORY_BARRIER
#endif
#ifdef AO_NO_DD_ORDERING
#  define ETHR_READ_DEPEND_MEMORY_BARRIER ETHR_READ_MEMORY_BARRIER
#else
#  define ETHR_READ_DEPEND_MEMORY_BARRIER AO_compiler_barrier()
#endif

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

static ETHR_INLINE ETHR_AINT_T__ *
ETHR_NATMC_FUNC__(addr)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__ *) &var->counter;
}

#if ETHR_SIZEOF_AO_T == 8
/*
 * We also need to provide an ethr_native_atomic32_addr(), since
 * this 64-bit implementation will be used implementing 32-bit
 * native atomics.
 */

static ETHR_INLINE ethr_sint32_t *
ethr_native_atomic32_addr(ETHR_ATMC_T__ *var)
{
    ETHR_ASSERT(((void *) &var->sint32[0]) == ((void *) &var->counter));
#ifdef ETHR_BIGENDIAN
    return &var->sint32[1];
#else
    return &var->sint32[0];
#endif
}

#endif /* ETHR_SIZEOF_AO_T == 8 */

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    AO_store(&var->counter, (AO_t) value);
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(init)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    ETHR_NATMC_FUNC__(set)(var, value);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__) AO_load(&var->counter);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
#ifdef AO_HAVE_fetch_and_add_full
    return ((ETHR_AINT_T__) AO_fetch_and_add_full(&var->counter, (AO_t) incr)) + incr;
#else
    while (1) {
	AO_t exp = AO_load(&var->counter);
	AO_t new = exp + (AO_t) incr;
	if (AO_compare_and_swap_full(&var->counter, exp, new))
	    return (ETHR_AINT_T__) new;
    }
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(add)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    (void) ETHR_NATMC_FUNC__(add_return)(var, incr);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return)(ETHR_ATMC_T__ *var)
{
#ifdef AO_HAVE_fetch_and_add1_full
    return ((ETHR_AINT_T__) AO_fetch_and_add1_full(&var->counter)) + 1;
#else
    return ETHR_NATMC_FUNC__(add_return)(var, 1);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(inc)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_NATMC_FUNC__(inc_return)(var);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return)(ETHR_ATMC_T__ *var)
{
#ifdef AO_HAVE_fetch_and_sub1_full
    return ((ETHR_AINT_T__) AO_fetch_and_sub1_full(&var->counter)) - 1;
#else
    return ETHR_NATMC_FUNC__(add_return)(var, -1);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_NATMC_FUNC__(dec_return)(var);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    while (1) {
	AO_t exp = AO_load(&var->counter);
	AO_t new = exp & ((AO_t) mask);
	if (AO_compare_and_swap_full(&var->counter, exp, new))
	    return (ETHR_AINT_T__) exp;
    }
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    while (1) {
	AO_t exp = AO_load(&var->counter);
	AO_t new = exp | ((AO_t) mask);
	if (AO_compare_and_swap_full(&var->counter, exp, new))
	    return (ETHR_AINT_T__) exp;
    }
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var,
			   ETHR_AINT_T__ new,
			   ETHR_AINT_T__ exp)
{
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_full(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (ETHR_AINT_T__) AO_load(&var->counter);
    } while (act == exp);
    return act;
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(xchg)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ new)
{
    while (1) {
	AO_t exp = AO_load(&var->counter);
	if (AO_compare_and_swap_full(&var->counter, exp, (AO_t) new))
	    return (ETHR_AINT_T__) exp;
    }
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_acqb)(ETHR_ATMC_T__ *var)
{
#ifdef AO_HAVE_load_acquire
    return (ETHR_AINT_T__) AO_load_acquire(&var->counter);
#else
    ETHR_AINT_T__ res = ETHR_NATMC_FUNC__(read)(var);
    ETHR_MEMORY_BARRIER;
    return res;
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_acqb)(ETHR_ATMC_T__ *var)
{
#ifdef AO_HAVE_fetch_and_add1_acquire
    return ((ETHR_AINT_T__) AO_fetch_and_add1_acquire(&var->counter)) + 1;
#else
    ETHR_AINT_T__ res = ETHR_NATMC_FUNC__(add_return)(var, 1);
    return res;
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
#ifdef AO_HAVE_store_release
    AO_store_release(&var->counter, (AO_t) value);
#else
    ETHR_MEMORY_BARRIER;
    ETHR_NATMC_FUNC__(set)(var, value);
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_relb)(ETHR_ATMC_T__ *var)
{
#ifdef AO_HAVE_fetch_and_sub1_release
    return ((ETHR_AINT_T__) AO_fetch_and_sub1_release(&var->counter)) - 1;
#else
    return ETHR_NATMC_FUNC__(dec_return)(var);
#endif
}

static ETHR_INLINE void
ETHR_NATMC_FUNC__(dec_relb)(ETHR_ATMC_T__ *var)
{
    (void) ETHR_NATMC_FUNC__(dec_return_relb)(var);
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ exp)
{
#ifdef AO_HAVE_compare_and_swap_acquire
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_acquire(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (ETHR_AINT_T__) AO_load(&var->counter);
    } while (act == exp);
    AO_nop_full();
    return act;
#else
    ETHR_AINT_T__ act = ETHR_NATMC_FUNC__(cmpxchg)(var, new, exp);
    return act;
#endif
}

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_relb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ exp)
{
#ifdef AO_HAVE_compare_and_swap_release
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_release(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (ETHR_AINT_T__) AO_load(&var->counter);
    } while (act == exp);
    return act;
#else
    return ETHR_NATMC_FUNC__(cmpxchg)(var, new, exp);
#endif
}


#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__

#endif /* !defined(ETHR_HAVE_NATIVE_ATOMICS) && defined(ETHR_HAVE_LIBATOMIC_OPS) */

#endif /* ETHR_LIBATOMIC_OPS_ATOMIC_H__ */
