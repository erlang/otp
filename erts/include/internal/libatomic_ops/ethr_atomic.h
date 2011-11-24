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
 */

#if ETHR_SIZEOF_AO_T == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL "libatomic_ops"
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#define ETHR_AINT_SUFFIX__ "l"
#elif ETHR_SIZEOF_AO_T == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATIVE_ATOMIC64_IMPL "libatomic_ops"
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic64_t
#define ETHR_AINT_T__ ethr_sint64_t
#define ETHR_AINT_SUFFIX__ "q"
#else
#error "Unsupported integer size"
#endif

typedef struct {
    volatile AO_t counter;
} ETHR_ATMC_T__;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADDR 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADDR 1
#endif

static ETHR_INLINE ETHR_AINT_T__ *
ETHR_NATMC_FUNC__(addr)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__ *) &var->counter;
}

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    AO_store(&var->counter, (AO_t) value);
}

#ifdef AO_HAVE_store_release

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_RELB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    AO_store_release(&var->counter, (AO_t) value);
}

#endif

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__) AO_load(&var->counter);
}

#ifdef AO_HAVE_load_acquire

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_acqb)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__) AO_load_acquire(&var->counter);
}

#endif

#ifdef AO_HAVE_fetch_and_add

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add(&var->counter, (AO_t) incr)) + incr;
}

#endif

#ifdef AO_HAVE_fetch_and_add1

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_INC_RETURN 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add1(&var->counter)) + 1;
}

#endif

#ifdef AO_HAVE_fetch_and_add1_acquire

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_INC_RETURN_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_acqb)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add1_acquire(&var->counter)) + 1;
}

#endif

#ifdef AO_HAVE_fetch_and_sub1

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_DEC_RETURN 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_sub1(&var->counter)) - 1;
}

#endif

#ifdef AO_HAVE_fetch_and_sub1_release

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_DEC_RETURN_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_relb)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_sub1_release(&var->counter)) - 1;
}

#endif

#ifdef AO_HAVE_compare_and_swap

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg)(ETHR_ATMC_T__ *var,
			   ETHR_AINT_T__ new,
			   ETHR_AINT_T__ exp)
{
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (ETHR_AINT_T__) AO_load(&var->counter);
    } while (act == exp);
    return act;
}

#endif

#ifdef AO_HAVE_compare_and_swap_acquire

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ exp)
{
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_acquire(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
#ifdef AO_HAVE_load_acquire
	act = (ETHR_AINT_T__) AO_load_acquire(&var->counter);
#else
	act = (ETHR_AINT_T__) AO_load(&var->counter);
#endif
    } while (act == exp);
#ifndef AO_HAVE_load_acquire
    AO_nop_full();
#endif
    return act;
}

#endif

#ifdef AO_HAVE_compare_and_swap_release

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_relb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ exp)
{
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_release(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
	act = (ETHR_AINT_T__) AO_load(&var->counter);
    } while (act == exp);
    return act;
}

#endif

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__

#endif /* ETHR_LIBATOMIC_OPS_ATOMIC_H__ */
