/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
 * - AO_load() || AO_load_aquire()
 * - AO_store() || AO_store_release()
 * - AO_compare_and_swap() || AO_compare_and_swap_acquire()
 *   || AO_compare_and_swap_release() || AO_compare_and_swap_full()
 *
 */

#if ETHR_SIZEOF_AO_T == 4
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL ETHR_NATIVE_IMPL__
#define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#define ETHR_ATMC_T__ ethr_native_atomic32_t
#define ETHR_AINT_T__ ethr_sint32_t
#define ETHR_AINT_SUFFIX__ "l"
#elif ETHR_SIZEOF_AO_T == 8
#define ETHR_HAVE_NATIVE_ATOMIC64 1
#define ETHR_NATIVE_ATOMIC64_IMPL ETHR_NATIVE_IMPL__
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

#ifdef AO_HAVE_store

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

#endif

#ifdef AO_HAVE_store_write

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_WB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_WB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_wb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    AO_store_write(&var->counter, (AO_t) value);
}

#endif

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

#ifdef AO_HAVE_store_full

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_MB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ value)
{
    AO_store_full(&var->counter, (AO_t) value);
}

#endif

#ifdef AO_HAVE_load

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

#endif

#ifdef AO_HAVE_load_read

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ_RB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ_RB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_rb)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__) AO_load_read(&var->counter);
}

#endif

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

#ifdef AO_HAVE_load_full

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read_mb)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__) AO_load_full(&var->counter);
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

#ifdef AO_HAVE_fetch_and_add_acquire

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_acqb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add_acquire(&var->counter, (AO_t) incr)) + incr;
}

#endif

#ifdef AO_HAVE_fetch_and_add_release

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add_release(&var->counter, (AO_t) incr)) + incr;
}

#endif

#ifdef AO_HAVE_fetch_and_add_full

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ incr)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add_full(&var->counter, (AO_t) incr)) + incr;
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

#ifdef AO_HAVE_fetch_and_add1_release

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_INC_RETURN_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_relb)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add1_release(&var->counter)) + 1;
}

#endif

#ifdef AO_HAVE_fetch_and_add1_full

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_INC_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_mb)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_add1_full(&var->counter)) + 1;
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

#ifdef AO_HAVE_fetch_and_sub1_acquire

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_DEC_RETURN_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_acqb)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_sub1_acquire(&var->counter)) - 1;
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

#ifdef AO_HAVE_fetch_and_sub1_full

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_DEC_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_mb)(ETHR_ATMC_T__ *var)
{
    return ((ETHR_AINT_T__) AO_fetch_and_sub1_full(&var->counter)) - 1;
}

#endif

#if defined(AO_HAVE_compare_and_swap_full) || defined(AO_HAVE_fetch_compare_and_swap_full)

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_mb)(ETHR_ATMC_T__ *var,
			      ETHR_AINT_T__ new,
			      ETHR_AINT_T__ exp)
{
#if defined(AO_HAVE_fetch_compare_and_swap_full)
    return (ETHR_AINT_T__) AO_fetch_compare_and_swap_full(&var->counter,
							  (AO_t) exp,
							  (AO_t) new);
#else
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_full(&var->counter, (AO_t) exp, (AO_t) new))
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
#endif
}

#endif

#if defined(AO_HAVE_compare_and_swap) || defined(AO_HAVE_fetch_compare_and_swap)

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
#if defined(AO_HAVE_fetch_compare_and_swap)
    return (ETHR_AINT_T__) AO_fetch_compare_and_swap(&var->counter,
						     (AO_t) exp,
						     (AO_t) new);
#else
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
#ifdef AO_HAVE_load
	act = (ETHR_AINT_T__) AO_load(&var->counter);
#else
	act = (ETHR_AINT_T__) AO_load_aquire(&var->counter);
#endif
    } while (act == exp);
    return act;
#endif
}

#endif

#if defined(AO_HAVE_compare_and_swap_acquire) || defined(AO_HAVE_fetch_compare_and_swap_acquire)

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
#if defined(AO_HAVE_fetch_compare_and_swap_acquire)
    return (ETHR_AINT_T__) AO_fetch_compare_and_swap_acquire(&var->counter,
							     (AO_t) exp,
							     (AO_t) new);
#else
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
#endif
}

#endif

#if defined(AO_HAVE_compare_and_swap_read) || defined(AO_HAVE_fetch_compare_and_swap_read)

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_RB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_RB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_rb)(ETHR_ATMC_T__ *var,
			      ETHR_AINT_T__ new,
			      ETHR_AINT_T__ exp)
{
#if defined(AO_HAVE_fetch_compare_and_swap_read)
    return (ETHR_AINT_T__) AO_fetch_compare_and_swap_read(&var->counter,
							     (AO_t) exp,
							     (AO_t) new);
#else
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_read(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
#if defined(AO_HAVE_load_read)
	act = (ETHR_AINT_T__) AO_load_read(&var->counter);
#elif defined(AO_HAVE_load)
	act = (ETHR_AINT_T__) AO_load(&var->counter);
#else
	act = (ETHR_AINT_T__) AO_load_acquire(&var->counter);
#endif
    } while (act == exp);
#ifndef AO_HAVE_load_read
#ifdef AO_HAVE_nop_read
    AO_nop_read();
#else
    AO_nop_full();
#endif
#endif
    return act;
#endif
}

#endif

#if defined(AO_HAVE_compare_and_swap_release) || defined(AO_HAVE_fetch_compare_and_swap_release)

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
#if defined(AO_HAVE_fetch_compare_and_swap_release)
    return (ETHR_AINT_T__) AO_fetch_compare_and_swap_release(&var->counter,
							     (AO_t) exp,
							     (AO_t) new);
#else
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_release(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
#ifdef AO_HAVE_load
	act = (ETHR_AINT_T__) AO_load(&var->counter);
#else
	act = (ETHR_AINT_T__) AO_load_acquire(&var->counter);
#endif
    } while (act == exp);
    return act;
#endif
}

#endif

#if defined(AO_HAVE_compare_and_swap_write) || defined(AO_HAVE_fetch_compare_and_swap_write)

#if ETHR_SIZEOF_AO_T == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_WB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_WB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_wb)(ETHR_ATMC_T__ *var,
			      ETHR_AINT_T__ new,
			      ETHR_AINT_T__ exp)
{
#if defined(AO_HAVE_fetch_compare_and_swap_write)
    return (ETHR_AINT_T__) AO_fetch_compare_and_swap_write(&var->counter,
							   (AO_t) exp,
							   (AO_t) new);
#else
    ETHR_AINT_T__ act;
    do {
	if (AO_compare_and_swap_write(&var->counter, (AO_t) exp, (AO_t) new))
	    return exp;
#ifdef AO_HAVE_load
	act = (ETHR_AINT_T__) AO_load(&var->counter);
#else
	act = (ETHR_AINT_T__) AO_load_acquire(&var->counter);
#endif
    } while (act == exp);
    return act;
#endif
}

#endif

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__

#endif /* ETHR_LIBATOMIC_OPS_ATOMIC_H__ */
