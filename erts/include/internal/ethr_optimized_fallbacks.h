/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
 * Description: Optimized fallbacks used when native ops are missing
 * Author: Rickard Green
 */

#ifndef ETHR_OPTIMIZED_FALLBACKS_H__
#define ETHR_OPTIMIZED_FALLBACKS_H__

#if defined(ETHR_HAVE_NATIVE_SPINLOCKS)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE int
ethr_native_spinlock_destroy(ethr_native_spinlock_t *lock)
{
    return 0;
}

#endif

#elif defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
/* --- Native spinlocks using pthread spinlocks -------------------------- */
#define ETHR_HAVE_NATIVE_SPINLOCKS 1

#define ETHR_NATIVE_SPINLOCK_IMPL "pthread"

typedef pthread_spinlock_t ethr_native_spinlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_spinlock_init(ethr_native_spinlock_t *lock)
{
    int err = pthread_spin_init((pthread_spinlock_t *) lock, 0);
    if (err)
	ETHR_FATAL_ERROR__(err);
}

static ETHR_INLINE int
ethr_native_spinlock_destroy(ethr_native_spinlock_t *lock)
{
    return pthread_spin_destroy((pthread_spinlock_t *) lock);
}

static ETHR_INLINE void
ethr_native_spin_unlock(ethr_native_spinlock_t *lock)
{
    int err = pthread_spin_unlock((pthread_spinlock_t *) lock);
    if (err)
	ETHR_FATAL_ERROR__(err);
}

static ETHR_INLINE void
ethr_native_spin_lock(ethr_native_spinlock_t *lock)
{
    int err = pthread_spin_lock((pthread_spinlock_t *) lock);
    if (err)
	ETHR_FATAL_ERROR__(err);
}

#endif

#elif defined(ETHR_HAVE_32BIT_NATIVE_ATOMIC_OPS)
/* --- Native spinlocks using native atomics -------------------------------- */
#define ETHR_HAVE_NATIVE_SPINLOCKS 1

#define ETHR_NATIVE_SPINLOCK_IMPL "native-atomics"

typedef ethr_atomic32_t ethr_native_spinlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

#undef ETHR_NSPN_AOP__
#define ETHR_NSPN_AOP__(X) ETHR_INLINE_ATMC32_FUNC_NAME_(ethr_atomic32_ ## X)

static ETHR_INLINE void
ethr_native_spinlock_init(ethr_native_spinlock_t *lock)
{
    ETHR_NSPN_AOP__(init)(lock, 0);
}

static ETHR_INLINE int
ethr_native_spinlock_destroy(ethr_native_spinlock_t *lock)
{
    return ETHR_NSPN_AOP__(read)(lock) == 0 ? 0 : EBUSY;
}

static ETHR_INLINE void
ethr_native_spin_unlock(ethr_native_spinlock_t *lock)
{
    ETHR_ASSERT(ETHR_NSPN_AOP__(read)(lock) == 1);
    ETHR_NSPN_AOP__(set_relb)(lock, 0);
}

static ETHR_INLINE void
ethr_native_spin_lock(ethr_native_spinlock_t *lock)
{
    while (ETHR_NSPN_AOP__(cmpxchg_acqb)(lock, 1, 0) != 0) {
	while (ETHR_NSPN_AOP__(read)(lock) != 0)
	    ETHR_SPIN_BODY;
    }
    ETHR_COMPILER_BARRIER;
}

#undef ETHR_NSPN_AOP__

#endif

#endif


#if defined(ETHR_HAVE_NATIVE_RWSPINLOCKS)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE int
ethr_native_rwlock_destroy(ethr_native_rwlock_t *lock)
{
    return 0;
}

#endif

#elif defined(ETHR_HAVE_32BIT_NATIVE_ATOMIC_OPS)
/* --- Native rwspinlocks using native atomics ------------------------------ */
#define ETHR_HAVE_NATIVE_RWSPINLOCKS 1
#define ETHR_NATIVE_RWSPINLOCK_IMPL "native-atomics"

typedef ethr_atomic32_t ethr_native_rwlock_t;
#  define ETHR_WLOCK_FLAG__ (((ethr_sint32_t) 1) << 30)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

#undef ETHR_NRWSPN_AOP__
#define ETHR_NRWSPN_AOP__(X) ETHR_INLINE_ATMC32_FUNC_NAME_(ethr_atomic32_ ## X)

static ETHR_INLINE void
ethr_native_rwlock_init(ethr_native_rwlock_t *lock)
{
    ETHR_NRWSPN_AOP__(init)(lock, 0);
}

static ETHR_INLINE int
ethr_native_rwlock_destroy(ethr_native_rwlock_t *lock)
{
    return ETHR_NRWSPN_AOP__(read)(lock) == 0 ? 0 : EBUSY;
}

static ETHR_INLINE void
ethr_native_read_unlock(ethr_native_rwlock_t *lock)
{
    ETHR_ASSERT(ETHR_NRWSPN_AOP__(read)(lock) >= 0);
    ETHR_NRWSPN_AOP__(dec_relb)(lock);
}

static ETHR_INLINE void
ethr_native_read_lock(ethr_native_rwlock_t *lock)
{
    ethr_sint32_t act, exp = 0;
    while (1) {
	act = ETHR_NRWSPN_AOP__(cmpxchg_acqb)(lock, exp+1, exp);
	if (act == exp)
	    break;
	/* Wait for writer to leave */
	while (act & ETHR_WLOCK_FLAG__) {
	    ETHR_SPIN_BODY;
	    act = ETHR_NRWSPN_AOP__(read)(lock);
	}
	exp = act;
    }
    ETHR_COMPILER_BARRIER;
}

static ETHR_INLINE void
ethr_native_write_unlock(ethr_native_rwlock_t *lock)
{
    ETHR_ASSERT(ETHR_NRWSPN_AOP__(read)(lock) == ETHR_WLOCK_FLAG__);
    ETHR_NRWSPN_AOP__(set_relb)(lock, 0);
}

static ETHR_INLINE void
ethr_native_write_lock(ethr_native_rwlock_t *lock)
{
    ethr_sint32_t act, exp = 0;
    while (1) {
	act = ETHR_NRWSPN_AOP__(cmpxchg_acqb)(lock, exp|ETHR_WLOCK_FLAG__, exp);
	if (act == exp)
	    break;
	/* Wait for writer to leave */
	while (act & ETHR_WLOCK_FLAG__) {
	    ETHR_SPIN_BODY;
	    act = ETHR_NRWSPN_AOP__(read)(lock);
	}
	exp = act;
    }
    act |= ETHR_WLOCK_FLAG__;
    /* Wait for readers to leave */
    while (act != ETHR_WLOCK_FLAG__) {
	ETHR_SPIN_BODY;
	act = ETHR_NRWSPN_AOP__(read_acqb)(lock);
    }
    ETHR_COMPILER_BARRIER;
}

#undef ETHR_NRWSPN_AOP__

#endif

#endif

#endif
