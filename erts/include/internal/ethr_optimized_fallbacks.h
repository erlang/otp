/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010. All Rights Reserved.
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
 * Description: "Optimized" fallbacks used when native ops are missing
 * Author: Rickard Green
 */

#ifndef ETHR_OPTIMIZED_FALLBACKS_H__
#define ETHR_OPTIMIZED_FALLBACKS_H__

#ifdef ETHR_HAVE_NATIVE_ATOMICS
#define ETHR_HAVE_OPTIMIZED_ATOMIC_OPS 1
#endif

#ifdef ETHR_HAVE_NATIVE_SPINLOCKS
#define ETHR_HAVE_OPTIMIZED_SPINLOCKS 1
#elif defined(ETHR_HAVE_PTHREAD_SPIN_LOCK)
/* --- Optimized spinlocks using pthread spinlocks -------------------------- */
#define ETHR_HAVE_OPTIMIZED_SPINLOCKS 1

typedef pthread_spinlock_t ethr_opt_spinlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE int
ethr_opt_spinlock_init(ethr_opt_spinlock_t *lock)
{
    return pthread_spin_init((pthread_spinlock_t *) lock, 0);
}

static ETHR_INLINE int
ethr_opt_spinlock_destroy(ethr_opt_spinlock_t *lock)
{
    return pthread_spin_destroy((pthread_spinlock_t *) lock);
}


static ETHR_INLINE int
ethr_opt_spin_unlock(ethr_opt_spinlock_t *lock)
{
    return pthread_spin_unlock((pthread_spinlock_t *) lock);
}

static ETHR_INLINE int
ethr_opt_spin_lock(ethr_opt_spinlock_t *lock)
{
    return pthread_spin_lock((pthread_spinlock_t *) lock);
}

#endif

#elif defined(ETHR_HAVE_NATIVE_ATOMICS)
/* --- Native spinlocks using native atomics -------------------------------- */
#define ETHR_HAVE_NATIVE_SPINLOCKS 1
#define ETHR_HAVE_OPTIMIZED_SPINLOCKS 1

typedef ethr_native_atomic_t ethr_native_spinlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_spinlock_init(ethr_native_spinlock_t *lock)
{
    ethr_native_atomic_init((ethr_native_atomic_t *) lock, 0);
}

static ETHR_INLINE void
ethr_native_spin_unlock(ethr_native_spinlock_t *lock)
{
    ETHR_COMPILER_BARRIER;
    ETHR_ASSERT(ethr_native_atomic_read((ethr_native_atomic_t *) lock) == 1);
    ethr_native_atomic_set_relb((ethr_native_atomic_t *) lock, 0);
}

static ETHR_INLINE void
ethr_native_spin_lock(ethr_native_spinlock_t *lock)
{
    while (ethr_native_atomic_cmpxchg_acqb((ethr_native_atomic_t *) lock,
					   (long) 1, (long) 0) != 0) {
	ETHR_SPIN_BODY;
    }
    ETHR_COMPILER_BARRIER;
}

#endif

#endif


#ifdef ETHR_HAVE_NATIVE_RWSPINLOCKS
#define ETHR_HAVE_OPTIMIZED_RWSPINLOCKS 1
#elif defined(ETHR_HAVE_NATIVE_ATOMICS)
/* --- Native rwspinlocks using native atomics ------------------------------ */
#define ETHR_HAVE_NATIVE_RWSPINLOCKS 1
#define ETHR_HAVE_OPTIMIZED_RWSPINLOCKS 1

typedef ethr_native_atomic_t ethr_native_rwlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

#define ETHR_WLOCK_FLAG__ (((long) 1) << 30)

static ETHR_INLINE void
ethr_native_rwlock_init(ethr_native_rwlock_t *lock)
{
    ethr_native_atomic_init((ethr_native_atomic_t *) lock, 0);
}

static ETHR_INLINE void
ethr_native_read_unlock(ethr_native_rwlock_t *lock)
{
    ETHR_COMPILER_BARRIER;
#ifdef DEBUG
    ETHR_ASSERT(ethr_native_atomic_read((ethr_native_atomic_t *) lock) >= 0);
#endif
    ethr_native_atomic_dec_relb((ethr_native_atomic_t *) lock);
}

static ETHR_INLINE void
ethr_native_read_lock(ethr_native_rwlock_t *lock)
{
    long act, exp = 0;
    while (1) {
	act = ethr_native_atomic_cmpxchg_acqb((ethr_native_atomic_t *) lock,
					      exp+1, exp);
	if (act == exp)
	    break;
	ETHR_SPIN_BODY;
	exp = (act & ETHR_WLOCK_FLAG__) ? 0 : act;
    }
    ETHR_COMPILER_BARRIER;
}

static ETHR_INLINE void
ethr_native_write_unlock(ethr_native_rwlock_t *lock)
{
    ETHR_COMPILER_BARRIER;
    ETHR_ASSERT(ethr_native_atomic_read((ethr_native_atomic_t *) lock)
		== ETHR_WLOCK_FLAG__);
    ethr_native_atomic_set_relb((ethr_native_atomic_t *) lock, 0);
}

static ETHR_INLINE void
ethr_native_write_lock(ethr_native_rwlock_t *lock)
{
    long act, exp = 0;
    while (1) {
	act = ethr_native_atomic_cmpxchg_acqb((ethr_native_atomic_t *) lock,
					      exp|ETHR_WLOCK_FLAG__, exp);
	if (act == exp)
	    break;
	ETHR_SPIN_BODY;
	exp = act & ~ETHR_WLOCK_FLAG__;
    }
    act |= ETHR_WLOCK_FLAG__;
    /* Wait for readers to leave */
    while (act != ETHR_WLOCK_FLAG__) {
	ETHR_SPIN_BODY;
	act = ethr_native_atomic_read_acqb((ethr_native_atomic_t *) lock);
    }
    ETHR_COMPILER_BARRIER;
}

#endif

#endif

#endif
