/* 
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2011. All Rights Reserved.
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
 * Native ethread rwlocks on SPARC V9.
 * Author: Mikael Pettersson.
 */
#ifndef ETHREAD_SPARC32_RWLOCK_H
#define ETHREAD_SPARC32_RWLOCK_H

#define ETHR_HAVE_NATIVE_RWSPINLOCKS 1
#define ETHR_NATIVE_RWSPINLOCK_IMPL "ethread"

/* Unlocked if zero, read-locked if positive, write-locked if -1. */
typedef struct {
    volatile int lock;
} ethr_native_rwlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_rwlock_init(ethr_native_rwlock_t *lock)
{
    lock->lock = 0;
}

static ETHR_INLINE void
ethr_native_read_unlock(ethr_native_rwlock_t *lock)
{
    unsigned int old, new;

    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_StoreLoad);
    do {
	old = lock->lock;
	new = old-1;
	__asm__ __volatile__(
	    "cas [%2], %1, %0"
	    : "=&r"(new)
	    : "r"(old), "r"(&lock->lock), "0"(new)
	    : "memory");
    } while (__builtin_expect(old != new, 0));
}

static ETHR_INLINE int
ethr_native_read_trylock(ethr_native_rwlock_t *lock)
{
    int old, new;

    do {
	old = lock->lock;
	if (__builtin_expect(old < 0, 0))
	    return 0;
	new = old+1;
	__asm__ __volatile__(
	    "cas [%2], %1, %0"
	    : "=&r"(new)
	    : "r"(old), "r"(&lock->lock), "0"(new)
	    : "memory");
    } while (__builtin_expect(old != new, 0));
    ETHR_MEMBAR(ETHR_StoreLoad|ETHR_StoreStore);
    return 1;
}

static ETHR_INLINE int
ethr_native_read_is_locked(ethr_native_rwlock_t *lock)
{
    return lock->lock < 0;
}

static ETHR_INLINE void
ethr_native_read_lock(ethr_native_rwlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_read_trylock(lock) != 0, 1))
	    break;
	do {
	    ETHR_MEMBAR(ETHR_LoadLoad);
	} while (ethr_native_read_is_locked(lock));
   }
}

static ETHR_INLINE void
ethr_native_write_unlock(ethr_native_rwlock_t *lock)
{
    ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);
    lock->lock = 0;
}

static ETHR_INLINE int
ethr_native_write_trylock(ethr_native_rwlock_t *lock)
{
    unsigned int old, new;

    do {
	old = lock->lock;
	if (__builtin_expect(old != 0, 0))
	    return 0;
	new = -1;
	__asm__ __volatile__(
	    "cas [%2], %1, %0"
	    : "=&r"(new)
	    : "r"(old), "r"(&lock->lock), "0"(new)
	    : "memory");
    } while (__builtin_expect(old != new, 0));
    ETHR_MEMBAR(ETHR_StoreLoad|ETHR_StoreStore);
    return 1;
}

static ETHR_INLINE int
ethr_native_write_is_locked(ethr_native_rwlock_t *lock)
{
    return lock->lock != 0;
}

static ETHR_INLINE void
ethr_native_write_lock(ethr_native_rwlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_write_trylock(lock) != 0, 1))
	    break;
	do {
	    ETHR_MEMBAR(ETHR_LoadLoad);
	} while (ethr_native_write_is_locked(lock));
   }
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_SPARC32_RWLOCK_H */
