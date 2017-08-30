/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
 * Native ethread rwlocks on PowerPC.
 * Author: Mikael Pettersson.
 *
 * Based on the examples in Appendix E of Motorola's
 * "Programming Environments Manual For 32-Bit Implementations
 * of the PowerPC Architecture".
 */
#ifndef ETHREAD_PPC_RWLOCK_H
#define ETHREAD_PPC_RWLOCK_H

#define ETHR_HAVE_NATIVE_RWSPINLOCKS 1
#define ETHR_NATIVE_RWSPINLOCK_IMPL "ethread"

/* Unlocked if zero, read-locked if negative, write-locked if +1. */
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
    int tmp;

    ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);

    /* this is ethr_native_atomic_inc() - isync */
    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"addic	%0,%0,1\n\t"
	"stwcx.	%0,0,%1\n\t"
	"bne-	1b"
	: "=&r"(tmp)
	: "r"(&lock->lock)
	: "cr0", "memory");
}

static ETHR_INLINE int
ethr_native_read_trylock(ethr_native_rwlock_t *lock)
{
    int counter;

    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%1\n\t"	/* read lock to counter */
	"addic.	%0,%0,-1\n\t"	/* decrement counter */
	"bge-	2f\n\t"		/* bail if >= 0 (write-locked) */
	"stwcx.	%0,0,%1\n\t"	/* try to store decremented counter */
	"bne-	1b\n\t"		/* loop if lost reservation */
	"isync\n\t"		/* wait for previous insns to complete */
	"2:"
	: "=&r"(counter)
	: "r"(&lock->lock)
	: "cr0", "memory"
#if __GNUC__ > 2
	,"xer"
#endif
	);
    return counter < 0;
}

static ETHR_INLINE int
ethr_native_read_is_locked(ethr_native_rwlock_t *lock)
{
    return lock->lock > 0;
}

static ETHR_INLINE void
ethr_native_read_lock(ethr_native_rwlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_read_trylock(lock) != 0, 1))
	    break;
	do {
	    __asm__ __volatile__("":::"memory");
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
    int prev;

    /* identical to ethr_native_spin_trylock() */
    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%1\n\t"	/* read lock to prev */
	"cmpwi	0,%0,0\n\t"
	"bne-	2f\n\t"		/* bail if non-zero (any lock) */
	"stwcx.	%2,0,%1\n\t"	/* try to make the lock positive */
	"bne-	1b\n\t"		/* loop if lost reservation */
	"isync\n\t"		/* wait for previous insns to complete */
	"2:"
	: "=&r"(prev)
	: "r"(&lock->lock), "r"(1)
	: "cr0", "memory");
    return prev == 0;
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
	    __asm__ __volatile__("":::"memory");
	} while (ethr_native_write_is_locked(lock));
    }
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_PPC_RWLOCK_H */
