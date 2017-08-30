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
 * Native ethread rwlocks on x86/x86-64.
 * Author: Mikael Pettersson.
 *
 * This code requires a 486 or newer processor.
 */
#ifndef ETHREAD_I386_RWLOCK_H
#define ETHREAD_I386_RWLOCK_H

#define ETHR_HAVE_NATIVE_RWSPINLOCKS 1
#define ETHR_NATIVE_RWSPINLOCK_IMPL "ethread"

/* XXX: describe the algorithm */
typedef struct {
    volatile int lock;
} ethr_native_rwlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

#define ETHR_RWLOCK_OFFSET	(1<<24)

static ETHR_INLINE void
ethr_native_rwlock_init(ethr_native_rwlock_t *lock)
{
    lock->lock = 0;
}

static ETHR_INLINE void
ethr_native_read_unlock(ethr_native_rwlock_t *lock)
{
    __asm__ __volatile__(
	"lock; decl %0"
	: "=m"(lock->lock)
	: "m"(lock->lock));
}

static ETHR_INLINE int
ethr_native_read_trylock(ethr_native_rwlock_t *lock)
{
    int tmp;

    tmp = 1;
    __asm__ __volatile__(
	"lock; xaddl %0, %1"
	: "=r"(tmp)
	: "m"(lock->lock), "0"(tmp));
    /* tmp is now the lock's previous value */
    if (__builtin_expect(tmp >= 0, 1))
	return 1;
    ethr_native_read_unlock(lock);
    return 0;
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
	    __asm__ __volatile__("rep;nop" : "=m"(lock->lock) : : "memory");
	} while (ethr_native_read_is_locked(lock));
    }
}

static ETHR_INLINE void
ethr_native_write_unlock(ethr_native_rwlock_t *lock)
{
    __asm__ __volatile__(
	"lock; addl %2,%0"
	: "=m"(lock->lock)
	: "m"(lock->lock), "i"(ETHR_RWLOCK_OFFSET));
}

static ETHR_INLINE int
ethr_native_write_trylock(ethr_native_rwlock_t *lock)
{
    int tmp;

    tmp = -ETHR_RWLOCK_OFFSET;
    __asm__ __volatile__(
	"lock; xaddl %0, %1"
	: "=r"(tmp)
	: "m"(lock->lock), "0"(tmp));
    /* tmp is now the lock's previous value */
    if (__builtin_expect(tmp == 0, 1))
	return 1;
    ethr_native_write_unlock(lock);
    return 0;
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
	    __asm__ __volatile__("rep;nop" : "=m"(lock->lock) : : "memory");
	} while (ethr_native_write_is_locked(lock));
    }
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_I386_RWLOCK_H */
