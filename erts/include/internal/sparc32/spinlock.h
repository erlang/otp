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
 * Native ethread spinlocks on SPARC V9.
 * Author: Mikael Pettersson.
 */
#ifndef ETHR_SPARC32_SPINLOCK_H
#define ETHR_SPARC32_SPINLOCK_H

#define ETHR_HAVE_NATIVE_SPINLOCKS 1
#define ETHR_NATIVE_SPINLOCK_IMPL "ethread"

/* Locked with ldstub, so unlocked when 0 and locked when non-zero. */
typedef struct {
    volatile unsigned char lock;
} ethr_native_spinlock_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_spinlock_init(ethr_native_spinlock_t *lock)
{
    lock->lock = 0;
}

static ETHR_INLINE void
ethr_native_spin_unlock(ethr_native_spinlock_t *lock)
{
    ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);
    lock->lock = 0;
}

static ETHR_INLINE int
ethr_native_spin_trylock(ethr_native_spinlock_t *lock)
{
    unsigned int prev;

    __asm__ __volatile__(
	"ldstub [%1], %0\n\t"
	: "=r"(prev)
	: "r"(&lock->lock)
	: "memory");
    ETHR_MEMBAR(ETHR_StoreLoad|ETHR_StoreStore);
    return prev == 0;
}

static ETHR_INLINE int
ethr_native_spin_is_locked(ethr_native_spinlock_t *lock)
{
    return lock->lock != 0;
}

static ETHR_INLINE void
ethr_native_spin_lock(ethr_native_spinlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_spin_trylock(lock) != 0, 1))
	    break;
	do {
	    ETHR_MEMBAR(ETHR_LoadLoad);
	} while (ethr_native_spin_is_locked(lock));
    }
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHR_SPARC32_SPINLOCK_H */
