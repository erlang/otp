/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
 * Native ethread atomics on x86/x86-64.
 * Author: Mikael Pettersson.
 *
 * This code requires a 486 or newer processor.
 */
#ifndef ETHREAD_I386_ATOMIC_H
#define ETHREAD_I386_ATOMIC_H

/* An atomic is an aligned long accessed via locked operations.
 */
typedef struct {
    volatile long counter;
} ethr_native_atomic_t;

#ifdef ETHR_TRY_INLINE_FUNCS

#ifdef __x86_64__
#define LONG_SUFFIX "q"
#else
#define LONG_SUFFIX "l"
#endif

static ETHR_INLINE void
ethr_native_atomic_init(ethr_native_atomic_t *var, long i)
{
    var->counter = i;
}
#define ethr_native_atomic_set(v, i)	ethr_native_atomic_init((v), (i))

static ETHR_INLINE long
ethr_native_atomic_read(ethr_native_atomic_t *var)
{
    return var->counter;
}

static ETHR_INLINE void
ethr_native_atomic_add(ethr_native_atomic_t *var, long incr)
{
    __asm__ __volatile__(
       "lock; add" LONG_SUFFIX " %1, %0"
       : "=m"(var->counter)
       : "ir"(incr), "m"(var->counter));
}      
       
static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    __asm__ __volatile__(
	"lock; inc" LONG_SUFFIX " %0"
	: "=m"(var->counter)
	: "m"(var->counter));
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    __asm__ __volatile__(
	"lock; dec" LONG_SUFFIX " %0"
	: "=m"(var->counter)
	: "m"(var->counter));
}

static ETHR_INLINE long
ethr_native_atomic_add_return(ethr_native_atomic_t *var, long incr)
{
    long tmp;

    tmp = incr;
    __asm__ __volatile__(
	"lock; xadd" LONG_SUFFIX " %0, %1" /* xadd didn't exist prior to the 486 */
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp));
    /* now tmp is the atomic's previous value */
    return tmp + incr;
}

#define ethr_native_atomic_inc_return(var) ethr_native_atomic_add_return((var), 1)
#define ethr_native_atomic_dec_return(var) ethr_native_atomic_add_return((var), -1)

static ETHR_INLINE long
ethr_native_atomic_cmpxchg(ethr_native_atomic_t *var, long new, long old)
{
    __asm__ __volatile__(
      "lock; cmpxchg" LONG_SUFFIX " %2, %3"
      : "=a"(old), "=m"(var->counter)
      : "r"(new), "m"(var->counter), "0"(old)
      : "cc", "memory"); /* full memory clobber to make this a compiler barrier */
    return old;
}

static ETHR_INLINE long
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, long mask)
{
    long tmp, old;

    tmp = var->counter;
    do {
	old = tmp;
        tmp = ethr_native_atomic_cmpxchg(var, tmp & mask, tmp);
    } while (__builtin_expect(tmp != old, 0));
    /* now tmp is the atomic's previous value */
    return tmp;
}

static ETHR_INLINE long
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, long mask)
{
    long tmp, old;

    tmp = var->counter;
    do {
	old = tmp;
        tmp = ethr_native_atomic_cmpxchg(var, tmp | mask, tmp);
    } while (__builtin_expect(tmp != old, 0));
    /* now tmp is the atomic's previous value */
    return tmp;
}

static ETHR_INLINE long
ethr_native_atomic_xchg(ethr_native_atomic_t *var, long val)
{   
    long tmp = val;
    __asm__ __volatile__(
	"xchg" LONG_SUFFIX " %0, %1"
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp));
    /* now tmp is the atomic's previous value */ 
    return tmp;
} 

#undef LONG_SUFFIX

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_I386_ATOMIC_H */
