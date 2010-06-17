/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2010. All Rights Reserved.
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
 * Native ethread atomics on PowerPC.
 * Author: Mikael Pettersson.
 *
 * Based on the examples in Appendix E of Motorola's
 * "Programming Environments Manual For 32-Bit Implementations
 * of the PowerPC Architecture".
 */
#ifndef ETHREAD_PPC_ATOMIC_H
#define ETHREAD_PPC_ATOMIC_H

typedef struct {
    volatile int counter;
} ethr_native_atomic_t;

#define ETHR_MEMORY_BARRIER __asm__ __volatile__("sync" : : : "memory")

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_AUX_IMPL__)

static ETHR_INLINE void
ethr_native_atomic_init(ethr_native_atomic_t *var, int i)
{
    var->counter = i;
}
#define ethr_native_atomic_set(v, i)	ethr_native_atomic_init((v), (i))

static ETHR_INLINE int
ethr_native_atomic_read(ethr_native_atomic_t *var)
{
    return var->counter;
}

static ETHR_INLINE int
ethr_native_atomic_add_return(ethr_native_atomic_t *var, int incr)
{
    int tmp;

    __asm__ __volatile__(
	"eieio\n\t"
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"add	%0,%2,%0\n\t"
	"stwcx.	%0,0,%1\n\t"
	"bne-	1b\n\t"
	"isync"
	: "=&r"(tmp)
	: "r"(&var->counter), "r"(incr)
	: "cc", "memory");
    return tmp;
}

static ETHR_INLINE void
ethr_native_atomic_add(ethr_native_atomic_t *var, int incr)
{
    /* XXX: could use weaker version here w/o eieio+isync */
    (void)ethr_native_atomic_add_return(var, incr);
}

static ETHR_INLINE int
ethr_native_atomic_inc_return(ethr_native_atomic_t *var)
{
    int tmp;

    __asm__ __volatile__(
	"eieio\n\t"
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"addic	%0,%0,1\n\t" /* due to addi's (rA|0) behaviour */
	"stwcx.	%0,0,%1\n\t"
	"bne-	1b\n\t"
	"isync"
	: "=&r"(tmp)
	: "r"(&var->counter)
	: "cc", "memory");
    return tmp;
}

static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    /* XXX: could use weaker version here w/o eieio+isync */
    (void)ethr_native_atomic_inc_return(var);
}

static ETHR_INLINE int
ethr_native_atomic_dec_return(ethr_native_atomic_t *var)
{
    int tmp;

    __asm__ __volatile__(
	"eieio\n\t"
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"addic	%0,%0,-1\n\t"
	"stwcx.	%0,0,%1\n\t"
	"bne-	1b\n\t"
	"isync"
	: "=&r"(tmp)
	: "r"(&var->counter)
	: "cc", "memory");
    return tmp;
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    /* XXX: could use weaker version here w/o eieio+isync */
    (void)ethr_native_atomic_dec_return(var);
}

static ETHR_INLINE int
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, int mask)
{
    int old, new;

    __asm__ __volatile__(
	"eieio\n\t"
	"1:\t"
	"lwarx	%0,0,%2\n\t"
	"and	%1,%0,%3\n\t"
	"stwcx.	%1,0,%2\n\t"
	"bne-	1b\n\t"
	"isync"
	: "=&r"(old), "=&r"(new)
	: "r"(&var->counter), "r"(mask)
	: "cc", "memory");
    return old;
}

static ETHR_INLINE int
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, int mask)
{
    int old, new;

    __asm__ __volatile__(
	"eieio\n\t"
	"1:\t"
	"lwarx	%0,0,%2\n\t"
	"or	%1,%0,%3\n\t"
	"stwcx.	%1,0,%2\n\t"
	"bne-	1b\n\t"
	"isync"
	: "=&r"(old), "=&r"(new)
	: "r"(&var->counter), "r"(mask)
	: "cc", "memory");
    return old;
}

static ETHR_INLINE int
ethr_native_atomic_xchg(ethr_native_atomic_t *var, int val)
{
    int tmp;

    __asm__ __volatile__(
	"eieio\n\t"
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"stwcx.	%2,0,%1\n\t"
	"bne-	1b\n\t"
	"isync"
	: "=&r"(tmp)
	: "r"(&var->counter), "r"(val)
	: "cc", "memory");
    return tmp;
}

static ETHR_INLINE int
ethr_native_atomic_cmpxchg(ethr_native_atomic_t *var, int new, int expected)
{
  int old;

  __asm__ __volatile__(
    "eieio\n\t"
    "1:\t"
    "lwarx	%0,0,%2\n\t"
    "cmpw	0,%0,%3\n\t"
    "bne	2f\n\t"
    "stwcx.	%1,0,%2\n\t"
    "bne-	1b\n\t"
    "isync\n"
    "2:"
    : "=&r"(old)
    : "r"(new), "r"(&var->counter), "r"(expected)
    : "cc", "memory");

    return old;
}

/*
 * Atomic ops with at least specified barriers.
 */

static ETHR_INLINE long
ethr_native_atomic_read_acqb(ethr_native_atomic_t *var)
{
    long res = ethr_native_atomic_read(var);
    ETHR_MEMORY_BARRIER;
    return res;
}

#define ethr_native_atomic_set_relb ethr_native_atomic_xchg
#define ethr_native_atomic_inc_return_acqb ethr_native_atomic_inc_return
#define ethr_native_atomic_dec_relb ethr_native_atomic_dec_return
#define ethr_native_atomic_dec_return_relb ethr_native_atomic_dec_return

#define ethr_native_atomic_cmpxchg_acqb ethr_native_atomic_cmpxchg
#define ethr_native_atomic_cmpxchg_relb ethr_native_atomic_cmpxchg

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_PPC_ATOMIC_H */
