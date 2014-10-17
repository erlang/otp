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
 * Native ethread atomics on PowerPC.
 * Author: Mikael Pettersson.
 *
 * Based on the examples in Appendix E of Motorola's
 * "Programming Environments Manual For 32-Bit Implementations
 * of the PowerPC Architecture".
 */
#ifndef ETHREAD_PPC_ATOMIC_H
#define ETHREAD_PPC_ATOMIC_H

#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL "ethread"

typedef struct {
    volatile ethr_sint32_t counter;
} ethr_native_atomic32_t;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADDR 1

static ETHR_INLINE ethr_sint32_t *
ethr_native_atomic32_addr(ethr_native_atomic32_t *var)
{
    return (ethr_sint32_t *) &var->counter;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET 1

static ETHR_INLINE void
ethr_native_atomic32_set(ethr_native_atomic32_t *var, ethr_sint32_t i)
{
    var->counter = i;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_read(ethr_native_atomic32_t *var)
{
    return var->counter;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_add_return(ethr_native_atomic32_t *var, ethr_sint32_t incr)
{
    ethr_sint32_t tmp;

    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"add	%0,%2,%0\n\t"
	"stwcx.	%0,0,%1\n\t"
	"bne-	1b\n\t"
	: "=&r"(tmp)
	: "r"(&var->counter), "r"(incr)
	: "cc", "memory");
    return tmp;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_add_return_acqb(ethr_native_atomic32_t *var, ethr_sint32_t incr)
{
    ethr_sint32_t res;
    res = ethr_native_atomic32_add_return(var, incr);
    __asm__ __volatile("isync\n\t" : : : "memory");
    return res;
}


#ifndef ETHR_PPC_HAVE_NO_LWSYNC

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_RELB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_add_return_relb(ethr_native_atomic32_t *var, ethr_sint32_t incr)
{
    ethr_lwsync__();
    return ethr_native_atomic32_add_return(var, incr);
}

#endif

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_inc_return(ethr_native_atomic32_t *var)
{
    ethr_sint32_t tmp;

    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"addic	%0,%0,1\n\t" /* due to addi's (rA|0) behaviour */
	"stwcx.	%0,0,%1\n\t"
	"bne-	1b\n\t"
	: "=&r"(tmp)
	: "r"(&var->counter)
	: "cc", "memory");
    return tmp;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_inc_return_acqb(ethr_native_atomic32_t *var)
{
    ethr_sint32_t res;
    res = ethr_native_atomic32_inc_return(var);
    __asm__ __volatile("isync\n\t" : : : "memory");
    return res;
}

#ifndef ETHR_PPC_HAVE_NO_LWSYNC

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN_RELB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_inc_return_relb(ethr_native_atomic32_t *var)
{
    ethr_lwsync__();
    return ethr_native_atomic32_inc_return(var);
}

#endif

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_dec_return(ethr_native_atomic32_t *var)
{
    ethr_sint32_t tmp;

    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"addic	%0,%0,-1\n\t"
	"stwcx.	%0,0,%1\n\t"
	"bne-	1b\n\t"
	: "=&r"(tmp)
	: "r"(&var->counter)
	: "cc", "memory");
    return tmp;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_dec_return_acqb(ethr_native_atomic32_t *var)
{
    ethr_sint32_t res;
    res = ethr_native_atomic32_dec_return(var);
    __asm__ __volatile("isync\n\t" : : : "memory");
    return res;
}

#ifndef ETHR_PPC_HAVE_NO_LWSYNC

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN_RELB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_dec_return_relb(ethr_native_atomic32_t *var)
{
    ethr_lwsync__();
    return ethr_native_atomic32_dec_return(var);
}

#endif

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_and_retold(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_sint32_t old, new;

    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%2\n\t"
	"and	%1,%0,%3\n\t"
	"stwcx.	%1,0,%2\n\t"
	"bne-	1b\n\t"
	: "=&r"(old), "=&r"(new)
	: "r"(&var->counter), "r"(mask)
	: "cc", "memory");
    return old;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_and_retold_acqb(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_sint32_t res;
    res = ethr_native_atomic32_and_retold(var, mask);
    __asm__ __volatile("isync\n\t" : : : "memory");
    return res;
}

#ifndef ETHR_PPC_HAVE_NO_LWSYNC

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD_RELB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_and_retold_relb(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_lwsync__();
    return ethr_native_atomic32_and_retold(var, mask);
}

#endif

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_or_retold(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_sint32_t old, new;

    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%2\n\t"
	"or	%1,%0,%3\n\t"
	"stwcx.	%1,0,%2\n\t"
	"bne-	1b\n\t"
	: "=&r"(old), "=&r"(new)
	: "r"(&var->counter), "r"(mask)
	: "cc", "memory");
    return old;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_or_retold_acqb(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_sint32_t res;
    res = ethr_native_atomic32_or_retold(var, mask);
    __asm__ __volatile("isync\n\t" : : : "memory");
    return res;
}

#ifndef ETHR_PPC_HAVE_NO_LWSYNC

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD_RELB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_or_retold_relb(ethr_native_atomic32_t *var, ethr_sint32_t mask)
{
    ethr_lwsync__();
    return ethr_native_atomic32_or_retold(var, mask);
}

#endif

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_XCHG 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_xchg(ethr_native_atomic32_t *var, ethr_sint32_t val)
{
    ethr_sint32_t tmp;

    __asm__ __volatile__(
	"1:\t"
	"lwarx	%0,0,%1\n\t"
	"stwcx.	%2,0,%1\n\t"
	"bne-	1b\n\t"
	: "=&r"(tmp)
	: "r"(&var->counter), "r"(val)
	: "cc", "memory");
    return tmp;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_XCHG_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_xchg_acqb(ethr_native_atomic32_t *var, ethr_sint32_t val)
{
    ethr_sint32_t res;
    res = ethr_native_atomic32_xchg(var, val);
    __asm__ __volatile("isync\n\t" : : : "memory");
    return res;
}

#ifndef ETHR_PPC_HAVE_NO_LWSYNC

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_XCHG_RELB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_xchg_relb(ethr_native_atomic32_t *var, ethr_sint32_t val)
{
    ethr_lwsync__();
    return ethr_native_atomic32_xchg(var, val);
}

#endif

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_cmpxchg(ethr_native_atomic32_t *var,
			     ethr_sint32_t new,
			     ethr_sint32_t expected)
{
  ethr_sint32_t old;

  __asm__ __volatile__(
    "1:\t"
    "lwarx	%0,0,%2\n\t"
    "cmpw	0,%0,%3\n\t"
    "bne	2f\n\t"
    "stwcx.	%1,0,%2\n\t"
    "bne-	1b\n\t"
    "2:"
    : "=&r"(old)
    : "r"(new), "r"(&var->counter), "r"(expected)
    : "cc", "memory");

    return old;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_ACQB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_cmpxchg_acqb(ethr_native_atomic32_t *var,
				  ethr_sint32_t new,
				  ethr_sint32_t expected)
{
  ethr_sint32_t old;

  __asm__ __volatile__(
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

#if !defined(ETHR_DISABLE_LWSYNC_FOR_CMPXCHG_RELB) && !defined(ETHR_PPC_HAVE_NO_LWSYNC)

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_RELB 1

static ETHR_INLINE ethr_sint32_t
ethr_native_atomic32_cmpxchg_relb(ethr_native_atomic32_t *var,
				  ethr_sint32_t new,
				  ethr_sint32_t expected)
{
    ethr_sint32_t actual;

    /*
     * We want to implement the release barrier using the
     * 'lwsync' instruction instead of using the more
     * expensive 'sync' instruction.
     *
     * cmpxchg looks something like this:
     *
     *   lwarx # Load
     *   ...
     *   if (fail)
     *      goto done;
     *   stwcx # Store
     *   if (fail)
     *      goto done;
     *   ...
     *
     * In the case we succeeded, 'lwsync' will have
     * ordered all previously issued loads and stores
     * against the successful store to this variable.
     * That is everything is fine!
     *
     * In the case we did not succeed, we need to order
     * all previously issued loads and stores against
     * the load of this variable. 'lwsync' does not
     * guarantee this. In order to solve this we issue
     * a 'sync' and redo the load. If the value has
     * changed to what the user passed as expected value
     * we need to try the cmpxchg operation again, since
     * this value indicates success.
     */

    ethr_lwsync__();

    actual = ethr_native_atomic32_cmpxchg(var, new, expected);

#ifndef ETHR_PPC_HAVE_LWSYNC
    /* We checked for lwsync support in runtime... */
    if (ETHR_PPC_RUNTIME_CONF_HAVE_NO_LWSYNC__)
	return actual; /* No need to; ethr_lwsync__() issued a sync... */
#endif

    /* ethr_lwsync__() issued an lwsync... */
    if (actual == expected)
	return actual; /* Successful operation */

    /* Failure... need to issue a sync... */
    ethr_sync__();
    actual = ethr_native_atomic32_read(var);
    if (actual != expected)
	return actual; /* Fail... */
    /* Try again... */
    return ethr_native_atomic32_cmpxchg(var, new, expected);
}

#endif

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_PPC_ATOMIC_H */
