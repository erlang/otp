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
 * Description: The ethread atomic API
 * Author: Rickard Green
 */

#ifndef ETHR_ATOMIC_H__
#define ETHR_ATOMIC_H__

#if !defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)
#  define ETHR_NEED_ATOMIC_PROTOTYPES__
#endif

#ifdef ETHR_HAVE_NATIVE_ATOMICS
/*
 * Map ethread native atomics to ethread API atomics.
 */
typedef ethr_native_atomic_t ethr_atomic_t;
#else
typedef ethr_sint_t ethr_atomic_t;
#endif

#ifdef ETHR_NEED_ATOMIC_PROTOTYPES__
void ethr_atomic_init(ethr_atomic_t *, ethr_sint_t);
void ethr_atomic_set(ethr_atomic_t *, ethr_sint_t);
ethr_sint_t ethr_atomic_read(ethr_atomic_t *);
ethr_sint_t ethr_atomic_inc_read(ethr_atomic_t *);
ethr_sint_t ethr_atomic_dec_read(ethr_atomic_t *);
void ethr_atomic_inc(ethr_atomic_t *);
void ethr_atomic_dec(ethr_atomic_t *);
ethr_sint_t ethr_atomic_add_read(ethr_atomic_t *, ethr_sint_t);
void ethr_atomic_add(ethr_atomic_t *, ethr_sint_t);
ethr_sint_t ethr_atomic_read_band(ethr_atomic_t *, ethr_sint_t);
ethr_sint_t ethr_atomic_read_bor(ethr_atomic_t *, ethr_sint_t);
ethr_sint_t ethr_atomic_xchg(ethr_atomic_t *, ethr_sint_t);
ethr_sint_t ethr_atomic_cmpxchg(ethr_atomic_t *, ethr_sint_t, ethr_sint_t);
ethr_sint_t ethr_atomic_read_acqb(ethr_atomic_t *);
ethr_sint_t ethr_atomic_inc_read_acqb(ethr_atomic_t *);
void ethr_atomic_set_relb(ethr_atomic_t *, ethr_sint_t);
void ethr_atomic_dec_relb(ethr_atomic_t *);
ethr_sint_t ethr_atomic_dec_read_relb(ethr_atomic_t *);
ethr_sint_t ethr_atomic_cmpxchg_acqb(ethr_atomic_t *, ethr_sint_t, ethr_sint_t);
ethr_sint_t ethr_atomic_cmpxchg_relb(ethr_atomic_t *, ethr_sint_t, ethr_sint_t);
#endif

int ethr_init_atomics(void);

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#ifndef ETHR_HAVE_NATIVE_ATOMICS
/*
 * Fallbacks for atomics used in absence of a native implementation.
 */

#define ETHR_ATOMIC_ADDR_BITS 10
#define ETHR_ATOMIC_ADDR_SHIFT 6

typedef struct {
    union {
	ethr_spinlock_t lck;
	char buf[ETHR_CACHE_LINE_SIZE];
    } u;
} ethr_atomic_protection_t;

extern ethr_atomic_protection_t ethr_atomic_protection__[1 << ETHR_ATOMIC_ADDR_BITS];

#define ETHR_ATOMIC_PTR2LCK__(PTR) \
(&ethr_atomic_protection__[((((unsigned long) (PTR)) >> ETHR_ATOMIC_ADDR_SHIFT) \
			& ((1 << ETHR_ATOMIC_ADDR_BITS) - 1))].u.lck)


#define ETHR_ATOMIC_OP_FALLBACK_IMPL__(AP, EXPS)			\
do {									\
    ethr_spinlock_t *slp__ = ETHR_ATOMIC_PTR2LCK__((AP));		\
    ethr_spin_lock(slp__);						\
    { EXPS; }								\
    ethr_spin_unlock(slp__);						\
} while (0)

#endif

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_init)(ethr_atomic_t *var, ethr_sint_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ethr_native_atomic_init(var, i);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = i);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_set)(ethr_atomic_t *var, ethr_sint_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ethr_native_atomic_set(var, i);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = i);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_read(var);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var);
    return res;
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_add)(ethr_atomic_t *var, ethr_sint_t incr)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ethr_native_atomic_add(var, incr);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var += incr);
#endif
}   
    
static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_add_read)(ethr_atomic_t *var, ethr_sint_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_add_return(var, i);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var += i; res = *var);
    return res;
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ethr_native_atomic_inc(var);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, ++(*var));
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ethr_native_atomic_dec(var);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, --(*var));
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc_read)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_inc_return(var);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = ++(*var));
    return res;
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec_read)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_dec_return(var);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = --(*var));
    return res;
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read_band)(ethr_atomic_t *var,
					      ethr_sint_t mask)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_and_retold(var, mask);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var; *var &= mask);
    return res;
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read_bor)(ethr_atomic_t *var,
					     ethr_sint_t mask)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_or_retold(var, mask);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var; *var |= mask);
    return res;
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_xchg)(ethr_atomic_t *var, ethr_sint_t new)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_xchg(var, new);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var; *var = new);
    return res;
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg)(ethr_atomic_t *var,
					    ethr_sint_t new,
                                            ethr_sint_t exp)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_cmpxchg(var, new, exp);
#else
    ethr_sint_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var,
    {
	res = *var;
	if (__builtin_expect(res == exp, 1))
	    *var = new;
    });
    return res;
#endif
}

/*
 * Important memory barrier requirements.
 *
 * The following atomic operations *must* supply a memory barrier of
 * at least the type specified by its suffix:
 *    _acqb = acquire barrier
 *    _relb = release barrier
 */

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read_acqb)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_read_acqb(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_read)(var);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc_read_acqb)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_inc_return_acqb(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc_read)(var);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_set_relb)(ethr_atomic_t *var,
					     ethr_sint_t val)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ethr_native_atomic_set_relb(var, val);
#else
    ETHR_INLINE_FUNC_NAME_(ethr_atomic_set)(var, val);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec_relb)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ethr_native_atomic_dec_relb(var);
#else
    ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec)(var);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec_read_relb)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_dec_return_relb(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec_read)(var);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg_acqb)(ethr_atomic_t *var,
						 ethr_sint_t new,
						 ethr_sint_t exp)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_cmpxchg_acqb(var, new, exp);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg)(var, new, exp);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg_relb)(ethr_atomic_t *var,
						 ethr_sint_t new,
						 ethr_sint_t exp)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic_cmpxchg_relb(var, new, exp);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg)(var, new, exp);
#endif
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif
