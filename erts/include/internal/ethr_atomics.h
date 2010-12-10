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

#ifndef ETHR_HAVE_NATIVE_ATOMICS
/*
 * No native atomic implementation available. :(
 * Use fallback...
 */
typedef ethr_sint32_t ethr_atomic32_t;
typedef ethr_sint_t ethr_atomic_t;
#else
/*
 * Map ethread native atomics to ethread API atomics.
 *
 * We do at least have a native atomic implementation that
 * can handle integers of a size larger than or equal to
 * the size of pointers.
 */

/* -- Pointer size atomics -- */

#undef ETHR_NAINT_T__
#undef ETHR_NATMC_FUNC__
#undef ETHR_NATMC_ADDR_FUNC__
#if ETHR_SIZEOF_PTR == 8
#  if defined(ETHR_HAVE_NATIVE_ATOMIC64)
#    define ETHR_NATMC_ADDR_FUNC__ ethr_native_atomic64_addr
typedef ethr_native_atomic64_t ethr_atomic_t;
#    define ETHR_NAINT_T__ ethr_sint64_t
#    define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#  else
#    error "Missing native atomic implementation"
#  endif
#elif ETHR_SIZEOF_PTR == 4
#  define ETHR_NATMC_ADDR_FUNC__ ethr_native_atomic32_addr
#  ifdef ETHR_HAVE_NATIVE_ATOMIC32
typedef ethr_native_atomic32_t ethr_atomic_t;
#    define ETHR_NAINT_T__ ethr_sint32_t
#    define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#  elif defined(ETHR_HAVE_NATIVE_ATOMIC64)
typedef ethr_native_atomic64_t ethr_atomic_t;
#    define ETHR_NATMC_T__ ethr_native_atomic64_t
#    define ETHR_NAINT_T__ ethr_sint64_t
#    define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#  else
#    error "Missing native atomic implementation"
#  endif
#endif

/* -- 32-bit atomics -- */

#undef ETHR_NAINT32_T__
#undef ETHR_NATMC32_FUNC__
#if defined(ETHR_HAVE_NATIVE_ATOMIC32)
typedef ethr_native_atomic32_t ethr_atomic32_t;
#  define ETHR_NAINT32_T__ ethr_sint32_t
#  define ETHR_NATMC32_FUNC__(X) ethr_native_atomic32_ ## X
#elif defined(ETHR_HAVE_NATIVE_ATOMIC64)
typedef ethr_native_atomic64_t ethr_atomic32_t;
#  define ETHR_NAINT32_T__ ethr_sint64_t
#  define ETHR_NATMC32_FUNC__(X) ethr_native_atomic64_ ## X
#else
#  error "Missing native atomic implementation"
#endif

#endif

#ifdef ETHR_NEED_ATOMIC_PROTOTYPES__
ethr_sint_t *ethr_atomic_addr(ethr_atomic_t *);
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

ethr_sint32_t *ethr_atomic32_addr(ethr_atomic32_t *);
void ethr_atomic32_init(ethr_atomic32_t *, ethr_sint32_t);
void ethr_atomic32_set(ethr_atomic32_t *, ethr_sint32_t);
ethr_sint32_t ethr_atomic32_read(ethr_atomic32_t *);
ethr_sint32_t ethr_atomic32_inc_read(ethr_atomic32_t *);
ethr_sint32_t ethr_atomic32_dec_read(ethr_atomic32_t *);
void ethr_atomic32_inc(ethr_atomic32_t *);
void ethr_atomic32_dec(ethr_atomic32_t *);
ethr_sint32_t ethr_atomic32_add_read(ethr_atomic32_t *, ethr_sint32_t);
void ethr_atomic32_add(ethr_atomic32_t *, ethr_sint32_t);
ethr_sint32_t ethr_atomic32_read_band(ethr_atomic32_t *, ethr_sint32_t);
ethr_sint32_t ethr_atomic32_read_bor(ethr_atomic32_t *, ethr_sint32_t);
ethr_sint32_t ethr_atomic32_xchg(ethr_atomic32_t *, ethr_sint32_t);
ethr_sint32_t ethr_atomic32_cmpxchg(ethr_atomic32_t *,
				    ethr_sint32_t,
				    ethr_sint32_t);
ethr_sint32_t ethr_atomic32_read_acqb(ethr_atomic32_t *);
ethr_sint32_t ethr_atomic32_inc_read_acqb(ethr_atomic32_t *);
void ethr_atomic32_set_relb(ethr_atomic32_t *, ethr_sint32_t);
void ethr_atomic32_dec_relb(ethr_atomic32_t *);
ethr_sint32_t ethr_atomic32_dec_read_relb(ethr_atomic32_t *);
ethr_sint32_t ethr_atomic32_cmpxchg_acqb(ethr_atomic32_t *,
					 ethr_sint32_t,
					 ethr_sint32_t);
ethr_sint32_t ethr_atomic32_cmpxchg_relb(ethr_atomic32_t *,
					 ethr_sint32_t,
					 ethr_sint32_t);
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
(&ethr_atomic_protection__[((((ethr_uint_t) (PTR)) >> ETHR_ATOMIC_ADDR_SHIFT) \
			& ((1 << ETHR_ATOMIC_ADDR_BITS) - 1))].u.lck)


#define ETHR_ATOMIC_OP_FALLBACK_IMPL__(AP, EXPS)			\
do {									\
    ethr_spinlock_t *slp__ = ETHR_ATOMIC_PTR2LCK__((AP));		\
    ethr_spin_lock(slp__);						\
    { EXPS; }								\
    ethr_spin_unlock(slp__);						\
} while (0)

#endif

/*
 * --- Pointer size atomics ---------------------------------------------------
 */

static ETHR_INLINE ethr_sint_t *
ETHR_INLINE_FUNC_NAME_(ethr_atomic_addr)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint_t *) ETHR_NATMC_ADDR_FUNC__(var);
#else
    return (ethr_sint_t *) var;
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_init)(ethr_atomic_t *var, ethr_sint_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC_FUNC__(init)(var, (ETHR_NAINT_T__) i);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = i);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_set)(ethr_atomic_t *var, ethr_sint_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC_FUNC__(set)(var, (ETHR_NAINT_T__) i);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = i);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_read)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint_t) ETHR_NATMC_FUNC__(read)(var);
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
    ETHR_NATMC_FUNC__(add)(var, (ETHR_NAINT_T__) incr);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var += incr);
#endif
}   
    
static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_add_read)(ethr_atomic_t *var, ethr_sint_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint_t) ETHR_NATMC_FUNC__(add_return)(var, (ETHR_NAINT_T__) i);
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
    ETHR_NATMC_FUNC__(inc)(var);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, ++(*var));
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC_FUNC__(dec)(var);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, --(*var));
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc_read)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint_t) ETHR_NATMC_FUNC__(inc_return)(var);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(dec_return)(var);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(and_retold)(var,
						       (ETHR_NAINT_T__) mask);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(or_retold)(var,
						      (ETHR_NAINT_T__) mask);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(xchg)(var,
						 (ETHR_NAINT_T__) new);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(cmpxchg)(var,
						    (ETHR_NAINT_T__) new,
						    (ETHR_NAINT_T__) exp);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(read_acqb)(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_read)(var);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc_read_acqb)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint_t) ETHR_NATMC_FUNC__(inc_return_acqb)(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_inc_read)(var);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_set_relb)(ethr_atomic_t *var,
					     ethr_sint_t val)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC_FUNC__(set_relb)(var, (ETHR_NAINT_T__) val);
#else
    ETHR_INLINE_FUNC_NAME_(ethr_atomic_set)(var, val);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec_relb)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC_FUNC__(dec_relb)(var);
#else
    ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec)(var);
#endif
}

static ETHR_INLINE ethr_sint_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic_dec_read_relb)(ethr_atomic_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint_t) ETHR_NATMC_FUNC__(dec_return_relb)(var);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(cmpxchg_acqb)(var,
							 (ETHR_NAINT_T__) new,
							 (ETHR_NAINT_T__) exp);
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
    return (ethr_sint_t) ETHR_NATMC_FUNC__(cmpxchg_relb)(var,
							 (ETHR_NAINT_T__) new,
							 (ETHR_NAINT_T__) exp);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic_cmpxchg)(var, new, exp);
#endif
}

/*
 * --- 32-bit atomics ---------------------------------------------------------
 */

static ETHR_INLINE ethr_sint32_t *
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_addr)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return ethr_native_atomic32_addr(var);
#else
    return (ethr_sint32_t *) var;
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_init)(ethr_atomic32_t *var,
					   ethr_sint32_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC32_FUNC__(init)(var, (ETHR_NAINT32_T__) i);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = i);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_set)(ethr_atomic32_t *var, ethr_sint32_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC32_FUNC__(set)(var, (ETHR_NAINT32_T__) i);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = i);
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_read)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(read)(var);
#else
    ethr_sint32_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var);
    return res;
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_add)(ethr_atomic32_t *var,
					  ethr_sint32_t incr)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC32_FUNC__(add)(var, (ETHR_NAINT32_T__) incr);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var += incr);
#endif
}   
    
static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_add_read)(ethr_atomic32_t *var,
					       ethr_sint32_t i)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t)
	ETHR_NATMC32_FUNC__(add_return)(var, (ETHR_NAINT32_T__) i);
#else
    ethr_sint32_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var += i; res = *var);
    return res;
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_inc)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC32_FUNC__(inc)(var);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, ++(*var));
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_dec)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC32_FUNC__(dec)(var);
#else
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, --(*var));
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_inc_read)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(inc_return)(var);
#else
    ethr_sint32_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = ++(*var));
    return res;
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_dec_read)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(dec_return)(var);
#else
    ethr_sint32_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = --(*var));
    return res;
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_read_band)(ethr_atomic32_t *var,
						ethr_sint32_t mask)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t)
	ETHR_NATMC32_FUNC__(and_retold)(var, (ETHR_NAINT32_T__) mask);
#else
    ethr_sint32_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var; *var &= mask);
    return res;
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_read_bor)(ethr_atomic32_t *var,
					       ethr_sint32_t mask)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return
	(ethr_sint32_t) ETHR_NATMC32_FUNC__(or_retold)(var,
						       (ETHR_NAINT32_T__) mask);
#else
    ethr_sint32_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var; *var |= mask);
    return res;
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_xchg)(ethr_atomic32_t *var,
					   ethr_sint32_t new)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(xchg)(var,
						     (ETHR_NAINT32_T__) new);
#else
    ethr_sint32_t res;
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, res = *var; *var = new);
    return res;
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_cmpxchg)(ethr_atomic32_t *var,
					    ethr_sint32_t new,
                                            ethr_sint32_t exp)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(cmpxchg)(var,
							(ETHR_NAINT32_T__) new,
							(ETHR_NAINT32_T__) exp);
#else
    ethr_sint32_t res;
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

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_read_acqb)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(read_acqb)(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic32_read)(var);
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_inc_read_acqb)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(inc_return_acqb)(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic32_inc_read)(var);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_set_relb)(ethr_atomic32_t *var,
					       ethr_sint32_t val)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC32_FUNC__(set_relb)(var, (ETHR_NAINT32_T__) val);
#else
    ETHR_INLINE_FUNC_NAME_(ethr_atomic32_set)(var, val);
#endif
}

static ETHR_INLINE void
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_dec_relb)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    ETHR_NATMC32_FUNC__(dec_relb)(var);
#else
    ETHR_INLINE_FUNC_NAME_(ethr_atomic32_dec)(var);
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_dec_read_relb)(ethr_atomic32_t *var)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t) ETHR_NATMC32_FUNC__(dec_return_relb)(var);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic32_dec_read)(var);
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_cmpxchg_acqb)(ethr_atomic32_t *var,
						   ethr_sint32_t new,
						   ethr_sint32_t exp)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t)
	ETHR_NATMC32_FUNC__(cmpxchg_acqb)(var,
					  (ETHR_NAINT32_T__) new,
					  (ETHR_NAINT32_T__) exp);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic32_cmpxchg)(var, new, exp);
#endif
}

static ETHR_INLINE ethr_sint32_t
ETHR_INLINE_FUNC_NAME_(ethr_atomic32_cmpxchg_relb)(ethr_atomic32_t *var,
						   ethr_sint32_t new,
						   ethr_sint32_t exp)
{
#ifdef ETHR_HAVE_NATIVE_ATOMICS
    return (ethr_sint32_t)
	ETHR_NATMC32_FUNC__(cmpxchg_relb)(var,
					  (ETHR_NAINT32_T__) new,
					  (ETHR_NAINT32_T__) exp);
#else
    return ETHR_INLINE_FUNC_NAME_(ethr_atomic32_cmpxchg)(var, new, exp);
#endif
}


#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_NAINT_T__
#undef ETHR_NATMC_FUNC__
#undef ETHR_NATMC_ADDR_FUNC__

#undef ETHR_NAINT32_T__
#undef ETHR_NATMC32_FUNC__

#endif
