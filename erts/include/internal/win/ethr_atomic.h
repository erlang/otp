/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
 * Description: Native atomics ethread support when using VC++
 * Author: Rickard Green
 */

#undef ETHR_INCLUDE_ATOMIC_IMPL__
#if !defined(ETHR_WIN_ATOMIC32_H__) && defined(ETHR_ATOMIC_WANT_32BIT_IMPL__)
#  define ETHR_WIN_ATOMIC32_H__
#  if (defined(ETHR_MEMBAR) \
       && defined(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE) \
       && defined(ETHR_HAVE__INTERLOCKEDEXCHANGE))
#    define ETHR_INCLUDE_ATOMIC_IMPL__ 4
#  endif
#  undef ETHR_ATOMIC_WANT_32BIT_IMPL__
#elif !defined(ETHR_WIN_ATOMIC64_H__) && defined(ETHR_ATOMIC_WANT_64BIT_IMPL__)
#  define ETHR_WIN_ATOMIC64_H__
#  if (defined(ETHR_MEMBAR) \
     && (defined(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64) \
	 || defined(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_ACQ) \
	 || defined(ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_REL)))
#    define ETHR_INCLUDE_ATOMIC_IMPL__ 8
#  endif
#  undef ETHR_ATOMIC_WANT_64BIT_IMPL__
#endif

#if !defined(_MSC_VER) || _MSC_VER < 1400
#  undef ETHR_INCLUDE_ATOMIC_IMPL__
#endif

#ifdef ETHR_INCLUDE_ATOMIC_IMPL__

#  ifndef ETHR_WIN_ATOMIC_COMMON__
#    define ETHR_WIN_ATOMIC_COMMON__

#    if defined(_M_IX86) || defined(_M_AMD64) || defined(_M_IA64)
#      define ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__ 1
#    else
#      define ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__ 0
#    endif

#  endif /* ETHR_WIN_ATOMIC_COMMON__ */

#  if ETHR_INCLUDE_ATOMIC_IMPL__ == 4

#    define ETHR_HAVE_NATIVE_ATOMIC32 1
#    define ETHR_NATIVE_ATOMIC32_IMPL "windows-interlocked"

#    ifdef ETHR_HAVE__INTERLOCKEDDECREMENT
#      define ETHR_WIN_HAVE_DEC
#      pragma intrinsic(_InterlockedDecrement)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDDECREMENT_REL
#      define ETHR_WIN_HAVE_DEC_REL
#      pragma intrinsic(_InterlockedDecrement_rel)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDINCREMENT
#      define ETHR_WIN_HAVE_INC
#      pragma intrinsic(_InterlockedIncrement)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDINCREMENT_ACQ
#      define ETHR_WIN_HAVE_INC_ACQ
#      pragma intrinsic(_InterlockedIncrement_acq)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDEXCHANGEADD
#      define ETHR_WIN_HAVE_XCHG_ADD
#      pragma intrinsic(_InterlockedExchangeAdd)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDEXCHANGEADD_ACQ
#      define ETHR_WIN_HAVE_XCHG_ADD_ACQ
#      pragma intrinsic(_InterlockedExchangeAdd_acq)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDEXCHANGE
#      define ETHR_WIN_HAVE_XCHG
#      pragma intrinsic(_InterlockedExchange)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDAND
#      define ETHR_WIN_HAVE_AND
#      pragma intrinsic(_InterlockedAnd)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDOR
#      define ETHR_WIN_HAVE_OR
#      pragma intrinsic(_InterlockedOr)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE
#      define ETHR_WIN_HAVE_CMPXCHG
#        pragma intrinsic(_InterlockedCompareExchange)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_ACQ
#      define ETHR_WIN_HAVE_CMPXCHG_ACQ
#      pragma intrinsic(_InterlockedCompareExchange_acq)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_REL
#      define ETHR_WIN_HAVE_CMPXCHG_REL
#      pragma intrinsic(_InterlockedCompareExchange_rel)
#    endif

#    define ETHR_ILCKD__(X) _Interlocked ## X
#    define ETHR_ILCKD_ACQ__(X) _Interlocked ## X ## _acq
#    define ETHR_ILCKD_REL__(X) _Interlocked ## X ## _rel

#    define ETHR_NATMC_FUNC__(X) ethr_native_atomic32_ ## X
#    define ETHR_ATMC_T__ ethr_native_atomic32_t
#    define ETHR_AINT_T__ ethr_sint32_t

#  elif ETHR_INCLUDE_ATOMIC_IMPL__ == 8

#    define ETHR_HAVE_NATIVE_ATOMIC64 1
#    define ETHR_NATIVE_ATOMIC64_IMPL "windows-interlocked"

#    ifdef ETHR_HAVE__INTERLOCKEDDECREMENT64
#      define ETHR_WIN_HAVE_DEC
#      pragma intrinsic(_InterlockedDecrement64)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDDECREMENT64_REL
#      define ETHR_WIN_HAVE_DEC_REL
#      pragma intrinsic(_InterlockedDecrement64_rel)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDINCREMENT64_ACQ
#      define ETHR_WIN_HAVE_INC_ACQ
#      pragma intrinsic(_InterlockedIncrement64_acq)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDINCREMENT64
#      define ETHR_WIN_HAVE_INC
#      pragma intrinsic(_InterlockedIncrement64)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDEXCHANGEADD64
#      define ETHR_WIN_HAVE_XCHG_ADD
#      pragma intrinsic(_InterlockedExchangeAdd64)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDEXCHANGEADD64_ACQ
#      define ETHR_WIN_HAVE_XCHG_ADD_ACQ
#      pragma intrinsic(_InterlockedExchangeAdd64_acq)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDEXCHANGE64
#      define ETHR_WIN_HAVE_XCHG
#      pragma intrinsic(_InterlockedExchange64)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDAND64
#      define ETHR_WIN_HAVE_AND
#      pragma intrinsic(_InterlockedAnd64)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDOR64
#      define ETHR_WIN_HAVE_OR
#      pragma intrinsic(_InterlockedOr64)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64
#      define ETHR_WIN_HAVE_CMPXCHG
#      pragma intrinsic(_InterlockedCompareExchange64)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_ACQ
#      define ETHR_WIN_HAVE_CMPXCHG_ACQ
#      pragma intrinsic(_InterlockedCompareExchange64_acq)
#    endif
#    ifdef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_REL
#      define ETHR_WIN_HAVE_CMPXCHG_REL
#      pragma intrinsic(_InterlockedCompareExchange64_rel)
#    endif

#    define ETHR_ILCKD__(X) _Interlocked ## X ## 64
#    define ETHR_ILCKD_ACQ__(X) _Interlocked ## X ## 64_acq
#    define ETHR_ILCKD_REL__(X) _Interlocked ## X ## 64_rel

#    define ETHR_NATMC_FUNC__(X) ethr_native_atomic64_ ## X
#    define ETHR_ATMC_T__ ethr_native_atomic64_t
#    define ETHR_AINT_T__ ethr_sint64_t

#  else
#    error "Unsupported integer size"
#  endif

typedef struct {
    volatile ETHR_AINT_T__ value;
} ETHR_ATMC_T__;

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADDR 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADDR 1
#endif

static ETHR_INLINE ETHR_AINT_T__ *
ETHR_NATMC_FUNC__(addr)(ETHR_ATMC_T__ *var)
{
    return (ETHR_AINT_T__ *) &var->value;
}

#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    var->value = i;
}

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_RELB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_relb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
#if defined(_M_IX86)
    if (ETHR_X86_RUNTIME_CONF_HAVE_NO_SSE2__)
	(void) ETHR_ILCKD__(Exchange)(&var->value, i);
    else
#endif /* _M_IX86 */
    {
	ETHR_MEMBAR(ETHR_LoadStore|ETHR_StoreStore);
	var->value = i;
    }
}

#endif /* ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__ */

#if defined(ETHR_WIN_HAVE_XCHG)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_SET_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_SET_MB 1
#endif

static ETHR_INLINE void
ETHR_NATMC_FUNC__(set_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    (void) ETHR_ILCKD__(Exchange)(&var->value, i);
}

#endif

#if ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_READ 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_READ 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(read)(ETHR_ATMC_T__ *var)
{
    return var->value;
}

#endif /* ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__ */

#if defined(ETHR_WIN_HAVE_XCHG_ADD)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADD_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_ADD_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(add_return_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ i)
{
    return ETHR_ILCKD__(ExchangeAdd)(&var->value, i) + i;
}

#endif

#if defined(ETHR_WIN_HAVE_INC)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_INC_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_mb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD__(Increment)(&var->value);
}

#endif

#if defined(ETHR_WIN_HAVE_INC_ACQ)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INC_RETURN_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_INC_RETURN_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(inc_return_acqb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD_ACQ__(Increment)(&var->value);
}

#endif

#if defined(ETHR_WIN_HAVE_DEC)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_DEC_RETURN_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_mb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD__(Decrement)(&var->value);
}

#endif

#if defined(ETHR_WIN_HAVE_DEC_REL)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_DEC_RETURN_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_DEC_RETURN_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(dec_return_relb)(ETHR_ATMC_T__ *var)
{
    return ETHR_ILCKD_REL__(Decrement)(&var->value);
}

#endif

#if defined(ETHR_WIN_HAVE_AND)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_AND_RETOLD_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_AND_RETOLD_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(and_retold_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return ETHR_ILCKD__(And)(&var->value, mask);
}

#endif

#if defined(ETHR_WIN_HAVE_OR)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_OR_RETOLD_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_OR_RETOLD_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(or_retold_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ mask)
{
    return ETHR_ILCKD__(Or)(&var->value, mask);
}

#endif

#if defined(ETHR_WIN_HAVE_XCHG)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_XCHG_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_XCHG_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(xchg_mb)(ETHR_ATMC_T__ *var, ETHR_AINT_T__ new)
{
    return ETHR_ILCKD__(Exchange)(&var->value, new);
}

#endif

#if defined(ETHR_WIN_HAVE_CMPXCHG)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_MB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_MB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_mb)(ETHR_ATMC_T__ *var,
			      ETHR_AINT_T__ new,
			      ETHR_AINT_T__ old)
{
    return ETHR_ILCKD__(CompareExchange)(&var->value, new, old);
}

#endif

#if defined(ETHR_WIN_HAVE_CMPXCHG_ACQ)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_ACQB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_ACQB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_acqb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_ILCKD_ACQ__(CompareExchange)(&var->value, new, old);
}

#endif

#if defined(ETHR_WIN_HAVE_CMPXCHG_REL)

#if ETHR_INCLUDE_ATOMIC_IMPL__ == 4
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG_RELB 1
#else
#  define ETHR_HAVE_ETHR_NATIVE_ATOMIC64_CMPXCHG_RELB 1
#endif

static ETHR_INLINE ETHR_AINT_T__
ETHR_NATMC_FUNC__(cmpxchg_relb)(ETHR_ATMC_T__ *var,
				ETHR_AINT_T__ new,
				ETHR_AINT_T__ old)
{
    return ETHR_ILCKD_REL__(CompareExchange)(&var->value, new, old);
}

#endif

#endif /* ETHR_TRY_INLINE_FUNCS */

#undef ETHR_ILCKD__
#undef ETHR_ILCKD_ACQ__
#undef ETHR_ILCKD_REL__
#undef ETHR_NATMC_FUNC__
#undef ETHR_ATMC_T__
#undef ETHR_AINT_T__
#undef ETHR_READ_AND_SET_WITHOUT_INTERLOCKED_OP__
#undef ETHR_WIN_HAVE_CMPXCHG
#undef ETHR_WIN_HAVE_DEC
#undef ETHR_WIN_HAVE_INC
#undef ETHR_WIN_HAVE_XCHG_ADD
#undef ETHR_WIN_HAVE_XCHG
#undef ETHR_WIN_HAVE_AND
#undef ETHR_WIN_HAVE_OR
#undef ETHR_WIN_HAVE_XCHG_ADD_ACQ
#undef ETHR_WIN_HAVE_INC_ACQ
#undef ETHR_WIN_HAVE_DEC_REL
#undef ETHR_WIN_HAVE_CMPXCHG_ACQ
#undef ETHR_WIN_HAVE_CMPXCHG_REL

#endif /* ETHR_INCLUDE_ATOMIC_IMPL__ */
