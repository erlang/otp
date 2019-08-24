/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
 * Description: Native double word atomics using libatomic_ops
 * Author: Rickard Green
 */

#ifndef ETHR_LIBATOMIC_OPS_DW_ATOMIC_H__
#define ETHR_LIBATOMIC_OPS_DW_ATOMIC_H__

#if defined(AO_HAVE_double_t)						\
    && (defined(AO_HAVE_double_load_acquire)				\
	|| defined(AO_HAVE_double_load))				\
    && (defined(AO_HAVE_compare_double_and_swap_double)			\
	|| defined(AO_HAVE_compare_double_and_swap_double_full)		\
	|| defined(AO_HAVE_compare_double_and_swap_double_acquire)	\
	|| defined(AO_HAVE_compare_double_and_swap_double_release)	\
	|| defined(AO_HAVE_double_compare_and_swap)			\
	|| defined(AO_HAVE_double_compare_and_swap_full)		\
	|| defined(AO_HAVE_double_compare_and_swap_acquire)		\
	|| defined(AO_HAVE_double_compare_and_swap_release))

#if ETHR_SIZEOF_PTR == 4
#  define ETHR_NATIVE_SU_DW_SINT_T ethr_sint64_t
#elif ETHR_SIZEOF_PTR == 8 && defined(ETHR_HAVE_INT128_T)
#  define ETHR_NATIVE_SU_DW_SINT_T ethr_sint128_t
#endif

typedef union {
    volatile AO_double_t dw_mem;
#if defined(ETHR_NATIVE_SU_DW_SINT_T)
    ETHR_NATIVE_SU_DW_SINT_T su_dw_sint;
#endif
} ethr_native_dw_atomic_t;

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_NATIVE_SU_DW_ATOMIC
#else
#  define ETHR_HAVE_NATIVE_DW_ATOMIC
#endif

#define ETHR_NATIVE_DW_ATOMIC_IMPL ETHR_NATIVE_IMPL__

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__)


#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_NDWA_FUNC__(Func) ethr_native_su_dw_atomic_ ## Func
#  define ETHR_NDWA_RET_3_TYPE__ ETHR_NATIVE_SU_DW_SINT_T
#  define ETHR_NDWA_RET_2_TYPE__ ETHR_NATIVE_SU_DW_SINT_T
#  define ETHR_NDWA_VAL_ARG_TYPE__ ETHR_NATIVE_SU_DW_SINT_T
#  define ETHR_NDWA_DECL_ARG__(Arg)
#  if defined(AO_HAVE_DOUBLE_PTR_STORAGE)
#    define ETHR_NDWA_VAL2AOVAL__(AOV, V)			\
    ((AOV).AO_whole = (double_ptr_storage) (V))
#    define ETHR_NDWA_AOVAL2VAL__(AOV, V)			\
    ((V) = (ETHR_NATIVE_SU_DW_SINT_T) (AOV).AO_whole)
#    define ETHR_NDWA_RETURN_VAL_3__(SUCCESS, AOVAL, VAL)	\
    do {							\
	return (ETHR_NATIVE_SU_DW_SINT_T) (AOVAL).AO_whole;	\
    } while (0)
#    define ETHR_NDWA_RETURN_VAL_2__(AOVAL, VAL)		\
    do {							\
	return (ETHR_NATIVE_SU_DW_SINT_T) (AOVAL).AO_whole;	\
    } while (0)
#    define ETHR_NDWA_AOVAL_EQ__(AOV1, AOV2)			\
    ((AOV1).AO_whole == (AOV2).AO_whole)
#  else
typedef union {
    ethr_sint_t sint[2];
    ETHR_NATIVE_SU_DW_SINT_T dw_sint;
}  ethr_dw_splitter_t;
#    define ETHR_NDWA_VAL2AOVAL__(AOV, V)			\
    do {							\
	ethr_dw_splitter_t tmp__;				\
	tmp__.dw_sint = (V);					\
	(AOV).AO_val1 = (AO_t) tmp__.sint[0];			\
	(AOV).AO_val2 = (AO_t) tmp__.sint[1];			\
    } while (0)
#    define ETHR_NDWA_AOVAL2VAL__(AOV, V)			\
    do {							\
	ethr_dw_splitter_t tmp__;				\
	tmp__.sint[0] = (ethr_sint_t) (AOV).AO_val1;		\
	tmp__.sint[1] = (ethr_sint_t) (AOV).AO_val2;		\
	(V) = tmp__.dw_sint;					\
    } while (0)
#    define ETHR_NDWA_RETURN_VAL_3__(SUCCESS, AOVAL, VAL)	\
    do {							\
	ethr_dw_splitter_t tmp__;				\
	tmp__.sint[0] = (ethr_sint_t) (AOVAL).AO_val1;		\
	tmp__.sint[1] = (ethr_sint_t) (AOVAL).AO_val2;		\
	return tmp__.dw_sint;					\
    } while (0)
#    define ETHR_NDWA_AOVAL_EQ__(AOV1, AOV2)			\
    ((AOV1).AO_val1 == (AOV2).AO_val1				\
	 && (AOV1).AO_val2 == (AOV2).AO_val2)
#  endif
#else
#  define ETHR_NDWA_FUNC__(Func) ethr_native_dw_atomic_ ## Func
#  define ETHR_NDWA_RET_3_TYPE__ int
#  define ETHR_NDWA_RET_2_TYPE__ void
#  define ETHR_NDWA_VAL_ARG_TYPE__ ethr_sint_t *
#  define ETHR_NDWA_DECL_ARG__(Arg) , ETHR_NDWA_VAL_ARG_TYPE__ Arg
#    define ETHR_NDWA_VAL2AOVAL__(AOV, V)			\
    do {							\
	(AOV).AO_val1 = (AO_t) (V)[0];				\
	(AOV).AO_val2 = (AO_t) (V)[1];				\
    } while (0)
#    define ETHR_NDWA_AOVAL2VAL__(AOV, V)			\
    do {							\
	ethr_dw_splitter_t tmp__;				\
	(V)[0] = (ethr_sint_t) (AOV).AO_val1;			\
	(V)[1] = (ethr_sint_t) (AOV).AO_val2;			\
    } while (0)
#    define ETHR_NDWA_RETURN_VAL_3__(SUCCESS, AOVAL, VAL)	\
    do {							\
	(VAL)[0] = (ethr_sint_t) (AOVAL).AO_val1;		\
	(VAL)[1] = (ethr_sint_t) (AOVAL).AO_val2;		\
	return (SUCCESS);					\
    } while (0)
#    define ETHR_NDWA_RETURN_VAL_2__(AOVAL, VAL)		\
    do {							\
	(VAL)[0] = (ethr_sint_t) (AOVAL).AO_val1;		\
	(VAL)[1] = (ethr_sint_t) (AOVAL).AO_val2;		\
	return;							\
    } while (0)
#  if defined(AO_HAVE_DOUBLE_PTR_STORAGE)
#    define ETHR_NDWA_AOVAL_EQ__(AOV1, AOV2)			\
    ((AOV1).AO_whole == (AOV2).AO_whole)
#  else
#    define ETHR_NDWA_AOVAL_EQ__(AOV1, AOV2)			\
    ((AOV1).AO_val1 == (AOV2).AO_val1				\
	 && (AOV1).AO_val2 == (AOV2).AO_val2)
#  endif
#endif

#define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_ADDR
static ETHR_INLINE ethr_sint_t *
ethr_native_dw_atomic_addr(ethr_native_dw_atomic_t *var)
{
    return (ethr_sint_t *) &var->dw_mem;
}

#ifdef AO_HAVE_double_load

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_READ
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_READ
#endif

static ETHR_INLINE ETHR_NDWA_RET_2_TYPE__
ETHR_NDWA_FUNC__(read)(ethr_native_dw_atomic_t *var
		       ETHR_NDWA_DECL_ARG__(val))
{
    AO_double_t act = AO_double_load(&var->dw_mem);
    ETHR_NDWA_RETURN_VAL_2__(act, val);
}

#endif

#ifdef AO_HAVE_double_load_read

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_READ_RB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_READ_RB
#endif

static ETHR_INLINE ETHR_NDWA_RET_2_TYPE__
ETHR_NDWA_FUNC__(read_rb)(ethr_native_dw_atomic_t *var
			  ETHR_NDWA_DECL_ARG__(val))
{
    AO_double_t act = AO_double_load_read(&var->dw_mem);
    ETHR_NDWA_RETURN_VAL_2__(act, val);
}

#endif

#ifdef AO_HAVE_double_load_acquire

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_READ_ACQB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_READ_ACQB
#endif

static ETHR_INLINE ETHR_NDWA_RET_2_TYPE__
ETHR_NDWA_FUNC__(read_acqb)(ethr_native_dw_atomic_t *var
			    ETHR_NDWA_DECL_ARG__(val))
{
    AO_double_t act = AO_double_load_acquire(&var->dw_mem);
    ETHR_NDWA_RETURN_VAL_2__(act, val);
}

#endif

#ifdef AO_HAVE_double_store

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_SET
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_SET
#endif

static ETHR_INLINE void
ETHR_NDWA_FUNC__(set)(ethr_native_dw_atomic_t *var,
		      ETHR_NDWA_VAL_ARG_TYPE__ val)
{
    AO_double_t new;
    ETHR_NDWA_VAL2AOVAL__(new, val);
    AO_double_store(&var->dw_mem, new);
}

#endif

#ifdef AO_HAVE_double_store_write

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_SET_WB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_SET_WB
#endif

static ETHR_INLINE void
ETHR_NDWA_FUNC__(set_wb)(ethr_native_dw_atomic_t *var,
			 ETHR_NDWA_VAL_ARG_TYPE__ val)
{
    AO_double_t new;
    ETHR_NDWA_VAL2AOVAL__(new, val);
    AO_double_store_write(&var->dw_mem, new);
}

#endif

#ifdef AO_HAVE_double_store_release

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_SET_RELB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_SET_RELB
#endif

static ETHR_INLINE void
ETHR_NDWA_FUNC__(set_relb)(ethr_native_dw_atomic_t *var,
			   ETHR_NDWA_VAL_ARG_TYPE__ val)
{
    AO_double_t new;
    ETHR_NDWA_VAL2AOVAL__(new, val);
    AO_double_store_release(&var->dw_mem, new);
}

#endif

#if defined(AO_HAVE_double_compare_and_swap_full) || defined(AO_HAVE_compare_double_and_swap_double_full)

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_MB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG_MB
#endif

static ETHR_INLINE ETHR_NDWA_RET_3_TYPE__
ETHR_NDWA_FUNC__(cmpxchg_mb)(ethr_native_dw_atomic_t *var,
			     ETHR_NDWA_VAL_ARG_TYPE__ new,
			     ETHR_NDWA_VAL_ARG_TYPE__ exp)
{
    AO_double_t ao_act, ao_new, ao_exp;

    ETHR_NDWA_VAL2AOVAL__(ao_exp, exp);
    ETHR_NDWA_VAL2AOVAL__(ao_new, new);

    do {
	int xchgd;
#if defined(AO_HAVE_double_compare_and_swap_full)
	xchgd = AO_double_compare_and_swap_full(&var->dw_mem, ao_exp, ao_new);
#elif defined(AO_HAVE_compare_double_and_swap_double_full)
	xchgd = AO_compare_double_and_swap_double_full(&var->dw_mem,
						       ao_exp.AO_val1,
						       ao_exp.AO_val2,
						       ao_new.AO_val1,
						       ao_new.AO_val2);
#endif

	if (xchgd)
	    ETHR_NDWA_RETURN_VAL_3__(1, ao_exp, exp);

#ifdef AO_HAVE_double_load_acquire
	ao_act = AO_double_load_acquire(&var->dw_mem);
#else
	ao_act = AO_double_load(&var->dw_mem);
#endif

    } while (ETHR_NDWA_AOVAL_EQ__(ao_exp, ao_act));

#ifndef AO_HAVE_double_load_acquire
    AO_nop_full();
#endif

    ETHR_NDWA_RETURN_VAL_3__(1, ao_act, exp);
}

#endif

#if defined(AO_HAVE_double_compare_and_swap) || defined(AO_HAVE_compare_double_and_swap_double)

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG
#endif

static ETHR_INLINE ETHR_NDWA_RET_3_TYPE__
ETHR_NDWA_FUNC__(cmpxchg)(ethr_native_dw_atomic_t *var,
			  ETHR_NDWA_VAL_ARG_TYPE__ new,
			  ETHR_NDWA_VAL_ARG_TYPE__ exp)
{
    AO_double_t ao_act, ao_new, ao_exp;

    ETHR_NDWA_VAL2AOVAL__(ao_exp, exp);
    ETHR_NDWA_VAL2AOVAL__(ao_new, new);

    do {
	int xchgd;
#if defined(AO_HAVE_double_compare_and_swap)
	xchgd = AO_double_compare_and_swap(&var->dw_mem, ao_exp, ao_new);
#elif defined(AO_HAVE_compare_double_and_swap_double)
	xchgd = AO_compare_double_and_swap_double(&var->dw_mem,
						  ao_exp.AO_val1,
						  ao_exp.AO_val2,
						  ao_new.AO_val1,
						  ao_new.AO_val2);
#endif

	if (xchgd)
	    ETHR_NDWA_RETURN_VAL_3__(1, ao_exp, exp);

#ifdef AO_HAVE_double_load
	ao_act = AO_double_load(&var->dw_mem);
#else
	ao_act = AO_double_load_acquire(&var->dw_mem);
#endif

    } while (ETHR_NDWA_AOVAL_EQ__(ao_exp, ao_act));

    ETHR_NDWA_RETURN_VAL_3__(1, ao_act, exp);
}

#endif

#if defined(AO_HAVE_double_compare_and_swap_read) || defined(AO_HAVE_compare_double_and_swap_double_read)

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_RB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG_RB
#endif

static ETHR_INLINE ETHR_NDWA_RET_3_TYPE__
ETHR_NDWA_FUNC__(cmpxchg_rb)(ethr_native_dw_atomic_t *var,
			     ETHR_NDWA_VAL_ARG_TYPE__ new,
			     ETHR_NDWA_VAL_ARG_TYPE__ exp)
{
    AO_double_t ao_act, ao_new, ao_exp;

    ETHR_NDWA_VAL2AOVAL__(ao_exp, exp);
    ETHR_NDWA_VAL2AOVAL__(ao_new, new);

    do {
	int xchgd;
#if defined(AO_HAVE_double_compare_and_swap_read)
	xchgd = AO_double_compare_and_swap_read(&var->dw_mem, ao_exp, ao_new);
#elif defined(AO_HAVE_compare_double_and_swap_double_read)
	xchgd = AO_compare_double_and_swap_double_read(&var->dw_mem,
							  ao_exp.AO_val1,
							  ao_exp.AO_val2,
							  ao_new.AO_val1,
							  ao_new.AO_val2);
#endif

	if (xchgd)
	    ETHR_NDWA_RETURN_VAL_3__(1, ao_exp, exp);

#if defined(AO_HAVE_double_load_read)
	ao_act = AO_double_load_read(&var->dw_mem);
#elif defined(AO_HAVE_double_load)
	ao_act = AO_double_load(&var->dw_mem);
#else
	ao_act = AO_double_load_acquire(&var->dw_mem);
#endif

    } while (ETHR_NDWA_AOVAL_EQ__(ao_exp, ao_act));

#ifndef AO_HAVE_double_load_read
#ifdef AO_HAVE_nop_read
    AO_nop_read();
#else
    AO_nop_full();
#endif
#endif

    ETHR_NDWA_RETURN_VAL_3__(1, ao_act, exp);
}

#endif

#if defined(AO_HAVE_double_compare_and_swap_acquire) || defined(AO_HAVE_compare_double_and_swap_double_acquire)

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_ACQB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG_ACQB
#endif

static ETHR_INLINE ETHR_NDWA_RET_3_TYPE__
ETHR_NDWA_FUNC__(cmpxchg_acqb)(ethr_native_dw_atomic_t *var,
			       ETHR_NDWA_VAL_ARG_TYPE__ new,
			       ETHR_NDWA_VAL_ARG_TYPE__ exp)
{
    AO_double_t ao_act, ao_new, ao_exp;

    ETHR_NDWA_VAL2AOVAL__(ao_exp, exp);
    ETHR_NDWA_VAL2AOVAL__(ao_new, new);

    do {
	int xchgd;
#if defined(AO_HAVE_double_compare_and_swap_acquire)
	xchgd = AO_double_compare_and_swap_acquire(&var->dw_mem, ao_exp, ao_new);
#elif defined(AO_HAVE_compare_double_and_swap_double_acquire)
	xchgd = AO_compare_double_and_swap_double_acquire(&var->dw_mem,
							  ao_exp.AO_val1,
							  ao_exp.AO_val2,
							  ao_new.AO_val1,
							  ao_new.AO_val2);
#endif

	if (xchgd)
	    ETHR_NDWA_RETURN_VAL_3__(1, ao_exp, exp);

#ifdef AO_HAVE_double_load_acquire
	ao_act = AO_double_load_acquire(&var->dw_mem);
#else
	ao_act = AO_double_load(&var->dw_mem);
#endif

    } while (ETHR_NDWA_AOVAL_EQ__(ao_exp, ao_act));

#ifndef AO_HAVE_double_load_acquire
    AO_nop_full();
#endif

    ETHR_NDWA_RETURN_VAL_3__(1, ao_act, exp);
}

#endif

#if defined(AO_HAVE_double_compare_and_swap_write) || defined(AO_HAVE_compare_double_and_swap_double_write)

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_WB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG_WB
#endif

static ETHR_INLINE ETHR_NDWA_RET_3_TYPE__
ETHR_NDWA_FUNC__(cmpxchg_wb)(ethr_native_dw_atomic_t *var,
			     ETHR_NDWA_VAL_ARG_TYPE__ new,
			     ETHR_NDWA_VAL_ARG_TYPE__ exp)
{
    AO_double_t ao_act, ao_new, ao_exp;

    ETHR_NDWA_VAL2AOVAL__(ao_exp, exp);
    ETHR_NDWA_VAL2AOVAL__(ao_new, new);

    do {
	int xchgd;
#if defined(AO_HAVE_double_compare_and_swap_write)
	xchgd = AO_double_compare_and_swap_write(&var->dw_mem, ao_exp, ao_new);
#elif defined(AO_HAVE_compare_double_and_swap_double_write)
	xchgd = AO_compare_double_and_swap_double_write(&var->dw_mem,
							ao_exp.AO_val1,
							ao_exp.AO_val2,
							ao_new.AO_val1,
							ao_new.AO_val2);
#endif

	if (xchgd)
	    ETHR_NDWA_RETURN_VAL_3__(1, ao_exp, exp);

#ifdef AO_HAVE_double_load
	ao_act = AO_double_load(&var->dw_mem);
#else
	ao_act = AO_double_load_acquire(&var->dw_mem);
#endif

    } while (ETHR_NDWA_AOVAL_EQ__(ao_exp, ao_act));

    ETHR_NDWA_RETURN_VAL_3__(1, ao_act, exp);
}

#endif

#if defined(AO_HAVE_double_compare_and_swap_release) || defined(AO_HAVE_compare_double_and_swap_double_release)

#if defined(ETHR_NATIVE_SU_DW_SINT_T)
#  define ETHR_HAVE_ETHR_NATIVE_SU_DW_ATOMIC_CMPXCHG_RELB
#else
#  define ETHR_HAVE_ETHR_NATIVE_DW_ATOMIC_CMPXCHG_RELB
#endif

static ETHR_INLINE ETHR_NDWA_RET_3_TYPE__
ETHR_NDWA_FUNC__(cmpxchg_relb)(ethr_native_dw_atomic_t *var,
			       ETHR_NDWA_VAL_ARG_TYPE__ new,
			       ETHR_NDWA_VAL_ARG_TYPE__ exp)
{
    AO_double_t ao_act, ao_new, ao_exp;

    ETHR_NDWA_VAL2AOVAL__(ao_exp, exp);
    ETHR_NDWA_VAL2AOVAL__(ao_new, new);

    do {
	int xchgd;
#if defined(AO_HAVE_double_compare_and_swap_release)
	xchgd = AO_double_compare_and_swap_release(&var->dw_mem, ao_exp, ao_new);
#elif defined(AO_HAVE_compare_double_and_swap_double_release)
	xchgd = AO_compare_double_and_swap_double_release(&var->dw_mem,
							  ao_exp.AO_val1,
							  ao_exp.AO_val2,
							  ao_new.AO_val1,
							  ao_new.AO_val2);
#endif

	if (xchgd)
	    ETHR_NDWA_RETURN_VAL_3__(1, ao_exp, exp);

	ao_act = AO_double_load(&var->dw_mem);

    } while (ETHR_NDWA_AOVAL_EQ__(ao_exp, ao_act));

    ETHR_NDWA_RETURN_VAL_3__(1, ao_act, exp);
}

#endif

#endif /* defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_ATOMIC_IMPL__) */

#endif /* Have AO double functionality ... */

#endif /* ETHR_LIBATOMIC_OPS_DW_ATOMIC_H__ */

