/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2014. All Rights Reserved.
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

#ifndef ERL_UTILS_H__
#define ERL_UTILS_H__

#include "sys.h"
#include "erl_smp.h"
#include "erl_printf.h"

struct process;

typedef struct {
#ifdef DEBUG
    int smp_api;
#endif
    union {
	Uint64 not_atomic;
#ifdef ARCH_64
	erts_atomic_t atomic;
#else
	erts_dw_atomic_t atomic;
#endif
    } counter;
} erts_interval_t;

void erts_interval_init(erts_interval_t *);
void erts_smp_interval_init(erts_interval_t *);
Uint64 erts_step_interval_nob(erts_interval_t *);
Uint64 erts_step_interval_relb(erts_interval_t *);
Uint64 erts_smp_step_interval_nob(erts_interval_t *);
Uint64 erts_smp_step_interval_relb(erts_interval_t *);
Uint64 erts_ensure_later_interval_nob(erts_interval_t *, Uint64);
Uint64 erts_ensure_later_interval_acqb(erts_interval_t *, Uint64);
Uint64 erts_smp_ensure_later_interval_nob(erts_interval_t *, Uint64);
Uint64 erts_smp_ensure_later_interval_acqb(erts_interval_t *, Uint64);
#ifdef ARCH_32
ERTS_GLB_INLINE Uint64 erts_interval_dw_aint_to_val__(erts_dw_aint_t *);
#endif
ERTS_GLB_INLINE Uint64 erts_current_interval_nob__(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_current_interval_acqb__(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_current_interval_nob(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_current_interval_acqb(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_smp_current_interval_nob(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_smp_current_interval_acqb(erts_interval_t *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef ARCH_32

ERTS_GLB_INLINE Uint64
erts_interval_dw_aint_to_val__(erts_dw_aint_t *dw)
{
#ifdef ETHR_SU_DW_NAINT_T__
    return (Uint64) dw->dw_sint;
#else
    Uint64 res;
    res = (Uint64) ((Uint32) dw->sint[ERTS_DW_AINT_HIGH_WORD]);
    res <<= 32;
    res |= (Uint64) ((Uint32) dw->sint[ERTS_DW_AINT_LOW_WORD]);
    return res;
#endif
}

#endif

ERTS_GLB_INLINE Uint64
erts_current_interval_nob__(erts_interval_t *icp)
{
#ifdef ARCH_64
    return (Uint64) erts_atomic_read_nob(&icp->counter.atomic);
#else
    erts_dw_aint_t dw;
    erts_dw_atomic_read_nob(&icp->counter.atomic, &dw);
    return erts_interval_dw_aint_to_val__(&dw);
#endif
}

ERTS_GLB_INLINE Uint64
erts_current_interval_acqb__(erts_interval_t *icp)
{
#ifdef ARCH_64
    return (Uint64) erts_atomic_read_acqb(&icp->counter.atomic);
#else
    erts_dw_aint_t dw;
    erts_dw_atomic_read_acqb(&icp->counter.atomic, &dw);
    return erts_interval_dw_aint_to_val__(&dw);
#endif
}

ERTS_GLB_INLINE Uint64
erts_current_interval_nob(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return erts_current_interval_nob__(icp);
}

ERTS_GLB_INLINE Uint64
erts_current_interval_acqb(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return erts_current_interval_acqb__(icp);
}

ERTS_GLB_INLINE Uint64
erts_smp_current_interval_nob(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return erts_current_interval_nob__(icp);
#else
    return icp->counter.not_atomic;
#endif
}

ERTS_GLB_INLINE Uint64
erts_smp_current_interval_acqb(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return erts_current_interval_acqb__(icp);
#else
    return icp->counter.not_atomic;
#endif
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

/*
 * To be used to silence unused result warnings, but do not abuse it.
 */
void erts_silence_warn_unused_result(long unused);


int erts_fit_in_bits_int64(Sint64);
int erts_fit_in_bits_int32(Sint32);
int erts_list_length(Eterm);
int erts_is_builtin(Eterm, Eterm, int);
Uint32 make_broken_hash(Eterm);
Uint32 block_hash(byte *, unsigned, Uint32);
Uint32 make_hash2(Eterm);
Uint32 make_hash(Eterm);

void erts_save_emu_args(int argc, char **argv);
Eterm erts_get_emu_args(struct process *c_p);
Eterm erts_get_ethread_info(struct process * c_p);

Eterm erts_bld_atom(Uint **hpp, Uint *szp, char *str);
Eterm erts_bld_uint(Uint **hpp, Uint *szp, Uint ui);
Eterm erts_bld_uword(Uint **hpp, Uint *szp, UWord uw);
Eterm erts_bld_uint64(Uint **hpp, Uint *szp, Uint64 ui64);
Eterm erts_bld_sint64(Uint **hpp, Uint *szp, Sint64 si64);
Eterm erts_bld_cons(Uint **hpp, Uint *szp, Eterm car, Eterm cdr);
Eterm erts_bld_tuple(Uint **hpp, Uint *szp, Uint arity, ...);
#define erts_bld_tuple2(H,S,E1,E2) erts_bld_tuple(H,S,2,E1,E2)
#define erts_bld_tuple3(H,S,E1,E2,E3) erts_bld_tuple(H,S,3,E1,E2,E3)
#define erts_bld_tuple4(H,S,E1,E2,E3,E4) erts_bld_tuple(H,S,4,E1,E2,E3,E4)
#define erts_bld_tuple5(H,S,E1,E2,E3,E4,E5) erts_bld_tuple(H,S,5,E1,E2,E3,E4,E5)
Eterm erts_bld_tuplev(Uint **hpp, Uint *szp, Uint arity, Eterm terms[]);
Eterm erts_bld_string_n(Uint **hpp, Uint *szp, const char *str, Sint len);
#define erts_bld_string(hpp,szp,str) erts_bld_string_n(hpp,szp,str,strlen(str))
Eterm erts_bld_list(Uint **hpp, Uint *szp, Sint length, Eterm terms[]);
Eterm erts_bld_2tup_list(Uint **hpp, Uint *szp,
			 Sint length, Eterm terms1[], Uint terms2[]);
Eterm
erts_bld_atom_uword_2tup_list(Uint **hpp, Uint *szp,
			     Sint length, Eterm atoms[], UWord uints[]);
Eterm
erts_bld_atom_2uint_3tup_list(Uint **hpp, Uint *szp, Sint length,
			      Eterm atoms[], Uint uints1[], Uint uints2[]);

void erts_init_utils(void);
void erts_init_utils_mem(void);

erts_dsprintf_buf_t *erts_create_tmp_dsbuf(Uint);
void erts_destroy_tmp_dsbuf(erts_dsprintf_buf_t *);

#if HALFWORD_HEAP
int eq_rel(Eterm a, Eterm* a_base, Eterm b, Eterm* b_base);
#  define eq(A,B) eq_rel(A,NULL,B,NULL)
#else
int eq(Eterm, Eterm);
#  define eq_rel(A,A_BASE,B,B_BASE) eq(A,B)
#endif

#define EQ(x,y) (((x) == (y)) || (is_not_both_immed((x),(y)) && eq((x),(y))))

#if HALFWORD_HEAP
Sint erts_cmp_rel_opt(Eterm, Eterm*, Eterm, Eterm*, int);
#define cmp_rel(A,A_BASE,B,B_BASE)       erts_cmp_rel_opt(A,A_BASE,B,B_BASE,0)
#define cmp_rel_term(A,A_BASE,B,B_BASE)  erts_cmp_rel_opt(A,A_BASE,B,B_BASE,1)
#define CMP(A,B)                         erts_cmp_rel_opt(A,NULL,B,NULL,0)
#define CMP_TERM(A,B)                    erts_cmp_rel_opt(A,NULL,B,NULL,1)
#else
Sint cmp(Eterm, Eterm);
Sint erts_cmp(Eterm, Eterm, int);
#define cmp_rel(A,A_BASE,B,B_BASE)       erts_cmp(A,B,0)
#define cmp_rel_term(A,A_BASE,B,B_BASE)  erts_cmp(A,B,1)
#define CMP(A,B)                         erts_cmp(A,B,0)
#define CMP_TERM(A,B)                    erts_cmp(A,B,1)
#endif

#define cmp_lt(a,b)          (CMP((a),(b)) <  0)
#define cmp_le(a,b)          (CMP((a),(b)) <= 0)
#define cmp_eq(a,b)          (CMP((a),(b)) == 0)
#define cmp_ne(a,b)          (CMP((a),(b)) != 0)
#define cmp_ge(a,b)          (CMP((a),(b)) >= 0)
#define cmp_gt(a,b)          (CMP((a),(b)) >  0)

#define cmp_lt_term(a,b)     (CMP_TERM((a),(b)) <  0)
#define cmp_le_term(a,b)     (CMP_TERM((a),(b)) <= 0)
#define cmp_ge_term(a,b)     (CMP_TERM((a),(b)) >= 0)
#define cmp_gt_term(a,b)     (CMP_TERM((a),(b)) >  0)

#define CMP_LT(a,b)          ((a) != (b) && cmp_lt((a),(b)))
#define CMP_GE(a,b)          ((a) == (b) || cmp_ge((a),(b)))
#define CMP_EQ(a,b)          ((a) == (b) || cmp_eq((a),(b)))
#define CMP_NE(a,b)          ((a) != (b) && cmp_ne((a),(b)))

#define CMP_LT_TERM(a,b)     ((a) != (b) && cmp_lt_term((a),(b)))
#define CMP_GE_TERM(a,b)     ((a) == (b) || cmp_ge_term((a),(b)))

#endif
