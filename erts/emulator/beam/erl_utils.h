/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2024. All Rights Reserved.
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

#ifndef ERL_UTILS_H__
#define ERL_UTILS_H__

#include "sys.h"
#include "atom.h"
#include "erl_printf.h"
#include "erl_term_hashing.h"

struct process;

typedef struct {
    union {
	Uint64 not_atomic;
	erts_atomic64_t atomic;
    } counter;
} erts_interval_t;

void erts_interval_init(erts_interval_t *);
Uint64 erts_step_interval_nob(erts_interval_t *);
Uint64 erts_step_interval_relb(erts_interval_t *);
Uint64 erts_ensure_later_interval_nob(erts_interval_t *, Uint64);
Uint64 erts_ensure_later_interval_acqb(erts_interval_t *, Uint64);
ERTS_GLB_INLINE Uint64 erts_current_interval_nob(erts_interval_t *);
ERTS_GLB_INLINE Uint64 erts_current_interval_acqb(erts_interval_t *);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Uint64
erts_current_interval_nob(erts_interval_t *icp)
{
    return (Uint64) erts_atomic64_read_nob(&icp->counter.atomic);
}

ERTS_GLB_INLINE Uint64
erts_current_interval_acqb(erts_interval_t *icp)
{
    return (Uint64) erts_atomic64_read_acqb(&icp->counter.atomic);
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

/*
 * To be used to silence unused result warnings, but do not abuse it.
 */
void erts_silence_warn_unused_result(long unused);

int erts_fit_in_bits_int64(Sint64);
int erts_fit_in_bits_int32(Sint32);
int erts_fit_in_bits_uint(Uint);
Sint erts_list_length(Eterm);
int erts_is_builtin(Eterm, Eterm, int);

void erts_save_emu_args(int argc, char **argv);
Eterm erts_get_emu_args(struct process *c_p);
Eterm erts_get_ethread_info(struct process * c_p);

Eterm erts_bld_atom(Uint **hpp, Uint *szp, char *str);
Eterm erts_bld_uint(Uint **hpp, Uint *szp, Uint ui);
Eterm erts_bld_uword(Uint **hpp, Uint *szp, UWord uw);
Eterm erts_bld_uint64(Uint **hpp, Uint *szp, Uint64 ui64);
Eterm erts_bld_sint64(Uint **hpp, Uint *szp, Sint64 si64);
#define erts_bld_monotonic_time erts_bld_sint64
Eterm erts_bld_cons(Uint **hpp, Uint *szp, Eterm car, Eterm cdr);
Eterm erts_bld_tuple(Uint **hpp, Uint *szp, Uint arity, ...);
#define erts_bld_tuple2(H,S,E1,E2) erts_bld_tuple(H,S,2,E1,E2)
#define erts_bld_tuple3(H,S,E1,E2,E3) erts_bld_tuple(H,S,3,E1,E2,E3)
#define erts_bld_tuple4(H,S,E1,E2,E3,E4) erts_bld_tuple(H,S,4,E1,E2,E3,E4)
#define erts_bld_tuple5(H,S,E1,E2,E3,E4,E5) erts_bld_tuple(H,S,5,E1,E2,E3,E4,E5)
Eterm erts_bld_tuplev(Uint **hpp, Uint *szp, Uint arity, Eterm terms[]);
Eterm erts_bld_string_n(Uint **hpp, Uint *szp, const char *str, Sint len);
#define erts_bld_string(hpp,szp,str) erts_bld_string_n(hpp,szp,str,sys_strlen(str))
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
void erts_utils_sched_spec_data_init(void);

void erts_destroy_tmp_dsbuf(erts_dsprintf_buf_t *);
erts_dsprintf_buf_t *erts_create_tmp_dsbuf(Uint) ERTS_ATTR_MALLOC_D(erts_destroy_tmp_dsbuf,1);

int eq(Eterm, Eterm);

#define EQ(x,y) (((x) == (y)) || (is_not_both_immed((x),(y)) && eq((x),(y))))

ERTS_GLB_INLINE Sint erts_cmp(Eterm, Eterm, int, int);
ERTS_GLB_INLINE int erts_cmp_atoms(Eterm a, Eterm b);
ERTS_GLB_INLINE Sint erts_cmp_flatmap_keys(Eterm, Eterm);

Sint erts_cmp_compound(Eterm, Eterm, int, int);

#define CMP(A,B)                         erts_cmp(A,B,0,0)
#define CMP_TERM(A,B)                    erts_cmp(A,B,1,0)
#define CMP_EQ_ONLY(A,B)                 erts_cmp(A,B,0,1)

#define CMP_LT(a,b)          ((a) != (b) && CMP((a),(b)) <  0)
#define CMP_LE(a,b)          ((a) == (b) || CMP((a),(b)) <= 0)
#define CMP_EQ(a,b)          ((a) == (b) || CMP_EQ_ONLY((a),(b)) == 0)
#define CMP_NE(a,b)          ((a) != (b) && CMP_EQ_ONLY((a),(b)) != 0)
#define CMP_GE(a,b)          ((a) == (b) || CMP((a),(b)) >= 0)
#define CMP_GT(a,b)          ((a) != (b) && CMP((a),(b)) >  0)

#define CMP_EQ_ACTION(X,Y,Action)	\
    if ((X) != (Y)) { EQ_SPEC((X),(Y),!=,Action); }
#define CMP_NE_ACTION(X,Y,Action)	\
    if ((X) == (Y)) { Action; } else { EQ_SPEC((X),(Y),==,Action); }

#define EQ_SPEC(X,Y,Op,Action)                                  \
    if (is_both_immed(X, Y)) {                                  \
        if (X Op Y) { Action; };                                \
    } else if (is_float(X) && is_float(Y)) {                    \
        FloatDef af, bf;                                        \
        GET_DOUBLE(X, af);                                      \
        GET_DOUBLE(Y, bf);                                      \
        if (af.fd Op bf.fd) { Action; };                        \
    } else {                                                    \
        if (erts_cmp_compound(X,Y,0,1) Op 0) { Action; };       \
    }

#define CMP_GE_ACTION(X,Y,Action)                       \
    if ((X) != (Y)) { CMP_SPEC((X),(Y),<,Action); }
#define CMP_LT_ACTION(X,Y,Action)	\
    if ((X) == (Y)) { Action; } else { CMP_SPEC((X),(Y),>=,Action); }

#define CMP_SPEC(X,Y,Op,Action)                                 \
    if (is_atom(X) && is_atom(Y)) {				\
	if (erts_cmp_atoms(X, Y) Op 0) { Action; };		\
    } else if (is_both_small(X, Y)) {				\
	if (signed_val(X) Op signed_val(Y)) { Action; };	\
    } else if (is_float(X) && is_float(Y)) {			\
        FloatDef af, bf;					\
        GET_DOUBLE(X, af);					\
        GET_DOUBLE(Y, bf);					\
        if (af.fd Op bf.fd) { Action; };			\
    } else {							\
	if (erts_cmp_compound(X,Y,0,0) Op 0) { Action; };	\
    }

/*
 * When either operand for is_lt or is_ge is a literal, that literal is
 * almost always an integer and almost never an atom. Therefore, only
 * special case the comparison of small integers before calling the
 * general compare function.
 */

#define CMP_GE_LITERAL_ACTION(X,Y,Action)                       \
    if ((X) != (Y)) { CMP_LITERAL_SPEC((X),(Y),<,Action); }
#define CMP_LT_LITERAL_ACTION(X,Y,Action)	\
    if ((X) == (Y)) { Action; } else { CMP_LITERAL_SPEC((X),(Y),>=,Action); }

#define CMP_LITERAL_SPEC(X,Y,Op,Action)                         \
    if (is_both_small(X, Y)) {                                  \
        if (signed_val(X) Op signed_val(Y)) { Action; };        \
    } else {                                                    \
        if (erts_cmp_compound(X,Y,0,0) Op 0) { Action; };       \
    }

#define erts_float_comp(x,y) (((x)<(y)) ? -1 : (((x)==(y)) ? 0 : 1))

ERTS_GLB_INLINE Uint64 erts_float_exact_sortable(const FloatDef *value);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int erts_cmp_atoms(Eterm a, Eterm b) {
    Atom *aa = atom_tab(atom_val(a));
    Atom *bb = atom_tab(atom_val(b));

    byte *name_a, *name_b;
    int len_a, len_b, diff;

    diff = aa->ord0 - bb->ord0;

    if (diff != 0) {
        return diff;
    }

    name_a = &aa->name[3];
    name_b = &bb->name[3];
    len_a = aa->len-3;
    len_b = bb->len-3;

    if (len_a > 0 && len_b > 0) {
        diff = sys_memcmp(name_a, name_b, MIN(len_a, len_b));

        if (diff != 0) {
            return diff;
        }
    }

    return len_a - len_b;
}

/* Provides a sortable integer from the raw bits of a floating-point number.
 *
 * When these are compared, they return the exact same result as a floating-
 * point comparison of the inputs aside from the fact that -0.0 < +0.0, making
 * it suitable for use in term equivalence operators and map key order. */
ERTS_GLB_INLINE Uint64 erts_float_exact_sortable(const FloatDef *value) {
    static const Uint64 sign_bit = ((Uint64)1) << 63;
    Uint64 float_bits = value->fdw;

    if (float_bits & sign_bit) {
        return ~float_bits;
    }

    return float_bits ^ sign_bit;
}

ERTS_GLB_INLINE Sint erts_cmp(Eterm a, Eterm b, int exact, int eq_only) {
    if (is_atom(a) && is_atom(b)) {
        return erts_cmp_atoms(a, b);
    } else if (is_both_small(a, b)) {
        return (signed_val(a) - signed_val(b));
    } else if (is_float(a) && is_float(b)) {
        FloatDef af, bf;

        GET_DOUBLE(a, af);
        GET_DOUBLE(b, bf);

        if (exact) {
            Uint64 sortable_a, sortable_b;

            sortable_a = erts_float_exact_sortable(&af);
            sortable_b = erts_float_exact_sortable(&bf);

            return erts_float_comp(sortable_a, sortable_b);
        } else {
            return erts_float_comp(af.fd, bf.fd);
        }
    }

    return erts_cmp_compound(a,b,exact,eq_only);
}

/*
 * Only to be used for the *internal* sort order of flatmap keys.
 */
ERTS_GLB_INLINE Sint erts_cmp_flatmap_keys(Eterm key_a, Eterm key_b) {
    if (is_atom(key_a) && is_atom(key_b)) {
        return key_a - key_b;
    }
    return erts_cmp(key_a, key_b, 1, 0);
}


#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif
