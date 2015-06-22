/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2014. All Rights Reserved.
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

#ifndef __ERL_TERM_H
#define __ERL_TERM_H

#include "sys.h" /* defines HALFWORD_HEAP */

typedef UWord Wterm;  /* Full word terms */

#if HALFWORD_HEAP
#  define HEAP_ON_C_STACK 0
#  if HALFWORD_ASSERT
#    ifdef ET_DEBUG
#      undef ET_DEBUG
#    endif
#    define ET_DEBUG 1
#  endif
#  if 1
#    define CHECK_POINTER_MASK 0xFFFFFFFF00000000UL
#    define COMPRESS_POINTER(APointer) ((Eterm) (UWord) (APointer))
#    define EXPAND_POINTER(AnEterm) ((UWord) (AnEterm))
#  else
#    define CHECK_POINTER_MASK 0x0UL
#    define COMPRESS_POINTER(AnUint) (AnUint)
#    define EXPAND_POINTER(APointer) (APointer)
#  endif
#else
#  define HEAP_ON_C_STACK 1
#  define CHECK_POINTER_MASK 0x0UL
#  define COMPRESS_POINTER(AnUint) (AnUint)
#  define EXPAND_POINTER(APointer) (APointer)
#endif

struct erl_node_; /* Declared in erl_node_tables.h */

/*
 * Defining ET_DEBUG to 1 causes all type-specific data access
 * macros to perform runtime type checking. This is very useful
 * during development but reduces performance, so ET_DEBUG should
 * be disabled during benchmarking or release.
 */
/* #define ET_DEBUG 1 */
#ifndef ET_DEBUG
#  ifdef DEBUG
#    define ET_DEBUG 1
#  else
#    define ET_DEBUG 0
#  endif
#endif

#if ET_DEBUG
#define _ET_DECLARE_CHECKED(TF,F,TX) extern TF checked_##F(TX,const char*,unsigned);
#define _ET_APPLY(F,X)	checked_##F(X,__FILE__,__LINE__)
#else
#define _ET_DECLARE_CHECKED(TF,F,TX)
#define _ET_APPLY(F,X)	_unchecked_##F(X)
#endif

#define _TAG_PRIMARY_SIZE	2
#define _TAG_PRIMARY_MASK	0x3
#define TAG_PRIMARY_HEADER	0x0
#define TAG_PRIMARY_LIST	0x1
#define TAG_PRIMARY_BOXED	0x2
#define TAG_PRIMARY_IMMED1	0x3

#define primary_tag(x)	((x) & _TAG_PRIMARY_MASK)

#define _TAG_IMMED1_SIZE	4
#define _TAG_IMMED1_MASK	0xF
#define _TAG_IMMED1_PID		((0x0 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_PORT	((0x1 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_IMMED2	((0x2 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_SMALL	((0x3 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)

#define _TAG_IMMED2_SIZE	6
#define _TAG_IMMED2_MASK	0x3F
#define _TAG_IMMED2_ATOM	((0x0 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_CATCH	((0x1 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_NIL		((0x3 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)

/*
 * HEADER representation:
 *
 *	aaaaaaaaaaaaaaaaaaaaaaaaaatttt00	arity:26, tag:4
 *
 * HEADER tags:
 *
 *	0000	ARITYVAL
 *      0001    BINARY_AGGREGATE                |
 *	001x	BIGNUM with sign bit		|
 *	0100	REF				|
 *	0101	FUN				| THINGS
 *	0110	FLONUM				|
 *      0111    EXPORT                          |
 *	1000	REFC_BINARY	|		|
 *	1001	HEAP_BINARY	| BINARIES	|
 *	1010	SUB_BINARY	|		|
 *      1011    Not used; see comment below
 *      1100    EXTERNAL_PID  |                 |
 *      1101    EXTERNAL_PORT | EXTERNAL THINGS |
 *      1110    EXTERNAL_REF  |                 |
 *      1111    MAP
 *
 * COMMENTS:
 *
 * - The tag is zero for arityval and non-zero for thing headers.
 * - A single bit differentiates between positive and negative bignums.
 * - If more tags are needed, the REF and and EXTERNAL_REF tags could probably
 *   be combined to one tag.
 *
 * XXX: globally replace XXX_SUBTAG with TAG_HEADER_XXX
 */
#define ARITYVAL_SUBTAG		(0x0 << _TAG_PRIMARY_SIZE) /* TUPLE */
#define BIN_MATCHSTATE_SUBTAG	(0x1 << _TAG_PRIMARY_SIZE) 
#define POS_BIG_SUBTAG		(0x2 << _TAG_PRIMARY_SIZE) /* BIG: tags 2&3 */
#define NEG_BIG_SUBTAG		(0x3 << _TAG_PRIMARY_SIZE) /* BIG: tags 2&3 */
#define _BIG_SIGN_BIT		(0x1 << _TAG_PRIMARY_SIZE)
#define REF_SUBTAG		(0x4 << _TAG_PRIMARY_SIZE) /* REF */
#define FUN_SUBTAG		(0x5 << _TAG_PRIMARY_SIZE) /* FUN */
#define FLOAT_SUBTAG		(0x6 << _TAG_PRIMARY_SIZE) /* FLOAT */
#define EXPORT_SUBTAG		(0x7 << _TAG_PRIMARY_SIZE) /* FLOAT */
#define _BINARY_XXX_MASK	(0x3 << _TAG_PRIMARY_SIZE)
#define REFC_BINARY_SUBTAG	(0x8 << _TAG_PRIMARY_SIZE) /* BINARY */
#define HEAP_BINARY_SUBTAG	(0x9 << _TAG_PRIMARY_SIZE) /* BINARY */
#define SUB_BINARY_SUBTAG	(0xA << _TAG_PRIMARY_SIZE) /* BINARY */
/*   _BINARY_XXX_MASK depends on 0xB being unused */
#define EXTERNAL_PID_SUBTAG	(0xC << _TAG_PRIMARY_SIZE) /* EXTERNAL_PID */
#define EXTERNAL_PORT_SUBTAG	(0xD << _TAG_PRIMARY_SIZE) /* EXTERNAL_PORT */
#define EXTERNAL_REF_SUBTAG	(0xE << _TAG_PRIMARY_SIZE) /* EXTERNAL_REF */
#define MAP_SUBTAG		(0xF << _TAG_PRIMARY_SIZE) /* MAP */


#define _TAG_HEADER_ARITYVAL       (TAG_PRIMARY_HEADER|ARITYVAL_SUBTAG)
#define _TAG_HEADER_FUN	           (TAG_PRIMARY_HEADER|FUN_SUBTAG)
#define _TAG_HEADER_POS_BIG        (TAG_PRIMARY_HEADER|POS_BIG_SUBTAG)
#define _TAG_HEADER_NEG_BIG        (TAG_PRIMARY_HEADER|NEG_BIG_SUBTAG)
#define _TAG_HEADER_FLOAT          (TAG_PRIMARY_HEADER|FLOAT_SUBTAG)
#define _TAG_HEADER_EXPORT         (TAG_PRIMARY_HEADER|EXPORT_SUBTAG)
#define _TAG_HEADER_REF            (TAG_PRIMARY_HEADER|REF_SUBTAG)
#define _TAG_HEADER_REFC_BIN       (TAG_PRIMARY_HEADER|REFC_BINARY_SUBTAG)
#define _TAG_HEADER_HEAP_BIN       (TAG_PRIMARY_HEADER|HEAP_BINARY_SUBTAG)
#define _TAG_HEADER_SUB_BIN        (TAG_PRIMARY_HEADER|SUB_BINARY_SUBTAG)
#define _TAG_HEADER_EXTERNAL_PID   (TAG_PRIMARY_HEADER|EXTERNAL_PID_SUBTAG)
#define _TAG_HEADER_EXTERNAL_PORT  (TAG_PRIMARY_HEADER|EXTERNAL_PORT_SUBTAG)
#define _TAG_HEADER_EXTERNAL_REF   (TAG_PRIMARY_HEADER|EXTERNAL_REF_SUBTAG)
#define _TAG_HEADER_BIN_MATCHSTATE (TAG_PRIMARY_HEADER|BIN_MATCHSTATE_SUBTAG)
#define _TAG_HEADER_MAP	           (TAG_PRIMARY_HEADER|MAP_SUBTAG)


#define _TAG_HEADER_MASK	0x3F
#define _HEADER_SUBTAG_MASK	0x3C	/* 4 bits for subtag */
#define _HEADER_ARITY_OFFS	6

#define header_is_transparent(x) \
 (((x) & (_HEADER_SUBTAG_MASK)) == ARITYVAL_SUBTAG)
#define header_is_arityval(x)	(((x) & _HEADER_SUBTAG_MASK) == ARITYVAL_SUBTAG)
#define header_is_thing(x)	(!header_is_transparent((x)))
#define header_is_bin_matchstate(x)	((((x) & (_HEADER_SUBTAG_MASK)) == BIN_MATCHSTATE_SUBTAG))

#define _CPMASK		0x3

/* immediate object access methods */
#define is_immed(x)		(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_IMMED1)
#define is_not_immed(x)		(!is_immed((x)))
#define IS_CONST(x)		is_immed((x))
#if TAG_PRIMARY_IMMED1 == _TAG_PRIMARY_MASK
#define is_both_immed(x,y)	is_immed(((x)&(y)))
#else
#define is_both_immed(x,y)	(is_immed((x)) && is_immed((y)))
#endif
#define is_not_both_immed(x,y)	(!is_both_immed((x),(y)))


/* boxed object access methods */
#if HALFWORD_HEAP
#define _is_taggable_pointer(x)	 (((UWord)(x) & (CHECK_POINTER_MASK | 0x3)) == 0)
#define _boxed_precond(x)        (is_boxed(x))
#else
#define _is_taggable_pointer(x)	 (((Uint)(x) & 0x3) == 0)
#define  _boxed_precond(x)       (is_boxed(x))
#endif
#define _is_aligned(x)		(((Uint)(x) & 0x3) == 0)
#define _unchecked_make_boxed(x) ((Uint) COMPRESS_POINTER(x) + TAG_PRIMARY_BOXED)
_ET_DECLARE_CHECKED(Eterm,make_boxed,const Eterm*)
#define make_boxed(x)		_ET_APPLY(make_boxed,(x))
#if 1
#define _is_not_boxed(x)	((x) & (_TAG_PRIMARY_MASK-TAG_PRIMARY_BOXED))
#define _unchecked_is_boxed(x)	(!_is_not_boxed((x)))
_ET_DECLARE_CHECKED(int,is_boxed,Eterm)
#define is_boxed(x)		_ET_APPLY(is_boxed,(x))
#else
#define is_boxed(x)		(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_BOXED)
#endif
#define _unchecked_boxed_val(x) ((Eterm*) EXPAND_POINTER(((x) - TAG_PRIMARY_BOXED)))
_ET_DECLARE_CHECKED(Eterm*,boxed_val,Wterm)
#define boxed_val(x)		_ET_APPLY(boxed_val,(x))

/* cons cell ("list") access methods */
#define _unchecked_make_list(x)	((Uint) COMPRESS_POINTER(x) + TAG_PRIMARY_LIST)
_ET_DECLARE_CHECKED(Eterm,make_list,const Eterm*)
#define make_list(x)		_ET_APPLY(make_list,(x))
#if 1
#define _unchecked_is_not_list(x) ((x) & (_TAG_PRIMARY_MASK-TAG_PRIMARY_LIST))
_ET_DECLARE_CHECKED(int,is_not_list,Eterm)
#define is_not_list(x)		_ET_APPLY(is_not_list,(x))
#define is_list(x)		(!is_not_list((x)))
#else
#define is_list(x)		(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_LIST)
#define is_not_list(x)		(!is_list((x)))
#endif
#if HALFWORD_HEAP
#define _list_precond(x)        (is_list(x))
#else
#define _list_precond(x)       (is_list(x))
#endif
#define _unchecked_list_val(x) ((Eterm*) EXPAND_POINTER((x) - TAG_PRIMARY_LIST))
_ET_DECLARE_CHECKED(Eterm*,list_val,Wterm)
#define list_val(x)		_ET_APPLY(list_val,(x))

#define CONS(hp, car, cdr) \
        (CAR(hp)=(car), CDR(hp)=(cdr), make_list(hp))

#define CAR(x)  ((x)[0])
#define CDR(x)  ((x)[1])

/* generic tagged pointer (boxed or list) access methods */
#define _unchecked_ptr_val(x)	((Eterm*) EXPAND_POINTER((x) & ~((Uint) 0x3)))
#define ptr_val(x)		_unchecked_ptr_val((x))	/*XXX*/
#define _unchecked_offset_ptr(x,offs)	((x)+((offs)*sizeof(Eterm)))
#define offset_ptr(x,offs)	_unchecked_offset_ptr(x,offs)	/*XXX*/
#define _unchecked_byte_offset_ptr(x,byte_offs)	((x)+(offs))
#define byte_offset_ptr(x,offs) _unchecked_byte_offset_ptr(x,offs)  /*XXX*/

/* fixnum ("small") access methods */
#if defined(ARCH_64) && !HALFWORD_HEAP
#define SMALL_BITS	(64-4)
#define SMALL_DIGITS	(17)
#else
#define SMALL_BITS	(28)
#define SMALL_DIGITS	(8)
#endif
#define MAX_SMALL	((SWORD_CONSTANT(1) << (SMALL_BITS-1))-1)
#define MIN_SMALL	(-(SWORD_CONSTANT(1) << (SMALL_BITS-1)))
#define make_small(x)	(((Uint)(x) << _TAG_IMMED1_SIZE) + _TAG_IMMED1_SMALL)
#define is_small(x)	(((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_SMALL)
#define is_not_small(x)	(!is_small((x)))
#define is_byte(x)	(((x) & ((~(Uint)0 << (_TAG_IMMED1_SIZE+8)) + _TAG_IMMED1_MASK)) == _TAG_IMMED1_SMALL)
#define is_valid_bit_size(x) (((Sint)(x)) >= 0 && ((x) & 0x7F) == _TAG_IMMED1_SMALL)
#define is_not_valid_bit_size(x) (!is_valid_bit_size((x)))
#define MY_IS_SSMALL(x) (((Uint) ((((x)) >> (SMALL_BITS-1)) + 1)) < 2)
#define _unchecked_unsigned_val(x)	((x) >> _TAG_IMMED1_SIZE)
_ET_DECLARE_CHECKED(Uint,unsigned_val,Eterm)
#define unsigned_val(x)	_ET_APPLY(unsigned_val,(x))
#define _unchecked_signed_val(x)	((Sint)(x) >> _TAG_IMMED1_SIZE)
_ET_DECLARE_CHECKED(Sint,signed_val,Eterm)
#define signed_val(x)	_ET_APPLY(signed_val,(x))

#if _TAG_IMMED1_SMALL == 0x0F
#define is_both_small(x,y) (((x) & (y) & _TAG_IMMED1_MASK) == _TAG_IMMED1_SMALL)
#elif _TAG_IMMED1_SMALL == 0x00
#define is_both_small(x,y) ((((x)|(y)) & _TAG_IMMED1_MASK) == _TAG_IMMED1_SMALL)
#else
#define is_both_small(x,y) (is_small(x) && is_small(y))
#endif

/* NIL access methods */
#define NIL		((~((Uint) 0) << _TAG_IMMED2_SIZE) | _TAG_IMMED2_NIL)
#define is_nil(x)	((x) == NIL)
#define is_not_nil(x)	((x) != NIL)

#define MAX_ATOM_INDEX (~(~((Uint) 0) << (sizeof(Uint)*8 - _TAG_IMMED2_SIZE)))

/* atom access methods */
#define make_atom(x)  ((Eterm)(((x) << _TAG_IMMED2_SIZE) + _TAG_IMMED2_ATOM))
#define is_atom(x)	(((x) & _TAG_IMMED2_MASK) == _TAG_IMMED2_ATOM)
#define is_not_atom(x)	(!is_atom(x))
#define _unchecked_atom_val(x)	((x) >> _TAG_IMMED2_SIZE)
_ET_DECLARE_CHECKED(Uint,atom_val,Eterm)
#define atom_val(x)	_ET_APPLY(atom_val,(x))

/* header (arityval or thing) access methods */
#define _make_header(sz,tag) ((Uint)(((Uint)(sz) << _HEADER_ARITY_OFFS) + (tag)))
#define is_header(x)	(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_HEADER)
#define _unchecked_header_arity(x) \
    (is_map_header(x) ? MAP_HEADER_ARITY(x) : ((x) >> _HEADER_ARITY_OFFS))
_ET_DECLARE_CHECKED(Uint,header_arity,Eterm)
#define header_arity(x)	_ET_APPLY(header_arity,(x))

/* arityval access methods */
/* Erlang Spec. 4.7.3 defines max arity to 65535
 * we will however enforce max arity of 16777215 (24 bits)
 * (checked in bifs and asserted in debug)
 */
#define MAX_ARITYVAL            ((((Uint)1) << 24) - 1)
#define ERTS_MAX_TUPLE_SIZE     MAX_ARITYVAL

#define make_arityval(sz)	_make_header((sz),_TAG_HEADER_ARITYVAL)
#define is_arity_value(x)	(((x) & _TAG_HEADER_MASK) == _TAG_HEADER_ARITYVAL)
#define is_sane_arity_value(x)	((((x) & _TAG_HEADER_MASK) == _TAG_HEADER_ARITYVAL) && \
				 (((x) >> _HEADER_ARITY_OFFS) <= MAX_ARITYVAL))
#define is_not_arity_value(x)	(!is_arity_value((x)))
#define _unchecked_arityval(x)	_unchecked_header_arity((x))
_ET_DECLARE_CHECKED(Uint,arityval,Eterm)
#define arityval(x)		_ET_APPLY(arityval,(x))

/* thing access methods */
#define is_thing(x)	(is_header((x)) && header_is_thing((x)))
#define is_thing_ptr(t) (is_thing((t)->header))
#define _unchecked_thing_arityval(x)	_unchecked_header_arity((x))
_ET_DECLARE_CHECKED(Uint,thing_arityval,Eterm)
#define thing_arityval(x)	_ET_APPLY(thing_arityval,(x))
#define _unchecked_thing_subtag(x)	((x) & _HEADER_SUBTAG_MASK)
_ET_DECLARE_CHECKED(Uint,thing_subtag,Eterm)
#define thing_subtag(x)		_ET_APPLY(thing_subtag,(x))

/*
 * Magic non-value object.
 * Used as function return error and "absent value" indicator
 * in the original runtime system. The new runtime system also
 * uses it as forwarding marker for CONS cells.
 *
 * This value is 0 in the original runtime system, which unfortunately
 * promotes sloppy programming practices. It also prevents some useful
 * tag assignment schemes, e.g. using a 2-bit tag 00 for FIXNUM.
 *
 * To help find code which makes unwarranted assumptions about zero,
 * we now use a non-zero bit-pattern in debug mode.
 */
#if ET_DEBUG
# ifdef HIPE
   /* A very large (or negative) value as work-around for ugly hipe-bifs
      that return untagged integers (eg hipe_bs_put_utf8) */
#  define THE_NON_VALUE	_make_header((Uint)~0,_TAG_HEADER_FLOAT)
# else
#  define THE_NON_VALUE	_make_header(0,_TAG_HEADER_FLOAT)
# endif
#else
#define THE_NON_VALUE	(0)
#endif
#define is_non_value(x)	((x) == THE_NON_VALUE)
#define is_value(x)	((x) != THE_NON_VALUE)

/* binary object access methods */
#define is_binary_header(x) \
	((((x) & (_TAG_HEADER_MASK)) == _TAG_HEADER_REFC_BIN) || \
	 (((x) & (_TAG_HEADER_MASK)) == _TAG_HEADER_HEAP_BIN) || \
	 (((x) & (_TAG_HEADER_MASK)) == _TAG_HEADER_SUB_BIN))

#define make_binary(x)	make_boxed((Eterm*)(x))
#define is_binary(x)	(is_boxed((x)) && is_binary_header(*boxed_val((x))))
#define is_not_binary(x) (!is_binary((x)))
#define _unchecked_binary_val(x) _unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,binary_val,Wterm)
#define binary_val(x)	_ET_APPLY(binary_val,(x))

/* process binaries stuff (special case of binaries) */
#define HEADER_PROC_BIN	_make_header(PROC_BIN_SIZE-1,_TAG_HEADER_REFC_BIN)

/* fun & export objects */
#define is_any_fun(x)   (is_fun((x)) || is_export((x)))
#define is_not_any_fun(x) (!is_any_fun((x)))

/* fun objects */
#define HEADER_FUN		_make_header(ERL_FUN_SIZE-2,_TAG_HEADER_FUN)
#define is_fun_header(x)	((x) == HEADER_FUN)
#define make_fun(x)		make_boxed((Eterm*)(x))
#define is_fun(x)		(is_boxed((x)) && is_fun_header(*boxed_val((x))))
#define is_not_fun(x)		(!is_fun((x)))
#define _unchecked_fun_val(x)   _unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,fun_val,Wterm)
#define fun_val(x)		_ET_APPLY(fun_val,(x))

/* export access methods */
#define make_export(x)	 make_boxed((x))
#define is_export(x)     (is_boxed((x)) && is_export_header(*boxed_val((x))))
#define is_not_export(x) (!is_export((x)))
#define _unchecked_export_val(x)   _unchecked_boxed_val(x)
_ET_DECLARE_CHECKED(Eterm*,export_val,Wterm)
#define export_val(x)	_ET_APPLY(export_val,(x))
#define is_export_header(x)	((x) == HEADER_EXPORT)
#if HALFWORD_HEAP
#define HEADER_EXPORT   _make_header(2,_TAG_HEADER_EXPORT)
#else
#define HEADER_EXPORT   _make_header(1,_TAG_HEADER_EXPORT)
#endif

/* bignum access methods */
#define make_pos_bignum_header(sz)	_make_header((sz),_TAG_HEADER_POS_BIG)
#define make_neg_bignum_header(sz)	_make_header((sz),_TAG_HEADER_NEG_BIG)
#define _is_bignum_header(x)	(((x) & (_TAG_HEADER_MASK-_BIG_SIGN_BIT)) == _TAG_HEADER_POS_BIG)
#define _unchecked_bignum_header_is_neg(x)	((x) & _BIG_SIGN_BIT)
_ET_DECLARE_CHECKED(int,bignum_header_is_neg,Eterm)
#define bignum_header_is_neg(x)	_ET_APPLY(bignum_header_is_neg,(x))
#define _unchecked_bignum_header_neg(x)	((x) | _BIG_SIGN_BIT)
_ET_DECLARE_CHECKED(Eterm,bignum_header_neg,Eterm)
#define bignum_header_neg(x)	_ET_APPLY(bignum_header_neg,(x))
#define _unchecked_bignum_header_arity(x)	_unchecked_header_arity((x))
_ET_DECLARE_CHECKED(Uint,bignum_header_arity,Eterm)
#define bignum_header_arity(x)	_ET_APPLY(bignum_header_arity,(x))
#define BIG_ARITY_MAX		((1 << 19)-1)
#define make_big(x)	make_boxed((x))
#define is_big(x)	(is_boxed((x)) && _is_bignum_header(*boxed_val((x))))
#define is_not_big(x)	(!is_big((x)))
#define _unchecked_big_val(x)	_unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,big_val,Wterm)
#define big_val(x)		_ET_APPLY(big_val,(x))

/* flonum ("float") access methods */
#if defined(ARCH_64) && !HALFWORD_HEAP
#define HEADER_FLONUM   _make_header(1,_TAG_HEADER_FLOAT)
#else
#define HEADER_FLONUM	_make_header(2,_TAG_HEADER_FLOAT)
#endif
#define make_float(x)	make_boxed((x))
#define is_float(x)	(is_boxed((x)) && *boxed_val((x)) == HEADER_FLONUM)
#define is_not_float(x)	(!is_float(x))
#define _unchecked_float_val(x)	_unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,float_val,Wterm)
#define float_val(x)	_ET_APPLY(float_val,(x))

/* Float definition for byte and word access */
typedef double ieee754_8;

typedef union float_def
{
    ieee754_8 fd;
    byte   fb[sizeof(ieee754_8)];
    Uint16 fs[sizeof(ieee754_8) / sizeof(Uint16)];
    Uint32 fw[sizeof(ieee754_8) / sizeof(Uint32)];
#if defined(ARCH_64) && !HALFWORD_HEAP
    Uint   fdw;
#endif
} FloatDef;

#if defined(ARCH_64) && !HALFWORD_HEAP

#define FLOAT_VAL_GET_DOUBLE(fval, f) (f).fdw = *((fval)+1)

#define PUT_DOUBLE(f, x)  *(x) = HEADER_FLONUM, \
                          *((x)+1) = (f).fdw
#define GET_DOUBLE_DATA(p, f) (f).fdw = *((Uint *) (p))
#define PUT_DOUBLE_DATA(f,p) *((Uint *) (p)) = (f).fdw
#else
#define FLOAT_VAL_GET_DOUBLE(fval, f) (f).fw[0] = *((fval)+1), \
				      (f).fw[1] = *((fval)+2)

#define PUT_DOUBLE(f, x)  *(x) = HEADER_FLONUM, \
                          *((x)+1) = (f).fw[0], \
			  *((x)+2) = (f).fw[1]
#define GET_DOUBLE_DATA(p, f) (f).fw[0] = *((Uint *) (p)),\
                              (f).fw[1] = *(((Uint *) (p))+1)
#define PUT_DOUBLE_DATA(f,p) *((Uint *) (p)) = (f).fw[0],\
                             *(((Uint *) (p))+1) = (f).fw[1]
#endif

#define GET_DOUBLE(x, f) FLOAT_VAL_GET_DOUBLE(float_val(x), f)

#define DOUBLE_DATA_WORDS (sizeof(ieee754_8)/sizeof(Eterm))
#define FLOAT_SIZE_OBJECT (DOUBLE_DATA_WORDS+1)

/* tuple access methods */
#define make_tuple(x)	make_boxed((x))
#define is_tuple(x)	(is_boxed((x)) && is_arity_value(*boxed_val((x))))
#define is_not_tuple(x)	(!is_tuple((x)))
#define is_tuple_arity(x, a) \
   (is_boxed((x)) && *boxed_val((x)) == make_arityval((a)))
#define is_not_tuple_arity(x, a) (!is_tuple_arity((x),(a)))
#define _unchecked_tuple_val(x)	_unchecked_boxed_val(x)
_ET_DECLARE_CHECKED(Eterm*,tuple_val,Wterm)
#define tuple_val(x)	_ET_APPLY(tuple_val,(x))

#define TUPLE0(t) \
        ((t)[0] = make_arityval(0), \
        make_tuple(t))
#define TUPLE1(t,e1) \
        ((t)[0] = make_arityval(1), \
        (t)[1] = (e1), \
        make_tuple(t))
#define TUPLE2(t,e1,e2) \
        ((t)[0] = make_arityval(2), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        make_tuple(t))
#define TUPLE3(t,e1,e2,e3) \
        ((t)[0] = make_arityval(3), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        make_tuple(t))
#define TUPLE4(t,e1,e2,e3,e4) \
        ((t)[0] = make_arityval(4), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        make_tuple(t))
#define TUPLE5(t,e1,e2,e3,e4,e5) \
        ((t)[0] = make_arityval(5), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
        make_tuple(t))
#define TUPLE6(t,e1,e2,e3,e4,e5,e6) \
        ((t)[0] = make_arityval(6), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
	(t)[6] = (e6), \
        make_tuple(t))

#define TUPLE7(t,e1,e2,e3,e4,e5,e6,e7) \
        ((t)[0] = make_arityval(7), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
	(t)[6] = (e6), \
	(t)[7] = (e7), \
        make_tuple(t))

#define TUPLE8(t,e1,e2,e3,e4,e5,e6,e7,e8) \
        ((t)[0] = make_arityval(8), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
	(t)[6] = (e6), \
	(t)[7] = (e7), \
	(t)[8] = (e8), \
        make_tuple(t))

/* This macro get Size bits starting at low order position Pos
   and adjusts the bits to the right 
   bits are numbered from 0 - (sizeof(Uint)*8-1) */

#define _GETBITS(X,Pos,Size) (((X) >> (Pos)) & ~(~((Uint) 0) << (Size)))

/*
 * Creation in node specific data (pids, ports, refs)
 */

#define _CRE_SIZE  		2

/* MAX value for the creation field in pid, port and reference */
#define MAX_CREATION	(1 << _CRE_SIZE)

/*
 *  PID layout (internal pids):
 *
 *   |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *   |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *   |               |               |               |               |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |n n n n n n n n n n n n n n n n n n n n n n n n n n n n|0 0|1 1|
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *  n : number
 *
 *  Old pid layout:
 *
 *   |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *   |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *   |               |               |               |               |
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *   |s s s|n n n n n n n n n n n n n n n|N N N N N N N N|c c|0 0|1 1|
 *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *  s : serial
 *  n : number
 *  c : creation
 *  N : node number
 *
 */

#define _PID_SER_SIZE		(_PID_DATA_SIZE - _PID_NUM_SIZE)
#define _PID_NUM_SIZE 		15

#define _PID_DATA_SIZE		28
#define _PID_DATA_SHIFT		(_TAG_IMMED1_SIZE)

#define _GET_PID_DATA(X)	_GETBITS((X),_PID_DATA_SHIFT,_PID_DATA_SIZE)
#define _GET_PID_NUM(X)		_GETBITS((X),0,_PID_NUM_SIZE)
#define _GET_PID_SER(X)		_GETBITS((X),_PID_NUM_SIZE,_PID_SER_SIZE)

#define make_pid_data(Ser, Num) \
  ((Uint) ((Ser) << _PID_NUM_SIZE | (Num)))

#define is_internal_pid(x)	(((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_PID)
#define is_not_internal_pid(x)	(!is_internal_pid((x)))

#define _unchecked_internal_pid_node(x) erts_this_node
_ET_DECLARE_CHECKED(struct erl_node_*,internal_pid_node,Eterm)
#define internal_pid_node(x) _ET_APPLY(internal_pid_node,(x))

#define internal_pid_data_words(x) (1)

/* 
 *  PORT layout (internal ports):
 *
 *  |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *  |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *  |               |               |               |               |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |n n n n n n n n n n n n n n n n n n n n n n n n n n n n|0 1|1 1|
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *  n : number
 *
 * Old port layout:
 *
 *  |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *  |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *  |               |               |               |               |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |N N N N N N N N|n n n n n n n n n n n n n n n n n n|c c|0 1|1 1|
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *  s : serial
 *  n : number
 *  c : creation
 *  N : node number
 *
 */
#define _PORT_NUM_SIZE		_PORT_DATA_SIZE

#define _PORT_DATA_SIZE		28
#define _PORT_DATA_SHIFT	(_TAG_IMMED1_SIZE)

#define _GET_PORT_DATA(X)	_GETBITS((X),_PORT_DATA_SHIFT,_PORT_DATA_SIZE)
#define _GET_PORT_NUM(X)	_GETBITS((X), 0, _PORT_NUM_SIZE)


#define is_internal_port(x)	(((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_PORT)
#define is_not_internal_port(x)	(!is_internal_port(x))

#define _unchecked_internal_port_node(x) erts_this_node
_ET_DECLARE_CHECKED(struct erl_node_*,internal_port_node,Eterm)
#define internal_port_node(x) _ET_APPLY(internal_port_node,(x))

#define internal_port_data_words(x) (1)
/*
 *  Ref layout (internal references):
 *
 *  |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *  |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *  |               |               |               |               |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1|0 1 0 0|0 0| Thing
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |0 0 0 0 0 0 0 0 0 0 0 0 0 0|r r r r r r r r r r r r r r r r r r| Data 0
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r| Data 1
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r| Data 2
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *
 *  r : reference number
 *  c : creation
 *
 *
 * Old "heap ref" layout:
 *
 *
 *  |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *  |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *  |               |               |               |               |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1|0 1 0 0|0 0| Thing
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |N N N N N N N N|0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0|c c|0 1 1 1| Head
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |0 0 0 0 0 0 0 0 0 0 0 0 0 0|r r r r r r r r r r r r r r r r r r| Word 0
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r| Word 1
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r r| Word 2
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *  r : reference number
 *  c : creation
 *  N : node index
 *
 * Old "one-word ref" layout:
 *
 *  |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *  |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *  |               |               |               |               |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |N N N N N N N N|r r r r r r r r r r r r r r r r r r|c c|T T T T|
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 *  r : reference number
 *  c : creation
 *  N : node index
 *
 */
#define _REF_NUM_SIZE		18

/* Old maximum number of references in the system */
#define MAX_REFERENCE		(1 << _REF_NUM_SIZE)
#define REF_MASK		(~(~((Uint)0) << _REF_NUM_SIZE))
#define ERTS_MAX_REF_NUMBERS	3
#define ERTS_REF_NUMBERS	ERTS_MAX_REF_NUMBERS

#if defined(ARCH_64) && !HALFWORD_HEAP
#  define ERTS_REF_WORDS	(ERTS_REF_NUMBERS/2 + 1)
#  define ERTS_REF_32BIT_WORDS  (ERTS_REF_NUMBERS+1)
#else
#  define ERTS_REF_WORDS	ERTS_REF_NUMBERS
#  define ERTS_REF_32BIT_WORDS  ERTS_REF_NUMBERS
#endif

typedef struct {
    Eterm      header;
    union {
	Uint32 ui32[ERTS_REF_32BIT_WORDS];
	Uint   ui[ERTS_REF_WORDS];
    } data;
} RefThing;

#define REF_THING_SIZE (sizeof(RefThing)/sizeof(Uint))
#define REF_THING_HEAD_SIZE (sizeof(Eterm)/sizeof(Uint))

#define make_ref_thing_header(DW) \
  _make_header((DW)+REF_THING_HEAD_SIZE-1,_TAG_HEADER_REF)

#if defined(ARCH_64) && !HALFWORD_HEAP

/*
 * Ref layout on a 64-bit little endian machine:
 *
 * 63             31             0
 * +--------------+--------------+
 * |         Thing word          |
 * +--------------+--------------+
 * |  Data 0      | 32-bit arity |
 * +--------------+--------------+
 * |  Data 2      | Data 1       |
 * +--------------+--------------+
 *
 * Data is stored as an Uint32 array with 32-bit arity as first number.
 */

#define write_ref_thing(Hp, R0, R1, R2)					\
do {									\
  ((RefThing *) (Hp))->header  = make_ref_thing_header(ERTS_REF_WORDS);	\
  ((RefThing *) (Hp))->data.ui32[0] = ERTS_REF_NUMBERS;			\
  ((RefThing *) (Hp))->data.ui32[1] = (R0);				\
  ((RefThing *) (Hp))->data.ui32[2] = (R1);				\
  ((RefThing *) (Hp))->data.ui32[3] = (R2);				\
} while (0)

#else

#define write_ref_thing(Hp, R0, R1, R2)					\
do {									\
  ((RefThing *) (Hp))->header  = make_ref_thing_header(ERTS_REF_WORDS);	\
  ((RefThing *) (Hp))->data.ui32[0] = (R0);				\
  ((RefThing *) (Hp))->data.ui32[1] = (R1);				\
  ((RefThing *) (Hp))->data.ui32[2] = (R2);				\
} while (0)

#endif

#define is_ref_thing_header(x)	(((x) & _TAG_HEADER_MASK) == _TAG_HEADER_REF)
#define make_internal_ref(x)	make_boxed((Eterm*)(x))

#define _unchecked_ref_thing_ptr(x) \
  ((RefThing*) _unchecked_internal_ref_val(x))
#define ref_thing_ptr(x) \
  ((RefThing*) internal_ref_val(x))

#define is_internal_ref(x) \
    (_unchecked_is_boxed((x)) && is_ref_thing_header(*boxed_val((x))))

#define is_not_internal_ref(x) \
  (!is_internal_ref((x)))

#define _unchecked_internal_ref_val(x) _unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,internal_ref_val,Wterm)
#define internal_ref_val(x) _ET_APPLY(internal_ref_val,(x))

#define internal_thing_ref_data_words(t) (thing_arityval(*(Eterm*)(t)))
#define _unchecked_internal_ref_data_words(x) \
 (_unchecked_thing_arityval(*_unchecked_internal_ref_val(x)))
_ET_DECLARE_CHECKED(Uint,internal_ref_data_words,Wterm)
#define internal_ref_data_words(x) _ET_APPLY(internal_ref_data_words,(x))

#define internal_thing_ref_data(thing) ((thing)->data.ui32)
#define _unchecked_internal_ref_data(x) (internal_thing_ref_data(_unchecked_ref_thing_ptr(x)))
_ET_DECLARE_CHECKED(Uint32*,internal_ref_data,Wterm)
#define internal_ref_data(x) _ET_APPLY(internal_ref_data,(x))

#define _unchecked_internal_ref_node(x) erts_this_node
_ET_DECLARE_CHECKED(struct erl_node_*,internal_ref_node,Eterm)
#define internal_ref_node(x) _ET_APPLY(internal_ref_node,(x))

/*
 *
 *  External thing layout (external pids, ports, and refs):
 *
 *  |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
 *  |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
 *  |               |               |               |               |
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |A A A A A A A A A A A A A A A A A A A A A A A A A A|t t t t|0 0| Thing
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |E E E E E E E E E E E E E E E E E E E E E E E E E E E E E E E E| ErlNode
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N N| Next
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  |X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X X| Data 0
 *  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *  .                                                               .   .
 *  .                                                               .   .
 *  .                                                               .   .
 *
 *  A : Arity
 *  t : External pid thing tag  (1100)
 *  t : External port thing tag (1101)
 *  t : External ref thing tag  (1110)
 *  N : Next (off_heap) pointer
 *  E : ErlNode pointer
 *  X : Type specific data
 *
 *  External pid and port layout:
 *    External pids and ports only have one data word (Data 0) which has
 *    the same layout as internal pids resp. internal ports.
 *
 *  External refs layout:
 *    External refs has the same layout for the data words as in the internal
 *    ref. 
 *
 */

/* XXX:PaN - this structure is not perfect for halfword heap, it takes
   a lot of memory due to padding, and the array will not begin at the end of the
   structure, as otherwise expected. Be sure to access data.ui32 array and not try
   to do pointer manipulation on an Eterm * to reach the actual data...

   XXX:Sverk - Problem made worse by "one off-heap list" when 'next' pointer
     must align with 'next' in ProcBin, erl_fun_thing and erl_off_heap_header.
*/
typedef struct external_thing_ {
    /*                                 ----+                        */
    Eterm                   header;     /* |                        */
    struct erl_node_*       node;       /*  > External thing head   */
    struct erl_off_heap_header* next;   /* |                        */
    /*                                 ----+                        */
    union {
	Uint32              ui32[1];
	Uint                ui[1];
    } data;
} ExternalThing;

#define EXTERNAL_THING_HEAD_SIZE (sizeof(ExternalThing)/sizeof(Uint) - 1)

#define make_external_pid_header(DW) \
  _make_header((DW)+EXTERNAL_THING_HEAD_SIZE-1,_TAG_HEADER_EXTERNAL_PID)
#define is_external_pid_header(x) \
  (((x) & _TAG_HEADER_MASK) == _TAG_HEADER_EXTERNAL_PID)

#define make_external_port_header(DW) \
  _make_header((DW)+EXTERNAL_THING_HEAD_SIZE-1,_TAG_HEADER_EXTERNAL_PORT)
#define is_external_port_header(x) \
  (((x) & _TAG_HEADER_MASK) == _TAG_HEADER_EXTERNAL_PORT)

#define make_external_ref_header(DW) \
  _make_header((DW)+EXTERNAL_THING_HEAD_SIZE-1,_TAG_HEADER_EXTERNAL_REF)
#define is_external_ref_header(x) \
  (((x) & _TAG_HEADER_MASK) == _TAG_HEADER_EXTERNAL_REF)

#define is_external_header(x) \
  (((x) & (_TAG_HEADER_MASK-_BINARY_XXX_MASK)) == _TAG_HEADER_EXTERNAL_PID \
   && ((x) & _TAG_HEADER_MASK) != _TAG_HEADER_MAP)

#define is_external(x) (is_boxed((x)) && is_external_header(*boxed_val((x))))

#define is_external_pid(x) \
  (is_boxed((x)) && is_external_pid_header(*boxed_val((x))))
#define is_external_port(x) \
    (is_boxed((x)) && is_external_port_header(*boxed_val((x))))

#define is_external_ref(x) (_unchecked_is_boxed((x)) && is_external_ref_header(*boxed_val((x))))

#define _unchecked_is_external(x) \
  (_unchecked_is_boxed((x)) && is_external_header(*_unchecked_boxed_val((x))))

#define is_not_external(x)	(!is_external((x)))
#define is_not_external_pid(x)	(!is_external_pid((x)))
#define is_not_external_port(x)	(!is_external_port((x)))
#define is_not_external_ref(x)	(!is_external_ref((x)))


#define make_external(x)		make_boxed((Eterm *) (x))

#define make_external_pid		make_external
#define make_external_port		make_external
#define make_external_ref		make_external

#define _unchecked_external_val(x) _unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,external_val,Wterm)
#define external_val(x) _ET_APPLY(external_val,(x))

#define external_thing_ptr(x) ((ExternalThing *) external_val((x)))
#define _unchecked_external_thing_ptr(x) \
  ((ExternalThing *) _unchecked_external_val((x)))

#define _unchecked_external_thing_data_words(thing) \
    (_unchecked_thing_arityval((thing)->header) + (1 - EXTERNAL_THING_HEAD_SIZE))
_ET_DECLARE_CHECKED(Uint,external_thing_data_words,ExternalThing*)
#define external_thing_data_words(thing) _ET_APPLY(external_thing_data_words,(thing))

#define _unchecked_external_data_words(x) \
    _unchecked_external_thing_data_words(_unchecked_external_thing_ptr((x)))
_ET_DECLARE_CHECKED(Uint,external_data_words,Wterm)
#define external_data_words(x) _ET_APPLY(external_data_words,(x))

#define _unchecked_external_data(x) (_unchecked_external_thing_ptr((x))->data.ui)
#define _unchecked_external_node(x) (_unchecked_external_thing_ptr((x))->node)

#define external_data(x) (external_thing_ptr((x))->data.ui)
#define external_node(x) (external_thing_ptr((x))->node)

#define _unchecked_external_pid_data_words(x) \
  _unchecked_external_data_words((x))
_ET_DECLARE_CHECKED(Uint,external_pid_data_words,Wterm)
#define external_pid_data_words(x) _ET_APPLY(external_pid_data_words,(x))

#define _unchecked_external_pid_data(x) _unchecked_external_data((x))[0]
_ET_DECLARE_CHECKED(Uint,external_pid_data,Wterm)
#define external_pid_data(x) _ET_APPLY(external_pid_data,(x))

#define _unchecked_external_pid_node(x) _unchecked_external_node((x))
_ET_DECLARE_CHECKED(struct erl_node_*,external_pid_node,Wterm)
#define external_pid_node(x) _ET_APPLY(external_pid_node,(x))

#define external_pid_number(x) _GET_PID_NUM(external_pid_data((x)))
#define external_pid_serial(x) _GET_PID_SER(external_pid_data((x)))

#define _unchecked_external_port_data_words(x) \
  _unchecked_external_data_words((x))
_ET_DECLARE_CHECKED(Uint,external_port_data_words,Wterm)
#define external_port_data_words(x) _ET_APPLY(external_port_data_words,(x))

#define _unchecked_external_port_data(x) _unchecked_external_data((x))[0]
_ET_DECLARE_CHECKED(Uint,external_port_data,Wterm)
#define external_port_data(x) _ET_APPLY(external_port_data,(x))

#define _unchecked_external_port_node(x) _unchecked_external_node((x))
_ET_DECLARE_CHECKED(struct erl_node_*,external_port_node,Wterm)
#define external_port_node(x) _ET_APPLY(external_port_node,(x))

#define external_port_number(x) _GET_PORT_NUM(external_port_data((x)))

#define _unchecked_external_ref_data_words(x) \
  _unchecked_external_data_words((x))
_ET_DECLARE_CHECKED(Uint,external_ref_data_words,Wterm)
#define external_ref_data_words(x) _ET_APPLY(external_ref_data_words,(x))
#define external_thing_ref_data_words(thing) external_thing_data_words(thing)

#define _unchecked_external_ref_data(x) (_unchecked_external_thing_ptr((x))->data.ui32)
_ET_DECLARE_CHECKED(Uint32*,external_ref_data,Wterm)
#define external_ref_data(x) _ET_APPLY(external_ref_data,(x))
#define external_thing_ref_data(thing) ((thing)->data.ui32)

#define _unchecked_external_ref_node(x) _unchecked_external_node((x))
_ET_DECLARE_CHECKED(struct erl_node_*,external_ref_node,Eterm)
#define external_ref_node(x) _ET_APPLY(external_ref_node,(x))

/* maps */

#define MAP_HEADER_TAG_SZ                 (2)
#define MAP_HEADER_ARITY_SZ               (8)
#define MAP_HEADER_VAL_SZ                 (16)

#define MAP_HEADER_TAG_FLATMAP_HEAD       (0x0)
#define MAP_HEADER_TAG_HAMT_NODE_BITMAP   (0x1)
#define MAP_HEADER_TAG_HAMT_HEAD_ARRAY    (0x2)
#define MAP_HEADER_TAG_HAMT_HEAD_BITMAP   (0x3)

#define MAP_HEADER_TYPE(Hdr)  (((Hdr) >> (_HEADER_ARITY_OFFS)) & (0x3))
#define MAP_HEADER_ARITY(Hdr) (((Hdr) >> (_HEADER_ARITY_OFFS + MAP_HEADER_TAG_SZ)) & (0xff))
#define MAP_HEADER_VAL(Hdr)   (((Hdr) >> (_HEADER_ARITY_OFFS + MAP_HEADER_TAG_SZ + MAP_HEADER_ARITY_SZ)) & (0xffff))

#define make_hashmap(x)		      make_boxed((Eterm*)(x))
#define make_hashmap_rel 	      make_boxed_rel
#define is_hashmap(x)		      (is_boxed((x)) && is_hashmap_header(*boxed_val((x))))
#define is_not_hashmap(x)             (!is_hashmap(x))
#define is_hashmap_rel(RTERM,BASE)    is_hashmap(rterm2wterm(RTERM,BASE))
#define is_hashmap_header(x)	      (((x) & (_HEADER_MAP_HASHMAP_HEAD_MASK)) == HAMT_SUBTAG_HEAD_ARRAY)
#define hashmap_val(x)		      _unchecked_boxed_val((x))
#define hashmap_val_rel(RTERM, BASE)  hashmap_val(rterm2wterm(RTERM, BASE))

#define make_flatmap(x)               make_boxed((Eterm*)(x))
#define make_flatmap_rel(x, BASE)     make_boxed_rel((Eterm*)(x),(BASE))
#define is_flatmap(x)                 (is_boxed((x)) && is_flatmap_header(*boxed_val((x))))
#define is_flatmap_rel(RTERM,BASE)    is_flatmap(rterm2wterm(RTERM,BASE))
#define is_not_flatmap(x)             (!is_flatmap((x)))
#define is_flatmap_header(x)          (((x) & (_HEADER_MAP_SUBTAG_MASK)) == HAMT_SUBTAG_HEAD_FLATMAP)
#define flatmap_val(x)                (_unchecked_boxed_val((x)))
#define flatmap_val_rel(RTERM, BASE)  flatmap_val(rterm2wterm(RTERM, BASE))

#define is_map_header(x)       (((x) & (_TAG_HEADER_MASK)) == _TAG_HEADER_MAP)
#define is_map(x)              (is_boxed((x)) && is_map_header(*boxed_val(x)))
#define is_not_map(x)          (!is_map(x))
#define is_map_rel(RTERM,BASE) is_map(rterm2wterm(RTERM,BASE))

/* number tests */

#define is_integer(x)		(is_small(x) || is_big(x))
#define is_not_integer(x)	(!is_integer(x))
#define is_number(x)		(is_integer(x) || is_float(x))

#define SMALL_MINUS_ONE	make_small(-1)
#define SMALL_ZERO	make_small(0)
#define SMALL_ONE	make_small(1)

#define ENULL		0

/* on some architectures CP contains labels which are not aligned */
#ifdef NOT_ALIGNED
#error "fix yer arch, like"
#endif

#define _unchecked_make_cp(x)	((Eterm) COMPRESS_POINTER(x))
_ET_DECLARE_CHECKED(Eterm,make_cp,BeamInstr*)
#define make_cp(x)	_ET_APPLY(make_cp,(x))

#define is_not_CP(x)	((x) & _CPMASK)
#define is_CP(x)	(!is_not_CP(x))

#define _unchecked_cp_val(x)	((BeamInstr*) EXPAND_POINTER(x))
_ET_DECLARE_CHECKED(BeamInstr*,cp_val,Eterm)
#define cp_val(x)	_ET_APPLY(cp_val,(x))

#define make_catch(x)	(((x) << _TAG_IMMED2_SIZE) | _TAG_IMMED2_CATCH)
#define is_catch(x)	(((x) & _TAG_IMMED2_MASK) == _TAG_IMMED2_CATCH)
#define is_not_catch(x)	(!is_catch(x))
#define _unchecked_catch_val(x)	((x) >> _TAG_IMMED2_SIZE)
_ET_DECLARE_CHECKED(Uint,catch_val,Eterm)
#define catch_val(x)	_ET_APPLY(catch_val,(x))

#define make_blank(X)	((X) = NIL)

/*
 * Overloaded tags.
 *
 * SMALL = 15
 * ATOM/NIL=7
 *
 * Note that the two least significant bits in SMALL/ATOM/NIL always are 3;
 * thus, we can distinguish register from literals by looking at only these
 * two bits.
 */

#define X_REG_DEF	0
#define Y_REG_DEF	1
#define R_REG_DEF	2

#define beam_reg_tag(x)	((x) & 3)

#define make_rreg()	R_REG_DEF
#define make_xreg(ix)	(((ix) * sizeof(Eterm)) | X_REG_DEF)
#define make_yreg(ix)	(((ix) * sizeof(Eterm)) | Y_REG_DEF)

#define _is_xreg(x)	(beam_reg_tag(x) == X_REG_DEF)
#define _is_yreg(x)	(beam_reg_tag(x) == Y_REG_DEF)

#define _unchecked_x_reg_offset(R)	((R) - X_REG_DEF)
_ET_DECLARE_CHECKED(Uint,x_reg_offset,Uint)
#define x_reg_offset(R)	_ET_APPLY(x_reg_offset,(R))

#define _unchecked_y_reg_offset(R)	((R) - Y_REG_DEF)
_ET_DECLARE_CHECKED(Uint,y_reg_offset,Uint)
#define y_reg_offset(R)	_ET_APPLY(y_reg_offset,(R))

#define reg_index(R) ((R) / sizeof(Eterm))

#define _unchecked_x_reg_index(R)	((R) >> 2)
_ET_DECLARE_CHECKED(Uint,x_reg_index,Uint)
#define x_reg_index(R)	_ET_APPLY(x_reg_index,(R))

#define _unchecked_y_reg_index(R)	((R) >> 2)
_ET_DECLARE_CHECKED(Uint,y_reg_index,Uint)
#define y_reg_index(R)	_ET_APPLY(y_reg_index,(R))

/*
 * Backwards compatibility definitions:
 * - #define virtual *_DEF constants with values that fit term order:
 *   number < atom < ref < fun < port < pid < tuple < map < nil < cons < binary
 * - tag_val_def() function generates virtual _DEF tag
 * - not_eq_tags() and NUMBER_CODE() defined in terms
 *   of the tag_val_def() function
 */

#define BINARY_DEF		0x0
#define LIST_DEF		0x1
#define NIL_DEF			0x2
#define MAP_DEF			0x3
#define TUPLE_DEF		0x4
#define PID_DEF			0x5
#define EXTERNAL_PID_DEF	0x6
#define PORT_DEF		0x7
#define EXTERNAL_PORT_DEF	0x8
#define EXPORT_DEF		0x9
#define FUN_DEF			0xa
#define REF_DEF			0xb
#define EXTERNAL_REF_DEF	0xc
#define ATOM_DEF		0xd
#define FLOAT_DEF		0xe
#define BIG_DEF			0xf
#define SMALL_DEF		0x10

#define FIRST_VACANT_TAG_DEF    0x11

#if ET_DEBUG
extern unsigned tag_val_def_debug(Wterm, const char*, unsigned);
#define tag_val_def(x)	tag_val_def_debug((x),__FILE__,__LINE__)
#else
extern unsigned tag_val_def(Wterm);
#endif
#define not_eq_tags(X,Y)	(tag_val_def((X)) ^ tag_val_def((Y)))

#define NUMBER_CODE(x,y)	((tag_val_def(x) << 5) | tag_val_def(y))
#define _NUMBER_CODE(TX,TY)	((TX << 5) | TY)
#define SMALL_SMALL	_NUMBER_CODE(SMALL_DEF,SMALL_DEF)
#define SMALL_BIG 	_NUMBER_CODE(SMALL_DEF,BIG_DEF)
#define SMALL_FLOAT 	_NUMBER_CODE(SMALL_DEF,FLOAT_DEF)
#define BIG_SMALL 	_NUMBER_CODE(BIG_DEF,SMALL_DEF)
#define BIG_BIG 	_NUMBER_CODE(BIG_DEF,BIG_DEF)
#define BIG_FLOAT 	_NUMBER_CODE(BIG_DEF,FLOAT_DEF)
#define FLOAT_SMALL 	_NUMBER_CODE(FLOAT_DEF,SMALL_DEF)
#define FLOAT_BIG 	_NUMBER_CODE(FLOAT_DEF,BIG_DEF)
#define FLOAT_FLOAT	_NUMBER_CODE(FLOAT_DEF,FLOAT_DEF)

#if HALFWORD_HEAP
#define ptr2rel(PTR,BASE) ((Eterm*)((char*)(PTR) - (char*)(BASE)))
#define rterm2wterm(REL,BASE) ((Wterm)(REL) + (Wterm)(BASE))

#else /* HALFWORD_HEAP */

#define ptr2rel(PTR,BASE) (PTR)
#define rterm2wterm(REL,BASE) (REL)

#endif /* !HALFWORD_HEAP */

#define make_list_rel(PTR, BASE) make_list(ptr2rel(PTR,BASE))
#define make_boxed_rel(PTR, BASE) make_boxed(ptr2rel(PTR,BASE))
#define make_fun_rel make_boxed_rel
#define make_binary_rel make_boxed_rel
#define make_tuple_rel make_boxed_rel
#define make_external_rel make_boxed_rel
#define make_internal_ref_rel make_boxed_rel
#define make_big_rel make_boxed_rel

#define binary_val_rel(RTERM, BASE) binary_val(rterm2wterm(RTERM, BASE))
#define list_val_rel(RTERM, BASE) list_val(rterm2wterm(RTERM, BASE))
#define boxed_val_rel(RTERM, BASE) boxed_val(rterm2wterm(RTERM, BASE))
#define tuple_val_rel(RTERM, BASE) tuple_val(rterm2wterm(RTERM, BASE))
#define export_val_rel(RTERM, BASE) export_val(rterm2wterm(RTERM, BASE))
#define fun_val_rel(RTERM, BASE) fun_val(rterm2wterm(RTERM, BASE))
#define big_val_rel(RTERM,BASE)	big_val(rterm2wterm(RTERM,BASE))
#define float_val_rel(RTERM,BASE) float_val(rterm2wterm(RTERM,BASE))
#define internal_ref_val_rel(RTERM,BASE) internal_ref_val(rterm2wterm(RTERM,BASE))

#define external_thing_ptr_rel(RTERM, BASE) external_thing_ptr(rterm2wterm(RTERM, BASE))
#define external_data_words_rel(RTERM,BASE) external_data_words(rterm2wterm(RTERM,BASE))

#define external_port_node_rel(RTERM,BASE) external_port_node(rterm2wterm(RTERM,BASE))
#define external_port_data_rel(RTERM,BASE) external_port_data(rterm2wterm(RTERM,BASE))

#define is_external_pid_rel(RTERM,BASE) is_external_pid(rterm2wterm(RTERM,BASE))
#define external_pid_node_rel(RTERM,BASE) external_pid_node(rterm2wterm(RTERM,BASE))
#define external_pid_data_rel(RTERM,BASE) external_pid_data(rterm2wterm(RTERM,BASE))

#define is_binary_rel(RTERM,BASE) is_binary(rterm2wterm(RTERM,BASE))
#define is_float_rel(RTERM,BASE) is_float(rterm2wterm(RTERM,BASE))
#define is_fun_rel(RTERM,BASE) is_fun(rterm2wterm(RTERM,BASE))
#define is_big_rel(RTERM,BASE) is_big(rterm2wterm(RTERM,BASE))
#define is_export_rel(RTERM,BASE) is_export(rterm2wterm(RTERM,BASE))
#define is_tuple_rel(RTERM,BASE) is_tuple(rterm2wterm(RTERM,BASE))

#define GET_DOUBLE_REL(RTERM, f, BASE) GET_DOUBLE(rterm2wterm(RTERM,BASE), f)

#define ref_thing_ptr_rel(RTERM,BASE) ref_thing_ptr(rterm2wterm(RTERM,BASE))
#define is_internal_ref_rel(RTERM,BASE) is_internal_ref(rterm2wterm(RTERM,BASE))
#define is_external_rel(RTERM,BASE) is_external(rterm2wterm(RTERM,BASE))
#define is_external_port_rel(RTERM,BASE) is_external_port(rterm2wterm(RTERM,BASE))
#define is_external_ref_rel(RTERM,BASE) is_external_ref(rterm2wterm(RTERM,BASE))

#define external_node_rel(RTERM,BASE) external_node(rterm2wterm(RTERM,BASE))


#if HALFWORD_HEAP
ERTS_GLB_INLINE int is_same(Eterm a, Eterm* a_base, Eterm b, Eterm* b_base);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE int is_same(Eterm a, Eterm* a_base, Eterm b, Eterm* b_base)
{
    /* If bases differ, assume a and b are on different "heaps",
       ie can only be same if immed */
    ASSERT(a_base == b_base || is_immed(a) || is_immed(b)
	   || rterm2wterm(a,a_base) != rterm2wterm(b,b_base));

    return a == b && (a_base == b_base || is_immed(a));
}
#endif

#else /* !HALFWORD_HEAP */
#define is_same(A,A_BASE,B,B_BASE) ((A)==(B))
#endif

#endif	/* __ERL_TERM_H */

