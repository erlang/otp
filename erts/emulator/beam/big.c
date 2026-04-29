/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "big.h"
#include "error.h"
#include "bif.h"
#include "erl_binary.h"

#define ZERO_DIGITS(v, sz) do {			\
	dsize_t _t_sz = sz;			\
	ErtsDigit* _t_v  = v;			\
	while(_t_sz--) *_t_v++ = 0;		\
    } while(0)

#define MOVE_DIGITS(dst, src, sz) do {				\
	dsize_t _t_sz = sz;					\
	ErtsDigit* _t_dst;					\
	ErtsDigit* _t_src;					\
	if (dst < src) {					\
	    _t_dst = dst;					\
	    _t_src = src;					\
	    while(_t_sz--) *_t_dst++ = *_t_src++;		\
	}							\
	else if (dst > src) {					\
	    _t_dst = (dst)+((sz)-1);				\
	    _t_src = (src)+((sz)-1);				\
	    while(_t_sz--) *_t_dst-- = *_t_src--;		\
        }							\
    } while(0)

/* add a and b with carry out */
#define DSUM(a,b,c,s) do {					\
	ErtsDigit ___xr = (a);					\
	ErtsDigit ___yr = (b);					\
	___xr = ___yr + ___xr;					\
	s = ___xr;						\
	c = (___xr < ___yr);					\
    }  while(0)

#define DSUBb(a,b,r,d) do {						\
	ErtsDigit ___cr = (r);						\
	ErtsDigit ___xr = (a);						\
	ErtsDigit ___yr = (b)+___cr;					\
	___cr = (___yr < ___cr);					\
	___yr = ___xr - ___yr;						\
	___cr += (___yr > ___xr);					\
	d = ___yr;							\
	r = ___cr;							\
    } while(0)

#define DSUB(a,b,r,d) do {			\
	ErtsDigit ___xr = (a);			\
	ErtsDigit ___yr = (b);			\
	___yr = ___xr - ___yr;			\
	r = (___yr > ___xr);			\
	d = ___yr;				\
    } while(0)

/* type a constant as a ErtsDigit - to get shifts correct */
#define DCONST(n) ((ErtsDigit)(n))

/*
 *   BIG_HAVE_DOUBLE_DIGIT is defined if we have defined
 *   the type ErtsDoubleDigit which MUST have 
 *    sizeof(ErtsDoubleDigit) >= sizeof(ErtsDigit)
 */
#ifdef BIG_HAVE_DOUBLE_DIGIT

/* ErtsDoubleDigit => ErtsDigit */
#define DLOW(x)        ((ErtsDigit)(x))
#define DHIGH(x)       ((ErtsDigit)(((ErtsDoubleDigit)(x)) >> D_EXP))

/* ErtsDigit => ErtsDoubleDigit */
#define DLOW2HIGH(x)   (((ErtsDoubleDigit)(x)) << D_EXP)
#define DDIGIT(a1,a0)  (DLOW2HIGH(a1) + (a0))

#define DMULc(a,b,c,p) do {			       \
        ErtsDoubleDigit _t = ((ErtsDoubleDigit)(a))*(b) + (c);	\
	p = DLOW(_t);						\
	c = DHIGH(_t);						\
    } while(0)
#define DMUL(a,b,c1,c0) do { \
	ErtsDoubleDigit _t = ((ErtsDoubleDigit)(a))*(b);	\
	c0 = DLOW(_t);					\
	c1 = DHIGH(_t);					\
    } while(0)

#define DDIV(a1,a0,b,q) do {						\
	ErtsDoubleDigit _t = DDIGIT((a1),(a0));				\
	q = _t / (b);							\
    } while(0)

#define DDIV2(a1,a0,b1,b0,q) do {					\
	ErtsDoubleDigit _t = DDIGIT((a1),(a0));				\
	q = _t / DDIGIT((b1),(b0));					\
    } while(0)

#define DREM(a1,a0,b,r) do { \
	ErtsDoubleDigit _t = DDIGIT((a1),(a0));		\
	r = _t % (b);					\
    } while(0)

/* add a and b with carry in + out */
#define DSUMc(a,b,c,s) do {                                     \
        ErtsDoubleDigit _t = (ErtsDoubleDigit)(a) + (b) + (c);  \
        s = DLOW(_t);                                           \
        c = DHIGH(_t);                                          \
    }  while(0)

#else

/* If we do not have double digit then we have some more work to do */
#define H_EXP (D_EXP >> 1)
#define LO_MASK ((ErtsDigit)((DCONST(1) << H_EXP)-1))
#define HI_MASK ((ErtsDigit)(LO_MASK << H_EXP))

#define DGT(a,b) ((a)>(b))
#define DEQ(a,b) ((a)==(b))

#define D2GT(a1,a0,b1,b0)  (DGT(a1,b1) || (((a1)==(b1)) && DGT(a0,b0)))
#define D2EQ(a1,a0,b1,b0)  (DEQ(a1,b1) && DEQ(a0,b0))
#define D2LT(a1,a0,b1,b0)  D2GT(b1,b0,a1,a0)
#define D2GTE(a1,a0,b1,b0) (!D2LT(a1,a0,b1,b0))
#define D2LTE(a1,a0,b1,b0) (!D2GT(a1,a0,b1,b0))

/* Add (A+B),  A=(a1B+a0) B=(b1B+b0) */
#define D2ADD(a1,a0,b1,b0,c1,c0) do { \
	ErtsDigit __ci = 0;	      \
	DSUM(a0,b0,__ci,c0); \
	DSUMc(a1,b1,__ci,c1);			\
    } while(0)

/* Subtract (A-B), A=(a1B+a0), B=(b1B+b0)  (A>=B) */
#define D2SUB(a1,a0,b1,b0,c1,c0) do { \
	ErtsDigit __bi;		      \
	DSUB(a0,b0,__bi,c0);	      \
	DSUBb(a1,b1,__bi,c1);	      \
    } while(0)


/* Left shift (multiply by 2) (A <<= 1 where A=a1*B+a0)  */
#define D2LSHIFT1(a1,a0) do {		\
	a1 = ((a0) >> (D_EXP-1)) | ((a1)<<1);		\
	a0 = (a0) << 1;					\
    } while(0)

/* Right shift (divide by 2) (A >>= 1 where A=a1*B+a0) */
#define D2RSHIFT1(a1,a0) do {		\
	a0 = (((a1) & 1) << (D_EXP-1)) | ((a0)>>1);	\
	a1 = ((a1) >> 1);				\
    } while(0)

/* Calculate a*b + d1 and store double prec result in d1, d0 */
#define DMULc(a,b,d1,d0) do {					\
	ErtsHalfDigit __a0 = (a);				\
	ErtsHalfDigit __a1 = ((a) >> H_EXP);			\
	ErtsHalfDigit __b0 = (b);				\
	ErtsHalfDigit __b1 = ((b) >> H_EXP);			\
	ErtsDigit __a0b0 = (ErtsDigit)__a0*__b0;		\
	ErtsDigit __a0b1 = (ErtsDigit)__a0*__b1;		\
	ErtsDigit __a1b0 = (ErtsDigit)__a1*__b0;		\
	ErtsDigit __a1b1 = (ErtsDigit)__a1*__b1;		\
	ErtsDigit __p0,__p1,__p2,__c0;				\
	DSUM(__a0b0,d1,__c0,__p0);				\
	DSUM((__c0<<H_EXP),(__p0>>H_EXP),__p2,__p1);		\
	DSUM(__p1,__a0b1,__c0,__p1);				\
	__p2 += __c0;						\
	DSUM(__p1,__a1b0,__c0,__p1);				\
	__p2 += __c0;						\
	DSUM(__p1,__a1b1<<H_EXP,__c0,__p1);			\
	__p2 += __c0;						\
	DSUM(__a1b1, (__p2<<H_EXP),__c0,__p2);			\
	d1 = (__p2 & HI_MASK) | (__p1 >> H_EXP);		\
	d0 = (__p1 << H_EXP) | (__p0 & LO_MASK);		\
    } while(0)

#define DMUL(a,b,d1,d0) do {				\
	ErtsDigit _ds = 0;				\
	DMULc(a,b,_ds,d0);				\
	d1 = _ds;					\
    } while(0)

/* Calculate a*(Bb1 + b0) + d2 = a*b1B + a*b0 + d2 */
#define D2MULc(a,b1,b0,d2,d1,d0) do { \
	DMULc(a, b0, d2, d0);	      \
	DMULc(a, b1, d2, d1);	      \
    } while(0)

/* Calculate s in a = 2^s*a1 */
/* NOTE since D2PF is used by other macros variables is prefixed bt __ */
#if D_EXP == 64
#define D2PF(a, s) do {							\
	ErtsDigit __x = (a);						\
	int __s = 0;							\
        if (__x <= 0x00000000FFFFFFFF) { __s += 32; __x <<= 32; }	\
        if (__x <= 0x0000FFFFFFFFFFFF) { __s += 16; __x <<= 16; }	\
	if (__x <= 0x00FFFFFFFFFFFFFF) { __s += 8;  __x <<= 8;  }	\
	if (__x <= 0x0FFFFFFFFFFFFFFF) { __s += 4;  __x <<= 4;  }	\
	if (__x <= 0x3FFFFFFFFFFFFFFF) { __s += 2;  __x <<= 2;  }	\
	if (__x <= 0x7FFFFFFFFFFFFFFF) { __s += 1; }		\
	s = __s;						\
    } while(0)
#elif D_EXP == 32
#define D2PF(a, s) do {						\
	ErtsDigit __x = (a);					\
	int __s = 0;						\
        if (__x <= 0x0000FFFF) { __s += 16; __x <<= 16; }		\
	if (__x <= 0x00FFFFFF) { __s += 8;  __x <<= 8;  }		\
	if (__x <= 0x0FFFFFFF) { __s += 4;  __x <<= 4;  }		\
	if (__x <= 0x3FFFFFFF) { __s += 2;  __x <<= 2;  }		\
	if (__x <= 0x7FFFFFFF) { __s += 1; }			\
	s = __s;							\
    } while(0)
#elif D_EXP == 16
#define D2PF(a, s) do {					\
	ErtsDigit __x = (a);				\
	int __s = 0;					\
	if (__x <= 0x00FF) { __s += 8; __x <<= 8; }	\
	if (__x <= 0x0FFF) { __s += 4; __x <<= 4; }	\
	if (__x <= 0x3FFF) { __s += 2; __x <<= 2; }	\
	if (__x <= 0x7FFF) { __s += 1; }		\
	s = __s;					\
    } while(0)
#elif D_EXP == 8
#define D2PF(a, s) do {					\
	ErtsDigit __x = (a);				\
	int __s = 0;					\
	if (__x <= 0x0F) { __s += 4; __x <<= 4; }	\
	if (__x <= 0x3F) { __s += 2; __x <<= 2; }	\
	if (__x <= 0x7F) { __s += 1; }			\
	s = _s;						\
    } while(0)
#endif

/* Calculate q = (a1B + a0) / b,  assume a1 < b */
#define DDIVREM(a1,a0,b,q,r) do {					\
	ErtsDigit _a1 = (a1);						\
	ErtsDigit _a0 = (a0);						\
	ErtsDigit _b = (b);						\
	ErtsHalfDigit _un1, _un0;					\
	ErtsHalfDigit _vn1, _vn0;					\
	ErtsDigit _q1, _q0;						\
	ErtsDigit _un32, _un21, _un10;					\
	ErtsDigit _rh;							\
	Sint _s;							\
	D2PF(_b, _s);							\
	_b = _b << _s;							\
	_vn1 = _b >> H_EXP;						\
	_vn0 = _b & LO_MASK;						\
        /* If needed to avoid undefined behaviour */                    \
        if (_s) _un32 = (_a1 << _s) | ((_a0>>(D_EXP-_s)) & (-_s >> (D_EXP-1))); \
        else _un32 = _a1;                                               \
	_un10 = _a0 << _s;						\
	_un1 = _un10 >> H_EXP;						\
	_un0 = _un10 & LO_MASK;						\
	_q1 = _un32/_vn1;						\
	_rh = _un32 - _q1*_vn1;						\
	while ((_q1 >= (DCONST(1)<<H_EXP))||(_q1*_vn0 > (_rh<<H_EXP)+_un1)) {	\
	    _q1--;							\
	    _rh += _vn1;						\
	    if (_rh >= (DCONST(1)<<H_EXP)) break;				\
	}								\
	_un21 = (_un32<<H_EXP) + _un1 - _q1*_b;				\
	_q0 = _un21/_vn1;						\
	_rh = _un21 - _q0*_vn1;						\
	while ((_q0 >= (DCONST(1)<<H_EXP))||(_q0*_vn0 > ((_rh<<H_EXP)+_un0))) {	\
	    _q0--;							\
	    _rh += _vn1;						\
	    if (_rh >= (DCONST(1)<<H_EXP)) break;				\
	}								\
	r = ((_un21<<H_EXP) + _un0 - _q0*_b) >> _s;			\
	q = (_q1<<H_EXP) + _q0;						\
    } while(0)

/* divide any a=(a1*B + a0) with b */
#define DDIVREM2(a1,a0,b,q1,q0,r) do {		\
	ErtsDigit __a1 = (a1);			\
	ErtsDigit __b = (b);			\
	q1 = __a1 / __b;			\
	DDIVREM(__a1 % __b, (a0), __b, q0, r);	\
    } while(0)


/* Calculate q = (a1B + a0) % b */
#define DREM(a1,a0,b,r) do {				\
	ErtsDigit __a1 = (a1);				\
	ErtsDigit __b = (b);				\
	ERTS_DECLARE_DUMMY(ErtsDigit __q0);		\
	DDIVREM((__a1 % __b), (a0), __b, __q0, r);	\
    } while(0)

#define DDIV(a1,a0,b,q)	do {			\
	ERTS_DECLARE_DUMMY(ErtsDigit _tmp);	\
	DDIVREM(a1,a0,b,q,_tmp);		\
    } while(0)


/* Calculate q, r  A = Bq+R when, assume A1 >= B */
#if (SIZEOF_VOID_P == 8)
#define QUOT_LIM 0x7FFFFFFFFFFFFFFF
#else
#define QUOT_LIM 0x7FFFFFFF
#endif

#define D2DIVREM(a1,a0,b1,b0,q0,r1,r0) do {			\
	ErtsDigit _a1 = (a1);					\
	ErtsDigit _a0 = (a0);					\
	ErtsDigit _b1 = (b1);					\
	ErtsDigit _b0 = (b0);					\
	ErtsDigit _q = 0;					\
	int _as = 1;						\
	while(D2GTE(_a1,_a0,_b1,_b0)) {				\
	    ErtsDigit _q1;					\
	    ErtsDigit _t2=0, _t1, _t0;				\
	    if ((_b1 == 1) && (_a1 > 1))			\
		_q1 = _a1 / 2;					\
	    else if ((_a1 > QUOT_LIM) && (_b1 < _a1))      	\
		_q1 = _a1/(_b1+1);				\
	    else						\
		_q1 = _a1/_b1;					\
	    if (_as<0)						\
		_q -= _q1;					\
	    else						\
		_q += _q1;					\
	    D2MULc(_q1, _b1, _b0, _t2, _t1, _t0);		\
            ASSERT(_t2 == 0);                                   \
	    if (D2GT(_t1,_t0,_a1,_a0)) {			\
		D2SUB(_t1,_t0,_a1,_a0,_a1,_a0);			\
		_as = -_as;					\
	    }							\
	    else {						\
		D2SUB(_a1,_a0,_t1,_t0,_a1,_a0);			\
	    }							\
	}							\
	if (_as < 0) {						\
	    _q--;						\
	    D2SUB(_b1,_b0,_a1,_a0,_a1,_a0);			\
	}							\
	q0 = _q;						\
	r1 = _a1;						\
	r0 = _a0;						\
    } while(0)


/* Calculate q, r  A = Bq+R when assume B>0 */
#define D2DIVREM_0(a1,a0,b1,b0,q1,q0,r1,r0) do {	\
	ErtsDigit _a1 = (a1);				\
	ErtsDigit _a0 = (a0);				\
	ErtsDigit _b1 = (b1);				\
	ErtsDigit _b0 = (b0);				\
	if (D2EQ(_a1,_a0,0,0)) {			\
	    q1 = q0 = 0;				\
	    r1 = r0 = 0;				\
	}						\
	else {						\
	    ErtsDigit _res1 = 0;			\
	    ErtsDigit _res0 = 0;			\
	    ErtsDigit _d1 = 0;				\
	    ErtsDigit _d0 = 1;				\
	    ErtsDigit _e1 = (1 << (D_EXP-1));		\
	    ErtsDigit _e0 = 0;				\
	    while(_e1 && !(_a1 & _e1))			\
		_e1 >>= 1;				\
	    if (_e1 == 0) {				\
		_e0 = (1 << (D_EXP-1));			\
		while(_e0 && !(_a0 & _e0))		\
		    _e0 >>= 1;				\
	    }						\
	    if (D2GT(_b1,_b0,0,0)) {			\
		while(D2GT(_e1,_e0,_b1,_b0)) {		\
		    D2LSHIFT1(_b1,_b0);			\
		    D2LSHIFT1(_d1,_d0);			\
		}					\
	    }						\
	    do {					\
		if (!D2GT(_b1,_b0,_a1,_a0)) {		\
		    D2SUB(_a1,_a0, _b1, _b0, _a1, _a0); \
		    D2ADD(_d1,_d0, _res1,_res0, _res1, _res0);	\
		}						\
		D2RSHIFT1(_b1,_b0);			\
		D2RSHIFT1(_d1,_d0);			\
	    } while (!D2EQ(_d1,_d0,0,0));		\
	    r1 = _a1;					\
	    r0 = _a0;					\
	    q1 = _res1;					\
	    q0 = _res0;					\
	}						\
    } while(0)

#define DDIV2(a1,a0,b1,b0,q) do {		\
	ERTS_DECLARE_DUMMY(ErtsDigit _tmp_r1);	\
	ERTS_DECLARE_DUMMY(ErtsDigit _tmp_r0);	 \
	D2DIVREM(a1,a0,b1,b0,q,_tmp_r1,_tmp_r0); \
    } while(0)

/* add a and b with carry in + out */
#define DSUMc(a,b,c,s) do {                     \
        ErtsDigit ___cr = (c);                  \
        ErtsDigit ___xr = (a)+(___cr);          \
        ErtsDigit ___yr = (b);                  \
        ___cr = (___xr < ___cr);                \
        ___xr = ___yr + ___xr;                  \
        ___cr += (___xr < ___yr);               \
        s = ___xr;                              \
        c = ___cr;                              \
    } while(0)

#endif

/* Forward declaration of lookup tables (See below in this file) used in list to
 * integer conversions for different bases. Also used in bignum printing.
 */
static const byte digits_per_sint_lookup[36-1];
static const byte digits_per_small_lookup[36-1];
static const Sint largest_power_of_base_lookup[36-1];
static const double lg2_lookup[36-1];

static ERTS_INLINE byte get_digits_per_signed_int(Uint base) {
    return digits_per_sint_lookup[base-2];
}

static ERTS_INLINE byte get_digits_per_small(Uint base) {
    return digits_per_small_lookup[base-2];
}

static ERTS_INLINE Sint get_largest_power_of_base(Uint base) {
    return largest_power_of_base_lookup[base-2];
}

static ERTS_INLINE double lookup_log2(Uint base) {
    return lg2_lookup[base - 2];
}

#ifdef DEBUG
static ERTS_INLINE int I_is_normalized(ErtsDigit* x, dsize_t xl) {
    return xl == 0 || x[xl - 1] != 0 || (xl == 1 && x[0] == 0);
}
#endif

/*
** compare two number vectors
*/
static int I_comp(ErtsDigit* x, dsize_t xl, ErtsDigit* y, dsize_t yl)
{
    if (xl < yl)
	return -1;
    else if (xl > yl)
	return 1;
    else {
	if (x == y)
	    return 0;
	x += (xl-1);
	y += (yl-1);
	while((xl > 0) && (*x == *y)) {
	    x--;
	    y--;
	    xl--;
	}
	if (xl == 0)
	    return 0;
	return (*x < *y) ? -1 : 1;
    }
}

/*
** Add digits in x and y and store them in r
** assumption: (xl >= yl)
*/
static dsize_t I_add(ErtsDigit* x, dsize_t xl, ErtsDigit* y, dsize_t yl, ErtsDigit* r)
{
    dsize_t sz = xl;
    register ErtsDigit yr, xr;
    register ErtsDigit c = 0;

    ASSERT(xl >= yl);

    xl -= yl;
    do {
        xr = *x++;
        yr = *y++;
        DSUMc(xr, yr, c, xr);
        *r++ = xr;
    } while(--yl);

    while(xl--) {
	xr = *x++ + c;
	c = (xr < c);
	*r++ = xr;
    }
    if (c) {
	*r = 1;
	return sz+1;
    }
    return sz;
}
/*
** Add a digits in v1 and store result in vr
*/
static dsize_t D_add(ErtsDigit* x, dsize_t xl, ErtsDigit c, ErtsDigit* r)
{
    dsize_t sz = xl;
    register ErtsDigit xr;

    while(xl--) {
	xr = *x++ + c;
	c = (xr < c);
	*r++ = xr;
    }
    if (c) {
	*r = 1;
	return sz+1;
    }
    return sz;
}

/*
** Subtract digits v2 from v1 and store result in v3
** Assert  I_comp(x, xl, y, yl) >= 0
**
*/
static dsize_t I_sub(ErtsDigit* x, dsize_t xl, ErtsDigit* y, dsize_t yl, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    register ErtsDigit yr, xr;
    register ErtsDigit c = 0;

    ASSERT(I_comp(x, xl, y, yl) >= 0);

    xl -= yl;
    do {
	yr = *y++ + c;
	xr = *x++;
	c = (yr < c);
	yr = xr - yr;
	c += (yr > xr);
	*r++ = yr;
    } while(--yl);

    while(xl--) {
	xr = *x++;
	yr = xr - c;
	c = (yr > xr);
	*r++ = yr;
    }
    do {
	r--;
    } while(*r == 0 && r != r0);

    return (r - r0) + 1;
}

/*
** Subtract digit d from v1 and store result in vr
*/
static dsize_t D_sub(ErtsDigit* x, dsize_t xl, ErtsDigit c, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    register ErtsDigit yr, xr;

    ASSERT(I_comp(x, xl, x, 1) >= 0);

    while(xl--) {
	xr = *x++;
	yr = xr - c;
	c = (yr > xr);
	*r++ = yr;
    }
    do {
	r--;
    } while(*r == 0 && r != r0);

    return (r - r0) + 1;
}

/*
** subtract Z000...0 - y and store result in r, return new size
*/
static dsize_t Z_sub(ErtsDigit* y, dsize_t yl, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    register ErtsDigit yr;
    register ErtsDigit c = 0;

    while(yl--) {
	yr = *y++ + c;
	c = (yr < c);
	yr = 0 - yr;
	c += (yr > 0);
	*r++ = yr;
    }
    do {
	r--;
    } while(*r == 0 && r != r0);
    return (r - r0) + 1;
}

/*
** Multiply digits in x with digits in y and store in r
** Assumption: digits in r must be 0 (up to the size of x)
*/
static dsize_t I_mul(ErtsDigit* x, dsize_t xl, ErtsDigit* y, dsize_t yl, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    ErtsDigit* rt = r;

    ASSERT(xl >= yl);
    ZERO_DIGITS(r, xl);

    while(xl--) {
	ErtsDigit cp = 0;
	ErtsDigit c = 0;
	dsize_t n = yl;
	ErtsDigit* yt = y;
	ErtsDigit d;
	ErtsDigit p;

	d = *x; 
	x++;
	rt = r;

	switch(d) {
	case 0:
	    rt = rt + n;
	    break;
	case 1:
	    while(n--) {
		DSUMc(*yt, *rt, c, p);
		*rt++ = p;
		yt++;
	    }
	    break;
	case 2:
	    while(n--) {
		p = *yt;
		DSUMc(p, p, cp, p);
		DSUMc(p, *rt, c, p);
		*rt++ = p;
		yt++;
	    }
	    break;
	default:
	    while(n--) {
		DMULc(d,*yt, cp, p);
		DSUMc(p,*rt, c, p);
		*rt++ = p;
		yt++;
	    }
	    break;
	}
	*rt = c + cp;
	r++;
    }
    if (*rt == 0)
	return (rt - r0);
    else
	return (rt - r0) + 1;
}

/*
** Square digits in x store in r (x & r may point into a common area)
** Assumption: x is destroyed if common area and digits in r are zero
**             to the size of xl+1
*/

static dsize_t I_sqr(ErtsDigit* x, dsize_t xl, ErtsDigit* r)
{
    ErtsDigit d;
    ErtsDigit* r0 = r;
    ErtsDigit* s = r;

    ZERO_DIGITS(r, (xl+1));

    if ((r + xl) == x)	/* "Inline" operation */
	*x = 0;

    while(xl--) {
	dsize_t y_l = xl;

        d = *x++;
        s = r;

        if (d == 0) {
            s += y_l + 1;
            if (xl != 0) {
                *++s = 0;
                r += 2;
            }
        } else {
            ErtsDigit* y;
            ErtsDigit y_0 = 0, y_1 = 0, y_2 = 0, y_3 = 0;
            ErtsDigit b0, b1;
            ErtsDigit z0, z1, z2;
            ErtsDigit t;

            y = x;

            DMUL(d, d, b1, b0);
            DSUMc(*s, b0, y_3, t);
            *s++ = t;
            z1 = b1;
            while(y_l--) {
                DMUL(d, *y, b1, b0);
                y++;
                DSUMc(b0, b0, y_0, z0);
                DSUMc(z0, z1, y_2, z2);
                DSUMc(*s, z2, y_3, t);
                *s++ = t;
                DSUMc(b1, b1, y_1, z1);
            }
            z0 = y_0;
            DSUMc(z0, z1, y_2, z2);
            DSUMc(*s, z2, y_3, t);
            *s = t;
            if (xl != 0) {
                s++;
                t = (y_1+y_2+y_3);
                *s = t;
                r += 2;
            }
            else {
                ASSERT((y_1+y_2+y_3) == 0);
            }
        }
    }
    if (*s == 0)
	return (s - r0);
    else
	return (s - r0) + 1;
}

/*
 * Multiply using the Karatsuba algorithm.
 *
 * Reference: https://en.wikipedia.org/wiki/Karatsuba_algorithm
 *
 * Precondition: both inputs are normalized (single zero digit, or top
 * digit non-zero). The recursive Karatsuba split sizes the internal
 * scratch buffers from the declared lengths, so leading zero cells cause
 * the inner I_sub to read one cell past its scratch (caught originally
 * by AddressSanitizer). Callers that may pass zero-padded inputs must
 * trim before calling.
 */
static dsize_t I_mul_karatsuba(ErtsDigit* x, dsize_t xl, ErtsDigit* y,
                               dsize_t yl, ErtsDigit* r)
{
    ASSERT(xl >= yl);
    ASSERT(I_is_normalized(x, xl));
    ASSERT(I_is_normalized(y, yl));

    if (yl < 16) {
        /* Use the basic algorithm. */
        if (x == y && xl > 1) {
            ASSERT(xl == yl);
            return I_sqr(x, xl, r);
        } else {
            return I_mul(x, xl, y, yl, r);
        }
    } else {
        /* Use the Karatsuba algorithm. */
        Eterm *heap;
        Uint temp_heap_size;
        Uint z0_len, z1_len, z2_len, tmp_len, diff0_len, diff1_len, res_len;
        Uint low_x_len, low_y_len, high_x_len, high_y_len;
        Eterm *z0_buf, *z1_buf, *z2_buf, *tmp_buf;
        Eterm *diff0_buf, *diff1_buf;
#ifdef DEBUG
        Eterm *alloc_end;
#endif
        Eterm *low_x, *low_y, *high_x, *high_y;
        ErtsDigit zero = 0;
        Uint m = (xl+1) / 2;
        int tmp_prod_negative = 0;
        int i;

        /* Set up pointers and sizes. */
        low_x = x;
        low_x_len = m;
        high_x = x + m;
        high_x_len = xl - m;
        while (low_x_len > 1 && low_x[low_x_len-1] == 0) {
            low_x_len--;
        }

        low_y = y;
        if (yl <= m) {
            /* High part of y is zero. */
            low_y_len = yl;
            high_y = &zero;
            high_y_len = 1;
        } else {
            low_y_len = m;
            high_y = y + m;
            high_y_len = yl - m;
        }
        while (low_y_len > 1 && low_y[low_y_len-1] == 0) {
            low_y_len--;
        }

        ASSERT(low_x_len <= m);
        ASSERT(high_x_len <= m);
        ASSERT(low_y_len <= m);
        ASSERT(high_y_len <= m);

        /*
         * Set up temporary buffers in allocated memory.
         *
         * z1_buf is not used at the same time as diff0_buf
         * and diff1_buf, so they can share memory.
         */
        temp_heap_size = (4*m + 1) * sizeof(Eterm);
#ifdef DEBUG
        temp_heap_size += sizeof(Eterm);
#endif
        heap = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, temp_heap_size);
        z1_buf = heap;
        diff0_buf = z1_buf + 1;
        diff1_buf = diff0_buf + m;
        tmp_buf = diff1_buf + m;

#ifdef DEBUG
        z1_buf[0] = ERTS_HOLE_MARKER;
        diff0_buf[0] = ERTS_HOLE_MARKER;
        diff1_buf[0] = ERTS_HOLE_MARKER;
        tmp_buf[0] = ERTS_HOLE_MARKER;

        alloc_end = tmp_buf + 2*m;
        alloc_end[0] = ERTS_HOLE_MARKER;
        ASSERT(alloc_end - heap + 1 == temp_heap_size / sizeof(Eterm));
#endif

        /* Set up pointers for the result. */
        z0_buf = r;
        z2_buf = r + 2*m;

#ifdef DEBUG
        z2_buf[0] = ERTS_HOLE_MARKER;
#endif

#define I_OPERATION(_result, _op, _p1, _sz1, _p2, _sz2, _buf)   \
        do {                                                    \
            if ((_sz1) >= (_sz2)) {                             \
                _result = _op(_p1, _sz1, _p2, _sz2, _buf);      \
            } else {                                            \
                _result = _op(_p2, _sz2, _p1, _sz1, _buf);      \
            }                                                   \
        } while (0)

        /*
         * The Karatsuba algorithm is a divide and conquer algorithm
         * for multi-word integer multiplication. The numbers to be
         * multiplied are split up like this:
         *
         *   high     low
         *  +--------+--------+
         *  | high_x | low_x  |
         *  +--------+--------+
         *
         *  +--------+--------+
         *  | high_y | low_y  |
         *  +--------+--------+
         *
         * Then the following values are calculated:
         *
         *  z0 = low_x * low_y
         *  z2 = high_x + high_y
         *  z1 = (low_x - high_x) * (high_y - low_y) + z2 + z0
         *
         * Note that this expression for z1 produces the same result
         * as:
         *
         *    low_x * high_y + high_x * low_y
         *
         * Finally, the z2, z1, z0 values are combined to form the
         * product of x and y:
         *
         *   high     low
         *  +--+--+ +--+--+
         *  | z2  | | z0  |
         *  +--+--+ +--+--+
         *      +--+--+
         *  add | z1  |
         *      +--+--+
         *
         * There is an alternate way to calculate z1 (commonly found
         * in descriptions of the Karatsuba algorithm);
         *
         *  z1 = (high_x + low_x) * (high_y + low_y) - z2 - z0
         *
         * But this way can lead to more additions and carry handling.
         */

        /*
         * z0 = low_x * low_y
         *
         * Store this product in its final location in the result buffer.
         */
        I_OPERATION(z0_len, I_mul_karatsuba, low_x, low_x_len, low_y, low_y_len, z0_buf);
        ASSERT(z2_buf[0] == ERTS_HOLE_MARKER);
        for (i = z0_len; i < 2*m; i++) {
            z0_buf[i] = 0;
        }
        while (z0_len > 1 && z0_buf[z0_len - 1] == 0) {
            z0_len--;
        }
        ASSERT(z0_len == 1 || z0_buf[z0_len-1] != 0);
        ASSERT(z0_len <= low_x_len + low_y_len);

        /*
         * z2 = high_x * high_y
         *
         * Store this product in its final location in the result buffer.
         */
        if (high_y != &zero) {
            I_OPERATION(z2_len, I_mul_karatsuba, high_x, high_x_len,
                        high_y, high_y_len, z2_buf);
            while (z2_len > 1 && z2_buf[z2_len - 1] == 0) {
                z2_len--;
            }
            ASSERT(z2_len == 1 || z2_buf[z2_len-1] != 0);
        } else {
            z2_buf[0] = 0;
            z2_len = 1;
        }
        ASSERT(z2_len <= high_x_len + high_y_len);

        /*
         * tmp = abs(low_x - high_x) * abs(high_y - low_y)
         *
         * The absolute value of each difference will fit in m words.
         *
         * Save the sign of the product so that we later can choose to
         * subtract or add this value.
         */
        if (I_comp(low_x, low_x_len, high_x, high_x_len) >= 0) {
            diff0_len = I_sub(low_x, low_x_len, high_x, high_x_len, diff0_buf);
        } else {
            tmp_prod_negative = !tmp_prod_negative;
            diff0_len = I_sub(high_x, high_x_len, low_x, low_x_len, diff0_buf);
        }
        ASSERT(diff1_buf[0] == ERTS_HOLE_MARKER);
        ASSERT(diff0_len == 1 || diff0_buf[diff0_len-1] != 0);
        ASSERT(diff0_len <= m);

        if (x == y) {
            ASSERT(xl == yl);
            tmp_prod_negative = 1;
            diff1_buf = diff0_buf;
            diff1_len = diff0_len;
        } else if (I_comp(high_y, high_y_len, low_y, low_y_len) >= 0) {
            diff1_len = I_sub(high_y, high_y_len, low_y, low_y_len, diff1_buf);
        } else {
            tmp_prod_negative = !tmp_prod_negative;
            if (high_y != &zero) {
                diff1_len = I_sub(low_y, low_y_len, high_y, high_y_len, diff1_buf);
            } else {
                diff1_buf = low_y;
                diff1_len = low_y_len;
            }
        }
        ASSERT(tmp_buf[0] == ERTS_HOLE_MARKER);
        ASSERT(diff1_len == 1 || diff1_buf[diff1_len-1] != 0);
        ASSERT(diff1_len <= m);

        I_OPERATION(tmp_len, I_mul_karatsuba, diff0_buf, diff0_len, diff1_buf, diff1_len, tmp_buf);
        ASSERT(alloc_end[0] == ERTS_HOLE_MARKER);
        while (tmp_len > 1 && tmp_buf[tmp_len - 1] == 0) {
            tmp_len--;
        }
        ASSERT(tmp_len == 1 || tmp_buf[tmp_len-1] != 0);
        ASSERT(tmp_len <= diff0_len + diff1_len);

        /*
         * z1 = z0 + z2
         */
        I_OPERATION(z1_len, I_add, z0_buf, z0_len, z2_buf, z2_len, z1_buf);
        ASSERT(z1_len == 1 || z1_buf[z1_len-1] != 0);

        if (tmp_prod_negative) {
            /* z1 = z1 - tmp */
            z1_len = I_sub(z1_buf, z1_len, tmp_buf, tmp_len, z1_buf);
        } else {
            /* z1 = z1 + tmp */
            I_OPERATION(z1_len, I_add, z1_buf, z1_len, tmp_buf, tmp_len, z1_buf);
        }

        /* Add z1 shifted into the result */
        I_OPERATION(res_len, I_add, z0_buf+m, z2_len+m, z1_buf, z1_len, z0_buf+m);

        /* Normalize */
        res_len += m;
        while (res_len > 1 && r[res_len - 1] == 0) {
            res_len--;
        }
        ASSERT(res_len == 1 || r[res_len-1] != 0);
        ASSERT(res_len <= xl + yl);

        erts_free(ERTS_ALC_T_TMP, (void *) heap);
        return res_len;
    }
#undef I_OPERATION
}

/*
** Multiply digits d with digits in x and store in r
*/
static dsize_t D_mul(ErtsDigit* x, dsize_t xl, ErtsDigit d, ErtsDigit* r)
{
    ErtsDigit c = 0;
    dsize_t rl = xl;
    ErtsDigit p;

    switch(d) {
    case 0:
	ZERO_DIGITS(r, 1);
	return 1;
    case 1:
	if (x != r)
	    MOVE_DIGITS(r, x, xl);
	return xl;
    case 2:
	while(xl--) {
	    p = *x;
	    DSUMc(p, p, c, p);
	    *r++ = p;
	    x++;
	}
	break;
    default:
	while(xl--) {
	    DMULc(d, *x, c, p);
	    *r++ = p;
	    x++;
	}
	break;
    }
    if (c == 0)
	return rl;
    *r = c;
    return rl+1;
}

/*
** Multiply and subtract
** calculate r(i) = x(i) - d*y(i)
** assumption: xl = yl || xl == yl+1
**
** Return size of r
** 0 means borrow
*/
static dsize_t D_mulsub(ErtsDigit* x, dsize_t xl, ErtsDigit d,
			ErtsDigit* y, dsize_t yl, ErtsDigit* r)
{
    ErtsDigit c = 0;
    ErtsDigit b = 0;
    ErtsDigit c0;
    ErtsDigit* r0 = r;
    ErtsDigit s;

    ASSERT(xl == yl || xl == yl+1);

    xl -= yl;
    while(yl--) {
	DMULc(d, *y, c, c0);
	DSUBb(*x, c0, b, s);
	*r++ = s;
	x++;
	y++;
    }
    if (xl == 0) {
	if (c != 0 || b != 0)
	    return 0;
    }
    else {			/* xl == 1 */
	DSUBb(*x, c, b, s);
	*r++ = s;
    }
    if (b != 0) return 0;

    do {
	r--;
    } while(*r == 0 && r != r0);
    return (r - r0) + 1;
}

/*
** Divide digits in x with a digit,
** quotient is returned in q and remainder digit in r
** x and q may be equal
*/
static dsize_t D_div(ErtsDigit* x, dsize_t xl, ErtsDigit d, ErtsDigit* q, ErtsDigit* r)
{
    ErtsDigit* xp = x + (xl-1);
    ErtsDigit* qp = q + (xl-1);
    dsize_t qsz = xl;
    ErtsDigit a1;
	
    a1 = *xp; 
    xp--;

    if (d > a1) {
	if (xl == 1) {
	    *r = a1;
	    *qp = 0;
	    return 1;
	}
	qsz--;
	qp--;
    }

    do {
	ErtsDigit q0, a0, b0;
	ERTS_DECLARE_DUMMY(ErtsDigit b);
	ERTS_DECLARE_DUMMY(ErtsDigit b1);

	if (d > a1) {
	    a0 = *xp; 
	    xp--;
	}
	else {
	    a0 = a1; a1 = 0;
	}
	DDIV(a1, a0, d, q0);
	DMUL(d, q0, b1, b0);
	DSUB(a0,b0, b, a1);
	*qp = q0;
	qp--;
    } while (xp >= x);

    *r = a1;
    return qsz;
}

/*
** Forward decls for the Burnikel-Ziegler recursive division wrapper
** so the existing I_div definition below continues to compile, then
** I_div_dispatch chooses the algorithm based on divisor size.
*/
static dsize_t I_div_dispatch(ErtsDigit* x, dsize_t xl,
                              ErtsDigit* y, dsize_t yl,
                              ErtsDigit* q, ErtsDigit* r, dsize_t* rlp);
static dsize_t I_lshift(ErtsDigit* x, dsize_t xl, Sint y,
                        short sign, ErtsDigit* r);

/*
** Divide digits in x with digits in y and return qutient in q
** and remainder in r
** assume that integer(x) > integer(y)
** Return remainder in x (length int rl)
** Return quotient size
*/

static dsize_t I_div(ErtsDigit* x, dsize_t xl, ErtsDigit* y, dsize_t yl,
		     ErtsDigit* q, ErtsDigit* r, dsize_t* rlp)
{
    ErtsDigit* rp;
    ErtsDigit* qp;
    ErtsDigit b1 = y[yl-1];
    ErtsDigit b2 = y[yl-2];
    ErtsDigit a1;
    ErtsDigit a2;
    int r_signed = 0;
    dsize_t ql;
    dsize_t rl;

    if (x != r)
	MOVE_DIGITS(r, x, xl);
    rp = r + (xl-yl);
    rl = xl;
	
    ZERO_DIGITS(q, xl-yl+1);
    qp = q + (xl-yl);
    ql = 0;
	
    /* Adjust length */
    a1 = rp[yl-1];
    a2 = rp[yl-2];
    if (b1 < a1 || (b1 == a1 && b2 <= a2))
	ql = 1;

    do {
	ErtsDigit q0;
	dsize_t nsz = yl;
	dsize_t nnsz;

	a1 = rp[yl-1];
	a2 = rp[yl-2];

	if (b1 < a1)
	    DDIV2(a1,a2,b1,b2,q0);
	else if (b1 > a1) {
	    DDIV(a1,a2,b1,q0);
	    nsz++;
	    rp--;
	    qp--;
	    ql++;
	}
	else {			/* (b1 == a1) */
	    if (b2 <= a2)
		q0 = 1;
	    else {
		q0 = D_MASK;
		nsz++;
		rp--;
		qp--;
		ql++;
	    }
	}

	if (r_signed)
	    ql = D_sub(qp, ql, q0, qp);
	else
	    ql = D_add(qp, ql, q0, qp);

	if ((nnsz = D_mulsub(rp, nsz, q0, y, yl, rp)) == 0) {
	    nnsz = Z_sub(r, rl, r);
	    if (nsz > (rl-nnsz))
		nnsz = nsz - (rl-nnsz);
	    else
		nnsz = 1;
	    r_signed = !r_signed;
	}
		
	if ((nnsz == 1) && (*rp == 0))
	    nnsz = 0;
	rp = rp - (yl-nnsz);
	rl -= (nsz-nnsz);
	qp = qp - (yl-nnsz);
	ql += (yl-nnsz);
    } while (I_comp(r, rl, y, yl) >= 0);

    ql -= (q - qp);
    qp = q;

    if (rl == 0)
	rl = 1;

    while(rl > 1 && r[rl-1] == 0) /* Remove "trailing zeroes" */
      --rl;

    if (r_signed && (rl > 1 || *r != 0)) {
	rl = I_sub(y, yl, r, rl, r);
	ql = D_sub(qp, ql, 1, qp);
    }

    *rlp = rl;
    return ql;
}

/* ============================================================
** Burnikel-Ziegler recursive division.
**
** Reference: Burnikel & Ziegler, "Fast Recursive Division",
** MPI-I-98-1-022 (1998).
**
** Recurrence with M(n) = cost of n-by-n multiplication:
**   T(n) = 2 T(n/2) + M(n/2) + O(n)
** Combined with Karatsuba M(n) = O(n^1.58) yields
**   T(n) = O(M(n) log n) ~ O(n^1.58 log n)
** which is sub-quadratic compared to schoolbook I_div.
**
** All BZ helpers operate on raw ErtsDigit arrays of fixed length.
** They never trim leading zeros — sizes are determined by the
** caller. Below BZ_DIV_THRESHOLD (in ErtsDigits) for the divisor
** size n, the dispatcher falls back to schoolbook I_div.
** ============================================================ */

/*
** BZ status: enabled (BZ_DIV_THRESHOLD = 8 ErtsDigits).
**
** The implementation was originally affected by a heap-buffer-overflow
** in I_mul_karatsuba reading 1 cell past its internal scratch. AddressSanitizer
** pinpointed the offending read in I_mul_karatsuba's I_sub call (line 991).
** Root cause: I_mul_karatsuba assumes its inputs are normalized (top digit
** non-zero), but BZ passes Q with leading-zero cells (the recursive
** bz_div_2n_1n always zero-pads its Q to size n). The sub-products zip
** along the heap until I_sub reaches a tmp_buf that's been sized assuming
** trimmed inputs, then reads one cell past.
**
** Fix: trim Q (and B2 for symmetry) before each I_mul_karatsuba call in
** bz_div_3n_2n. See the call site below.
*/
#ifndef BZ_DIV_THRESHOLD
#define BZ_DIV_THRESHOLD 8
#endif

/* If non-zero: I_div_dispatch verifies BZ result against I_div on the
 * original input. Use only for debugging. */
#ifndef BZ_SELFCHECK
#define BZ_SELFCHECK 0
#endif

#ifndef BARRETT_LEVEL_THRESHOLD
#define BARRETT_LEVEL_THRESHOLD 100
#endif

/* Maximum Barrett correction iterations before falling back to I_div_dispatch.
 * BZ correctness implies <= 2; the prod buffer is sized to hold this many
 * carry-extensions of qhat. */
#define BARRETT_MAX_CORRECTIONS 4

/* Fixed-length addition: r = a + b, all of length n.
 * Returns the final carry-out (0 or 1). */
static ErtsDigit bz_add_n(const ErtsDigit *a, const ErtsDigit *b,
                          ErtsDigit *r, dsize_t n)
{
    ErtsDigit c = 0;
    dsize_t i;
    for (i = 0; i < n; i++) {
        ErtsDigit s = a[i] + c;
        ErtsDigit c1 = (s < c);
        s += b[i];
        c1 += (s < b[i]);
        r[i] = s;
        c = c1;
    }
    return c;
}

/* Fixed-length subtraction: r = a - b, all of length n.
 * Returns the final borrow (0 or 1). */
static ErtsDigit bz_sub_n(const ErtsDigit *a, const ErtsDigit *b,
                          ErtsDigit *r, dsize_t n)
{
    ErtsDigit borrow = 0;
    dsize_t i;
    for (i = 0; i < n; i++) {
        ErtsDigit yb = b[i] + borrow;
        ErtsDigit b1 = (yb < borrow);
        ErtsDigit res = a[i] - yb;
        b1 += (res > a[i]);
        r[i] = res;
        borrow = b1;
    }
    return borrow;
}

/* Compare two fixed-length n arrays: returns -1 / 0 / 1 (a vs b). */
static int bz_cmp_n(const ErtsDigit *a, const ErtsDigit *b, dsize_t n)
{
    dsize_t i;
    for (i = n; i-- > 0; ) {
        if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
    }
    return 0;
}

/* Forward declarations. */
static void bz_div_2n_1n(ErtsDigit *A, ErtsDigit *B, dsize_t n,
                         ErtsDigit *Q, ErtsDigit *R, ErtsDigit *scratch);
static void bz_div_3n_2n(ErtsDigit *A, ErtsDigit *B, dsize_t n,
                         ErtsDigit *Q, ErtsDigit *R, ErtsDigit *scratch);

/* Scratch space estimator for bz_div_2n_1n with parameter n.
 *
 *   bz_div_2n_1n(n) needs:
 *     - n digits for R1_temp (intermediate remainder of step 1)
 *     - bz_div_3n_2n(half) scratch
 *
 *   bz_div_3n_2n(n') needs:
 *     - 2n' digits for D (product Q*B2)
 *     - bz_div_2n_1n(n') scratch (recursive)
 *
 * Loose upper bound: O(n) per recursion frame, log2(n) frames -> O(n log n).
 * We use a single contiguous buffer and let each frame allocate its sub-slice
 * sequentially.
 */
static dsize_t bz_scratch_size_2n_1n(dsize_t n)
{
    /* Per-frame scratch: n (R1_temp) + 2n (D) + 2n (R1 for div_3n_2n call slack)
     * + recursive(half). Worst case ~ 6n at top + 6*(n/2) + ... ~ 12n. */
    if (n < BZ_DIV_THRESHOLD) {
        return n + 2 + 2*n;  /* I_div needs n+1 q_buf + 2n r_buf */
    }
    return 6*n + bz_scratch_size_2n_1n(n / 2);
}

/* bz_div_2n_1n: divide A (length 2n) by B (length n).
 *   Pre: B is normalized (B[n-1] has its top bit set), A < B*beta^n
 *        (i.e., the n+1th digit and above of A are zero — equivalently the
 *        top n digits of A < B).
 *   Post: Q has length n (zero-padded), R has length n.
 *   Destroys A.
 */
static void bz_div_2n_1n(ErtsDigit *A, ErtsDigit *B, dsize_t n,
                         ErtsDigit *Q, ErtsDigit *R, ErtsDigit *scratch)
{
    dsize_t half;
    ErtsDigit *R1_temp;
    ErtsDigit *sub_scratch;
    dsize_t i;

    if (n < BZ_DIV_THRESHOLD || (n & 1)) {
        /* Base case: schoolbook. */
        ErtsDigit *q_buf = scratch;        /* size n+1 */
        ErtsDigit *r_buf = scratch + n + 1;/* size 2n */
        dsize_t qsz, rsz;

        ASSERT(bz_cmp_n(A + n, B, n) < 0);  /* precondition */

        qsz = I_div(A, 2*n, B, n, q_buf, r_buf, &rsz);
        ASSERT(qsz <= n);
        if (qsz > n) qsz = n;     /* defensive cap */
        if (rsz > n) rsz = n;
        for (i = 0; i < qsz; i++) Q[i] = q_buf[i];
        for (i = qsz; i < n; i++) Q[i] = 0;
        for (i = 0; i < rsz; i++) R[i] = r_buf[i];
        for (i = rsz; i < n; i++) R[i] = 0;
        return;
    }

    half = n / 2;
    R1_temp = scratch;                  /* n digits */
    sub_scratch = scratch + n;          /* sub-frame scratch */

    /* Step 1: divide top 3*half = 3n/2 digits of A by B. A's top 3*half digits
     * sit at A[half..2n-1]. The function below treats those as a 3*half array. */
    bz_div_3n_2n(A + half, B, half, Q + half, R1_temp, sub_scratch);

    /* Step 2: write R1_temp into A[half..n+half-1] so that A[0..3*half-1]
     * holds [A4 || R1_temp] = R1_temp * beta^half + A4. (A4 = original
     * A[0..half-1] is undisturbed; A[half..2n-1] was destroyed in step 1.) */
    for (i = 0; i < n; i++) {
        A[half + i] = R1_temp[i];
    }

    /* Step 3: divide that 3*half-digit value by B (which is 2*half = n). */
    bz_div_3n_2n(A, B, half, Q, R, sub_scratch);
}

/* bz_div_3n_2n: divide A (length 3n) by B (length 2n).
 *   Pre: B normalized (B[2n-1] has top bit set), A < B*beta^n.
 *   Post: Q has length n (zero-padded), R has length 2n.
 *   Destroys A and may mutate Q/R/scratch.
 */
static void bz_div_3n_2n(ErtsDigit *A, ErtsDigit *B, dsize_t n,
                         ErtsDigit *Q, ErtsDigit *R, ErtsDigit *scratch)
{
    ErtsDigit *A1 = A + 2*n;     /* high n */
    ErtsDigit *A2 = A + n;       /* mid n */
    ErtsDigit *A3 = A;           /* low n */
    ErtsDigit *B1 = B + n;       /* high n */
    ErtsDigit *B2 = B;           /* low n */

    /* scratch layout:
     *   D       : 2n   digits  (Q * B2)
     *   sub_scr : ...
     */
    ErtsDigit *D = scratch;
    ErtsDigit *sub_scratch = scratch + 2*n;
    dsize_t i;
    int q_overflow = 0;        /* Q' = beta^n in the else branch */

    if (bz_cmp_n(A1, B1, n) < 0) {
        /* (Q', R1) = bz_div_2n_1n([A1 || A2], B1).
         * [A1 || A2] is a 2n-digit value at A + n, contiguous. */
        ErtsDigit *R1 = R + n;   /* park R1 in the high half of R */
        bz_div_2n_1n(A + n, B1, n, Q, R1, sub_scratch);
        /* R now holds [garbage_low_n || R1_high_n]. Move R1 into a known
         * scratch position: keep it in R[n..2n-1] for now. */
    } else {
        /* Q' = beta^n - 1 (all ones). */
        for (i = 0; i < n; i++) Q[i] = (ErtsDigit) -1;

        /* From the precondition A < B*beta^n we have that A1 == B1 in
         * this branch (a strict A1 > B1 would imply A >= B*beta^n).
         *
         *   R1 = [A1 || A2] - Q'*B1
         *      = A1*beta^n + A2 - (beta^n - 1)*B1
         *      = (A1 - B1)*beta^n + A2 + B1
         *      = A2 + B1                     (since A1 == B1)
         *
         * The sum A2 + B1 fits in n+1 digits worst case (when both are
         * close to beta^n). We track the carry-out as part of R1.
         */
        {
            ErtsDigit *R1 = R + n;          /* parked in high n of R */
            ErtsDigit carry = bz_add_n(A2, B1, R1, n);
            /* R1 has n digits + 1-bit carry. We must propagate this carry
             * into the high end when forming [R1 || A3] later. We do this
             * via a single explicit "extra digit" tracked in q_overflow. */
            q_overflow = (int) carry;
        }
    }

    /* D = Q' * B2 (n × n -> 2n digits).
     * I_mul_karatsuba assumes normalized inputs (single zero digit, or
     * top digit non-zero).
     * Q from the recursive bz_div_2n_1n is zero-padded to n cells, and
     * B2 (low half of normalized divisor) often has leading zeros too
     * after several recursion levels. Trim both before the call. */
    {
        dsize_t qsz_trimmed = n;
        dsize_t b2sz_trimmed = n;
        dsize_t prod_sz;
        while (qsz_trimmed > 1 && Q[qsz_trimmed - 1] == 0) qsz_trimmed--;
        while (b2sz_trimmed > 1 && B2[b2sz_trimmed - 1] == 0) b2sz_trimmed--;
        if (qsz_trimmed >= b2sz_trimmed) {
            prod_sz = I_mul_karatsuba(Q, qsz_trimmed, B2, b2sz_trimmed, D);
        } else {
            prod_sz = I_mul_karatsuba(B2, b2sz_trimmed, Q, qsz_trimmed, D);
        }
        for (i = prod_sz; i < 2*n; i++) D[i] = 0;
    }

    /* Form [R1 || A3] in R: R[0..n-1] = A3, R[n..2n-1] = R1.
     * R1 is currently at R[n..2n-1] from the steps above. A3 = A[0..n-1]. */
    for (i = 0; i < n; i++) {
        R[i] = A3[i];
    }
    /* R[n..2n-1] already holds R1. */

    /* R = [R1 || A3] - D, 2n-digit subtract. There may be an extra
     * "digit-of-overflow" coming from R1 in the q_overflow case, so we
     * handle that as an extra borrow correction. */
    {
        ErtsDigit borrow = bz_sub_n(R, D, R, 2*n);

        /* If q_overflow is set, R conceptually has a +beta^(2n) term.
         * That fully cancels one borrow. */
        if (q_overflow && borrow) {
            borrow = 0;
            q_overflow = 0;
        }

        /* While R is "negative" (borrow remains), correct: Q -= 1, R += B.
         * BZ guarantees at most 2 iterations are required. */
        while (borrow) {
            ErtsDigit carry;
            /* Q -= 1 */
            for (i = 0; i < n; i++) {
                if (Q[i] != 0) {
                    Q[i]--;
                    break;
                }
                Q[i] = (ErtsDigit) -1;
            }
            ASSERT(i < n);  /* would mean Q wrapped to 0 -> bug */

            /* R += B */
            carry = bz_add_n(R, B, R, 2*n);
            if (carry) {
                ASSERT(borrow);
                borrow--;
            }
        }

        /* If q_overflow remained set, R still has a +beta^(2n) term that we
         * haven't applied. This can happen if no borrow occurred, in which
         * case we need to subtract B and add 1 to Q ... but that contradicts
         * the postcondition R < B. In practice, q_overflow without borrow
         * cannot occur given the BZ correctness proof, so assert. */
        ASSERT(!q_overflow);
    }
}

/* Top-level BZ wrapper. Handles arbitrary xl, yl by:
 *   - Choosing n = round-up-to-power-of-2 of yl (so the recursion is balanced).
 *   - Padding y with leading zeros to length n.
 *   - Normalizing (left-shift to put top bit of y_padded at position D_EXP-1).
 *   - Padding x to a multiple of n digits, plus shift overflow.
 *   - Outer loop: process the padded x in 2n-digit chunks, where the high half
 *     of each chunk is the running remainder.
 *   - Denormalizing the final remainder.
 *
 * Quotient size is returned, remainder size in *rlp. q must have capacity
 * xl - yl + 1; r must have capacity yl. */
static dsize_t I_div_bz(ErtsDigit *x, dsize_t xl,
                        ErtsDigit *y, dsize_t yl,
                        ErtsDigit *q, ErtsDigit *r, dsize_t *rlp)
{
    dsize_t n;
    int shift;
    ErtsDigit top_y;
    ErtsDigit *y_norm;        /* n digits */
    ErtsDigit *x_norm;        /* k*n + n digits (k chunks plus running remainder) */
    dsize_t x_norm_capacity;
    dsize_t k;                /* number of chunks */
    dsize_t i;
    ErtsDigit *q_buf;         /* k*n digits (full quotient before trimming) */
    ErtsDigit *bz_scratch;
    dsize_t scratch_size;
    dsize_t qsz, rsz;

    ASSERT(yl >= BZ_DIV_THRESHOLD);

    /* n = round up to power of 2 of yl */
    n = 1;
    while (n < yl) n *= 2;

    /* Compute normalization shift so the top bit of the padded y is set. */
    top_y = y[yl - 1];
    {
        int p = 0;
        ErtsDigit d = top_y;
        while (d > 1) { d >>= 1; p++; }
        shift = (D_EXP - 1) - p;
    }
    /* If yl < n (we padded with zeros at the top), the top bit isn't even
     * in y[yl-1]; it's far below. So the shift would be huge (> D_EXP).
     * To avoid this, we extend the shift conceptually so that y_norm[n-1]
     * has its top bit set. The actual shift in bits is:
     *   shift_bits = (n - yl) * D_EXP + ((D_EXP - 1) - top_bit_pos(top_y))
     * which is fine because I_lshift handles arbitrary positive shifts. */
    {
        Sint shift_total = (Sint)((n - yl) * D_EXP) + shift;

        /* Allocate y_norm of n digits. */
        y_norm = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsDigit) * (n + 2));
        {
            dsize_t y_sz = I_lshift(y, yl, shift_total, 0, y_norm);
            ASSERT(y_sz <= n);
            /* Pad up to n with zeros. */
            for (i = y_sz; i < n; i++) y_norm[i] = 0;
        }

        /* Pad x to length k*n where k = ceil((xl + 1) / n) (the +1 accounts
         * for normalization shift overflow), AND ensure the top n digits of
         * the padded x are < y_norm (so the chunked division's first
         * 2n-by-n call satisfies its precondition).
         *
         * Concretely: padded_x_digits = ceil((xl + shift_in_digits + 1) / n) * n.
         * We pad with zeros at the high end after shifting. */
        {
            dsize_t shift_overflow = (shift_total + D_EXP - 1) / D_EXP;
            dsize_t padded_x = xl + shift_overflow;
            dsize_t target;

            /* k chunks, each 2n digits viewed as [running_rem || x_chunk_n].
             * Running remainder starts at 0. We process from MSB to LSB. */

            /* Round padded_x up to a multiple of n. Add one extra n for the
             * running remainder slot (top of x_norm is the rem; rest is the
             * dividend chunks). */
            k = (padded_x + n - 1) / n;
            target = (k + 1) * n;
            x_norm_capacity = target + 4;
            x_norm = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                              sizeof(ErtsDigit) * x_norm_capacity);
            for (i = 0; i < x_norm_capacity; i++) x_norm[i] = 0;
            (void) I_lshift(x, xl, shift_total, 0, x_norm);
        }
    }

    /* Allocate quotient buffer and scratch. */
    q_buf = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsDigit) * (k * n + 1));
    for (i = 0; i < k * n + 1; i++) q_buf[i] = 0;

    scratch_size = bz_scratch_size_2n_1n(n);
    bz_scratch = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                          sizeof(ErtsDigit) * scratch_size);

    /* Outer loop: chunk dividend MSB-first, n digits at a time.
     * Running remainder lives in x_norm[k*n .. (k+1)*n - 1] (top slot).
     * Initially zero. For chunk index j (counting down from k-1 to 0):
     *   - Form a 2n-digit value: top n = rem, bottom n = x_norm[j*n..(j+1)*n-1].
     *   - bz_div_2n_1n on that, B = y_norm.
     *   - Quotient Q goes into q_buf[j*n..(j+1)*n-1].
     *   - New rem replaces top.
     */
    {
        ErtsDigit *rem_slot = x_norm + k * n;  /* top n */
        ErtsDigit *tmp_2n = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                                     sizeof(ErtsDigit) * 2 * n);
        Sint j;

        for (j = (Sint)k - 1; j >= 0; j--) {
            /* Build [rem || chunk] in tmp_2n. */
            for (i = 0; i < n; i++) tmp_2n[i] = x_norm[j*n + i];
            for (i = 0; i < n; i++) tmp_2n[n + i] = rem_slot[i];

            /* Precondition for bz_div_2n_1n: top n of tmp_2n < B.
             * top n of tmp_2n = rem_slot, which we maintain as the prior
             * remainder, satisfying < B by construction. */
            ASSERT(bz_cmp_n(tmp_2n + n, y_norm, n) < 0);

            bz_div_2n_1n(tmp_2n, y_norm, n,
                         q_buf + j * n, rem_slot, bz_scratch);
        }

        erts_free(ERTS_ALC_T_TMP, tmp_2n);
    }

    /* Quotient: trim trailing zeros from q_buf. */
    qsz = k * n;
    while (qsz > 1 && q_buf[qsz - 1] == 0) qsz--;
    /* Caller's q has capacity xl - yl + 1; verify and copy. */
    {
        dsize_t cap = xl - yl + 1;
        if (qsz > cap) qsz = cap;  /* shouldn't happen since x < y * beta^cap */
    }
    for (i = 0; i < qsz; i++) q[i] = q_buf[i];

    /* Remainder: shift right by shift_total bits. */
    {
        ErtsDigit *rem_slot = x_norm + k * n;
        Sint shift_total = (Sint)((n - yl) * D_EXP) + shift;
        rsz = I_lshift(rem_slot, n, -shift_total, 0, r);
        /* I_lshift may return a size that exceeds the actual bit width
         * of the value; trim any trailing zeros. */
        while (rsz > 1 && r[rsz - 1] == 0) rsz--;
        if (rsz == 0) rsz = 1;
    }
    *rlp = rsz;

    erts_free(ERTS_ALC_T_TMP, bz_scratch);
    erts_free(ERTS_ALC_T_TMP, q_buf);
    erts_free(ERTS_ALC_T_TMP, x_norm);
    erts_free(ERTS_ALC_T_TMP, y_norm);

    return qsz;
}

/* Dispatcher: pick BZ for large divisors, schoolbook otherwise. */
static dsize_t I_div_dispatch(ErtsDigit *x, dsize_t xl,
                              ErtsDigit *y, dsize_t yl,
                              ErtsDigit *q, ErtsDigit *r, dsize_t *rlp)
{
    if (yl >= BZ_DIV_THRESHOLD && xl >= 2 * yl) {
#if BZ_SELFCHECK
        /* Save x (BZ destroys it) and the expected schoolbook result. */
        ErtsDigit *x_copy = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                                     sizeof(ErtsDigit) * xl);
        ErtsDigit *q_ref  = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                                     sizeof(ErtsDigit) * (xl - yl + 1));
        ErtsDigit *r_ref  = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                                     sizeof(ErtsDigit) * xl);
        ErtsDigit *x_for_ref = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                                       sizeof(ErtsDigit) * xl);
        dsize_t qsz_ref, rsz_ref, qsz, rsz;
        dsize_t i;
        int mismatch;

        for (i = 0; i < xl; i++) x_copy[i] = x[i];
        for (i = 0; i < xl; i++) x_for_ref[i] = x[i];

        qsz_ref = I_div(x_for_ref, xl, y, yl, q_ref, r_ref, &rsz_ref);

        qsz = I_div_bz(x, xl, y, yl, q, r, rlp);
        rsz = *rlp;

        mismatch = 0;
        if (qsz != qsz_ref || rsz != rsz_ref) {
            mismatch = 1;
        } else {
            for (i = 0; i < qsz && !mismatch; i++) {
                if (q[i] != q_ref[i]) mismatch = 1;
            }
            for (i = 0; i < rsz && !mismatch; i++) {
                if (r[i] != r_ref[i]) mismatch = 1;
            }
        }

        if (mismatch) {
            dsize_t maxq = qsz > qsz_ref ? qsz : qsz_ref;
            dsize_t maxr = rsz > rsz_ref ? rsz : rsz_ref;
            dsize_t shown;
            fprintf(stderr,
                "BZ MISMATCH: xl=%lu yl=%lu | qsz=%lu(ref %lu) rsz=%lu(ref %lu)\n",
                (unsigned long)xl, (unsigned long)yl,
                (unsigned long)qsz, (unsigned long)qsz_ref,
                (unsigned long)rsz, (unsigned long)rsz_ref);
            shown = 0;
            fprintf(stderr, "  Q diffs (idx, bz, ref):");
            for (i = 0; i < maxq && shown < 8; i++) {
                ErtsDigit qv = (i < qsz) ? q[i] : 0;
                ErtsDigit qr = (i < qsz_ref) ? q_ref[i] : 0;
                if (qv != qr) {
                    fprintf(stderr, " (%lu,%lx,%lx)",
                            (unsigned long)i,
                            (unsigned long)qv,
                            (unsigned long)qr);
                    shown++;
                }
            }
            shown = 0;
            fprintf(stderr, "\n  R diffs (idx, bz, ref):");
            for (i = 0; i < maxr && shown < 8; i++) {
                ErtsDigit rv = (i < rsz) ? r[i] : 0;
                ErtsDigit rr = (i < rsz_ref) ? r_ref[i] : 0;
                if (rv != rr) {
                    fprintf(stderr, " (%lu,%lx,%lx)",
                            (unsigned long)i,
                            (unsigned long)rv,
                            (unsigned long)rr);
                    shown++;
                }
            }
            fprintf(stderr, "\n");
            fflush(stderr);
            /* Fall back to the reference result so the test continues. */
            for (i = 0; i < qsz_ref; i++) q[i] = q_ref[i];
            for (i = 0; i < rsz_ref; i++) r[i] = r_ref[i];
            qsz = qsz_ref;
            *rlp = rsz_ref;
        }

        erts_free(ERTS_ALC_T_TMP, x_copy);
        erts_free(ERTS_ALC_T_TMP, q_ref);
        erts_free(ERTS_ALC_T_TMP, r_ref);
        erts_free(ERTS_ALC_T_TMP, x_for_ref);
        return qsz;
#else
        return I_div_bz(x, xl, y, yl, q, r, rlp);
#endif
    }
    return I_div(x, xl, y, yl, q, r, rlp);
}

/*
** Remainder of digits in x and a digit d
*/
static ErtsDigit D_rem(ErtsDigit* x, dsize_t xl, ErtsDigit d)
{
    ErtsDigit rem = 0;

    x += (xl-1);
    do {
	if (rem != 0)
	    DREM(rem, *x, d, rem);
	else
	    DREM(0, *x, d, rem);
	x--;
	xl--;
    } while(xl > 0);
    return rem;
}

/*
** Remainder of x and y
**
** Assumptions: xl >= yl, yl > 1
**			   r must contain at least xl number of digits
*/
static dsize_t I_rem(ErtsDigit* x, dsize_t xl, ErtsDigit* y, dsize_t yl, ErtsDigit* r)
{
    ErtsDigit* rp;
    ErtsDigit b1 = y[yl-1];
    ErtsDigit b2 = y[yl-2];
    ErtsDigit a1;
    ErtsDigit a2;
    int r_signed = 0;
    dsize_t rl;
	
    if (x != r)
	MOVE_DIGITS(r, x, xl);
    rp = r + (xl-yl);
    rl = xl;

    do {
	ErtsDigit q0;
	dsize_t nsz = yl;
	dsize_t nnsz;
		
	a1 = rp[yl-1];
	a2 = rp[yl-2];

	if (b1 < a1)
	    DDIV2(a1,a2,b1,b2,q0);
	else if (b1 > a1) {
	    DDIV(a1,a2,b1,q0);
	    nsz++;
	    rp--;
	}
	else {			/* (b1 == a1) */
	    if (b2 <= a2)
		q0 = 1;
	    else {
		q0 = D_MASK;
		nsz++;
		rp--;
	    }
	}

	if ((nnsz = D_mulsub(rp, nsz, q0, y, yl, rp)) == 0) {
	    nnsz = Z_sub(r, rl, r);
	    if (nsz > (rl-nnsz))
		nnsz = nsz - (rl-nnsz);
	    else
		nnsz = 1;
	    r_signed = !r_signed;
	}

	if (nnsz == 1 && *rp == 0)
	    nnsz = 0;

	rp = rp - (yl-nnsz);
	rl -= (nsz-nnsz);
    } while (I_comp(r, rl, y, yl) >= 0);

    if (rl == 0)
	rl = 1;

    while(rl > 1 && r[rl-1] == 0) /* Remove "trailing zeroes" */
      --rl;

    if (r_signed && (rl > 1 || *r != 0))
	rl = I_sub(y, yl, r, rl, r);
    return rl;
}

/*
** Remove trailing digits from bitwise operations
*/
static dsize_t I_btrail(ErtsDigit* r0, ErtsDigit* r, short sign)
{
    /* convert negative numbers to one complement */
    if (sign) {
	dsize_t rl;
	ErtsDigit d;

	/* 1 remove all 0xffff words */
	do {
	    r--;
	} while(((d = *r) == D_MASK) && (r != r0));

	/* 2 complement high digit */
	if (d == D_MASK)
	    *r = 0;
	else {
	    ErtsDigit prev_mask = 0;
	    ErtsDigit mask = (DCONST(1) << (D_EXP-1));

	    while((d & mask) == mask) {
		prev_mask = mask;
		mask = (prev_mask >> 1) | (DCONST(1)<<(D_EXP-1));
	    }
	    *r = ~d & ~prev_mask;
	}
	rl = (r - r0) + 1;
	while(r != r0) {
	    r--;
	    *r = ~*r;
	}
	return D_add(r0, rl, 1, r0);
    }

    do {
	r--;
    } while(*r == 0 && r != r0);
    return (r - r0) + 1;
}

/* 
** Bitwise and
*/
static dsize_t I_band(ErtsDigit* x, dsize_t xl, short xsgn,
		      ErtsDigit* y, dsize_t yl, short ysgn, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    short sign = xsgn && ysgn;

    ASSERT(xl >= yl);

    xl -= yl;

    if (!xsgn) {
	if (!ysgn) {
	    while(yl--)
		*r++ = *x++ & *y++;
	}
	else {
	    ErtsDigit b;
	    ErtsDigit c;

	    DSUB(*y,1,b,c);
	    *r++ = *x++ & ~c;
	    y++;
	    yl--;
	    while(yl--) {
		DSUBb(*y,0,b,c);
		*r++ = *x++ & ~c;
		y++;
	    }
	    while (xl--) {
		*r++ = *x++;
	    }
	}
    }
    else {
	if (!ysgn) {
	    ErtsDigit b;
	    ErtsDigit c;

	    DSUB(*x,1,b,c);
	    *r = ~c & *y;
	    x++; y++; r++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b,c);
		*r++ = ~c & *y++;
		x++;
	    }
	}
	else {
	    ErtsDigit b1, b2;
	    ErtsDigit c1, c2;

	    DSUB(*x,1,b1,c1);
	    DSUB(*y,1,b2,c2);
	    *r++ = ~c1 & ~c2;
	    x++; y++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b1,c1);
		DSUBb(*y,0,b2,c2);
		*r++ = ~c1 & ~c2;
		x++; y++;
	    }
	    while(xl--) {
                DSUBb(*x,0,b1,c1);
                *r++ = ~c1;
		x++;
            }
	}
    }
    return I_btrail(r0, r, sign);
}

/* 
 * Bitwise 'or'.
 */
static dsize_t
I_bor(ErtsDigit* x, dsize_t xl, short xsgn, ErtsDigit* y,
      dsize_t yl, short ysgn, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    short sign = xsgn || ysgn;

    ASSERT(xl >= yl);

    xl -= yl;

    if (!xsgn) {
	if (!ysgn) {
	    while(yl--)
		*r++ = *x++ | *y++;
	    while(xl--)
		*r++ = *x++;
	}
	else {
	    ErtsDigit b;
	    ErtsDigit c;

	    DSUB(*y,1,b,c);
	    *r++ = *x++ | ~c;
	    y++;
	    yl--;
	    while(yl--) {
		DSUBb(*y,0,b,c);
		*r++ = *x++ | ~c;
		y++;
	    }
	}
    }
    else {
	if (!ysgn) {
	    ErtsDigit b;
	    ErtsDigit c;

	    DSUB(*x,1,b,c);
	    *r++ = ~c | *y++;
	    x++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b,c);
		*r++ = ~c | *y++;
		x++;
	    }
	    while(xl--) {
		DSUBb(*x,0,b,c);
 		*r++ = ~c;
 		x++;
	    }
	}
	else {
	    ErtsDigit b1, b2;
	    ErtsDigit c1, c2;

	    DSUB(*x,1,b1,c1);
	    DSUB(*y,1,b2,c2);
	    *r++ = ~c1 | ~c2;
	    x++; y++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b1,c1);
		DSUBb(*y,0,b2,c2);
		*r++ = ~c1 | ~c2;
		x++; y++;
	    }
	}
    }
    return I_btrail(r0, r, sign);
}

/* 
** Bitwise xor
*/
static dsize_t I_bxor(ErtsDigit* x, dsize_t xl, short xsgn,
		      ErtsDigit* y, dsize_t yl, short ysgn, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    short sign = xsgn != ysgn;

    ASSERT(xl >= yl);

    xl -= yl;

    if (!xsgn) {
	if (!ysgn) {
	    while(yl--)
		*r++ = *x++ ^ *y++;
	    while(xl--)
		*r++ = *x++;
	}
	else {
	    ErtsDigit b;
	    ErtsDigit c;

	    DSUB(*y,1,b,c);
	    *r++ = *x++ ^ ~c;
	    y++;
	    yl--;
	    while(yl--) {
		DSUBb(*y,0,b,c);
		*r++ = *x++ ^ ~c;
		y++;
	    }
	    while(xl--)
		*r++ = ~*x++;
	}
    }
    else {
	if (!ysgn) {
	    ErtsDigit b;
	    ErtsDigit c;

	    DSUB(*x,1,b,c);
	    *r++ = ~c ^ *y++;
	    x++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b,c);
		*r++ = ~c ^ *y++;
		x++;
	    }
	    while(xl--) {
		DSUBb(*x,0,b,c);
		*r++ = ~c;
		x++;
	    }
	}
	else {
	    ErtsDigit b1, b2;
	    ErtsDigit c1, c2;

	    DSUB(*x,1,b1,c1);
	    DSUB(*y,1,b2,c2);
	    *r++ = ~c1 ^ ~c2;
	    x++; y++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b1,c1);
		DSUBb(*y,0,b2,c2);
		*r++ = ~c1 ^ ~c2;
		x++; y++;
	    }
	    while(xl--) {
                DSUBb(*x,0,b1,c1);
		*r++ = c1;
                x++;
	    }
	}
    }
    return I_btrail(r0, r, sign);
}

/*
** Bitwise not  simulated as
** bnot -X  == (X - 1)
** bnot +X  == -(X + 1)
*/
static dsize_t I_bnot(ErtsDigit* x, dsize_t xl, short xsgn, ErtsDigit* r)
{
    if (xsgn)
	return D_add(x, xl, 1, r);
    else
	return D_sub(x, xl, 1, r);
}

/*
** Arithmetic left shift or right
*/
static dsize_t I_lshift(ErtsDigit* x, dsize_t xl, Sint y, 
			short sign, ErtsDigit* r)
{
    if (y == 0) {
	MOVE_DIGITS(r, x, xl);
	return xl;
    }
    else if (xl == 1 && *x == 0) {
	*r = 0;
	return 1;
    }
    else {
	Uint ay = (y < 0) ? -y : y;
	Uint bw = ay / D_EXP;
	Uint sw = ay % D_EXP;
	dsize_t rl;
	ErtsDigit a1=0;
	ErtsDigit a0=0;

	if (y > 0) {		/* shift left */
	    rl = xl + bw + 1;

	    while(bw--)
		*r++ = 0;
	    if (sw) {  /* NOTE! x >> 32 is not = 0! */
		while(xl--) {
		    a0 = (*x << sw) | a1;
		    a1 = (*x >> (D_EXP - sw));
		    *r++ = a0;
		    x++;
		}
	    }
	    else {
		while(xl--) {
		    *r++ = *x++;
		}
	    }
	    if (a1 == 0)
		return rl-1;
	    *r = a1;
	    return rl;
	}
	else {			/* shift right */
	    ErtsDigit* r0 = r;
	    int add_one = 0;

	    if (xl <= bw) {
		if (sign)
		    *r = 1;
		else
		    *r = 0;
		return 1;
	    }

	    if (sign) {
		Uint zl = bw;
		ErtsDigit* z = x;

		while(zl--) {
		    if (*z != 0) {
			add_one = 1;
			break;
		    }
		    z++;
		}
	    }

	    rl = xl - bw;
	    x += (xl-1);
	    r += (rl-1);
	    xl -= bw;
	    if (sw) { /* NOTE! x >> 32 is not = 0! */
		while(xl--) {
		    a1 = (*x >> sw) | a0;
		    a0 = (*x << (D_EXP-sw));
		    *r-- = a1;
		    x--;
		}
	    }
	    else {
		while(xl--) {
		    *r-- = *x--;
		}
	    }

	    if (sign && (a0 != 0))
		add_one = 1;

	    if (r[rl] == 0) {
		if (rl == 1) {
		    if (sign)
			r[1] = 1;
		    return 1;
		}
		rl--;
	    }
	    if (add_one)
		return D_add(r0, rl, 1, r0);
	    return rl;
	}
    }
}

/*
** Return log(x)/log(2)
*/
static int I_lg(ErtsDigit* x, dsize_t xl)
{
    dsize_t sz = xl - 1;
    ErtsDigit d = x[sz];

    sz *= D_EXP;
    while(d != 0) {
	d >>= 1;
	sz++;
    }
    return sz - 1;
}

/*
** Create bigint on heap if necessary. Like the previously existing
** make_small_or_big(), except for a HAlloc() instead of an
** ArithAlloc().
** NOTE: Only use erts_make_integer(), when order of heap fragments is
**       guaranteed to be correct.
*/
Eterm
erts_make_integer(Uint x, Process *p)
{
    Eterm* hp;
    if (IS_USMALL(0,x))
	return make_small(x);
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	return uint_to_big(x,hp);
    }
}

Eterm
erts_make_integer_fact(Uint x, ErtsHeapFactory *hf)
{
    Eterm* hp;
    if (IS_USMALL(0,x))
	return make_small(x);
    else {
	hp = erts_produce_heap(hf, BIG_UINT_HEAP_SIZE, 0);
	return uint_to_big(x, hp);
    }
}

/*
** convert Uint to bigint
** (must only be used if x is to big to be stored as a small)
*/
Eterm uint_to_big(Uint x, Eterm *y)
{
    *y = make_pos_bignum_header(1);
    BIG_DIGIT(y, 0) = x;
    return make_big(y);
}
/*
** convert UWord to bigint
** (must only be used if x is to big to be stored as a small)
** Allocation is tricky, the heap need has to be calculated
** with the macro BIG_UWORD_HEAP_SIZE(x)
*/

Eterm uword_to_big(UWord x, Eterm *y)
{
    *y = make_pos_bignum_header(1);
    BIG_DIGIT(y, 0) = x;
    return make_big(y);
}

/*
** convert signed int to bigint
*/
Eterm small_to_big(Sint x, Eterm *y)
{
    Uint xu;
    if (x >= 0) {
        xu = x;
	*y = make_pos_bignum_header(1);
    } else {
        xu = -(Uint)x;
	*y = make_neg_bignum_header(1);
    }
    BIG_DIGIT(y, 0) = xu;
    return make_big(y);
}


Eterm erts_uint64_to_big(Uint64 x, Eterm **hpp)
{
    Eterm *hp = *hpp;
#if defined(ARCH_32)
    if (x >= (((Uint64) 1) << 32)) {
	*hp = make_pos_bignum_header(2);
	BIG_DIGIT(hp, 0) = (Uint) (x & ((Uint) 0xffffffff));
	BIG_DIGIT(hp, 1) = (Uint) ((x >> 32) & ((Uint) 0xffffffff));
	*hpp += 3;
    }
    else
#endif
    {
	*hp = make_pos_bignum_header(1);
	BIG_DIGIT(hp, 0) = (Uint) x;
	*hpp += 2;
    }
    return make_big(hp);
}

Eterm erts_sint64_to_big(Sint64 x, Eterm **hpp)
{
    Eterm *hp = *hpp;
    Uint64 ux;
    int neg;
    if (x >= 0) {
	neg = 0;
        ux = x;
    }
    else {
	neg = 1;
	ux = -(Uint64)x;
    }
#if defined(ARCH_32)
    if (ux >= (((Uint64) 1) << 32)) {
	if (neg)
	    *hp = make_neg_bignum_header(2);
	else
	    *hp = make_pos_bignum_header(2);
	BIG_DIGIT(hp, 0) = (Uint) (ux & ((Uint) 0xffffffff));
	BIG_DIGIT(hp, 1) = (Uint) ((ux >> 32) & ((Uint) 0xffffffff));
	*hpp += 3;
    }
    else
#endif
    {
	if (neg)
	    *hp = make_neg_bignum_header(1);
	else
	    *hp = make_pos_bignum_header(1);
	BIG_DIGIT(hp, 0) = (Uint) ux;
	*hpp += 2;
    }
    return make_big(hp);
}

Eterm
erts_uint64_array_to_big(Uint **hpp, int neg, int len, Uint64 *array)
{
    Uint *headerp;
    int i, pot_digits, digits;

    headerp = *hpp;

    pot_digits = digits = 0;
    for (i = 0; i < len; i++) {
#if defined(ARCH_32)
	Uint low_val = array[i] & ((Uint) 0xffffffff);
	Uint high_val = (array[i] >> 32) & ((Uint) 0xffffffff);
	BIG_DIGIT(headerp, pot_digits) = low_val;
	pot_digits++;
	if (low_val)
	    digits = pot_digits;
	BIG_DIGIT(headerp, pot_digits) = high_val;
	pot_digits++;
	if (high_val)
	    digits = pot_digits;
#else
	Uint val = array[i];
	BIG_DIGIT(headerp, pot_digits) = val;
	pot_digits++;
	if (val)
	    digits = pot_digits;
#endif
    }	

    if (neg)
	*headerp = make_neg_bignum_header(digits);
    else
	*headerp = make_pos_bignum_header(digits);

    *hpp = headerp + 1 + digits;

    return make_big(headerp);
}

/*
** Convert a bignum to a double float
*/
int
big_to_double(Eterm x, double* resp)
{
    double d = 0.0;
    Eterm* xp = big_val(x);
    dsize_t xl = BIG_SIZE(xp);
    ErtsDigit* s = BIG_V(xp) + xl;
    short xsgn = BIG_SIGN(xp);
    double dbase = ((double)(D_MASK)+1);

    while (xl--) {
        d = d * dbase + *--s;

        if (!erts_isfinite(d)) {
            return -1;
        }
    }

    *resp = xsgn ? -d : d;
    return 0;
}

/*
 * Logic has been copied from erl_bif_guard.c and slightly
 * modified to use a static instead of dynamic heap
 */
Eterm
double_to_big(double x, Eterm *heap, Uint hsz)
{
    int is_negative;
    int ds;
    ErtsDigit* xp;
    Eterm res;
    int i;
    size_t sz;
    Eterm* hp;
    double dbase;

    if (x >= 0) {
	is_negative = 0;
    } else {
	is_negative = 1;
	x = -x;
    }

    /* Unscale & (calculate exponent) */
    ds = 0;
    dbase = ((double) (D_MASK) + 1);
    while (x >= 1.0) {
	x /= dbase; /* "shift" right */
	ds++;
    }
    sz = BIG_NEED_SIZE(ds); /* number of words including arity */

    hp = heap;
    res = make_big(hp);
    xp = (ErtsDigit*) (hp + 1);

    ASSERT(ds < hsz);
    for (i = ds - 1; i >= 0; i--) {
	ErtsDigit d;

	x *= dbase; /* "shift" left */
	d = x; /* trunc */
	xp[i] = d; /* store digit */
	x -= d; /* remove integer part */
    }

    if (is_negative) {
	*hp = make_neg_bignum_header(sz-1);
    } else {
	*hp = make_pos_bignum_header(sz-1);
    }
    return res;
}


/*
 ** Estimate the number of digits in given base (include sign)
 */
int big_integer_estimate(Eterm x, Uint base)
{
    Eterm* xp = big_val(x);
    int lg = I_lg(BIG_V(xp), BIG_SIZE(xp));
    int lgBase = ((lg + 1) / lookup_log2(base)) + 1;

    if (BIG_SIGN(xp)) lgBase++;	/* add sign */
    return lgBase + 1;		/* add null */
}

/*
** Single-digit-extraction (schoolbook) renderer.
**
** Renders v[0..vl] into out_end backwards (out_end is decremented past
** each char written), most-significant char ending up at the lowest
** address. Returns the number of chars written.
**
** If `width` is non-zero, the output is left-padded with leading zeros
** to exactly `width` chars (used by the recursive divide-and-conquer
** renderer for the low halves of splits). If `width` is zero, only
** significant digits are emitted.
**
** Destroys v.
*/
static Uint write_big_simple(ErtsDigit *v, dsize_t vl, int base,
                             char **out_end_pp, Uint width)
{
    char *p = *out_end_pp;
    Uint n = 0;
    const Uint digits_per_Sint = get_digits_per_signed_int(base);
    const ErtsDigit largest_pow = (ErtsDigit) get_largest_power_of_base(base);
    ErtsDigit rem;

    if (vl == 1 && v[0] < largest_pow) {
        rem = v[0];
        if (rem == 0 && width == 0) {
            *--p = '0';
            n++;
        } else {
            while (rem) {
                int d = rem % base;
                *--p = (d < 10) ? (char)('0' + d) : (char)('A' + d - 10);
                rem /= base;
                n++;
            }
        }
    } else {
        while (1) {
            vl = D_div(v, vl, largest_pow, v, &rem);
            if (vl == 1 && v[0] == 0) {
                while (rem) {
                    int d = rem % base;
                    *--p = (d < 10) ? (char)('0' + d) : (char)('A' + d - 10);
                    rem /= base;
                    n++;
                }
                break;
            } else {
                Uint i = digits_per_Sint;
                while (i--) {
                    int d = rem % base;
                    *--p = (d < 10) ? (char)('0' + d) : (char)('A' + d - 10);
                    rem /= base;
                    n++;
                }
            }
        }
    }

    /* Pad with leading zeros to `width` (when used for a fixed-width slot). */
    while (n < width) {
        *--p = '0';
        n++;
    }

    *out_end_pp = p;
    return n;
}

/*
** Divide-and-conquer integer-to-string.
**
** The single-digit-extraction loop in write_big_simple is O(N^2) in the
** number of decimal digits because each pass divides an N-digit value
** by a single ErtsDigit (cost O(N)) and there are N/digits_per_Sint
** passes.
**
** The divide-and-conquer wrapper splits an N-digit value at base^(N/2)
** via a single bignum divmod, recurses on each half, and writes the
** halves into adjacent positions in the output buffer (the low half
** zero-padded to fill its width). Below WRITE_BIG_DC_THRESHOLD decimal
** digits the schoolbook routine is used.
**
** Asymptotically the recurrence is T(N) = 2 T(N/2) + Div(N, N/2). With
** the existing O(xl*yl) Knuth-D division (I_div) this is still O(N^2),
** so the win is constant-factor. With a sub-quadratic division
** (Burnikel-Ziegler) the recurrence becomes sub-quadratic.
*/

#ifndef WRITE_BIG_DC_THRESHOLD
#define WRITE_BIG_DC_THRESHOLD 250
#endif

struct dc_pow_cache {
    int n_levels;
    Uint *widths;        /* widths[i] = base-`base` digit width of vals[i] */
    dsize_t *sizes;      /* sizes[i] = ErtsDigit count of vals[i] */
    ErtsDigit **vals;    /* vals[i] = base ^ widths[i] */
    /* Barrett reciprocals:
     *   mus[i] = floor(beta^(2*sizes[i] + 1) / vals[i])
     * with one extra ErtsDigit of precision so the quotient estimate
     * is correct after at most 2 corrections even though vals[i] is
     * not normalized (top bit set). Per-divmod cost drops from BZ's
     * O(M(n) log n) to Barrett's O(M(n)) — one log-factor saved per
     * recursion level in the render D&C tree. */
    dsize_t *mu_sizes;
    ErtsDigit **mus;
    void *meta_alloc;    /* metadata arrays */
    ErtsDigit *backing;  /* digit storage for all powers */
    ErtsDigit *mu_backing; /* digit storage for all Barrett reciprocals */
};

/* Compute base^n into out (capacity must be sufficient). Returns size. */
static dsize_t compute_base_to_n(int base, Uint n, ErtsDigit *out)
{
    dsize_t sz = 1;
    out[0] = 1;
    while (n--) {
        sz = D_mul(out, sz, (ErtsDigit) base, out);
    }
    return sz;
}

/*
** Build the power cache for splitting a value of up to `target_width`
** decimal digits. Levels: vals[0] = base^T, vals[1] = base^(2T),
** vals[2] = base^(4T), ..., up to a level whose width is >= target_width.
** This guarantees we always have a power suitable for splitting at
** width/2 of any width <= target_width that exceeds T.
*/
static int build_dc_powers(struct dc_pow_cache *c, int base, Uint target_width)
{
    Uint t = WRITE_BIG_DC_THRESHOLD;
    int n = 0;
    Uint w;

    c->n_levels = 0;
    c->widths = NULL;
    c->sizes = NULL;
    c->vals = NULL;
    c->mu_sizes = NULL;
    c->mus = NULL;
    c->meta_alloc = NULL;
    c->backing = NULL;
    c->mu_backing = NULL;

    if (target_width <= t) {
        return 1;
    }

    /* Count levels with width T, 2T, 4T, ... while the level is < target_width.
     * We never split at a power whose width >= the input value's width, so
     * those levels would be dead weight (and pay mu-build overhead). */
    w = t;
    n = 1;
    while (w * 2 < target_width) {
        if (w > (UINT_MAX / 2)) {
            return 0;
        }
        w *= 2;
        n++;
    }

    /* Total digit storage: each level i has size <= ceil(widths[i] *
     * log2(base) / D_EXP) + 1 ErtsDigits. The sum across geometric
     * widths is bounded by 2 * top_size. We size pessimistically. */

    {
        Uint top_width = w;  /* widths[n-1] */
        Uint top_size_est;
        Uint total_size;
        Uint mu_total_size;
        char *meta_buf;
        Uint meta_size;
        int i;
        ErtsDigit *cur;
        ErtsDigit *mu_cur;

        /* Per-level upper bound: width * log2(base) / D_EXP + 2. The lookup
         * table indices give (width * lg2(base)) bits; divide by D_EXP and
         * round up, plus a slack digit. */
        top_size_est = (Uint) (((double) top_width * lookup_log2(base)) / D_EXP) + 2;
        /* Sum of sizes is <= 2 * top_size_est (geometric). Add per-level slack. */
        total_size = 2 * top_size_est + (Uint) n * 2;
        /* mu has <= sizes[i] + 2 ErtsDigits. Total mu storage <= 2x of vals. */
        mu_total_size = 2 * (top_size_est + 2) + (Uint) n * 4;

        meta_size = (sizeof(Uint) + sizeof(dsize_t) + sizeof(ErtsDigit *)
                     + sizeof(dsize_t) + sizeof(ErtsDigit *)) * (Uint) n;
        meta_buf = (char *) erts_alloc(ERTS_ALC_T_TMP, meta_size);
        c->meta_alloc = meta_buf;
        c->widths    = (Uint *)        meta_buf;
        c->sizes     = (dsize_t *)    (meta_buf + sizeof(Uint) * (Uint) n);
        c->vals      = (ErtsDigit **) (meta_buf + (sizeof(Uint) + sizeof(dsize_t)) * (Uint) n);
        c->mu_sizes  = (dsize_t *)    (meta_buf + (sizeof(Uint) + sizeof(dsize_t) + sizeof(ErtsDigit *)) * (Uint) n);
        c->mus       = (ErtsDigit **) (meta_buf + (sizeof(Uint) + 2*sizeof(dsize_t) + sizeof(ErtsDigit *)) * (Uint) n);
        c->backing = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                              sizeof(ErtsDigit) * total_size);
        /* mu_backing is allocated lazily in the loop below if any level
         * is large enough to merit a Barrett reciprocal. */
        c->mu_backing = NULL;

        /* Level 0: base^T computed directly. */
        cur = c->backing;
        c->vals[0] = cur;
        c->widths[0] = t;
        c->sizes[0] = compute_base_to_n(base, t, cur);
        cur += c->sizes[0] + 1;
        ASSERT(cur <= c->backing + total_size);

        /* Levels 1..n-1: square the previous level. */
        for (i = 1; i < n; i++) {
            ErtsDigit *prev = c->vals[i-1];
            dsize_t prev_sz = c->sizes[i-1];
            dsize_t sq_sz;

            c->vals[i] = cur;
            c->widths[i] = c->widths[i-1] * 2;
            sq_sz = I_sqr(prev, prev_sz, cur);
            /* I_sqr writes up to 2*prev_sz+1 cells; trim trailing zeros. */
            while (sq_sz > 1 && cur[sq_sz - 1] == 0) {
                sq_sz--;
            }
            c->sizes[i] = sq_sz;
            cur += sq_sz + 1;
            ASSERT(cur <= c->backing + total_size);
        }

        /* For each level large enough to benefit, compute
         *   mu = floor(beta^(2*sizes[i] + 1) / vals[i]).
         * Numerator is a single 1 at position 2*sizes[i] + 1, zeros elsewhere.
         * Use I_div_dispatch which picks BZ for large divisors automatically.
         *
         * Below BARRETT_LEVEL_THRESHOLD ErtsDigits, the per-call cost of
         * Barrett (two Karatsuba mults + allocs) doesn't pay back the
         * mu-build overhead, and the underlying division is in the regime
         * where I_div is faster than BZ anyway. Skip mu for those levels;
         * barrett_divmod will fall through to I_div_dispatch when mu is
         * NULL. */
        mu_cur = NULL;
        /* Quick pre-pass: zero out mu metadata; common case is the
         * top-level cache levels are too small to merit Barrett, so
         * we want this no-mu fast-path well-isolated from the heavier
         * mu-build loop body for the compiler. */
        {
            int any_mu = 0;
            int j_pre;
            for (j_pre = 0; j_pre < n; j_pre++) {
                c->mus[j_pre] = NULL;
                c->mu_sizes[j_pre] = 0;
                if (c->sizes[j_pre] >= BARRETT_LEVEL_THRESHOLD) any_mu = 1;
            }
            if (!any_mu) {
                c->n_levels = n;
                return 1;
            }
        }
        for (i = 0; i < n; i++) {
            dsize_t pn = c->sizes[i];
            dsize_t numer_sz;
            ErtsDigit *numer;
            ErtsDigit *qbuf, *rbuf;
            dsize_t qsz, rsz;
            dsize_t j;

            if (pn < BARRETT_LEVEL_THRESHOLD) {
                continue;
            }

            /* Lazy alloc mu_backing on first level that needs it. */
            if (c->mu_backing == NULL) {
                c->mu_backing = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                                         sizeof(ErtsDigit) * mu_total_size);
                mu_cur = c->mu_backing;
            }

            numer_sz = 2 * pn + 2;
            numer = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                             sizeof(ErtsDigit) * numer_sz);
            for (j = 0; j < numer_sz; j++) numer[j] = 0;
            numer[2 * pn + 1] = 1;  /* beta^(2*pn + 1) */

            qbuf = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                            sizeof(ErtsDigit) * (numer_sz - pn + 2));
            rbuf = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                            sizeof(ErtsDigit) * numer_sz);

            qsz = I_div_dispatch(numer, numer_sz, c->vals[i], pn, qbuf, rbuf, &rsz);
            while (qsz > 1 && qbuf[qsz - 1] == 0) qsz--;

            c->mus[i] = mu_cur;
            c->mu_sizes[i] = qsz;
            for (j = 0; j < qsz; j++) mu_cur[j] = qbuf[j];
            mu_cur += qsz + 1;
            ASSERT(mu_cur <= c->mu_backing + mu_total_size);

            erts_free(ERTS_ALC_T_TMP, numer);
            erts_free(ERTS_ALC_T_TMP, qbuf);
            erts_free(ERTS_ALC_T_TMP, rbuf);
        }

        c->n_levels = n;
    }

    return 1;
}

static void free_dc_powers(struct dc_pow_cache *c)
{
    if (c->mu_backing) {
        erts_free(ERTS_ALC_T_TMP, c->mu_backing);
    }
    if (c->backing) {
        erts_free(ERTS_ALC_T_TMP, c->backing);
    }
    if (c->meta_alloc) {
        erts_free(ERTS_ALC_T_TMP, c->meta_alloc);
    }
}

/*
** Barrett divmod by vals[level] using the precomputed reciprocal mus[level].
**
** Computes q = floor(v / d), r = v - q*d, where d = vals[level].
**
** Algorithm:
**   1. prod = v * mu
**   2. q_hat = prod >> ((2*dsz + 1) * D_EXP) bits = top digits of prod
**   3. r_hat = v - q_hat * d
**   4. Correct: while r_hat >= d, r_hat -= d, q_hat += 1 (at most 2 iters)
**
** Caller's q has capacity vl - dsz + 1; r has capacity vl. Returns sizes via
** output params. v is read-only here (unlike I_div which destroys it via the
** scratch r buffer).
*/
static dsize_t barrett_divmod(ErtsDigit *v, dsize_t vl,
                              int level, const struct dc_pow_cache *c,
                              ErtsDigit *q, ErtsDigit *r, dsize_t *rsz_out)
{
    ErtsDigit *d = c->vals[level];
    dsize_t dsz = c->sizes[level];
    ErtsDigit *mu = c->mus[level];
    dsize_t musz = c->mu_sizes[level];

    dsize_t v_trim = vl;
    dsize_t mu_trim = musz;
    dsize_t qhat_offset = 2 * dsz + 1;
    dsize_t prod_cap;
    dsize_t prod_sz;
    ErtsDigit *prod;
    dsize_t qhat_sz;
    ErtsDigit *qhat;
    dsize_t qd_cap;
    dsize_t qd_sz;
    ErtsDigit *qd;
    dsize_t rsz;
    dsize_t i;

    /* Fall back to I_div_dispatch when this level wasn't worth a Barrett
     * reciprocal (small divisor — mu-build cost outweighs per-call savings).
     * The render D&C callers don't read v after divmod, so it's safe to
     * let I_div destroy it. */
    if (mu == NULL) {
        return I_div_dispatch(v, vl, d, dsz, q, r, rsz_out);
    }

    /* Trim leading zeros from v and mu before passing to Karatsuba.
     * Karatsuba assumes normalized inputs (single zero digit, or top
     * digit non-zero); leading zeros there cause out-of-bounds reads. */
    while (v_trim > 1 && v[v_trim - 1] == 0) v_trim--;
    while (mu_trim > 1 && mu[mu_trim - 1] == 0) mu_trim--;

    /* Special case: v < d. */
    if (I_comp(v, v_trim, d, dsz) < 0) {
        q[0] = 0;
        for (i = 0; i < v_trim; i++) r[i] = v[i];
        rsz = v_trim;
        if (rsz == 0) rsz = 1;
        *rsz_out = rsz;
        return 1;
    }

    /* prod = v * mu, full product. The +BARRETT_MAX_CORRECTIONS slack lets
     * the correction loop's D_add carries grow qhat (a slice into prod) up
     * to its theoretical worst case without overrunning the buffer. */
    prod_cap = v_trim + mu_trim + 1 + BARRETT_MAX_CORRECTIONS;
    prod = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsDigit) * prod_cap);
    if (v_trim >= mu_trim) {
        prod_sz = I_mul_karatsuba(v, v_trim, mu, mu_trim, prod);
    } else {
        prod_sz = I_mul_karatsuba(mu, mu_trim, v, v_trim, prod);
    }
    while (prod_sz > 1 && prod[prod_sz - 1] == 0) prod_sz--;

    /* q_hat = prod >> (qhat_offset digits). */
    if (prod_sz <= qhat_offset) {
        /* q_hat is 0; rare for valid inputs but handle gracefully. */
        q[0] = 0;
        qhat = q;
        qhat_sz = 1;
    } else {
        qhat_sz = prod_sz - qhat_offset;
        qhat = prod + qhat_offset;  /* in-place slice of prod */
    }

    /* qd = q_hat * d. */
    qd_cap = qhat_sz + dsz + 1;
    qd = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsDigit) * qd_cap);
    if (qhat_sz == 1 && qhat[0] == 0) {
        qd[0] = 0;
        qd_sz = 1;
    } else if (qhat_sz >= dsz) {
        qd_sz = I_mul_karatsuba(qhat, qhat_sz, d, dsz, qd);
    } else {
        qd_sz = I_mul_karatsuba(d, dsz, qhat, qhat_sz, qd);
    }
    while (qd_sz > 1 && qd[qd_sz - 1] == 0) qd_sz--;

    /* r = v - qd. By Barrett property, qd <= v always (q_hat <= q_real). */
    if (I_comp(v, v_trim, qd, qd_sz) < 0) {
        /* Should not happen with the +1 ErtsDigit precision; safety
         * fallback. v is unmodified at this point so I_div_dispatch
         * may destroy it. */
        ERTS_INTERNAL_ERROR("cannot happen");
    }
    rsz = I_sub(v, v_trim, qd, qd_sz, r);

    /* Correction: at most a couple of iterations. */
    {
        int corrections = 0;
        while (I_comp(r, rsz, d, dsz) >= 0) {
            rsz = I_sub(r, rsz, d, dsz, r);
            ASSERT(qhat + qhat_sz < prod + prod_cap);
            qhat_sz = D_add(qhat, qhat_sz, 1, qhat);
            corrections++;
            if (corrections > BARRETT_MAX_CORRECTIONS) {
                /* Should not exceed 2 in theory; safety fallback. v is
                 * unmodified by the steps above (I_sub reads only). */
                ERTS_INTERNAL_ERROR("too many corrections");
            }
        }
    }

    /* Copy qhat to caller's q (qhat may alias prod's interior). */
    if (qhat != q) {
        for (i = 0; i < qhat_sz; i++) q[i] = qhat[i];
    }
    if (rsz == 0) rsz = 1;
    *rsz_out = rsz;

    erts_free(ERTS_ALC_T_TMP, prod);
    erts_free(ERTS_ALC_T_TMP, qd);
    return qhat_sz;
}

/*
** Render v of size vl into out_end (writing backwards, decrementing
** *out_end_pp), padded to exactly `width` decimal digits.
**
** Destroys v. The caller owns v's storage.
*/
static void write_big_dc_padded(ErtsDigit *v, dsize_t vl, int base,
                                Uint width, char **out_end_pp,
                                const struct dc_pow_cache *c)
{
    int i;
    dsize_t pl;
    Uint pw;
    ErtsDigit *p;
    ErtsDigit *q, *r;
    dsize_t qsz, rsz;
    Uint hi_width;
    Uint n;

    if (width <= WRITE_BIG_DC_THRESHOLD) {
        n = write_big_simple(v, vl, base, out_end_pp, width);
        ASSERT(n == width);
        (void) n;
        return;
    }

    /* Find largest level i with widths[i] <= width/2. */
    i = c->n_levels - 1;
    while (i >= 0 && c->widths[i] > width / 2) {
        i--;
    }
    if (i < 0) {
        /* width > THRESHOLD but all cache widths exceed width/2 — only
         * possible when no cache was built. */
        ERTS_INTERNAL_ERROR("unexpected missing cache");
    }

    p = c->vals[i];
    pl = c->sizes[i];
    pw = c->widths[i];
    hi_width = width - pw;

    /* If v < powers[i] then the high half is zero — emit zero pad and
     * recurse on the low half = v. */
    if (I_comp(v, vl, p, pl) < 0) {
        write_big_dc_padded(v, vl, base, pw, out_end_pp, c);
        /* Pad the missing high half with zeros. */
        {
            char *cp = *out_end_pp;
            Uint k = hi_width;
            while (k--) {
                *--cp = '0';
            }
            *out_end_pp = cp;
        }
        return;
    }

    /* divmod: q = v / p, r = v % p, using the precomputed Barrett mu. */
    {
        ErtsDigit *scratch;
        dsize_t q_cap = vl - pl + 1;
        dsize_t r_cap = vl;

        scratch = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                           sizeof(ErtsDigit) * (q_cap + r_cap));
        q = scratch;
        r = scratch + q_cap;
        qsz = barrett_divmod(v, vl, i, c, q, r, &rsz);
    }

    /* Render low half (padded to pw) first — it ends up at higher
     * addresses in the buffer, then high half before it. */
    write_big_dc_padded(r, rsz, base, pw, out_end_pp, c);
    write_big_dc_padded(q, qsz, base, hi_width, out_end_pp, c);

    erts_free(ERTS_ALC_T_TMP, q);  /* q points at start of scratch */
}

/*
** Top-level renderer (no padding). Walks the cache to find the largest
** power that fits, splits there, recurses padded on the low half, and
** recurses unpadded on the high half.
**
** Destroys v. Returns total chars written.
*/
static Uint write_big_dc_top(ErtsDigit *v, dsize_t vl, int base,
                             char **out_end_pp,
                             const struct dc_pow_cache *c)
{
    int i;
    dsize_t pl;
    Uint pw;
    ErtsDigit *q, *r;
    dsize_t qsz, rsz;
    Uint n;

    /* Walk down the cache to the largest power <= v. */
    i = c->n_levels - 1;
    while (i >= 0 && I_comp(v, vl, c->vals[i], c->sizes[i]) < 0) {
        i--;
    }

    if (i < 0) {
        /* v < base^T, fits in the schoolbook routine without recursion. */
        return write_big_simple(v, vl, base, out_end_pp, 0);
    }

    pl = c->sizes[i];
    pw = c->widths[i];

    /* divmod via Barrett. */
    {
        ErtsDigit *scratch;
        dsize_t q_cap = vl - pl + 1;
        dsize_t r_cap = vl;

        scratch = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP,
                                           sizeof(ErtsDigit) * (q_cap + r_cap));
        q = scratch;
        r = scratch + q_cap;
        qsz = barrett_divmod(v, vl, i, c, q, r, &rsz);
    }

    /* Low half: padded to pw chars. */
    write_big_dc_padded(r, rsz, base, pw, out_end_pp, c);
    /* High half: top-level (unpadded). */
    n = write_big_dc_top(q, qsz, base, out_end_pp, c);

    erts_free(ERTS_ALC_T_TMP, q);
    return n + pw;
}

/*
** Convert a bignum into a string of numbers in given base.
**
** For bignums whose decimal width exceeds WRITE_BIG_DC_THRESHOLD the
** divide-and-conquer renderer is used (write_big_dc_top). Smaller
** values use the schoolbook routine directly. Output is delivered to
** the caller's per-char write_func in least-significant-first order
** (matching the order produced by single-digit extraction), so the
** existing write_string and write_list callbacks continue to work
** unchanged.
*/
static Uint write_big(Eterm x, int base, void (*write_func)(void *, char),
                      void *arg)
{
    Eterm* xp = big_val(x);
    ErtsDigit* dx = BIG_V(xp);
    dsize_t xl = BIG_SIZE(xp);
    short sign = BIG_SIGN(xp);
    Uint n = 0;
    Uint width_estimate;
    int use_dc;

    /* Quick path for one-ErtsDigit values. */
    {
        const ErtsDigit largest_pow = (ErtsDigit) get_largest_power_of_base(base);
        if (xl == 1 && dx[0] < largest_pow) {
            ErtsDigit rem = dx[0];
            if (rem == 0) {
                (*write_func)(arg, '0'); n++;
            } else {
                while (rem) {
                    int d = rem % base;
                    (*write_func)(arg, (d < 10) ? (char)('0' + d) : (char)('A' + d - 10));
                    rem /= base;
                    n++;
                }
            }
            if (sign) { (*write_func)(arg, '-'); n++; }
            return n;
        }
    }

    width_estimate = (Uint) (((double) xl * D_EXP) / lookup_log2(base)) + 2;
    /* Only invoke D&C when there's enough work to recurse at least once;
     * otherwise the cache-build overhead dominates the saving. */
    use_dc = (width_estimate >= 2 * WRITE_BIG_DC_THRESHOLD);

    if (!use_dc) {
        /* Small-bignum schoolbook path: write into a scratch buffer
         * back-to-front, then feed chars to write_func in the same
         * order the original loop did (least significant first). */
        ErtsDigit *tmp;
        char *buf;
        char *out_end;
        Uint i;

        tmp = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsDigit) * xl);
        MOVE_DIGITS(tmp, dx, xl);

        buf = (char *) erts_alloc(ERTS_ALC_T_TMP, width_estimate);
        out_end = buf + width_estimate;
        n = write_big_simple(tmp, xl, base, &out_end, 0);
        /* out_end now points at the first (most-significant) char. */
        for (i = 0; i < n; i++) {
            (*write_func)(arg, out_end[n - 1 - i]);
        }
        erts_free(ERTS_ALC_T_TMP, buf);
        erts_free(ERTS_ALC_T_TMP, tmp);
    } else {
        ErtsDigit *tmp;
        char *buf;
        char *out_end;
        struct dc_pow_cache cache;
        Uint i;

        tmp = (ErtsDigit *) erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsDigit) * xl);
        MOVE_DIGITS(tmp, dx, xl);

        if (!build_dc_powers(&cache, base, width_estimate)) {
            /* Cache build refused (overflow); should be impossible. */
            ERTS_INTERNAL_ERROR("cache build failed");
        } else {
            buf = (char *) erts_alloc(ERTS_ALC_T_TMP, width_estimate);
            out_end = buf + width_estimate;
            n = write_big_dc_top(tmp, xl, base, &out_end, &cache);
            for (i = 0; i < n; i++) {
                (*write_func)(arg, out_end[n - 1 - i]);
            }
            free_dc_powers(&cache);
            erts_free(ERTS_ALC_T_TMP, buf);
            erts_free(ERTS_ALC_T_TMP, tmp);
        }
    }

    if (sign) {
        (*write_func)(arg, '-'); n++;
    }

    return n;
}

struct big_list__ {
    Eterm *hp;
    Eterm res;
};

static void
write_list(void *arg, char c)
{
    struct big_list__ *blp = (struct big_list__ *) arg;
    blp->res = CONS(blp->hp, make_small(c), blp->res);
    blp->hp += 2;
}

Eterm erts_big_to_list(Eterm x, int base, Eterm **hpp)
{
    struct big_list__ bl;
    bl.hp = *hpp;
    bl.res = NIL;
    write_big(x, base, write_list, (void *) &bl);
    *hpp = bl.hp;
    return bl.res;
}

static void
write_string(void *arg, char c)
{
    *(--(*((char **) arg))) = c;
}

char *erts_big_to_string(Eterm x, int base, char *buf, Uint buf_sz)
{
    char *big_str = buf + buf_sz - 1;
    *big_str = '\0';
    write_big(x, base, write_string, (void*)&big_str);
    ASSERT(buf <= big_str && big_str <= buf + buf_sz - 1);
    return big_str;
}

/* Bignum to binary bytes
 * e.g. 1 bsl 64 -> "18446744073709551616"
 */

Uint erts_big_to_binary_bytes(Eterm x, int base, char *buf, Uint buf_sz)
{
    char *big_str = buf + buf_sz;
    Uint n;
    n = write_big(x, base, write_string, (void *) &big_str);
    ASSERT(buf <= big_str && big_str <= buf + buf_sz);
    return n;
}


/*
** Normalize a bignum given thing pointer length in digits and a sign
** patch zero if odd length
*/
static Eterm big_norm(Eterm *x, dsize_t xl, short sign)
{
    Uint arity;

    if (xl == 1) {
	Uint y = BIG_DIGIT(x, 0);

	if (D_EXP < SMALL_BITS || IS_USMALL(sign, y)) {
	    if (sign)
		return make_small(-((Sint)y));
	    else
		return make_small(y);
	}
    }

    /* __alpha__: This was fixed */
    if ((arity = BIG_NEED_SIZE(xl)-1) > BIG_ARITY_MAX)
      return NIL;  /* signal error (too big) */

    if (sign) {
      *x = make_neg_bignum_header(arity);
    }
    else {
      *x = make_pos_bignum_header(arity);
    }
    return make_big(x);
}

/*
** Compare bignums
*/
int big_comp(Eterm x, Eterm y)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    if (BIG_SIGN(xp) == BIG_SIGN(yp)) {
	int c = I_comp(BIG_V(xp), BIG_SIZE(xp), BIG_V(yp), BIG_SIZE(yp));
	if (BIG_SIGN(xp))
	    return -c;
	else
	    return c;
    }
    else
	return BIG_SIGN(xp) ? -1 : 1;
}

/*
** Unsigned compare
*/
int big_ucomp(Eterm x, Eterm y)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    return I_comp(BIG_V(xp), BIG_SIZE(xp), BIG_V(yp), BIG_SIZE(yp));
}

/*
** Return number of bytes in the bignum
*/
dsize_t big_bytes(Eterm x)
{
    Eterm* xp = big_val(x);
    dsize_t sz = BIG_SIZE(xp);
    ErtsDigit d = BIG_DIGIT(xp, sz-1);

    sz = (sz-1) * sizeof(ErtsDigit);
    while (d != 0) {
	++sz;
	d >>= 8;
    }
    return sz;
}

/*
** Load a bignum from bytes
** xsz is the number of bytes in xp
** *r is untouched if number fits in small
*/
Eterm bytes_to_big(const byte *xp, dsize_t xsz, int xsgn, Eterm *r)
{
    ErtsDigit* rwp = BIG_V(r);
    dsize_t rsz = 0;
    ErtsDigit d;
    int i;

    while(xsz > sizeof(ErtsDigit)) {
	d = 0;
	for(i = sizeof(ErtsDigit); --i >= 0;)
	    d = (d << 8) | xp[i];
	*rwp = d;
	rwp++;
	xsz -= sizeof(ErtsDigit);
	xp += sizeof(ErtsDigit);
	rsz++;
    }

    if (xsz > 0) {
	d = 0;
	for(i = xsz; --i >= 0;)
	    d = (d << 8) | xp[i];
	if (++rsz == 1 && IS_USMALL(xsgn,d)) {
	    if (xsgn) d = -d;
	    return make_small(d);
	}
	*rwp = d;
	rwp++;
    }
    if (rsz > BIG_ARITY_MAX)
	return NIL;
    if (xsgn) {
      *r = make_neg_bignum_header(rsz);
    }
    else {
      *r = make_pos_bignum_header(rsz);
    }
    return make_big(r);
}

/*
** Store digits in the array of bytes pointed to by p
*/
byte* big_to_bytes(Eterm x, byte *p)
{
    ErtsDigit* xr = big_v(x);
    dsize_t  xl = big_size(x);
    ErtsDigit d;
    int i;

    while(xl > 1) {
	d = *xr;
	xr++;
	for(i = 0; i < sizeof(ErtsDigit); ++i) {
	    p[i] = d & 0xff;
	    d >>= 8;
	}
	p += sizeof(ErtsDigit);
	xl--;
    }
    d = *xr;
    do {
	*p++ = d & 0xff;
	d >>= 8;
    } while (d != 0);
    return p;
}

/*
 * Converts a positive term (small or bignum) to an Uint.
 *
 * Fails returning 0 if the term is neither a small nor a bignum,
 * if it's negative, or the big number does not fit in an Uint;
 * in addition the error reason, BADARG or SYSTEM_LIMIT, will be
 * stored in *up.
 *
 * Otherwise returns a non-zero value and the converted number
 * in *up.
 */

int
term_to_Uint(Eterm term, Uint *up)
{
    if (is_small(term)) {
	Sint i = signed_val(term);
	if (i < 0) {
	    *up = BADARG;
	    return 0;
	}
	*up = (Uint) i;
	return 1;
    } else if (is_big(term)) {
	ErtsDigit* xr = big_v(term);
	dsize_t  xl = big_size(term);
	Uint uval = 0;
	int n = 0;
	
	if (big_sign(term)) {
	    *up = BADARG;
	    return 0;
	} else if (xl*D_EXP > sizeof(Uint)*8) {
	    *up = SYSTEM_LIMIT;
	    return 0;
	}
	while (xl-- > 0) {
            ASSERT(n < 64);
	    uval |= ((Uint)(*xr++)) << n;
	    n += D_EXP;
	}
	*up = uval;
	return 1;
    } else {
	*up = BADARG;
	return 0;
    }
}

/* same as term_to_Uint()
   but also accept larger bignums by masking
 */
int
term_to_Uint_mask(Eterm term, Uint *up)
{
    if (is_small(term)) {
	Sint i = signed_val(term);
	if (i < 0) {
	    *up = BADARG;
	    return 0;
	}
	*up = (Uint) i;
	return 1;
    } else if (is_big(term) && !big_sign(term)) {
	ErtsDigit* xr = big_v(term);

	ERTS_CT_ASSERT(sizeof(ErtsDigit) == sizeof(Uint));
	*up = (Uint)*xr;  /* just pick first word */
	return 1;
    } else {
	*up = BADARG;
	return 0;
    }
}

int
term_to_UWord(Eterm term, UWord *up)
{
#if SIZEOF_VOID_P == ERTS_SIZEOF_ETERM
    return term_to_Uint(term,up);
#else
    if (is_small(term)) {
	Sint i = signed_val(term);
	if (i < 0) {
	    *up = BADARG;
	    return 0;
	}
	*up = (UWord) i;
	return 1;
    } else if (is_big(term)) {
	ErtsDigit* xr = big_v(term);
	dsize_t  xl = big_size(term);
	UWord uval = 0;
	int n = 0;

	if (big_sign(term)) {
	    *up = BADARG;
	    return 0;
	} else if (xl*D_EXP > sizeof(UWord)*8) {
	    *up = SYSTEM_LIMIT;
	    return 0;
	}
	while (xl-- > 0) {
	    uval |= ((UWord)(*xr++)) << n;
	    n += D_EXP;
	}
	*up = uval;
	return 1;
    } else {
	*up = BADARG;
	return 0;
    }
#endif
}

int
term_to_Uint64(Eterm term, Uint64 *up)
{
#if SIZEOF_VOID_P == 8
    return term_to_UWord(term,up);
#else
    if (is_small(term)) {
	Sint i = signed_val(term);
	if (i < 0) {
	    *up = BADARG;
	    return 0;
	}
	*up = (Uint64) i;
	return 1;
    } else if (is_big(term)) {
	ErtsDigit* xr = big_v(term);
	dsize_t  xl = big_size(term);
	Uint64 uval = 0;
	int n = 0;

	if (big_sign(term)) {
	    *up = BADARG;
	    return 0;
	} else if (xl*D_EXP > sizeof(Uint64)*8) {
	    *up = SYSTEM_LIMIT;
	    return 0;
	}
	while (xl-- > 0) {
	    uval |= ((Uint64)(*xr++)) << n;
	    n += D_EXP;
	}
	*up = uval;
	return 1;
    } else {
	*up = BADARG;
	return 0;
    }
#endif
}

int
term_to_Uint32(Eterm term, Uint32 *up)
{
#if ERTS_SIZEOF_ETERM == 4
    return term_to_Uint(term,up);
#else
    if (is_small(term)) {
	Sint i = signed_val(term);
	if (i >= 0) {
            *up = (Uint32) i;
            return 1;
        }
    }
    *up = BADARG;
    return 0;
#endif
}


int term_to_Sint(Eterm term, Sint *sp)
{
    if (is_small(term)) {
	*sp = signed_val(term);
	return 1;
    } else if (is_big(term)) {
	ErtsDigit* xr = big_v(term);
	dsize_t xl = big_size(term);
	int sign = big_sign(term);
	Uint uval = 0;
	int n = 0;

	if (xl*D_EXP > sizeof(Uint)*8) {
	    return 0;
	}
	while (xl-- > 0) {
            ASSERT(n < 64);
	    uval |= ((Uint)(*xr++)) << n;
	    n += D_EXP;
	}
	if (sign) {
	    uval = -uval;
	    if ((Sint)uval > 0)
		return 0;
	} else {
	    if ((Sint)uval < 0)
		return 0;
	}
	*sp = uval;
	return 1;
    } else {
	return 0;
    }
}

#if HAVE_INT64
int term_to_Sint64(Eterm term, Sint64 *sp)
{
#if ERTS_SIZEOF_ETERM == 8
    return term_to_Sint(term, sp);
#else
    if (is_small(term)) {
	*sp = signed_val(term);
	return 1;
    } else if (is_big(term)) {
	ErtsDigit* xr = big_v(term);
	dsize_t xl = big_size(term);
	int sign = big_sign(term);
	Uint64 uval = 0;
	int n = 0;

	if (xl*D_EXP > sizeof(Uint64)*8) {
	    return 0;
	}
	while (xl-- > 0) {
	    uval |= ((Uint64)(*xr++)) << n;
	    n += D_EXP;
	}
	if (sign) {
	    uval = -uval;
	    if ((Sint64)uval > 0)
		return 0;
	} else {
	    if ((Sint64)uval < 0)
		return 0;
	}
	*sp = uval;
	return 1;
    } else {
	return 0;
    }
#endif
}
#endif /* HAVE_INT64 */


/*
** Add and subtract
*/
static Eterm B_plus_minus(ErtsDigit *x, dsize_t xl, short xsgn, 
			  ErtsDigit *y, dsize_t yl, short ysgn, Eterm *r)
{
    if (xsgn == ysgn) {
	if (xl > yl)
	    return big_norm(r, I_add(x,xl,y,yl,BIG_V(r)), xsgn);
	else
	    return big_norm(r, I_add(y,yl,x,xl,BIG_V(r)), xsgn);
    }
    else {
	int comp = I_comp(x, xl, y, yl);
	if (comp == 0)
	    return make_small(0);
	else if (comp > 0)
	    return big_norm(r, I_sub(x,xl,y,yl,BIG_V(r)), xsgn);
	else
	    return big_norm(r, I_sub(y,yl,x,xl,BIG_V(r)), ysgn);
    }
}

/*
** Add bignums
*/
Eterm big_plus(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    return B_plus_minus(BIG_V(xp),BIG_SIZE(xp),(short) BIG_SIGN(xp),
			BIG_V(yp),BIG_SIZE(yp),(short) BIG_SIGN(yp), r);
}

/*
** Subtract bignums
*/

Eterm big_minus(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    return B_plus_minus(BIG_V(xp),BIG_SIZE(xp),(short) BIG_SIGN(xp),
			BIG_V(yp),BIG_SIZE(yp),(short) !BIG_SIGN(yp), r);
}

/*
** Multiply smallnums
*/

Eterm small_times(Sint x, Sint y, Eterm *r)
{
    short sign = (x<0) != (y<0);
    ErtsDigit xu = (x > 0) ? x : -x;
    ErtsDigit yu = (y > 0) ? y : -y;
    ErtsDigit d1=0;
    ErtsDigit d0;
    Uint arity;

    DMULc(xu, yu, d1, d0);

    if (!d1 && ((D_EXP < SMALL_BITS) || IS_USMALL(sign, d0))) {
      if (sign)
	return make_small(-((Sint)d0));
      else
	return make_small(d0);
    }

    BIG_DIGIT(r,0) = d0;
    arity = d1 ? 2 : 1;
    if (sign)
      *r = make_neg_bignum_header(arity);
    else
      *r = make_pos_bignum_header(arity);
    if (d1)
      BIG_DIGIT(r,1) = d1;
    return make_big(r);
}

/*
** Multiply bignums
*/

Eterm big_times(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    short sign = BIG_SIGN(xp) != BIG_SIGN(yp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);
    dsize_t rsz;

    if (ysz == 1)
        rsz = D_mul(BIG_V(xp), xsz, BIG_DIGIT(yp, 0), BIG_V(r));
    else if (xsz == 1)
	rsz = D_mul(BIG_V(yp), ysz, BIG_DIGIT(xp, 0), BIG_V(r));
    else if (xsz >= ysz) {
	rsz = I_mul_karatsuba(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
    }
    else {
	rsz = I_mul_karatsuba(BIG_V(yp), ysz, BIG_V(xp), xsz, BIG_V(r));
    }
    return big_norm(r, rsz, sign);
}

/*
** Fused multiplication and addition of bignums
*/

Eterm big_mul_add(Eterm x, Eterm y, Eterm z, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);
    Eterm* zp = big_val(z);

    short sign = BIG_SIGN(xp) != BIG_SIGN(yp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);
    dsize_t rsz;

    if (ysz == 1)
        rsz = D_mul(BIG_V(xp), xsz, BIG_DIGIT(yp, 0), BIG_V(r));
    else if (xsz == 1)
        rsz = D_mul(BIG_V(yp), ysz, BIG_DIGIT(xp, 0), BIG_V(r));
    else if (xsz >= ysz) {
        rsz = I_mul_karatsuba(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
    }
    else {
        rsz = I_mul_karatsuba(BIG_V(yp), ysz, BIG_V(xp), xsz, BIG_V(r));
    }
    return B_plus_minus(BIG_V(r), rsz, sign,
                        BIG_V(zp), BIG_SIZE(zp), (short) BIG_SIGN(zp),
                        r);
}

/*
** Fused div_rem for bignums
*/
int big_div_rem(Eterm lhs, Eterm rhs,
                Eterm *q_hp, Eterm *q,
                Eterm *r_hp, Eterm *r)
{
    Eterm *lhs_val = big_val(lhs);
    Eterm *rhs_val = big_val(rhs);

    int div_sign = BIG_SIGN(lhs_val) != BIG_SIGN(rhs_val);
    int rem_sign = BIG_SIGN(lhs_val);

    dsize_t lhs_size = BIG_SIZE(lhs_val);
    dsize_t rhs_size = BIG_SIZE(rhs_val);

    dsize_t quotient_size, remainder_size;
    Eterm quotient, remainder;

    if (rhs_size == 1) {
        quotient_size = D_div(BIG_V(lhs_val), lhs_size, BIG_DIGIT(rhs_val, 0),
                              BIG_V(q_hp), BIG_V(r_hp));
        remainder_size = 1;
    } else {
        quotient_size = I_div_dispatch(BIG_V(lhs_val), lhs_size,
                                       BIG_V(rhs_val), rhs_size,
                                       BIG_V(q_hp), BIG_V(r_hp),
                                       &remainder_size);
    }

    quotient = big_norm(q_hp, quotient_size, div_sign);
    if (quotient == NIL) {
        return 0;
    }

    remainder = big_norm(r_hp, remainder_size, rem_sign);
    if (remainder == NIL) {
        return 0;
    }

    *q = quotient;
    *r = remainder;

    return 1;
}

/*
** Divide bignums
*/
Eterm big_div(Eterm x, Eterm y, Eterm *q)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    short sign = BIG_SIGN(xp) != BIG_SIGN(yp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);
    dsize_t qsz;

    if (ysz == 1) {
	ErtsDigit rem;
	qsz = D_div(BIG_V(xp), xsz, BIG_DIGIT(yp,0), BIG_V(q), &rem);
    }
    else {
	Eterm* remp;
	dsize_t rem_sz;

	qsz = xsz - ysz + 1;
	remp = q + BIG_NEED_SIZE(qsz);
        qsz = I_div_dispatch(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(q), BIG_V(remp),
                             &rem_sz);
    }
    return big_norm(q, qsz, sign);
}

/*
** Remainder
*/
Eterm big_rem(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);
    short sign = BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (ysz == 1) {
	ErtsDigit rem;
	rem = D_rem(BIG_V(xp), xsz, BIG_DIGIT(yp,0));
	if (IS_USMALL(sign, rem)) {
	    if (sign)
		return make_small(-(Sint)rem);
	    else
		return make_small(rem);
	}
	else {
	    if (sign)
		*r = make_neg_bignum_header(1);
	    else
		*r = make_pos_bignum_header(1);
	    BIG_DIGIT(r, 0) = rem;
	    return make_big(r);
	}
    }
    else {
	dsize_t rsz = I_rem(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
	return big_norm(r, rsz, sign);
    }
}

Eterm big_band(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    short xsgn = BIG_SIGN(xp);
    short ysgn = BIG_SIGN(yp);
    short sign = xsgn && ysgn;
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (xsz >= ysz)
	return big_norm(r,I_band(BIG_V(xp),xsz,xsgn,
				 BIG_V(yp),ysz,ysgn,
				 BIG_V(r)),sign);
    else
	return big_norm(r,I_band(BIG_V(yp),ysz,ysgn,
				 BIG_V(xp),xsz,xsgn,
				 BIG_V(r)),sign);
}


Eterm big_bor(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);
    short xsgn = BIG_SIGN(xp);
    short ysgn = BIG_SIGN(yp);
    short sign = (xsgn || ysgn);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (xsz >= ysz)
	return big_norm(r,I_bor(BIG_V(xp),xsz,xsgn,
				BIG_V(yp),ysz,ysgn,
				BIG_V(r)),sign);
    else
	return big_norm(r,I_bor(BIG_V(yp),ysz,ysgn,
				BIG_V(xp),xsz,xsgn,
				BIG_V(r)),sign);
}


Eterm big_bxor(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);
    short xsgn = BIG_SIGN(xp);
    short ysgn = BIG_SIGN(yp);
    short sign = (xsgn != ysgn);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (xsz >= ysz)
	return big_norm(r,I_bxor(BIG_V(xp),xsz,xsgn,
				 BIG_V(yp),ysz,ysgn,
				 BIG_V(r)),sign);
    else
	return big_norm(r,I_bxor(BIG_V(yp),ysz,ysgn,
				 BIG_V(xp),xsz,xsgn,
				 BIG_V(r)),sign);
}

Eterm big_bnot(Eterm x,  Eterm *r)
{
    Eterm* xp = big_val(x);
    short sign = !BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);

    return big_norm(r, I_bnot(BIG_V(xp), xsz, sign, BIG_V(r)), sign);
}

Eterm big_lshift(Eterm x, Sint y, Eterm *r)
{
    Eterm* xp = big_val(x);
    short sign = BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);

    return big_norm(r, I_lshift(BIG_V(xp), xsz, y, sign, BIG_V(r)), sign);
}


/* add unsigned small int y to x */

Eterm big_plus_small(Eterm x, Uint y, Eterm *r)
{
    Eterm* xp = big_val(x);

    if (BIG_SIGN(xp))
	return big_norm(r, D_sub(BIG_V(xp),BIG_SIZE(xp), (ErtsDigit) y, 
				 BIG_V(r)), (short) BIG_SIGN(xp));
    else
	return big_norm(r, D_add(BIG_V(xp),BIG_SIZE(xp), (ErtsDigit) y, 
				 BIG_V(r)), (short) BIG_SIGN(xp));
}

/*
** Expects the big to fit.
*/
Uint32 big_to_uint32(Eterm b)
{
    Uint u;
    if (!term_to_Uint(b, &u)) {
	ASSERT(0);
        return 0;
    }
    return u;
}

/*
 * Check if a fixnum or bignum equals 2^32.
 */
int term_equals_2pow32(Eterm x)
{
    if (sizeof(Uint) > 4) {
	Uint u;
	if (!term_to_Uint(x, &u))
	    return 0;
	return (u & 0xFFFFFFFF) == 0 && ((u >> 16) >> 16) == 1;
    } else {
	Eterm *bp;
	if (!is_big(x))
	    return 0;
	bp = big_val(x);
#if D_EXP == 16   /* 16 bit platform not really supported!!! */
	return (BIG_SIZE(bp) == 3) && !BIG_DIGIT(bp,0) && !BIG_DIGIT(bp,1) && 
	    BIG_DIGIT(bp,2) == 1;
#elif D_EXP == 32
	return (BIG_SIZE(bp) == 2) && !BIG_DIGIT(bp,0) &&
	    BIG_DIGIT(bp,1) == 1;
#elif D_EXP == 64
	return (BIG_SIZE(bp) == 1) && 
	    ((BIG_DIGIT(bp,0) & 0xffffffff) == 0) &&
	    ((BIG_DIGIT(bp,0) >> 32) == 1);
#endif
	return 0;
    }
}

static ERTS_INLINE int c2int_is_valid_char(byte ch, int base) {
    if (base <= 10)
        return (ch >= '0' && ch < ('0' + base));
    else
        return (ch >= '0' && ch <= '9')
            || (ch >= 'A' && ch < ('A' + base - 10))
            || (ch >= 'a' && ch < ('a' + base - 10));
}

static ERTS_INLINE int c2int_is_invalid_char(byte ch, int base) {
    return !c2int_is_valid_char(ch, base);
}

static ERTS_INLINE byte c2int_digit_from_base(byte ch) {
    return ch <= '9' ? ch - '0'
            : (10 + (ch <= 'Z' ? ch - 'A' : ch - 'a'));
}

/*
 * How many bits are needed to store 1 digit of given base in binary
 * Wo.Alpha formula: Table [log2[n], {n,2,36}]
 */
static const double lg2_lookup[36-1] = {
    1.0, 1.58496, 2.0, 2.32193, 2.58496, 2.80735, 3.0, 3.16993, 3.32193,
    3.45943, 3.58496, 3.70044, 3.80735, 3.90689, 4.0, 4.08746, 4.16993, 4.24793,
    4.32193, 4.39232, 4.45943, 4.52356, 4.58496, 4.64386, 4.70044, 4.75489,
    4.80735, 4.85798, 4.90689, 4.9542, 5.0, 5.04439, 5.08746, 5.12928, 5.16993
};

/*
 * How many digits can fit into a signed int (Sint) for given base, we take
 * one digit away just to be on the safer side (some corner cases).
 */
static const byte digits_per_sint_lookup[36-1] = {
#if (SIZEOF_VOID_P == 4)
    /* Wo.Alpha formula: Table [Trunc[31 / log[2,n]]-1, {n, 2, 36}] */
    30, 18, 14, 12, 10, 10, 9, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4
#elif (SIZEOF_VOID_P == 8)
    /* Wo.Alpha formula: Table [Trunc[63 / log[2,n]]-1, {n, 2, 36}] */
    62, 38, 30, 26, 23, 21, 20, 18, 17, 17, 16, 16, 15, 15, 14, 14, 14, 13, 13,
    13, 13, 12, 12, 12, 12, 12, 12, 11, 11, 11, 11, 11, 11, 11, 11
#else
    #error "Please produce a lookup table for the new architecture"
#endif
};

/*
 * How many digits can fit into Erlang Small (SMALL_BITS-1) counting sign bit
 */
static const byte digits_per_small_lookup[36-1] = {
#if (SIZEOF_VOID_P == 4)
    /* Wo.Alpha formula: Table [Trunc[27 / log[2,n]]-1, {n, 2, 36}] */
    27, 17, 13, 11, 10, 9, 9, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
#elif (SIZEOF_VOID_P == 8)
    /* Wo.Alpha formula: Table [Trunc[59 / log[2,n]]-1, {n, 2, 36}] */
    59, 37, 29, 25, 22, 21, 19, 18, 17, 17, 16, 15, 15, 15, 14, 14, 14, 13, 13,
    13, 13, 13, 12, 12, 12, 12, 12, 12, 12, 11, 11, 11, 11, 11, 11
#else
    #error "Please produce a lookup table for the new architecture"
#endif
};

/*
 * Largest power of base which can be represented in a signed int (Sint).
 * Calculated by base 2..36 to the power of corresponding element from
 * digits_per_sint_lookup.
 */
static const Sint largest_power_of_base_lookup[36-1] = {
#if (SIZEOF_VOID_P == 4)
    /* Wo.Alpha formula: Table [Pow[n, Trunc[31 / log[2,n]]-1], {n, 2, 36}] */
    1073741824, 387420489, 268435456, 244140625, 60466176, 282475249, 134217728,
    43046721, 100000000, 19487171, 35831808, 62748517, 105413504, 11390625,
    16777216, 24137569, 34012224, 47045881, 64000000, 85766121, 5153632,
    6436343,7962624, 9765625, 11881376, 14348907, 17210368, 20511149, 24300000,
    28629151, 33554432, 39135393, 45435424, 52521875, 1679616
#elif (SIZEOF_VOID_P == 8)
    /* Wo.Alpha formula: Table [Pow[n, Trunc[63 / log[2,n]]-1], {n, 2, 36}]
     * with LL added after each element manually */
    4611686018427387904LL, 1350851717672992089LL, 1152921504606846976LL,
    1490116119384765625LL, 789730223053602816LL, 558545864083284007LL,
    1152921504606846976LL, 150094635296999121LL, 100000000000000000LL,
    505447028499293771LL, 184884258895036416LL, 665416609183179841LL,
    155568095557812224LL, 437893890380859375LL, 72057594037927936LL,
    168377826559400929LL, 374813367582081024LL, 42052983462257059LL,
    81920000000000000LL, 154472377739119461LL, 282810057883082752LL,
    21914624432020321LL, 36520347436056576LL, 59604644775390625LL,
    95428956661682176LL, 150094635296999121LL, 232218265089212416LL,
    12200509765705829LL, 17714700000000000LL, 25408476896404831LL,
    36028797018963968LL, 50542106513726817LL, 70188843638032384LL,
    96549157373046875LL, 131621703842267136LL
#else
    #error "Please produce a lookup table for the new architecture"
#endif
};

static Eterm chars_to_integer(const byte *bytes, Uint size, const Uint base)
{
    Sint i = 0;
    int neg = 0;

    if (size == 0) {
	return am_badarg;
    }

    if (bytes[0] == '-') {
	neg = 1;
	bytes++;
	size--;
    } else if (bytes[0] == '+') {
	bytes++;
	size--;
    }

    if (size == 0) {
	return am_badarg;
    }

    /* Trim leading zeroes */
    while (*bytes == '0') {
        bytes++;
        size--;
        if (size == 0) {
            /* All zero! */
            return make_small(0);
        }
    }

    if (size > get_digits_per_small(base)) {
	return am_big;
    }

    if (base <= 10) {
        /*
         * Take shortcut if we know that all chars are '0' < b < '9'.
         * This improves speed by about 10% over the generic small
         * case.
         */
        while (size--) {
            Uint digit = *bytes++ - '0';
            if (digit >= base) {
                return am_badarg;
            }
            i = i * base + digit;
        }
    } else {
        while (size) {
            byte b = *bytes++;
            size--;

            if (c2int_is_invalid_char(b, base)) {
                return am_badarg;
            }

            i = i * base + c2int_digit_from_base(b);
        }
    }

    if (neg) {
        i = -i;
    }
    ASSERT(IS_SSMALL(i));
    return make_small(i);
}

BIF_RETTYPE erts_internal_binary_to_integer_2(BIF_ALIST_2)
{
    const byte *temp_alloc = NULL, *bytes;
    Uint size;
    Uint base;
    Eterm res;

    if (!is_small(BIF_ARG_2)) {
        BIF_RET(am_badarg);
    }

    base = (Uint)signed_val(BIF_ARG_2);

    if (base < 2 || base > 36) {
        BIF_RET(am_badarg);
    }

    bytes = erts_get_aligned_binary_bytes(BIF_ARG_1, &size, &temp_alloc);
    if (bytes == NULL) {
        BIF_RET(am_badarg);
    }

    res = chars_to_integer(bytes, size, base);
    erts_free_aligned_binary_bytes(temp_alloc);
    BIF_RET(res);
}

BIF_RETTYPE erts_internal_list_to_integer_2(BIF_ALIST_2)
{
    Eterm res;
    Sint i = 0;
    Uint ui = 0;
    int neg = 0;
    Sint n = 0;
    byte c;
    Eterm list = BIF_ARG_1;
    Uint base;
    Uint digits_per_small;
    Eterm *hp;

    if (is_nil(list) ) {
        BIF_RET(am_no_integer);
    } else if (is_not_list(list)) {
        BIF_RET(am_not_a_list);
    }

    if (is_not_small(BIF_ARG_2)) {
        BIF_RET(am_badarg);
    }
    base = unsigned_val(BIF_ARG_2);
    if (base < 2 || base > 36) {
        BIF_RET(am_badarg);
    }

    if (CAR(list_val(list)) == make_small('-')) {
        neg = 1;
        list = CDR(list_val(list));
    } else if (CAR(list_val(list)) == make_small('+')) {
        list = CDR(list_val(list));
    }

    while (is_list(list)) {     /* Skip zero digits */
        Eterm *list_ptr = list_val(list);

        if (is_not_small(CAR(list_ptr))) {
            break;
        }
        c = unsigned_val(CAR(list_ptr));
        if (c != '0') {
            if (c2int_is_invalid_char(c, base)) {
                if (n == 0) {
                    BIF_RET(am_no_integer);
                } else {
                    res = make_small(0);
                    hp = HAlloc(BIF_P, 3);
                    BIF_RET(TUPLE2(hp, res, list));
                }
            }
            break;
        }
        n++;
        list = CDR(list_ptr);
    }

    if (is_not_list(list)) {
        if (n == 0) {
            BIF_RET(am_no_integer);
        } else {
            res = make_small(0);
            hp = HAlloc(BIF_P, 3);
            BIF_RET(TUPLE2(hp, res, list));
        }
    }

    n = 0;
    digits_per_small = get_digits_per_small(base);
    while (n <= digits_per_small) {
        if (is_not_small(CAR(list_val(list)))) {
            break;
        }
        c = unsigned_val(CAR(list_val(list)));
        if (c2int_is_invalid_char(c, base)) {
            break;
        }
        ui = ui * base + c2int_digit_from_base(c);
        n++;
        list = CDR(list_val(list));
        if (is_not_list(list)) {
            break;
        }
    }

    if (n == 0) {
        BIF_RET(am_no_integer);
    }

    if (n > digits_per_small) {
        BIF_RET(am_big);
    } else {
        i = neg ? -(Sint)ui : (Sint)ui;
        res = make_small(i);
        hp = HAlloc(BIF_P, 3);
        BIF_RET(TUPLE2(hp, res, list));
    }
}
