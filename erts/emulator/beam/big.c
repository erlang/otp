/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

/* add a and b with carry in + out */
#define DSUMc(a,b,c,s) do {						\
	ErtsDigit ___cr = (c);						\
	ErtsDigit ___xr = (a)+(___cr);					\
	ErtsDigit ___yr = (b);						\
	___cr = (___xr < ___cr);					\
	___xr = ___yr + ___xr;						\
	___cr += (___xr < ___yr);					\
	s = ___xr;							\
	c = ___cr;							\
    }  while(0)

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

#endif

/* Forward declaration of lookup tables (See below in this file) used in list to
 * integer conversions for different bases. Also used in bignum printing.
 */
static const byte digits_per_sint_lookup[36-1];
static const byte digits_per_small_lookup[36-1];
static const Sint largest_power_of_base_lookup[36-1];

static ERTS_INLINE byte get_digits_per_signed_int(Uint base) {
    return digits_per_sint_lookup[base-2];
}

static ERTS_INLINE byte get_digits_per_small(Uint base) {
    return digits_per_small_lookup[base-2];
}

static ERTS_INLINE Sint get_largest_power_of_base(Uint base) {
    return largest_power_of_base_lookup[base-2];
}

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
	xr = *x++ + c;
	yr = *y++;
	c = (xr < c);
	xr = yr + xr;
	c += (xr < yr);
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
** Assumption: digits in r must be 0 (upto the size of x)
*/
static dsize_t I_mul(ErtsDigit* x, dsize_t xl, ErtsDigit* y, dsize_t yl, ErtsDigit* r)
{
    ErtsDigit* r0 = r;
    ErtsDigit* rt = r;

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
    ErtsDigit d_next = *x;
    ErtsDigit d;
    ErtsDigit* r0 = r;
    ErtsDigit* s = r;

    if ((r + xl) == x)	/* "Inline" operation */
	*x = 0;
    x++;
	
    while(xl--) {
	ErtsDigit* y = x;
	ErtsDigit y_0 = 0, y_1 = 0, y_2 = 0, y_3 = 0;
	ErtsDigit b0, b1;
	ErtsDigit z0, z1, z2;
	ErtsDigit t;
	dsize_t y_l = xl;
		
	s = r;
	d = d_next;
	d_next = *x; 
	x++;

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
    if (*s == 0)
	return (s - r0);
    else
	return (s - r0) + 1;
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
** Assumtions: xl >= yl, yl > 1
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
	    while(xl--)
		*r++ = ~*x++;
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
	    while(xl--)
		*r++ = ~*x++;
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
		*r++ = *x++;
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
/*
 * As erts_make_integer, but from a whole UWord.
 */
Eterm
erts_make_integer_from_uword(UWord x, Process *p)
{
    Eterm* hp;
    if (IS_USMALL(0,x))
	return make_small(x);
    else {
	hp = HAlloc(p, BIG_UWORD_HEAP_SIZE(x));
	return uword_to_big(x,hp);
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
big_to_double(Wterm x, double* resp)
{
    double d = 0.0;
    Eterm* xp = big_val(x);
    dsize_t xl = BIG_SIZE(xp);
    ErtsDigit* s = BIG_V(xp) + xl;
    short xsgn = BIG_SIGN(xp);
    double dbase = ((double)(D_MASK)+1);
#ifndef NO_FPE_SIGNALS 
    volatile unsigned long *fpexnp = erts_get_current_fp_exception();
#endif
    __ERTS_SAVE_FP_EXCEPTION(fpexnp);

    __ERTS_FP_CHECK_INIT(fpexnp);
    while (xl--) {
	d = d * dbase + *--s;

	__ERTS_FP_ERROR(fpexnp, d, __ERTS_RESTORE_FP_EXCEPTION(fpexnp); return -1);
    }

    *resp = xsgn ? -d : d;
    __ERTS_FP_ERROR(fpexnp,*resp,;);
    __ERTS_RESTORE_FP_EXCEPTION(fpexnp);
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
    while ((ds & (BIG_DIGITS_PER_WORD - 1)) != 0) {
	xp[ds++] = 0;
    }

    if (is_negative) {
	*hp = make_neg_bignum_header(sz-1);
    } else {
	*hp = make_pos_bignum_header(sz-1);
    }
    return res;
}


/*
 ** Estimate the number of decimal digits (include sign)
 */
int big_decimal_estimate(Wterm x)
{
    Eterm* xp = big_val(x);
    int lg = I_lg(BIG_V(xp), BIG_SIZE(xp));
    int lg10 = ((lg+1)*28/93)+1;

    if (BIG_SIGN(xp)) lg10++;	/* add sign */
    return lg10+1;		/* add null */
}

/*
** Convert a bignum into a string of decimal numbers
*/

static Uint write_big(Wterm x, void (*write_func)(void *, char), void *arg)
{
    Eterm* xp = big_val(x);
    ErtsDigit* dx = BIG_V(xp);
    dsize_t xl = BIG_SIZE(xp);
    short sign = BIG_SIGN(xp);
    ErtsDigit rem;
    Uint n = 0;
    const Uint digits_per_Sint = get_digits_per_signed_int(10);
    const Sint largest_pow_of_base = get_largest_power_of_base(10);

    if (xl == 1 && *dx < largest_pow_of_base) {
	rem = *dx;
	if (rem == 0) {
	    (*write_func)(arg, '0'); n++;
	} else {
	    while(rem) {
		(*write_func)(arg, (rem % 10) + '0'); n++;
		rem /= 10;
	    }
	}
    } else {
	ErtsDigit* tmp  = (ErtsDigit*) erts_alloc(ERTS_ALC_T_TMP,
					      sizeof(ErtsDigit)*xl);
	dsize_t tmpl = xl;

	MOVE_DIGITS(tmp, dx, xl);

	while(1) {
            tmpl = D_div(tmp, tmpl, largest_pow_of_base, tmp, &rem);
	    if (tmpl == 1 && *tmp == 0) {
		while(rem) {
		    (*write_func)(arg, (rem % 10)+'0'); n++;
		    rem /= 10;
		}
		break;
	    } else {
                Uint i = digits_per_Sint;
		while(i--) {
		    (*write_func)(arg, (rem % 10)+'0'); n++;
		    rem /= 10;
		}
	    }
	}
	erts_free(ERTS_ALC_T_TMP, (void *) tmp);
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

Eterm erts_big_to_list(Eterm x, Eterm **hpp)
{
    struct big_list__ bl;
    bl.hp = *hpp;
    bl.res = NIL;
    write_big(x, write_list, (void *) &bl);
    *hpp = bl.hp;
    return bl.res;
}

static void
write_string(void *arg, char c)
{
    *(--(*((char **) arg))) = c;
}

char *erts_big_to_string(Wterm x, char *buf, Uint buf_sz)
{
    char *big_str = buf + buf_sz - 1;
    *big_str = '\0';
    write_big(x, write_string, (void *) &big_str);
    ASSERT(buf <= big_str && big_str <= buf + buf_sz - 1);
    return big_str;
}

/* Bignum to binary bytes
 * e.g. 1 bsl 64 -> "18446744073709551616"
 */

Uint erts_big_to_binary_bytes(Eterm x, char *buf, Uint buf_sz)
{
    char *big_str = buf + buf_sz;
    Uint n;
    n = write_big(x, write_string, (void *) &big_str);
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
int big_comp(Wterm x, Wterm y)
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
Eterm bytes_to_big(byte *xp, dsize_t xsz, int xsgn, Eterm *r)
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
Eterm big_plus(Wterm x, Wterm y, Eterm *r)
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
** Subtract a digit from big number
*/
Eterm big_minus_small(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);

    if (BIG_SIGN(xp))
	return big_norm(r, D_add(BIG_V(xp),BIG_SIZE(xp), (ErtsDigit) y, BIG_V(r)), 
			(short) BIG_SIGN(xp));
    else
	return big_norm(r, D_sub(BIG_V(xp),BIG_SIZE(xp), (ErtsDigit) y, BIG_V(r)), 
			(short) BIG_SIGN(xp));
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
    else if (xp == yp) {
	ZERO_DIGITS(BIG_V(r), xsz+1);
	rsz = I_sqr(BIG_V(xp), xsz, BIG_V(r));
    }
    else if (xsz >= ysz) {
	ZERO_DIGITS(BIG_V(r), xsz);
	rsz = I_mul(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
    }
    else {
	ZERO_DIGITS(BIG_V(r), ysz);
	rsz = I_mul(BIG_V(yp), ysz, BIG_V(xp), xsz, BIG_V(r));
    }
    return big_norm(r, rsz, sign);
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
	qsz = I_div(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(q), BIG_V(remp),
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

Eterm big_neg(Eterm x, Eterm *r)
{
    Eterm* xp = big_val(x);
    dsize_t xsz = BIG_SIZE(xp);
    short xsgn = BIG_SIGN(xp);
    
    MOVE_DIGITS(BIG_V(r), BIG_V(xp), xsz);
    return big_norm(r, xsz, (short) !xsgn);
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

Eterm big_times_small(Eterm x, Uint y, Eterm *r)
{
    Eterm* xp = big_val(x);

    return big_norm(r, D_mul(BIG_V(xp),BIG_SIZE(xp), (ErtsDigit) y, 
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
#if D_EXP == 16   /* 16 bit platfrom not really supported!!! */
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

static ERTS_INLINE int c2int_is_invalid_char(byte ch, int base) {
    return (ch < '0'
            || (ch > ('0' + base - 1)
                && !(base > 10
                     && ((ch >= 'a' && ch < ('a' + base - 10))
                         || (ch >= 'A' && ch < ('A' + base - 10))))));
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
static ERTS_INLINE double lookup_log2(Uint base) {
    return lg2_lookup[base - 2];
}

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

Eterm erts_chars_to_integer(Process *BIF_P, char *bytes, 
			   Uint size, const int base) {
    Eterm res;
    Sint i = 0;
    int n = 0;
    int neg = 0;
    byte b;
    Eterm *hp, *hp_end;
    Sint m;
    int lg2;
    const Uint digits_per_small = get_digits_per_small(base);
    const Uint digits_per_Sint = get_digits_per_signed_int(base);
    const Sint largest_pow_of_base = get_largest_power_of_base(base);

    if (size == 0)
	goto bytebuf_to_integer_1_error;

    if (bytes[0] == '-') {
	neg = 1;
	bytes++;
	size--;

    } else if (bytes[0] == '+') {
	bytes++;
	size--;
    }

    /* Trim leading zeroes */
    if (size) {
        while (*bytes == '0') {
            bytes++;
            size--;
            if (!size) {
                /* All zero! */
                res = make_small(0);
                goto bytebuf_to_integer_1_done;
            }
        }
    }

    if (size == 0)
	goto bytebuf_to_integer_1_error;

    if (size < digits_per_small) {
        if (base <= 10) {
            /* *
         * Take shortcut if we know that all chars are '0' < b < '9' and
         * fit in a small. This improves speed by about 10% over the generic
         * small case.
         * */
            while (size--) {
                b = *bytes++;

                if (b < '0' || b > ('0'+base-1))
                    goto bytebuf_to_integer_1_error;

                i = i * base + b - '0';
            }

            if (neg)
                i = -i;
            res = make_small(i);
            goto bytebuf_to_integer_1_done;
        }

        /* Take shortcut if we know it will fit in a small.
         * This improves speed by about 30%.
         */
        while (size) {
            b = *bytes++;
            size--;

            if (c2int_is_invalid_char(b, base))
                goto bytebuf_to_integer_1_error;

            i = i * base + c2int_digit_from_base(b);
        }

        if (neg)
            i = -i;
        res = make_small(i);
        goto bytebuf_to_integer_1_done;
    }

    /*
     * Calculate the maximum number of bits which will
     * be needed to represent the binary
     */
    lg2 = ((size+2)*lookup_log2(base)+1);

    /* Start calculating bignum */
    m = (lg2 + D_EXP-1)/D_EXP;
    m = BIG_NEED_SIZE(m);

    hp = HAlloc(BIF_P, m);
    hp_end = hp + m;

    if ((i = (size % digits_per_Sint)) == 0)
        i = digits_per_Sint;

    n = size - i;
    m = 0;

    while (i--) {
	b = *bytes++;

        if (c2int_is_invalid_char(b,base)) {
	    HRelease(BIF_P, hp_end, hp);
	    goto bytebuf_to_integer_1_error;
	}

        m = base * m + c2int_digit_from_base(b);
    }

    res = small_to_big(m, hp);

    while (n) {
        i = digits_per_Sint;
        n -= digits_per_Sint;
	m = 0;
	while (i--) {
	    b = *bytes++;

            if (c2int_is_invalid_char(b,base)) {
	      HRelease(BIF_P, hp_end, hp);
	      goto bytebuf_to_integer_1_error;
	    }

            m = base * m + c2int_digit_from_base(b);
	}
	if (is_small(res)) {
	    res = small_to_big(signed_val(res), hp);
	}
        res = big_times_small(res, largest_pow_of_base, hp);
	if (is_small(res)) {
	    res = small_to_big(signed_val(res), hp);
	}
	res = big_plus_small(res, m, hp);
    }

    if (neg) {
	if (is_small(res))
	    res = make_small(-signed_val(res));
	else {
	    Uint *big = big_val(res); /* point to thing */
	    *big = bignum_header_neg(*big);
	}
    }

    if (is_not_small(res)) {
	res = big_plus_small(res, 0, hp); /* includes conversion to small */

	if (is_not_small(res)) {
	    hp += (big_arity(res) + 1);
	}
    }
    HRelease(BIF_P, hp_end, hp);
    goto bytebuf_to_integer_1_done;

bytebuf_to_integer_1_error:
    return THE_NON_VALUE;

bytebuf_to_integer_1_done:
    return res;
}

/* Converts list of digits with given 'base' to integer sequentially. Returns
 * result in 'integer_out', remaining tail goes to 'tail_out' and returns result
 * code if the list was consumed fully or partially or there was an error
 */
LTI_result_t erts_list_to_integer(Process *BIF_P, Eterm orig_list,
                                  const Uint base,
                                  Eterm *integer_out, Eterm *tail_out)
{
     Sint i = 0;
     Uint ui = 0;
     int skip = 0;
     int neg = 0;
     Sint n = 0;
     Sint m;
     int lg2;
     Eterm res;
     Eterm lst = orig_list;
     Eterm tail = lst;
     int error_res = LTI_BAD_STRUCTURE;
     const Uint digits_per_small = get_digits_per_small(base);
     const Uint digits_per_Sint = get_digits_per_signed_int(base);

     if (is_nil(lst)) {
       error_res = LTI_NO_INTEGER;
     error:
         *tail_out = tail;
         *integer_out = make_small(0);
         return error_res;
     }
     if (is_not_list(lst))
       goto error;

     /* if first char is a '-' then it is a negative integer */
     if (CAR(list_val(lst)) == make_small('-')) {
          neg = 1;
          skip = 1;
          lst = CDR(list_val(lst));
          if (is_not_list(lst)) {
              tail = lst;
              error_res = LTI_NO_INTEGER;
              goto error;
          }
     } else if (CAR(list_val(lst)) == make_small('+')) {
         /* ignore plus */
         skip = 1;
         lst = CDR(list_val(lst));
         if (is_not_list(lst)) {
             tail = lst;
             error_res = LTI_NO_INTEGER;
             goto error;
         }
     }

     /* Calculate size and do type check */

     while(1) {
         byte ch;
         if (is_not_small(CAR(list_val(lst)))) {
             break;
         }
         ch = unsigned_val(CAR(list_val(lst)));
         if (c2int_is_invalid_char(ch, base)) {
             break;
         }
         ui = ui * base;
         ui = ui + c2int_digit_from_base(ch);
         n++;
         lst = CDR(list_val(lst));
         if (is_nil(lst)) {
             break;
         }
         if (is_not_list(lst)) {
             break;
         }
     }

     tail = lst;
     if (!n) {
         error_res = LTI_NO_INTEGER;
         goto error;
     }


      /* If length fits inside Sint then we know it's a small int. Else we
       * must construct a bignum and let that routine do the checking
       */

     if (n <= digits_per_small) {  /* It must be small */
         i = neg ? -(Sint)ui : (Sint)ui;
         res = make_small(i);
     } else {
         const Sint largest_pow_of_base = get_largest_power_of_base(base);
         Eterm *hp;
         Eterm *hp_end;

         /* Convert from log_base to log2 using lookup table */
         lg2 = ((n+2)*lookup_log2(base)+1);
         m  = (lg2+D_EXP-1)/D_EXP; /* number of digits */
         m  = BIG_NEED_SIZE(m);    /* number of words + thing */

         hp = HAlloc(BIF_P, m);
         hp_end = hp + m;

         lst = orig_list;
         if (skip)
             lst = CDR(list_val(lst));

         /* load first digits (at least one digit) */
         if ((i = (n % digits_per_Sint)) == 0)
             i = digits_per_Sint;
         n -= i;
         m = 0;
         while(i--) {
             m *= base;
             m += c2int_digit_from_base(unsigned_val(CAR(list_val(lst))));
             lst = CDR(list_val(lst));
         }
         res = small_to_big(m, hp);  /* load first digits */

         while(n) {
             i = digits_per_Sint;
             n -= digits_per_Sint;
             m = 0;
             while(i--) {
                 m *= base;
                 m += c2int_digit_from_base(unsigned_val(CAR(list_val(lst))));
                 lst = CDR(list_val(lst));
             }
             if (is_small(res))
                 res = small_to_big(signed_val(res), hp);
             res = big_times_small(res, largest_pow_of_base, hp);
             if (is_small(res))
                 res = small_to_big(signed_val(res), hp);
             res = big_plus_small(res, m, hp);
         }

         if (neg) {
             if (is_small(res))
                 res = make_small(-signed_val(res));
             else {
                 Uint *big = big_val(res); /* point to thing */
                 *big = bignum_header_neg(*big);
             }
         }

         if (is_not_small(res)) {
             res = big_plus_small(res, 0, hp); /* includes conversion to small */

             if (is_not_small(res)) {
                 hp += (big_arity(res)+1);
             }
         }
         HRelease(BIF_P, hp_end, hp);
     }
     *integer_out = res;
     *tail_out = tail;
     if (tail != NIL) {
         return LTI_SOME_INTEGER;
     }
     return LTI_ALL_INTEGER;
}
