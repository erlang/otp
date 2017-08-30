/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
#include <string.h>
#include <stdlib.h>

#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_decode_big(const char *buf, int *index, erlang_big *b) {
    unsigned int digit_bytes;
    const unsigned char *s = (unsigned char*) buf + *index;
    const unsigned char *s0 = s;

    switch ( get8(s) ) {
	case ERL_SMALL_BIG_EXT:
	digit_bytes = get8(s);
	break;
    case ERL_LARGE_BIG_EXT:
	digit_bytes = get32be(s);
	break;
    default:
	return -1;
    }
    if ( b ) {
	unsigned short *dt = b->digits;
	unsigned int n = (digit_bytes+1)/2;
	int i;

	if ( digit_bytes != b->arity ) {
	    return -1;
	}

	b->is_neg = get8(s);
	  
	for (i = 0; i < n; ++i) {
	    dt[i] = s[i*2];
	    if ((i*2 + 1) < digit_bytes) {
		dt[i] |= ((unsigned short) s[(i*2)+1]) << 8;
	    }
	}
    } else {
	s++; /* skip sign byte */
    }

    s += digit_bytes;

    *index += s-s0; 
  
    return 0; 
}

erlang_big *ei_alloc_big(unsigned int digit_bytes) {
    erlang_big *b;
    unsigned int n = (digit_bytes+1)/2;

    if ( (b = malloc(sizeof(erlang_big))) == NULL) return NULL;
    memset(b,(char)0,sizeof(erlang_big));
    if ( (b->digits = malloc(2*n)) == NULL) {
        free(b);
        return NULL;
    }
   
    b->arity = digit_bytes;
    memset(b->digits,(char)0, 2*n);
    return b;
}

void ei_free_big(erlang_big *b)
{
    if (!b) return;
    if (b->digits) free(b->digits);
    free(b);
}

/* big compare functions */

typedef unsigned short Uint16;
typedef unsigned int Uint;

typedef Uint16 digit_t;
typedef Uint dsize_t;

static int I_comp(digit_t *x, dsize_t xl, digit_t *y, dsize_t yl)
{
    if (xl<yl) {
        return -1;
    } else if (xl>yl) {
        return 1;
    } else {
        if ( x == y ) return 0;
        x += (xl-1);
        y += (yl-1);
        while( (xl>0) && (*x==*y) ) {
            x--;
            y--;
            xl--;
        }
        if ( xl == 0 ) return 0;
        return ( *x < *y ) ? -1 : 1;
    }
}

int ei_big_comp(erlang_big *x, erlang_big *y)
{
    if ( x->is_neg == y->is_neg ) {
        int c = I_comp(x->digits,(x->arity+1)/2,y->digits,(y->arity+1)/2);
        if ( x->is_neg ) 
            return -c;
        else
            return c;
    } else {
        return x->is_neg ? -1 : 1;
    }
}

#define D_EXP      16
#define D_BASE     (1<<D_EXP)

#define D_DECIMAL_EXP   4           /* 10^4 == 10000 */
#define D_DECIMAL_BASE  10000       /* Max decimal exponent in a digit */

#define DLOW(x)        ((digit_t)((x) & (D_BASE-1)))
#define DHIGH(x)       ((digit_t)((x) >> D_EXP))

/*
 * Handling of floating point exceptions.
 */

#if defined(VXWORKS) && CPU == PPC860
#undef NO_FPE_SIGNALS
#define NO_FPE_SIGNALS 1
#undef INLINED_FP_CONVERSION
#define INLINED_FP_CONVERSION 1
#endif

#ifdef NO_FPE_SIGNALS
#  define ERTS_FP_CHECK_INIT() do {} while (0)
#  define ERTS_FP_ERROR(f, Action) if (!isfinite(f)) { Action; } else {}
#  define ERTS_SAVE_FP_EXCEPTION()
#  define ERTS_RESTORE_FP_EXCEPTION()
#else
/* extern volatile int erl_fp_exception; */
static volatile int erl_fp_exception;
#  define ERTS_FP_CHECK_INIT() do {erl_fp_exception = 0;} while (0)
#  if defined(__i386__) && defined(__GNUC__)
/* extern void erts_restore_x87(void); */

static void unmask_fpe(void)
{
    unsigned short cw;
    __asm__ __volatile__("fstcw %0" : "=m"(cw));
    cw &= ~(0x01|0x04|0x08);   /* unmask IM, ZM, OM */
    __asm__ __volatile__("fldcw %0" : : "m"(cw));
}

static void erts_restore_x87(void)
{
    __asm__ __volatile__("fninit");
    unmask_fpe();
}

static int erts_check_x87(double f)
{
    __asm__ __volatile__("fwait" : "=m"(erl_fp_exception) : "m"(f));
    if( !erl_fp_exception )
       return 0;
    erts_restore_x87();
    return 1;
}
#  define ERTS_FP_ERROR(f, Action) do { if( erts_check_x87((f)) ) { Action; } } while (0)
#  else
#  define ERTS_FP_ERROR(f, Action) if (erl_fp_exception) { Action; } else {}
#  endif
#  define ERTS_SAVE_FP_EXCEPTION() int old_erl_fp_exception = erl_fp_exception
#  define ERTS_RESTORE_FP_EXCEPTION() \
              do {erl_fp_exception = old_erl_fp_exception;} while (0)
#endif


#ifdef INLINED_FP_CONVERSION
static void join(unsigned d_split[4], unsigned *d)
{
    d[0] = (d_split[0] << 31) |         /* Sign bit */
	((d_split[1] & 0x7FFU) << 20) | /* Exponent */
	(d_split[2] & 0xFFFFFU);        /* Mantissa MS bits */
    d[1] = d_split[3];                  /* Mantissa LS bits */
}

static int blength(unsigned long l)
{
    int i;
    for(i = 0; l; ++i)
	l >>= 1;
    return i;
}

static int bblength(erlang_big *b)
{
    unsigned int wholebytes = (b->arity+1)/2;
    digit_t *dp = b->digits;

    while(wholebytes > 0 && dp[--wholebytes] == 0U)
	;

    return (wholebytes * sizeof(digit_t) * 8) +  blength(dp[wholebytes]);
}

static unsigned long bindex(erlang_big *b, int ndx) {
    digit_t *dp = b->digits;
    int skipdigits;
    int dnum;

    if (ndx < 0)
	return 0;

    skipdigits = ndx / (sizeof(digit_t) * 8);
    dnum = ndx % (sizeof(digit_t) * 8);
    return !!(dp[skipdigits] & (1UL << dnum));
}


#endif


int ei_big_to_double(erlang_big *b, double *resp)
{
#ifdef INLINED_FP_CONVERSION
    unsigned d_split[4];
    unsigned *uresp = (unsigned *) resp;
    unsigned len = bblength(b);
    int i;
    unsigned long msm = 0, lsm = 0;
    
    /* OK, this is not the most efficient conversion in the world, especially
       not the bit-by-bit copying to the mantissa.... Simple, working and 
       only for vxworks ppc860 where no sane person would use floating
       point anyway, eh? /Patrik */

    if (!len) {
	memset(d_split,0,sizeof(d_split)); /* 0 */
    } else {
	--len;
	if (len > 1023) { /* Infinite */
	    d_split[1] = 2047;
	    d_split[2] = d_split[3] = 0;
	} else {
	    d_split[1] = 1023 + len; 
	    --len; /* skip the implicit binary 1. */
	    for (i = 0; i < 20; ++i, --len) {
		msm <<= 1;
		msm |= bindex(b,len);
	    }
	    for (i = 0; i < 32; ++i, --len) {
		lsm <<= 1;
		lsm |= bindex(b,len);
	    }
	    d_split[2] = msm;
	    d_split[3] = lsm;
	}	
    }
    d_split[0] = (unsigned) !!(b->is_neg);
    join(d_split,uresp);
    return 0;
#else
    double d = 0.0;
    double d_base = 1.0;

    digit_t* s = (digit_t *)b->digits;
    dsize_t xl = (b->arity + 1)/2;
    short xsgn = b->is_neg;
    ERTS_SAVE_FP_EXCEPTION();

    ERTS_FP_CHECK_INIT();
    while(xl--) {
	digit_t ds = *s;
	double d_next = ds * d_base + d;

	ERTS_FP_ERROR(d_next, ERTS_RESTORE_FP_EXCEPTION(); {fprintf(stderr,"\r\n### fp exception ###\r\n"); return -1;});
	s++;
	d = d_next;
	d_base *= D_BASE;
    }

    /*
     * Note: The last multiplication in the loop could trigger an exception,
     * which we will ignore because the result will never be used.
     */

    *resp = xsgn ? -d : d;
    ERTS_FP_ERROR(*resp,;);
    ERTS_RESTORE_FP_EXCEPTION();
    return 0;
#endif
}

int ei_small_to_big(int s, erlang_big *b)
{
    digit_t *d;
    unsigned int n = (b->arity+1)/2;

    if ( n < 2 ) return -1;

    b->is_neg = ( s < 0 );
    d = (digit_t *)b->digits;
    d[0] = DLOW(s);
    d[1] = DHIGH(s);

    return 0;
}
