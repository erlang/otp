/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
 * fmt:
 *    '%' <flag>* [ <width> [.<precision>]][<length>]<conversion>
 *
 *  flag: # | O | - | <sp> | + | ' | I
 *  width:      [0-9]+ | '*' 
 *  precision:  [0-9]+ | '*' 
 *  length: hh | h | l | ll | L | j | t | b<sz>
 *  conversion: d,i | o,u,x,X | e,E | f,F | g,G | a,A | c | s | T | R |
 *              p | n | %
 *  sz: 8 | 16 | 32 | 64 | p | e
 */

/* Without this, variable argument lists break on VxWorks */
#ifdef VXWORKS
#include <vxWorks.h>
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef __WIN32__
#undef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <ctype.h>
#include <string.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "erl_errno.h"
#include <limits.h>
#include "erl_printf.h"
#include "erl_printf_format.h"

#ifdef DEBUG
#include <assert.h>
#define ASSERT(X) assert(X)
#else
#define ASSERT(X) 
#endif

#ifdef __WIN32__
#define long_long		LONGLONG
#define signed_long_long	LONGLONG
#define unsigned_long_long	ULONGLONG
#undef SIZEOF_LONG_LONG
#define SIZEOF_LONG_LONG 8
#else
#if SIZEOF_LONG_LONG
#define long_long		long long
#define signed_long_long	signed long long
#define unsigned_long_long	unsigned long long
#endif
#endif

#ifndef ERTS_SIZEOF_ETERM
#define ERTS_SIZEOF_ETERM SIZEOF_VOID_P
#endif

#if defined(__GNUC__)
#  undef inline
#  define inline __inline__
#elif defined(__WIN32__)
#  undef inline
#  define inline __forceinline
#else
#  ifndef inline
#    define inline
#  endif
#endif

#define FMTC_d    0x0000
/*empty           0x0001 was RELATIVE */
#define FMTC_o    0x0002
#define FMTC_u    0x0003
#define FMTC_x    0x0004
#define FMTC_X    0x0005
#define FMTC_e    0x0006
#define FMTC_E    0x0007
#define FMTC_f    0x0008
#define FMTC_T    0x0009
#define FMTC_g    0x000a
#define FMTC_G    0x000b
#define FMTC_c    0x000c
#define FMTC_s    0x000d
#define FMTC_p    0x000e
#define FMTC_n    0x000f
#define FMTC_MASK 0x000f

#define FMTL_no   0x0000
#define FMTL_hh   0x0010
#define FMTL_h    0x0020
#define FMTL_l    0x0030
#define FMTL_ll   0x0040
#define FMTL_L    0x0050
#define FMTL_j    0x0060
#define FMTL_t    0x0070
#define FMTL_MASK 0x00f0

#define FMTF_alt  0x0100    /* # alterlate form ie 0x */
#define FMTF_pad  0x0200    /* 0 zero pad */
#define FMTF_adj  0x0400    /* left adjust */
#define FMTF_blk  0x0800    /* add blank */
#define FMTF_sgn  0x1000    /* add sign */
#define FMTF_cnv  0x2000    /* decimal conversion */
#define FMTF_cnV  0x4000    /* alternate decimal conversion */
#define FMTF_MASK 0x7f00


static char zeros[]  = "00000000000000000000000000000000";
static char blanks[] = "                                ";
static char hex[] = "0123456789abcdef";
static char heX[] = "0123456789ABCDEF";

#define FMT(fn,arg,buf,len,count) do { \
    int res__ = (fn)((arg),(buf),(len)); \
    if (res__ < 0) \
	return res__; \
    (count) += (len); \
 } while(0)

#define FILL(fn,arg,cs,len, count) do { \
    int __i = (len); \
    while(__i >= sizeof(cs)-1) { \
        FMT((fn),(arg),(cs),sizeof(cs)-1,(count)); \
        __i -= sizeof(cs)-1; \
    } \
    if (__i) FMT((fn),(arg),(cs),__i,(count)); \
 } while(0)

#define BLANKS(fn,arg,n,count) FILL((fn),(arg),blanks,(n),count)
#define ZEROS(fn,arg,n,count)  FILL((fn),(arg),zeros,(n),count)

#define SIGN(X) ((X) > 0 ? 1 : ((X) < 0 ? -1 : 0)) 
#define USIGN(X) ((X) == 0 ? 0 : 1)

int (*erts_printf_eterm_func)(fmtfn_t, void*, ErlPfEterm, long) = NULL;

static int
noop_fn(void *vfp, char* buf, size_t len)
{
    return 0;
}

static int fmt_fld(fmtfn_t fn,void* arg,
		   char* wbuf, int w, int sign,
		   int width,int precision,int fmt,int* count)
{
    char prefix[8];
    char* pp = prefix;
    int pw   = 0;
    int len;

    /* format the prefix */
    if ((sign || (fmt & (FMTF_sgn|FMTF_blk)))
	&& (fmt & FMTC_MASK) == FMTC_d) {
	if (sign < 0)
	    *pp++ = '-';
	else if ((fmt & FMTF_sgn))
	    *pp++ = '+';
	else if (fmt & FMTF_blk)
	    *pp++ = ' ';
    }

    if ((fmt & FMTF_alt)) {
	switch((fmt & FMTC_MASK)) {
	case FMTC_X: *pp++ = '0'; *pp++ = 'X'; break;
	case FMTC_x: *pp++ = '0'; *pp++ = 'x'; break;
	case FMTC_o: *pp++ = '0'; if (precision>1) precision--; break;
	}
    }

    pw = pp-prefix;
    len = ((w < precision) ? precision : w) + pw;

    if (fmt & FMTF_adj) { /* left adjust */
	if (pw)
	    FMT(fn,arg,prefix,pw,*count);
	if (w < precision)
	    ZEROS(fn,arg,precision-w,*count);
	FMT(fn,arg, wbuf, w, *count);
	if (len < width)
	    BLANKS(fn,arg,width-len,*count);
    }
    else if ((fmt & FMTF_pad) && (precision<0)) { /* pad zeros */
	if (pw)
	    FMT(fn,arg, prefix, pw, *count);
	if (w < precision)
	    ZEROS(fn, arg, precision-w, *count);
	if (len < width)
	    ZEROS(fn,arg,width-len,*count);
	FMT(fn,arg,wbuf,w,*count);
    }
    else {
	if (len < width)
	    BLANKS(fn,arg,width-len,*count);
	if (pw)
	    FMT(fn,arg,prefix,pw,*count);
	if (w < precision)
	    ZEROS(fn,arg,precision-w,*count);
	FMT(fn,arg,wbuf,w,*count);
    }
    return 0;
}

static int fmt_uword(fmtfn_t fn,void* arg,int sign,ErlPfUWord uval,
		    int width,int precision,int fmt,int* count)
{
    char buf[32];
    int base = 10;
    int w    = 0;
    char* dc = hex;
    char* p = buf+sizeof(buf);

    switch(fmt & FMTC_MASK) {
    case FMTC_d:
    case FMTC_u:
	break;
    case FMTC_o: 
	base = 8;
	break;
    case FMTC_X:
	dc = heX;
    case FMTC_x:
	base = 16;
	break;
    default: 
	return -EINVAL;
    }

    /* format the unsigned value */
    if (!sign && precision) {
	*--p = '0';
	w++;
    }
    else {
	while(uval) {
	    *--p = dc[(uval % base)];
	    uval /= base;
	    w++;
	}
    }
    return fmt_fld(fn, arg, p, w, sign, width, precision, fmt, count);
}

#if SIZEOF_LONG_LONG

static inline int
do_div(unsigned_long_long *n, unsigned_long_long base)
{
    unsigned_long_long q = *n/base;
    int mod = (int) (*n - q*base);
    *n = q;
    return mod;
}

static int fmt_long_long(fmtfn_t fn,void* arg,int sign,
			 unsigned_long_long uval,
			 int width,int precision,int fmt,int* count)
{
    char buf[32];
    int base = 10;
    int w    = 0;
    char* dc = hex;
    char* p = buf+sizeof(buf);

    switch(fmt & FMTC_MASK) {
    case FMTC_d:
    case FMTC_u:
	break;
    case FMTC_o: 
	base = 8;
	break;
    case FMTC_X:
	dc = heX;
    case FMTC_x:
	base = 16;
	break;
    default: 
	return -EINVAL;
    }

    /* format the unsigned value */
    if (!sign && precision) {
	*--p = '0';
	w++;
    }
    else {
	while(uval) {
	    int m = do_div(&uval,base);
	    *--p = dc[m];
	    w++;
	}
    }
    return fmt_fld(fn, arg, p, w, sign, width, precision, fmt, count);
}

#endif /* #if SIZEOF_LONG_LONG */

static int fmt_double(fmtfn_t fn,void*arg,double val,
		      int width, int precision, int fmt,int* count)
{
    int res;
    int fi = 0;
    char format_str[7];
    char sbuf[32];
    char *bufp = sbuf;
    double dexp;
    int exp;
    size_t max_size = 1;
    int size;
    int new_fmt = fmt;
    int fpe_was_unmasked;

    fpe_was_unmasked = erts_printf_block_fpe ? (*erts_printf_block_fpe)() : 0;

    if (val < 0.0)
	dexp = log10(-val);
    else if (val == 0.0)
	dexp = 0.0;
    else
	dexp = log10(val);
    exp = (int) dexp;

    new_fmt &= ~FMTF_sgn;
    new_fmt &= ~FMTF_blk;

    format_str[fi++] = '%';
    if (fmt & FMTF_alt)
	format_str[fi++] = '#';
    if (fmt & FMTF_sgn)
	format_str[fi++] = '+';
    else if (fmt & FMTF_blk)
	format_str[fi++] = ' ';
    format_str[fi++] = '0';
    format_str[fi++] = '.';
    format_str[fi++] = '*';
	
    switch(fmt & FMTC_MASK) {
    case FMTC_G:
	format_str[fi] = 'E';
	goto gG_common;
    case FMTC_g:
	format_str[fi] = 'e';
    gG_common:
	if (dexp < -4.0 || exp >= precision) {
	    fi++;
	    precision--;
	    if (precision < 1)
		precision = 1;
	    goto eE_common;
	}
	/* fall through ... */
    case FMTC_f:
	format_str[fi++] = 'f';
	max_size += exp > 0 ? exp : 1;
	max_size++;
	if (precision)
	    max_size += precision;
	else if (fmt & FMTF_alt)
	    max_size++;
	break;
    case FMTC_E:
	format_str[fi++] = 'E';
	goto eE_common;
    case FMTC_e:
	format_str[fi++] = 'e';
    eE_common: {
	int aexp;

	max_size += 4;
	if (precision)
	    max_size += precision;
	else if (fmt & FMTF_alt)
	    max_size++;
	aexp = exp >= 0 ? exp : -exp;
	if (aexp < 100)
	    max_size += 2;
	else {
	    while (aexp) {
		max_size++;
		aexp /= 10;
	    }
	}
	break;
    }
    default:
	res = -EINVAL;
	goto out;
    }

    format_str[fi++] = '\0';
    ASSERT(fi <= sizeof(format_str));

    max_size++; /* '\0' */

    if (max_size >= sizeof(sbuf)) {
	bufp = (char *) malloc(sizeof(char)*max_size);
	if (!bufp) {
	    res = -ENOMEM;
	    /* Make sure not to trigger free */
	    bufp = sbuf;
	    goto out;
	}
    }

    size = sprintf(bufp, format_str, precision, val);
    if (size < 0) {
	if (errno > 0)
	    res = -errno;
	else
	    res = -EIO;
	goto out;
    }

    ASSERT(max_size >= size);

    res = fmt_fld(fn, arg, bufp, size, 0, width, 0, new_fmt, count);

 out:
    if (bufp != sbuf)
	free((void *) bufp);

    if (erts_printf_unblock_fpe)
	(*erts_printf_unblock_fpe)(fpe_was_unmasked);
    return res;
}

/* strnlen doesn't exist everywhere */
static size_t my_strnlen(const char *s, size_t maxlen)
{
    size_t i = 0;
    while (i < maxlen && s[i] != '\0')
	i++;
    return i;
}

int erts_printf_format(fmtfn_t fn, void* arg, char* fmt, va_list ap)
{
    char* ptr0 = fmt;
    char* ptr = ptr0;
    int count = 0;
    int n;
    int res = 0;

    while(*ptr) {
	ErlPfUWord ul_val;
	int fmt        = 0;
	int width      = -1;
	int precision  = -1;

	if (res < 0)
	    return res;

	if (*ptr == '%') {
	    if ((n=ptr-ptr0))
		FMT(fn,arg,ptr0,n,count);
	    ptr++;

	do_flag:
	    switch(*ptr) {
	    case '#':  fmt |= FMTF_alt; ptr++; goto do_flag;
	    case '0':  fmt |= FMTF_pad; ptr++; goto do_flag;
	    case '-':  fmt |= FMTF_adj; ptr++; goto do_flag;
	    case ' ':  fmt |= FMTF_blk; ptr++; goto do_flag;
	    case '+':  fmt |= FMTF_sgn; ptr++; goto do_flag;
	    case '\'': fmt |= FMTF_cnv; ptr++; goto do_flag;
	    case 'I':  fmt |= FMTF_cnV; ptr++; goto do_flag;
	    }

	    /* width */
	    if (*ptr == '*') {
		width = va_arg(ap, int);
		ptr++;
	    }
	    else if (isdigit((int) *ptr)) {
		width = *ptr++ - '0';
		while(isdigit((int) *ptr))
		    width = 10*width + (*ptr++ - '0');
	    }

	    /* precision */
	    if (*ptr == '.') {
		ptr++;
		if (*ptr == '*') {
		    precision = va_arg(ap, int);
		    ptr++;
		}
		else if (isdigit((int) *ptr)) {
		    precision = *ptr++ - '0';
		    while(isdigit((int) *ptr))
			precision = 10*precision + (*ptr++ - '0');
		}
	    }

	    /* length modifier */
	    switch(*ptr) {
	    case 'b': {
		ptr++;
		if (*ptr == 'p') {
		    ptr++;
#if SIZEOF_INT == SIZEOF_VOID_P
#elif SIZEOF_LONG == SIZEOF_VOID_P
		    fmt |= FMTL_l;
#elif SIZEOF_LONG_LONG == SIZEOF_VOID_P
		    fmt |= FMTL_ll;
#else
#error No integer datatype with the same size as 'void *' found
#endif
		}
		else if (*ptr == 'e') {
		    ptr++;
#if SIZEOF_INT == ERTS_SIZEOF_ETERM
#elif SIZEOF_LONG == ERTS_SIZEOF_ETERM
		    fmt |= FMTL_l;
#elif SIZEOF_LONG_LONG == ERTS_SIZEOF_ETERM
		    fmt |= FMTL_ll;
#else
#error No integer datatype with the same size as Eterm found
#endif
		}
		else {
		    int bits = 0;
		    while(isdigit((int) *ptr))
			bits = 10*bits + (*ptr++ - '0');
		    switch (bits) {
		    case 64:
#if SIZEOF_INT == 8
#elif SIZEOF_LONG == 8
			fmt |= FMTL_l;
#elif SIZEOF_LONG_LONG == 8
			fmt |= FMTL_ll;
#else
#error No 64-bit integer datatype found
#endif
			break;
		    case 32:
#if SIZEOF_INT == 4
#elif SIZEOF_SHORT == 4
			fmt |= FMTL_h;
#elif SIZEOF_LONG == 4
			fmt |= FMTL_l;
#elif SIZEOF_LONG_LONG == 4
			fmt |= FMTL_ll;
#else
#error No 32-bit integer datatype found
#endif
			break;
		    case 16:
#if SIZEOF_INT == 2
#elif SIZEOF_SHORT == 2
			fmt |= FMTL_h;
#elif SIZEOF_LONG == 2
			fmt |= FMTL_l;
#else
#error No 16-bit integer datatype found
#endif
		    case 8:
#if SIZEOF_CHAR == 1
			fmt |= FMTL_hh;
#else
#error Unexpected size of char
#endif
			break;
		    default:
			return -EINVAL;
		    }
		}
		break;
	    }
	    case 'h': 
		ptr++;
		if (*ptr == 'h') {
		    ptr++;
		    fmt |= FMTL_hh;
		}
		else
		    fmt |= FMTL_h;
		break;
	    case 'l':
		ptr++;
		if (*ptr == 'l') {
		    ptr++;
#if SIZEOF_LONG_LONG
		    fmt |= FMTL_ll;
#else
		    fmt |= FMTL_l;
#endif
		}
		else
		    fmt |= FMTL_l;
		break;
	    case 'L': ptr++; fmt |= FMTL_L; break;
	    case 'j': ptr++; fmt |= FMTL_j; break;
	    case 't': ptr++; fmt |= FMTL_t; break;
	    }

	    /* specifier */
	    switch(*ptr) {
	    case 'd': ptr++; fmt |= FMTC_d; break;
	    case 'i': ptr++; fmt |= FMTC_d; break;
	    case 'o': ptr++; fmt |= FMTC_o; break;
	    case 'u': ptr++; fmt |= FMTC_u; break;
	    case 'x': ptr++; fmt |= FMTC_x; break;
	    case 'X': ptr++; fmt |= FMTC_X; break;
	    case 'e': ptr++; fmt |= FMTC_e; break;
	    case 'E': ptr++; fmt |= FMTC_E; break;
	    case 'f': ptr++; fmt |= FMTC_f; break;
	    case 'g': ptr++; fmt |= FMTC_g; break;
	    case 'G': ptr++; fmt |= FMTC_G; break;
	    case 'c': ptr++; fmt |= FMTC_c; break;
	    case 's': ptr++; fmt |= FMTC_s; break;
	    case 'p': ptr++; fmt |= FMTC_p; break;
	    case 'n': ptr++; fmt |= FMTC_n; break;
	    case 'T': ptr++; fmt |= FMTC_T; break;
	    case '%':
		FMT(fn,arg,ptr,1,count);
		ptr++;
		ptr0 = ptr;
		continue;
	    default:
		/* ignore */
		ptr0 = ptr;
		continue;
	    }

	    switch(fmt & FMTC_MASK) {
	    case FMTC_d:
		switch(fmt & FMTL_MASK) {
		case FMTL_hh: {
		    signed char tval = (signed char) va_arg(ap,int);
		    ul_val = (ErlPfUWord) (tval < 0 ? (-tval) : tval);
		    res = fmt_uword(fn,arg,SIGN(tval),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
		case FMTL_h: {
		    signed short tval = (signed short) va_arg(ap,int);
		    ul_val = (ErlPfUWord) (tval < 0 ? (-tval) : tval);
		    res = fmt_uword(fn,arg,SIGN(tval),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
		case FMTL_l: {
		    signed long tval = (signed long) va_arg(ap,long);
		    ul_val = (ErlPfUWord) (tval < 0 ? (-tval) : tval);
		    res = fmt_uword(fn,arg,SIGN(tval),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
#if SIZEOF_LONG_LONG
		case FMTL_ll: {
		    unsigned_long_long ull_val;
		    signed_long_long tval;
		    tval = (signed_long_long) va_arg(ap,long_long);
		    ull_val = (unsigned_long_long) (tval < 0 ? (-tval) : tval);
		    res = fmt_long_long(fn,arg,SIGN(tval),ull_val,
					width,precision,fmt,&count);
		    break;
		}
#endif
		default: {
		    signed int tval = (signed int) va_arg(ap,int);
		    ul_val = (ErlPfUWord) (tval < 0 ? (-tval) : tval);
		    res = fmt_uword(fn,arg,SIGN(tval),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
		}
		break;
	    case FMTC_o:
	    case FMTC_u:
	    case FMTC_x:
	    case FMTC_X:
		switch(fmt & FMTL_MASK) {
		case FMTL_hh: {
		    unsigned char tval = (unsigned char) va_arg(ap,int);
		    ul_val = (ErlPfUWord) tval;
		    res = fmt_uword(fn,arg,USIGN(tval),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
		case FMTL_h: {
		    unsigned short tval = (unsigned short) va_arg(ap,int);
		    ul_val = (ErlPfUWord) tval;
		    res = fmt_uword(fn,arg,USIGN(tval),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
		case FMTL_l: {
		    ul_val = (ErlPfUWord) va_arg(ap,long);
		    res = fmt_uword(fn,arg,USIGN(ul_val),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
#if SIZEOF_LONG_LONG
		case FMTL_ll: {
		    unsigned_long_long ull_val;
		    ull_val = (signed_long_long) va_arg(ap,long_long);
		    res = fmt_long_long(fn,arg,USIGN(ull_val),ull_val,
					width,precision,fmt,&count);
		    break;
		}
#endif
		default: {
		    unsigned int tval = (unsigned int) va_arg(ap,int);
		    ul_val = (ErlPfUWord) tval;
		    res = fmt_uword(fn,arg,USIGN(tval),ul_val,
				   width,precision,fmt,&count);
		    break;
		}
		}
		break;
	    case FMTC_e:
	    case FMTC_E:
	    case FMTC_f:
	    case FMTC_g:
	    case FMTC_G:
		if (precision < 0)
		    precision = 6;
		switch(fmt & FMTL_MASK) {
		case FMTL_L:
		    return -EINVAL;
		    break;
		default:
		    res = fmt_double(fn,arg,va_arg(ap,double),
				     width,precision,fmt,&count);
		    break;
		}
		break;

	    case FMTC_c: {
		/* fixme: add wide char support l-modifier */
		char c = va_arg(ap,int);
		int len = 1;
		if (precision == 0)
		    len = 0;
		if (width > 0 && !(fmt & FMTF_adj)) {
		    if (width > len)
			BLANKS(fn, arg, width - len, count);
		}
		if (len)
		    FMT(fn,arg,&c,len,count);
		if (width > len && fmt & FMTF_adj)
		    BLANKS(fn, arg, width - len, count);
		break;
	    }
		
	    case FMTC_s: {
		char* str = va_arg(ap,char*);
		int len = (precision >= 0) ? my_strnlen(str,precision) : strlen(str);
		if (width > 0 && !(fmt & FMTF_adj)) {
		    if (width > len)
			BLANKS(fn, arg, width - len, count);
		}
		if (len)
		    FMT(fn,arg,str,len,count);
		if (width > len && fmt & FMTF_adj)
		    BLANKS(fn, arg, width - len, count);
		break;
	    }

	    case FMTC_p: {
		void* addr = va_arg(ap, void*);

		res = fmt_uword(fn,
			       arg,
			       USIGN((ErlPfUWord) addr),
			       (ErlPfUWord) addr,
			       width < 0 ? ((int) 2*sizeof(void *)) : width,
			       (precision < 0
				? ((int) 2*sizeof(void *))
				: precision),
			       FMTC_x|FMTF_pad|FMTF_alt,
			       &count);
		break;
	    }

	    case FMTC_n:
		switch(fmt & FMTL_MASK) {
		case FMTL_hh: *va_arg(ap,char*) = count; break;
		case FMTL_h:  *va_arg(ap,short*) = count; break;
		case FMTL_l:  *va_arg(ap,long*) = count; break;
#if SIZEOF_LONG_LONG
		case FMTL_ll: *va_arg(ap,long_long*) = count; break;
#endif
		default: *va_arg(ap,int*) = count; break;
		}
		break;
	    case FMTC_T: {    /* Eterm */
		long prec;
		ErlPfEterm eterm;
		
		if (!erts_printf_eterm_func)
		    return -EINVAL;
		if (precision < 0)
		    prec = 100000;
		else if (precision == INT_MAX)
		    prec = LONG_MAX;
		else
		    prec = (long) precision;
		eterm = va_arg(ap, ErlPfEterm);
		if (width > 0 && !(fmt & FMTF_adj)) {
		    res = (*erts_printf_eterm_func)(noop_fn, NULL, eterm, prec);
		    if (res < 0)
			return res;
		    if (width > res)
			BLANKS(fn, arg, width - res, count);
		}
		res = (*erts_printf_eterm_func)(fn, arg, eterm, prec);
		if (res < 0)
		    return res;
		count += res;
		if (width > res && fmt & FMTF_adj)
		    BLANKS(fn, arg, width - res, count);
		break;
	    }
	    default:
		if ((n=ptr-ptr0))
		    FMT(fn,arg,ptr0,n,count);
	    }
	    ptr0 = ptr;
	}
	else 
	    ptr++;
    }
    
    if ((n=ptr-ptr0))
	FMT(fn,arg,ptr0,n,count);
    return count;
}


int
erts_printf_char(fmtfn_t fn, void *arg, char c)
{
    return (*fn)(arg, &c, 1);
}

int
erts_printf_string(fmtfn_t fn, void *arg, char *str)
{
    size_t sz = strlen(str);
    return (*fn)(arg, str, sz);
}

int
erts_printf_buf(fmtfn_t fn, void *arg, char *buf, size_t sz)
{
    return (*fn)(arg, buf, sz);
}

int
erts_printf_pointer(fmtfn_t fn, void *arg, void *ptr)
{
    int count = 0;
    int res = fmt_uword(fn, arg, USIGN((ErlPfUWord) ptr),
		       (ErlPfUWord) ptr, 2*sizeof(void *),
		       2*sizeof(void *), FMTC_x|FMTF_pad|FMTF_alt, &count);
    if (res < 0)
	return res;
    return count;
}

int
erts_printf_uword(fmtfn_t fn, void *arg, char conv, int pad, int width,
		  ErlPfUWord val)
{
    int count = 0;
    int res;
    int fmt = 0;
    int prec = -1;
    switch (conv) {
    case 'o': fmt |= FMTC_o; break;
    case 'u': fmt |= FMTC_u; break;
    case 'x': fmt |= FMTC_x; break;
    case 'X': fmt |= FMTC_X; break;
    case 'p': fmt |= FMTC_p; break;
    default:
	return -EINVAL;
    }
    if (pad)
	prec = width;
    res = fmt_uword(fn, arg, USIGN(val), val, width, prec, fmt, &count);
    if (res < 0)
	return res;
    return count;
}

int
erts_printf_sword(fmtfn_t fn, void *arg, char conv, int pad, int width,
		  ErlPfSWord val)
{
    int count = 0;
    int res;
    int fmt = 0;
    int prec = -1;
    ErlPfUWord ul_val;
    switch (conv) {
    case 'd': fmt |= FMTC_d; break;
    case 'i': fmt |= FMTC_d; break;
    case 'o': fmt |= FMTC_o; break;
    case 'x': fmt |= FMTC_x; break;
    case 'X': fmt |= FMTC_X; break;
    default:
	return -EINVAL;
    }
    if (pad)
	prec = width;
    ul_val = (ErlPfUWord) (val < 0 ? -val : val);
    res = fmt_uword(fn, arg, SIGN(val), ul_val, width, prec, fmt, &count);
    if (res < 0)
	return res;
    return count;
}

int
erts_printf_double(fmtfn_t fn, void *arg, char conv, int precision, int width,
		   double val)
{
    int count = 0;
    int res;
    int fmt = 0;
    switch (conv) {
    case 'e': fmt |= FMTC_e; break;
    case 'E': fmt |= FMTC_E; break;
    case 'f': fmt |= FMTC_f; break;
    case 'g': fmt |= FMTC_g; break;
    case 'G': fmt |= FMTC_G; break;
    default:
	return -EINVAL;
    }
    res = fmt_double(fn, arg, val, width, precision, fmt, &count);
    if (res < 0)
	return res;
    return count;
}
