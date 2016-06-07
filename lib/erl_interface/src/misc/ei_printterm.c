/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
 *

 */
/*
 * Function:
 * ei_print_term to print out a binary coded term
 */

#ifdef VXWORKS
#include <vxWorks.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#ifdef VRTX
#define __READY_EXTENSIONS__
#include <errno.h>
#endif

#include "eidef.h"
#include "eiext.h"
#include "ei_printterm.h"
#include "ei_malloc.h"

#define BINPRINTSIZE 30

/*
 * PRINT out a binary term (hacked from 'erl'_print_term)
 */

static int print_string(FILE* fp, ei_x_buff* x, char* s, int len);
static int print_term(FILE* fp, ei_x_buff* x,
			       const char* buf, int* index);

static void xputc(char c, FILE* fp, ei_x_buff* x)
{
    if (fp != NULL)
	putc(c, fp);
    else
	ei_x_append_buf(x, &c, 1);
}

static void xputs(const char* s, FILE* fp, ei_x_buff* x)
{
    if (fp != NULL)
	fputs(s, fp);
    else
	ei_x_append_buf(x, s, strlen(s));
}

static int xprintf(FILE* fp, ei_x_buff* x, const char* fmt, ...)
{
    int r = 0;
    va_list ap;
    va_start(ap, fmt);
    if (fp != NULL) {
	r = vfprintf(fp, fmt, ap);
    } else {
	/* FIXME always enough in buffer??? */
	char tmpbuf[2000];
	r = vsprintf(tmpbuf, fmt, ap);
	ei_x_append_buf(x, tmpbuf, strlen(tmpbuf));
    }
    va_end(ap);
    return r;
}

static char *ei_big_to_str(erlang_big *b)
{
    int buf_len;
    char *s,*buf;
    unsigned short *sp;
    int i;

    buf_len = 64+b->is_neg+10*b->arity;
    if ( (buf=malloc(buf_len)) == NULL) return NULL;

    memset(buf,(char)0,buf_len);

    s = buf;
    if ( b->is_neg ) 
        s += sprintf(s,"-");
    s += sprintf(s,"#integer(%d) = {",b->arity);
    for(sp=b->digits,i=0;i<b->arity;i++) {
        s += sprintf(s,"%d",sp[i]);
        if ( (i+1) != b->arity ) 
            s += sprintf(s,",");
    }
    s += sprintf(s,"}");
    return buf;
}

static int print_term(FILE* fp, ei_x_buff* x,
			       const char* buf, int* index)
{
    int i, doquote, n, m, ty, r;
    char a[MAXATOMLEN], *p;
    int ch_written = 0;		/* counter of written chars */
    erlang_pid pid;
    erlang_port port;
    erlang_ref ref;
    double d;
    long l;

    int tindex = *index;

    /* use temporary index for multiple (and failable) decodes */

    if (fp == NULL && x == NULL) return -1;

    doquote = 0;
    ei_get_type_internal(buf, index, &ty, &n);
    switch (ty) {
    case ERL_ATOM_EXT:   
    case ERL_ATOM_UTF8_EXT:
    case ERL_SMALL_ATOM_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
	if (ei_decode_atom(buf, index, a) < 0)
	   goto err;
	doquote = !islower((int)a[0]);
	for (p = a; !doquote && *p != '\0'; ++p)
	    doquote = !(isalnum((int)*p) || *p == '_' || *p == '@');
	if (doquote) {
	    xputc('\'', fp, x); ++ch_written; 
	}
	xputs(a, fp, x); ch_written += strlen(a);
	if (doquote) {
	    xputc('\'', fp, x); ++ch_written;
	}
	break;
    case ERL_PID_EXT:
    case ERL_NEW_PID_EXT:
	if (ei_decode_pid(buf, index, &pid) < 0) goto err;
	ch_written += xprintf(fp, x, "<%s.%d.%d>", pid.node,
			      pid.num, pid.serial);
	break;
    case ERL_PORT_EXT:
    case ERL_NEW_PORT_EXT:
	if (ei_decode_port(buf, index, &port) < 0) goto err;
	ch_written += xprintf(fp, x, "#Port<%d.%d>", port.id, port.creation);
	break;
    case ERL_NEW_REFERENCE_EXT:
    case ERL_NEWER_REFERENCE_EXT:
    case ERL_REFERENCE_EXT:
	if (ei_decode_ref(buf, index, &ref) < 0) goto err;
	ch_written += xprintf(fp, x, "#Ref<");
	for (i = 0; i < ref.len; ++i) {
	    ch_written += xprintf(fp, x, "%d", ref.n[i]);
	    if (i < ref.len - 1) {
		xputc('.', fp, x); ++ch_written;
	    }
	}
	xputc('>', fp, x); ++ch_written;
	break;
    case ERL_NIL_EXT:
	if (ei_decode_list_header(buf, index, &n) < 0) goto err;
	ch_written += xprintf(fp, x, "[]");
	break;
    case ERL_LIST_EXT:
	if (ei_decode_list_header(buf, &tindex, &n) < 0) goto err;
	xputc('[', fp, x); ch_written++;
	for (i = 0; i < n; ++i) {
	    r = print_term(fp, x, buf, &tindex);
	    if (r < 0) goto err;
	    ch_written += r;
	    if (i < n - 1) {
		xputs(", ", fp, x); ch_written += 2;
	    }
	}
	if (ei_get_type_internal(buf, &tindex, &ty, &n) < 0) goto err;
	if (ty != ERL_NIL_EXT) {
	    xputs(" | ", fp, x); ch_written += 3;
	    r = print_term(fp, x, buf, &tindex);
	    if (r < 0) goto err;
	    ch_written += r;
	} else {
	    if (ei_decode_list_header(buf, &tindex, &n) < 0) goto err;
	}
	xputc(']', fp, x); ch_written++;
	*index = tindex;
	break;
    case ERL_STRING_EXT:
	p = ei_malloc(n+1);
	if (p == NULL) goto err;
	if (ei_decode_string(buf, index, p) < 0) {
	    ei_free(p);
	    goto err;
	}
	ch_written += print_string(fp, x, p, n);
	ei_free(p);
	break;
    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
	if (ei_decode_tuple_header(buf, &tindex, &n) < 0) goto err;
	xputc('{', fp, x); ch_written++;

	for (i = 0; i < n; ++i) {
	    r = print_term(fp, x, buf, &tindex);
	    if (r < 0) goto err;
	    ch_written += r;
	    if (i < n-1) {
		xputs(", ", fp, x); ch_written += 2;
	    }
	}
	*index = tindex;
	xputc('}', fp, x); ch_written++;
	break;
    case ERL_BINARY_EXT:
	p = ei_malloc(n);
	if (p == NULL) goto err;
	if (ei_decode_binary(buf, index, p, &l) < 0) {
	    ei_free(p);
	    goto err;
	}
	ch_written += xprintf(fp, x, "#Bin<");
	if (l > BINPRINTSIZE)
	    m = BINPRINTSIZE;
	else
	    m = l;
	--m;
	for (i = 0; i < m; ++i) {
	    ch_written += xprintf(fp, x, "%d,", p[i]);
	}
	ch_written += xprintf(fp, x, "%d", p[i]);
	if (l > BINPRINTSIZE)
	    ch_written += xprintf(fp, x, ",...");
	xputc('>', fp, x); ++ch_written;
	ei_free(p);
	break;
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
	if (ei_decode_long(buf, index, &l) < 0) goto err;
	ch_written += xprintf(fp, x, "%ld", l);
	break;
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
        {
            erlang_big *b;
            char *ds;

            if ( (b = ei_alloc_big(n)) == NULL) goto err;

            if (ei_decode_big(buf, index, b) < 0) {
                ei_free_big(b);
                goto err;
            }
            
            if ( (ds = ei_big_to_str(b)) == NULL ) {
                ei_free_big(b);
                goto err;
            }
            
            ch_written += xprintf(fp, x, ds);
            free(ds);
            ei_free_big(b);
            
        }
        break;

    case ERL_FLOAT_EXT:
    case NEW_FLOAT_EXT:
	if (ei_decode_double(buf, index, &d) < 0) goto err;
	ch_written += xprintf(fp, x, "%f", d);
	break;
    default:
	goto err;
    }
    return ch_written;
 err:
    return -1;
}

static int print_string(FILE* fp, ei_x_buff* x, char* s, int len)
{
    int ch_written = 0;		/* counter of written chars */

    xputc('"', fp, x);
    ++ch_written;
    for (; len > 0; ++s, --len) {
	int c = *s;
	if (c >= ' ') {
	    xputc((char)c, fp, x); ++ch_written; }
	else {
	    switch (c) {
	    case '\n': xputs("\\n", fp, x); ch_written += 2; break;
	    case '\r': xputs("\\r", fp, x); ch_written += 2; break;
	    case '\t': xputs("\\t", fp, x); ch_written += 2; break;
	    case '\v': xputs("\\v", fp, x); ch_written += 2; break;
	    case '\b': xputs("\\b", fp, x); ch_written += 2; break;
	    case '\f': xputs("\\f", fp, x); ch_written += 2; break;
		break;
	    default:
		ch_written += xprintf(fp, x, "\\x%x", c);
		break;
	    }
	}
    }
    xputc('"', fp, x); ch_written++;
    return ch_written;
}

/* ------------------------------------------ */

/*
 * skip a binary term
 */


int ei_print_term(FILE *fp, const char* buf, int* index)
{
    return print_term(fp, NULL, buf, index);
}

int ei_s_print_term(char** s, const char* buf, int* index)
{
    int r;
    ei_x_buff x;
    if (*s != NULL) {
	x.buff = *s;
	x.index = 0;
	x.buffsz = BUFSIZ;
    } else {
	ei_x_new(&x);
    }
    r = print_term(NULL, &x, buf, index);
    ei_x_append_buf(&x, "", 1); /* append '\0' */
    *s = x.buff;
    return r;
}
