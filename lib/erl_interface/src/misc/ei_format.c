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
 * ei_format to build binary format terms a bit like printf
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
#include "ei_malloc.h"
#include "ei_format.h"

/*
 * To avoid problems we read the variable number of arguments to an
 * array of unions.
 */
union arg {
  char c;
  char* s;
  long l;
  unsigned long u;
  double d;
  erlang_pid* pid;
};

static int eiformat(const char** s, union arg** args, ei_x_buff* x);

/* forwards of parse functions */
static int pformat(const char** fmt, union arg**, ei_x_buff* x);
static int plist(const char** fmt, union arg**, ei_x_buff* x, int size);
static int ptuple(const char** fmt, union arg**, ei_x_buff* x, int size);
static int pquotedatom(const char** fmt, ei_x_buff* x);
static int pdigit(const char** fmt, ei_x_buff* x);
static int patom(const char** fmt, ei_x_buff* x);
static int pstring(const char** fmt, ei_x_buff* x);

/* format a string into an ei_x_buff, except the version token */
static int eiformat(const char** fmt, union arg** args, ei_x_buff* x)
{
    const char* p = *fmt;
    int res;
    ei_x_buff x2;

    while (isspace((int)*p))
	++p;
    switch (*p) {
    case '~':
	res = pformat(&p, args, x);
	break;
    case '[':
	res = ei_x_new(&x2);
	if (res >= 0)
	    res = plist(&p, args, &x2, 0);
	if (res > 0)
	    res = ei_x_encode_list_header(x, res);
	if (res >= 0)
	    res = ei_x_append(x, &x2);
	ei_x_free(&x2);
	break;
    case '{':
	res = ei_x_new(&x2);
	if (res >= 0)
	    res = ptuple(&p, args, &x2, 0);
	if (res >= 0)
	    res = ei_x_encode_tuple_header(x, res);
	if (res >= 0)
	    res = ei_x_append(x, &x2);
	ei_x_free(&x2);
	break;
    case '"':
	res = pstring(&p, x);
	break;
    case '\'':
	res = pquotedatom(&p, x);
	break;
    default:
	if (isdigit((int)*p))
	    res = pdigit(&p, x);
	else if ((*p == '-' || *p == '+') && isdigit((int)*(p+1)))
	    res = pdigit(&p, x);
	else if (islower((int)*p))
	    res = patom(&p, x);
	else
	    res = -1;
	break;
	/*
	Variables
	*/
    }
    *fmt = p;
    return res;
}

static int patom(const char** fmt, ei_x_buff* x)
{
    const char* start = *fmt;
    char c;
    int len;
    
    for (;;) {
	c = *(*fmt)++;
	if (isalnum((int) c) || (c == '_') || (c == '@'))
	    continue;
	else
	    break;
    }
    --(*fmt);
    len = *fmt - start;
    /* FIXME why truncate atom name and not fail?! */
    if (len >= MAXATOMLEN)
	len = MAXATOMLEN-1;
    return ei_x_encode_atom_len(x, start, len);
}

/* Check if integer or float */
static int pdigit(const char** fmt, ei_x_buff* x)
{
    const char* start = *fmt;
    char c;
    int dotp=0;
    double d;
    long l;

    if (**fmt == '-' || **fmt == '+')
	(*fmt)++;
    for (;;) {
	c = *(*fmt)++;
	if (isdigit((int)c))
	    continue;
	else if (!dotp && (c == '.')) {
	    dotp = 1;
	    continue;
	} else
	    break;
    } 
    --(*fmt);
    if (dotp) {
	sscanf(start, "%lf", &d);
	return ei_x_encode_double(x, d);
    } else {
	sscanf(start, "%ld", &l);
	return ei_x_encode_long(x, l);
    }
}

/* "string" */
static int pstring(const char** fmt, ei_x_buff* x)
{
    const char* start = ++(*fmt); /* skip first quote */
    char c;
    int res;
    
    for (;;) {
	c = *(*fmt)++;
	if (c == '\0')
	    return -1;
	if (c == '"') {
	    if (*((*fmt)-1) == '\\')
		continue;
	    else
		break;
	} else
	    continue;
    }
    res = ei_x_encode_string_len(x, start, *fmt - start - 1);
    return res;
}

/* 'atom' */
static int pquotedatom(const char** fmt, ei_x_buff* x)
{
    const char* start = ++(*fmt); /* skip first quote */
    char c;
    int res;
    
    for (;;) {
	c = *(*fmt)++;
	if (c == 0)
	    return -1;
	if (c == '\'') {
	    if (*((*fmt)-1) == '\\')
		continue;
	    else
		break;
	} else 
	    continue;
    } 
    res = ei_x_encode_atom_len(x, start, *fmt - start - 1);
    return res;
}


 /* 
  * The format letters are:
  *   a  -  An atom
  *   c  -  A character
  *   s  -  A string
  *   i  -  An integer
  *   l  -  A long integer
  *   u  -  An unsigned long integer
  *   f  -  A float 
  *   d  -  A double float 
  *   p  -  An Erlang PID
  */
static int pformat(const char** fmt, union arg** args, ei_x_buff* x)
{
    int res = 0;
    ++(*fmt);	/* skip tilde */
    switch (*(*fmt)++) {
    case 'a': 
	res = ei_x_encode_atom(x, (*args)->s);
	(*args)++;
	break;
    case 'c':
	res = ei_x_encode_char(x, (*args)->c);
	(*args)++;
	break;
    case 's':
	res = ei_x_encode_string(x, (*args)->s);
	(*args)++;
	break;
    case 'i':
	res = ei_x_encode_long(x, (*args)->l);
	(*args)++;
	break;
    case 'l':
	res = ei_x_encode_long(x, (*args)->l);
	(*args)++;
	break;
    case 'u':
	res = ei_x_encode_ulong(x, (*args)->u);
	(*args)++;
	break;
    case 'f':     /* float is expanded to double (C calling conventions) */
    case 'd':
	res = ei_x_encode_double(x, (*args)->d);
	(*args)++;
	break;	
    case 'p':
	res = ei_x_encode_pid(x, (*args)->pid);
	(*args)++;
	break;
    default:
	res = -1;
	break;
    }
    return res;
}

/* encode a tuple */
static int ptuple(const char** fmt, union arg** args, ei_x_buff* x, int size)
{
    int res = 0;
    const char* p = *fmt;
    char after = *p++;
    
    if (after == '}') {
	*fmt = p;
	return size;
    }
    while (isspace((int)*p))
	++p;
    switch (*p++) {
    case '}':
	if (after == ',')
	    res = -1;
	else
	    res = size;
	break;
    case ',':
	if (after == ',' || after == '{')
	    res = -1;
	else
	    res = ptuple(&p, args, x, size);
	break;
    default:
	--p;
	res = eiformat(&p, args, x);
	if (res >= 0)
	    res = ptuple(&p, args, x, size + 1);
	break;
	/*
	Variables
	*/
    }
    *fmt = p;
    return res;
}

/* encode a list */
static int plist(const char** fmt, union arg** args, ei_x_buff* x, int size)
{
    int res = 0;
    const char* p = *fmt;
    char after = *p++;

    if (after == ']')
	--p;
    while (isspace((int)*p))
	++p;
    switch (*p++) {
    case ']':
	if (after == ',')
	    res = -1;
	else {
	    if (after != '|')
		ei_x_encode_empty_list(x);
	    res = size;
	}
	break;
    case '|':
	if (after == '|' || after == ',')
	    res = -1;
	else
	    res = plist(&p, args, x, size);
	break;
    case ',':
	if (after == '|' || after == ',')
	    res = -1;
	else
	    res = plist(&p, args, x, size);
	break;
    default:
	--p;
	res = eiformat(&p, args, x);
	++size;
	if (res >= 0) {
	    if (after == '|') {
	        while (isspace((int)*p))
		    ++p;
		if (*p != ']')
		    res = -1;
	    } else
		res = plist(&p, args, x, size);
	}
	break;
	/*
	Variables
	*/
    }
    *fmt = p;
    return res;
}

static int read_args(const char* fmt, va_list ap, union arg **argp)
{
    const char* p = fmt;
    int arg_count = 0;
    union arg* args;
    int i = 0;

    /* Count the number of format strings. Assume null terminated string. */

    *argp = NULL;

    while (*p) if (*p++ == '~') arg_count++;


    if (!arg_count) {
	return 0;
    }
    /* Allocate space for the arguments */

    args = (union arg*)ei_malloc(arg_count * sizeof(union arg));

    if (!args) 
	return -1;

    p = fmt;			/* Start again and fill array */

    while (*p) {
      if (*p++ == '~') {
	if (!*p) {
	  ei_free(args);
	  return -1;	/* Error, string not complete */
	}
	switch (*p++) {
	case 'c':
	  args[i++].c = (char) va_arg(ap, int);
	  break;
	case 'a': 
	case 's':
	  args[i++].s = va_arg(ap, char*);
	  break;
	case 'i':
#ifdef EI_64BIT	  
	  args[i++].l = (long) va_arg(ap, int);
	  break;
#endif
	case 'l':
	  args[i++].l = va_arg(ap, long);
	  break;
	case 'u':
	  args[i++].u = va_arg(ap, unsigned long);
	  break;
	case 'f':     /* float is expanded to double (C calling conventions) */
	case 'd':
	  args[i++].d = va_arg(ap, double);
	  break;	
	case 'p':
	  args[i++].pid = va_arg(ap, erlang_pid*);
	  break;
	default:
	  ei_free(args);	/* Invalid specifier */
	  return -1;
	}
      }
    }
    *argp = args;
    return 0;
}
       
int ei_x_format(ei_x_buff* x, const char* fmt, ... )
{
    va_list ap;
    union arg* args;
    union arg* saved_args; 
    int res;

    res = ei_x_encode_version(x);
    if (res < 0) return res;

    va_start(ap, fmt);
    res = read_args(fmt,ap,&args);
    saved_args = args;
    va_end(ap);
    if (res < 0) {
	return -1;
    }

    res = eiformat(&fmt, &args, x);
    ei_free(saved_args);

    return res;
}

int ei_x_format_wo_ver(ei_x_buff* x, const char* fmt, ... )
{
    va_list ap;
    union arg* args; 
    union arg* saved_args; 
    int res;

    va_start(ap, fmt);
    res = read_args(fmt,ap,&args);
    saved_args = args;
    va_end(ap);
    if (res < 0) {
	return -1;
    }
    res = eiformat(&fmt, &args, x);
    ei_free(saved_args);

    return res;
}
