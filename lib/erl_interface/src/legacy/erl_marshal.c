/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
 * Purpose: Decoding and encoding Erlang terms.
 */  
#include "eidef.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <string.h>
#include <limits.h>

#include "erl_interface.h"
#include "erl_marshal.h"
#include "erl_eterm.h"
#include "erl_malloc.h"
#include "erl_error.h"
#include "erl_internal.h"

#include "eiext.h" /* replaces external.h */
#include "putget.h"

static int is_string(ETERM* term);
#if defined(VXWORKS) && CPU == PPC860
int erl_fp_compare(unsigned *a, unsigned *b);
static void erl_long_to_fp(long l, unsigned *d);
#endif

static int cmpbytes(unsigned char* s1,int l1,unsigned char* s2,int l2);
static int cmpatoms(unsigned char* s1, int l1, unsigned char tag1, unsigned char* s2, int l2, unsigned char tag2);

/* Used when comparing two encoded byte arrays */
/* this global data is ok (from threading point of view) since it is
 * initialized once and never changed
 */

#define CMP_ARRAY_SIZE 256
/* FIXME problem for threaded ? */

static enum
{
    ERL_NUM_CMP=1, ERL_ATOM_CMP, ERL_REF_CMP, ERL_FUN_CMP, ERL_PORT_CMP,
    ERL_PID_CMP, ERL_TUPLE_CMP, ERL_NIL_CMP, ERL_LIST_CMP, ERL_BIN_CMP
}cmp_array[CMP_ARRAY_SIZE];

static int init_cmp_array_p=1; /* initialize array, the first time */

#if defined(VXWORKS) && CPU == PPC860
#include <limits.h>
#endif

#if defined(__GNUC__)
#  define INLINE __inline__
#elif defined(__WIN32__)
#  define INLINE __inline
#else
#  define INLINE
#endif

static int cmp_floats(double f1, double f2);
static INLINE double to_float(long l);

#define IS_ERL_NUM(t) (cmp_array[t]==ERL_NUM_CMP)
#define IS_ERL_ATOM(t) (cmp_array[t]==ERL_ATOM_CMP)

#define CMP_NUM_CLASS_SIZE 256
static unsigned char cmp_num_class[CMP_NUM_CLASS_SIZE]; 
static int init_cmp_num_class_p=1; /* initialize array, the first time */

#define MK_CMP_NUM_CODE(x,y)    (((x)<<2)|(y))
#define CMP_NUM_CLASS(x)        (cmp_num_class[x] & 0x03)
#define CMP_NUM_CODE(x,y)       (MK_CMP_NUM_CODE(CMP_NUM_CLASS(x),CMP_NUM_CLASS(y)))

#define SMALL 1
#define FLOAT 2
#define BIG   3

#define SMALL_SMALL    MK_CMP_NUM_CODE(SMALL,SMALL)
#define SMALL_FLOAT    MK_CMP_NUM_CODE(SMALL,FLOAT)
#define SMALL_BIG      MK_CMP_NUM_CODE(SMALL,BIG)
#define FLOAT_SMALL    MK_CMP_NUM_CODE(FLOAT,SMALL)
#define FLOAT_FLOAT    MK_CMP_NUM_CODE(FLOAT,FLOAT)
#define FLOAT_BIG      MK_CMP_NUM_CODE(FLOAT,BIG)
#define BIG_SMALL      MK_CMP_NUM_CODE(BIG,SMALL)
#define BIG_FLOAT      MK_CMP_NUM_CODE(BIG,FLOAT)
#define BIG_BIG        MK_CMP_NUM_CODE(BIG,BIG)

void erl_init_marshal(void)
{
  if (init_cmp_array_p) {
    memset(cmp_array, 0, CMP_ARRAY_SIZE);
    cmp_array[ERL_SMALL_INTEGER_EXT] = ERL_NUM_CMP;
    cmp_array[ERL_INTEGER_EXT]       = ERL_NUM_CMP;
    cmp_array[ERL_FLOAT_EXT]         = ERL_NUM_CMP;
    cmp_array[NEW_FLOAT_EXT]         = ERL_NUM_CMP;
    cmp_array[ERL_SMALL_BIG_EXT]     = ERL_NUM_CMP;
    cmp_array[ERL_LARGE_BIG_EXT]     = ERL_NUM_CMP;
    cmp_array[ERL_ATOM_EXT]          = ERL_ATOM_CMP;
    cmp_array[ERL_ATOM_UTF8_EXT]     = ERL_ATOM_CMP;
    cmp_array[ERL_SMALL_ATOM_EXT]    = ERL_ATOM_CMP;
    cmp_array[ERL_SMALL_ATOM_UTF8_EXT] = ERL_ATOM_CMP;
    cmp_array[ERL_REFERENCE_EXT]     = ERL_REF_CMP;
    cmp_array[ERL_NEW_REFERENCE_EXT] = ERL_REF_CMP;
    cmp_array[ERL_NEWER_REFERENCE_EXT]=ERL_REF_CMP;
    cmp_array[ERL_FUN_EXT]           = ERL_FUN_CMP;
    cmp_array[ERL_NEW_FUN_EXT]       = ERL_FUN_CMP;
    cmp_array[ERL_PORT_EXT]          = ERL_PORT_CMP;
    cmp_array[ERL_NEW_PORT_EXT]      = ERL_PORT_CMP;
    cmp_array[ERL_PID_EXT]           = ERL_PID_CMP;
    cmp_array[ERL_NEW_PID_EXT]       = ERL_PID_CMP;
    cmp_array[ERL_SMALL_TUPLE_EXT]   = ERL_TUPLE_CMP;
    cmp_array[ERL_LARGE_TUPLE_EXT]   = ERL_TUPLE_CMP;
    cmp_array[ERL_NIL_EXT]           = ERL_NIL_CMP;
    cmp_array[ERL_STRING_EXT]        = ERL_LIST_CMP;
    cmp_array[ERL_LIST_EXT]          = ERL_LIST_CMP;
    cmp_array[ERL_BINARY_EXT]        = ERL_BIN_CMP;
    init_cmp_array_p = 0;
  }
  if (init_cmp_num_class_p) {
    memset(cmp_num_class, 0, CMP_NUM_CLASS_SIZE);
    cmp_num_class[ERL_SMALL_INTEGER_EXT] = SMALL;
    cmp_num_class[ERL_INTEGER_EXT]       = SMALL;
    cmp_num_class[ERL_FLOAT_EXT]         = FLOAT;
    cmp_num_class[NEW_FLOAT_EXT]         = FLOAT;
    cmp_num_class[ERL_SMALL_BIG_EXT]     = BIG;
    cmp_num_class[ERL_LARGE_BIG_EXT]     = BIG;
    init_cmp_num_class_p = 0;
  }
}

/* The encoder calls length, if erl_length() should return */
/* -1 for dotted pairs (why !!!!) we can't use erl_length() */
/* from the encoder in erl_marshal.c */
 
static int erl_length_x(const ETERM *ep) {
    int n = 0;

    if (!ep) return -1;

    while (ERL_TYPE(ep) == ERL_LIST) {
       n++;
       ep = TAIL(ep);
    }

    return n;
}


/*==============================================================
 * Marshalling routines.
 *==============================================================
 */

static void encode_atom(Erl_Atom_data* a, unsigned char **ext)
{
    int ix = 0;
    if (a->latin1) {
	ei_encode_atom_len_as((char*)*ext, &ix, a->latin1, a->lenL,
			      ERLANG_LATIN1, ERLANG_UTF8);
    }
    else {
	ei_encode_atom_len_as((char*)*ext, &ix, a->utf8, a->lenU,
			      ERLANG_UTF8, ERLANG_UTF8);
    }
    *ext += ix;
}

/* 
 * The actual ENCODE engine.
 * Returns 0 on success, otherwise 1.
 */
int erl_encode_it(ETERM *ep, unsigned char **ext, int dist)
{
    int i;
    unsigned int u;
    long long l;
    unsigned long long ul;
    
    switch(ERL_TYPE(ep)) 
    {
    case ERL_ATOM:
	encode_atom(&ep->uval.aval.d, ext);
	return 0;

    case ERL_INTEGER:
	i = ep->uval.ival.i;	
	/* SMALL_INTEGER */
	if ((i < 256) && (i >= 0)) {
	    *(*ext)++ = ERL_SMALL_INTEGER_EXT;
	    *(*ext)++ = i & 0xff;
	    return 0;
	}
	/* R14B: Use all 32 bits of INTEGER_EXT */
	*(*ext)++ = ERL_INTEGER_EXT;
	*(*ext)++ = (i >> 24) & 0xff;
	*(*ext)++ = (i >> 16) & 0xff;
	*(*ext)++ = (i >> 8) & 0xff;
	*(*ext)++ = i  & 0xff;
	return 0;

    case ERL_U_INTEGER:
	u = ep->uval.uival.u;
	/* ERL_U_SMALL_BIG */
	if ((int)u < 0) {
	    *(*ext)++ = ERL_SMALL_BIG_EXT;
	    *(*ext)++ = 4;		/* four bytes */
	    *(*ext)++ = 0;		/* sign byte  */ 
	    *(*ext)++ = u  & 0xff;	/* LSB first  */
	    *(*ext)++ = (u >> 8) & 0xff;
	    *(*ext)++ = (u >> 16) & 0xff;
	    *(*ext)++ = (u >> 24) & 0xff; 
	    return 0;
	}
	/* SMALL_INTEGER */
	if (u < 256) {
	    *(*ext)++ = ERL_SMALL_INTEGER_EXT;
	    *(*ext)++ = u & 0xff;
	    return 0;
	}
	/* R14B: Use all 32 bits of INTEGER_EXT */
	*(*ext)++ = ERL_INTEGER_EXT;
	*(*ext)++ = (u >> 24) & 0xff;
	*(*ext)++ = (u >> 16) & 0xff;
	*(*ext)++ = (u >> 8) & 0xff;
	*(*ext)++ = u  & 0xff;
	return 0;
    case ERL_LONGLONG:
	l = ep->uval.llval.i;
	/* ERL_SMALL_BIG */
	if (l > ((long long) INT_MAX) || l < ((long long) INT_MIN)) {
	    *(*ext)++ = ERL_SMALL_BIG_EXT;
	    *(*ext)++ = 8;
	    if ((*(*ext)++ = (l<0))) /* sign byte  */ 
	    	l = -l;
	    *(*ext)++ =  l        & 0xff; /* LSB first  */
	    *(*ext)++ = (l >>  8) & 0xff;
	    *(*ext)++ = (l >> 16) & 0xff;
	    *(*ext)++ = (l >> 24) & 0xff; 
	    *(*ext)++ = (l >> 32) & 0xff; 
	    *(*ext)++ = (l >> 40) & 0xff; 
	    *(*ext)++ = (l >> 48) & 0xff; 
	    *(*ext)++ = (l >> 56) & 0xff; 
	    return 0;
	} 
	/* SMALL_INTEGER */
	if ((l < 256) && (l >= 0)) {
	    *(*ext)++ = ERL_SMALL_INTEGER_EXT;
	    *(*ext)++ = l & 0xff;
	    return 0;
	}
	/* R14B: Use all 32 bits of INTEGER_EXT */
	*(*ext)++ = ERL_INTEGER_EXT;
	*(*ext)++ = (l >> 24) & 0xff;
	*(*ext)++ = (l >> 16) & 0xff;
	*(*ext)++ = (l >>  8) & 0xff;
	*(*ext)++ = l  & 0xff;
	return 0;

    case ERL_U_LONGLONG:
	ul = ep->uval.ullval.u;
	/* ERL_U_SMALL_BIG */
	if (ul > ((unsigned long long) INT_MAX)) {
	    *(*ext)++ = ERL_SMALL_BIG_EXT;
	    *(*ext)++ = 8; /* eight bytes */
	    *(*ext)++ = 0; /* sign byte  */ 
	    *(*ext)++ =  ul        & 0xff; /* LSB first  */
	    *(*ext)++ = (ul >>  8) & 0xff;
	    *(*ext)++ = (ul >> 16) & 0xff;
	    *(*ext)++ = (ul >> 24) & 0xff; 
	    *(*ext)++ = (ul >> 32) & 0xff; 
	    *(*ext)++ = (ul >> 40) & 0xff; 
	    *(*ext)++ = (ul >> 48) & 0xff; 
	    *(*ext)++ = (ul >> 56) & 0xff; 
	    return 0;
	}
	/* SMALL_INTEGER */
	if (ul < 256) {
	    *(*ext)++ = ERL_SMALL_INTEGER_EXT;
	    *(*ext)++ = ul & 0xff;
	    return 0;
	}
	/* R14B: Use all 32 bits of INTEGER_EXT */
	*(*ext)++ = ERL_INTEGER_EXT;
	*(*ext)++ = (ul >> 24) & 0xff;
	*(*ext)++ = (ul >> 16) & 0xff;
	*(*ext)++ = (ul >>  8) & 0xff;
	*(*ext)++ =  ul        & 0xff;
	return 0;

    case ERL_PID: {
        unsigned char* tagp = (*ext)++;
	/* First poke in node as an atom */    
	encode_atom(&ep->uval.pidval.node, ext);
	/* And then fill in the integer fields */
	i = ERL_PID_NUMBER(ep);
	*(*ext)++ = (i >> 24) &0xff;
	*(*ext)++ = (i >> 16) &0xff;
	*(*ext)++ = (i >>  8) &0xff;
	*(*ext)++ = i &0xff;
	i = ERL_PID_SERIAL(ep);
	*(*ext)++ = (i >> 24) &0xff;
	*(*ext)++ = (i >> 16) &0xff;
	*(*ext)++ = (i >>  8) &0xff;
	*(*ext)++ = i &0xff;

        i = ERL_PID_CREATION(ep);
        if ((unsigned int)i <= 3) {
            *tagp = ERL_PID_EXT;
            *(*ext)++ = i;
        } else {
            *tagp = ERL_NEW_PID_EXT;
            *(*ext)++ = (i >> 24) &0xff;
            *(*ext)++ = (i >> 16) &0xff;
            *(*ext)++ = (i >>  8) &0xff;
            *(*ext)++ = i &0xff;
        }
	return 0;
    }
    case ERL_REF: {
            unsigned char* tagp = (*ext)++;

	    int len, j;

	    /* Always encode as an extended reference; all
	       participating parties are now expected to be
	       able to decode extended references. */

	    i = strlen((char *)ERL_REF_NODE(ep));
	    len = ERL_REF_LEN(ep);
	    *(*ext)++ = (len >> 8) &0xff;
	    *(*ext)++ = len &0xff;

	    encode_atom(&ep->uval.refval.node, ext);

            i = ERL_REF_CREATION(ep);
            if ((unsigned int)i <= 3) {
                *tagp = ERL_NEW_REFERENCE_EXT;
                *(*ext)++ = i;
            } else {
                *tagp = ERL_NEWER_REFERENCE_EXT;
                *(*ext)++ = (i >> 24) &0xff;
                *(*ext)++ = (i >> 16) &0xff;
                *(*ext)++ = (i >>  8) &0xff;
                *(*ext)++ = i &0xff;
            }

	    /* Then the integer fields */
	    for (j = 0; j < ERL_REF_LEN(ep); j++) {
		i = ERL_REF_NUMBERS(ep)[j];
		*(*ext)++ = (i >> 24) &0xff;
		*(*ext)++ = (i >> 16) &0xff;
		*(*ext)++ = (i >>  8) &0xff;
		*(*ext)++ = i &0xff;
	    }
	}
	return 0;
    case ERL_PORT: {
	unsigned char* tagp = (*ext)++;
	/* First poke in node as an atom */
	encode_atom(&ep->uval.portval.node, ext);
	/* Then the integer fields */
	i = ERL_PORT_NUMBER(ep);
	*(*ext)++ = (i >> 24) &0xff;
	*(*ext)++ = (i >> 16) &0xff;
	*(*ext)++ = (i >>  8) &0xff;
	*(*ext)++ = i &0xff;

        i = ERL_PORT_CREATION(ep);
        if ((unsigned int)i <= 3) {
            *tagp = ERL_PORT_EXT;
            *(*ext)++ = i;
        } else {
            *tagp = ERL_NEW_PORT_EXT;
            *(*ext)++ = (i >> 24) &0xff;
            *(*ext)++ = (i >> 16) &0xff;
            *(*ext)++ = (i >>  8) &0xff;
            *(*ext)++ = i &0xff;
        }
	return 0;
    }
    case ERL_EMPTY_LIST:
	*(*ext)++ = ERL_NIL_EXT;
	break;
    case ERL_LIST:
	i = is_string(ep);
	if (0 < i && i < 0x10000) { /* String. */
	    *(*ext)++ = ERL_STRING_EXT;
	    *(*ext)++ = (i >>8) &0xff;
	    *(*ext)++ = i &0xff;
	    while (ERL_TYPE(ep) == ERL_LIST) {
		*(*ext)++ = HEAD(ep)->uval.ival.i;
		ep = TAIL(ep);
	    }
	    break;
	} else {		/* List. */
	    i = erl_length_x(ep);
	    *(*ext)++ = ERL_LIST_EXT;
	    *(*ext)++ = (i >> 24) &0xff;
	    *(*ext)++ = (i >> 16) &0xff;
	    *(*ext)++ = (i >>  8) &0xff;
	    *(*ext)++ = i &0xff;
	    while (ERL_TYPE(ep) == ERL_LIST) {
		if (erl_encode_it(HEAD(ep), ext, dist))
		    return 1;
		ep = TAIL(ep);
	    }
	    i = erl_encode_it(ep, ext, dist);
	    return i;
	}
    case ERL_TUPLE:
	i = ep->uval.tval.size;
	if (i <= 0xff) {
	    *(*ext)++ = ERL_SMALL_TUPLE_EXT;
	    *(*ext)++ = i & 0xff;
	}
	else {
	    *(*ext)++ = ERL_LARGE_TUPLE_EXT;
	    *(*ext)++ = (i >> 24) & 0xff;
	    *(*ext)++ = (i >> 16) & 0xff;
	    *(*ext)++ = (i >>  8) & 0xff;
	    *(*ext)++ = i & 0xff;
	}
	for (i=0; i<ep->uval.tval.size; i++)
	    if (erl_encode_it(ep->uval.tval.elems[i], ext, dist))
		return 1;
	break;
    case ERL_FLOAT:
	*(*ext)++ = ERL_FLOAT_EXT;
	memset(*ext, 0, 31);
	sprintf((char *) *ext, "%.20e", ep->uval.fval.f);
	*ext += 31;
	break;
    case ERL_BINARY:
	*(*ext)++ = ERL_BINARY_EXT;
	i = ep->uval.bval.size;
	*(*ext)++ = (i >> 24) & 0xff;
	*(*ext)++ = (i >> 16) & 0xff;
	*(*ext)++ = (i >>  8) & 0xff;
	*(*ext)++ = i  & 0xff;
	memcpy((char *) *ext, (char*) ep->uval.bval.b, i);
	*ext += i;
	break;
    case ERL_FUNCTION:
	if (ERL_FUN_ARITY(ep) != -1) {
	    unsigned char *size_p = *ext + 1;
	    *(*ext)++ = ERL_NEW_FUN_EXT;
	    *ext += 4;
	    i = ERL_FUN_ARITY(ep);
	    put8(*ext, i);
	    memcpy(*ext, ERL_FUN_MD5(ep), 16);
	    *ext += 16;
	    i = ERL_FUN_NEW_INDEX(ep);
	    put32be(*ext, i);
	    i = ERL_CLOSURE_SIZE(ep);
	    put32be(*ext, i);
	    erl_encode_it(ERL_FUN_MODULE(ep), ext, dist);
	    erl_encode_it(ERL_FUN_INDEX(ep), ext, dist);
	    erl_encode_it(ERL_FUN_UNIQ(ep), ext, dist);
	    erl_encode_it(ERL_FUN_CREATOR(ep), ext, dist);
	    for (i = 0; i < ERL_CLOSURE_SIZE(ep); i++)
		erl_encode_it(ep->uval.funcval.closure[i], ext, dist);
	    if (size_p != NULL) {
		i = *ext - size_p;
		put32be(size_p, i);
	    }
	} else {
	    *(*ext)++ = ERL_FUN_EXT;
	    i = ERL_CLOSURE_SIZE(ep);
	    *(*ext)++ = (i >> 24) & 0xff;
	    *(*ext)++ = (i >> 16) & 0xff;
	    *(*ext)++ = (i >>  8) & 0xff;
	    *(*ext)++ = i  & 0xff;
	    erl_encode_it(ERL_FUN_CREATOR(ep), ext, dist);
	    erl_encode_it(ERL_FUN_MODULE(ep), ext, dist);
	    erl_encode_it(ERL_FUN_INDEX(ep), ext, dist);
	    erl_encode_it(ERL_FUN_UNIQ(ep), ext, dist);
	    for (i = 0; i < ERL_CLOSURE_SIZE(ep); i++)
		erl_encode_it(ep->uval.funcval.closure[i], ext, dist);
	}
	break;
    default:
	return 1;
    }
    return 0;
}

/* 
 * ENCODE an ETERM into a BUFFER, assuming BUFFER is of 
 * enough size. At success return number of bytes written 
 * into it, otherwise return 0.
 */
static int erl_encode3(ETERM *ep, unsigned char *t, int dist)
{
  unsigned char *x = t;
  
  *x++ = ERL_VERSION_MAGIC;
  if (erl_encode_it(ep, &x, dist)) {
#ifdef DEBUG
    erl_err_msg("<ERROR> erl_encode: Error while encoding");
#endif
    return 0;
  }
  return (x - t);

}

/* API */

int erl_encode(ETERM *ep, unsigned char *t)
{
    return erl_encode3(ep, t, 4);
}

/* determine the buffer size that will be required for the eterm */
static int erl_term_len_helper(ETERM *ep, int dist);

/* FIXME hard coded dist version */
int erl_term_len(ETERM *ep)
{
  return 1+erl_term_len_helper(ep, 4);
}

static int atom_len_helper(Erl_Atom_data* a)
{
    (void) erl_atom_ptr_utf8(a);
    return 1 + 1 + (a->lenU > 255) + a->lenU;
}

static int erl_term_len_helper(ETERM *ep, int dist)
{
  int len = 0;
  int i;
  unsigned int u;
  long long l;
  unsigned long long ul;

  if (ep) {
    switch (ERL_TYPE(ep)) {
    case ERL_ATOM:
      len = atom_len_helper(&ep->uval.aval.d);
      break;

    case ERL_INTEGER:
      i = ep->uval.ival.i;
      if ((i < 256) && (i >= 0)) len = 2; 
      else len = 5;
      break;

    case ERL_U_INTEGER:
      u = ep->uval.uival.u;
      if ((int)u < 0) len = 7;
      else if (u  < 256) len = 2;
      else len = 5;
      break;

    case ERL_LONGLONG:
      l = ep->uval.llval.i;
      if ((l > ((long long) INT_MAX)) || 
         (l < ((long long) INT_MIN))) len = 11;
      else if ((l < 256) && (l >= 0)) len = 2; 
      else len = 5;
      break;

    case ERL_U_LONGLONG:
      ul = ep->uval.ullval.u;
      if (ul > ((unsigned long long) INT_MAX)) len = 11;
      else if (ul  < 256) len = 2;
      else len = 5;
      break;

    case ERL_PID:
      len = 1 + atom_len_helper(&ep->uval.pidval.node) + 4 + 4 + 1;
      break;

    case ERL_REF:
      len = 1 + 2 + atom_len_helper(&ep->uval.refval.node) + 1 + ERL_REF_LEN(ep) * 4;
      break;

    case ERL_PORT:
      len = 1 + atom_len_helper(&ep->uval.portval.node) + 4 + 1;
      break;

    case ERL_EMPTY_LIST:
      len = 1;
      break;

    case ERL_LIST:
      i = is_string(ep);
      if ((i > 0) && (i < 0x10000)) { /* string: 3 + strlen */
	for (len = 3; ERL_TYPE(ep) == ERL_LIST; ep =  TAIL(ep)) {
	  len++;
	}
      }
      else { /* list: 5 + len(elem1) + len(elem2) ... */
	for (len = 5; ERL_TYPE(ep) == ERL_LIST; ep =  TAIL(ep)) {
	  len += erl_term_len_helper(HEAD(ep), dist);
	}
	len += erl_term_len_helper(ep, dist); /* last element */
      }
      break;

    case ERL_TUPLE:
      /* (2 or 5) + len(elem1) + len(elem2) ... */
      i = ep->uval.tval.size;
      if (i <= 0xff) len = 2;
      else len = 5;
      
      for (i=0; i<ep->uval.tval.size; i++) {
	len += erl_term_len_helper(ep->uval.tval.elems[i], dist);
      }
      break;

    case ERL_FLOAT:
      len = 32;
      break;

    case ERL_BINARY:
      i = ep->uval.bval.size;
      len = 5 + i;
      break;

    case ERL_FUNCTION:
      if (ERL_FUN_ARITY(ep) == -1) {
	  len = 1 + 4;
	  len += erl_term_len_helper(ERL_FUN_CREATOR(ep),dist);
	  len += erl_term_len_helper(ERL_FUN_MODULE(ep),dist);
	  len += erl_term_len_helper(ERL_FUN_INDEX(ep),dist);
	  len += erl_term_len_helper(ERL_FUN_UNIQ(ep),dist);
	  for (i = 0; i < ERL_CLOSURE_SIZE(ep); i++)
	      len += erl_term_len_helper(ERL_CLOSURE_ELEMENT(ep,i), dist);
      } else {
	  len = 1 + 4 + 16 + 4 + 4;
	  len += erl_term_len_helper(ERL_FUN_MODULE(ep),dist);
	  len += erl_term_len_helper(ERL_FUN_INDEX(ep),dist);
	  len += erl_term_len_helper(ERL_FUN_UNIQ(ep),dist);
	  len += erl_term_len_helper(ERL_FUN_CREATOR(ep),dist);
	  for (i = 0; i < ERL_CLOSURE_SIZE(ep); i++)
	      len += erl_term_len_helper(ERL_CLOSURE_ELEMENT(ep,i), dist);
      }
      break;

    default:
#ifdef DEBUG
	fprintf(stderr, "Shouldn't happen: erl_term_len, unknown term type: '%c'\n",ERL_TYPE(ep));
#endif
      erl_errno = EINVAL;
      exit(1);
    }
  }

  return len;
}

/* 
 * This one makes it easy to ENCODE several CONSECUTIVE
 * ETERM's into the same buffer. 
 */
int erl_encode_buf(ETERM *ep, unsigned char **ext)
{
  unsigned char *start=*ext;
  
  *(*ext)++ = ERL_VERSION_MAGIC;
  if (erl_encode_it(ep, ext, 0)) {
#ifdef DEBUG
    erl_err_msg("<ERROR> erl_encode_buf: Error while encoding\n");
#endif
    return 0;
  }
  return (*ext - start);

} /* erl_encode_buf */


static int read_atom(unsigned char** ext, Erl_Atom_data* a)
{
    char buf[MAXATOMLEN_UTF8];
    int offs = 0;
    erlang_char_encoding enc;
    int ret = ei_decode_atom_as((char*)*ext, &offs, buf, MAXATOMLEN_UTF8,
				ERLANG_LATIN1|ERLANG_UTF8, NULL, &enc);
    *ext += offs;

    if (ret == 0) {
	int i = strlen(buf);
	char* clone = erl_malloc(i+1);
	memcpy(clone, buf, i+1);

	a->latin1 = NULL; 	    
	a->lenL = 0;
	a->utf8 = NULL;
	a->lenU = 0;
	if (enc & (ERLANG_LATIN1 | ERLANG_ASCII)) {
	    a->latin1 = clone; 	    
	    a->lenL = i;
	}
	if (enc & (ERLANG_UTF8 | ERLANG_ASCII)) {
	    a->utf8 = clone;
	    a->lenU = i;
	}
    }
    return ret;
}

/*
 * The actual DECODE engine.
 * Returns NULL in case of failure.
 */
static ETERM *erl_decode_it(unsigned char **ext)
{
    char *cp;
    ETERM *ep,*tp,*np;
    unsigned int u,sign;
    int i,j,arity;
    double ff;
    unsigned char tag;
    
    /* Assume we are going to decode an integer */
    ep = erl_alloc_eterm(ERL_INTEGER);
    ERL_COUNT(ep) = 1;

    tag = *(*ext)++;
    switch (tag)
    {
    case ERL_INTEGER_EXT:
	i = (int) (**ext << 24) | ((*ext)[1] << 16) |
	    ((*ext)[2] << 8) | (*ext)[3];
	*ext += 4;
	ep->uval.ival.i = i;
	return ep;

    case ERL_SMALL_INTEGER_EXT:
	i = *(*ext)++;
	ep->uval.ival.i = i;
	return ep;

        /* NOTE: The arity below for bigs is not really the arity (= number of digits) */
        /*       It is the byte count and this might cause problems in other parts...  */
    case ERL_SMALL_BIG_EXT:
        arity = *(*ext)++; 
	goto big_cont;
    case ERL_LARGE_BIG_EXT:
	arity = (**ext << 24) | ((*ext)[1])<< 16 | 
	    ((*ext)[2]) << 8 |((*ext)[3]); 
	*ext += 4;
    big_cont:

#ifdef _MSC_VER
#define MAX_TO_NEGATE 0x8000000000000000Ui64
#else
#define MAX_TO_NEGATE 0x8000000000000000ULL
#endif

	sign = *(*ext)++; 
	if (arity > 8)             
	    goto big_truncate;

	if (arity == 4 && ((*ext)[3] & 0x80) && !sign) {
	    /* It will fit into an unsigned int !! */
	    u = (((*ext)[3] << 24)|((*ext)[2])<< 16|((*ext)[1]) << 8 |(**ext));
	    ERL_TYPE(ep) = ERL_U_INTEGER;
	    ep->uval.uival.u = u;
	    /* *ext += i; */
	    *ext += arity;
	    return ep;
	} else if (arity == 4 && !((*ext)[3] & 0x80)) {
	    /* It will fit into an int !! 
	     */
	    i = (int) (((*ext)[3] << 24) | ((*ext)[2])<< 16 |
		       ((*ext)[1]) << 8 | (**ext));
	    if (sign) i = -i;
	    ERL_TYPE(ep) = ERL_INTEGER;
	    ep->uval.ival.i = i;
	    *ext += arity;
	    return ep;
	} else if (arity == 8 && ((*ext)[7] & 0x80) && !sign) {
	    /* Fits in an unsigned long long */
	    int x;
	    unsigned long long ul = 0LL;

	    for(x = 0 ; x < arity ; x++) {
		ul |= ((unsigned long long)(*ext)[x]) << ((unsigned long long)(8*x));
	    }
	   
	    ERL_TYPE(ep) = ERL_U_LONGLONG;
	    ep->uval.ullval.u = ul;
	    *ext += arity;
	    return ep;
	} else {
            /* Fits in a signed long long */
            int x;
            unsigned long long l = 0LL;
            long long sl;

            for(x = 0 ; x < arity ; x++) {
                l |= ((unsigned long long)(*ext)[x]) << ((unsigned long long)(8*x));
            }

            sl = (long long)l;

            if (sign && l != MAX_TO_NEGATE) {
                sl = -sl;
                if (sl > 0) goto big_truncate;
            }

            ERL_TYPE(ep) = ERL_LONGLONG;
            ep->uval.llval.i = sl;
            *ext += arity;
            return ep;
	}
#undef MAX_TO_NEGATE
    big_truncate: 
	/* truncate to: (+/-) 1 */
#ifdef DEBUG
	erl_err_msg("<WARNING> erl_decode_it: Integer truncated...");
#endif
	ERL_TYPE(ep) = ERL_INTEGER;
	ep->uval.ival.i = sign?-1:1;
	*ext += arity;
	return ep;
      
    case ERL_ATOM_EXT:
    case ERL_SMALL_ATOM_EXT:
    case ERL_ATOM_UTF8_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
	
	ERL_TYPE(ep) = ERL_ATOM;
	--(*ext);
	if (read_atom(ext, &ep->uval.aval.d) < 0) return NULL;
	return ep;
      
    case ERL_PID_EXT:
    case ERL_NEW_PID_EXT:
	{
	    unsigned int number, serial;
	    unsigned int creation;

	    ERL_TYPE(ep) = ERL_PID;
	    if (read_atom(ext, &ep->uval.pidval.node) < 0) return NULL;

	    /* get the integers */
	    number = ((*ext)[0] << 24) | ((*ext)[1]) << 16 | 
		((*ext)[2]) << 8 | ((*ext)[3]);	
	    *ext += 4;
	    serial = ((*ext)[0] << 24) | ((*ext)[1]) << 16 | 
		((*ext)[2]) << 8 | ((*ext)[3]);	
	    *ext += 4;
            if (tag == ERL_PID_EXT)
                creation =  *(*ext)++;
            else {
                creation = ((*ext)[0] << 24) | ((*ext)[1]) << 16 |
                    ((*ext)[2]) << 8 | ((*ext)[3]);
                *ext += 4;
            }
	    erl_mk_pid_helper(ep, number, serial, creation);
	    return ep;
	}
    case ERL_REFERENCE_EXT:
	{
	    unsigned int n[3] = {0, 0, 0};
	    unsigned char creation;

	    ERL_TYPE(ep) = ERL_REF;
	    if (read_atom(ext, &ep->uval.refval.node) < 0) return NULL;

	    /* get the integers */
	    n[0] = ((*ext)[0] << 24) | ((*ext)[1]) << 16 | 
		((*ext)[2]) << 8 | ((*ext)[3]);	
	    *ext += 4;
	    creation =  *(*ext)++; 
	    __erl_mk_reference(ep, NULL, 1, n, creation);
	    return ep;
	}

    case ERL_NEW_REFERENCE_EXT:
    case ERL_NEWER_REFERENCE_EXT:
	{
	    size_t cnt, i;
	    unsigned int n[3];
	    unsigned int creation;

	    ERL_TYPE(ep) = ERL_REF;
	    cnt = ((*ext)[0] << 8) | (*ext)[1];
	    *ext += 2;

	    if (read_atom(ext, &ep->uval.refval.node) < 0) return NULL;

	    /* get the integers */
            if (tag == ERL_NEW_REFERENCE_EXT)
                creation =  *(*ext)++;
            else {
                creation = ((*ext)[0] << 24) | ((*ext)[1]) << 16 |
                    ((*ext)[2]) << 8 | ((*ext)[3]);
                *ext += 4;
            }
	    for(i = 0; i < cnt; i++)
	    {
		n[i] = ((*ext)[0] << 24) | ((*ext)[1]) << 16 | 
		    ((*ext)[2]) << 8 | ((*ext)[3]);	
		*ext += 4;
	    }
	    __erl_mk_reference(ep, NULL, cnt, n, creation);
	    return ep;
	}

    case ERL_PORT_EXT:
    case ERL_NEW_PORT_EXT:
	{
	    unsigned int number;
	    unsigned int creation;

	    ERL_TYPE(ep) = ERL_PORT;
	    if (read_atom(ext, &ep->uval.portval.node) < 0) return NULL;

	    /* get the integers */
	    number = ((*ext)[0] << 24) | ((*ext)[1]) << 16 | 
		((*ext)[2]) << 8 | ((*ext)[3]);	
	    *ext += 4;
            if (tag == ERL_PORT_EXT)
                creation =  *(*ext)++;
            else {
                creation = (((*ext)[0] << 24) | ((*ext)[1]) << 16 |
                            ((*ext)[2]) << 8  | ((*ext)[3]));
                *ext += 4;
            }
	    erl_mk_port_helper(ep, number, creation);
	    return ep;
	}

    case ERL_NIL_EXT:
	ERL_TYPE(ep) = ERL_EMPTY_LIST;
	return ep;

    case ERL_LIST_EXT:
	ERL_TYPE(ep) = ERL_LIST;
	i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	*ext += 4;	
	/* ASSERT(i != 0);	*/	/* Should be represented by ERL_NIL_EXT. */
	tp = ep;
	for (j = 0; j < i; j++) 
	    if ((HEAD(tp) = erl_decode_it(ext)) == NULL) 
		goto failure;
	    else if (j + 1 < i) {
		/* We have to watch out for how we allocates the
		 * last tail element since we may encounter non-
		 * well formed lists.
		 */
		np = erl_alloc_eterm(ERL_LIST);
		ERL_COUNT(np) = 1;
                TAIL(np) = NULL; /* in case of failure */
		TAIL(tp) = np;
		tp = np;
	    }
	if ((TAIL(tp) = erl_decode_it(ext)) == NULL) 
	    goto failure;
	return ep;

    case ERL_STRING_EXT:
	{
	    unsigned char* s;
	  
	    ERL_TYPE(ep) = ERL_EMPTY_LIST;
	    i = (**ext << 8) | ((*ext)[1]);
	    *ext += 2;
	    s = *ext+i;

	    while (*ext < s) {
		ETERM* integer;
		ETERM* cons;

		integer = erl_alloc_eterm(ERL_INTEGER);
		ERL_COUNT(integer) = 1;
		integer->uval.ival.i = *--s;

		cons = erl_alloc_eterm(ERL_LIST);
		ERL_COUNT(cons) = 1;
		HEAD(cons) = integer;
		TAIL(cons) = ep;
		ep = cons;
	    }
	    *ext += i;
	    return ep;
	}

    case ERL_SMALL_TUPLE_EXT:
	ERL_TYPE(ep) = ERL_TUPLE;
	i = *(*ext)++;
	goto decode_tuple;

    case ERL_LARGE_TUPLE_EXT:
	i = (**ext << 24) | ((*ext)[1]) << 16 | 
	    ((*ext)[2]) << 8 | ((*ext)[3]) ;	
	*ext += 4;
    decode_tuple:
	ep->uval.tval.size = i;
	j = (i + 1) * sizeof(ETERM*);
	ep->uval.tval.elems = (ETERM**) erl_malloc(j);
	memset(ep->uval.tval.elems, 0, j); /* in case of failure below... */
	for (i=0; i<ep->uval.tval.size; i++)
	    if ((tp = erl_decode_it(ext)) == NULL)
		goto failure;
	    else
		ep->uval.tval.elems[i] = tp;
	return ep;

    case ERL_FLOAT_EXT:
    case NEW_FLOAT_EXT:
	ERL_TYPE(ep) = ERL_FLOAT;
	cp = (char *) *ext;
	i = -1;
	if (ei_decode_double(cp, &i, &ff) == -1)
	    goto failure;
	*ext += i;
	ep->uval.fval.f = ff;
	return ep;

    case ERL_BINARY_EXT:
	ERL_TYPE(ep) = ERL_BINARY;
	i = (**ext << 24) | ((*ext)[1] << 16) |
	    ((*ext)[2] << 8) | (*ext)[3];
	*ext += 4;
	ep->uval.bval.size = i;
	ep->uval.bval.b = (unsigned char *) erl_malloc(i);
	memcpy(ep->uval.bval.b, *ext, i);
	*ext += i;
	return ep;

    case ERL_FUN_EXT:		/* FIXME: error checking */
	ERL_TYPE(ep) = ERL_FUNCTION;
	i = get32be(*ext);
	/*i = *(**ext << 24) | ((*ext)[1] << 16) | ((*ext)[2] << 8) | (*ext)[3];
	 *ext += 4; */
	ERL_FUN_ARITY(ep) = -1;
	ERL_CLOSURE_SIZE(ep) = i;
	ERL_FUN_CREATOR(ep) = erl_decode_it(ext);
	ERL_FUN_MODULE(ep) = erl_decode_it(ext);
	ERL_FUN_INDEX(ep) = erl_decode_it(ext);
	ERL_FUN_UNIQ(ep) = erl_decode_it(ext);
	j = i * sizeof(ETERM*);
	ERL_CLOSURE(ep) = (ETERM**) erl_malloc(j);
	memset(ERL_CLOSURE(ep), 0, j);
	for (i = 0; i < ERL_CLOSURE_SIZE(ep); i++)
	    ERL_CLOSURE_ELEMENT(ep,i) = erl_decode_it(ext);
	return ep;

    case ERL_NEW_FUN_EXT:	/* FIXME: error checking */
	ERL_TYPE(ep) = ERL_FUNCTION;
	i = get32be(*ext);	/* size, we don't use it here */
	ERL_FUN_ARITY(ep) = get8(*ext);
	memcpy(ERL_FUN_MD5(ep), *ext, 16);
	*ext += 16;
	ERL_FUN_NEW_INDEX(ep) = get32be(*ext);
	i = get32be(*ext);
	ERL_CLOSURE_SIZE(ep) = i;
	ERL_FUN_MODULE(ep) = erl_decode_it(ext);
	ERL_FUN_INDEX(ep) = erl_decode_it(ext);
	ERL_FUN_UNIQ(ep) = erl_decode_it(ext);
	ERL_FUN_CREATOR(ep) = erl_decode_it(ext);
	j = i * sizeof(ETERM*);
	ERL_CLOSURE(ep) = (ETERM**) erl_malloc(j);
	memset(ERL_CLOSURE(ep), 0, j);
	for (i = 0; i < ERL_CLOSURE_SIZE(ep); i++)
	    ERL_CLOSURE_ELEMENT(ep,i) = erl_decode_it(ext);
	return ep;

    } /* switch */
    
 failure:
    erl_free_term(ep);
    return (ETERM *) NULL;
    
} /* erl_decode_it */

/*
 * DECODE a buffer of BYTES into an ETERM.
 * Returns NULL in case of failure.
 */
ETERM *erl_decode(unsigned char *t) 
{
  ETERM *ep;
  unsigned char *ext;

  ext = t;

  /* We ignore the version magic since it might be
   * possible that the buffer has been manipulated
   * with erl_peek_ext.
   */
  if (*ext == ERL_VERSION_MAGIC) 
    ext++;  

  ep = NULL;
  ep = erl_decode_it(&ext);
#ifdef DEBUG
  if (!ep) erl_err_msg("<ERROR> erl_decode: Error while decoding");
#endif
  return ep;

} /* erl_decode */

/* 
 * This one makes it possible to DECODE two CONSECUTIVE 
 * ETERM's in the same buffer. 
 */
ETERM *erl_decode_buf(unsigned char **ext) 
{
  ETERM *ep;
  
  /* We ignore the version magic since it might be
   * possible that the buffer has been manipulated
   * with erl_peek_ext.
   */
  if (**ext == ERL_VERSION_MAGIC) 
    (*ext)++;

  ep = NULL;
  ep = erl_decode_it(ext);
#ifdef DEBUG
    if (!ep) erl_err_msg("<ERROR> erl_decode_buf: Error while decoding");
#endif
  return ep;

} /* erl_decode_buf */


/*==============================================================
 * Ok, here comes routines for inspecting/manipulating 
 * an encoded buffer of bytes.
 *==============================================================
 */

/*
 * Return 1 if the VERSION MAGIC in the BUFFER is the
 * same as the this library version.
 */
int erl_verify_magic(unsigned char *ext)
{

  if (*ext == ERL_VERSION_MAGIC) 
    return 1;
  else
    return 0;

} /* erl_verify_magic */

/*
 * Return the TYPE of an ENCODED ETERM.
 * At failure, return 0.
 */ 
unsigned char erl_ext_type(unsigned char *ext)
{
    /* FIXME old code could skip multiple magic */

    /* Move over magic number if any */
    if (*ext == ERL_VERSION_MAGIC) ext++;
  
    switch (*ext) {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
	return ERL_INTEGER;
    case ERL_ATOM_EXT:
    case ERL_ATOM_UTF8_EXT:
    case ERL_SMALL_ATOM_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
	return ERL_ATOM;
    case ERL_PID_EXT:
    case ERL_NEW_PID_EXT:
	return ERL_PID;
    case ERL_PORT_EXT:
    case ERL_NEW_PORT_EXT:
	return ERL_PORT;
    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_NEWER_REFERENCE_EXT:
	return ERL_REF;
    case ERL_NIL_EXT: 
	return ERL_EMPTY_LIST;
    case ERL_LIST_EXT:
	return ERL_LIST;
    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
	return ERL_TUPLE;
    case ERL_FLOAT_EXT:
    case NEW_FLOAT_EXT:
	return ERL_FLOAT;
    case ERL_BINARY_EXT:
	return ERL_BINARY;
    case ERL_FUN_EXT:
    case ERL_NEW_FUN_EXT:
	return ERL_FUNCTION;
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
        return ERL_BIG;
    default:
	return 0;

    } /* switch */

} /* erl_ext_type */

/* 
 * Returns the number of elements in compund
 * terms. For other kind of terms zero is returned.
 * At failure -1 is returned.
 */
int erl_ext_size(unsigned char *t)
{
    int i;
    unsigned char *v;

    if (*t == ERL_VERSION_MAGIC) 
	return erl_ext_size(t+1);
 
    v = t+1;
    switch(*t) {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
    case ERL_ATOM_EXT:
    case ERL_ATOM_UTF8_EXT:
    case ERL_SMALL_ATOM_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
    case ERL_PID_EXT:
    case ERL_NEW_PID_EXT:
    case ERL_PORT_EXT:
    case ERL_NEW_PORT_EXT:
    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_NEWER_REFERENCE_EXT:
    case ERL_NIL_EXT: 
    case ERL_BINARY_EXT:
    case ERL_STRING_EXT:
    case ERL_FLOAT_EXT:
    case NEW_FLOAT_EXT:
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
	return 0;
	break;
    case ERL_SMALL_TUPLE_EXT:
	i = v[0];
	return i;
	break;
    case ERL_LIST_EXT:
    case ERL_LARGE_TUPLE_EXT:
	i = (v[0] << 24) | (v[1] << 16) | (v[2] << 8) | v[3];
	return i;
	break;
    case ERL_FUN_EXT:
	i = (v[0] << 24) | (v[1] << 16) | (v[2] << 8) | v[3];
	return i+4;
	break;
    case ERL_NEW_FUN_EXT:
        v += 4 + 1 + 16 + 4;
	i = get32be(v);
	return i + 4;
	break;
    default:
	return -1;
	break;
    } /* switch */

} /* ext_size */


static int jump_atom(unsigned char** ext)
{
    unsigned char* e = *ext;
    int len;

    switch (*e++) {
    case ERL_ATOM_EXT:
    case ERL_ATOM_UTF8_EXT:
	len = (e[0] << 8) | e[1];
	e += (len + 2);
	break;

    case ERL_SMALL_ATOM_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
	len = e[0];
	e += (len + 1);
	break;

    default:
	return 0;
    }
    *ext = e;
    return 1;
}


/*
 * MOVE the POINTER PAST the ENCODED ETERM we
 * are currently pointing at. Returns 1 at
 * success, otherwise 0.
 */
static int jump(unsigned char **ext) 
{
    int j,k,i=0;
    int n;
    const int tag = *(*ext)++;
    
    switch (tag) {
    case ERL_VERSION_MAGIC:
	return jump(ext);
    case ERL_INTEGER_EXT:
	*ext += 4;
	break;
    case ERL_SMALL_INTEGER_EXT:
	*ext += 1;
	break;
    case ERL_ATOM_EXT:
    case ERL_ATOM_UTF8_EXT:
    case ERL_SMALL_ATOM_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
	jump_atom(ext);
	break;
    case ERL_PID_EXT:
	if (!jump_atom(ext)) return 0;
	*ext += 4 + 4 + 1;
	break;
    case ERL_NEW_PID_EXT:
        if (!jump_atom(ext)) return 0;
        *ext += 4 + 4 + 4;
        break;
    case ERL_REFERENCE_EXT:
    case ERL_PORT_EXT:
	if (!jump_atom(ext)) return 0;
	*ext += 4 + 1;
	break;
    case ERL_NEW_PORT_EXT:
        if (!jump_atom(ext)) return 0;
        *ext += 4 + 4;
        break;
    case ERL_NEW_REFERENCE_EXT:
    case ERL_NEWER_REFERENCE_EXT:
	n = (**ext << 8) | (*ext)[1];
	*ext += 2;
	/* first field is an atom */
	if (!jump_atom(ext)) return 0;
	*ext += 4*n + (tag == ERL_NEW_REFERENCE_EXT ? 1 : 4);
	break;
    case ERL_NIL_EXT:
	/* We just passed it... */
	break;
    case ERL_LIST_EXT:
	i = j = 0;
	j = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	*ext += 4;	
	for(k=0; k<j; k++) 
	    if ((i = jump(ext)) == 0)
		return(0);
	if (**ext == ERL_NIL_EXT) {
	    *ext += 1;
	    break;
	}
	if (jump(ext) == 0) return 0;
	break;
    case ERL_STRING_EXT:
	i = **ext << 8 | (*ext)[1];
	*ext += 2 + i;
	break;
    case ERL_SMALL_TUPLE_EXT:
	i = *(*ext)++;
	goto jump_tuple;
    case ERL_LARGE_TUPLE_EXT:
	i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	*ext += 4;
    jump_tuple:
	for (j = 0; j < i; j++) 
	    if ((k = jump(ext)) == 0)
		return(0);
	break;
    case ERL_FLOAT_EXT:
	*ext += 31;
	break;
    case NEW_FLOAT_EXT:
	*ext += 8;
	break;
    case ERL_BINARY_EXT:
	i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	*ext += 4+i;
	break;
    case ERL_FUN_EXT:
	i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	*ext += 4;
	i += 4;
	for (j = 0; j < i; j++)
	    if ((k = jump(ext)) == 0)
		return(0);
	break;
    case ERL_NEW_FUN_EXT:
	i = get32be(*ext);
	*ext += i + 4;
	break;
    case ERL_SMALL_BIG_EXT:
        i = *(*ext);
        *ext += i + 1;
        break;
    case ERL_LARGE_BIG_EXT:
	i = get32be(*ext);
        *ext += i + 4;
        break;
    default:
	return 0;
    } /* switch */

    return 1;

} /* jump */

/* 
 * The actual PEEK engine.
 */
static unsigned char *peek_ext(unsigned char **ext, int jumps)
{
  int i;

  switch (*(*ext)++) 
    {
    case ERL_VERSION_MAGIC:
      return peek_ext(ext, jumps);
    case ERL_SMALL_TUPLE_EXT:
      i = *(*ext)++;
      goto do_the_peek_stuff;
    case ERL_LARGE_TUPLE_EXT:
    case ERL_LIST_EXT:
      i = (**ext << 24) | ((*ext)[1]) << 16| ((*ext)[2]) << 8| ((*ext)[3]) ;  
      *ext += 4;
    do_the_peek_stuff:
      if (i <= jumps)   {
#ifdef DEBUG
	erl_err_msg("<ERROR> peek_ext: Out of range"); 
#endif
	return NULL;
      }
      for(i=0; i<jumps; i++)
	if (!jump(ext)) {
#ifdef DEBUG
	  erl_err_msg("<ERROR> peek_ext: Bad data"); 
#endif
	  return NULL;
	}
      return *ext;
    default:
#ifdef DEBUG
      erl_err_msg("<ERROR> peek_ext: Can't peek in non list/tuple type");
#endif
      return NULL;
    } /* switch */

} /* peek_ext */
	
/*
 * Return a POINTER TO the N:TH ELEMENT in a
 * COMPUND ENCODED ETERM.
 */
unsigned char *erl_peek_ext(unsigned char *ext, int jumps)
{
  unsigned char *x=ext;

  return peek_ext(&x, jumps);  

} /* erl_peek_ext */

/* 
 * Lexically compare two strings of bytes,
 * (string s1 length l1 and s2 l2).
 * Return: -1 if s1 < s2
 *	    0 if s1 = s2
 *	    1 if s1 > s2 
 */
static int cmpbytes(unsigned char* s1,int l1,unsigned char* s2,int l2)
{
  int i;
  i = 0;
  while((i < l1) && (i < l2)) {
    if (s1[i] < s2[i]) return(-1);
    if (s1[i] > s2[i]) return(1);
    i++;
  }
  if (l1 < l2) return(-1);
  if (l1 > l2) return(1);
  return(0);

} /* cmpbytes */

#define tag2enc(T) ((T)==ERL_ATOM_EXT || (T)==ERL_SMALL_ATOM_EXT ? ERLANG_LATIN1 : ERLANG_UTF8)

static int cmpatoms(unsigned char* s1, int l1, unsigned char tag1,	 
                    unsigned char* s2, int l2, unsigned char tag2)
{
    erlang_char_encoding enc1 = tag2enc(tag1);
    erlang_char_encoding enc2 = tag2enc(tag2);

    if (enc1 == enc2) {
	return cmpbytes(s1, l1,s2,l2);
    }
    
    if (enc1 == ERLANG_LATIN1) {
	return cmp_latin1_vs_utf8((char*)s1, l1, (char*)s2, l2);
    }
    else {
	return -cmp_latin1_vs_utf8((char*)s2, l2, (char*)s1, l1);
    }
}

int cmp_latin1_vs_utf8(const char* strL, int lenL, const char* strU, int lenU)
{
    unsigned char* sL = (unsigned char*)strL;
    unsigned char* sU = (unsigned char*)strU;
    unsigned char* sL_end = sL + lenL;
    unsigned char* sU_end = sU + lenU;

    while(sL < sL_end && sU < sU_end) {
	unsigned char UasL;
	if (*sL >= 0x80) {
	    if (*sU < 0xC4 && (sU+1) < sU_end) {
		UasL = ((sU[0] & 0x3) << 6) | (sU[1] & 0x3F);
	    }
	    else return -1;
	}
	else {
	    UasL = *sU;
	}
	if (*sL < UasL) return -1;
	if (*sL > UasL) return 1;

	sL++;
	if (*sU < 0x80) sU++;
	else if (*sU < 0xE0) sU += 2;
	else if (*sU < 0xF0) sU += 3;
	else /*if (*sU < 0xF8)*/ sU += 4;
    }

    return (sU >= sU_end) - (sL >= sL_end); /* -1, 0 or 1 */
}


#define CMP_EXT_ERROR_CODE 4711

#define CMP_EXT_INT32_BE(AP, BP)				\
do {								\
    if ((AP)[0] != (BP)[0]) return (AP)[0] < (BP)[0] ? -1 : 1;	\
    if ((AP)[1] != (BP)[1]) return (AP)[1] < (BP)[1] ? -1 : 1;	\
    if ((AP)[2] != (BP)[2]) return (AP)[2] < (BP)[2] ? -1 : 1;	\
    if ((AP)[3] != (BP)[3]) return (AP)[3] < (BP)[3] ? -1 : 1;	\
} while (0)

#define CMP_EXT_SKIP_ATOM(EP)					\
do {								\
    if (!jump_atom(&(EP)))					\
	return CMP_EXT_ERROR_CODE;				\
} while (0)

/* 
 * We now know that both byte arrays are of the same type.
 */
static int compare_top_ext(unsigned char**, unsigned char **); /* forward */
static int cmp_exe2(unsigned char **e1, unsigned char **e2);

static int cmp_refs(unsigned char **e1, unsigned char **e2)
{
    int tmp, n1, n2;
    unsigned char *node1, *node2, *id1, *id2, cre1, cre2;

    if (*((*e1)++) == ERL_REFERENCE_EXT) {
	node1 = *e1;
	CMP_EXT_SKIP_ATOM(*e1);
	n1 = 1;
	id1 = *e1;
	cre1 = (*e1)[4];
	*e1 += 5;
    } else {
	n1 = get16be(*e1);
	node1 = *e1;
	CMP_EXT_SKIP_ATOM(*e1);
	cre1 = **e1;
	id1 = (*e1) + 1 + (n1 - 1)*4;
	*e1 = id1 + 4;
    }

    if (*((*e2)++) == ERL_REFERENCE_EXT) {
	node2 = *e2;
	CMP_EXT_SKIP_ATOM(*e2);
	n2 = 1;
	id2 = *e2;
	cre2 = (*e2)[4];
	*e2 += 5;
    } else {
	n2 = get16be(*e2);
	node2 = *e2;
	CMP_EXT_SKIP_ATOM(*e2);
	cre2 = **e2;
	id2 = (*e2) + 1 + (n2 - 1)*4;
	*e2 = id2 + 4;
    }

    /* First compare node names... */
    tmp = cmp_exe2(&node1, &node2);
    if (tmp != 0)
	return tmp;

    /* ... then creations ... */
    if (cre1 != cre2)
	return cre1 < cre2 ? -1 : 1;

    /* ... and then finally ids. */
    if (n1 != n2) {
	unsigned char zero[] = {0, 0, 0, 0};
	if (n1 > n2)
	    do {
		CMP_EXT_INT32_BE(id1, zero);
		id1 -= 4;
		n1--;
	    } while (n1 > n2);
	else
	    do {
		CMP_EXT_INT32_BE(zero, id2);
		id2 -= 4;
		n2--;
	    } while (n2 > n1);
    }
    
    for (; n1 > 0; n1--, id1 -= 4, id2 -= 4)
	CMP_EXT_INT32_BE(id1, id2);

    return 0;
}

static int cmp_string_list(unsigned char **e1, unsigned char **e2) {
  
  /* we need to compare a string in **e1 and a list in **e2               */
  /* convert the string to list representation and convert that with e2   */
  /* we need a temporary buffer of:                                       */
  /* 5 (list tag + length) + 2*string length + 1 (end of list tag)        */
  /* for short lists we use a stack allocated buffer, otherwise we malloc */

  unsigned char *bp;
  unsigned char buf[5+2*255+1]; /* used for short lists */
  int i,e1_len;
  int res;
  
  e1_len = ((*e1)[1] << 8) | ((*e1)[2]);
  if ( e1_len < 256 ) {
    bp = buf;
  } else {
    bp = erl_malloc(5+(2*e1_len)+1);
  }

  bp[0] = ERL_LIST_EXT;
  bp[1] = bp[2] = 0;
  bp[3] = (*e1)[1];
  bp[4] = (*e1)[2];

  for(i=0;i<e1_len;i++) {
    bp[5+2*i] = ERL_SMALL_INTEGER_EXT;
    bp[5+2*i+1] = (*e1)[3+i];
  }

  bp[5+2*e1_len] = ERL_NIL_EXT;

  res = cmp_exe2(&bp, e2);

  if ( e1_len >= 256 ) free(bp);

  return res;
}

static int cmp_exe2(unsigned char **e1, unsigned char **e2)
{
  int min,  ret,i,j,k;
  double ff1, ff2;
  unsigned char tag1, tag2;

  if ( ((*e1)[0] == ERL_STRING_EXT) && ((*e2)[0] == ERL_LIST_EXT) ) {
    return cmp_string_list(e1, e2);
  } else if ( ((*e1)[0] == ERL_LIST_EXT) && ((*e2)[0] == ERL_STRING_EXT) ) {
    return -cmp_string_list(e2, e1);
  }

  tag1 = *(*e1)++;
  tag2 = *(*e2)++;
  i = j = 0;
  switch (tag1) 
    {
    case ERL_SMALL_INTEGER_EXT:
      if (**e1 < **e2) ret = -1;
      else if (**e1 > **e2) ret = 1;
      else ret = 0;
      *e1 += 1; *e2 += 1;
      return ret;
    case ERL_INTEGER_EXT:
      i = (int) (**e1 << 24) | ((*e1)[1] << 16) |((*e1)[2] << 8) | (*e1)[3];
      j = (int) (**e2 << 24) | ((*e2)[1] << 16) |((*e2)[2] << 8) | (*e2)[3];
      if ( i < j) 
	ret = -1;
      else if ( i > j) 
	ret = 1;
      else 
	ret = 0;
      *e1 += 4; *e2 += 4;
      return ret;
    case ERL_ATOM_EXT:
    case ERL_ATOM_UTF8_EXT:
      i = (**e1) << 8; (*e1)++;
      j = (**e2) << 8; (*e2)++;
      /*fall through*/
    case ERL_SMALL_ATOM_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
      i |= (**e1); (*e1)++;
      j |= (**e2); (*e2)++;
      ret = cmpatoms(*e1, i, tag1, *e2, j, tag2);
      *e1 += i;
      *e2 += j;
      return ret;
    case ERL_PID_EXT:
    case ERL_NEW_PID_EXT: {
      erlang_pid pid1, pid2;
      unsigned char* buf1 = *e1 - 1;
      unsigned char* buf2 = *e2 - 1;
      int ix1 = 0, ix2 = 0;

      if (ei_decode_pid((char*)buf1, &ix1, &pid1) ||
          ei_decode_pid((char*)buf2, &ix2, &pid2))
          return CMP_EXT_ERROR_CODE;

      *e1 = buf1 + ix1;
      *e2 = buf2 + ix2;

      /* First compare serials ... */
      if      (pid1.serial < pid2.serial) return -1;
      else if (pid1.serial > pid2.serial) return 1;

      /* ... then ids ... */
      if      (pid1.num < pid2.num) return -1;
      else if (pid1.num > pid2.num) return 1;

      /* ... then node names ... */
      j = strcmp(pid1.node, pid2.node);
      if      (j < 0) return -1;
      else if (j > 0) return 1;

      /* ... and then finaly creations. */
      if      (pid1.creation < pid2.creation) return -1;
      else if (pid1.creation > pid2.creation) return 1;

      return 0;
    }
    case ERL_PORT_EXT:
    case ERL_NEW_PORT_EXT: {
        erlang_port port1, port2;
        unsigned char* buf1 = *e1 - 1;
        unsigned char* buf2 = *e2 - 1;
        int ix1 = 0, ix2 = 0;

        if (ei_decode_port((char*)buf1, &ix1, &port1) ||
            ei_decode_port((char*)buf2, &ix2, &port2))
            return CMP_EXT_ERROR_CODE;

        *e1 = buf1 + ix1;
        *e2 = buf2 + ix2;

        /* First compare node names ... */
        j = strcmp(port1.node, port2.node);
        if      (j < 0) return -1;
        else if (j > 0) return 1;

      /* ... then creations ... */
        if      (port1.creation < port2.creation) return -1;
        else if (port1.creation > port2.creation) return 1;

      /* ... and then finally ids. */
        if      (port1.id < port2.id) return -1;
        else if (port1.id > port2.id) return 1;

        return 0;
    }
    case ERL_NIL_EXT: return 0;
    case ERL_LIST_EXT:
      i = (**e1 << 24) | ((*e1)[1] << 16) |((*e1)[2] << 8) | (*e1)[3];
      *e1 += 4;
      j = (**e2 << 24) | ((*e2)[1] << 16) |((*e2)[2] << 8) | (*e2)[3];
      *e2 += 4;
      if ( i == j && j == 0 ) return 0;
      min = (i < j) ? i : j;
      k = 0;
      while (1) {
	  if (k++ == min){
	      if (i == j) return 0;
	      if (i < j) return -1;
	      return 1;
	  }
	  if ((ret = compare_top_ext(e1 , e2)) == 0)
	      continue;
	  return ret;
      }
    case ERL_STRING_EXT:
      i = (**e1 << 8) | ((*e1)[1]);
      *e1 += 2;
      j = (**e2 << 8) | ((*e2)[1]);
      *e2 += 2;
      ret = cmpbytes(*e1, i, *e2, j);
      *e1 += i;
      *e2 += j;
      return ret;
    case ERL_SMALL_TUPLE_EXT:
      i = *(*e1)++; 	j = *(*e2)++;
      if (i < j) return -1;
      if (i > j ) return 1;
      while (i--) {
	if ((j = compare_top_ext(e1, e2))) return j;
      }
      return 0;
    case ERL_LARGE_TUPLE_EXT:
      i = (**e1 << 24) | ((*e1)[1]) << 16| ((*e1)[2]) << 8| ((*e1)[3]) ;	
      *e1 += 4;
      j = (**e2 << 24) | ((*e2)[1]) << 16| ((*e2)[2]) << 8| ((*e2)[3]) ;	
      *e2 += 4;
      if (i < j) return -1;
      if (i > j ) return 1;
      while (i--) {
	if ((j = compare_top_ext(e1, e2))) return j;
      }
      return 0;
    case ERL_FLOAT_EXT:
    case NEW_FLOAT_EXT:
      i = -1;
      if (ei_decode_double((char *) *e1, &i, &ff1) != 0)
        return -1;
      *e1 += i;
      j = -1;
      if (ei_decode_double((char *) *e2, &j, &ff2) != 0)
        return -1;
      *e2 += j;
      return cmp_floats(ff1,ff2);

    case ERL_BINARY_EXT:
      i = (**e1 << 24) | ((*e1)[1] << 16) |((*e1)[2] << 8) | (*e1)[3];
      *e1 += 4;
      j = (**e2 << 24) | ((*e2)[1] << 16) |((*e2)[2] << 8) | (*e2)[3];
      *e2 += 4;
      ret = cmpbytes(*e1, i , *e2 , j);
      *e1 += i; *e2 += j;
      return ret;

    case ERL_FUN_EXT:  /* FIXME: */
    case ERL_NEW_FUN_EXT:  /* FIXME: */
      return -1;

    default:
      return cmpbytes(*e1, 1, *e2, 1);

    } /* switch */
  
} /* cmp_exe2 */

/* Number compare */

static int cmp_floats(double f1, double f2)
{
#if defined(VXWORKS) && CPU == PPC860
      return erl_fp_compare((unsigned *) &f1, (unsigned *) &f2);
#else
      if (f1<f2) return -1;
      else if (f1>f2) return 1;
      else return 0;
#endif
}

static INLINE double to_float(long l) 
{
    double f;
#if defined(VXWORKS) && CPU == PPC860
    erl_long_to_fp(l, (unsigned *) &f);
#else
    f = l;
#endif
    return f;
}


static int cmp_small_big(unsigned char**e1, unsigned char **e2)
{
    int i1,i2;
    int t2;
    int n2;
    long l1;
    int res;

    erlang_big *b1,*b2;

    i1 = i2 = 0;
    if ( ei_decode_long((char *)*e1,&i1,&l1) < 0 ) return -1;
    
    ei_get_type((char *)*e2,&i2,&t2,&n2);
    
    /* any small will fit in two digits */
    if ( (b1 = ei_alloc_big(2)) == NULL ) return -1;
    if ( ei_small_to_big(l1,b1) < 0 ) {
        ei_free_big(b1);
        return -1;
    }
    
    if ( (b2 = ei_alloc_big(n2)) == NULL ) {
        ei_free_big(b1);
        return 1;
    }

    if ( ei_decode_big((char *)*e2,&i2,b2) < 0 ) {
        ei_free_big(b1);
        ei_free_big(b2);
        return 1;
    }
    
    res = ei_big_comp(b1,b2);
    
    ei_free_big(b1);
    ei_free_big(b2);

    *e1 += i1;
    *e2 += i2;

    return res;
}

static int cmp_small_float(unsigned char**e1, unsigned char **e2)
{
    int i1,i2;
    long l1;
    double f1,f2;

    /* small -> float -> float_comp */

    i1 = i2 = 0;
    if ( ei_decode_long((char *)*e1,&i1,&l1) < 0 ) return -1;
    if ( ei_decode_double((char *)*e2,&i2,&f2) < 0 ) return 1;
    
    f1 = to_float(l1);

    *e1 += i1;
    *e2 += i2;

    return cmp_floats(f1,f2);
}

static int cmp_float_big(unsigned char**e1, unsigned char **e2)
{
    int res;
    int i1,i2;
    int t2,n2;
    double f1,f2;
    erlang_big *b2;
    
    /* big -> float if overflow return big sign else float_comp */
    
    i1 = i2 = 0;
    if ( ei_decode_double((char *)*e1,&i1,&f1) < 0 ) return -1;
    
    if (ei_get_type((char *)*e2,&i2,&t2,&n2) < 0) return 1;
    if ((b2 = ei_alloc_big(n2)) == NULL) return 1;
    if (ei_decode_big((char *)*e2,&i2,b2) < 0) return 1;
    
    /* convert the big to float */
    if ( ei_big_to_double(b2,&f2) < 0 ) {
        /* exception look at the sign */
        res = b2->is_neg ? 1 : -1;
        ei_free_big(b2);
        return res;
    }
    
    ei_free_big(b2);

    *e1 += i1;
    *e2 += i2;

    return cmp_floats(f1,f2);
}

static int cmp_small_small(unsigned char**e1, unsigned char **e2)
{
    int i1,i2;
    long l1,l2;

    i1 = i2 = 0;
    if ( ei_decode_long((char *)*e1,&i1,&l1) < 0 ) {
        fprintf(stderr,"Failed to decode 1\r\n");
        return -1;
    }
    if ( ei_decode_long((char *)*e2,&i2,&l2) < 0 ) {
        fprintf(stderr,"Failed to decode 2\r\n");
        return 1;
    }
    
    *e1 += i1;
    *e2 += i2;
    
    if ( l1 < l2 ) return -1;
    else if ( l1 > l2 ) return 1;
    else return 0;
}

static int cmp_float_float(unsigned char**e1, unsigned char **e2)
{
    int i1,i2;
    double f1,f2;

    i1 = i2 = 0;
    if ( ei_decode_double((char *)*e1,&i1,&f1) < 0 ) return -1;
    if ( ei_decode_double((char *)*e2,&i2,&f2) < 0 ) return 1;
    
    *e1 += i1;
    *e2 += i2;
    
    return cmp_floats(f1,f2);
}

static int cmp_big_big(unsigned char**e1, unsigned char **e2)
{
    int res;
    int i1,i2;
    int t1,t2;
    int n1,n2;
    erlang_big *b1,*b2;

    i1 = i2 = 0;
    ei_get_type((char *)*e1,&i1,&t1,&n1);
    ei_get_type((char *)*e2,&i2,&t2,&n2);
    
    if ( (b1 = ei_alloc_big(n1)) == NULL) return -1;
    if ( (b2 = ei_alloc_big(n2)) == NULL) {
        ei_free_big(b1);
        return 1;
    }
    
    ei_decode_big((char *)*e1,&i1,b1);
    ei_decode_big((char *)*e2,&i2,b2);
    
    res = ei_big_comp(b1,b2);
    
    ei_free_big(b1);
    ei_free_big(b2);
    
    *e1 += i1;
    *e2 += i2;

    return res;
}

static int cmp_number(unsigned char**e1, unsigned char **e2)
{
    switch (CMP_NUM_CODE(**e1,**e2)) {

      case SMALL_BIG:
        /* fprintf(stderr,"compare small_big\r\n"); */
        return cmp_small_big(e1,e2);

      case BIG_SMALL:
        /* fprintf(stderr,"compare sbig_small\r\n"); */
        return -cmp_small_big(e2,e1);

      case SMALL_FLOAT:
        /* fprintf(stderr,"compare small_float\r\n"); */
        return cmp_small_float(e1,e2);
        
      case FLOAT_SMALL:
        /* fprintf(stderr,"compare float_small\r\n"); */
        return -cmp_small_float(e2,e1);

      case FLOAT_BIG:
        /* fprintf(stderr,"compare float_big\r\n"); */
        return cmp_float_big(e1,e2);

      case BIG_FLOAT:
        /* fprintf(stderr,"compare big_float\r\n"); */
        return -cmp_float_big(e2,e1);

      case SMALL_SMALL:
        /* fprintf(stderr,"compare small_small\r\n"); */
        return cmp_small_small(e1,e2);

      case FLOAT_FLOAT:
        /* fprintf(stderr,"compare float_float\r\n"); */
        return cmp_float_float(e1,e2);

      case BIG_BIG:
        /* fprintf(stderr,"compare big_big\r\n"); */
        return cmp_big_big(e1,e2);

      default:
        /* should never get here ... */
        /* fprintf(stderr,"compare standard\r\n"); */
        return cmp_exe2(e1,e2);
    }

}

/* 
 * If the arrays are of the same type, then we
 * have to do a real compare.
 */
/* 
 * COMPARE TWO encoded BYTE ARRAYS e1 and e2.
 * Return: -1 if e1 < e2
 *          0 if e1 == e2 
 *          1 if e2 > e1   
 */
static int compare_top_ext(unsigned char**e1, unsigned char **e2)
{
  if (**e1 == ERL_VERSION_MAGIC) (*e1)++;
  if (**e2 == ERL_VERSION_MAGIC) (*e2)++;

  if (cmp_array[**e1] < cmp_array[**e2]) return -1;
  if (cmp_array[**e1] > cmp_array[**e2]) return 1;
  
  if (IS_ERL_NUM(**e1)) 
      return cmp_number(e1,e2);

  if (cmp_array[**e1] == ERL_REF_CMP)
      return cmp_refs(e1, e2);

  return cmp_exe2(e1, e2);
}

int erl_compare_ext(unsigned char *e1, unsigned char *e2)
{
  return compare_top_ext(&e1, &e2); 
} /* erl_compare_ext */

#if defined(VXWORKS) && CPU == PPC860
/* FIXME we have no floating point but don't we have emulation?! */
int erl_fp_compare(unsigned *a, unsigned *b) 
{
    /* Big endian mode of powerPC, IEEE floating point. */
    unsigned a_split[4] = {a[0] >> 31,             /* Sign bit */
                           (a[0] >> 20) & 0x7FFU,  /* Exponent */
                           a[0] & 0xFFFFFU,        /* Mantissa MS bits */
                           a[1]};                  /* Mantissa LS bits */
    unsigned b_split[4] = {b[0] >> 31,
                           (b[0] >> 20) & 0x7FFU,
                           b[0] & 0xFFFFFU,
                           b[1]};
    int a_is_infinite, b_is_infinite;
    int res;


    /* Make -0 be +0 */
    if (a_split[1] == 0 && a_split[2] == 0 && a_split[3] == 0)
        a_split[0] = 0;
    if (b_split[1] == 0 && b_split[2] == 0 && b_split[3] == 0)
        b_split[0] = 0;
    /* Check for infinity */
    a_is_infinite = (a_split[1] == 0x7FFU && a_split[2] == 0 && 
                     a_split[3] == 0);
    b_is_infinite = (b_split[1] == 0x7FFU && b_split[2] == 0 && 
                     b_split[3] == 0);

    if (a_is_infinite && !b_is_infinite)
        return (a_split[0]) ? -1 : 1;
    if (b_is_infinite && !a_is_infinite)
        return (b_split[0]) ? 1 : -1;
    if (a_is_infinite && b_is_infinite)
        return b[0] - a[0]; 
    /* Check for indeterminate or nan, infinite is already handled, 
     so we only check the exponent. */
    if((a_split[1] == 0x7FFU) || (b_split[1] == 0x7FFU))
        return INT_MAX; /* Well, they are not equal anyway, 
                           abort() could be an alternative... */

    if (a_split[0] && !b_split[0])
        return -1;
    if (b_split[0] && !a_split[0])
        return 1;
    /* Compare */
    res = memcmp(a_split + 1, b_split + 1, 3 * sizeof(unsigned));
    /* Make -1, 0 or 1 */
    res = (!!res) * ((res < 0) ? -1 : 1); 
    /* Turn sign if negative values */
    if (a_split[0]) /* Both are negative */
        res = -1 * res;
    return res;
}

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

static void erl_long_to_fp(long l, unsigned *d) 
{
    unsigned d_split[4];
    unsigned x;
    if (l < 0) {
	d_split[0] = 1;
	x = -l;
    } else {
	d_split[0] = 0;
	x = l;
    }

    if (!l) {
	memset(d_split,0,sizeof(d_split));
    } else {
	int len = blength(x);
	x <<= (33 - len);
	d_split[2] = (x >> 12);
	d_split[3] = (x << 20);
	d_split[1] = 1023 + len - 1;
    }
    join(d_split,d);
}

#endif


/* 
 * Checks if a term is a "string": a flat list of byte-sized integers.
 *
 * Returns: 0 if the term is not a string, otherwise the length is returned.
 */

static int is_string(ETERM* term)
{
    int len = 0;

    while (ERL_TYPE(term) == ERL_LIST) {
	ETERM* head = HEAD(term);

	if (!ERL_IS_INTEGER(head) || ((unsigned)head->uval.ival.i) > 255) {
	    return 0;
	}
	len++;
	term = TAIL(term);
    }

    if (ERL_IS_EMPTY_LIST(term)) {
	return len;
    }
    return 0;
}
