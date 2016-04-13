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
/*
 * Purpose: Representation of Erlang terms.
 */  

#include "eidef.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#if defined(HAVE_ISFINITE)
#include <math.h>
#endif

#include "ei_locking.h"
#include "ei_resolve.h"
#include "erl_interface.h"
#include "erl_eterm.h"
#include "erl_malloc.h"
#include "erl_marshal.h"
#include "erl_error.h"
#include "erl_internal.h"
#include "ei_internal.h"
#include "putget.h"

#define ERL_IS_BYTE(x) (ERL_IS_INTEGER(x) && (ERL_INT_VALUE(x) & ~0xFF) == 0)

/* FIXME use unsigned char, or uint8 for buffers, cast (int) really needed? */

static void iolist_to_buf(const ETERM* term, char** bufp);
static char* strsave(const char *src);

/***************************************************************************
 *
 *  API: erl_init()
 *
 *  Not documented to set erl_errno.
 *
 ***************************************************************************/

/* all initialisation of erl_interface modules should be called from here */
/* order is important: erl_malloc and erl_resolve depend on ei_locking */
/* NOTE: don't call this directly - please use erl_init() macro defined 
   in ei_locking.h! */
void erl_init(void *hp,long heap_size)
{
    erl_init_malloc(hp, heap_size);
    erl_init_marshal();
    ei_init_resolve();
}

void erl_set_compat_rel(unsigned rel)
{
    ei_set_compat_rel(rel);
}

/*
 * Create an INTEGER. Depending on its value it 
 * may end up as a BigNum.
 */
ETERM *erl_mk_int (int i)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_INTEGER);
    ERL_COUNT(ep) = 1;
    ERL_INT_VALUE(ep) = i;
    return ep;
}

ETERM *erl_mk_longlong (long long i)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_LONGLONG);
    ERL_COUNT(ep) = 1;
    ERL_LL_VALUE(ep) = i;
    return ep;
}

/*
 * Create an UNSIGNED INTEGER. Depending on its 
 * value it may end up as a BigNum.
 */

ETERM *erl_mk_uint (unsigned int u)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_U_INTEGER);
    ERL_COUNT(ep) = 1;
    ERL_INT_UVALUE(ep) = u;
    return ep;
}

ETERM *erl_mk_ulonglong (unsigned long long i)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_U_LONGLONG);
    ERL_COUNT(ep) = 1;
    ERL_LL_UVALUE(ep) = i;
    return ep;
}

/*
 * Create a FLOAT.
 */
ETERM *erl_mk_float (double d)
{
    ETERM *ep;

#if defined(HAVE_ISFINITE)
    /* Erlang does not handle Inf and NaN, so we return an error
     * rather than letting the Erlang VM complain about a bad external
     * term. */
    if(!isfinite(d)) {
        return NULL;
    }
#endif

    ep = erl_alloc_eterm(ERL_FLOAT);
    ERL_COUNT(ep) = 1;
    ERL_FLOAT_VALUE(ep) = d;
    return ep;
}

/*
 * Create an ATOM 
 */
ETERM *erl_mk_atom (const char *s)
{
  ETERM *ep;

  /* ASSERT(s != NULL); */
  if (!s) return NULL;

  ep = erl_alloc_eterm(ERL_ATOM);
  ERL_COUNT(ep) = 1;
  if (erl_atom_init_latin1(&ep->uval.aval.d, s) == NULL) {
      erl_free_term(ep);
      erl_errno = ENOMEM;
      return NULL;
  }
  return ep;
} 

char* erl_atom_ptr_latin1(Erl_Atom_data* a)
{
    if (a->latin1 == NULL) {
	erlang_char_encoding enc;
	a->lenL = utf8_to_latin1(NULL, a->utf8, a->lenU, a->lenU, &enc);
	if (a->lenL < 0) {
	    a->lenL = 0;
	    return NULL;
	}
	if (enc == ERLANG_ASCII) {
	    a->latin1 = a->utf8; 
	}
	else {
	    a->latin1 = malloc(a->lenL+1);
	    utf8_to_latin1(a->latin1, a->utf8, a->lenU, a->lenL, NULL);
	    a->latin1[a->lenL] = '\0';
	}
    }
    return a->latin1;
}

char* erl_atom_ptr_utf8(Erl_Atom_data* a)
{
    if (a->utf8 == NULL) {
	int dlen = a->lenL * 2; /* over estimation */
	a->utf8 = malloc(dlen + 1); 
	a->lenU = latin1_to_utf8(a->utf8, a->latin1, a->lenL, dlen, NULL);
	a->utf8[a->lenU] = '\0';
    }
    return a->utf8;

}
int erl_atom_size_latin1(Erl_Atom_data* a)
{
    if (a->latin1 == NULL) {
	erl_atom_ptr_latin1(a);
    }
    return a->lenL;
}
int erl_atom_size_utf8(Erl_Atom_data* a)
{
    if (a->utf8 == NULL) {
	erl_atom_ptr_utf8(a);
    }
    return a->lenU;
}
char* erl_atom_init_latin1(Erl_Atom_data* a, const char* s)
{
    a->lenL = strlen(s);
    if ((a->latin1 = strsave(s)) == NULL)
    {
	return NULL;
    }
    a->utf8 = NULL;
    a->lenU = 0;
    return a->latin1;
}


/*
 * Given a string as input, creates a list.
 */
ETERM *erl_mk_string(const char *s)
{
  /* ASSERT(s != NULL); */
  if (!s) return NULL;

    return erl_mk_estring(s, strlen(s));
}

ETERM *erl_mk_estring(const char *s, int len)
{
    ETERM *ep;
    int i;

    if ((!s) || (len < 0)) return NULL;

    /*
     * ASSERT(s != NULL);
     * ASSERT(len >= 0);
     */

    ep = erl_mk_empty_list();
    for (i = len-1; i >= 0; i--) {
	ETERM* integer;
	ETERM* cons;

	integer = erl_alloc_eterm(ERL_INTEGER);
	ERL_COUNT(integer) = 1;
	ERL_INT_VALUE(integer) = (unsigned char)s[i];

	cons = erl_alloc_eterm(ERL_LIST);
	ERL_COUNT(cons) = 1;
	HEAD(cons) = integer;
	TAIL(cons) = ep;
	ep = cons;
    }
    return ep;
}

/*
 * Create a PID.
 */
ETERM *erl_mk_pid(const char *node, 
		  unsigned int number, 
		  unsigned int serial, 
		  unsigned char creation)
{
    ETERM *ep;

    if (!node) return NULL;
    /* ASSERT(node != NULL); */

    ep = erl_alloc_eterm(ERL_PID);
    ERL_COUNT(ep) = 1;
    if (erl_atom_init_latin1(&ep->uval.pidval.node, node) == NULL)
    {	     
	erl_free_term(ep);
	erl_errno = ENOMEM;
	return NULL;
    }
    erl_mk_pid_helper(ep, number, serial, creation & 0x03);
    return ep;
}

void erl_mk_pid_helper(ETERM *ep, unsigned int number, 
		       unsigned int serial, unsigned int creation)
{
    ERL_PID_NUMBER(ep)   = number & 0x7fff; /* 15 bits */
    if (ei_internal_use_r9_pids_ports()) {
	ERL_PID_SERIAL(ep)   = serial & 0x07;  /* 3 bits */
    }
    else {
	ERL_PID_SERIAL(ep)   = serial & 0x1fff;  /* 13 bits */
    }
    ERL_PID_CREATION(ep) = creation; /* 32 bits */
}

/*
 * Create a PORT.
 */
ETERM *erl_mk_port(const char *node, 
		   unsigned int number, 
		   unsigned char creation)
{
    ETERM *ep;

    if (!node) return NULL;
    /* ASSERT(node != NULL); */

    ep = erl_alloc_eterm(ERL_PORT);
    ERL_COUNT(ep) = 1;
    if (erl_atom_init_latin1(&ep->uval.portval.node, node) == NULL)
    {	     
	erl_free_term(ep);
	erl_errno = ENOMEM;
	return NULL;
    }
    erl_mk_port_helper(ep, number, creation);
    return ep;
}

void erl_mk_port_helper(ETERM* ep, unsigned number, unsigned int creation)
{
    if (ei_internal_use_r9_pids_ports()) {
	ERL_PORT_NUMBER(ep)   = number & 0x3ffff; /* 18 bits */
    }
    else {
	ERL_PORT_NUMBER(ep)   = number & 0x0fffffff; /* 18 bits */
    }
    ERL_PORT_CREATION(ep) = creation; /* 32 bits */
}

/*
 * Create any kind of reference.
 */
ETERM *__erl_mk_reference (ETERM* t,
			   const char *node,
			   size_t len,
			   unsigned int n[],
			   unsigned int creation)
{
    if (t == NULL) {
	if (node == NULL) return NULL;
    
	t = erl_alloc_eterm(ERL_REF);
	ERL_COUNT(t) = 1;
    
	if (erl_atom_init_latin1(&t->uval.refval.node, node) == NULL)
	{	     
	    erl_free_term(t);
	    erl_errno = ENOMEM;
	    return NULL;
	}
    }
    ERL_REF_LEN(t) = len;
    ERL_REF_NUMBERS(t)[0]   = n[0] & 0x3ffff; /* 18 bits */
    ERL_REF_NUMBERS(t)[1]   = n[1];
    ERL_REF_NUMBERS(t)[2]   = n[2];
    ERL_REF_CREATION(t) = creation; /* 32 bits */

    return t;
}

/*
 * Create a REFERENCE.
 */
ETERM *erl_mk_ref (const char *node, 
		   unsigned int number, 
		   unsigned char creation)
{
    unsigned int n[3] = {0, 0, 0};
    n[0] = number;
    return __erl_mk_reference(NULL, node, 1, n, creation);
}

/*
 * Create a long REFERENCE.
 */
ETERM *
erl_mk_long_ref (const char *node, 
		 unsigned int n1, unsigned int n2, unsigned int n3,
		 unsigned char creation)
{
    unsigned int n[3] = {0, 0, 0};
    n[0] = n3; n[1] = n2; n[2] = n1;
    return __erl_mk_reference(NULL, node, 3, n, creation);
}

/*
 * Create a BINARY.
 */
ETERM *erl_mk_binary (const char *b, int size)
{
    ETERM *ep;

    if ((!b) || (size < 0)) return NULL;
    /* ASSERT(b != NULL); */

    ep = erl_alloc_eterm(ERL_BINARY);
    ERL_COUNT(ep) = 1;
    ERL_BIN_SIZE(ep) = size;
    ERL_BIN_PTR(ep) = (unsigned char *) erl_malloc(size);
    memcpy(ERL_BIN_PTR(ep), b, size);
    return ep;
}

/*
 * Create a TUPLE. For each element in the tuple
 * bump its reference counter.
 */
ETERM *erl_mk_tuple (ETERM **arr,int size)
{
    ETERM *ep;
    int i;

    if ((!arr) || (size < 0)) return NULL;
    for (i=0; i<size; i++) if (!arr[i]) return NULL;
    /* ASSERT(arr != NULL); */
	
    ep = erl_alloc_eterm(ERL_TUPLE);
    ERL_COUNT(ep) = 1;
    ERL_TUPLE_SIZE(ep) = size;
    ERL_TUPLE_ELEMS(ep) = (ETERM**) erl_malloc((size) * (sizeof(ETERM*)));
    for (i = 0; i < size; i++) {
      /* ASSERT(arr[i] != NULL); */
      ERL_COUNT(arr[i])++;
      ERL_TUPLE_ELEMENT(ep, i) = arr[i];
    }
    return ep;
}

/*
 * SET an ELEMENT in a TUPLE. Free the old element
 * and bump the reference counter of the new one.
 * Return 1 on success, otherwise 0.
 */
#if 0
int erl_setelement (int ix, ETERM *ep, ETERM *vp)
{
  if ((!ep) || (!vp)) return 0;
  /* ASSERT(ep != NULL);
   * ASSERT(vp != NULL);
   */

  if ((ERL_TYPE(ep) == ERL_TUPLE) && (ix <= ERL_TUPLE_SIZE(ep))) {
      erl_free_term(ERL_TUPLE_ELEMENT(ep, ix-1));
      ERL_TUPLE_ELEMENT(ep, ix-1) = vp;
      ERL_COUNT(vp)++;
      return 1;
  }  
  erl_err_msg("<ERROR> erl_setelement: Bad type to setelement or out of range \n");
  return 0;
}
#endif

/* 
 * Extract an ELEMENT from a TUPLE. Bump the 
 * reference counter on the extracted object.
 */
ETERM *erl_element (int ix, const ETERM *ep)
{
  if ((!ep) || (ix < 0)) return NULL;
  /*
   * ASSERT(ep != NULL);
   * ASSERT(ix >= 0);
   */

  if ((ERL_TYPE(ep) == ERL_TUPLE) &&  (ix <= ERL_TUPLE_SIZE(ep))) {
      ERL_COUNT(ERL_TUPLE_ELEMENT(ep, ix-1))++;
      return ERL_TUPLE_ELEMENT(ep, ix-1);
    }
    else 
	return NULL;
} /* erl_element */

ETERM *erl_mk_empty_list(void)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_EMPTY_LIST);
    ERL_COUNT(ep) = 1;
    return ep;
}

/*
 * Construct a new list by CONS'ing a HEAD on
 * to the TAIL. Bump the reference counter on
 * the head and tail object. Note that we allow
 * non-well formed lists to be created.
 */
ETERM *erl_cons(ETERM *hd, ETERM *tl)
{
    ETERM *ep;

    if ((!hd) || (!tl)) return NULL;

    /*
     * ASSERT(hd != NULL);
     * ASSERT(tl != NULL);
     */

    ep = erl_alloc_eterm(ERL_LIST);
    ERL_COUNT(ep) = 1;
    HEAD(ep) = hd;
    TAIL(ep) = tl;
    ERL_COUNT(hd)++;
    ERL_COUNT(tl)++;
    return ep;
}

/*
 * Extract the HEAD of a LIST. Bump the reference 
 * counter on the head object.
 */
ETERM *erl_hd (const ETERM *ep)
{
  if (!ep) return NULL;
  /* ASSERT(ep != NULL); */

    if (ERL_TYPE(ep) != ERL_LIST) {
	return (ETERM *) NULL; 
    }
    ERL_COUNT(ERL_CONS_HEAD(ep))++;
    return ERL_CONS_HEAD(ep);
}

/* 
 * Extract the TAIL of a LIST. Bump the reference
 * counter on the tail object.
 */
ETERM *erl_tl (const ETERM *ep)
{
    ETERM *tl;

    if (!ep) return NULL;
    /* ASSERT(ep != NULL); */

    if (ERL_TYPE(ep) != ERL_LIST) {
	return (ETERM *) NULL; 
    }

    tl = TAIL(ep);
    ERL_COUNT(tl)++;
    return tl;
}

/*
 * Create a LIST from an array of elements. Note that
 * we create it from the last element in the array to
 * the first. Also, note that we decrement the reference
 * counter for each member in the list but the first one.
 * This is done because of the use of erl_cons.
 */

ETERM *erl_mk_list (ETERM **arr, int size)
{
    ETERM *ep;
    int i;

    if ((!arr) || (size < 0)) return NULL;
    for (i=0; i<size; i++) if (!arr[i]) return NULL;
    
    /* ASSERT(arr != NULL); */
    ep = erl_mk_empty_list();
    if (size > 0) {
	ERL_COUNT(ep)--;
    }

    for (i = size-1; i >= 0; i--) {
      /* ASSERT(arr[i] != NULL); */
	ep = erl_cons(arr[i], ep);
	if (i > 0)
	    ERL_COUNT(ep)--;	/* Internal reference */
    }
    return ep;
}

/* 
 * Create an empty VARIABLE. 
 */
ETERM *erl_mk_var(const char *s)
{
    ETERM *ep;

    if (!s) return NULL;
    
    /* ASSERT(s != NULL); */

    ep = erl_alloc_eterm(ERL_VARIABLE);
    ERL_COUNT(ep) = 1;
    ERL_VAR_LEN(ep) = strlen(s);    
    if ((ERL_VAR_NAME(ep) = strsave(s)) == NULL)
    {
	erl_free_term(ep);
	erl_errno = ENOMEM;
	return NULL;
    }   
    ERL_VAR_VALUE(ep) = (ETERM *) NULL;
    return ep;
}

/* 
 * Return the CONTENT of a VARIABLE with NAME.
 * If the content is non-nil then bump its
 * reference counter.
 */
ETERM *erl_var_content (const ETERM *ep, const char *name)
{
  int i;
  ETERM *vp;

  if ((!ep) || (!name)) return NULL;
    
  /*   ASSERT(ep != NULL); */

  switch(ERL_TYPE(ep)) 
    {
    case ERL_VARIABLE:
	if (strcmp(ERL_VAR_NAME(ep), name) == 0) {
	    if ((vp = ERL_VAR_VALUE(ep)) != NULL) {
		ERL_COUNT(vp)++;
		return vp;
	    }
	}
	break;

    case ERL_LIST:
      while (ep && (ERL_TYPE(ep) != ERL_EMPTY_LIST)) {
	if ((vp = erl_var_content(HEAD(ep), name))) return vp;
	ep = TAIL(ep);
      }
      break;

    case ERL_TUPLE:
      for (i=0; i < ERL_TUPLE_SIZE(ep); i++) 
	  if ((vp = erl_var_content(ERL_TUPLE_ELEMENT(ep, i), name)))
	  {
	      return vp;
	  }
      break;
    
    default:
      /* variables can't occur in other types */
      break;
    }
    
  /* nothing found ! */
  return NULL;
}

/*
 * Return the SIZE of a TUPLE or a BINARY.
 * At failure -1 is returned.
 */
int erl_size (const ETERM *ep)
{
  if (!ep) return -1;
  
  /* ASSERT(ep != NULL); */

  switch (ERL_TYPE(ep)) {
  case ERL_TUPLE:
      return ERL_TUPLE_SIZE(ep);

  case ERL_BINARY:
      return ERL_BIN_SIZE(ep);

  default:
      return -1;

  }
}

/*
 * Return the LENGTH of a LIST.
 * At failure -1 is returned (this include non-proper lists like [a|b]).
 */
int erl_length(const ETERM *ep)
{
    int n = 0;

    if (!ep) return -1;
    /* ASSERT(ep != NULL); */

    while (ERL_TYPE(ep) == ERL_LIST) {
      n++;
      ep = TAIL(ep);
    }

    if (!ERL_IS_EMPTY_LIST(ep)) return -1;

    return n;
}


/***********************************************************************
 * I o l i s t   f u n c t i o n s
 *
 * The following functions handles I/O lists.
 *
 * Informally, an I/O list is a deep list of characters and binaries,
 * which can be sent to an Erlang port.
 *
 * Formally, in BNF, an I/O list is defined as:
 *
 * iolist ::= []
 *        |   Binary
 *        |   [iohead | iolist]
 *        ;
 *
 * iohead ::= Binary
 *        |   Byte (integer in the range [0..255])
 *        |   iolist
 *        ;
 *
 * Note that versions of Erlang/OTP prior to R2 had a slightly more
 * restricted definition of I/O lists, in that the tail of a an I/O list
 * was not allowed to be a binary.  The erl_interface functions
 * for I/O lists follows the more liberal rules described by the BNF
 * description above.
 ***********************************************************************/

/*
 * This function converts an I/O list to a '\0' terminated C string.
 * The I/O list must not contain any occurrences of the integer 0.
 *
 * The string will be in memory allocated by erl_malloc().  It is the
 * responsibility of the caller to eventually call erl_free() to free
 * the memory.
 *
 * Returns: NULL if the list was not an I/O list or contained
 * the integer 0, otherwise a pointer to '\0' terminated string.
 */

char* erl_iolist_to_string(const ETERM* term)
{
    ETERM* bin;

    if ((bin = erl_iolist_to_binary(term)) == NULL) {
	return NULL;
    } else {
	char* result = NULL;

	if (memchr(ERL_BIN_PTR(bin), '\0', ERL_BIN_SIZE(bin)) == NULL) {
	    result = (char *) erl_malloc(ERL_BIN_SIZE(bin)+1);
	    memcpy(result, ERL_BIN_PTR(bin), ERL_BIN_SIZE(bin));
	    result[ERL_BIN_SIZE(bin)] = '\0';
	}
	erl_free_term(bin);
	return result;
    }
}

/*
 * This function converts an I/O list to a binary term.
 *
 * Returns: NULL if the list was not an I/O list, otherwise
 * an ETERM pointer pointing to a binary term.
 */

ETERM *erl_iolist_to_binary (const ETERM* term)
{
    ETERM *dest;
    int size;
    char* ptr;

    if (!term) return NULL;
    /* ASSERT(term != NULL); */

    /*
     * Verify that the term is an I/O list and get its length.
     */

    size = erl_iolist_length(term);
    if (size == -1) {
	return NULL;
    }

    /*
     * Allocate the binary and copy the contents of the I/O list into it.
     */

    dest = erl_alloc_eterm(ERL_BINARY);
    ERL_COUNT(dest) = 1;
    ERL_BIN_SIZE(dest) = size;
    ptr = (char *)erl_malloc(size);
    ERL_BIN_PTR(dest) = (unsigned char *)ptr;
    iolist_to_buf(term, &ptr);

    /*
     * If ptr doesn't point exactly one byte beyond the end of the
     * binary, something must be seriously wrong.
     */

    if (ERL_BIN_PTR(dest) + size != (unsigned char *) ptr) return NULL;
    /* ASSERT(ERL_BIN_PTR(dest) + size == (unsigned char *) ptr); */

    return dest;
}

/*
 * Returns the length of an I/O list.
 *
 * Returns: -1 if the term if the given term is not a I/O list,
 * or the length otherwise.
 */

int erl_iolist_length (const ETERM* term)
{
    int len = 0;

    while (ERL_IS_CONS(term)) {
	ETERM* obj = HEAD(term);

	if (ERL_IS_BYTE(obj)) {
	    len++;
	} else if (ERL_IS_CONS(obj)) {
	    int i;
	    if ((i = erl_iolist_length(obj)) < 0)
		return i;
	    len += i;
	} else if (ERL_IS_BINARY(obj)) {
	    len += ERL_BIN_SIZE(obj);
	} else if (!ERL_IS_EMPTY_LIST(obj)) {
	    return(-1);
	}
	term = TAIL(term);
    }
    if (ERL_IS_EMPTY_LIST(term))
	return len;
    else if (ERL_IS_BINARY(term))
	return len + ERL_BIN_SIZE(term);
    else
	return -1;
}

static int erl_atom_copy(Erl_Atom_data* dst, const Erl_Atom_data* src)
{
    if (src->latin1 == src->utf8) {
	dst->latin1 = dst->utf8 = strsave(src->latin1);
	dst->lenL = dst->lenU = strlen(src->latin1);
    }
    else if (src->latin1) {
	dst->latin1 = strsave(src->latin1);
	dst->lenL = strlen(src->latin1);
	dst->utf8 = NULL;
	dst->lenU = 0;
    }
    else {
	dst->utf8 = strsave(src->utf8);
	dst->lenU = strlen(src->utf8);
	dst->latin1 = NULL;
	dst->lenL = 0;
    }
    return (dst->latin1 != NULL || dst->utf8 == NULL);
}


/*
 * Return a brand NEW COPY of an ETERM.
 */
/*
 * FIXME: Deep (the whole tree) or shallow (just the top term) copy?
 * The documentation never says, but the code as written below will
 * make a deep copy. This should be documented.
 */
ETERM *erl_copy_term(const ETERM *ep)
{
    int i;
    ETERM *cp;

    if (!ep) return NULL;
    /* ASSERT(ep != NULL); */
    
    cp = erl_alloc_eterm(ERL_TYPE(ep));
    ERL_COUNT(cp) = 1;

    switch(ERL_TYPE(cp)) {
    case ERL_INTEGER:
    case ERL_SMALL_BIG:
	ERL_INT_VALUE(cp) = ERL_INT_VALUE(ep);
	break;
    case ERL_U_INTEGER:
    case ERL_U_SMALL_BIG:
	ERL_INT_UVALUE(cp) = ERL_INT_UVALUE(ep);
	break;
    case ERL_LONGLONG:
	ERL_LL_VALUE(cp) = ERL_LL_VALUE(ep);
	break;
    case ERL_U_LONGLONG:
	ERL_LL_UVALUE(cp) = ERL_LL_UVALUE(ep);
	break;
    case ERL_FLOAT:
	ERL_FLOAT_VALUE(cp) = ERL_FLOAT_VALUE(ep);
	break;
    case ERL_ATOM:
	if (!erl_atom_copy(&cp->uval.aval.d, &ep->uval.aval.d))
	{
	    erl_free_term(cp);
	    erl_errno = ENOMEM;
	    return NULL;
	}
	break;
    case ERL_PID:
	/* FIXME: First copy the bit pattern, then duplicate the node
           name and plug in. Somewhat ugly (also done with port and
           ref below). */
	memcpy(&cp->uval.pidval, &ep->uval.pidval, sizeof(Erl_Pid));
	erl_atom_copy(&cp->uval.pidval.node, &ep->uval.pidval.node);
	ERL_COUNT(cp) = 1;
	break;
    case ERL_PORT:
	memcpy(&cp->uval.portval, &ep->uval.portval, sizeof(Erl_Port));
	erl_atom_copy(&cp->uval.portval.node, &ep->uval.portval.node);
	ERL_COUNT(cp) = 1;
	break;
    case ERL_REF:
	memcpy(&cp->uval.refval, &ep->uval.refval, sizeof(Erl_Ref));
	erl_atom_copy(&cp->uval.refval.node, &ep->uval.refval.node);
	ERL_COUNT(cp) = 1;
	break;
    case ERL_LIST:
	HEAD(cp) = erl_copy_term(HEAD(ep));
	TAIL(cp) = erl_copy_term(TAIL(ep));
	break;
    case ERL_EMPTY_LIST:
	break;
    case ERL_TUPLE:
	i = ERL_TUPLE_SIZE(cp) = ERL_TUPLE_SIZE(ep);
	ERL_TUPLE_ELEMS(cp) = (ETERM**) erl_malloc(i * sizeof(ETERM*));
	for(i=0; i < ERL_TUPLE_SIZE(ep); i++) 
	    ERL_TUPLE_ELEMENT(cp,i) = erl_copy_term(ERL_TUPLE_ELEMENT(ep, i));
	break;
    case ERL_BINARY:
	ERL_BIN_SIZE(cp) = ERL_BIN_SIZE(ep);
	ERL_BIN_PTR(cp) = (unsigned char *) erl_malloc(ERL_BIN_SIZE(ep));
	memcpy(ERL_BIN_PTR(cp), ERL_BIN_PTR(ep), ERL_BIN_SIZE(ep));
	break;
    case ERL_FUNCTION:
	i = ERL_CLOSURE_SIZE(cp) = ERL_CLOSURE_SIZE(ep);
	ERL_FUN_ARITY(cp)     = ERL_FUN_ARITY(ep);
	ERL_FUN_NEW_INDEX(cp) = ERL_FUN_NEW_INDEX(ep);
	ERL_FUN_INDEX(cp)     = erl_copy_term(ERL_FUN_INDEX(ep));
	ERL_FUN_UNIQ(cp)      = erl_copy_term(ERL_FUN_UNIQ(ep));
	ERL_FUN_CREATOR(cp)   = erl_copy_term(ERL_FUN_CREATOR(ep));
	ERL_FUN_MODULE(cp)    = erl_copy_term(ERL_FUN_MODULE(ep));
	memcpy(ERL_FUN_MD5(cp), ERL_FUN_MD5(ep), sizeof(ERL_FUN_MD5(ep)));
	ERL_CLOSURE(cp) = (ETERM**) erl_malloc(i * sizeof(ETERM*));
	for(i=0; i < ERL_CLOSURE_SIZE(ep); i++) 
	    ERL_CLOSURE_ELEMENT(cp,i) = 
		erl_copy_term(ERL_CLOSURE_ELEMENT(ep, i));
	break;
    default:
	erl_err_msg("<ERROR> erl_copy_term: wrong type encountered !");
	erl_free_term(cp);
	return (ETERM *) NULL;
    }
    
    return cp;
}

#ifndef SILENT

static int print_string(FILE* fp, const ETERM* ep);
static int is_printable_list(const ETERM* term);

/*
 * PRINT out an ETERM.  
 */

int erl_print_term(FILE *fp, const ETERM *ep)
{
    int j,i,doquote;
    int ch_written = 0; /* counter of written chars */

    if ((!fp) || (!ep)) return 0;
    /* ASSERT(ep != NULL); */

    j = i = doquote = 0;
    switch(ERL_TYPE(ep)) 
    {
    case ERL_ATOM: {
	char* adata = ERL_ATOM_PTR(ep);
	/* FIXME: what if some weird locale is in use? */
	if (!islower(adata[0]))
	    doquote = 1;

	for (i = 0; !doquote && i < ERL_ATOM_SIZE(ep); i++) 
	{
	    doquote = !(isalnum(adata[i]) || (adata[i] == '_'));
	}

	if (doquote) {
	    putc('\'', fp);
	    ch_written++; 
	}
	fputs(adata, fp);
	ch_written += ERL_ATOM_SIZE(ep);	
	if (doquote) {
	    putc('\'', fp);
	    ch_written++;
	}
	break;
    }
    case ERL_VARIABLE:
	if (!isupper((int)ERL_VAR_NAME(ep)[0])) {
	    doquote = 1;
	    putc('\'', fp);
	    ch_written++;
	}
	
	fputs(ERL_VAR_NAME(ep), fp);
	ch_written += ERL_VAR_LEN(ep);
	
	if (doquote) {
	    putc('\'', fp);
	    ch_written++;
	}
	break;

    case ERL_PID:
	ch_written += fprintf(fp, "<%s.%d.%d>", 
			    ERL_PID_NODE(ep), 
			    ERL_PID_NUMBER(ep), ERL_PID_SERIAL(ep));
      break;
    case ERL_PORT:
      ch_written += fprintf(fp, "#Port");
      break;
    case ERL_REF:
      ch_written += fprintf(fp, "#Ref");
      break;
    case ERL_EMPTY_LIST:
      ch_written += fprintf(fp, "[]");
      break;
    case ERL_LIST: 
	if (is_printable_list(ep)) {
	    ch_written += print_string(fp, ep);
	} else {
	    putc('[', fp);
	    ch_written++;
	    while (ERL_IS_CONS(ep)) {
		ch_written += erl_print_term(fp, HEAD(ep));
		ep = TAIL(ep);
		if (ERL_IS_CONS(ep)) {
		    putc(',', fp);
		    ch_written++;
		}
	    }
	    if (!ERL_IS_EMPTY_LIST(ep)) {
		putc('|', fp);
		ch_written++;
		ch_written += erl_print_term(fp, ep);
	    }
	    putc(']', fp);
	    ch_written++;
	}
	break;
    case ERL_TUPLE:
      putc('{', fp);
      ch_written++;
      for (i=0; i < ERL_TUPLE_SIZE(ep); i++) {
	ch_written += erl_print_term(fp, ERL_TUPLE_ELEMENT(ep, j++) );
	if (i != ERL_TUPLE_SIZE(ep)-1) {
	  putc(',', fp);
	  ch_written++;
	}
      }
      putc('}', fp);
      ch_written++;
      break;
    case ERL_BINARY: {
	int sz = (ERL_BIN_SIZE(ep) > 20) ? 20 : ERL_BIN_SIZE(ep);
	unsigned char *ptr = ERL_BIN_PTR(ep);
	ch_written += fprintf(fp, "#Bin<");
	for (i = 0; i < sz; i++) { 
	    putc(ptr[i], fp); ch_written++;
	}
	if (sz == 20) ch_written += fprintf(fp, "(%d)....>", ERL_BIN_SIZE(ep)-20);
	else ch_written += fprintf(fp, ">");
	break;
      }
    case ERL_INTEGER:
    case ERL_SMALL_BIG:
      ch_written += fprintf(fp, "%d", ERL_INT_VALUE(ep));
      break;
    case ERL_U_INTEGER:
    case ERL_U_SMALL_BIG:
      ch_written += fprintf(fp, "%d", ERL_INT_UVALUE(ep));
      break;
    case ERL_LONGLONG:
    case ERL_U_LONGLONG:
      ch_written += fprintf(fp, "%lld", ERL_LL_UVALUE(ep));
      break;
    case ERL_FLOAT:
      ch_written += fprintf(fp, "%f", ERL_FLOAT_VALUE(ep));
      break;
    case ERL_FUNCTION:
      ch_written += fprintf(fp, "#Fun<");
      ch_written += erl_print_term(fp, ERL_FUN_MODULE(ep));
      putc('.', fp);
      ch_written++;
      ch_written += erl_print_term(fp, ERL_FUN_INDEX(ep));
      putc('.', fp);
      ch_written++;
      ch_written += erl_print_term(fp, ERL_FUN_UNIQ(ep));
      putc('>', fp);
      ch_written++;
      break;
    default:
      ch_written = -10000;
      erl_err_msg("<ERROR> erl_print_term: Bad type of term !");
    }
  return ch_written;
}

/*
 * FIXME not done yet....
 */

#if 0

int erl_sprint_term(char *buf, const ETERM *ep)
{
    int j,i,doquote;
    int ch_written = 0; /* counter of written chars */

    if ((!buf) || (!ep)) return 0;
    /* ASSERT(ep != NULL); */

    j = i = doquote = 0;
    switch(ERL_TYPE(ep)) 
    {
    case ERL_ATOM:
	/* FIXME: what if some weird locale is in use? */
	if (!islower((int)ERL_ATOM_PTR(ep)[0]))
	    doquote = 1;

	for (i = 0; !doquote && i < ERL_ATOM_SIZE(ep); i++) 
	{
	    doquote = !(isalnum((int)ERL_ATOM_PTR(ep)[i]) 
			|| (ERL_ATOM_PTR(ep)[i] == '_'));
	}

	if (doquote) {
	    *buf++ = '\'';
	    ch_written++; 
	}
	{
	    int len = ERL_ATOM_SIZE(ep);
	    strncpy(buf, ERL_ATOM_PTR(ep), len);
	    buf += len;
	    ch_written += len;	
	}
	if (doquote) {
	    *buf++ = '\'';
	    ch_written++;
	}
	break;

    case ERL_VARIABLE:
	if (!isupper((int)ERL_VAR_NAME(ep)[0])) {
	    doquote = 1;
	    *buf++ = '\'';
	    ch_written++;
	}
	len = ERL_VAR_LEN(ep);
	strncpy(buf, ERL_VAR_NAME(ep), len);
	buf += len;
	ch_written += len;
	
	if (doquote) {
	    *buf++ = '\'';
	    ch_written++;
	}
	break;

    case ERL_PID:
	len = sprintf(buf, "<%s.%d.%d>", 
		      ERL_PID_NODE(ep), 
		      ERL_PID_NUMBER(ep), ERL_PID_SERIAL(ep));
        buf += len;
        ch_written += len;
	break;
    case ERL_PORT:
	len = sprintf(buf , "#Port");
        buf += len;
        ch_written += len;
	break;
    case ERL_REF:
	len = sprintf(buf , "#Ref");
        buf += len;
        ch_written += len;
	break;
    case ERL_EMPTY_LIST:
	len = sprintf(buf , "[]");
        buf += len;
        ch_written += len;
	break;
    case ERL_LIST: 
	if (is_printable_list(ep)) {
	    ch_written += print_string(fp, ep);
	} else {
	    putc('[', fp);
	    ch_written++;
	    while (ERL_IS_CONS(ep)) {
		ch_written += erl_sprint_term(fp, HEAD(ep));
		ep = TAIL(ep);
		if (ERL_IS_CONS(ep)) {
		    putc(',', fp);
		    ch_written++;
		}
	    }
	    if (!ERL_IS_EMPTY_LIST(ep)) {
		putc('|', fp);
		ch_written++;
		ch_written += erl_sprint_term(fp, ep);
	    }
	    putc(']', fp);
	    ch_written++;
	}
	break;
    case ERL_TUPLE:
      putc('{', fp);
      ch_written++;
      for (i=0; i < ERL_TUPLE_SIZE(ep); i++) {
	ch_written += erl_sprint_term(fp, ERL_TUPLE_ELEMENT(ep, j++) );
	if (i != ERL_TUPLE_SIZE(ep)-1) {
	  putc(',', fp);
	  ch_written++;
	}
      }
      putc('}', fp);
      ch_written++;
      break;
    case ERL_BINARY:
	len = sprintf(buf , "#Bin");
        buf += len;
        ch_written += len;
	break;
    case ERL_INTEGER:
    case ERL_SMALL_BIG:
	len = sprintf(buf , "%d", ERL_INT_VALUE(ep));
        buf += len;
        ch_written += len;
	break;
    case ERL_U_INTEGER:
    case ERL_U_SMALL_BIG:
	len = sprintf(buf , "%d", ERL_INT_UVALUE(ep));
        buf += len;
        ch_written += len;
	break;
    case ERL_FLOAT:
	len = sprintf(buf , "%f", ERL_FLOAT_VALUE(ep));
        buf += len;
        ch_written += len;
	break;
    case ERL_FUNCTION:
	len = sprintf(buf , "#Fun<");
        buf += len;
        ch_written += len;
	ch_written += erl_sprint_term(fp, ERL_FUN_MODULE(ep));
	putc('.', fp);
	ch_written++;
	ch_written += erl_sprint_term(fp, ERL_FUN_INDEX(ep));
	putc('.', fp);
	ch_written++;
	ch_written += erl_sprint_term(fp, ERL_FUN_UNIQ(ep));
	putc('>', fp);
	ch_written++;
	break;
    default:
	ch_written = -10000;
	erl_err_msg("<ERROR> erl_sprint_term: Bad type of term !");
    }
    return ch_written;
}
#endif

static int print_string(FILE* fp, const ETERM* ep)
{
    int ch_written = 0; /* counter of written chars */
  
    putc('"', fp);
    ch_written++;
    while (ERL_IS_CONS(ep)) {
	int c = ERL_INT_VALUE(HEAD(ep));

	if (c >= ' ') {
	    putc(c, fp);
	    ch_written++;
	}
	else {
	    switch (c) {
	    case '\n': fputs("\\n", fp); ch_written += 2; break;
	    case '\r': fputs("\\r", fp); ch_written += 2; break;
	    case '\t': fputs("\\t", fp); ch_written += 2; break;
	    case '\v': fputs("\\v", fp); ch_written += 2; break;
	    case '\b': fputs("\\b", fp); ch_written += 2; break;
	    case '\f': fputs("\\f", fp); ch_written += 2; break;
		break;
	    default:
		ch_written += fprintf(fp, "\\%o", c);
		break;
	    }
	}
	ep = TAIL(ep);
    }
    putc('"', fp);
    ch_written++;
    return ch_written;
}

/*
 * Returns 1 if term is a list of printable character, otherwise 0.
 */

static int is_printable_list(const ETERM* term)
{
    while (ERL_TYPE(term) == ERL_LIST) {
	ETERM* head = HEAD(term);

	if (!ERL_IS_BYTE(head)) {
	    return 0;
	}
	if (ERL_INT_VALUE(head) < ' ') {
	    switch (ERL_INT_VALUE(head)) {
	    case '\n':
	    case '\r':
	    case '\t':
	    case '\v':
	    case '\b':
	    case '\f':
		break;
	    default:
		return 0;
	    }
	}
	term = TAIL(term);
    }

    return ERL_IS_EMPTY_LIST(term);
}

#endif

/*
 * Retrieves the bytes from an I/O list and copy into a buffer.
 *
 * NOTE! It is the responsibility of the caller to ensure that
 * that the buffer is big enough (typically by calling
 * erl_iolist_length()), and that the term is an I/O list.
 *
 *  ETERM* term;		Term to convert to bytes.
 *  char** bufp;		Pointer to pointer to buffer
 *				where the bytes should be stored.
 *				On return, the pointer will point beyond
 *				the last byte stored.
 */

static void iolist_to_buf(const ETERM* term, char** bufp)
{
    char* dest = *bufp;

    while (ERL_IS_CONS(term)) {
	ETERM* obj = HEAD(term);

	if (ERL_IS_BYTE(obj)) {
	    *dest++ = ERL_INT_VALUE(obj);
	} else if (ERL_IS_CONS(obj)) {
	    iolist_to_buf(obj, &dest);
	} else if (ERL_IS_BINARY(obj)) {
	    memcpy(dest, ERL_BIN_PTR(obj), ERL_BIN_SIZE(obj));
	    dest += ERL_BIN_SIZE(obj);
	} else {
	    /*
	     * Types have been checked by caller.
	     */
	  if (!ERL_IS_EMPTY_LIST(obj)) return;
	  /* ASSERT(ERL_IS_EMPTY_LIST(obj)); */
	}
	term = TAIL(term);
    }
    if (ERL_IS_BINARY(term)) {
	memcpy(dest, ERL_BIN_PTR(term), ERL_BIN_SIZE(term));
	dest += ERL_BIN_SIZE(term);
    } else {
	/*
	 * Types have been checked by caller.
	 */
      if (!ERL_IS_EMPTY_LIST(term)) return;
      /* ASSERT(ERL_IS_EMPTY_LIST(term));*/
    }
    *bufp = dest;
}

static char* strsave(const char *src)
{
    char * dest = malloc(strlen(src)+1);

    if (dest != NULL)
	strcpy(dest, src);
    return dest;
}


/*
 * Local Variables:
 * compile-command: "cd ..; ERL_TOP=/clearcase/otp/erts make -k"
 * End:
 */
