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
 * Function: Provides two primitives: erl_format to build
 *    Erlang terms in an easy way, and erl_match to perform
 *    pattern match similar to what is done in Erlang.
 * 
 */

#include "eidef.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#ifdef VRTX
#define __READY_EXTENSIONS__
#include <errno.h>
#endif
#include "erl_interface.h"
#include "erl_eterm.h"
#include "erl_malloc.h"
#include "erl_error.h"
#include "erl_internal.h"

#define ERL_TRUE 1
#define ERL_FALSE 0
#define ERL_OK 0
#define ERL_FORMAT_ERROR -1

#define ERL_MAX_ENTRIES 255      /* Max entries in a tuple/list term */
#define ERL_MAX_NAME_LENGTH 255  /* Max length of variable names */

#define PRINT(t) \
{ \
    print_term(stderr,t); \
			    fprintf(stderr,"\n"); \
						    }


typedef struct lvar {
  ETERM *var;             
  struct lvar *next; 
} lvar;


/* Forward */
static ETERM *eformat(char**, va_list*);
static int ematch(ETERM*, ETERM*);

/* FIXME not thread safe */
struct _ef {
  lvar *chain;  /* Chain of local variables */
  lvar *idle;   /* Idle list of lvar's */
} ef;

/* Find local variable in term.
 */
static ETERM *find_lvar(char *name)
{
  lvar *tmp=ef.chain;

  while (tmp != NULL) {
    if (strcmp(tmp->var->uval.vval.name,name) == 0)
      return tmp->var->uval.vval.v;
    tmp = tmp->next;
  }
  return (ETERM *) NULL;

} /* find_lvar */

static void lvar_free(lvar *lv)
{
  lvar *tmp=ef.chain;

  /* Link in the chain into the idle list */
  if (ef.idle == NULL)
    ef.idle = lv;
  else {
    tmp = ef.idle;
    while (tmp->next != NULL)
      tmp = tmp->next;
    tmp->next = lv;
  }


  /* Clear out the variable information */
  tmp = lv;
  while (tmp != NULL) {
    tmp->var = (ETERM *) NULL;
    tmp = tmp->next;
  }

} /* lvar_free */

static lvar *lvar_alloc(void)
{
  lvar *tmp;
  
  if ((tmp = ef.idle) == NULL) {
    tmp = (lvar *) erl_malloc(sizeof(lvar));
  }
  else {
    tmp = ef.idle;
    ef.idle = tmp->next;
  }
  return tmp;

} /* lvar_alloc */ 

static void undo_bindings(void)
{
  lvar *tmp=ef.chain;

  while (tmp != NULL) {
    erl_free_term(tmp->var->uval.vval.v);
    tmp->var->uval.vval.v = (ETERM *) NULL;
    tmp = tmp->next;
  }

} /* undo_bindings */

static void release_chain(void)
{

  lvar_free(ef.chain);
  ef.chain = (lvar *) NULL;

} /* release_chain */

static void add_lvar(ETERM *t)
{
  lvar *lv;

  lv = lvar_alloc();
  lv->var = t;
  lv->next = ef.chain;
  ef.chain = lv;

} /* add_lvar */

static char *pvariable(char **fmt, char *buf)
{
  char *start=*fmt;
  char c;
  int len;
  
  while (1) {
    c = *(*fmt)++;
    if (isalnum((int) c) || (c == '_'))
      continue;
    else
      break;
  } 
  (*fmt)--;
  len = *fmt - start;
  memcpy(buf, start, len);
  buf[len] = 0;
  
  return buf;
  
} /* pvariable */

static char *patom(char **fmt, char *buf)
{
  char *start=*fmt;
  char c;
  int len;
  
  while (1) {
    c = *(*fmt)++;
    if (isalnum((int) c) || (c == '_') || (c == '@'))
      continue;
    else
      break;
  } 
  (*fmt)--;
  len = *fmt - start;
  memcpy(buf, start, len);
  buf[len] = 0;
  
  return buf;
  
} /* patom */

/* Check if integer or float
 */
static char *pdigit(char **fmt, char *buf)
{
  char *start=*fmt;
  char c;
  int len,dotp=0;
  
  while (1) {
    c = *(*fmt)++;
    if (isdigit((int) c))
      continue;
    else if (!dotp && (c == '.')) {
      dotp = 1;
      continue;
    }
    else
      break;
  } 
  (*fmt)--;
  len = *fmt - start;
  memcpy(buf, start, len);
  buf[len] = 0;
  
  return buf;
  
} /* pdigit */

static char *pstring(char **fmt, char *buf)
{
  char *start=++(*fmt); /* skip first quote */
  char c;
  int len;
  
  while (1) {
    c = *(*fmt)++;
    if (c == '"') {
      if (*((*fmt)-1) == '\\')
	continue;
      else
	break;
    } else 
      continue;
  } 
  len = *fmt - 1 - start; /* skip last quote */
  memcpy(buf, start, len);
  buf[len] = 0;
  
  return buf;
  
} /* pstring */

static char *pquotedatom(char **fmt, char *buf)
{
  char *start=++(*fmt); /* skip first quote */
  char c;
  int len;
  
  while (1) {
    c = *(*fmt)++;
    if (c == '\'') {
      if (*((*fmt)-1) == '\\')
	continue;
      else
	break;
    } else 
      continue;
  } 
  len = *fmt - 1 - start; /* skip last quote */
  memcpy(buf, start, len);
  buf[len] = 0;
  
  return buf;
  
} /* pquotedatom */


/* 
 * The format letters are:
 *   w  -  Any Erlang term
 *   a  -  An Atom
 *   b  -  A Binary
 *   s  -  A String
 *   i  -  An Integer
 *   f  -  A Float (double)
 */
static int pformat(char **fmt, va_list *pap, ETERM *v[], int size)
{
  int rc=ERL_OK;
  
  /* this next section hacked to remove the va_arg calls */
  switch (*(*fmt)++) {

  case 'w':
    v[size] = va_arg(*pap, ETERM*);  
    ERL_COUNT(v[size])++;
    break;
    
  case 'a':
    v[size] = erl_mk_atom(va_arg(*pap, char *));   
    break;
    
  case 's':
    v[size] = erl_mk_string(va_arg(*pap, char *));  
    break;
    
  case 'i':
    v[size] = erl_mk_int(va_arg(*pap, int));   
    break;
    
  case 'f':
    v[size] = erl_mk_float(va_arg(*pap, double));  
    break;

  case 'b': {
    char *sarg = va_arg(*pap, char *);
    v[size] = erl_mk_binary(sarg, strlen(sarg));
    break;
  }
    
  default:
    rc = ERL_FORMAT_ERROR;
    break;
  }

  return rc;

} /* pformat */

static int ptuple(char **fmt, va_list *pap, ETERM *v[], int size)
{
  int res=ERL_FORMAT_ERROR;

  switch (*(*fmt)++) {

  case '}':
    res = size;
    break;

  case ',':
    res = ptuple(fmt, pap, v, size);
    break;

  case '~': 

    if (pformat(fmt, pap, v, size) == ERL_OK) 
      res = ptuple(fmt, pap, v, ++size);
    else
      erl_err_msg("ptuple(1):  Wrong format sequence !");
    break;

  case ' ':
    return ptuple(fmt, pap, v, size);
    break;

  default: {
      (*fmt)--;
      if ((v[size++] = eformat(fmt, pap)) != (ETERM *) NULL)
	res = ptuple(fmt, pap, v, size);
      break;

      /*
	if (isupper(**fmt)) {
	v[size++] = erl_mk_var(pvariable(fmt, wbuf));
	res = ptuple(fmt, pap, v, size);
	}
	else if ((v[size++] = eformat(fmt, pap)) != (ETERM *) NULL)
	res = ptuple(fmt, pap, v, size);
	break;
	*/
    }

  } /* switch */

  return res;

} /* ptuple */


static int plist(char **fmt, va_list *pap, ETERM *v[], int size)
{
  int res=ERL_FORMAT_ERROR;

  switch (*(*fmt)++) {

  case ']':
    res = size;
    break;

  case ',':
    res = plist(fmt, pap, v, size);
    break;

  case '~': 

    if (pformat(fmt, pap, v, size) == ERL_OK) 
      res = plist(fmt, pap, v, ++size);
    else 
      erl_err_msg("plist(1):  Wrong format sequence !");
    break;

  case ' ':
    return plist(fmt, pap, v, size);
    break;

  default: {
      (*fmt)--;
      if ((v[size++] = eformat(fmt, pap)) != (ETERM *) NULL)
	res = plist(fmt, pap, v, size);
      break;

      /*
	if (isupper(**fmt)) {
	v[size++] = erl_mk_var(pvariable(fmt, wbuf));
	res = plist(fmt, pap, v, size);
	}
	else if ((v[size++] = eformat(fmt, pap)) != (ETERM *) NULL)
	res = plist(fmt, pap, v, size);
	break;
	*/
    }

  } /* switch */

  return res;

} /* plist */


static ETERM *eformat(char **fmt, va_list *pap)
{
  int size;
  ETERM *v[ERL_MAX_ENTRIES],*ep;

  switch (*(*fmt)++) {
  case '{':
    if ((size = ptuple(fmt, pap , v, 0)) != ERL_FORMAT_ERROR) {
      ep = erl_mk_tuple(v, size);
      erl_free_array(v, size);
      return ep;
    }
    else
      return (ETERM *) NULL;
    break;

  case '[':
    if (**fmt == ']') {
      (*fmt)++;
      return erl_mk_empty_list();
    } else if ((size = plist(fmt, pap , v, 0)) != ERL_FORMAT_ERROR) {
      ep = erl_mk_list(v, size);
      erl_free_array(v, size);
      return ep;
    } else
      return (ETERM *) NULL;
    break;

  case '$': /* char-value? */
    return erl_mk_int((int)(*(*fmt)++));
    break;

  case '~':
    if (pformat(fmt, pap, v, 0) == ERL_OK) {
      ep = erl_copy_term(v[0]);
      erl_free_term(v[0]);
      return ep;
    }
    break;

  case ' ':
    return eformat(fmt, pap);
    break;

    /* handle negative numbers too...
     * case '-':
     * {
     * ETERM *tmp;
     *     
     * tmp = eformat(fmt,pap);
     *     if (ERL_IS_INTEGER(tmp)) ERL_INT_VALUE(tmp) = -(ERL_INT_VALUE(tmp));
     * return tmp;
     * }
     * 
     * 
     * break;
     */
    
  default:
    {
      char wbuf[BUFSIZ];  /* now local to this function for reentrancy */

      (*fmt)--;
      if (islower((int)**fmt)) {         /* atom  ? */
	char *atom=patom(fmt, wbuf);
	return erl_mk_atom(atom);
      }
      else if (isupper((int)**fmt) || (**fmt == '_')) {
	char *var=pvariable(fmt, wbuf);
	return erl_mk_var(var);
      }
      else if (isdigit((int)**fmt)) {    /* integer/float ? */
	char *digit=pdigit(fmt, wbuf);
	if (strchr(digit,(int) '.') == NULL)
	  return erl_mk_int(atoi((const char *) digit));
	else
	  return erl_mk_float(atof((const char *) digit));
      }
      else if (**fmt == '"') {      /* string ? */
	char *string=pstring(fmt, wbuf);
	return erl_mk_string(string);
      }
      else if (**fmt == '\'') {     /* quoted atom ? */
	char *qatom=pquotedatom(fmt, wbuf);
	return erl_mk_atom(qatom); 
      }
    }
    break;

  }

  erl_err_msg("<ERROR> Syntax error in eformat, char was: %c !", **fmt);
  return (ETERM *) NULL;

} /* eformat */
      

ETERM *erl_format(char *fmt, ... )
{
  ETERM *res=NULL;
  va_list ap;

  va_start(ap, fmt);
  res = eformat(&fmt, &ap);
  va_end(ap);

  return res;
} /* erl_format */


/* 
 * Perform a pattern match between a pattern p and a term t. 
 * As a side effect bind any unbound variables in p.
 * Return true or false.
 */
static int ematch(ETERM *p, ETERM *t)
{
    unsigned int type_p;
    unsigned int type_t;
  ETERM *tmp;

  /* two NULLs are equal, one is not... */
  if (!p && !t) return ERL_TRUE;
  if (!p || !t) return ERL_FALSE;
  /*
   * ASSERT(p != NULL);
   * ASSERT(t != NULL);
   */

  type_p = ERL_TYPE(p);
  type_t = ERL_TYPE(t);

  if (type_t == ERL_VARIABLE) {
    if (t->uval.vval.v == NULL)
      return ERL_FALSE; /* Can't have an unbound variable here ! */
    else 
      t = t->uval.vval.v;
  }

  if (type_p != ERL_VARIABLE && type_p != type_t)
    return ERL_FALSE;

  switch (type_p) {

  case ERL_ATOM: {
      Erl_Atom_data* pa = &p->uval.aval.d;
      Erl_Atom_data* ta = &t->uval.aval.d;
      if (pa->utf8 && ta->utf8) {
	  return pa->lenU == ta->lenU && memcmp(pa->utf8, ta->utf8, pa->lenU)==0; 
      }
      else if (pa->latin1 && ta->latin1) {
	  return pa->lenL == ta->lenL && memcmp(pa->latin1, ta->latin1, pa->lenL)==0; 
      }
      else if (pa->latin1) {
	  return cmp_latin1_vs_utf8(pa->latin1, pa->lenL, ta->utf8, ta->lenU)==0;
      }
      else {
	  return cmp_latin1_vs_utf8(ta->latin1, ta->lenL, pa->utf8, pa->lenU)==0;
      }
  }
  case ERL_VARIABLE:
    if (strcmp(p->uval.vval.name, "_") == 0) /* anon. variable */
      return ERL_TRUE;
    else if ((tmp = find_lvar(p->uval.vval.name)) != (ETERM *) NULL) {
      /* v points to NULL in cases like erl_format("{X,X}") for the
	 second variable */
      if (p->uval.vval.v == NULL) 	  
	p->uval.vval.v = erl_copy_term(tmp); 
      return ematch(p->uval.vval.v, t);
    }
    else {
      /* check if the variable is bound already */
      if (p->uval.vval.v != NULL) {
	if (ematch(p->uval.vval.v, t) == ERL_TRUE ){
	  add_lvar(p);
	  return ERL_TRUE;
	} 
	else
	  return ERL_FALSE;
      }
      else {
	p->uval.vval.v = erl_copy_term(t);
	add_lvar(p);
	return ERL_TRUE;
      }
    }
    break;
	
  case ERL_PID:
    if ((strcmp(ERL_PID_NODE(p), ERL_PID_NODE(t)) == 0) &&
	(ERL_PID_NUMBER(p) == ERL_PID_NUMBER(t)) &&
	(ERL_PID_SERIAL(p) == ERL_PID_SERIAL(t)) &&
	(ERL_PID_CREATION(p) == ERL_PID_CREATION(t)))
      return ERL_TRUE;
    else
      return ERL_FALSE;
    break;
	
  case ERL_PORT:
    if ((strcmp(ERL_PORT_NODE(p), ERL_PORT_NODE(t)) == 0) &&
	(ERL_PORT_NUMBER(p) == ERL_PORT_NUMBER(t)) &&
	(ERL_PORT_CREATION(p) == ERL_PORT_CREATION(t)))
      return ERL_TRUE;
    else
      return ERL_FALSE;
    break;
	
  case ERL_REF: {
      int i, len;

      if (strcmp(ERL_REF_NODE(p), ERL_REF_NODE(t)) != 0 ||
	  ERL_REF_CREATION(p) != ERL_REF_CREATION(t))
	  return ERL_FALSE;

      /* FIXME: {len=1, n={42}} and {len=3, n={42, 17, 13}} tests equal. */
      len = ERL_REF_LEN(p);
      if (len > ERL_REF_LEN(t))
	  len = ERL_REF_LEN(t);

      for (i = 0; i < len; i++)
	  if (ERL_REF_NUMBERS(p)[i] != ERL_REF_NUMBERS(t)[i])
	  return ERL_FALSE;

      return ERL_TRUE;
      break;
  }
	
  case ERL_EMPTY_LIST:
    return ERL_TRUE;
	
  case ERL_LIST: 
    while (ERL_IS_CONS(p) && ERL_IS_CONS(t)) {
      if (ematch(p->uval.lval.head, t->uval.lval.head) == ERL_FALSE)
	return ERL_FALSE;
      p = p->uval.lval.tail;
      t = t ->uval.lval.tail;
    }
    return ematch(p, t);
	
  case ERL_TUPLE: 
    {
      int i;
      if (erl_size(p) != erl_size(t))
	return ERL_FALSE;
      else {
	for(i=0; i<erl_size(p); i++)
	  if (ematch(p->uval.tval.elems[i],t->uval.tval.elems[i]) == ERL_FALSE)
	    return ERL_FALSE;
	return ERL_TRUE;
      }
    }
  break;
	
  case ERL_BINARY: 
    {
      int i;
      if ((i = p->uval.bval.size) != t->uval.bval.size)
	return ERL_FALSE;
      else
	return (memcmp(p->uval.bval.b,t->uval.bval.b,i)==0) ? ERL_TRUE : ERL_FALSE;
    }
  break;
	
  case ERL_INTEGER:
    return (p->uval.ival.i == t->uval.ival.i) ? ERL_TRUE : ERL_FALSE;
    break;
	
  case ERL_SMALL_BIG:
  case ERL_U_SMALL_BIG:
    /* This case can't happend since it is impossible
     * to create a bignum from the C code.
     */
    return ERL_FALSE; 
    break;
	
  case ERL_FLOAT:
#if defined(VXWORKS) && CPU == PPC860
      {
	return (erl_fp_compare((unsigned *)&(p->uval.fval.f),
			       (unsigned *)&(t->uval.fval.f)) == 0) 
	    ? ERL_TRUE : ERL_FALSE;
      }
#else
    return (p->uval.fval.f == t->uval.fval.f) ? ERL_TRUE : ERL_FALSE;
#endif
    break;
  default:
    return ERL_FALSE;
    break;
  }
    
  /* erl_err_msg("ematch: Unknown type == %c\n", type_p);   */
  return ERL_FALSE;
    
} /* ematch */


int erl_match(ETERM *p, ETERM *t)
{
  int i; 

  if ((i = ematch(p, t)) == ERL_FALSE)
    undo_bindings();
  release_chain();
  return i;

} /* erl_match */


