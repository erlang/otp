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

#include "eidef.h"

#include <stddef.h>
#include <stdlib.h>

#include "erl_interface.h"
#include "erl_fix_alloc.h"
#include "erl_malloc.h"
#include "erl_internal.h"
#include "erl_eterm.h"
#include "ei_malloc.h"

void erl_init_malloc(Erl_Heap *hp, long heap_size) 
{
  erl_init_eterm_alloc();
} /* erl_init_malloc */

ETERM *erl_alloc_eterm(unsigned char type)
{
  ETERM *e;

  /* Use fix size allocator */
  if (!(e = (ETERM *) erl_eterm_alloc())) 
    erl_err_sys("<ERROR> erl_alloc_eterm: Failed to allocate more memory\n");

  ERL_HEADER(e)->count = 0;
  ERL_HEADER(e)->type  = type;
  return e;

} /* erl_alloc_eterm */

#define EXTERNAL 1
#define INTERNAL 0
#define COMPOUND     1
#define NOT_COMPOUND 0

static void _erl_free_term (ETERM *ep, int external, int compound);

/* 
 * Free a term, but don't deallocate it until
 * the reference counter triggers.
 */
void erl_free_term(ETERM *ep)
{
  _erl_free_term(ep, EXTERNAL, NOT_COMPOUND);
} /* erl_free_term */

/* 
 * Free a term regardless of its reference 
 * counter value. Use this when you have 
 * built compound terms such as lists or tuples.
 */

/*
 * FIXME is this true?!
 * Tearing down term structures no-matter-what is a horrible idea if
 * any term happens to be shared (with some other structure or even
 * with yourself).
 */

void erl_free_compound (ETERM *ep)
{
  _erl_free_term(ep, EXTERNAL, COMPOUND);
} /* erl_free_compound */


/*
** The actual free'ing is done here in _erl_free_term.
** It is by nature recursive, but does not recurse 
** on the CDR of a list, which makes it usable for large lists.
*/

/*
** Convenience macro, called for variables and lists,
** avoids deep recursions.
*/
#define RESTART(Eterm, External, Compound) 		\
do { 							\
    ETERM *sep;						\
    sep = (Eterm);					\
    external = (External);				\
    compound = (Compound);				\
    /* Clear header info */				\
    ERL_TYPE(ep)  = ERL_UNDEF;				\
    erl_eterm_free((unsigned int *) ep);		\
    ep = sep;						\
    goto restart;      	       			        \
} while(0)

#define FREE_AND_CLEAR(ptr)			\
do {						\
    erl_free(ptr);				\
    (ptr) = NULL;				\
} while (0)

static void erl_atom_free(Erl_Atom_data* p)
{
    erl_free(p->latin1);
    if (p->utf8 != p->latin1) {
	erl_free(p->utf8);
    }
    p->latin1 = NULL;
    p->utf8 = NULL;
    p->lenL = 0;
    p->lenU = 0;
}

static void _erl_free_term (ETERM *ep, int external, int compound)
{
restart:
    if (ep == NULL) 
	return;
    if (compound || ERL_NO_REF(ep)) {
	/* Yes, it's time to *really* free this one ! */
	switch(ERL_TYPE(ep)) 
	    {
	    case ERL_ATOM:
		erl_atom_free(&ep->uval.aval.d);
		break;
	    case ERL_VARIABLE:
		FREE_AND_CLEAR(ERL_VAR_NAME(ep));
		/* Note: It may be unbound ! */
		if (ERL_VAR_VALUE(ep) != NULL) {
		    ERL_COUNT(ERL_VAR_VALUE(ep))--;
		    /* Cleanup and Restart with the actual value */
		    RESTART(ERL_VAR_VALUE(ep), INTERNAL, compound);
		}
		break;
	    case ERL_LIST: 
		if (HEAD(ep)) {
		    ERL_COUNT(HEAD(ep))--;
		    /* FIXME added cast, is this correct? */
		    _erl_free_term((ETERM *)HEAD(ep), INTERNAL, compound);
		}
		if (TAIL(ep)) {
		    ERL_COUNT(TAIL(ep))--;
		    /* Clean up and walk on to CDR in list */
		    RESTART(TAIL(ep), INTERNAL, compound);
		}
		break;
	    case ERL_TUPLE: 
		{
		    int i;
		    for (i=0; i < ERL_TUPLE_SIZE(ep); i++) 
			if (ERL_TUPLE_ELEMENT(ep, i)) {
			    ERL_COUNT(ERL_TUPLE_ELEMENT(ep, i))--;
			    _erl_free_term(ERL_TUPLE_ELEMENT(ep, i), 
					   INTERNAL, compound);
			}
		    FREE_AND_CLEAR(ERL_TUPLE_ELEMS(ep));
		}
	    break;
	    case ERL_BINARY:
		FREE_AND_CLEAR(ERL_BIN_PTR(ep));
		break;
	    case ERL_PID:
		erl_atom_free(&ep->uval.pidval.node);
		break;
	    case ERL_PORT:
		erl_atom_free(&ep->uval.portval.node);
		break;
	    case ERL_REF:
		erl_atom_free(&ep->uval.refval.node);
		break;
	    case ERL_EMPTY_LIST:
	    case ERL_INTEGER:
	    case ERL_SMALL_BIG:
	    case ERL_U_SMALL_BIG:
	    case ERL_FLOAT:
		break;
	    case ERL_FUNCTION: 
		{
		    int i;

		    _erl_free_term(ERL_FUN_INDEX(ep), INTERNAL, compound);
		    _erl_free_term(ERL_FUN_UNIQ(ep), INTERNAL, compound);
		    _erl_free_term(ERL_FUN_CREATOR(ep), INTERNAL, compound);
		    _erl_free_term(ERL_FUN_MODULE(ep), INTERNAL, compound);
		    if (ERL_CLOSURE(ep) != NULL) {
			for (i = 0;  i < ERL_CLOSURE_SIZE(ep); i++) 
			    _erl_free_term(ERL_CLOSURE_ELEMENT(ep,i),
					   INTERNAL, compound);
		    }
		}
		break;
	    } /* switch */

	/* Clear header info for those cases where we are done */
	ERL_TYPE(ep)  = ERL_UNDEF;
	erl_eterm_free(ep);
    } else if (external) {
	ERL_COUNT(ep)--;
	external = INTERNAL;
	goto restart;
    }
} /* _erl_free_term */
#undef RESTART
#undef FREE_AND_CLEAR

void erl_free_array(ETERM **arr, int size)
{
  int i;

  for (i=0; i<size; i++)
    erl_free_term(arr[i]);

} /* erl_free_array */


void* erl_malloc (long size)
{
    void *res;
  
    if ((res =  ei_malloc(size)) == NULL)
	erl_err_sys("<ERROR> erl_malloc: Failed to allocate more memory");
    
    return res;
}

void* erl_realloc(void* orig, long size)
{
    void *res;
  
    if ((res = ei_realloc(orig, size)) == NULL)
	erl_err_sys("<ERROR> erl_realloc: Failed to allocate more memory");
    return res;
}

void erl_free (void *ptr)
{
    ei_free(ptr);
}
