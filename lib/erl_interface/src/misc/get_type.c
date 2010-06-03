/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 *

 */
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

/* report type identifier from the start of the buffer */
/* for types with meaningful length attributes, return the length too.
   In other cases, return length 0 */

/* FIXME working on this one.... */

int ei_get_type(const char *buf, const int *index, int *type, int *len)
{
    return ei_get_type_internal(buf, index, type, len);
}

#if 0
int ei_get_type(const char *buf, const int *index, int *type, int *len)
{
    const char *s = buf + *index;
    int itype = get8(s);	/* Internal type */

    *len = 0;
  
    switch (*type) {

    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
	*type = EI_TYPE_INTEGER;
	break;

    case ERL_FLOAT_EXT:
	*type = EI_TYPE_FLOAT;
	break;

    case ERL_SMALL_TUPLE_EXT:
	*len  = get8(s);
	break;
    
    case ERL_ATOM_EXT:
    case ERL_STRING_EXT:
	*len  = get16be(s);
	break;
    
    case ERL_LARGE_TUPLE_EXT:
    case ERL_LIST_EXT:
    case ERL_BINARY_EXT:
	*len  = get32be(s);
	break;
    
    case ERL_SMALL_BIG_EXT:
	*len  = (get8(s)+1)/2; /* big arity */
	break;

    case ERL_LARGE_BIG_EXT:
	*len  = (get32be(s)+1)/2; /* big arity */
	break;

    case ERL_BINARY_EXT:
	*type = EI_TYPE_BINARY;
	break;

    case ERL_PID_EXT:
	*type = EI_TYPE_PID;
	break;

    case ERL_PORT_EXT:
	*type = EI_TYPE_PORT;
	break;

    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
	*type = EI_TYPE_REF;
	break;

    default:
	break;
    }

    /* leave index unchanged */
    return 0;
}
#endif


/* Old definition of function above */
   
int ei_get_type_internal(const char *buf, const int *index,
			 int *type, int *len)
{
  const char *s = buf + *index;

  *type = get8(s);
  
  switch (*type) {
  case ERL_SMALL_TUPLE_EXT:
    *len = get8(s);
    break;
    
  case ERL_ATOM_EXT:
  case ERL_STRING_EXT:
    *len = get16be(s);
    break;

  case ERL_FLOAT_EXT:
  case NEW_FLOAT_EXT:
    *type = ERL_FLOAT_EXT;
    break;

  case ERL_LARGE_TUPLE_EXT:
  case ERL_LIST_EXT:
  case ERL_BINARY_EXT:
    *len = get32be(s);
    break;
    
  case ERL_SMALL_BIG_EXT:
    *len = get8(s); /* #digit_bytes */
    break;

  case ERL_LARGE_BIG_EXT:
    *len = get32be(s); /* #digit_bytes */
    break;

  default:
    *len = 0;
    break;
  }

  /* leave index unchanged */
  return 0;
}


