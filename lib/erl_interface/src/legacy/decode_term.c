/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#include "eiext.h"
#include "putget.h"
#include "erl_interface.h"

/*
 * This file is actually part of the erl_interface library,
 * not the newer 'ei' library. The header file is still in "ei.h"
 */

/* FIXME: is this to be completed? */

#if (0)
int ei_decode_term(const char *buf, int *index, void *t)
{
  const char *s = buf + *index;
  const char *s0 = s;

  if (t) {
    ETERM *tmp;
    
    /* this decodes and advances s */
    if (!(tmp = erl_decode_buf((unsigned char **)&s))) return -1;

    *(ETERM **)t = tmp;
    *index += s - s0;

    return 0;
  }
  else {
    int tmpindex = *index;
    long ttype;
    int arity;
    int i;
    
    /* these are all the external types */
    switch ((ttype = get8(s))) {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
    case ERL_SMALL_BIG_EXT:
      return ei_decode_long(buf,index,NULL);

    case ERL_FLOAT_EXT:  
    case NEW_FLOAT_EXT:
      return ei_decode_double(buf,index,NULL);

    case ERL_ATOM_EXT:
      return ei_decode_atom(buf,index,NULL);

    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
      return ei_decode_ref(buf,index,NULL);
      
    case ERL_PORT_EXT:
      return ei_decode_port(buf,index,NULL);
      
    case ERL_PID_EXT:     
      return ei_decode_pid(buf,index,NULL);

    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
      if (ei_decode_tuple_header(buf,index,&arity) < 0)
	return -1;

      for (i=0; i<arity; i++) {
	if (ei_decode_term(buf,index,NULL)) {
	  /* restore possibly changed index before returning */
	  *index = tmpindex;
	  return -1;
	}
      }
      return 0;
      
    case ERL_STRING_EXT:
      return ei_decode_string(buf,index,NULL);

    case ERL_LIST_EXT:   
    case ERL_NIL_EXT:
      if (ei_decode_list_header(buf,index,&arity) < 0)
	return -1;
      
      if (arity) {
	for (i=0; i<arity; i++) {
	  if (ei_decode_term(buf,index,NULL) < 0) {
	    /* restore possibly changed index before returning */
	    *index = tmpindex;
	    return -1;
	  }
	}
	if (ei_decode_list_header(buf,index,&arity) < 0) {
	  *index = tmpindex;
	  return -1;
	}
      }
      return 0;

    case ERL_BINARY_EXT:     
      return ei_decode_binary(buf,index,NULL,NULL);
      
    case ERL_LARGE_BIG_EXT:
    default:
      break;
    }
  }
  
  return -1;
}
#else
int ei_decode_term(const char *buf, int *index, void *t)
{
  const char *s = buf + *index;
  const char *s0 = s;
  ETERM *tmp;

  /* this decodes and advances s */
  if (!(tmp = erl_decode_buf((unsigned char **)&s))) return -1;

  if (t) *(ETERM **)t = tmp;
  else erl_free_term(tmp);

  *index += s - s0;
  
  return 0;
}
#endif
