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
 *

 */
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

/* report type identifier from the start of the buffer */
/* for types with meaningful length attributes, return the length too.
   In other cases, return length 0 */

int ei_get_type(const char *buf, const int *index, int *type, int *len)
{
  const char *s = buf + *index;

  *type = get8(s);
  
  switch (*type) {
  case ERL_SMALL_ATOM_EXT:
  case ERL_SMALL_ATOM_UTF8_EXT:
    *type = ERL_ATOM_EXT;
  case ERL_SMALL_TUPLE_EXT:
    *len = get8(s);
    break;

  case ERL_ATOM_UTF8_EXT:
    *type = ERL_ATOM_EXT;
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
  case ERL_MAP_EXT:
  case ERL_BINARY_EXT:
  case ERL_BIT_BINARY_EXT:
    *len = get32be(s);
    break;
    
  case ERL_SMALL_BIG_EXT:
    *len = get8(s); /* #digit_bytes */
    break;

  case ERL_LARGE_BIG_EXT:
    *len = get32be(s); /* #digit_bytes */
    break;

  case ERL_NEW_PID_EXT:
      *type = ERL_PID_EXT;
      break;
  case ERL_NEW_PORT_EXT:
      *type = ERL_PORT_EXT;
      break;
  case ERL_NEWER_REFERENCE_EXT:
      *type = ERL_NEW_REFERENCE_EXT;
      break;
  default:
    *len = 0;
    break;
  }

  /* leave index unchanged */
  return 0;
}


