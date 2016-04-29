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

int ei_decode_list_header(const char *buf, int *index, int *arity)
{
  const char *s = buf + *index;
  const char *s0 = s;

  switch (get8(s)) {
  case ERL_NIL_EXT:
    if (arity) *arity = 0;
    break;
    
  case ERL_LIST_EXT:
    if (arity) *arity = get32be(s);
    else s+= 4;
    break;

  default:
    return -1;
  }
  
  *index += s-s0; 

  return 0;
}
