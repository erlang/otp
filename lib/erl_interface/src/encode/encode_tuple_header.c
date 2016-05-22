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

int ei_encode_tuple_header(char *buf, int *index, int arity)
{
  char *s = buf + *index;
  char *s0 = s;
  
  if (arity < 0) return -1;

  if (arity <= 0xff) {
    if (!buf) s += 2;
    else {
      put8(s,ERL_SMALL_TUPLE_EXT);
      put8(s,arity);
    }
  }
  else {
    if (!buf) s += 5;
    else {
      put8(s,ERL_LARGE_TUPLE_EXT);
      put32be(s,arity);
    }
  }

  *index += s-s0; 

  return 0;
}

int ei_encode_map_header(char *buf, int *index, int arity)
{
  char *s = buf + *index;
  char *s0 = s;

  if (arity < 0) return -1;

  if (!buf) s += 5;
  else {
      put8(s,ERL_MAP_EXT);
      put32be(s,arity);
  }

  *index += s-s0;

  return 0;
}
