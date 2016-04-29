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
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

/* c non-zero -> erlang "true" atom, otherwise "false" */
int ei_decode_boolean(const char *buf, int *index, int *p)
{
  char tbuf[6];
  int t;

  if (ei_decode_atom_as(buf, index, tbuf, sizeof(tbuf), ERLANG_ASCII, NULL, NULL) < 0)
      return -1;

  if (memcmp(tbuf, "true", 5) == 0)
      t = 1;
  else if (memcmp(tbuf, "false", 6) == 0)
      t = 0;
  else
      return -1;
      
  if (p) *p = t;
  return 0; 
}

