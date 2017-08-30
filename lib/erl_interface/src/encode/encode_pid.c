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

int ei_encode_pid(char *buf, int *index, const erlang_pid *p)
{
  char* s = buf + *index;
  const char tag = (p->creation > 3) ? ERL_NEW_PID_EXT : ERL_PID_EXT;

  ++(*index); /* skip ERL_PID_EXT */
  if (ei_encode_atom_len_as(buf, index, p->node, strlen(p->node),
			    ERLANG_UTF8, ERLANG_LATIN1|ERLANG_UTF8) < 0)
      return -1;

  if (buf) {
    put8(s, tag);

    s = buf + *index;

    /* now the integers */
    put32be(s,p->num & 0x7fff); /* 15 bits */
    put32be(s,p->serial & 0x1fff); /* 13 bits */
    if (tag == ERL_PID_EXT) {
        put8(s,(p->creation & 0x03)); /* 2 bits */
    } else {
        put32be(s, p->creation); /* 32 bits */
    }
  }

  *index += 4 + 4 + (tag == ERL_PID_EXT ? 1 : 4);
  return 0;
}

