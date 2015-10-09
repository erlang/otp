/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2013. All Rights Reserved.
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

int ei_decode_port(const char *buf, int *index, erlang_port *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  
  if (get8(s) != ERL_PORT_EXT) return -1;

  if (p) {
    if (get_atom(&s, p->node, NULL) < 0) return -1;
    p->id = get32be(s) & 0x0fffffff /* 28 bits */;
    p->creation = get8(s) & 0x03;
  }
  else {
      if (get_atom(&s, NULL, NULL) < 0) return -1;
      s += 5;
  }
  
  *index += s-s0;
  
  return 0;
}
