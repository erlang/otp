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
#include <limits.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"


int ei_encode_string(char *buf, int *index, const char *p)
{
    size_t len = strlen(p);

    if (len >= INT_MAX) return -1;
    return ei_encode_string_len(buf, index, p, len);
}

int ei_encode_string_len(char *buf, int *index, const char *p, int len)
{
    char *s = buf + *index;
    char *s0 = s;
    int i;

    if (len == 0) {

      if (!buf) {
	s += 1;
      } else {
	put8(s,ERL_NIL_EXT);
      }

    } else if (len <= 0xffff) {

      if (!buf) {
	s += 3;
      } else {
	put8(s,ERL_STRING_EXT);
	put16be(s,len);
	memmove(s,p,len);	/* unterminated string */
      }
      s += len;

    } else {

      if (!buf) {
	s += 5 + (2*len) + 1;
      } else {
	/* strings longer than 65535 are encoded as lists */
	put8(s,ERL_LIST_EXT);
	put32be(s,len);

	for (i=0; i<len; i++) {
	  put8(s,ERL_SMALL_INTEGER_EXT);
	  put8(s,p[i]);
	}
	put8(s,ERL_NIL_EXT);
      }

    }

    *index += s-s0; 

    return 0; 
}

