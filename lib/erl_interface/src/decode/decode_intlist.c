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
/* since Erlang sends int-lists as either lists or strings, this
 * function can be used when the caller needs an array but doesn't
 * know which type to decode
 */
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_decode_intlist(const char *buf, int *index, long *a, int *count)
{
  const unsigned char *s = (const unsigned char *)(buf + *index);
  const unsigned char *s0 = s;
  int idx;
  int len;
  int i;

  switch (get8(s)) {
  case ERL_STRING_EXT:
    len = get16be(s);

    /* transfer and cast chars one at a time into array */
    if (a) {
      for (i=0; i<len; i++) {
	a[i] = (long)(s[i]);
      }
    }
    if (count) *count = len;
    s += len;
    break;

  case ERL_LIST_EXT:
    len = get32be(s);
    idx = 0;
    
    if (a) {
      for (i=0; i<len; i++) {
	if (ei_decode_long((char*)s,&idx,a+i) < 0) {
	  if (count) *count = i;
	  return -1;
	}
      }
    }
    else {
      for (i=0; i<len; i++) {
	if (ei_decode_long((char*)s,&idx,NULL) < 0) {
	  if (count) *count = i;
	  return -1;
	}
      }
    }

    if (count) *count = len;
    s += idx;
    break;

  default:
    return -1;
  }


  *index += s-s0; 

  return 0; 
}
