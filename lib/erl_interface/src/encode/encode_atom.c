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


static int verify_ascii_atom(const char* src, int slen);
static int verify_utf8_atom(const char* src, int slen);
static int is_latin1_as_utf8(const char *p, int len);

int ei_encode_atom(char *buf, int *index, const char *p)
{
    size_t len = strlen(p);

    if (len >= MAXATOMLEN)
	len = MAXATOMLEN - 1;
    return ei_encode_atom_len_as(buf, index, p, len, ERLANG_LATIN1, ERLANG_LATIN1);
}

int ei_encode_atom_len(char *buf, int *index, const char *p, int len)
{
    /* This function is documented to truncate at MAXATOMLEN (256) */ 
    if (len >= MAXATOMLEN)
	len = MAXATOMLEN - 1;
    return ei_encode_atom_len_as(buf, index, p, len, ERLANG_LATIN1, ERLANG_LATIN1);
}

int ei_encode_atom_as(char *buf, int *index, const char *p,
		      erlang_char_encoding from_enc,
		      erlang_char_encoding to_enc)
{	
    return ei_encode_atom_len_as(buf, index, p, strlen(p), from_enc, to_enc);
}

int ei_encode_atom_len_as(char *buf, int *index, const char *p, int len,
			  erlang_char_encoding from_enc,
			  erlang_char_encoding to_enc)
{
  char *s = buf + *index;
  char *s0 = s;
  int offs;

  if (len >= MAXATOMLEN && (from_enc & (ERLANG_LATIN1|ERLANG_ASCII))) {
      return -1;
  }

  if (to_enc == (ERLANG_LATIN1 | ERLANG_UTF8)) {
    if (from_enc == ERLANG_UTF8) {
      to_enc = is_latin1_as_utf8(p, len) ? ERLANG_LATIN1 : ERLANG_UTF8;
    }
    else {
      to_enc = from_enc;
    }
  }
  switch(to_enc) {
  case ERLANG_LATIN1:
      if (buf) {
	  put8(s,ERL_ATOM_EXT);
	  switch (from_enc) {
	  case ERLANG_UTF8:
	      len = utf8_to_latin1(s+2, p, len, MAXATOMLEN-1, NULL);
	      if (len < 0) return -1;
	      break;
	  case ERLANG_ASCII:
	      if (verify_ascii_atom(p, len) < 0) return -1;
	      memcpy(s+2, p, len);
	      break;
	  case ERLANG_LATIN1:
	      memcpy(s+2, p, len);
	      break;
	  default:
	      return -1;
	  }
	  put16be(s,len);
      }
      else {
	  s += 3;
	  if (from_enc == ERLANG_UTF8) {
	      len = utf8_to_latin1(NULL, p, len, MAXATOMLEN-1, NULL);
	      if (len < 0) return -1;
	  } else if (from_enc == ERLANG_ASCII)
	    if (verify_ascii_atom(p, len) < 0) return -1;
      }
      break;
      
  case ERLANG_UTF8:
      offs =  1 + 1;
      switch (from_enc) {
      case ERLANG_LATIN1:
	  if (len >= 256/2) offs++;
	  len = latin1_to_utf8((buf ? s+offs : NULL), p, len, MAXATOMLEN_UTF8-1, NULL);
	  break;
      case ERLANG_ASCII:
	  if (verify_ascii_atom(p, len) < 0) return -1;
	  if (buf) memcpy(s+offs,p,len);
	  break;
      case ERLANG_UTF8:
	  if (len >= 256) offs++;
	  if (verify_utf8_atom(p, len) < 0) return -1;
	  if (buf) memcpy(s+offs,p,len);
	  break;
      default:
	  return -1;
      }
      if (buf) {
	  if (offs == 2) {
	      put8(s, ERL_SMALL_ATOM_UTF8_EXT);
	      put8(s, len);
	  }
	  else {
	      put8(s, ERL_ATOM_UTF8_EXT);
	      put16be(s, len);
	  }
      }
      else s+= offs;
      break;

  default:
      return -1;
  }
  s += len;

  *index += s-s0; 

  return 0; 
}

int
ei_internal_put_atom(char** bufp, const char* p, int slen,
		     erlang_char_encoding to_enc)
{
    int ix = 0;
    if (ei_encode_atom_len_as(*bufp, &ix, p, slen, ERLANG_UTF8, to_enc) < 0)
	return -1;
    *bufp += ix;
    return 0;
}


static int verify_ascii_atom(const char* src, int slen)
{
    while (slen > 0) {
	if ((src[0] & 0x80) != 0) return -1;
	src++;
	slen--;
    }
    return 0;
}

static int verify_utf8_atom(const char* src, int slen)
{
    int num_chars = 0;

    while (slen > 0) {
	if (++num_chars >= MAXATOMLEN) return -1;
	if ((src[0] & 0x80) != 0) {
	    if ((src[0] & 0xE0) == 0xC0) {
		if (slen < 2 || (src[1] & 0xC0) != 0x80) return -1;
		src++;
		slen--;
	    }
	    else if ((src[0] & 0xF0) == 0xE0) {
		if (slen < 3 || (src[1] & 0xC0) != 0x80 || (src[2] & 0xC0) != 0x80) return -1;
		src += 2;
		slen -= 2;
	    }
	    else if ((src[0] & 0xF8) == 0xF0) {
		if (slen < 4 || (src[1] & 0xC0) != 0x80 || (src[2] & 0xC0) != 0x80 || (src[3] & 0xC0) != 0x80) return -1;
		src += 3;
		slen -= 3;
	    }
	    else return -1;
	}
	src++;
	slen--;
    }
    return 0;
}

/* Only latin1 code points in utf8 string?
 */
static int is_latin1_as_utf8(const char *p, int len)
{
  int i;
  for (i=0; i<len; i++) {
    if ((unsigned char)p[i] > 0xC3) return 0;
  }
  return 1;
}
