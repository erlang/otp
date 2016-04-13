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


int ei_decode_atom(const char *buf, int *index, char *p)
{
    return ei_decode_atom_as(buf, index, p, MAXATOMLEN, ERLANG_LATIN1, NULL, NULL);
}

int ei_decode_atom_as(const char *buf, int *index, char* p, int destlen,
		      erlang_char_encoding want_enc,
		      erlang_char_encoding* was_encp,
		      erlang_char_encoding* res_encp)
{
    const char *s = buf + *index;
    const char *s0 = s;
    int len;
    erlang_char_encoding got_enc;
    
    switch (get8(s)) {
    case ERL_ATOM_EXT:
	len = get16be(s);
	got_enc = ERLANG_LATIN1;
	break;
    case ERL_SMALL_ATOM_EXT:
	len = get8(s);
	got_enc = ERLANG_LATIN1;
	break;
    case ERL_ATOM_UTF8_EXT:
	len = get16be(s);
	got_enc = ERLANG_UTF8;
	break;
    case ERL_SMALL_ATOM_UTF8_EXT:
	len = get8(s);
	got_enc = ERLANG_UTF8;
	break;
    default:
	return -1;
    }
    
    if ((want_enc & got_enc) || want_enc == ERLANG_ASCII) {
	int i, found_non_ascii = 0;
	if (len >= destlen)
	    return -1;
	for (i=0; i<len; i++) {
	    if (s[i] & 0x80) found_non_ascii = 1;
	    if (p) p[i] = s[i];
	}
	if (p) p[len] = 0;
	if (want_enc == ERLANG_ASCII && found_non_ascii) {
	    return -1;
	}
	if (res_encp) {
	    *res_encp = found_non_ascii ? got_enc : ERLANG_ASCII;
	}
    }
    else {
	int plen = (got_enc == ERLANG_LATIN1) ?
	  latin1_to_utf8(p, s, len, destlen-1, res_encp) :
	  utf8_to_latin1(p, s, len, destlen-1, res_encp);
	if (plen < 0) return -1;
	if (p) p[plen] = 0;
    }
    if (was_encp) {
	*was_encp = got_enc;
    }
    
    s += len;
    *index += s-s0;
    return 0;
}	


int utf8_to_latin1(char* dst, const char* src, int slen, int destlen,
		   erlang_char_encoding* res_encp)
{
    const char* const dst_start = dst;
    const char* const dst_end = dst + destlen;
    int found_non_ascii = 0;

    while (slen > 0) {
	if (dst >= dst_end) return -1;
	if ((src[0] & 0x80) == 0) {
	    if (dst_start) {
		*dst = *src;
	    }
	    ++dst;
	    ++src;
	    --slen;
	}
	else if (slen > 1 &&
		 (src[0] & 0xFE) == 0xC2 &&
		 (src[1] & 0xC0) == 0x80) {
	    if (dst_start) {
		*dst = (char) ((src[0] << 6) | (src[1] & 0x3F));
	    }
	    ++dst;
	    src += 2;
	    slen -= 2;
	    found_non_ascii = 1;
	}
	else return -1;
    }
    if (res_encp) {
	*res_encp = found_non_ascii ? ERLANG_LATIN1 : ERLANG_ASCII;
    }
    return dst - dst_start;
}

int latin1_to_utf8(char* dst, const char* src, int slen, int destlen,
		   erlang_char_encoding* res_encp)
{
    const char* const src_end = src + slen;
    const char* const dst_start = dst;
    const char* const dst_end = dst + destlen;
    int found_non_ascii = 0;

    while (src < src_end) {
	if (dst >= dst_end) return -1;
	if ((src[0] & 0x80) == 0) {
	    if (dst_start) {
		*dst = *src;
	    }
	    ++dst;
	}
	else {
	    if (dst_start) {
		unsigned char ch = *src;
		dst[0] = 0xC0 | (ch >> 6);
		dst[1] = 0x80 | (ch & 0x3F);
	    }
	    dst += 2;
	    found_non_ascii = 1;
	}
	++src;
    }
    if (res_encp) {
	*res_encp = found_non_ascii ? ERLANG_UTF8 : ERLANG_ASCII;
    }
    return dst - dst_start;
}



int ei_internal_get_atom(const char** bufp, char* p,
			 erlang_char_encoding* was_encp)
{
    int ix = 0;
    if (ei_decode_atom_as(*bufp, &ix, p, MAXATOMLEN_UTF8, ERLANG_UTF8, was_encp, NULL) < 0)
	return -1;
    *bufp += ix;
    return 0;
}


