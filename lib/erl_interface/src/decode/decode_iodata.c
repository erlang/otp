/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2020. All Rights Reserved.
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
    
static int decode_list_ext_iodata(const char *buf, int* index,
                                  int *szp, unsigned char **pp);

int ei_decode_iodata(const char *buf, int* index, int *szp, char *out_buf)
{
    int type, len;

    if (szp)
        *szp = 0;
    
    if (ei_get_type(buf, index, &type, &len) < 0)
        return -1;

    /* Top level of iodata is either a list or a binary... */
    
    switch (type) {

    case ERL_BINARY_EXT: {
        long llen;
        if (ei_decode_binary(buf, index, out_buf, &llen) < 0)
            return -1;
        if (llen != (long) len)
            return -1; /* general 64-bit issue with ei api... */
        if (szp)
            *szp += len;
        return 0;
    }

    case ERL_STRING_EXT:
        if (ei_decode_string(buf, index, out_buf) < 0)
            return -1;
        if (szp)
            *szp += len;
        return 0;

    case ERL_NIL_EXT:
        return ei_decode_list_header(buf, index, NULL);

    case ERL_LIST_EXT: {
        unsigned char *ptr = (unsigned char *) out_buf;
        len = 0;
        return decode_list_ext_iodata(buf, index,
                                      szp ? szp : &len,
                                      ptr ? &ptr : NULL);
    }

    default:
        return -1; /* Not a list or binary... */
    }
}
    
static int decode_list_ext_iodata(const char *buf, int* index,
                                  int *szp, unsigned char **pp)
{
    int type, len, i, conses;
    
    if (ei_decode_list_header(buf, index, &conses) < 0)
        return -1;
    
    for (i = 0; i <= conses; i++) {
        
        if (ei_get_type(buf, index, &type, &len) < 0)
            return -1;

        switch (type) {
        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT: {
            long val;
            if (i == conses)
                return -1; /* int not allowed in cdr of cons */
            if (ei_decode_long(buf, index, &val) < 0)
                return -1;
            if (val < 0 || 255 < val)
                return -1;
            if (pp)
                *((*pp)++) = (unsigned char) val;
            *szp += 1;
            break;
        }
        case ERL_BINARY_EXT: {
            void *p = pp ? *pp : NULL;
            long llen;
            if (ei_decode_binary(buf, index, p, &llen) < 0)
                return -1;
            if (llen != (long) len)
                return -1; /* general 64-bit issue with ei api... */
            if (pp)
                *pp += len;
            *szp += len;
            break;
        }
        case ERL_STRING_EXT: {
            void *p = pp ? *pp : NULL;
            if (ei_decode_string(buf, index, p) < 0)
                return -1;
            if (pp)
                *pp += len;
            *szp += len;
            break;
        }
        case ERL_LIST_EXT:
            if (decode_list_ext_iodata(buf, index, szp, pp) < 0)
                return -1;
            break;
        case ERL_NIL_EXT:
            if (ei_decode_list_header(buf, index, NULL) < 0)
                return -1;
            break;
        default:
            /* Not a list, binary, nor byte sized integer... */
            return -1;
        }
    }
    
    return 0;
}
    
