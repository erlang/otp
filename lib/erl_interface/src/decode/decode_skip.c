/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2002-2025. All Rights Reserved.
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
#include "decode_skip.h"
#include <limits.h>

int ei_skip_term(const char* buf, int* index)
{
    int n, ty;
    int term_cnt = 1;

    do {
        int dummy;
        ei_get_type(buf, index, &ty, &dummy);
        n = 0;
        switch (ty) {
        case ERL_ATOM_EXT:
            if (ei_decode_atom_as(buf, index, NULL, MAXATOMLEN_UTF8, (ERLANG_LATIN1|ERLANG_UTF8),
                                  NULL, NULL) < 0) return -1;
            break;
        case ERL_PID_EXT:
        case ERL_NEW_PID_EXT:
            if (ei_decode_pid(buf, index, NULL) < 0) return -1;
            break;
        case ERL_PORT_EXT:
        case ERL_NEW_PORT_EXT:
            if (ei_decode_port(buf, index, NULL) < 0) return -1;
            break;
        case ERL_NEW_REFERENCE_EXT:
        case ERL_NEWER_REFERENCE_EXT:
        case ERL_REFERENCE_EXT:
            if (ei_decode_ref(buf, index, NULL) < 0) return -1;
            break;
        case ERL_NIL_EXT:
            ++(*index);
            break;
        case ERL_LIST_EXT:
            if (ei_decode_list_header(buf, index, &n) < 0
                || n < 0 || n == INT_MAX) {
                return -1;
            }
            ++n;  // last tail term
            break;
        case ERL_STRING_EXT:
            if (ei_decode_string(buf, index, NULL) < 0) return -1;
            break;
        case ERL_SMALL_TUPLE_EXT:
        case ERL_LARGE_TUPLE_EXT:
            if (ei_decode_tuple_header(buf, index, &n) < 0) return -1;
            break;
        case ERL_MAP_EXT:
            if (ei_decode_map_header(buf, index, &n) < 0
                || n < 0 || n > INT_MAX / 2) {
                return -1;
            }
            n *= 2;
            break;
        case ERL_BINARY_EXT:
            if (ei_decode_binary(buf, index, NULL, NULL) < 0)
                return -1;
            break;
        case ERL_BIT_BINARY_EXT:
            if (ei_decode_bitstring(buf, index, NULL, NULL, NULL) < 0)
                return -1;
            break;
        case ERL_SMALL_INTEGER_EXT:
        case ERL_INTEGER_EXT:
            if (ei_decode_long(buf, index, NULL) < 0) return -1;
            break;
        case ERL_SMALL_BIG_EXT:
        case ERL_LARGE_BIG_EXT:
            if (ei_decode_big(buf, index, NULL) < 0) return -1;
            break;
        case ERL_FLOAT_EXT:
        case NEW_FLOAT_EXT:
            if (ei_decode_double(buf, index, NULL) < 0) return -1;
            break;
        case ERL_FUN_EXT:
        case ERL_NEW_FUN_EXT:
        case ERL_EXPORT_EXT:
            if (ei_decode_fun(buf, index, NULL) < 0) return -1;
            break;
        default:
            return -1;
        }
        // One term done, 'n' new terms left to skip
        term_cnt--;
        if (n < 0 || n > INT_MAX - term_cnt) {
            return -1;
        }
        term_cnt += n;
    } while (term_cnt > 0);

    return 0;
}

