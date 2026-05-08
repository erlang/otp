/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2005-2025. All Rights Reserved.
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

#ifndef ERL_PRINTF_TERM_H__
#define ERL_PRINTF_TERM_H__

#include "sys.h"
#include "erl_printf_format.h"

#define PRINT_TERM_CURSOR_NORMAL   0
#define PRINT_TERM_CURSOR_STRING   1
#define PRINT_TERM_CURSOR_BINARY   2
#define PRINT_TERM_CURSOR_PRINTBIN 3
#define PRINT_TERM_CURSOR_BIGNUM   4
#define PRINT_TERM_CURSOR_DONE     5

typedef struct {
    UWord *wstart;
    UWord *wsp;
    UWord *wend;
    UWord *wdefault;
    int    alloc_type;
} ErtsPrintTermWStackState;

typedef struct {
    struct {
        UWord                 default_stack[16];
        ErtsPrintTermWStackState ws;
    }                  wstack;
    Eterm              obj;
    int                sub;
    Eterm             *str_nobj;
    byte              *bin_bytep;
    Uint               bin_bytesize;
    Uint               bin_bitoffs;
    Uint               bin_bitsize;
    int                bin_is_first;
    char              *big_str;
    Uint               big_pos;
    Uint               big_len;
} ErtsPrintTermCursor;

int erts_printf_term(fmtfn_t fn, void* arg, ErlPfEterm term, long precision);
void erts_print_term_cursor_init(ErtsPrintTermCursor *cur, Eterm root);
void erts_print_term_cursor_destroy(ErtsPrintTermCursor *cur);
int erts_print_term_step(fmtfn_t fn, void *arg,
                         ErtsPrintTermCursor *cur, long max_bytes);
#endif
