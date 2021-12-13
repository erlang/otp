/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2021. All Rights Reserved.
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

#ifndef ERL_PRINTF_FORMAT_H__
#define ERL_PRINTF_FORMAT_H__

#ifdef __WIN32__
#undef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <sys/types.h>
#include <stdarg.h>
#include <stdlib.h>

#include "erl_int_sizes_config.h"
#include "erl_printf.h"

#ifdef __WIN32__

typedef ULONGLONG ErlPfUWord64;
typedef LONGLONG ErlPfSWord64;
#if SIZEOF_VOID_P == 8
typedef ULONGLONG ErlPfUWord;
typedef LONGLONG ErlPfSWord;
#elif SIZEOF_INT == 4
typedef unsigned int ErlPfUWord;
typedef signed int ErlPfSWord;
#else
#error Found no appropriate type to use for 'ErlPfUWord' and 'ErlPfSWord'
#endif

#else /* !win */

#if SIZEOF_LONG == 8
typedef unsigned long ErlPfUWord64;
typedef long          ErlPfSWord64;
#elif SIZEOF_LONG_LONG == 8
typedef unsigned long long ErlPfUWord64;
typedef long long          ErlPfSWord64;
#else
#error Found no appropriate type to use for 'ErlPfUWord64' and 'ErlPfSWord64'
#endif

#if SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long ErlPfUWord;
typedef long          ErlPfSWord;
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int ErlPfUWord;
typedef int          ErlPfSWord;
#else
#error Found no appropriate type to use for 'ErlPfUWord', 'ErlPfSWord'
#endif

#endif

extern int erts_printf_format(fmtfn_t, void*, char*, va_list);

extern int erts_printf_char(fmtfn_t, void*, char);
extern int erts_printf_string(fmtfn_t, void*, char *);
extern int erts_printf_buf(fmtfn_t, void*, char *, size_t);
extern int erts_printf_pointer(fmtfn_t, void*, void *);
extern int erts_printf_uword(fmtfn_t, void*, char, int, int, ErlPfUWord);
extern int erts_printf_sword(fmtfn_t, void*, char, int, int, ErlPfSWord);
extern int erts_printf_uword64(fmtfn_t, void*, char, int, int, ErlPfUWord64);
extern int erts_printf_sword64(fmtfn_t, void*, char, int, int, ErlPfSWord64);
extern int erts_printf_double(fmtfn_t, void *, char, int, int, double);

typedef ErlPfUWord ErlPfEterm;

extern int (*erts_printf_eterm_func)(fmtfn_t, void*, ErlPfEterm, long);

#endif /* ERL_PRINTF_FORMAT_H__ */

