/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2005-2013. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

#ifndef ERL_PRINTF_FORMAT_H__
#define ERL_PRINTF_FORMAT_H__

#ifdef VXWORKS
#include <vxWorks.h>
#endif

#include <sys/types.h>
#include <stdarg.h>
#include <stdlib.h>

#include "erl_int_sizes_config.h"

#if SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long ErlPfUWord;
typedef long          ErlPfSWord;
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int ErlPfUWord;
typedef int          ErlPfSWord;
#elif SIZEOF_VOID_P == SIZEOF_LONG_LONG
typedef unsigned long long ErlPfUWord;
typedef long long          ErlPfSWord;
#else
#error Found no appropriate type to use for 'Eterm', 'Uint' and 'Sint'
#endif


typedef int (*fmtfn_t)(void*, char*, size_t);

extern int erts_printf_format(fmtfn_t, void*, char*, va_list);

extern int erts_printf_char(fmtfn_t, void*, char);
extern int erts_printf_string(fmtfn_t, void*, char *);
extern int erts_printf_buf(fmtfn_t, void*, char *, size_t);
extern int erts_printf_pointer(fmtfn_t, void*, void *);
extern int erts_printf_uword(fmtfn_t, void*, char, int, int, ErlPfUWord);
extern int erts_printf_sword(fmtfn_t, void*, char, int, int, ErlPfSWord);
extern int erts_printf_double(fmtfn_t, void *, char, int, int, double);

#ifdef HALFWORD_HEAP_EMULATOR
#  if SIZEOF_INT != 4
#    error Unsupported integer size for HALFWORD_HEAP_EMULATOR
#  endif
typedef unsigned int ErlPfEterm;
#else
typedef ErlPfUWord ErlPfEterm;
#endif

extern int (*erts_printf_eterm_func)(fmtfn_t, void*, ErlPfEterm, long, ErlPfEterm*);


#endif /* ERL_PRINTF_FORMAT_H__ */

