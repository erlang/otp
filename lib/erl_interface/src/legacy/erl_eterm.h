/* 
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
#ifndef _ERL_ETERM_H
#define _ERL_ETERM_H

#ifndef SILENT
#include <stdio.h>
#endif

#include "portability.h"

#define ERL_MAX_COUNT     0xffffff
#define ERL_MAX ((1 << 27)-1)
#define ERL_MIN -(1 << 27)

/* FIXME should this be documented and in erl_interface.h ??? */
#define ERL_BIG_ARITY(x)     ((x)->uval.bigval.arity)
#define ERL_BIG_IS_NEG(x)    ((x)->uval.bigval.is_neg)
#define ERL_BIG_DIGITS(x)    ((x)->uval.bigval.digits)
#define ERL_BIG_DIGIT(x,i)   (ERL_BIG_DIGITS(x)[(i)])

/*
 * Typing checking macros.
 */

/* FIXME should this be documented and in erl_interface.h ??? */
#define ERL_IS_DEFINED(x)  (ERL_TYPE(x) != 0)
#define ERL_IS_COMPOUND(x) (ERL_TYPE(x) & ERL_COMPOUND)
#define ERL_IS_FUNCTION(x) (ERL_TYPE(x) == ERL_FUNCTION)
#define ERL_IS_BIG(x)      (ERL_TYPE(x) == ERL_BIG)


typedef struct _heapmark {
  unsigned long mark;      /* id */
  int size;                /* size of buffer */
  Erl_Heap *base;          /* points to start of buffer */
  Erl_Heap *cur;           /* points into buffer */
  struct _heapmark *prev;  /* previous heapmark */
} Erl_HeapMark;


void erl_mk_port_helper(ETERM* ep, unsigned number, unsigned int creation);
void erl_mk_pid_helper(ETERM*, unsigned,unsigned, unsigned int);
ETERM * __erl_mk_reference(ETERM*, const char *, size_t, unsigned int n[], unsigned int);
int erl_current_fix_desc(void);

#endif /* _ERL_ETERM_H */
