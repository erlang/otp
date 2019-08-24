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
#ifndef _ERL_INTERNAL_H
#define _ERL_INTERNAL_H

/* 
 * Function: Some useful stuff not to be exported to users.
 */

#define HEAD(ep) ep->uval.lval.head
#define TAIL(ep) ep->uval.lval.tail
#define ERL_NO_REF(x) (ERL_COUNT(x) == 0)

#ifdef DEBUG
#define ASSERT(e) \
  if (e) { \
     ; \
  } else { \
     erl_assert_error(#e, __FILE__, __LINE__); \
  }

extern void erl_assert_error(char* expr, char* file, int line) 
	__attribute__ ((__noreturn__));

#else

#define ASSERT(e)

#endif

#endif /* _ERL_INTERNAL_H */
