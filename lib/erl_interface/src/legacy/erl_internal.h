/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
