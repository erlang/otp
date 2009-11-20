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

#ifndef __DECL_H__
#define __DECL_H__

#if defined(__STDC__) || defined(_MSC_VER)
#define EXTERN_FUNCTION(t, f, x)  extern t f x
#define FUNCTION(t, f, x) t f x
#define _DOTS_ ...
#define _VOID_      void
#elif defined(__cplusplus)
#define EXTERN_FUNCTION(f, x) extern "C" { f x }
#define FUNCTION(t, f, x) t f x
#define _DOTS_ ...
#define _VOID_    void
#else
#define EXTERN_FUNCTION(t, f, x) extern t f (/*x*/)
#define FUNCTION(t, f, x) t f (/*x*/)
#define _DOTS_
#define _VOID_
#endif

/*
** Example of declarations
**
** EXTERN_FUNCTION(void, foo, (int, int, char));
** FUNCTION(void, bar, (int, char));
**
** struct funcs {
**    FUNCTION(int*, (*f1), (int, int));
**    FUNCTION(void, (*f2), (int, char));
**    FUNCTION(void, (*f3), (_VOID_));
**    FUNCTION(int,  (*f4), (char*, _DOTS_));
** };
**
*/

#endif
