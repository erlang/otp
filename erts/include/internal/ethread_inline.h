/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2014. All Rights Reserved.
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

#ifndef ETHREAD_INLINE_H__
#define ETHREAD_INLINE_H__

#if !defined(__GNUC__)
#  define ETHR_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) 0
#elif !defined(__GNUC_MINOR__)
#  define ETHR_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) \
  ((__GNUC__ << 24) >= (((MAJ) << 24) | ((MIN) << 12) | (PL)))
#elif !defined(__GNUC_PATCHLEVEL__)
#  define ETHR_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) \
  (((__GNUC__ << 24) | (__GNUC_MINOR__ << 12)) >= (((MAJ) << 24) | ((MIN) << 12) | (PL)))
#else
#  define ETHR_AT_LEAST_GCC_VSN__(MAJ, MIN, PL) \
  (((__GNUC__ << 24) | (__GNUC_MINOR__ << 12) | __GNUC_PATCHLEVEL__) >= (((MAJ) << 24) | ((MIN) << 12) | (PL)))
#endif

#undef ETHR_INLINE
#if defined(__GNUC__)
#  define ETHR_INLINE __inline__
#  if ETHR_AT_LEAST_GCC_VSN__(3, 1, 1)
#    define ETHR_FORCE_INLINE __inline__ __attribute__((__always_inline__))
#  else
#    define ETHR_FORCE_INLINE __inline__
#  endif
#elif defined(__WIN32__)
#  define ETHR_INLINE __forceinline
#  define ETHR_FORCE_INLINE __forceinline
#endif

#endif /* #ifndef ETHREAD_INLINE_H__ */
