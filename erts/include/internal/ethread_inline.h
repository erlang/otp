/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2014. All Rights Reserved.
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

#ifndef ETHREAD_INLINE_H__
#define ETHREAD_INLINE_H__

#define ETHR_GCC_COMPILER_FALSE 0 /* Not a gcc compatible compiler */
#define ETHR_GCC_COMPILER_TRUE 1 /* The GNU gcc compiler */
/* Negative integers for gcc compatible compilers */
#define ETHR_GCC_COMPILER_CLANG -1 /* The Clang gcc compatible compiler */
#define ETHR_GCC_COMPILER_ICC -2 /* The Intel gcc compatible compiler */
/* Line them up... */

/*
 * Unfortunately there is no easy and certain way of
 * detecting a true gcc compiler, since the compatible
 * ones all define the same defines as the true gnu-gcc...
 */
#if !defined(__GNUC__) && !defined(__GNUG__) 
#  define ETHR_GCC_COMPILER ETHR_GCC_COMPILER_FALSE
#elif defined(__clang__)
#  define ETHR_GCC_COMPILER ETHR_GCC_COMPILER_CLANG
#elif defined(__ICC) || defined(__INTEL_COMPILER)
#  define ETHR_GCC_COMPILER ETHR_GCC_COMPILER_ICC
#else
/* Seems to be the true gnu-gcc... */
#  define ETHR_GCC_COMPILER ETHR_GCC_COMPILER_TRUE
#endif

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
