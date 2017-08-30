/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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


/*
 * Description:	
 *
 * Author: 	Rickard Green
 */

#ifndef FIXED_SIZE_INT_TYPES_H__
#define FIXED_SIZE_INT_TYPES_H__

#ifdef SIZEOF_CHAR
#  define SIZEOF_CHAR_SAVED__ SIZEOF_CHAR
#  undef SIZEOF_CHAR
#endif

#ifdef SIZEOF_SHORT
#  define SIZEOF_SHORT_SAVED__ SIZEOF_SHORT
#  undef SIZEOF_SHORT
#endif

#ifdef SIZEOF_INT
#  define SIZEOF_INT_SAVED__ SIZEOF_INT
#  undef SIZEOF_INT
#endif

#ifdef SIZEOF_LONG
#  define SIZEOF_LONG_SAVED__ SIZEOF_LONG
#  undef SIZEOF_LONG
#endif

#ifdef SIZEOF_LONG_LONG
#  define SIZEOF_LONG_LONG_SAVED__ SIZEOF_LONG_LONG
#  undef SIZEOF_LONG_LONG
#endif

#include "erl_int_sizes_config.h"

#ifdef SIZEOF_CHAR_SAVED__
#  if SIZEOF_CHAR != SIZEOF_CHAR_SAVED__
#     error char type size mismatch
#  endif
#  undef SIZEOF_CHAR_SAVED__
#endif

#ifdef SIZEOF_SHORT_SAVED__
#  if SIZEOF_SHORT != SIZEOF_SHORT_SAVED__
#     error short type size mismatch
#  endif
#  undef SIZEOF_SHORT_SAVED__
#endif

#ifdef SIZEOF_INT_SAVED__
#  if SIZEOF_INT != SIZEOF_INT_SAVED__
#     error int type size mismatch
#  endif
#  undef SIZEOF_INT_SAVED__
#endif

#ifdef SIZEOF_LONG_SAVED__
#  if SIZEOF_LONG != SIZEOF_LONG_SAVED__
#     error long type size mismatch
#  endif
#  undef SIZEOF_LONG_SAVED__
#endif

#ifdef SIZEOF_LONG_LONG_SAVED__
#  if SIZEOF_LONG_LONG != SIZEOF_LONG_LONG_SAVED__
#     error long long type size mismatch
#  endif
#  undef SIZEOF_LONG_LONG_SAVED__
#endif


#if SIZEOF_LONG == 8
#define HAVE_INT_64 1
typedef unsigned long		usgnd_int_64;
typedef signed   long		sgnd_int_64;
#define USGND_INT_64_FSTR	"lu"
#define SGND_INT_64_FSTR	"ld"
#elif SIZEOF_LONG_LONG == 8
#define HAVE_INT_64 1
typedef unsigned long long	usgnd_int_64;
typedef signed   long long	sgnd_int_64;
#define USGND_INT_64_FSTR	"llu"
#define SGND_INT_64_FSTR	"lld"
#else
#define HAVE_INT_64 0
#endif

#if SIZEOF_LONG == 4
typedef unsigned long		usgnd_int_32;
typedef signed   long		sgnd_int_32;
#define USGND_INT_32_FSTR	"lu"
#define SGND_INT_32_FSTR	"ld"
#elif SIZEOF_INT == 4
typedef unsigned int		usgnd_int_32;
typedef signed   int		sgnd_int_32;
#define USGND_INT_32_FSTR	"u"
#define SGND_INT_32_FSTR	"d"
#else
#error Found no appropriate type to use for 'usgnd_int_32' and 'sgnd_int_32'
#endif

#if SIZEOF_INT == 2
typedef unsigned int		usgnd_int_16;
typedef signed   int		sgnd_int_16;
#define USGND_INT_16_FSTR	"u"
#define SI_16_FSTR		"d"
#elif SIZEOF_SHORT == 2
typedef unsigned short		usgnd_int_16;
typedef signed   short		sgnd_int_16;
#define USGND_INT_16_FSTR	"u"
#define SGND_INT_16_FSTR	"d"
#else
#error Found no appropriate type to use for 'usgnd_int_16' and 'sgnd_int_16'
#endif

#if SIZEOF_CHAR == 1
typedef unsigned char		usgnd_int_8;
typedef signed char		sgnd_int_8;
#define USGND_INT_8_FSTR	"u"
#define SGND_INT_8_FSTR		"d"
#else
/* This should *never* happen! */
#error Found no appropriate type to use for 'usgnd_int_8' and 'sgnd_int_8'
#endif


#if HAVE_INT_64
typedef usgnd_int_64		usgnd_int_max;
typedef sgnd_int_64		sgnd_int_max;
#define USGND_INT_MAX_FSTR	USGND_INT_64_FSTR
#define SGND_INT_MAX_FSTR	SGND_INT_64_FSTR
#else
typedef usgnd_int_32		usgnd_int_max;
typedef sgnd_int_32		sgnd_int_max;
#define USGND_INT_MAX_FSTR	USGND_INT_32_FSTR
#define SGND_INT_MAX_FSTR	SGND_INT_32_FSTR
#endif

#endif /* #ifndef FIXED_SIZE_INT_TYPES_H__ */
