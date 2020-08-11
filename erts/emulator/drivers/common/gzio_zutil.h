/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

/* A replacement for zlib internal header file zutil.h.
 * Just the minimal things that our gzio.c seems to need.
 * We don't want to be dependant on some internal header file
 * that may change or not exist at all.
 */

#define local static
#define DEF_MEM_LEVEL 8
#define zmemcpy sys_memcpy

#if defined(MSDOS) || (defined(WINDOWS) && !defined(WIN32))
#  define OS_CODE  0x00
#endif

#ifdef AMIGA
#  define OS_CODE  0x01
#endif

#if defined(VAXC) || defined(VMS)
#  define OS_CODE  0x02
#  define F_OPEN(name, mode) \
     fopen((name), (mode), "mbc=60", "ctx=stm", "rfm=fix", "mrs=512")
#endif

#if defined(ATARI) || defined(atarist)
#  define OS_CODE  0x05
#endif

#ifdef OS2
#  define OS_CODE  0x06
#endif

#if defined(MACOS) || defined(TARGET_OS_MAC)
#  define OS_CODE  0x07
#endif

#ifdef TOPS20
#  define OS_CODE  0x0a
#endif

#ifdef WIN32
#  ifndef __CYGWIN__  /* Cygwin is Unix, not Win32 */
#    define OS_CODE  0x0b
#  endif
#endif

#ifdef __50SERIES /* Prime/PRIMOS */
#  define OS_CODE  0x0f
#endif

#ifndef OS_CODE
#  define OS_CODE  0x03  /* assume Unix */
#endif

