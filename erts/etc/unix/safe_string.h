/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
 * Module: safe_string.h
 * 
 * This is an interface to a bunch of generic string operation
 * that are safe regarding buffer overflow.
 *
 * All string functions terminate the process with an error message
 * on buffer overflow.
 */

#include <stdio.h>
#include <stdarg.h>

/* Like vsnprintf()
 */
int vsn_printf(char* dst, size_t size, const char* format, va_list args);

/* Like snprintf()
 */
int sn_printf(char* dst, size_t size, const char* format, ...);

/* Like strncpy()
 * Returns length of copied string.
 */
int strn_cpy(char* dst, size_t size, const char* src);

/* Almost like strncat()
 * size is sizeof entire dst buffer.
 * Returns length of resulting string.
 */
int strn_cat(char* dst, size_t size, const char* src);

/* Combination of strncat() and snprintf()
 * size is sizeof entire dst buffer.
 * Returns length of resulting string.
 */
int strn_catf(char* dst, size_t size, const char* format, ...);

/* Simular to strstr() but search size bytes of haystack
 * without regard to '\0' characters.
 */
char* find_str(const char* haystack, int size, const char* needle);

#ifndef HAVE_MEMMOVE
void* memmove(void *dest, const void *src, size_t n);
#endif

