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
 * Module: safe_string.c
 * 
 * This is a bunch of generic string operation
 * that are safe regarding buffer overflow.
 *
 * All string functions terminate the process with an error message
 * on buffer overflow.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "safe_string.h"
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>


static void string_overflow_handler(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr,format,args);
    va_end(args);
    exit(1);
}

int vsn_printf(char* dst, size_t size, const char* format, va_list args)
{
    int ret = vsnprintf(dst, size, format, args);
    if (ret >= size || ret < 0) {
	string_overflow_handler("Buffer truncated '%s'\n",dst);
    }
    return ret;
}

int sn_printf(char* dst, size_t size, const char* format, ...)
{
    va_list args;
    int ret;
    va_start(args, format);
    ret = vsn_printf(dst,size,format,args);
    va_end(args);
    return ret;
}

int strn_cpy(char* dst, size_t size, const char* src)
{
    return sn_printf(dst,size,"%s",src);
}

int strn_cat(char* dst, size_t size, const char* src)
{
    return strn_catf(dst,size,"%s",src);
}

int strn_catf(char* dst, size_t size, const char* format, ...)
{
    int ret;
    va_list args;
#ifdef _GNU_SOURCE
    int len = strnlen(dst,size);
#else
    int len = strlen(dst);
#endif

    if (len >= size) {
	string_overflow_handler("Buffer already overflowed '%.*s'\n",
				size, dst);
    }
    va_start(args, format);
    ret = vsn_printf(dst+len, size-len, format, args);
    va_end(args);
    return len+ret;
}

char* find_str(const char* haystack, int hsize, const char* needle)
{
    int i = 0;
    int nsize = strlen(needle);
    hsize -= nsize - 1;
    for (i=0; i<hsize; i++) {
	if (haystack[i]==needle[0] && strncmp(haystack+i,needle,nsize)==0) {
	    return (char*)(haystack+i);
	}
    }
    return NULL;
}

#ifndef HAVE_MEMMOVE
void* memmove(void *dest, const void *src, size_t n)
{
    int i;
    if (src > dest) {
	for (i=0; i<n; i++) ((char*)dest)[i] = ((char*)src)[i];
    }
    else {
	for (i=(int)(n-1); i>=0; i--) ((char*)dest)[i] = ((char*)src)[i];
    }
    return dest;
}
#endif /* HAVE_MEMMOVE */

