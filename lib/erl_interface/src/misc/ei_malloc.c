/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
 *

 */

#include "eidef.h"

#include <stddef.h>
#include <stdlib.h>
#include "ei_malloc.h"

static ei_malloc_fun_t  ei_m_fun = (ei_malloc_fun_t) malloc;
static ei_realloc_fun_t ei_r_fun = (ei_realloc_fun_t) realloc;
static ei_free_fun_t    ei_f_fun = (ei_free_fun_t) free;


void* ei_malloc (long size)
{
  return ei_m_fun(size);
}

void* ei_realloc(void* orig, long size)
{
  return ei_r_fun(orig, size);
}

void ei_free (void *ptr)
{
  ei_f_fun(ptr);
}

void ei_set_malloc(ei_malloc_fun_t my_malloc,
		   ei_realloc_fun_t my_realloc,
		   ei_free_fun_t my_free)
{
    ei_m_fun = my_malloc;
    ei_r_fun = my_realloc;
    ei_f_fun = my_free;
}
