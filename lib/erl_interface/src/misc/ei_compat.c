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
 *

 */
#include "ei.h"
#include "ei_internal.h"

#include <limits.h>

#ifndef EI_COMPAT
#  define EI_COMPAT UINT_MAX
#endif

static unsigned compat_rel = EI_COMPAT;

void
ei_set_compat_rel(unsigned rel)
{
    compat_rel = rel;
}

int ei_internal_use_21_bitstr_expfun(void)
{
    return compat_rel < 22;
}

