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


#ifndef HIPE_GC_H
#define HIPE_GC_H

#if defined(__sparc__)
#include "hipe_sparc_gc.h"
#endif
#if defined(__i386__)
#include "hipe_x86_gc.h"
#endif
#if defined(__x86_64__)
#include "hipe_amd64_gc.h"
#endif
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc_gc.h"
#endif
#if defined(__arm__)
#include "hipe_arm_gc.h"
#endif

#endif /* HIPE_GC_H */
