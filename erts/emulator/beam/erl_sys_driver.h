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
 */

/*
 * Include file for erlang driver writers.
 */

#ifndef __ERL_SYS_DRIVER_H__
#define __ERL_SYS_DRIVER_H__

#ifdef __ERL_DRIVER_H__
#error erl_sys_driver.h cannot be included after erl_driver.h
#endif

#define ERL_SYS_DRV

typedef long ErlDrvEvent; /* An event to be selected on. */

/* typedef struct _SysDriverOpts SysDriverOpts; defined in sys.h */

#include "erl_driver.h"

#endif




