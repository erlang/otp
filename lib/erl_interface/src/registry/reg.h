/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#ifndef _REG_H
#define _REG_H

#include "ei.h"		/* Our public defines, types and declarations */
#include "hash.h"

#define EI_MNESIA_MODULE  "mnesia_registry"

#define EI_MNESIA_DUMP    "start_dump"
#define EI_MNESIA_WRITE   "write"
#define EI_MNESIA_DELETE  "delete"
#define EI_MNESIA_COMMIT  "commit"

#define EI_MNESIA_RESTORE "start_restore"
#define EI_MNESIA_SEND    "send_records"
#define EI_MNESIA_RECV    "restore"
#define EI_MNESIA_SIZE    "size"

#define EI_REG_TYPEMASK 0xf8 /* all but lowest bits */
#define ei_reg_typeof(r) (r->attr & EI_REG_TYPEMASK) 

ei_reg_obj *ei_reg_make(ei_reg *reg, int attr);

void ei_reg_free(ei_reg *reg, ei_reg_obj *obj);

#endif /* _REG_H */
