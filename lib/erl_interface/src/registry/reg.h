/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
