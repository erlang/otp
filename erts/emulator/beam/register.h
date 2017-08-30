/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
** Registered processes
*/

#ifndef __REGPROC_H__
#define __REGPROC_H__

#include "sys.h"
#include "hash.h"
#include "erl_process.h"
#define ERL_PORT_GET_PORT_TYPE_ONLY__
#include "erl_port.h"
#undef ERL_PORT_GET_PORT_TYPE_ONLY__

typedef struct reg_proc
{
    HashBucket bucket;  /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    Process *p;         /* The process registered (only one of this and
			   'pt' is non-NULL */
    Port *pt;		/* The port registered */
    Eterm name;         /* Atom name */
} RegProc;

int process_reg_size(void);
int process_reg_sz(void);
void init_register_table(void);
void register_info(int, void *);
int erts_register_name(Process *, Eterm, Eterm);
Eterm erts_whereis_name_to_id(Process *, Eterm);
void erts_whereis_name(Process *, ErtsProcLocks,
		       Eterm, Process**, ErtsProcLocks, int,
		       Port**, int);
Process *erts_whereis_process(Process *,
			      ErtsProcLocks,
			      Eterm,
			      ErtsProcLocks,
			      int);
int erts_unregister_name(Process *, ErtsProcLocks, Port *, Eterm);

#endif
