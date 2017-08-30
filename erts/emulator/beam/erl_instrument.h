/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

#ifndef ERL_INSTRUMENT_H__
#define ERL_INSTRUMENT_H__

#include "erl_mtrace.h"

#define ERTS_INSTR_VSN 2

extern int erts_instr_memory_map;
extern int erts_instr_stat;

Uint  erts_instr_init(int stat, int map_stat);
int   erts_instr_dump_memory_map_to_fd(int fd);
int   erts_instr_dump_memory_map(const char *name);
Eterm erts_instr_get_memory_map(Process *process);
int   erts_instr_dump_stat_to_fd(int fd, int begin_max_period);
int   erts_instr_dump_stat(const char *name, int begin_max_period);
Eterm erts_instr_get_stat(Process *proc, Eterm what, int begin_max_period);
Eterm erts_instr_get_type_info(Process *proc);
Uint  erts_instr_get_total(void);
Uint  erts_instr_get_max_total(void);

#endif
