/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
 * Description:	CPU topology and related functionality
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_CPU_TOPOLOGY_H__
#define ERL_CPU_TOPOLOGY_H__

void
erts_pre_early_init_cpu_topology(int *max_dcg_p,
                                 int *max_rg_p,
				 int *conf_p,
				 int *onln_p,
				 int *avail_p);
void
erts_early_init_cpu_topology(int no_schedulers,
			     int *max_main_threads_p,
			     int max_reader_groups,
			     int *reader_groups_p,
                             int max_decentralized_counter_groups,
                             int *decentralized_counter_groups_p);
void erts_init_cpu_topology(void);


#define ERTS_INIT_SCHED_BIND_TYPE_SUCCESS		0
#define ERTS_INIT_SCHED_BIND_TYPE_NOT_SUPPORTED		1
#define ERTS_INIT_SCHED_BIND_TYPE_ERROR_NO_CPU_TOPOLOGY	2
#define ERTS_INIT_SCHED_BIND_TYPE_ERROR_BAD_TYPE	3

int erts_init_scheduler_bind_type_string(char *how);


#define ERTS_INIT_CPU_TOPOLOGY_OK			0
#define ERTS_INIT_CPU_TOPOLOGY_INVALID_ID		1
#define ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_RANGE		2
#define ERTS_INIT_CPU_TOPOLOGY_INVALID_HIERARCHY	3
#define ERTS_INIT_CPU_TOPOLOGY_INVALID_ID_TYPE		4
#define ERTS_INIT_CPU_TOPOLOGY_INVALID_NODES		5
#define ERTS_INIT_CPU_TOPOLOGY_MISSING_LID		6
#define ERTS_INIT_CPU_TOPOLOGY_NOT_UNIQUE_LIDS		7
#define ERTS_INIT_CPU_TOPOLOGY_NOT_UNIQUE_ENTITIES	8
#define ERTS_INIT_CPU_TOPOLOGY_MISSING			9

int erts_init_cpu_topology_string(char *topology_str);

void erts_sched_check_cpu_bind(ErtsSchedulerData *esdp);
void erts_sched_init_check_cpu_bind(ErtsSchedulerData *esdp);
void erts_sched_check_cpu_bind_prep_suspend(ErtsSchedulerData *esdp);
void erts_sched_check_cpu_bind_post_suspend(ErtsSchedulerData *esdp);

int erts_update_cpu_info(void);

Eterm erts_bind_schedulers(Process *c_p, Eterm how);
Eterm erts_get_schedulers_binds(Process *c_p);

Eterm erts_get_reader_groups_map(Process *c_p);
Eterm erts_get_decentralized_counter_groups_map(Process *c_p);

Eterm erts_set_cpu_topology(Process *c_p, Eterm term);
Eterm erts_get_cpu_topology_term(Process *c_p, Eterm which);

int erts_update_cpu_info(void);
void erts_get_logical_processors(int *conf, int *onln, int *avail);

int erts_sched_bind_atthrcreate_prepare(void);
int erts_sched_bind_atthrcreate_child(int unbind);
void erts_sched_bind_atthrcreate_parent(int unbind);

int erts_sched_bind_atfork_prepare(void);
int erts_sched_bind_atfork_child(int unbind);
void erts_sched_bind_atfork_parent(int unbind);

Eterm erts_fake_scheduler_bindings(Process *p, Eterm how);
Eterm erts_debug_cpu_groups_map(Process *c_p, int groups);

typedef void (*erts_cpu_groups_callback_t)(int,
					   ErtsSchedulerData *,
					   int,
					   void *);

#endif
