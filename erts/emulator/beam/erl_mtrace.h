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

#ifndef ERL_MTRACE_H__
#define ERL_MTRACE_H__

#include "erl_alloc_types.h"

#if (defined(ERTS___AFTER_MORECORE_HOOK_CAN_TRACK_MALLOC) \
     || defined(ERTS_BRK_WRAPPERS_CAN_TRACK_MALLOC))
#undef ERTS_CAN_TRACK_MALLOC
#define ERTS_CAN_TRACK_MALLOC
#endif

#define ERTS_MTRACE_SEGMENT_ID ERTS_ALC_A_INVALID

extern int erts_mtrace_enabled;

void erts_mtrace_pre_init(void);
void erts_mtrace_init(char *receiver, char *nodename);
void erts_mtrace_install_wrapper_functions(void);
void erts_mtrace_stop(void);
void erts_mtrace_exit(Uint32 exit_value);

void erts_mtrace_crr_alloc(void*, ErtsAlcType_t, ErtsAlcType_t, Uint);
void erts_mtrace_crr_realloc(void*, ErtsAlcType_t, ErtsAlcType_t, void*, Uint);
void erts_mtrace_crr_free(ErtsAlcType_t, ErtsAlcType_t, void*);


void erts_mtrace_update_heap_size(void); /* Implemented in
					  * ../sys/common/erl_mtrace_sys_wrap.c
					  */

#endif /* #ifndef ERL_MTRACE_H__ */

