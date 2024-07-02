/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2021. All Rights Reserved.
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

#ifndef __ERL_GLOBAL_LITERALS_H__
#define __ERL_GLOBAL_LITERALS_H__

extern Eterm ERTS_GLOBAL_LIT_OS_TYPE;
extern Eterm ERTS_GLOBAL_LIT_OS_VERSION;
extern Eterm ERTS_GLOBAL_LIT_DFLAGS_RECORD;
extern Eterm ERTS_GLOBAL_LIT_ERL_FILE_SUFFIX;
extern Eterm ERTS_GLOBAL_LIT_EMPTY_TUPLE;

void init_global_literals(void);
Eterm *erts_global_literal_allocate(Uint sz, struct erl_off_heap_header ***ohp);
void erts_global_literal_register(Eterm *variable, Eterm *hp, Uint heap_size);
ErtsLiteralArea *erts_global_literal_iterate_area(ErtsLiteralArea *prev);

#endif
