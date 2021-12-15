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

#define ERTS_LIT_OS_TYPE         0
#define ERTS_LIT_OS_VERSION      1
#define ERTS_LIT_DFLAGS_RECORD   2
#define ERTS_LIT_EMPTY_TUPLE     3
#define ERTS_LIT_ERL_FILE_SUFFIX 4

#define ERTS_NUM_GLOBAL_LITERALS 5

extern Eterm ERTS_GLOBAL_LIT_EMPTY_TUPLE;

Eterm* erts_alloc_global_literal(Uint index, Uint sz);
void erts_register_global_literal(Uint index, Eterm term);
Eterm erts_get_global_literal(Uint index);
ErtsLiteralArea* erts_get_global_literal_area(Uint index);

#endif
