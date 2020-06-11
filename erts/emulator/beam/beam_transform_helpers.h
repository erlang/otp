/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020. All Rights Reserved.
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

#ifndef __BEAM_TRANSFORM_HELPERS__
#define __BEAM_TRANSFORM_HELPERS__

int beam_load_safe_mul(UWord a, UWord b, UWord* resp);
int beam_load_map_key_sort(LoaderState* stp, GenOpArg Size, GenOpArg* Rest);
Eterm beam_load_get_literal(LoaderState* stp, GenOpArg Key);
void beam_load_sort_select_vals(GenOpArg* base, size_t n);
#endif
