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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_global_literals.h"

struct literal {
    Eterm term;
    ErtsLiteralArea* area;
};

static struct literal literals[ERTS_NUM_GLOBAL_LITERALS];

Eterm* erts_alloc_global_literal(Uint index, Uint sz)
{
    ErtsLiteralArea* area;
    Uint area_sz;

    ASSERT(index < ERTS_NUM_GLOBAL_LITERALS);
    area_sz = sizeof(ErtsLiteralArea) + (sz-1)*sizeof(Eterm);
    area = erts_alloc(ERTS_ALC_T_LITERAL, area_sz);
    area->end = area->start + sz;
    literals[index].area = area;
    return area->start;
}

void erts_register_global_literal(Uint index, Eterm term)
{
    Eterm* start;

    ASSERT(index < ERTS_NUM_GLOBAL_LITERALS);
    start = literals[index].area->start;
    erts_set_literal_tag(&term, start, literals[index].area->end - start);
    literals[index].term = term;
}

Eterm erts_get_global_literal(Uint index)
{
    ASSERT(index < ERTS_NUM_GLOBAL_LITERALS);
    return literals[index].term;
}

ErtsLiteralArea* erts_get_global_literal_area(Uint index)
{
    ASSERT(index < ERTS_NUM_GLOBAL_LITERALS);
    return literals[index].area;
}
