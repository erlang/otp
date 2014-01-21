/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2014. All Rights Reserved.
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
 */


#ifndef __ERL_MAP_H__
#define __ERL_MAP_H__

#include "sys.h"
/* MAP */

typedef struct map_s {
    Eterm thing_word;
    Uint  size;
    Eterm keys;      /* tuple */
} map_t;
/* map node
 *
 * -----------
 * Eterm   THING	
 * Uint    size
 * Eterm   Keys -> {K1, K2, K3, ..., Kn} where n = size
 * ----
 * Eterm   V1
 * ...
 * Eterm   Vn, where n = size
 * -----------
 */



/* erl_term.h stuff */
#define make_map(x)		make_boxed((Eterm*)(x))
#define make_map_rel(x, BASE)   make_boxed_rel((Eterm*)(x),(BASE))
#define is_map(x)		(is_boxed((x)) && is_map_header(*boxed_val((x))))
#define is_map_rel(RTERM,BASE)  is_map(rterm2wterm(RTERM,BASE))
#define is_not_map(x)           (!is_map((x)))
#define is_map_header(x)	(((x) & (_TAG_HEADER_MASK)) == _TAG_HEADER_MAP)
#define header_is_map(x)        ((((x) & (_HEADER_SUBTAG_MASK)) == MAP_SUBTAG))
#define map_val(x)		(_unchecked_boxed_val((x)))
#define map_val_rel(RTERM, BASE) map_val(rterm2wterm(RTERM, BASE))

#define map_get_values(x)      (((Eterm *)(x)) + 3)
#define map_get_keys(x)        (((Eterm *)tuple_val(((map_t *)(x))->keys)) + 1)
#define map_get_size(x)        (((map_t*)(x))->size)

#define MAP_HEADER             _make_header(1,_TAG_HEADER_MAP)
#define MAP_HEADER_SIZE        (sizeof(map_t) / sizeof(Eterm))

Eterm erts_maps_put(Process *p, Eterm key, Eterm value, Eterm map);
int   erts_maps_update(Process *p, Eterm key, Eterm value, Eterm map, Eterm *res);
int   erts_maps_find(Eterm key, Eterm map, Eterm *value);
int   erts_maps_get(Eterm key, Eterm map, Eterm *value);
int   erts_maps_remove(Process *p, Eterm key, Eterm map, Eterm *res);
int   erts_validate_and_sort_map(map_t* map);
#endif

