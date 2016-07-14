/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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


#ifndef __ERL_MAP_H__
#define __ERL_MAP_H__

#include "sys.h"

/* instrinsic wrappers */
#if ERTS_AT_LEAST_GCC_VSN__(3, 4, 0)
#define hashmap_clz(x)       ((Uint32) __builtin_clz((unsigned int)(x)))
#define hashmap_bitcount(x)  ((Uint32) __builtin_popcount((unsigned int) (x)))
#else
Uint32 hashmap_clz(Uint32 x);
Uint32 hashmap_bitcount(Uint32 x);
#endif

/* MAP */

typedef struct flatmap_s {
    Eterm thing_word;
    Uint  size;
    Eterm keys;      /* tuple */
} flatmap_t;
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

/* the head-node is a bitmap or array with an untagged size */


#define hashmap_size(x)               (((hashmap_head_t*) hashmap_val(x))->size)
#define hashmap_make_hash(Key)        make_internal_hash(Key)

#define hashmap_restore_hash(Heap,Lvl,Key) \
    (((Lvl) < 8) ? hashmap_make_hash(Key) >> (4*(Lvl)) : hashmap_make_hash(CONS(Heap, make_small((Lvl)>>3), (Key))) >> (4*((Lvl) & 7)))
#define hashmap_shift_hash(Heap,Hx,Lvl,Key) \
    (((++(Lvl)) & 7) ? (Hx) >> 4 : hashmap_make_hash(CONS(Heap, make_small((Lvl)>>3), Key)))


/* erl_term.h stuff */
#define flatmap_get_values(x)        (((Eterm *)(x)) + sizeof(flatmap_t)/sizeof(Eterm))
#define flatmap_get_keys(x)          (((Eterm *)tuple_val(((flatmap_t *)(x))->keys)) + 1)
#define flatmap_get_size(x)          (((flatmap_t*)(x))->size)

#ifdef DEBUG
#define MAP_SMALL_MAP_LIMIT    (3)
#else
#define MAP_SMALL_MAP_LIMIT    (32)
#endif

struct ErtsWStack_;
struct ErtsEStack_;

Eterm  erts_maps_put(Process *p, Eterm key, Eterm value, Eterm map);
int    erts_maps_update(Process *p, Eterm key, Eterm value, Eterm map, Eterm *res);
int    erts_maps_remove(Process *p, Eterm key, Eterm map, Eterm *res);
int    erts_maps_take(Process *p, Eterm key, Eterm map, Eterm *res, Eterm *value);

Eterm  erts_hashmap_insert(Process *p, Uint32 hx, Eterm key, Eterm value,
			   Eterm node, int is_update);
int    erts_hashmap_insert_down(Uint32 hx, Eterm key, Eterm node, Uint *sz,
			        Uint *upsz, struct ErtsEStack_ *sp, int is_update);
Eterm  erts_hashmap_insert_up(Eterm *hp, Eterm key, Eterm value,
			      Uint *upsz, struct ErtsEStack_ *sp);

int    erts_validate_and_sort_flatmap(flatmap_t* map);
void   hashmap_iterator_init(struct ErtsWStack_* s, Eterm node, int reverse);
Eterm* hashmap_iterator_next(struct ErtsWStack_* s);
Eterm* hashmap_iterator_prev(struct ErtsWStack_* s);
int    hashmap_key_hash_cmp(Eterm* ap, Eterm* bp);
Eterm  erts_hashmap_from_array(ErtsHeapFactory*, Eterm *leafs, Uint n, int reject_dupkeys);

#define erts_hashmap_from_ks_and_vs(F, KS, VS, N) \
    erts_hashmap_from_ks_and_vs_extra((F), (KS), (VS), (N), THE_NON_VALUE, THE_NON_VALUE);

Eterm erts_map_from_ks_and_vs(ErtsHeapFactory *factory, Eterm *ks, Eterm *vs, Uint n);
Eterm  erts_hashmap_from_ks_and_vs_extra(ErtsHeapFactory *factory,
                                         Eterm *ks, Eterm *vs, Uint n,
					 Eterm k, Eterm v);

const Eterm *erts_maps_get(Eterm key, Eterm map);

const Eterm *erts_hashmap_get(Uint32 hx, Eterm key, Eterm map);

/* hamt nodes v2.0
 *
 * node :: leaf | array | bitmap
 * head
 */
typedef struct hashmap_head_s {
    Eterm thing_word;
    Uint size;
    Eterm items[1];
} hashmap_head_t;

/* thing_word tagscheme
 * Need two bits for map subtags
 *
 * Original HEADER representation:
 *
 *     aaaaaaaaaaaaaaaa aaaaaaaaaatttt00       arity:26, tag:4
 *
 * For maps we have:
 *
 *     vvvvvvvvvvvvvvvv aaaaaaaamm111100       val:16, arity:8, mtype:2
 *
 * unsure about trailing zeros
 *
 * map-tag:
 *     00 - flat map tag (non-hamt) -> val:16 = #items
 *     01 - map-node bitmap tag     -> val:16 = bitmap
 *     10 - map-head (array-node)   -> val:16 = 0xffff
 *     11 - map-head (bitmap-node)  -> val:16 = bitmap
 */

/* erl_map.h stuff */

#define is_hashmap_header_head(x) ((MAP_HEADER_TYPE(x) & (0x2)))

#define MAKE_MAP_HEADER(Type,Arity,Val) \
    (_make_header(((((Uint16)(Val)) << MAP_HEADER_ARITY_SZ) | (Arity)) << MAP_HEADER_TAG_SZ | (Type) , _TAG_HEADER_MAP))

#define MAP_HEADER_FLATMAP \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_FLATMAP_HEAD,0x1,0x0)

#define MAP_HEADER_HAMT_HEAD_ARRAY \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_HEAD_ARRAY,0x1,0xffff)

#define MAP_HEADER_HAMT_HEAD_BITMAP(Bmp) \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_HEAD_BITMAP,0x1,Bmp)

#define MAP_HEADER_HAMT_NODE_BITMAP(Bmp) \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_NODE_BITMAP,0x0,Bmp)

#define MAP_HEADER_FLATMAP_SZ  (sizeof(flatmap_t) / sizeof(Eterm))

#define HAMT_NODE_ARRAY_SZ      (17)
#define HAMT_HEAD_ARRAY_SZ      (18)
#define HAMT_NODE_BITMAP_SZ(n)  (1 + n)
#define HAMT_HEAD_BITMAP_SZ(n)  (2 + n)

/* 2 bits maps tag + 4 bits subtag + 2 ignore bits */
#define _HEADER_MAP_SUBTAG_MASK       (0xfc)
/* 1 bit map tag + 1 ignore bit + 4 bits subtag + 2 ignore bits */
#define _HEADER_MAP_HASHMAP_HEAD_MASK (0xbc)

#define HAMT_SUBTAG_NODE_BITMAP  ((MAP_HEADER_TAG_HAMT_NODE_BITMAP << _HEADER_ARITY_OFFS) | MAP_SUBTAG)
#define HAMT_SUBTAG_HEAD_ARRAY   ((MAP_HEADER_TAG_HAMT_HEAD_ARRAY  << _HEADER_ARITY_OFFS) | MAP_SUBTAG)
#define HAMT_SUBTAG_HEAD_BITMAP  ((MAP_HEADER_TAG_HAMT_HEAD_BITMAP << _HEADER_ARITY_OFFS) | MAP_SUBTAG)
#define HAMT_SUBTAG_HEAD_FLATMAP ((MAP_HEADER_TAG_FLATMAP_HEAD << _HEADER_ARITY_OFFS) | MAP_SUBTAG)

#define hashmap_index(hash)      (((Uint32)hash) & 0xf)

/* hashmap heap size:
   [one cons cell + one list term in parent node] per key
   [one header + one boxed term in parent node] per inner node
   [one header + one size word] for root node
   Observed average number of nodes per key is about 0.35.
*/
#define HASHMAP_WORDS_PER_KEY 3
#define HASHMAP_WORDS_PER_NODE 2
#ifdef DEBUG
#  define HASHMAP_ESTIMATED_TOT_NODE_SIZE(KEYS) \
    (HASHMAP_WORDS_PER_NODE * (KEYS) * 3/10)   /* slightly under estimated */
#else
#  define HASHMAP_ESTIMATED_TOT_NODE_SIZE(KEYS) \
    (HASHMAP_WORDS_PER_NODE * (KEYS) * 4/10)   /* slightly over estimated */
#endif
#define HASHMAP_ESTIMATED_HEAP_SIZE(KEYS) \
        ((KEYS)*HASHMAP_WORDS_PER_KEY + HASHMAP_ESTIMATED_TOT_NODE_SIZE(KEYS))
#endif
