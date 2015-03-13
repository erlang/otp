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


#define hashmap_size(x) (((hashmap_head_t*) hashmap_val(x))->size)
#define hashmap_size_rel(RTERM, BASE) hashmap_size(rterm2wterm(RTERM, BASE))
#define hashmap_make_hash(Key) make_internal_hash(Key)

#define hashmap_restore_hash(Heap,Lvl,Key) \
    (((Lvl) < 8) ? hashmap_make_hash(Key) >> (4*(Lvl)) : hashmap_make_hash(CONS(Heap, make_small((Lvl)>>3), (Key))) >> (4*((Lvl) & 7)))
#define hashmap_shift_hash(Heap,Hx,Lvl,Key) \
    (((++(Lvl)) & 7) ? (Hx) >> 4 : hashmap_make_hash(CONS(Heap, make_small((Lvl)>>3), Key)))


/* erl_term.h stuff */
#define make_flatmap(x)		make_boxed((Eterm*)(x))
#define make_flatmap_rel(x, BASE)   make_boxed_rel((Eterm*)(x),(BASE))
#define is_flatmap(x)		(is_boxed((x)) && is_flatmap_header(*boxed_val((x))))
#define is_flatmap_rel(RTERM,BASE)  is_flatmap(rterm2wterm(RTERM,BASE))
#define is_not_flatmap(x)           (!is_flatmap((x)))
#define is_flatmap_header(x)	(((x) & (_TAG_HEADER_MASK)) == _TAG_HEADER_MAP)
#define header_is_flatmap(x)        ((((x) & (_HEADER_SUBTAG_MASK)) == MAP_SUBTAG))
#define flatmap_val(x)		(_unchecked_boxed_val((x)))
#define flatmap_val_rel(RTERM, BASE) flatmap_val(rterm2wterm(RTERM, BASE))

#define flatmap_get_values(x)      (((Eterm *)(x)) + 3)
#define flatmap_get_keys(x)        (((Eterm *)tuple_val(((flatmap_t *)(x))->keys)) + 1)
#define flatmap_get_size(x)        (((flatmap_t*)(x))->size)

#ifdef DEBUG
#define MAP_SMALL_MAP_LIMIT    (3)
#else
#define MAP_SMALL_MAP_LIMIT    (32)
#endif
#define MAP_HEADER             _make_header(1,_TAG_HEADER_MAP)
#define MAP_HEADER_SIZE        (sizeof(flatmap_t) / sizeof(Eterm))

struct ErtsWStack_;
struct ErtsEStack_;

Eterm  erts_maps_put(Process *p, Eterm key, Eterm value, Eterm map);
int    erts_maps_update(Process *p, Eterm key, Eterm value, Eterm map, Eterm *res);
int    erts_maps_remove(Process *p, Eterm key, Eterm map, Eterm *res);

Eterm  erts_hashmap_insert(Process *p, Uint32 hx, Eterm key, Eterm value,
			   Eterm node, int is_update);
int    erts_hashmap_insert_down(Uint32 hx, Eterm key, Eterm node, Uint *sz,
			        Uint *upsz, struct ErtsEStack_ *sp, int is_update);
Eterm  erts_hashmap_insert_up(Eterm *hp, Eterm key, Eterm value,
			      Uint *upsz, struct ErtsEStack_ *sp);

int    erts_validate_and_sort_flatmap(flatmap_t* map);
Uint   hashmap_over_estimated_heap_size(Uint n);
void   hashmap_iterator_init(struct ErtsWStack_* s, Eterm node, int reverse);
Eterm* hashmap_iterator_next(struct ErtsWStack_* s);
Eterm* hashmap_iterator_prev(struct ErtsWStack_* s);
int    hashmap_key_hash_cmp(Eterm* ap, Eterm* bp);
Eterm  erts_hashmap_from_array(ErtsHeapFactory*, Eterm *leafs, Uint n, int reject_dupkeys);
const  Eterm *erts_hashmap_get(Uint32 hx, Eterm key, Eterm map);

#define erts_hashmap_from_ks_and_vs(P, KS, VS, N) \
    erts_hashmap_from_ks_and_vs_extra((P), (KS), (VS), (N), THE_NON_VALUE, THE_NON_VALUE);

Eterm  erts_hashmap_from_ks_and_vs_extra(Process *p, Eterm *ks, Eterm *vs, Uint n,
					 Eterm k, Eterm v);

#if HALFWORD_HEAP
const Eterm *
erts_maps_get_rel(Eterm key, Eterm map, Eterm *map_base);
#  define erts_maps_get(A, B) erts_maps_get_rel(A, B, NULL)
#else
const Eterm *
erts_maps_get(Eterm key, Eterm map);
#  define erts_maps_get_rel(A, B, B_BASE) erts_maps_get(A, B)
#endif

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
    (_make_header(((((Uint16)(Val)) << MAP_HEADER_ARITY_SZ) | (Arity)) << MAP_HEADER_TAG_SZ | (Type) , _TAG_HEADER_HASHMAP))

#define MAP_HEADER_HAMT_HEAD_ARRAY \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_HEAD_ARRAY,0x1,0xffff)

#define MAP_HEADER_HAMT_HEAD_BITMAP(Bmp) \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_HEAD_BITMAP,0x1,Bmp)

#define MAP_HEADER_HAMT_NODE_ARRAY \
    make_arityval(16)

#define MAP_HEADER_HAMT_NODE_BITMAP(Bmp) \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_NODE_BITMAP,0x0,Bmp)

#define HAMT_HEAD_EMPTY_SZ     (2)
#define HAMT_NODE_ARRAY_SZ     (17)
#define HAMT_HEAD_ARRAY_SZ     (18)
#define HAMT_NODE_BITMAP_SZ(n) (1 + n)
#define HAMT_HEAD_BITMAP_SZ(n) (2 + n)

#define _HEADER_MAP_SUBTAG_MASK    (0xfc) /* 2 bits maps tag + 4 bits subtag + 2 ignore bits */
/* SUBTAG_NODE_ARRAY is in fact a tuple with 16 elements */
#define HAMT_SUBTAG_NODE_ARRAY   (((16 << _HEADER_ARITY_OFFS) | ARITYVAL_SUBTAG) & _HEADER_MAP_SUBTAG_MASK)
#define HAMT_SUBTAG_NODE_BITMAP  ((MAP_HEADER_TAG_HAMT_NODE_BITMAP << _HEADER_ARITY_OFFS) | HASHMAP_SUBTAG)
#define HAMT_SUBTAG_HEAD_ARRAY   ((MAP_HEADER_TAG_HAMT_HEAD_ARRAY << _HEADER_ARITY_OFFS) | HASHMAP_SUBTAG)
#define HAMT_SUBTAG_HEAD_BITMAP  ((MAP_HEADER_TAG_HAMT_HEAD_BITMAP << _HEADER_ARITY_OFFS) | HASHMAP_SUBTAG)

#define hashmap_index(hash)      (((Uint32)hash) & 0xf)


#endif
