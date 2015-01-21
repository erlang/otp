/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2011. All Rights Reserved.
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


#ifndef __ERL_HASH_H__
#define __ERL_HASH_H__

#include "sys.h"

Eterm erts_hashmap_get(Eterm key, Eterm map);

/* erl_term.h stuff */
#define make_hashmap(x)		make_boxed((Eterm*)(x))
#define make_hashmap_rel 	make_boxed_rel
#define is_hashmap(x)		(is_boxed((x)) && is_hashmap_header(*boxed_val((x))))
#define is_hashmap_header(x)	(((x) & (_TAG_HEADER_MASK)) == _TAG_HEADER_HASHMAP)
#define hashmap_val(x)		_unchecked_boxed_val((x))
#define hashmap_val_rel(RTERM, BASE) hashmap_val(rterm2wterm(RTERM, BASE))

/* HASH */


#if defined(__GNUC__)
#define hashmap_bitcount(x) 	(Uint32) __builtin_popcount((unsigned int) (x))
#else
const Uint32 SK5 = 0x55555555, SK3 = 0x33333333;
const Uint32 SKF0 = 0xF0F0F0F, SKFF = 0xFF00FF;

/* CTPOP emulation */
Uint32 hashmap_bitcount(Uint32 map) {
    map -= (( map >> 1  ) & SK5 );
    map  = ( map & SK3  ) + (( map >> 2 ) & SK3 );
    map  = ( map & SKF0 ) + (( map >> 4 ) & SKF0);
    map +=   map >> 8;
    return ( map + ( map >> 16)) & 0x3F;
}
#endif

/* hamt nodes v2.0
 * 
 * node :: leaf | array | bitmap
 * head
 */

/* the head-node is a bitmap or array with an untagged size
 */
typedef struct hashmap_head_s {
    Eterm thing_word;
    Uint size;
    Eterm items[1];
} hashmap_head_t;
 
/* the bitmap-node
 * typedef struct hashmap_bitmap_node_s {
 *     Eterm thing_word;
 *     Eterm items[1];
 * } hashmap_bitmap_node_t;
 *
 * the array-node is a tuple
 * typedef struct hashmap_bitmap_node_s {
 *     Eterm thing_word; 
 *     Eterm items[1];
 * } hashmap_bitmap_node_t;
 *
 * the leaf-node
 * cons-cell
 */

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

#define MAP_HEADER_TAG_SZ                 (2)
#define MAP_HEADER_ARITY_SZ               (8)
#define MAP_HEADER_VAL_SZ                 (16)

#define MAP_HEADER_TAG_FLAT               (0x0)
#define MAP_HEADER_TAG_HAMT_NODE_BITMAP   (0x1)
#define MAP_HEADER_TAG_HAMT_HEAD_ARRAY    (0x2)
#define MAP_HEADER_TAG_HAMT_HEAD_BITMAP   (0x3)

#define MAP_HEADER_TYPE(Hdr)  (((Hdr) >> (_HEADER_ARITY_OFFS)) & (0x3))
#define MAP_HEADER_ARITY(Hdr) (((Hdr) >> (_HEADER_ARITY_OFFS + MAP_HEADER_TAG_SZ)) & (0xff))
#define MAP_HEADER_VAL(Hdr)   (((Hdr) >> (_HEADER_ARITY_OFFS + MAP_HEADER_TAG_SZ + MAP_HEADER_ARITY_SZ)) & (0xffff))

#define is_hashmap_header_head(x) ((MAP_HEADER_TYPE(x) & (0x2)))

#define MAKE_MAP_HEADER(Type,Arity,Val) \
    (_make_header(((((Uint16)(Val)) << MAP_HEADER_ARITY_SZ) | (Arity)) << MAP_HEADER_TAG_SZ | (Type) , _TAG_HEADER_HASHMAP))

#define MAP_HEADER_HAMT_HEAD_ARRAY \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_HEAD_ARRAY,0x1,0xffff)

#define MAP_HEADER_HAMT_HEAD_BITMAP(Bmp) \
    MAKE_MAP_HEADER(MAP_HEADER_TAG_HAMT_HEAD_BITMAP,0x1,Bmp)

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
