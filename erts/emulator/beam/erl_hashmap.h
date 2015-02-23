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
#include "erl_term.h"

int hashmap_key_hash_cmp(Eterm* ap, Eterm* bp);

/* HASH */

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
