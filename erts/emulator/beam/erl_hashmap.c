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
 *
 * hashmaps are an adaption of Rich Hickeys Persistent HashMaps
 *   which were an adaption of Phil Bagwells - Hash Array Mapped Tries
 *
 * Author: Björn-Egil Dahlberg
 */
/*
 * Ls = lists:seq(1,188888).
 * A = lists:foldl(fun(I,O) -> hashmap:put(I,I,O) end, hashmap:new(), Ls).
 * lists:foreach(fun(I) -> io:format("looking up ~p got ~p~n", [I, hashmap:get(I, A)]), I = hashmap:get(I,A) end, Ls).
 *
 * lists:foldl(fun(I,O) -> hashmap:put(I,I,O) end, hashmap:new(), lists:seq(1,7)).
 * lists:foldl(fun(I,O) -> hashmap:info(O), hashmap:put(I,I,O) end, hashmap:new(), lists:seq(1,5)).
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"

#include "erl_hashmap.h"

#ifndef DECL_AM
#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)
#endif

static char *format_binary(Uint64 x, char *b) {
    int z;
    b[64] = '\0';
    for (z = 0; z < 64; z++) { 
	b[63-z] = ((x>>z) & 0x1) ? '1' : '0'; 
    }
    return b;
}

static Uint32 hashmap_shift_hash(Uint32 hx, Uint *lvl, Eterm key);
static Uint32 hashmap_restore_hash(Uint lvl, Eterm key);
static Eterm hashmap_insert(Process *p, Uint32 hx, Eterm key, Eterm value, Eterm node);
static const Eterm *hashmap_get(Uint32 hx, Eterm key, Eterm node);
static Eterm hashmap_to_list(Process *p, Eterm map);
static Eterm hashmap_bld_tuple_uint(Uint **hpp, Uint *szp, Uint n, Uint nums[]);

/* hashmap:new/0 */

BIF_RETTYPE hashmap_new_0(BIF_ALIST_0) {
    Eterm* hp;
    hashmap_head_t *head;

    hp   = HAlloc(BIF_P, HAMT_HEAD_EMPTY_SZ);
    head = (hashmap_head_t *) hp;

    head->thing_word = MAP_HEADER_HAMT_HEAD_BITMAP(0);
    head->size       = 0;

    BIF_RET(make_hashmap(head));
}

/* hashmap:put/3 */

BIF_RETTYPE hashmap_put_3(BIF_ALIST_3) {
    if (is_hashmap(BIF_ARG_3)) {
	Uint32 hx = make_hash2(BIF_ARG_1);
	Eterm map;

	map = hashmap_insert(BIF_P, hx, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
	ASSERT(is_hashmap(map));
	BIF_RET(map);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* hashmap:to_list/1 */

BIF_RETTYPE hashmap_to_list_1(BIF_ALIST_1) {
    if (is_hashmap(BIF_ARG_1)) {
	return hashmap_to_list(BIF_P, BIF_ARG_1);
    }

    BIF_ERROR(BIF_P, BADARG);
}

/* hashmap:get/2 */

BIF_RETTYPE hashmap_get_2(BIF_ALIST_2) {
    if (is_hashmap(BIF_ARG_2)) {
	const Eterm *value;
	Uint32 hx = make_hash2(BIF_ARG_1);
	
	if ((value = hashmap_get(hx, BIF_ARG_1, BIF_ARG_2)) != NULL) {
	    BIF_RET(*value);
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* hashmap:size/1 */

BIF_RETTYPE hashmap_size_1(BIF_ALIST_1) {
    if (is_hashmap(BIF_ARG_1)) {
	Eterm *head, *hp, res;
	Uint size, hsz=0;

	head = hashmap_val(BIF_ARG_1);
	size = head[1];
	(void) erts_bld_uint(NULL, &hsz, size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, size);
	BIF_RET(res);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* erlang:is_hashmap/1 */

BIF_RETTYPE is_hashmap_1(BIF_ALIST_1) {
    if (is_hashmap(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

/* impl. */

static const Eterm *hashmap_get(Uint32 hx, Eterm key, Eterm node) {
    Eterm *ptr, hdr;
    Uint ix,slot, lvl = 0;
    Uint32 hval,bp;

    for (;;) {
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST: /* LEAF NODE [K|V] */
		ptr = list_val(node);
		if (EQ(CAR(ptr), key)) {
		    return &(CDR(ptr));
		}
		return NULL;
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));

		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_NODE_ARRAY:
			ix   = hashmap_index(hx);
			hx   = hashmap_shift_hash(hx,&lvl,key);
			node = ptr[ix+1];
			break;
		    case HAMT_SUBTAG_HEAD_ARRAY:
			ix   = hashmap_index(hx);
			hx   = hashmap_shift_hash(hx,&lvl,key);
			node = ptr[ix+2];
			break;
		    case HAMT_SUBTAG_NODE_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
			bp   = 1 << ix;
			slot = hashmap_bitcount(hval & (bp - 1));

			/* occupied */
			if (bp & hval) {
			    hx    = hashmap_shift_hash(hx,&lvl,key);
			    node  = ptr[slot+1];
			    break;
			}
			/* not occupied */
			return NULL;
		    case HAMT_SUBTAG_HEAD_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
			bp   = 1 << ix;
			slot = hashmap_bitcount(hval & (bp - 1));

			/* occupied */
			if (bp & hval) {
			    hx    = hashmap_shift_hash(hx,&lvl,key);
			    node  = ptr[slot+2];
			    break;
			}
			/* not occupied */
			return NULL;
		    default:
			erl_exit(1, "bad header tag %ld\r\n", hdr & _HEADER_MAP_SUBTAG_MASK);
			break;
		}
		break;
	    default:
		erl_exit(1, "bad primary tag %p\r\n", node);
		break;
	}
    }
    return NULL;
}

static Eterm hashmap_insert(Process *p, Uint32 hx, Eterm key, Eterm value, Eterm node) {
    Eterm *hp = NULL, *nhp = NULL;
    Eterm *ptr;
    Eterm hdr,res,ckey,fake;
    Uint32 ix, cix, bp, hval, chx;
    Uint slot, lvl = 0, clvl;
    Uint size = 0, n = 0, update_size = 1;
    DECLARE_ESTACK(stack);

    for (;;) {
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST: /* LEAF NODE [K|V] */
		ptr  = list_val(node);
		ckey = CAR(ptr);
		if (EQ(ckey, key)) {
		    update_size = 0;
		    goto unroll;
		}
		goto insert_subnodes;
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));

		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_NODE_ARRAY:
			ix    = hashmap_index(hx);
			hx    = hashmap_shift_hash(hx,&lvl,key);
			size += HAMT_NODE_ARRAY_SZ;
			ESTACK_PUSH2(stack, ix, node);
			node  = ptr[ix+1];
			break;
		    case HAMT_SUBTAG_HEAD_ARRAY:
			ix    = hashmap_index(hx);
			hx    = hashmap_shift_hash(hx,&lvl,key);
			size += HAMT_HEAD_ARRAY_SZ;
			ESTACK_PUSH2(stack, ix, node);
			node  = ptr[ix+2];
			break;
		    case HAMT_SUBTAG_NODE_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
			bp   = 1 << ix;
			slot = hashmap_bitcount(hval & (bp - 1));
			n    = hashmap_bitcount(hval);

			ESTACK_PUSH(stack, n);
			ESTACK_PUSH3(stack, bp, slot, node);

			/* occupied */
			if (bp & hval) {
			    hx    = hashmap_shift_hash(hx,&lvl,key);
			    node  = ptr[slot+1];
			    ASSERT(HAMT_NODE_BITMAP_SZ(n) <= 17); 
			    size += HAMT_NODE_BITMAP_SZ(n);
			    break;
			}
			/* not occupied */
			size += HAMT_NODE_BITMAP_SZ(n+1);
			goto unroll;
		    case HAMT_SUBTAG_HEAD_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
			bp   = 1 << ix;
			slot = hashmap_bitcount(hval & (bp - 1));
			n    = hashmap_bitcount(hval);

			ESTACK_PUSH(stack, n);
			ESTACK_PUSH3(stack, bp, slot, node);

			/* occupied */
			if (bp & hval) {
			    hx    = hashmap_shift_hash(hx,&lvl,key);
			    node  = ptr[slot+2];
			    ASSERT(HAMT_HEAD_BITMAP_SZ(n) <= 18); 
			    size += HAMT_HEAD_BITMAP_SZ(n);
			    break;
			}
			/* not occupied */
			size += HAMT_HEAD_BITMAP_SZ(n+1);
			goto unroll;
		    default:
			erl_exit(1, "bad header tag %ld\r\n", hdr & _HEADER_MAP_SUBTAG_MASK);
			break;
		}
		break;
	    default:
		erl_exit(1, "bad primary tag %p\r\n", node);
		break;
	}
    }
insert_subnodes:
    clvl  = lvl;
    chx   = hashmap_restore_hash(clvl,ckey);
    size += HAMT_NODE_BITMAP_SZ(2);
    ix    = hashmap_index(hx);
    cix   = hashmap_index(chx);

    while (cix == ix) {
	ESTACK_PUSH(stack, 0);
	ESTACK_PUSH3(stack, 1 << ix, 0, MAP_HEADER_HAMT_NODE_BITMAP(0));
	size += HAMT_NODE_BITMAP_SZ(1);
	hx    = hashmap_shift_hash(hx,&lvl,key);
	chx   = hashmap_shift_hash(chx,&clvl,ckey);
	ix    = hashmap_index(hx);
	cix   = hashmap_index(chx);
    }
    ESTACK_PUSH3(stack, cix, ix, node);

unroll:
    size += 2;
    hp  = HAlloc(p, size);
    res = CONS(hp, key, value); hp += 2;

    do {
	node = ESTACK_POP(stack);
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST:
		ix   = (Uint32) ESTACK_POP(stack);
		cix  = (Uint32) ESTACK_POP(stack);

		nhp   = hp;
		*hp++ = MAP_HEADER_HAMT_NODE_BITMAP((1 << ix) | (1 << cix));
		if (ix < cix) {
		    *hp++ = res;
		    *hp++ = node;
		} else {
		    *hp++ = node;
		    *hp++ = res;
		}
		res = make_hashmap(nhp);
		break;
	    case TAG_PRIMARY_HEADER:
		/* subnodes, fake it */
		fake = node;
		node = make_boxed(&fake);
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));

		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_NODE_ARRAY:
			slot  = (Uint)   ESTACK_POP(stack);
			nhp   = hp;
			n     = HAMT_NODE_ARRAY_SZ;
			while(n--) { *hp++ = *ptr++; }
			nhp[slot+1] = res;
			res = make_hashmap(nhp);
			break;
		    case HAMT_SUBTAG_HEAD_ARRAY:
			slot  = (Uint)   ESTACK_POP(stack);
			nhp   = hp;
			n     = HAMT_HEAD_ARRAY_SZ - 2;
			*hp++ = MAP_HEADER_HAMT_HEAD_ARRAY; ptr++;
			*hp++ = (*ptr++) + update_size;
			while(n--) { *hp++ = *ptr++; }
			nhp[slot+2] = res;
			res = make_hashmap(nhp);
			break;
		    case HAMT_SUBTAG_NODE_BITMAP:
			slot  = (Uint)   ESTACK_POP(stack);
			bp    = (Uint32) ESTACK_POP(stack);
			n     = (Uint32) ESTACK_POP(stack);
			hval  = MAP_HEADER_VAL(hdr);
			nhp   = hp;
			*hp++ = MAP_HEADER_HAMT_NODE_BITMAP(hval | bp); ptr++;

			n -= slot;
			while(slot--) { *hp++ = *ptr++; }
			*hp++ = res;
			if (hval & bp) { ptr++; n--; }
			while(n--) { *hp++ = *ptr++; }

			if ((hval | bp) == 0xffff) {
			    *nhp = make_arityval(16);
			}
			res = make_hashmap(nhp);
			break;
		    case HAMT_SUBTAG_HEAD_BITMAP:
			slot  = (Uint)   ESTACK_POP(stack);
			bp    = (Uint32) ESTACK_POP(stack);
			n     = (Uint32) ESTACK_POP(stack);
			hval  = MAP_HEADER_VAL(hdr);
			nhp   = hp;
			*hp++ = MAP_HEADER_HAMT_HEAD_BITMAP(hval | bp); ptr++;
			*hp++ = (*ptr++) + update_size;

			n -= slot;
			while(slot--) { *hp++ = *ptr++; }
			*hp++ = res;
			if (hval & bp) { ptr++; n--; }
			while(n--) { *hp++ = *ptr++; }

			if ((hval | bp) == 0xffff) {
			    *nhp = MAP_HEADER_HAMT_HEAD_ARRAY;
			}
			res = make_hashmap(nhp);
			break;
		    default:
			erl_exit(1, "bad header tag %x\r\n", hdr & _HEADER_MAP_SUBTAG_MASK);
			break;
		}
		break;
	    default:
		erl_exit(1, "bad primary tag %x\r\n", primary_tag(node));
		break;
	}

    } while(!ESTACK_ISEMPTY(stack));

    DESTROY_ESTACK(stack);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);
    ERTS_HOLE_CHECK(p);
    return res;
}

static Eterm hashmap_to_list(Process *p, Eterm node) {
    Eterm *hp;
    Eterm res = NIL;
    Eterm *ptr, tup, hdr;
    Uint sz, n;
    DECLARE_ESTACK(stack);

    ptr = boxed_val(node);
    n   = (Uint)ptr[1];
    hp  = HAlloc(p, n * (2 + 3));
    ESTACK_PUSH(stack, node);
    do {
	node = ESTACK_POP(stack);
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST:
		ptr = list_val(node);
		tup = TUPLE2(hp, CAR(ptr), CDR(ptr)); hp += 3;
		res = CONS(hp, tup, res); hp += 2;
		break;
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));
		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_HEAD_ARRAY:
			ptr++;
		    case HAMT_SUBTAG_NODE_ARRAY:
			ptr++;
			sz = 16;
			while(sz--) { ESTACK_PUSH(stack, ptr[sz]); }
			break;
		    case HAMT_SUBTAG_HEAD_BITMAP:
			ptr++;
		    case HAMT_SUBTAG_NODE_BITMAP:
			sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
			ASSERT(sz < 17);
			ptr++;
			while(sz--) { ESTACK_PUSH(stack, ptr[sz]); }
			break;
		    default:
			erl_exit(1, "bad header\r\n");
			break;
		}
	}
    } while(!ESTACK_ISEMPTY(stack));

    DESTROY_ESTACK(stack);
    ERTS_HOLE_CHECK(p);
    return res;
}

static Uint32 hashmap_restore_hash(Uint lvl, Eterm key) {
    Eterm heap[2], ret;

    if (lvl < 8) {
	return make_hash2(key) >> (4*lvl);
    }
    ret = CONS(heap, make_small(lvl), key);

    return make_hash2(ret) >> (4*(lvl % 8));
}

static Uint32 hashmap_shift_hash(Uint32 hx, Uint *lvl, Eterm key) {
    Eterm heap[2], ret;

    /* rehash with [ lvl | key ] */
    *lvl = *lvl + 1;
    if (*lvl % 8) {
	return hx >> 4;
    }

    ret = CONS(heap, make_small((*lvl)), key);
    return make_hash2(ret);
}

/* hashmap:info/0 */

static Eterm hashmap_info(Process *p, Eterm node) {
    Eterm *hp, **hpp;
    Eterm res = NIL, info = NIL;
    Eterm *ptr, tup, hdr;
    Uint sz;
    DECL_AM(depth);
    DECL_AM(leafs);
    DECL_AM(bitmaps);
    DECL_AM(arrays);
    Uint nleaf=0, nbitmap=0, narray=0;
    Uint bitmap_usage[16], leaf_usage[16];
    Uint lvl = 0, clvl;
    DECLARE_ESTACK(stack);

    for (sz = 0; sz < 16; sz++) {
	bitmap_usage[sz] = 0;
	leaf_usage[sz] = 0;
    }

    ptr = boxed_val(node);
    ESTACK_PUSH(stack, 0);
    ESTACK_PUSH(stack, node);
    do {
	node = ESTACK_POP(stack);
	clvl = ESTACK_POP(stack);
	lvl  = MAX(lvl,clvl);
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST:
		nleaf++;
		leaf_usage[clvl] += 1;
		break;
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));
		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_NODE_ARRAY:
			narray++;
			sz = 16;
			while(sz--) {
			    ESTACK_PUSH(stack, clvl + 1);
			    ESTACK_PUSH(stack, ptr[sz+1]);
			}
			break;
		    case HAMT_SUBTAG_NODE_BITMAP:
			nbitmap++;
			sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
			ASSERT(sz < 17);
			bitmap_usage[sz-1] += 1;
			while(sz--) {
			    ESTACK_PUSH(stack, clvl + 1);
			    ESTACK_PUSH(stack, ptr[sz+1]);
			}
			break;
		    case HAMT_SUBTAG_HEAD_BITMAP:
			nbitmap++;
			sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
			bitmap_usage[sz-1] += 1;
			while(sz--) {
			    ESTACK_PUSH(stack, clvl + 1);
			    ESTACK_PUSH(stack, ptr[sz+2]);
			}
			break;
		    case HAMT_SUBTAG_HEAD_ARRAY:
			narray++;
			sz = 16;
			while(sz--) {
			    ESTACK_PUSH(stack, clvl + 1);
			    ESTACK_PUSH(stack, ptr[sz+2]);
			}
			break;
		    default:
			erl_exit(1, "bad header\r\n");
			break;
		}
	}
    } while(!ESTACK_ISEMPTY(stack));


    /* size */
    sz = 0;
    hashmap_bld_tuple_uint(NULL,&sz,16,leaf_usage);
    hashmap_bld_tuple_uint(NULL,&sz,16,bitmap_usage);

    /* alloc */
    hp   = HAlloc(p, 2+3 + 3*(2+4) + sz);

    info = hashmap_bld_tuple_uint(&hp,NULL,16,leaf_usage);
    tup  = TUPLE3(hp, AM_leafs, make_small(nleaf),info); hp += 4;
    res  = CONS(hp, tup, res); hp += 2;

    info = hashmap_bld_tuple_uint(&hp,NULL,16,bitmap_usage);
    tup  = TUPLE3(hp, AM_bitmaps, make_small(nbitmap), info); hp += 4;
    res  = CONS(hp, tup, res); hp += 2;

    tup  = TUPLE3(hp, AM_arrays, make_small(narray),NIL); hp += 4;
    res  = CONS(hp, tup, res); hp += 2;

    tup  = TUPLE2(hp, AM_depth, make_small(lvl)); hp += 3;
    res  = CONS(hp, tup, res); hp += 2;

    DESTROY_ESTACK(stack);
    ERTS_HOLE_CHECK(p);
    return res;
}

static Eterm hashmap_bld_tuple_uint(Uint **hpp, Uint *szp, Uint n, Uint nums[]) {
    Eterm res = THE_NON_VALUE;
    Eterm *ts = (Eterm *)erts_alloc(ERTS_ALC_T_TMP, n * sizeof(Eterm));
    Uint i;


    for (i = 0; i < n; i++) {
	ts[i] = erts_bld_uint(hpp, szp, nums[i]);
    }
    res = erts_bld_tuplev(hpp, szp, n, ts);
    erts_free(ERTS_ALC_T_TMP, (void *) ts);
    return res;
}

BIF_RETTYPE hashmap_info_1(BIF_ALIST_1) {
    if (is_hashmap(BIF_ARG_1)) {
	BIF_RET(hashmap_info(BIF_P,BIF_ARG_1));
    }
    BIF_ERROR(BIF_P, BADARG);
}
