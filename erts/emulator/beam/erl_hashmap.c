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

#include "erl_map.h"
#include "erl_hashmap.h"

#if 0
static char *format_binary(Uint64 x, char *b) {
    int z;
    b[64] = '\0';
    for (z = 0; z < 64; z++) { 
	b[63-z] = ((x>>z) & 0x1) ? '1' : '0'; 
    }
    return b;
}
#endif


static Eterm hashmap_merge(Process *p, Eterm nodeA, Eterm nodeB);

/* hashmap:new/0 */

/* hashmap:put/3 */

/* hashmap:update/3 */

/* hashmap:to_list/1 */

/* hashmap:from_list/1 */

/* hashmap:get/2 */

/* hashmap:find/2 */

/* hashmap:remove/2 */

/* hashmap:size/1 */

/* erlang:is_hashmap/1 */

/* hashmap:is_key/2 */

/* hashmap:keys/1 */

/* hashmap:values/1 */

BIF_RETTYPE hashmap_merge_2(BIF_ALIST_2) {
    if (is_hashmap(BIF_ARG_1) && is_hashmap(BIF_ARG_2)) {
	BIF_RET(hashmap_merge(BIF_P, BIF_ARG_1, BIF_ARG_2));
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* impl. */


#define HALLOC_EXTRA 200

static Eterm hashmap_merge(Process *p, Eterm nodeA, Eterm nodeB) {
#define PSTACK_TYPE struct HashmapMergePStackType
    struct HashmapMergePStackType {
	Eterm *srcA, *srcB;
	Uint32 abm, bbm, rbm;        /* node bitmaps */
	int keepA;
	int ix;
	Eterm array[16];
    };
    PSTACK_DECLARE(s, 4);
    struct HashmapMergePStackType* sp = PSTACK_PUSH(s);
    Eterm *hp, *nhp;
    Eterm hdrA, hdrB;
    Eterm th[2];
    Uint32 ahx, bhx;
    Uint size;  /* total key-value counter */
    int keepA = 0;
    unsigned lvl = 0;
    Eterm res = THE_NON_VALUE;

    /*
     * Strategy: Do depth-first traversal of both trees (at the same time)
     * and merge each pair of nodes.
     */

    {
	hashmap_head_t* a = (hashmap_head_t*) hashmap_val(nodeA);
	hashmap_head_t* b = (hashmap_head_t*) hashmap_val(nodeB);
	size = a->size + b->size;
    }

recurse:

    if (primary_tag(nodeA) == TAG_PRIMARY_BOXED &&
	primary_tag(nodeB) == TAG_PRIMARY_LIST) {
	/* Avoid implementing this combination by switching places */
	Eterm tmp = nodeA;
	nodeA = nodeB;
	nodeB = tmp;
	keepA = !keepA;
    }

    switch (primary_tag(nodeA)) {
    case TAG_PRIMARY_LIST: {
	sp->srcA = list_val(nodeA);
	switch (primary_tag(nodeB)) {
	case TAG_PRIMARY_LIST: { /* LEAF + LEAF */
	    sp->srcB = list_val(nodeB);

	    if (EQ(CAR(sp->srcA), CAR(sp->srcB))) {
		--size;
		res = keepA ? nodeA : nodeB;
	    } else {
		ahx = hashmap_restore_hash(th, lvl, CAR(sp->srcA));
		bhx = hashmap_restore_hash(th, lvl, CAR(sp->srcB));
		sp->abm = 1 << hashmap_index(ahx);
		sp->bbm = 1 << hashmap_index(bhx);

		sp->srcA = &nodeA;
		sp->srcB = &nodeB;
	    }
	    break;
	}
	case TAG_PRIMARY_BOXED: { /* LEAF + NODE */
	    sp->srcB = boxed_val(nodeB);
	    ASSERT(is_header(*sp->srcB));
	    hdrB = *sp->srcB++;

	    ahx = hashmap_restore_hash(th, lvl, CAR(sp->srcA));
	    sp->abm = 1 << hashmap_index(ahx);
	    sp->srcA = &nodeA;
	    switch(hdrB & _HEADER_MAP_SUBTAG_MASK) {
	    case HAMT_SUBTAG_HEAD_ARRAY: sp->srcB++;
	    case HAMT_SUBTAG_NODE_ARRAY:
		sp->bbm = 0xffff;
		break;

	    case HAMT_SUBTAG_HEAD_BITMAP: sp->srcB++;
	    case HAMT_SUBTAG_NODE_BITMAP:
		sp->bbm = MAP_HEADER_VAL(hdrB);
		break;

	    default:
		erl_exit(1, "bad header tag %ld\r\n", *sp->srcB & _HEADER_MAP_SUBTAG_MASK);
		break;
	    }
	    break;
	}
	default:
	    erl_exit(1, "bad primary tag %ld\r\n", nodeB);
	}
	break;
    }
    case TAG_PRIMARY_BOXED: { /* NODE + NODE */
	sp->srcA = boxed_val(nodeA);
	hdrA = *sp->srcA++;
	ASSERT(is_header(hdrA));
	switch (hdrA & _HEADER_MAP_SUBTAG_MASK) {
	case HAMT_SUBTAG_HEAD_ARRAY: sp->srcA++;
	case HAMT_SUBTAG_NODE_ARRAY: {
	    ASSERT(primary_tag(nodeB) == TAG_PRIMARY_BOXED);
	    sp->abm = 0xffff;
	    sp->srcB = boxed_val(nodeB);
	    hdrB = *sp->srcB++;
	    ASSERT(is_header(hdrB));
	    switch (hdrB & _HEADER_MAP_SUBTAG_MASK) {
	    case HAMT_SUBTAG_HEAD_ARRAY: sp->srcB++;
	    case HAMT_SUBTAG_NODE_ARRAY:
		sp->bbm = 0xffff;
		break;
	    case HAMT_SUBTAG_HEAD_BITMAP: sp->srcB++;
	    case HAMT_SUBTAG_NODE_BITMAP:
		sp->bbm = MAP_HEADER_VAL(hdrB);
		break;
	    default:
		erl_exit(1, "bad header tag %ld\r\n", *sp->srcB & _HEADER_MAP_SUBTAG_MASK);
	    }
	    break;
	}
	case HAMT_SUBTAG_HEAD_BITMAP: sp->srcA++;
	case HAMT_SUBTAG_NODE_BITMAP: {
	    ASSERT(primary_tag(nodeB) == TAG_PRIMARY_BOXED);
	    sp->abm = MAP_HEADER_VAL(hdrA);
	    sp->srcB = boxed_val(nodeB);
	    hdrB = *sp->srcB++;
	    ASSERT(is_header(hdrB));
	    switch (hdrB & _HEADER_MAP_SUBTAG_MASK) {
	    case HAMT_SUBTAG_HEAD_ARRAY: sp->srcB++;
	    case HAMT_SUBTAG_NODE_ARRAY:
		sp->bbm = 0xffff;
		break;
	    case HAMT_SUBTAG_HEAD_BITMAP: sp->srcB++;
	    case HAMT_SUBTAG_NODE_BITMAP:
		sp->bbm = MAP_HEADER_VAL(hdrB);
		break;

	    default:
		erl_exit(1, "bad header tag %ld\r\n", *sp->srcB & _HEADER_MAP_SUBTAG_MASK);
	    }
	    break;
	}
	default:
	    erl_exit(1, "bad primary tag %ld\r\n", nodeA);
	}
	break;
    }
    default:
	erl_exit(1, "bad primary tag %ld\r\n", nodeA);
    }

    for (;;) {
	if (is_value(res)) { /* We have a complete (sub-)tree or leaf */
	    if (lvl == 0)
		break;

	    /* Pop from stack and continue build parent node */
	    lvl--;
	    sp = PSTACK_POP(s);
	    sp->array[sp->ix++] = res;
	    res = THE_NON_VALUE;
	    if (sp->rbm) {
		sp->srcA++;
		sp->srcB++;
		keepA = sp->keepA;
	    }
	} else { /* Start build a node */
	    sp->ix = 0;
	    sp->rbm = sp->abm | sp->bbm;
	    ASSERT(!(sp->rbm == 0 && lvl > 0));
	}

	while (sp->rbm) {
	    Uint32 next = sp->rbm & (sp->rbm-1);
	    Uint32 bit = sp->rbm ^ next;
	    sp->rbm = next;
	    if (sp->abm & bit) {
		if (sp->bbm & bit) {
		    /* Bit clash. Push and resolve by recursive merge */
		    if (sp->rbm) {
			sp->keepA = keepA;
		    }
		    nodeA = *sp->srcA;
		    nodeB = *sp->srcB;
		    lvl++;
		    sp = PSTACK_PUSH(s);
		    goto recurse;
		} else {
		    sp->array[sp->ix++] = *sp->srcA++;
		}
	    } else {
		ASSERT(sp->bbm & bit);
		sp->array[sp->ix++] = *sp->srcB++;
	    }
	}

	ASSERT(sp->ix == hashmap_bitcount(sp->abm | sp->bbm));
	if (lvl == 0) {
	    nhp = HAllocX(p, HAMT_HEAD_BITMAP_SZ(sp->ix), HALLOC_EXTRA);
	    hp = nhp;
	    *hp++ = (sp->ix == 16 ? MAP_HEADER_HAMT_HEAD_ARRAY
		     : MAP_HEADER_HAMT_HEAD_BITMAP(sp->abm | sp->bbm));
	    *hp++ = size;
	} else {
	    nhp = HAllocX(p, HAMT_NODE_BITMAP_SZ(sp->ix), HALLOC_EXTRA);
	    hp = nhp;
	    *hp++ = (sp->ix == 16 ? make_arityval(16)
		     : MAP_HEADER_HAMT_NODE_BITMAP(sp->abm | sp->bbm));
	}
	memcpy(hp, sp->array, sp->ix * sizeof(Eterm));
	res = make_boxed(nhp);
    }
    PSTACK_DESTROY(s);
    return res;
}

static int hash_cmp(Uint32 ha, Uint32 hb)
{
    int i;
    for (i=0; i<8; i++) {
	int cmp = (int)(ha & 0xF) - (int)(hb & 0xF);
	if (cmp)
	    return cmp;
	ha >>= 4;
	hb >>= 4;
    }
    return 0;
}

int hashmap_key_hash_cmp(Eterm* ap, Eterm* bp)
{
    Eterm th[2];
    unsigned lvl = 0;

    if (ap && bp) {
	ASSERT(CMP_TERM(CAR(ap), CAR(bp)) != 0);
	for (;;) {
	    Uint32 ha = hashmap_restore_hash(th, lvl, CAR(ap));
	    Uint32 hb = hashmap_restore_hash(th, lvl, CAR(bp));
	    int cmp = hash_cmp(ha, hb);
	    if (cmp)
		return cmp;
	    lvl += 8;
	}
    }
    return ap ? -1 : 1;
}

/* hashmap:info/0 */
