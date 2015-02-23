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

#ifndef DECL_AM
#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)
#endif

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


/* for hashmap_from_list/1 */
typedef struct {
    Uint32 hx;
    Uint32 skip;
    Uint i;
    Eterm  val;
} hxnode_t;

static Eterm hashmap_from_list(Process *p, Eterm node);
static Eterm hashmap_merge(Process *p, Eterm nodeA, Eterm nodeB);
static Eterm hashmap_bld_tuple_uint(Uint **hpp, Uint *szp, Uint n, Uint nums[]);
static Eterm hashmap_from_unsorted_array(Process *p, hxnode_t *hxns, Uint n);
static Eterm hashmap_from_sorted_unique_array(Process *p, hxnode_t *hxns, Uint n, int is_root);
static Eterm hashmap_from_chunked_array(Process *p, hxnode_t *hxns, Uint n, int is_root);
static int hxnodecmp(hxnode_t* a, hxnode_t* b);
static int hxnodecmpkey(hxnode_t* a, hxnode_t* b);

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

/* hashmap:update/3 */

/* hashmap:to_list/1 */

/* hashmap:from_list/1 */

BIF_RETTYPE hashmap_from_list_1(BIF_ALIST_1) {
    if (is_list(BIF_ARG_1) || is_nil(BIF_ARG_1)) {
	return hashmap_from_list(BIF_P, BIF_ARG_1);
    }

    BIF_ERROR(BIF_P, BADARG);
}
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


#define swizzle32(D,S) \
    do { \
	(D) = ((S) & 0x0000000f) << 28 | ((S) & 0x000000f0) << 20  \
	    | ((S) & 0x00000f00) << 12 | ((S) & 0x0000f000) << 4   \
	    | ((S) & 0x000f0000) >> 4  | ((S) & 0x00f00000) >> 12  \
	    | ((S) & 0x0f000000) >> 20 | ((S) & 0xf0000000) >> 28; \
    } while(0)


static Eterm hashmap_from_list(Process *p, Eterm list) {
    Eterm *kv, res, item = list;
    Eterm *hp;
    Eterm tmp[2];
    Uint32 sw, hx;
    Uint ix = 0, n = 0;
    hxnode_t *hxns;

    /* Calculate size and check validity */

    if (is_nil(list)) {
	hp    = HAlloc(p, HAMT_HEAD_EMPTY_SZ);
	hp[0] = MAP_HEADER_HAMT_HEAD_BITMAP(0);
	hp[1] = 0;
	return make_hashmap(hp);
    }

    while(is_list(item)) {
	res = CAR(list_val(item));
	if (is_not_tuple(res))
	    goto error;

	kv = tuple_val(res);
	if (*kv != make_arityval(2))
	    goto error;

	n++;
	item = CDR(list_val(item));
    }

    if (is_not_nil(item))
	goto error;

    hp = HAlloc(p, (2 * n));

    /* create tmp hx values and leaf ptrs */
    hxns = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP, n * sizeof(hxnode_t));
    item = list;

    while(is_list(item)) {
	res = CAR(list_val(item));
	kv  = tuple_val(res);
	hx  = hashmap_restore_hash(tmp,0,kv[1]);
	swizzle32(sw,hx);
	hxns[ix].hx   = sw;
	hxns[ix].val  = CONS(hp, kv[1], kv[2]); hp += 2;
	hxns[ix].skip = 1; /* will be reassigned in from_array */
	hxns[ix].i    = ix;
	ix++;
	item = CDR(list_val(item));
    }

    ASSERT(n > 0);

    res = hashmap_from_unsorted_array(p, hxns, n);

    erts_free(ERTS_ALC_T_TMP, (void *) hxns);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);

    BIF_RET(res);
error:
    BIF_ERROR(p, BADARG);
}

Eterm erts_hashmap_from_array(Process *p, Eterm *leafs, Uint n) {
    Eterm tmp[2];
    Uint32 sw, hx;
    Uint ix;
    hxnode_t *hxns;
    Eterm res;

    /* create tmp hx values and leaf ptrs */
    hxns = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP, n * sizeof(hxnode_t));

    for (ix = 0; ix < n; ix++) {
	hx  = hashmap_restore_hash(tmp,0,CAR(list_val(leafs[ix])));
	swizzle32(sw,hx);
	hxns[ix].hx   = sw;
	hxns[ix].val  = leafs[ix];
	hxns[ix].skip = 1;
	hxns[ix].i    = ix;
    }

    res = hashmap_from_unsorted_array(p, hxns, n);

    erts_free(ERTS_ALC_T_TMP, (void *) hxns);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);

    return res;
}

static Eterm hashmap_from_unsorted_array(Process *p, hxnode_t *hxns, Uint n) {
    Uint jx = 0, ix = 0, lx, cx;
    Eterm res;

    /* sort and compact array (remove non-unique entries) */
    qsort(hxns, n, sizeof(hxnode_t), (int (*)(const void *, const void *)) hxnodecmp);

    ix = 0, cx = 0;
    while(ix < n - 1) {
	if (hxns[ix].hx == hxns[ix+1].hx) {

	    /* find region of equal hash values */
	    jx = ix + 1;
	    while(jx < n && hxns[ix].hx == hxns[jx].hx) { jx++; }
	    /* find all correct keys from region
	     * (last in list but now hash sorted so we check highest id instead) */

	    /* resort with keys instead of hash value within region */
	    
	    qsort(&hxns[ix], jx - ix, sizeof(hxnode_t),
		    (int (*)(const void *, const void *)) hxnodecmpkey);

	    while(ix < jx) {
		lx = ix;
		while(ix < jx && EQ(CAR(list_val(hxns[ix].val)), CAR(list_val(hxns[lx].val)))) {
		    if (hxns[ix].i > hxns[lx].i) {
			lx = ix;
		    }
		    ix++;
		}
		hxns[cx].hx  = hxns[lx].hx;
		hxns[cx].val = hxns[lx].val;
		cx++;
	    }
	    ix = jx;
	    continue;
	}
	if (ix > cx) {
	    hxns[cx].hx  = hxns[ix].hx;
	    hxns[cx].val = hxns[ix].val;
	}
	cx++;
	ix++;
    }

    if (ix < n) {
	hxns[cx].hx  = hxns[ix].hx;
	hxns[cx].val = hxns[ix].val;
	cx++;
    }

    if (cx > 1) {
	/* recursive decompose array */
	res = hashmap_from_sorted_unique_array(p, hxns, cx, 0);
    } else {
	Eterm *hp;
	 /* hash value has been swizzled, need to drag it down to get the
	 * correct slot. */
	hp    = HAlloc(p, HAMT_HEAD_BITMAP_SZ(1));
	hp[0] = MAP_HEADER_HAMT_HEAD_BITMAP(1 << ((hxns[0].hx >> 0x1c) & 0xf));
	hp[1] = 1;
	hp[2] = hxns[0].val;
	res   = make_hashmap(hp);
    }

    return res;
}

static Eterm hashmap_from_sorted_unique_array(Process *p, hxnode_t *hxns, Uint n, int lvl) {
    Eterm res = NIL;
    Uint i,ix,jx,elems;
    Uint32 sw, hx;
    Eterm val;
    Eterm th[2];
    hxnode_t *tmp;

    ASSERT(lvl < 32);
    ix = 0;
    elems = 1;
    while (ix < n - 1) {
	if (hxns[ix].hx == hxns[ix+1].hx) {
	    jx = ix + 1;
	    while (jx < n && hxns[ix].hx == hxns[jx].hx) { jx++; }
	    tmp = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP, ((jx - ix)) * sizeof(hxnode_t));

	    for(i = 0; i < jx - ix; i++) {
		val = hxns[i + ix].val;
		hx  = hashmap_restore_hash(th, lvl + 8, CAR(list_val(val)));
		swizzle32(sw,hx);
		tmp[i].hx   = sw;
		tmp[i].val  = val;
		tmp[i].i    = i;
		tmp[i].skip = 1;
	    }

	    qsort(tmp, jx - ix, sizeof(hxnode_t), (int (*)(const void *, const void *)) hxnodecmp);

	    hxns[ix].skip = jx - ix;
	    hxns[ix].val  = hashmap_from_sorted_unique_array(p, tmp, jx - ix, lvl + 8);
	    erts_free(ERTS_ALC_T_TMP, (void *) tmp);
	    ix = jx;
	    if (ix < n) { elems++; }
	    continue;
	}
	hxns[ix].skip = 1;
	elems++;
	ix++;
    }

    res = hashmap_from_chunked_array(p, hxns, elems, !lvl);

    ERTS_HOLE_CHECK(p);

    return res;
}

#define maskval(V,L)      (((V) >> ((7 - (L))*4)) & 0xf)
#define cdepth(V1,V2)     (hashmap_clz((V1) ^ (V2)) >> 2)

/* n must be > 1
 * hash values in hxns has to be unique
 */

#define HALLOC_EXTRA 200
static Eterm hashmap_from_chunked_array(Process *p, hxnode_t *hxns, Uint n, int is_root) {
    Uint ix, d, dn, dc, slot, elems;
    Uint32 v, vp, vn, hdr;
    Uint bp, sz;
    DECLARE_ESTACK(stack);
    Eterm res = NIL, *hp = NULL, *nhp;

    ASSERT(n > 1);

    /* push initial nodes on the stack,
     * this is the starting depth */

    ix = 0;
    d  = 0;
    vp = hxns[ix].hx;
    v  = hxns[ix + hxns[ix].skip].hx;

    ASSERT(vp > v);
    slot = maskval(vp,d);

    while(slot == maskval(v,d)) {
	ESTACK_PUSH(stack, 1 << slot);
	d++;
	slot = maskval(vp,d);
    }

    res = hxns[ix].val;

    if (hxns[ix].skip > 1) {
	dc = 7;
	/* build collision nodes */
	while (dc > d) {
	    hp    = HAllocX(p, HAMT_NODE_BITMAP_SZ(1), HALLOC_EXTRA);
	    hp[0] = MAP_HEADER_HAMT_NODE_BITMAP(1 << maskval(vp,dc));
	    hp[1] = res;
	    res   = make_hashmap(hp);
	    dc--;
	}
    }

    ESTACK_PUSH(stack, res);
    ESTACK_PUSH(stack, 1 << slot);

    /* all of the other nodes .. */
    elems = n - 2; /* remove first and last elements */
    while(elems--) {
	hdr = ESTACK_POP(stack);
	ix  = ix + hxns[ix].skip;

	/* determine if node or subtree should be built by looking
	 * at the next value. */

	vn = hxns[ix + hxns[ix].skip].hx;
	dn = cdepth(v,vn);
	ASSERT(v > vn);

	res = hxns[ix].val;

	if (hxns[ix].skip > 1) {
	    int wat = (d > dn) ? d : dn;
	    dc = 7;
	    /* build collision nodes */
	    while (dc > wat) {
		hp    = HAllocX(p, HAMT_NODE_BITMAP_SZ(1), HALLOC_EXTRA);
		hp[0] = MAP_HEADER_HAMT_NODE_BITMAP(1 << maskval(v,dc));
		hp[1] = res;
		res   = make_hashmap(hp);
		dc--;
	    }
	}

	/* next depth is higher (implies collision) */
	if (d < dn) {
	    /* hdr is the popped one initially */
	    while(d < dn) {
		slot = maskval(v, d);
		bp   = 1 << slot;
		ESTACK_PUSH(stack, hdr | bp);
		d++;
		hdr = 0; /* clear hdr for all other collisions */
	    }

	    slot = maskval(v, d);
	    bp   = 1 << slot;
	    /* no more collisions */
	    ESTACK_PUSH(stack,res);
	    ESTACK_PUSH(stack,bp);
	} else if (d == dn) {
	    /* no collisions at all */
	    slot = maskval(v, d);
	    bp   = 1 << slot;
	    ESTACK_PUSH(stack,res);
	    ESTACK_PUSH(stack,hdr | bp);
	} else {
	    /* dn < n, we have a drop and we are done
	     * build nodes and subtree */
	    while (dn != d) {
		slot  = maskval(v, d);
		bp    = 1 << slot;
		/* OR bitposition before sz calculation to handle
		 * redundant collisions */
		hdr  |= bp;
		sz    = hashmap_bitcount(hdr);
		hp    = HAllocX(p, HAMT_NODE_BITMAP_SZ(sz), HALLOC_EXTRA);
		nhp   = hp;
		*hp++ = (hdr == 0xffff) ? MAP_HEADER_HAMT_NODE_ARRAY : MAP_HEADER_HAMT_NODE_BITMAP(hdr);
		*hp++ = res; sz--;
		while (sz--) { *hp++ = ESTACK_POP(stack); }
		ASSERT((hp - nhp) < 18);
		res = make_hashmap(nhp);

		/* we need to pop the next hdr and push if we don't need it */

		hdr = ESTACK_POP(stack);
		d--;
	    }
	    ESTACK_PUSH(stack, res);
	    ESTACK_PUSH(stack, hdr);
	}

	vp = v;
	v  = vn;
	d  = dn;
	ERTS_HOLE_CHECK(p);
    }

    /* v and vp are reused from above */
    dn  = cdepth(vp,v);
    ix  = ix + hxns[ix].skip;
    res = hxns[ix].val;

    if (hxns[ix].skip > 1) {
	dc = 7;
	/* build collision nodes */
	while (dc > dn) {
	    hp    = HAllocX(p, HAMT_NODE_BITMAP_SZ(1), HALLOC_EXTRA);
	    hp[0] = MAP_HEADER_HAMT_NODE_BITMAP(1 << maskval(v,dc));
	    hp[1] = res;
	    res   = make_hashmap(hp);
	    dc--;
	}
    }

    hdr = ESTACK_POP(stack);
    /* pop remaining subtree if any */
    while (dn) {
	slot  = maskval(v, dn);
	bp    = 1 << slot;
	/* OR bitposition before sz calculation to handle
	 * redundant collisions */
	hdr  |= bp;
	sz    = hashmap_bitcount(hdr);
	hp    = HAllocX(p, HAMT_NODE_BITMAP_SZ(sz), HALLOC_EXTRA);
	nhp   = hp;
	*hp++ = (hdr == 0xffff) ? MAP_HEADER_HAMT_NODE_ARRAY : MAP_HEADER_HAMT_NODE_BITMAP(hdr);
	*hp++ = res; sz--;

	while (sz--) { *hp++ = ESTACK_POP(stack); }
	res = make_hashmap(nhp);
	hdr = ESTACK_POP(stack);
	dn--;
    }

    /* and finally the root .. */

    slot  = maskval(v, dn);
    bp    = 1 << slot;
    hdr  |= bp;
    sz    = hashmap_bitcount(hdr);
    hp    = HAlloc(p, sz + /* hdr + item */ (is_root ? 2 : 1));
    nhp   = hp;

    if (is_root) {
	*hp++ = (hdr == 0xffff) ? MAP_HEADER_HAMT_HEAD_ARRAY : MAP_HEADER_HAMT_HEAD_BITMAP(hdr);
	*hp++ = n;
    } else {
	*hp++ = (hdr == 0xffff) ? MAP_HEADER_HAMT_NODE_ARRAY : MAP_HEADER_HAMT_NODE_BITMAP(hdr);
    }

    *hp++ = res; sz--;
    while (sz--) { *hp++ = ESTACK_POP(stack); }

    res = make_hashmap(nhp);

    ASSERT(ESTACK_COUNT(stack) == 0);
    DESTROY_ESTACK(stack);
    ERTS_HOLE_CHECK(p);
    return res;
}
#undef HALLOC_EXTRA

static int hxnodecmpkey(hxnode_t *a, hxnode_t *b) {
    return CMP_TERM(CAR(list_val(a->val)), CAR(list_val(b->val)));
}

static int hxnodecmp(hxnode_t *a, hxnode_t *b) {
    if (a->hx < b->hx)
	return 1;
    else if (a->hx == b->hx)
	return 0;
    else
	return -1;
}

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

static Eterm hashmap_info(Process *p, Eterm node) {
    Eterm *hp;
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

/* implementation of builtin emulations */

#if !defined(__GNUC__)
/* Count leading zeros emulation */
Uint32 hashmap_clz(Uint32 x) {
    Uint32 y;
    int n = 32;
    y = x >>16;  if (y != 0) {n = n -16;  x = y;}
    y = x >> 8;  if (y != 0) {n = n - 8;  x = y;}
    y = x >> 4;  if (y != 0) {n = n - 4;  x = y;}
    y = x >> 2;  if (y != 0) {n = n - 2;  x = y;}
    y = x >> 1;  if (y != 0) return n - 2;
    return n - x;
}
const Uint32 SK5 = 0x55555555, SK3 = 0x33333333;
const Uint32 SKF0 = 0xF0F0F0F, SKFF = 0xFF00FF;

/* CTPOP emulation */
Uint32 hashmap_bitcount(Uint32 x) {
    x -= ((x >> 1  ) & SK5);
    x  =  (x & SK3 ) + ((x >> 2 ) & SK3 );
    x  =  (x & SKF0) + ((x >> 4 ) & SKF0);
    x +=   x >> 8;
    return (x + (x >> 16)) & 0x3F;
}
#endif
