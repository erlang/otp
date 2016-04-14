/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014. All Rights Reserved.
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
 *
 * hashmaps are an adaption of Rich Hickeys Persistent HashMaps
 *   which were an adaption of Phil Bagwells - Hash Array Mapped Tries
 *
 * Author: Björn-Egil Dahlberg
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#define ERL_WANT_HIPE_BIF_WRAPPER__
#include "bif.h"
#undef ERL_WANT_HIPE_BIF_WRAPPER__
#include "erl_binary.h"

#include "erl_map.h"

/* BIFs
 *
 * DONE:
 * - erlang:is_map/1
 * - erlang:map_size/1
 *
 * - maps:find/2
 * - maps:from_list/1
 * - maps:get/2
 * - maps:is_key/2
 * - maps:keys/1
 * - maps:merge/2
 * - maps:new/0
 * - maps:put/3
 * - maps:remove/2
 * - maps:to_list/1
 * - maps:update/3
 * - maps:values/1
 *
 * TODO:
 * - maps:foldl/3
 * - maps:foldr/3
 * - maps:map/3
 * - maps:size/1
 * - maps:without/2
 *
 * DEBUG: for sharing calculation
 * - erts_internal:map_to_tuple_keys/1
 */

#ifndef DECL_AM
#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)
#endif

/* for hashmap_from_list/1 */
typedef struct {
    Uint32 hx;
    Uint32 skip;
    Uint i;
    Eterm  val;
} hxnode_t;


static Eterm flatmap_merge(Process *p, Eterm nodeA, Eterm nodeB);
static BIF_RETTYPE map_merge_mixed(Process *p, Eterm flat, Eterm tree, int swap_args);
struct HashmapMergeContext_;
static BIF_RETTYPE hashmap_merge(Process *p, Eterm nodeA, Eterm nodeB, int swap_args,
                                 struct HashmapMergeContext_*);
static Export hashmap_merge_trap_export;
static BIF_RETTYPE maps_merge_trap_1(BIF_ALIST_1);
static Uint hashmap_subtree_size(Eterm node);
static Eterm hashmap_to_list(Process *p, Eterm map);
static Eterm hashmap_keys(Process *p, Eterm map);
static Eterm hashmap_values(Process *p, Eterm map);
static Eterm hashmap_delete(Process *p, Uint32 hx, Eterm key, Eterm node);
static Eterm flatmap_from_validated_list(Process *p, Eterm list, Uint size);
static Eterm hashmap_from_validated_list(Process *p, Eterm list, Uint size);
static Eterm hashmap_from_unsorted_array(ErtsHeapFactory*, hxnode_t *hxns, Uint n, int reject_dupkeys);
static Eterm hashmap_from_sorted_unique_array(ErtsHeapFactory*, hxnode_t *hxns, Uint n, int is_root);
static Eterm hashmap_from_chunked_array(ErtsHeapFactory*, hxnode_t *hxns, Uint n, Uint size, int is_root);
static Eterm hashmap_info(Process *p, Eterm node);
static Eterm hashmap_bld_tuple_uint(Uint **hpp, Uint *szp, Uint n, Uint nums[]);
static int hxnodecmp(hxnode_t* a, hxnode_t* b);
static int hxnodecmpkey(hxnode_t* a, hxnode_t* b);


void erts_init_map(void) {
    erts_init_trap_export(&hashmap_merge_trap_export,
			  am_maps, am_merge_trap, 1,
			  &maps_merge_trap_1);
    return;
}


/* erlang:map_size/1
 * the corresponding instruction is implemented in:
 *     beam/erl_bif_guard.c
 */

BIF_RETTYPE map_size_1(BIF_ALIST_1) {
    if (is_flatmap(BIF_ARG_1)) {
	flatmap_t *mp = (flatmap_t*)flatmap_val(BIF_ARG_1);
	BIF_RET(make_small(flatmap_get_size(mp)));
    } else if (is_hashmap(BIF_ARG_1)) {
	Eterm *head, *hp, res;
	Uint size, hsz=0;

	head = hashmap_val(BIF_ARG_1);
	size = head[1];
	(void) erts_bld_uint(NULL, &hsz, size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, size);
	BIF_RET(res);
    }

    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, BADMAP);
}

/* maps:to_list/1 */

BIF_RETTYPE maps_to_list_1(BIF_ALIST_1) {
    if (is_flatmap(BIF_ARG_1)) {
	Uint n;
	Eterm* hp;
	Eterm *ks,*vs, res, tup;
	flatmap_t *mp = (flatmap_t*)flatmap_val(BIF_ARG_1);

	ks  = flatmap_get_keys(mp);
	vs  = flatmap_get_values(mp);
	n   = flatmap_get_size(mp);
	hp  = HAlloc(BIF_P, (2 + 3) * n);
	res = NIL;

	while(n--) {
	    tup = TUPLE2(hp, ks[n], vs[n]); hp += 3;
	    res = CONS(hp, tup, res); hp += 2;
	}

	BIF_RET(res);
    } else if (is_hashmap(BIF_ARG_1)) {
	return hashmap_to_list(BIF_P, BIF_ARG_1);
    }

    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, BADMAP);
}

/* maps:find/2
 * return value if key *matches* a key in the map
 */

const Eterm *
#if HALFWORD_HEAP
erts_maps_get_rel(Eterm key, Eterm map, Eterm *map_base)
#else
erts_maps_get(Eterm key, Eterm map)
#endif
{
    Uint32 hx;
    if (is_flatmap_rel(map, map_base)) {
	Eterm *ks, *vs;
	flatmap_t *mp;
	Uint n, i;

	mp  = (flatmap_t *)flatmap_val_rel(map, map_base);
	n   = flatmap_get_size(mp);

	if (n == 0) {
	    return NULL;
	}

	ks  = (Eterm *)tuple_val_rel(mp->keys, map_base) + 1;
	vs  = flatmap_get_values(mp);

	if (is_immed(key)) {
	    for (i = 0; i < n; i++) {
		if (ks[i] == key) {
		    return &vs[i];
		}
	    }
	}

	for (i = 0; i < n; i++) {
	    if (eq_rel(ks[i], map_base, key, NULL)) {
		return &vs[i];
	    }
	}
	return NULL;
    }
    ASSERT(is_hashmap_rel(map, map_base));
    hx = hashmap_make_hash(key);

    return erts_hashmap_get_rel(hx, key, map, map_base);
}

BIF_RETTYPE maps_find_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
        Eterm *hp, res;
        const Eterm *value;

        value = erts_maps_get(BIF_ARG_1, BIF_ARG_2);
	if (value) {
	    hp    = HAlloc(BIF_P, 3);
	    res   = make_tuple(hp);
	    *hp++ = make_arityval(2);
	    *hp++ = am_ok;
            *hp++ = *value;
	    BIF_RET(res);
	}
	BIF_RET(am_error);
    }
    BIF_P->fvalue = BIF_ARG_2;
    BIF_ERROR(BIF_P, BADMAP);
}

/* maps:get/2
 * return value if key *matches* a key in the map
 * exception badkey if none matches
 */

BIF_RETTYPE maps_get_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
        const Eterm *value;

        value = erts_maps_get(BIF_ARG_1, BIF_ARG_2);
        if (value) {
            BIF_RET(*value);
	}

	BIF_P->fvalue = BIF_ARG_1;
	BIF_ERROR(BIF_P, BADKEY);
    }
    BIF_P->fvalue = BIF_ARG_2;
    BIF_ERROR(BIF_P, BADMAP);
}

/* maps:from_list/1
 * List may be unsorted [{K,V}]
 */

BIF_RETTYPE maps_from_list_1(BIF_ALIST_1) {
    Eterm item = BIF_ARG_1, res, *kv;
    Uint  size = 0;
    if (is_list(item) || is_nil(item)) {

	/* Calculate size and check validity */

	while(is_list(item)) {
	    res = CAR(list_val(item));
	    if (is_not_tuple(res))
		goto error;

	    kv = tuple_val(res);
	    if (*kv != make_arityval(2))
		goto error;

	    size++;
	    item = CDR(list_val(item));
	}

	if (is_not_nil(item))
	    goto error;

	if (size > MAP_SMALL_MAP_LIMIT) {
	    BIF_RET(hashmap_from_validated_list(BIF_P, BIF_ARG_1, size));
	} else {
	    BIF_RET(flatmap_from_validated_list(BIF_P, BIF_ARG_1, size));
	}
    }

error:

    BIF_ERROR(BIF_P, BADARG);
}

static Eterm flatmap_from_validated_list(Process *p, Eterm list, Uint size) {
    Eterm *kv, item = list;
    Eterm *hp, *thp,*vs, *ks, keys, res;
    flatmap_t *mp;
    Uint  unused_size = 0;
    Sint  c = 0;
    Sint  idx = 0;


    hp    = HAlloc(p, 3 + 1 + (2 * size));
    thp   = hp;
    keys  = make_tuple(hp);
    *hp++ = make_arityval(size);
    ks    = hp;
    hp   += size;
    mp    = (flatmap_t*)hp;
    res   = make_flatmap(mp);
    hp   += MAP_HEADER_FLATMAP_SZ;
    vs    = hp;

    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = size; /* set later, might shrink*/
    mp->keys = keys;

    if (size == 0)
	return res;

    /* first entry */
    kv    = tuple_val(CAR(list_val(item)));
    ks[0] = kv[1];
    vs[0] = kv[2];
    size  = 1;
    item  = CDR(list_val(item));

    /* insert sort key/value pairs */
    while(is_list(item)) {

	kv = tuple_val(CAR(list_val(item)));

	/* compare ks backwards
	 * idx represent word index to be written (hole position).
	 * We cannot copy the elements when searching since we might
	 * have an equal key. So we search for just the index first =(
	 *
	 * It is perhaps faster to move the values in the first pass.
	 * Check for uniqueness during insert phase and then have a
	 * second phace compacting the map if duplicates are found
	 * during insert. .. or do someother sort .. shell-sort perhaps.
	 */

	idx = size;

	while(idx > 0 && (c = CMP_TERM(kv[1],ks[idx-1])) < 0) { idx--; }

	if (c == 0) {
	    /* last compare was equal,
	     * i.e. we have to release memory
	     * and overwrite that key/value
	     */
	    ks[idx-1] = kv[1];
	    vs[idx-1] = kv[2];
	    unused_size++;
	} else {
	    Uint i = size;
	    while(i > idx) {
		ks[i] = ks[i-1];
		vs[i] = vs[i-1];
		i--;
	    }
	    ks[idx] = kv[1];
	    vs[idx] = kv[2];
	    size++;
	}
	item = CDR(list_val(item));
    }

    if (unused_size) {
	/* the key tuple is embedded in the heap
	 * write a bignum to clear it.
	 */
	/* release values as normal since they are on the top of the heap */

	ks[size] = make_pos_bignum_header(unused_size - 1);
	HRelease(p, vs + size + unused_size, vs + size);
    }

    *thp = make_arityval(size);
    mp->size = size;
    return res;
}

#define swizzle32(D,S) \
    do { \
	(D) = ((S) & 0x0000000f) << 28 | ((S) & 0x000000f0) << 20  \
	    | ((S) & 0x00000f00) << 12 | ((S) & 0x0000f000) << 4   \
	    | ((S) & 0x000f0000) >> 4  | ((S) & 0x00f00000) >> 12  \
	    | ((S) & 0x0f000000) >> 20 | ((S) & 0xf0000000) >> 28; \
    } while(0)

#define maskval(V,L)      (((V) >> ((7 - (L))*4)) & 0xf)
#define cdepth(V1,V2)     (hashmap_clz((V1) ^ (V2)) >> 2)

static Eterm hashmap_from_validated_list(Process *p, Eterm list, Uint size) {
    Eterm item = list;
    Eterm *hp;
    Eterm *kv, res;
    Uint32 sw, hx;
    Uint ix = 0;
    hxnode_t *hxns;
    ErtsHeapFactory factory;
    DeclareTmpHeap(tmp,2,p);
    ASSERT(size > 0);

    hp = HAlloc(p, (2 * size));

    /* create tmp hx values and leaf ptrs */
    hxns = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP, size * sizeof(hxnode_t));

    UseTmpHeap(2,p);
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
    UnUseTmpHeap(2,p);

    erts_factory_proc_init(&factory, p);
    res = hashmap_from_unsorted_array(&factory, hxns, size, 0);
    erts_factory_close(&factory);

    erts_free(ERTS_ALC_T_TMP, (void *) hxns);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);

    if (hashmap_size(res) <= MAP_SMALL_MAP_LIMIT) {
        DECLARE_WSTACK(wstack);
	Eterm *kv, *ks, *vs;
	flatmap_t *mp;
	Eterm keys;
        Uint n = hashmap_size(res);

	/* build flat structure */
	hp    = HAlloc(p, 3 + 1 + (2 * n));
	keys  = make_tuple(hp);
	*hp++ = make_arityval(n);
	ks    = hp;
	hp   += n;
	mp    = (flatmap_t*)hp;
	hp   += MAP_HEADER_FLATMAP_SZ;
	vs    = hp;

	mp->thing_word = MAP_HEADER_FLATMAP;
	mp->size = n;
	mp->keys = keys;

	hashmap_iterator_init(&wstack, res, 0);

	while ((kv=hashmap_iterator_next(&wstack)) != NULL) {
	    *ks++ = CAR(kv);
	    *vs++ = CDR(kv);
	}

	/* it cannot have multiple keys */
	erts_validate_and_sort_flatmap(mp);

	DESTROY_WSTACK(wstack);
	return make_flatmap(mp);
    }

    return res;
}

Eterm erts_hashmap_from_array(ErtsHeapFactory* factory, Eterm *leafs, Uint n,
                              int reject_dupkeys) {
    Uint32 sw, hx;
    Uint ix;
    hxnode_t *hxns;
    Eterm res;

    /* create tmp hx values and leaf ptrs */
    hxns = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP, n * sizeof(hxnode_t));

    for (ix = 0; ix < n; ix++) {
	hx  = hashmap_make_hash(*leafs);
	swizzle32(sw,hx);
	hxns[ix].hx   = sw;
	hxns[ix].val  = make_list(leafs);
	hxns[ix].skip = 1;
	hxns[ix].i    = ix;
	leafs += 2;
    }

    res = hashmap_from_unsorted_array(factory, hxns, n, reject_dupkeys);

    erts_free(ERTS_ALC_T_TMP, (void *) hxns);

    return res;
}


Eterm erts_hashmap_from_ks_and_vs_extra(Process *p, Eterm *ks, Eterm *vs, Uint n,
					Eterm key, Eterm value) {
    Uint32 sw, hx;
    Uint i,sz;
    hxnode_t *hxns;
    ErtsHeapFactory factory;
    Eterm *hp, res;

    sz = (key == THE_NON_VALUE) ? n : (n + 1);
    ASSERT(sz > MAP_SMALL_MAP_LIMIT);
    hp = HAlloc(p, 2 * sz);

    /* create tmp hx values and leaf ptrs */
    hxns = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP, sz * sizeof(hxnode_t));

    for(i = 0; i < n; i++) {
	hx = hashmap_make_hash(ks[i]);
	swizzle32(sw,hx);
	hxns[i].hx   = sw;
	hxns[i].val  = CONS(hp, ks[i], vs[i]); hp += 2;
	hxns[i].skip = 1; /* will be reassigned in from_array */
	hxns[i].i    = i;
    }

    if (key != THE_NON_VALUE) {
	hx = hashmap_make_hash(key);
	swizzle32(sw,hx);
	hxns[i].hx   = sw;
	hxns[i].val  = CONS(hp, key, value); hp += 2;
	hxns[i].skip = 1;
	hxns[i].i    = i;
    }

    erts_factory_proc_init(&factory, p);
    res = hashmap_from_unsorted_array(&factory, hxns, sz, 0);
    erts_factory_close(&factory);

    erts_free(ERTS_ALC_T_TMP, (void *) hxns);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);

    return res;
}

static Eterm hashmap_from_unsorted_array(ErtsHeapFactory* factory,
                                         hxnode_t *hxns, Uint n,
                                         int reject_dupkeys) {
    Uint jx = 0, ix = 0, lx, cx;
    Eterm res;

    if (n == 0) {
	Eterm *hp;
	hp = erts_produce_heap(factory, 2, 0);
	hp[0] = MAP_HEADER_HAMT_HEAD_BITMAP(0);
	hp[1] = 0;

	return make_hashmap(hp);
    }

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
		while(++ix < jx && EQ(CAR(list_val(hxns[ix].val)),
				      CAR(list_val(hxns[lx].val)))) {
                    if (reject_dupkeys)
                        return THE_NON_VALUE;

                    if (hxns[ix].i > hxns[lx].i) {
			lx = ix;
		    }
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
	res = hashmap_from_sorted_unique_array(factory, hxns, cx, 0);
    } else {
	Eterm *hp;

	/* we only have one item, either because n was 1 or
	 * because we hade multiples of the same key.
	 *
	 * hash value has been swizzled, need to drag it down to get the
	 * correct slot. */

	hp    = erts_produce_heap(factory, HAMT_HEAD_BITMAP_SZ(1), 0);
	hp[0] = MAP_HEADER_HAMT_HEAD_BITMAP(1 << ((hxns[0].hx >> 0x1c) & 0xf));
	hp[1] = 1;
	hp[2] = hxns[0].val;
	res   = make_hashmap(hp);
    }

    return res;
}

static Eterm hashmap_from_sorted_unique_array(ErtsHeapFactory* factory,
                                              hxnode_t *hxns, Uint n, int lvl) {
    Eterm res = NIL;
    Uint i,ix,jx,elems;
    Uint32 sw, hx;
    Eterm val;
    hxnode_t *tmp;
    DeclareTmpHeapNoproc(th,2);
    UseTmpHeapNoproc(2);
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
	    hxns[ix].val  = hashmap_from_sorted_unique_array(factory, tmp, jx - ix, lvl + 8);
	    erts_free(ERTS_ALC_T_TMP, (void *) tmp);
	    ix = jx;
	    if (ix < n) { elems++; }
	    continue;
	}
	hxns[ix].skip = 1;
	elems++;
	ix++;
    }

    res = hashmap_from_chunked_array(factory, hxns, elems, n, !lvl);

    ERTS_FACTORY_HOLE_CHECK(factory);

    UnUseTmpHeapNoproc(2);
    return res;
}

#define HALLOC_EXTRA 200
static Eterm hashmap_from_chunked_array(ErtsHeapFactory *factory, hxnode_t *hxns, Uint n,
                                        Uint size, int is_root) {
    Uint ix, d, dn, dc, slot, elems;
    Uint32 v, vp, vn, hdr;
    Uint bp, sz;
    DECLARE_ESTACK(stack);
    Eterm res = NIL, *hp = NULL, *nhp;


    /* if we get here with only one element then
     * we have eight levels of collisions
     */

    if (n == 1) {
	res = hxns[0].val;
	v   = hxns[0].hx;
	for (d = 7; d > 0; d--) {
	    slot  = maskval(v,d);
	    hp    = erts_produce_heap(factory, HAMT_NODE_BITMAP_SZ(1), HALLOC_EXTRA);
	    hp[0] = MAP_HEADER_HAMT_NODE_BITMAP(1 << slot);
	    hp[1] = res;
	    res   = make_hashmap(hp);
	}

	slot  = maskval(v,0);
	hp    = erts_produce_heap(factory, (is_root ? 3 : 2), 0);

	if (is_root) {
	    hp[0] = MAP_HEADER_HAMT_HEAD_BITMAP(1 << slot);
	    hp[1] = size;
	    hp[2] = res;
	} else {
	    hp[0] = MAP_HEADER_HAMT_NODE_BITMAP(1 << slot);
	    hp[1] = res;
	}
	return make_hashmap(hp);
    }

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
	    hp    = erts_produce_heap(factory, HAMT_NODE_BITMAP_SZ(1), HALLOC_EXTRA);
	    hp[0] = MAP_HEADER_HAMT_NODE_BITMAP(1 << maskval(vp,dc));
	    hp[1] = res;
	    res   = make_hashmap(hp);
	    dc--;
	}
    }

    ESTACK_PUSH2(stack,res,1 << slot);

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
		hp    = erts_produce_heap(factory, HAMT_NODE_BITMAP_SZ(1), HALLOC_EXTRA);
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
            ESTACK_PUSH2(stack,res,bp);
	} else if (d == dn) {
	    /* no collisions at all */
	    slot = maskval(v, d);
	    bp   = 1 << slot;
            ESTACK_PUSH2(stack,res,hdr | bp);
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
		hp    = erts_produce_heap(factory, HAMT_NODE_BITMAP_SZ(sz), HALLOC_EXTRA);
		nhp   = hp;
		*hp++ = MAP_HEADER_HAMT_NODE_BITMAP(hdr);
		*hp++ = res; sz--;
		while (sz--) { *hp++ = ESTACK_POP(stack); }
		ASSERT((hp - nhp) < 18);
		res = make_hashmap(nhp);

		/* we need to pop the next hdr and push if we don't need it */

		hdr = ESTACK_POP(stack);
		d--;
	    }
            ESTACK_PUSH2(stack,res,hdr);
	}

	vp = v;
	v  = vn;
	d  = dn;
	ERTS_FACTORY_HOLE_CHECK(factory);
    }

    /* v and vp are reused from above */
    dn  = cdepth(vp,v);
    ix  = ix + hxns[ix].skip;
    res = hxns[ix].val;

    if (hxns[ix].skip > 1) {
	dc = 7;
	/* build collision nodes */
	while (dc > dn) {
	    hp    = erts_produce_heap(factory, HAMT_NODE_BITMAP_SZ(1), HALLOC_EXTRA);
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
	hp    = erts_produce_heap(factory, HAMT_NODE_BITMAP_SZ(sz), HALLOC_EXTRA);
	nhp   = hp;
	*hp++ = MAP_HEADER_HAMT_NODE_BITMAP(hdr);
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
    hp    = erts_produce_heap(factory, sz + /* hdr + item */ (is_root ? 2 : 1), 0);
    nhp   = hp;

    if (is_root) {
	*hp++ = (hdr == 0xffff) ? MAP_HEADER_HAMT_HEAD_ARRAY : MAP_HEADER_HAMT_HEAD_BITMAP(hdr);
	*hp++ = size;
    } else {
	*hp++ = MAP_HEADER_HAMT_NODE_BITMAP(hdr);
    }

    *hp++ = res; sz--;
    while (sz--) { *hp++ = ESTACK_POP(stack); }

    res = make_hashmap(nhp);

    ASSERT(ESTACK_COUNT(stack) == 0);
    DESTROY_ESTACK(stack);
    ERTS_FACTORY_HOLE_CHECK(factory);
    return res;
}
#undef HALLOC_EXTRA

static int hxnodecmpkey(hxnode_t *a, hxnode_t *b) {
    Sint c = CMP_TERM(CAR(list_val(a->val)), CAR(list_val(b->val)));
#if ERTS_SIZEOF_ETERM <= SIZEOF_INT
    return c;
#else
    return c > 0 ? 1 : (c < 0 ? -1 : 0);
#endif
}

static int hxnodecmp(hxnode_t *a, hxnode_t *b) {
    if (a->hx < b->hx)
	return 1;
    else if (a->hx == b->hx)
	return 0;
    else
	return -1;
}

/* maps:is_key/2 */

BIF_RETTYPE maps_is_key_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
	BIF_RET(erts_maps_get(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
    }
    BIF_P->fvalue = BIF_ARG_2;
    BIF_ERROR(BIF_P, BADMAP);
}

/* maps:keys/1 */

BIF_RETTYPE maps_keys_1(BIF_ALIST_1) {
    if (is_flatmap(BIF_ARG_1)) {
	Eterm *hp, *ks, res = NIL;
	flatmap_t *mp;
	Uint n;

	mp  = (flatmap_t*)flatmap_val(BIF_ARG_1);
	n   = flatmap_get_size(mp);

	if (n == 0)
	    BIF_RET(res);

	hp  = HAlloc(BIF_P, (2 * n));
	ks  = flatmap_get_keys(mp);

	while(n--) {
	    res = CONS(hp, ks[n], res); hp += 2;
	}

	BIF_RET(res);
    } else if (is_hashmap(BIF_ARG_1)) {
	BIF_RET(hashmap_keys(BIF_P, BIF_ARG_1));
    }
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, BADMAP);
}

/* maps:merge/2 */

HIPE_WRAPPER_BIF_DISABLE_GC(maps_merge, 2)

BIF_RETTYPE maps_merge_2(BIF_ALIST_2) {
    if (is_flatmap(BIF_ARG_1)) {
	if (is_flatmap(BIF_ARG_2)) {
	    BIF_RET(flatmap_merge(BIF_P, BIF_ARG_1, BIF_ARG_2));
	} else if (is_hashmap(BIF_ARG_2)) {
	    /* Will always become a tree */
            return map_merge_mixed(BIF_P, BIF_ARG_1, BIF_ARG_2, 0);
	}
	BIF_P->fvalue = BIF_ARG_2;
    } else if (is_hashmap(BIF_ARG_1)) {
	if (is_hashmap(BIF_ARG_2)) {
	    return hashmap_merge(BIF_P, BIF_ARG_1, BIF_ARG_2, 0, NULL);
	} else if (is_flatmap(BIF_ARG_2)) {
	    /* Will always become a tree */
	    return map_merge_mixed(BIF_P, BIF_ARG_2, BIF_ARG_1, 1);
	}
	BIF_P->fvalue = BIF_ARG_2;
    } else {
	BIF_P->fvalue = BIF_ARG_1;
    }
    BIF_ERROR(BIF_P, BADMAP);
}

static Eterm flatmap_merge(Process *p, Eterm nodeA, Eterm nodeB) {
    Eterm *hp,*thp;
    Eterm tup;
    Eterm *ks,*vs,*ks1,*vs1,*ks2,*vs2;
    flatmap_t *mp1,*mp2,*mp_new;
    Uint n,n1,n2,i1,i2,need,unused_size=0;
    Sint c = 0;

    mp1  = (flatmap_t*)flatmap_val(nodeA);
    mp2  = (flatmap_t*)flatmap_val(nodeB);
    n1   = flatmap_get_size(mp1);
    n2   = flatmap_get_size(mp2);

    need = MAP_HEADER_FLATMAP_SZ + 1 + 2 * (n1 + n2);

    hp     = HAlloc(p, need);
    thp    = hp;
    tup    = make_tuple(thp);
    ks     = hp + 1; hp += 1 + n1 + n2;
    mp_new = (flatmap_t*)hp; hp += MAP_HEADER_FLATMAP_SZ;
    vs     = hp; hp += n1 + n2;

    mp_new->thing_word = MAP_HEADER_FLATMAP;
    mp_new->size = 0;
    mp_new->keys = tup;

    i1  = 0; i2 = 0;
    ks1 = flatmap_get_keys(mp1);
    vs1 = flatmap_get_values(mp1);
    ks2 = flatmap_get_keys(mp2);
    vs2 = flatmap_get_values(mp2);

    while(i1 < n1 && i2 < n2) {
	c = CMP_TERM(ks1[i1],ks2[i2]);
	if (c == 0) {
	    /* use righthand side arguments map value,
	     * but advance both maps */
	    *ks++ = ks2[i2];
	    *vs++ = vs2[i2];
	    i1++, i2++, unused_size++;
	} else if (c < 0) {
	    *ks++ = ks1[i1];
	    *vs++ = vs1[i1];
	    i1++;
	} else {
	    *ks++ = ks2[i2];
	    *vs++ = vs2[i2];
	    i2++;
	}
    }

    /* copy remaining */
    while (i1 < n1) {
	*ks++ = ks1[i1];
	*vs++ = vs1[i1];
	i1++;
    }

    while (i2 < n2) {
	*ks++ = ks2[i2];
	*vs++ = vs2[i2];
	i2++;
    }

    if (unused_size) {
	/* the key tuple is embedded in the heap, write a bignum to clear it.
	 *
	 * release values as normal since they are on the top of the heap
	 * size = n1 + n1 - unused_size
	 */

	*ks = make_pos_bignum_header(unused_size - 1);
	HRelease(p, vs + unused_size, vs);
    }

    n = n1 + n2 - unused_size;
    *thp = make_arityval(n);
    mp_new->size = n;

    /* Reshape map to a hashmap if the map exceeds the limit */

    if (n > MAP_SMALL_MAP_LIMIT) {
	Uint32 hx,sw;
	Uint i;
	Eterm res;
	hxnode_t *hxns;
        ErtsHeapFactory factory;

	ks = flatmap_get_keys(mp_new);
	vs = flatmap_get_values(mp_new);

	hp = HAlloc(p, 2 * n);

	hxns = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP,n * sizeof(hxnode_t));

	for (i = 0; i < n; i++) {
	    hx = hashmap_make_hash(ks[i]);
	    swizzle32(sw,hx);
	    hxns[i].hx   = sw;
	    hxns[i].val  = CONS(hp, ks[i], vs[i]); hp += 2;
	    hxns[i].skip = 1;
	    hxns[i].i    = i;
	}

        erts_factory_proc_init(&factory, p);
	res = hashmap_from_unsorted_array(&factory, hxns, n, 0);
	erts_factory_close(&factory);

	erts_free(ERTS_ALC_T_TMP, (void *) hxns);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);

	return res;
    }

    return make_flatmap(mp_new);
}

static Eterm map_merge_mixed(Process *p, Eterm flat, Eterm tree, int swap_args) {
    Eterm *ks, *vs, *hp, res;
    flatmap_t *mp;
    Uint n, i;
    hxnode_t *hxns;
    Uint32 sw, hx;
    ErtsHeapFactory factory;

    /* convert flat to tree */

    ASSERT(is_flatmap(flat));
    ASSERT(is_hashmap(tree));

    mp = (flatmap_t*)flatmap_val(flat);
    n  = flatmap_get_size(mp);

    ks = flatmap_get_keys(mp);
    vs = flatmap_get_values(mp);

    hp = HAlloc(p, 2 * n);

    hxns = (hxnode_t *)erts_alloc(ERTS_ALC_T_TMP, n * sizeof(hxnode_t));

    for (i = 0; i < n; i++) {
	hx = hashmap_make_hash(ks[i]);
	swizzle32(sw,hx);
	hxns[i].hx   = sw;
	hxns[i].val  = CONS(hp, ks[i], vs[i]); hp += 2;
	hxns[i].skip = 1;
	hxns[i].i    = i;
    }

    erts_factory_proc_init(&factory, p);
    res = hashmap_from_unsorted_array(&factory, hxns, n, 0);
    erts_factory_close(&factory);

    erts_free(ERTS_ALC_T_TMP, (void *) hxns);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);

    return hashmap_merge(p, res, tree, swap_args, NULL);
}

#define PSTACK_TYPE struct HashmapMergePStackType
struct HashmapMergePStackType {
    Eterm nodeA, nodeB;
    Eterm *srcA, *srcB;
    Uint32 abm, bbm, rbm; /* node bitmaps */
    int mix;       /* &1: there are unique A stuff in node
                    * &2: there are unique B stuff in node */
    int ix;
    Eterm array[16];   /* temp node construction area */
};

typedef struct HashmapMergeContext_ {
    Uint size;  /* total key-value counter */
    unsigned int lvl;
    Eterm trap_bin;
    ErtsPStack pstack;
#ifdef DEBUG
    Eterm dbg_map_A, dbg_map_B;
#endif
} HashmapMergeContext;

static void hashmap_merge_ctx_destructor(Binary* ctx_bin)
{
    HashmapMergeContext* ctx = (HashmapMergeContext*) ERTS_MAGIC_BIN_DATA(ctx_bin);
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(ctx_bin) == hashmap_merge_ctx_destructor);

    PSTACK_DESTROY_SAVED(&ctx->pstack);
}

BIF_RETTYPE maps_merge_trap_1(BIF_ALIST_1) {
    Binary* ctx_bin = ((ProcBin *) binary_val(BIF_ARG_1))->val;

    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(ctx_bin) == hashmap_merge_ctx_destructor);

    return hashmap_merge(BIF_P, NIL, NIL, 0,
                         (HashmapMergeContext*) ERTS_MAGIC_BIN_DATA(ctx_bin));
}

#define HALLOC_EXTRA 200
#define MAP_MERGE_LOOP_FACTOR 8

static BIF_RETTYPE hashmap_merge(Process *p, Eterm map_A, Eterm map_B,
                                 int swap_args, HashmapMergeContext* ctx) {
#define PSTACK_TYPE struct HashmapMergePStackType
    PSTACK_DECLARE(s, 4);
    HashmapMergeContext local_ctx;
    struct HashmapMergePStackType* sp;
    Uint32 hx;
    Eterm res = THE_NON_VALUE;
    Eterm hdrA, hdrB;
    Eterm *hp, *nhp;
    Eterm trap_ret;
    Sint initial_reds = (Sint) (ERTS_BIF_REDS_LEFT(p) * MAP_MERGE_LOOP_FACTOR);
    Sint reds =  initial_reds;
    DeclareTmpHeap(th,2,p);
    UseTmpHeap(2,p);

    /*
     * Strategy: Do depth-first traversal of both trees (at the same time)
     * and merge each pair of nodes.
     */

    PSTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);

    if (ctx == NULL) { /* first call */
        hashmap_head_t* a = (hashmap_head_t*) hashmap_val(map_A);
        hashmap_head_t* b = (hashmap_head_t*) hashmap_val(map_B);

        sp = PSTACK_PUSH(s);
        sp->srcA = swap_args ? &map_B : &map_A;
        sp->srcB = swap_args ? &map_A : &map_B;
        sp->mix = 0;
        local_ctx.size = a->size + b->size;
        local_ctx.lvl = 0;
    #ifdef DEBUG
        local_ctx.dbg_map_A = map_A;
        local_ctx.dbg_map_B = map_B;
        local_ctx.trap_bin = THE_NON_VALUE;
    #endif
        ctx = &local_ctx;
    }
    else {
        PSTACK_RESTORE(s, &ctx->pstack);
        sp = PSTACK_TOP(s);
        goto resume_from_trap;
    }

recurse:

    sp->nodeA = *sp->srcA;
    sp->nodeB = *sp->srcB;

    if (sp->nodeA == sp->nodeB) {
        res = sp->nodeA;
        ctx->size -= is_list(sp->nodeB) ? 1 : hashmap_subtree_size(sp->nodeB);
    }
    else {
        if (is_list(sp->nodeA)) { /* A is LEAF */
            Eterm keyA = CAR(list_val(sp->nodeA));

            if (is_list(sp->nodeB)) { /* LEAF + LEAF */
                Eterm keyB = CAR(list_val(sp->nodeB));

                if (EQ(keyA, keyB)) {
                    --ctx->size;
                    res = sp->nodeB;
                    sp->mix = 2;   /* We assume values differ.
                                      + Don't spend time comparing big values.
                                      - Might waste some heap space for internal
                                        nodes that could otherwise be reused. */
                    goto merge_nodes;
                }
            }
            hx = hashmap_restore_hash(th, ctx->lvl, keyA);
            sp->abm = 1 << hashmap_index(hx);
            /* keep srcA pointing at the leaf */
        }
        else { /* A is NODE */
            sp->srcA = boxed_val(sp->nodeA);
            hdrA = *sp->srcA++;
            ASSERT(is_header(hdrA));
            switch (hdrA & _HEADER_MAP_SUBTAG_MASK) {
            case HAMT_SUBTAG_HEAD_ARRAY: {
                sp->srcA++;
                sp->abm = 0xffff;
                break;
            }
            case HAMT_SUBTAG_HEAD_BITMAP: sp->srcA++;
            case HAMT_SUBTAG_NODE_BITMAP: {
                sp->abm = MAP_HEADER_VAL(hdrA);
                break;
            }
            default:
                erts_exit(ERTS_ABORT_EXIT, "bad header %ld\r\n", hdrA);
            }
        }

        if (is_list(sp->nodeB)) { /* B is LEAF */
            Eterm keyB = CAR(list_val(sp->nodeB));

            hx = hashmap_restore_hash(th, ctx->lvl, keyB);
            sp->bbm = 1 << hashmap_index(hx);
            /* keep srcB pointing at the leaf */
        }
        else { /* B is NODE */
            sp->srcB = boxed_val(sp->nodeB);
            hdrB = *sp->srcB++;
            ASSERT(is_header(hdrB));
            switch (hdrB & _HEADER_MAP_SUBTAG_MASK) {
            case HAMT_SUBTAG_HEAD_ARRAY: {
                sp->srcB++;
                sp->bbm = 0xffff;
                break;
            }
            case HAMT_SUBTAG_HEAD_BITMAP: sp->srcB++;
            case HAMT_SUBTAG_NODE_BITMAP: {
                sp->bbm = MAP_HEADER_VAL(hdrB);
                break;
            }
            default:
                erts_exit(ERTS_ABORT_EXIT, "bad header %ld\r\n", hdrB);
            }
        }
    }

merge_nodes:

    for (;;) {
	if (is_value(res)) { /* We have a complete (sub-)tree or leaf */
            int child_mix;
	    if (ctx->lvl == 0)
		break;

	    /* Pop from stack and continue build parent node */
	    ctx->lvl--;
            child_mix = sp->mix;
	    sp = PSTACK_POP(s);
	    sp->array[sp->ix++] = res;
            sp->mix |= child_mix;
	    res = THE_NON_VALUE;
	    if (sp->rbm) {
		sp->srcA++;
		sp->srcB++;
	    }
	} else { /* Start build a node */
	    sp->ix = 0;
	    sp->rbm = sp->abm | sp->bbm;
	    ASSERT(!(sp->rbm == 0 && ctx->lvl > 0));
	}

        if (--reds <= 0) {
            goto trap;
        }
resume_from_trap:

	while (sp->rbm) {
	    Uint32 next = sp->rbm & (sp->rbm-1);
	    Uint32 bit = sp->rbm ^ next;
	    sp->rbm = next;
	    if (sp->abm & bit) {
		if (sp->bbm & bit) {
		    /* Bit clash. Push and resolve by recursive merge */
		    Eterm* srcA = sp->srcA;
		    Eterm* srcB = sp->srcB;
		    ctx->lvl++;
		    sp = PSTACK_PUSH(s);
                    sp->srcA = srcA;
                    sp->srcB = srcB;
                    sp->mix = 0;
		    goto recurse;
		} else {
		    sp->array[sp->ix++] = *sp->srcA++;
                    sp->mix |= 1;
		}
	    } else {
		ASSERT(sp->bbm & bit);
		sp->array[sp->ix++] = *sp->srcB++;
                sp->mix |=  2;
	    }
	}

        switch (sp->mix) {
        case 0: /* Nodes A and B contain the *EXACT* same sub-trees
                   => fall through and reuse nodeA */

        case 1: /* Only unique A stuff => reuse nodeA */
            res = sp->nodeA;
            break;

        case 2: /* Only unique B stuff => reuse nodeB */
            res = sp->nodeB;
            break;

        case 3: /* We have a mix => must build new node */
            ASSERT(sp->ix == hashmap_bitcount(sp->abm | sp->bbm));
            if (ctx->lvl == 0) {
                nhp = HAllocX(p, HAMT_HEAD_BITMAP_SZ(sp->ix), HALLOC_EXTRA);
                hp = nhp;
                *hp++ = (sp->ix == 16 ? MAP_HEADER_HAMT_HEAD_ARRAY
                         : MAP_HEADER_HAMT_HEAD_BITMAP(sp->abm | sp->bbm));
                *hp++ = ctx->size;
            } else {
                nhp = HAllocX(p, HAMT_NODE_BITMAP_SZ(sp->ix), HALLOC_EXTRA);
                hp = nhp;
                *hp++ = MAP_HEADER_HAMT_NODE_BITMAP(sp->abm | sp->bbm);
            }
            sys_memcpy(hp, sp->array, sp->ix * sizeof(Eterm));
            res = make_boxed(nhp);
            break;
        default:
            erts_exit(ERTS_ABORT_EXIT, "strange mix %d\r\n", sp->mix);
        }
    }

    /* Done */

#ifdef DEBUG
    {
        Eterm *head = hashmap_val(res);
        Uint size = head[1];
        Uint real_size = hashmap_subtree_size(res);
        ASSERT(size == real_size);
    }
#endif

    if (ctx != &local_ctx) {
        ASSERT(ctx->trap_bin != THE_NON_VALUE);
        ASSERT(p->flags & F_DISABLE_GC);
        erts_set_gc_state(p, 1);
    }
    else {
        ASSERT(ctx->trap_bin == THE_NON_VALUE);
        ASSERT(!(p->flags & F_DISABLE_GC));
    }
    PSTACK_DESTROY(s);
    UnUseTmpHeap(2,p);
    BUMP_REDS(p, (initial_reds - reds) / MAP_MERGE_LOOP_FACTOR);
    return res;

trap:  /* Yield */

    if (ctx == &local_ctx) {
        Binary* ctx_b = erts_create_magic_binary(sizeof(HashmapMergeContext),
                                                 hashmap_merge_ctx_destructor);
        ctx = ERTS_MAGIC_BIN_DATA(ctx_b);
        sys_memcpy(ctx, &local_ctx, sizeof(HashmapMergeContext));
        hp = HAlloc(p, PROC_BIN_SIZE);
        ASSERT(ctx->trap_bin == THE_NON_VALUE);
        ctx->trap_bin = erts_mk_magic_binary_term(&hp, &MSO(p), ctx_b);

        erts_set_gc_state(p, 0);
    }
    else {
        ASSERT(ctx->trap_bin != THE_NON_VALUE);
        ASSERT(p->flags & F_DISABLE_GC);
    }

    PSTACK_SAVE(s, &ctx->pstack);

    BUMP_ALL_REDS(p);
    ERTS_BIF_PREP_TRAP1(trap_ret, &hashmap_merge_trap_export,
                        p, ctx->trap_bin);
    UnUseTmpHeap(2,p);
    return trap_ret;
}

static Uint hashmap_subtree_size(Eterm node) {
    DECLARE_WSTACK(stack);
    Uint size = 0;

    hashmap_iterator_init(&stack, node, 0);
    while (hashmap_iterator_next(&stack)) {
        size++;
    }
    DESTROY_WSTACK(stack);
    return size;
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
    unsigned int lvl = 0;
    DeclareTmpHeapNoproc(th,2);
    UseTmpHeapNoproc(2);

    if (ap && bp) {
	ASSERT(CMP_TERM(CAR(ap), CAR(bp)) != 0);
	for (;;) {
	    Uint32 ha = hashmap_restore_hash(th, lvl, CAR(ap));
	    Uint32 hb = hashmap_restore_hash(th, lvl, CAR(bp));
	    int cmp = hash_cmp(ha, hb);
	    if (cmp) {
                UnUseTmpHeapNoproc(2);
		return cmp;
            }
	    lvl += 8;
	}
    }
    UnUseTmpHeapNoproc(2);
    return ap ? -1 : 1;
}

/* maps:new/0 */

BIF_RETTYPE maps_new_0(BIF_ALIST_0) {
    Eterm* hp;
    Eterm tup;
    flatmap_t *mp;

    hp    = HAlloc(BIF_P, (MAP_HEADER_FLATMAP_SZ + 1));
    tup   = make_tuple(hp);
    *hp++ = make_arityval(0);

    mp    = (flatmap_t*)hp;
    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = 0;
    mp->keys = tup;

    BIF_RET(make_flatmap(mp));
}

/* maps:put/3 */

BIF_RETTYPE maps_put_3(BIF_ALIST_3) {
    if (is_map(BIF_ARG_3)) {
	BIF_RET(erts_maps_put(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3));
    }
    BIF_P->fvalue = BIF_ARG_3;
    BIF_ERROR(BIF_P, BADMAP);
}

/* maps:remove/3 */

int erts_maps_remove(Process *p, Eterm key, Eterm map, Eterm *res) {
    Uint32 hx;
    if (is_flatmap(map)) {
	Sint n;
	Uint need;
	Eterm *hp_start;
	Eterm *thp, *mhp;
	Eterm *ks, *vs, tup;
	flatmap_t *mp = (flatmap_t*)flatmap_val(map);

	n = flatmap_get_size(mp);

	if (n == 0) {
	    *res = map;
	    return 1;
	}

	ks = flatmap_get_keys(mp);
	vs = flatmap_get_values(mp);

	/* Assume key exists.
	 * Release allocated if it didn't.
	 * Allocate key tuple first.
	 */

	need   = n + 1 - 1 + 3 + n - 1; /* tuple - 1 + map - 1 */
	hp_start = HAlloc(p, need);
	thp    = hp_start;
	mhp    = thp + n;               /* offset with tuple heap size */

	tup    = make_tuple(thp);
	*thp++ = make_arityval(n - 1);

	*res   = make_flatmap(mhp);
	*mhp++ = MAP_HEADER_FLATMAP;
	*mhp++ = n - 1;
	*mhp++ = tup;

	if (is_immed(key)) {
	    while (1) {
		if (*ks == key) {
		    goto found_key;
		} else if (--n) {
		    *mhp++ = *vs++;
		    *thp++ = *ks++;
		} else
		    break;
	    }
	} else {
	    while(1) {
		if (EQ(*ks, key)) {
		    goto found_key;
		} else if (--n) {
		    *mhp++ = *vs++;
		    *thp++ = *ks++;
		} else
		    break;
	    }
	}

	/* Not found, remove allocated memory
	 * and return previous map.
	 */
	HRelease(p, hp_start + need, hp_start);

	*res = map;
	return 1;

found_key:
	/* Copy rest of keys and values */
	if (--n) {
	    sys_memcpy(mhp, vs+1, n*sizeof(Eterm));
	    sys_memcpy(thp, ks+1, n*sizeof(Eterm));
	}
	return 1;
    }
    ASSERT(is_hashmap(map));
    hx = hashmap_make_hash(key);
    *res = hashmap_delete(p, hx, key, map);
    return 1;
}

BIF_RETTYPE maps_remove_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
	Eterm res;
	if (erts_maps_remove(BIF_P, BIF_ARG_1, BIF_ARG_2, &res)) {
	    BIF_RET(res);
	}
    }
    BIF_P->fvalue = BIF_ARG_2;
    BIF_ERROR(BIF_P, BADMAP);
}

int erts_maps_update(Process *p, Eterm key, Eterm value, Eterm map, Eterm *res) {
    Uint32 hx;
    if (is_flatmap(map)) {
	Sint n,i;
	Eterm* hp,*shp;
	Eterm *ks,*vs;
	flatmap_t *mp = (flatmap_t*)flatmap_val(map);

	if ((n = flatmap_get_size(mp)) == 0) {
	    return 0;
	}

	ks  = flatmap_get_keys(mp);
	vs  = flatmap_get_values(mp);

	/* only allocate for values,
	 * assume key-tuple will be intact
	 */

	hp  = HAlloc(p, MAP_HEADER_FLATMAP_SZ + n);
	shp = hp;
	*hp++ = MAP_HEADER_FLATMAP;
	*hp++ = n;
	*hp++ = mp->keys;

	if (is_immed(key)) {
	    for( i = 0; i < n; i ++) {
		if (ks[i] == key) {
		    goto found_key;
		} else {
		    *hp++ = *vs++;
		}
	    }
	} else {
	    for( i = 0; i < n; i ++) {
		if (EQ(ks[i], key)) {
		    goto found_key;
		} else {
		    *hp++ = *vs++;
		}
	    }
	}

	HRelease(p, shp + MAP_HEADER_FLATMAP_SZ + n, shp);
	return 0;

found_key:
	*hp++ = value;
	vs++;
	if (++i < n)
	    sys_memcpy(hp, vs, (n - i)*sizeof(Eterm));
	*res = make_flatmap(shp);
	return 1;
    }

    ASSERT(is_hashmap(map));
    hx = hashmap_make_hash(key);
    *res = erts_hashmap_insert(p, hx, key, value, map, 1);
    if (is_value(*res))
	return 1;

    return 0;
}

Eterm erts_maps_put(Process *p, Eterm key, Eterm value, Eterm map) {
    Uint32 hx;
    Eterm res;
    if (is_flatmap(map)) {
	Sint n,i;
	Sint c = 0;
	Eterm* hp, *shp;
	Eterm *ks, *vs, tup;
	flatmap_t *mp = (flatmap_t*)flatmap_val(map);

	n = flatmap_get_size(mp);

	if (n == 0) {
	    hp    = HAlloc(p, MAP_HEADER_FLATMAP_SZ + 1 + 2);
	    tup   = make_tuple(hp);
	    *hp++ = make_arityval(1);
	    *hp++ = key;
	    res   = make_flatmap(hp);
	    *hp++ = MAP_HEADER_FLATMAP;
	    *hp++ = 1;
	    *hp++ = tup;
	    *hp++ = value;

	    return res;
	}

	ks = flatmap_get_keys(mp);
	vs = flatmap_get_values(mp);

	/* only allocate for values,
	 * assume key-tuple will be intact
	 */

	hp  = HAlloc(p, MAP_HEADER_FLATMAP_SZ + n);
	shp = hp; /* save hp, used if optimistic update fails */
	res = make_flatmap(hp);
	*hp++ = MAP_HEADER_FLATMAP;
	*hp++ = n;
	*hp++ = mp->keys;

	if (is_immed(key)) {
	    for( i = 0; i < n; i ++) {
		if (ks[i] == key) {
		    *hp++ = value;
		    vs++;
		    c = 1;
		} else {
		    *hp++ = *vs++;
		}
	    }
	} else {
	    for( i = 0; i < n; i ++) {
		if (EQ(ks[i], key)) {
		    *hp++ = value;
		    vs++;
		    c = 1;
		} else {
		    *hp++ = *vs++;
		}
	    }
	}

	if (c)
	    return res;

	/* the map will grow */

	if (n >= MAP_SMALL_MAP_LIMIT) {
	    HRelease(p, shp + MAP_HEADER_FLATMAP_SZ + n, shp);
	    ks = flatmap_get_keys(mp);
	    vs = flatmap_get_values(mp);

	    res = erts_hashmap_from_ks_and_vs_extra(p,ks,vs,n,key,value);

	    return res;
	}

	/* still a small map. need to make a new tuple,
	 * use old hp since it needs to be recreated anyway. */

	tup    = make_tuple(shp);
	*shp++ = make_arityval(n+1);

	hp    = HAlloc(p, 3 + n + 1);
	res   = make_flatmap(hp);
	*hp++ = MAP_HEADER_FLATMAP;
	*hp++ = n + 1;
	*hp++ = tup;

	ks = flatmap_get_keys(mp);
	vs = flatmap_get_values(mp);

	ASSERT(n >= 0);

	/* copy map in order */
	while (n && ((c = CMP_TERM(*ks, key)) < 0)) {
	    *shp++ = *ks++;
	    *hp++  = *vs++;
	    n--;
	}

	*shp++ = key;
	*hp++  = value;

	ASSERT(n >= 0);

	while(n--) {
	    *shp++ = *ks++;
	    *hp++  = *vs++;
	}
	/* we have one word remaining
	 * this will work out fine once we get the size word
	 * in the header.
	 */
	*shp = make_pos_bignum_header(0);
	return res;
    }
    ASSERT(is_hashmap(map));

    hx  = hashmap_make_hash(key);
    res = erts_hashmap_insert(p, hx, key, value, map, 0);
    ASSERT(is_hashmap(res));

    return res;
}

/* maps:update/3 */

BIF_RETTYPE maps_update_3(BIF_ALIST_3) {
    if (is_not_map(BIF_ARG_3)) {
	BIF_P->fvalue = BIF_ARG_3;
	BIF_ERROR(BIF_P, BADMAP);
    } else {
	Eterm res;
	if (erts_maps_update(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &res)) {
	    BIF_RET(res);
	}
	BIF_P->fvalue = BIF_ARG_1;
	BIF_ERROR(BIF_P, BADKEY);
    }
}


/* maps:values/1 */

BIF_RETTYPE maps_values_1(BIF_ALIST_1) {
    if (is_flatmap(BIF_ARG_1)) {
	Eterm *hp, *vs, res = NIL;
	flatmap_t *mp;
	Uint n;

	mp  = (flatmap_t*)flatmap_val(BIF_ARG_1);
	n   = flatmap_get_size(mp);

	if (n == 0)
	    BIF_RET(res);

	hp  = HAlloc(BIF_P, (2 * n));
	vs  = flatmap_get_values(mp);

	while(n--) {
	    res = CONS(hp, vs[n], res); hp += 2;
	}

	BIF_RET(res);
    } else if (is_hashmap(BIF_ARG_1)) {
	BIF_RET(hashmap_values(BIF_P, BIF_ARG_1));
    }
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, BADMAP);
}

static Eterm hashmap_to_list(Process *p, Eterm node) {
    DECLARE_WSTACK(stack);
    Eterm *hp, *kv;
    Eterm res = NIL;

    hp  = HAlloc(p, hashmap_size(node) * (2 + 3));
    hashmap_iterator_init(&stack, node, 0);
    while ((kv=hashmap_iterator_next(&stack)) != NULL) {
	Eterm tup = TUPLE2(hp, CAR(kv), CDR(kv));
	hp += 3;
	res = CONS(hp, tup, res);
	hp += 2;
    }
    DESTROY_WSTACK(stack);
    return res;
}

void hashmap_iterator_init(ErtsWStack* s, Eterm node, int reverse) {
    Eterm hdr = *hashmap_val(node);
    Uint sz;

    switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
    case HAMT_SUBTAG_HEAD_ARRAY:
	sz = 16;
	break;
    case HAMT_SUBTAG_HEAD_BITMAP:
    case HAMT_SUBTAG_NODE_BITMAP:
        sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
	break;
    default:
	erts_exit(ERTS_ABORT_EXIT, "bad header");
    }

    WSTACK_PUSH3((*s), (UWord)THE_NON_VALUE,  /* end marker */
		 (UWord)(!reverse ? 0 : sz+1),
		 (UWord)node);
}

Eterm* hashmap_iterator_next(ErtsWStack* s) {
    Eterm node, *ptr, hdr;
    Uint32 sz;
    Uint idx;

    for (;;) {
        ASSERT(!WSTACK_ISEMPTY((*s)));
	node = (Eterm) WSTACK_POP((*s));
        if (is_non_value(node)) {
            return NULL;
        }
	idx = (Uint) WSTACK_POP((*s));
        for (;;) {
	    ASSERT(is_boxed(node));
	    ptr = boxed_val(node);
	    hdr = *ptr;
	    ASSERT(is_header(hdr));
	    switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
	    case HAMT_SUBTAG_HEAD_ARRAY:
		ptr++;
		sz = 16;
		break;
	    case HAMT_SUBTAG_HEAD_BITMAP:
		ptr++;
	    case HAMT_SUBTAG_NODE_BITMAP:
		sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
		ASSERT(sz < 17);
		break;
	    default:
		erts_exit(ERTS_ABORT_EXIT, "bad header");
	    }

	    idx++;

	    if (idx <= sz) {
		WSTACK_PUSH2((*s), (UWord)idx, (UWord)node);

		if (is_list(ptr[idx])) {
		    return list_val(ptr[idx]);
		}
		ASSERT(is_boxed(ptr[idx]));
		node = ptr[idx];
		idx = 0;
	    }
	    else
		break; /* and pop parent node */
        }
    }
}

Eterm* hashmap_iterator_prev(ErtsWStack* s) {
    Eterm node, *ptr, hdr;
    Uint32 sz;
    Uint idx;

    for (;;) {
        ASSERT(!WSTACK_ISEMPTY((*s)));
	node = (Eterm) WSTACK_POP((*s));
        if (is_non_value(node)) {
            return NULL;
        }
	idx = (Uint) WSTACK_POP((*s));
        for (;;) {
	    ASSERT(is_boxed(node));
	    ptr = boxed_val(node);
	    hdr = *ptr;
	    ASSERT(is_header(hdr));
	    switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
	    case HAMT_SUBTAG_HEAD_ARRAY:
		ptr++;
		sz = 16;
		break;
	    case HAMT_SUBTAG_HEAD_BITMAP:
		ptr++;
	    case HAMT_SUBTAG_NODE_BITMAP:
		sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
		ASSERT(sz < 17);
		break;
	    default:
		erts_exit(ERTS_ERROR_EXIT, "bad header");
	    }

            if (idx > sz)
		idx = sz;
	    else
		idx--;

	    if (idx >= 1) {
		WSTACK_PUSH2((*s), (UWord)idx, (UWord)node);

		if (is_list(ptr[idx])) {
		    return list_val(ptr[idx]);
		}
		ASSERT(is_boxed(ptr[idx]));
		node = ptr[idx];
		idx = 17;
	    }
	    else
		break; /* and pop parent node */
        }
    }
}

const Eterm *
#if HALFWORD_HEAP
erts_hashmap_get_rel(Uint32 hx, Eterm key, Eterm node, Eterm *map_base)
#else
erts_hashmap_get(Uint32 hx, Eterm key, Eterm node)
#endif
{
    Eterm *ptr, hdr, *res;
    Uint ix, lvl = 0;
    Uint32 hval,bp;
    DeclareTmpHeapNoproc(th,2);
    UseTmpHeapNoproc(2);

    ASSERT(is_boxed(node));
    ptr = boxed_val_rel(node, map_base);
    hdr = *ptr;
    ASSERT(is_header(hdr));
    ASSERT(is_hashmap_header_head(hdr));
    ptr++;

    for (;;) {
        hval = MAP_HEADER_VAL(hdr);
        ix   = hashmap_index(hx);
        if (hval != 0xffff) {
            bp   = 1 << ix;
            if (!(bp & hval)) {
                /* not occupied */
                res = NULL;
                break;
            }
            ix = hashmap_bitcount(hval & (bp - 1));
        }
        node  = ptr[ix+1];

        if (is_list(node)) { /* LEAF NODE [K|V] */
            ptr = list_val_rel(node,map_base);
            res = eq_rel(CAR(ptr), map_base, key, NULL) ? &(CDR(ptr)) : NULL;
            break;
        }

        hx = hashmap_shift_hash(th,hx,lvl,key);

        ASSERT(is_boxed(node));
        ptr = boxed_val_rel(node, map_base);
        hdr = *ptr;
        ASSERT(is_header(hdr));
        ASSERT(!is_hashmap_header_head(hdr));
    }

    UnUseTmpHeapNoproc(2);
    return res;
}

Eterm erts_hashmap_insert(Process *p, Uint32 hx, Eterm key, Eterm value,
			  Eterm map, int is_update) {
    Uint size, upsz;
    Eterm *hp, res = THE_NON_VALUE;
    DECLARE_ESTACK(stack);
    if (erts_hashmap_insert_down(hx, key, map, &size, &upsz, &stack, is_update)) {
	hp  = HAlloc(p, size);
	res = erts_hashmap_insert_up(hp, key, value, &upsz, &stack);
    }

    DESTROY_ESTACK(stack);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);
    ERTS_HOLE_CHECK(p);

    return res;
}


int erts_hashmap_insert_down(Uint32 hx, Eterm key, Eterm node, Uint *sz,
			     Uint *update_size, ErtsEStack *sp, int is_update) {
    Eterm *ptr;
    Eterm hdr, ckey;
    Uint32 ix, cix, bp, hval, chx;
    Uint slot, lvl = 0, clvl;
    Uint size = 0, n = 0;
    DeclareTmpHeapNoproc(th,2);

    *update_size = 1;

    UseTmpHeapNoproc(2);
    for (;;) {
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST: /* LEAF NODE [K|V] */
		ptr  = list_val(node);
		ckey = CAR(ptr);
		if (EQ(ckey, key)) {
		    *update_size = 0;
		    goto unroll;
		}
		if (is_update) {
                    UnUseTmpHeapNoproc(2);
		    return 0;
		}
		goto insert_subnodes;
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));

		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_HEAD_ARRAY:
			ix    = hashmap_index(hx);
			hx    = hashmap_shift_hash(th,hx,lvl,key);
			size += HAMT_HEAD_ARRAY_SZ;
			ESTACK_PUSH2(*sp, ix, node);
			node  = ptr[ix+2];
			break;
		    case HAMT_SUBTAG_NODE_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
                        bp   = 1 << ix;
                        if (hval == 0xffff) {
                            slot = ix;
                            n = 16;
                        } else {
                            slot = hashmap_bitcount(hval & (bp - 1));
                            n    = hashmap_bitcount(hval);
                        }

                        ESTACK_PUSH4(*sp, n, bp, slot, node);

                        if (!(bp & hval)) { /* not occupied */
                            if (is_update) {
				UnUseTmpHeapNoproc(2);
                                return 0;
                            }
                            size += HAMT_NODE_BITMAP_SZ(n+1);
                            goto unroll;
                        }

                        hx    = hashmap_shift_hash(th,hx,lvl,key);
                        node  = ptr[slot+1];
                        ASSERT(HAMT_NODE_BITMAP_SZ(n) <= 17);
                        size += HAMT_NODE_BITMAP_SZ(n);
                        break;

		    case HAMT_SUBTAG_HEAD_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
			bp   = 1 << ix;
			slot = hashmap_bitcount(hval & (bp - 1));
			n    = hashmap_bitcount(hval);

			ESTACK_PUSH4(*sp, n, bp, slot, node);

			/* occupied */
			if (bp & hval) {
			    hx    = hashmap_shift_hash(th,hx,lvl,key);
			    node  = ptr[slot+2];
			    ASSERT(HAMT_HEAD_BITMAP_SZ(n) <= 18);
			    size += HAMT_HEAD_BITMAP_SZ(n);
			    break;
			}
			/* not occupied */
			if (is_update) {
                            UnUseTmpHeapNoproc(2);
			    return 0;
			}
			size += HAMT_HEAD_BITMAP_SZ(n+1);
			goto unroll;
		    default:
			erts_exit(ERTS_ERROR_EXIT, "bad header tag %ld\r\n", hdr & _HEADER_MAP_SUBTAG_MASK);
			break;
		}
		break;
	    default:
		erts_exit(ERTS_ERROR_EXIT, "bad primary tag %p\r\n", node);
		break;
	}
    }
insert_subnodes:
    clvl  = lvl;
    chx   = hashmap_restore_hash(th,clvl,ckey);
    size += HAMT_NODE_BITMAP_SZ(2);
    ix    = hashmap_index(hx);
    cix   = hashmap_index(chx);

    while (cix == ix) {
	ESTACK_PUSH4(*sp, 0, 1 << ix, 0, MAP_HEADER_HAMT_NODE_BITMAP(0));
	size += HAMT_NODE_BITMAP_SZ(1);
	hx    = hashmap_shift_hash(th,hx,lvl,key);
	chx   = hashmap_shift_hash(th,chx,clvl,ckey);
	ix    = hashmap_index(hx);
	cix   = hashmap_index(chx);
    }
    ESTACK_PUSH3(*sp, cix, ix, node);

unroll:
    *sz = size + /* res cons */ 2;
    UnUseTmpHeapNoproc(2);
    return 1;
}

Eterm erts_hashmap_insert_up(Eterm *hp, Eterm key, Eterm value,
			     Uint *update_size, ErtsEStack *sp) {
    Eterm node, *ptr, hdr;
    Eterm res;
    Eterm *nhp = NULL;
    Uint32 ix, cix, bp, hval;
    Uint slot, n;
    /* Needed for halfword */
    DeclareTmpHeapNoproc(fake,1);
    UseTmpHeapNoproc(1);

    res = CONS(hp, key, value); hp += 2;

    do {
	node = ESTACK_POP(*sp);
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST:
		ix  = (Uint32) ESTACK_POP(*sp);
		cix = (Uint32) ESTACK_POP(*sp);

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
		*fake = node;
		node  = make_boxed(fake);
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));

		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_HEAD_ARRAY:
			slot  = (Uint) ESTACK_POP(*sp);
			nhp   = hp;
			n     = HAMT_HEAD_ARRAY_SZ - 2;
			*hp++ = MAP_HEADER_HAMT_HEAD_ARRAY; ptr++;
			*hp++ = (*ptr++) + *update_size;
			while(n--) { *hp++ = *ptr++; }
			nhp[slot+2] = res;
			res = make_hashmap(nhp);
			break;
		    case HAMT_SUBTAG_NODE_BITMAP:
			slot  = (Uint)   ESTACK_POP(*sp);
			bp    = (Uint32) ESTACK_POP(*sp);
			n     = (Uint32) ESTACK_POP(*sp);
			hval  = MAP_HEADER_VAL(hdr);
			nhp   = hp;
			*hp++ = MAP_HEADER_HAMT_NODE_BITMAP(hval | bp); ptr++;

			n -= slot;
			while(slot--) { *hp++ = *ptr++; }
			*hp++ = res;
			if (hval & bp) { ptr++; n--; }
			while(n--) { *hp++ = *ptr++; }

			res = make_hashmap(nhp);
			break;
		    case HAMT_SUBTAG_HEAD_BITMAP:
			slot  = (Uint)   ESTACK_POP(*sp);
			bp    = (Uint32) ESTACK_POP(*sp);
			n     = (Uint32) ESTACK_POP(*sp);
			hval  = MAP_HEADER_VAL(hdr);
			nhp   = hp;
			*hp++ = MAP_HEADER_HAMT_HEAD_BITMAP(hval | bp); ptr++;
			*hp++ = (*ptr++) + *update_size;

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
			erts_exit(ERTS_ERROR_EXIT, "bad header tag %x\r\n", hdr & _HEADER_MAP_SUBTAG_MASK);
			break;
		}
		break;
	    default:
		erts_exit(ERTS_ERROR_EXIT, "bad primary tag %x\r\n", primary_tag(node));
		break;
	}

    } while(!ESTACK_ISEMPTY(*sp));

    UnUseTmpHeapNoproc(1);
    return res;
}

static Eterm hashmap_keys(Process* p, Eterm node) {
    DECLARE_WSTACK(stack);
    hashmap_head_t* root;
    Eterm *hp, *kv;
    Eterm res = NIL;

    root = (hashmap_head_t*) boxed_val(node);
    hp  = HAlloc(p, root->size * 2);
    hashmap_iterator_init(&stack, node, 0);
    while ((kv=hashmap_iterator_next(&stack)) != NULL) {
	res = CONS(hp, CAR(kv), res);
	hp += 2;
    }
    DESTROY_WSTACK(stack);
    return res;
}

static Eterm hashmap_values(Process* p, Eterm node) {
    DECLARE_WSTACK(stack);
    hashmap_head_t* root;
    Eterm *hp, *kv;
    Eterm res = NIL;

    root = (hashmap_head_t*) boxed_val(node);
    hp  = HAlloc(p, root->size * 2);
    hashmap_iterator_init(&stack, node, 0);
    while ((kv=hashmap_iterator_next(&stack)) != NULL) {
	res = CONS(hp, CDR(kv), res);
	hp += 2;
    }
    DESTROY_WSTACK(stack);
    return res;
}

static Eterm hashmap_delete(Process *p, Uint32 hx, Eterm key, Eterm map) {
    Eterm *hp = NULL, *nhp = NULL, *hp_end = NULL;
    Eterm *ptr;
    Eterm hdr, res = map, node = map;
    Uint32 ix, bp, hval;
    Uint slot, lvl = 0;
    Uint size = 0, n = 0;
    DECLARE_ESTACK(stack);
    DeclareTmpHeapNoproc(th,2);
    UseTmpHeapNoproc(2);

    for (;;) {
	switch(primary_tag(node)) {
	    case TAG_PRIMARY_LIST:
		if (EQ(CAR(list_val(node)), key)) {
		    goto unroll;
		}
		goto not_found;
	    case TAG_PRIMARY_BOXED:
		ptr = boxed_val(node);
		hdr = *ptr;
		ASSERT(is_header(hdr));

		switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
		    case HAMT_SUBTAG_HEAD_ARRAY:
			ix    = hashmap_index(hx);
			hx    = hashmap_shift_hash(th,hx,lvl,key);
			size += HAMT_HEAD_ARRAY_SZ;
			ESTACK_PUSH2(stack, ix, node);
			node  = ptr[ix+2];
			break;
		    case HAMT_SUBTAG_NODE_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
			bp   = 1 << ix;
                        if (hval == 0xffff) {
                            slot = ix;
                            n = 16;
                        } else if (bp & hval) {
                            slot = hashmap_bitcount(hval & (bp - 1));
                            n    = hashmap_bitcount(hval);
                        } else {
                            /* not occupied */
                            goto not_found;
                        }

			ESTACK_PUSH4(stack, n, bp, slot, node);

                        hx    = hashmap_shift_hash(th,hx,lvl,key);
                        node  = ptr[slot+1];
                        ASSERT(HAMT_NODE_BITMAP_SZ(n) <= 17);
                        size += HAMT_NODE_BITMAP_SZ(n);
                        break;

		    case HAMT_SUBTAG_HEAD_BITMAP:
			hval = MAP_HEADER_VAL(hdr);
			ix   = hashmap_index(hx);
			bp   = 1 << ix;
			slot = hashmap_bitcount(hval & (bp - 1));
			n    = hashmap_bitcount(hval);

			ESTACK_PUSH4(stack, n, bp, slot, node);

			/* occupied */
			if (bp & hval) {
			    hx    = hashmap_shift_hash(th,hx,lvl,key);
			    node  = ptr[slot+2];
			    ASSERT(HAMT_HEAD_BITMAP_SZ(n) <= 18);
			    size += HAMT_HEAD_BITMAP_SZ(n);
			    break;
			}
			/* not occupied */
			goto not_found;
		    default:
			erts_exit(ERTS_ERROR_EXIT, "bad header tag %ld\r\n", hdr & _HEADER_MAP_SUBTAG_MASK);
			break;
		}
		break;
	    default:
		erts_exit(ERTS_ERROR_EXIT, "bad primary tag %p\r\n", node);
		break;
	}
    }

unroll:
    /* the size is bounded and atleast one less than the previous size */
    size -= 1;
    n     = hashmap_size(map) - 1;

    if (n <= MAP_SMALL_MAP_LIMIT) {
	DECLARE_WSTACK(wstack);
	Eterm *kv, *ks, *vs;
	flatmap_t *mp;
	Eterm keys;

	DESTROY_ESTACK(stack);

	/* build flat structure */
	hp    = HAlloc(p, 3 + 1 + (2 * n));
	keys  = make_tuple(hp);
	*hp++ = make_arityval(n);
	ks    = hp;
	hp   += n;
	mp    = (flatmap_t*)hp;
	hp   += MAP_HEADER_FLATMAP_SZ;
	vs    = hp;

	mp->thing_word = MAP_HEADER_FLATMAP;
	mp->size = n;
	mp->keys = keys;

	hashmap_iterator_init(&wstack, map, 0);

	while ((kv=hashmap_iterator_next(&wstack)) != NULL) {
	    if (EQ(CAR(kv),key))
		continue;
	    *ks++ = CAR(kv);
	    *vs++ = CDR(kv);
	}

	/* it cannot have multiple keys */
	erts_validate_and_sort_flatmap(mp);

	DESTROY_WSTACK(wstack);
        UnUseTmpHeapNoproc(2);
	return make_flatmap(mp);
    }

    hp     = HAlloc(p, size);
    hp_end = hp + size;
    res    = THE_NON_VALUE;

    do {
	node = ESTACK_POP(stack);

	/* all nodes are things */
	ptr = boxed_val(node);
	hdr = *ptr;
	ASSERT(is_header(hdr));

	switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
	    case HAMT_SUBTAG_HEAD_ARRAY:
		ix  = (Uint) ESTACK_POP(stack);
		nhp = hp;
		if (res == THE_NON_VALUE) {
		    n     = 16;
		    n    -= ix;
		    *hp++ = MAP_HEADER_HAMT_HEAD_BITMAP(0xffff ^ (1 << ix)); ptr++;
		    *hp++ = (*ptr++) - 1;
		    while(ix--) { *hp++ = *ptr++; }
		    ptr++; n--;
		    while(n--) { *hp++ = *ptr++; }
		    res = make_hashmap(nhp);
		} else {
		    n     = 16;
		    *hp++ = MAP_HEADER_HAMT_HEAD_ARRAY; ptr++;
		    *hp++ = (*ptr++) - 1;
		    while(n--) { *hp++ = *ptr++; }
		    nhp[ix+2] = res;
		    res = make_hashmap(nhp);
		}
		break;
	    case HAMT_SUBTAG_NODE_BITMAP:
		slot = (Uint)   ESTACK_POP(stack);
		bp   = (Uint32) ESTACK_POP(stack);
		n    = (Uint32) ESTACK_POP(stack);
		nhp  = hp;

		/* bitmap change matrix
		 * res | none    leaf    bitmap
		 * ----------------------------
		 * n=1 | remove  remove  keep
		 * n=2 | other   keep    keep
		 * n>2 | shrink  keep    keep
		 *
		 * other: (remember, n is 2)
		 *   shrink if the other bitmap value is a bitmap node
		 *   remove if the other bitmap value is a leaf
		 *
		 * remove:
		 *   this bitmap node is removed, res is moved up in tree (could be none)
		 *   this is a special case of shrink
		 *
		 * keep:
		 *   the current path index is still used down in the tree, need to keep it
		 *   copy as usual with the updated res
		 *
		 * shrink:
		 *   the current path index is no longer used down in the tree, remove it (shrink)
		 */
		if (res == THE_NON_VALUE) {
		    if (n == 1) {
			break;
		    } else if (n == 2) {
			if (slot == 0) {
			    ix = 2; /* off by one 'cause hdr */
			} else {
			    ix = 1; /* off by one 'cause hdr */
			}
			if (primary_tag(ptr[ix]) == TAG_PRIMARY_LIST) {
			    res = ptr[ix];
			} else {
			    hval  = MAP_HEADER_VAL(hdr);
			    *hp++ = MAP_HEADER_HAMT_NODE_BITMAP(hval ^ bp);
			    *hp++ = ptr[ix];
			    res = make_hashmap(nhp);
			}
		    } else {
			/* n > 2 */
			hval  = MAP_HEADER_VAL(hdr);
			*hp++ = MAP_HEADER_HAMT_NODE_BITMAP(hval ^ bp); ptr++;
			n    -= slot;
			while(slot--) { *hp++ = *ptr++; }
			ptr++; n--;
			while(n--) { *hp++ = *ptr++; }
			res = make_hashmap(nhp);
		    }
		} else if (primary_tag(res) == TAG_PRIMARY_LIST && n == 1) {
		    break;
		} else {
		    /* res is bitmap or leaf && n > 1, keep */
		    n    -= slot;
		    *hp++ = *ptr++;
		    while(slot--) { *hp++ = *ptr++; }
		    *hp++ = res;
		    ptr++; n--;
		    while(n--) { *hp++ = *ptr++; }
		    res = make_hashmap(nhp);
		}
		break;
	    case HAMT_SUBTAG_HEAD_BITMAP:
		slot = (Uint)   ESTACK_POP(stack);
		bp   = (Uint32) ESTACK_POP(stack);
		n    = (Uint32) ESTACK_POP(stack);
		nhp  = hp;

		if (res != THE_NON_VALUE) {
		    *hp++ = *ptr++;
		    *hp++ = (*ptr++) - 1;
		    n    -= slot;
		    while(slot--) { *hp++ = *ptr++; }
		    *hp++ = res;
		    ptr++; n--;
		    while(n--) { *hp++ = *ptr++; }
		} else {
		    hval  = MAP_HEADER_VAL(hdr);
		    *hp++ = MAP_HEADER_HAMT_HEAD_BITMAP(hval ^ bp); ptr++;
		    *hp++ = (*ptr++) - 1;
		    n    -= slot;
		    while(slot--) { *hp++ = *ptr++; }
		    ptr++; n--;
		    while(n--) { *hp++ = *ptr++; }
		}
		res = make_hashmap(nhp);
		break;
	    default:
		erts_exit(ERTS_ERROR_EXIT, "bad header tag %x\r\n", hdr & _HEADER_MAP_SUBTAG_MASK);
		break;
	}
    } while(!ESTACK_ISEMPTY(stack));
    HRelease(p, hp_end, hp);
not_found:
    DESTROY_ESTACK(stack);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);
    ERTS_HOLE_CHECK(p);
    UnUseTmpHeapNoproc(2);
    return res;
}


int erts_validate_and_sort_flatmap(flatmap_t* mp)
{
    Eterm *ks  = flatmap_get_keys(mp);
    Eterm *vs  = flatmap_get_values(mp);
    Uint   sz  = flatmap_get_size(mp);
    Uint   ix,jx;
    Eterm  tmp;
    Sint c;

    /* sort */

    for (ix = 1; ix < sz; ix++) {
	jx = ix;
	while( jx > 0 && (c = CMP_TERM(ks[jx],ks[jx-1])) <= 0 ) {
	    /* identical key -> error */
	    if (c == 0) return 0;

	    tmp = ks[jx];
	    ks[jx] = ks[jx - 1];
	    ks[jx - 1] = tmp;

	    tmp = vs[jx];
	    vs[jx] = vs[jx - 1];
	    vs[jx - 1] = tmp;

	    jx--;
	}
    }
    return 1;
}

#if 0 /* Can't get myself to remove this beautiful piece of code
         for probabilistic overestimation of nr of nodes in a hashmap */

/* Really rough estimate of sqrt(x)
 * Guaranteed not to be less than sqrt(x)
 */
static int int_sqrt_ceiling(Uint x)
{
    int n;

    if (x <= 2)
	return x;

    n = erts_fit_in_bits_uint(x-1);
    if (n & 1) {
	/* Calc: sqrt(2^n) = 2^(n/2) * sqrt(2) ~= 2^(n/2) * 3 / 2 */
	return (1 << (n/2 - 1)) * 3;
    }
    else {
	/* Calc: sqrt(2^n) = 2^(n/2) */
	return 1 << (n / 2);
    }
}

/* May not be enough if hashing is broken (not uniform)
 * or if hell freezes over.
 */
Uint hashmap_overestimated_node_count(Uint k)
{
    /* k is nr of key-value pairs.
       N(k) is expected nr of nodes in hamt.

       Observation:
       For uniformly distributed hash values, average of N varies between
       0.3*k and 0.4*k (with a beautiful sine curve)
       and standard deviation of N is about sqrt(k)/3.

       Assuming normal probability distribution, we overestimate nr of nodes
       by 15 std.devs above the average, which gives a probability for overrun
       less than 1.0e-49 (same magnitude as a git SHA1 collision).
     */
    return 2*k/5 + 1 + (15/3)*int_sqrt_ceiling(k);
}
#endif

BIF_RETTYPE erts_debug_map_info_1(BIF_ALIST_1) {
    if (is_hashmap(BIF_ARG_1)) {
	BIF_RET(hashmap_info(BIF_P,BIF_ARG_1));
    } else if (is_flatmap(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    } else {
	BIF_P->fvalue = BIF_ARG_1;
	BIF_ERROR(BIF_P, BADMAP);
    }
}

/*
 * erts_internal:map_to_tuple_keys/1
 *
 * Used in erts_debug:size/1
 */

BIF_RETTYPE erts_internal_map_to_tuple_keys_1(BIF_ALIST_1) {
    if (is_flatmap(BIF_ARG_1)) {
	flatmap_t *mp = (flatmap_t*)flatmap_val(BIF_ARG_1);
	BIF_RET(mp->keys);
    } else if (is_hashmap(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    } else {
	BIF_P->fvalue = BIF_ARG_1;
	BIF_ERROR(BIF_P, BADMAP);
    }
}

/*
 * erts_internal:map_type/1
 *
 * Used in erts_debug:size/1
 */

BIF_RETTYPE erts_internal_map_type_1(BIF_ALIST_1) {
    DECL_AM(hashmap);
    DECL_AM(hashmap_node);
    DECL_AM(flatmap);
    if (is_map(BIF_ARG_1)) {
        Eterm hdr = *(boxed_val(BIF_ARG_1));
        ASSERT(is_header(hdr));
        switch (hdr & _HEADER_MAP_SUBTAG_MASK) {
            case HAMT_SUBTAG_HEAD_FLATMAP:
                BIF_RET(AM_flatmap);
            case HAMT_SUBTAG_HEAD_ARRAY:
            case HAMT_SUBTAG_HEAD_BITMAP:
                BIF_RET(AM_hashmap);
            case HAMT_SUBTAG_NODE_BITMAP:
                BIF_RET(AM_hashmap_node);
            default:
                erts_exit(ERTS_ERROR_EXIT, "bad header");
        }
    }
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, BADMAP);
}

/*
 * erts_internal:map_hashmap_children/1
 *
 * Used in erts_debug:size/1
 */

BIF_RETTYPE erts_internal_map_hashmap_children_1(BIF_ALIST_1) {
    if (is_map(BIF_ARG_1)) {
        Eterm node = BIF_ARG_1;
        Eterm *ptr, hdr, *hp, res = NIL;
        Uint  sz = 0;
        ptr = boxed_val(node);
        hdr = *ptr;
        ASSERT(is_header(hdr));

        switch(hdr & _HEADER_MAP_SUBTAG_MASK) {
            case HAMT_SUBTAG_HEAD_FLATMAP:
                BIF_ERROR(BIF_P, BADARG);
            case HAMT_SUBTAG_HEAD_BITMAP:
                ptr++;
            case HAMT_SUBTAG_NODE_BITMAP:
                ptr++;
                sz = hashmap_bitcount(MAP_HEADER_VAL(hdr));
                break;
            case HAMT_SUBTAG_HEAD_ARRAY:
                sz   = 16;
                ptr += 2;
                break;
            default:
                erts_exit(ERTS_ERROR_EXIT, "bad header\r\n");
                break;
        }
        ASSERT(sz < 17);
        hp = HAlloc(BIF_P, 2*sz);
        while(sz--) { res = CONS(hp, *ptr++, res); hp += 2; }
        BIF_RET(res);
    }
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, BADMAP);
}


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
	if (lvl < clvl)
            lvl = clvl;
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
			erts_exit(ERTS_ERROR_EXIT, "bad header\r\n");
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


/* implementation of builtin emulations */

#if !ERTS_AT_LEAST_GCC_VSN__(3, 4, 0)
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
