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
#include "bif.h"

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

/* erlang:map_size/1
 * the corresponding instruction is implemented in:
 *     beam/erl_bif_guard.c
 */

BIF_RETTYPE map_size_1(BIF_ALIST_1) {
    if (is_map(BIF_ARG_1)) {
	Eterm *hp;
	Uint hsz  = 0;
	map_t *mp = (map_t*)map_val(BIF_ARG_1);
	Uint n    = map_get_size(mp);

	erts_bld_uint(NULL, &hsz, n);
	hp = HAlloc(BIF_P, hsz);
	BIF_RET(erts_bld_uint(&hp, NULL, n));
    }

    BIF_ERROR(BIF_P, BADARG);
}

/* maps:to_list/1
 */

BIF_RETTYPE maps_to_list_1(BIF_ALIST_1) {
    if (is_map(BIF_ARG_1)) {
	Uint n;
	Eterm* hp;
	Eterm *ks,*vs, res, tup;
	map_t *mp = (map_t*)map_val(BIF_ARG_1);

	ks  = map_get_keys(mp);
	vs  = map_get_values(mp);
	n   = map_get_size(mp);
	hp  = HAlloc(BIF_P, (2 + 3) * n);
	res = NIL;

	while(n--) {
	    tup = TUPLE2(hp, ks[n], vs[n]); hp += 3;
	    res = CONS(hp, tup, res); hp += 2;
	}

	BIF_RET(res);
    }

    BIF_ERROR(BIF_P, BADARG);
}

/* maps:find/2
 * return value if key *matches* a key in the map
 */

int erts_maps_find(Eterm key, Eterm map, Eterm *value) {

    Eterm *ks,*vs;
    map_t *mp;
    Uint n,i;

    mp  = (map_t*)map_val(map);
    n   = map_get_size(mp);
    ks  = map_get_keys(mp);
    vs  = map_get_values(mp);

    for( i = 0; i < n; i++) {
	if (EQ(ks[i], key)) {
	    *value = vs[i];
	    return 1;
	}
    }
    return 0;
}

BIF_RETTYPE maps_find_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
	Eterm *hp, value,res;

	if (erts_maps_find(BIF_ARG_1, BIF_ARG_2, &value)) {
	    hp    = HAlloc(BIF_P, 3);
	    res   = make_tuple(hp);
	    *hp++ = make_arityval(2);
	    *hp++ = am_ok;
	    *hp++ = value;
	    BIF_RET(res);
	}

	BIF_RET(am_error);
    }
    BIF_ERROR(BIF_P, BADARG);
}
/* maps:get/2
 * return value if key *matches* a key in the map
 * exception bad_key if none matches
 */


int erts_maps_get(Eterm key, Eterm map, Eterm *value) {
    Eterm *ks,*vs;
    map_t *mp;
    Uint n,i;

    mp  = (map_t*)map_val(map);
    n   = map_get_size(mp);

    if (n == 0)
	return 0;

    ks  = map_get_keys(mp);
    vs  = map_get_values(mp);

    if (is_immed(key)) {
	for( i = 0; i < n; i++) {
	    if (ks[i] == key) {
		*value = vs[i];
		return 1;
	    }
	}
    }

    for( i = 0; i < n; i++) {
	if (EQ(ks[i], key)) {
	    *value = vs[i];
	    return 1;
	}
    }
    return 0;
}

BIF_RETTYPE maps_get_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
	Eterm *hp;
	Eterm value, error;
	char *s_error;

	if (erts_maps_get(BIF_ARG_1, BIF_ARG_2, &value)) {
	    BIF_RET(value);
	}

	s_error = "bad_key";
	error = am_atom_put(s_error, sys_strlen(s_error));

	hp = HAlloc(BIF_P, 3);
	BIF_P->fvalue = TUPLE2(hp, error, BIF_ARG_1);
	BIF_ERROR(BIF_P, EXC_ERROR_2);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* maps:from_list/1
 * List may be unsorted [{K,V}]
 */

BIF_RETTYPE maps_from_list_1(BIF_ALIST_1) {
    Eterm *kv, item = BIF_ARG_1;
    Eterm *hp, *thp,*vs, *ks, keys, res;
    map_t *mp;
    Uint  size = 0, unused_size = 0;
    Sint  c = 0;
    Sint  idx = 0;

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

	hp    = HAlloc(BIF_P, 3 + 1 + (2 * size));
	thp   = hp;
	keys  = make_tuple(hp);
	*hp++ = make_arityval(size);
	ks    = hp;
	hp   += size;
	mp    = (map_t*)hp;
	res   = make_map(mp);
	hp   += MAP_HEADER_SIZE;
	vs    = hp;

	mp->thing_word = MAP_HEADER;
	mp->size = size; /* set later, might shrink*/
	mp->keys = keys;

	if (size == 0)
	    BIF_RET(res);

	item  = BIF_ARG_1;

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
	    HRelease(BIF_P, vs + size + unused_size, vs + size);
	}

	*thp = make_arityval(size);
	mp->size = size;
	BIF_RET(res);
    }

error:

    BIF_ERROR(BIF_P, BADARG);
}

/* maps:is_key/2
 */

BIF_RETTYPE maps_is_key_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
	Eterm *ks, key;
	map_t *mp;
	Uint n,i;

	mp  = (map_t*)map_val(BIF_ARG_2);
	key = BIF_ARG_1;
	n   = map_get_size(mp);
	ks  = map_get_keys(mp);

	if (n == 0)
	    BIF_RET(am_false);

	if (is_immed(key)) {
	    for( i = 0; i < n; i++) {
		if (ks[i] == key) {
		    BIF_RET(am_true);
		}
	    }
	}

	for( i = 0; i < n; i++) {
	    if (EQ(ks[i], key)) {
		BIF_RET(am_true);
	    }
	}
	BIF_RET(am_false);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* maps:keys/1
 */

BIF_RETTYPE maps_keys_1(BIF_ALIST_1) {
    if (is_map(BIF_ARG_1)) {
	Eterm *hp, *ks, res = NIL;
	map_t *mp;
	Uint n;

	mp  = (map_t*)map_val(BIF_ARG_1);
	n   = map_get_size(mp);

	if (n == 0)
	    BIF_RET(res);

	hp  = HAlloc(BIF_P, (2 * n));
	ks  = map_get_keys(mp);

	while(n--) {
	    res = CONS(hp, ks[n], res); hp += 2;
	}

	BIF_RET(res);
    }
    BIF_ERROR(BIF_P, BADARG);
}
/* maps:merge/2
 */

BIF_RETTYPE maps_merge_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_1) && is_map(BIF_ARG_2)) {
	Eterm *hp,*thp;
	Eterm tup;
	Eterm *ks,*vs,*ks1,*vs1,*ks2,*vs2;
	map_t *mp1,*mp2,*mp_new;
	Uint n1,n2,i1,i2,need,unused_size=0;
	int c = 0;

	mp1  = (map_t*)map_val(BIF_ARG_1);
	mp2  = (map_t*)map_val(BIF_ARG_2);
	n1   = map_get_size(mp1);
	n2   = map_get_size(mp2);

	need = MAP_HEADER_SIZE + 1 + 2*(n1 + n2);

	hp     = HAlloc(BIF_P, need);
	thp    = hp;
	tup    = make_tuple(thp);
	ks     = hp + 1; hp += 1 + n1 + n2;
	mp_new = (map_t*)hp; hp += MAP_HEADER_SIZE;
	vs     = hp; hp += n1 + n2;

	mp_new->thing_word = MAP_HEADER;
	mp_new->size = 0;
	mp_new->keys = tup;

	i1  = 0; i2 = 0;
	ks1 = map_get_keys(mp1);
	vs1 = map_get_values(mp1);
	ks2 = map_get_keys(mp2);
	vs2 = map_get_values(mp2);

	while(i1 < n1 && i2 < n2) {
	    c = CMP_TERM(ks1[i1],ks2[i2]);
	    if ( c == 0) {
		/* use righthand side arguments map value,
		 * but advance both maps */
		*ks++ = ks2[i2];
		*vs++ = vs2[i2];
		i1++, i2++, unused_size++;
	    } else if ( c < 0) {
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
	    HRelease(BIF_P, vs + unused_size, vs);
	}

	mp_new->size = n1 + n2 - unused_size;
	*thp = make_arityval(n1 + n2 - unused_size);

	BIF_RET(make_map(mp_new));
    }
    BIF_ERROR(BIF_P, BADARG);
}
/* maps:new/2
 */

BIF_RETTYPE maps_new_0(BIF_ALIST_0) {
    Eterm* hp;
    Eterm tup;
    map_t *mp;

    hp    = HAlloc(BIF_P, (MAP_HEADER_SIZE + 1));
    tup   = make_tuple(hp);
    *hp++ = make_arityval(0);

    mp    = (map_t*)hp;
    mp->thing_word = MAP_HEADER;
    mp->size = 0;
    mp->keys = tup;

    BIF_RET(make_map(mp));
}

/* maps:put/3
 */

Eterm erts_maps_put(Process *p, Eterm key, Eterm value, Eterm map) {
    Sint n,i;
    Sint c = 0;
    Eterm* hp, *shp;
    Eterm *ks,*vs, res, tup;
    map_t *mp = (map_t*)map_val(map);

    n = map_get_size(mp);

    if (n == 0) {
	hp    = HAlloc(p, MAP_HEADER_SIZE + 1 + 2);
	tup   = make_tuple(hp);
	*hp++ = make_arityval(1);
	*hp++ = key;
	res   = make_map(hp);
	*hp++ = MAP_HEADER;
	*hp++ = 1;
	*hp++ = tup;
	*hp++ = value;

	return res;
    }

    ks  = map_get_keys(mp);
    vs  = map_get_values(mp);

    /* only allocate for values,
     * assume key-tuple will be intact
     */

    hp  = HAlloc(p, MAP_HEADER_SIZE + n);
    shp = hp; /* save hp, used if optimistic update fails */
    res = make_map(hp);
    *hp++ = MAP_HEADER;
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

    /* need to make a new tuple,
     * use old hp since it needs to be recreated anyway.
     */
    tup    = make_tuple(shp);
    *shp++ = make_arityval(n+1);

    hp    = HAlloc(p, 3 + n + 1);
    res   = make_map(hp);
    *hp++ = MAP_HEADER;
    *hp++ = n + 1;
    *hp++ = tup;

    ks  = map_get_keys(mp);
    vs  = map_get_values(mp);

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

BIF_RETTYPE maps_put_3(BIF_ALIST_3) {
    if (is_map(BIF_ARG_3)) {
	BIF_RET(erts_maps_put(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3));
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* maps:remove/3
 */

int erts_maps_remove(Process *p, Eterm key, Eterm map, Eterm *res) {
    Sint n;
    Uint need;
    Eterm *hp_start;
    Eterm *thp, *mhp;
    Eterm *ks, *vs, tup;
    map_t *mp = (map_t*)map_val(map);

    n = map_get_size(mp);

    if (n == 0) {
	*res = map;
	return 1;
    }

    ks = map_get_keys(mp);
    vs = map_get_values(mp);

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

    *res   = make_map(mhp);
    *mhp++ = MAP_HEADER;
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

BIF_RETTYPE maps_remove_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
	Eterm res;
	if (erts_maps_remove(BIF_P, BIF_ARG_1, BIF_ARG_2, &res)) {
	    BIF_RET(res);
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* maps:update/3
 */

int erts_maps_update(Process *p, Eterm key, Eterm value, Eterm map, Eterm *res) {
	Sint n,i;
	Eterm* hp,*shp;
	Eterm *ks,*vs;
	map_t *mp = (map_t*)map_val(map);

	if ((n = map_get_size(mp)) == 0) {
	    return 0;
	}

	ks  = map_get_keys(mp);
	vs  = map_get_values(mp);

	/* only allocate for values,
	 * assume key-tuple will be intact
	 */

	hp  = HAlloc(p, MAP_HEADER_SIZE + n);
	shp = hp;
	*hp++ = MAP_HEADER;
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

	HRelease(p, shp + MAP_HEADER_SIZE + n, shp);
	return 0;

found_key:
	*hp++ = value;
	vs++;
	if (++i < n)
	    sys_memcpy(hp, vs, (n - i)*sizeof(Eterm));
	*res = make_map(shp);
	return 1;
}

BIF_RETTYPE maps_update_3(BIF_ALIST_3) {
    if (is_map(BIF_ARG_3)) {
	Eterm res;
	if (erts_maps_update(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &res)) {
	    BIF_RET(res);
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}


/* maps:values/1
 */

BIF_RETTYPE maps_values_1(BIF_ALIST_1) {
    if (is_map(BIF_ARG_1)) {
	Eterm *hp, *vs, res = NIL;
	map_t *mp;
	Uint n;

	mp  = (map_t*)map_val(BIF_ARG_1);
	n   = map_get_size(mp);

	if (n == 0)
	    BIF_RET(res);

	hp  = HAlloc(BIF_P, (2 * n));
	vs  = map_get_values(mp);

	while(n--) {
	    res = CONS(hp, vs[n], res); hp += 2;
	}

	BIF_RET(res);
    }
    BIF_ERROR(BIF_P, BADARG);
}

int erts_validate_and_sort_map(map_t* mp)
{
    Eterm *ks  = map_get_keys(mp);
    Eterm *vs  = map_get_values(mp);
    Uint   sz  = map_get_size(mp);
    Uint   ix,jx;
    Eterm  tmp;
    int c;

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

/*
 * erts_internal:map_to_tuple_keys/1
 *
 * Used in erts_debug:size/1
 */

BIF_RETTYPE erts_internal_map_to_tuple_keys_1(BIF_ALIST_1) {
    if (is_map(BIF_ARG_1)) {
	map_t *mp = (map_t*)map_val(BIF_ARG_1);
	BIF_RET(mp->keys);
    }
    BIF_ERROR(BIF_P, BADARG);
}
