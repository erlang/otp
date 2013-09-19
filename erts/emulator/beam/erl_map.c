/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013. All Rights Reserved.
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

BIF_RETTYPE map_to_list_1(BIF_ALIST_1) {
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

BIF_RETTYPE map_new_0(BIF_ALIST_0) {
    Eterm* hp;
    Eterm tup;
    map_t *mp;

    hp    = HAlloc(BIF_P, (3 + 1));
    tup   = make_tuple(hp);
    *hp++ = make_arityval(0);

    mp    = (map_t*)hp;
    mp->thing_word = MAP_HEADER;
    mp->size = 0;
    mp->keys = tup;

    BIF_RET(make_map(mp));
}

BIF_RETTYPE map_get_2(BIF_ALIST_2) {
    if (is_map(BIF_ARG_2)) {
	Eterm *hp, *ks,*vs, key, error;
	map_t *mp;
	Uint n,i;
	char *s_error;

	mp  = (map_t*)map_val(BIF_ARG_2);
	key = BIF_ARG_1;
	n   = map_get_size(mp);
	
	if (n == 0)
	    goto error;

	ks  = map_get_keys(mp);
	vs  = map_get_values(mp);

	if (is_immed(key)) {
	    for( i = 0; i < n; i++) {
		if (ks[i] == key) {
		    BIF_RET(vs[i]);
		}
	    }
	}

	for( i = 0; i < n; i++) {
	    if (eq(ks[i], key)) {
		BIF_RET(vs[i]);
	    }
	}
error:

	s_error = "bad_key";
	error = am_atom_put(s_error, sys_strlen(s_error));

	hp = HAlloc(BIF_P, 3);
	BIF_P->fvalue = TUPLE2(hp, error, key);
	BIF_ERROR(BIF_P, EXC_ERROR_2);
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE map_put_3(BIF_ALIST_3) {
    if (is_map(BIF_ARG_3)) {
	Sint n,i;
	Sint c = 0;
	Eterm* hp, *shp;
	Eterm *ks,*vs, res, key, tup;
	map_t *mp = (map_t*)map_val(BIF_ARG_3);

	key = BIF_ARG_1;
	n   = map_get_size(mp);

	if (n == 0) {
	    hp    = HAlloc(BIF_P, 4 + 2);
	    tup   = make_tuple(hp);
	    *hp++ = make_arityval(1);
	    *hp++ = key;
	    res   = make_map(hp);
	    *hp++ = MAP_HEADER;
	    *hp++ = 1;
	    *hp++ = tup;
	    *hp++ = BIF_ARG_2;

	    BIF_RET(res);
	}

	ks  = map_get_keys(mp);
	vs  = map_get_values(mp);
	/* only allocate for values,
	 * assume key-tuple will be intact
	 */

	hp  = HAlloc(BIF_P, 3 + n);
	shp = hp; /* save hp, used if optimistic update fails */
	res = make_map(hp);
	*hp++ = MAP_HEADER;
	*hp++ = n;
	*hp++ = mp->keys;

	if (is_immed(key)) {
	    for( i = 0; i < n; i ++) {
		if (ks[i] == key) {
		    *hp++ = BIF_ARG_2;
		    vs++;
		    c = 1;
		} else {
		    *hp++ = *vs++;
		}
	    }
	} else {
	    for( i = 0; i < n; i ++) {
		if (eq(ks[i], key)) {
		    *hp++ = BIF_ARG_2;
		    vs++;
		    c = 1;
		} else {
		    *hp++ = *vs++;
		}
	    }
	}

	if (c)
	    BIF_RET(res);

	/* need to make a new tuple,
	 * use old hp since it needs to be recreated anyway.
	 */
	tup    = make_tuple(shp);
	*shp++ = make_arityval(n+1);

	hp    = HAlloc(BIF_P, 3 + n + 1);
	res   = make_map(hp);
	*hp++ = MAP_HEADER;
	*hp++ = n + 1;
	*hp++ = tup;

	ks  = map_get_keys(mp);
	vs  = map_get_values(mp);

	ASSERT(n >= 0);

	/* copy map in order */
	while (n && ((c = CMP(*ks, key)) < 0)) {
	    *shp++ = *ks++;
	    *hp++  = *vs++;
	    n--;
	}

	*shp++ = key;
	*hp++  = BIF_ARG_2;

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
	BIF_RET(res);
    }

    BIF_ERROR(BIF_P, BADARG);
}
