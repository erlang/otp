/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2009. All Rights Reserved.
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
#include <string.h>
#include <stdlib.h>

#include "eidef.h"
#include "eiext.h"
#include "ei_malloc.h"
#include "decode_skip.h"
#include "putget.h"

int ei_decode_fun(const char *buf, int *index, erlang_fun *p)
{
    const char *s = buf + *index;
    const char *s0 = s;
    int i, ix, ix0, n;

    switch (get8(s)) {
    case ERL_FUN_EXT:
	/* mark as old (R7 and older) external fun */
	if (p != NULL) p->arity = -1;
	/* first number of free vars (environment) */
	n = get32be(s);
	/* then the pid */
	ix = 0;
	if (ei_decode_pid(s, &ix, (p == NULL ? (erlang_pid*)NULL : &p->pid)) < 0)
	    return -1;
	/* then the module (atom) */
	if (ei_decode_atom(s, &ix, (p == NULL ? (char*)NULL : p->module)) < 0)
	    return -1;
	/* then the index */
	if (ei_decode_long(s, &ix, (p == NULL ? (long*)NULL : &p->index)) < 0)
	    return -1;
	/* then the uniq */
	if (ei_decode_long(s, &ix, (p == NULL ? (long*)NULL : &p->uniq)) < 0)
	    return -1;
	/* finally the free vars */
	ix0 = ix;
	for (i = 0; i < n; ++i) {
	    if (ei_skip_term(s, &ix) < 0)
		return -1;
	}
	if (p != NULL) {
	    p->n_free_vars = n;
	    p->free_var_len = ix - ix0;
	    p->free_vars = ei_malloc(ix - ix0);
	    if (!(p->free_vars)) return -1;
	    memcpy(p->free_vars, s + ix0, ix - ix0);
	}
	s += ix;
	*index += s-s0;
        return 0;
	break;
    case ERL_NEW_FUN_EXT:
	/* first total size */
	n = get32be(s);
	/* then the arity */
	i = get8(s);
	if (p != NULL) p->arity = i;
	/* then md5 */
	if (p != NULL) memcpy(p->md5, s, 16);
	s += 16;
	/* then index */
	i = get32be(s);
	if (p != NULL) p->index = i;
	/* then the number of free vars (environment) */
	i = get32be(s);
	if (p != NULL) p->n_free_vars = i;
	/* then the module (atom) */
	ix = 0;
	if (ei_decode_atom(s, &ix, (p == NULL ? (char*)NULL : p->module)) < 0)
	    return -1;
	/* then the old_index */
	if (ei_decode_long(s, &ix, (p == NULL ? (long*)NULL : &p->old_index)) < 0)
	    return -1;
	/* then the old_uniq */
	if (ei_decode_long(s, &ix, (p == NULL ? (long*)NULL : &p->uniq)) < 0)
	    return -1;
	/* the the pid */
	if (ei_decode_pid(s, &ix, (p == NULL ? (erlang_pid*)NULL : &p->pid)) < 0)
	    return -1;
	/* finally the free vars */
	s += ix;
	n = n - (s - s0) + 1;
	if (n < 0) return -1;
	if (p != NULL) {
	    p->free_var_len = n;
	    if (n > 0) {
		p->free_vars = malloc(n);
		if (!(p->free_vars)) return -1;
		memcpy(p->free_vars, s, n);
	    }
	}
	s += n;
	*index += s-s0;
        return 0;
	break;
    default:
	return -1;
    }
}

void free_fun(erlang_fun* f)
{
  if (f->free_var_len > 0)
      ei_free(f->free_vars);
}
