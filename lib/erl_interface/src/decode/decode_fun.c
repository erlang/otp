/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
    erlang_pid* p_pid;
    char* p_module;
    long* p_index;
    long* p_uniq;
    long* p_old_index;

    if (p != NULL) {
	p_pid = &p->u.closure.pid;
	p_module = &p->module[0];
	p_index = &p->u.closure.index;
	p_uniq = &p->u.closure.uniq;
	p_old_index = &p->u.closure.old_index;
    }
    else {
	p_index = NULL; p_uniq = NULL; p_old_index = NULL;
	p_pid = NULL; p_module = NULL;
    }

    switch (get8(s)) {
    case ERL_FUN_EXT:
	/* mark as old (R7 and older) external fun */
	if (p != NULL) {
            p->type = EI_FUN_CLOSURE;
            p->arity = -1;
        }
	/* first number of free vars (environment) */
	n = get32be(s);
	/* then the pid */
	ix = 0;
	if (ei_decode_pid(s, &ix, p_pid) < 0)
	    return -1;
	/* then the module (atom) */
	if (ei_decode_atom_as(s, &ix, p_module, MAXATOMLEN_UTF8, ERLANG_UTF8,
                              NULL, NULL) < 0)
	    return -1;
	/* then the index */
	if (ei_decode_long(s, &ix, p_index) < 0)
	    return -1;
	/* then the uniq */
	if (ei_decode_long(s, &ix, p_uniq) < 0)
	    return -1;
	/* finally the free vars */
	ix0 = ix;
	for (i = 0; i < n; ++i) {
	    if (ei_skip_term(s, &ix) < 0)
		return -1;
	}
	if (p != NULL) {
	    p->u.closure.n_free_vars = n;
            p->u.closure.free_var_len = ix - ix0;
            if (p->u.closure.free_var_len > 0) {
                p->u.closure.free_vars = ei_malloc(p->u.closure.free_var_len);
                if (!(p->u.closure.free_vars)) return -1;
                memcpy(p->u.closure.free_vars, s + ix0, p->u.closure.free_var_len);
            }
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
	if (p != NULL) {
            p->type = EI_FUN_CLOSURE;
            p->arity = i;
            /* then md5 */
            memcpy(p->u.closure.md5, s, 16);
        }
	s += 16;
	/* then index */
	i = get32be(s);
	if (p != NULL) p->u.closure.index = i;
	/* then the number of free vars (environment) */
	i = get32be(s);
	if (p != NULL) p->u.closure.n_free_vars = i;
	/* then the module (atom) */
	ix = 0;
	if (ei_decode_atom_as(s, &ix, p_module, MAXATOMLEN_UTF8, ERLANG_UTF8,
                              NULL, NULL) < 0)
	    return -1;
	/* then the old_index */
	if (ei_decode_long(s, &ix, p_old_index) < 0)
	    return -1;
	/* then the old_uniq */
	if (ei_decode_long(s, &ix, p_uniq) < 0)
	    return -1;
	/* the the pid */
	if (ei_decode_pid(s, &ix, p_pid) < 0)
	    return -1;
	/* finally the free vars */
	s += ix;
	n = n - (s - s0) + 1;
	if (n < 0) return -1;
	if (p != NULL) {
	    p->u.closure.free_var_len = n;
	    if (n > 0) {
		p->u.closure.free_vars = malloc(n);
		if (!(p->u.closure.free_vars)) return -1;
		memcpy(p->u.closure.free_vars, s, n);
	    }
	}
	s += n;
	*index += s-s0;
        return 0;
	break;
    case ERL_EXPORT_EXT: {
        char* p_func;
        long* p_arity;
        int used;

        if (p) {
            p->type = EI_FUN_EXPORT;
            p_arity = &p->arity;
        }
        else {
            p_arity = NULL;
        }
	ix = 0;
        if (ei_decode_atom_as(s, &ix, p_module, MAXATOMLEN_UTF8, ERLANG_UTF8,
                              NULL, NULL) < 0)
            return -1;
        if (p) {
            /* try use module buffer for function name */
            used = strlen(p->module) + 1;
            p_func = p->module + used;
            p->u.exprt.func = p_func;
            p->u.exprt.func_allocated = 0;
        }
        else {
            used = 0;
            p_func = NULL;
        }
        while (ei_decode_atom_as(s, &ix, p_func, MAXATOMLEN_UTF8-used,
                                 ERLANG_UTF8, NULL, NULL) < 0) {
            if (!used)
                return -1;
            p_func = malloc(MAXATOMLEN_UTF8);
            p->u.exprt.func = p_func;
            p->u.exprt.func_allocated = 1;
            used = 0;
        }
        if (ei_decode_long(s, &ix, p_arity) < 0)
            return -1;
	s += ix;
	*index += s - s0;
        return 0;
    }
    default:
	return -1;
    }
}

void free_fun(erlang_fun* f)
{
    switch (f->type) {
    case EI_FUN_CLOSURE:
        if (f->u.closure.free_var_len > 0)
            ei_free(f->u.closure.free_vars);
        break;
    case EI_FUN_EXPORT:
        if (f->u.exprt.func_allocated)
            ei_free(f->u.exprt.func);
        break;
    }
}
