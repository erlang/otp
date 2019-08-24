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
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_encode_fun(char *buf, int *index, const erlang_fun *p)
{
    int ix = *index;

    switch (p->type) {
    case EI_FUN_CLOSURE:
	if (p->arity == -1) {
	    /* ERL_FUN_EXT */
	    if (buf != NULL) {
		char* s = buf + ix;
		put8(s, ERL_FUN_EXT);
		put32be(s, p->u.closure.n_free_vars);
	    }
	    ix += sizeof(char) + 4;
	    if (ei_encode_pid(buf, &ix, &p->u.closure.pid) < 0)
		return -1;
	    if (ei_encode_atom_as(buf, &ix, p->module, ERLANG_UTF8, ERLANG_UTF8) < 0)
		return -1;
	    if (ei_encode_long(buf, &ix, p->u.closure.index) < 0)
		return -1;
	    if (ei_encode_long(buf, &ix, p->u.closure.uniq) < 0)
		return -1;
	    if (buf != NULL)
		memcpy(buf + ix, p->u.closure.free_vars, p->u.closure.free_var_len);
	    ix += p->u.closure.free_var_len;
	} else {
	    char *size_p;
	    if (buf != NULL) {
		char* s = buf + ix;
		put8(s, ERL_NEW_FUN_EXT);
		size_p = s;
		s += 4;
		put8(s, p->arity);
		memcpy(s, p->u.closure.md5, sizeof(p->u.closure.md5));
		s += sizeof(p->u.closure.md5);
		put32be(s, p->u.closure.index);
		put32be(s, p->u.closure.n_free_vars);
	    } else
		size_p = NULL;
	    ix += 1 + 4 + 1 + sizeof(p->u.closure.md5) + 4 + 4;
	    if (ei_encode_atom_as(buf, &ix, p->module, ERLANG_UTF8, ERLANG_UTF8) < 0)
		return -1;
	    if (ei_encode_long(buf, &ix, p->u.closure.old_index) < 0)
		return -1;
	    if (ei_encode_long(buf, &ix, p->u.closure.uniq) < 0)
		return -1;
	    if (ei_encode_pid(buf, &ix, &p->u.closure.pid) < 0)
		return -1;
	    if (buf != NULL)
		memcpy(buf + ix, p->u.closure.free_vars, p->u.closure.free_var_len);
	    ix += p->u.closure.free_var_len;
	    if (size_p != NULL) {
		int sz = buf + ix - size_p;
		put32be(size_p, sz);
	    }
	}
	break;
    case EI_FUN_EXPORT:
        if (buf != NULL) {
            char* s = buf + ix;
            put8(s, ERL_EXPORT_EXT);
        }
        ix++;
        if (ei_encode_atom_as(buf, &ix, p->module, ERLANG_UTF8, ERLANG_UTF8) < 0)
            return -1;
        if (ei_encode_atom_as(buf, &ix, p->u.exprt.func, ERLANG_UTF8, ERLANG_UTF8) < 0)
            return -1;
        if (ei_encode_long(buf, &ix, p->arity) < 0)
            return -1;
        break;
    }
    *index = ix;
    return 0;
}

