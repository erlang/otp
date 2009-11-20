/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2009. All Rights Reserved.
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

/*
 * BIFs belonging to the 'os' module.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"

/*
 * Return the pid for the Erlang process in the host OS.
 */

 /* return a timestamp */
BIF_RETTYPE os_timestamp_0(BIF_ALIST_0)
{
    Uint megasec, sec, microsec;
    Eterm* hp;

    get_sys_now(&megasec, &sec, &microsec);
    hp = HAlloc(BIF_P, 4);
    BIF_RET(TUPLE3(hp, make_small(megasec), make_small(sec),
		   make_small(microsec)));
}


Eterm
os_getpid_0(Process* p)
{
     char pid_string[21]; /* enough for a 64 bit number */
     int n;
     Eterm* hp;
     sys_get_pid(pid_string); /* In sys.c */
     n = sys_strlen(pid_string);
     hp = HAlloc(p, n*2);
     BIF_RET(buf_to_intlist(&hp, pid_string, n, NIL));
}

Eterm
os_getenv_0(Process* p)
{
    GETENV_STATE state;
    char *cp;
    Eterm* hp;
    Eterm ret;
    Eterm str;
    int len;

    init_getenv_state(&state);

    ret = NIL;
    while ((cp = getenv_string(&state)) != NULL) {
	len = strlen(cp);
	hp = HAlloc(p, len*2+2);
	str = buf_to_intlist(&hp, cp, len, NIL);
	ret = CONS(hp, str, ret);
    }

    fini_getenv_state(&state);

    return ret;
}

Eterm
os_getenv_1(Process* p, Eterm key)
{
    Eterm str;
    int len, res;
    char *key_str, *val;
    char buf[1024];
    size_t val_size = sizeof(buf);

    len = is_string(key);
    if (!len) {
	BIF_ERROR(p, BADARG);
    }
    /* Leave at least one byte in buf for value */
    key_str = len < sizeof(buf)-2 ? &buf[0] : erts_alloc(ERTS_ALC_T_TMP, len+1);
    if (intlist_to_buf(key, key_str, len) != len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
    key_str[len] = '\0';

    if (key_str != &buf[0])
	val = &buf[0];
    else {
	val_size -= len + 1;
	val = &buf[len + 1];
    }
    res = erts_sys_getenv(key_str, val, &val_size);

    if (res < 0) {
    no_var:
	str = am_false;
    } else {
	Eterm* hp;
	if (res > 0) {
	    val = erts_alloc(ERTS_ALC_T_TMP, val_size);
	    while (1) {
		res = erts_sys_getenv(key_str, val, &val_size);
		if (res == 0)
		    break;
		else if (res < 0)
		    goto no_var;
		else
		    val = erts_realloc(ERTS_ALC_T_TMP, val, val_size);
	    }
	}
	if (val_size)
	    hp = HAlloc(p, val_size*2);
	str = buf_to_intlist(&hp, val, val_size, NIL);
    }
    if (key_str != &buf[0])
	erts_free(ERTS_ALC_T_TMP, key_str);
    if (val < &buf[0] || &buf[sizeof(buf)-1] < val)
	erts_free(ERTS_ALC_T_TMP, val);
    BIF_RET(str);
}

Eterm
os_putenv_2(Process* p, Eterm key, Eterm value)
{
    char def_buf[1024];
    char *buf = NULL;
    int sep_ix, i, key_len, value_len, tot_len;
    key_len = is_string(key);
    if (!key_len) {
    error:
	if (buf)
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	BIF_ERROR(p, BADARG);
    }
    if (is_nil(value))
	value_len = 0;
    else {
	value_len = is_string(value);
	if (!value_len)
	    goto error;
    }
    tot_len = key_len + 1 + value_len + 1;
    if (tot_len <= sizeof(def_buf))
	buf = &def_buf[0];
    else
	buf = erts_alloc(ERTS_ALC_T_TMP, tot_len);
    i = intlist_to_buf(key, buf, key_len);
    if (i != key_len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);	
    sep_ix = i;
    buf[i++] = '=';
    if (is_not_nil(value))
	i += intlist_to_buf(value, &buf[i], value_len);
    if (i != key_len + 1 + value_len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);	
    buf[i] = '\0';
    if (erts_sys_putenv(buf, sep_ix)) {
	goto error;
    }
    if (buf != &def_buf[0])
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(am_true);
}

