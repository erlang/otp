/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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


BIF_RETTYPE os_getpid_0(BIF_ALIST_0)
{
     char pid_string[21]; /* enough for a 64 bit number */
     int n;
     Eterm* hp;
     sys_get_pid(pid_string, sizeof(pid_string)); /* In sys.c */
     n = sys_strlen(pid_string);
     hp = HAlloc(BIF_P, n*2);
     BIF_RET(buf_to_intlist(&hp, pid_string, n, NIL));
}

BIF_RETTYPE os_getenv_0(BIF_ALIST_0)
{
    GETENV_STATE state;
    char *cp;
    Eterm* hp;
    Eterm ret;
    Eterm str;

    init_getenv_state(&state);

    ret = NIL;
    while ((cp = getenv_string(&state)) != NULL) {
	str = erts_convert_native_to_filename(BIF_P,(byte *)cp);
	hp = HAlloc(BIF_P, 2);
	ret = CONS(hp, str, ret);
    }

    fini_getenv_state(&state);

    return ret;
}

#define STATIC_BUF_SIZE 1024
BIF_RETTYPE os_getenv_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm str;
    Sint len;
    int res;
    char *key_str, *val;
    char buf[STATIC_BUF_SIZE];
    size_t val_size = sizeof(buf);

    key_str = erts_convert_filename_to_native(BIF_ARG_1,buf,STATIC_BUF_SIZE,
					      ERTS_ALC_T_TMP,1,0,&len);

    if (!key_str) {
	BIF_ERROR(p, BADARG);
    }

    if (key_str != &buf[0])
	val = &buf[0];
    else {
	/* len includes zero byte */
	val_size -= len;
	val = &buf[len];
    }
    res = erts_sys_getenv(key_str, val, &val_size);

    if (res < 0) {
    no_var:
	str = am_false;
    } else {
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
	str = erts_convert_native_to_filename(p,(byte *)val);
    }
    if (key_str != &buf[0])
	erts_free(ERTS_ALC_T_TMP, key_str);
    if (val < &buf[0] || &buf[sizeof(buf)-1] < val)
	erts_free(ERTS_ALC_T_TMP, val);
    BIF_RET(str);
}

BIF_RETTYPE os_putenv_2(BIF_ALIST_2)
{
    char def_buf_key[STATIC_BUF_SIZE];
    char def_buf_value[STATIC_BUF_SIZE];
    char *key_buf, *value_buf;

    key_buf = erts_convert_filename_to_native(BIF_ARG_1,def_buf_key,
					      STATIC_BUF_SIZE,
					      ERTS_ALC_T_TMP,0,0,NULL);
    if (!key_buf) {
	BIF_ERROR(BIF_P, BADARG);
    }
    value_buf = erts_convert_filename_to_native(BIF_ARG_2,def_buf_value,
						STATIC_BUF_SIZE,
						ERTS_ALC_T_TMP,1,0,
						NULL);
    if (!value_buf) {
	if (key_buf != def_buf_key) {
	    erts_free(ERTS_ALC_T_TMP, key_buf);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
	    

    if (erts_sys_putenv(key_buf, value_buf)) {
	if (key_buf != def_buf_key) {
	    erts_free(ERTS_ALC_T_TMP, key_buf);
	}
	if (value_buf != def_buf_value) {
	    erts_free(ERTS_ALC_T_TMP, value_buf);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    if (key_buf != def_buf_key) {
	erts_free(ERTS_ALC_T_TMP, key_buf);
    }
    if (value_buf != def_buf_value) {
	erts_free(ERTS_ALC_T_TMP, value_buf);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE os_unsetenv_1(BIF_ALIST_1)
{
    char *key_buf;
    char buf[STATIC_BUF_SIZE];

    key_buf = erts_convert_filename_to_native(BIF_ARG_1,buf,STATIC_BUF_SIZE,
					      ERTS_ALC_T_TMP,0,0,NULL);
    if (!key_buf) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (erts_sys_unsetenv(key_buf)) {
	if (key_buf != buf) {
	    erts_free(ERTS_ALC_T_TMP, key_buf);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    if (key_buf != buf) {
	erts_free(ERTS_ALC_T_TMP, key_buf);
    }
    BIF_RET(am_true);
}
