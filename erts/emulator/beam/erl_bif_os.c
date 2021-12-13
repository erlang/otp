/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2021. All Rights Reserved.
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
#include "erl_osenv.h"

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

static void os_getenv_foreach(Process *process, Eterm *result, Eterm key, Eterm value)
{
    Eterm kvp_term, *hp;

    hp = HAlloc(process, 5);
    kvp_term = TUPLE2(hp, key, value);
    hp += 3;

    (*result) = CONS(hp, kvp_term, (*result));
}

BIF_RETTYPE os_env_0(BIF_ALIST_0)
{
    const erts_osenv_t *global_env;
    Eterm result = NIL;

    global_env = erts_sys_rlock_global_osenv();
    erts_osenv_foreach_term(global_env, BIF_P, &result, (void*)&os_getenv_foreach);
    erts_sys_runlock_global_osenv();

    return result;
}

BIF_RETTYPE os_getenv_1(BIF_ALIST_1)
{
    const erts_osenv_t *global_env;
    Eterm out_term;
    int error;

    global_env = erts_sys_rlock_global_osenv();
    error = erts_osenv_get_term(global_env, BIF_P, BIF_ARG_1, &out_term);
    erts_sys_runlock_global_osenv();

    if (error == 0) {
        return am_false;
    } else if (error < 0) {
        BIF_ERROR(BIF_P, BADARG);
    } 

    return out_term;
}

BIF_RETTYPE os_putenv_2(BIF_ALIST_2)
{
    erts_osenv_t *global_env;
    int error;

    global_env = erts_sys_rwlock_global_osenv();
    error = erts_osenv_put_term(global_env, BIF_ARG_1, BIF_ARG_2);
    erts_sys_rwunlock_global_osenv();

    if (error < 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    BIF_RET(am_true);
}

BIF_RETTYPE os_unsetenv_1(BIF_ALIST_1)
{
    erts_osenv_t *global_env;
    int error;

    global_env = erts_sys_rwlock_global_osenv();
    error = erts_osenv_unset_term(global_env, BIF_ARG_1);
    erts_sys_rwunlock_global_osenv();

    if (error < 0) {
        BIF_ERROR(BIF_P, BADARG);
    }

    BIF_RET(am_true);
}

BIF_RETTYPE os_set_signal_2(BIF_ALIST_2) {
    if (! ( (BIF_ARG_2 == am_ignore) ||
            (BIF_ARG_2 == am_default) ||
            (BIF_ARG_2 == am_handle) )) {
        BIF_P->fvalue = am_badopt;
        BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
    }

    if (is_atom(BIF_ARG_1)) {
        if (erts_set_signal(BIF_ARG_1, BIF_ARG_2)) {
            BIF_RET(am_ok);
        }
    }

    BIF_ERROR(BIF_P, BADARG);
}
