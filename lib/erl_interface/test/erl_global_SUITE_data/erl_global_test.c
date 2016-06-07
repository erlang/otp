/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
 * Purpose: Tests the functions in erl_global.c.
 *
 * See the erl_global_SUITE.erl file for a "table of contents".
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "runner.h"

static void cmd_erl_connect(ETERM* args);
static void cmd_erl_global_register(ETERM *args);
static void cmd_erl_global_whereis(ETERM *args);
static void cmd_erl_global_names(ETERM *args);
static void cmd_erl_global_unregister(ETERM *args);
static void cmd_erl_close_connection(ETERM *args);

static void send_errno_result(int value);

static struct {
    char* name;
    int num_args;		/* Number of arguments. */
    void (*func)(ETERM* args);
} commands[] = {
    "erl_connect", 	     4, cmd_erl_connect,
    "erl_close_connection",  1, cmd_erl_close_connection,
    "erl_global_register",   2, cmd_erl_global_register,
    "erl_global_whereis",    2, cmd_erl_global_whereis,
    "erl_global_names",      1, cmd_erl_global_names,
    "erl_global_unregister", 2, cmd_erl_global_unregister,
};


/*
 * Sends a list contaning all data types to the Erlang side.
 */

TESTCASE(interpret)
{
    ETERM* term;

    erl_init(NULL, 0);

    outer_loop:

    term = get_term();

    if (term == NULL) {
	report(1);
	return;
    } else {
	ETERM* Func;
	ETERM* Args;
	int i;

	if (!ERL_IS_TUPLE(term) || ERL_TUPLE_SIZE(term) != 2) {
	    fail("term should be a tuple of size 2");
	}

	Func = erl_element(1, term);
	if (!ERL_IS_ATOM(Func)) {
	    fail("function name should be an atom");
	}
	Args = erl_element(2, term);
	if (!ERL_IS_TUPLE(Args)) {
	    fail("function arguments should be a tuple");
	}
	erl_free_term(term);
	for (i = 0; i < sizeof(commands)/sizeof(commands[0]); i++) {
	    int n = strlen(commands[i].name);
	    if (ERL_ATOM_SIZE(Func) != n) {
		continue;
	    }
	    if (memcmp(ERL_ATOM_PTR(Func), commands[i].name, n) == 0) {
		erl_free_term(Func);
		if (ERL_TUPLE_SIZE(Args) != commands[i].num_args) {
		    fail("wrong number of arguments");
		}
		commands[i].func(Args);
		erl_free_term(Args);
		goto outer_loop;
	    }
	}
	fail("bad command");
    }
}

#define VERIFY_TYPE(Test, Term)		        \
if (!Test(Term)) { 				\
    fail("wrong type for " #Term);		\
} else {					\
}

static void
cmd_erl_connect(ETERM* args)
{
    ETERM* number;
    ETERM* node;
    ETERM* cookie;

    int res;
    char buffer[256];

    number = ERL_TUPLE_ELEMENT(args, 0);
    VERIFY_TYPE(ERL_IS_INTEGER, number);
    node = ERL_TUPLE_ELEMENT(args, 1);
    VERIFY_TYPE(ERL_IS_ATOM, node);
    cookie = ERL_TUPLE_ELEMENT(args, 2);
    VERIFY_TYPE(ERL_IS_ATOM, cookie);

    if (ERL_ATOM_SIZE(cookie) == 0) {
	res = erl_connect_init(ERL_INT_VALUE(number), 0, 0);
    } else {
	memcpy(buffer, ERL_ATOM_PTR(cookie), ERL_ATOM_SIZE(cookie));
	buffer[ERL_ATOM_SIZE(cookie)] = '\0';
	res = erl_connect_init(ERL_INT_VALUE(number), buffer, 0);
    }

    if(!res) {
	send_errno_result(res);
	return;
    }

    memcpy(buffer, ERL_ATOM_PTR(node), ERL_ATOM_SIZE(node));
    buffer[ERL_ATOM_SIZE(node)] = '\0';
    send_errno_result(erl_connect(buffer));
}

static void
cmd_erl_close_connection(ETERM* args)
{
    ETERM* number;
    ETERM* res;

    number = ERL_TUPLE_ELEMENT(args, 0);
    VERIFY_TYPE(ERL_IS_INTEGER, number);
    res = erl_mk_int(erl_close_connection(ERL_INT_VALUE(number)));
    send_term(res);
    erl_free_term(res);
}

static void
cmd_erl_global_register(ETERM* args)
{
    ETERM* fd_term = ERL_TUPLE_ELEMENT(args, 0);
    ETERM* name = ERL_TUPLE_ELEMENT(args, 1);
    ETERM* pid = erl_mk_pid(erl_thisnodename(), 14, 0, 0);

    char buffer[256];

    VERIFY_TYPE(ERL_IS_INTEGER, fd_term);
    VERIFY_TYPE(ERL_IS_ATOM, name);

    memcpy(buffer, ERL_ATOM_PTR(name), ERL_ATOM_SIZE(name));
    buffer[ERL_ATOM_SIZE(name)] = '\0';

    send_errno_result(erl_global_register(ERL_INT_VALUE(fd_term), buffer, pid));
    erl_free_term(pid);
}

static void
cmd_erl_global_whereis(ETERM* args)
{
    ETERM* fd_term = ERL_TUPLE_ELEMENT(args, 0);
    ETERM* name = ERL_TUPLE_ELEMENT(args, 1);
    ETERM* pid = NULL;

    char buffer[256];

    VERIFY_TYPE(ERL_IS_INTEGER, fd_term);
    VERIFY_TYPE(ERL_IS_ATOM, name);

    memcpy(buffer, ERL_ATOM_PTR(name), ERL_ATOM_SIZE(name));
    buffer[ERL_ATOM_SIZE(name)] = '\0';

    pid = erl_global_whereis(ERL_INT_VALUE(fd_term), buffer, NULL);
    send_term(pid);
    erl_free_term(pid);
}

static void
cmd_erl_global_names(ETERM* args)
{
    ETERM* fd_term = ERL_TUPLE_ELEMENT(args, 0);

    ETERM* res_array[2], *res_tuple, *name;
    char** names = NULL;
    int count = 0, i;

    VERIFY_TYPE(ERL_IS_INTEGER, fd_term);

    names = erl_global_names(ERL_INT_VALUE(fd_term), &count);

    res_array[0] = erl_mk_empty_list();
    for(i=0; i<count; i++) {
	name = erl_mk_string(names[i]);
	res_array[0] = erl_cons(name, res_array[0]);
    }

    free(names);

    res_array[1] = erl_mk_int(count);
    res_tuple = erl_mk_tuple(res_array, 2);

    send_term(res_tuple);

    erl_free_compound(res_array[0]);
    erl_free_term(res_array[1]);
    erl_free_term(res_tuple);
}

static void
cmd_erl_global_unregister(ETERM* args)
{
    ETERM* fd_term = ERL_TUPLE_ELEMENT(args, 0);
    ETERM* name = ERL_TUPLE_ELEMENT(args, 1);

    char buffer[256];

    VERIFY_TYPE(ERL_IS_INTEGER, fd_term);
    VERIFY_TYPE(ERL_IS_ATOM, name);

    memcpy(buffer, ERL_ATOM_PTR(name), ERL_ATOM_SIZE(name));
    buffer[ERL_ATOM_SIZE(name)] = '\0';

    send_errno_result(erl_global_unregister(ERL_INT_VALUE(fd_term), buffer));
}

static void
send_errno_result(int value)
{
    ETERM* res_array[2];
    ETERM* res_tuple;

    res_array[0] = erl_mk_int(value);
    res_array[1] = erl_mk_int(erl_errno);
    res_tuple = erl_mk_tuple(res_array, 2);
    send_term(res_tuple);
    erl_free_term(res_array[0]);
    erl_free_term(res_array[1]);
    erl_free_term(res_tuple);
}
