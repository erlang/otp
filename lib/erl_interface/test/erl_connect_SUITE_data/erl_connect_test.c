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
 * Purpose: Tests the functions in erl_connect.c.
 * Author: Bjorn Gustavsson
 *
 * See the erl_connect_SUITE.erl file for a "table of contents".
 */

#include <stdio.h>
#include <string.h>

#include "runner.h"

static void cmd_erl_connect_init(ETERM* args);
static void cmd_erl_connect(ETERM* args);
static void cmd_erl_send(ETERM* args);
static void cmd_erl_reg_send(ETERM* args);
static void cmd_erl_close_connection(ETERM *args);

static void send_errno_result(int value);

static struct {
    char* name;
    int num_args;		/* Number of arguments. */
    void (*func)(ETERM* args);
} commands[] = {
    "erl_connect_init", 3, cmd_erl_connect_init,
    "erl_connect", 	1, cmd_erl_connect,
    "erl_close_connection", 	1, cmd_erl_close_connection,
    "erl_send", 	3, cmd_erl_send,
    "erl_reg_send", 	3, cmd_erl_reg_send,
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
cmd_erl_connect_init(ETERM* args)
{
    ETERM* number;
    ETERM* res;
    ETERM* cookie;
    char cookie_buffer[256];

    number = ERL_TUPLE_ELEMENT(args, 0);
    VERIFY_TYPE(ERL_IS_INTEGER, number);
    cookie = ERL_TUPLE_ELEMENT(args, 1);
    VERIFY_TYPE(ERL_IS_ATOM, cookie);
    if (ERL_ATOM_SIZE(cookie) == 0) {
	res = erl_mk_int(erl_connect_init(ERL_INT_VALUE(number), 0, 0));
    } else {
	memcpy(cookie_buffer, ERL_ATOM_PTR(cookie), ERL_ATOM_SIZE(cookie));
	cookie_buffer[ERL_ATOM_SIZE(cookie)] = '\0';
	res = erl_mk_int(erl_connect_init(ERL_INT_VALUE(number), 
					  cookie_buffer, 0));
    }
    send_term(res);
    erl_free_term(res);
}

static void
cmd_erl_connect(ETERM* args)
{
    ETERM* node;
    char node_buffer[256];

    node = ERL_TUPLE_ELEMENT(args, 0);
    VERIFY_TYPE(ERL_IS_ATOM, node);
    memcpy(node_buffer, ERL_ATOM_PTR(node), ERL_ATOM_SIZE(node));
    node_buffer[ERL_ATOM_SIZE(node)] = '\0';
    send_errno_result(erl_connect(node_buffer));
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
cmd_erl_send(ETERM* args)
{
    ETERM* fd_term = ERL_TUPLE_ELEMENT(args, 0);
    ETERM* to = ERL_TUPLE_ELEMENT(args, 1);
    ETERM* msg = ERL_TUPLE_ELEMENT(args, 2);

    VERIFY_TYPE(ERL_IS_INTEGER, fd_term);
    send_errno_result(erl_send(ERL_INT_VALUE(fd_term), to, msg));
}

static void
cmd_erl_reg_send(ETERM* args)
{
    ETERM* fd_term = ERL_TUPLE_ELEMENT(args, 0);
    ETERM* to = ERL_TUPLE_ELEMENT(args, 1);
    ETERM* msg = ERL_TUPLE_ELEMENT(args, 2);
    char reg_name[256];

    VERIFY_TYPE(ERL_IS_INTEGER, fd_term);
    VERIFY_TYPE(ERL_IS_ATOM, to);
    memcpy(reg_name, ERL_ATOM_PTR(to), ERL_ATOM_SIZE(to));
    reg_name[ERL_ATOM_SIZE(to)] = '\0';
    send_errno_result(erl_reg_send(ERL_INT_VALUE(fd_term), reg_name, msg));
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
