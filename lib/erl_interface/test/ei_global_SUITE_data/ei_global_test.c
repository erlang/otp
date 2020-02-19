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
 * See the ei_global_SUITE.erl file for a "table of contents".
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ei_runner.h"
#include "my_ussi.h"
#include "ei_connect.h"

static void cmd_ei_connect_init(char* buf, int len);
static void cmd_ei_connect(char* buf, int len);
static void cmd_ei_global_register(char* buf, int len);
static void cmd_ei_global_whereis(char* buf, int len);
static void cmd_ei_global_names(char* buf, int len);
static void cmd_ei_global_unregister(char* buf, int len);
static void cmd_ei_close_connection(char* buf, int len);

static void send_errno_result(int value);

ei_cnode ec;

static struct {
    char* name;
    int num_args;		/* Number of arguments. */
    void (*func)(char* buf, int len);
} commands[] = {
    "ei_connect_init", 	     4, cmd_ei_connect_init,
    "ei_connect", 	     1, cmd_ei_connect,
    "ei_global_register",   2, cmd_ei_global_register,
    "ei_global_whereis",    2, cmd_ei_global_whereis,
    "ei_global_names",      1, cmd_ei_global_names,
    "ei_global_unregister", 2, cmd_ei_global_unregister
};


/*
 * Sends a list contaning all data types to the Erlang side.
 */

TESTCASE(interpret)
{
    ei_x_buff x;
    int i;
    ei_term term;

    ei_init();

    ei_x_new(&x);
    while (get_bin_term(&x, &term) == 0) {
	    char* buf = x.buff, func[MAXATOMLEN];
	    int index = x.index, arity;
	    if (term.ei_type != ERL_SMALL_TUPLE_EXT || term.arity != 2)
		fail("term should be a tuple of size 2");
	    if (ei_decode_atom(buf, &index, func) < 0)
		fail("function name should be an atom");
	    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
		fail("function arguments should be a tuple");
	    for (i = 0; i < sizeof(commands)/sizeof(commands[0]); i++) {
		if (strcmp(func, commands[i].name) == 0) {
		    if (arity != commands[i].num_args)
			fail("wrong number of arguments");
		    commands[i].func(buf + index, x.buffsz - index);
		    break;
		}
	    }
	    if (i >= sizeof(commands)/sizeof(commands[0])) {
		message("\"%d\" \n", func);
		fail("bad command");
	    }
    }
    report(1);
    ei_x_free(&x);
    return;
}

static void cmd_ei_connect_init(char* buf, int len)
{
    int index = 0, r = 0;
    long l, creation;
    char b[100];
    char cookie[MAXATOMLEN], * cp = cookie;
    char socket_impl[10];
    int use_ussi;
    ei_x_buff res;
    if (ei_decode_long(buf, &index, &l) < 0)
	fail("expected int");
    sprintf(b, "c%ld", l);
    if (ei_decode_atom(buf, &index, cookie) < 0)
	fail("expected atom (cookie)");
    if (cookie[0] == '\0')
	cp = NULL;
    if (ei_decode_long(buf, &index, &creation) < 0)
	fail("expected int (creation)");
    if (ei_decode_atom_as(buf, &index, socket_impl,
                          sizeof(socket_impl), ERLANG_ASCII, NULL, NULL) < 0)
	fail("expected atom (socket_impl)");
    if (strcmp(socket_impl, "default") == 0)
        use_ussi = 0;
    else if (strcmp(socket_impl, "ussi") == 0)
        use_ussi = 1;
    else
	fail1("expected atom 'default' or 'ussi', got '%s'", socket_impl);

    if (use_ussi)
        r = ei_connect_init_ussi(&ec, b, cp, (short)creation,
                                 &my_ussi, sizeof(my_ussi), NULL);
    else
        r = ei_connect_init(&ec, b, cp, (short)creation);
    ei_x_new_with_version(&res);
    ei_x_encode_long(&res, r);
    send_bin_term(&res);
    ei_x_free(&res);
}

static void cmd_ei_connect(char* buf, int len)
{
    int index = 0;
    char node[256];
    int i;
    if (ei_decode_atom(buf, &index, node) < 0)
	fail("expected atom");
    i=ei_connect(&ec, node);
#ifdef VXWORKS
    if(i >= 0) {
	save_fd(i);
    }
#endif
    send_errno_result(i);
}

static void
cmd_ei_global_register(char* buf, int len)
{
    int index = 0;
    long fd;
    char name[256];
    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");
    if (ei_decode_atom(buf, &index, name) < 0)
        fail("expected atom");
    send_errno_result(ei_global_register((int)fd, name, ei_self(&ec)));
}

static void
cmd_ei_global_whereis(char* buf, int len)
{
    int index = 0;
    long fd;
    char name[512];
    char node_name[512];
    erlang_pid pid;
    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");
    if (ei_decode_atom(buf, &index, name) < 0)
        fail("expected atom");

    if (ei_global_whereis(&ec, fd, name, &pid, node_name) < 0)
        fail("ei_global_whereis error code");

    {
        ei_x_buff x;
        ei_x_new_with_version(&x);
        ei_x_encode_pid(&x, &pid);
        send_bin_term(&x);
        ei_x_free(&x);
    }
}

static void
cmd_ei_global_names(char* buf, int len)
{
    int index = 0;
    long fd;
    char** names = NULL;
    int count = 0, i;
    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");

    names = ei_global_names(&ec, (int)fd, &count);

    {
        ei_x_buff x;
        ei_x_new_with_version(&x);
        ei_x_encode_tuple_header(&x, 2);
        ei_x_encode_list_header(&x, count);
        for(i=0; i<count; i++) {
            ei_x_encode_string(&x, names[i]);
        }
        ei_x_encode_empty_list(&x);
        ei_x_encode_long(&x, count);
        send_bin_term(&x);
        ei_x_free(&x);
    }
}

static void
cmd_ei_global_unregister(char* buf, int len)
{
    int index = 0;
    long fd;
    char name[256];
    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");
    if (ei_decode_atom(buf, &index, name) < 0)
        fail("expected atom");

    send_errno_result(ei_global_unregister(&ec, (int)fd, name));
}

static void send_errno_result(int value)
{
    ei_x_buff x;
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_long(&x, value);
    ei_x_encode_long(&x, erl_errno);
    send_bin_term(&x);
    ei_x_free(&x);
}
