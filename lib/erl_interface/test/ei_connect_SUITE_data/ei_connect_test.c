/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2018. All Rights Reserved.
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
 * Purpose: Tests the functions in ei_connect.c.
 * Author: Bjorn Gustavsson (rewritten somewhat by Jakob Cederlund)
 *
 * See the ei_connect_SUITE.erl file for a "table of contents".
 */

#include <stdio.h>
#include <string.h>
#ifdef VXWORKS
#include "reclaim.h"
#endif

#include "ei_runner.h"

static void cmd_ei_connect_init(char* buf, int len);
static void cmd_ei_connect(char* buf, int len);
static void cmd_ei_send(char* buf, int len);
static void cmd_ei_format_pid(char* buf, int len);
static void cmd_ei_send_funs(char* buf, int len);
static void cmd_ei_reg_send(char* buf, int len);
static void cmd_ei_rpc(char* buf, int len);
static void cmd_ei_set_get_tracelevel(char* buf, int len);

static void send_errno_result(int value);

ei_cnode ec;


static struct {
    char* name;
    int num_args;		/* Number of arguments. */
    void (*func)(char* buf, int len);
} commands[] = {
    "ei_connect_init",       3, cmd_ei_connect_init,
    "ei_connect", 	     1, cmd_ei_connect,
    "ei_send",  	     3, cmd_ei_send,
    "ei_send_funs",  	     3, cmd_ei_send_funs,
    "ei_reg_send", 	     3, cmd_ei_reg_send,
    "ei_rpc",  		     4, cmd_ei_rpc,
    "ei_set_get_tracelevel", 1, cmd_ei_set_get_tracelevel,
    "ei_format_pid",         2, cmd_ei_format_pid,
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
    long l;
    char b[100];
    char cookie[MAXATOMLEN], * cp = cookie;
    ei_x_buff res;
    if (ei_decode_long(buf, &index, &l) < 0)
	fail("expected int");
    sprintf(b, "c%ld", l);
    if (ei_decode_atom(buf, &index, cookie) < 0)
	fail("expected atom (cookie)");
    if (cookie[0] == '\0')
	cp = NULL;
    r = ei_connect_init(&ec, b, cp, 0);
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

static void cmd_ei_set_get_tracelevel(char* buf, int len)
{
    int  index = 0;
    long level = 0;
    long ret   = 0;
    ei_x_buff x;

    if (ei_decode_long(buf, &index, &level) < 0) {
	fail("expected long");
    }
    
    ei_set_tracelevel((int)level);

    ret = (long) ei_get_tracelevel();

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, "tracelevel");
    ei_x_encode_long(&x, ret);
    send_bin_term(&x);
    ei_x_free(&x);
}

static void cmd_ei_send(char* buf, int len)
{
    int index = 0;
    long fd;
    erlang_pid pid;
    ei_x_buff x;

    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");
    if (ei_decode_pid(buf, &index, &pid) < 0)
	fail("expected pid (node)");
    if (ei_x_new_with_version(&x) < 0)
	fail("ei_x_new_with_version");
    if (ei_x_append_buf(&x, &buf[index], len - index) < 0)
	fail("append");
    send_errno_result(ei_send(fd, &pid, x.buff, x.index));
    ei_x_free(&x);
}

static void cmd_ei_format_pid(char* buf, int len)
{
    int index = 0;
    long fd;
    erlang_pid pid;
    ei_x_buff x;

    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");
    if (ei_decode_pid(buf, &index, &pid) < 0)
	fail("expected pid (node)");
    if (ei_x_new_with_version(&x) < 0)
	fail("ei_x_new_with_version");
    if (ei_x_format_wo_ver(&x, "~p", &pid) < 0)
	fail("ei_x_format_wo_ver");
    send_errno_result(ei_send(fd, &pid, x.buff, x.index));
    ei_x_free(&x);
}

static void cmd_ei_send_funs(char* buf, int len)
{
    int index = 0, n;
    long fd;
    erlang_pid pid;
    ei_x_buff x;
    erlang_fun fun1, fun2;
    char* bitstring;
    size_t bits;
    int bitoffs;

    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");
    if (ei_decode_pid(buf, &index, &pid) < 0)
	fail("expected pid (node)");
    if (ei_decode_tuple_header(buf, &index, &n) < 0)
	fail("expected tuple");
    if (n != 3)
	fail("expected tuple");
    if (ei_decode_fun(buf, &index, &fun1) < 0)
	fail("expected Fun1");
    if (ei_decode_fun(buf, &index, &fun2) < 0)
	fail("expected Fun2");
    if (ei_decode_bitstring(buf, &index, &bitstring, &bitoffs, &bits) < 0)
	fail("expected bitstring");
    if (ei_x_new_with_version(&x) < 0)
	fail("ei_x_new_with_version");
    if (ei_x_encode_tuple_header(&x, 3) < 0)
	fail("encode tuple header");
    if (ei_x_encode_fun(&x, &fun1) < 0)
	fail("encode fun1");
    if (ei_x_encode_fun(&x, &fun2) < 0)
	fail("encode fun2");
    if (ei_x_encode_bitstring(&x, bitstring, bitoffs, bits) < 0)
	fail("encode bitstring");
    free_fun(&fun1);
    free_fun(&fun2);
    send_errno_result(ei_send(fd, &pid, x.buff, x.index));
    ei_x_free(&x);
}

static void cmd_ei_reg_send(char* buf, int len)
{
    int index = 0;
    long fd;
    char reg_name[MAXATOMLEN];
    erlang_pid pid;
    ei_x_buff x;
    
    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long (fd)");
    if (ei_decode_atom(buf, &index, reg_name) < 0)
	fail("expected atom (reg name)");
    if (ei_x_new_with_version(&x) < 0)
	fail("ei_x_new_with_version");
    if (ei_x_append_buf(&x, &buf[index], len - index) < 0)
	fail("append");
    send_errno_result(ei_reg_send(&ec, fd,
				  reg_name, x.buff, x.index));
    ei_x_free(&x);
}

static void cmd_ei_rpc(char* buf, int len)
{
    int index = 0, n;
    long fd;
    erlang_pid pid;
    ei_x_buff x, rpc_x;
    int r;
    char mod[MAXATOMLEN], func[MAXATOMLEN];

#if 0 && defined(__WIN32__) 
    DebugBreak();
#endif

    if (ei_decode_long(buf, &index, &fd) < 0)
	fail("expected long");
    if (ei_decode_pid(buf, &index, &pid) < 0)
	fail("expected pid (node)");
    if (ei_decode_tuple_header(buf, &index, &n) < 0 && n < 2)
	fail("expected tuple {module, function}");
    if (ei_decode_atom(buf, &index, mod) < 0)
	fail("expected atom (module)");
    if (ei_decode_atom(buf, &index, func) < 0)
	fail("expected atom (function)");
    message("pid %s %d %d %d\n", pid.node, pid.num, pid.serial, pid.creation);
    message("{%s, %s}\n", mod, func);
    if (ei_x_new(&rpc_x) < 0)
	fail("ei_x_new");
    if (ei_rpc(&ec, fd, mod, func, &buf[index], len - index, &rpc_x) < 0)
	fail("ei_rpc");
    if (ei_x_new_with_version(&x) < 0)
 	fail("ei_x_new_with_version");
    if (ei_x_append(&x, &rpc_x) < 0)
	fail("append");
    send_bin_term(&x);
    /*send_errno_result(ei_send(&ec, fd, &pid, x.buff, x.index));*/
    ei_x_free(&x);
    ei_x_free(&rpc_x);
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
