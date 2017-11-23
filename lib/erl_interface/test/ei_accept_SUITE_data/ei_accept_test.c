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

/*
 * Purpose: Tests the accept function in ei_connect.c.
 * Author: Jakob Cederlund (taken from erl_connect by Björn Gustavsson)
 *
 * See the ei_accept_SUITE.erl file for a "table of contents".
 */

#include <stdio.h>
#include <string.h>
#ifdef VXWORKS
#include "reclaim.h"
#endif

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "ei_runner.h"

static void cmd_ei_connect_init(char* buf, int len);
static void cmd_ei_publish(char* buf, int len);
static void cmd_ei_accept(char* buf, int len);
static void cmd_ei_receive(char* buf, int len);
static void cmd_ei_unpublish(char* buf, int len);

static void send_errno_result(int value);

ei_cnode ec;


static struct {
    char* name;
    int num_args;		/* Number of arguments. */
    void (*func)(char* buf, int len);
} commands[] = {
    "ei_connect_init",  3, cmd_ei_connect_init,
    "ei_publish", 	1, cmd_ei_publish,
    "ei_accept", 	1, cmd_ei_accept,
    "ei_receive",  	1, cmd_ei_receive,
    "ei_unpublish",     0, cmd_ei_unpublish
};

/*
 * Sends a list contaning all data types to the Erlang side.
 */
TESTCASE(interpret)
{
    ei_x_buff x;
    int i;
    ei_term term;

    ei_x_new(&x);
    for (;;) {
	if (get_bin_term(&x, &term)) {
	    report(1);
	    return;
	} else {
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
    }	
}

static void cmd_ei_connect_init(char* buf, int len)
{
    int index = 0, r = 0;
    int type, size;
    long l;
    char b[100];
    char cookie[MAXATOMLEN], * cp = cookie;
    ei_x_buff res;
    if (ei_decode_long(buf, &index, &l) < 0)
	fail("expected int");
    sprintf(b, "c%d", l);
    /* FIXME don't use internal and maybe use skip?! */
    ei_get_type_internal(buf, &index, &type, &size);
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

static int my_listen(int port)
{
    int listen_fd;
    struct sockaddr_in addr;
    const char *on = "1";
    
    if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	return -1;
    
    setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, on, sizeof(on));
    
    memset((void*) &addr, 0, (size_t) sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    
    if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
	return -1;

    listen(listen_fd, 5);
    return listen_fd;
}

static void cmd_ei_publish(char* buf, int len)
{
    int index = 0;
    int listen, r;
    long port;
    ei_x_buff x;
    int i;

    /* get port */
    if (ei_decode_long(buf, &index, &port) < 0)
	fail("expected int (port)");
    /* Make a listen socket */
    if ((listen = my_listen(port)) <= 0)
	fail("listen");
    
    if ((i = ei_publish(&ec, port)) == -1)
	fail("ei_publish");
#ifdef VXWORKS
    save_fd(i);
#endif
    /* send listen-fd, result and errno */
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 3);
    ei_x_encode_long(&x, listen);
    ei_x_encode_long(&x, i);
    ei_x_encode_long(&x, erl_errno);
    send_bin_term(&x);
    ei_x_free(&x);
}

static void cmd_ei_accept(char* buf, int len)
{
    int index = 0;
    int r;
    ErlConnect conn;
    long listen;
    ei_x_buff x;
    int i;

    /* get port */
    if (ei_decode_long(buf, &index, &listen) < 0)
	fail("expected int (listen fd)");

    r = ei_accept(&ec, listen, &conn);
#ifdef VXWORKS
    save_fd(r);
#endif
    /* send result, errno and nodename */
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 3);
    ei_x_encode_long(&x, r);
    ei_x_encode_long(&x, erl_errno);
    ei_x_encode_atom(&x, conn.nodename); /* or rather string? */
    send_bin_term(&x);
    ei_x_free(&x);
}

static void cmd_ei_receive(char* buf, int len)
{
    ei_x_buff x;
    erlang_msg msg;
    long l;
    int fd, index = 0;
    
    if (ei_decode_long(buf, &index, &l) < 0)
	fail("expected int (fd)");
    fd = l;
    ei_x_new(&x);
    for (;;) {
	int got = ei_xreceive_msg(fd, &msg, &x);
	if (got == ERL_TICK)
	    continue;
	if (got == ERL_ERROR)
	    fail1("ei_xreceive_msg, got==%d", got);
	break;
    }
    index = 1;
    send_bin_term(&x);
    ei_x_free(&x);
}

static void cmd_ei_unpublish(char* buf, int len)
{
    send_errno_result(ei_unpublish(&ec));
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
