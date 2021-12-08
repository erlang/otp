/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2021. All Rights Reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __WIN32__
#  include <io.h>
#else
#  include <unistd.h>
#  include <errno.h>
#  include <pthread.h>
#endif
#include "ei.h"

static void usage1(char *what);
static void usage2(char *what, char *val);
static void fail1(char *what);

char *progname;
ei_cnode cnode;
char *node_name = NULL;
char *cookie = NULL;
short creation = -1;

#ifdef __WIN32__
static unsigned __stdcall ei_node_executor(LPVOID unused)
#else
static void *ei_node_executor(void *unused)
#endif
{
    int lfd, pfd, res, port;

    ei_init();
    res = ei_connect_init(&cnode, node_name, cookie, creation);
    if (res < 0)
        fail1("ei_connect_init");
    port = 0;
    lfd = ei_listen(&cnode, &port, 5);
    if (lfd < 0)
	fail1("ei_listen");
    pfd = ei_publish(&cnode, port);
    if (pfd < 0)
	fail1("ei_publish");

    printf("accepting");
    fflush(stdout);

    while (!0)  {
        ErlConnect conn;
        int afd = ei_accept(&cnode, lfd, &conn);
        if (afd < 0)
            fail1("ei_accept");

        while (!0) {
            ei_x_buff x;
            erlang_msg msg;
            ei_x_new(&x);
            do {
                res = ei_xreceive_msg(afd, &msg, &x);
            } while (res == ERL_TICK);
            if (res != ERL_MSG) {
                /* connection closed... */
                ei_close_connection(afd);
                break;
            }

            switch (msg.msgtype) {
            case ERL_SEND:
            case ERL_REG_SEND:
                ei_send_reg_encoded(afd, &cnode.self, "cnode_forward_receiver",
                                    x.buff, x.index);
                break;
            case ERL_LINK:
                fail1("received link");
                break;
            case ERL_UNLINK:
                fail1("received unlink");
                break;
            case ERL_EXIT:
                fail1("received exit");
                break;
            default:
                fail1("received invalid signal");
                break;
            }
            ei_x_free(&x);
        }
    }
    
#ifdef __WIN32__
    return 0;
#else
    return NULL;
#endif
}

int
main(int argc, char *argv[])
{
#ifdef __WIN32__
    unsigned tid;
    HANDLE thndl;
#else
    pthread_t tid;
#endif
    int i;
    
    progname = argv[0];
    i = 1;
    while (i < argc) {
        if (strcmp(argv[i], "-sname") == 0) {
            i++;
            if (i == argc)
                usage1("Missing -sname value");
            node_name = argv[i];
        }
        else if (strcmp(argv[i], "-cookie") == 0) {
            i++;
            if (i == argc)
                usage1("Missing -cookie value");
            cookie = argv[i];
        }
        else if (strcmp(argv[i], "-creation") == 0) {
            char *endp;
            long int val;
            i++;
            if (i == argc)
                usage1("Missing -creation value");
            val = strtol(argv[i], &endp, 10);
            creation = (short) val;
            if (*endp != '\0' || creation < 4)
                usage2("Invalid creation", argv[i]);
        }
        else {
            usage2("Unknown parameter", argv[i]);
        }
        i++;
    }

    if (!node_name)
        usage1("Missing -sname parameter");
    if (!cookie)
        usage1("Missing -cookie parameter");
    if (creation < 0)
        usage1("Missing -creation parameter");

#ifdef __WIN32__
    thndl = (HANDLE) _beginthreadex(NULL, 0, ei_node_executor,
                                    NULL, 0, &tid);
    if (thndl == (HANDLE) 0)
        fail1("_beginthreadex() failed");
#else
    if (pthread_create(&tid, NULL, ei_node_executor, NULL) != 0)
        fail1("pthread_create() failed");
#endif
    
    /* Wait until stdin is closed. Terminate when that happens... */
    while (!0) {
        char buf[128];
#ifdef __WIN32__
        int res = _read(0, (void *) &buf[0], sizeof(buf));
#else
        ssize_t res = read(0, (void *) &buf[0], sizeof(buf));
#endif
        if (res == 0)
            exit(0); /* erlang port closed... */
        if (res < 0 && errno != EINTR) {
            fail1("read() failed");
        }
    }

    return 0;
}

static void
usage1(char *what)
{
    fprintf(stderr,
            "ERROR: %s\n\n"
            "  Usage:\n"
            "    %s -sname <node name> -cookie <cookie> -creation <creation>\n",
            what, progname);
    exit(1);
}

static void
usage2(char *what, char *val)
{
    fprintf(stderr,
            "ERROR: %s: %s\n\n"
            "  Usage:\n"
            "    %s -sname <node name> -cookie <cookie> -creation <creation>\n",
            what, val, progname);
    exit(1);
}

static void
fail1(char *what)
{
    fprintf(stderr, "%s FAILED: %s; %d\n",
            cnode.thisalivename, what, erl_errno);
    exit(1);
}
