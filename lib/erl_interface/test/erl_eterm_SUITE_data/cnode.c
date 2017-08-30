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

#include <stdlib.h>
#include <stdio.h>

#include "ei.h"
#include "erl_interface.h"

#define MSGSIZE	13

#define SELF(fd) erl_mk_pid(erl_thisnodename(),fd,0,erl_thiscreation())

#ifdef VXWORKS
#define MAIN cnode
#else
#define MAIN main
#endif

/* FIXME uses mix och ei and erl_interface */

/*
   A small cnode.
   To be called from the test case erl_eterm_SUITE:cnode_1.

   1) Set up connection to node 'test_server' on the same host.
   All sends are done to a registered process named 'mip'.
   2) Create a long ref and send it.
   3) Create a pid for ourselves and send it.
   4) Receive a message.
   5) Send back the message part of the message.
   6) Send back the 'to' part of the message.
   7) Exit.
*/

MAIN(int argc, char **argv)

{
    unsigned char *msgbufp;
    int msgsize;
    ErlMessage msg;
    char msgbuf[MSGSIZE];
    char buf[100];
    char buf1[100];
    char buf2[100];
    int ix;
    int s;
    int fd;
    char node[80];
    char server[80];
    char host[80];
    int number;
    ETERM *ref, *ref1, *ref2;

    erl_init(NULL, 0);

    number = 1;
    if (argc >= 2) {
	s = erl_connect_init(number, argv[1], 0);
    } else {
	s = erl_connect_init(number, (char *) 0, 0);
    }
    gethostname(host, sizeof(host));
    sprintf(node, "c%d@%s", number, host);

    printf("s = %d\n", s);

    sprintf(server, "test_server@%s", host);
    fd = erl_connect(server);
    printf("fd = %d\n", fd);

/*    printf("dist = %d\n", erl_distversion(fd)); */

#if 1
    ref = erl_mk_long_ref(node, 4711, 113, 98, 0);
#else
    ref = erl_mk_ref(node, 4711, 0);
#endif
    printf("ref = %d\n", ref);

    s = erl_reg_send(fd, "mip", ref);
    printf("s = %d\n", s);

    {
      ETERM* emsg;
      emsg = SELF(fd);
      erl_reg_send(fd,"mip",emsg);
      erl_free_term(emsg);
    }

    msgsize = 4;
    msgbufp = (unsigned char *) malloc(msgsize);

    do {
#if 0
	s = erl_receive_msg(fd, msgbuf, MSGSIZE, &msg);
#else
	s = erl_xreceive_msg(fd, &msgbufp, &msgsize, &msg);
#endif
	switch (s) {
	  case ERL_TICK:
	    printf("tick\n");
	    break;
	  case ERL_ERROR:
	    printf("error\n");
	    break;
	  case ERL_MSG:
	    printf("msg %d\n", msgsize);
	    break;
	  default:
	    printf("unknown result %d\n", s);
	    break;
	}
    } while (s == ERL_TICK);

    s = erl_reg_send(fd, "mip", msg.msg);
    printf("s = %d\n", s);
    s = erl_reg_send(fd, "mip", msg.to);
    printf("s = %d\n", s);
#if 0
    /* from = NULL! */
    s = erl_reg_send(fd, "mip", msg.from);
    printf("s = %d\n", s);
#endif

#if 0
    /* Unused code which tests refs in some ways. */
    ix = 0;
    s = ei_encode_term(buf, &ix, ref);
    printf ("ei encode = %d, ix = %d\n", s, ix);

    /* Compare old and new ref equal */
    ref1 = erl_mk_long_ref(node, 4711, 113, 98, 0);
    ref2 = erl_mk_ref(node, 4711, 0);
    s = erl_encode(ref1, buf1);
    printf("enc1 s = %d\n", s);
    s = erl_encode(ref2, buf2);
    printf("enc2 s = %d\n", s);
    s = erl_compare_ext(buf1, buf2);
    printf("comp s = %d\n", s);
    
    /* Compare, in another way */
    s = erl_match(ref1, ref2);
    printf("match s = %d\n", s);
#endif

    erl_close_connection(fd);

    return 0;
}
