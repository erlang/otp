/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
/* Some stuff to let the Erlang and VxWorks shells coexist peacefully.
   Basically, run Erlang as a spawned task with input redirected to
   the slave side of a pseudo-tty, and connect explicitly to the master
   side of the pseudo-tty to send input to Erlang when desired. */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <ioLib.h>
#include <taskLib.h>
#include <ptyDrv.h>

extern int spTaskPriority, spTaskOptions;

#define TBUFSIZ 512

#define DEFAULT_STACK_SIZE    100000

static int slavefd = -1, masterfd = -1;
static run_erl();

/* Frontend to the Erlang startup function - callable from VxWorks shell
   or script. 'arg' is actually a string passed to the real startup. */
start_erl(arg)
int arg;
{
  int stacksize;
  char *stackenv;

    /* create and open the pty - we want the master side to be open
       all the time, since closing it probably generates EOF on the
       slave side */
    (void)ptyDevCreate("/pty/erlang.", TBUFSIZ, TBUFSIZ);
    if (slavefd != -1)
	(void)close(slavefd);
    slavefd = open("/pty/erlang.S", O_RDONLY, 0);
    if (masterfd != -1)
	(void)close(masterfd);
    masterfd = open("/pty/erlang.M", O_WRONLY, 0);

    /* flush any old leftover garbage */
    (void) ioctl(masterfd, FIOFLUSH, 0);
    if ((stackenv = getenv("ERLSTACKSIZE")) == NULL)
      stacksize = DEFAULT_STACK_SIZE;
    else
      stacksize = atoi(stackenv);
    /* spawn Erlang, via stub below */
    return(taskSpawn("erlang", spTaskPriority, spTaskOptions, stacksize,
		     run_erl, arg, 0,0,0,0,0,0,0,0,0));
}

/* Little stub that runs in the spawned task - we need this to redirect
   stdin reliably (redirections aren't "inherited" in VxWorks) */
static
run_erl(arg)
int arg;
{
    ioTaskStdSet(0, 0, slavefd); /* redirect stdin to slave side of pty */

    /* We don't want to redirect stdout/err since no one will be reading
       from the master side (to_erl - and the open()s above - could be
       made bidirectional, but still the master side would only be read
       when to_erl was running), and output can eventually fill the pty
       buffer and cause the Erlang system to block. Not redirecting
       stdout/err will have the effect that output from Erlang, e.g. the
       shell prompt, will appear on console/rlogin/whatever even when
       to_erl isn't running, which may be confusing - can't win 'em all... */

    erl_exec(arg, 0,0,0,0,0,0,0,0);	/* call the real startup */
}

/* Function callable from VxWorks shell to talk to Erlang - stop talking
   and return to VxWorks shell through ^D (EOF) */
to_erl()
{
    char buf[TBUFSIZ];
    int cc;

    if (masterfd == -1) {	/* sanity check */
	fprintf(stderr, "Must start_erl first!\n");
	return(-1);
    }
    while ((cc = read(0, buf, TBUFSIZ)) > 0) /* just pass everything through */
	if (write(masterfd, buf, cc) != cc) {
	    fprintf(stderr, "Write to Erlang failed!\n");
	    return(-1);
	}
    return(cc);
}
