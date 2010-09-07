/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2010. All Rights Reserved.
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

#ifdef VXWORKS
#include <vxWorks.h>
#include <taskVarLib.h>
#include <taskLib.h>
#include <sysLib.h>
#include <string.h>
#include <ioLib.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef __WIN32__
#include <unistd.h>

#ifdef VXWORKS
#include "reclaim.h"
#include <sys/times.h>
#else
#include <sys/time.h>
#endif

#define O_BINARY 0
#define _setmode(fd, mode)
#endif

#ifdef __WIN32__
#include "windows.h"
#include "winbase.h"
#endif


#ifdef VXWORKS
#define MAIN(argc, argv) port_test(argc, argv)
#else
#define MAIN(argc, argv) main(argc, argv)
#endif


extern int errno;

static void delay(unsigned ms);


MAIN(argc, argv)
int argc;
char *argv[];
{
    int x;
    if (argc < 2) {
	fprintf(stderr,"Usage %s <seconds>\n",argv[0]);
	return 1;
    }
    if ((x = atoi(argv[1])) <= 0) {
	fprintf(stderr,"Usage %s <seconds>\n",argv[0]);
	return 1;
    }
    delay(x*1000);
    return 0;
}

static void
delay(unsigned ms)
{
#ifdef VXWORKS
  taskDelay((sysClkRateGet() * ms) / 1000);
#else
#ifdef __WIN32__
  Sleep(ms);
#else
  struct timeval t;
  t.tv_sec = ms/1000;
  t.tv_usec = (ms % 1000) * 1000;

  select(0, NULL, NULL, NULL, &t);
#endif
#endif
}
