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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef __WIN32__
#include <unistd.h>

#include <sys/time.h>

#define O_BINARY 0
#define _setmode(fd, mode)
#endif

#ifdef __WIN32__
#include "windows.h"
#include "winbase.h"
#endif

#define MAIN(argc, argv) main(argc, argv)

extern int errno;

static void delay(unsigned ms);


MAIN(argc, argv)
int argc;
char *argv[];
{
    int x;
    if (argc < 2) {
	fprintf(stderr,"Usage %s <milliseconds>\n",argv[0]);
	return 1;
    }
    if ((x = atoi(argv[1])) <= 0) {
	fprintf(stderr,"Usage %s <milliseconds>\n",argv[0]);
	return 1;
    }
    delay(x);
    return 0;
}

static void
delay(unsigned ms)
{
#ifdef __WIN32__
  Sleep(ms);
#else
  struct timeval t;
  t.tv_sec = ms/1000;
  t.tv_usec = (ms % 1000) * 1000;

  select(0, NULL, NULL, NULL, &t);
#endif
}
