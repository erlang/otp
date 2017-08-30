/* 
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
 * Purpose: Test the erl_print_term() function.
 * Author:  Bjorn Gustavsson
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifndef __WIN32__
#include <unistd.h>
#endif

#include "erl_interface.h"

#ifndef __WIN32__
#define _O_BINARY 0
#define _setmode(fd, mode)
#endif

#define HEADER_SIZE 2

static int readn(int, unsigned char*, int);

/*
 * This program doesn't use the runner, because it needs a packet
 * on input, but the result will be as a stream of bytes (since
 * erl_print_term() prints directly on a file).
 *
 * Input is a package of with a packet header size of two bytes.
 *
 *  +------------------------------------------------------------+
 *  | length    | Encoded term...                                |
 *  | (2 bytes) | (as given by "length")                         |
 *  +------------------------------------------------------------+
 *
 *              <------------------- length --------------------->
 *
 * This program decodes the encoded terms and passes it to
 * erl_print_term().  Then this program prints
 *
 *    CR <result> LF
 *
 * and waits for a new package.  <result> is the return value from
 * erl_print_term(), formatted as an ASCII string.
 */

#ifdef VXWORKS
int print_term()
#else
int main()
#endif
{
    _setmode(0, _O_BINARY);
    _setmode(1, _O_BINARY);

    erl_init(NULL, 0);

    for (;;) {
	char buf[4*1024];
	ETERM* term;
	char* message;
	int n;

	if (readn(0, buf, 2) <= 0) {
	  /* fprintf(stderr, "error reading message header\n"); */
	  /* actually this is where we leave the infinite loop */
	  exit(1);
	}
	n = buf[0] * 256 + buf[1];
	if (readn(0, buf, n) < 0) {
	  fprintf(stderr, "error reading message contents\n");
	  exit(1);
	}

	term = erl_decode(buf);
	if (term == NULL) {
	    fprintf(stderr, "erl_decode() failed\n");
	    exit(1);
	}
	n = erl_print_term(stdout, term);
	erl_free_compound(term);
	fprintf(stdout,"\r%d\n", n);
	fflush(stdout);
    }
}

/*
 * Reads len number of bytes.
 */

static int
readn(fd, buf, len)
     int fd;			/* File descriptor to read from. */
     unsigned char *buf;	/* Store in this buffer. */
     int len;			/* Number of bytes to read. */
{
    int n;			/* Byte count in last read call. */
    int sofar = 0;		/* Bytes read so far. */

    do {
	if ((n = read(fd, buf+sofar, len-sofar)) <= 0)
	    /* error or EOF in read */
	    return(n);
	sofar += n;
    } while (sofar < len);
    return sofar;
}

