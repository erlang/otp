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

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifndef __WIN32__
#include <unistd.h>
#endif
#include <stdarg.h>

#include "runner.h"

#ifndef __WIN32__
#define _O_BINARY 0
#define _setmode(fd, mode)
#endif

#define HEADER_SIZE 4

static char* progname;		/* Name of this program (from argv[0]). */
static int fd_from_erl;		/* File descriptor from Erlang. */
static int fd_to_erl;		/* File descriptor to Erlang. */

static int packet_loop();
static void ensure_buf_big_enough();
static int readn();
static void reply(char* buf, unsigned size);
static void dump();

void
run_tests(char* argv0, TestCase test_cases[], unsigned number)
{
    int i;
    int n;
    char* packet;

    progname = argv0;
    _setmode(0, _O_BINARY);
    _setmode(1, _O_BINARY);
    fd_from_erl = 0;
    fd_to_erl = 1;

    packet = read_packet(&n);

    /*
     * Dispatch to the appropriate test function.
     */

    i = packet[0] * 256 + packet[1];
    if (i >= number) {
	fprintf(stderr, "%s: bad test case number %d",
		progname, i);
	free(packet);
	exit(1);
    } else {
	(*test_cases[i])();
	free(packet);
    }
}


/***********************************************************************
 *
 * R e a d i n g   p a c k e t s
 *
 ************************************************************************/

/*
 * Reads an Erlang term.
 *
 * Returns: A pointer to a term (an ETERM structure) if there was
 * at term available, or a NULL pointer if there was an 'eot' (end-of-test)
 * packet.  Aborts if anything else received.
 */

ETERM*
get_term(void)
{
    char* encoded;
    ETERM* term;
    int n;

    encoded = read_packet(&n);

    switch (encoded[0]) {
    case 'e':
      free(encoded);
      return NULL;
    case 't':
	term = erl_decode(encoded+1);
	free(encoded);
	if (term == NULL) {
	    fail("Failed to decode term");
	    exit(0);
	}
	return term;
    default:
	fprintf(stderr, "Garbage received: ");
	dump(encoded, n, 16);
	putc('\n', stderr);
	fail("C program received garbage");
	free(encoded);
	exit(1);
    }
}


/*
 * Reads a packet from Erlang.  The packet must be a standard {packet, 2}
 * packet.  This function aborts if any error is detected (including EOF).
 *
 * Returns: The number of bytes in the packet.
 */

char *read_packet(int *len)
{

  unsigned char* io_buf = NULL; /* Buffer for file i/o. */
  int i;
  unsigned char header[HEADER_SIZE];
  unsigned packet_length;		/* Length of current packet. */
  int bytes_read;
    
  /*
   * Read the packet header.
   */
    
  bytes_read = readn(fd_from_erl, header, HEADER_SIZE);

  if (bytes_read == 0) {
    fprintf(stderr, "%s: Unexpected end of file\n", progname);
    exit(1);
  }
  if (bytes_read != HEADER_SIZE) {
    fprintf(stderr, "%s: Failed to read packet header\n", progname);
    exit(1);
  }

  /*
   * Get the length of this packet.
   */
	
  packet_length = 0;

  for (i = 0; i < HEADER_SIZE; i++)
    packet_length = (packet_length << 8) | header[i];
    
  if (len) *len=packet_length; /* report length only if caller requested it */

  if ((io_buf = (char *) malloc(packet_length)) == NULL) {
    fprintf(stderr, "%s: insufficient memory for i/o buffer of size %d\n",
	    progname, packet_length);
    exit(1);
  }

  /*
   * Read the packet itself.
   */
    
  bytes_read = readn(fd_from_erl, io_buf, packet_length);
  if (bytes_read != packet_length) {
    fprintf(stderr, "%s: couldn't read packet of length %d\r\n",
	    progname, packet_length);
    free(io_buf);
    exit(1);
  }

  return io_buf;
}


/***********************************************************************
 * S e n d i n g   r e p l i e s
 *
 * The functions below send various types of replies back to Erlang.
 * Each reply start with a letter indicating the type of reply.
 *
 * Reply		Translated to on Erlang side
 * -----		----------------------------
 * [$b|Bytes]		{bytes, Bytes}
 * [$e]			eot
 * [$f]			ct:fail()
 * [$f|Reason]		ct:fail(Reason)
 * [$t|EncodedTerm]	{term, Term}
 * [$N]			'NULL'
 * [$m|Message]		io:format("~s", [Message])   (otherwise ignored)
 *
 ***********************************************************************/

/*
 * This function reports the outcome of a test fail.  It is useful if
 * you implement a test case entirely in C code.
 *
 * If the ok argument is zero, a [$f] reply will be sent to the
 * Erlang side (causing ct:fail() to be called); otherwise,
 * the atom 'eot' will be sent to Erlang.
 *
 * If you need to provide more details on a failure, use the fail() function.
 */

void
do_report(file, line, ok)
    char* file;
    int line;
    int ok;			/* Zero if failed; non-zero otherwise. */
{
    char reason;
    unsigned long ab;
    unsigned long fb;

    reason = ok ? 'e' : 'f';

    if (!ok) {
	do_fail(file, line, "Generic failure");
    } else {
      /* release all unallocated blocks */
      erl_eterm_release();
      /* check mem usage stats */
      erl_eterm_statistics(&ab, &fb);
      if ((ab == 0) && (fb == 0) ) {
	reply(&reason, 1);
      }
      else {
	char sbuf[128];
	    
	sprintf(sbuf, "still %lu terms allocated,"
		" %lu on freelist at end of test", ab, fb);
	do_fail(file, line, sbuf);
      }
    }
}


/*
 * This function causes a call to ct:fail(Reason) on the
 * Erlang side.
 */

void
do_fail(char* file, int line, char* reason)
{
    char sbuf[2048];

    sbuf[0] = 'f';
    sprintf(sbuf+1, "%s, line %d: %s", file, line, reason);
    reply(sbuf, 1+strlen(sbuf+1));
}

/*
 * This function sends a message to the Erlang side.
 * The message will be written to the test servers log file,
 * but will otherwise be completly ignored.
 */

void
message(char* format, ...)
{
    va_list ap;
    char sbuf[1024];

    sbuf[0] = 'm';
    va_start(ap, format);
    vsprintf(sbuf+1, format, ap);
    va_end(ap);

    reply(sbuf, 1+strlen(sbuf+1));
}

/*
 * This function sends the given term to the Erlang side,
 * where it will be received as {term, Term}.
 *
 * If the given pointer is NULL (indicating an invalid term),
 * the result on the Erlang side will be the atom 'NULL'.
 *
 * After sending the term, this function frees the term by
 * calling erl_free_term().
 */

void
send_term(term)
    ETERM* term;		/* Term to be sent to Erlang side. */
{
    char encoded[64*1024];
    int n;

    if (term == NULL) {
	encoded[0] = 'N';
	n = 1;
    } else {
	encoded[0] = 't';
	n = 1 + erl_encode(term, encoded+1);
	erl_free_term(term);
    }
    reply(encoded, n);
}

#if 0

/* Seriously broken!!! */

void
send_bin_term(x_ei_buff* x)
{
    x_ei_buff x2;
    x_ei_new(&x2);
    x2.buff[x2.index++] = 't';
    x_ei_append(&x2, x);
    reply(x2.buff, x2.index);
    free(x2.buff);
}
#endif

/*
 * This function sends a raw buffer of data to the
 * Erlang side, where it will be received as {bytes, Bytes}.
 */

void
send_buffer(buf, size)
    char* buf;			/* Buffer with bytes to send to Erlang. */
    int size;			/* Size of data to send to Erlang. */
{
    char* send_buf;

    send_buf = (char *) malloc(size+1);
    send_buf[0] = 'b';
    memcpy(send_buf+1, buf, size);
    reply(send_buf, size+1);
    free(send_buf);
}

/***********************************************************************
 *
 * P r i v a t e   h e l p e r s
 *
 ***********************************************************************/

/*
 * Sends a packet back to Erlang.
 */

static void
reply(reply_buf, size)
     char* reply_buf;		/* Buffer with reply. */
     unsigned size;		/* Size of reply. */
{
    int n;			/* Temporary to hold size. */
    int i;			/* Loop counter. */
    char* buf;


    buf = (char *) malloc(size+HEADER_SIZE);
    memcpy(buf+HEADER_SIZE, reply_buf, size);

    /*
     * Fill the header starting with the least significant byte.
     */

    n = size;
    for (i = HEADER_SIZE-1; i >= 0; i--) {
	buf[i] = (char) n;	/* Store least significant byte. */
	n = n >> 8;
    }

    size += HEADER_SIZE;
/*
    fprintf(stderr, "\r\nReply size: %u\r\n",
	    (unsigned)buf[0] << 8 + (unsigned)buf[1]);

    for (i = 0; i < size; i++) {
	fprintf(stderr,"%u %c\r\n",buf[i],buf[i]);
    }

    fprintf(stderr, "\r\n");
*/
    write(fd_to_erl, buf, size);
    free(buf);
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

void
dump(buf, sz, max)
     unsigned char* buf;
     int sz;
     int max;
{
    int i, imax;
    char comma[5] = ",";
    
    if (!sz)
	return;
    if (sz > max)
	imax = max;
    else
	imax = sz;
    
    for (i=0; i<imax; i++) {
	if (i == imax-1) {
	    if (sz > max)
		strcpy(comma, ",...");
	    else
		comma[0] = 0;
	}
	if (isdigit(buf[i]))
	    fprintf(stderr, "%u%s", (int)(buf[i]), comma);
	else {
	    if (isalpha(buf[i])) {
		fprintf(stderr, "%c%s", buf[i], comma);
	    }
	    else
		fprintf(stderr, "%u%s", (int)(buf[i]), comma);
	}
    }
}

