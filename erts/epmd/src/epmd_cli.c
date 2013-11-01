/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2013. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "epmd.h"     /* Renamed from 'epmd_r4.h' */
#include "epmd_int.h"
#include "erl_printf.h" /* erts_snprintf */

/* forward declarations */

static int conn_to_epmd(EpmdVars*);
static int read_fill(int,char*,int);
static void epmd_call3(EpmdVars *g, int what, int fd, char* buf, int size);


void kill_epmd(EpmdVars *g)
{
    char buf[5];
    int fd, rval, sz;
    const char *p;

    fd = conn_to_epmd(g);
    put_int16(1,buf);
    buf[2] = g->version >=3 ? EPMD_KILL3_REQ : EPMD_KILL_REQ;
    if (write(fd, buf, 3) != 3) {
	printf("epmd: Can't write to epmd\n");
	epmd_cleanup_exit(g,1);
    }
    sz = g->version >= 3 ? 4 : 2;
    p  = sz == 2 ? buf : buf+2;

    if ((rval = read_fill(fd,buf,sz)) == sz) {
	if (strncmp(p, "OK", 2) == 0) {
	    printf("Killed\n");
	} else {
	    printf("Killing not allowed - living nodes in database.\n");
	}
	epmd_cleanup_exit(g,0);
    } else if (rval < 0) {
	printf("epmd: failed to read answer from local epmd\n");
	epmd_cleanup_exit(g,1);
    } else { 			/* rval is now 0 or 1 */
	buf[rval] = '\0';
	printf("epmd: local epmd responded with <%s>\n", buf);
	epmd_cleanup_exit(g,1);
    }
}

void stop_cli(EpmdVars *g, char *name)
{
    char buf[1024];
    int fd, rval, bsize;
    const char *begin = buf+2;
    char *p = begin;

    bsize = strlen(name);
    if (bsize > 1000) {
	printf("epmd: Name too long!");
	epmd_cleanup_exit(g, 1);
    }

    fd = conn_to_epmd(g);
    *p++ = g->version >=3 ? EPMD_STOP3_REQ : EPMD_STOP_REQ;
    strcpy(p, name);
    p += bsize+1;
    bsize = p - begin;
    put_int16(bsize, buf);
    bsize = p - buf;

    if (write(fd, buf, bsize) != bsize) {
	printf("epmd: Can't write to epmd\n");
	epmd_cleanup_exit(g,1);
    }

    bsize = g->version >= 3 ? 9 : 7;
    p = bsize == 9 ? buf : buf+2;

    if ((rval = read_fill(fd,p,bsize)) == bsize) {
	*(p+bsize) = '\0';
	printf("%s\n", begin);
	epmd_cleanup_exit(g,0);
    } else if (rval < 0) {
	printf("epmd: failed to read answer from local epmd\n");
	epmd_cleanup_exit(g,1);
    } else { 			/* rval is now 0 or 1 */
	*(p+rval) = '\0';
	printf("epmd: local epmd responded with <%s>\n", begin);
	epmd_cleanup_exit(g,1);
    }
}

/* what == EPMD_NAMES_REQ || EPMD_DUMP_REQ */
/* what == EPMD_NAMES3_REQ || EPMD_DUMP3_REQ */

void epmd_call(EpmdVars *g,int what)
{
    char buf[OUTBUF_SIZE];
    int rval,fd,i,j;
    int sz;

    fd = conn_to_epmd(g);
    put_int16(1,buf);
    buf[2] = what;

    if (write(fd, buf, 3) != 3) {
	printf("epmd: Can't write to epmd\n");
	epmd_cleanup_exit(g,1);
    }

    switch (what) {
        case EPMD_NAMES3_REQ:
        case EPMD_DUMP3_REQ:
            epmd_call3(g, what, fd, buf, sizeof(buf));
            return;
        default:
            break;
    }

    if (read(fd,(char *)&i,4) != 4) {
	if (!g->silent)
	    printf("epmd: no response from local epmd\n");
	epmd_cleanup_exit(g,1);
    }
    j = ntohl(i);
    if (!g->silent) {
	rval = erts_snprintf(buf, OUTBUF_SIZE,
			     "epmd: up and running on port %d with data:\n", j);
	write(1, buf, rval);
    }
    while(1) {
	if ((rval = read(fd,buf,OUTBUF_SIZE)) <= 0)  {
	    close(fd);
	    epmd_cleanup_exit(g,0);
	}
	if (!g->silent)
	    write(1, buf, rval); /* Potentially UTF-8 encoded */
    }
}

void epmd_call3(EpmdVars *g, int what, int fd, char* buf, int size)
{
    char *pbuf = buf, *p = buf;
    int ret = 0,i,sz;

    if (read(fd,(char *)&i,2) != 2)
        goto cleanup;
    sz = htons(i);
    if (sz > sizeof(buf))
        p = pbuf = malloc(sz);

    if (read(fd,pbuf,sz) != sz)
        goto error_cleanup;

    i = ntohl(*((int*)p)); p += 4;

    if (!g->silent) {
	i = erts_snprintf(buf, OUTBUF_SIZE,
	        "epmd: up and running on port %d with data:\n", i);
	write(1, buf, i);

        write(1, p, sz); /* Potentially UTF-8 encoded */
    }
    goto cleanup;

error_cleanup:
    ret = 1;
    if (!g->silent)
        printf("epmd: no response from local epmd\n");
cleanup:
    close(fd);
    if (pbuf != buf)
        free(pbuf);
    epmd_cleanup_exit(g,ret);
}



static int conn_to_epmd(EpmdVars *g)
{
    struct EPMD_SOCKADDR_IN address;
    int connect_sock;

    connect_sock = socket(FAMILY, SOCK_STREAM, 0);
    if (connect_sock<0)
	goto error;

    { /* store port number in unsigned short */
      unsigned short sport = g->port;
      SET_ADDR(address, EPMD_ADDR_LOOPBACK, sport);
    }

    if (connect(connect_sock, (struct sockaddr*)&address, sizeof address) < 0)
	goto error;
    return connect_sock;

 error:
    if (!g->silent) {
	fprintf(stderr, "epmd: Cannot connect to local epmd\n");
    }
    epmd_cleanup_exit(g,1);
    return -1;
}

/* Fill buffer, return buffer length, 0 for EOF, < 0 for error. */
static int read_fill(int fd,char *buf,int len)
{
    int i;
    int got = 0;

    do {
	if ((i = read(fd, buf+got, len-got)) <= 0)
	    return (i);
	got += i;
    } while (got < len);
    return (len);
}
