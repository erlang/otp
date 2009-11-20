/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
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

/* Purpose: Access to elib memory statistics */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_driver.h"
#include "elib_stat.h"

#define MAP_BUF_SIZE    1000   /* Max map size */
#define HISTO_BUF_SIZE  100    /* Max histogram buckets */

static ErlDrvData mem_start(ErlDrvPort);
static int mem_init(void); 
static void mem_stop(ErlDrvData);
static void mem_command(ErlDrvData, char*, int);

const struct driver_entry mem_driver_entry = {
    mem_init,
    mem_start,
    mem_stop,
    mem_command,
    NULL,
    NULL,
    "mem_drv"
};

static int mem_init(void)
{
    return 0;
}

static ErlDrvData mem_start(ErlDrvPort port, char* buf)
{
    return (ErlDrvData)port;
}

static void mem_stop(ErlDrvData port)
{
}

void putint32(p, v)
byte* p; int v;
{
    p[0] = (v >> 24) & 0xff;
    p[1] = (v >> 16) & 0xff;
    p[2] = (v >> 8) & 0xff;
    p[3] = (v) & 0xff;
}

int getint16(p)
byte* p;
{
    return (p[0] << 8) | p[1];
}

/*
** Command:
**   m L1 L0  -> a heap map of length L1*256 + L0 is returned
**   s        -> X3 X2 X1 X0 Y3 Y2 Y1 Y0 Z3 Z2 Z1 Z0
**               X == Total heap size bytes
**               Y == Total free bytes
**               Z == Size of largest free block in bytes
**
**   h L1 L0 B0 -> Generate a logarithm historgram base B with L buckets
**   l L1 L0 S0 -> Generate a linear histogram with step S with L buckets
*/
unsigned char outbuf[HISTO_BUF_SIZE*2*4];

static void mem_command(ErlDrvData port, char* buf, int count)
{
    if ((count == 1) && buf[0] == 's') {
	struct elib_stat info;
	char v[3*4];

	elib_stat(&info);

	putint32(v, info.mem_total*4);
	putint32(v+4, info.mem_free*4);
	putint32(v+8, info.max_free*4);
	driver_output((ErlDrvPort)port, v, 12);
	return;
    }
    else if ((count == 3) && buf[0] == 'm') {
	char w[MAP_BUF_SIZE];
	int n = getint16(buf+1);

	if (n > MAP_BUF_SIZE)
	    n = MAP_BUF_SIZE;
	elib_heap_map(w, n);
	driver_output((ErlDrvPort)port, w, n);
	return;
    }
    else if ((count == 4) && (buf[0] == 'h' || buf[0] == 'l')) {
	unsigned long vf[HISTO_BUF_SIZE];
	unsigned long va[HISTO_BUF_SIZE];
	int n = getint16(buf+1);
	int base = (unsigned char) buf[3];

	if (n >= HISTO_BUF_SIZE)
	    n = HISTO_BUF_SIZE;
	if (buf[0] == 'l')
	    base = -base;
	if (elib_histo(vf, va, n, base) < 0) {
	    driver_failure((ErlDrvPort)port, -1);
	    return;
	}
	else {
	    char* p = outbuf;
	    int i;

	    for (i = 0; i < n; i++) {
		putint32(p, vf[i]);
		p += 4;
	    }
	    for (i = 0; i < n; i++) {
		putint32(p, va[i]);
		p += 4;
	    }
	    driver_output((ErlDrvPort)port, outbuf, n*8);
	}
	return;
    }
    driver_failure((ErlDrvPort)port, -1);
}
