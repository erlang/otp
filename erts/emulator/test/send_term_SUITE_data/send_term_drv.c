/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#include "erl_driver.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>

static ErlDrvPort erlang_port;
static ErlDrvData send_term_drv_start(ErlDrvPort port, char *command);
static void send_term_drv_stop(ErlDrvData drv_data);
static void send_term_drv_run(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);


static int make_ext_term_list(ErlDrvTermData *td, int bad);

#define FAIL_TERM(M, L) fail_term((M), (L), __LINE__)

static ErlDrvEntry send_term_drv_entry = { 
    NULL,
    send_term_drv_start,
    send_term_drv_stop,
    send_term_drv_run,
    NULL,
    NULL,
    "send_term_drv",
    NULL,
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,
};

DRIVER_INIT(send_term_drv)
{
    erlang_port = (ErlDrvPort)-1;
    return &send_term_drv_entry;
}

static ErlDrvData send_term_drv_start(ErlDrvPort port, char *buf)
{
    if (erlang_port != (ErlDrvPort)-1) {
	return ERL_DRV_ERROR_GENERAL;
    }
    
    erlang_port = port;
    return (ErlDrvData)port;
}

static void send_term_drv_stop(ErlDrvData drv_data)
{
}

static void output_term(ErlDrvTermData* msg, int len);
static void fail_term(ErlDrvTermData* msg, int len, int line);

static void send_term_drv_run(ErlDrvData port, char *buf, ErlDrvSizeT count)
{
    char buf7[1024];
    ErlDrvTermData spec[1024];
    ErlDrvTermData* msg = spec;
    ErlDrvBinary* bins[15];
    int bin_ix = 0;
    ErlDrvSInt64 s64[15];
    int s64_ix = 0;
    ErlDrvUInt64 u64[15];
    int u64_ix = 0;
    int i = 0;

    for (i=0; i<count; i++) switch (buf[i]) {
    case 0:
	msg[0] = ERL_DRV_NIL;
	msg += 1;
	break;

    case 1: 			/* Most term types inside a tuple. */
	{
	    double f = 3.1416;

	    msg[0] = ERL_DRV_ATOM;
	    msg[1] = driver_mk_atom("blurf");
	    msg[2] = ERL_DRV_INT;
	    msg[3] = (ErlDrvTermData) 42;
	    msg[4] = ERL_DRV_NIL;
	    msg[5] = ERL_DRV_INT;
	    msg[6] = (ErlDrvTermData) -42;
	    msg[7] = ERL_DRV_TUPLE;
	    msg[8] = (ErlDrvTermData) 0;
	    msg[9] = ERL_DRV_PORT;
	    msg[10] = driver_mk_port(erlang_port);
	    msg[11] = ERL_DRV_STRING_CONS;
	    msg[12] = (ErlDrvTermData) "abc";
	    msg[13] = (ErlDrvTermData) 3;
	    msg[14] = ERL_DRV_LIST;
	    msg[15] = (ErlDrvTermData) 3;
	    msg[16] = ERL_DRV_STRING;
	    msg[17] = (ErlDrvTermData) "kalle";
	    msg[18] = (ErlDrvTermData) 5;
	    msg[19] = ERL_DRV_FLOAT;
	    msg[20] = (ErlDrvTermData) &f;
	    msg[21] = ERL_DRV_PID;
	    msg[22] = driver_connected(erlang_port);
	    msg[23] = ERL_DRV_MAP;
	    msg[24] = (ErlDrvTermData) 0;
	    msg[25] = ERL_DRV_TUPLE;
	    msg[26] = (ErlDrvTermData) 8;
	    msg += 27;
	}
	break;

    case 2:			/* Deep stack */
	{
	    int i;
	    
	    for (i = 0; i < 400; i += 2) {
		msg[i] = ERL_DRV_INT;
		msg[i+1] = (ErlDrvTermData) (i / 2);
	    }
	    msg[i] = ERL_DRV_NIL;
	    msg[i+1] = ERL_DRV_LIST;
	    msg[i+2] = (ErlDrvTermData) 201;
	    msg += i+3;
	}
	break;

    case 3:			/* Binaries */
	{
	    ErlDrvBinary* bin;
	    int i;

	    bin = bins[bin_ix++] = driver_alloc_binary(256);
	    for (i = 0; i < 256; i++) {
		bin->orig_bytes[i] = i;
	    }
	    msg[0] = ERL_DRV_BINARY;
	    msg[1] = (ErlDrvTermData) bin;
	    msg[2] = (ErlDrvTermData) 256;
	    msg[3] = (ErlDrvTermData) 0;
	    msg[4] = ERL_DRV_BINARY;
	    msg[5] = (ErlDrvTermData) bin;
	    msg[6] = (ErlDrvTermData) 256-23-17;
	    msg[7] = (ErlDrvTermData) 23;
	    msg[8] = ERL_DRV_TUPLE;
	    msg[9] = (ErlDrvTermData) 2;
	    msg += 10;
	}
	break;

    case 4:			/* Pids */
	msg[0] = ERL_DRV_PID;
	msg[1] = driver_connected(erlang_port);
	msg[2] = ERL_DRV_PID;
	msg[3] = driver_caller(erlang_port);
	msg[4] = ERL_DRV_TUPLE;
	msg[5] = (ErlDrvTermData) 2;
	msg += 6;
	break;

    case 5:
	msg += make_ext_term_list(msg, 0);
	break;

    case 6:
	msg[0] = ERL_DRV_INT;
	msg[1] = ~((ErlDrvTermData) 0);
	msg[2] = ERL_DRV_UINT;
	msg[3] = ~((ErlDrvTermData) 0);
	msg[4] = ERL_DRV_TUPLE;
	msg[5] = (ErlDrvTermData) 2;
	msg += 6;
	break;

    case 7: {
	int len = 0;
	memset(buf7, 17, sizeof(buf7));
	/* empty heap binary */
	msg[len++] = ERL_DRV_BUF2BINARY;
	msg[len++] = (ErlDrvTermData) NULL; /* NULL is ok if size == 0 */
	msg[len++] = (ErlDrvTermData) 0;
	/* empty heap binary again */
	msg[len++] = ERL_DRV_BUF2BINARY;
	msg[len++] = (ErlDrvTermData) buf7; /* ptr is ok if size == 0 */
	msg[len++] = (ErlDrvTermData) 0;
	/* heap binary */
	msg[len++] = ERL_DRV_BUF2BINARY;
	msg[len++] = (ErlDrvTermData) buf7;
	msg[len++] = (ErlDrvTermData) 17;
	/* off heap binary */
	msg[len++] = ERL_DRV_BUF2BINARY;
	msg[len++] = (ErlDrvTermData) buf7;
	msg[len++] = (ErlDrvTermData) sizeof(buf7);

	msg[len++] = ERL_DRV_TUPLE;
	msg[len++] = (ErlDrvTermData) 4;

	msg += len;
	break;
    }

    case 8:
	msg[0] = ERL_DRV_NIL;
	msg += 1;
	break;

    case 9:
	msg[0] = ERL_DRV_ATOM;
	msg[1] = (ErlDrvTermData) driver_mk_atom("");
	msg += 2;
	break;

    case 10:
	msg[0] = ERL_DRV_ATOM;
	msg[1] = (ErlDrvTermData) driver_mk_atom("an_atom");
	msg += 2;
	break;

    case 11:
	msg[0] = ERL_DRV_INT;
	msg[1] = (ErlDrvTermData) -4711;
	msg += 2;
	break;
	  
    case 12:  
	msg[0] = ERL_DRV_UINT;
	msg[1] = (ErlDrvTermData) 4711;
	msg += 2;
	  
	break;
    case 13:  
	msg[0] = ERL_DRV_PORT;
	msg[1] = driver_mk_port(erlang_port);
	msg += 2;
	break;

    case 14: {
	ErlDrvBinary *dbin = bins[bin_ix++] = driver_alloc_binary(0);
	msg[0] = ERL_DRV_BINARY;
	msg[1] = (ErlDrvTermData) dbin;
	msg[2] = (ErlDrvTermData) 0;
	msg[3] = (ErlDrvTermData) 0;
	msg += 4;
	break;
	}

    case 15: {
	static const char buf[] = "hejsan";
	ErlDrvBinary *dbin = bins[bin_ix++] = driver_alloc_binary(sizeof(buf)-1);
	if (dbin)
	    memcpy((void *) dbin->orig_bytes, (void *) buf, sizeof(buf)-1);
	msg[0] = ERL_DRV_BINARY;
	msg[1] = (ErlDrvTermData) dbin;
	msg[2] = (ErlDrvTermData) (dbin ? sizeof(buf)-1 : 0);
	msg[3] = (ErlDrvTermData) 0;
	msg += 4;
	break;
	}

    case 16:
	msg[0] = ERL_DRV_BUF2BINARY;
	msg[1] = (ErlDrvTermData) NULL;
	msg[2] = (ErlDrvTermData) 0;
	msg += 3;
	break;
	
    case 17: {
	static const char buf[] = "";
	msg[0] = ERL_DRV_BUF2BINARY;
	msg[1] = (ErlDrvTermData) buf;
	msg[2] = (ErlDrvTermData) sizeof(buf)-1;
	msg += 3;
	break;
	}

    case 18: {
	static const char buf[] = "hoppsan";
	msg[0] = ERL_DRV_BUF2BINARY;
	msg[1] = (ErlDrvTermData) buf;
	msg[2] = (ErlDrvTermData) sizeof(buf)-1;
	msg += 3;
	break;
    }

    case 19: 
	msg[0] = ERL_DRV_STRING;
	msg[1] = (ErlDrvTermData) buf;
	msg[2] = (ErlDrvTermData) 0;
	msg += 3;
	break;

    case 20: {
	static const char buf[] = "";
	msg[0] = ERL_DRV_STRING;
	msg[1] = (ErlDrvTermData) buf;
	msg[2] = (ErlDrvTermData) sizeof(buf)-1;
	msg += 3;
	break;
    }
	
    case 21: {
	static const char buf[] = "hippsan";
	msg[0] = ERL_DRV_STRING;
	msg[1] = (ErlDrvTermData) buf;
	msg[2] = (ErlDrvTermData) sizeof(buf)-1;
	msg += 3;
	break;
	}

    case 22:
	msg[0] = ERL_DRV_TUPLE;
	msg[1] = (ErlDrvTermData) 0;
	msg += 2;
	break;

    case 23:
	msg[0] = ERL_DRV_NIL;
	msg[1] = ERL_DRV_LIST;
	msg[2] = (ErlDrvTermData) 1;
	msg += 3;
	break;
	
    case 24:
	msg[0] = ERL_DRV_PID;
	msg[1] = driver_connected(erlang_port);
	msg += 2;
	break;
	
    case 25:
	msg[0] = ERL_DRV_NIL;
	msg[1] = ERL_DRV_STRING_CONS;
	msg[2] = (ErlDrvTermData) "";
	msg[3] = (ErlDrvTermData) 0;
	msg += 4;
	break;

    case 26: {
	static double my_float = 0.0;
	msg[0] = ERL_DRV_FLOAT;
	msg[1] = (ErlDrvTermData) &my_float; 
	msg += 2;
	break;
    }

    case 27: {
	static char buf[] = {131, 106}; /* [] */
	msg[0] = ERL_DRV_EXT2TERM;
	msg[1] = (ErlDrvTermData) buf;
	msg[2] = (ErlDrvTermData) sizeof(buf);
	msg += 3;
	break;
    }

    case 28: {
	ErlDrvUInt64* x = &u64[u64_ix++];
	*x = ~((ErlDrvUInt64) 0);
	msg[0] = ERL_DRV_UINT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 29: {
	ErlDrvUInt64* x = &u64[u64_ix++];
	*x = ((ErlDrvUInt64) 4711) << 32;
	msg[0] = ERL_DRV_UINT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 30: {
	ErlDrvUInt64* x = &u64[u64_ix++];
	*x = 4711;
	msg[0] = ERL_DRV_UINT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 31: {
	ErlDrvUInt64* x = &u64[u64_ix++];
	*x = 0;
	msg[0] = ERL_DRV_UINT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 32: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = ((((ErlDrvUInt64) 0x7fffffff) << 32) | ((ErlDrvUInt64) 0xffffffff));
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 33: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = (ErlDrvSInt64) (((ErlDrvUInt64) 4711) << 32);
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 34: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = 4711;
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 35: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = 0;
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 36: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = -1;
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 37: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = -4711;
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 38: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = ((ErlDrvSInt64) ((ErlDrvUInt64) 4711) << 32)*-1;
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 39: {
	ErlDrvSInt64* x = &s64[s64_ix++];
	*x = ((ErlDrvSInt64) 1) << 63;
	msg[0] = ERL_DRV_INT64;
	msg[1] = (ErlDrvTermData) x;
	msg += 2;
	break;
    }

    case 40: {
	msg[0] = ERL_DRV_MAP;
	msg[1] = (ErlDrvTermData) 0;
	msg += 2;
	break;
    }

    case 41:    /* Most term types inside a map */
    case 42: {
	double f = 3.1416;

	if (buf[i] == 41) {
	    *msg++ = ERL_DRV_ATOM;
	    *msg++ = driver_mk_atom("blurf");
	}
	*msg++ = ERL_DRV_INT;
	*msg++ = (ErlDrvTermData)42;
	*msg++ = ERL_DRV_NIL;
	*msg++ = ERL_DRV_INT;
	*msg++ = (ErlDrvTermData)-42;
	*msg++ = ERL_DRV_TUPLE;
	*msg++ = (ErlDrvTermData)0;
	*msg++ = ERL_DRV_PORT;
	*msg++ = driver_mk_port(erlang_port);
	*msg++ = ERL_DRV_STRING_CONS;
	*msg++ = (ErlDrvTermData)"abc";
	*msg++ = (ErlDrvTermData)3;
	*msg++ = ERL_DRV_LIST;
	*msg++ = (ErlDrvTermData)3;
	*msg++ = ERL_DRV_STRING;
	*msg++ = (ErlDrvTermData)"kalle";
	*msg++ = (ErlDrvTermData)5;
	*msg++ = ERL_DRV_FLOAT;
	*msg++ = (ErlDrvTermData)&f;
	*msg++ = ERL_DRV_PID;
	*msg++ = driver_connected(erlang_port);
	*msg++ = ERL_DRV_MAP;
	*msg++ = (ErlDrvTermData)0;
	if (buf[i] == 42) {
	    *msg++ = ERL_DRV_ATOM;
	    *msg++ = driver_mk_atom("blurf");
	}
	*msg++ = ERL_DRV_MAP;
	*msg++ = (ErlDrvTermData)4;
	break;
    }

    case 127:			/* Error cases */
	{
	    long refc;
	    ErlDrvBinary* bin = bins[bin_ix++] = driver_alloc_binary(256);

	    FAIL_TERM(msg, 0);

	    msg[0] = ERL_DRV_LIST;
	    msg[1] = (ErlDrvTermData) 0;
	    FAIL_TERM(msg, 2);

	    /* Not an atom */
	    msg[0] = ERL_DRV_ATOM;
	    msg[1] = (ErlDrvTermData) driver_connected(erlang_port);
	    FAIL_TERM(msg, 2);
	    msg[0] = ERL_DRV_ATOM;
	    msg[1] = driver_term_nil;
	    FAIL_TERM(msg, 2);

	    /* Not a pid */
	    msg[0] = ERL_DRV_PID;
	    msg[1] = (ErlDrvTermData) driver_mk_atom("blurf");
	    FAIL_TERM(msg, 2);
	    msg[0] = ERL_DRV_PID;
	    msg[1] = driver_term_nil;
	    FAIL_TERM(msg, 2);

	    /* Not a port */
	    msg[0] = ERL_DRV_PORT;
	    msg[1] = (ErlDrvTermData) driver_mk_atom("blurf");
	    FAIL_TERM(msg, 2);
	    msg[0] = ERL_DRV_PORT;
	    msg[1] = driver_term_nil;
	    FAIL_TERM(msg, 2);

	    /* Missing parameter on stack */
	    msg[0] = ERL_DRV_STRING_CONS;
	    msg[1] = (ErlDrvTermData) "abc";
	    msg[2] = (ErlDrvTermData) 3;
	    FAIL_TERM(msg, 3);

	    /*
	     * The first binary reference is correct, the second is incorrect.
	     * There should not be any "binary leak".
	     */
	    msg[0] = ERL_DRV_BINARY;
	    msg[1] = (ErlDrvTermData) bin;
	    msg[2] = (ErlDrvTermData) 256;
	    msg[3] = (ErlDrvTermData) 0;
	    msg[4] = ERL_DRV_BINARY;
	    msg[5] = (ErlDrvTermData) bin;
	    msg[6] = (ErlDrvTermData) 257;
	    msg[7] = (ErlDrvTermData) 0;
	    msg[8] = ERL_DRV_TUPLE;
	    msg[9] = (ErlDrvTermData) 2;
	    FAIL_TERM(msg, 10);

	    msg[0] = ERL_DRV_BINARY;
	    msg[1] = (ErlDrvTermData) bin;
	    msg[2] = (ErlDrvTermData) 256;
	    msg[3] = (ErlDrvTermData) 0;
	    msg[4] = ERL_DRV_BINARY;
	    msg[5] = (ErlDrvTermData) bin;
	    msg[6] = (ErlDrvTermData) 256;
	    msg[7] = (ErlDrvTermData) 50;
	    msg[8] = ERL_DRV_TUPLE;
	    msg[9] = (ErlDrvTermData) 2;
	    FAIL_TERM(msg, 10);
	    
	    /*
	     * We have succefully built two binaries. We expect the ref count
	     * to be 1 (SMP) or 3 (non-SMP).
	     */
	    refc = driver_binary_get_refc(bin);
	    if (refc > 3) {
		char sbuf[128];
		sprintf(sbuf, "bad_refc:%ld", refc);
		driver_failure_atom(erlang_port, sbuf);
	    }
	    driver_free_binary(bin);


	    FAIL_TERM(msg, make_ext_term_list(msg, 1));


	    /*
	     * Check that we fail for missing args.
	     *
	     * We setup valid terms but pass a too small size. We
	     * want valid terms since we want to verify that the
	     * failure really is due to the small size. 
	     */
	    msg[0] = ERL_DRV_ATOM;
	    msg[1] = (ErlDrvTermData) driver_mk_atom("an_atom");
	    FAIL_TERM(msg, 1);

	    msg[0] = ERL_DRV_INT;
	    msg[1] = (ErlDrvTermData) -4711;
	    FAIL_TERM(msg, 1);
	    
	    msg[0] = ERL_DRV_UINT;
	    msg[1] = (ErlDrvTermData) 4711;
	    FAIL_TERM(msg, 1);
	    
	    msg[0] = ERL_DRV_PORT;
	    msg[1] = driver_mk_port(erlang_port);
	    FAIL_TERM(msg, 1);
	    
	    {
		char buf[] = "hejsan";
		ErlDrvBinary *dbin = driver_alloc_binary(sizeof(buf)-1);
		if (!dbin)
		    driver_failure_posix(erlang_port, ENOMEM);
		else {
		    memcpy((void *) dbin->orig_bytes, (void *) buf, sizeof(buf)-1);
		    msg[0] = ERL_DRV_BINARY;
		    msg[1] = (ErlDrvTermData) dbin;
		    msg[2] = (ErlDrvTermData) sizeof(buf)-1;
		    msg[3] = (ErlDrvTermData) 0;
		    FAIL_TERM(msg, 1);
		    FAIL_TERM(msg, 2);
		    FAIL_TERM(msg, 3);
		    driver_free_binary(dbin);
		}
	    }

	    {
		char buf[] = "hoppsan";
		msg[0] = ERL_DRV_BUF2BINARY;
		msg[1] = (ErlDrvTermData) buf;
		msg[2] = (ErlDrvTermData) sizeof(buf)-1;
		FAIL_TERM(msg, 1);
		FAIL_TERM(msg, 2);
	    }

	    {
		char buf[] = "hippsan";
		msg[0] = ERL_DRV_STRING;
		msg[1] = (ErlDrvTermData) buf;
		msg[2] = (ErlDrvTermData) sizeof(buf)-1;
		FAIL_TERM(msg, 1);
		FAIL_TERM(msg, 2);
	    }
	    
	    msg[0] = ERL_DRV_TUPLE;
	    msg[1] = (ErlDrvTermData) 0;
	    FAIL_TERM(msg, 1);
	    
	    msg[0] = ERL_DRV_NIL;
	    msg[1] = ERL_DRV_LIST;
	    msg[2] = (ErlDrvTermData) 1;
	    FAIL_TERM(msg, 2);
	    
	    msg[0] = ERL_DRV_PID;
	    msg[1] = driver_connected(erlang_port);
	    FAIL_TERM(msg, 1);
	    
	    msg[0] = ERL_DRV_NIL;
	    msg[1] = ERL_DRV_STRING_CONS;
	    msg[2] = (ErlDrvTermData) "";
	    msg[3] = (ErlDrvTermData) 0;
	    FAIL_TERM(msg, 2);
	    FAIL_TERM(msg, 3);

	    {
		double my_float = 0.0;
		msg[0] = ERL_DRV_FLOAT;
		msg[1] = (ErlDrvTermData) &my_float; 
		FAIL_TERM(msg, 1);
	    }

	    {
		char buf[] = {131, 106}; /* [] */
		msg[0] = ERL_DRV_EXT2TERM;
		msg[1] = (ErlDrvTermData) buf;
		msg[2] = (ErlDrvTermData) sizeof(buf);
		FAIL_TERM(msg, 1);
		FAIL_TERM(msg, 2);
	    }

	    msg[0] = ERL_DRV_MAP;
	    msg[1] = (ErlDrvTermData) 0;
	    FAIL_TERM(msg, 1);

	    /* map with duplicate key */
	    msg[0] = ERL_DRV_ATOM;
	    msg[1] = driver_mk_atom("key");
	    msg[2] = ERL_DRV_NIL;
	    msg[3] = ERL_DRV_ATOM;
	    msg[4] = driver_mk_atom("key");
	    msg[5] = ERL_DRV_INT;
	    msg[6] = (ErlDrvTermData) -4711;
	    msg[7] = ERL_DRV_MAP;
	    msg[8] = 2;
	    FAIL_TERM(msg, 9);

	    /* Signal end of test case */
	    msg[0] = ERL_DRV_NIL;
	    erl_drv_output_term(driver_mk_port(erlang_port), msg, 1);
	    return;
	}
	break;

    default:
	driver_failure_atom(erlang_port, "bad_request");
	break;
    }
    if (count > 1) {
	*msg++ = ERL_DRV_NIL;
	*msg++ = ERL_DRV_LIST;
	*msg++ = count + 1;
    }
    output_term(spec, msg-spec);
    if ((bin_ix|s64_ix|u64_ix) > 15) abort();
    while (bin_ix) {
	driver_free_binary(bins[--bin_ix]);
    }
}

static void output_term(ErlDrvTermData* msg, int len)
{
    if (erl_drv_output_term(driver_mk_port(erlang_port), msg, len) <= 0) {
	driver_failure_atom(erlang_port, "erl_drv_output_term_failed");
    }
}

static void fail_term(ErlDrvTermData* msg, int len, int line)
{
    int status = erl_drv_output_term(driver_mk_port(erlang_port), msg, len);

    if (status == 1) {
	char buf[1024];
	sprintf(buf, "%s:%d: unexpected success", __FILE__, line);
	driver_failure_atom(erlang_port, buf);
    } else if (status == 0) {
	char buf[1024];
	sprintf(buf, "%s:%d: unexpected port error", __FILE__, line);
	driver_failure_atom(erlang_port, buf);
    }
}

#include "ext_terms.h"

/*
 * <<131,103,100,0,12,97,110,111,100,101,64,103,111,114,98,97,103,0,0,0,36,0,0,0,0,1>>
 * is a valid pid: <0.36.0>
 *
 * We replace the nodename tag (atom tag: 100) with a pid tag (103) to get an
 * invalid pid.
 */
static unsigned char bad_ext_term[] = {
    131,103,103,0,12,97,110,111,100,101,64,103,111,114,98,97,103,0,0,0,36,0,0,0,0,1
    /*       ^
     *       |
     * The bad tag.
     */
};

static int make_ext_term_list(ErlDrvTermData *td, int bad)
{
    int tdix = 0;
    int i;
    for (i = 0; i < NO_OF_EXT_TERMS; i++) {
	td[tdix++] = ERL_DRV_EXT2TERM;
	td[tdix++] = (ErlDrvTermData) &ext_terms[i].ext[0];
	td[tdix++] = (ErlDrvTermData) ext_terms[i].ext_size;
	td[tdix++] = ERL_DRV_EXT2TERM;
	td[tdix++] = (ErlDrvTermData) &ext_terms[i].cext[0];
	td[tdix++] = (ErlDrvTermData) ext_terms[i].cext_size;
	td[tdix++] = ERL_DRV_TUPLE;
	td[tdix++] = (ErlDrvTermData) 2;
    }
    if (bad) { /* Include a bad ext term */
	td[tdix++] = ERL_DRV_EXT2TERM;
	td[tdix++] = (ErlDrvTermData) &bad_ext_term[0];
	td[tdix++] = (ErlDrvTermData) sizeof(bad_ext_term);
    }
    td[tdix++] = ERL_DRV_NIL;
    td[tdix++] = ERL_DRV_LIST;
    td[tdix++] = (ErlDrvTermData) (NO_OF_EXT_TERMS + (bad ? 2 : 1));
    return tdix;
}
