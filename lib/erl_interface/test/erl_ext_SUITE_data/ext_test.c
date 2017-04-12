/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
 *

 */
/*
 * Author: Rickard Green
 * Modified: Björn-Egil Dahlberg
 * -	compare_tuple
 * -	compare_string
 * -	compare_list
 * -	compare_list and string
 */

#include "runner.h"
#include "erl_interface.h"
#include <stdio.h>
#include <string.h>

typedef unsigned int uint;

#define MAX_NC_EXT_SIZE 100

static unsigned char *
write_pid(unsigned char *buf, char *node, uint cre, uint ser, uint num);
static unsigned char *
write_port(unsigned char *buf, char *node, uint cre, uint id);
static unsigned char *
write_ref(unsigned char *buf, char *node, uint cre, uint id[], uint no_ids);
static void
test_compare_ext(char *test_desc,
		 unsigned char *ext1,
		 unsigned char *end_ext1,
		 unsigned char *ext2,
		 unsigned char *end_ext2,
		 int exp_res);

/*
 * Test erl_compare_ext with tuples
 */
TESTCASE(compare_tuple) {
    // erlang:term_to_binary ({'b'})
    unsigned char term1[] = { 131, 104, 1, 100, 0, 1, 98 };
    // erlang:term_to_binary ({'a', 'a'})
    unsigned char term2[] = { 131, 104, 2, 100, 0, 1, 97, 100, 0, 1, 97 };
    unsigned char *start_a, *start_b, *end_a, *end_b;
    
    erl_init(NULL, 0);
    start_a = term1;
    start_b = term2;
    end_a   = term1 + 7;
    end_b   = term2 + 11;

    test_compare_ext("tuples", start_a, end_a, start_b, end_b, -1);
    
    report(1);
}

/*
 * Test erl_compare_ext with lists
 */

TESTCASE(compare_list) {
    unsigned char *start_a, *start_b, *end_a, *end_b;
    // erlang:term_to_binary([a,b,[],3412])
    unsigned char term1[] = {131,108,0,0,0,4,100,0,1,97,100,0,1,98,106,98,0,0,13,84,106};
    // erlang:term_to_binary([34,{a,n},a,erlang])
    unsigned char term2[] = {131,108,0,0,0,4,97,34,104,2,100,0,1,97,100,0,1,110,100,0,1,97,100,0,6,101,114,108,97,110,103,106};

    // erlang:term_to_binary([0])
    unsigned char term3[] = {131,107,0,1,0};
    // erlang:term_to_binary([0, 1000])
    unsigned char term4[] = {131,108,0,0,0,2,97,0,98,0,0,3,232,106};

    erl_init(NULL, 0);
    start_a = term1;
    start_b = term2;
    end_a   = term1 + 21;
    end_b   = term2 + 32;
    
    test_compare_ext("lists", start_a, end_a, start_b, end_b, 1);

    start_a = term3;
    start_b = term4;
    end_a   = term3 + sizeof(term3);
    end_b   = term4 + sizeof(term4);

    test_compare_ext("lists1", start_a, end_a, start_b, end_b, -1);

    report(1);
}

/*
 * Test erl_compare_ext with strings
 */

TESTCASE(compare_string) {
    unsigned char *start_a, *start_b, *end_a, *end_b;
    // erlang:term_to_binary("hej")
    unsigned char term1[] = {131,107,0,3,104,101,106};
    // erlang:term_to_binary("erlang")
    unsigned char term2[] = {131,107,0,6,101,114,108,97,110,103};

    erl_init(NULL, 0);
    start_a = term1;
    start_b = term2;
    end_a   = term1 + 7;
    end_b   = term2 + 10;
    
    test_compare_ext("strings", start_a, end_a, start_b, end_b, 1);
    
    report(1);
}

/*
 * Test erl_compare_ext with lists and strings
 */

TESTCASE(compare_list_string) {
    unsigned char *start_a, *start_b, *end_a, *end_b;
    // erlang:term_to_binary("hej")
    unsigned char term1[] = {131,107,0,3,104,101,106};
    // erlang:term_to_binary([a,b,[],3412])
    unsigned char term2[] = {131,108,0,0,0,4,100,0,1,97,100,0,1,98,106,98,0,0,13,84,106};

    erl_init(NULL, 0);
    start_a = term1;
    start_b = term2;
    end_a   = term1 + 7;
    end_b   = term2 + 21;
    
    test_compare_ext("strings", start_a, end_a, start_b, end_b, -1);
    
    report(1);
}



/*
 * Test erl_compare_ext with node containers
 */
TESTCASE(compare_nc_ext)
{
    int res;
    unsigned char buf_a[MAX_NC_EXT_SIZE], buf_b[MAX_NC_EXT_SIZE];
    unsigned char *end_a, *end_b;
    uint id[3];

    erl_init(NULL, 0);


    /*
     * Test pids ----------------------------------------------------
     *
     * Significance (most -> least):
     *   nodename, creation, serial, number, nodename, creation
     *
     */
	
    end_a = write_pid(buf_a, "b@b", 2, 4711, 1);

    end_b = write_pid(buf_b, "a@b", 1, 4710, 2);
    test_compare_ext("pid test 1", buf_a, end_a, buf_b, end_b, -1);

    end_b = write_pid(buf_b, "a@b", 1, 4712, 1);
    test_compare_ext("pid test 2", buf_a, end_a, buf_b, end_b, -1);

    end_b = write_pid(buf_b, "c@b", 1, 4711, 1);
    test_compare_ext("pid test 3", buf_a, end_a, buf_b, end_b, -1);

    end_b = write_pid(buf_b, "b@b", 3, 4711, 1);
    test_compare_ext("pid test 4", buf_a, end_a, buf_b, end_b, -1);

    end_b = write_pid(buf_b, "b@b", 2, 4711, 1);
    test_compare_ext("pid test 5", buf_a, end_a, buf_b, end_b, 0);


    /*
     * Test ports ---------------------------------------------------
     *
     * Significance (most -> least):
     *   nodename, creation, number 
     *
     * OBS: Comparison between ports has changed in R9. This
     *      since it wasn't stable in R8 (and eariler releases).
     *      Significance used to be: dist_slot, number,
     *      creation.
     */

    end_a = write_port(buf_a, "b@b", 2, 4711),

    end_b = write_port(buf_b, "c@b", 1, 4710);
    test_compare_ext("port test 1", buf_a, end_a, buf_b, end_b, -1);

    end_b = write_port(buf_b, "b@b", 3, 4710);
    test_compare_ext("port test 2", buf_a, end_a, buf_b, end_b, -1);

    end_b = write_port(buf_b, "b@b", 2, 4712);
    test_compare_ext("port test 3", buf_a, end_a, buf_b, end_b, -1);

    end_b = write_port(buf_b, "b@b", 2, 4711);
    test_compare_ext("port test 4", buf_a, end_a, buf_b, end_b, 0);

    /*
     * Test refs ----------------------------------------------------
     * Significance (most -> least):
     * nodename, creation, (number high, number mid), number low, 
     *
     * OBS: Comparison between refs has changed in R9. This
     *      since it wasn't stable in R8 (and eariler releases).
     *      Significance used to be: dist_slot, number,
     *      creation.
     *
     */

    /* Long & Long */

    id[0] = 4711; id[1] = 4711, id[2] = 4711;
    end_a = write_ref(buf_a, "b@b", 2, id, 3);


    id[0] = 4710; id[1] = 4710; id[2] = 4710;
    end_b = write_ref(buf_b, "c@b", 1, id, 3);
    test_compare_ext("ref test 1", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4710; id[1] = 4710; id[2] = 4710;
    end_b = write_ref(buf_b, "b@b", 3, id, 3);
    test_compare_ext("ref test 2", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4710; id[1] = 4710; id[2] = 4712;
    end_b = write_ref(buf_b, "b@b", 2, id, 3);
    test_compare_ext("ref test 3", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4710; id[1] = 4712; id[2] = 4711;
    end_b = write_ref(buf_b, "b@b", 2, id, 3);
    test_compare_ext("ref test 4", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4712; id[1] = 4711; id[2] = 4711;
    end_b = write_ref(buf_b, "b@b", 2, id, 3);
    test_compare_ext("ref test 5", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4711; id[1] = 4711; id[2] = 4711;
    end_b = write_ref(buf_b, "b@b", 2, id, 3);
    test_compare_ext("ref test 6", buf_a, end_a, buf_b, end_b, 0);

    /* Long & Short */
    id[0] = 4711; id[1] = 0, id[2] = 0;
    end_a = write_ref(buf_a, "b@b", 2, id, 3);


    id[0] = 4710;
    end_b = write_ref(buf_b, "c@b", 1, id, 1);
    test_compare_ext("ref test 7", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4710;
    end_b = write_ref(buf_b, "b@b", 3, id, 1);
    test_compare_ext("ref test 8", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4712;
    end_b = write_ref(buf_b, "b@b", 2, id, 1);
    test_compare_ext("ref test 9", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4711;
    end_b = write_ref(buf_b, "b@b", 2, id, 1);
    test_compare_ext("ref test 10", buf_a, end_a, buf_b, end_b, 0);

    /* Short & Long */
    id[0] = 4711;
    end_a = write_ref(buf_a, "b@b", 2, id, 1);


    id[0] = 4710; id[1] = 0, id[2] = 0;
    end_b = write_ref(buf_b, "c@b", 1, id, 3);
    test_compare_ext("ref test 11", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4710; id[1] = 0, id[2] = 0;
    end_b = write_ref(buf_b, "b@b", 3, id, 3);
    test_compare_ext("ref test 12", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4712; id[1] = 0, id[2] = 0;
    end_b = write_ref(buf_b, "b@b", 2, id, 3);
    test_compare_ext("ref test 13", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4711; id[1] = 0, id[2] = 0;
    end_b = write_ref(buf_b, "b@b", 2, id, 3);
    test_compare_ext("ref test 14", buf_a, end_a, buf_b, end_b, 0);

    /* Short & Short */
    id[0] = 4711;
    end_a = write_ref(buf_a, "b@b", 2, id, 1);


    id[0] = 4710;
    end_b = write_ref(buf_b, "c@b", 1, id, 1);
    test_compare_ext("ref test 15", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4710;
    end_b = write_ref(buf_b, "b@b", 3, id, 1);
    test_compare_ext("ref test 16", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4712;
    end_b = write_ref(buf_b, "b@b", 2, id, 1);
    test_compare_ext("ref test 17", buf_a, end_a, buf_b, end_b, -1);

    id[0] = 4711;
    end_b = write_ref(buf_b, "b@b", 2, id, 1);
    test_compare_ext("ref test 18", buf_a, end_a, buf_b, end_b, 0);

    report(1);
}

static void
test_compare_ext(char *test_desc,
		 unsigned char *ext1,
		 unsigned char *end_ext1,
		 unsigned char *ext2,
		 unsigned char *end_ext2,
		 int exp_res)
{
    int er, ar;
    unsigned char *e1, *e2;
    int reversed_args;
    char ext_str[MAX_NC_EXT_SIZE*4 + 1];
    char *es;

    message("*** %s ***", test_desc);
    message("  erl_compare_ext() arguments:", test_desc);

    es = &ext_str[0];

    e1 = ext1;
    while (e1 < end_ext1)
	es += sprintf(es, "%d,", *(e1++));
    *(--es) = '\0';
    message("    e1 = <<%s>>", ext_str);


    es = &ext_str[0];

    e2 = ext2;
    while (e2 < end_ext2)
	es += sprintf(es, "%d,", *(e2++));
    *(--es) = '\0';
    message("    e2 = <<%s>>", ext_str);

    message("Starting %s...", test_desc);


    reversed_args = 0;
    er = exp_res;
    e1 = ext1;
    e2 = ext2;

 reversed_args_start:

    ar = erl_compare_ext(e1, e2);
    if (er < 0) {
	if (ar > 0)
	    fail("expected result e1 < e2; actual result e1 > e2\n");
	else if (ar == 0)
	    fail("expected result e1 < e2; actual result e1 = e2\n");
    }
    else if (er > 0) {
	if (ar < 0)
	    fail("expected result e1 > e2; actual result e1 < e2\n");
	else if (ar == 0)
	    fail("expected result e1 > e2; actual result e1 = e2\n");
    }
    else {
	if (ar > 0)
	    fail("expected result e1 = e2; actual result e1 > e2\n");
	else if (ar < 0)
	    fail("expected result e1 = e2; actual result e1 < e2\n");
    }

    message("%s", "SUCCEEDED!");
    if (!reversed_args) {
	message("Starting %s with reversed arguments...", test_desc);
	e2 = ext1;
	e1 = ext2;
	if (exp_res < 0)
	    er = 1;
	else if (exp_res > 0)
	    er = -1;
	reversed_args = 1;
	goto reversed_args_start;
    }

    message("%s", "");

}


#define SMALL_ATOM_UTF8_EXT (119)
#define REFERENCE_EXT     (101)
#define PORT_EXT          (102)
#define PID_EXT           (103)
#define NEW_REFERENCE_EXT (114)


#define PUT_UINT16(E, X) ((E)[0] = ((X) >> 8) & 0xff,  \
			  (E)[1] = (X) & 0xff)

#define PUT_UINT32(E, X) ((E)[0] = ((X) >> 24) & 0xff, \
			  (E)[1] = ((X) >> 16) & 0xff, \
			  (E)[2] = ((X) >> 8) & 0xff,  \
			  (E)[3] = (X) & 0xff)

static unsigned char *
write_atom(unsigned char *buf, char *atom)
{
    uint len;

    len = 0;
    while(atom[len]) {
	buf[len + 2] = atom[len];
	len++;
    }
    buf[0] = SMALL_ATOM_UTF8_EXT;
    buf[1] = len;

    return buf + 2 + len;
}

static unsigned char *
write_pid(unsigned char *buf, char *node, uint cre, uint num, uint ser)
{
    unsigned char *e = buf;

    *(e++) = PID_EXT;
    e = write_atom(e, node);
    PUT_UINT32(e, num & ((1 << 15) - 1));
    e += 4;
    PUT_UINT32(e, ser & ((1 << 3) - 1));
    e += 4;
    *(e++) = cre & ((1 << 2) - 1);

    return e;
}

static unsigned char *
write_port(unsigned char *buf, char *node, uint cre, uint id)
{
    unsigned char *e = buf;

    *(e++) = PORT_EXT;
    e = write_atom(e, node);
    PUT_UINT32(e, id & ((1 << 15) - 1));
    e += 4;
    *(e++) = cre & ((1 << 2) - 1);

    return e;
}

static unsigned char *
write_ref(unsigned char *buf, char *node, uint cre, uint id[], uint no_ids)
{
    int i;
    unsigned char *e = buf;

    if (no_ids == 1) {
	*(e++) = REFERENCE_EXT;
	e = write_atom(e, node);
	PUT_UINT32(e, id[0] & ((1 << 15) - 1));
	e += 4;
	*(e++) = cre & ((1 << 2) - 1);
    }
    else {
	*(e++) = NEW_REFERENCE_EXT;
	PUT_UINT16(e, no_ids);
	e += 2;
	e = write_atom(e, node);
	*(e++) = cre & ((1 << 2) - 1);
	for (i = 0; i < no_ids; i++) {
	    PUT_UINT32(e, id[i]);
	    e += 4;
	}
    }

    return e;
}

