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
 * Purpose: Tests the functions in erl_eterm.c and erl_malloc.c.
 * Author: Bjorn Gustavsson
 *
 * See the erl_eterm_SUITE.erl file for a "table of contents".
 */

#include <stdio.h>
#include <string.h>

#include "runner.h"

/*
 * Find out which version of erl_interface we are using.
 */

#ifdef ERL_IS_STRING
#undef NEW_ERL_INTERFACE
#else
#define NEW_ERL_INTERFACE
#endif

void dump_term (FILE *fp, ETERM *t);

static ETERM* all_types();

/***********************************************************************
 *
 *	1.   B a s i c    t e s t s
 *
 ***********************************************************************/

/*
 * Sends a list contaning all data types to the Erlang side.
 */

TESTCASE(build_terms)
{
    ETERM* t;

    erl_init(NULL, 0);
    t = all_types();
    send_term(t);
    report(1);
}

static int abs_and_sign(ETERM* v, unsigned long long* av, int* sign)
{
    long long sv;
    switch (ERL_TYPE(v)) {
    case ERL_INTEGER: sv = ERL_INT_VALUE(v); break;
    case ERL_U_INTEGER: *av = ERL_INT_UVALUE(v); *sign = 0; return 1;
    case ERL_LONGLONG: sv = ERL_LL_VALUE(v); break;
    case ERL_U_LONGLONG: *av = ERL_LL_UVALUE(v); *sign = 0; return 1;
    default: return 0;
    }
    if (sv < 0) {
	*av = -sv;
	*sign = 1;
    }
    else {
	*av = sv;
	*sign = 0;
    }
    return 1;
}

/* Shouldn't erl_match() cope with this?
*/
static int eq_ints(ETERM* a, ETERM* b)
{
    unsigned long long a_abs, b_abs;
    int a_sign, b_sign;
    return abs_and_sign(a, &a_abs, &a_sign) && abs_and_sign(b, &b_abs, &b_sign)
           && (a_abs == b_abs) && (a_sign == b_sign);
}

static void encode_decode(ETERM* original, const char* text)
{
    static unsigned char encoded[16*1024];
    ETERM* new_terms;
    ETERM* head;
    int bytes;
    int len;

    /* If a list, check the elements one by one first */
    head = erl_hd(original);
    if (head != NULL) {
	encode_decode(head, "CAR");
	encode_decode(erl_tl(original), "CDR");
    }

    bytes = erl_encode(original, encoded);
    if (bytes == 0) {
	fail("failed to encode terms");
    } 
    else if (bytes > sizeof(encoded)) {
	fail("encoded terms buffer overflow");
    }
    else if (bytes != (len=erl_term_len(original))) {
	fprintf(stderr, "bytes(%d) != len(%d) for term ", bytes, len);
	erl_print_term(stderr, original);
	fprintf(stderr, " [%s]\r\n", text);
	fail("erl_encode and erl_term_len do not agree");
    }
    else if ((new_terms = erl_decode(encoded)) == NULL) {
	fail("failed to decode terms");
    }
    else if (!erl_match(original, new_terms) && !eq_ints(original, new_terms)) {
	erl_print_term(stderr, original);
	fprintf(stderr, "(%i) != (%i)", ERL_TYPE(original), ERL_TYPE(new_terms));
	erl_print_term(stderr, new_terms);
	fprintf(stderr, " [%s]\r\n", text);
	fail("decoded terms didn't match original");
    }
    erl_free_term(original);
    erl_free_term(new_terms);
}
/*
 * Converts an Erlang term to the external term format and back again.
 */

TESTCASE(round_trip_conversion)
{
    int n, i;

    erl_init(NULL, 0);
    encode_decode(all_types(), "ALL");

    {
	int v;
	for (v = 8, n = 0; n < (sizeof(v)*8-4-1); v <<= 1, n++) {
	    for (i=-4; i<4; i++) {
		encode_decode(erl_mk_int(v+i), "INT");
		encode_decode(erl_mk_int(-(v+i)), "NEG INT");
	    }
	}
    }
    {
	unsigned int v;
	for (v = 8; v; v <<= 1) {
	    for (i=-4; i<4; i++) {
		encode_decode(erl_mk_uint(v+i), "UINT");
	    }
	}
    }
    {
	long long v;
	for (v = 8, n = 0; n < (sizeof(v)*8-4-1); v <<= 1, n++) {
	    for (i=-4; i<4; i++) {
		encode_decode(erl_mk_longlong(v+i), "LONGLONG");
		encode_decode(erl_mk_longlong(-(v+i)), "NEG LONGLONG");
	    }
	}
    }
    {
	unsigned long long v;
	for (v = 8; v; v <<= 1) {
	    for (i=-4; i<4; i++) {
		encode_decode(erl_mk_ulonglong(v+i), "ULONGLONG");
	    }
	}
    }

    report(1);
}

/*
 * Decodes data from the Erlang side and verifies.
 */

TESTCASE(decode_terms)
{
    ETERM* terms;
    char* message;

    erl_init(NULL, 0);
    terms = get_term();
    if (terms == NULL) {
	fail("unexpected end of file");
    } else {
	ETERM* all;
	ETERM* p;
	ETERM* t;
	int i;

	all = p = all_types();
	t = terms;

	/*
	 * XXX For now, skip the reference, pid, and port, because
	 * the match will fail.  Must write code here to do some other
	 * validating.
	 */

	for (i=0; i<6; i++) {

	  p = erl_tl(p);
	  t = erl_tl(t);
	  erl_free_term(p);
	  erl_free_term(t);

	}

	/* 
	 * Match the tail of the lists.
	 */

	if (!erl_match(p, t))
	{
	    fail("Received terms didn't match expected");
	}
	erl_free_term(all);
	erl_free_term(terms);
	report(1);
    }
}

/*
 * Decodes a float from the Erlang side and verifies.
 */

TESTCASE(decode_float)
{
    ETERM* afnum;
    ETERM* efnum;
    int result;

    erl_init(NULL, 0);
    afnum = get_term();
    efnum = erl_mk_float(3.1415);
    result = erl_match(efnum, afnum);
    erl_free_term(afnum);
    erl_free_term(efnum);
    report(result);
}    
    
/*
 * Tests the erl_free_compound() function.
 */

TESTCASE(t_erl_free_compound)
{
    ETERM* t;

    erl_init(NULL, 0);

    t = all_types();
    erl_free_compound(t);
    report(1);
}


/***********************************************************************
 *
 *	2.   C o n s t r u c t i n g   t e r m s
 *
 ***********************************************************************/

/*
 * Makes various integers, and sends them to Erlang for verification.
 */

TESTCASE(t_erl_mk_int)
{
#define SEND_INT(i) \
    do { \
	 ETERM* t = erl_mk_int(i); \
	 send_term(t); \
     } while (0);

    erl_init(NULL, 0);

    SEND_INT(0);
    SEND_INT(127);
    SEND_INT(128);
    SEND_INT(255);
    SEND_INT(256);

    SEND_INT(0xFFFF);
    SEND_INT(0x10000);

    SEND_INT(0x07FFFFFF);
    SEND_INT(0x0FFFFFFF);
    SEND_INT(0x1FFFFFFF);
    SEND_INT(0x3FFFFFFF);
    SEND_INT(0x7FFFFFFF);

    SEND_INT(0x08000000);
    SEND_INT(0x10000000);
    SEND_INT(0x20000000);
    SEND_INT(0x40000000);

    SEND_INT(-0x07FFFFFF);
    SEND_INT(-0x0FFFFFFF);
    SEND_INT(-0x1FFFFFFF);
    SEND_INT(-0x3FFFFFFF);
    SEND_INT(-0x7FFFFFFF);

    SEND_INT(-0x08000000);
    SEND_INT(-0x10000000);
    SEND_INT(-0x20000000);
    SEND_INT(-0x40000000);

    SEND_INT(-0x08000001);
    SEND_INT(-0x10000001);
    SEND_INT(-0x20000001);
    SEND_INT(-0x40000001);

    SEND_INT(-0x08000002);
    SEND_INT(-0x10000002);
    SEND_INT(-0x20000002);
    SEND_INT(-0x40000002);

    SEND_INT(-1999999999);
    SEND_INT(-2000000000);
    SEND_INT(-2000000001);

    report(1);
}


/*
 * Makes lists of various sizes, and sends them to Erlang for verification.
 */

TESTCASE(t_erl_mk_list)
{
    ETERM* a[4];

    erl_init(NULL, 0);

    /*
     * Empty list.
     */

    send_term(erl_mk_list(a, 0));

    /*
     * One element: [abc]
     */

    a[0] = erl_mk_atom("abc");
    send_term(erl_mk_list(a, 1));
    erl_free_term(a[0]);

    /*
     * Two elements: [abcdef, 42].
     */

    a[0] = erl_mk_atom("abcdef");
    a[1] = erl_mk_int(42);
    send_term(erl_mk_list(a, 2));
    erl_free_term(a[0]);
    erl_free_term(a[1]);

    /*
     * Four elements.
     */

    a[0] = erl_mk_float(0.0);
    a[1] = erl_mk_int(23);
    a[2] = erl_mk_empty_list();
    a[3] = erl_mk_float(3.1415);
    send_term(erl_mk_list(a, 4));
    erl_free_term(a[0]);
    erl_free_term(a[1]);
    erl_free_term(a[2]);
    erl_free_term(a[3]);
    
    report(1);
}

/*
 * A basic test of erl_copy_term().
 */

TESTCASE(basic_copy)
{
    ETERM* original;
    ETERM* copy;
    int result;

    erl_init(NULL, 0);
    original = all_types();
    copy = erl_copy_term(original);
    if (copy == NULL) {
	fail("erl_copy_term() failed");
    } else if (!erl_match(original, copy))
    {
	fail("copy doesn't match original");
    }
    
    erl_free_term(original);
    erl_free_term(copy);
    report(1);
}


/*
 * A basic test of erl_mk_atom().
 */

TESTCASE(t_erl_mk_atom)
{
    erl_init(NULL, 0);

    send_term(erl_mk_atom("madonna"));
    send_term(erl_mk_atom("Madonna"));
    send_term(erl_mk_atom("mad donna"));
    send_term(erl_mk_atom("_madonna_"));
    send_term(erl_mk_atom("/home/madonna/tour_plan"));
    send_term(erl_mk_atom("http://www.madonna.com/tour_plan"));
    send_term(erl_mk_atom("\'madonna\'"));
    send_term(erl_mk_atom("\"madonna\""));
    send_term(erl_mk_atom("\\madonna\\"));
    send_term(erl_mk_atom("{madonna,21,'mad donna',12}"));

    report(1);
}


/*
 * A basic test of erl_mk_binary().
 */

TESTCASE(t_erl_mk_binary)
{

    char* string;
    erl_init(NULL, 0);

    string = "{madonna,21,'mad donna',1234.567.890, !#$%&/()=?+-@, \" \\}";
    send_term(erl_mk_binary(string,strlen(string)));

    report(1);
}


/*
 * A basic test of erl_mk_empty_list().
 */

TESTCASE(t_erl_mk_empty_list)
{
    erl_init(NULL, 0);

    send_term(erl_mk_empty_list());
    report(1);
}


/*
 * A basic test of erl_mk_float().
 */

TESTCASE(t_erl_mk_float)
{
    ETERM* arr[6];
    ETERM* emsg;

    erl_init(NULL, 0);

    arr[0] = erl_mk_float(3.1415);
    arr[1] = erl_mk_float(1.999999);
    arr[2] = erl_mk_float(2.000000);
    arr[3] = erl_mk_float(2.000001);
    arr[4] = erl_mk_float(2.000002);
    arr[5] = erl_mk_float(12345.67890);
    emsg = (erl_mk_tuple(arr,6));

    send_term(emsg);

    erl_free_array(arr,6);
    /* emsg already freed by send_term() */
    /* erl_free_term(emsg); */ 

    report(1);
}


/*
 * A basic test of erl_mk_pid().
 */

TESTCASE(t_erl_mk_pid)
{
    erl_init(NULL, 0);

    send_term(erl_mk_pid("kalle@localhost", 3, 2, 1));
    report(1);
}

/*
 * A basic test of erl_mk_pid().
 */

TESTCASE(t_erl_mk_xpid)
{
    erl_init(NULL, 0);

    send_term(erl_mk_pid("kalle@localhost", 32767, 8191, 1));
    report(1);
}


/*
 * A basic test of erl_mk_port().
 */

TESTCASE(t_erl_mk_port)
{
    erl_init(NULL, 0);

    send_term(erl_mk_port("kalle@localhost", 4, 1));
    report(1);
}

/*
 * A basic test of erl_mk_port().
 */

TESTCASE(t_erl_mk_xport)
{
    erl_init(NULL, 0);

    send_term(erl_mk_port("kalle@localhost", 268435455, 1));
    report(1);
}

/*
 * A basic test of erl_mk_ref().
 */

TESTCASE(t_erl_mk_ref)
{
    erl_init(NULL, 0);

    send_term(erl_mk_ref("kalle@localhost", 6, 1));
    report(1);
}

/*
 * A basic test of erl_mk_long_ref().
 */


TESTCASE(t_erl_mk_long_ref)
{
    erl_init(NULL, 0);

    send_term(erl_mk_long_ref("kalle@localhost",
			      4294967295, 4294967295, 262143,
			      1));
    report(1);
}


/*
 * A basic test of erl_mk_string().
 */

TESTCASE(t_erl_mk_string)
{

    erl_init(NULL, 0);

    send_term(erl_mk_string("madonna"));
    send_term(erl_mk_string("Madonna"));
    send_term(erl_mk_string("mad donna"));
    send_term(erl_mk_string("_madonna_"));
    send_term(erl_mk_string("/home/madonna/tour_plan"));
    send_term(erl_mk_string("http://www.madonna.com/tour_plan"));
    send_term(erl_mk_string("\'madonna\'"));
    send_term(erl_mk_string("\"madonna\""));
    send_term(erl_mk_string("\\madonna\\"));
    send_term(erl_mk_string("{madonna,21,'mad donna',12}"));

    report(1);
}


/*
 * A basic test of erl_mk_estring().
 */

TESTCASE(t_erl_mk_estring)
{
    char* string;
    erl_init(NULL, 0);

    string = "madonna";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "Madonna";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "mad donna";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "_madonna_";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "/home/madonna/tour_plan";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "http://www.madonna.com/tour_plan";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "\'madonna\'";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "\"madonna\"";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "\\madonna\\";
    send_term(erl_mk_estring(string,strlen(string)));
    string = "{madonna,21,'mad donna',12}";
    send_term(erl_mk_estring(string,strlen(string)));

    report(1);
}


/*
 * A basic test of erl_mk_tuple().
 */

TESTCASE(t_erl_mk_tuple)
{
    ETERM* arr[4];
    ETERM* arr2[2];
    ETERM* arr3[2];
    ETERM* arr4[2];

    erl_init(NULL, 0);

    /* {madonna,21,'mad donna',12} */
    arr[0] = erl_mk_atom("madonna");
    arr[1] = erl_mk_int(21);
    arr[2] = erl_mk_atom("mad donna");
    arr[3] = erl_mk_int(12);

    send_term(erl_mk_tuple(arr,4));

    erl_free_array(arr,4);


    /* {'Madonna',21,{children,{"Isabella",2}},{'home page',"http://www.madonna.com/"} */
    arr4[0] = erl_mk_atom("home page");
    arr4[1] = erl_mk_string("http://www.madonna.com/");

    arr3[0] = erl_mk_string("Isabella");
    arr3[1] = erl_mk_int(2);

    arr2[0] = erl_mk_atom("children");
    arr2[1] = erl_mk_tuple(arr3,2);

    arr[0] = erl_mk_atom("Madonna");
    arr[1] = erl_mk_int(21);
    arr[2] = erl_mk_tuple(arr2,2);
    arr[3] = erl_mk_tuple(arr4,2);

    send_term(erl_mk_tuple(arr,4));

    erl_free_array(arr,4);
    erl_free_array(arr2,2);
    erl_free_array(arr3,2);
    erl_free_array(arr4,2);


    report(1);
}


/*
 * A basic test of erl_mk_uint().
 */

TESTCASE(t_erl_mk_uint)
{
    unsigned i;

    erl_init(NULL, 0);

    send_term(erl_mk_uint(54321));
    i = 2147483647;
    send_term(erl_mk_uint(i));
    send_term(erl_mk_uint(i+1));
    send_term(erl_mk_uint(i+2));
    send_term(erl_mk_uint(i+3));
    send_term(erl_mk_uint(i+i+1));

    report(1);
}


/*
 * A basic test of erl_mk_var().
 */

TESTCASE(t_erl_mk_var)
{
    ETERM* mk_var;
    ETERM* term;
    ETERM* term2;
    ETERM* arr[4];
    ETERM* arr_term[2];
    ETERM* mk_var_tuple;
    ETERM* term_tuple;

    erl_init(NULL, 0);


    /* match unbound/bound variable against an integer */
    term = erl_mk_int(17);
    term2 = erl_mk_int(2);
    mk_var = erl_mk_var("New_var");
    send_term(erl_mk_int(erl_match(mk_var, term))); /* should be ok */
    send_term(erl_mk_int(erl_match(mk_var, term2))); /* should fail */
    send_term(erl_mk_int(erl_match(mk_var, term))); /* should be ok */
    send_term(erl_mk_int(erl_match(mk_var, term2))); /* should fail */
    erl_free_term(mk_var);
    erl_free_term(term);
    erl_free_term(term2);

    /* match unbound variable against a tuple */    
    arr[0] = erl_mk_atom("madonna");
    arr[1] = erl_mk_int(21);
    arr[2] = erl_mk_atom("mad donna");
    arr[3] = erl_mk_int(12);
    mk_var = erl_mk_var("New_var");
    term = erl_mk_tuple(arr,4);
    send_term(erl_mk_int(erl_match(mk_var, term))); /* should be ok */
    erl_free_term(mk_var);
    erl_free_term(term);
    erl_free_array(arr,4);


    /* match (twice) unbound variable against an incorrect tuple */    
    arr[0] = erl_mk_var("New_var");
    arr[1] = erl_mk_var("New_var"); 
    arr_term[0] = erl_mk_int(17);
    arr_term[1] = erl_mk_int(27);
    mk_var_tuple = erl_mk_tuple(arr,2);
    term_tuple = erl_mk_tuple(arr_term,2);
    send_term(erl_mk_int(erl_match(mk_var_tuple, term_tuple))); /* should fail */
    erl_free_array(arr,2);
    erl_free_array(arr_term,2);
    erl_free_term(mk_var_tuple);
    erl_free_term(term_tuple);


    /* match (twice) unbound variable against a correct tuple */    
    arr[0] = erl_mk_var("New_var");
    arr[1] = erl_mk_var("New_var"); 
    arr_term[0] = erl_mk_int(17);
    arr_term[1] = erl_mk_int(17);
    mk_var_tuple = erl_mk_tuple(arr,2);
    term_tuple = erl_mk_tuple(arr_term,2);
    send_term(erl_mk_int(erl_match(mk_var_tuple, term_tuple))); /* should be ok */
    erl_free_array(arr,2);
    erl_free_array(arr_term,2);
    erl_free_term(mk_var_tuple);
    erl_free_term(term_tuple);

    report(1);
}


/*
 * A basic test of erl_size().
 */

TESTCASE(t_erl_size)
{
    ETERM* arr[4];
    ETERM* tuple;
    ETERM* bin;
    char* string;

    erl_init(NULL, 0);

    /* size of a tuple */
    tuple = erl_format("{}");
    send_term(erl_mk_int(erl_size(tuple)));
    erl_free_term(tuple);

    arr[0] = erl_mk_atom("madonna");
    arr[1] = erl_mk_int(21);
    arr[2] = erl_mk_atom("mad donna");
    arr[3] = erl_mk_int(12);
    tuple = erl_mk_tuple(arr,4);

    send_term(erl_mk_int(erl_size(tuple)));

    erl_free_array(arr,4);
    erl_free_term(tuple);

    /* size of a binary */
    string = "";
    bin = erl_mk_binary(string,strlen(string));
    send_term(erl_mk_int(erl_size(bin)));
    erl_free_term(bin);

    string = "{madonna,21,'mad donna',12}";
    bin = erl_mk_binary(string,strlen(string));
    send_term(erl_mk_int(erl_size(bin)));
    erl_free_term(bin);

    report(1);
}


/*
 * A basic test of erl_var_content().
 */

TESTCASE(t_erl_var_content)
{
    ETERM* mk_var;
    ETERM* term;
    ETERM* tuple;
    ETERM* list;
    ETERM* a;
    ETERM* b;
    ETERM* arr[4];
    ETERM* arr2[2];
    ETERM* arr3[2];
    ETERM* arr4[2];

    erl_init(NULL, 0);

    term = erl_mk_int(17);
    mk_var = erl_mk_var("Var");

    /* unbound, should return NULL */
    if (erl_var_content(mk_var,"Var") != NULL) 
      fail("t_erl_var_content() failed");

    erl_match(mk_var, term); 
    send_term(erl_var_content(mk_var,"Var")); /* should return 17 */

    /* integer, should return NULL */
    if (erl_var_content(term,"Var") != NULL) 
      fail("t_erl_var_content() failed");

    /* unknown variable, should return NULL */
    if (erl_var_content(mk_var,"Unknown_Var") != NULL) 
      fail("t_erl_var_content() failed");

    erl_free_term(mk_var);
    erl_free_term(term);

    /* {'Madonna',21,{children,{"Name","Age"}},{"Home_page","Tel_no"}} */
    arr4[0] = erl_mk_var("Home_page");
    arr4[1] = erl_mk_var("Tel_no");
    a = erl_mk_string("http://www.madonna.com"); 
    erl_match(arr4[0], a); 

    arr3[0] = erl_mk_var("Name");
    arr3[1] = erl_mk_var("Age");
    b = erl_mk_int(2); 
    erl_match(arr3[1], b); 

    arr2[0] = erl_mk_atom("children");
    arr2[1] = erl_mk_tuple(arr3,2);

    arr[0] = erl_mk_atom("Madonna");
    arr[1] = erl_mk_int(21);
    arr[2] = erl_mk_tuple(arr2,2);
    arr[3] = erl_mk_tuple(arr4,2);

    tuple = erl_mk_tuple(arr,4);

    /* should return "http://www.madonna.com" */
    send_term(erl_var_content(tuple,"Home_page"));  
                                                 
    /* unbound, should return NULL */
    if (erl_var_content(tuple,"Tel_no") != NULL) 
      fail("t_erl_var_content() failed");

    /* unbound, should return NULL */
    if (erl_var_content(tuple,"Name") != NULL) 
      fail("t_erl_var_content() failed");

    /* should return 2 */
    send_term(erl_var_content(tuple,"Age")); 

    erl_free_array(arr,4);
    erl_free_array(arr2,2);
    erl_free_array(arr3,2);
    erl_free_array(arr4,2);
    erl_free_term(tuple);
    erl_free_term(a);
    erl_free_term(b);


    /* [] */
    list = erl_mk_empty_list();
    if (erl_var_content(list,"Tel_no") != NULL) 
      fail("t_erl_var_content() failed");
    erl_free_term(list);
    

    /* ['Madonna',[],{children,{"Name","Age"}},{"Home_page","Tel_no"}] */
    arr4[0] = erl_mk_var("Home_page");
    arr4[1] = erl_mk_var("Tel_no");
    a = erl_mk_string("http://www.madonna.com"); 
    erl_match(arr4[0], a); 

    arr3[0] = erl_mk_var("Name");
    arr3[1] = erl_mk_var("Age");
    b = erl_mk_int(2); 
    erl_match(arr3[1], b); 

    arr2[0] = erl_mk_atom("children");
    arr2[1] = erl_mk_tuple(arr3,2);

    arr[0] = erl_mk_atom("Madonna");
    arr[1] = erl_mk_empty_list();
    arr[2] = erl_mk_tuple(arr2,2);
    arr[3] = erl_mk_tuple(arr4,2);

    list = erl_mk_list(arr,4);

    /* should return "http://www.madonna.com" */
    send_term(erl_var_content(list,"Home_page"));  
                                                 
    /* unbound, should return NULL */
    if (erl_var_content(list,"Tel_no") != NULL) 
      fail("t_erl_var_content() failed");

    /* unbound, should return NULL */
    if (erl_var_content(list,"Name") != NULL) 
      fail("t_erl_var_content() failed");

    /* should return 2 */
    send_term(erl_var_content(list,"Age")); 

    erl_free_array(arr,4);
    erl_free_array(arr2,2);
    erl_free_array(arr3,2);
    erl_free_array(arr4,2);
    erl_free_term(list);
    erl_free_term(a);
    erl_free_term(b);

    report(1);
}


/*
 * A basic test of erl_element().
 */

TESTCASE(t_erl_element)
{
    ETERM* arr[4];
    ETERM* arr2[2];
    ETERM* arr3[2];
    ETERM* arr4[2];
    ETERM* tuple;

    erl_init(NULL, 0);

    arr[0] = erl_mk_atom("madonna");
    arr[1] = erl_mk_int(21);
    arr[2] = erl_mk_atom("mad donna");
    arr[3] = erl_mk_int(12);
    tuple = erl_mk_tuple(arr,4);

    send_term(erl_element(1,tuple));
    send_term(erl_element(2,tuple));
    send_term(erl_element(3,tuple));
    send_term(erl_element(4,tuple));

    erl_free_array(arr,4);
    erl_free_term(tuple);

    /* {'Madonna',21,{children,{"Isabella",2}},{'home page',"http://www.madonna.com/"} */
    arr4[0] = erl_mk_atom("home page");
    arr4[1] = erl_mk_string("http://www.madonna.com/");

    arr3[0] = erl_mk_string("Isabella");
    arr3[1] = erl_mk_int(2);

    arr2[0] = erl_mk_atom("children");
    arr2[1] = erl_mk_tuple(arr3,2);

    arr[0] = erl_mk_atom("Madonna");
    arr[1] = erl_mk_int(21);
    arr[2] = erl_mk_tuple(arr2,2);
    arr[3] = erl_mk_tuple(arr4,2);

    tuple = erl_mk_tuple(arr,4);
    send_term(erl_element(1,tuple));
    send_term(erl_element(2,tuple));
    send_term(erl_element(3,tuple));
    send_term(erl_element(4,tuple));

    erl_free_term(tuple);
    erl_free_array(arr,4);
    erl_free_array(arr2,2);
    erl_free_array(arr3,2);
    erl_free_array(arr4,2);

    report(1);
}


/*
 * A basic test of erl_cons().
 */

TESTCASE(t_erl_cons)
{
    ETERM* list;
    ETERM* anAtom;
    ETERM* anInt;

    erl_init(NULL, 0);

    anAtom = erl_mk_atom("madonna");
    anInt = erl_mk_int(21);
    list = erl_mk_empty_list();
    list = erl_cons(anInt, list);
    send_term(erl_cons(anAtom, list));

    erl_free_term(anAtom); 
    erl_free_term(anInt);
    erl_free_compound(list);

    report(1);
}




/***********************************************************************
 *
 *	3.   E x t r a c t i n g  &   i n f o    f u n c t i o n s
 *
 ***********************************************************************/

/*
 * Calculates the length of each list sent to it and sends back the result.
 */

TESTCASE(t_erl_length)
{
    erl_init(NULL, 0);

    for (;;) {
	ETERM* term = get_term();

	if (term == NULL) {
	    report(1);
	    return;
	} else {
	    ETERM* len_term;

	    len_term = erl_mk_int(erl_length(term));
	    erl_free_term(term);
	    send_term(len_term);
	}
    }
}

/*
 * Gets the head of each term and sends the result back.
 */

TESTCASE(t_erl_hd)
{
    erl_init(NULL, 0);

    for (;;) {
	ETERM* term = get_term();

	if (term == NULL) {
	    report(1);
	    return;
	} else {
	    ETERM* head;

	    head = erl_hd(term);
	    send_term(head);
	    erl_free_term(term);
	}
    }
}

/*
 * Gets the tail of each term and sends the result back.
 */

TESTCASE(t_erl_tl)
{
    erl_init(NULL, 0);

    for (;;) {
	ETERM* term = get_term();

	if (term == NULL) {
	    report(1);
	    return;
	} else {
	    ETERM* tail;

	    tail = erl_tl(term);
	    send_term(tail);
	    erl_free_term(term);
	}
    }
}

/*
 * Checks the type checking macros.
 */

TESTCASE(type_checks)
{
    ETERM* t;
    ETERM* atom;

    erl_init(NULL, 0);
    atom = erl_mk_atom("an_atom");

#define TYPE_CHECK(macro, term) \
    { ETERM* t = term; \
      if (macro(t)) { \
	 erl_free_term(t); \
      } else { \
	 fail("Macro " #macro " failed on " #term); \
      } \
    }
    
    TYPE_CHECK(ERL_IS_INTEGER, erl_mk_int(0x7FFFFFFF));
#ifdef NEW_ERL_INTERFACE
    TYPE_CHECK(ERL_IS_UNSIGNED_INTEGER, erl_mk_uint(0x7FFFFFFF));
#endif
    TYPE_CHECK(ERL_IS_FLOAT, erl_mk_float(5.5));
    TYPE_CHECK(ERL_IS_ATOM, erl_mk_atom("another_atom"));

    TYPE_CHECK(ERL_IS_EMPTY_LIST, erl_mk_empty_list());
    TYPE_CHECK(!ERL_IS_EMPTY_LIST, erl_cons(atom, atom));

#ifdef NEW_ERL_INTERFACE
    TYPE_CHECK(!ERL_IS_CONS, erl_mk_empty_list());
    TYPE_CHECK(ERL_IS_CONS, erl_cons(atom, atom));
#endif

    TYPE_CHECK(ERL_IS_LIST, erl_mk_empty_list());
    TYPE_CHECK(ERL_IS_LIST, erl_cons(atom, atom));

    TYPE_CHECK(ERL_IS_PID, erl_mk_pid("a@a", 42, 1, 1));
    TYPE_CHECK(ERL_IS_PORT, erl_mk_port("a@a", 42, 1));
    TYPE_CHECK(ERL_IS_REF, erl_mk_ref("a@a", 42, 1));
    
    TYPE_CHECK(ERL_IS_BINARY, erl_mk_binary("a", 1));
    TYPE_CHECK(ERL_IS_TUPLE, erl_mk_tuple(&atom, 1));
#undef TYPE_CHECK

    erl_free_term(atom);

    report(1);
}

/*
 * Checks the extractor macros.
 */

TESTCASE(extractor_macros)
{
  ETERM* t;

  erl_init(NULL, 0);

#ifdef NEW_ERL_INTERFACE
#define MATCH(a, b) ((a) == (b) ? 1 : fail("bad match: " #a))
#define STR_MATCH(a, b) (strcmp((a), (b)) ? fail("bad match: " #a) : 0)

  {				/* Integer */
    int anInt = 0x7FFFFFFF;
    t = erl_mk_int(anInt);
    MATCH(ERL_INT_VALUE(t), anInt);
    MATCH(ERL_INT_UVALUE(t), anInt);
    erl_free_term(t);
  }

  {				/* Float */
    double aFloat = 3.1415;
    t = erl_mk_float(aFloat);
    MATCH(ERL_FLOAT_VALUE(t), aFloat);
    erl_free_term(t);
  }

  {				/* Atom. */
    char* aString = "nisse";
    t = erl_mk_atom(aString);
    if (memcmp(ERL_ATOM_PTR(t), aString, strlen(aString)) != 0)
      fail("bad match");
    MATCH(ERL_ATOM_SIZE(t), strlen(aString));
    erl_free_term(t);
  }

  {				/* Pid. */
    char* node = "arne@strider";
    int number = 42;
    int serial = 5;
    int creation = 1;

    t = erl_mk_pid(node, number, serial, creation);
    STR_MATCH(ERL_PID_NODE(t), node);
    MATCH(ERL_PID_NUMBER(t), number);
    MATCH(ERL_PID_SERIAL(t), serial);
    MATCH(ERL_PID_CREATION(t), creation);
    erl_free_term(t);
  }

  {				/* Port. */
    char* node = "kalle@strider";
    int number = 45;
    int creation = 1;

    t = erl_mk_port(node, number, creation);
    STR_MATCH(ERL_PORT_NODE(t), node);
    MATCH(ERL_PORT_NUMBER(t), number);
    MATCH(ERL_PORT_CREATION(t), creation);
    erl_free_term(t);
  }

  {				/* Reference. */
    char* node = "kalle@strider";
    int number = 48;
    int creation = 1;

    t = erl_mk_ref(node, number, creation);
    STR_MATCH(ERL_REF_NODE(t), node);
    MATCH(ERL_REF_NUMBER(t), number);
    MATCH(ERL_REF_CREATION(t), creation);
    erl_free_term(t);
  }

  {				/* Tuple. */
    ETERM* arr[2];

    arr[0] = erl_mk_int(51);
    arr[1] = erl_mk_int(52);
    t = erl_mk_tuple(arr, ASIZE(arr));
    MATCH(ERL_TUPLE_SIZE(t), ASIZE(arr));
    MATCH(ERL_TUPLE_ELEMENT(t, 0), arr[0]);
    MATCH(ERL_TUPLE_ELEMENT(t, 1), arr[1]);
    erl_free_array(arr, ASIZE(arr));
    erl_free_term(t);
  }

  {				/* Binary. */
    static char bin[] = {1, 2, 3, 0, 4, 5};

    t = erl_mk_binary(bin, ASIZE(bin));
    MATCH(ERL_BIN_SIZE(t), ASIZE(bin));
    if (memcmp(ERL_BIN_PTR(t), bin, ASIZE(bin)) != 0)
      fail("bad match");
    erl_free_term(t);
  }

  {
    ETERM* head = erl_mk_atom("head");
    ETERM* tail = erl_mk_atom("tail");

    t = erl_cons(head, tail);
    MATCH(ERL_CONS_HEAD(t), head);
    MATCH(ERL_CONS_TAIL(t), tail);
    erl_free_term(head);
    erl_free_term(tail);
    erl_free_term(t);
  }
#undef MATCH
#undef STR_MATCH
#endif

  report(1);
}



/***********************************************************************
 *
 *	4.   I / O   l i s t   f u n c t i o n s
 *
 ***********************************************************************/

/*
 * Invokes erl_iolist_length() on each term and send backs the result.
 */

TESTCASE(t_erl_iolist_length)
{
    erl_init(NULL, 0);

    for (;;) {
	ETERM* term = get_term();

	if (term == NULL) {
	    report(1);
	    return;
	} else {
#ifndef NEW_ERL_INTERFACE
	    fail("Function not present in this version of erl_interface");
#else
	    ETERM* len_term;

	    len_term = erl_mk_int(erl_iolist_length(term));
	    erl_free_term(term);
	    send_term(len_term);
#endif
	}
    }
}

/*
 * Invokes erl_iolist_to_binary() on each term and send backs the result.
 */

TESTCASE(t_erl_iolist_to_binary)
{
    erl_init(NULL, 0);

    for (;;) {
	ETERM* term = get_term();

	if (term == NULL) {
	    report(1);
	    return;
	} else {
#ifndef NEW_ERL_INTERFACE
	    fail("Function not present in this version of erl_interface");
#else
	    ETERM* new_term;

	    new_term = erl_iolist_to_binary(term);

	    erl_free_term(term);
	    send_term(new_term);
#endif
	}
    }
}

/*
 * Invokes erl_iolist_to_string() on each term and send backs the result.
 */

TESTCASE(t_erl_iolist_to_string)
{
    erl_init(NULL, 0);

    for (;;) {
	ETERM* term = get_term();

	if (term == NULL) {
	    report(1);
	    return;
	} else {
#ifndef NEW_ERL_INTERFACE
	    fail("Function not present in this version of erl_interface");
#else
	    char* result;

	    result = erl_iolist_to_string(term);
	    erl_free_term(term);
	    if (result != NULL) {
		send_buffer(result, strlen(result)+1);
		erl_free(result);
	    } else {
		send_term(NULL);
	    }
#endif
	}
    }
}


/***********************************************************************
 *
 *	5.   M i s c e l l a n o u s   T e s t s
 *
 ***********************************************************************/

/*
 * Test some combinations of operations to verify that the reference pointers
 * are handled correctly.
 *
 * "Det verkar vara lite High Chaparal med minneshanteringen i erl_interface"
 * Per Lundgren, ERV.
 */

TESTCASE(high_chaparal)
{
    ETERM *L1, *A1, *L2, *A2, *L3;

    erl_init(NULL, 0);

    L1 = erl_mk_empty_list();
    A1 = erl_mk_atom("world");
    L2 = erl_cons(A1, L1);
    A2 = erl_mk_atom("hello");
    L3 = erl_cons(A2, L2);

    erl_free_term(L1);
    erl_free_term(A1);
    erl_free_term(L2);
    erl_free_term(A2);

    send_term(L3);

    /* already freed by send_term() */
    /* erl_free_term(L3);*/

    report(1);
}

/*
 * Test erl_decode to recover from broken list data (OTP-7448)
 */
TESTCASE(broken_data)
{
    ETERM* original;
    ETERM* new_terms;
    char encoded[16*1024];
    int n;

    erl_init(NULL, 0);
    original = all_types();
    if ((n=erl_encode(original, encoded)) == 0) 
    {
	fail("failed to encode terms");
    } else 
    {
        int offs = n/2;
        memset(encoded+offs,0,n-offs); /* destroy */

        if ((new_terms = erl_decode(encoded)) != NULL)
	{
	    fail("decode accepted broken data");
	    erl_free_term(new_terms);
	}
    }
    erl_free_term(original);
    report(1);
}

/*
 * Returns a list containing instances of all types.
 *
 * Be careful changing the contents of the list returned, because both
 * the build_terms() and decode_terms() test cases depend on it.
 */

static ETERM*
all_types(void)
{
    ETERM* t;
    ETERM* terms[3];
    int i;
    static char a_binary[] = "A binary";

#define CONS_AND_FREE(expr, tail) \
  do { \
    ETERM* term = expr; \
    ETERM* nl = erl_cons(term, tail); \
    erl_free_term(term); \
    erl_free_term(tail); \
    tail = nl; \
  } while (0)

    t = erl_mk_empty_list();

    CONS_AND_FREE(erl_mk_atom("I am an atom"), t);
    CONS_AND_FREE(erl_mk_binary("A binary", sizeof(a_binary)-1), t);
    CONS_AND_FREE(erl_mk_float(3.0), t);
    CONS_AND_FREE(erl_mk_int(0), t);
    CONS_AND_FREE(erl_mk_int(-1), t);
    CONS_AND_FREE(erl_mk_int(1), t);

    CONS_AND_FREE(erl_mk_string("A string"), t);

    terms[0] = erl_mk_atom("element1");
    terms[1] = erl_mk_int(42);
    terms[2] = erl_mk_int(767);
    CONS_AND_FREE(erl_mk_tuple(terms, ASIZE(terms)), t);
    for (i = 0; i < ASIZE(terms); i++) {
	erl_free_term(terms[i]);
    }

    CONS_AND_FREE(erl_mk_pid("kalle@localhost", 3, 2, 1), t);
    CONS_AND_FREE(erl_mk_pid("abcdefghijabcdefghij@localhost", 3, 2, 1), t);
    CONS_AND_FREE(erl_mk_port("kalle@localhost", 4, 1), t);
    CONS_AND_FREE(erl_mk_port("abcdefghijabcdefghij@localhost", 4, 1), t);
    CONS_AND_FREE(erl_mk_ref("kalle@localhost", 6, 1), t);
    CONS_AND_FREE(erl_mk_ref("abcdefghijabcdefghij@localhost", 6, 1), t);
    return t;

#undef CONS_AND_FREE
}

/*
 * Dump (print for debugging) a term. Useful if/when things go wrong.
 */
void
dump_term (FILE *fp, ETERM *t)
{
    if (fp == NULL) return;

    fprintf(fp, "#<%p ", t);

    if(t != NULL)
    {
	fprintf(fp, "count:%d, type:%d", ERL_COUNT(t), ERL_TYPE(t));

	switch(ERL_TYPE(t))
	{
	case ERL_UNDEF:
	    fprintf(fp, "==undef");
	    break;
	case ERL_INTEGER:
	    fprintf(fp, "==int, val:%d", ERL_INT_VALUE(t));
	    break;
	case ERL_U_INTEGER:
	    fprintf(fp, "==uint, val:%u", ERL_INT_UVALUE(t));
	    break;
	case ERL_FLOAT:
	    fprintf(fp, "==float, val:%g", ERL_FLOAT_VALUE(t));
	    break;
	case ERL_ATOM:
	    fprintf(fp, "==atom, name:%p \"%s\"", 
		    ERL_ATOM_PTR(t), ERL_ATOM_PTR(t));
	    break;
	case ERL_BINARY:
	    fprintf(fp, "==binary, data:%p,%u",
		    ERL_BIN_PTR(t), ERL_BIN_SIZE(t));
	    break;
	case ERL_PID:
	    fprintf(fp, "==pid, node:%p \"%s\"",
		    ERL_PID_NODE(t), ERL_PID_NODE(t));
	    break;
	case ERL_PORT:
	    fprintf(fp, "==port, node:%p \"%s\"",
		    ERL_PORT_NODE(t), ERL_PORT_NODE(t));
	    break;
	case ERL_REF:
	    fprintf(fp, "==ref, node:%p \"%s\"",
		    ERL_REF_NODE(t), ERL_REF_NODE(t));
	    break;
	case ERL_CONS:
	    fprintf(fp, "==cons");
	    fprintf(fp, ", car:");
	    dump_term(fp, ERL_CONS_HEAD(t));
	    fprintf(fp, ", cdr:");
	    dump_term(fp, ERL_CONS_TAIL(t));
	    break;
	case ERL_NIL:
	    fprintf(fp, "==nil");
	    break;
	case ERL_TUPLE:
	    fprintf(fp, "==tuple, elems:%p,%u", 
		    ERL_TUPLE_ELEMS(t), ERL_TUPLE_SIZE(t));
	    {
		size_t i;
		for(i = 0; i < ERL_TUPLE_SIZE(t); i++)
		{
		    fprintf(fp, "elem[%u]:", i);
		    dump_term(fp, ERL_TUPLE_ELEMENT(t, i));		    
		}
	    }
	    break;
	case ERL_VARIABLE:
	    fprintf(fp, "==variable, name:%p \"%s\"",
		    ERL_VAR_NAME(t), ERL_VAR_NAME(t));
	    fprintf(fp, ", value:");
	    dump_term(fp, ERL_VAR_VALUE(t));	    
	    break;

	default:
	    break;
	}
    }
    fprintf(fp, ">");
}

