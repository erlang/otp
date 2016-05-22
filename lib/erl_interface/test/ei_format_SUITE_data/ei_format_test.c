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

#ifdef VXWORKS
#include "reclaim.h"
#endif

#include "ei_runner.h"
#include <string.h>

/*
 * Purpose: Tests the ei_format() function.
 * Author:  Jakob 
 */

static void
send_format2(char* format, char* p)
{
    ei_x_buff x;
    ei_x_new(&x);
    ei_x_format(&x, format, p);
    send_bin_term(&x);
    free(x.buff);
}

static void
send_format(char* format)
{
    send_format2(format, NULL);
}

TESTCASE(atoms)
{
    send_format("''");
    send_format("'a'");
    send_format("'A'");
    send_format("'abc'");
    send_format("'Abc'");
    send_format("'ab@c'");
    send_format("'The rain in Spain stays mainly in the plains'");

    send_format("a");
    send_format("ab");
    send_format("abc");
    send_format("ab@c");
    send_format("   abcdefghijklmnopq   ");

    send_format2("~a", "");
    send_format2("~a", "a");
    send_format2("~a", "A");
    send_format2("~a", "abc");
    send_format2("~a", "Abc");
    send_format2("~a", "ab@c");
    send_format2("~a", "The rain in Spain stays mainly in the plains");

    send_format2("~a", "a");
    send_format2("~a", "ab");
    send_format2("~a", "abc");
    send_format2("~a","ab@c");
    send_format2("~a", "   abcdefghijklmnopq   ");


    report(1);
}

TESTCASE(tuples)
{
    send_format("{}");
    send_format("{a}");
    send_format("{a, b}");
    send_format("{a, b, c}");
    send_format("{1}");
    send_format("{[]}");
    send_format("{[], []}");
    send_format("{[], a, b, c}");
    send_format("{[], a, [], b, c}");
    send_format("{[], a, '', b, c}");

    report(1);
}



TESTCASE(lists)
{
/* FIXME cases to add?
    ETERM* a;
    ETERM* b;
    ETERM* c;
*/
    ei_x_buff x;
    static char str[65537];

    send_format("[]");
    send_format("[a]");
    send_format("[a, b]");
    send_format("[a, b, c]");
    send_format("[1]");
    send_format("[[]]");
    send_format("[[], []]");
    send_format("[[], a, b, c]");
    send_format("[[], a, [], b, c]");
    send_format("[[], a, '', b, c]");
    send_format("[[x, 2], [y, 3], [z, 4]]");
    send_format("[{a,b},{c,d}]"); /* OTP-4777 */

    ei_x_new(&x);
/*
    b = erl_format("[{addr, ~s, ~i}]", "E-street", 42);
    a = ei_format(x, "[{name, ~a}, {age, ~i}, {data, ~w}]", "Madonna", 21, b);
    send_bin_term(a);
    erl_free_term(b);*/
    ei_x_format(&x, "[{pi, ~f}, {'cos(70)', ~f}]", (float)3.1415, (float)0.34202);
    send_bin_term(&x);
    x.index = 0;        /* otherwise it'll send the previous term again */
    ei_x_format(&x, "[[pi, ~d], ['cos(70)', ~d]]", 3.1415, 0.34202);
    send_bin_term(&x);

/*    a = erl_mk_float(3.1415);
    b = erl_mk_float(0.34202);
    send_bin_term(ei_format("[[pi, ~w], ['cos(70)', ~w]]", a, b));
    erl_free_term(a);
    erl_free_term(b);

    a = erl_mk_float(3.1415);
    b = erl_mk_float(0.34202);
    c = erl_mk_empty_list();
    send_bin_term(ei_format("[[~a, ~w], ~w, [~s, ~w]]", "pi", a, c, "cos(70)", b));
    erl_free_term(a);
    erl_free_term(b);
    erl_free_term(c);
*/
    x.index = 0;        /* otherwise it'll send the previous term again */
    ei_x_format(&x, "[~i]", -1);
    send_bin_term(&x);

    x.index = 0;
    ei_x_format(&x, "~s","hejsan");
    send_bin_term(&x);
    
    memset(str,'A',65535);
    str[65535] = '\0';
    str[65536] = '\0';
    x.index = 0;
    ei_x_format(&x, "~s",str);
    send_bin_term(&x);
    str[65535] = 'A';
    x.index = 0;
    ei_x_format(&x, "~s",str);
    send_bin_term(&x);


    free(x.buff);
    report(1);
}

TESTCASE(format_wo_ver) {
/* OTP-6795 
 * make example with format_wo_ver 
 */
    ei_x_buff x;
    
    ei_x_new (&x);
    ei_x_format(&x, "[-1, +2, ~c, {~a,~s},{~a,~i}]", 'c', "a", "b", "c", 10);
    send_bin_term(&x);

    free(x.buff);
    report(1);
}
