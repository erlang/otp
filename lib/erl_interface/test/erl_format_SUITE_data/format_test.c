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

#include "runner.h"

/*
 * Purpose: Tests the erl_format() function.
 * Author:  Bjorn Gustavsson
 */

static void
send_format(char* format)
{
    send_term(erl_format(format));
}

TESTCASE(atoms)
{
    erl_init(NULL, 0);

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

    send_term(erl_format("~a", ""));
    send_term(erl_format("~a", "a"));
    send_term(erl_format("~a", "A"));
    send_term(erl_format("~a", "abc"));
    send_term(erl_format("~a", "Abc"));
    send_term(erl_format("~a", "ab@c"));
    send_term(erl_format("~a", "The rain in Spain stays mainly in the plains"));

    send_term(erl_format("~a", "a"));
    send_term(erl_format("~a", "ab"));
    send_term(erl_format("~a", "abc"));
    send_term(erl_format("~a","ab@c"));
    send_term(erl_format("~a", "   abcdefghijklmnopq   "));


    report(1);
}

TESTCASE(tuples)
{
    erl_init(NULL, 0);

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
    ETERM* a;
    ETERM* b;
    ETERM* c;

    erl_init(NULL, 0);

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

    b = erl_format("[{addr, ~s, ~i}]", "E-street", 42);
    a = erl_format("[{name, ~a}, {age, ~i}, {data, ~w}]", "Madonna", 21, b);
    send_term(a);
    erl_free_term(b);

    send_term(erl_format("[{pi, ~f}, {'cos(70)', ~f}]", 3.1415, 0.34202));

    a = erl_mk_float(3.1415);
    b = erl_mk_float(0.34202);
    send_term(erl_format("[[pi, ~w], ['cos(70)', ~w]]", a, b));
    erl_free_term(a);
    erl_free_term(b);

    a = erl_mk_float(3.1415);
    b = erl_mk_float(0.34202);
    c = erl_mk_empty_list();
    send_term(erl_format("[[~a, ~w], ~w, [~s, ~w]]", "pi", a, c, "cos(70)", b));
    erl_free_term(a);
    erl_free_term(b);
    erl_free_term(c);

    send_term(erl_format("[~i]", -1));

    report(1);
}
