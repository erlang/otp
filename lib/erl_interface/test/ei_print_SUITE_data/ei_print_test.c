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

#include "ei_runner.h"

#include <string.h>
#include <stdlib.h>

/*
 * Purpose: Tests the ei_print() function.
 * Author:  Jakob 
 */

static void
send_printed3(char* format, char* p1, char* p2, int fl)
{
    char* b = NULL;
    char fn[100], * tmp = getenv("temp");
    FILE* f;
    int n, index = 0, ver;
    ei_x_buff x;

    ei_x_new(&x);
    if (fl) {
	ei_x_format(&x, format, *(float*)p1, *(float*)p2);
    } else {
	ei_x_format(&x, format, p1, p2);
    }
#ifdef VXWORKS
    tmp = ".";
#else
    if (tmp == NULL) tmp = "/tmp";
#endif
    strcpy(fn, tmp);
    strcat(fn, "/ei_print_test.txt");
    f = fopen(fn, "w+");
    ei_decode_version(x.buff, &index, &ver);
    n = ei_print_term(f, x.buff, &index);
    fseek(f, 0, SEEK_SET);
    b = malloc(n+1);
    fread(b, 1, n, f);
    b[n] = '\0';
    fclose(f);
    x.index = 0;
    ei_x_format(&x, "~s", b);
    send_bin_term(&x);
    free(b);
    ei_x_free(&x);
}

static void
send_printed(char* format)
{
    send_printed3(format, NULL, NULL, 0);
}

static void
send_printed2(char* format, char* p)
{
    send_printed3(format, p, NULL, 0);
}

static void send_printed3f(char* format, float f1, float f2)
{
    send_printed3(format, (char*)&f1, (char*)&f2, 1);
}

TESTCASE(atoms)
{
    send_printed("''");
    send_printed("'a'");
    send_printed("'A'");
    send_printed("'abc'");
    send_printed("'Abc'");
    send_printed("'ab@c'");
    send_printed("'The rain in Spain stays mainly in the plains'");

    send_printed("a");
    send_printed("ab");
    send_printed("abc");
    send_printed("ab@c");
    send_printed("   abcdefghijklmnopq   ");

    send_printed2("~a", "");
    send_printed2("~a", "a");
    send_printed2("~a", "A");
    send_printed2("~a", "abc");
    send_printed2("~a", "Abc");
    send_printed2("~a", "ab@c");
    send_printed2("~a", "The rain in Spain stays mainly in the plains");

    send_printed2("~a", "a");
    send_printed2("~a", "ab");
    send_printed2("~a", "abc");
    send_printed2("~a","ab@c");
    send_printed2("~a", "   abcdefghijklmnopq   ");


    report(1);
}

TESTCASE(tuples)
{
    send_printed("{}");
    send_printed("{a}");
    send_printed("{a, b}");
    send_printed("{a, b, c}");
    send_printed("{1}");
    send_printed("{[]}");
    send_printed("{[], []}");
    send_printed("{[], a, b, c}");
    send_printed("{[], a, [], b, c}");
    send_printed("{[], a, '', b, c}");

    report(1);
}



TESTCASE(lists)
{
    ei_x_buff x;

    send_printed("[]");
    send_printed("[a]");
    send_printed("[a, b]");
    send_printed("[a, b, c]");
    send_printed("[1]");
    send_printed("[[]]");
    send_printed("[[], []]");
    send_printed("[[], a, b, c]");
    send_printed("[[], a, [], b, c]");
    send_printed("[[], a, '', b, c]");
    send_printed("[[x, 2], [y, 3], [z, 4]]");

    /* more tests needed */
    send_printed3f("[{pi, ~f}, {'cos(70)', ~f}]",
		  (float)3.1415, (float)0.34202);
    send_printed3f("[[pi, ~f], ['cos(70)', ~f]]",
		  (float)3.1415, (float)0.34202);

    send_printed2("[~i]", (char*)-1);
    report(1);
}

TESTCASE(strings)
{
    ei_x_buff x;

    send_printed("\"\n\"");
    send_printed("\"\r\n\"");
    send_printed("\"a\"");
    send_printed("\"A\"");
    send_printed("\"0\"");
    send_printed("\"9\"");
    send_printed("\"The rain in Spain stays mainly in the plains\"");
    send_printed("\"   abcdefghijklmnopq   \"");

    report(1);
}


