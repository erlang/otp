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
send_printed_buf(ei_x_buff* x)
{
    char* b = NULL;
    char fn[256];
    char *tmp = getenv("temp");
    FILE* f;
    int n, index = 0, ver;

#ifdef VXWORKS
    tmp = ".";
#else
    if (tmp == NULL) {
        tmp = "/tmp";
    }
#endif
    strcpy(fn, tmp);
    strcat(fn, "/ei_print_test.txt");
    f = fopen(fn, "w+");
    ei_decode_version(x->buff, &index, &ver);
    n = ei_print_term(f, x->buff, &index);
    if (n < 0) {
        fclose(f);
        x->index = 0;
        ei_x_format(x, "~s", "ERROR: term decoding failed");
        send_bin_term(x);
    } else {
        fseek(f, 0, SEEK_SET);
        b = malloc(n+1);
        fread(b, 1, n, f);
        b[n] = '\0';
        fclose(f);
        x->index = 0;
        ei_x_format(x, "~s", b);
        send_bin_term(x);
        free(b);
    }
}


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
    send_printed_buf(&x);
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
    ei_init();

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
    ei_init();

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

    ei_init();

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

    ei_init();

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

TESTCASE(maps)
{
    ei_x_buff x;

    ei_init();

    ei_x_new_with_version(&x);
    ei_x_encode_map_header(&x, 0);
    send_printed_buf(&x);
    ei_x_free(&x);

    ei_x_new_with_version(&x);
    ei_x_encode_map_header(&x, 1);
    ei_x_encode_atom(&x, "key");
    ei_x_encode_atom(&x, "value");
    send_printed_buf(&x);
    ei_x_free(&x);

    ei_x_new_with_version(&x);
    ei_x_encode_map_header(&x, 2);
    ei_x_encode_atom(&x, "key");
    ei_x_encode_atom(&x, "value");
    ei_x_encode_atom(&x, "another_key");
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, "ok");
    ei_x_encode_long(&x, 42L);
    send_printed_buf(&x);
    ei_x_free(&x);

    report(1);
}

TESTCASE(funs)
{
    ei_x_buff x;
    erlang_pid self;
    erlang_fun fun;

    strcpy(self.node, "node@host");
    self.num = 9;
    self.serial = 99;
    self.creation = 1;

    ei_init();

    ei_x_new_with_version(&x);
    fun.arity = -1;             /* Will encode as FUN_EXT */
    strcpy(fun.module, "some_module");
    fun.type = EI_FUN_CLOSURE;
    fun.u.closure.pid = self;
    fun.u.closure.index = fun.u.closure.old_index = 42;
    fun.u.closure.uniq = 0xDEADBEEF;
    fun.u.closure.n_free_vars = 0;
    fun.u.closure.free_var_len = 0;
    ei_x_encode_fun(&x, &fun);
    send_printed_buf(&x);
    ei_x_free(&x);

    ei_x_new_with_version(&x);
    fun.arity = 0;              /* Will encode as NEW_FUN_EXT */
    strcpy(fun.module, "some_module");
    fun.type = EI_FUN_CLOSURE;
    fun.u.closure.pid = self;
    fun.u.closure.index = fun.u.closure.old_index = 37;
    fun.u.closure.uniq = 0xBADBEEF;
    fun.u.closure.n_free_vars = 0;
    fun.u.closure.free_var_len = 0;
    ei_x_encode_fun(&x, &fun);
    send_printed_buf(&x);
    ei_x_free(&x);

    ei_x_new_with_version(&x);
    fun.arity = 1;
    strcpy(fun.module, "erlang");
    fun.type = EI_FUN_EXPORT;
    fun.u.exprt.func = "abs";
    ei_x_encode_fun(&x, &fun);
    send_printed_buf(&x);
    ei_x_free(&x);

    report(1);
}


TESTCASE(binaries)
{
    char *buf;
    long len;
    int err, n, index;
    ei_x_buff x;

    ei_init();

    for (n = 5; n; n--) {
        buf = read_packet(NULL);

        index = 0;
        err = ei_decode_version(buf, &index, NULL);
        if (err != 0)
            fail1("ei_decode_version returned %d", err);
        err = ei_decode_binary(buf, &index, NULL, &len);
        if (err != 0)
            fail1("ei_decode_binary returned %d", err);

        ei_x_new(&x);
        ei_x_append_buf(&x, buf, index);
        send_printed_buf(&x);
        ei_x_free(&x);

        free_packet(buf);
    }
    report(1);
}

TESTCASE(bitstrings)
{
    char *buf;
    long len;
    int err, n, index;
    ei_x_buff x;

    ei_init();

    for (n = 7; n; n--) {
        buf = read_packet(NULL);

        index = 0;
        err = ei_decode_version(buf, &index, NULL);
        if (err != 0)
            fail1("ei_decode_version returned %d", err);
        err = ei_decode_bitstring(buf, &index, NULL, NULL, NULL);
        if (err != 0)
            fail1("ei_decode_bitstring returned %d", err);

        ei_x_new(&x);
        ei_x_append_buf(&x, buf, index);
        send_printed_buf(&x);
        ei_x_free(&x);

        free_packet(buf);
    }
    report(1);
}
