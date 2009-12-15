/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

#include "ei.h"

typedef void (*TestCase)(void);

#define TESTCASE(name) void name(void)
#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

void run_tests(char* argv0, TestCase cases[], unsigned number);

#ifndef _MSC_VER
#  define ll(val) (val##LL)
#else /* assume gcc or C99 */
#  define ll(val) (val##i64)
#endif

#ifndef _MSC_VER
#  define ull(val) (val##LL)
#else /* assume gcc or C99 */
#  define ull(val) (val##i64)
#endif

/*
 * Reading.
 */

int get_bin_term(ei_x_buff* x, ei_term* term);
char *read_packet(int *len);

/*
 * Sending replies.
 */

#define fail(reason) do_fail(__FILE__, __LINE__, reason)
#define report(ok) do_report(__FILE__, __LINE__, ok)

void do_report(char* file, int line, int ok);
void do_fail(char* file, int line, char* reason);
void send_buffer(char* buf, int size);
void message(char* format, ...);

void send_bin_term(ei_x_buff* x);

