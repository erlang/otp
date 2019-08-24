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

#include "erl_interface.h"

typedef void (*TestCase)(void);

#define TESTCASE(name) void name(void)
#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

void run_tests(char* argv0, TestCase cases[], unsigned number);

/*
 * Reading.
 */

ETERM* get_term(void);
char *read_packet(int *len);

/*
 * Sending replies.
 */

#define fail(reason) do_fail(__FILE__, __LINE__, reason)
#define report(ok) do_report(__FILE__, __LINE__, ok)

void do_report(char* file, int line, int ok);
void do_fail(char* file, int line, char* reason);
void send_term(ETERM* term);
void send_buffer(char* buf, int size);
void message(char* format, ...);

void send_bin_term(ei_x_buff* x);

