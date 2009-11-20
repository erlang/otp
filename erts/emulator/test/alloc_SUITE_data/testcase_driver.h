/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef TESTCASE_DRIVER_H__
#define TESTCASE_DRIVER_H__

#include "erl_driver.h"
#include <stdlib.h>

typedef struct {
    char *testcase_name;
    char *command;
    int command_len;
    void *extra;
} TestCaseState_t;

#define ASSERT(TCS, B) \
  ((void) ((B) ? 1 : testcase_assertion_failed((TCS), __FILE__, __LINE__, #B)))


void testcase_printf(TestCaseState_t *tcs, char *frmt, ...);
void testcase_succeeded(TestCaseState_t *tcs, char *frmt, ...);
void testcase_skipped(TestCaseState_t *tcs, char *frmt, ...);
void testcase_failed(TestCaseState_t *tcs, char *frmt, ...);
int testcase_assertion_failed(TestCaseState_t *tcs, char *file, int line,
			      char *assertion);
void *testcase_alloc(size_t size);
void *testcase_realloc(void *ptr, size_t size);
void testcase_free(void *ptr);


char *testcase_name(void);
void testcase_run(TestCaseState_t *tcs);
void testcase_cleanup(TestCaseState_t *tcs);

#endif
