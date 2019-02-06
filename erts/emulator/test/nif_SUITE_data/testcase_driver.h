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

#ifndef TESTCASE_DRIVER_H__
#define TESTCASE_DRIVER_H__

#include <erl_nif.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
    char *testcase_name;
    char *command;
    int command_len;
    void *extra;
} TestCaseState_t;

#define ASSERT_CLNUP(TCS, B, CLN)					\
do {									\
    if (!(B)) {								\
	CLN;								\
	testcase_assertion_failed((TCS), __FILE__, __LINE__, #B);	\
    }									\
} while (0)

#define ASSERT(TCS, B) ASSERT_CLNUP(TCS, B, (void) 0)


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
