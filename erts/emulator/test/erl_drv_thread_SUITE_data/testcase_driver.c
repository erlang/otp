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

#include "testcase_driver.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include <string.h>

#ifdef __WIN32__
#undef HAVE_VSNPRINTF
#define HAVE_VSNPRINTF 1
#define vsnprintf _vsnprintf
#endif

#ifndef HAVE_VSNPRINTF
#define HAVE_VSNPRINTF 0
#endif

#define COMMENT_BUF_SZ 4096

#define TESTCASE_FAILED		0
#define TESTCASE_SKIPPED	1
#define TESTCASE_SUCCEEDED	2

typedef struct {
    TestCaseState_t visible;
    ErlDrvPort port;
    ErlDrvTermData port_id;
    int result;
    jmp_buf done_jmp_buf;
    char *comment;
    char comment_buf[COMMENT_BUF_SZ];
} InternalTestCaseState_t;

ErlDrvData testcase_drv_start(ErlDrvPort port, char *command);
void testcase_drv_stop(ErlDrvData drv_data);
void testcase_drv_run(ErlDrvData drv_data, char *buf, ErlDrvSizeT len);

static ErlDrvEntry testcase_drv_entry = { 
    NULL,
    testcase_drv_start,
    testcase_drv_stop,
    testcase_drv_run,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL,

};


DRIVER_INIT(testcase_drv)
{
    testcase_drv_entry.driver_name = testcase_name();
    return &testcase_drv_entry;
}

ErlDrvData
testcase_drv_start(ErlDrvPort port, char *command)
{
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *)
	driver_alloc(sizeof(InternalTestCaseState_t));
    if (!itcs) {
	return ERL_DRV_ERROR_GENERAL;
    }

    itcs->visible.testcase_name = testcase_name();
    itcs->visible.extra = NULL;
    itcs->port = port;
    itcs->port_id = driver_mk_port(port);
    itcs->result = TESTCASE_FAILED;
    itcs->comment = "";

    return (ErlDrvData) itcs;
}

void
testcase_drv_stop(ErlDrvData drv_data)
{
    testcase_cleanup((TestCaseState_t *) drv_data);
    driver_free((void *) drv_data);
}

void
testcase_drv_run(ErlDrvData drv_data, char *buf, ErlDrvSizeT len)
{
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *) drv_data;
    ErlDrvTermData result_atom;
    ErlDrvTermData msg[12];

    itcs->visible.command = buf;
    itcs->visible.command_len = len;

    if (setjmp(itcs->done_jmp_buf) == 0) {
	testcase_run((TestCaseState_t *) itcs);
	itcs->result = TESTCASE_SUCCEEDED;
    }

    switch (itcs->result) {
    case TESTCASE_SUCCEEDED:
	result_atom = driver_mk_atom("succeeded");
	break;
    case TESTCASE_SKIPPED:
	result_atom = driver_mk_atom("skipped");
	break;
    case TESTCASE_FAILED:
    default:
	result_atom = driver_mk_atom("failed");
	break;
    }

    msg[0] = ERL_DRV_ATOM;
    msg[1] = (ErlDrvTermData) result_atom;

    msg[2] = ERL_DRV_PORT;
    msg[3] = itcs->port_id;

    msg[4] = ERL_DRV_ATOM;
    msg[5] = driver_mk_atom(itcs->visible.testcase_name);
 
    msg[6] = ERL_DRV_STRING;
    msg[7] = (ErlDrvTermData) itcs->comment;
    msg[8] = (ErlDrvTermData) strlen(itcs->comment);

    msg[9] = ERL_DRV_TUPLE;
    msg[10] = (ErlDrvTermData) 4;

    erl_drv_output_term(itcs->port_id, msg, 11);
}

int
testcase_assertion_failed(TestCaseState_t *tcs,
			  char *file, int line, char *assertion)
{
    testcase_failed(tcs, "%s:%d: Assertion failed: \"%s\"",
		    file, line, assertion);
    return 0;
}

void
testcase_printf(TestCaseState_t *tcs, char *frmt, ...)
{
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *) tcs;
    ErlDrvTermData msg[12];
    va_list va;
    va_start(va, frmt);
#if HAVE_VSNPRINTF
    vsnprintf(itcs->comment_buf, COMMENT_BUF_SZ, frmt, va);
#else
    vsprintf(itcs->comment_buf, frmt, va);
#endif
    va_end(va);

    msg[0] = ERL_DRV_ATOM;
    msg[1] = (ErlDrvTermData) driver_mk_atom("print");

    msg[2] = ERL_DRV_PORT;
    msg[3] = itcs->port_id;

    msg[4] = ERL_DRV_ATOM;
    msg[5] = driver_mk_atom(itcs->visible.testcase_name);

    msg[6] = ERL_DRV_STRING;
    msg[7] = (ErlDrvTermData) itcs->comment_buf;
    msg[8] = (ErlDrvTermData) strlen(itcs->comment_buf);

    msg[9] = ERL_DRV_TUPLE;
    msg[10] = (ErlDrvTermData) 4;

    erl_drv_output_term(itcs->port_id, msg, 11);
}


void testcase_succeeded(TestCaseState_t *tcs, char *frmt, ...)
{
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *) tcs;
    va_list va;
    va_start(va, frmt);
#if HAVE_VSNPRINTF
    vsnprintf(itcs->comment_buf, COMMENT_BUF_SZ, frmt, va);
#else
    vsprintf(itcs->comment_buf, frmt, va);
#endif
    va_end(va);

    itcs->result = TESTCASE_SUCCEEDED;
    itcs->comment = itcs->comment_buf;

    longjmp(itcs->done_jmp_buf, 1);
}

void testcase_skipped(TestCaseState_t *tcs, char *frmt, ...)
{
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *) tcs;
    va_list va;
    va_start(va, frmt);
#if HAVE_VSNPRINTF
    vsnprintf(itcs->comment_buf, COMMENT_BUF_SZ, frmt, va);
#else
    vsprintf(itcs->comment_buf, frmt, va);
#endif
    va_end(va);

    itcs->result = TESTCASE_SKIPPED;
    itcs->comment = itcs->comment_buf;

    longjmp(itcs->done_jmp_buf, 1);
}

void testcase_failed(TestCaseState_t *tcs, char *frmt, ...)
{
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *) tcs;
    char buf[10];
    size_t bufsz = sizeof(buf);
    va_list va;
    va_start(va, frmt);
#if HAVE_VSNPRINTF
    vsnprintf(itcs->comment_buf, COMMENT_BUF_SZ, frmt, va);
#else
    vsprintf(itcs->comment_buf, frmt, va);
#endif
    va_end(va);

    itcs->result = TESTCASE_FAILED;
    itcs->comment = itcs->comment_buf;

    if (erl_drv_getenv("ERL_ABORT_ON_FAILURE", buf, &bufsz) == 0
	&& strcmp("true", buf) == 0) {
	fprintf(stderr, "Testcase \"%s\" failed: %s\n",
		itcs->visible.testcase_name, itcs->comment);
	abort();
    }

    longjmp(itcs->done_jmp_buf, 1);
}

void *testcase_alloc(size_t size)
{
    return driver_alloc(size);
}

void *testcase_realloc(void *ptr, size_t size)
{
    return driver_realloc(ptr, size);
}

void testcase_free(void *ptr)
{
    driver_free(ptr);
}
