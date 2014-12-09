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
#define TESTCASE_CONTINUE	3

typedef struct {
    TestCaseState_t visible;
    ErlNifEnv* curr_env;
    int result;
    jmp_buf done_jmp_buf;
    char *comment;
    char comment_buf[COMMENT_BUF_SZ];
} InternalTestCaseState_t;

ERL_NIF_TERM testcase_nif_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM testcase_nif_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM testcase_nif_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ErlNifFunc testcase_nif_funcs[] =
{
    {"start", 1, testcase_nif_start},
    {"run", 1, testcase_nif_run},
    {"stop", 1, testcase_nif_stop}
};

static ErlNifResourceType* testcase_rt;
static ERL_NIF_TERM print_atom;

int testcase_nif_init(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    testcase_rt = enif_open_resource_type(env, NULL, "testcase_rt", NULL,
					  ERL_NIF_RT_CREATE, NULL);

    print_atom = enif_make_atom(env, "print");
    return 0;
}

ERL_NIF_TERM
testcase_nif_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *)
             enif_alloc_resource(testcase_rt, sizeof(InternalTestCaseState_t));

    if (!itcs || !enif_get_int(env, argv[0], &itcs->visible.thr_nr)) {
	enif_make_badarg(env);
    }

    itcs->visible.testcase_name = testcase_name();

    itcs->visible.extra = NULL;
    itcs->result = TESTCASE_FAILED;
    itcs->comment = "";

    ret = enif_make_resource(env, itcs);
    enif_release_resource(itcs);
    return ret;
}

ERL_NIF_TERM
testcase_nif_stop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    InternalTestCaseState_t *itcs;
    if (!enif_get_resource(env, argv[0], testcase_rt, (void**)&itcs))
	return enif_make_badarg(env);
    testcase_cleanup(&itcs->visible);
    return enif_make_atom(env,"ok");
}

ERL_NIF_TERM
testcase_nif_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    InternalTestCaseState_t *itcs;
    const char* result_atom;

    if (!enif_get_resource(env, argv[0], testcase_rt, (void**)&itcs))
	return enif_make_badarg(env);

    itcs->curr_env = env;

    if (setjmp(itcs->done_jmp_buf) == 0) {
	testcase_run(&itcs->visible);
	itcs->result = TESTCASE_SUCCEEDED;
    }

    switch (itcs->result) {
    case TESTCASE_CONTINUE:
	return enif_make_atom(env, "continue");

    case TESTCASE_SUCCEEDED: result_atom = "succeeded"; break;
    case TESTCASE_SKIPPED: result_atom = "skipped"; break;
    case TESTCASE_FAILED: result_atom = "failed"; break;
    }

    return enif_make_tuple2(env, enif_make_atom(env, result_atom),
			    enif_make_string(env, itcs->comment, ERL_NIF_LATIN1));
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
    InternalTestCaseState_t* itcs = (InternalTestCaseState_t*)tcs;
    ErlNifPid pid;
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM msg;
    va_list va;
    va_start(va, frmt);
#if HAVE_VSNPRINTF
    vsnprintf(itcs->comment_buf, COMMENT_BUF_SZ, frmt, va);
#else
    vsprintf(itcs->comment_buf, frmt, va);
#endif
    va_end(va);

    msg = enif_make_tuple2(msg_env, print_atom,
		    enif_make_string(msg_env, itcs->comment_buf, ERL_NIF_LATIN1));

    enif_send(itcs->curr_env, enif_self(itcs->curr_env, &pid),
	      msg_env, msg);

    enif_free_env(msg_env);
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

void testcase_continue(TestCaseState_t *tcs)
{
    InternalTestCaseState_t *itcs = (InternalTestCaseState_t *) tcs;
    itcs->result = TESTCASE_CONTINUE;
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

    if (enif_getenv("ERL_ABORT_ON_FAILURE", buf, &bufsz) == 0
	&& strcmp("true", buf) == 0) {
	fprintf(stderr, "Testcase \"%s\" failed: %s\n",
		itcs->visible.testcase_name, itcs->comment);
	abort();
    }

    longjmp(itcs->done_jmp_buf, 1);
}

void *testcase_alloc(size_t size)
{
    return enif_alloc(size);
}

void *testcase_realloc(void *ptr, size_t size)
{
    return enif_realloc(ptr, size);
}

void testcase_free(void *ptr)
{
    enif_free(ptr);
}
