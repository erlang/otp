#include <erl_nif.h>

#include <stdio.h>
#include <stdarg.h>

void testcase_printf(TestCaseState_t *tcs, char *frmt, ...)
{
    va_list va;
    va_start(va, frmt);
    vfprintf(stderr, frmt, va);
    va_end(va);
    fprintf(stderr, "\r");
}

void testcase_succeeded(TestCaseState_t *tcs, char *frmt, ...)
{
}

void testcase_skipped(TestCaseState_t *tcs, char *frmt, ...)
{
}

void testcase_failed(TestCaseState_t *tcs, char *frmt, ...)
{
    va_list va;
    va_start(va, frmt);
    vfprintf(stderr, frmt, va);
    va_end(va);
    abort();
}

int testcase_assertion_failed(TestCaseState_t *tcs, char *file, int line,
                              char *assertion)
{
    testcase_failed(tcs, "ASSERTION '%s' FAILED at %s:%d\r\n",
		    assertion, file, line);
    abort();
    return 0; /*?*/
}

void *testcase_alloc(size_t size)
{
    return malloc(size);
}
void *testcase_realloc(void *ptr, size_t size)
{
    return realloc(ptr, size);
}
void testcase_free(void *ptr)
{
    free(ptr);
}

void testcase_run(TestCaseState_t *tcs);

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    testcase_run(NULL);
    testcase_cleanup(NULL);
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"run", 0, run}
};

ERL_NIF_INIT(tester,nif_funcs,NULL,NULL,upgrade,NULL)

