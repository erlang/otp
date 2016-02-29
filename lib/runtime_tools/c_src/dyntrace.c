/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2016. All Rights Reserved.
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

/*
 * Purpose:  Dynamically loadable NIF library for DTrace
 */



#include "erl_nif.h"
#include "config.h"
#include "sys.h"
#include "dtrace-wrapper.h"
#if defined(USE_DYNAMIC_TRACE) && (defined(USE_DTRACE) || defined(USE_SYSTEMTAP))
#  define HAVE_USE_DTRACE 1
#endif
#if defined(USE_LTTNG)
#  define HAVE_USE_LTTNG 1
#  define TRACEPOINT_DEFINE
#  define TRACEPOINT_CREATE_PROBES
#  include "dyntrace_lttng.h"
#endif

void dtrace_nifenv_str(ErlNifEnv *env, char *process_buf);
void get_string_maybe(ErlNifEnv *env, const ERL_NIF_TERM term, char **ptr, char *buf, int bufsiz);
#ifdef HAVE_USE_DTRACE
ERL_NIF_TERM erl_nif_user_trace_s1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM erl_nif_user_trace_i4s4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#endif

#ifdef VALGRIND
    #  include <valgrind/memcheck.h>
#endif

#ifdef __GNUC__
    #  define INLINE __inline__
#else
    #  define INLINE
#endif

#define MESSAGE_BUFSIZ 1024

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

/* The NIFs: */
static ERL_NIF_TERM available(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM user_trace_s1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM user_trace_i4s4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM user_trace_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"available", 0, available},
    {"user_trace_s1", 1, user_trace_s1},
    {"user_trace_i4s4", 9, user_trace_i4s4},
    {"user_trace_n", 10, user_trace_n}
    {"enabled", 3, enabled},
    {"trace", 5, trace},
    {"trace", 6, trace},
    {"trace_procs", 6, trace_procs},
    {"trace_send", 6, trace_send},
    {"trace_receive", 6, trace_receive},
    {"trace_garbage_collection", 6, trace_garbage_collection}
};

ERL_NIF_INIT(dyntrace, nif_funcs, load, NULL, NULL, NULL)

static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_not_available;
static ERL_NIF_TERM atom_badarg;
static ERL_NIF_TERM atom_ok;

static ERL_NIF_TERM atom_trace;
static ERL_NIF_TERM atom_remove;
static ERL_NIF_TERM atom_discard;

/* gc atoms */

static ERL_NIF_TERM atom_gc_start;
static ERL_NIF_TERM atom_gc_end;


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_true = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    atom_error = enif_make_atom(env,"error");
    atom_not_available = enif_make_atom(env,"not_available");
    atom_badarg = enif_make_atom(env,"badarg");
    atom_ok = enif_make_atom(env,"ok");

    atom_trace = enif_make_atom(env,"trace");
    atom_remove = enif_make_atom(env,"remove");
    atom_discard = enif_make_atom(env,"discard");

    atom_gc_start = enif_make_atom(env,"gc_start");
    atom_gc_end = enif_make_atom(env,"gc_end");

    return 0;
}

static ERL_NIF_TERM available(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    return atom_true;
#else
    return atom_false;
#endif
}

static ERL_NIF_TERM user_trace_s1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    return erl_nif_user_trace_s1(env, argc, argv);
#else
    return atom_error;
#endif
}

static ERL_NIF_TERM user_trace_i4s4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    return erl_nif_user_trace_i4s4(env, argc, argv);
#else
    return atom_error;
#endif
}

static ERL_NIF_TERM user_trace_n(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_USE_DTRACE
    return erl_nif_user_trace_n(env, argc, argv);
#else
    return atom_error;
#endif
}

static ERL_NIF_TERM enabled(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
    ErlNifPid to;
    ASSERT(argc == 3);
    /* Only generate trace events when probe is enabled */
    if (argv[0] == atom_seq_trace) {
        if (!erlang_trace_seq_enabled())
            return atom_discard;
    } else if (!erlang_trace_enabled()) {
        return atom_discard;
    }

    return atom_trace;
#elif HAVE_USE_LTTNG
    /*
    int i;
    erts_fprintf(stderr, "enabled:\r\n");
    for (i = 0; i < argc; i++) {
        erts_fprintf(stderr, "  %T\r\n", argv[i]);
    }
    */
    return atom_trace;
#endif
    return atom_remove;
}

static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#define BUFF_SIZE 1024
    size_t sz = BUFF_SIZE;
    char buff[BUFF_SIZE];
    ASSERT(argc == 6);

    if (argv[0] == atom_seq_trace) {
        if (erlang_trace_seq_enabled()) {
            char *label = buff,
                *seq_info = buff + BUFF_SIZE/4;
            erts_snprintf(label, 1*BUFF_SIZE/4, "%T", argv[2]);
            erts_snprintf(seq_info, 3*BUFF_SIZE/4, "%T", argv[3]);
            erlang_trace_seq(label, seq_info);
        }
    } else {
        char *event, p[DTRACE_TERM_BUF_SIZE], state, arg1, arg2, arg3;

        event = buff + BUFF_SIZE - sz;
        sz -= enif_get_atom(env, argv[0], event, sz, ERL_NIF_LATIN1);

        state = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(state, sz, "%T", argv[1]);

        if (enif_is_pid(argv[2]) || enif_is_port(argv[2]))
            dtrace_pid_str(argv[2], p);
        else
            p = NULL;

        arg1 = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(arg1, sz, "%T", argv[3]);

        if (argc == 6) {
            arg2 = buff + BUFF_SIZE - sz;
            sz -= erts_snprintf(arg2, sz, "%T", argv[4]);
        } else
            args2 = NULL;

        erlang_trace(p, event, state, arg1, arg2);

    }
#elif HAVE_USE_LTTNG
    int i;
    erts_fprintf(stderr, "trace:\r\n");
    for (i = 0; i < argc; i++) {
        erts_fprintf(stderr, "  %T\r\n", argv[i]);
    }
#endif
    return atom_ok;
}
static ERL_NIF_TERM trace_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#elif HAVE_USE_LTTNG
    lttng_decl_procbuf(pid);

    lttng_pid_to_str(argv[2], pid);

    if (argv[0] == atom_gc_start) {
        LTTNG2(gc_minor_start, pid, 0);
    } else if (argv[0] == atom_gc_end) {
        LTTNG2(gc_minor_end, pid, 0);
    } else {
        int i;
        erts_fprintf(stderr, "trace send:\r\n");
        for (i = 0; i < argc; i++) {
            erts_fprintf(stderr, "  %T\r\n", argv[i]);
        }
    }
#endif
    return atom_ok;
}


static ERL_NIF_TERM trace_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#define BUFF_SIZE 1024
    size_t sz = BUFF_SIZE;
    char buff[BUFF_SIZE];
    ASSERT(argc == 6);

    if (argv[0] == atom_seq_trace) {
        if (erlang_trace_seq_enabled()) {
            char *label = buff,
                *seq_info = buff + BUFF_SIZE/4;
            erts_snprintf(label, 1*BUFF_SIZE/4, "%T", argv[2]);
            erts_snprintf(seq_info, 3*BUFF_SIZE/4, "%T", argv[3]);
            erlang_trace_seq(label, seq_info);
        }
    } else {
        char *event, p[DTRACE_TERM_BUF_SIZE], state, arg1, arg2, arg3;

        event = buff + BUFF_SIZE - sz;
        sz -= enif_get_atom(env, argv[0], event, sz, ERL_NIF_LATIN1);

        state = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(state, sz, "%T", argv[1]);

        if (enif_is_pid(argv[2]) || enif_is_port(argv[2]))
            dtrace_pid_str(argv[2], p);
        else
            p = NULL;

        arg1 = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(arg1, sz, "%T", argv[3]);

        if (argc == 6) {
            arg2 = buff + BUFF_SIZE - sz;
            sz -= erts_snprintf(arg2, sz, "%T", argv[4]);
        } else
            args2 = NULL;

        erlang_trace(p, event, state, arg1, arg2);

    }
#elif HAVE_USE_LTTNG
    int i;
    erts_fprintf(stderr, "trace:\r\n");
    for (i = 0; i < argc; i++) {
        erts_fprintf(stderr, "  %T\r\n", argv[i]);
    }
#endif
    return atom_ok;
}


static ERL_NIF_TERM trace_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#define BUFF_SIZE 1024
    size_t sz = BUFF_SIZE;
    char buff[BUFF_SIZE];
    ASSERT(argc == 6);

    if (argv[0] == atom_seq_trace) {
        if (erlang_trace_seq_enabled()) {
            char *label = buff,
                *seq_info = buff + BUFF_SIZE/4;
            erts_snprintf(label, 1*BUFF_SIZE/4, "%T", argv[2]);
            erts_snprintf(seq_info, 3*BUFF_SIZE/4, "%T", argv[3]);
            erlang_trace_seq(label, seq_info);
        }
    } else {
        char *event, p[DTRACE_TERM_BUF_SIZE], state, arg1, arg2, arg3;

        event = buff + BUFF_SIZE - sz;
        sz -= enif_get_atom(env, argv[0], event, sz, ERL_NIF_LATIN1);

        state = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(state, sz, "%T", argv[1]);

        if (enif_is_pid(argv[2]) || enif_is_port(argv[2]))
            dtrace_pid_str(argv[2], p);
        else
            p = NULL;

        arg1 = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(arg1, sz, "%T", argv[3]);

        if (argc == 6) {
            arg2 = buff + BUFF_SIZE - sz;
            sz -= erts_snprintf(arg2, sz, "%T", argv[4]);
        } else
            args2 = NULL;

        erlang_trace(p, event, state, arg1, arg2);

    }
#elif HAVE_USE_LTTNG
    int i;
    erts_fprintf(stderr, "trace:\r\n");
    for (i = 0; i < argc; i++) {
        erts_fprintf(stderr, "  %T\r\n", argv[i]);
    }
#endif
    return atom_ok;
}

static ERL_NIF_TERM trace_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#define BUFF_SIZE 1024
    size_t sz = BUFF_SIZE;
    char buff[BUFF_SIZE];
    ASSERT(argc == 6);

    if (argv[0] == atom_seq_trace) {
        if (erlang_trace_seq_enabled()) {
            char *label = buff,
                *seq_info = buff + BUFF_SIZE/4;
            erts_snprintf(label, 1*BUFF_SIZE/4, "%T", argv[2]);
            erts_snprintf(seq_info, 3*BUFF_SIZE/4, "%T", argv[3]);
            erlang_trace_seq(label, seq_info);
        }
    } else {
        char *event, p[DTRACE_TERM_BUF_SIZE], state, arg1, arg2, arg3;

        event = buff + BUFF_SIZE - sz;
        sz -= enif_get_atom(env, argv[0], event, sz, ERL_NIF_LATIN1);

        state = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(state, sz, "%T", argv[1]);

        if (enif_is_pid(argv[2]) || enif_is_port(argv[2]))
            dtrace_pid_str(argv[2], p);
        else
            p = NULL;

        arg1 = buff + BUFF_SIZE - sz;
        sz -= erts_snprintf(arg1, sz, "%T", argv[3]);

        if (argc == 6) {
            arg2 = buff + BUFF_SIZE - sz;
            sz -= erts_snprintf(arg2, sz, "%T", argv[4]);
        } else
            args2 = NULL;

        erlang_trace(p, event, state, arg1, arg2);

    }
#elif HAVE_USE_LTTNG
    int i;
    erts_fprintf(stderr, "trace:\r\n");
    for (i = 0; i < argc; i++) {
        erts_fprintf(stderr, "  %T\r\n", argv[i]);
    }
#endif
    return atom_ok;
}

