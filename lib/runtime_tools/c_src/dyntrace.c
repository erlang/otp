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
static ERL_NIF_TERM trace_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_running(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
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
    {"trace_ports", 6, trace_ports},
    {"trace_running", 6, trace_running},
    {"trace_call", 6, trace_call},
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

/* process 'procs' */

static ERL_NIF_TERM atom_spawn;
static ERL_NIF_TERM atom_exit;
static ERL_NIF_TERM atom_register;
static ERL_NIF_TERM atom_unregister;
static ERL_NIF_TERM atom_link;
static ERL_NIF_TERM atom_unlink;
static ERL_NIF_TERM atom_getting_linked;
static ERL_NIF_TERM atom_getting_unlinked;

/* process 'running' and 'exiting' */

static ERL_NIF_TERM atom_in;
static ERL_NIF_TERM atom_out;
static ERL_NIF_TERM atom_in_exiting;
static ERL_NIF_TERM atom_out_exiting;
static ERL_NIF_TERM atom_out_exited;

/* process messages 'send' and 'receive' */

static ERL_NIF_TERM atom_send;
static ERL_NIF_TERM atom_receive;
static ERL_NIF_TERM atom_send_to_non_existing_process;

/* ports 'ports' */

static ERL_NIF_TERM atom_open;
static ERL_NIF_TERM atom_closed;

/* 'call' */

static ERL_NIF_TERM atom_call;
static ERL_NIF_TERM atom_return_from;
static ERL_NIF_TERM atom_exception_from;

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

    /* gc */

    atom_gc_start = enif_make_atom(env,"gc_start");
    atom_gc_end = enif_make_atom(env,"gc_end");

    /* process 'proc' */

    atom_spawn = enif_make_atom(env,"spawn");
    atom_exit = enif_make_atom(env,"exit");
    atom_register = enif_make_atom(env,"register");
    atom_unregister = enif_make_atom(env,"unregister");
    atom_link = enif_make_atom(env,"link");
    atom_unlink = enif_make_atom(env,"unlink");
    atom_getting_unlinked = enif_make_atom(env,"getting_unlinked");
    atom_getting_linked = enif_make_atom(env,"getting_linked");

    /* process 'running' and 'exiting' */

    atom_in = enif_make_atom(env,"in");
    atom_out = enif_make_atom(env,"out");
    atom_in_exiting = enif_make_atom(env,"in_exiting");
    atom_out_exiting = enif_make_atom(env,"out_exiting");
    atom_out_exited = enif_make_atom(env,"out_exited");

    /* process messages 'send' and 'receive' */

    atom_send = enif_make_atom(env,"send");
    atom_receive = enif_make_atom(env,"receive");
    atom_send_to_non_existing_process = enif_make_atom(env,"send_to_non_existing_process");

    /* ports 'ports' */

    atom_open = enif_make_atom(env,"open");
    atom_closed = enif_make_atom(env,"closed");

    /* 'call' */

    atom_call = enif_make_atom(env,"call");
    atom_return_from = enif_make_atom(env,"return_from");
    atom_exception_from = enif_make_atom(env,"exception_from");

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

static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#elif HAVE_USE_LTTNG
    lttng_decl_procbuf(pid);
    unsigned int len;
    char undef[] = "undefined";

    lttng_pid_to_str(argv[2], pid);

    if (argv[0] == atom_call) {
        const ERL_NIF_TERM* tuple;
        int arity;
        lttng_decl_mfabuf(mfa);

        if (enif_get_tuple(env, argv[3], &arity, &tuple)) {
            if (enif_is_list(env, tuple[2])) {
                enif_get_list_length(env, tuple[2], &len);
            } else {
                enif_get_uint(env, tuple[2], &len);
            }
            lttng_mfa_to_str(tuple[0], tuple[1], len, mfa);
            LTTNG3(function_call, pid, mfa, 0);
        } else {
            LTTNG3(function_call, pid, undef, 0);
        }
    } else if (argv[0] == atom_return_from) {
        const ERL_NIF_TERM* tuple;
        int arity;
        lttng_decl_mfabuf(mfa);

        if (enif_get_tuple(env, argv[3], &arity, &tuple)) {
            enif_get_uint(env, tuple[2], &len);
            lttng_mfa_to_str(tuple[0], tuple[1], len, mfa);
            LTTNG3(function_return, pid, mfa, 0);
        } else {
            LTTNG3(function_return, pid, undef, 0);
        }
    } else if (argv[0] == atom_exception_from) {
        const ERL_NIF_TERM* tuple;
        int arity;
        lttng_decl_mfabuf(mfa);
        char class[LTTNG_BUFFER_SZ];

        enif_get_tuple(env, argv[4], &arity, &tuple);
        erts_snprintf(class, LTTNG_BUFFER_SZ, "%T", tuple[0]);

        if (enif_get_tuple(env, argv[3], &arity, &tuple)) {
            enif_get_uint(env, tuple[2], &len);
            lttng_mfa_to_str(tuple[0], tuple[1], len, mfa);
            LTTNG3(function_exception, pid, mfa, class);
        } else {
            LTTNG3(function_exception, pid, undef, class);
        }
    } else {
        int i;
        erts_fprintf(stderr, "trace call:\r\n");
        for (i = 0; i < argc; i++) {
            erts_fprintf(stderr, "  %T\r\n", argv[i]);
        }
    }
#endif
    return atom_ok;
}


static ERL_NIF_TERM trace_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#elif HAVE_USE_LTTNG
    lttng_decl_procbuf(pid);
    lttng_pid_to_str(argv[2], pid);

    if (argv[0] == atom_send) {
        lttng_decl_procbuf(to);
        char msg[LTTNG_BUFFER_SZ];

        lttng_pid_to_str(argv[4], to);
        erts_snprintf(msg, LTTNG_BUFFER_SZ, "%T", argv[3]);

        LTTNG3(message_send, pid, to, msg);
    } else if (argv[0] == atom_send_to_non_existing_process) {
        lttng_decl_procbuf(to);
        char msg[LTTNG_BUFFER_SZ];

        lttng_pid_to_str(argv[4], to);
        erts_snprintf(msg, LTTNG_BUFFER_SZ, "%T", argv[3]);
        /* mark it as non existing ? */

        LTTNG3(message_send, pid, to, msg);
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
#elif HAVE_USE_LTTNG
    if (argv[0] == atom_receive) {
        lttng_decl_procbuf(pid);
        char msg[LTTNG_BUFFER_SZ];

        lttng_pid_to_str(argv[2], pid);
        erts_snprintf(msg, LTTNG_BUFFER_SZ, "%T", argv[3]);

        LTTNG2(message_receive, pid, msg);
    } else {
        int i;
        erts_fprintf(stderr, "trace receive:\r\n");
        for (i = 0; i < argc; i++) {
            erts_fprintf(stderr, "  %T\r\n", argv[i]);
        }
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
    lttng_decl_procbuf(pid);
    lttng_decl_procbuf(to);

    lttng_pid_to_str(argv[2], pid);

    /* spawn */
    if (argv[0] == atom_spawn) {
        char undef[] = "undefined";
        const ERL_NIF_TERM* tuple;
        int arity;
        unsigned int len;
        lttng_decl_mfabuf(mfa);

        lttng_pid_to_str(argv[3], to);

        if (enif_get_tuple(env, argv[4], &arity, &tuple)) {
            if (enif_is_list(env, tuple[2])) {
                enif_get_list_length(env, tuple[2], &len);
            } else {
                enif_get_uint(env, tuple[2], &len);
            }
            lttng_mfa_to_str(tuple[0], tuple[1], len, mfa);
            LTTNG3(process_spawn, to, pid, mfa);
        } else {
            LTTNG3(process_spawn, to, pid, undef);
        }

    /* register */
    } else if (argv[0] == atom_register) {
        char name[LTTNG_BUFFER_SZ];
        erts_snprintf(name, LTTNG_BUFFER_SZ, "%T", argv[3]);
        LTTNG3(process_register, pid, name, "register");
    } else if (argv[0] == atom_unregister) {
        char name[LTTNG_BUFFER_SZ];
        erts_snprintf(name, LTTNG_BUFFER_SZ, "%T", argv[3]);
        LTTNG3(process_register, pid, name, "unregister");
    /* link */
    } else if (argv[0] == atom_link) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(process_link, pid, to, "link");
    } else if (argv[0] == atom_unlink) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(process_link, pid, to, "unlink");
    } else if (argv[0] == atom_getting_linked) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(process_link, to, pid, "link");
    } else if (argv[0] == atom_getting_unlinked) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(process_link, to, pid, "unlink");
    /* exit */
    } else if (argv[0] == atom_exit) {
        char reason[LTTNG_BUFFER_SZ];
        erts_snprintf(reason, LTTNG_BUFFER_SZ, "%T", argv[3]);
        LTTNG2(process_exit, pid, reason);
    } else {
        int i;
        erts_fprintf(stderr, "proc trace:\r\n");
        for (i = 0; i < argc; i++) {
            erts_fprintf(stderr, "  %T\r\n", argv[i]);
        }
    }
#endif
    return atom_ok;
}

static ERL_NIF_TERM trace_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#elif HAVE_USE_LTTNG
    lttng_decl_portbuf(port);
    lttng_decl_procbuf(to);

    lttng_portid_to_str(argv[2], port);

    /* open and closed */
    if (argv[0] == atom_open) {
        char driver[LTTNG_BUFFER_SZ];
        lttng_decl_procbuf(pid);
        lttng_pid_to_str(argv[3], pid);

        erts_snprintf(driver, LTTNG_BUFFER_SZ, "%T", argv[4]);
        LTTNG3(port_open, pid, driver, port);
    } else if (argv[0] == atom_closed) {
        char reason[LTTNG_BUFFER_SZ];
        erts_snprintf(reason, LTTNG_BUFFER_SZ, "%T", argv[3]);

        LTTNG2(port_exit, port, reason);
    /* link */
    } else if (argv[0] == atom_link) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(port_link, port, to, "link");
    } else if (argv[0] == atom_unlink) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(port_link, port, to, "unlink");
    } else if (argv[0] == atom_getting_linked) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(port_link, to, port, "link");
    } else if (argv[0] == atom_getting_unlinked) {
        lttng_pid_to_str(argv[3], to);
        LTTNG3(port_link, to, port, "unlink");
    } else {
        int i;
        erts_fprintf(stderr, "ports trace:\r\n");
        for (i = 0; i < argc; i++) {
            erts_fprintf(stderr, "  %T\r\n", argv[i]);
        }
    }
#endif
    return atom_ok;
}


static ERL_NIF_TERM trace_running(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_USE_DTRACE
#elif HAVE_USE_LTTNG
    lttng_decl_procbuf(pid);
    const ERL_NIF_TERM* tuple;
    char *mfastr = "undefined";
    int arity;

    lttng_pid_to_str(argv[2], pid);
    lttng_decl_mfabuf(mfa);

    if (enif_get_tuple(env, argv[3], &arity, &tuple)) {
        int val;
        enif_get_int(env, tuple[2], &val);
        lttng_mfa_to_str(tuple[0], tuple[1], val, mfa);
        mfastr = mfa;
    }

    /* running */
    if (argv[0] == atom_in) {
        LTTNG3(process_scheduled, pid, mfastr, "in");
    } else if (argv[0] == atom_out) {
        LTTNG3(process_scheduled, pid, mfastr, "out");
    /* exiting */
    } else if (argv[0] == atom_in_exiting) {
        LTTNG3(process_scheduled, pid, mfastr, "in_exiting");
    } else if (argv[0] == atom_out_exiting) {
        LTTNG3(process_scheduled, pid, mfastr, "out_exiting");
    } else if (argv[0] == atom_out_exited) {
        LTTNG3(process_scheduled, pid, mfastr, "out_exited");
    }
#endif
    return atom_ok;
}
