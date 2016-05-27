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

#ifdef HAVE_USE_LTTNG
static ERL_NIF_TERM trace_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_running_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM enabled_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_running_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM enabled_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
#endif


static ErlNifFunc nif_funcs[] = {
    {"available", 0, available},
    {"user_trace_s1", 1, user_trace_s1},
    {"user_trace_i4s4", 9, user_trace_i4s4},
    {"user_trace_n", 10, user_trace_n},
#ifdef HAVE_USE_LTTNG
    {"trace_procs", 5, trace_procs},
    {"trace_ports", 5, trace_ports},
    {"trace_running_procs", 5, trace_running_procs},
    {"trace_running_ports", 5, trace_running_ports},
    {"trace_call", 5, trace_call},
    {"trace_send", 5, trace_send},
    {"trace_receive", 5, trace_receive},
    {"trace_garbage_collection", 5, trace_garbage_collection},
    {"enabled_procs", 3, enabled_procs},
    {"enabled_ports", 3, enabled_ports},
    {"enabled_running_procs", 3, enabled_running_procs},
    {"enabled_running_ports", 3, enabled_running_ports},
    {"enabled_call", 3, enabled_call},
    {"enabled_send", 3, enabled_send},
    {"enabled_receive", 3, enabled_receive},
    {"enabled_garbage_collection", 3, enabled_garbage_collection},
#endif
    {"enabled", 3, enabled},
    {"trace", 5, trace}
};

ERL_NIF_INIT(dyntrace, nif_funcs, load, NULL, NULL, NULL)

static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_extra;
static ERL_NIF_TERM atom_not_available;
static ERL_NIF_TERM atom_badarg;
static ERL_NIF_TERM atom_ok;

static ERL_NIF_TERM atom_trace;
static ERL_NIF_TERM atom_seq_trace;
static ERL_NIF_TERM atom_remove;
static ERL_NIF_TERM atom_discard;

#ifdef HAVE_USE_LTTNG

/* gc atoms */

static ERL_NIF_TERM atom_gc_minor_start;
static ERL_NIF_TERM atom_gc_minor_end;
static ERL_NIF_TERM atom_gc_major_start;
static ERL_NIF_TERM atom_gc_major_end;

static ERL_NIF_TERM atom_old_heap_block_size; /* for debug */
static ERL_NIF_TERM atom_heap_block_size;     /* for debug */

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
static ERL_NIF_TERM atom_return_to;
static ERL_NIF_TERM atom_exception_from;
#endif

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_true = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    atom_error = enif_make_atom(env,"error");
    atom_extra = enif_make_atom(env,"extra");
    atom_not_available = enif_make_atom(env,"not_available");
    atom_badarg = enif_make_atom(env,"badarg");
    atom_ok = enif_make_atom(env,"ok");

    atom_trace = enif_make_atom(env,"trace");
    atom_seq_trace = enif_make_atom(env,"seq_trace");
    atom_remove = enif_make_atom(env,"remove");
    atom_discard = enif_make_atom(env,"discard");

#ifdef HAVE_USE_LTTNG

    /* gc */

    atom_gc_minor_start = enif_make_atom(env,"gc_minor_start");
    atom_gc_minor_end   = enif_make_atom(env,"gc_minor_end");
    atom_gc_major_start = enif_make_atom(env,"gc_major_start");
    atom_gc_major_end   = enif_make_atom(env,"gc_major_end");

    atom_old_heap_block_size = enif_make_atom(env,"old_heap_block_size"); 
    atom_heap_block_size     = enif_make_atom(env,"heap_block_size");

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
    atom_return_to = enif_make_atom(env,"return_to");
    atom_exception_from = enif_make_atom(env,"exception_from");
#endif

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
#ifdef HAVE_USE_LTTNG
    ASSERT(argc == 3);
    return atom_trace;
#endif
    return atom_remove;
}

static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return atom_ok;
}

#ifdef HAVE_USE_LTTNG
static ERL_NIF_TERM enabled_garbage_collection(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);

    if (argv[0] == atom_gc_minor_start && LTTNG_ENABLED(gc_minor_start)) {
        return atom_trace;
    } else if (argv[0] == atom_gc_minor_end && LTTNG_ENABLED(gc_minor_end)) {
        return atom_trace;
    } else if (argv[0] == atom_gc_major_start && LTTNG_ENABLED(gc_major_start)) {
        return atom_trace;
    } else if (argv[0] == atom_gc_major_end && LTTNG_ENABLED(gc_major_end)) {
        return atom_trace;
    }

    return atom_discard;
}

static ERL_NIF_TERM trace_garbage_collection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lttng_decl_procbuf(pid);
    ERL_NIF_TERM gci, tup;
    const ERL_NIF_TERM *vals;
    int arity;
    unsigned long ohbsz, nhbsz, size;

    ASSERT(argc == 5);

    /* Assume gc info order does not change */
    gci = argv[3];

    /* get reclaimed or need */
    enif_get_list_cell(env, gci, &tup, &gci);
    enif_get_tuple(env, tup, &arity, &vals);
    ASSERT(arity == 2);
    enif_get_ulong(env, vals[1], &size);

    /* get old heap block size */
    enif_get_list_cell(env, gci, &tup, &gci);
    enif_get_tuple(env, tup, &arity, &vals);
    ASSERT(arity == 2);
    ASSERT(vals[0] == atom_old_heap_block_size);
    enif_get_ulong(env, vals[1], &ohbsz);

    /* get new heap block size */
    enif_get_list_cell(env, gci, &tup, &gci);
    enif_get_tuple(env, tup, &arity, &vals);
    ASSERT(arity == 2);
    ASSERT(vals[0] == atom_heap_block_size);
    enif_get_ulong(env, vals[1], &nhbsz);

    lttng_pid_to_str(argv[2], pid);

    if (argv[0] == atom_gc_minor_start) {
        LTTNG4(gc_minor_start, pid, size, nhbsz, ohbsz);
    } else if (argv[0] == atom_gc_minor_end) {
        LTTNG4(gc_minor_end, pid, size, nhbsz, ohbsz);
    } else if (argv[0] == atom_gc_major_start) {
        LTTNG4(gc_major_start, pid, size, nhbsz, ohbsz);
    } else if (argv[0] == atom_gc_major_end) {
        LTTNG4(gc_major_end, pid, size, nhbsz, ohbsz);
    }
    return atom_ok;
}

static ERL_NIF_TERM enabled_call(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);

    if (argv[0] == atom_call && LTTNG_ENABLED(function_call))
        return atom_trace;
    else if (argv[0] == atom_return_from && LTTNG_ENABLED(function_return))
        return atom_trace;
    else if (argv[0] == atom_exception_from && LTTNG_ENABLED(function_exception))
        return atom_trace;

    return atom_discard;
}

static ERL_NIF_TERM trace_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
    } else if (argv[0] == atom_return_to) {
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
        ERL_NIF_TERM extra;
        int arity;
        lttng_decl_mfabuf(mfa);
        char class[LTTNG_BUFFER_SZ];

        enif_get_map_value(env, argv[4], atom_extra, &extra);
        enif_get_tuple(env, extra, &arity, &tuple);
        enif_snprintf(class, LTTNG_BUFFER_SZ, "%T", tuple[0]);

        if (enif_get_tuple(env, argv[3], &arity, &tuple)) {
            enif_get_uint(env, tuple[2], &len);
            lttng_mfa_to_str(tuple[0], tuple[1], len, mfa);
            LTTNG3(function_exception, pid, mfa, class);
        } else {
            LTTNG3(function_exception, pid, undef, class);
        }
    }
    return atom_ok;
}

static ERL_NIF_TERM enabled_send(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);
    if (LTTNG_ENABLED(message_send))
        return atom_trace;

    return atom_discard;
}

static ERL_NIF_TERM trace_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM extra;
    lttng_decl_procbuf(pid);
    lttng_pid_to_str(argv[2], pid);

    enif_get_map_value(env, argv[4], atom_extra, &extra);

    if (argv[0] == atom_send) {
        lttng_decl_procbuf(to);
        char msg[LTTNG_BUFFER_SZ];

        lttng_pid_to_str(extra, to);
        enif_snprintf(msg, LTTNG_BUFFER_SZ, "%T", argv[3]);

        LTTNG3(message_send, pid, to, msg);
    } else if (argv[0] == atom_send_to_non_existing_process) {
        lttng_decl_procbuf(to);
        char msg[LTTNG_BUFFER_SZ];

        lttng_pid_to_str(extra, to);
        enif_snprintf(msg, LTTNG_BUFFER_SZ, "%T", argv[3]);
        /* mark it as non existing ? */

        LTTNG3(message_send, pid, to, msg);
    }
    return atom_ok;
}

static ERL_NIF_TERM enabled_receive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (LTTNG_ENABLED(message_receive))
        return atom_trace;

    return atom_discard;
}

static ERL_NIF_TERM trace_receive(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argv[0] == atom_receive) {
        lttng_decl_procbuf(pid);
        char msg[LTTNG_BUFFER_SZ];

        lttng_pid_to_str(argv[2], pid);
        enif_snprintf(msg, LTTNG_BUFFER_SZ, "%T", argv[3]);

        LTTNG2(message_receive, pid, msg);
    }
    return atom_ok;
}

static ERL_NIF_TERM enabled_procs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);

    if (argv[0] == atom_spawn && LTTNG_ENABLED(process_spawn)) {
        return atom_trace;
    } else if (argv[0] == atom_register && LTTNG_ENABLED(process_register)) {
        return atom_trace;
    } else if (argv[0] == atom_unregister && LTTNG_ENABLED(process_register)) {
        return atom_trace;
    } else if (argv[0] == atom_link && LTTNG_ENABLED(process_link)) {
        return atom_trace;
    } else if (argv[0] == atom_unlink && LTTNG_ENABLED(process_link)) {
        return atom_trace;
    } else if (argv[0] == atom_getting_linked && LTTNG_ENABLED(process_link)) {
        return atom_trace;
    } else if (argv[0] == atom_getting_unlinked && LTTNG_ENABLED(process_link)) {
        return atom_trace;
    } else if (argv[0] == atom_exit && LTTNG_ENABLED(process_exit)) {
        return atom_trace;
    }

    return atom_discard;
}

static ERL_NIF_TERM trace_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lttng_decl_procbuf(pid);
    lttng_decl_procbuf(to);

    lttng_pid_to_str(argv[2], pid);

    /* spawn */
    if (argv[0] == atom_spawn) {
        ERL_NIF_TERM extra;
        char undef[] = "undefined";
        const ERL_NIF_TERM* tuple;
        int arity;
        unsigned int len;
        lttng_decl_mfabuf(mfa);

        lttng_pid_to_str(argv[3], to);

        enif_get_map_value(env, argv[4], atom_extra, &extra);

        if (enif_get_tuple(env, extra, &arity, &tuple)) {
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
        enif_snprintf(name, LTTNG_BUFFER_SZ, "%T", argv[3]);
        LTTNG3(process_register, pid, name, "register");
    } else if (argv[0] == atom_unregister) {
        char name[LTTNG_BUFFER_SZ];
        enif_snprintf(name, LTTNG_BUFFER_SZ, "%T", argv[3]);
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
        enif_snprintf(reason, LTTNG_BUFFER_SZ, "%T", argv[3]);
        LTTNG2(process_exit, pid, reason);
    }
    return atom_ok;
}

static ERL_NIF_TERM enabled_ports(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);

    if (argv[0] == atom_open && LTTNG_ENABLED(port_open)) {
        return atom_trace;
    } else if (argv[0] == atom_link && LTTNG_ENABLED(port_link)) {
        return atom_trace;
    } else if (argv[0] == atom_unlink && LTTNG_ENABLED(port_link)) {
        return atom_trace;
    } else if (argv[0] == atom_getting_linked && LTTNG_ENABLED(port_link)) {
        return atom_trace;
    } else if (argv[0] == atom_getting_unlinked && LTTNG_ENABLED(port_link)) {
        return atom_trace;
    } else if (argv[0] == atom_closed && LTTNG_ENABLED(port_exit)) {
        return atom_trace;
    }

    return atom_discard;
}

static ERL_NIF_TERM trace_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lttng_decl_portbuf(port);
    lttng_decl_procbuf(to);

    lttng_portid_to_str(argv[2], port);

    /* open and closed */
    if (argv[0] == atom_open) {
        ERL_NIF_TERM extra;
        char driver[LTTNG_BUFFER_SZ];
        lttng_decl_procbuf(pid);
        lttng_pid_to_str(argv[3], pid);
        enif_get_map_value(env, argv[4], atom_extra, &extra);

        enif_snprintf(driver, LTTNG_BUFFER_SZ, "%T", extra);
        LTTNG3(port_open, pid, driver, port);
    } else if (argv[0] == atom_closed) {
        char reason[LTTNG_BUFFER_SZ];
        enif_snprintf(reason, LTTNG_BUFFER_SZ, "%T", argv[3]);

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
    }
    return atom_ok;
}

static ERL_NIF_TERM enabled_running_procs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);

    if (LTTNG_ENABLED(process_scheduled))
        return atom_trace;
 
    return atom_discard;
}

static ERL_NIF_TERM trace_running_procs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lttng_decl_procbuf(pid);
    const ERL_NIF_TERM* tuple;
    char *mfastr = "undefined";
    int arity;
    lttng_decl_mfabuf(mfa);

    lttng_pid_to_str(argv[2], pid);

    if (enif_get_tuple(env, argv[3], &arity, &tuple)) {
        int val;
        enif_get_int(env, tuple[2], &val);
        lttng_mfa_to_str(tuple[0], tuple[1], val, mfa);
        mfastr = mfa;
    }
    /* running processes */
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

    return atom_ok;
}

static ERL_NIF_TERM enabled_running_ports(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ASSERT(argc == 3);

    if (LTTNG_ENABLED(port_scheduled))
        return atom_trace;

    return atom_discard;
}

static ERL_NIF_TERM trace_running_ports(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lttng_decl_procbuf(pid);
    lttng_decl_mfabuf(where);

    lttng_portid_to_str(argv[2], pid);
    enif_snprintf(where, LTTNG_BUFFER_SZ, "%T", argv[3]);

    /* running  ports */
    if (argv[0] == atom_in) {
        LTTNG3(port_scheduled, pid, where, "in");
    } else if (argv[0] == atom_out) {
        LTTNG3(port_scheduled, pid, where, "out");
        /* exiting */
    } else if (argv[0] == atom_in_exiting) {
        LTTNG3(port_scheduled, pid, where, "in_exiting");
    } else if (argv[0] == atom_out_exiting) {
        LTTNG3(port_scheduled, pid, where, "out_exiting");
    } else if (argv[0] == atom_out_exited) {
        LTTNG3(port_scheduled, pid, where, "out_exited");
    }
    return atom_ok;
}
#endif
