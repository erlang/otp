/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson 2015. All Rights Reserved.
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
 * Purpose:  NIF library for process/port tracer
 *
 */


#define STATIC_ERLANG_NIF 1

#include "erl_nif.h"
#include "config.h"
#include "sys.h"

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"enabled", 3, enabled},
    {"trace", 5, trace}
};


ERL_NIF_INIT(erl_tracer, nif_funcs, load, NULL, upgrade, unload)

#define ATOMS                                      \
    ATOM_DECL(call);                               \
    ATOM_DECL(command);                            \
    ATOM_DECL(cpu_timestamp);                      \
    ATOM_DECL(discard);                            \
    ATOM_DECL(exception_from);                     \
    ATOM_DECL(extra);                              \
    ATOM_DECL(match_spec_result);                  \
    ATOM_DECL(monotonic);                          \
    ATOM_DECL(ok);                                 \
    ATOM_DECL(remove);                             \
    ATOM_DECL(return_from);                        \
    ATOM_DECL(scheduler_id);                       \
    ATOM_DECL(send);                               \
    ATOM_DECL(send_to_non_existing_process);       \
    ATOM_DECL(seq_trace);                          \
    ATOM_DECL(spawn);                              \
    ATOM_DECL(strict_monotonic);                   \
    ATOM_DECL(timestamp);                          \
    ATOM_DECL(trace);                              \
    ATOM_DECL(trace_status);                       \
    ATOM_DECL(trace_ts);                           \
    ATOM_DECL(true);                               \
    ATOM_DECL(gc_minor_start);                     \
    ATOM_DECL(gc_minor_end);                       \
    ATOM_DECL(gc_major_start);                     \
    ATOM_DECL(gc_major_end);

#define ATOM_DECL(A) static ERL_NIF_TERM atom_##A
ATOMS
#undef ATOM_DECL

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

#define ATOM_DECL(A) atom_##A = enif_make_atom(env, #A)
ATOMS
#undef ATOM_DECL

    *priv_data = NULL;

    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{

}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
	return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
	return -1;
    }
    return 0;
}

static ERL_NIF_TERM enabled(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid to_pid;
    ErlNifPort to_port;
    ERL_NIF_TERM ret = enif_is_identical(argv[0], atom_trace_status) ?
        atom_remove : atom_discard;

    ASSERT(argc == 3);

    if (enif_get_local_pid(env, argv[1], &to_pid)) {
        if (!enif_is_process_alive(env, &to_pid))
            /* tracer is dead so we should remove this trace point */
            return ret;
    } else if (enif_get_local_port(env, argv[1], &to_port)) {
        if (!enif_is_port_alive(env, &to_port))
            /* tracer is dead so we should remove this trace point */
            return ret;
    } else {
        /* The state was not a pid or a port */
        return ret;
    }

    /* Only generate trace for when tracer != tracee */
    if (enif_is_identical(argv[1], argv[2])) {
        return atom_discard;
    }

    return atom_trace;
}

/*
  -spec trace(seq_trace, TracerState :: pid() | port(),
              Label :: non_neg_integer(),
              Msg :: term(),
              Opts :: map()) -> ignored();
        trace(Tag :: atom(), TracerState :: pid() | port(),
              Tracee :: pid() || port() || undefined,
              Msg :: term(),
              Opts :: map()) -> ignored().
*/
static ERL_NIF_TERM trace(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM value, msg, tt[8], opts;
    ErlNifPid to_pid;
    ErlNifPort to_port;
    size_t tt_sz = 0;
    int is_port = 0;
    size_t opts_sz = 0;
    ASSERT(argc == 5);

    if (!enif_get_local_pid(env, argv[1], &to_pid)) {
        if (!enif_get_local_port(env, argv[1], &to_port)) {
            /* This only fails if argv[1] is a not a local port/pid
               which should not happen as it is checked in enabled */
            ASSERT(0);
            return atom_ok;
        }
        is_port = 1;
    }

    opts = argv[4];

    if (!enif_get_map_size(env, opts, &opts_sz))
        opts_sz = 0;

    if (opts_sz && enif_get_map_value(env, opts, atom_extra, &value)) {
        tt[tt_sz++] = atom_trace;
        tt[tt_sz++] = argv[2];
        tt[tt_sz++] = argv[0];
        tt[tt_sz++] = argv[3];
        tt[tt_sz++] = value;
    } else {
        if (enif_is_identical(argv[0], atom_seq_trace)) {
            tt[tt_sz++] = atom_seq_trace;
            tt[tt_sz++] = argv[2];
            tt[tt_sz++] = argv[3];
        } else {
            tt[tt_sz++] = atom_trace;
            tt[tt_sz++] = argv[2];
            tt[tt_sz++] = argv[0];
            tt[tt_sz++] = argv[3];
        }
    }


    if (opts_sz && enif_get_map_value(env, opts, atom_match_spec_result, &value)) {
        tt[tt_sz++] = value;
    }

    if (opts_sz && enif_get_map_value(env, opts, atom_scheduler_id, &value)) {
        tt[tt_sz++] = value;
    }

    if (opts_sz && enif_get_map_value(env, opts, atom_timestamp, &value)) {
        ERL_NIF_TERM ts;
        if (enif_is_identical(value, atom_monotonic)) {
            ErlNifTime mon = enif_monotonic_time(ERL_NIF_NSEC);
            ts = enif_make_int64(env, mon);
        } else if (enif_is_identical(value, atom_strict_monotonic)) {
            ErlNifTime mon = enif_monotonic_time(ERL_NIF_NSEC);
            ERL_NIF_TERM unique = enif_make_unique_integer(
                env, ERL_NIF_UNIQUE_MONOTONIC);
            ts = enif_make_tuple2(env, enif_make_int64(env, mon), unique);
        } else if (enif_is_identical(value, atom_timestamp)) {
            ts = enif_now_time(env);
        } else if (enif_is_identical(value, atom_cpu_timestamp)) {
            ts = enif_cpu_time(env);
        } else {
            ASSERT(0);
            goto error;
        }
        tt[tt_sz++] = ts;
        if (tt[0] == atom_trace)
            tt[0] = atom_trace_ts;
    }

    msg = enif_make_tuple_from_array(env, tt, tt_sz);

    if (is_port) {
        ErlNifBinary bin;

        if (!enif_term_to_binary(env, msg, &bin))
            goto error;

        msg = enif_make_binary(env, &bin);

        if (!enif_port_command(env, &to_port, NULL, msg))
            /* port has probably died, enabled will clean up */;

        enif_release_binary(&bin);
    } else {

        if (!enif_send(env, &to_pid, NULL, msg))
            /* process has probably died, enabled will clean up */;
    }

error:

    return atom_ok;
}
