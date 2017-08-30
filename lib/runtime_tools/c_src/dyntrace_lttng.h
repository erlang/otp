/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2016. All Rights Reserved.
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

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER org_erlang_dyntrace

#if !defined(DYNTRACE_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define DYNTRACE_LTTNG_H

#include <lttng/tracepoint.h>

#define LTTNG1(Name, Arg1) \
    tracepoint(org_erlang_dyntrace, Name, (Arg1))

#define LTTNG2(Name, Arg1, Arg2) \
    tracepoint(org_erlang_dyntrace, Name, (Arg1), (Arg2))

#define LTTNG3(Name, Arg1, Arg2, Arg3) \
    tracepoint(org_erlang_dyntrace, Name, (Arg1), (Arg2), (Arg3))

#define LTTNG4(Name, Arg1, Arg2, Arg3, Arg4) \
    tracepoint(org_erlang_dyntrace, Name, (Arg1), (Arg2), (Arg3), (Arg4))

#define LTTNG5(Name, Arg1, Arg2, Arg3, Arg4, Arg5) \
    tracepoint(org_erlang_dyntrace, Name, (Arg1), (Arg2), (Arg3), (Arg4), (Arg5))

#define LTTNG_ENABLED(Name) \
    tracepoint_enabled(org_erlang_dyntrace, Name)

#define LTTNG_BUFFER_SZ      (256)
#define LTTNG_PROC_BUFFER_SZ (16)
#define LTTNG_PORT_BUFFER_SZ (20)
#define LTTNG_MFA_BUFFER_SZ  (256)

#define lttng_decl_procbuf(Name) \
    char Name[LTTNG_PROC_BUFFER_SZ]

#define lttng_decl_portbuf(Name) \
    char Name[LTTNG_PORT_BUFFER_SZ]

#define lttng_decl_mfabuf(Name) \
    char Name[LTTNG_MFA_BUFFER_SZ]

#define lttng_pid_to_str(pid, name) \
    enif_snprintf(name, LTTNG_PROC_BUFFER_SZ, "%T", (pid))

#define lttng_portid_to_str(pid, name) \
    enif_snprintf(name, LTTNG_PORT_BUFFER_SZ, "%T", (pid))

#define lttng_proc_to_str(p, name) \
    lttng_pid_to_str(((p) ? (p)->common.id : ERTS_INVALID_PID), name)

#define lttng_port_to_str(p, name) \
    lttng_portid_to_str(((p) ? (p)->common.id : ERTS_INVALID_PORT), name)

#define lttng_mfa_to_str(m,f,a, Name) \
    enif_snprintf(Name, LTTNG_MFA_BUFFER_SZ, "%T:%T/%lu", (Eterm)(m), (Eterm)(f), (Uint)(a))

/* Process scheduling */

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    process_spawn,
    TP_ARGS(
        char*, p,
        char*, parent,
        char*, mfa
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_string(parent, parent)
        ctf_string(entry, mfa)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    process_link,
    TP_ARGS(
        char*, from,
        char*, to,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(from, from)
        ctf_string(to, to)
        ctf_string(type, type)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    process_exit,
    TP_ARGS(
        char*, p,
        char*, reason
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_string(reason, reason)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    process_register,
    TP_ARGS(
        char*, pid,
        char*, name,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(name, name)
        ctf_string(type, type)
    )
)

/* Scheduled */

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    process_scheduled,
    TP_ARGS(
        char*, p,
        char*, mfa,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_string(entry, mfa)
        ctf_string(type, type)
    )
)

/* Ports */


TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    port_open,
    TP_ARGS(
        char*, pid,
        char*, driver,
        char*, port
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(driver, driver)
        ctf_string(port, port)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    port_exit,
    TP_ARGS(
        char*, port,
        char*, reason
    ),
    TP_FIELDS(
        ctf_string(port, port)
        ctf_string(reason, reason)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    port_link,
    TP_ARGS(
        char*, from,
        char*, to,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(from, from)
        ctf_string(to, to)
        ctf_string(type, type)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    port_scheduled,
    TP_ARGS(
        char*, p,
        char*, op,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_string(entry, op)
        ctf_string(type, type)
    )
)

/* Call tracing */

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    function_call,
    TP_ARGS(
        char*, pid,
        char*, mfa,
        unsigned int, depth
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(entry, mfa)
        ctf_integer(unsigned int, depth, depth)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    function_return,
    TP_ARGS(
        char*, pid,
        char*, mfa,
        unsigned int, depth
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(entry, mfa)
        ctf_integer(unsigned int, depth, depth)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    function_exception,
    TP_ARGS(
        char*, pid,
        char*, mfa,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(entry, mfa)
        ctf_string(class, type)
    )
)

/* Process messages */

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    message_send,
    TP_ARGS(
        char*, sender,
        char*, receiver,
        char*, msg
    ),
    TP_FIELDS(
        ctf_string(from, sender)
        ctf_string(to, receiver)
        ctf_string(message, msg)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    message_receive,
    TP_ARGS(
        char*, receiver,
        char*, msg
    ),
    TP_FIELDS(
        ctf_string(to, receiver)
        ctf_string(message, msg)
    )
)

/* Process Memory */

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    gc_minor_start,
    TP_ARGS(
        char*, p,
        unsigned long, need,
        unsigned long, nh,
        unsigned long, oh
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(unsigned long, need, need)
        ctf_integer(unsigned long, heap, nh)
        ctf_integer(unsigned long, old_heap, oh)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    gc_minor_end,
    TP_ARGS(
        char*, p,
        unsigned long, reclaimed,
        unsigned long, nh,
        unsigned long, oh
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(unsigned long, reclaimed, reclaimed)
        ctf_integer(unsigned long, heap, nh)
        ctf_integer(unsigned long, old_heap, oh)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    gc_major_start,
    TP_ARGS(
        char*, p,
        unsigned long, need,
        unsigned long, nh,
        unsigned long, oh
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(unsigned long, need, need)
        ctf_integer(unsigned long, heap, nh)
        ctf_integer(unsigned long, old_heap, oh)
    )
)

TRACEPOINT_EVENT(
    org_erlang_dyntrace,
    gc_major_end,
    TP_ARGS(
        char*, p,
        unsigned long, reclaimed,
        unsigned long, nh,
        unsigned long, oh
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(unsigned long, reclaimed, reclaimed)
        ctf_integer(unsigned long, heap, nh)
        ctf_integer(unsigned long, old_heap, oh)
    )
)

#endif /* DYNTRACE_LTTNG_H */

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "./dyntrace_lttng.h"

/* This part must be outside protection */
#include <lttng/tracepoint-event.h>
