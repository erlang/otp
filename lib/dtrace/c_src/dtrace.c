/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Purpose:  Dynamically loadable NIF library for DTrace
 */


#include "erl_nif.h"
#include "config.h"
#include "sys.h"
#define DTRACE_DRIVER_SKIP_FUNC_DECLARATIONS
#include "dtrace-wrapper.h"
#ifdef  HAVE_DTRACE
#include "dtrace_user.h"
#endif

void dtrace_nifenv_str(ErlNifEnv *env, char *process_buf);
void get_string_maybe(ErlNifEnv *env, const ERL_NIF_TERM term, char **ptr, char *buf, int bufsiz);

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

static ErlNifFunc nif_funcs[] = {
    {"available", 0, available},
    {"user_trace_s1", 1, user_trace_s1},
    {"user_trace_i4s4", 9, user_trace_i4s4}
};

ERL_NIF_INIT(dtrace, nif_funcs, load, NULL, NULL, NULL)

static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_not_available;
static ERL_NIF_TERM atom_badarg;
static ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_true = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");
    atom_error = enif_make_atom(env,"error");
    atom_not_available = enif_make_atom(env,"not_available");
    atom_badarg = enif_make_atom(env,"badarg");
    atom_ok = enif_make_atom(env,"ok");

    return 0;
}

static ERL_NIF_TERM available(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_DTRACE
    return atom_true;
#else
    return atom_false;
#endif
}

static ERL_NIF_TERM user_trace_s1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_DTRACE
    ErlNifBinary message_bin;
    DTRACE_CHARBUF(messagebuf, MESSAGE_BUFSIZ + 1);

    if (DTRACE_ENABLED(user_trace_s1)) {
	if (!enif_inspect_iolist_as_binary(env, argv[0], &message_bin) ||
	    message_bin.size > MESSAGE_BUFSIZ) {
	    return atom_badarg;
	}
	memcpy(messagebuf, (char *) message_bin.data, message_bin.size);
        messagebuf[message_bin.size] = '\0';
	DTRACE1(user_trace_s1, messagebuf);
	return atom_true;
    } else {
	return atom_false;
    }
#else
    return atom_error;
#endif
}

void
get_string_maybe(ErlNifEnv *env,
                 const ERL_NIF_TERM term, char **ptr, char *buf, int bufsiz)
{
    ErlNifBinary str_bin;

    if (!enif_inspect_iolist_as_binary(env, term, &str_bin) ||
        str_bin.size > bufsiz) {
        *ptr = NULL;
    } else {
        memcpy(buf, (char *) str_bin.data, str_bin.size);
        buf[str_bin.size] = '\0';
        *ptr = buf;
    }
}

static ERL_NIF_TERM user_trace_i4s4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef	HAVE_DTRACE
    DTRACE_CHARBUF(procbuf, 32 + 1);
    DTRACE_CHARBUF(user_tagbuf, MESSAGE_BUFSIZ + 1);
    char *utbuf = NULL;
    ErlNifSInt64 i1, i2, i3, i4;
    DTRACE_CHARBUF(messagebuf1, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf2, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf3, MESSAGE_BUFSIZ + 1);
    DTRACE_CHARBUF(messagebuf4, MESSAGE_BUFSIZ + 1);
    char *mbuf1 = NULL, *mbuf2 = NULL, *mbuf3 = NULL, *mbuf4 = NULL;

    if (DTRACE_ENABLED(user_trace_i4s4)) {
	dtrace_nifenv_str(env, procbuf);
        get_string_maybe(env, argv[0], &utbuf,
                         user_tagbuf, sizeof(user_tagbuf)-1);
        if (! enif_get_int64(env, argv[1], &i1))
            i1 = 0;
        if (! enif_get_int64(env, argv[2], &i2))
            i2 = 0;
        if (! enif_get_int64(env, argv[3], &i3))
            i3 = 0;
        if (! enif_get_int64(env, argv[4], &i4))
            i4 = 0;
        get_string_maybe(env, argv[5], &mbuf1,
                         messagebuf1, sizeof(messagebuf1)-1);
        get_string_maybe(env, argv[6], &mbuf2,
                         messagebuf2, sizeof(messagebuf2)-1);
        get_string_maybe(env, argv[7], &mbuf3,
                         messagebuf3, sizeof(messagebuf3)-1);
        get_string_maybe(env, argv[8], &mbuf4,
                         messagebuf4, sizeof(messagebuf4)-1);
	DTRACE10(user_trace_i4s4, procbuf, utbuf,
		 i1, i2, i3, i4, mbuf1, mbuf2, mbuf3, mbuf4);
	return atom_true;
    } else {
	return atom_false;
    }
#else
    return atom_error;
#endif
}
