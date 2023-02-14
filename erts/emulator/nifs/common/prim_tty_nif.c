/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson 2015-2023. All Rights Reserved.
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
 * Purpose:  NIF library for interacting with the tty
 *
 */

#define STATIC_ERLANG_NIF 1

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif

#include "config.h"
#include "sys.h"
#include "erl_nif.h"
#include "erl_driver.h"

#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <wchar.h>
#include <stdio.h>
#include <signal.h>
#include <locale.h>
#ifdef HAVE_TERMCAP
  #include <termios.h>
  #include <curses.h>
  #include <term.h>
#endif
#ifndef __WIN32__
  #include <unistd.h>
  #include <sys/ioctl.h>
#endif
#ifdef HAVE_SYS_UIO_H
  #include <sys/uio.h>
#endif

#if defined IOV_MAX
#define MAXIOV IOV_MAX
#elif defined UIO_MAXIOV
#define MAXIOV UIO_MAXIOV
#else
#define MAXIOV 16
#endif

#if !defined(HAVE_SETLOCALE) || !defined(HAVE_NL_LANGINFO) || !defined(HAVE_LANGINFO_H)
#define PRIMITIVE_UTF8_CHECK 1
#else
#include <langinfo.h>
#endif

#ifdef VALGRIND
#  include <valgrind/memcheck.h>
#endif

#define DEF_HEIGHT 24
#define DEF_WIDTH 80

typedef struct {
#ifdef __WIN32__
    HANDLE ofd;
    HANDLE ifd;
    DWORD dwOriginalOutMode;
    DWORD dwOriginalInMode;
    DWORD dwOutMode;
    DWORD dwInMode;
#else
    int ofd;       /* stdout */
    int ifd;       /* stdin */
#endif
    ErlNifPid self;
    ErlNifPid reader;
    int tty;       /* if the tty is initialized */
#ifdef THREADED_READER
    ErlNifTid reader_tid;
#endif
#ifndef __WIN32__
    int signal[2]; /* Pipe used for signal (winch + cont) notifications */
#endif
#ifdef HAVE_TERMCAP
    struct termios tty_smode;
    struct termios tty_rmode;
#endif
} TTYResource;

static ErlNifResourceType *tty_rt;

/* The NIFs: */
static ERL_NIF_TERM isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_create_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_set_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM setlocale_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_select_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM isprint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM wcwidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM wcswidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sizeof_wchar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_window_size_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetent_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetnum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetflag_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgetstr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_tgoto_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM tty_read_signal_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"isatty", 1, isatty_nif},
    {"tty_create", 0, tty_create_nif},
    {"tty_init", 3, tty_init_nif},
    {"tty_set", 1, tty_set_nif},
    {"tty_read_signal", 2, tty_read_signal_nif},
    {"setlocale", 1, setlocale_nif},
    {"tty_select", 3, tty_select_nif},
    {"tty_window_size", 1, tty_window_size_nif},
    {"write_nif", 2, tty_write_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"read_nif", 2, tty_read_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"isprint", 1, isprint_nif},
    {"wcwidth", 1, wcwidth_nif},
    {"wcswidth", 1, wcswidth_nif},
    {"sizeof_wchar", 0, sizeof_wchar_nif},
    {"tgetent_nif", 1, tty_tgetent_nif},
    {"tgetnum_nif", 1, tty_tgetnum_nif},
    {"tgetflag_nif", 1, tty_tgetflag_nif},
    {"tgetstr_nif", 1, tty_tgetstr_nif},
    {"tgoto_nif", 2, tty_tgoto_nif},
    {"tgoto_nif", 3, tty_tgoto_nif}
};

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

ERL_NIF_INIT(prim_tty, nif_funcs, load, NULL, upgrade, unload)

#define ATOMS                                     \
    ATOM_DECL(canon);                             \
    ATOM_DECL(echo);                              \
    ATOM_DECL(ebadf);                             \
    ATOM_DECL(undefined);                         \
    ATOM_DECL(error);                             \
    ATOM_DECL(true);                              \
    ATOM_DECL(ok);                                \
    ATOM_DECL(input);                             \
    ATOM_DECL(false);                             \
    ATOM_DECL(stdin);                             \
    ATOM_DECL(stdout);                            \
    ATOM_DECL(stderr);                            \
    ATOM_DECL(sig);


#define ATOM_DECL(A) static ERL_NIF_TERM atom_##A
ATOMS
#undef ATOM_DECL

static ERL_NIF_TERM make_error(ErlNifEnv *env, ERL_NIF_TERM reason) {
    return enif_make_tuple2(env, atom_error, reason);
}

static ERL_NIF_TERM make_enotsup(ErlNifEnv *env) {
    return make_error(env, enif_make_atom(env, "enotsup"));
}

static ERL_NIF_TERM make_errno(ErlNifEnv *env) {
#ifdef __WIN32__
    return enif_make_atom(env, last_error());
#else
    return enif_make_atom(env, erl_errno_id(errno));
#endif
}

static ERL_NIF_TERM make_errno_error(ErlNifEnv *env, const char *function) {
    return make_error(
        env, enif_make_tuple2(
            env, enif_make_atom(env, function), make_errno(env)));
}

static int tty_get_fd(ErlNifEnv *env, ERL_NIF_TERM atom, int *fd) {
    if (enif_is_identical(atom, atom_stdout)) {
        *fd = fileno(stdout);
    } else if (enif_is_identical(atom, atom_stdin)) {
        *fd =  fileno(stdin);
    } else if (enif_is_identical(atom, atom_stderr)) {
        *fd =  fileno(stderr);
    } else {
        return 0;
    }
    return 1;
}

static ERL_NIF_TERM isatty_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int fd;
    if (tty_get_fd(env, argv[0], &fd)) {
        if (isatty(fd)) {
            return atom_true;
        } else if (errno == EINVAL || errno == ENOTTY) {
            return atom_false;
        } else {
            return atom_ebadf;
        }
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM isprint_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int i;
    if (enif_get_int(env, argv[0], &i)) {
        ASSERT(i > 0 && i < 256);
        return isprint((char)i) ? atom_true : atom_false;
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM wcwidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int i;
    if (enif_get_int(env, argv[0], &i)) {
#ifndef __WIN32__
        int width;
        ASSERT(i > 0 && i < (1l << 21));
        width = wcwidth((wchar_t)i);
        if (width == -1) {
            return make_error(env, enif_make_atom(env, "not_printable"));
        }
        return enif_make_int(env, width);
#else
        return make_enotsup(env);
#endif
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM wcswidth_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    if (enif_inspect_iolist_as_binary(env, argv[0], &bin)) {
        wchar_t *chars = (wchar_t*)bin.data;
        int width;
#ifdef DEBUG
        for (int i = 0; i < bin.size / sizeof(wchar_t); i++) {
            ASSERT(chars[i] >= 0 && chars[i] < (1l << 21));
        }
#endif
#ifndef __WIN32__
        width = wcswidth(chars, bin.size / sizeof(wchar_t));
#else
        width = bin.size / sizeof(wchar_t);
#endif
        if (width == -1) {
            return make_error(env, enif_make_atom(env, "not_printable"));
        }
        return enif_make_tuple2(env, atom_ok, enif_make_int(env, width));
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM sizeof_wchar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_int(env, sizeof(wchar_t));
}

static ERL_NIF_TERM tty_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM head = argv[1], tail;
    ErlNifIOQueue *q = NULL;
    ErlNifIOVec vec, *iovec = &vec;
    SysIOVec *iov;
    int iovcnt;
    TTYResource *tty;
    ssize_t res = 0;
    size_t size;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

    while (!enif_is_identical(head, enif_make_list(env, 0))) {
        if (!enif_inspect_iovec(env, MAXIOV, head, &tail, &iovec))
            return enif_make_badarg(env);

        head = tail;

        iov = iovec->iov;
        size = iovec->size;
        iovcnt = iovec->iovcnt;

        do {
#ifndef __WIN32__
            do {
                res = writev(tty->ofd, iov, iovcnt);
            } while(res < 0 && (errno == EINTR || errno == EAGAIN));
#else
            for (int i = 0; i < iovec->iovcnt; i++) {
                ssize_t written;
                BOOL r = WriteFile(tty->ofd, iovec->iov[i].iov_base,
                                   iovec->iov[i].iov_len, &written, NULL);
                if (!r) {
                    res = -1;
                    break;
                }
                res += written;
            }
#endif
            if (res < 0) {
                if (q) enif_ioq_destroy(q);
                return make_error(env, make_errno(env));
            }
            if (res != size) {
                if (!q) {
                    q = enif_ioq_create(ERL_NIF_IOQ_NORMAL);
                    enif_ioq_enqv(q, iovec, 0);
                }
            }

            if (q) {
                enif_ioq_deq(q, res, &size);
                if (size == 0) {
                    enif_ioq_destroy(q);
                    q = NULL;
                } else {
                    iov = enif_ioq_peek(q, &iovcnt);
                }
            }
        } while(q);

    };
    return atom_ok;
}

static ERL_NIF_TERM tty_read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    ErlNifBinary bin;
    ERL_NIF_TERM res_term;
    ssize_t res = 0;

    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

#ifdef __WIN32__
    if (tty->dwInMode) {
        ssize_t inputs_read, num_characters = 0;
        wchar_t *characters = NULL;
        INPUT_RECORD inputs[128];
        if (!ReadConsoleInputW(tty->ifd, inputs, sizeof(inputs)/sizeof(*inputs),
                              &inputs_read)) {
            return make_errno_error(env, "ReadConsoleInput");
        }
        for (int i = 0; i < inputs_read; i++) {
            if (inputs[i].EventType == KEY_EVENT) {
                if (inputs[i].Event.KeyEvent.bKeyDown &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar < 256 &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    num_characters++;
                }
                if (!inputs[i].Event.KeyEvent.bKeyDown &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar > 255 &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    num_characters++;
                }
            }
        }
        enif_alloc_binary(num_characters * sizeof(wchar_t), &bin);
        characters = (wchar_t*)bin.data;
        for (int i = 0; i < inputs_read; i++) {
            switch (inputs[i].EventType)
            {
            case KEY_EVENT:
                if (inputs[i].Event.KeyEvent.bKeyDown &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar < 256 &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    characters[res++] = inputs[i].Event.KeyEvent.uChar.UnicodeChar;
                }
                if (!inputs[i].Event.KeyEvent.bKeyDown &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar > 255 &&
                    inputs[i].Event.KeyEvent.uChar.UnicodeChar != 0) {
                    characters[res++] = inputs[i].Event.KeyEvent.uChar.UnicodeChar;
                }
                break;
            case WINDOW_BUFFER_SIZE_EVENT:
                enif_send(env, &tty->self, NULL,
                    enif_make_tuple2(env, enif_make_atom(env, "resize"),
                        enif_make_tuple2(env,
                            enif_make_int(env, inputs[i].Event.WindowBufferSizeEvent.dwSize.Y),
                            enif_make_int(env, inputs[i].Event.WindowBufferSizeEvent.dwSize.X))));
                break;
            case MENU_EVENT:
            case FOCUS_EVENT:
                /* Should be ignored according to
                   https://docs.microsoft.com/en-us/windows/console/input-record-str */
                break;
            default:
                fprintf(stderr,"Unknown event: %d\r\n", inputs[i].EventType);
                break;
            }
        }
        res *= sizeof(wchar_t);
    } else {
        DWORD bytesTransferred;
        enif_alloc_binary(1024, &bin);
        if (ReadFile(tty->ifd, bin.data, bin.size,
                     &bytesTransferred, NULL)) {
            res = bytesTransferred;
            if (res == 0) {
                enif_release_binary(&bin);
                return make_error(env, enif_make_atom(env, "closed"));
            }
        } else {
            DWORD error = GetLastError();
            enif_release_binary(&bin);
            if (error == ERROR_BROKEN_PIPE)
                return make_error(env, enif_make_atom(env, "closed"));
            return make_errno_error(env, "ReadFile");
        }
    }
#else
    enif_alloc_binary(1024, &bin);
    res = read(tty->ifd, bin.data, bin.size);
    if (res < 0) {
        if (errno != EAGAIN && errno != EINTR) {
            enif_release_binary(&bin);
            return make_errno_error(env, "read");
        }
        res = 0;
    } else if (res == 0) {
        enif_release_binary(&bin);
        return make_error(env, enif_make_atom(env, "closed"));
    }
#endif
    enif_select(env, tty->ifd, ERL_NIF_SELECT_READ, tty, NULL, argv[1]);
    if (res == bin.size) {
	res_term = enif_make_binary(env, &bin);
    } else if (res < bin.size / 2) {
        unsigned char *buff = enif_make_new_binary(env, res, &res_term);
        if (res > 0) {
            memcpy(buff, bin.data, res);
        }
        enif_release_binary(&bin);
    } else {
        enif_realloc_binary(&bin, res);
        res_term = enif_make_binary(env, &bin);
    }

    return enif_make_tuple2(env, atom_ok, res_term);
}

static ERL_NIF_TERM setlocale_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef __WIN32__
    TTYResource *tty;

    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

    if (tty->dwOutMode)
    {
        if (!SetConsoleOutputCP(CP_UTF8)) {
            return make_errno_error(env, "SetConsoleOutputCP");
        }
    }
    return atom_true;
#elif defined(PRIMITIVE_UTF8_CHECK)
    setlocale(LC_CTYPE, "");  /* Set international environment, 
				 ignore result */
    return enif_make_atom(env, "primitive");
#else
    char *l = setlocale(LC_CTYPE, "");  /* Set international environment */
    if (l != NULL) {
	if (strcmp(nl_langinfo(CODESET), "UTF-8") == 0)
            return atom_true;
    }
    return atom_false;
#endif
}

static ERL_NIF_TERM tty_tgetent_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef HAVE_TERMCAP
    ErlNifBinary TERM;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
    if (tgetent((char *)NULL /* ignored */, (char *)TERM.data) <= 0) {
        return make_errno_error(env, "tgetent");
    }
    return atom_ok;
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_tgetnum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef HAVE_TERMCAP
    ErlNifBinary TERM;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
    return enif_make_int(env, tgetnum((char*)TERM.data));
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_tgetflag_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef HAVE_TERMCAP
    ErlNifBinary TERM;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
    if (tgetflag((char*)TERM.data))
        return atom_true;
    return atom_false;
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_tgetstr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef HAVE_TERMCAP
    ErlNifBinary TERM, ret;
    /* tgetstr seems to use a lot of stack buffer space,
       so buff needs to be relatively "small" */
    char *str = NULL;
    char buff[BUFSIZ] = {0};

    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM))
        return enif_make_badarg(env);
    str = tgetstr((char*)TERM.data, (char**)&buff);
    if (!str) return atom_false;
    enif_alloc_binary(strlen(str), &ret);
    memcpy(ret.data, str, strlen(str));
    return enif_make_tuple2(
        env, atom_ok, enif_make_binary(env, &ret));
#else
    return make_enotsup(env);
#endif
}

#ifdef HAVE_TERMCAP
static int tputs_buffer_index;
static unsigned char tputs_buffer[1024];

#if defined(__sun) && defined(__SVR4) /* Solaris */
static int tty_puts_putc(char c) {
#else
static int tty_puts_putc(int c) {
#endif
    tputs_buffer[tputs_buffer_index++] = (unsigned char)c;
    return 0;
}
#endif

static ERL_NIF_TERM tty_tgoto_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#ifdef HAVE_TERMCAP
    ErlNifBinary TERM;
    char *ent;
    int value1, value2 = 0;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &TERM) ||
        !enif_get_int(env, argv[1], &value1))
        return enif_make_badarg(env);
    if (argc == 2) {
        ent = tgoto((char*)TERM.data, 0, value1);
    } else {
        ASSERT(argc == 3);
        ent = tgoto((char*)TERM.data, value1, value2);
    }
    if (!ent) return make_errno_error(env, "tgoto");

    tputs_buffer_index = 0;
    if (tputs(ent, 1, tty_puts_putc)) {
        return make_errno_error(env, "tputs");
    } else {
        ERL_NIF_TERM ret;
        unsigned char *buff = enif_make_new_binary(env, tputs_buffer_index, &ret);
        memcpy(buff, tputs_buffer, tputs_buffer_index);
        return enif_make_tuple2(env, atom_ok, ret);
    }
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_create_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    TTYResource *tty = enif_alloc_resource(tty_rt, sizeof(TTYResource));
    ERL_NIF_TERM tty_term;
    memset(tty, 0, sizeof(*tty));
#ifndef __WIN32__
    tty->ifd = 0;
    tty->ofd = 1;
#else
    tty->ifd = GetStdHandle(STD_INPUT_HANDLE);
    if (tty->ifd == INVALID_HANDLE_VALUE || tty->ifd == NULL) {
        tty->ifd = CreateFile("nul", GENERIC_READ, 0,
                              NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    }
    tty->ofd = GetStdHandle(STD_OUTPUT_HANDLE);
    if (tty->ofd == INVALID_HANDLE_VALUE || tty->ofd == NULL) {
        tty->ofd = CreateFile("nul", GENERIC_WRITE, 0,
                              NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    }
    if (GetConsoleMode(tty->ofd, &tty->dwOriginalOutMode))
    {
        tty->dwOutMode = ENABLE_VIRTUAL_TERMINAL_PROCESSING | tty->dwOriginalOutMode;
        if (!SetConsoleMode(tty->ofd, tty->dwOutMode)) {
            /* Failed to set any VT mode, can't do anything here. */
            return make_errno_error(env, "SetConsoleMode");
        }
    }
    if (GetConsoleMode(tty->ifd, &tty->dwOriginalInMode))
    {
        tty->dwInMode = ENABLE_VIRTUAL_TERMINAL_INPUT | tty->dwOriginalInMode;
        if (!SetConsoleMode(tty->ifd, tty->dwInMode)) {
            /* Failed to set any VT mode, can't do anything here. */
            return make_errno_error(env, "SetConsoleMode");
        }
    }
#endif

    tty_term = enif_make_resource(env, tty);
    enif_release_resource(tty);

    enif_set_pid_undefined(&tty->self);
    enif_set_pid_undefined(&tty->reader);

    return enif_make_tuple2(env, atom_ok, tty_term);
}

static ERL_NIF_TERM tty_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

#if defined(HAVE_TERMCAP) || defined(__WIN32__)
    ERL_NIF_TERM canon, echo, sig;
    TTYResource *tty;
    int fd;

    if (argc != 3 ||
        !tty_get_fd(env, argv[1], &fd) ||
        !enif_is_map(env, argv[2])) {
        return enif_make_badarg(env);
    }

    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

    if (!enif_get_map_value(env, argv[2], enif_make_atom(env,"canon"), &canon))
        canon = enif_make_atom(env, "undefined");
    if (!enif_get_map_value(env, argv[2], enif_make_atom(env,"echo"), &echo))
        echo = enif_make_atom(env, "undefined");
    if (!enif_get_map_value(env, argv[2], enif_make_atom(env,"sig"), &sig))
        sig = enif_make_atom(env, "undefined");

#ifndef __WIN32__
    if (tcgetattr(fd, &tty->tty_rmode) < 0) {
        return make_errno_error(env, "tcgetattr");
    }

    tty->tty_smode = tty->tty_rmode;

    /* Default characteristics for all usage including termcap output. */
    tty->tty_smode.c_iflag &= ~ISTRIP;

    /* erts_fprintf(stderr,"canon %T\r\n", canon); */
    /* Turn canonical (line mode) on off. */
    if (enif_is_identical(canon, atom_true)) {
        tty->tty_smode.c_iflag |= ICRNL;
        tty->tty_smode.c_lflag |= ICANON;
        tty->tty_smode.c_oflag |= OPOST;
        tty->tty_smode.c_cc[VEOF] = tty->tty_rmode.c_cc[VEOF];
#ifdef VDSUSP
        tty->tty_smode.c_cc[VDSUSP] = tty->tty_rmode.c_cc[VDSUSP];
#endif
    }
    if (enif_is_identical(canon, atom_false)) {
        tty->tty_smode.c_iflag &= ~ICRNL;
        tty->tty_smode.c_lflag &= ~ICANON;
        tty->tty_smode.c_oflag &= ~OPOST;

        tty->tty_smode.c_cc[VMIN] = 1;
        tty->tty_smode.c_cc[VTIME] = 0;
#ifdef VDSUSP
        tty->tty_smode.c_cc[VDSUSP] = 0;
#endif
    }

    /* Turn echo on or off. */
    /* erts_fprintf(stderr,"echo %T\r\n", echo); */
    if (enif_is_identical(echo, atom_true))
        tty->tty_smode.c_lflag |= ECHO;
    if (enif_is_identical(echo, atom_false))
        tty->tty_smode.c_lflag &= ~ECHO;

    /* erts_fprintf(stderr,"sig %T\r\n", sig); */
    /* Set extra characteristics for "RAW" mode, no signals. */
    if (enif_is_identical(sig, atom_true)) {
        /* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
        tty->tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
        tty->tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON);
#endif
        tty->tty_smode.c_lflag |= (ISIG|IEXTEN);
    }
    if (enif_is_identical(sig, atom_false)) {
        /* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
        tty->tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
        tty->tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON);
#endif
        tty->tty_smode.c_lflag &= ~(ISIG|IEXTEN);
    }

#else
    /* fprintf(stderr, "origOutMode: %x origInMode: %x\r\n", */
    /*     tty->dwOriginalOutMode, tty->dwOriginalInMode); */

    /* If we cannot disable NEWLINE_AUTO_RETURN we continue anyway as things work */
    if (SetConsoleMode(tty->ofd, tty->dwOutMode | DISABLE_NEWLINE_AUTO_RETURN)) {
        tty->dwOutMode |= DISABLE_NEWLINE_AUTO_RETURN;
    }

    tty->dwInMode &= ~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT);
    if (!SetConsoleMode(tty->ifd, tty->dwInMode))
    {
        /* Failed to set disable echo or line input mode */
        return make_errno_error(env, "SetConsoleMode");
    }

#endif /* __WIN32__ */

    tty->tty = 1;

    return atom_ok;
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_set_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
#if defined(HAVE_TERMCAP) || defined(__WIN32__)
    TTYResource *tty;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
#ifdef HAVE_TERMCAP
    if (tty->tty && tcsetattr(tty->ifd, TCSANOW, &tty->tty_smode) < 0) {
        return make_errno_error(env, "tcsetattr");
    }
#endif
    enif_self(env, &tty->self);
    enif_monitor_process(env, tty, &tty->self, NULL);
    return atom_ok;
#else
    return make_enotsup(env);
#endif
}

static ERL_NIF_TERM tty_window_size_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    int width = -1, height = -1;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
    {
#ifdef TIOCGWINSZ
        struct winsize ws;
        if (ioctl(tty->ifd,TIOCGWINSZ,&ws) == 0) {
            if (ws.ws_col > 0)
                width = ws.ws_col;
            if (ws.ws_row > 0)
                height = ws.ws_row;
        } else if (ioctl(tty->ofd,TIOCGWINSZ,&ws) == 0) {
            if (ws.ws_col > 0)
                width = ws.ws_col;
            if (ws.ws_row > 0)
                height = ws.ws_row;
        }
#elif defined(__WIN32__)
        CONSOLE_SCREEN_BUFFER_INFOEX buffer_info;
        buffer_info.cbSize = sizeof(buffer_info);
        if (GetConsoleScreenBufferInfoEx(tty->ofd, &buffer_info)) {
            height = buffer_info.dwSize.Y;
            width = buffer_info.dwSize.X;
        } else {
            return make_errno_error(env,"GetConsoleScreenBufferInfoEx");
        }
#endif
    }
    if (width == -1 && height == -1) {
        return make_enotsup(env);
    }
    return enif_make_tuple2(
                env, atom_ok,
                enif_make_tuple2(
                    env,
                    enif_make_int(env, width),
                    enif_make_int(env, height)
                    ));
}

#ifndef __WIN32__

static int tty_signal_fd = -1;

static RETSIGTYPE tty_cont(int sig)
{
    if (tty_signal_fd != 1) {
        while (write(tty_signal_fd, "c", 1) < 0 && errno == EINTR) { };
    }
}


static RETSIGTYPE tty_winch(int sig)
{
    if (tty_signal_fd != 1) {
        while (write(tty_signal_fd, "w", 1) < 0 && errno == EINTR) { };
    }
}

#endif

static ERL_NIF_TERM tty_read_signal_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
    char buff[1];
    ssize_t ret;
    ERL_NIF_TERM res;
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);
#ifndef __WIN32__
    do {
        ret = read(tty->signal[0], buff, 1);
    } while (ret < 0 && errno == EAGAIN);

    if (ret < 0) {
        return make_errno_error(env, "read");
    } else if (ret == 0) {
        return make_error(env, enif_make_atom(env,"empty"));
    }

    enif_select(env, tty->signal[0], ERL_NIF_SELECT_READ, tty, NULL, argv[1]);

    if (buff[0] == 'w') {
        res = enif_make_atom(env, "winch");
    } else if (buff[0] == 'c') {
        res = enif_make_atom(env, "cont");
    } else {
        res = enif_make_string_len(env, buff, 1, ERL_NIF_LATIN1);
    }
    return enif_make_tuple2(env, atom_ok, res);
#else
    return make_enotsup(env);
#endif
}

#ifdef THREADED_READED
struct tty_reader_init {
    ErlNifEnv *env;
    ERL_NIF_TERM tty;
};

#define TTY_READER_BUF_SIZE 1024

static void *tty_reader_thread(void *args) {
    struct tty_reader_init *tty_reader_init = (struct tty_reader_init*)args;
    TTYResource *tty;
    ErlNifBinary binary;
    ErlNifEnv *env = NULL;
    ERL_NIF_TERM data[10];
    int cnt = 0;

    enif_alloc_binary(TTY_READER_BUF_SIZE, &binary);

    enif_get_resource(tty_reader_init->env, tty_reader_init->tty, tty_rt, (void **)&tty);

    SET_BLOCKING(tty->ifd);

    while(true) {
        ssize_t i = read(tty->ifd, binary.data, TTY_READER_BUF_SIZE);
        /* fprintf(stderr,"Read: %ld bytes from %d\r\n", i, tty->ifd); */
        if (i < 0) {
            int saved_errno = errno;
            if (env) {
                ERL_NIF_TERM msg = enif_make_list_from_array(env, data, cnt);
                enif_send(env, &tty->self, NULL, enif_make_tuple2(env, atom_input, msg));
                cnt = 0;
                env = NULL;
            }
            if (saved_errno != EAGAIN) {
                env = enif_alloc_env();
                errno = saved_errno;
                enif_send(env, &tty->self, NULL, make_errno_error(env, "read"));
                break;
            }
        } else {
            if (!env) {
                env = enif_alloc_env();
            }
            enif_realloc_binary(&binary, i);
            data[cnt++] = enif_make_binary(env, &binary);
            if (cnt == 10 || i != TTY_READER_BUF_SIZE) {
                ERL_NIF_TERM msg = enif_make_list_from_array(env, data, cnt);
                enif_send(env, &tty->self, NULL, enif_make_tuple2(env, atom_input, msg));
                cnt = 0;
                env = NULL;
            }
            enif_alloc_binary(TTY_READER_BUF_SIZE, &binary);
        }
    }

    enif_free_env(tty_reader_init->env);
    enif_free(tty_reader_init);
    return (void*)0;
}

#endif

static ERL_NIF_TERM tty_select_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    TTYResource *tty;
#ifdef THREADED_READER
    struct tty_reader_init *tty_reader_init;
#endif
#ifndef __WIN32__
    extern int using_oldshell; /* set this to let the rest of erts know */
#endif
    if (!enif_get_resource(env, argv[0], tty_rt, (void **)&tty))
        return enif_make_badarg(env);

#ifndef __WIN32__
    if (pipe(tty->signal) == -1) {
        return make_errno_error(env, "pipe");
    }
    SET_NONBLOCKING(tty->signal[0]);
    enif_select(env, tty->signal[0], ERL_NIF_SELECT_READ, tty, NULL, argv[1]);
    tty_signal_fd = tty->signal[1];

    sys_signal(SIGCONT, tty_cont);
    sys_signal(SIGWINCH, tty_winch);

    using_oldshell = 0;

#endif

    enif_select(env, tty->ifd, ERL_NIF_SELECT_READ, tty, NULL, argv[2]);

    enif_self(env, &tty->reader);
    enif_monitor_process(env, tty, &tty->reader, NULL);

#ifdef THREADED_READER

    tty_reader_init = enif_alloc(sizeof(struct tty_reader_init));
    tty_reader_init->env = enif_alloc_env();
    tty_reader_init->tty = enif_make_copy(tty_reader_init->env, argv[0]);

    if (enif_thread_create(
            "stdin_reader",
            &tty->reader_tid,
            tty_reader_thread, tty_reader_init, NULL)) {
        enif_free(tty_reader_init);
        return make_errno_error(env, "enif_thread_create");
    }
#endif
    return atom_ok;
}

static void tty_monitor_down(ErlNifEnv* caller_env, void* obj, ErlNifPid* pid, ErlNifMonitor* mon) {
    TTYResource *tty = obj;
#ifdef HAVE_TERMCAP
    if (enif_compare_pids(pid, &tty->self) == 0) {
        tcsetattr(tty->ifd, TCSANOW, &tty->tty_rmode);
    }
#endif
    if (enif_compare_pids(pid, &tty->reader) == 0) {
        enif_select(caller_env, tty->ifd, ERL_NIF_SELECT_STOP, tty, NULL, atom_undefined);
#ifndef __WIN32__
        enif_select(caller_env, tty->signal[0], ERL_NIF_SELECT_STOP, tty, NULL, atom_undefined);
        close(tty->signal[1]);
        sys_signal(SIGCONT, SIG_DFL);
        sys_signal(SIGWINCH, SIG_DFL);
#endif
    }
}

static void tty_select_stop(ErlNifEnv* caller_env, void* obj, ErlNifEvent event, int is_direct_call) {
/* Only used to close the signal pipe on unix */
#ifndef __WIN32__
    if (event != 0)
        close(event);
#endif
}

static void load_resources(ErlNifEnv* env, ErlNifResourceFlags rt_flags) {
    ErlNifResourceTypeInit rt = {
        NULL /* dtor */,
        tty_select_stop,
        tty_monitor_down};

#define ATOM_DECL(A) atom_##A = enif_make_atom(env, #A)
ATOMS
#undef ATOM_DECL

    tty_rt = enif_open_resource_type_x(env, "tty", &rt, rt_flags, NULL);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = NULL;
    load_resources(env, ERL_NIF_RT_CREATE);
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
    *priv_data = NULL;
    load_resources(env, ERL_NIF_RT_TAKEOVER);
    return 0;
}
