/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef ISC32
#define _POSIX_SOURCE
#define _XOPEN_SOURCE
#endif

#include <sys/times.h>		/* ! */
#include <time.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/uio.h>
#include <termios.h>
#include <ctype.h>
#include <sys/utsname.h>
#include <sys/select.h>
#include <arpa/inet.h>

#ifdef ISC32
#include <sys/bsdtypes.h>
#endif

#include <termios.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#define WANT_NONBLOCKING    /* must define this to pull in defs from sys.h */
#include "sys.h"

#ifdef USE_THREADS
#include "erl_threads.h"
#endif

extern char **environ;
extern erts_smp_rwmtx_t environ_rwmtx;

extern erts_smp_atomic_t sys_misc_mem_sz;

static Eterm forker_port;

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */
/*
 * Don't need global.h, but erl_cpu_topology.h won't compile otherwise
 */
#include "global.h"
#include "erl_cpu_topology.h"

#include "erl_sys_driver.h"
#include "sys_uds.h"

#include "erl_child_setup.h"

#if defined IOV_MAX
#define MAXIOV IOV_MAX
#elif defined UIO_MAXIOV
#define MAXIOV UIO_MAXIOV
#else
#define MAXIOV 16
#endif

#ifdef USE_THREADS
#  define FDBLOCK 1
#else
#  define FDBLOCK 0
#endif

/* Used by the fd driver iff the fd could not be set to non-blocking */
typedef struct ErtsSysBlocking_ {
    ErlDrvPDL pdl;
    int res;
    int err;
    unsigned int pkey;
} ErtsSysBlocking;

typedef struct fd_data {
    int   fd;
    char  pbuf[4];   /* hold partial packet bytes */
    int   psz;       /* size of pbuf */
    char  *buf;
    char  *cpos;
    int   sz;
    int   remain;  /* for input on fd */
} ErtsSysFdData;

typedef struct driver_data {
    ErlDrvPort port_num;
    ErtsSysFdData *ofd;
    ErtsSysFdData *ifd;
    int packet_bytes;
    int pid;
    int alive;
    int status;
    int terminating;
    ErtsSysBlocking *blocking;
} ErtsSysDriverData;

#define DIR_SEPARATOR_CHAR    '/'

#if defined(__ANDROID__)
#define SHELL "/system/bin/sh"
#else
#define SHELL "/bin/sh"
#endif /* __ANDROID__ */

#if defined(DEBUG)
#define ERL_BUILD_TYPE_MARKER ".debug"
#elif defined(PURIFY)
#define ERL_BUILD_TYPE_MARKER ".purify"
#elif defined(QUANTIFY)
#define ERL_BUILD_TYPE_MARKER ".quantify"
#elif defined(PURECOV)
#define ERL_BUILD_TYPE_MARKER ".purecov"
#elif defined(VALGRIND)
#define ERL_BUILD_TYPE_MARKER ".valgrind"
#else /* opt */
#define ERL_BUILD_TYPE_MARKER
#endif

#ifdef DEBUG
#define close(fd) do { int res = close(fd); ASSERT(res > -1); } while(0)
#endif

#define CHILD_SETUP_PROG_NAME	"erl_child_setup" ERL_BUILD_TYPE_MARKER

// #define HARD_DEBUG
#ifdef HARD_DEBUG
#define driver_select(port_num, fd, flags, onoff)                       \
    do {                                                                \
        if (((flags) & ERL_DRV_READ) && onoff)                          \
            fprintf(stderr,"%010d %p: read select %d\r\n", __LINE__, port_num, (int)fd); \
        if (((flags) & ERL_DRV_WRITE) && onoff)                         \
            fprintf(stderr,"%010d %p: writ select %d\r\n", __LINE__, port_num, (int)fd); \
        if (((flags) & ERL_DRV_READ) && !onoff)                          \
            fprintf(stderr,"%010d %p: read unsele %d\r\n", __LINE__, port_num, (int)fd); \
        if (((flags) & ERL_DRV_WRITE) && !onoff)                         \
            fprintf(stderr,"%010d %p: writ unsele %d\r\n", __LINE__, port_num, (int)fd); \
        driver_select_nkp(port_num, fd, flags, onoff);                  \
    } while(0)
#endif

/*
 * Decreasing the size of it below 16384 is not allowed.
 */

#define ERTS_SYS_READ_BUF_SZ (64*1024)

/* I. Initialization */

void
erl_sys_late_init(void)
{
    SysDriverOpts opts;
#ifdef ERTS_SMP
    Port *port;
#endif

    sys_signal(SIGPIPE, SIG_IGN); /* Ignore - we'll handle the write failure */

    opts.packet_bytes = 0;
    opts.use_stdio = 1;
    opts.redir_stderr = 0;
    opts.read_write = 0;
    opts.hide_window = 0;
    opts.wd = NULL;
    opts.envir = NULL;
    opts.exit_status = 0;
    opts.overlapped_io = 0;
    opts.spawn_type = ERTS_SPAWN_ANY;
    opts.argv = NULL;
    opts.parallelism = erts_port_parallelism;

#ifdef ERTS_SMP
    port =
#endif
        erts_open_driver(&forker_driver, make_internal_pid(0), "forker", &opts, NULL, NULL);
#ifdef ERTS_SMP
    erts_mtx_unlock(port->lock);
#endif
    erts_sys_unix_later_init(); /* Need to be called after forker has been started */
}

/* II. Prototypes */

/* II.I Spawn prototypes */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvSSizeT spawn_control(ErlDrvData, unsigned int, char *,
                                  ErlDrvSizeT, char **, ErlDrvSizeT);

/* II.II Vanilla prototypes */
static ErlDrvData vanilla_start(ErlDrvPort, char*, SysDriverOpts*);


/* II.III FD prototypes */
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
#if FDBLOCK
static void fd_async(void *);
static void fd_ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data);
#endif
static ErlDrvSSizeT fd_control(ErlDrvData, unsigned int, char *, ErlDrvSizeT,
			       char **, ErlDrvSizeT);
static void fd_stop(ErlDrvData);
static void fd_flush(ErlDrvData);

/* II.IV Common prototypes */
static void stop(ErlDrvData);
static void ready_input(ErlDrvData, ErlDrvEvent);
static void ready_output(ErlDrvData, ErlDrvEvent);
static void output(ErlDrvData, char*, ErlDrvSizeT);
static void outputv(ErlDrvData, ErlIOVec*);
static void stop_select(ErlDrvEvent, void*);

/* II.V Forker prototypes */
static ErlDrvData forker_start(ErlDrvPort, char*, SysDriverOpts*);
static void forker_stop(ErlDrvData);
static void forker_ready_input(ErlDrvData, ErlDrvEvent);
static void forker_ready_output(ErlDrvData, ErlDrvEvent);
static ErlDrvSSizeT forker_control(ErlDrvData, unsigned int, char *,
                                   ErlDrvSizeT, char **, ErlDrvSizeT);

/* III Driver entries */

/* III.I The spawn driver */
struct erl_drv_entry spawn_driver_entry = {
    NULL,
    spawn_start,
    stop,
    output,
    ready_input,
    ready_output,
    "spawn",
    NULL,
    NULL,
    spawn_control,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING | ERL_DRV_FLAG_USE_INIT_ACK,
    NULL, NULL,
    stop_select
};

/* III.II The fd driver */
struct erl_drv_entry fd_driver_entry = {
    NULL,
    fd_start,
    fd_stop,
    output,
    ready_input,
    ready_output,
    "fd",
    NULL,
    NULL,
    fd_control,
    NULL,
    outputv,
#if FDBLOCK
    fd_ready_async, /* ready_async */
#else
    NULL,
#endif
    fd_flush, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0, /* ERL_DRV_FLAGs */
    NULL, /* handle2 */
    NULL, /* process_exit */
    stop_select
};

/* III.III The vanilla driver */
struct erl_drv_entry vanilla_driver_entry = {
    NULL,
    vanilla_start,
    stop,
    output,
    ready_input,
    ready_output,
    "vanilla",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL, /* flush */
    NULL, /* call */
    NULL, /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0, /* ERL_DRV_FLAGs */
    NULL, /* handle2 */
    NULL, /* process_exit */
    stop_select
};

/* III.III The forker driver */
struct erl_drv_entry forker_driver_entry = {
    NULL,
    forker_start,
    forker_stop,
    NULL,
    forker_ready_input,
    forker_ready_output,
    "spawn_forker",
    NULL,
    NULL,
    forker_control,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL, NULL,
    stop_select
};

/* Untility functions */

static int set_blocking_data(ErtsSysDriverData *dd) {

    dd->blocking = erts_alloc(ERTS_ALC_T_SYS_BLOCKING, sizeof(ErtsSysBlocking));

    erts_smp_atomic_add_nob(&sys_misc_mem_sz, sizeof(ErtsSysBlocking));

    dd->blocking->pdl = driver_pdl_create(dd->port_num);
    dd->blocking->res = 0;
    dd->blocking->err = 0;
    dd->blocking->pkey = driver_async_port_key(dd->port_num);

    return 1;
}

static void init_fd_data(ErtsSysFdData *fd_data, int fd)
{
    fd_data->fd = fd;
    fd_data->buf = NULL;
    fd_data->cpos = NULL;
    fd_data->remain = 0;
    fd_data->sz = 0;
    fd_data->psz = 0;
}

static ErtsSysDriverData *
create_driver_data(ErlDrvPort port_num,
                   int ifd,
                   int ofd,
                   int packet_bytes,
                   int read_write,
                   int exit_status,
                   int pid,
                   int is_blocking)
{
    Port *prt;
    ErtsSysDriverData *driver_data;
    char *data;
    int size = sizeof(ErtsSysDriverData);

    if (read_write & DO_READ)
        size += sizeof(ErtsSysFdData);

    if ((read_write & DO_WRITE) &&
        ((ifd != ofd || ofd == -1) || !(read_write & DO_READ)))
        size += sizeof(ErtsSysFdData);

    data = erts_alloc(ERTS_ALC_T_DRV_TAB,size);
    erts_smp_atomic_add_nob(&sys_misc_mem_sz, size);

    driver_data = (ErtsSysDriverData*)data;
    data += sizeof(*driver_data);

    prt = erts_drvport2port(port_num);
    if (prt != ERTS_INVALID_ERL_DRV_PORT)
	prt->os_pid = pid;

    driver_data->packet_bytes = packet_bytes;
    driver_data->port_num = port_num;
    driver_data->pid = pid;
    driver_data->alive = exit_status ? 1 : 0;
    driver_data->status = 0;
    driver_data->terminating = 0;
    driver_data->blocking = NULL;

    if (read_write & DO_READ) {
        driver_data->ifd = (ErtsSysFdData*)data;
        data += sizeof(*driver_data->ifd);
        init_fd_data(driver_data->ifd, ifd);
        driver_select(port_num, ifd, (ERL_DRV_READ|ERL_DRV_USE), 1);
    } else {
        driver_data->ifd = NULL;
    }

    if (read_write & DO_WRITE) {
        if (ofd != -1 && ifd == ofd && read_write & DO_READ) {
            /* This is for when ifd and ofd are the same fd */
            driver_data->ofd = driver_data->ifd;
        } else {
            driver_data->ofd = (ErtsSysFdData*)data;
            data += sizeof(*driver_data->ofd);
            init_fd_data(driver_data->ofd, ofd);
        }
        if (is_blocking && FDBLOCK)
            if (!set_blocking_data(driver_data)) {
                erts_free(ERTS_ALC_T_DRV_TAB, driver_data);
                return NULL;
            }
    } else {
        driver_data->ofd = NULL;
    }

    return driver_data;
}

/* Spawn driver */

static void close_pipes(int ifd[2], int ofd[2])
{
    close(ifd[0]);
    close(ifd[1]);
    close(ofd[0]);
    close(ofd[1]);
}

static char **build_unix_environment(char *block)
{
    int i;
    int j;
    int len;
    char *cp;
    char **cpp;
    char** old_env;
    
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rlocked(&environ_rwmtx));

    cp = block;
    len = 0;
    while (*cp != '\0') {
	cp += strlen(cp) + 1;
	len++;
    }
    old_env = environ;
    while (*old_env++ != NULL) {
	len++;
    }
    
    cpp = (char **) erts_alloc_fnf(ERTS_ALC_T_ENVIRONMENT,
				   sizeof(char *) * (len+1));
    if (cpp == NULL) {
	return NULL;
    }

    cp = block;
    len = 0;
    while (*cp != '\0') {
	cpp[len] = cp;
	cp += strlen(cp) + 1;
	len++;
    }
    
    i = len;
    for (old_env = environ; *old_env; old_env++) {
	char* old = *old_env;

	for (j = 0; j < len; j++) {
	    char *s, *t;

	    /* check if cpp[j] equals old
	       before the = sign,
	       i.e.
	       "TMPDIR=/tmp/" */
	    s = cpp[j];
	    t = old;
	    while (*s == *t && *s != '=') {
		s++, t++;
	    }
	    if (*s == '=' && *t == '=') {
		break;
	    }
	}

	if (j == len) {		/* New version not found */
	    cpp[len++] = old;
	}
    }

    for (j = 0; j < i; ) {
        size_t last = strlen(cpp[j])-1;
	if (cpp[j][last] == '=' && strchr(cpp[j], '=') == cpp[j]+last) {
	    cpp[j] = cpp[--len];
	    if (len < i) {
		i--;
	    } else {
		j++;
	    }
	}
	else {
	    j++;
	}
    }

    cpp[len] = NULL;
    return cpp;
}

static ErlDrvData spawn_start(ErlDrvPort port_num, char* name,
                              SysDriverOpts* opts)
{
#define CMD_LINE_PREFIX_STR "exec "
#define CMD_LINE_PREFIX_STR_SZ (sizeof(CMD_LINE_PREFIX_STR) - 1)

    int len;
    char **new_environ;
    ErtsSysDriverData *dd;
    char *cmd_line;
    char wd_buff[MAXPATHLEN+1];
    char *wd, *cwd;
    int ifd[2], ofd[2], stderrfd;

    if (pipe(ifd) < 0) return ERL_DRV_ERROR_ERRNO;
    errno = EMFILE;		/* default for next three conditions */
    if (ifd[0] >= sys_max_files() || pipe(ofd) < 0) {
        close(ifd[0]);
        close(ifd[1]);
        return ERL_DRV_ERROR_ERRNO;
    }
    if (ofd[1] >= sys_max_files()) {
        close_pipes(ifd, ofd);
        errno = EMFILE;
        return ERL_DRV_ERROR_ERRNO;
    }

    SET_NONBLOCKING(ifd[0]);
    SET_NONBLOCKING(ofd[1]);

    stderrfd = opts->redir_stderr ? ifd[1] : dup(2);

    if (stderrfd >= sys_max_files() || stderrfd < 0) {
        close_pipes(ifd, ofd);
        if (stderrfd > -1)
            close(stderrfd);
        return ERL_DRV_ERROR_ERRNO;
    }

    if (opts->spawn_type == ERTS_SPAWN_EXECUTABLE) {
	/* started with spawn_executable, not with spawn */
	len = strlen(name);
	cmd_line = (char *) erts_alloc_fnf(ERTS_ALC_T_TMP, len + 1);
	if (!cmd_line) {
            close_pipes(ifd, ofd);
	    errno = ENOMEM;
	    return ERL_DRV_ERROR_ERRNO;
	}
	memcpy((void *) cmd_line,(void *) name, len);
	cmd_line[len] = '\0';
	len = len + 1;
	if (access(cmd_line,X_OK) != 0) {
	    int save_errno = errno;
	    erts_free(ERTS_ALC_T_TMP, cmd_line);
            close_pipes(ifd, ofd);
	    errno = save_errno;
	    return ERL_DRV_ERROR_ERRNO;
	}
    } else {
	/* make the string suitable for giving to "sh" */
	len = strlen(name);
	cmd_line = (char *) erts_alloc_fnf(ERTS_ALC_T_TMP,
					   CMD_LINE_PREFIX_STR_SZ + len + 1);
	if (!cmd_line) {
            close_pipes(ifd, ofd);
	    errno = ENOMEM;
	    return ERL_DRV_ERROR_ERRNO;
	}
	memcpy((void *) cmd_line,
	       (void *) CMD_LINE_PREFIX_STR,
	       CMD_LINE_PREFIX_STR_SZ);
	memcpy((void *) (cmd_line + CMD_LINE_PREFIX_STR_SZ), (void *) name, len);
	cmd_line[CMD_LINE_PREFIX_STR_SZ + len] = '\0';
	len = CMD_LINE_PREFIX_STR_SZ + len + 1;
    }

    erts_smp_rwmtx_rlock(&environ_rwmtx);

    if (opts->envir == NULL) {
	new_environ = environ;
    } else if ((new_environ = build_unix_environment(opts->envir)) == NULL) {
	erts_smp_rwmtx_runlock(&environ_rwmtx);
        close_pipes(ifd, ofd);
	erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }

    if ((cwd = getcwd(wd_buff, MAXPATHLEN+1)) == NULL) {
        /* on some OSs this call opens a fd in the
           background which means that this can
           return EMFILE */
        int err = errno;
        close_pipes(ifd, ofd);
        erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);
        if (new_environ != environ)
            erts_free(ERTS_ALC_T_ENVIRONMENT, (void *) new_environ);
        erts_smp_rwmtx_runlock(&environ_rwmtx);
        errno = err;
        return ERL_DRV_ERROR_ERRNO;
    }

    wd = opts->wd;

    {
        struct iovec *io_vector;
        int iov_len = 5;
        char nullbuff[] = "\0";
        int j, i = 0, res;
        Sint32 buffsz = 0, env_len = 0, argv_len = 0,
            flags = (opts->use_stdio ? FORKER_FLAG_USE_STDIO : 0)
            | (opts->exit_status ? FORKER_FLAG_EXIT_STATUS : 0)
            | (opts->read_write & DO_READ ? FORKER_FLAG_DO_READ : 0)
            | (opts->read_write & DO_WRITE ? FORKER_FLAG_DO_WRITE : 0);

        if (wd) iov_len++;

        /* count number of elements in environment */
        while(new_environ[env_len] != NULL)
            env_len++;
        iov_len += 1 + env_len; /* num envs including size int */

        /* count number of element in argument list */
        if (opts->spawn_type == ERTS_SPAWN_EXECUTABLE) {
            if (opts->argv != NULL) {
                while(opts->argv[argv_len] != NULL)
                    argv_len++;
            } else {
                argv_len++;
            }
            iov_len += 1 + argv_len; /* num argvs including size int */
        }

        io_vector = erts_alloc_fnf(ERTS_ALC_T_TMP, sizeof(struct iovec) * iov_len);

        if (!io_vector) {
            close_pipes(ifd, ofd);
            erts_smp_rwmtx_runlock(&environ_rwmtx);
            erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);
            if (new_environ != environ)
                erts_free(ERTS_ALC_T_ENVIRONMENT, (void *) new_environ);
            errno = ENOMEM;
            return ERL_DRV_ERROR_ERRNO;
        }

        /*
         * Whitebox test port_SUITE:pipe_limit_env
         * assumes this command payload format.
         */
        io_vector[i].iov_base = (void*)&buffsz;
        io_vector[i++].iov_len = sizeof(buffsz);

        io_vector[i].iov_base = (void*)&flags;
        flags = htonl(flags);
        io_vector[i++].iov_len = sizeof(flags);
        buffsz += sizeof(flags);

        io_vector[i].iov_base = cmd_line;
        io_vector[i++].iov_len = len;
        buffsz += len;

        io_vector[i].iov_base = cwd;
        io_vector[i].iov_len = strlen(io_vector[i].iov_base) + 1;
        buffsz += io_vector[i++].iov_len;

        if (wd) {
            io_vector[i].iov_base = wd;
            io_vector[i].iov_len = strlen(io_vector[i].iov_base) + 1;
            buffsz += io_vector[i++].iov_len;
        }

        io_vector[i].iov_base = nullbuff;
        io_vector[i++].iov_len = 1;
        buffsz += io_vector[i-1].iov_len;

        io_vector[i].iov_base = (void*)&env_len;
        env_len = htonl(env_len);
        io_vector[i++].iov_len = sizeof(env_len);
        buffsz += io_vector[i-1].iov_len;

        for (j = 0; new_environ[j] != NULL; j++) {
            io_vector[i].iov_base = new_environ[j];
            io_vector[i++].iov_len = strlen(new_environ[j]) + 1;
            buffsz += io_vector[i-1].iov_len;
        }

        /* only append arguments if this was a spawn_executable */
        if (opts->spawn_type == ERTS_SPAWN_EXECUTABLE) {

            io_vector[i].iov_base = (void*)&argv_len;
            argv_len = htonl(argv_len);
            io_vector[i++].iov_len = sizeof(argv_len);
            buffsz += io_vector[i-1].iov_len;

            if (opts->argv) {
                /* If there are arguments we copy in the references to
                   them into the iov */
                for (j = 0; opts->argv[j]; j++) {
                    if (opts->argv[j] == erts_default_arg0)
                        io_vector[i].iov_base = cmd_line;
                    else
                        io_vector[i].iov_base = opts->argv[j];
                    io_vector[i].iov_len = strlen(io_vector[i].iov_base) + 1;
                    buffsz += io_vector[i++].iov_len;
                }
            } else {
                io_vector[i].iov_base = cmd_line;
                io_vector[i].iov_len = strlen(io_vector[i].iov_base) + 1;
                buffsz += io_vector[i++].iov_len;
            }
        }

        /* we send the request to do the fork */
        if ((res = writev(ofd[1], io_vector, iov_len > MAXIOV ? MAXIOV : iov_len)) < 0) {
            if (errno == ERRNO_BLOCK) {
                res = 0;
            } else {
                int err = errno;
                close_pipes(ifd, ofd);
                erts_free(ERTS_ALC_T_TMP, io_vector);
                if (new_environ != environ)
                    erts_free(ERTS_ALC_T_ENVIRONMENT, (void *) new_environ);
                erts_smp_rwmtx_runlock(&environ_rwmtx);
                erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);
                errno = err;
                return ERL_DRV_ERROR_ERRNO;
            }
        }

        if (res < (buffsz + sizeof(buffsz))) {
            /* we only wrote part of the command payload. Enqueue the rest. */
            for (i = 0; i < iov_len; i++) {
                if (res >= io_vector[i].iov_len)
                    res -= io_vector[i].iov_len;
                else {
                    driver_enq(port_num, io_vector[i].iov_base + res,
                               io_vector[i].iov_len - res);
                    res = 0;
                }
            }
            driver_select(port_num, ofd[1], ERL_DRV_WRITE|ERL_DRV_USE, 1);
        }

        erts_free(ERTS_ALC_T_TMP, io_vector);
    }

    erts_free(ERTS_ALC_T_TMP, (void *) cmd_line);

    if (new_environ != environ)
	erts_free(ERTS_ALC_T_ENVIRONMENT, (void *) new_environ);

    erts_smp_rwmtx_runlock(&environ_rwmtx);

    dd = create_driver_data(port_num, ifd[0], ofd[1], opts->packet_bytes,
                             DO_WRITE | DO_READ, opts->exit_status,
                             0, 0);

    {
        /* send ofd[0] + ifd[1] + stderrfd to forker port */
        ErtsSysForkerProto *proto =
            erts_alloc(ERTS_ALC_T_DRV_CTRL_DATA,
                       sizeof(ErtsSysForkerProto));
        memset(proto, 0, sizeof(ErtsSysForkerProto));
        proto->action = ErtsSysForkerProtoAction_Start;
        proto->u.start.fds[0] = ofd[0];
        proto->u.start.fds[1] = ifd[1];
        proto->u.start.fds[2] = stderrfd;
        proto->u.start.port_id = opts->exit_status ? erts_drvport2id(port_num) : THE_NON_VALUE;
        if (erl_drv_port_control(forker_port, 'S', (char*)proto, sizeof(*proto))) {
            /* The forker port has been killed, we close both fd's which will
               make open_port throw an epipe error */
            close(ofd[0]);
            close(ifd[1]);
        }
    }

    /* we set these fds to negative to mark if
       they should be closed after the handshake */
    if (!(opts->read_write & DO_READ))
        dd->ifd->fd *= -1;

    if (!(opts->read_write & DO_WRITE))
        dd->ofd->fd *= -1;

    return (ErlDrvData)dd;
#undef CMD_LINE_PREFIX_STR
#undef CMD_LINE_PREFIX_STR_SZ
}

static ErlDrvSSizeT spawn_control(ErlDrvData e, unsigned int cmd, char *buf,
                                  ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    ErtsSysDriverData *dd = (ErtsSysDriverData*)e;
    ErtsSysForkerProto *proto = (ErtsSysForkerProto *)buf;

    ASSERT(len == sizeof(*proto));
    ASSERT(proto->action == ErtsSysForkerProtoAction_SigChld);

    dd->status = proto->u.sigchld.error_number;
    dd->alive = -1;

    if (dd->ifd)
        driver_select(dd->port_num, abs(dd->ifd->fd), ERL_DRV_READ | ERL_DRV_USE, 1);

    if (dd->ofd)
        driver_select(dd->port_num, abs(dd->ofd->fd), ERL_DRV_WRITE | ERL_DRV_USE, 1);

    return 0;
}

#define FD_DEF_HEIGHT 24
#define FD_DEF_WIDTH 80
/* Control op */
#define FD_CTRL_OP_GET_WINSIZE 100

static int fd_get_window_size(int fd, Uint32 *width, Uint32 *height)
{
#ifdef TIOCGWINSZ 
    struct winsize ws;
    if (ioctl(fd,TIOCGWINSZ,&ws) == 0) {
	*width = (Uint32) ws.ws_col;
	*height = (Uint32) ws.ws_row;
	return 0;
    }
#endif
    return -1;
}

static ErlDrvSSizeT fd_control(ErlDrvData drv_data,
			       unsigned int command,
			       char *buf, ErlDrvSizeT len,
			       char **rbuf, ErlDrvSizeT rlen)
{
    int fd = (int)(long)drv_data;
    char resbuff[2*sizeof(Uint32)];
    switch (command) {
    case FD_CTRL_OP_GET_WINSIZE:
	{
	    Uint32 w,h;
	    if (fd_get_window_size(fd,&w,&h)) 
		return 0;
	    memcpy(resbuff,&w,sizeof(Uint32));
	    memcpy(resbuff+sizeof(Uint32),&h,sizeof(Uint32));
	}
	break;
    default:
	return 0;
    }
    if (rlen < 2*sizeof(Uint32)) {
	*rbuf = driver_alloc(2*sizeof(Uint32));
    }
    memcpy(*rbuf,resbuff,2*sizeof(Uint32));
    return 2*sizeof(Uint32);
}

static ErlDrvData fd_start(ErlDrvPort port_num, char* name,
			   SysDriverOpts* opts)
{
    int non_blocking = 0;

    if (((opts->read_write & DO_READ) && opts->ifd >= sys_max_files()) ||
	((opts->read_write & DO_WRITE) && opts->ofd >= sys_max_files()))
	return ERL_DRV_ERROR_GENERAL;

    /*
     * Historical:
     *
     * "Note about nonblocking I/O.
     *
     * At least on Solaris, setting the write end of a TTY to nonblocking,
     * will set the input end to nonblocking as well (and vice-versa).
     * If erl is run in a pipeline like this:  cat | erl
     * the input end of the TTY will be the standard input of cat.
     * And cat is not prepared to handle nonblocking I/O."
     *
     * Actually, the reason for this is not that the tty itself gets set
     * in non-blocking mode, but that the "input end" (cat's stdin) and
     * the "output end" (erlang's stdout) are typically the "same" file
     * descriptor, dup()'ed from a single fd by one of this process'
     * ancestors.
     *
     * The workaround for this problem used to be a rather bad kludge,
     * interposing an extra process ("internal cat") between erlang's
     * stdout and the original stdout, allowing erlang to set its stdout
     * in non-blocking mode without affecting the stdin of the preceding
     * process in the pipeline - and being a kludge, it caused all kinds
     * of weird problems.
     *
     * So, this is the current logic:
     *
     * The only reason to set non-blocking mode on the output fd at all is
     * if it's something that can cause a write() to block, of course,
     * i.e. primarily if it points to a tty, socket, pipe, or fifo. 
     *
     * If we don't set non-blocking mode when we "should" have, and output
     * becomes blocked, the entire runtime system will be suspended - this
     * is normally bad of course, and can happen fairly "easily" - e.g. user
     * hits ^S on tty - but doesn't necessarily happen.
     * 
     * If we do set non-blocking mode when we "shouldn't" have, the runtime
     * system will end up seeing EOF on the input fd (due to the preceding
     * process dying), which typically will cause the entire runtime system
     * to terminate immediately (due to whatever erlang process is seeing
     * the EOF taking it as a signal to halt the system). This is *very* bad.
     * 
     * I.e. we should take a conservative approach, and only set non-
     * blocking mode when we a) need to, and b) are reasonably certain
     * that it won't be a problem. And as in the example above, the problem
     * occurs when input fd and output fd point to different "things".
     *
     * However, determining that they are not just the same "type" of
     * "thing", but actually the same instance of that type of thing, is
     * unreasonably complex in many/most cases.
     *
     * Also, with pipes, sockets, and fifos it's far from obvious that the
     * user *wants* non-blocking output: If you're running erlang inside
     * some complex pipeline, you're probably not running a real-time system
     * that must never stop, but rather *want* it to suspend if the output
     * channel is "full".
     *
     * So, the bottom line: We will only set the output fd non-blocking if
     * it points to a tty, and either a) the input fd also points to a tty,
     * or b) we can make sure that setting the output fd non-blocking
     * doesn't interfere with someone else's input, via a somewhat milder
     * kludge than the above.
     *
     * Also keep in mind that while this code is almost exclusively run as
     * a result of an erlang open_port({fd,0,1}, ...), that isn't the only
     * case - it can be called with any old pre-existing file descriptors,
     * the relations between which (if they're even two) we can only guess
     * at - still, we try our best...
     *
     * Added note OTP 18: Some systems seem to use stdout/stderr to log data
     * using unix pipes, so we cannot allow the system to block on a write.
     * Therefore we use an async thread to write the data to fd's that could
     * not be set to non-blocking. When no async threads are available we
     * fall back on the old behaviour.
     *
     * Also the guarantee about what is delivered to the OS has changed.
     * Pre 18 the fd driver did no flushing of data before terminating.
     * Now it does. This is because we want to be able to guarantee that things
     * such as escripts and friends really have outputted all data before
     * terminating. This could potentially block the termination of the system
     * for a very long time, but if the user wants to terminate fast she should
     * use erlang:halt with flush=false.
     */

    /* Try to figure out if we can use non-blocking writes */
    if (opts->read_write & DO_WRITE) {

	/* If we don't have a read end, all bets are off - no non-blocking. */
	if (opts->read_write & DO_READ) {

	    if (isatty(opts->ofd)) { /* output fd is a tty:-) */

		if (isatty(opts->ifd)) { /* input fd is also a tty */

		    /* To really do this "right", we should also check that
		       input and output fd point to the *same* tty - but
		       this seems like overkill; ttyname() isn't for free,
		       and this is a very common case - and it's hard to
		       imagine a scenario where setting non-blocking mode
		       here would cause problems - go ahead and do it. */

                    non_blocking = 1;
		    SET_NONBLOCKING(opts->ofd);

		} else {	/* output fd is a tty, input fd isn't */

		    /* This is a "problem case", but also common (see the
		       example above) - i.e. it makes sense to try a bit
		       harder before giving up on non-blocking mode: Try to
		       re-open the tty that the output fd points to, and if
		       successful replace the original one with the "new" fd
		       obtained this way, and set *that* one in non-blocking
		       mode. (Yes, this is a kludge.)

		       However, re-opening the tty may fail in a couple of
		       (unusual) cases:

		       1) The name of the tty (or an equivalent one, i.e.
			  same major/minor number) can't be found, because
			  it actually lives somewhere other than /dev (or
			  wherever ttyname() looks for it), and isn't
			  equivalent to any of those that do live in the
			  "standard" place - this should be *very* unusual.

		       2) Permissions on the tty don't allow us to open it -
			  it's perfectly possible to have an fd open to an
			  object whose permissions wouldn't allow us to open
			  it. This is not as unusual as it sounds, one case
			  is if the user has su'ed to someone else (not
			  root) - we have a read/write fd open to the tty
			  (because it has been inherited all the way down
			  here), but we have neither read nor write
			  permission for the tty.

		       In these cases, we finally give up, and don't set the
		       output fd in non-blocking mode. */

		    char *tty;
		    int nfd;

		    if ((tty = ttyname(opts->ofd)) != NULL &&
			(nfd = open(tty, O_WRONLY)) != -1) {
			dup2(nfd, opts->ofd);
			close(nfd);
                        non_blocking = 1;
			SET_NONBLOCKING(opts->ofd);
		    }
		}
	    }
	}
    }
    return (ErlDrvData)create_driver_data(port_num, opts->ifd, opts->ofd,
                                          opts->packet_bytes,
                                          opts->read_write, 0, -1,
                                          !non_blocking);
}

static void clear_fd_data(ErtsSysFdData *fdd)
{
    if (fdd->sz > 0) {
	erts_free(ERTS_ALC_T_FD_ENTRY_BUF, (void *) fdd->buf);
	ASSERT(erts_smp_atomic_read_nob(&sys_misc_mem_sz) >= fdd->sz);
	erts_smp_atomic_add_nob(&sys_misc_mem_sz, -1*fdd->sz);
    }
    fdd->buf = NULL;
    fdd->sz = 0;
    fdd->remain = 0;
    fdd->cpos = NULL;
    fdd->psz = 0;
}

static void nbio_stop_fd(ErlDrvPort prt, ErtsSysFdData *fdd)
{
    driver_select(prt, abs(fdd->fd), DO_READ|DO_WRITE, 0);
    clear_fd_data(fdd);
    SET_BLOCKING(abs(fdd->fd));

}

static void fd_stop(ErlDrvData ev)  /* Does not close the fds */
{
    ErtsSysDriverData* dd = (ErtsSysDriverData*)ev;
    ErlDrvPort prt = dd->port_num;
    int sz = sizeof(ErtsSysDriverData);

#if FDBLOCK
    if (dd->blocking) {
        erts_free(ERTS_ALC_T_SYS_BLOCKING, dd->blocking);
        dd->blocking = NULL;
        sz += sizeof(ErtsSysBlocking);
    }
#endif

    if (dd->ifd) {
        sz += sizeof(ErtsSysFdData);
        nbio_stop_fd(prt, dd->ifd);
    }
    if (dd->ofd && dd->ofd != dd->ifd) {
        sz += sizeof(ErtsSysFdData);
        nbio_stop_fd(prt, dd->ofd);
    }

     erts_free(ERTS_ALC_T_DRV_TAB, dd);
     erts_smp_atomic_add_nob(&sys_misc_mem_sz, -sz);
}

static void fd_flush(ErlDrvData ev)
{
    ErtsSysDriverData* dd = (ErtsSysDriverData*)ev;
    if (!dd->terminating)
        dd->terminating = 1;
}

static ErlDrvData vanilla_start(ErlDrvPort port_num, char* name,
				SysDriverOpts* opts)
{
    int flags, fd;
    ErlDrvData res;

    flags = (opts->read_write == DO_READ ? O_RDONLY :
	     opts->read_write == DO_WRITE ? O_WRONLY|O_CREAT|O_TRUNC :
	     O_RDWR|O_CREAT);
    if ((fd = open(name, flags, 0666)) < 0)
	return ERL_DRV_ERROR_GENERAL;
    if (fd >= sys_max_files()) {
	close(fd);
	return ERL_DRV_ERROR_GENERAL;
    }
    SET_NONBLOCKING(fd);

    res = (ErlDrvData)(long)create_driver_data(port_num, fd, fd,
                                               opts->packet_bytes,
                                               opts->read_write, 0, -1, 0);
    return res;
}

/* Note that driver_data[fd].ifd == fd if the port was opened for reading, */
/* otherwise (i.e. write only) driver_data[fd].ofd = fd.  */

static void stop(ErlDrvData ev)
{
    ErtsSysDriverData* dd = (ErtsSysDriverData*)ev;
    ErlDrvPort prt = dd->port_num;

    if (dd->ifd) {
        nbio_stop_fd(prt, dd->ifd);
        driver_select(prt, abs(dd->ifd->fd), ERL_DRV_USE, 0);  /* close(ifd); */
    }

    if (dd->ofd && dd->ofd != dd->ifd) {
	nbio_stop_fd(prt, dd->ofd);
	driver_select(prt, abs(dd->ofd->fd), ERL_DRV_USE, 0);  /* close(ofd); */
    }

    erts_free(ERTS_ALC_T_DRV_TAB, dd);
}

/* used by fd_driver */
static void outputv(ErlDrvData e, ErlIOVec* ev)
{
    ErtsSysDriverData *dd = (ErtsSysDriverData*)e;
    ErlDrvPort ix = dd->port_num;
    int pb = dd->packet_bytes;
    int ofd = dd->ofd ? dd->ofd->fd : -1;
    ssize_t n;
    ErlDrvSizeT sz;
    char lb[4];
    char* lbp;
    ErlDrvSizeT len = ev->size;

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    /*    if (pb >= 0 && (len & (((ErlDrvSizeT)1 << (pb*8))) - 1) != len) {*/
    if (((pb == 2) && (len > 0xffff)) || (pb == 1 && len > 0xff)) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    /* Handles 0 <= pb <= 4 only */
    put_int32((Uint32) len, lb);
    lbp = lb + (4-pb);

    ev->iov[0].iov_base = lbp;
    ev->iov[0].iov_len = pb;
    ev->size += pb;

    if (dd->blocking && FDBLOCK)
        driver_pdl_lock(dd->blocking->pdl);

    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enqv(ix, ev, 0);

        if (dd->blocking && FDBLOCK)
            driver_pdl_unlock(dd->blocking->pdl);

	if (sz + ev->size >= (1 << 13))
	    set_busy_port(ix, 1);
    }
    else if (!dd->blocking || !FDBLOCK) {
        /* We try to write directly if the fd in non-blocking */
	int vsize = ev->vsize > MAX_VSIZE ? MAX_VSIZE : ev->vsize;

	n = writev(ofd, (const void *) (ev->iov), vsize);
	if (n == ev->size)
	    return; /* 0;*/
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return; /* -1;*/
	    }
	    n = 0;
	}
	driver_enqv(ix, ev, n);  /* n is the skip value */
	driver_select(ix, ofd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
    }
#if FDBLOCK
    else {
        if (ev->size != 0) {
            driver_enqv(ix, ev, 0);
            driver_pdl_unlock(dd->blocking->pdl);
            driver_async(ix, &dd->blocking->pkey,
                         fd_async, dd, NULL);
        } else {
            driver_pdl_unlock(dd->blocking->pdl);
        }
    }
#endif
    /* return 0;*/
}

/* Used by spawn_driver and vanilla driver */
static void output(ErlDrvData e, char* buf, ErlDrvSizeT len)
{
    ErtsSysDriverData *dd = (ErtsSysDriverData*)e;
    ErlDrvPort ix = dd->port_num;
    int pb = dd->packet_bytes;
    int ofd = dd->ofd ? dd->ofd->fd : -1;
    ssize_t n;
    ErlDrvSizeT sz;
    char lb[4];
    char* lbp;
    struct iovec iv[2];

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    if (((pb == 2) && (len > 0xffff))
        || (pb == 1 && len > 0xff)
        || dd->pid == 0 /* Attempt at output before port is ready */) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    put_int32(len, lb);
    lbp = lb + (4-pb);

    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enq(ix, lbp, pb);
	driver_enq(ix, buf, len);
	if (sz + len + pb >= (1 << 13))
	    set_busy_port(ix, 1);
    }
    else {
	iv[0].iov_base = lbp;
	iv[0].iov_len = pb;  /* should work for pb=0 */
	iv[1].iov_base = buf;
	iv[1].iov_len = len;
	n = writev(ofd, iv, 2);
	if (n == pb+len)
	    return; /* 0; */
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return; /* -1; */
	    }
	    n = 0;
	}
	if (n < pb) {
	    driver_enq(ix, lbp+n, pb-n);
	    driver_enq(ix, buf, len);
	}
	else {
	    n -= pb;
	    driver_enq(ix, buf+n, len-n);
	}
	driver_select(ix, ofd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
    }
    return; /* 0; */
}

static int port_inp_failure(ErtsSysDriverData *dd, int res)
				/* Result: 0 (eof) or -1 (error) */
{
    int err = errno;

    ASSERT(res <= 0);
    if (dd->ifd) {
        driver_select(dd->port_num, dd->ifd->fd, ERL_DRV_READ|ERL_DRV_WRITE, 0);
        clear_fd_data(dd->ifd);
    }

    if (dd->blocking && FDBLOCK) {
        driver_pdl_lock(dd->blocking->pdl);
        if (driver_sizeq(dd->port_num) > 0) {
            driver_pdl_unlock(dd->blocking->pdl);
            /* We have stuff in the output queue, so we just
               set the state to terminating and wait for fd_async_ready
               to terminate the port */
            if (res == 0)
                dd->terminating = 2;
            else
                dd->terminating = -err;
            return 0;
        }
        driver_pdl_unlock(dd->blocking->pdl);
    }

    if (res == 0) {
        if (dd->alive == 1) {
            /*
             * We have eof and want to report exit status, but the process
             * hasn't exited yet. When it does ready_input will
             * driver_select() this fd which will make sure that we get
             * back here with dd->alive == -1 and dd->status set.
             */
            return 0;
        }
        else if (dd->alive == -1) {
            int status = dd->status;

            /* We need not be prepared for stopped/continued processes. */
            if (WIFSIGNALED(status))
                status = 128 + WTERMSIG(status);
            else
                status = WEXITSTATUS(status);
            driver_report_exit(dd->port_num, status);
        }
       driver_failure_eof(dd->port_num);
    } else if (dd->ifd) {
        erl_drv_init_ack(dd->port_num, ERL_DRV_ERROR_ERRNO);
    } else {
	driver_failure_posix(dd->port_num, err);
    }
    return 0;
}

/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_input(ErlDrvData e, ErlDrvEvent ready_fd)
{
    ErtsSysDriverData *dd = (ErtsSysDriverData*)e;
    ErlDrvPort port_num;
    int packet_bytes;
    int res;
    Uint h;

    port_num = dd->port_num;
    packet_bytes = dd->packet_bytes;

    ASSERT(abs(dd->ifd->fd) == ready_fd);

    if (dd->pid == 0) {
        /* the pid is sent from erl_child_setup. spawn driver only. */
        ErtsSysForkerProto proto;
        int res;

        if((res = read(ready_fd, &proto, sizeof(proto))) <= 0) {
            /* hmm, child setup seems to have closed the pipe too early...
               we close the port as there is not much else we can do */
            if (res < 0 && errno == ERRNO_BLOCK)
                return;
            driver_select(port_num, ready_fd, ERL_DRV_READ, 0);
            if (res == 0)
                errno = EPIPE;
            port_inp_failure(dd, -1);
            return;
        }

        ASSERT(proto.action == ErtsSysForkerProtoAction_Go);
        dd->pid = proto.u.go.os_pid;

        if (dd->pid == -1) {
            /* Setup failed! The only reason why this should happen is if
               the fork fails. */
            errno = proto.u.go.error_number;
            port_inp_failure(dd, -1);
            return;
        }

        proto.action = ErtsSysForkerProtoAction_Ack;

        if (driver_sizeq(port_num) > 0) {
            driver_enq(port_num, (char*)&proto, sizeof(proto));
            } else {
                if (write(abs(dd->ofd->fd), &proto, sizeof(proto)) < 0)
                    if (errno == ERRNO_BLOCK || errno == EINTR)
                        driver_enq(port_num, (char*)&proto, sizeof(proto));
                /* do nothing on failure here. If the ofd is broken, then
                   the ifd will probably also be broken and trigger
                   a port_inp_failure */
            }

            if (dd->ifd->fd < 0) {
                driver_select(port_num, abs(dd->ifd->fd), ERL_DRV_READ|ERL_DRV_USE, 0);
                erts_smp_atomic_add_nob(&sys_misc_mem_sz, -sizeof(ErtsSysFdData));
                dd->ifd = NULL;
            }

            if (dd->ofd->fd < 0  || driver_sizeq(port_num) > 0)
                /* we select in order to close fd or write to queue,
                   child setup will close this fd if fd < 0 */
                driver_select(port_num, abs(dd->ofd->fd), ERL_DRV_WRITE|ERL_DRV_USE, 1);

            erl_drv_set_os_pid(port_num, dd->pid);
            erl_drv_init_ack(port_num, e);
            return;
    }

    if (packet_bytes == 0) {
	byte *read_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_READ_BUF,
					     ERTS_SYS_READ_BUF_SZ);
	res = read(ready_fd, read_buf, ERTS_SYS_READ_BUF_SZ);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(dd, res);
	}
	else if (res == 0)
	    port_inp_failure(dd, res);
	else
	    driver_output(port_num, (char*) read_buf, res);
	erts_free(ERTS_ALC_T_SYS_READ_BUF, (void *) read_buf);
    }
    else if (dd->ifd->remain > 0) { /* We try to read the remainder */
	/* space is allocated in buf */
	res = read(ready_fd, dd->ifd->cpos,
		   dd->ifd->remain);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(dd, res);
	}
	else if (res == 0) {
	    port_inp_failure(dd, res);
	}
	else if (res == dd->ifd->remain) { /* we're done  */
	    driver_output(port_num, dd->ifd->buf,
			  dd->ifd->sz);
	    clear_fd_data(dd->ifd);
	}
	else { /*  if (res < dd->ifd->remain) */
	    dd->ifd->cpos += res;
	    dd->ifd->remain -= res;
	}
    }
    else if (dd->ifd->remain == 0) { /* clean fd */
	byte *read_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_READ_BUF,
					     ERTS_SYS_READ_BUF_SZ);
	/* We make one read attempt and see what happens */
	res = read(ready_fd, read_buf, ERTS_SYS_READ_BUF_SZ);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(dd, res);
	}
	else if (res == 0) {     	/* eof */
	    port_inp_failure(dd, res);
	}
	else if (res < packet_bytes - dd->ifd->psz) {
	    memcpy(dd->ifd->pbuf+dd->ifd->psz,
		   read_buf, res);
	    dd->ifd->psz += res;
	}
	else  { /* if (res >= packet_bytes) */
	    unsigned char* cpos = read_buf;
	    int bytes_left = res;

	    while (1) {
		int psz = dd->ifd->psz;
		char* pbp = dd->ifd->pbuf + psz;

		while(bytes_left && (psz < packet_bytes)) {
		    *pbp++ = *cpos++;
		    bytes_left--;
		    psz++;
		}

		if (psz < packet_bytes) {
		    dd->ifd->psz = psz;
		    break;
		}
		dd->ifd->psz = 0;

		switch (packet_bytes) {
		case 1: h = get_int8(dd->ifd->pbuf);  break;
		case 2: h = get_int16(dd->ifd->pbuf); break;
		case 4: h = get_int32(dd->ifd->pbuf); break;
		default: ASSERT(0); return; /* -1; */
		}

		if (h <= (bytes_left)) {
		    driver_output(port_num, (char*) cpos, h);
		    cpos += h;
		    bytes_left -= h;
		    continue;
		}
		else {		/* The last message we got was split */
		        char *buf = erts_alloc_fnf(ERTS_ALC_T_FD_ENTRY_BUF, h);
		    if (!buf) {
			errno = ENOMEM;
			port_inp_failure(dd, -1);
		    }
		    else {
			erts_smp_atomic_add_nob(&sys_misc_mem_sz, h);
			sys_memcpy(buf, cpos, bytes_left);
			dd->ifd->buf = buf;
			dd->ifd->sz = h;
			dd->ifd->remain = h - bytes_left;
			dd->ifd->cpos = buf + bytes_left;
		    }
		    break;
		}
	    }
	}
	erts_free(ERTS_ALC_T_SYS_READ_BUF, (void *) read_buf);
    }
}


/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_output(ErlDrvData e, ErlDrvEvent ready_fd)
{
    ErtsSysDriverData *dd = (ErtsSysDriverData*)e;
    ErlDrvPort ix = dd->port_num;
    int n;
    struct iovec* iv;
    int vsize;

    if ((iv = (struct iovec*) driver_peekq(ix, &vsize)) == NULL) {
	driver_select(ix, ready_fd, ERL_DRV_WRITE, 0);
        if (dd->pid > 0 && dd->ofd->fd < 0) {
            /* The port was opened with 'in' option, which means we
               should close the output fd as soon as the command has
               been sent. */
            driver_select(ix, ready_fd, ERL_DRV_WRITE|ERL_DRV_USE, 0);
            erts_smp_atomic_add_nob(&sys_misc_mem_sz, -sizeof(ErtsSysFdData));
            dd->ofd = NULL;
        }
        if (dd->terminating)
            driver_failure_atom(dd->port_num,"normal");
	return; /* 0; */
    }
    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
    if ((n = writev(ready_fd, iv, vsize)) > 0) {
	if (driver_deq(ix, n) == 0)
	    set_busy_port(ix, 0);
    }
    else if (n < 0) {
	if (errno == ERRNO_BLOCK || errno == EINTR)
	    return; /* 0; */
	else {
	    int res = errno;
	    driver_select(ix, ready_fd, ERL_DRV_WRITE, 0);
	    driver_failure_posix(ix, res);
	    return; /* -1; */
	}
    }
    return; /* 0; */
}

static void stop_select(ErlDrvEvent fd, void* _)
{
    close((int)fd);
}

#if FDBLOCK

static void
fd_async(void *async_data)
{
    int res;
    ErtsSysDriverData *dd = (ErtsSysDriverData *)async_data;
    SysIOVec      *iov0;
    SysIOVec      *iov;
    int            iovlen;
    int            err = 0;
    /* much of this code is stolen from efile_drv:invoke_writev */
    driver_pdl_lock(dd->blocking->pdl);
    iov0 = driver_peekq(dd->port_num, &iovlen);
    iovlen = iovlen < MAXIOV ? iovlen : MAXIOV;
    iov = erts_alloc_fnf(ERTS_ALC_T_SYS_WRITE_BUF,
                         sizeof(SysIOVec)*iovlen);
    if (!iov) {
        res = -1;
        err = ENOMEM;
        driver_pdl_unlock(dd->blocking->pdl);
    } else {
        memcpy(iov,iov0,iovlen*sizeof(SysIOVec));
        driver_pdl_unlock(dd->blocking->pdl);

        do {
            res = writev(dd->ofd->fd, iov, iovlen);
        } while (res < 0 && errno == EINTR);
        if (res < 0)
            err = errno;
        err = errno;

        erts_free(ERTS_ALC_T_SYS_WRITE_BUF, iov);
    }
    dd->blocking->res = res;
    dd->blocking->err = err;
}

void fd_ready_async(ErlDrvData drv_data,
                    ErlDrvThreadData thread_data) {
    ErtsSysDriverData *dd = (ErtsSysDriverData *)thread_data;
    ErlDrvPort port_num = dd->port_num;

    ASSERT(dd->blocking);

    if (dd->blocking->res > 0) {
        driver_pdl_lock(dd->blocking->pdl);
        if (driver_deq(port_num, dd->blocking->res) == 0) {
            driver_pdl_unlock(dd->blocking->pdl);
            set_busy_port(port_num, 0);
            if (dd->terminating) {
                /* The port is has been ordered to terminate
                   from either fd_flush or port_inp_failure */
                if (dd->terminating == 1)
                    driver_failure_atom(port_num, "normal");
                else if (dd->terminating == 2)
                    driver_failure_eof(port_num);
                else if (dd->terminating < 0)
                    driver_failure_posix(port_num, -dd->terminating);
                return; /* -1; */
            }
        } else {
            driver_pdl_unlock(dd->blocking->pdl);
            /* still data left to write in queue */
            driver_async(port_num, &dd->blocking->pkey, fd_async, dd, NULL);
            return /* 0; */;
        }
    } else if (dd->blocking->res < 0) {
        if (dd->blocking->err == ERRNO_BLOCK) {
            set_busy_port(port_num, 1);
            /* still data left to write in queue */
            driver_async(port_num, &dd->blocking->pkey, fd_async, dd, NULL);
        } else
            driver_failure_posix(port_num, dd->blocking->err);
        return; /* -1; */
    }
    return; /* 0; */
}

#endif

/* Forker driver */

static int forker_fd;

static ErlDrvData forker_start(ErlDrvPort port_num, char* name,
                               SysDriverOpts* opts)
{

    int i;
    int fds[2];
    int res, unbind;
    char bindir[MAXPATHLEN];
    size_t bindirsz = sizeof(bindir);
    Uint csp_path_sz;
    char *child_setup_prog;

    forker_port = erts_drvport2id(port_num);

    res = erts_sys_getenv_raw("BINDIR", bindir, &bindirsz);
    if (res != 0) {
        if (res < 0)
            erts_exit(1,
                     "Environment variable BINDIR is not set\n");
        if (res > 0)
            erts_exit(1,
                     "Value of environment variable BINDIR is too large\n");
    }
    if (bindir[0] != DIR_SEPARATOR_CHAR)
        erts_exit(1,
                 "Environment variable BINDIR does not contain an"
                 " absolute path\n");
    csp_path_sz = (strlen(bindir)
                   + 1 /* DIR_SEPARATOR_CHAR */
                   + sizeof(CHILD_SETUP_PROG_NAME)
                   + 1);
    child_setup_prog = erts_alloc(ERTS_ALC_T_CS_PROG_PATH, csp_path_sz);
    erts_snprintf(child_setup_prog, csp_path_sz,
                  "%s%c%s",
                  bindir,
                  DIR_SEPARATOR_CHAR,
                  CHILD_SETUP_PROG_NAME);
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) < 0) {
        erts_exit(ERTS_ABORT_EXIT,
                 "Could not open unix domain socket in spawn_init: %d\n",
                 errno);
    }

    forker_fd = fds[0];

    unbind = erts_sched_bind_atfork_prepare();

    i = fork();

    if (i == 0) {
        /* The child */
        char *cs_argv[FORKER_ARGV_NO_OF_ARGS] =
            {CHILD_SETUP_PROG_NAME, NULL, NULL};
        char buff[128];

        erts_sched_bind_atfork_child(unbind);

        snprintf(buff, 128, "%d", sys_max_files());
        cs_argv[FORKER_ARGV_MAX_FILES] = buff;

        /* We preallocate fd 3 for the uds fd */
        if (fds[1] != 3) {
            dup2(fds[1], 3);
        }

#if defined(USE_SETPGRP_NOARGS)		/* SysV */
    (void) setpgrp();
#elif defined(USE_SETPGRP)		/* BSD */
    (void) setpgrp(0, getpid());
#else					/* POSIX */
    (void) setsid();
#endif

        execv(child_setup_prog, cs_argv);
        _exit(1);
    }

    erts_sched_bind_atfork_parent(unbind);

    erts_free(ERTS_ALC_T_CS_PROG_PATH, child_setup_prog);

    close(fds[1]);

    SET_NONBLOCKING(forker_fd);

    driver_select(port_num, forker_fd, ERL_DRV_READ|ERL_DRV_USE, 1);

    return (ErlDrvData)port_num;
}

static void forker_stop(ErlDrvData e)
{
    /* we probably should do something here,
       the port has been closed by the user. */
}

static void forker_ready_input(ErlDrvData e, ErlDrvEvent fd)
{
    int res;
    ErtsSysForkerProto *proto;

    proto = erts_alloc(ERTS_ALC_T_DRV_CTRL_DATA, sizeof(*proto));

    if ((res = read(fd, proto, sizeof(*proto))) < 0) {
        if (errno == ERRNO_BLOCK)
            return;
        erts_exit(ERTS_DUMP_EXIT, "Failed to read from erl_child_setup: %d\n", errno);
    }

    if (res == 0)
        erts_exit(ERTS_DUMP_EXIT, "erl_child_setup closed\n");

    ASSERT(res == sizeof(*proto));

#ifdef FORKER_PROTO_START_ACK
    if (proto->action == ErtsSysForkerProtoAction_StartAck) {
        /* Ideally we would like to not have to ack each Start
           command being sent over the uds, but it would seem
           that some operating systems (only observed on FreeBSD)
           throw away data on the uds when the socket becomes full,
           so we have to.
        */
        ErlDrvPort port_num = (ErlDrvPort)e;
        int vlen;
        SysIOVec *iov = driver_peekq(port_num, &vlen);
        ErtsSysForkerProto *proto = (ErtsSysForkerProto *)iov[0].iov_base;

        close(proto->u.start.fds[0]);
        close(proto->u.start.fds[1]);
        if (proto->u.start.fds[1] != proto->u.start.fds[2])
            close(proto->u.start.fds[2]);

        driver_deq(port_num, sizeof(*proto));

        if (driver_sizeq(port_num) > 0)
            driver_select(port_num, forker_fd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
    } else
#endif
    {
        ASSERT(proto->action == ErtsSysForkerProtoAction_SigChld);

        /* ideally this would be a port_command call, but as command is
           already used by the spawn_driver, we use control instead.
           Note that when using erl_drv_port_control it is an asynchronous
           control. */
        erl_drv_port_control(proto->u.sigchld.port_id, 'S',
                             (char*)proto, sizeof(*proto));
    }

}

static void forker_ready_output(ErlDrvData e, ErlDrvEvent fd)
{
    ErlDrvPort port_num = (ErlDrvPort)e;

#ifndef FORKER_PROTO_START_ACK
    while (driver_sizeq(port_num) > 0) {
#endif
        int vlen;
        SysIOVec *iov = driver_peekq(port_num, &vlen);
        ErtsSysForkerProto *proto = (ErtsSysForkerProto *)iov[0].iov_base;
        ASSERT(iov[0].iov_len >= (sizeof(*proto)));
        if (sys_uds_write(forker_fd, (char*)proto, sizeof(*proto),
                          proto->u.start.fds, 3, 0) < 0) {
            if (errno == ERRNO_BLOCK)
                return;
            erts_exit(ERTS_DUMP_EXIT, "Failed to write to erl_child_setup: %d\n", errno);
        }
#ifndef FORKER_PROTO_START_ACK
        close(proto->u.start.fds[0]);
        close(proto->u.start.fds[1]);
        if (proto->u.start.fds[1] != proto->u.start.fds[2])
            close(proto->u.start.fds[2]);
        driver_deq(port_num, sizeof(*proto));
    }
#endif

    driver_select(port_num, forker_fd, ERL_DRV_WRITE, 0);
}

static ErlDrvSSizeT forker_control(ErlDrvData e, unsigned int cmd, char *buf,
                                   ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen)
{
    ErtsSysForkerProto *proto = (ErtsSysForkerProto *)buf;
    ErlDrvPort port_num = (ErlDrvPort)e;
    int res;

    driver_enq(port_num, buf, len);
    if (driver_sizeq(port_num) > sizeof(*proto)) {
        return 0;
    }

    if ((res = sys_uds_write(forker_fd, (char*)proto, sizeof(*proto),
                             proto->u.start.fds, 3, 0)) < 0) {
        if (errno == ERRNO_BLOCK) {
            driver_select(port_num, forker_fd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
            return 0;
        }
        erts_exit(ERTS_DUMP_EXIT, "Failed to write to erl_child_setup: %d\n", errno);
    }

#ifndef FORKER_PROTO_START_ACK
    ASSERT(res == sizeof(*proto));
    close(proto->u.start.fds[0]);
    close(proto->u.start.fds[1]);
    if (proto->u.start.fds[1] != proto->u.start.fds[2])
        close(proto->u.start.fds[2]);
    driver_deq(port_num, sizeof(*proto));
#endif

    return 0;
}
