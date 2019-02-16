/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2018. All Rights Reserved.
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
 * This program is started at erts startup and all fork's that
 * have to be done are done in here. This is done for a couple
 * of reasons:
 *  - Allow usage of fork without a memory explosion.
 *  -- we do not want to use vfork, as it blocks the VM
 *     until the execv is done, and if the program that
 *     is to be executed is on an NFS that is unavailable,
 *     the execv can block for a very long time.
 *  -- we cannot do fork inside the VM as that would temporarily
 *     duplicate the memory usage of the VM per parallel exec.
 *
 * Some implementation notes:
 *  - A single Unix Domain Socket is setup in between the VM and
 *    this program. Over that UDS the file descriptors that should
 *    be used to talk to the child program are sent.
 *    The actual command to execute, together with options and the
 *    environment, is sent over the pipe represented by the
 *    file descriptors mentioned above. We don't send the
 *    command over the UDS as that would increase the likely hood
 *    that it's buffer would be full.
 *
 *  - Since it is this program that execv's, it has to take care of
 *    all the SIGCHLD signals that the child programs generate. The
 *    signals are received and the pid+exit reason is sent as data
 *    on the UDS to the VM. The VM is then able to map the pid to the
 *    port of the child program that just exited and deliver the status
 *    code if requested.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>

#define WANT_NONBLOCKING

#include "erl_driver.h"
#include "sys_uds.h"
#include "hash.h"
#include "erl_term.h"
#include "erl_child_setup.h"

#define SET_CLOEXEC(fd) fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC)

#if defined(__ANDROID__)
#define SHELL "/system/bin/sh"
#else
#define SHELL "/bin/sh"
#endif /* __ANDROID__ */

//#define HARD_DEBUG
#ifdef HARD_DEBUG
#define DEBUG_PRINT(fmt, ...) fprintf(stderr, "%d:" fmt "\r\n", getpid(), ##__VA_ARGS__)
#else
#define DEBUG_PRINT(fmt, ...)
#endif

static char abort_reason[200]; /* for core dump inspection */

static void ABORT(const char* fmt, ...)
{
    va_list arglist;
    va_start(arglist, fmt);
    vsprintf(abort_reason, fmt, arglist);
    fprintf(stderr, "erl_child_setup: %s\r\n", abort_reason);
    va_end(arglist);
    abort();
}

#ifdef DEBUG
void
erl_assert_error(const char* expr, const char* func, const char* file, int line)
{
    fflush(stdout);
    fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n",
            file, line, func, expr);
    fflush(stderr);
    abort();
}
#endif

void sys_sigblock(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_BLOCK, &mask, (sigset_t *)NULL);
}

void sys_sigrelease(int sig)
{
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *)NULL);
}

static void add_os_pid_to_port_id_mapping(Eterm, pid_t);
static Eterm get_port_id(pid_t);
static int forker_hash_init(void);

static int max_files = -1;
static int sigchld_pipe[2];

static int
start_new_child(int pipes[])
{
    struct sigaction sa;
    int errln = -1;
    int size, res, i, pos = 0;
    char *buff, *o_buff;

    char *cmd, *cwd, *wd, **new_environ, **args = NULL;

    Sint32 cnt, flags;

    /* only child executes here */

    /* Restore default handling of sigterm... */
    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    if (sigaction(SIGTERM, &sa, 0) == -1) {
        perror(NULL);
        exit(1);
    }
    
    do {
        res = read(pipes[0], (char*)&size, sizeof(size));
    } while(res < 0 && (errno == EINTR || errno == ERRNO_BLOCK));

    if (res <= 0) {
        errln = __LINE__;
        goto child_error;
    }

    buff = malloc(size);

    DEBUG_PRINT("size = %d", size);

    do {
        if ((res = read(pipes[0], buff + pos, size - pos)) < 0) {
            if (errno == ERRNO_BLOCK || errno == EINTR)
                continue;
            errln = __LINE__;
            goto child_error;
        }
        if (res == 0) {
            errno = EPIPE;
            errln = __LINE__;
            goto child_error;
        }
        pos += res;
    } while(size - pos != 0);

    o_buff = buff;

    flags = get_int32(buff);
    buff += sizeof(flags);

    DEBUG_PRINT("flags = %d", flags);

    cmd = buff;
    buff += strlen(buff) + 1;

    cwd = buff;
    buff += strlen(buff) + 1;

    if (*buff == '\0') {
        wd = NULL;
    } else {
        wd = buff;
        buff += strlen(buff) + 1;
    }
    buff++;

    DEBUG_PRINT("wd = %s", wd);

    cnt = get_int32(buff);
    buff += sizeof(cnt);
    new_environ = malloc(sizeof(char*)*(cnt + 1));

    DEBUG_PRINT("env_len = %d", cnt);
    for (i = 0; i < cnt; i++, buff++) {
        new_environ[i] = buff;
        while(*buff != '\0') buff++;
    }
    new_environ[cnt] = NULL;

    if (o_buff + size != buff) {
        /* This is a spawn executable call */
        cnt = get_int32(buff);
        buff += sizeof(cnt);
        args = malloc(sizeof(char*)*(cnt + 1));
        for (i = 0; i < cnt; i++, buff++) {
            args[i] = buff;
            while(*buff != '\0') buff++;
        }
        args[cnt] = NULL;
    }

    if (o_buff + size != buff) {
        errno = EINVAL;
        errln = __LINE__;
        fprintf(stderr,"erl_child_setup: failed with protocol "
                "error %d on line %d", errno, errln);
        /* we abort here as it is most likely a symptom of an
           emulator/erl_child_setup bug */
        abort();
    }

    DEBUG_PRINT("read ack");
    do {
        ErtsSysForkerProto proto;
        res = read(pipes[0], &proto, sizeof(proto));
        if (res > 0) {
            ASSERT(proto.action == ErtsSysForkerProtoAction_Ack);
            ASSERT(res == sizeof(proto));
        }
    } while(res < 0 && (errno == EINTR || errno == ERRNO_BLOCK));

    if (res < 1) {
        errno = EPIPE;
        errln = __LINE__;
        goto child_error;
    }

    DEBUG_PRINT("Set cwd to: '%s'",cwd);

    if (chdir(cwd) < 0) {
        /* This is not good, it probably means that the cwd of
           beam is invalid. We ignore it and try anyways as
           the child might now need a cwd or the chdir below
           could take us to a valid directory.
        */
    }

    DEBUG_PRINT("Set wd to: '%s'",wd);

    if (wd && chdir(wd) < 0) {
        int err = errno;
        fprintf(stderr,"spawn: Could not cd to %s\r\n", wd);
        _exit(err);
    }

    DEBUG_PRINT("Do that forking business: '%s'",cmd);

    /* When the dup2'ing below is done, only
       fd's 0, 1, 2 and maybe 3, 4 should survive the
       exec. All other fds (i.e. the unix domain sockets
       and stray pipe ends) should have CLOEXEC set on them
       so they will be closed when the exec happens */
    if (flags & FORKER_FLAG_USE_STDIO) {
        /* stdin for process */
        if (flags & FORKER_FLAG_DO_WRITE &&
            dup2(pipes[0], 0) < 0) {
            errln = __LINE__;
            goto child_error;
        }
        /* stdout for process */
        if (flags & FORKER_FLAG_DO_READ &&
            dup2(pipes[1], 1) < 0) {
            errln = __LINE__;
            goto child_error;
        }
    }
    else {	/* XXX will fail if pipes[0] == 4 (unlikely..) */
        if (flags & FORKER_FLAG_DO_READ && dup2(pipes[1], 4) < 0) {
            errln = __LINE__;
            goto child_error;
        }
        if (flags & FORKER_FLAG_DO_WRITE && dup2(pipes[0], 3) < 0) {
            errln = __LINE__;
            goto child_error;
        }
    }

    /* we do the dup2 of stderr last so that errors
       in child_error will be printed to stderr */
    if (dup2(pipes[2], 2) < 0) {
        errln = __LINE__;
        goto child_error;
    }

#if defined(USE_SETPGRP_NOARGS)		/* SysV */
    (void) setpgrp();
#elif defined(USE_SETPGRP)		/* BSD */
    (void) setpgrp(0, getpid());
#else					/* POSIX */
    (void) setsid();
#endif

    close(pipes[0]);
    close(pipes[1]);
    close(pipes[2]);

    sys_sigrelease(SIGCHLD);

    if (args) {
        /* spawn_executable */
        execve(cmd, args, new_environ);
    } else {
        execle(SHELL, "sh", "-c", cmd, (char *) NULL, new_environ);
    }

    DEBUG_PRINT("exec error: %d",errno);
    _exit(errno);

child_error:
    fprintf(stderr,"erl_child_setup: failed with error %d on line %d\r\n",
            errno, errln);
    _exit(errno);
}


/*
 * [OTP-3906]
 * Solaris signal management gets confused when threads are used and a
 * lot of child processes dies. The confusion results in that SIGCHLD
 * signals aren't delivered to the emulator which in turn results in
 * a lot of defunct processes in the system.
 *
 * The problem seems to appear when a signal is frequently
 * blocked/unblocked at the same time as the signal is frequently
 * propagated. The child waiter thread is a workaround for this problem.
 * The SIGCHLD signal is always blocked (in all threads), and the child
 * waiter thread fetches the signal by a call to sigwait(). See
 * child_waiter().
 *
 * This should be a non-issue since the fork:ing was moved outside of
 * the emulator into erl_child_setup. I'm leaving the comment here
 * for posterity. */

static void handle_sigchld(int sig) {
    int buff[2], res, __preverrno = errno;

    sys_sigblock(SIGCHLD);

    while ((buff[0] = waitpid((pid_t)(-1), buff+1, WNOHANG)) > 0) {
        do {
            res = write(sigchld_pipe[1], buff, sizeof(buff));
        } while (res < 0 && errno == EINTR);
        if (res <= 0)
            ABORT("Failed to write to sigchld_pipe (%d): %d (%d)", sigchld_pipe[1], res, errno);
        DEBUG_PRINT("Reap child %d (%d)", buff[0], buff[1]);
    }

    sys_sigrelease(SIGCHLD);

    /* We save and restore the original errno as otherwise
       the thread we are running in may end up with an
       unexpected errno. An example of when this happened
       was when the select in main had gotten an EINTR but
       before the errno was checked the signal handler
       was called and set errno to ECHILD from waitpid
       which caused erl_child_setup to abort as it does
       not expect ECHILD to be set after select */
    errno = __preverrno;
}

#if defined(__ANDROID__)
static int system_properties_fd(void)
{
    static int fd = -2;
    char *env;

    if (fd != -2) return fd;
    env = getenv("ANDROID_PROPERTY_WORKSPACE");
    if (!env) {
        fd = -1;
        return -1;
    }
    fd = atoi(env);
    return fd;
}
#endif /* __ANDROID__ */

int
main(int argc, char *argv[])
{
    /* This fd should be open from beam */
    int uds_fd = 3, max_fd = 3;
#ifndef HAVE_CLOSEFROM
    int i;
#endif
    struct sigaction sa;

    if (argc < 1 || sscanf(argv[1],"%d",&max_files) != 1) {
        ABORT("Invalid arguments to child_setup");
    }

/* We close all fds except the uds from beam.
   All other fds from now on will have the
   CLOEXEC flags set on them. This means that we
   only have to close a very limited number of fds
   after we fork before the exec. */
#if defined(HAVE_CLOSEFROM)
    closefrom(4);
#else
    for (i = 4; i < max_files; i++)
#if defined(__ANDROID__)
        if (i != system_properties_fd())
#endif
        (void) close(i);
#endif

    if (pipe(sigchld_pipe) < 0) {
        ABORT("Failed to setup sigchld pipe (%d)", errno);
    }

    SET_CLOEXEC(sigchld_pipe[0]);
    SET_CLOEXEC(sigchld_pipe[1]);

    max_fd = max_fd < sigchld_pipe[0] ? sigchld_pipe[0] : max_fd;

    sa.sa_handler = &handle_sigchld;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
    if (sigaction(SIGCHLD, &sa, 0) == -1) {
        perror(NULL);
        exit(1);
    }

    /* Ignore SIGTERM.
       Some container environments send SIGTERM to all processes
       when terminating. We don't want erl_child_setup to terminate
       in these cases as that will prevent beam from properly
       cleaning up.
    */
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    if (sigaction(SIGTERM, &sa, 0) == -1) {
        perror(NULL);
        exit(1);
    }

    forker_hash_init();

    SET_CLOEXEC(uds_fd);

    DEBUG_PRINT("Starting forker %d", max_files);

    while (1) {
        fd_set read_fds;
        int res;
        FD_ZERO(&read_fds);
        FD_SET(uds_fd, &read_fds);
        FD_SET(sigchld_pipe[0], &read_fds);
        DEBUG_PRINT("child_setup selecting on %d, %d (%d)",
                uds_fd, sigchld_pipe[0], max_fd);
        res = select(max_fd+1, &read_fds, NULL, NULL, NULL);

        if (res < 0) {
            if (errno == EINTR) continue;
            ABORT("Select failed: %d (%d)",res, errno);
        }

        if (FD_ISSET(uds_fd, &read_fds)) {
            int pipes[3], res, os_pid;
            ErtsSysForkerProto proto;
            errno = 0;
            if ((res = sys_uds_read(uds_fd, (char*)&proto, sizeof(proto),
                                    pipes, 3, MSG_DONTWAIT)) < 0) {
                if (errno == EINTR)
                    continue;
                DEBUG_PRINT("erl_child_setup failed to read from uds: %d, %d", res, errno);
                _exit(0);
            }

            if (res == 0) {
                DEBUG_PRINT("uds was closed!");
                _exit(0);
            }
            /* Since we use unix domain sockets and send the entire data in
               one go we *should* get the entire payload at once. */
            ASSERT(res == sizeof(proto));
            ASSERT(proto.action == ErtsSysForkerProtoAction_Start);

            sys_sigblock(SIGCHLD);

            errno = 0;

            os_pid = fork();
            if (os_pid == 0)
                start_new_child(pipes);

            add_os_pid_to_port_id_mapping(proto.u.start.port_id, os_pid);

            /* We write an ack here, but expect the reply on
               the pipes[0] inside the fork */
            proto.action = ErtsSysForkerProtoAction_Go;
            proto.u.go.os_pid = os_pid;
            proto.u.go.error_number = errno;
            while (write(pipes[1], &proto, sizeof(proto)) < 0 && errno == EINTR)
                ; /* remove gcc warning */

#ifdef FORKER_PROTO_START_ACK
            proto.action = ErtsSysForkerProtoAction_StartAck;
            while (write(uds_fd, &proto, sizeof(proto)) < 0 && errno == EINTR)
                ; /* remove gcc warning */
#endif

            sys_sigrelease(SIGCHLD);
            close(pipes[0]);
            close(pipes[1]);
            close(pipes[2]);
        }

        if (FD_ISSET(sigchld_pipe[0], &read_fds)) {
            int ibuff[2];
            ErtsSysForkerProto proto;
            res = read(sigchld_pipe[0], ibuff, sizeof(ibuff));
            if (res <= 0) {
                if (errno == EINTR)
                    continue;
                ABORT("Failed to read from sigchld pipe: %d (%d)", res, errno);
            }

            proto.u.sigchld.port_id = get_port_id((pid_t)(ibuff[0]));

            if (proto.u.sigchld.port_id == THE_NON_VALUE)
                continue; /* exit status report not requested */

            proto.action = ErtsSysForkerProtoAction_SigChld;
            proto.u.sigchld.error_number = ibuff[1];
            DEBUG_PRINT("send sigchld to %d (errno = %d)", uds_fd, ibuff[1]);
            if (write(uds_fd, &proto, sizeof(proto)) < 0) {
                if (errno == EINTR)
                    continue;
                /* The uds was close, which most likely means that the VM
                   has exited. This will be detected when we try to read
                   from the uds_fd. */
                DEBUG_PRINT("Failed to write to uds: %d (%d)", uds_fd, errno);
            }
        }
    }
    return 1;
}

typedef struct exit_status {
    HashBucket hb;
    pid_t os_pid;
    Eterm port_id;
} ErtsSysExitStatus;

static Hash *forker_hash;

static void add_os_pid_to_port_id_mapping(Eterm port_id, pid_t os_pid)
{
    if (port_id != THE_NON_VALUE) {
        /* exit status report requested */
        ErtsSysExitStatus es;
        es.os_pid = os_pid;
        es.port_id = port_id;
        hash_put(forker_hash, &es);
    }
}

static Eterm get_port_id(pid_t os_pid)
{
    ErtsSysExitStatus est, *es;
    Eterm port_id;
    est.os_pid = os_pid;
    es = hash_remove(forker_hash, &est);
    if (!es) return THE_NON_VALUE;
    port_id = es->port_id;
    free(es);
    return port_id;
}

static int fcmp(void *a, void *b)
{
    ErtsSysExitStatus *sa = a;
    ErtsSysExitStatus *sb = b;
    return !(sa->os_pid == sb->os_pid);
}

static HashValue fhash(void *e)
{
    ErtsSysExitStatus *se = e;
    Uint32 val = se->os_pid;
    val = (val+0x7ed55d16) + (val<<12);
    val = (val^0xc761c23c) ^ (val>>19);
    val = (val+0x165667b1) + (val<<5);
    val = (val+0xd3a2646c) ^ (val<<9);
    val = (val+0xfd7046c5) + (val<<3);
    val = (val^0xb55a4f09) ^ (val>>16);
    return val;
}

static void *falloc(void *e)
{
    ErtsSysExitStatus *se = e;
    ErtsSysExitStatus *ne = malloc(sizeof(ErtsSysExitStatus));
    ne->os_pid = se->os_pid;
    ne->port_id = se->port_id;
    return ne;
}

static void *meta_alloc(int type, size_t size) { return malloc(size); }
static void meta_free(int type, void *p)       { free(p); }

static int forker_hash_init(void)
{
    HashFunctions forker_hash_functions;
    forker_hash_functions.hash = fhash;
    forker_hash_functions.cmp = fcmp;
    forker_hash_functions.alloc = falloc;
    forker_hash_functions.free = free;
    forker_hash_functions.meta_alloc = meta_alloc;
    forker_hash_functions.meta_free  = meta_free;
    forker_hash_functions.meta_print = NULL;

    forker_hash = hash_new(0, "forker_hash",
                           16, forker_hash_functions);

    return 1;
}
