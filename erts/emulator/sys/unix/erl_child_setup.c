/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2015. All Rights Reserved.
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
#include <sys/wait.h>

#include "erl_driver.h"
#include "sys_uds.h"

#define SET_CLOEXEC(fd) fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC)

#if defined(__ANDROID__)
#define SHELL "/system/bin/sh"
#else
#define SHELL "/bin/sh"
#endif /* __ANDROID__ */

//#define HARD_DEBUG
#ifdef HARD_DEBUG
#define DEBUG_PRINT(fmt, ...) fprintf(stderr, fmt "\r\n", ##__VA_ARGS__)
#else
#define DEBUG_PRINT(fmt, ...)
#endif

#define ABORT(fmt, ...) do {                                            \
        fprintf(stderr, "erl_child_setup: " fmt "\r\n", ##__VA_ARGS__); \
        abort();                                                        \
    } while(0)

#undef ASSERT
#ifdef DEBUG
#define ASSERT(cnd) do { if (!(cnd)) { ABORT("assertion %s failed", #cnd); } } while(0)
#else
#define ASSERT(cnd)
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

static int max_files = -1;
static int sigchld_pipe[2];

static int
start_new_child(int pipes[])
{
    int size, res, i, pos = 0;
    char *buff, *o_buff;

    char *cmd, *wd, **new_environ, **args = NULL, cbuff[1];

    Sint cnt, flags;

    /* only child executes here */

    if ((res = read(pipes[0], (char*)&size, sizeof(size))) < 0) {
        ABORT("Failed to read size from %d (%d)", pipes[0], errno);
    }

    buff = malloc(size);

    DEBUG_PRINT("size = %d", size);

    do {
        if ((res = read(pipes[0], buff + pos, size - pos)) < 0) {
            if (errno == EAGAIN || errno == EINTR)
                continue;
            ABORT("Failed to read %d bytes from %d (%d,%d)",
                  size, pipes[0], res, errno);
        }
        if (res == 0) {
            errno = EPIPE;
            goto child_error;
        }
        pos += res;
    } while(size - pos != 0);

    o_buff = buff;

    flags = get_int32(buff);
    buff += sizeof(Sint32);

    DEBUG_PRINT("flags = %d", flags);

    cmd = buff;
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
    buff += sizeof(Sint32);
    new_environ = malloc(sizeof(char*)*(cnt + 1));

    DEBUG_PRINT("env_len = %ld", cnt);
    for (i = 0; i < cnt; i++, buff++) {
        new_environ[i] = buff;
        while(*buff != '\0') buff++;
    }
    new_environ[cnt] = NULL;

    if (o_buff + size != buff) {
        /* This is a spawn executable call */
        cnt = get_int32(buff);
        buff += sizeof(Sint32);
        args = malloc(sizeof(char*)*(cnt + 1));
        for (i = 0; i < cnt; i++, buff++) {
            args[i] = buff;
            while(*buff != '\0') buff++;
        }
        args[cnt] = NULL;
    }

    if (o_buff + size != buff) {
        ABORT("Buff error: %p, %p:%p", o_buff, o_buff+size, buff);
    }

    DEBUG_PRINT("read ack");
    if (read(pipes[0], cbuff, 1) < 1)
        goto child_error;

    DEBUG_PRINT("Do that forking business: '%s'\n",cmd);

    /* When the dup2'ing below is done, only
       fd's 0, 1, 2 and maybe 3, 4 should survive the
       exec. All other fds (i.e. the unix domain sockets
       and stray pipe ends) should have CLOEXEC set on them
       so they will be closed when the exec happens */
    if (flags & FORKER_FLAG_USE_STDIO) {
        /* stdin for process */
        if (flags & FORKER_FLAG_DO_WRITE &&
            dup2(pipes[0], 0) < 0)
            goto child_error;
        /* stdout for process */
        if (flags & FORKER_FLAG_DO_READ &&
            dup2(pipes[1], 1) < 0)
            goto child_error;
    }
    else {	/* XXX will fail if pipes[0] == 4 (unlikely..) */
        if (flags & FORKER_FLAG_DO_READ && dup2(pipes[1], 4) < 0)
            goto child_error;
        if (flags & FORKER_FLAG_DO_WRITE && dup2(pipes[0], 3) < 0)
            goto child_error;
    }

    if (dup2(pipes[2], 2) < 0)
        goto child_error;

    if (wd && chdir(wd) < 0)
        goto child_error;

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
child_error:
    DEBUG_PRINT("exec error: %d\r\n",errno);
    _exit(128 + errno);
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
    int buff[2], res;

    sys_sigblock(SIGCHLD);

    while ((buff[0] = waitpid((pid_t)(-1), buff+1, WNOHANG)) > 0) {
        if ((res = write(sigchld_pipe[1], buff, sizeof(buff))) <= 0)
            ABORT("Failed to write to sigchld_pipe (%d): %d (%d)", sigchld_pipe[1], res, errno);
        DEBUG_PRINT("Reap child %d (%d)", buff[0], buff[1]);
    }

    sys_sigrelease(SIGCHLD);
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
        perror(0);
        exit(1);
    }

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
            char buff[256];
            errno = 0;
            if ((res = sys_uds_read(uds_fd, buff, 1, pipes, 3, MSG_DONTWAIT)) < 0) {
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
            ASSERT(res == 1);
            ASSERT(buff[0] == 'S');

            sys_sigblock(SIGCHLD);

            errno = 0;

            os_pid = fork();
            if (os_pid == 0)
                start_new_child(pipes);

            /* We write an ack here, but expect the reply on
               the pipes[0] inside the fork */
            res = sprintf(buff,"GO:%010d:%010d", os_pid, errno);
            if (write(pipes[1], buff, res + 1))
                ; /* remove gcc warning */

            sys_sigrelease(SIGCHLD);
            close(pipes[0]);
            close(pipes[1]);
            close(pipes[2]);
        }

        if (FD_ISSET(sigchld_pipe[0], &read_fds)) {
            int ibuff[2];
            char buff[256];
            res = read(sigchld_pipe[0], ibuff, sizeof(ibuff));
            if (res <= 0) {
                if (errno == EINTR)
                    continue;
                ABORT("Failed to read from sigchld pipe: %d (%d)", res, errno);
            }
            res = snprintf(buff, 256, "SIGCHLD:%010d:%010d", ibuff[0], ibuff[1]);
            DEBUG_PRINT("send %s to %d", buff, uds_fd);
            if (write(uds_fd, buff, res + 1) < 0) {
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
