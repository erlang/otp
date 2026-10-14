/*
 * %CopyrightBegin%
 *
 * SPDX-FileCopyrightText: 2025 Ericsson and contributors
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * %CopyrightEnd%
 */

/*
 * Test utility which waits for SIGTERM and responds by deleting a temp file
 * path.
 */
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>

#ifndef __WIN32__
#include <unistd.h>

#define O_BINARY 0
#define _setmode(fd, mode)
#endif

#ifdef __WIN32__
#include "windows.h"
#include "winbase.h"
#endif

extern int errno;

const char* MSG_WAITING = "waiting.";
int ERL_FD = 1;

char* tmp_file;

static void handle_sigterm(int sig);
static int delay(unsigned ms);

int main(int argc, char *argv[]) {
    if (argc < 2 || argc > 3)
        exit(1);

    tmp_file = argv[1];
    int os_pid_to_kill = (argc == 3 ? atoi(argv[2]) : 0);

    struct sigaction sa;
    sa.sa_handler = &handle_sigterm;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    if (sigaction(SIGTERM, &sa, 0) == -1) {
        fprintf(stderr, "Failed to set signal handler: %d\n", errno);
        exit(1);
    }

    _setmode(ERL_FD, O_BINARY);

    if (os_pid_to_kill) {
        kill(os_pid_to_kill, SIGKILL);
    }

    write(ERL_FD, MSG_WAITING, sizeof(MSG_WAITING));
    /* Wait 60s for SIGTERM. */
    if (delay(60000) == 0) {
        fprintf(stderr, "Failure to stop child process.\n");
        exit(1);
    }
}

static void
handle_sigterm(int sig) {
    unlink(tmp_file);
    /* TODO: Ideally we send a message to the grandparent process here. */
}

static int
delay(unsigned ms)
{
#ifdef __WIN32__
    Sleep(ms);
    /* TODO: Does win32 also continue here after signal? */
    return 0;
#else
    struct timeval t;
    t.tv_sec = ms / 1000;
    t.tv_usec = (ms % 1000) * 1000;

    return select(0, NULL, NULL, NULL, &t);
#endif
}
