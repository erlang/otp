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
static void delay(unsigned ms);
static void write_erl(const char* buf, unsigned size);

int main(int argc, char *argv[]) {
    if (argc != 2)
        exit(1);

    tmp_file = argv[1];

    struct sigaction sa;
    sa.sa_handler = &handle_sigterm;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    if (sigaction(SIGTERM, &sa, 0) == -1) {
        fprintf(stderr, "Failed to set signal handler: %d\n", errno);
        exit(1);
    }

    _setmode(ERL_FD, O_BINARY);

    write_erl(MSG_WAITING, sizeof(MSG_WAITING));
    /* Wait 60s for SIGTERM. */
    delay(60000);

    fprintf(stderr, "Failure to stop child process.\n");

    return 1;
}

static void
write_erl(const char* buf, unsigned size) {
    if (size != write(ERL_FD, buf, size)) {
        fprintf(stderr, "Failed to write message to erlang (%d): %s\n", errno, buf);
        exit(1);
    }
}

static void
handle_sigterm(int sig) {
    unlink(tmp_file);
    /* TODO: Ideally we send a message to the grandparent process here. */
}

static void
delay(unsigned ms)
{
#ifdef __WIN32__
    Sleep(ms);
#else
    struct timeval t;
    t.tv_sec = ms/1000;
    t.tv_usec = (ms % 1000) * 1000;

    select(0, NULL, NULL, NULL, &t);
#endif
}
