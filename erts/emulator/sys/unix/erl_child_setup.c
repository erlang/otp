/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
 * After a vfork() (or fork()) the child exec()s to this program which
 * sets up the child and exec()s to the user program (see spawn_start()
 * in sys.c and ticket OTP-4389).
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define NEED_CHILD_SETUP_DEFINES
#include "sys.h"
#include "erl_misc_utils.h"

#ifdef SIG_SIGSET		/* Old SysV */
void sys_sigrelease(int sig)
{
    sigrelse(sig);
}
#else /* !SIG_SIGSET */
#ifdef SIG_SIGNAL		/* Old BSD */
sys_sigrelease(int sig)
{
    sigsetmask(sigblock(0) & ~sigmask(sig));
}
#else /* !SIG_SIGNAL */	/* The True Way - POSIX!:-) */
void sys_sigrelease(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *)NULL);
}
#endif /* !SIG_SIGNAL */
#endif /* !SIG_SIGSET */

#if defined(__ANDROID__)
static int system_properties_fd(void);
#endif /* __ANDROID__ */

#if defined(__ANDROID__)
#define SHELL "/system/bin/sh"
#else
#define SHELL "/bin/sh"
#endif /* __ANDROID__ */


int
main(int argc, char *argv[])
{
    int i, from, to;
    int erts_spawn_executable = 0;

    /* OBSERVE!
     * Keep child setup after fork() (implemented in sys.c) up to date
     * if changes are made here.
     */

    if (argc != CS_ARGV_NO_OF_ARGS) {
	if (argc < CS_ARGV_NO_OF_ARGS) {
	    return 1;
	} else {
	    erts_spawn_executable = 1;
	}
    }

    if (strcmp("false", argv[CS_ARGV_UNBIND_IX]) != 0)
	if (erts_unbind_from_cpu_str(argv[CS_ARGV_UNBIND_IX]) != 0)
	    return 1;

    for (i = 0; i < CS_ARGV_NO_OF_DUP2_OPS; i++) {
	if (argv[CS_ARGV_DUP2_OP_IX(i)][0] == '-'
	    && argv[CS_ARGV_DUP2_OP_IX(i)][1] == '\0')
	    break;
	if (sscanf(argv[CS_ARGV_DUP2_OP_IX(i)], "%d:%d", &from, &to) != 2)
	    return 1;
	if (dup2(from, to) < 0)
	    return 1;
    }

    if (sscanf(argv[CS_ARGV_FD_CR_IX], "%d:%d", &from, &to) != 2)
	return 1;

#if defined(HAVE_CLOSEFROM)
    closefrom(from);
#elif defined(__ANDROID__)
    if (from <= to) {
	int spfd = system_properties_fd();
	for (i = from; i <= to; i++) {
	    if (i != spfd) {
		(void) close(i);
            }
	}
    }
#else  /* !__ANDROID__ */
    for (i = from; i <= to; i++) {
	(void) close(i);
    }
#endif /* HAVE_CLOSEFROM */

    if (!(argv[CS_ARGV_WD_IX][0] == '.' && argv[CS_ARGV_WD_IX][1] == '\0')
	&& chdir(argv[CS_ARGV_WD_IX]) < 0)
	return 1;

#if defined(USE_SETPGRP_NOARGS)		/* SysV */
    (void) setpgrp();
#elif defined(USE_SETPGRP)		/* BSD */
    (void) setpgrp(0, getpid());
#else					/* POSIX */
    (void) setsid();
#endif

    sys_sigrelease(SIGCHLD);
    sys_sigrelease(SIGINT);
    sys_sigrelease(SIGUSR1);

    if (erts_spawn_executable) {
	if (argv[CS_ARGV_NO_OF_ARGS + 1] == NULL) {
	    execl(argv[CS_ARGV_NO_OF_ARGS],argv[CS_ARGV_NO_OF_ARGS],
		  (char *) NULL);
	} else {
	    execv(argv[CS_ARGV_NO_OF_ARGS],&(argv[CS_ARGV_NO_OF_ARGS + 1]));
	}
    } else {
        execl(SHELL, "sh", "-c", argv[CS_ARGV_CMD_IX], (char *) NULL);
    }
    return 1;
}

#if defined(__ANDROID__)
static int system_properties_fd(void)
{
    int fd;
    char *env;

    env = getenv("ANDROID_PROPERTY_WORKSPACE");
    if (!env) {
        return -1;
    }
    fd = atoi(env);
    return fd;
}
#endif /* __ANDROID__ */
