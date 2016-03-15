/*
 * %CopyrightBegin%

 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
 * hipe_x86_signal.c
 *
 * Erlang code compiled to x86 native code uses the x86 %esp as its
 * stack pointer. This improves performance in several ways:
 * - It permits the use of the x86 call and ret instructions, which
 *   reduces code volume and improves branch prediction.
 * - It avoids stealing a gp register to act as a stack pointer.
 *
 * Unix signal handlers are by default delivered onto the current
 * stack, i.e. %esp. This is a problem since our native-code stacks
 * are small and may not have room for the Unix signal handler.
 *
 * There is a way to redirect signal handlers to an "alternate" signal
 * stack by using the SA_ONSTACK flag with the sigaction() library call.
 * Unfortunately, this has to be specified explicitly for each signal,
 * and it is difficult to enforce given the presence of libraries.
 *
 * Our solution is to override the C library's signal handler setup
 * procedure with our own which enforces the SA_ONSTACK flag.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef ERTS_SMP
#include "sys.h"
#include "erl_alloc.h"
#endif
#include "hipe_signal.h"

#if __GLIBC__ == 2 && (__GLIBC_MINOR__ >= 3)
/*
 * __libc_sigaction() is the core routine.
 * Without libpthread, sigaction() and __sigaction() are both aliases
 * for __libc_sigaction().
 * libpthread redefines __sigaction() as a non-trivial wrapper around
 * __libc_sigaction(), and makes sigaction() an alias for __sigaction().
 * glibc has internal calls to both sigaction() and __sigaction().
 *
 * Overriding __libc_sigaction() would be ideal, but doing so breaks
 * libpthread (threads hang).
 *
 * Overriding __sigaction(), using dlsym RTLD_NEXT to find glibc's
 * version of __sigaction(), works with glibc-2.2.4 and 2.2.5.
 * Unfortunately, this solution doesn't work with earlier versions,
 * including glibc-2.2.2 and glibc-2.1.92 (2.2 despite its name):
 * 2.2.2 SIGSEGVs in dlsym RTLD_NEXT (known glibc bug), and 2.1.92
 * SIGSEGVs inexplicably in two test cases in the HiPE test suite.
 *
 * Instead we only override sigaction() and call __sigaction()
 * directly. This should work for HiPE/x86 as long as only the Posix
 * signal interface is used, i.e. there are no calls to simulated
 * old BSD or SysV interfaces.
 * glibc's internal calls to __sigaction() appear to be mostly safe.
 * hipe_signal_init() fixes some unsafe ones, e.g. the SIGPROF handler.
 */
#ifndef __USE_GNU
#define __USE_GNU		/* to un-hide RTLD_NEXT */
#endif
#define NEXT_SIGACTION "__sigaction"
#define LIBC_SIGACTION __sigaction
#define OVERRIDE_SIGACTION
#endif	/* glibc >= 2.3 */

/* Is there no standard identifier for Darwin/MacOSX ? */
#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#if defined(__DARWIN__)
/*
 * Assumes Mac OS X >= 10.3 (dlsym operations not available in 10.2 and
 * earlier).
 *
 * The code below assumes that is is part of the main image (earlier
 * in the load order than libSystem and certainly before any dylib
 * that might use sigaction) -- a standard RTLD_NEXT caveat.
 *
 * _sigaction lives in /usr/lib/libSystem.B.dylib and can be found
 * with the standard dlsym(RTLD_NEXT) call. The proviso on Mac OS X
 * being that the symbol for dlsym doesn't include a leading '_'.
 *
 * The other _sigaction, _sigaction_no_bind I don't understand the purpose
 * of and don't modify.
 */
#define NEXT_SIGACTION "sigaction"
#define LIBC_SIGACTION _sigaction
#undef OVERRIDE_SIGACTION
#define _NSIG NSIG
#endif /* __DARWIN__ */

#if defined(__sun__)
/*
 * Assume Solaris/x86 2.8.
 * There is a number of sigaction() procedures in libc:
 * * sigaction(): weak reference to _sigaction().
 * * _sigaction(): apparently a simple wrapper around __sigaction().
 * * __sigaction(): apparently the procedure doing the actual system call.
 * * _libc_sigaction(): apparently some thread-related wrapper, which ends
 *   up calling __sigaction().
 * The threads library redefines sigaction() and _sigaction() to its
 * own wrapper, which checks for and restricts access to threads-related
 * signals. The wrapper appears to eventually call libc's __sigaction().
 *
 * We catch and override _sigaction() since overriding __sigaction()
 * causes fatal errors in some cases.
 *
 * When linked with thread support, there are calls to sigaction() before
 * our init routine has had a chance to find _sigaction()'s address.
 * This forces us to initialise at the first call.
 */
#define NEXT_SIGACTION "_sigaction"
#define LIBC_SIGACTION _sigaction
#define OVERRIDE_SIGACTION
#define _NSIG NSIG
#endif /* __sun__ */

#if defined(__FreeBSD__)
/*
 * This is a copy of Darwin code for FreeBSD.
 * CAVEAT: detailed semantics are not verified yet.
 */
#define NEXT_SIGACTION "sigaction"
#define LIBC_SIGACTION _sigaction
#undef OVERRIDE_SIGACTION
#define _NSIG NSIG
#endif /* __FreeBSD__ */

#if defined(__NetBSD__)
/*
 * Note: This is only stub code to allow the build to succeed.
 * Whether this actually provides the needed overrides for safe
 * signal delivery or not is unknown.
 */
#undef NEXT_SIGACTION
#undef OVERRIDE_SIGACTION
#endif /* __NetBSD__ */

#if !(defined(__GLIBC__) || defined(__DARWIN__) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__sun__))
/*
 * Unknown libc -- assume musl.  Note: musl deliberately does not provide a musl-specific
 * feature test macro, so we cannot check for it.
 *
 * sigaction is a weak alias for __sigaction, which is a wrapper for __libc_sigaction.
 * There are libc-internal calls to __libc_sigaction which install handlers, so we must
 * override __libc_sigaction rather than __sigaction.
 */
#define NEXT_SIGACTION "__libc_sigaction"
#define LIBC_SIGACTION __libc_sigaction
#define OVERRIDE_SIGACTION
#ifndef _NSIG
#define _NSIG NSIG
#endif
#endif	/* !(__GLIBC__ || __DARWIN__ || __NetBSD__ || __FreeBSD__ || __sun__) */

#if defined(NEXT_SIGACTION)
/*
 * Initialize a function pointer to the libc core sigaction routine,
 * to be used by our wrappers.
 */
#include <dlfcn.h>
static int (*next_sigaction)(int, const struct sigaction*, struct sigaction*);
static void do_init(void)
{
    next_sigaction = dlsym(RTLD_NEXT, NEXT_SIGACTION);
    if (next_sigaction != 0)
	return;
    perror("dlsym");
    abort();
}
#define INIT()	do { if (!next_sigaction) do_init(); } while (0)
#else	/* !defined(NEXT_SIGACTION) */
#define INIT()	do { } while (0)
#endif	/* !defined(NEXT_SIGACTION) */

#if defined(NEXT_SIGACTION)
/*
 * This is our wrapper for sigaction(). sigaction() can be called before
 * hipe_signal_init() has been executed, especially when threads support
 * has been linked with the executable. Therefore, we must initialise
 * next_sigaction() dynamically, the first time it's needed.
 */
static int my_sigaction(int signum, const struct sigaction *act, struct sigaction *oldact)
{
    struct sigaction newact;

    INIT();

    if (act &&
	act->sa_handler != SIG_DFL &&
	act->sa_handler != SIG_IGN &&
	!(act->sa_flags & SA_ONSTACK)) {
	newact = *act;
	newact.sa_flags |= SA_ONSTACK;
	act = &newact;
    }
    return next_sigaction(signum, act, oldact);
}
#endif

#if defined(LIBC_SIGACTION)
/*
 * This overrides the C library's core sigaction() procedure, catching
 * all its internal calls.
 */
extern int LIBC_SIGACTION(int, const struct sigaction*, struct sigaction*);
int LIBC_SIGACTION(int signum, const struct sigaction *act, struct sigaction *oldact)
{
    return my_sigaction(signum, act, oldact);
}
#endif

#if defined(OVERRIDE_SIGACTION)
/*
 * This catches the application's own sigaction() calls.
 */
int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact)
{
    return my_sigaction(signum, act, oldact);
}
#endif

/*
 * Set alternate signal stack for the invoking thread.
 */
static void hipe_sigaltstack(void *ss_sp)
{
    stack_t ss;

    ss.ss_sp = ss_sp;
    ss.ss_flags = 0;
    ss.ss_size = SIGSTKSZ;
    if (sigaltstack(&ss, NULL) < 0) {
	perror("sigaltstack");
	abort();
    }
}

#ifdef ERTS_SMP
/*
 * Set up alternate signal stack for an Erlang process scheduler thread.
 */
void hipe_thread_signal_init(void)
{
    /* Stack don't really need to be cache aligned.
       We use it to suppress false leak report from valgrind */
    hipe_sigaltstack(erts_alloc_permanent_cache_aligned(ERTS_ALC_T_HIPE, SIGSTKSZ));
}
#endif

/*
 * Set up alternate signal stack for the main thread,
 * unless this is a multithreaded runtime system.
 */
static void hipe_sigaltstack_init(void)
{
#if !defined(ERTS_SMP)
    static unsigned long my_sigstack[SIGSTKSZ/sizeof(long)];
    hipe_sigaltstack(my_sigstack);
#endif
}

/*
 * 1. Set up alternate signal stack for the main thread.
 * 2. Add SA_ONSTACK to existing user-defined signal handlers.
 */
void hipe_signal_init(void)
{
    struct sigaction sa;
    int i;

    INIT();

    hipe_sigaltstack_init();

    for (i = 1; i < _NSIG; ++i) {
	if (sigaction(i, NULL, &sa)) {
	    /* This will fail with EINVAL on Solaris if 'i' is one of the
	       thread library's private signals. We DO catch the initial
	       setup of these signals, so things MAY be OK anyway. */
	    continue;
	}
	if (sa.sa_handler == SIG_DFL ||
	    sa.sa_handler == SIG_IGN ||
	    (sa.sa_flags & SA_ONSTACK))
	    continue;
	sa.sa_flags |= SA_ONSTACK;
	if (sigaction(i, &sa, NULL)) {
#ifdef SIGCANCEL
	    /* Solaris 9 x86 refuses to let us modify SIGCANCEL. */
	    if (i == SIGCANCEL)
		continue;
#endif
	    perror("sigaction");
	    abort();
	}
    }
}
