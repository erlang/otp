/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2024. All Rights Reserved.
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
 * Erlang code compiled to x86 native code uses RSP as its stack pointer. This
 * improves performance in several ways:
 *
 * - It permits the use of the x86 call and ret instructions, which
 *   reduces code volume and improves branch prediction.
 * - It avoids stealing a gp register to act as a stack pointer.
 *
 * Unix signal handlers are by default delivered onto the current stack, i.e.
 * RSP. This is a problem since our native-code stacks are small and may not
 * have room for the Unix signal handler.
 *
 * There is a way to redirect signal handlers to an "alternate" signal stack by
 * using the SA_ONSTACK flag with the sigaction(2) system call. Unfortunately,
 * this has to be specified explicitly for each signal, and it is impossible to
 * enforce given the presence of libraries.
 *
 * We used to attempt to override the C library's signal handler setup
 * procedure with our own that added the SA_ONSTACK flag, but it only worked
 * with `GNU libc` which is not always the current libc. As many of our users
 * liked to run docker images with `Alpine` which uses `musl` instead, they got
 * needlessly bad performance without knowing it.
 *
 * Instead, we now explicitly add SA_ONSTACK to our own uses of sigaction(2)
 * and ignore the library problem altogether because:
 * 
 *   1. We don't care about this problem on non-scheduler threads: if a library
 *      wants to fiddle around with signals on its own threads then it doesn't
 *      affect us.
 *   2. We don't care about this problem when executing on the runtime stack:
 *      if a NIF or driver uses signals in a creative manner locally during a
 *      call, then that's fine as long as they restore them before returning to
 *      Erlang code.
 *
 *      A NIF or driver that doesn't do this is misbehaving to begin with and
 *      we can't shield ourselves against that.
 *   3. If a library that we're statically linked to messes around with signals
 *      in the initialization phase (think C++ constructors of static objects),
 *      all of it will happen before `main` runs and we'll set things straight
 *      in `sys_init_signal_stack`.
 *
 *      If a dynamically linked library does the same, the same restrictions as
 *      ordinary NIF/driver calls apply to the initialization phase and the
 *      library must restore the signals before returning.
 *
 *      If any threads are created in either of these phases, they're still not
 *      scheduler threads so we don't have to care then either.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#include "sys.h"
#include "erl_alloc.h"
#include "erl_vm.h"

#if (defined(BEAMASM) && defined(NATIVE_ERLANG_STACK))

#if defined(NSIG)
#  define HIGHEST_SIGNAL NSIG
#elif defined(_NSIG)
#  define HIGHEST_SIGNAL _NSIG
#endif

/*
 * Set alternate signal stack for the invoking thread.
 */
static void sys_sigaltstack(void *ss_sp) {
    stack_t ss;

    ss.ss_sp = ss_sp;
    ss.ss_flags = 0;
    ss.ss_size = SIGSTKSZ;

    if (sigaltstack(&ss, NULL) < 0) {
        ERTS_INTERNAL_ERROR("Failed to set alternate signal stack");
    }
}

/*
 * Set up alternate signal stack for an Erlang process scheduler thread.
 */
void sys_thread_init_signal_stack(void) {
    /* This will never be freed. */
    char *stack = malloc(SIGSTKSZ);
    sys_sigaltstack(stack);
}

/*
 * 1. Set up alternate signal stack for the main thread.
 * 2. Add SA_ONSTACK to existing user-defined signal handlers.
 */
void sys_init_signal_stack(void) {
    struct sigaction sa;
    int i;

    sys_thread_init_signal_stack();

    for (i = 1; i < HIGHEST_SIGNAL; ++i) {
        if (sigaction(i, NULL, &sa)) {
            /* This will fail with EINVAL on Solaris if 'i' is one of the
               thread library's private signals. We DO catch the initial
               setup of these signals, so things MAY be OK anyway. */
            continue;
        }

        if (sa.sa_handler == SIG_DFL || sa.sa_handler == SIG_IGN ||
            (sa.sa_flags & SA_ONSTACK)) {
            continue;
        }

        sa.sa_flags |= SA_ONSTACK;

        if (sigaction(i, &sa, NULL)) {
#ifdef SIGCANCEL
            /* Solaris 9 x86 refuses to let us modify SIGCANCEL. */
            if (i == SIGCANCEL)
                continue;
#endif
            ERTS_INTERNAL_ERROR("Failed to use alternate signal stack");
        }
    }
}

#else

void sys_init_signal_stack(void) {
    /* Not required for this configuration. */
}

void sys_thread_init_signal_stack(void) {
    /* Not required for this configuration. */
}

#endif
