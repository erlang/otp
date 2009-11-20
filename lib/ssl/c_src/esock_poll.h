/*<copyright>
 * <year>2005-2008</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
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
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */
#ifndef ESOCK_POLL_SELECT_H
#define ESOCK_POLL_SELECT_H

#if !defined(USE_SELECT)
#include <poll.h>
#endif

typedef struct esock_poll {
#ifdef USE_SELECT
    fd_set readmask;
    fd_set writemask;
    fd_set exceptmask;
#else
    int* fd_to_poll;		/* Map from fd to index into poll
				 * descriptor array.
				 */
    int num_fds;		/* Number of entries in fd_to_poll. */
    struct pollfd* fds;		/* Array of poll descriptors. */
    int allocated;		/* Allocated number of fds. */
    int active;			/* Active number of fds */
#endif
} EsockPoll;

void esock_poll_init(EsockPoll *ep);
void esock_poll_zero(EsockPoll *ep);

void esock_poll_fd_set_read(EsockPoll *ep, FD fd);
void esock_poll_fd_set_write(EsockPoll *ep, FD fd);

void esock_poll_clear_event(EsockPoll *ep, FD fd);

int esock_poll_fd_isset_read(EsockPoll *ep, FD fd);
int esock_poll_fd_isset_write(EsockPoll *ep, FD fd);

#ifdef __WIN32__
void esock_poll_fd_set_exception(EsockPoll *ep, FD fd);
int esock_poll_fd_isset_exception(EsockPoll *ep, FD fd);
#endif

int esock_poll(EsockPoll *ep, int seconds);
#endif
