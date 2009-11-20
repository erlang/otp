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

/*
 * Purpose: Hide poll() and select() behind an API so that we
 * can use either one.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef __WIN32__
#include "esock_winsock.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>

#ifdef __WIN32__
#include <process.h>
#else
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/time.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <fcntl.h>
#endif

#include "esock.h"
#include "esock_ssl.h"
#include "esock_utils.h"
#include "esock_poll.h"
#include "debuglog.h"

#if !defined(USE_SELECT)

/* At least on FreeBSD, we need POLLRDNORM for normal files, not POLLIN. */
/* Whether this is a bug in FreeBSD, I don't know. */
#ifdef POLLRDNORM
#define POLL_INPUT	(POLLIN | POLLRDNORM)
#else
#define POLL_INPUT	POLLIN
#endif

static void poll_fd_set(EsockPoll *ep, FD fd, short events)
{
    int i, j;
    int prev_num_fds = ep->num_fds;

    if (ep->num_fds <= fd) {
	ep->num_fds = fd + 64;
	ep->fd_to_poll = (int *) esock_realloc(ep->fd_to_poll,
					       ep->num_fds*sizeof(int));
	for (j = prev_num_fds; j < ep->num_fds; j++)
	    ep->fd_to_poll[j] = -1;
    }
    i = ep->fd_to_poll[fd];
    if (i > 0 && i < ep->active && ep->fds[i].fd == fd) {
	/* Already present in poll array */
	ep->fds[i].events |= events;
    } else {
	/* Append to poll array */
	if (ep->active >= ep->allocated) {
	    ep->allocated *= 2;
	    ep->fds = (struct pollfd *)
		esock_realloc(ep->fds, ep->allocated*sizeof(struct pollfd));
	}
	ep->fd_to_poll[fd] = ep->active;
	ep->fds[ep->active].fd = fd;
	ep->fds[ep->active].events = events;
	ep->fds[ep->active].revents = 0;
	ep->active++;
    }
}

static int poll_is_set(EsockPoll *ep, FD fd, short mask)
{
    if (fd >= ep->num_fds) {
	return 0;
    } else {
	int i = ep->fd_to_poll[fd];
	return 0 <= i && i < ep->active && ep->fds[i].fd == fd &&
	    (ep->fds[i].revents & mask) != 0;
    }
}

#endif

void esock_poll_init(EsockPoll *ep)
{
#ifdef USE_SELECT
    /* Nothing to do here */
#else
    ep->allocated = 2;
    ep->fds = (struct pollfd *) esock_malloc(ep->allocated*sizeof(struct pollfd));
    ep->num_fds = 1;
    ep->fd_to_poll = esock_malloc(ep->num_fds*sizeof(int));
#endif    
}

void esock_poll_zero(EsockPoll *ep)
{
#ifdef USE_SELECT
    FD_ZERO(&ep->readmask);
    FD_ZERO(&ep->writemask);
    FD_ZERO(&ep->exceptmask);
#else
    int i;

    for (i = 0; i < ep->num_fds; i++)
	ep->fd_to_poll[i] = -1;
    ep->active = 0;
#endif    
}

void esock_poll_fd_set_read(EsockPoll *ep, FD fd)
{
#ifdef USE_SELECT
    FD_SET(fd, &ep->readmask);
#else
    poll_fd_set(ep, fd, POLL_INPUT);
#endif    
}

void esock_poll_fd_set_write(EsockPoll *ep, FD fd)
{
#ifdef USE_SELECT
    FD_SET(fd, &ep->writemask);
#else
    poll_fd_set(ep, fd, POLLOUT);
#endif    
}

int esock_poll_fd_isset_read(EsockPoll *ep, FD fd)
{
#ifdef USE_SELECT
    return FD_ISSET(fd, &ep->readmask);
#else
    return poll_is_set(ep, fd, (POLL_INPUT|POLLHUP|POLLERR|POLLNVAL));
#endif    
}

int esock_poll_fd_isset_write(EsockPoll *ep, FD fd)
{
#ifdef USE_SELECT
    return FD_ISSET(fd, &ep->writemask);
#else
    return poll_is_set(ep, fd, (POLLOUT|POLLHUP|POLLERR|POLLNVAL));
#endif    
}

#ifdef __WIN32__
void esock_poll_fd_set_exception(EsockPoll *ep, FD fd)
{
    FD_SET(fd, &ep->exceptmask);
}

int esock_poll_fd_isset_exception(EsockPoll *ep, FD fd)
{
    return FD_ISSET(fd, &ep->exceptmask);
}
#endif

int esock_poll(EsockPoll *ep, int seconds)
{
    int sret;

#ifdef USE_SELECT
    struct timeval tv;

    tv.tv_sec = seconds;
    tv.tv_usec = 0;
    sret = select(FD_SETSIZE, &ep->readmask, &ep->writemask, &ep->exceptmask, &tv);
    if (sret == 0) {
	FD_ZERO(&ep->readmask);
	FD_ZERO(&ep->writemask);
	FD_ZERO(&ep->exceptmask);
    }
#else
    sret = poll(ep->fds, ep->active, 1000*seconds);
#endif
    return sret;
}

void esock_poll_clear_event(EsockPoll* ep, FD fd)
{
#ifdef USE_SELECT
    FD_CLR(fd, &ep->readmask);
    FD_CLR(fd, &ep->writemask);
    FD_CLR(fd, &ep->exceptmask);
#else
    int i = ep->fd_to_poll[fd];
    if (i > 0 && ep->fds[i].fd == fd)
	ep->fds[i].revents = 0;
#endif
}
