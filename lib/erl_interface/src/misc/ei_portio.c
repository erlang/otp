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
 *

 */
#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <errno.h>

static unsigned long param_zero = 0;
static unsigned long param_one = 1;
#define SET_BLOCKING(Sock) ioctlsocket((Sock),FIONBIO,&param_zero)
#define SET_NONBLOCKING(Sock) ioctlsocket((Sock),FIONBIO,&param_one)

#define ERROR_WOULDBLOCK WSAEWOULDBLOCK
#define ERROR_TIMEDOUT WSAETIMEDOUT 
#define ERROR_INPROGRESS WSAEINPROGRESS
#define GET_SOCKET_ERROR() WSAGetLastError()
#define MEANS_SOCKET_ERROR(Ret) ((Ret == SOCKET_ERROR))
#define IS_INVALID_SOCKET(Sock) ((Sock) == INVALID_SOCKET)

#elif VXWORKS
#include <vxWorks.h>
#include <hostLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>
#include <selectLib.h>
#include <sys/types.h>
#include <ioLib.h>
#include <unistd.h>

static unsigned long param_zero = 0;
static unsigned long param_one = 1;
#define SET_BLOCKING(Sock) ioctl((Sock),FIONBIO,(int)&param_zero)
#define SET_NONBLOCKING(Sock) ioctl((Sock),FIONBIO,(int)&param_one)
#define ERROR_WOULDBLOCK EWOULDBLOCK
#define ERROR_TIMEDOUT ETIMEDOUT 
#define ERROR_INPROGRESS EINPROGRESS
#define GET_SOCKET_ERROR() (errno)
#define MEANS_SOCKET_ERROR(Ret) ((Ret) == ERROR)
#define IS_INVALID_SOCKET(Sock) ((Sock) < 0)

#else /* other unix */
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#ifndef EWOULDBLOCK
#define ERROR_WOULDBLOCK EAGAIN
#else
#define ERROR_WOULDBLOCK EWOULDBLOCK
#endif
#define SET_BLOCKING(fd)  fcntl((fd), F_SETFL, \
				fcntl((fd), F_GETFL, 0) & ~O_NONBLOCK)
#define SET_NONBLOCKING(fd) fcntl((fd), F_SETFL, \
				  fcntl((fd), F_GETFL, 0) | O_NONBLOCK)
#define ERROR_TIMEDOUT ETIMEDOUT 
#define ERROR_INPROGRESS EINPROGRESS
#define GET_SOCKET_ERROR() (errno)
#define MEANS_SOCKET_ERROR(Ret) ((Ret) < 0)
#define IS_INVALID_SOCKET(Sock) ((Sock) < 0)

#endif

/* common includes */
#include "eidef.h"

#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include "ei_portio.h"
#include "ei_internal.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif

#ifdef HAVE_WRITEV
static int ei_writev_t(int fd,  struct  iovec  *iov,  int iovcnt, unsigned ms)
{
    int res;
    if (ms != 0) {
	fd_set writemask;
	struct timeval tv;
	tv.tv_sec = (time_t) (ms / 1000U);
	ms %= 1000U;
	tv.tv_usec = (time_t) (ms * 1000U);
	FD_ZERO(&writemask);
	FD_SET(fd,&writemask);
	switch (select(fd+1, NULL, &writemask, NULL, &tv)) {
	case -1 : 
	    return -1; /* i/o error */
	case 0:
	    return -2; /* timeout */
	default:
	    if (!FD_ISSET(fd, &writemask)) {
		return -1; /* Other error */
	    }
	}
    }
    res = writev(fd, iov, iovcnt);
    return (res < 0) ? -1 : res;
}

int ei_writev_fill_t(int fd,  const  struct  iovec  *iov,  int iovcnt, unsigned ms)
{
    int i;
    int done;
    struct iovec *iov_base = NULL;
    struct iovec *current_iov;
    int current_iovcnt;
    int sum;

    for (sum = 0, i = 0; i < iovcnt; ++i) {
	sum += iov[i].iov_len;
    }
    if (ms != 0U) {
	SET_NONBLOCKING(fd);
    } 
    current_iovcnt = iovcnt;
    current_iov = (struct iovec *) iov;
    done = 0;
    for (;;) {
	i = ei_writev_t(fd, current_iov, current_iovcnt, ms);
	if (i <= 0) { /* ei_writev_t should always return at least 1 */ 
	    if (ms != 0U) {
		SET_BLOCKING(fd);
	    }    
	    if (iov_base != NULL) {
		free(iov_base);
	    }
	    return (i);
	}
	done += i;

	if (done < sum) {
	    if (iov_base == NULL) {
		iov_base = malloc(sizeof(struct iovec) * iovcnt);
		if (iov_base == NULL) {
		return -1;
		}
		memcpy(iov_base, iov, sizeof(struct iovec) * iovcnt);
		current_iov = iov_base;
	    }
	    while (i > 0) {
		if (i < current_iov[0].iov_len) {
		    char *p = (char*)current_iov[0].iov_base;
		    current_iov[0].iov_base = p + i;
		    current_iov[0].iov_len -= i;
		    i = 0;
		} else {
		    i -= current_iov[0].iov_len;
		    current_iov++;
		    current_iovcnt--;
		}
	    }
	} else {
	    break;
	}
    } 
    if (ms != 0U) {
	SET_BLOCKING(fd);
    }
    if (iov_base != NULL) {
	free(iov_base);
    }
    return (sum);
}


#endif

int ei_connect_t(int fd, void *sinp, int sin_siz, unsigned ms)
{
    int res;
    int error;
    int s_res;
    struct timeval tv;
    fd_set writefds;
    fd_set exceptfds;

    if (ms == 0) {
	res = connect(fd, sinp, sin_siz);
	return (res < 0) ? -1 : res;
    } else {
	SET_NONBLOCKING(fd);
	res = connect(fd, sinp, sin_siz);
	error = GET_SOCKET_ERROR();
	SET_BLOCKING(fd);
	if (!MEANS_SOCKET_ERROR(res)) {
	    return (res < 0) ? -1 : res;
	} else {
	    if (error != ERROR_WOULDBLOCK && 
		error != ERROR_INPROGRESS) {
		return -1;
	    } else {
		tv.tv_sec = (long) (ms/1000U);
		ms %= 1000U;
		tv.tv_usec = (long) (ms * 1000U);
		FD_ZERO(&writefds);
		FD_SET(fd,&writefds);
		FD_ZERO(&exceptfds);
		FD_SET(fd,&exceptfds);
		s_res = select(fd + 1, NULL, &writefds, &exceptfds, &tv);
		switch (s_res) {
		case 0:
		    return -2;
		case 1:
		    if (FD_ISSET(fd, &exceptfds)) {
			return  -1;
		    } else {
			return 0; /* Connect completed */
		    }
		default:
		    return -1;
		}
	    }
	} 
    }
}

int ei_accept_t(int fd, void  *addr,   void  *addrlen, unsigned ms)
{
    int res;
    if (ms != 0) {
	fd_set readmask;
	struct timeval tv;
	tv.tv_sec = (time_t) (ms / 1000U);
	ms %= 1000U;
	tv.tv_usec = (time_t) (ms * 1000U);
	FD_ZERO(&readmask);
	FD_SET(fd,&readmask);
	switch (select(fd+1, &readmask, NULL, NULL, &tv)) {
	case -1 : 
	    return -1; /* i/o error */
	case 0:
	    return -2; /* timeout */
	default:
	    if (!FD_ISSET(fd, &readmask)) {
		return -1; /* Other error */
	    }
	}
    }
    res = (int) accept(fd,addr,addrlen);
    return (res < 0) ? -1 : res;
}
    


static int ei_read_t(int fd, char* buf, int len, unsigned  ms)
{
    int res;
    if (ms != 0) {
	fd_set readmask;
	struct timeval tv;
	tv.tv_sec = (time_t) (ms / 1000U);
	ms %= 1000U;
	tv.tv_usec = (time_t) (ms * 1000U);
	FD_ZERO(&readmask);
	FD_SET(fd,&readmask);
	switch (select(fd+1, &readmask, NULL, NULL, &tv)) {
	case -1 : 
	    return -1; /* i/o error */
	case 0:
	    return -2; /* timeout */
	default:
	    if (!FD_ISSET(fd, &readmask)) {
		return -1; /* Other error */
	    }
	}
    }
    res = readsocket(fd, buf, len);
    return (res < 0) ? -1 : res;
}

static int ei_write_t(int fd, const char* buf, int len, unsigned  ms)
{
    int res;
    if (ms != 0) {
	fd_set writemask;
	struct timeval tv;
	tv.tv_sec = (time_t) (ms / 1000U);
	ms %= 1000U;
	tv.tv_usec = (time_t) (ms * 1000U);
	FD_ZERO(&writemask);
	FD_SET(fd,&writemask);
	switch (select(fd+1, NULL, &writemask, NULL, &tv)) {
	case -1 : 
	    return -1; /* i/o error */
	case 0:
	    return -2; /* timeout */
	default:
	    if (!FD_ISSET(fd, &writemask)) {
		return -1; /* Other error */
	    }
	}
    }
    res =  writesocket(fd, buf, len);
    return (res < 0) ? -1 : res;
}
	
/* 
 * Fill buffer, return buffer length, 0 for EOF, < 0 (and sets errno)
 * for error.  */
int ei_read_fill_t(int fd, char* buf, int len, unsigned  ms)
{
    int i,got=0;

    do {
	i = ei_read_t(fd, buf+got, len-got, ms);
	if (i <= 0)
	    return (i);
	got += i;
    } while (got < len);
    return (len);
    
} /* read_fill */

int ei_read_fill(int fd, char* buf, int len)
{
    return ei_read_fill_t(fd, buf, len, 0);
}

/* write entire buffer on fd  or fail (setting errno)
 */
int ei_write_fill_t(int fd, const char *buf, int len, unsigned ms)
{
    int i,done=0;
    if (ms != 0U) {
	SET_NONBLOCKING(fd);
    }    
    do {
	i = ei_write_t(fd, buf+done, len-done, ms);
	if (i <= 0) {
	    if (ms != 0U) {
		SET_BLOCKING(fd);
	    }    
	    return (i);
	}
	done += i;
    } while (done < len);
    if (ms != 0U) {
	SET_BLOCKING(fd);
    }
    return (len);
}

int ei_write_fill(int fd, const char *buf, int len) 
{
    return ei_write_fill_t(fd, buf, len, 0);
}

