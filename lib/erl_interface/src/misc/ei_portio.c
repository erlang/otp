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

#include "eidef.h"

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>
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
#include <ioLib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <timers.h>

static unsigned long param_zero = 0;
static unsigned long param_one = 1;
#define SET_BLOCKING(Sock) ioctl((Sock),FIONBIO,(int)&param_zero)
#define SET_NONBLOCKING(Sock) ioctl((Sock),FIONBIO,(int)&param_one)
#define MEANS_SOCKET_ERROR(Ret) ((Ret) == ERROR)
#define IS_INVALID_SOCKET(Sock) ((Sock) < 0)

#else /* other unix */
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <arpa/inet.h>
#include <netdb.h>

#define SET_BLOCKING(fd)  fcntl((fd), F_SETFL, \
				fcntl((fd), F_GETFL, 0) & ~O_NONBLOCK)
#define SET_NONBLOCKING(fd) fcntl((fd), F_SETFL, \
				  fcntl((fd), F_GETFL, 0) | O_NONBLOCK)
#define MEANS_SOCKET_ERROR(Ret) ((Ret) < 0)
#define IS_INVALID_SOCKET(Sock) ((Sock) < 0)

#endif

/* common includes */

#include <sys/types.h>
#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#include "ei_portio.h"
#include "ei_internal.h"

#ifdef __WIN32__

#define writesocket(sock,buf,nbyte) send(sock,buf,nbyte,0)
#define readsocket(sock,buf,nbyte) recv(sock,buf,nbyte,0)

static int get_error(void)
{
    switch (WSAGetLastError()) {
    case WSAEWOULDBLOCK: return EWOULDBLOCK;
    case WSAETIMEDOUT: return ETIMEDOUT;
    case WSAEINPROGRESS: return EINPROGRESS;
    case WSA_NOT_ENOUGH_MEMORY: return ENOMEM;
    case WSA_INVALID_PARAMETER: return EINVAL;
    case WSAEBADF: return EBADF;
    case WSAEINVAL: return EINVAL;
    case WSAEADDRINUSE: return EADDRINUSE;
    case WSAENETUNREACH: return ENETUNREACH;
    case WSAECONNABORTED: return ECONNABORTED;
    case WSAECONNRESET: return ECONNRESET;
    case WSAECONNREFUSED: return ECONNREFUSED;
    case WSAEHOSTUNREACH: return EHOSTUNREACH;
    case WSAEMFILE: return EMFILE;
    case WSAEALREADY: return EALREADY;
    default: return EIO;
    }
}

#else /* not __WIN32__ */

#define writesocket write
#define readsocket  read
#define closesocket close
#define ioctlsocket ioctl

static int get_error(void)
{
    int err = errno;
    if (err == 0)
        return EIO; /* Make sure never to return 0 as error code... */
    return err;
}

#endif

int ei_plugin_socket_impl__ = 0;

/*
 * Callbacks for communication over TCP/IPv4
 */

static int tcp_get_fd(void *ctx, int *fd)
{
    return EI_DFLT_CTX_TO_FD__(ctx, fd);
}

static int tcp_hs_packet_header_size(void *ctx, int *sz)
{
    int fd;
    *sz = 2;
    return EI_DFLT_CTX_TO_FD__(ctx, &fd);
}

static int tcp_handshake_complete(void *ctx)
{
    int res, fd, one = 1;

    res = EI_DFLT_CTX_TO_FD__(ctx, &fd);
    if (res)
        return res;

    res = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *)&one, sizeof(one));
    if (MEANS_SOCKET_ERROR(res))
        return get_error();
    
    res = setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char *)&one, sizeof(one));
    if (MEANS_SOCKET_ERROR(res))
        return get_error();

    return 0;
}

static int tcp_socket(void **ctx, void *setup_ctx)
{
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (MEANS_SOCKET_ERROR(fd))
        return get_error();

    *ctx = EI_FD_AS_CTX__(fd);
    return 0;
}

static int tcp_close(void *ctx)
{
    int fd, res;

    res = EI_DFLT_CTX_TO_FD__(ctx, &fd);
    if (res)
        return res;

    res = closesocket(fd);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();

    return 0;
}

static int tcp_listen(void *ctx, void *addr, int *len, int backlog)
{
    int res, fd;
    socklen_t sz = (socklen_t) *len;
    int on = 1;

    res = EI_DFLT_CTX_TO_FD__(ctx, &fd);
    if (res)
        return res;

    res = setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *) &on, sizeof(on));
    if (MEANS_SOCKET_ERROR(res))
        return get_error();

    res = bind(fd, (struct sockaddr *) addr, sz);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();

    res = getsockname(fd, (struct sockaddr *) addr, (socklen_t *) &sz);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();
    *len = (int) sz;
    
    res = listen(fd, backlog);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();

    return 0;
}

static int tcp_accept(void **ctx, void *addr, int *len, unsigned unused)
{
    int fd, res;
    socklen_t addr_len = (socklen_t) *len;

    if (!ctx)
        return EINVAL;

    res = EI_DFLT_CTX_TO_FD__(*ctx, &fd);
    if (res)
        return res;
    
    res = accept(fd, (struct sockaddr*) addr, &addr_len);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();

    *len = (int) addr_len;

    *ctx = EI_FD_AS_CTX__(res);
    return 0;
}

static int tcp_connect(void *ctx, void *addr, int len, unsigned unused)
{
    int res, fd;

    res = EI_DFLT_CTX_TO_FD__(ctx, &fd);
    if (res)
        return res;

    res = connect(fd, (struct sockaddr *) addr, len);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();
    
    return 0;
}

#if defined(EI_HAVE_STRUCT_IOVEC__) && defined(HAVE_WRITEV)

static int tcp_writev(void *ctx, const void *viov, int iovcnt, ssize_t *len, unsigned unused)
{
    const struct iovec *iov = (const struct iovec *) viov;
    int fd, error;
    ssize_t res;

    error = EI_DFLT_CTX_TO_FD__(ctx, &fd);
    if (error)
        return error;

    res = writev(fd, iov, iovcnt);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();
    *len = res;
    return 0;
}

#endif

static int tcp_write(void *ctx, const char* buf, ssize_t *len, unsigned unused)
{
    int error, fd;
    ssize_t res;

    error = EI_DFLT_CTX_TO_FD__(ctx, &fd);
    if (error)
        return error;

    res = writesocket(fd, buf, *len);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();
    *len = res;
    return 0;
}

static int tcp_read(void *ctx, char* buf, ssize_t *len, unsigned unused)
{
    int error, fd;
    ssize_t res;

    error = EI_DFLT_CTX_TO_FD__(ctx, &fd);
    if (error)
        return error;

    res = readsocket(fd, buf, *len);
    if (MEANS_SOCKET_ERROR(res))
        return get_error();
    *len = res;
    return 0;
}

ei_socket_callbacks ei_default_socket_callbacks = {
    0, /* flags */
    tcp_socket,
    tcp_close,
    tcp_listen,
    tcp_accept,
    tcp_connect,
#if defined(EI_HAVE_STRUCT_IOVEC__) && defined(HAVE_WRITEV)
    tcp_writev,
#else
    NULL,
#endif
    tcp_write,
    tcp_read,

    tcp_hs_packet_header_size,
    tcp_handshake_complete,
    tcp_handshake_complete,
    tcp_get_fd

};


/*
 *
 */

#if defined(EI_HAVE_STRUCT_IOVEC__)

int ei_socket_callbacks_have_writev__(ei_socket_callbacks *cbs)
{
    return !!cbs->writev;
}

static int writev_ctx_t__(ei_socket_callbacks *cbs, void *ctx,
                          const struct iovec *iov, int iovcnt,
                          ssize_t *len,
                          unsigned ms)
{
    int error;

    if (!(cbs->flags & EI_SCLBK_FLG_FULL_IMPL) && ms != EI_SCLBK_INF_TMO) {
        int fd;

        error = EI_GET_FD__(cbs, ctx, &fd);
        if (error)
            return error;

        do {
            fd_set writemask;
            struct timeval tv;
            
            tv.tv_sec = (time_t) (ms / 1000U);
            ms %= 1000U;
            tv.tv_usec = (time_t) (ms * 1000U);
            FD_ZERO(&writemask);
            FD_SET(fd,&writemask);
            switch (select(fd+1, NULL, &writemask, NULL, &tv)) {
            case -1 : 
                error = get_error();
                if (error != EINTR)
                    return error;
                break;
            case 0:
                return ETIMEDOUT; /* timeout */
            default:
                if (!FD_ISSET(fd, &writemask)) {
                    return EIO; /* Other error */
                }
                error = 0;
                break;
            }
        } while (error == EINTR);
    }
    do {
        error = cbs->writev(ctx, (const void *) iov, iovcnt, len, ms);
    } while (error == EINTR);
    return error;
}

int ei_writev_fill_ctx_t__(ei_socket_callbacks *cbs, void *ctx,
                           const struct iovec *iov, int iovcnt,
                           ssize_t *len,
                           unsigned ms)
{
    ssize_t i, done, sum;
    struct iovec *iov_base = NULL;
    struct iovec *current_iov;
    int current_iovcnt;
    int fd, error;
    int basic;

    if (!cbs->writev)
        return ENOTSUP;
    
    error = EI_GET_FD__(cbs, ctx, &fd);
    if (error)
        return error;

    basic = !(cbs->flags & EI_SCLBK_FLG_FULL_IMPL);
    
    for (sum = 0, i = 0; i < iovcnt; ++i) {
	sum += iov[i].iov_len;
    }
    if (basic && ms != 0U) {
	SET_NONBLOCKING(fd);
    } 
    current_iovcnt = iovcnt;
    current_iov = (struct iovec *) iov;
    done = 0;
    for (;;) {
        
	error = writev_ctx_t__(cbs, ctx, current_iov, current_iovcnt, &i, ms);
        if (error) {
            *len = done;
	    if (ms != 0U) {
		SET_BLOCKING(fd);
	    }    
	    if (iov_base != NULL) {
		free(iov_base);
	    }
	    return error;
        }
	done += i;

	if (done < sum) {
	    if (iov_base == NULL) {
		iov_base = malloc(sizeof(struct iovec) * iovcnt);
		if (iov_base == NULL) {
                    *len = done;
                    return ENOMEM;
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
    if (basic && ms != 0U) {
	SET_BLOCKING(fd);
    }
    if (iov_base != NULL) {
	free(iov_base);
    }
    *len = done;
    return 0;
}

#endif /* defined(EI_HAVE_STRUCT_IOVEC__) */

int ei_socket_ctx__(ei_socket_callbacks *cbs, void **ctx, void *setup_ctx)
{
    int res;

    do {
        res = cbs->socket(ctx, setup_ctx);
    } while (res == EINTR);

    return res;
}

int ei_close_ctx__(ei_socket_callbacks *cbs, void *ctx)
{
    return cbs->close(ctx);
}

int ei_connect_ctx_t__(ei_socket_callbacks *cbs, void *ctx,
                       void *addr, int len, unsigned ms)
{
    int res, fd;

    if ((cbs->flags & EI_SCLBK_FLG_FULL_IMPL) || ms == EI_SCLBK_INF_TMO) {
        do {
            res = cbs->connect(ctx, addr, len, ms);
        } while (res == EINTR);
        return res;
    }

    res = EI_GET_FD__(cbs, ctx, &fd);
    if (res)
        return res;
    
    SET_NONBLOCKING(fd);
    do {
        res = cbs->connect(ctx, addr, len, 0);
    } while (res == EINTR);
    SET_BLOCKING(fd);

    switch (res) {
    case EINPROGRESS:
    case EAGAIN:
#ifdef EWOULDBLOCK
#if EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK:
#endif
#endif
        break;
    default:
        return res;
    }

    while (1) {
        struct timeval tv;
        fd_set writefds;
        fd_set exceptfds;
        
        tv.tv_sec = (long) (ms/1000U);
        ms %= 1000U;
        tv.tv_usec = (long) (ms * 1000U);
        FD_ZERO(&writefds);
        FD_SET(fd,&writefds);
        FD_ZERO(&exceptfds);
        FD_SET(fd,&exceptfds);
        res = select(fd + 1, NULL, &writefds, &exceptfds, &tv);
        switch (res) {
        case -1:
            res = get_error();
            if (res != EINTR)
                return res;
            break;
        case 0:
            return ETIMEDOUT;
        case 1:
            if (!FD_ISSET(fd, &exceptfds))
                return 0; /* Connect completed */
            /* fall through... */
        default:
            return EIO;
        }
    }
}

int ei_listen_ctx__(ei_socket_callbacks *cbs, void *ctx,
                    void *adr, int *len, int backlog)
{
    int res;

    do {
        res = cbs->listen(ctx, adr, len, backlog);
    } while (res == EINTR);
    return res;
}

int ei_accept_ctx_t__(ei_socket_callbacks *cbs, void **ctx,
                      void *addr, int *len, unsigned ms)
{
    int error;

    if (!(cbs->flags & EI_SCLBK_FLG_FULL_IMPL) && ms != EI_SCLBK_INF_TMO) {
        int fd;

        error = EI_GET_FD__(cbs, *ctx, &fd);
        if (error)
            return error;

        do {
            fd_set readmask;
            struct timeval tv;
            
            tv.tv_sec = (time_t) (ms / 1000U);
            ms %= 1000U;
            tv.tv_usec = (time_t) (ms * 1000U);
            FD_ZERO(&readmask);
            FD_SET(fd,&readmask);
            switch (select(fd+1, &readmask, NULL, NULL, &tv)) {
            case -1 : 
                error = get_error();
                if (error != EINTR)
                    return error;
                break;
            case 0:
                return ETIMEDOUT; /* timeout */
            default:
                if (!FD_ISSET(fd, &readmask)) {
                    return EIO; /* Other error */
                }
                error = 0;
                break;
            }
        } while (error == EINTR);
    }
    do {
        error = cbs->accept(ctx, addr, len, ms);
    } while (error == EINTR);
    return error;
}

static int read_ctx_t__(ei_socket_callbacks *cbs, void *ctx,
                        char* buf, ssize_t *len, unsigned  ms)
{
    int error;

    if (!(cbs->flags & EI_SCLBK_FLG_FULL_IMPL) && ms != EI_SCLBK_INF_TMO) {
        int fd;

        error = EI_GET_FD__(cbs, ctx, &fd);
        if (error)
            return error;
        
        do {
            fd_set readmask;
            struct timeval tv;
            
            tv.tv_sec = (time_t) (ms / 1000U);
            ms %= 1000U;
            tv.tv_usec = (time_t) (ms * 1000U);
            FD_ZERO(&readmask);
            FD_SET(fd,&readmask);
            switch (select(fd+1, &readmask, NULL, NULL, &tv)) {
            case -1 :
                error = get_error();
                if (error != EINTR)
                    return error;
                break;
            case 0:
                return ETIMEDOUT; /* timeout */
            default:
                if (!FD_ISSET(fd, &readmask)) {
                    return EIO; /* Other error */
                }
                error = 0;
                break;
            }
        } while (error == EINTR);
    }
    do {
        error = cbs->read(ctx, buf, len, ms);
    } while (error == EINTR);
    return error;
}

static int write_ctx_t__(ei_socket_callbacks *cbs, void *ctx, const char* buf, ssize_t *len, unsigned  ms)
{
    int error;
    
    if (!(cbs->flags & EI_SCLBK_FLG_FULL_IMPL) && ms != EI_SCLBK_INF_TMO) {
        int fd;

        error = EI_GET_FD__(cbs, ctx, &fd);
        if (error)
            return error;

        do {
            fd_set writemask;
            struct timeval tv;
            
            tv.tv_sec = (time_t) (ms / 1000U);
            ms %= 1000U;
            tv.tv_usec = (time_t) (ms * 1000U);
            FD_ZERO(&writemask);
            FD_SET(fd,&writemask);
            switch (select(fd+1, NULL, &writemask, NULL, &tv)) {
            case -1 : 
                error = get_error();
                if (error != EINTR)
                    return error;
                break;
            case 0:
                return ETIMEDOUT; /* timeout */
            default:
                if (!FD_ISSET(fd, &writemask)) {
                    return EIO; /* Other error */
                }
                error = 0;
                break;
            }
        } while (error == EINTR);
    }
    do {
        error = cbs->write(ctx, buf, len, ms);
    } while (error == EINTR);
    return error;
}
	
/* 
 * Fill buffer, return buffer length, 0 for EOF, < 0 (and sets errno)
 * for error.  */
int ei_read_fill_ctx_t__(ei_socket_callbacks *cbs, void *ctx, char* buf, ssize_t *len, unsigned  ms)
{
    ssize_t got = 0;
    ssize_t want = *len;

    do {
        ssize_t read_len = want-got;
        int error;

        do {
            error = read_ctx_t__(cbs, ctx, buf+got, &read_len, ms);
        } while (error == EINTR);
        if (error)
            return error;
        if (read_len == 0) {
            *len = got;
            return 0;
        }
	got += read_len;
    } while (got < want);

    *len = got;
    return 0;
} /* read_fill */

int ei_read_fill_ctx__(ei_socket_callbacks *cbs, void *ctx, char* buf, ssize_t *len)
{
    return ei_read_fill_ctx_t__(cbs, ctx, buf, len, 0);
}

/* write entire buffer on fd  or fail (setting errno)
 */
int ei_write_fill_ctx_t__(ei_socket_callbacks *cbs, void *ctx, const char *buf, ssize_t *len, unsigned ms)
{
    ssize_t tot = *len, done = 0;
    int error, fd = -1, basic = !(cbs->flags & EI_SCLBK_FLG_FULL_IMPL);
    
    if (basic && ms != 0U) {
        error = EI_GET_FD__(cbs, ctx, &fd);
        if (error)
            return error;
	SET_NONBLOCKING(fd);
    }    
    do {
        ssize_t write_len = tot-done;
	error = write_ctx_t__(cbs, ctx, buf+done, &write_len, ms);
        if (error) {
            *len = done;
	    if (basic && ms != 0U) {
		SET_BLOCKING(fd);
	    }    
	    return error;
	}
	done += write_len;
    } while (done < tot);
    if (basic && ms != 0U) {
	SET_BLOCKING(fd);
    }
    *len = done;
    return 0;
}

int ei_write_fill_ctx__(ei_socket_callbacks *cbs, void *ctx, const char *buf, ssize_t *len) 
{
    return ei_write_fill_ctx_t__(cbs, ctx, buf, len, 0);
}

/*
 * Internal API for TCP/IPv4
 */

int ei_connect_t__(int fd, void *addr, int len, unsigned ms)
{
    return ei_connect_ctx_t__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd),
                              addr, len, ms);
}

int ei_socket__(int *fd)
{
    void *ctx;
    int error = ei_socket_ctx__(&ei_default_socket_callbacks, &ctx, NULL);
    if (error)
        return error;
    return EI_GET_FD__(&ei_default_socket_callbacks, ctx, fd);
}

int ei_close__(int fd)
{
    return ei_close_ctx__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd));
}

int ei_listen__(int fd, void *adr, int *len, int backlog)
{
    return ei_listen_ctx__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd),
                           adr, len, backlog);
}

int ei_accept_t__(int *fd, void *addr, int *len, unsigned ms)
{
    void *ctx = EI_FD_AS_CTX__(*fd);
    int error = ei_accept_ctx_t__(&ei_default_socket_callbacks, &ctx,
                                  addr, len, ms);
    if (error)
        return error;
    return EI_GET_FD__(&ei_default_socket_callbacks, ctx, fd);
}

int ei_read_fill_t__(int fd, char* buf, ssize_t *len, unsigned  ms)
{
    return ei_read_fill_ctx_t__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd),
                                buf, len, ms);
}

int ei_read_fill__(int fd, char* buf, ssize_t *len)
{
    return ei_read_fill_ctx_t__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd),
                                buf, len, 0);
}

int ei_write_fill_t__(int fd, const char *buf, ssize_t *len, unsigned ms)
{
    return ei_write_fill_ctx_t__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd),
                                 buf, len, ms);
}

int ei_write_fill__(int fd, const char *buf, ssize_t *len) 
{
    return ei_write_fill_ctx_t__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd),
                                 buf, len, 0);
}

#if defined(EI_HAVE_STRUCT_IOVEC__) && defined(HAVE_WRITEV)

int ei_writev_fill_t__(int fd, const struct iovec  *iov,  int iovcnt, ssize_t *len, unsigned ms)
{
    return ei_writev_fill_ctx_t__(&ei_default_socket_callbacks, EI_FD_AS_CTX__(fd),
                                  iov, iovcnt, len, ms);
}

#endif

