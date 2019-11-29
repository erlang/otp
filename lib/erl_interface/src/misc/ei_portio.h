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
#ifndef _EI_PORTIO_H
#define _EI_PORTIO_H

#undef EI_HAVE_STRUCT_IOVEC__
#if !defined(__WIN32__) && !defined(VXWORKS) && defined(HAVE_SYS_UIO_H)
/* Declaration of struct iovec *iov should be visible in this scope. */
#  include <sys/uio.h>
#  define EI_HAVE_STRUCT_IOVEC__
#endif

/*
 * Internal API. Should not be used outside of the erl_interface application...
 */

int ei_socket_ctx__(ei_socket_callbacks *cbs, void **ctx, void *setup);
int ei_close_ctx__(ei_socket_callbacks *cbs, void *ctx);
int ei_listen_ctx__(ei_socket_callbacks *cbs, void *ctx, void *adr, int *len, int backlog);
int ei_accept_ctx_t__(ei_socket_callbacks *cbs, void **ctx, void *addr, int *len, unsigned ms);
int ei_connect_ctx_t__(ei_socket_callbacks *cbs, void *ctx, void *addr, int len, unsigned ms);
int ei_read_fill_ctx__(ei_socket_callbacks *cbs, void *ctx, char* buf, ssize_t *len);
int ei_write_fill_ctx__(ei_socket_callbacks *cbs, void *ctx, const char *buf, ssize_t *len);
int ei_read_fill_ctx_t__(ei_socket_callbacks *cbs, void *ctx, char* buf, ssize_t *len, unsigned  ms);
int ei_write_fill_ctx_t__(ei_socket_callbacks *cbs, void *ctx, const char *buf, ssize_t *len, unsigned ms);
#if defined(EI_HAVE_STRUCT_IOVEC__)
int ei_writev_fill_ctx_t__(ei_socket_callbacks *cbs, void *ctx, const struct iovec *iov, int iovcnt, ssize_t *len, unsigned ms);
int ei_socket_callbacks_have_writev__(ei_socket_callbacks *cbs);
#endif

ei_socket_callbacks ei_default_socket_callbacks;

#define EI_FD_AS_CTX__(FD)                                              \
    ((void *) (long) (FD))

#define EI_DFLT_CTX_TO_FD__(CTX, FD)                                    \
    ((int) (long) (CTX) < 0                                             \
     ? EBADF                                                            \
     : (*(FD) = (int) (long) (CTX), 0))

#define EI_GET_FD__(CBS, CTX, FD)                                       \
    ((CBS) == &ei_default_socket_callbacks                              \
     ? EI_DFLT_CTX_TO_FD__((CTX), FD)                                   \
     : (CBS)->get_fd((CTX), (FD)))

extern int ei_plugin_socket_impl__;

#if !defined(_REENTRANT)

#define EI_HAVE_PLUGIN_SOCKET_IMPL__                                    \
    ei_plugin_socket_impl__
#define EI_SET_HAVE_PLUGIN_SOCKET_IMPL__                                \
    ei_plugin_socket_impl__ = 1

#elif ((ETHR_HAVE___atomic_load_n & SIZEOF_INT)                         \
       && (ETHR_HAVE___atomic_store_n & SIZEOF_INT))

#define EI_HAVE_PLUGIN_SOCKET_IMPL__                                    \
    __atomic_load_n(&ei_plugin_socket_impl__, __ATOMIC_ACQUIRE)
#define EI_SET_HAVE_PLUGIN_SOCKET_IMPL__                                \
    __atomic_store_n(&ei_plugin_socket_impl__, 1, __ATOMIC_RELEASE)

#else

/* No gcc atomics; always lookup using ei_get_cbs_ctx()... */
#define EI_HAVE_PLUGIN_SOCKET_IMPL__ 1
#define EI_SET_HAVE_PLUGIN_SOCKET_IMPL__ (void) 0

#endif

#define EI_GET_CBS_CTX__(CBS, CTX, FD)                                  \
    (EI_HAVE_PLUGIN_SOCKET_IMPL__                                       \
     ? ei_get_cbs_ctx__((CBS), (CTX), (FD))                             \
     : ((FD) < 0                                                        \
        ? EBADF                                                         \
        : (*(CBS) = &ei_default_socket_callbacks,                       \
           *(CTX) = EI_FD_AS_CTX__((FD)),                               \
           0)))
/*      
 * The following uses our own TCP/IPv4 socket implementation...
 */
int ei_socket__(int *fd);
int ei_close__(int fd);
int ei_listen__(int fd, void *adr, int *len, int backlog);
int ei_accept_t__(int *fd, void *addr, int *len, unsigned ms);
int ei_connect_t__(int fd, void *addr, int len, unsigned ms);
int ei_read_fill__(int fd, char* buf, ssize_t *len);
int ei_write_fill__(int fd, const char *buf, ssize_t *len);
int ei_read_fill_t__(int fd, char* buf, ssize_t *len, unsigned ms);
int ei_write_fill_t__(int fd, const char *buf, ssize_t *len, unsigned ms);
#if defined(EI_HAVE_STRUCT_IOVEC__) && defined(HAVE_WRITEV)
int ei_writev_fill_t__(int fd, const struct iovec *iov, int iovcnt, ssize_t *len, unsigned ms);
#endif

#endif /* _EI_PORTIO_H */
