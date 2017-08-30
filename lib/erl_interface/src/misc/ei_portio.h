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
#if !defined(__WIN32__) || !defined(VXWORKS)
#ifdef HAVE_WRITEV
/* Declaration of struct iovec *iov should be visible in this scope. */
#include <sys/uio.h>
#endif
#endif

int ei_accept_t(int fd, void   *addr,   void  *addrlen, unsigned ms);
int ei_connect_t(int fd, void *sinp, int sin_siz, unsigned ms);
int ei_read_fill(int fd, char* buf, int len);
int ei_write_fill(int fd, const char *buf, int len);
int ei_read_fill_t(int fd, char* buf, int len, unsigned ms);
int ei_write_fill_t(int fd, const char *buf, int len, unsigned ms);
#ifdef HAVE_WRITEV
int ei_writev_fill_t(int fd,  const  struct  iovec  *iov,  int iovcnt, unsigned ms);
#endif

#endif /* _EI_PORTIO_H */
