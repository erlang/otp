/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

#ifndef _ERL_UNIX_UDS_H
#define _ERL_UNIX_UDS_H

#include <sys/uio.h>

#if defined IOV_MAX
#define MAXIOV IOV_MAX
#elif defined UIO_MAXIOV
#define MAXIOV UIO_MAXIOV
#else
#define MAXIOV 16
#endif

int sys_uds_readv(int fd, struct iovec *iov, size_t iov_len,
                  int *fds, int fd_count, int flags);
int sys_uds_read(int fd, char *buff, size_t len,
                 int *fds, int fd_count, int flags);
int sys_uds_writev(int fd, struct iovec *iov, size_t iov_len,
                   int *fds, int fd_count, int flags);
int sys_uds_write(int fd, char *buff, size_t len,
                  int *fds, int fd_count, int flags);

#endif /* #ifndef _ERL_UNIX_UDS_H */
