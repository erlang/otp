/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2018. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#if defined(__sun__) && !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE 500
#endif

#include <limits.h>

#include <sys/types.h>
#include <sys/socket.h>

#ifdef HAVE_SYS_SOCKETIO_H
#   include <sys/socketio.h>
#endif
#ifdef HAVE_SYS_SOCKIO_H
#   include <sys/sockio.h>
#endif

#ifdef HAVE_NET_ERRNO_H
#include <net/errno.h>
#endif

#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif

#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "sys_uds.h"

int
sys_uds_readv(int fd, struct iovec *iov, size_t iov_len,
             int *fds, int fd_count, int flags) {
    struct msghdr msg;
    struct cmsghdr *cmsg = NULL;
    char ancillary_buff[256] = {0};
    int res, i = 0;

    /* setup a place to fill in message contents */
    memset(&msg, 0, sizeof(struct msghdr));
    msg.msg_iov = iov;
    msg.msg_iovlen = iov_len;

    /* provide space for the ancillary data */
    msg.msg_control = ancillary_buff;
    msg.msg_controllen = sizeof(ancillary_buff);

    if((res = recvmsg(fd, &msg, flags)) < 0) {
#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
        /* When some OS X versions run out of fd's
           they give EMSGSIZE instead of EMFILE.
           We remap this as we want the correct
           error to appear for the user */
        if (errno == EMSGSIZE)
            errno = EMFILE;
#endif
        return res;
    }

    if((msg.msg_flags & MSG_CTRUNC) == MSG_CTRUNC)
    {
        /* We assume that we have given enough space for any header
           that are sent to us. So the only remaining reasons to get
           this flag set is if the caller has run out of file descriptors
           or an SELinux policy prunes the response (eg. O_APPEND on STDERR).
        */
        errno = EMFILE;
        return -1;
    }

    for (cmsg = CMSG_FIRSTHDR(&msg); cmsg; cmsg = CMSG_NXTHDR(&msg, cmsg) ) {
        if ((cmsg->cmsg_level == SOL_SOCKET) &&
            (cmsg->cmsg_type == SCM_RIGHTS)) {
                int *cmsg_data = (int *)CMSG_DATA(cmsg);
                while ((char*)cmsg_data < (char*)cmsg + cmsg->cmsg_len) {
                    if (i < fd_count) {
                        fds[i++] = *cmsg_data++;
                    } else {
                        /* for some strange reason, we have received more FD's
                           than we wanted... close them if we are not running
                           debug. */
                        if(i >= fd_count) abort();
                        close(*cmsg_data++);
                    }
                }
            }
    }

    return res;
}

int
sys_uds_read(int fd, char *buff, size_t len,
             int *fds, int fd_count, int flags) {
    struct iovec iov;
    iov.iov_base = buff;
    iov.iov_len = len;
    return sys_uds_readv(fd, &iov, 1, fds, fd_count, flags);
}


int
sys_uds_writev(int fd, struct iovec *iov, size_t iov_len,
               int *fds, int fd_count, int flags) {

    struct msghdr msg;
    struct cmsghdr *cmsg = NULL;
    int res, i, error;

    /* initialize socket message */
    memset(&msg, 0, sizeof(struct msghdr));

    /* We flatten the iov if it is too long */
    if (iov_len > MAXIOV) {
        int size = 0;
        char *buff;
        for (i = 0; i < iov_len; i++)
            size += iov[i].iov_len;
        buff = malloc(size);

        for (i = 0; i < iov_len; i++) {
            memcpy(buff, iov[i].iov_base, iov[i].iov_len);
            buff += iov[i].iov_len;
        }

        iov[0].iov_base = buff - size;
        iov[0].iov_len = size;
        msg.msg_iov = iov;
        msg.msg_iovlen = 1;
    } else {
        msg.msg_iov = iov;
        msg.msg_iovlen = iov_len;
    }

    /* initialize the ancillary data */
    msg.msg_control = calloc(1, CMSG_SPACE(sizeof(int) * fd_count));
    msg.msg_controllen = CMSG_SPACE(sizeof(int) * fd_count);

    /* copy the fd array into the ancillary data */
    cmsg = CMSG_FIRSTHDR(&msg);
    if(!cmsg) abort();
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    cmsg->cmsg_len = CMSG_LEN(sizeof(int) * fd_count);
    memcpy(CMSG_DATA(cmsg), fds, sizeof(int) * fd_count);

    res = sendmsg(fd, &msg, flags);

#ifdef ETOOMANYREFS
    /* Linux may give ETOOMANYREFS when there are too many fds in transit.
       We map this to EMFILE as bsd and other use this error code and we want
       the behaviour to be the same on all OSs */
    if (errno == ETOOMANYREFS)
        errno = EMFILE;
#endif
    error = errno;

    if (iov_len > MAXIOV)
        free(iov[0].iov_base);

    free(msg.msg_control);

    errno = error;

    return res;
}

int
sys_uds_write(int fd, char *buff, size_t len,
              int *fds, int fd_count, int flags) {
    struct iovec iov;
    iov.iov_base = buff;
    iov.iov_len = len;
    return sys_uds_writev(fd, &iov, 1, fds, fd_count, flags);
}
