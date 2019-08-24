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
* Purpose: Connect to any node at any host. (EI version)
*/

/* FIXME far to many includes here... */

/* some send() functions use buffer on heap for "small" messages */
/* messages larger than this require call to malloc() */

#ifndef _EI_CONNECT_H
#define _EI_CONNECT_H

#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <vxWorks.h>
#include <hostLib.h>
#include <selectLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>
#include <ioLib.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <timers.h> 

#define getpid() taskIdSelf()
extern int h_errno;

#else /* some other unix */
#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/utsname.h>  /* for gen_challenge (NEED FIX?) */
#endif

/* FIXME remove duplicate defintions */

#define DEFBUF_SIZ 100

/* rpc_from() uses a buffer this size */
#ifndef MAX_RECEIVE_BUF
# define MAX_RECEIVE_BUF 32*1024
#endif

/* Distribution capability flags */
#define DFLAG_PUBLISHED           1
#define DFLAG_ATOM_CACHE          2
#define DFLAG_EXTENDED_REFERENCES 4
#define DFLAG_DIST_MONITOR        8
#define DFLAG_FUN_TAGS            16
#define DFLAG_NEW_FUN_TAGS        0x80
#define DFLAG_EXTENDED_PIDS_PORTS 0x100
#define DFLAG_EXPORT_PTR_TAG      0x200
#define DFLAG_BIT_BINARIES        0x400
#define DFLAG_NEW_FLOATS          0x800
#define DFLAG_SMALL_ATOM_TAGS     0x4000
#define DFLAG_UTF8_ATOMS          0x10000
#define DFLAG_MAP_TAG             0x20000
#define DFLAG_BIG_CREATION        0x40000

ei_cnode   *ei_fd_to_cnode(int fd);
int         ei_distversion(int fd);
const char* ei_getfdcookie(int fd);
short       ei_thiscreation(const ei_cnode* ec);
const char *ei_thiscookie(const ei_cnode* ec);

int ei_do_receive_msg(int fd, int staticbuffer_p, 
		      erlang_msg* msg, ei_x_buff* x, unsigned ms);

#endif /* _EI_CONNECT_H */
