/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009. All Rights Reserved.
 * 
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
 * %CopyrightEnd%
 */

#ifndef ERL_ERRNO_H__
#define ERL_ERRNO_H__

#include <errno.h>
/*
 * Make sure that ENOTSUP is defined.
 */
#ifndef ENOTSUP
#  ifdef EOPNOTSUPP
#    define ENOTSUP EOPNOTSUPP
#else
#    define ENOTSUP INT_MAX
#  endif
#endif

#ifdef __WIN32__
#  ifndef EWOULDBLOCK
#    define EWOULDBLOCK (10035) /* WSAEWOULDBLOCK */
#  endif
#  ifndef ETIMEDOUT
#    define ETIMEDOUT (10060) /* WSAETIMEDOUT */
#  endif
#else
#  ifndef EWOULDBLOCK
#    define EWOULDBLOCK EAGAIN
#  endif
#  ifndef ETIMEDOUT
#    define ETIMEDOUT EAGAIN
#  endif
#endif

#endif
