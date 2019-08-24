/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
