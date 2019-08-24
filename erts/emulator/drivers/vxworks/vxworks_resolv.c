/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
 * Interface functions to different versions of gethostbyname.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "netdb.h"
#include "erl_resolv.h"

struct hostent *erl_gethostbyname(name)
     char *name;
{
    return gethostbyname(name);
}

struct hostent *erl_gethostbyaddr(addr, len, type)
     char *addr;
     int len;
     int type;
{
    return gethostbyaddr(addr, len, type);
}


