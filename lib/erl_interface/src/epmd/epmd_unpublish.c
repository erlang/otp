/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif  VXWORKS
#include <vxWorks.h>
#include <ifLib.h>
#include <sockLib.h>
#include <inetLib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#endif

#include <stdlib.h>
#include <string.h>

#include "eidef.h"
#include "ei_internal.h"
#include "putget.h"
#include "ei_epmd.h"
#include "ei_portio.h"


/* stop the specified node */
int ei_unpublish_tmo(const char *alive, unsigned ms)
{
    char buf[EPMDBUF];
    char *s = (char*)buf;
    int len = 1 + strlen(alive);
    int fd, res;

    if (len > sizeof(buf)-3) {
	erl_errno = ERANGE;
	return -1;
    }

    put16be(s,len);
    put8(s,EI_EPMD_STOP_REQ);
    strcpy(s, alive);

    /* FIXME can't connect, return success?! At least commen whats up */
    if ((fd = ei_epmd_connect_tmo(NULL,ms)) < 0) return fd;

    if ((res = ei_write_fill_t(fd, buf, len+2,ms)) != len+2) {
	closesocket(fd);
	erl_errno = (res == -2) ? ETIMEDOUT : EIO;
	return -1;
    }

    EI_TRACE_CONN1("ei_unpublish_tmo","-> STOP %s",alive);
  
    if ((res = ei_read_fill_t(fd, buf, 7, ms)) != 7) {
	closesocket(fd);
	erl_errno = (res == -2) ? ETIMEDOUT : EIO;
	return -1; 
    }
    closesocket(fd);
    buf[7]=(char)0;		/* terminate the string */
  
    if (!strcmp("STOPPED",(char *)buf)) {
	EI_TRACE_CONN0("ei_unpublish_tmo","<- STOPPED (success)");
	return 0;
    }
    else if (!strcmp("NOEXIST",(char *)buf)) {
	EI_TRACE_ERR0("ei_unpublish_tmo","<- NOEXIST (failure)");
	erl_errno = EIO;
	return -1;
    }
    else {
	EI_TRACE_ERR0("ei_unpublish_tmo","<- unknown (failure)");
	erl_errno = EIO;
	return -1;		/* this shouldn't happen */
    }
    return 0;
}


int ei_unpublish(ei_cnode* ec)
{
    return ei_unpublish_tmo(ei_thisalivename(ec),0);
}
