/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <vxWorks.h>
#include <timers.h>
#ifdef NETDB
#include <netdb.h>
#endif
#include <sys/socket.h>
#include <netinet/in.h>

/*
   rdate("host") - Set the time from "host".
*/

/* No getservbyname() available... */
#define TIME_PORT 37

rdate(host)
char *host;
{
    u_long haddr;
#ifdef NETDB
    struct hostent *hp;
#endif
    struct sockaddr_in saddr;
    int sock;
    u_long net_time;
    struct timespec t;

    if ((haddr = inet_addr(host)) == ERROR) {
#ifdef NETDB
	if ((hp = gethostbyname(host)) == NULL) {
#else
	if ((haddr = hostGetByName(host)) == ERROR) {
#endif
	    printf("Host not found.\n");
	    return(-1);
	}
#ifdef NETDB
	memcpy(&haddr, hp->h_addr, sizeof(haddr));
#endif
    }
    memset(&saddr, 0, sizeof(saddr));
    saddr.sin_family = AF_INET;
    memcpy(&saddr.sin_addr, &haddr, sizeof(haddr));
    saddr.sin_port = htons(TIME_PORT);
    if ((sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0) {
	perror("socket");
	return(-1);
    }
    if (connect(sock, (struct sockaddr *)&saddr, sizeof(saddr)) < 0) {
	perror("connect");
	close(sock);
	return(-1);
    }
    if (read(sock, &net_time, 4) != 4) {
	perror("read");
	close(sock);
	return(-1);
    }
    t.tv_sec = ntohl(net_time);
    t.tv_sec -= 2208988800;	/* seconds 1900-01-01 -- 1970-01-01 */
    t.tv_nsec = 0;
    clock_settime(CLOCK_REALTIME, &t);
    close(sock);
    return(0);
}
