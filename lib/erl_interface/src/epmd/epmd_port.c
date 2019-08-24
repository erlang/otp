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

#include "eidef.h"

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

#include "ei.h"
#include "ei_internal.h"
#include "ei_epmd.h"
#include "ei_portio.h"
#include "putget.h"


/* connect to epmd on given host (use NULL for localhost) */
/* 
 * FIXME: Expects IPv4 addresses (excludes IPv6, Appletalk, IRDA and
 * whatever) */
int ei_epmd_connect_tmo(struct in_addr *inaddr, unsigned ms)
{
  static unsigned int epmd_port = 0;
  int port, sd, err;
  struct in_addr ip_addr;
  struct sockaddr_in addr;
  unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;

  err = ei_socket__(&sd);
  if (err) {
      erl_errno = err;
      return -1;
  }

  if (epmd_port == 0) {
      char* port_str = getenv("ERL_EPMD_PORT");
      epmd_port = (port_str != NULL) ? atoi(port_str) : EPMD_PORT;
  }

  port = (int) epmd_port;

  if (!inaddr) {      
      ip_addr.s_addr = htonl(INADDR_LOOPBACK);
      inaddr = &ip_addr;
  }
  
  memset((void *) &addr, 0, sizeof(struct sockaddr_in));
  memcpy((void *) &addr.sin_addr, (void *) inaddr, sizeof(addr.sin_addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);

  err = ei_connect_t__(sd, (void *) &addr, sizeof(addr), tmo);
  if (err) {
      erl_errno = err;
      ei_close__(sd);
      return -1;
  }

  return sd;
}

static int ei_epmd_r4_port (struct in_addr *addr, const char *alive,
			    int *dist, unsigned ms)
{
  char buf[EPMDBUF];
  char *s = buf;
  int len = strlen(alive) + 1;
  int fd;
  int ntype;
  int port;
  int dist_high, dist_low, proto;
  int res;
  int err;
  ssize_t dlen;
  unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;
#if defined(VXWORKS)
  char ntoabuf[32];
#endif

  if (len > sizeof(buf) - 3)
  {
      erl_errno = ERANGE;
      return -1;
  }
  
  put16be(s,len);
  put8(s,EI_EPMD_PORT2_REQ);
  strcpy(s,alive);
  
  /* connect to epmd */
  if ((fd = ei_epmd_connect_tmo(addr,ms)) < 0)
  {
      return -1;
  }

  dlen = len + 2;
  err = ei_write_fill_t__(fd, buf, &dlen, tmo);
  if (!err && dlen != (ssize_t) len + 2)
      erl_errno = EIO;
  if (err) {
      ei_close__(fd);
      EI_CONN_SAVE_ERRNO__(err);
      return -1;
  }

#ifdef VXWORKS
  /* FIXME use union/macro for level. Correct level? */
  if (ei_tracelevel > 2) {
    inet_ntoa_b(*addr,ntoabuf);
    EI_TRACE_CONN2("ei_epmd_r4_port",
		   "-> PORT2_REQ alive=%s ip=%s",alive,ntoabuf);
  }
#else
  EI_TRACE_CONN2("ei_epmd_r4_port",
		 "-> PORT2_REQ alive=%s ip=%s",alive,inet_ntoa(*addr));
#endif

  dlen = (ssize_t) 2;
  err = ei_read_fill_t__(fd, buf, &dlen, tmo);
  if (!err && dlen != (ssize_t) 2)
      erl_errno = EIO;
  if (err) {
      EI_TRACE_ERR0("ei_epmd_r4_port","<- CLOSE");
      ei_close__(fd);
      EI_CONN_SAVE_ERRNO__(err);
      return -2;
  }

  s = buf;
  res = get8(s);
  
  if (res != EI_EPMD_PORT2_RESP) { /* response type */
    EI_TRACE_ERR1("ei_epmd_r4_port","<- unknown (%d)",res);
    EI_TRACE_ERR0("ei_epmd_r4_port","-> CLOSE");
    ei_close__(fd);
    erl_errno = EIO;
    return -1;
  }

  

  /* got negative response */
  if ((res = get8(s))) {
    /* got negative response */
    EI_TRACE_ERR1("ei_epmd_r4_port","<- PORT2_RESP result=%d (failure)",res);
    ei_close__(fd);
    erl_errno = EIO;
    return -1;
  }

  EI_TRACE_CONN1("ei_epmd_r4_port","<- PORT2_RESP result=%d (ok)",res);

  /* expecting remaining 8 bytes */
  dlen = (ssize_t) 8;
  err = ei_read_fill_t__(fd, buf, &dlen, tmo);
  if (!err && dlen != (ssize_t) 8)
      err = EIO;
  if (err) {
    EI_TRACE_ERR0("ei_epmd_r4_port","<- CLOSE");
    ei_close__(fd);
    EI_CONN_SAVE_ERRNO__(err);
    return -1;
  }
  
  ei_close__(fd);
  s = buf;

  port = get16be(s);
  ntype = get8(s); 
  proto = get8(s);
  dist_high = get16be(s);
  dist_low = get16be(s);
  
  EI_TRACE_CONN5("ei_epmd_r4_port",
		"   port=%d ntype=%d proto=%d dist-high=%d dist-low=%d",
		port,ntype,proto,dist_high,dist_low);

  /* right network protocol? */
  if (EI_MYPROTO != proto)
  {
      erl_errno = EIO;
      return -1;
  }

  /* is there overlap in our distribution versions? */
  if ((EI_DIST_HIGH < dist_low) || (EI_DIST_LOW > dist_high)) 
  {
      erl_errno = EIO;
      return -1;
  }

  /* choose the highest common version */
  /* i.e. min(his-max, my-max) */
  *dist = (dist_high > EI_DIST_HIGH ? EI_DIST_HIGH : dist_high);
    
  /* ignore the remaining fields */
  return port;
}

/* lookup the port number of the given node. 'dist' is an out-argument
 * which, if the lookup is successful, will be initialized to contain
 * the highest distribution version that is common to the calling node
 * and the node looked up. The function will attempt to contact epmd
 * version 4 before trying version 3. R3 (and earlier) nodes have
 * dist=0.
 */
int ei_epmd_port (struct in_addr *addr, const char *alive, int *dist)
{
    return ei_epmd_port_tmo (addr, alive, dist, 0);
}

int ei_epmd_port_tmo (struct in_addr *addr, const char *alive, int *dist, unsigned ms)
{
    return ei_epmd_r4_port(addr,alive,dist,ms);
}

