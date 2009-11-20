/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
#ifndef _EI_EPMD_H
#define _EI_EPMD_H

#ifndef INADDR_LOOPBACK  
#define INADDR_LOOPBACK ((u_long) 0x7F000001)
#endif

#ifndef EI_DIST_HIGH
#define EI_DIST_HIGH 5 /* R4 and later */
#define EI_DIST_LOW  1 /* R3 and earlier */
#endif

#ifndef EPMD_PORT
#define EPMD_PORT 4369
#endif

#ifndef EPMDBUF
#define EPMDBUF 512
#endif

#ifndef EI_MYPROTO
#define EI_MYPROTO 0 /* tcp/ip */
#endif

/* epmd r3 protocol */
#ifndef EI_EPMD_ALIVE_REQ
#define EI_EPMD_ALIVE_REQ     'a'
#define EI_EPMD_ALIVE_OK_RESP 'Y'
#define EI_EPMD_PORT_REQ      'p'
#define EI_EPMD_STOP_REQ      's'
#endif

/* epmd r4 */
#ifndef EI_EPMD_ALIVE2_REQ
#define EI_EPMD_ALIVE2_REQ  120
#define EI_EPMD_ALIVE2_RESP 121
#define EI_EPMD_PORT2_REQ   122
#define EI_EPMD_PORT2_RESP  119
#endif

/* internal functions */
int ei_epmd_connect_tmo(struct in_addr *inaddr, unsigned ms);
int ei_epmd_publish(int port, const char *alive);
int ei_epmd_publish_tmo(int port, const char *alive, unsigned ms);
int ei_epmd_port(struct in_addr *inaddr, const char *alive, int *dist);
int ei_epmd_port_tmo(struct in_addr *inaddr, const char *alive, int *dist, unsigned ms);

#endif /* _EI_EPMD_H */
