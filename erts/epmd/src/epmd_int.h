/* -*- c-indent-level: 2; c-continued-statement-offset: 2 -*- */
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
/* 
 * This file is for internal use within epmd.
 */

/* This file don't depend on "sys.h" so we have to do some target
   definitions ourselves */

#ifdef __WIN32__
#define NO_SYSCONF
#define NO_DAEMON
#endif

#ifdef VXWORKS
#define NO_SYSCONF
#define NO_DAEMON
#define NO_FCNTL
#define DONT_USE_MAIN
#endif

/* ************************************************************************ */
/* Standard includes                                                        */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __WIN32__
#  ifndef WINDOWS_H_INCLUDES_WINSOCK2_H
#    include <winsock2.h>
#  endif
#  include <ws2tcpip.h>
#  include <windows.h>
#  include <process.h>
#endif

#include <sys/types.h>
#include <fcntl.h>

#ifdef VXWORKS
#  include <sys/times.h>
#  include <time.h>
#  include <selectLib.h>
#  include <sysLib.h>
#  include <sockLib.h>
#  include <ioLib.h>
#  include <taskLib.h>
#  include <rpc/rpc.h>
#else /* ! VXWORKS */
#ifndef __WIN32__
#  ifdef TIME_WITH_SYS_TIME
#    include <sys/time.h>
#    include <time.h>
#  else
#    ifdef HAVE_SYS_TIME_H
#       include <sys/time.h>
#    else
#       include <time.h>
#    endif
#  endif
#endif
#endif /* ! VXWORKS */

#if !defined(__WIN32__)
#  include <netinet/in.h>
#  include <sys/socket.h>
#  include <sys/stat.h>

#  ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#    include <rpc/types.h>
#  endif

#  include <arpa/inet.h>
#  include <netinet/tcp.h>
#endif /* ! WIN32 */

#include <ctype.h>
#include <signal.h>
#include <errno.h>

#ifdef HAVE_SYSLOG_H
#  include <syslog.h>
#endif

#ifdef SYS_SELECT_H
#  include <sys/select.h>
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include <stdarg.h>

#ifdef HAVE_SYSTEMD_DAEMON
#  include <systemd/sd-daemon.h>
#endif /* HAVE_SYSTEMD_DAEMON */

#if defined(HAVE_IN6) && defined(AF_INET6) && defined(HAVE_INET_PTON)
#  define EPMD6
#endif

/* ************************************************************************ */
/* Replace some functions by others by making the function name a macro */

#ifdef __WIN32__
#  define close(s) closesocket((s))
#  define write(a,b,c) send((a),(b),(c),0)
#  define read(a,b,c) recv((a),(char *)(b),(c),0)
#  define sleep(s) Sleep((s) * 1000)
#  define ioctl(s,r,o) ioctlsocket((s),(r),(o))
#endif /* WIN32 */

#ifdef VXWORKS
#define sleep(n) taskDelay((n) * sysClkRateGet())
#endif /* VXWORKS */

#ifdef USE_BCOPY
#  define memcpy(a, b, c) bcopy((b), (a), (c))
#  define memcmp(a, b, c) bcmp((a), (b), (c))
#  define memzero(buf, len) bzero((buf), (len))
#else
#  define memzero(buf, len) memset((buf), '\0', (len))
#endif

/* ************************************************************************ */
/* Try to find replacement values for undefined system parameters           */

#if defined(__WIN32__) && !defined(EADDRINUSE)
#  define EADDRINUSE WSAEADDRINUSE
#endif

#if defined(__WIN32__) && !defined(ECONNABORTED)
#  define ECONNABORTED WSAECONNABORTED
#endif

#ifndef SOMAXCONN
#  define SOMAXCONN 128
#endif

/*
 * How to get max no of file descriptors? We used to use NOFILE from
 * <sys/param.h>, but that tends to have little relation to reality.
 * Best is to use sysconf() (POSIX), but we'll just punt if that isn't
 * available. Start out with a high value because it will also be
 * used as the number of file descriptors given to select() (it's is
 * a terrible bug not to have all file descriptors included in the select()).
 * The value will be adjusted down if FD_SETSIZE is smaller.
 */

#define MAX_FILES 2048		/* if sysconf() isn't available, or fails */

/* ************************************************************************ */
/* Macros that let us use IPv6                                              */

#if HAVE_IN6
#  if ! defined(HAVE_IN6ADDR_ANY) || ! HAVE_IN6ADDR_ANY
#    if HAVE_DECL_IN6ADDR_ANY_INIT
static const struct in6_addr in6addr_any = { { IN6ADDR_ANY_INIT } };
#    else
static const struct in6_addr in6addr_any =
    { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 } } };
#    endif /* HAVE_IN6ADDR_ANY_INIT */
#  endif /* ! HAVE_DECL_IN6ADDR_ANY */

#  if ! defined(HAVE_IN6ADDR_LOOPBACK) || ! HAVE_IN6ADDR_LOOPBACK
#    if HAVE_DECL_IN6ADDR_LOOPBACK_INIT
static const struct in6_addr in6addr_loopback =
    { { IN6ADDR_LOOPBACK_INIT } };
#    else
static const struct in6_addr in6addr_loopback =
    { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 } } };
#    endif /* HAVE_IN6ADDR_LOOPBACK_INIT */
#  endif /* ! HAVE_DECL_IN6ADDR_LOOPBACK */
#endif /* HAVE_IN6 */

#define IS_ADDR_LOOPBACK(addr) ((addr).s_addr == htonl(INADDR_LOOPBACK))

#if defined(EPMD6)

#define EPMD_SOCKADDR_IN sockaddr_storage
#define FAMILY AF_INET6

#define SET_ADDR6(dst, addr, port) do { \
    struct sockaddr_in6 *sa = (struct sockaddr_in6 *)&(dst); \
    memset(sa, 0, sizeof(dst)); \
    sa->sin6_family = AF_INET6; \
    sa->sin6_addr = (addr); \
    sa->sin6_port = htons(port); \
 } while(0)

#define SET_ADDR(dst, addr, port) do { \
    struct sockaddr_in *sa = (struct sockaddr_in *)&(dst); \
    memset(sa, 0, sizeof(dst)); \
    sa->sin_family = AF_INET; \
    sa->sin_addr.s_addr = (addr); \
    sa->sin_port = htons(port); \
 } while(0)

#else /* Not IP v6 */

#define EPMD_SOCKADDR_IN sockaddr_in
#define FAMILY AF_INET

#define SET_ADDR(dst, addr, port) do { \
    memset((char*)&(dst), 0, sizeof(dst)); \
    (dst).sin_family = AF_INET; \
    (dst).sin_addr.s_addr = (addr); \
    (dst).sin_port = htons(port); \
 } while(0)

#endif /* Not IP v6 */

/* ************************************************************************ */
/* Our own definitions                                                      */

#define EPMD_FALSE 0
#define EPMD_TRUE 1

/* If no activity we let select() return every IDLE_TIMEOUT second
   A file descriptor that has been idle for CLOSE_TIMEOUT seconds and
   isn't an ALIVE socket has probably hanged and should be closed */

#define IDLE_TIMEOUT 5
#define CLOSE_TIMEOUT 60

/* We save the name of nodes that are unregistered. If a new
   node register the name we want to increment the "creation",
   a constant 1..3. But we put an limit to this saving to keep
   the lookup fast and not to leak memory. */

#define MAX_UNREG_COUNT 1000
#define DEBUG_MAX_UNREG_COUNT 5

/*
 * Maximum length of a node name == atom name
 *   255 characters; UTF-8 encoded -> max 255*4
 */
#define MAXSYMLEN (255*4)

#define MAX_LISTEN_SOCKETS 16

/*
 * Largest request: ALIVE2_REQ
 *  2 + 13 + 2*MAXSYMLEN
 * Largest response: PORT2_RESP
 *  2 + 14 + 2*MAXSYMLEN
 *
 * That is, 3*MAXSYMLEN should be large enough
 */

#define INBUF_SIZE (3*MAXSYMLEN)
#define OUTBUF_SIZE (3*MAXSYMLEN)

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))

#define put_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                        ((unsigned char*)(s))[1] = (i)         & 0xff;}

#if defined(__GNUC__)
#  define EPMD_INLINE __inline__
#elif defined(__WIN32__)
#  define EPMD_INLINE __inline
#else
#  define EPMD_INLINE
#endif

/* ************************************************************************ */

/* Stuctures used by server */

typedef struct {
  int fd;			/* File descriptor */
  unsigned char open;		/* TRUE if open */
  unsigned char keep;		/* Don't close when sent reply */
  unsigned char local_peer;     /* The peer of this connection is via
                                   loopback interface */
  unsigned got;			/* # of bytes we have got */
  unsigned want;		/* Number of bytes we want */
  char *buf;			/* The remaining buffer */

  time_t mod_time;		/* Last activity on this socket */
} Connection;

struct enode {
  struct enode *next;
  int fd;			/* The socket in use */
  unsigned short port;		/* Port number of Erlang node */
  char symname[MAXSYMLEN+1];	/* Name of the Erlang node */
  short creation;		/* Started as a random number 1..3 */
  char nodetype;                /* 77 = normal erlang node 72 = hidden (c-node */
  char protocol;                /* 0 = tcp/ipv4 */
  unsigned short highvsn;       /* 0 = OTP-R3 erts-4.6.x, 1 = OTP-R4 erts-4.7.x*/
  unsigned short lowvsn;
  int extralen;
  char extra[MAXSYMLEN+1];
};

typedef struct enode Node;

typedef struct {
  Node *reg;
  Node *unreg;
  Node *unreg_tail;
  int unreg_count;
} Nodes;


/* This is the structure with all variables needed to pass on
   to all functions. This makes this program reentrant */

typedef struct {
  int port;
  int debug;
  int silent; 
  int is_daemon;
  int brutal_kill;
  unsigned packet_timeout;
  unsigned delay_accept;
  unsigned delay_write;
  int max_conn;
  int active_conn;
  int select_fd_top;
  char *progname;
  Connection *conn;
  Nodes nodes;
  fd_set orig_read_mask;
  int listenfd[MAX_LISTEN_SOCKETS];
  char *addresses;
  char **argv;
#ifdef HAVE_SYSTEMD_DAEMON
  int is_systemd;
#endif /* HAVE_SYSTEMD_DAEMON */
} EpmdVars;

void dbg_printf(EpmdVars*,int,const char*,...);
void dbg_tty_printf(EpmdVars*,int,const char*,...);
void dbg_perror(EpmdVars*,const char*,...);
void kill_epmd(EpmdVars*);
void epmd_call(EpmdVars*,int);
void run(EpmdVars*);
void epmd_cleanup_exit(EpmdVars*, int);
int epmd_conn_close(EpmdVars*,Connection*);
void stop_cli(EpmdVars *g, char *name);

#ifdef DONT_USE_MAIN
int  start_epmd(char *,char *,char *,char *,char *,char *,char *,char *,char *,char *);
int  epmd(int,char **);
int  epmd_dbg(int,int);
#endif


