/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2018. All Rights Reserved.
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

#include "eidef.h"

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

#include <unistd.h>
#include <sys/times.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <timers.h> 

#define getpid() taskIdSelf()

#else /* some other unix */
#include <unistd.h>
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
#include <time.h>
#endif

/* common includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stddef.h>

#include "eiext.h"
#include "ei_portio.h"
#include "ei_internal.h"
#include "ei_connect_int.h"
#include "ei_locking.h"
#include "eisend.h"
#include "eirecv.h"
#include "eimd5.h"
#include "putget.h"
#include "ei_resolve.h"
#include "ei_epmd.h"
#include "ei_internal.h"

static int ei_connect_initialized = 0;
int ei_tracelevel = 0;

#define COOKIE_FILE "/.erlang.cookie"
#define EI_MAX_HOME_PATH 1024

#define EI_SOCKET_CALLBACKS_SZ_V1                       \
    (offsetof(ei_socket_callbacks, get_fd)              \
     + sizeof(int (*)(void *)))

/* FIXME why not macro? */
static char *null_cookie = "";

static int get_cookie(char *buf, int len);
static int get_home(char *buf, int size);

/* forwards */
static unsigned gen_challenge(void);
static void gen_digest(unsigned challenge, char cookie[], 
		       unsigned char digest[16]);
static int send_status(ei_socket_callbacks *cbs, void *ctx,
                       int pkt_sz, char *status, unsigned ms);
static int recv_status(ei_socket_callbacks *cbs, void *ctx,
                       int pkt_sz, unsigned ms);
static int send_challenge(ei_socket_callbacks *cbs, void *ctx, int pkt_sz,
                          char *nodename, unsigned challenge,
                          unsigned version, unsigned ms);
static int recv_challenge(ei_socket_callbacks *cbs, void *ctx, int pkt_sz,
                          unsigned *challenge, unsigned *version,
			  unsigned *flags, char *namebuf, unsigned ms);
static int send_challenge_reply(ei_socket_callbacks *cbs, void *ctx,
                                int pkt_sz, unsigned char digest[16], 
				unsigned challenge, unsigned ms);
static int recv_challenge_reply(ei_socket_callbacks *cbs, void *ctx,
                                int pkt_sz, unsigned our_challenge,
				char cookie[], 
				unsigned *her_challenge, unsigned ms);
static int send_challenge_ack(ei_socket_callbacks *cbs, void *ctx,
                              int pkt_sz, unsigned char digest[16],
                              unsigned ms);
static int recv_challenge_ack(ei_socket_callbacks *cbs, void *ctx, 
			      int pkt_sz, unsigned our_challenge,
			      char cookie[], unsigned ms);
static int send_name(ei_socket_callbacks *cbs, void *ctx, int pkt_sz,
                     char *nodename, unsigned version, unsigned ms); 

static int recv_name(ei_socket_callbacks *cbs, void *ctx, int pkt_sz,
                     unsigned *version, unsigned *flags, char *namebuf,
                     unsigned ms);

static struct hostent*
dyn_gethostbyname_r(const char *name, struct hostent *hostp, char **buffer_p,
                    int buflen, int *h_errnop);

static void abort_connection(ei_socket_callbacks *cbs, void *ctx);
static int close_connection(ei_socket_callbacks *cbs, void *ctx, int fd);

static char *
estr(int e)
{
    char *str = strerror(e);
    if (!str)
        return "unknown error";
    return str;
}


/***************************************************************************
 *
 *  For each file descriptor returned from ei_connect() we save information
 *  about distribution protocol version, node information for this node
 *  and the cookie.
 *
 ***************************************************************************/

typedef struct ei_socket_info_s {
    int socket;
    ei_socket_callbacks *cbs;
    void *ctx;
    int dist_version;
    ei_cnode cnode;	/* A copy, not a pointer. We don't know when freed */
    char cookie[EI_MAX_COOKIE_SIZE+1];
} ei_socket_info;

/***************************************************************************
 *
 *  XXX
 *
 ***************************************************************************/

#ifndef ETHR_HAVE___atomic_compare_exchange_n
#  define ETHR_HAVE___atomic_compare_exchange_n 0
#endif
#ifndef ETHR_HAVE___atomic_load_n
#  define ETHR_HAVE___atomic_load_n 0
#endif
#ifndef ETHR_HAVE___atomic_store_n
#  define ETHR_HAVE___atomic_store_n 0
#endif

#if defined(_REENTRANT)                                                 \
    && (!(ETHR_HAVE___atomic_compare_exchange_n & SIZEOF_VOID_P)        \
        || !(ETHR_HAVE___atomic_load_n & SIZEOF_VOID_P)                 \
        || !(ETHR_HAVE___atomic_store_n & SIZEOF_VOID_P))
#  undef EI_DISABLE_SEQ_SOCKET_INFO
#  define EI_DISABLE_SEQ_SOCKET_INFO
#endif

#ifdef __WIN32__
#  undef EI_DISABLE_SEQ_SOCKET_INFO
#  define EI_DISABLE_SEQ_SOCKET_INFO
#endif

#ifndef EI_DISABLE_SEQ_SOCKET_INFO

#ifdef _REENTRANT

#define EI_ATOMIC_CMPXCHG_ACQ_REL(VARP, XCHGP, NEW) \
    __atomic_compare_exchange_n((VARP), (XCHGP), (NEW), 0, \
                                __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)
#define EI_ATOMIC_LOAD_ACQ(VARP) \
    __atomic_load_n((VARP), __ATOMIC_ACQUIRE)
#define EI_ATOMIC_STORE_REL(VARP, NEW) \
    __atomic_store_n((VARP), (NEW), __ATOMIC_RELEASE)

#else /* ! _REENTRANT */

#define EI_ATOMIC_CMPXCHG_ACQ_REL(VARP, XCHGP, NEW)    \
    (*(VARP) == *(XCHGP)                               \
     ? ((*(VARP) = (NEW)), !0)                         \
     : ((*(XCHGP) = *(VARP)), 0))
#define EI_ATOMIC_LOAD_ACQ(VARP) (*(VARP))
#define EI_ATOMIC_STORE_REL(VARP, NEW) (*(VARP) = (NEW))

#endif /* ! _REENTRANT */

#define EI_SOCKET_INFO_SEG_BITS 5
#define EI_SOCKET_INFO_SEG_SIZE (1 << EI_SOCKET_INFO_SEG_BITS)
#define EI_SOCKET_INFO_SEG_MASK (EI_SOCKET_INFO_SEG_SIZE - 1)

typedef struct {
    int max_fds;
    ei_socket_info *segments[1]; /* Larger in reality... */
} ei_socket_info_data__;

static ei_socket_info_data__ *socket_info_data = NULL;

static int init_socket_info(int late)
{
    int max_fds;
    int i;
    size_t segments_len;
    ei_socket_info_data__ *info_data, *xchg;

    if (EI_ATOMIC_LOAD_ACQ(&socket_info_data) != NULL)
        return 0; /* Already initialized... */
    
#if defined(HAVE_SYSCONF) && defined(_SC_OPEN_MAX)
    max_fds = sysconf(_SC_OPEN_MAX);
#else
    max_fds = 1024;
#endif

    if (max_fds < 0)
        return EIO;

    segments_len = ((max_fds-1)/EI_SOCKET_INFO_SEG_SIZE + 1);
    
    info_data = malloc(sizeof(ei_socket_info_data__)
                       + (sizeof(ei_socket_info *)*(segments_len-1)));
    if (!info_data)
        return ENOMEM;

    info_data->max_fds = max_fds;
    for (i = 0; i < segments_len; i++)
        info_data->segments[i] = NULL;

    xchg = NULL;
    if (!EI_ATOMIC_CMPXCHG_ACQ_REL(&socket_info_data, &xchg, info_data))
        free(info_data); /* Already initialized... */

    return 0;
}

static int put_ei_socket_info(int fd, int dist_version, char* cookie, ei_cnode *ec,
                              ei_socket_callbacks *cbs, void *ctx)
{
    int six;
    ei_socket_info *seg, *si;
    int socket;

    if (fd < 0 || socket_info_data->max_fds <= fd)
        return -1;

    socket = fd;
    six = fd >> EI_SOCKET_INFO_SEG_BITS;
    seg = EI_ATOMIC_LOAD_ACQ(&socket_info_data->segments[six]);
    
    if (!seg) {
        ei_socket_info *xchg;
        int i;
        seg = malloc(sizeof(ei_socket_info)*EI_SOCKET_INFO_SEG_SIZE);
        if (!seg)
            return -1;
        for (i = 0; i < EI_SOCKET_INFO_SEG_SIZE; i++) {
            seg[i].socket = -1;
        }

        xchg = NULL;
        if (!EI_ATOMIC_CMPXCHG_ACQ_REL(&socket_info_data->segments[six], &xchg, seg)) {
            free(seg);
            seg = xchg;
        }
    }

    si = &seg[fd & EI_SOCKET_INFO_SEG_MASK];

    if (dist_version < 0) {
        socket = -1;
        si->cbs = NULL;
        si->ctx = NULL;
    }
    else {
        si->dist_version = dist_version;
        si->cnode = *ec;
        si->cbs = cbs;
        si->ctx = ctx;
        strcpy(si->cookie, cookie);
    }

    EI_ATOMIC_STORE_REL(&si->socket, socket);

    return 0;
}

static ei_socket_info* get_ei_socket_info(int fd)
{
    int six, socket;
    ei_socket_info *seg, *si;
    
    if (fd < 0 || socket_info_data->max_fds <= fd)
        return NULL;
    
    six = fd >> EI_SOCKET_INFO_SEG_BITS;
    seg = EI_ATOMIC_LOAD_ACQ(&socket_info_data->segments[six]);

    if (!seg)
        return NULL;
    
    si = &seg[fd & EI_SOCKET_INFO_SEG_MASK];
    socket = EI_ATOMIC_LOAD_ACQ(&si->socket);
    if (socket != fd)
        return NULL;
    return si;
}
    
#else /* EI_DISABLE_SEQ_SOCKET_INFO */

int ei_n_sockets = 0, ei_sz_sockets = 0;
ei_socket_info *ei_sockets = NULL;

#ifdef _REENTRANT
ei_mutex_t* ei_sockets_lock = NULL;
#endif /* _REENTRANT */

static int init_socket_info(int late)
{
#ifdef _REENTRANT
    if (late)
        return ENOTSUP; /* Refuse doing unsafe initialization... */
    ei_sockets_lock = ei_mutex_create();
    if (!ei_sockets_lock)
        return ENOMEM;
#endif /* _REENTRANT */
    return 0;
}

static int put_ei_socket_info(int fd, int dist_version, char* cookie, ei_cnode *ec,
                              ei_socket_callbacks *cbs, void *ctx)
{
    int i;

#ifdef _REENTRANT
    ei_mutex_lock(ei_sockets_lock, 0);
#endif /* _REENTRANT */
    for (i = 0; i < ei_n_sockets; ++i) {
	if (ei_sockets[i].socket == fd) {
	    if (dist_version == -1) {
                memmove(&ei_sockets[i], &ei_sockets[i+1],
			sizeof(ei_sockets[0])*(ei_n_sockets-i-1));
	    } else {
		ei_sockets[i].dist_version = dist_version;
		/* Copy the content, see ei_socket_info */
                ei_sockets[i].cbs = cbs;
                ei_sockets[i].ctx = ctx;
		ei_sockets[i].cnode = *ec;
		strcpy(ei_sockets[i].cookie, cookie);
	    }
#ifdef _REENTRANT
	    ei_mutex_unlock(ei_sockets_lock);
#endif /* _REENTRANT */
	    return 0;
	}
    }
    if (ei_n_sockets == ei_sz_sockets) {
	ei_sz_sockets += 5;
	ei_sockets = realloc(ei_sockets,
			     sizeof(ei_sockets[0])*ei_sz_sockets);
	if (ei_sockets == NULL) {
	    ei_sz_sockets = ei_n_sockets = 0;
#ifdef _REENTRANT
	    ei_mutex_unlock(ei_sockets_lock);
#endif /* _REENTRANT */
	    return -1;
	}
    }
    ei_sockets[ei_n_sockets].socket = fd;
    ei_sockets[ei_n_sockets].dist_version = dist_version;
    ei_sockets[ei_n_sockets].cnode = *ec;
    ei_sockets[ei_n_sockets].cbs = cbs;
    ei_sockets[ei_n_sockets].ctx = ctx;
    strcpy(ei_sockets[ei_n_sockets].cookie, cookie);
    ++ei_n_sockets;
#ifdef _REENTRANT
    ei_mutex_unlock(ei_sockets_lock);
#endif /* _REENTRANT */
    return 0;
}

static ei_socket_info* get_ei_socket_info(int fd)
{
    int i;
#ifdef _REENTRANT
    ei_mutex_lock(ei_sockets_lock, 0);
#endif /* _REENTRANT */
    for (i = 0; i < ei_n_sockets; ++i)
	if (ei_sockets[i].socket == fd) {
	    /*fprintf("get_ei_socket_info %d  %d \"%s\"\n",
		    fd, ei_sockets[i].dist_version, ei_sockets[i].cookie);*/
#ifdef _REENTRANT
	    ei_mutex_unlock(ei_sockets_lock);
#endif /* _REENTRANT */
	    return &ei_sockets[i];
	}
#ifdef _REENTRANT
    ei_mutex_unlock(ei_sockets_lock);
#endif /* _REENTRANT */
    return NULL;
}

#endif /* EI_DISABLE_SEQ_SOCKET_INFO */

static int remove_ei_socket_info(int fd)
{
    return put_ei_socket_info(fd, -1, NULL, NULL, NULL, NULL);
}

ei_cnode *ei_fd_to_cnode(int fd)
{
    ei_socket_info *sockinfo = get_ei_socket_info(fd);
    if (sockinfo == NULL) return NULL;
    return &sockinfo->cnode;
}

int ei_get_cbs_ctx__(ei_socket_callbacks **cbs, void **ctx, int fd)
{
    ei_socket_info *sockinfo = get_ei_socket_info(fd);
    if (sockinfo) {
        *cbs = sockinfo->cbs;
        *ctx = sockinfo->ctx;
        return 0;
    }

    *cbs = NULL;
    *ctx = NULL;
    return EBADF;
}

/***************************************************************************
 *  Get/Set tracelevel
 ***************************************************************************/

void ei_set_tracelevel(int level) {
    ei_tracelevel = level;
}

int ei_get_tracelevel(void) {
    return ei_tracelevel;
}


/***************************************************************************
 *  Distversion 
 ***************************************************************************/

int ei_distversion(int fd)
{
    ei_socket_info* e = get_ei_socket_info(fd);
    if (e == NULL)
	return -1;
    else
	return e->dist_version;
}

static const char* ei_cookie(int fd)
{
    ei_socket_info* e = get_ei_socket_info(fd);
    if (e == NULL)
	return NULL;
    else
	return e->cookie;
}

const char *ei_thisnodename(const ei_cnode* ec)
{
    return ec->thisnodename;
}

const char *ei_thishostname(const ei_cnode* ec)
{
    return ec->thishostname;
}

const char *ei_thisalivename(const ei_cnode* ec)
{
    return ec->thisalivename;
}

short ei_thiscreation(const ei_cnode* ec)
{
    return ec->creation;
}

/* FIXME: this function is not an api, why not? */
const char *ei_thiscookie(const ei_cnode* ec)
{
    return (const char *)ec->ei_connect_cookie;
}

erlang_pid *ei_self(ei_cnode* ec)
{
    return &ec->self;
}

/* two internal functions that will let us support different cookies
* (to be able to connect to other nodes that don't have the same
* cookie as each other or us)
*/
const char *ei_getfdcookie(int fd)
{
    const char* r = ei_cookie(fd);
    if (r == NULL) r = "";
    return r;
}

static int get_int32(unsigned char *s)
{
    return ((s[0] << 24) | (s[1] << 16) | (s[2] << 8) | (s[3] ));
}


#ifdef __WIN32__
void win32_error(char *buf, int buflen)
{
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
	0,	/* n/a */
	WSAGetLastError(), /* error code */
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* language */
	buf,
	buflen,
	NULL);
    return;
}

static int initWinSock(void)
{
    WORD wVersionRequested;  
    WSADATA wsaData; 
    int i; 

    static LONG volatile initialized = 0;
    
    wVersionRequested = MAKEWORD(1, 1); 
    if (InterlockedCompareExchange((LPLONG) &initialized,1L,0L) == 0L) {
	/* FIXME not terminate, just a message?! */
	if ((i = WSAStartup(wVersionRequested, &wsaData))) {
	    EI_TRACE_ERR1("ei_connect_init",
			  "ERROR: can't initialize windows sockets: %d",i);
	    initialized = 2L;
	    return 0;
	}
	
	if (LOBYTE(wsaData.wVersion) != 1 || HIBYTE(wsaData.wVersion) != 1) { 
	    EI_TRACE_ERR0("initWinSock","ERROR: this version of windows "
			  "sockets not supported");
	    WSACleanup(); 
	    initialized = 2L;
	    return 0;
	}
	initialized = 3L;
    } else while (initialized < 2) {
	SwitchToThread();
    }
    return (int) (initialized - 2);
}
#endif

static int init_connect(int late)
{
    int error;

    /*
     * 'late' is non-zero when not called via ei_init(). Such a
     * call is not supported, but we for now save the day if
     * it easy to do so; otherwise, return ENOTSUP.
     */

#ifdef __WIN32__
    if (!initWinSock()) {
	EI_TRACE_ERR0("ei_init_connect","can't initiate winsock");
	return EIO;
    }
#endif /* win32 */

    error = init_socket_info(late);
    if (error) {
        EI_TRACE_ERR0("ei_init_connect","can't initiate socket info");
        return error;
    }

    ei_connect_initialized = !0;
    return 0;
}

int ei_init_connect(void)
{
    return init_connect(0);
}

/*
* Perhaps run this routine instead of ei_connect_init/2 ?
* Initailize by setting:
* thishostname, thisalivename, thisnodename and thisipaddr
*/
int ei_connect_xinit_ussi(ei_cnode* ec, const char *thishostname,
                          const char *thisalivename, const char *thisnodename,
                          Erl_IpAddr thisipaddr, const char *cookie,
                          const short creation, ei_socket_callbacks *cbs,
                          int cbs_sz, void *setup_context)
{
    char *dbglevel;

    if (!ei_connect_initialized)
        init_connect(!0);

    if (cbs != &ei_default_socket_callbacks)
        EI_SET_HAVE_PLUGIN_SOCKET_IMPL__;
    
    if (cbs_sz < EI_SOCKET_CALLBACKS_SZ_V1) {
	EI_TRACE_ERR0("ei_connect_xinit","invalid size of ei_socket_callbacks struct");
        return ERL_ERROR;
    }
    
    ec->creation = creation & 0x3; /* 2 bits */
    
    if (cookie) {
	if (strlen(cookie) >= sizeof(ec->ei_connect_cookie)) { 
	    EI_TRACE_ERR0("ei_connect_xinit",
			  "ERROR: Cookie size too large");
	    return ERL_ERROR;
	} else {
	    strcpy(ec->ei_connect_cookie, cookie);
	}
    } else if (!get_cookie(ec->ei_connect_cookie, sizeof(ec->ei_connect_cookie))) {
	return ERL_ERROR;
    }
    
    if (strlen(thishostname) >= sizeof(ec->thishostname)) {
	EI_TRACE_ERR0("ei_connect_xinit","ERROR: Thishostname too long");
	return ERL_ERROR;
    }
    strcpy(ec->thishostname, thishostname);
    
    if (strlen(thisalivename) >= sizeof(ec->thisalivename)) {
	EI_TRACE_ERR0("ei_connect_init","Thisalivename too long");
	return ERL_ERROR;
    }
	
    strcpy(ec->thisalivename, thisalivename);
    
    if (strlen(thisnodename) >= sizeof(ec->thisnodename)) {
	EI_TRACE_ERR0("ei_connect_init","Thisnodename too long");
	return ERL_ERROR;
    }
    strcpy(ec->thisnodename, thisnodename);

/* FIXME right now this_ipaddr is never used */    
/*    memmove(&ec->this_ipaddr, thisipaddr, sizeof(ec->this_ipaddr)); */
    
    strcpy(ec->self.node,thisnodename);
    ec->self.num = 0;
    ec->self.serial = 0;
    ec->self.creation = creation & 0x3; /* 2 bits */

    ec->cbs = cbs;
    ec->setup_context = setup_context;

    if ((dbglevel = getenv("EI_TRACELEVEL")) != NULL ||
	(dbglevel = getenv("ERL_DEBUG_DIST")) != NULL)
	ei_tracelevel = atoi(dbglevel);

    return 0;
}

int ei_connect_xinit(ei_cnode* ec, const char *thishostname,
                     const char *thisalivename, const char *thisnodename,
                     Erl_IpAddr thisipaddr, const char *cookie,
                     const short creation)
{
    return ei_connect_xinit_ussi(ec, thishostname, thisalivename, thisnodename,
                                 thisipaddr, cookie, creation,
                                 &ei_default_socket_callbacks,
                                 sizeof(ei_default_socket_callbacks),
                                 NULL);
}

/*
* Initialize by set: thishostname, thisalivename, 
* thisnodename and thisipaddr. At success return 0,
* otherwise return -1.
*/
int ei_connect_init_ussi(ei_cnode* ec, const char* this_node_name,
                         const char *cookie, short creation,
                         ei_socket_callbacks *cbs, int cbs_sz,
                         void *setup_context)
{
    char thishostname[EI_MAXHOSTNAMELEN+1];
    char thisnodename[MAXNODELEN+1];
    char thisalivename[EI_MAXALIVELEN+1];
    struct hostent host, *hp;
    char buffer[1024];
    char *buf = buffer;
    int ei_h_errno;
    int res;

    if (!ei_connect_initialized)
        init_connect(!0);
    
    /* gethostname requires len to be max(hostname) + 1 */
    if (gethostname(thishostname, EI_MAXHOSTNAMELEN+1) == -1) {
#ifdef __WIN32__
	EI_TRACE_ERR1("ei_connect_init","Failed to get host name: %d",
		      WSAGetLastError());
#else
	EI_TRACE_ERR1("ei_connect_init","Failed to get host name: %d", errno);
#endif /* win32 */
	return ERL_ERROR;
    }

    if (this_node_name == NULL) {
	sprintf(thisalivename, "c%d", (int) getpid());
    } else if (strlen(this_node_name) >= sizeof(thisalivename)) {
	EI_TRACE_ERR0("ei_connect_init","ERROR: this_node_name too long");
	return ERL_ERROR;
    } else {
	strcpy(thisalivename, this_node_name);
    }
    
    hp = dyn_gethostbyname_r(thishostname,&host,&buf,sizeof(buffer),&ei_h_errno);
    if (hp == NULL) {
	/* Looking up IP given hostname fails. We must be on a standalone
	   host so let's use loopback for communication instead. */
	hp = dyn_gethostbyname_r("localhost", &host, &buf, sizeof(buffer),
                                 &ei_h_errno);
        if (hp == NULL) {
#ifdef __WIN32__
	    char reason[1024];
	
	    win32_error(reason,sizeof(reason));
	    EI_TRACE_ERR2("ei_connect_init",
			  "Can't get ip address for host %s: %s",
			  thishostname, reason);
#else
	    EI_TRACE_ERR2("ei_connect_init",
			  "Can't get ip address for host %s: %d",
			  thishostname, h_errno);
#endif /* win32 */
	    return ERL_ERROR;
	}
    }
    {
	char* ct;
	if (strcmp(hp->h_name, "localhost") == 0) {
	    /* We use a short node name */    
	    if ((ct = strchr(thishostname, '.')) != NULL) *ct = '\0';
	} else {
	    /* We use a short node name */    
	    if ((ct = strchr(hp->h_name, '.')) != NULL) *ct = '\0';
	    strcpy(thishostname, hp->h_name);
	}
    }
    if (strlen(this_node_name) + 1 + strlen(thishostname) > MAXNODELEN) {
        EI_TRACE_ERR0("ei_connect_init_ussi","this node name is too long");
        return ERL_ERROR;
    }
    sprintf(thisnodename, "%s@%s", this_node_name, thishostname);
    res = ei_connect_xinit_ussi(ec, thishostname, thisalivename, thisnodename,
                                (struct in_addr *)*hp->h_addr_list, cookie, creation,
                                cbs, cbs_sz, setup_context);
    if (buf != buffer)
        free(buf);
    return res;
}

int ei_connect_init(ei_cnode* ec, const char* this_node_name,
                    const char *cookie, short creation)
{
    return ei_connect_init_ussi(ec, this_node_name, cookie, creation,
                                &ei_default_socket_callbacks,
                                sizeof(ei_default_socket_callbacks),
                                NULL);
}

/*
 * Same as ei_gethostbyname_r, but also handles ERANGE error
 * and may allocate larger buffer with malloc.
 */
static
struct hostent *dyn_gethostbyname_r(const char *name,
                                    struct hostent *hostp,
                                    char **buffer_p,
				    int buflen,
				    int *h_errnop)
{
#ifdef __WIN32__
    /*
     * Apparently ei_gethostbyname_r not implemented for Windows (?)
     * Fall back on ei_gethostbyname like before.
     */
    return ei_gethostbyname(name);
#else
    char* buf = *buffer_p;
    struct hostent *hp;

    while (1) {
        hp = ei_gethostbyname_r(name, hostp, buf, buflen, h_errnop);
        if (hp) {
            *buffer_p = buf;
            break;
        }

        if (*h_errnop != ERANGE) {
            if (buf != *buffer_p)
                free(buf);
            break;
        }

        buflen *= 2;
        if (buf == *buffer_p)
            buf = malloc(buflen);
        else {
            char* buf2 = realloc(buf, buflen);
            if (buf2)
                buf = buf2;
            else {
                free(buf);
                buf = NULL;
            }
        }
        if (!buf) {
            *h_errnop = ENOMEM;
            break;
        }
    }
    return hp;
#endif
}

  /* 
  * Set up a connection to a given Node, and 
  * interchange hand shake messages with it.
  * Returns a valid file descriptor at success,
  * otherwise a negative error code.
*/
int ei_connect_tmo(ei_cnode* ec, char *nodename, unsigned ms)
{
    char *hostname, alivename[BUFSIZ];
    struct hostent *hp;
#if !defined (__WIN32__) 
    /* these are needed for the call to gethostbyname_r */
    struct hostent host;
    char buffer[1024];
    char *buf = buffer;
    int ei_h_errno;
#endif /* !win32 */
    int res;

    if (strlen(nodename) > MAXNODELEN) {
	EI_TRACE_ERR0("ei_connect","Too long nodename");
	return ERL_ERROR;
    }
    
    /* extract the host and alive parts from nodename */
    if (!(hostname = strchr(nodename,'@'))) {
	EI_TRACE_ERR0("ei_connect","Node name has no @ in name");
	return ERL_ERROR;
    } else {
	strncpy(alivename, nodename, hostname - nodename);
	alivename[hostname - nodename] = 0x0;
	hostname++;
    }
    
#ifndef __WIN32__
    hp = dyn_gethostbyname_r(hostname,&host,&buf,sizeof(buffer),&ei_h_errno);
    if (hp == NULL) {
	char thishostname[EI_MAXHOSTNAMELEN+1];
        /* gethostname requies len to be max(hostname) + 1*/
	if (gethostname(thishostname,EI_MAXHOSTNAMELEN+1) < 0) {
	    EI_TRACE_ERR0("ei_connect_tmo",
			  "Failed to get name of this host");
	    erl_errno = EHOSTUNREACH;
	    return ERL_ERROR;
	} else {
	    char *ct;
	    /* We use a short node name */    
	    if ((ct = strchr(thishostname, '.')) != NULL) *ct = '\0';
	}
	if (strcmp(hostname,thishostname) == 0)
	    /* Both nodes on same standalone host, use loopback */
	    hp = dyn_gethostbyname_r("localhost",&host,&buf,sizeof(buffer),&ei_h_errno);
	if (hp == NULL) {
	    EI_TRACE_ERR2("ei_connect",
			  "Can't find host for %s: %d\n",nodename,ei_h_errno);
	    erl_errno = EHOSTUNREACH;
	    return ERL_ERROR;
	}
    }
#else /* __WIN32__ */
    if ((hp = ei_gethostbyname(hostname)) == NULL) {
	char thishostname[EI_MAXHOSTNAMELEN+1];
        /* gethostname requires len to be max(hostname) + 1 */
	if (gethostname(thishostname,EI_MAXHOSTNAMELEN+1) < 0) {
	    EI_TRACE_ERR1("ei_connect_tmo",
			  "Failed to get name of this host: %d", 
			  WSAGetLastError());
	    erl_errno = EHOSTUNREACH;
	    return ERL_ERROR;
	} else {
	    char *ct;
	    /* We use a short node name */    
	    if ((ct = strchr(thishostname, '.')) != NULL) *ct = '\0';
	}
	if (strcmp(hostname,thishostname) == 0)
	    /* Both nodes on same standalone host, use loopback */
	    hp = ei_gethostbyname("localhost");
	if (hp == NULL) {
	    char reason[1024];
	    win32_error(reason,sizeof(reason));
	    EI_TRACE_ERR2("ei_connect",
			  "Can't find host for %s: %s",nodename,reason);
	    erl_errno = EHOSTUNREACH;
	    return ERL_ERROR;
	}
    }
#endif /* win32 */

    res = ei_xconnect_tmo(ec, (Erl_IpAddr) *hp->h_addr_list, alivename, ms);

#ifndef __WIN32__
    if (buf != buffer)
        free(buf);
#endif
    return res;
} /* ei_connect */

int ei_connect(ei_cnode* ec, char *nodename)
{
    return ei_connect_tmo(ec, nodename, 0);
}


 /* ip_addr is now in network byte order 
  *
  * first we have to get hold of the portnumber to
  *  the node through epmd at that host 
  *
*/
int ei_xconnect_tmo(ei_cnode* ec, Erl_IpAddr ip_addr, char *alivename, unsigned ms)
{
    ei_socket_callbacks *cbs = ec->cbs;
    void *ctx;
    int rport = 0; /*uint16 rport = 0;*/
    int sockd;
    int dist = 0;
    unsigned her_flags, her_version;
    unsigned our_challenge, her_challenge;
    unsigned char our_digest[16];
    int err;
    int pkt_sz;
    struct sockaddr_in addr;
    unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;
    
    erl_errno = EIO;		/* Default error code */
    
    EI_TRACE_CONN1("ei_xconnect","-> CONNECT attempt to connect to %s",
		   alivename);
    
    if ((rport = ei_epmd_port_tmo(ip_addr,alivename,&dist, tmo)) < 0) {
	EI_TRACE_ERR0("ei_xconnect","-> CONNECT can't get remote port");
	/* ei_epmd_port_tmo() has set erl_errno */
	return ERL_NO_PORT;
    }

    if (dist <= 4) {
	EI_TRACE_ERR0("ei_xconnect","-> CONNECT remote version not compatible");
	return ERL_ERROR;
    }

    err = ei_socket_ctx__(cbs, &ctx, ec->setup_context);
    if (err) {
        EI_TRACE_ERR2("ei_xconnect","-> SOCKET failed: %s (%d)",
                      estr(err), err);
        erl_errno = err;
        return ERL_CONNECT_FAIL;
    }

    memset((void *) &addr, 0, sizeof(struct sockaddr_in));
    memcpy((void *) &addr.sin_addr, (void *) ip_addr, sizeof(addr.sin_addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(rport);

    err = ei_connect_ctx_t__(cbs, ctx, (void *) &addr, sizeof(addr), tmo);
    if (err) {
        EI_TRACE_ERR2("ei_xconnect","-> CONNECT socket connect failed: %s (%d)",
                      estr(err), err);
        abort_connection(cbs, ctx);
        erl_errno = err;
        return ERL_CONNECT_FAIL;
    }
    
    EI_TRACE_CONN0("ei_xconnect","-> CONNECT connected to remote");

    err = EI_GET_FD__(cbs, ctx, &sockd);
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
        goto error;
    }

    err = cbs->handshake_packet_header_size(ctx, &pkt_sz);
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
        goto error;
    }
        
    if (send_name(cbs, ctx, pkt_sz, ec->thisnodename, (unsigned) dist, tmo))
        goto error;
    if (recv_status(cbs, ctx, pkt_sz, tmo))
        goto error;
    if (recv_challenge(cbs, ctx, pkt_sz, &her_challenge,
                       &her_version, &her_flags, NULL, tmo))
        goto error;
    our_challenge = gen_challenge();
    gen_digest(her_challenge, ec->ei_connect_cookie, our_digest);
    if (send_challenge_reply(cbs, ctx, pkt_sz, our_digest, our_challenge, tmo))
        goto error;
    if (recv_challenge_ack(cbs, ctx, pkt_sz, our_challenge, 
                           ec->ei_connect_cookie, tmo))
        goto error;
    if (put_ei_socket_info(sockd, dist, null_cookie, ec, cbs, ctx) != 0)
        goto error;

    if (cbs->connect_handshake_complete) {
        err = cbs->connect_handshake_complete(ctx);
        if (err) {
            EI_TRACE_ERR2("ei_xconnect","-> CONNECT failed: %s (%d)",
                          estr(err), err);
            close_connection(cbs, ctx, sockd);
            EI_CONN_SAVE_ERRNO__(err);
            return ERL_ERROR;
        }
    }    
    
    EI_TRACE_CONN1("ei_xconnect","-> CONNECT (ok) remote = %s",alivename);

    erl_errno = 0;
    return sockd;
    
error:
    EI_TRACE_ERR0("ei_xconnect","-> CONNECT failed");
    abort_connection(cbs, ctx);
    return ERL_ERROR;
} /* ei_xconnect */

int ei_xconnect(ei_cnode* ec, Erl_IpAddr ip_addr, char *alivename)
{
    return ei_xconnect_tmo(ec, ip_addr, alivename, 0);
}

int ei_listen(ei_cnode *ec, int *port, int backlog)
{
    struct in_addr ip_addr;
    ip_addr.s_addr = htonl(INADDR_ANY);
    return ei_xlisten(ec, &ip_addr, port, backlog);
}

int ei_xlisten(ei_cnode *ec, struct in_addr *ip_addr, int *port, int backlog)
{
    ei_socket_callbacks *cbs = ec->cbs;
    struct sockaddr_in sock_addr;
    void *ctx;
    int fd, err, len;

    err = ei_socket_ctx__(cbs, &ctx, ec->setup_context);
    if (err) {
        EI_TRACE_ERR2("ei_xlisten","-> SOCKET failed: %s (%d)",
                      estr(err), err);
        erl_errno = err;
        return ERL_ERROR;
    }

    memset((void *) &sock_addr, 0, sizeof(struct sockaddr_in));
    memcpy((void *) &sock_addr.sin_addr, (void *) ip_addr, sizeof(*ip_addr));
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_port = htons((short) *port);

    len = sizeof(sock_addr);
    err = ei_listen_ctx__(cbs, ctx, (void *) &sock_addr, &len, backlog);
    if (err) {
        EI_TRACE_ERR2("ei_xlisten","-> listen failed: %s (%d)",
                      estr(err), err);
        erl_errno = err;
        goto error;
    }

    if (len != sizeof(sock_addr)) {
        if (len < offsetof(struct sockaddr_in, sin_addr) + sizeof(sock_addr.sin_addr)
            || len < offsetof(struct sockaddr_in, sin_port) + sizeof(sock_addr.sin_port)) {
            erl_errno = EIO;
            EI_TRACE_ERR0("ei_xlisten","-> get info failed");
            goto error;
        }
    }

    memcpy((void *) ip_addr, (void *) &sock_addr.sin_addr, sizeof(*ip_addr));
    *port = (int) ntohs(sock_addr.sin_port);
    
    err = EI_GET_FD__(cbs, ctx, &fd);
    if (err) {
        erl_errno = err;
        goto error;
    }

    if (put_ei_socket_info(fd, 0, null_cookie, ec, cbs, ctx) != 0) {
        EI_TRACE_ERR0("ei_xlisten","-> save socket info failed");
        erl_errno = EIO;
        goto error;
    }
    
    erl_errno = 0;

    return fd;
    
error:
    abort_connection(cbs, ctx);
    return ERL_ERROR;
}
    
static int close_connection(ei_socket_callbacks *cbs, void *ctx, int fd)
{
    int err;
    remove_ei_socket_info(fd);
    err = ei_close_ctx__(cbs, ctx);
    if (err) {
        erl_errno = err;
        return ERL_ERROR;
    }
    return 0;
}

int ei_close_connection(int fd)
{
    ei_socket_callbacks *cbs;
    void *ctx;
    int err = EI_GET_CBS_CTX__(&cbs, &ctx, fd);
    if (err)
        erl_errno = err;
    else {
        if (close_connection(cbs, ctx, fd) == 0)
            return 0;
    }
    EI_TRACE_ERR2("ei_close_connection","<- CLOSE socket close failed: %s (%d)",
                  estr(erl_errno), erl_errno);
    return ERL_ERROR;
} /* ei_close_connection */

static void abort_connection(ei_socket_callbacks *cbs, void *ctx)
{
    (void) ei_close_ctx__(cbs, ctx);
}

  /*
  * Accept and initiate a connection from another
  * Erlang node. Return a file descriptor at success,
  * otherwise -1;
*/
int ei_accept(ei_cnode* ec, int lfd, ErlConnect *conp)
{
    return ei_accept_tmo(ec, lfd, conp, 0);
}

int ei_accept_tmo(ei_cnode* ec, int lfd, ErlConnect *conp, unsigned ms)
{
    int fd;
    unsigned her_version, her_flags;
    char tmp_nodename[MAXNODELEN+1];
    char *her_name;
    int pkt_sz, err;
    struct sockaddr_in addr;
    int addr_len = sizeof(struct sockaddr_in);
    ei_socket_callbacks *cbs;
    void *ctx;
    unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;

    erl_errno = EIO;		/* Default error code */
    
    err = EI_GET_CBS_CTX__(&cbs, &ctx, lfd);
    if (err) {
        if (lfd < 0) {
            EI_CONN_SAVE_ERRNO__(err);
            return ERL_ERROR;
        }
        /*
         * This can be a listen socket created without ei_listen or ei_xlisten,
         * so we must assume it is.
         */
        cbs = &ei_default_socket_callbacks;
        ctx = EI_FD_AS_CTX__(lfd);
    }


    EI_TRACE_CONN0("ei_accept","<- ACCEPT waiting for connection");

    if (conp) {
        her_name = &conp->nodename[0];
    }
    else {
        her_name = &tmp_nodename[0];
    }
    
    /*
     * ei_accept_ctx_t__() replaces the pointer to the listen context
     * with a pointer to the accepted connection context on success.
     */
    err = ei_accept_ctx_t__(cbs, &ctx, (void *) &addr, &addr_len, tmo);
    if (err) {
	EI_TRACE_ERR2("ei_accept","<- ACCEPT socket accept failed: %s (%d)",
                      estr(err), err);
        EI_CONN_SAVE_ERRNO__(err);
        return ERL_ERROR;
    }

    err = EI_GET_FD__(cbs, ctx, &fd);
    if (err) {
	EI_TRACE_ERR2("ei_accept","<- ACCEPT get fd failed: %s (%d)",
                      estr(err), err);
        EI_CONN_SAVE_ERRNO__(err);
    }

    if (addr_len != sizeof(struct sockaddr_in)) {
        if (addr_len < (offsetof(struct sockaddr_in, sin_addr)
                        + sizeof(addr.sin_addr))) {
            EI_TRACE_ERR0("ei_accept","<- ACCEPT get addr failed");
            goto error;
        }
    }

    err = cbs->handshake_packet_header_size(ctx, &pkt_sz);
    if (err) {
	EI_TRACE_ERR2("ei_accept","<- ACCEPT get packet size failed: %s (%d)",
                      estr(err), err);
        EI_CONN_SAVE_ERRNO__(err);
    }
    
    EI_TRACE_CONN0("ei_accept","<- ACCEPT connected to remote");
    
    if (recv_name(cbs, ctx, pkt_sz, &her_version, &her_flags, her_name, tmo)) {
	EI_TRACE_ERR0("ei_accept","<- ACCEPT initial ident failed");
	goto error;
    }
    
    if (her_version <= 4) {
	EI_TRACE_ERR0("ei_accept","<- ACCEPT remote version not compatible");
	goto error;
    }
    else {
	unsigned our_challenge;
	unsigned her_challenge;
	unsigned char our_digest[16];
        
	if (send_status(cbs, ctx, pkt_sz, "ok", tmo))
	    goto error;
	our_challenge = gen_challenge();
	if (send_challenge(cbs, ctx, pkt_sz, ec->thisnodename, 
                           our_challenge, her_version, tmo))
	    goto error;
	if (recv_challenge_reply(cbs, ctx, pkt_sz, our_challenge, 
                                 ec->ei_connect_cookie, &her_challenge, tmo))
	    goto error;
	gen_digest(her_challenge, ec->ei_connect_cookie, our_digest);
	if (send_challenge_ack(cbs, ctx, pkt_sz, our_digest, tmo))
	    goto error;
	if (put_ei_socket_info(fd, her_version, null_cookie, ec, cbs, ctx) != 0)
            goto error;
    }
    if (conp) {
        memcpy((void *) conp->ipadr, (void *) &addr.sin_addr, sizeof(conp->ipadr));
    }

    if (cbs->accept_handshake_complete) {
        err = cbs->accept_handshake_complete(ctx);
        if (err) {
            EI_TRACE_ERR2("ei_xconnect","-> ACCEPT handshake failed: %s (%d)",
                          estr(err), err);
            close_connection(cbs, ctx, fd);
            EI_CONN_SAVE_ERRNO__(err);
            return ERL_ERROR;
        }
    }
    
    EI_TRACE_CONN1("ei_accept","<- ACCEPT (ok) remote = %s",her_name);

    erl_errno = 0;		/* No error */
    return fd;
    
error:
    EI_TRACE_ERR0("ei_accept","<- ACCEPT failed");
    abort_connection(cbs, ctx);
    return ERL_ERROR;
} /* ei_accept */


/* Receives a message from an Erlang socket.
 * If the message was a TICK it is immediately
 * answered. Returns: ERL_ERROR, ERL_TICK or
 * the number of bytes read.
 */
int ei_receive_tmo(int fd, unsigned char *bufp, int bufsize, unsigned ms) 
{
    ssize_t len;
    unsigned char fourbyte[4]={0,0,0,0};
    int err;
    ei_socket_callbacks *cbs;
    void *ctx;
    unsigned tmo = ms == 0 ? EI_SCLBK_INF_TMO : ms;

    err = EI_GET_CBS_CTX__(&cbs, &ctx, fd);
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
        return ERL_ERROR;
    }

    len = (ssize_t) 4;
    err = ei_read_fill_ctx_t__(cbs, ctx, (char *) bufp, &len, tmo);
    if (!err && len != (ssize_t) 4)
        err = EIO;
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
	return ERL_ERROR;
    }
    
    /* Tick handling */
    len = get_int32(bufp);
    if (len == ERL_TICK) {
        len = 4;
	ei_write_fill_ctx_t__(cbs, ctx, (char *) fourbyte, &len, tmo);
	/* FIXME ok to ignore error or timeout? */
	erl_errno = EAGAIN;
	return ERL_TICK;
    }
    
    if (len > bufsize) {
	/* FIXME: We should drain the message. */
	erl_errno = EMSGSIZE;
	return ERL_ERROR;
    }
    else {
        ssize_t need = len;
        err = ei_read_fill_ctx_t__(cbs, ctx, (char *) bufp, &len, tmo);
        if (err) {
            EI_CONN_SAVE_ERRNO__(err);
            return ERL_ERROR;
        }
        if (len != need) {
            erl_errno = EIO;
            return ERL_ERROR;
        }
    }
    
    return (int) len;
    
}

int ei_receive(int fd, unsigned char *bufp, int bufsize) 
{
    return ei_receive_tmo(fd, bufp, bufsize, 0);
} 

int ei_reg_send_tmo(ei_cnode* ec, int fd, char *server_name,
		    char* buf, int len, unsigned ms)
{
    erlang_pid *self = ei_self(ec);
    self->num = fd;

    /* erl_errno and return code is set by ei_reg_send_encoded_tmo() */
    return ei_send_reg_encoded_tmo(fd, self, server_name, buf, len, ms);
}


int ei_reg_send(ei_cnode* ec, int fd, char *server_name, char* buf, int len)
{
    return ei_reg_send_tmo(ec,fd,server_name,buf,len,0);
}

/* 
* Sends an Erlang message to a process at an Erlang node
*/
int ei_send_tmo(int fd, erlang_pid* to, char* buf, int len, unsigned ms)
{
    /* erl_errno and return code is set by ei_reg_send_encoded_tmo() */
    return ei_send_encoded_tmo(fd, to, buf, len, ms);
}

int ei_send(int fd, erlang_pid* to, char* buf, int len)
{
    return ei_send_tmo(fd, to, buf, len, 0);
}


/* 
* Try to receive an Erlang message on a given socket. Returns
* ERL_TICK, ERL_MSG, or ERL_ERROR. Sets `erl_errno' on ERL_ERROR and
* ERL_TICK (to EAGAIN in the latter case).
*/

int ei_do_receive_msg(int fd, int staticbuffer_p, 
		      erlang_msg* msg, ei_x_buff* x, unsigned ms)
{
    int msglen;
    int i;
    
    if (!(i=ei_recv_internal(fd, &x->buff, &x->buffsz, msg, &msglen, 
	staticbuffer_p, ms))) {
	erl_errno = EAGAIN;
	return ERL_TICK;
    }
    if (i<0) {
	/* erl_errno set by ei_recv_internal() */
	return ERL_ERROR;
    }
    if (staticbuffer_p && msglen > x->buffsz)
    {
	erl_errno = EMSGSIZE;
	return ERL_ERROR;
    }
    x->index = msglen;
    switch (msg->msgtype) {	/* FIXME does not handle trace tokens and monitors */
    case ERL_SEND:
    case ERL_REG_SEND:
    case ERL_LINK:
    case ERL_UNLINK:
    case ERL_GROUP_LEADER:
    case ERL_EXIT:
    case ERL_EXIT2:
	return ERL_MSG;
	
    default:
	/*if (emsg->to) 'erl'_free_term(emsg->to);
	  if (emsg->from) 'erl'_free_term(emsg->from);
	  if (emsg->msg) 'erl'_free_term(emsg->msg);
	  emsg->to = NULL;
	  emsg->from = NULL;
	  emsg->msg = NULL;*/
	
	erl_errno = EIO;
	return ERL_ERROR;
    }
} /* do_receive_msg */


int ei_receive_msg(int fd, erlang_msg* msg, ei_x_buff* x)
{
    return ei_do_receive_msg(fd, 1, msg, x, 0);
}

int ei_xreceive_msg(int fd, erlang_msg *msg, ei_x_buff *x)
{
    return ei_do_receive_msg(fd, 0, msg, x, 0);
}

int ei_receive_msg_tmo(int fd, erlang_msg* msg, ei_x_buff* x, unsigned ms)
{
    return ei_do_receive_msg(fd, 1, msg, x, ms);
}

int ei_xreceive_msg_tmo(int fd, erlang_msg *msg, ei_x_buff *x, unsigned ms)
{
    return ei_do_receive_msg(fd, 0, msg, x, ms);
}

/* 
* The RPC consists of two parts, send and receive.
* Here is the send part ! 
* { PidFrom, { call, Mod, Fun, Args, user }} 
*/
/*
* Now returns non-negative number for success, negative for failure.
*/
int ei_rpc_to(ei_cnode *ec, int fd, char *mod, char *fun,
	      const char *buf, int len)
{

    ei_x_buff x;
    erlang_pid *self = ei_self(ec);
    self->num = fd;

    /* encode header */
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);  /* A */
    
    self->num = fd;
    ei_x_encode_pid(&x, self);	      /* A 1 */
    
    ei_x_encode_tuple_header(&x, 5);  /* B A 2 */
    ei_x_encode_atom(&x, "call");     /* B 1 */
    ei_x_encode_atom(&x, mod);	      /* B 2 */
    ei_x_encode_atom(&x, fun);	      /* B 3 */
    ei_x_append_buf(&x, buf, len);    /* B 4 */
    ei_x_encode_atom(&x, "user");     /* B 5 */

    /* ei_x_encode_atom(&x,"user"); */
    ei_send_reg_encoded(fd, self, "rex", x.buff, x.index);
    ei_x_free(&x);
	
    return 0;
} /* rpc_to */

  /*
  * And here is the rpc receiving part. A negative
  * timeout means 'infinity'. Returns either of: ERL_MSG,
  * ERL_TICK, ERL_ERROR or ERL_TIMEOUT.
*/
int ei_rpc_from(ei_cnode *ec, int fd, int timeout, erlang_msg *msg,
		ei_x_buff *x) 
{
    unsigned tmo = timeout < 0 ? EI_SCLBK_INF_TMO : (unsigned) timeout;
    int res = ei_xreceive_msg_tmo(fd, msg, x, tmo);
    if (res < 0 && erl_errno == ETIMEDOUT)
        return ERL_TIMEOUT;
    return res;
} /* rpc_from */

  /*
  * A true RPC. It return a NULL pointer
  * in case of failure, otherwise a valid
  * (ETERM *) pointer containing the reply
  */
int ei_rpc(ei_cnode* ec, int fd, char *mod, char *fun,
	   const char* inbuf, int inbuflen, ei_x_buff* x)
{
    int i, index;
    ei_term t;
    erlang_msg msg;
    char rex[MAXATOMLEN];

    if (ei_rpc_to(ec, fd, mod, fun, inbuf, inbuflen) < 0) {
	return -1;
    }
    /* FIXME are we not to reply to the tick? */
    while ((i = ei_rpc_from(ec, fd, ERL_NO_TIMEOUT, &msg, x)) == ERL_TICK)
	;

    if (i == ERL_ERROR)  return -1;
    /*ep = 'erl'_element(2,emsg.msg);*/ /* {RPC_Tag, RPC_Reply} */
    index = 0;
    if (ei_decode_version(x->buff, &index, &i) < 0
	|| ei_decode_ei_term(x->buff, &index, &t) < 0)
	return -1;		/* FIXME ei_decode_version don't set erl_errno as before */
    /* FIXME this is strange, we don't check correct "rex" atom
       and we let it pass if not ERL_SMALL_TUPLE_EXT and arity == 2 */
    if (t.ei_type == ERL_SMALL_TUPLE_EXT && t.arity == 2)
	if (ei_decode_atom(x->buff, &index, rex) < 0)
	    return -1;
    /* remove header */
    x->index -= index;
    memmove(x->buff, &x->buff[index], x->index);
    return 0;
}


  /*
  ** Handshake
*/


/* FROM RTP RFC 1889  (except that we use all bits, bug in RFC?) */
static unsigned int md_32(char* string, int length)
{
    MD5_CTX ctx;
    union {
	char c[16];
	unsigned x[4];
    } digest;
    ei_MD5Init(&ctx);
    ei_MD5Update(&ctx, (unsigned char *) string, 
	       (unsigned) length);
    ei_MD5Final((unsigned char *) digest.c, &ctx);
    return (digest.x[0] ^ digest.x[1] ^ digest.x[2] ^ digest.x[3]);
}

#if defined(__WIN32__)
unsigned int gen_challenge(void)
{
    struct {
	SYSTEMTIME tv;
	DWORD cpu;
	int pid;
    } s;
    GetSystemTime(&s.tv);
    s.cpu  = GetTickCount();
    s.pid  = getpid();
    return md_32((char*) &s, sizeof(s));
}

#elif  defined(VXWORKS)

static unsigned int gen_challenge(void)
{
    struct {
	struct timespec tv;
	clock_t cpu;
	int pid;
    } s;
    s.cpu  = clock();
    clock_gettime(CLOCK_REALTIME, &s.tv);
    s.pid = getpid();
    return md_32((char*) &s, sizeof(s));
}

#else  /* some unix */

static unsigned int gen_challenge(void)
{
    struct {
	struct timeval tv;
	clock_t cpu;
	pid_t pid;
	u_long hid;
	uid_t uid;
	gid_t gid;
	struct utsname name;
    } s;

    memset(&s, 0, sizeof(s));
    gettimeofday(&s.tv, 0);
    uname(&s.name);
    s.cpu  = clock();
    s.pid  = getpid();
#ifndef __ANDROID__
    s.hid  = gethostid();
#else
    s.hid  = 0;
#endif
    s.uid  = getuid();
    s.gid  = getgid();

    return md_32((char*) &s, sizeof(s));
}
#endif

static void gen_digest(unsigned challenge, char cookie[], 
		       unsigned char digest[16])
{
    MD5_CTX c;
    
    char chbuf[21];
    
    sprintf(chbuf,"%u", challenge);
    ei_MD5Init(&c);
    ei_MD5Update(&c, (unsigned char *) cookie, 
	       (unsigned) strlen(cookie));
    ei_MD5Update(&c, (unsigned char *) chbuf, 
	       (unsigned) strlen(chbuf));
    ei_MD5Final(digest, &c);
}


static char *hex(char digest[16], char buff[33])
{
    static char tab[] = "0123456789abcdef";
    unsigned char *d = (unsigned char *) digest;
    //static char buff[sizeof(digest)*2 + 1];
    char *p = buff;
    int i;
    
    for (i = 0; i < 16; ++i) {
	*p++ = tab[(int)((*d) >> 4)];
	*p++ = tab[(int)((*d++) & 0xF)];
    }
    *p = '\0';
    return buff;
}

static int read_hs_package(ei_socket_callbacks *cbs, void *ctx,
                           int pkt_sz, char **buf, int *buflen, 
                           int *is_static, unsigned ms)
{
    unsigned char nbuf[4];
    unsigned char *x = nbuf;
    ssize_t len, need;
    int err;

    len = (ssize_t) pkt_sz;
    err = ei_read_fill_ctx_t__(cbs, ctx, (char *)nbuf, &len, ms);
    if (!err && len != (ssize_t) pkt_sz)
        err = EIO;
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
	return -1;
    }
    
    switch (pkt_sz) {
    case 2:
        len = get16be(x);
        break;
    case 4:
        len = get32be(x);
        break;
    default:
        return -1;
    }
    
    if (len > *buflen) {
	if (*is_static) {
	    char *tmp = malloc(len);
	    if (!tmp) {
		erl_errno = ENOMEM;
		return -1;
	    }
	    *buf = tmp;
	    *is_static = 0;
	    *buflen = len;
	} else {
	    char *tmp = realloc(*buf, len);
	    if (!tmp) {
		erl_errno = ENOMEM;
		return -1;
	    }
	    *buf = tmp;
	    *buflen = len;
	}
    }
    need = len;
    err = ei_read_fill_ctx_t__(cbs, ctx, *buf, &len, ms);
    if (!err && len != need)
        err = EIO;
    if (err) {
        EI_CONN_SAVE_ERRNO__(err);
	return -1;
    }
    return len;
}


static int send_status(ei_socket_callbacks *cbs, void *ctx,
                       int pkt_sz, char *status, unsigned ms)
{
    char *buf, *s;
    char dbuf[DEFBUF_SIZ];
    int siz = strlen(status) + 1 + pkt_sz;
    int err;
    ssize_t len;

    buf = (siz > DEFBUF_SIZ) ? malloc(siz) : dbuf;
    if (!buf) {
	erl_errno = ENOMEM;
	return -1;
    }
    s = buf;
    switch (pkt_sz) {
    case 2:
        put16be(s,siz - 2);
        break;
    case 4:
        put32be(s,siz - 4);
        break;
    default:
        return -1;
    }
    put8(s, 's');
    memcpy(s, status, strlen(status));
    len = (ssize_t) siz;
    err = ei_write_fill_ctx_t__(cbs, ctx, buf, &len, ms);
    if (!err && len != (ssize_t) siz)
        err = EIO;
    if (err) {
	EI_TRACE_ERR2("send_status","-> SEND_STATUS socket write failed: %s (%d)",
                      estr(err), err);
	if (buf != dbuf)
	    free(buf);        
        EI_CONN_SAVE_ERRNO__(err);
	return -1;
    }
    EI_TRACE_CONN1("send_status","-> SEND_STATUS (%s)",status);

    if (buf != dbuf)
	free(buf);
    return 0;
}

static int recv_status(ei_socket_callbacks *cbs, void *ctx,
                       int pkt_sz, unsigned ms)
{
    char dbuf[DEFBUF_SIZ];
    char *buf = dbuf;
    int is_static = 1;
    int buflen = DEFBUF_SIZ;
    int rlen;
    
    if ((rlen = read_hs_package(cbs, ctx, pkt_sz,
                                &buf, &buflen, &is_static, ms)) <= 0) {
	EI_TRACE_ERR1("recv_status",
		      "<- RECV_STATUS socket read failed (%d)", rlen);
	goto error;
    }

    EI_TRACE_CONN2("recv_status",
                   "<- RECV_STATUS (%.*s)", (rlen>20 ? 20 : rlen), buf);

    if (rlen >= 3 && buf[0] == 's' && buf[1] == 'o' && buf[2] == 'k') {
        /* Expecting "sok" or "sok_simultaneous" */
	if (!is_static)
	    free(buf);
	return 0;
    }
error:
    if (!is_static)
	free(buf);
    return -1;
}

static int send_name_or_challenge(ei_socket_callbacks *cbs,
                                  void *ctx,
                                  int pkt_sz,
                                  char *nodename,
				  int f_chall,
				  unsigned challenge,
				  unsigned version,
				  unsigned ms) 
{
    char *buf;
    unsigned char *s;
    char dbuf[DEFBUF_SIZ];
    int siz = pkt_sz + 1 + 2 + 4 + strlen(nodename);
    const char* function[] = {"SEND_NAME", "SEND_CHALLENGE"};
    int err;
    ssize_t len;
    unsigned int flags;

    if (f_chall)
	siz += 4;
    buf = (siz > DEFBUF_SIZ) ? malloc(siz) : dbuf;
    if (!buf) {
	erl_errno = ENOMEM;
	return -1;
    }
    s = (unsigned char *)buf;
    switch (pkt_sz) {
    case 2:
        put16be(s,siz - 2);
        break;
    case 4:
        put32be(s,siz - 4);
        break;
    default:
        return -1;
    }
    put8(s, 'n');
    put16be(s, version);
    flags = (DFLAG_EXTENDED_REFERENCES
		| DFLAG_DIST_MONITOR
		| DFLAG_EXTENDED_PIDS_PORTS
		| DFLAG_FUN_TAGS
		| DFLAG_NEW_FUN_TAGS
                | DFLAG_NEW_FLOATS
		| DFLAG_SMALL_ATOM_TAGS
		| DFLAG_UTF8_ATOMS
		| DFLAG_MAP_TAG
		| DFLAG_BIG_CREATION
                | DFLAG_EXPORT_PTR_TAG
                | DFLAG_BIT_BINARIES);
    if (ei_internal_use_21_bitstr_expfun()) {
        flags &= ~(DFLAG_EXPORT_PTR_TAG
                   | DFLAG_BIT_BINARIES);
    }
    put32be(s, flags);
    if (f_chall)
	put32be(s, challenge);
    memcpy(s, nodename, strlen(nodename));
    len = (ssize_t) siz;
    err = ei_write_fill_ctx_t__(cbs, ctx, buf, &len, ms);
    if (!err && len != (ssize_t) siz)
        err = EIO;
    if (err) {
	EI_TRACE_ERR1("send_name_or_challenge",
		      "-> %s socket write failed", function[f_chall]);
	if (buf != dbuf)
	    free(buf);
        EI_CONN_SAVE_ERRNO__(err);
	return -1;
    }
    
    if (buf != dbuf)
	free(buf);
    return 0;
}

static int recv_challenge(ei_socket_callbacks *cbs, void *ctx,
                          int pkt_sz, unsigned *challenge, unsigned *version,
			  unsigned *flags, char *namebuf, unsigned ms)
{
    char dbuf[DEFBUF_SIZ];
    char *buf = dbuf;
    int is_static = 1;
    int buflen = DEFBUF_SIZ;
    int rlen;
    char *s;
    char tag;
    char tmp_nodename[MAXNODELEN+1];

    erl_errno = EIO;		/* Default */

    if ((rlen = read_hs_package(cbs, ctx, pkt_sz, &buf, &buflen,
                                &is_static, ms)) <= 0) {
	EI_TRACE_ERR1("recv_challenge",
		      "<- RECV_CHALLENGE socket read failed (%d)",rlen);
	goto error;
    }
    if ((rlen - 11) > MAXNODELEN) {
	EI_TRACE_ERR1("recv_challenge",
		      "<- RECV_CHALLENGE nodename too long (%d)",rlen - 11);
	goto error;
    }
    s = buf;
    if ((tag = get8(s)) != 'n') {
	EI_TRACE_ERR2("recv_challenge",
		      "<- RECV_CHALLENGE incorrect tag, "
		      "expected 'n' got '%c' (%u)",tag,tag);
	goto error;
    }
    *version = get16be(s);
    *flags = get32be(s);
    *challenge = get32be(s);

    if (!(*flags & DFLAG_EXTENDED_REFERENCES)) {
	EI_TRACE_ERR0("recv_challenge","<- RECV_CHALLENGE peer cannot "
		      "handle extended references");
	goto error;
    }

    if (!(*flags & DFLAG_EXTENDED_PIDS_PORTS)) {
	EI_TRACE_ERR0("recv_challenge","<- RECV_CHALLENGE peer cannot "
		      "handle extended pids and ports");
	erl_errno = EIO;
	goto error;
    }
	    
    if (!(*flags & DFLAG_NEW_FLOATS)) {
	EI_TRACE_ERR0("recv_challenge","<- RECV_CHALLENGE peer cannot "
		      "handle binary float encoding");
	goto error;
    }

    if (!namebuf)
        namebuf = &tmp_nodename[0];
    
    memcpy(namebuf, s, rlen - 11);
    namebuf[rlen - 11] = '\0';
    
    if (!is_static)
	free(buf);
    EI_TRACE_CONN4("recv_challenge","<- RECV_CHALLENGE (ok) node = %s, "
	    "version = %u, "
	    "flags = %u, "
	    "challenge = %d",
	    namebuf,
	    *version,
	    *flags,
	    *challenge
	    );
    erl_errno = 0;
    return 0;
error:
    if (!is_static)
	free(buf);
    return -1;
}

static int send_challenge_reply(ei_socket_callbacks *cbs, void *ctx,
                                int pkt_sz, unsigned char digest[16], 
				unsigned challenge, unsigned ms) 
{
    char *s;
    char buf[DEFBUF_SIZ];
    int siz = pkt_sz + 1 + 4 + 16;
    int err;
    ssize_t len;

    s = buf;
    switch (pkt_sz) {
    case 2:
        put16be(s,siz - 2);
        break;
    case 4:
        put32be(s,siz - 4);
        break;
    default:
        return -1;
    }
    put8(s, 'r');
    put32be(s, challenge);
    memcpy(s, digest, 16);

    len = (ssize_t) siz;
    err = ei_write_fill_ctx_t__(cbs, ctx, buf, &len, ms);
    if (!err && len != (ssize_t) siz)
        err = EIO;
    if (err) {
	EI_TRACE_ERR2("send_challenge_reply",
		      "-> SEND_CHALLENGE_REPLY socket write failed: %s (%d)",
                      estr(err), err);
        EI_CONN_SAVE_ERRNO__(err);
	return -1;
    }
    
    if (ei_tracelevel >= 3) {
	char buffer[33];	
   	EI_TRACE_CONN2("send_challenge_reply",
		   "-> SEND_CHALLENGE_REPLY (ok) challenge = %d, digest = %s",
		   challenge,hex((char*)digest, buffer));
    }
    return 0;
}

static int recv_challenge_reply(ei_socket_callbacks *cbs,
                                void *ctx,
                                int pkt_sz,
                                unsigned our_challenge,
                                char cookie[], 
                                unsigned *her_challenge,
                                unsigned ms)
{
    char dbuf[DEFBUF_SIZ];
    char *buf = dbuf;
    int is_static = 1;
    int buflen = DEFBUF_SIZ;
    int rlen;
    char *s;
    char tag;
    char her_digest[16], expected_digest[16];
    
    erl_errno = EIO;		/* Default */

    if ((rlen = read_hs_package(cbs, ctx, pkt_sz, &buf, &buflen, &is_static, ms)) != 21) {
	EI_TRACE_ERR1("recv_challenge_reply",
		      "<- RECV_CHALLENGE_REPLY socket read failed (%d)",rlen);
	goto error;
    }
    
    s = buf;
    if ((tag = get8(s)) != 'r') {
	EI_TRACE_ERR2("recv_challenge_reply",
		      "<- RECV_CHALLENGE_REPLY incorrect tag, "
		      "expected 'r' got '%c' (%u)",tag,tag);
	goto error;
    }
    *her_challenge = get32be(s);
    memcpy(her_digest, s, 16);
    gen_digest(our_challenge, cookie, (unsigned char*)expected_digest);
    if (memcmp(her_digest, expected_digest, 16)) {
	EI_TRACE_ERR0("recv_challenge_reply",
		      "<- RECV_CHALLENGE_REPLY authorization failure");
	goto error;
    }
    if (!is_static)
	free(buf);

   
    if (ei_tracelevel >= 3) {
	char buffer[33];	
        EI_TRACE_CONN2("recv_challenge_reply",
		   "<- RECV_CHALLENGE_REPLY (ok) challenge = %u, digest = %s",
		   *her_challenge,hex(her_digest,buffer));
    }
    erl_errno = 0;
    return 0;
    
error:
    if (!is_static)
	free(buf);
    return -1;
}

static int send_challenge_ack(ei_socket_callbacks *cbs, void *ctx, int pkt_sz,
                              unsigned char digest[16], unsigned ms) 
{
    char *s;
    char buf[DEFBUF_SIZ];
    int siz = pkt_sz + 1 + 16;
    int err;
    ssize_t len;

    s = buf;
    switch (pkt_sz) {
    case 2:
        put16be(s,siz - 2);
        break;
    case 4:
        put32be(s,siz - 4);
        break;
    default:
        return -1;
    }
    put8(s, 'a');
    memcpy(s, digest, 16);

    len = (ssize_t) siz;
    err = ei_write_fill_ctx_t__(cbs, ctx, buf, &len, ms);
    if (!err && len != (ssize_t) siz)
        err = EIO;
    if (err) {
	EI_TRACE_ERR2("recv_challenge_reply",
		      "-> SEND_CHALLENGE_ACK socket write failed: %s (%d)",
                      estr(err), err);
        EI_CONN_SAVE_ERRNO__(err);
	return -1;
    }
    
    if (ei_tracelevel >= 3) {
	char buffer[33];	
    	EI_TRACE_CONN1("recv_challenge_reply",
		   "-> SEND_CHALLENGE_ACK (ok) digest = %s",hex((char *)digest,buffer));
    }
    
    return 0;
}

static int recv_challenge_ack(ei_socket_callbacks *cbs, void *ctx,
			      int pkt_sz, unsigned our_challenge,
			      char cookie[], unsigned ms)
{
    char dbuf[DEFBUF_SIZ];
    char *buf = dbuf;
    int is_static = 1;
    int buflen = DEFBUF_SIZ;
    int rlen;
    char *s;
    char tag;
    char her_digest[16], expected_digest[16];
    
    erl_errno = EIO;		/* Default */

    if ((rlen = read_hs_package(cbs, ctx, pkt_sz, &buf, &buflen, &is_static, ms)) != 17) {
	EI_TRACE_ERR1("recv_challenge_ack",
		      "<- RECV_CHALLENGE_ACK socket read failed (%d)",rlen);
	goto error;
    }
    
    s = buf;
    if ((tag = get8(s)) != 'a') {
	EI_TRACE_ERR2("recv_challenge_ack",
		      "<- RECV_CHALLENGE_ACK incorrect tag, "
		      "expected 'a' got '%c' (%u)",tag,tag);
	goto error;
    }
    memcpy(her_digest, s, 16);
    gen_digest(our_challenge, cookie, (unsigned char *)expected_digest);
    if (memcmp(her_digest, expected_digest, 16)) {
	EI_TRACE_ERR0("recv_challenge_ack",
		      "<- RECV_CHALLENGE_ACK authorization failure");
	goto error;
    }
    if (!is_static)
	free(buf);

    if (ei_tracelevel >= 3) {
	char buffer[33];	
	EI_TRACE_CONN1("recv_challenge_ack",
		   "<- RECV_CHALLENGE_ACK (ok) digest = %s",hex(her_digest,buffer));
    }
    erl_errno = 0;
    return 0;

error:
    if (!is_static)
	free(buf);
    return -1;
}

static int send_name(ei_socket_callbacks *cbs, void *ctx, int pkt_sz,
                     char *nodename, unsigned version, unsigned ms) 
{
    return send_name_or_challenge(cbs, ctx, pkt_sz, nodename, 0,
                                  0, version, ms);
}

static int send_challenge(ei_socket_callbacks *cbs, void *ctx, int pkt_sz,
                          char *nodename, unsigned challenge, unsigned version,
                          unsigned ms)
{
    return send_name_or_challenge(cbs, ctx, pkt_sz, nodename, 1,
                                  challenge, version, ms);
}

static int recv_name(ei_socket_callbacks *cbs, void *ctx,
                     int pkt_sz, unsigned *version,
		     unsigned *flags, char *namebuf, unsigned ms)
{
    char dbuf[DEFBUF_SIZ];
    char *buf = dbuf;
    int is_static = 1;
    int buflen = DEFBUF_SIZ;
    int rlen;
    char *s;
    char tmp_nodename[MAXNODELEN+1];
    char tag;
    
    erl_errno = EIO;		/* Default */

    if ((rlen = read_hs_package(cbs, ctx, pkt_sz, &buf, &buflen,
                                &is_static, ms)) <= 0) {
	EI_TRACE_ERR1("recv_name","<- RECV_NAME socket read failed (%d)",rlen);
	goto error;
    }
    if ((rlen - 7) > MAXNODELEN) {
	EI_TRACE_ERR1("recv_name","<- RECV_NAME nodename too long (%d)",rlen-7);
	goto error;
    }
    s = buf;
    tag = get8(s);
    if (tag != 'n') {
	EI_TRACE_ERR2("recv_name","<- RECV_NAME incorrect tag, "
		      "expected 'n' got '%c' (%u)",tag,tag);
	goto error;
    }
    *version = get16be(s);
    *flags = get32be(s);

    if (!(*flags & DFLAG_EXTENDED_REFERENCES)) {
	EI_TRACE_ERR0("recv_name","<- RECV_NAME peer cannot handle"
		      "extended references");
	goto error;
    }

    if (!(*flags & DFLAG_EXTENDED_PIDS_PORTS)) {
	EI_TRACE_ERR0("recv_name","<- RECV_NAME peer cannot "
		      "handle extended pids and ports");
	erl_errno = EIO;
	goto error;
    }

    if (!namebuf)
        namebuf = &tmp_nodename[0];
    
    memcpy(namebuf, s, rlen - 7);
    namebuf[rlen - 7] = '\0';

    if (!is_static)
	free(buf);
    EI_TRACE_CONN3("recv_name",
		   "<- RECV_NAME (ok) node = %s, version = %u, flags = %u",
		   namebuf,*version,*flags);
    erl_errno = 0;
    return 0;
    
error:
    if (!is_static)
	free(buf);
    return -1;
}

/***************************************************************************
 *
 *  Returns 1 on success and 0 on failure.
 *
 ***************************************************************************/


/* size is the buffer size, e.i. string length + 1 */

static int get_home(char *buf, int size)
{
#ifdef __WIN32__
    char* homedrive = getenv("HOMEDRIVE");
    char* homepath = getenv("HOMEPATH");
    
    if (homedrive && homepath) {
	if (strlen(homedrive)+strlen(homepath) >= size)
	    return 0;
	strcpy(buf, homedrive);
	strcat(buf, homepath);
	return 1;
    }
    else {
	int len = GetWindowsDirectory(buf, size);
	if (len) {
	    return (len < size);
	}
    }
#else
    char* homepath = getenv("HOME");
    if (homepath) {
	if (strlen(homepath) >= size)
	    return 0;
	strcpy(buf, homepath);
	return 1;
    }
#endif

    buf[0] = '.';
    buf[1] = '\0';
    return 1;
}


static int get_cookie(char *buf, int bufsize)
{
    char fname[EI_MAX_HOME_PATH + sizeof(COOKIE_FILE) + 1];
    int fd;
    int len;
    unsigned char next_c;
    
    if (!get_home(fname, EI_MAX_HOME_PATH+1)) {
	fprintf(stderr,"<ERROR> get_cookie: too long path to home");
	return 0;
    }

    strcat(fname, COOKIE_FILE);
    if ((fd = open(fname, O_RDONLY, 0777)) < 0) {
	fprintf(stderr,"<ERROR> get_cookie: can't open cookie file");
	return 0;
    }
    
    if ((len = read(fd, buf, bufsize-1)) < 0) {
	fprintf(stderr,"<ERROR> get_cookie: reading cookie file");
	close(fd);
	return 0;
    }

    /* If more to read it is too long. Not 100% correct test but will do. */
    if (read(fd, &next_c, 1) > 0 && !isspace(next_c)) {
	fprintf(stderr,"<ERROR> get_cookie: cookie in %s is too long",fname);
	close(fd);
	return 0;
    }

    close(fd);

    /* Remove all newlines after the first newline */
    buf[len] = '\0';		/* Terminate string */
    len = strcspn(buf,"\r\n");
    buf[len] = '\0';		/* Terminate string again */

    return 1;			/* Success! */
}

