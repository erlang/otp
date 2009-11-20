/*<copyright>
 * <year>1999-2008</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
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
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */

/*
 * Purpose:  Implementation of Secure Socket Layer (SSL).
 *
 * This is an "SSL proxy" for Erlang in the form of a port
 * program. 
 *
 * The implementation has borrowed somewhat from the original
 * implementation of `socket' by Claes Wikström, and the former
 * implementation of `ssl_socket' by Helen Ariyan.
 *
 * All I/O is now non-blocking. 
 *
 * When a connection (cp) is in the state JOINED we have the following
 * picture:
 *
 *            proxy->fd                          fd
 *               |                               |
 *  proxy->eof   |  -------->  wq  ----------->  |   bp 
 *               |                               |
 *  Erlang       |                               |   SSL
 *               |                               |
 *  proxy->bp    |  <------ proxy->wq ---------  |   eof
 *               |                               |
 * 
 * We read from Erlang (proxy->fd) and write to SSL (fd); and read from 
 * SSL (fd) and write to Erlang (proxy->fd).  
 *
 * The variables bp (broken pipe) and eof (end of file) take the
 * values 0 and 1.
 *
 * What has been read and cannot be immediately written is put in a
 * write queue (wq). A wq is emptied before reads are continued, which
 * means that at most one chunk that is read can be in a wq.
 *
 * The proxy-to-ssl part of a cp is valid iff 
 *
 *         !bp && (wq.len > 0 || !proxy->eof).
 *
 * The ssl-to-proxy part of a cp is valid iff 
 *
 *         !proxy->bp && (proxy->wq.len > 0 || !eof). 
 *
 * The connection is valid if any of the above parts are valid, i.e.
 * invalid if both parts are invalid.
 *
 * Every SELECT_TIMEOUT second we try to write to those file
 * descriptors that have non-empty wq's (the only way to detect that a
 * far end has gone away is to write to it).
 *
 * STATE TRANSITIONS
 *
 * Below (*) means that the corresponding file descriptor is published
 * (i.e. kwown outside this port program) when the state is entered,
 * and thus cannot be closed without synchronization with the
 * ssl_server.
 *
 * Listen:
 *
 * STATE_NONE ---> (*) PASSIVE_LISTENING <---> ACTIVE_LISTENING
 *
 * Accept:
 *
 * STATE_NONE ---> SSL_ACCEPT ---> (*) CONNECTED ---> JOINED ---> 
 *  ---> SSL_SHUTDOWN ---> DEFUNCT
 *
 * Connect:
 *
 * STATE_NONE ---> (*) WAIT_CONNECT ---> SSL_CONNECT ---> CONNECTED ---> 
 *  ---> JOINED ---> SSL_SHUTDOWN ---> DEFUNCT
 * 
 * In states where file descriptors has been published, and where
 * something goes wrong, the state of the connection is set to
 * DEFUNCT. A connection in such a state can only be closed by a CLOSE
 * message from Erlang (a reception of such a message is registered in
 * cp->closed). The possible states are: WAIT_CONNECT, SSL_CONNECT,
 * CONNECTED, JOINED, and SSL_SHUTDOWN.
 *
 * A connection in state SSL_ACCEPT can be closed and removed without
 * synchronization.
 *
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef __WIN32__
#include "esock_winsock.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>

#ifdef __WIN32__
#include <process.h>
#else
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/time.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <fcntl.h>
#endif

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff  /* Should be in <netinet/in.h>.  */
#endif

#include "esock.h"
#include "debuglog.h"
#include "esock_utils.h"
#include "esock_ssl.h"
#include "esock_osio.h"
#include "esock_posix_str.h"
#include "esock_poll.h"

#define MAJOR_VERSION   2
#define MINOR_VERSION   0
#define MAXREPLYBUF	256
#define RWBUFLEN	(32*1024)
#define IS_CLIENT       0
#define IS_SERVER       1
#define SELECT_TIMEOUT  2	/* seconds */

#define psx_errstr()	esock_posix_str(sock_errno())
#define ssl_errstr()	esock_ssl_errstr

#define PROXY_TO_SSL_VALID(cp) (!(cp)->bp && \
				((cp)->wq.len > 0 || !(cp)->proxy->eof))

#define SSL_TO_PROXY_VALID(cp) (!(cp)->proxy->bp && \
				((cp)->proxy->wq.len > 0 || !(cp)->eof))

#define JOINED_STATE_INVALID(cp) (!(PROXY_TO_SSL_VALID(cp)) && \
				!(SSL_TO_PROXY_VALID(cp)))
static int loop(void);
static int set_poll_conns(Connection *cp, EsockPoll *ep, int verbose);
static Connection *next_polled_conn(Connection *cp, Connection **cpnext,
				    EsockPoll *ep, int set_wq_fds);

static void leave_joined_state(Connection *cp);
static void do_shutdown(Connection *cp);
static void close_and_remove_connection(Connection *cp);
static int reply(int cmd, char *fmt, ...);
static int input(char *fmt, ...);
static int put_pars(unsigned char *buf, char *fmt, va_list args);
static int get_pars(unsigned char *buf, char *fmt, va_list args);
static FD do_connect(char *lipstring, int lport, char *fipstring, int fport);
static FD do_listen(char *ipstring, int lport, int backlog, int *aport);
static FD do_accept(FD listensock, struct sockaddr *saddr, int *len);
static void print_connections(void);
static void dump_connections(void);
static int check_num_sock_fds(FD fd); 
static void safe_close(FD fd);
static Connection *new_connection(int state, FD fd);
static Connection *get_connection(FD fd);
static void remove_connection(Connection *conn);
static Proxy *get_proxy_by_peerport(int port);
static Proxy *new_proxy(FD fd);
static void remove_proxy(Proxy *proxy);
static void ensure_write_queue(WriteQueue *wq, int size);
static void clean_up(void);

static Connection  *connections = NULL;
static int num_sock_fds;	/* On UNIX all file descriptors */
static Proxy *proxies = NULL;
static int proxy_listensock = INVALID_FD;
static int proxy_listenport = 0;
static int proxy_backlog = 128;
static int proxysock_last_err = 0;
static int proxysock_err_cnt = 0;
static char rwbuf[RWBUFLEN];
static unsigned char *ebuf = NULL; /* Set by read_ctrl() */

static char *connstr[] = {
    "STATE_NONE", 
    "ACTIVE_LISTENING",
    "PASSIVE_LISTENING",
    "CONNECTED",
    "WAIT_CONNECT",
    "SSL_CONNECT",
    "SSL_ACCEPT",
    "TRANSPORT_ACCEPT",
    "JOINED",
    "SSL_SHUTDOWN",
    "DEFUNCT"
};

static char *originstr[] = {
    "listen",
    "accept",
    "connect"
};

int main(int argc, char **argv) 
{
    char *logfile = NULL;
    int i;
    esock_version *vsn;
    char *ciphers; 
#ifdef __WIN32__
    int pid;
    WORD version;
    WSADATA wsa_data;

    set_binary_mode();
    setvbuf(stderr, NULL, _IONBF, 0);
    /* Two sockets for the stdin socket pipe (local thread). */
    num_sock_fds = 2;		
#else
    pid_t pid;
    num_sock_fds = 3;		/* 0, 1, 2 */
#endif

    pid = getpid();
    i = 1;
    while (i < argc) {
	if (strcmp(argv[i], "-d") == 0) {
	    debug = 1;
	    i++;
	} else if (strcmp(argv[i], "-dm") == 0) {
	    debugmsg = 1;
	    i++;
	} else if (strcmp(argv[i], "-pp") == 0) {
	    i++;
	    proxy_listenport = atoi(argv[i]);
	    i++;
	} else if (strcmp(argv[i], "-pb") == 0) {
	    i++;
	    proxy_backlog = atoi(argv[i]);
	    i++;
	} else if (strcmp(argv[i], "-pv") == 0) {
	    i++;
	    protocol_version = atoi(argv[i]);
	    i++;
	} else if (strcmp(argv[i], "-dd") == 0) {
	    i++;
	    logfile = esock_malloc(strlen(argv[i]) + 64);
	    sprintf(logfile, "%s/ssl_esock.%d.log", argv[i], (int)pid);
	    i++;
	} else if (strcmp(argv[i], "-ersa") == 0) {
	    ephemeral_rsa = 1;
	    i++;
	} else if (strcmp(argv[i], "-edh") == 0) {
	    ephemeral_dh = 1;
	    i++;
	}
    }
    if (debug || debugmsg) {
	DEBUGF(("Starting ssl_esock\n"));
	if (logfile) {
	    open_ssllog(logfile);
#ifndef __WIN32__
	    num_sock_fds++;
#endif
	}
	atexit(close_ssllog);
	DEBUGF(("pid = %d\n", getpid()));
    }
    if (esock_ssl_init() < 0) {
	fprintf(stderr, "esock: Could not do esock_ssl_init\n");
	exit(EXIT_FAILURE);
    }

    atexit(esock_ssl_finish);

#ifdef __WIN32__
    /* Start Windows' sockets */
    version = MAKEWORD(MAJOR_VERSION, MINOR_VERSION);
    if (WSAStartup(version, &wsa_data) != 0) {
	fprintf(stderr, "esock: Could not start up Windows' sockets\n");
	exit(EXIT_FAILURE);
    }
    atexit((void (*)(void))WSACleanup);
    if (LOBYTE(wsa_data.wVersion) < MAJOR_VERSION ||
	(LOBYTE(wsa_data.wVersion) == MAJOR_VERSION && 
	 HIBYTE(wsa_data.wVersion) < MINOR_VERSION)) {
	fprintf(stderr, "esock: Windows socket version error. "
		"Requested version:"
		"%d.%d, version found: %d.%d\n", MAJOR_VERSION, 
		MINOR_VERSION, LOBYTE(wsa_data.wVersion), 
		HIBYTE(wsa_data.wVersion));
	exit(EXIT_FAILURE);
    }
    DEBUGF(("Using Windows socket version: %d.%d\n", 
	   LOBYTE(wsa_data.wVersion), HIBYTE(wsa_data.wVersion)));
    DEBUGF(("Maximum number of sockets available: %d\n", 
	    wsa_data.iMaxSockets));
 
    if (esock_osio_init() < 0) {
	fprintf(stderr, "esock: Could not init osio\n");
	exit(EXIT_FAILURE);
    }
    atexit(esock_osio_finish);
#endif

    /* Create the local proxy listen socket and set it to non-blocking */
    proxy_listensock = do_listen("127.0.0.1", proxy_listenport, 
				 proxy_backlog, &proxy_listenport);
    if (proxy_listensock == INVALID_FD) {
	fprintf(stderr, "esock: Cannot create local listen socket\n");
	exit(EXIT_FAILURE);
    }
    SET_NONBLOCKING(proxy_listensock);
    DEBUGF(("Local proxy listen socket: fd = %d, port = %d\n", 
	   proxy_listensock, proxy_listenport));

    vsn = esock_ssl_version();
    ciphers = esock_ssl_ciphers();

    /* Report: port number of the local proxy listen socket, the native
     * os pid, the compile and lib versions of the ssl library, and 
     * the list of available ciphers. */
    reply(ESOCK_PROXY_PORT_REP, "24sss", proxy_listenport, (int)pid, 
	  vsn->compile_version, vsn->lib_version, ciphers);

    atexit(clean_up);

    loop();

    if (logfile) 
	esock_free(logfile);
    exit(EXIT_SUCCESS);
}


/*
 * Local functions
 *
 */

static int loop(void)
{
    EsockPoll pollfd;
    FD fd, msgsock, listensock, connectsock, proxysock;
    int cc, wc, fport, lport, pport, length, backlog, intref, op;
    int value;
    char *lipstring, *fipstring;
    char *flags;
    char *protocol_vsn, *cipher;
    unsigned char *cert, *bin;
    int certlen, binlen;
    struct sockaddr_in iserv_addr;
    int sret = 1;
    Connection *cp, *cpnext, *newcp;
    Proxy *pp;
    time_t last_time = 0, now = 0;
    int set_wq_fds;

    esock_poll_init(&pollfd);

    while(1) {
	esock_poll_zero(&pollfd);
	esock_poll_fd_set_read(&pollfd, proxy_listensock);
	esock_poll_fd_set_read(&pollfd, local_read_fd);

	set_wq_fds = 0;

	if (sret)		/* sret == 1 the first time. */
	    DEBUGF(("==========LOOP=============\n"));

	cc = set_poll_conns(connections, &pollfd, sret) + 1;

	if (sret) {
	    print_connections();
	    DEBUGF(("Before poll/select: %d descriptor%s (total %d)\n",
		    cc, (cc == 1) ? "" : "s", num_sock_fds));
	}

	sret = esock_poll(&pollfd, SELECT_TIMEOUT);
	if (sret < 0) {
	    DEBUGF(("select/poll error: %s\n", psx_errstr()));
	    continue;
	}
	
	time(&now);
	if (now >= last_time + SELECT_TIMEOUT) {
	    set_wq_fds = 1;
	    last_time = now;
	}
	/*
	 * First accept as many connections as possible on the
	 * proxy listen socket. We record the peer port, which
	 * is later used as a reference for joining a proxy 
	 * connection with a network connection.
	 */

	if (esock_poll_fd_isset_read(&pollfd, proxy_listensock)) {
	    while (1) {
		length = sizeof(iserv_addr);
		proxysock = do_accept(proxy_listensock, 
				      (struct sockaddr *)&iserv_addr, 
				      (int*)&length);
		if(proxysock == INVALID_FD) {
		    if (sock_errno() != ERRNO_BLOCK) {
			/* We can here for example get the error
			 * EMFILE, i.e. no more file descriptors
			 * available, but we do not have any specific
			 * connection to report the error to.  We
			 * increment the error counter and saves the
			 * last err.  
			 */
			proxysock_err_cnt++;
			proxysock_last_err = sock_errno();
			DEBUGF(("accept error (proxy_listensock): %s\n", 
				psx_errstr()));
		    }
		    break;
		} else {
		    /* Get peer port number */
/* 		    length = sizeof(iserv_addr); */
/* 		    if (getpeername(proxysock, (struct sockaddr *)&iserv_addr,  */
/* 				    &length) < 0) { */
/* 			DEBUGF(("Can't get peername of proxy socket")); */
/* 			safe_close(proxysock); */
/* 		    } else { */
			/* Add to pending proxy connections */
			SET_NONBLOCKING(proxysock);
			pp = new_proxy(proxysock);
			pp->peer_port = ntohs(iserv_addr.sin_port);
			DEBUGF(("-----------------------------------\n"));
			DEBUGF(("[PROXY_LISTEN_SOCK] conn accepted: "
				"proxyfd = %d, "
			       "peer port = %d\n", proxysock, pp->peer_port));
/* 		    } */
		}
	    }
	}

	/* 
	 * Read control messages from Erlang
	 */
	if (esock_poll_fd_isset_read(&pollfd, local_read_fd)) {  
	    cc = read_ctrl(&ebuf);
	    if ( cc < 0 ) {
		DEBUGF(("Read loop -1 or 0\n"));
		return -1;
	    } else if (cc == 0) {  /* not eof  */
		DEBUGF(("GOT empty string \n"));

	    } else {

		switch((int)*ebuf) {

		case ESOCK_SET_SEED_CMD:
		    /* 
		     * ebuf = {cmd(1), binary(N) }
		     */
		    input("b", &binlen, &bin);
		    DEBUGF(("[SET_SEED_CMD]\n"));
		    esock_ssl_seed(bin, binlen);
		    /* no reply */
		    break;

		case ESOCK_GETPEERNAME_CMD:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    DEBUGF(("[GETPEERNAME_CMD] fd = %d\n", fd)); 
		    cp = get_connection(fd);
		    length = sizeof(iserv_addr);
		    if (!cp) {
			sock_set_errno(ERRNO_NOTSOCK);
			reply(ESOCK_GETPEERNAME_ERR, "4s", fd, psx_errstr());
		    } else if (getpeername(fd, 
					   (struct sockaddr *) &iserv_addr, 
					   &length) < 0) {
			reply(ESOCK_GETPEERNAME_ERR, "4s", fd, psx_errstr());
		    } else {
			/*
			 * reply  = {cmd(1), fd(4), port(2), 
			 * 	    ipstring(N), 0(1)}
			 */
			reply(ESOCK_GETPEERNAME_REP, "42s", fd, 
			      ntohs(iserv_addr.sin_port), 
			      inet_ntoa(iserv_addr.sin_addr));
		    }
		    break;

		case ESOCK_GETSOCKNAME_CMD:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    DEBUGF(("[GETSOCKNAME_CMD] fd = %d\n", fd)); 
		    cp = get_connection(fd);
		    length = sizeof(iserv_addr);
		    if (!cp) {
			sock_set_errno(ERRNO_NOTSOCK);
			reply(ESOCK_GETSOCKNAME_ERR, "4s", fd, psx_errstr());
		    } else if (getsockname(fd, 
					   (struct sockaddr *)&iserv_addr, 
					   &length) < 0) {
			reply(ESOCK_GETSOCKNAME_ERR, "4s", fd, psx_errstr());
		    } else {
			/*
			 * reply  = {cmd(1), fd(4), port(2), 
			 * 	    ipstring(N), 0(1)}
			 */
			reply(ESOCK_GETSOCKNAME_REP, "42s", fd, 
			      ntohs(iserv_addr.sin_port),
			      inet_ntoa(iserv_addr.sin_addr));
		    }
		    break;

		case ESOCK_GETCONNINFO_CMD:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    DEBUGF(("[GETCONNINFO_CMD] fd = %d\n", fd)); 
		    cp = get_connection(fd);
		    if (!cp) {
			sock_set_errno(ERRNO_NOTSOCK);
			reply(ESOCK_GETCONNINFO_ERR, "4s", fd, psx_errstr());
		    } else {
			if (esock_ssl_getprotocol_version(cp,
							  &protocol_vsn) < 0)
			    reply(ESOCK_GETCONNINFO_ERR, "4s", fd, psx_errstr());
			else if (esock_ssl_getcipher(cp, &cipher) < 0)
			    reply(ESOCK_GETCONNINFO_ERR, "4s", fd, psx_errstr());
			else
			/*
			 * reply  = {cmd(1), fd(4), protocol(N), 0(1),
			 * 	    cipher(N), 0(1)}
			 */
			    reply(ESOCK_GETCONNINFO_REP, "4ss", fd, 
				  protocol_vsn, cipher);
		    }
		    break;

		case ESOCK_GETPEERCERT_CMD:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    DEBUGF(("[GETPEERCERT_CMD] fd = %d\n", fd)); 
		    cp = get_connection(fd);
		    if (!cp) {
			sock_set_errno(ERRNO_NOTSOCK);
			reply(ESOCK_GETPEERCERT_ERR, "4s", fd, psx_errstr());
		    } else {
			if ((certlen = esock_ssl_getpeercert(cp, &cert)) < 0)
			    reply(ESOCK_GETPEERCERT_ERR, "4s", fd, psx_errstr());
			else {
			    /*
			     * reply  = {cmd(1), fd(4), certlen(4), cert(N)}
			     */
			    reply(ESOCK_GETPEERCERT_REP, "4b", fd, 
				  certlen, cert);
			    esock_free(cert);
			}
		    }
		    break;

		case ESOCK_CONNECT_CMD:
		    /* 
		     * ebuf = {cmd(1), intref(4), 
		     * 	       lport(2), lipstring(N), 0(1),  -- local
		     *         fport(2), fipstring(N), 0(1),  -- foreign
		     * 	       flags(N), 0(1)}
		     */
		    input("42s2ss", &intref, &lport, &lipstring, 
			  &fport, &fipstring, &flags);
		    DEBUGF(("[CONNECT_CMD] intref = %d, "
			    "lipstring = %s lport = %d, "
			    "fipstring = %s fport = %d, "
			    "flags = %s\n", intref, lipstring, lport,
			    fipstring, fport, flags));
		    connectsock = do_connect(lipstring, lport, 
					     fipstring, fport);
		    if(connectsock == INVALID_FD) {
			reply(ESOCK_CONNECT_SYNC_ERR, "4s", intref, psx_errstr());
			break;
		    }
		    DEBUGF(("  fd = %d\n", connectsock));
		    cp = new_connection(ESOCK_WAIT_CONNECT, connectsock);
		    cp->origin = ORIG_CONNECT;
		    length = strlen(flags);
		    cp->flags = esock_malloc(length + 1);
		    strcpy(cp->flags, flags);
		    DEBUGF(("-> WAIT_CONNECT fd = %d\n", connectsock));
		    /* Publish connectsock */
		    reply(ESOCK_CONNECT_WAIT_REP, "44", intref, connectsock);
		    break;
		    
		case ESOCK_TERMINATE_CMD:
		    /* 
		     * ebuf = {cmd(1)}
		     */
		    exit(EXIT_SUCCESS);
		    break;

		case ESOCK_CLOSE_CMD:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    if ((cp = get_connection(fd))) {
			DEBUGF(("%s[CLOSE_CMD]: fd = %d\n", 
				connstr[cp->state], fd));
			if (cp->proxy)
			    cp->proxy->bp = 1;
			switch (cp->state) {
			case ESOCK_JOINED:
			    cp->close = 1;
			    if (JOINED_STATE_INVALID(cp))
				leave_joined_state(cp);
			    break;
			case ESOCK_SSL_SHUTDOWN:
			    cp->close = 1;
			    DEBUGF(("  close flag set\n"));
			    break;
			default:
			    DEBUGF(("-> (removal)\n"));
			    close_and_remove_connection(cp);
			}
		    } else 
			DEBUGF(("[CLOSE_CMD]: ERROR: fd = %d not found\n", fd));
		    break;

		case ESOCK_SET_SOCKOPT_CMD:
		    /* 
		     * ebuf = {cmd(1), fd(4), op(1), on(1)}
		     */
		    input("411", &fd, &op, &value);
		    switch(op) {
		    case ESOCK_SET_TCP_NODELAY:
			if(setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, 
				      (void *)&value, sizeof(value)) < 0) {
			    DEBUGF(("Error: setsockopt TCP_NODELAY\n"));
			    reply(ESOCK_IOCTL_ERR, "4s", fd, psx_errstr());
			} else {
			    reply(ESOCK_IOCTL_OK, "4", fd);
			}
			break;
		    default:
			DEBUGF(("Error: set_sock_opt - Not implemented\n"));
			sock_set_errno(ERRNO_OPNOTSUPP);
			reply(ESOCK_IOCTL_ERR, "4", fd, psx_errstr());
			break;
		    }
		    break;

		case ESOCK_LISTEN_CMD:
		    /* 
		     * ebuf = {cmd(1), intref(4), lport(2), ipstring(N), 0(1),
		     * 	       backlog(2), flags(N), 0(1)}
		     */
		    input("42s2s", &intref, &lport, &lipstring, &backlog,
			  &flags);
		    DEBUGF(("[LISTEN_CMD] intref = %d, port = %d, "
			   "ipstring = %s, backlog = %d, flags = %s\n", 
			   intref, lport, lipstring, backlog, flags));
		    
		    listensock = do_listen(lipstring, lport, backlog, &lport);
		    if(listensock == INVALID_FD) {
			reply(ESOCK_LISTEN_SYNC_ERR, "4s", intref, psx_errstr());
			break;
		    }
		    cp = new_connection(ESOCK_PASSIVE_LISTENING, listensock);
		    /* Flags may be an empty string */
		    length = strlen(flags);
		    cp->flags = esock_malloc(length + 1);
		    strcpy(cp->flags, flags);

		    cp->origin = ORIG_LISTEN;
		    if (esock_ssl_listen_init(cp) < 0) {
			DEBUGF(("esock_ssl_listen_init() failed.\n"));
			reply(ESOCK_LISTEN_SYNC_ERR, "4s", intref, 
			      ssl_errstr());
			close_and_remove_connection(cp);
			break;
		    }
		    DEBUGF(("-> PASSIVE_LISTENING (fd = %d)\n", listensock));
		    /* Publish listensock */
		    reply(ESOCK_LISTEN_REP, "442", intref, listensock,
			  ntohs(iserv_addr.sin_port));
		    break;

		case ESOCK_TRANSPORT_ACCEPT_CMD:
		    /* 
		     * ebuf =  { op(1), fd(4), flags(N), 0(1)} 
		     */
		    input("4s", &fd, &flags);
		    DEBUGF(("[TRANSPORT_ACCEPT_CMD] listenfd = %d, flags = %s\n", fd, 
			   flags));
		    cp = get_connection(fd);
		    if (cp) {
			 /* We store the flags in the listen socket's 
			  * connection, and overwrite previous flags.
			  */
			if ((length = strlen(flags)) > 0) {
			    if (cp->flags)
				cp->flags = esock_realloc(cp->flags, 
							  length + 1);
			    else
				cp->flags = esock_malloc(length + 1);
			    strcpy(cp->flags, flags);
			}
			if (cp->flags && cp->flags[0] != '\0') {
			    cp->acceptors++;
			    cp->state = ESOCK_ACTIVE_LISTENING; 
			    DEBUGF(("-> ACTIVE_LISTENING\n"));
			    break;
			}
			DEBUGF(("ERROR: flags empty\n"));
		    }
		    reply(ESOCK_TRANSPORT_ACCEPT_ERR, "4s", fd, "ebadf");
		    break;

		case ESOCK_SSL_ACCEPT_CMD:
		    input("4s", &fd, &flags);
		    DEBUGF(("[SSL_ACCEPT_CMD] fd = %d, flags = %s\n", fd, flags));
		    cp = get_connection(fd);
		    if (cp)
			cp->state = ESOCK_SSL_ACCEPT;
		    //reply(ESOCK_SSL_ACCEPT_REP, "4", fd);
		    break;

		case ESOCK_NOACCEPT_CMD:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    DEBUGF(("[NOACCEPT_CMD] listenfd = %d\n", fd));
		    cp = get_connection(fd);
		    if (cp && (--cp->acceptors <= 0)) {
			cp->acceptors = 0;
			cp->state = ESOCK_PASSIVE_LISTENING;
			esock_poll_clear_event(&pollfd, fd);
			DEBUGF(("-> PASSIVE_LISTENING\n"));
		    }
		    break;

		case ESOCK_PROXY_JOIN_CMD:
		    /*
		     * ebuf = {cmd(1), fd(4), portnum(2)}
		     *
		     * fd      - file descriptor of a connection in state
		     *           CONNECTED
		     * portnum - port number of the Erlang proxy peer 
		     */
		    input("42", &fd, &pport);
		    cp = get_connection(fd);
		    pp = get_proxy_by_peerport(pport);
		    if (cp && cp->state == ESOCK_CONNECTED && pp) {
			DEBUGF(("CONNECTED[PROXY_JOIN_CMD] fd = %d "
				"portnum = %d\n", fd, pport));
			cp->proxy = pp;
			pp->conn = cp;
			reply(ESOCK_PROXY_JOIN_REP, "4", fd);
			cp->state = ESOCK_JOINED;
			DEBUGF(("-> JOINED\n"));
			break;
		    }
		    if (!cp) {
			DEBUGF(("[PROXY_JOIN_CMD] ERROR: No connection "
				"having fd = %d\n", fd));
			reply(ESOCK_PROXY_JOIN_ERR, "4s", fd, "ebadsocket");
		    } else if (cp->state != ESOCK_CONNECTED) {
			DEBUGF(("%s[PROXY_JOIN_CMD] ERROR: Bad state: "
			       "fd = %d\n", connstr[cp->state], cp->fd));
			reply(ESOCK_PROXY_JOIN_ERR, "4s", fd, "ebadstate");
		    } else {
			DEBUGF(("ERROR: No proxy: fd = %d, pport = %d\n",
			       fd, pport));
			if (proxysock_err_cnt > 0) {
			    proxysock_err_cnt--;
			    reply(ESOCK_PROXY_JOIN_ERR, "4s", fd, 
				  esock_posix_str(proxysock_last_err));
			} else {
			    reply(ESOCK_PROXY_JOIN_ERR, "4s", fd, 
				  "enoproxysocket");
			}
			cp->state = ESOCK_DEFUNCT;
		    }
		    break;

		case ESOCK_DUMP_STATE_CMD:
		    dump_connections();
		  break;
 
                case ESOCK_SET_DEBUG_CMD:
                  /* 
                   * ebuf = {cmd(1), debug(1)}
                   */
                  input("1", &debug);
                  break;
		  
		case ESOCK_SET_DEBUGMSG_CMD:
                  /* 
                   * ebuf = {cmd(1), debugmsg(1)}
                   */
                  input("1", &debugmsg);
                  break;
		  
		default:
		    fprintf(stderr, "esock: default value in loop %c\n", 
			    *ebuf);
		    exit(EXIT_FAILURE);
		    break;
		}
	    }
	}

	/* Go through all connections that have their file descriptors
           set. */

	/* Note: We may remove the current connection (cp). Thus we
	 * must be careful not to read cp->next after cp has been
	 * removed.  */
	for (cp = next_polled_conn(connections, &cpnext, &pollfd, set_wq_fds); 
	     cp != NULL; 
	     cp = next_polled_conn(cpnext, &cpnext, &pollfd, set_wq_fds)
	     ) {

	    switch(cp->state) {

	    case ESOCK_PASSIVE_LISTENING:
		DEBUGF(("-----------------------------------\n"));
		fprintf(stderr, "esock: Got connect request while PASSIVE\n");
		exit(EXIT_FAILURE);
		break;
		
	    case ESOCK_ACTIVE_LISTENING:
		/* new connect from network */
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("ACTIVE_LISTENING - trying to accept on %d\n", 
		       cp->fd));
		length = sizeof(iserv_addr);
		msgsock = do_accept(cp->fd, (struct sockaddr*)&iserv_addr, 
				    (int*)&length);
		if(msgsock == INVALID_FD)  {
		    DEBUGF(("accept error: %s\n", psx_errstr()));
		    reply(ESOCK_TRANSPORT_ACCEPT_ERR, "4s", cp->fd, psx_errstr());
		    break;
		}
		SET_NONBLOCKING(msgsock);
		if (--cp->acceptors <= 0) {
		    cp->acceptors = 0;
		    cp->state = ESOCK_PASSIVE_LISTENING;
		    DEBUGF(("-> PASSIVE_LISTENING\n"));
		}
		DEBUGF(("server accepted connection on fd %d\n", msgsock));
		newcp = new_connection(ESOCK_TRANSPORT_ACCEPT, msgsock);
		newcp->origin = ORIG_ACCEPT;
		reply(ESOCK_TRANSPORT_ACCEPT_REP, "44", cp->fd, msgsock);
		newcp->listen_fd = cp->fd; /* Needed for ESOCK_ACCEPT_ERR  */
		length = strlen(cp->flags);
		/* XXX new flags are not needed */
		newcp->flags = esock_malloc(length + 1);
		strcpy(newcp->flags, cp->flags); /* XXX Why? */
		if (esock_ssl_accept_init(newcp, cp->opaque) < 0) {
		    cp->errstr = ssl_errstr();
		    break;
		}
		newcp->ssl_want = ESOCK_SSL_WANT_READ;
		break;

	    case ESOCK_SSL_ACCEPT:
		/* SSL accept handshake. msgsock is *not* published yet. */
		msgsock = cp->fd;
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("SSL_ACCEPT fd = %d\n", msgsock));
		if (cp->errstr != NULL) { /* this means we got an error in ssl_accept_init */
		    /* N.B.: The *listen fd* is reported. */
		    reply(ESOCK_SSL_ACCEPT_ERR, "4s", msgsock, cp->errstr);
		    close_and_remove_connection(cp);
		    break;
		}
		if (esock_ssl_accept(cp) < 0) {
		    if (sock_errno() != ERRNO_BLOCK) {
			/* Handshake failed. */
			reply(ESOCK_SSL_ACCEPT_ERR, "4s", msgsock,
			      ssl_errstr());
			DEBUGF(("ERROR: handshake: %s\n", ssl_errstr()));
			close_and_remove_connection(cp);
		    }
		} else {
		    /* SSL handshake successful: publish */
		    reply(ESOCK_SSL_ACCEPT_REP, "4", msgsock);
		    DEBUGF(("-> CONNECTED\n"));
		    DEBUGF((" Session was %sreused.\n", 
			    (esock_ssl_session_reused(cp)) ? "" : "NOT "));
		    cp->state = ESOCK_CONNECTED;
		}
		break;

	    case ESOCK_CONNECTED:
		/* Should not happen. We do not read or write until
		   the connection is in state JOINED. */
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("CONNECTED: Error: should not happen. fd = %d\n", 
			cp->fd));
		break;

	    case ESOCK_JOINED:
		/* 
		 * Reading from Proxy, writing to SSL 
		 */
		if (esock_poll_fd_isset_write(&pollfd, cp->fd)) {
		    /* If there is a write queue, write to ssl only */
		    if (cp->wq.len > 0) { 
			/* The write retry semantics of SSL_write in
			 * the OpenSSL package is strange. Partial
			 * writes never occur, only complete writes or
			 * failures.  A failure, however, still
			 * consumes all data written, although not all
			 * encrypted data could be written to the
			 * underlying socket. To retry a write we have
			 * to provide the same buf and length as in
			 * the original call, in our case rwbuf and
			 * the original buffer length. Hence the
			 * strange memcpy(). Note that wq.offset will
			 * always be zero when we use OpenSSL.  
			 */
			DEBUGF(("-----------------------------------\n"));
			DEBUGF(("JOINED: writing to ssl "
				"fd = %d, from write queue only, wc = %d\n", 
				cp->fd, cp->wq.len - cp->wq.offset));
			memcpy(rwbuf, cp->wq.buf, cp->wq.len - cp->wq.offset);

			/* esock_ssl_write sets cp->eof, cp->bp when return
			 * value is zero */
			wc = esock_ssl_write(cp, rwbuf, 
					     cp->wq.len - cp->wq.offset);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
				/* Assume broken SSL pipe */
				DEBUGF(("broken SSL pipe\n"));
				cp->bp = 1;
				shutdown(cp->proxy->fd, SHUTDOWN_READ);
				cp->proxy->eof = 1;
				if (JOINED_STATE_INVALID(cp)) {
				    leave_joined_state(cp);
				    break;
				}
			    }
			} else if (wc == 0) {
			    /* SSL broken pipe */
			    DEBUGF(("broken SSL pipe\n"));
			    cp->bp = 1;
			    shutdown(cp->proxy->fd, SHUTDOWN_READ);
			    cp->proxy->eof = 1;
			    if (JOINED_STATE_INVALID(cp)) {
				leave_joined_state(cp);
				break;
			    }
			} else {
			    cp->wq.offset += wc;
			    if (cp->wq.offset == cp->wq.len)
				cp->wq.len = 0;
			}
		    }
		} else if (esock_poll_fd_isset_read(&pollfd, cp->proxy->fd)) {
		    /* Read from proxy and write to SSL */
		    DEBUGF(("-----------------------------------\n"));
		    DEBUGF(("JOINED: reading from proxy, "
			   "proxyfd = %d\n", cp->proxy->fd));
		    cc = sock_read(cp->proxy->fd, rwbuf, RWBUFLEN); 
		    DEBUGF(("read from proxyfd = %d, cc = %d\n", 
			   cp->proxy->fd, cc));
		    if (cc > 0) {
			/* esock_ssl_write sets cp->eof, cp->bp when return
			 * value is zero */
			wc = esock_ssl_write(cp, rwbuf, cc);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
				/* Assume broken pipe */
				DEBUGF(("broken SSL pipe\n"));
				cp->bp = 1;
				shutdown(cp->proxy->fd, SHUTDOWN_READ);
				cp->proxy->eof = 1;
				if (JOINED_STATE_INVALID(cp)) {
				    leave_joined_state(cp);
				    break;
				}
			    } else {
				/* add to write queue */
				DEBUGF(("adding all to write queue "
					"%d bytes\n", cc));
				ensure_write_queue(&cp->wq, cc);
				memcpy(cp->wq.buf, rwbuf, cc);
				cp->wq.len = cc;
				cp->wq.offset = 0;
			    }
			} else if (wc == 0) {
				/* Broken SSL pipe */
				DEBUGF(("broken SSL pipe\n"));
				cp->bp = 1;
				shutdown(cp->proxy->fd, SHUTDOWN_READ);
				cp->proxy->eof = 1;
				if (JOINED_STATE_INVALID(cp)) {
				    leave_joined_state(cp);
				    break;
				}
			} else if (wc < cc) {
			    /* add remainder to write queue */
			    DEBUGF(("adding remainder to write queue "
				    "%d bytes\n", cc - wc));
			    ensure_write_queue(&cp->wq, cc - wc);
			    memcpy(cp->wq.buf, rwbuf + wc, cc - wc);
			    cp->wq.len = cc - wc;
			    cp->wq.offset = 0;
			} 
		    } else {
			/* EOF proxy or error */
		       DEBUGF(("proxy eof or error %d\n", errno));
			cp->proxy->eof = 1;
			if (cp->wq.len == 0) {
			    esock_ssl_shutdown(cp);
			    cp->bp = 1;
			}
			if (JOINED_STATE_INVALID(cp)) {
			    leave_joined_state(cp);
			    break;
			}
		    }
		}
		/* 
		 * Reading from SSL, writing to proxy 
		 */
		if (esock_poll_fd_isset_write(&pollfd, cp->proxy->fd)) {
		    /* If there is a write queue, write to proxy only */
		    if (cp->proxy->wq.len > 0) {
			DEBUGF(("-----------------------------------\n"));
			DEBUGF(("JOINED: writing to proxyfd = %d, "
				"from write queue only, wc = %d\n", 
				cp->proxy->fd, cp->proxy->wq.len - 
				cp->proxy->wq.offset));
			wc = sock_write(cp->proxy->fd, cp->proxy->wq.buf + 
					cp->proxy->wq.offset,
					cp->proxy->wq.len - 
					cp->proxy->wq.offset);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
				/* Assume broken pipe */
				DEBUGF(("broken proxy pipe\n"));
				cp->proxy->bp = 1;
				/* There is no SSL shutdown for read */
				cp->eof = 1;
				if (JOINED_STATE_INVALID(cp)) {
				    leave_joined_state(cp);
				    break;
				}
			    }
			} else {
			    cp->proxy->wq.offset += wc;
			    if (cp->proxy->wq.offset == cp->proxy->wq.len)
				cp->proxy->wq.len = 0;
			}
		    }
		} else if (esock_poll_fd_isset_read(&pollfd, cp->fd)) {
		    /* Read from SSL and write to proxy */
		    DEBUGF(("-----------------------------------\n"));
		    DEBUGF(("JOINED: read from ssl fd = %d\n",
			   cp->fd));
		    cc = esock_ssl_read(cp, rwbuf, RWBUFLEN);
		    DEBUGF(("read from fd = %d, cc = %d\n", cp->fd, cc));
		    if (cc > 0) {
			wc = sock_write(cp->proxy->fd, rwbuf, cc);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
				DEBUGF(("broken proxy pipe\n"));
				/* Assume broken pipe */
				cp->proxy->bp = 1;
				/* There is no SSL shutdown for read */
				cp->eof = 1;
				if (JOINED_STATE_INVALID(cp)) {
				    leave_joined_state(cp);
				    break;
				}
			    } else {
				/* add all to write queue */
				DEBUGF(("adding to write queue %d bytes\n", 
					cc));
				ensure_write_queue(&cp->proxy->wq, cc);
				memcpy(cp->proxy->wq.buf, rwbuf, cc);
				cp->proxy->wq.len = cc;
				cp->proxy->wq.offset = 0;
			    }
			} else if (wc < cc) {
			    /* add to write queue */
			    DEBUGF(("adding to write queue %d bytes\n",
				    cc - wc));
			    ensure_write_queue(&cp->proxy->wq, cc - wc);
			    memcpy(cp->proxy->wq.buf, rwbuf + wc, cc - wc);
			    cp->proxy->wq.len = cc - wc;
			    cp->proxy->wq.offset = 0;
			} 
		    } else if (cc == 0) {
			/* SSL eof */
			DEBUGF(("SSL eof\n"));
			cp->eof = 1;
			if (cp->proxy->wq.len == 0) {
			    shutdown(cp->proxy->fd, SHUTDOWN_WRITE);
			    cp->proxy->bp = 1;
			}
			if (JOINED_STATE_INVALID(cp)) {
			    leave_joined_state(cp);
			    break;
			}
		    } else {
			/* This may very well happen when reading from SSL. */
			DEBUGF(("NOTE: readmask set, cc < 0,  fd = %d, "
				"is ok\n", cp->fd));
		    }
		}
		break;

	    case ESOCK_SSL_SHUTDOWN:
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("SSL_SHUTDOWN: fd = %d\n", cp->fd));
		do_shutdown(cp);
		break;

	    case ESOCK_DEFUNCT:
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("DEFUNCT: ERROR: should not happen. fd = %d\n", 
			cp->fd));
		break;

	    case ESOCK_WAIT_CONNECT:
		/* New connection shows up */
		connectsock = cp->fd;/* Is published */
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("WAIT_CONNECT fd = %d\n", connectsock));

		/* If the connection did succeed it's possible to
		 * fetch the peer name (UNIX); or failure shows in
		 * exceptmask (WIN32). Sorry for the mess below, but
		 * we have to have balanced paren's in #ifdefs in
		 * order not to confuse Emacs' indentation.  */
		length = sizeof(iserv_addr);
		if (
#ifdef __WIN32__
		    esock_poll_fd_isset_exception(&pollfd, connectsock)
#else
		    getpeername(connectsock, (struct sockaddr *)&iserv_addr, 
				&length) < 0
#endif
		    ) {
		    sock_set_errno(ERRNO_CONNREFUSED);
		    DEBUGF(("connect error: %s\n", psx_errstr()));
		    reply(ESOCK_CONNECT_ERR, "4s", connectsock, psx_errstr());
		    cp->state = ESOCK_DEFUNCT;
		    break;
		}
		if (esock_ssl_connect_init(cp) < 0) {
		    DEBUGF(("esock_ssl_connect_init() failed\n"));
		    reply(ESOCK_CONNECT_ERR, "4s", connectsock, ssl_errstr());
		    cp->state = ESOCK_DEFUNCT;
		    break;
		}
		DEBUGF(("-> SSL_CONNECT\n"));
		cp->state = ESOCK_SSL_CONNECT;
		cp->ssl_want = ESOCK_SSL_WANT_WRITE;
		break;

	    case ESOCK_SSL_CONNECT:
		/* SSL connect handshake. connectsock is published. */
		connectsock = cp->fd;
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("SSL_CONNECT fd = %d\n", connectsock));
		if (esock_ssl_connect(cp) < 0) {
		    if (sock_errno() != ERRNO_BLOCK) {
			/* Handshake failed */
			DEBUGF(("ERROR: handshake: %s\n", ssl_errstr()));
			reply(ESOCK_CONNECT_ERR, "4s", connectsock,
			      ssl_errstr());
			cp->state = ESOCK_DEFUNCT;
		    }
		} else {
		    /* SSL connect handshake successful */
		    DEBUGF(("-> CONNECTED\n"));
		    reply(ESOCK_CONNECT_REP, "4", connectsock);
		    cp->state = ESOCK_CONNECTED;
		}
		break;

	    default:
		DEBUGF(("ERROR: Connection in unknown state.\n"));
	    }
	}
   }
}

static int set_poll_conns(Connection *cp, EsockPoll *ep, int verbose)
{
    int i = 0;
    
    if (verbose)
	DEBUGF(("MASKS SET FOR FD: "));
    while (cp) {
	switch (cp->state) {
	case ESOCK_ACTIVE_LISTENING:
	    if (verbose)
		DEBUGF(("%d (read) ", cp->fd));
	    esock_poll_fd_set_read(ep, cp->fd);
	    break;
	case ESOCK_WAIT_CONNECT:
	    if (verbose)
		DEBUGF(("%d (write) ", cp->fd));
	    esock_poll_fd_set_write(ep, cp->fd);
#ifdef __WIN32__
	    esock_poll_fd_set_exception(ep, cp->fd); /* Failure shows in exceptions */
#endif
	    break;
	case ESOCK_SSL_CONNECT:
	case ESOCK_SSL_ACCEPT:
	    if (cp->ssl_want == ESOCK_SSL_WANT_READ) {
		if (verbose)
		    DEBUGF(("%d (read) ", cp->fd));
		esock_poll_fd_set_read(ep, cp->fd);
	    } else if (cp->ssl_want == ESOCK_SSL_WANT_WRITE) {
		if (verbose)
		    DEBUGF(("%d (write) ", cp->fd));
		esock_poll_fd_set_write(ep, cp->fd);
	    }
	    break;
	case ESOCK_JOINED:
	    if (!cp->bp) {
		if (cp->wq.len) {
		    if (verbose)
			DEBUGF(("%d (write) ", cp->fd));
		    esock_poll_fd_set_write(ep, cp->fd);
		} else if (!cp->proxy->eof) {
		    if (verbose)
			DEBUGF(("%d (read) ", cp->proxy->fd));
		    esock_poll_fd_set_read(ep, cp->proxy->fd);
		}
	    }
	    if (!cp->proxy->bp) {
		if (cp->proxy->wq.len) {
		    if (verbose)
			DEBUGF(("%d (write) ", cp->proxy->fd));
		    esock_poll_fd_set_write(ep, cp->proxy->fd);
		} else if (!cp->eof) {
		    if (verbose)
			DEBUGF(("%d (read) ", cp->fd));
		    esock_poll_fd_set_read(ep, cp->fd);
		}
	    }
	    break;
	case ESOCK_SSL_SHUTDOWN:
	    if (cp->ssl_want == ESOCK_SSL_WANT_READ) {
		if (verbose)
		    DEBUGF(("%d (read) ", cp->fd));
		esock_poll_fd_set_read(ep, cp->fd);
	    } else if (cp->ssl_want == ESOCK_SSL_WANT_WRITE) {
		if (verbose)
		    DEBUGF(("%d (write) ", cp->fd));
		esock_poll_fd_set_write(ep, cp->fd);
	    }
	    break;
	default:
	    break;
	}
	i++;
	cp = cp->next;
    }
    if (verbose)
	DEBUGF(("\n"));
    return i;
}


static Connection *next_polled_conn(Connection *cp, Connection **cpnext,
				    EsockPoll *ep, int set_wq_fds)
{
    while(cp) {
	if (esock_poll_fd_isset_read(ep, cp->fd) ||
	    (cp->proxy && esock_poll_fd_isset_read(ep, cp->proxy->fd)) ||
	    (esock_poll_fd_isset_write(ep, cp->fd)) ||
	    (cp->proxy && esock_poll_fd_isset_write(ep, cp->proxy->fd))
#ifdef __WIN32__
	    || esock_poll_fd_isset_exception(ep, cp->fd) /* Connect failure in WIN32 */
#endif
	    || (set_wq_fds && (cp->wq.len || 
			       (cp->proxy && cp->proxy->wq.len)))
	    || cp->errstr != NULL) {
	    *cpnext = cp->next;
	    return cp;
	}
	cp = cp->next;
    }
    *cpnext = NULL;
    return NULL;
}

static void leave_joined_state(Connection *cp)
{
    shutdown(cp->proxy->fd, SHUTDOWN_ALL);
    if (((cp->bp || cp->eof) && cp->clean) ||
	(!cp->bp && !cp->eof)) {
	DEBUGF(("-> SSL_SHUTDOWN\n"));
	cp->state = ESOCK_SSL_SHUTDOWN;
	cp->ssl_want = ESOCK_SSL_WANT_WRITE;
	do_shutdown(cp);
    } else if (cp->close) {
	DEBUGF(("-> (removal)\n"));
	close_and_remove_connection(cp);
    } else {
	DEBUGF(("-> DEFUNCT\n"));
	cp->state = ESOCK_DEFUNCT;
    }
}

/* We are always in state SHUTDOWN here */
static void do_shutdown(Connection *cp)
{
    int ret;

    ret = esock_ssl_shutdown(cp); 
    if (ret < 0) {
	if (sock_errno() == ERRNO_BLOCK) {
	    return;
	} else {
	    /* Something is wrong -- close and remove or move to DEFUNCT */
	    DEBUGF(("Error in SSL shutdown\n"));
	    if (cp->close) {
		DEBUGF(("-> (removal)\n"));
		close_and_remove_connection(cp);
	    } else {
		DEBUGF(("-> DEFUNCT\n"));
		cp->state = ESOCK_DEFUNCT;
	    }
	}
    } else if (ret == 0) {
	/* `close_notify' has been sent. Wait for reception of
           same. */
	return; 
    } else if (ret == 1) {
	/* `close_notify' has been sent, and received. */
	if (cp->close) {
	    DEBUGF(("-> (removal)\n"));
	    close_and_remove_connection(cp);
	} else {
	    DEBUGF(("-> DEFUNCT\n"));
	    cp->state = ESOCK_DEFUNCT;
	}
    }
}

static void close_and_remove_connection(Connection *cp)
{
    safe_close(cp->fd);
    remove_connection(cp);
}

static int reply(int cmd, char *fmt, ...)
{
    static unsigned char replybuf[MAXREPLYBUF];
    unsigned char *buf = replybuf;
    va_list args;
    int len;

    va_start(args, fmt);
    len = put_pars(NULL, fmt, args);
    va_end(args);
    len++;
    if (len > sizeof(replybuf))
	buf = esock_malloc(len);

    PUT_INT8(cmd, buf);
    va_start(args, fmt);
    (void) put_pars(buf + 1, fmt, args);
    va_end(args);
    write_ctrl(buf, len);
    if (buf != replybuf)
	esock_free(buf);
    return len;
}

static int input(char *fmt, ...)
{
    va_list args;
    int len;

    va_start(args, fmt);
    len = get_pars(ebuf + 1, fmt, args);
    va_end(args);
    return len + 1;
}

static int put_pars(unsigned char *buf, char *fmt, va_list args)
{
    char *s, *str, *bin;
    int val, len, pos = 0;

    s = fmt;
    while (*s) {
	switch (*s) {
	case '1':
	    val = va_arg(args, int);
	    if (buf)
		PUT_INT8(val, buf + pos);
	    pos++;
	    break;
	case '2':
	    val = va_arg(args, int);
	    if (buf)
		PUT_INT16(val, buf + pos);
	    pos += 2;
	    break;
	case '4':
	    val = va_arg(args, int);
	    if (buf)
		PUT_INT32(val, buf + pos);
	    pos += 4;
	    break;
	case 's':		/* string */
	    str = va_arg(args, char *);
	    if (buf)
		strcpy((char *)(buf + pos), str);
	    pos += strlen(str) + 1;
	    break;
	case 'b':		/* binary */
	    len = va_arg(args, int);
	    if (buf)
		PUT_INT32(len, buf + pos);
	    pos += 4;
	    bin = va_arg(args, char *);
	    if (buf)
		memcpy(buf + pos, bin, len);
	    pos += len;
	    break;
	default:
	    fprintf(stderr, "esock: Invalid format character: %c\n", *s);
	    exit(EXIT_FAILURE);
	    break;
	}
	s++;
    }
    return pos;
}


static int get_pars(unsigned char *buf, char *fmt, va_list args)
{
    int *ip;
    char *s, **strp, **bin;
    int pos = 0;

    s = fmt;
    while (*s) {
	switch (*s) {
	case '1':
	    ip = va_arg(args, int *);
	    *ip = GET_INT8(buf + pos);
	    pos++;
	    break;
	case '2':
	    ip = va_arg(args, int *);
	    *ip = GET_INT16(buf + pos);
	    pos += 2;
	    break;
	case '4':
	    ip = va_arg(args, int *);
	    *ip = GET_INT32(buf + pos);
	    pos += 4;
	    break;
	case 's':
	    strp = va_arg(args, char **);
	    *strp = (char *)(buf + pos);
	    pos += strlen(*strp) + 1;
	    break;
	case 'b':
	    ip = va_arg(args, int *);
	    *ip = GET_INT32(buf + pos);
	    pos += 4;
	    bin = va_arg(args, char **);
	    *bin = (char *)(buf + pos);
	    pos += *ip;
	    break;
	default:
	    fprintf(stderr, "esock: Invalid format character: %c\n", *s);
	    exit(EXIT_FAILURE);
	    break;
	}
	s++;
    }
    return pos;
}

static FD do_connect(char *lipstring, int lport, char *fipstring, int fport)
{
    struct sockaddr_in sock_addr;
    long inaddr;
    FD fd;
   
    if ((fd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_FD) {
	DEBUGF(("Error calling socket()\n"));
	return fd;
    }
    if (check_num_sock_fds(fd) < 0) 
	return INVALID_FD;
    DEBUGF(("  fd = %d\n", fd));

    /* local */
    if ((inaddr = inet_addr(lipstring)) == INADDR_NONE) {
	DEBUGF(("Error in inet_addr(): lipstring = %s\n", lipstring));
	safe_close(fd);
	sock_set_errno(ERRNO_ADDRNOTAVAIL);
	return INVALID_FD;
    }
    memset(&sock_addr, 0, sizeof(sock_addr));
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_addr.s_addr = inaddr;
    sock_addr.sin_port = htons(lport);
    if(bind(fd, (struct sockaddr*) &sock_addr, sizeof(sock_addr)) < 0) {
	DEBUGF(("Error in bind()\n"));
	safe_close(fd);
	/* XXX Set error code for bind error */
	return INVALID_FD;
    }

    /* foreign */
    if ((inaddr = inet_addr(fipstring)) == INADDR_NONE) {
	DEBUGF(("Error in inet_addr(): fipstring = %s\n", fipstring));
	safe_close(fd);
	sock_set_errno(ERRNO_ADDRNOTAVAIL);
	return INVALID_FD;
    }
    memset(&sock_addr, 0, sizeof(sock_addr));
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_addr.s_addr = inaddr;
    sock_addr.sin_port = htons(fport);

    SET_NONBLOCKING(fd);

    if(connect(fd, (struct sockaddr*)&sock_addr, sizeof(sock_addr)) < 0) {
	if (sock_errno() != ERRNO_PROGRESS && /* UNIX */
	    sock_errno() != ERRNO_BLOCK) { /* WIN32 */
	    DEBUGF(("Error in connect()\n"));
	    safe_close(fd);
	    return INVALID_FD;
	}
    }
    return fd;
}

static FD do_listen(char *ipstring, int lport, int backlog, int *aport)
{
    static int one = 1;		/* Type must be int, not long */
    struct sockaddr_in sock_addr;
    long inaddr;
    int length;
    FD fd;
    
    if ((fd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_FD) {
	DEBUGF(("Error calling socket()\n"));
	return fd;
    }
    if (check_num_sock_fds(fd) < 0) 
	return INVALID_FD;
    DEBUGF(("  fd = %d\n", fd));
    if ((inaddr = inet_addr(ipstring)) == INADDR_NONE) {
	DEBUGF(("Error in inet_addr(): ipstring = %s\n", ipstring));
	safe_close(fd);
	sock_set_errno(ERRNO_ADDRNOTAVAIL);
	return INVALID_FD;
    }
    memset(&sock_addr, 0, sizeof(sock_addr));
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_addr.s_addr = inaddr;
    sock_addr.sin_port = htons(lport);

    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (void *)&one, sizeof(one));

    if(bind(fd, (struct sockaddr*) &sock_addr, sizeof(sock_addr)) < 0) {
	DEBUGF(("Error in bind()\n"));
	safe_close(fd);
	return INVALID_FD;
    }
    if (listen(fd, backlog) < 0) {
	DEBUGF(("Error in listen()\n"));
	safe_close(fd);
	return INVALID_FD;
    }
    /* find out assigned local port number */
    length = sizeof(sock_addr);
    if (getsockname(fd, (struct sockaddr *)&sock_addr, &length) < 0) {
	DEBUGF(("Error in getsockname()\n"));
	safe_close(fd);
	return INVALID_FD;
    }
    if (aport)
	*aport = ntohs(sock_addr.sin_port);
    return fd;
}

static FD do_accept(FD listensock, struct sockaddr *saddr, int *len)
{
    FD fd;

    if ((fd = accept(listensock, saddr, len)) == INVALID_FD) {
	DEBUGF(("Error calling accept()\n"));
	return fd;
    }
    if (check_num_sock_fds(fd) < 0) 
	return INVALID_FD;
    return fd;
}

static Connection *new_connection(int state, FD fd)
{
    Connection *cp;
    
    if (!(cp = esock_malloc(sizeof(Connection))))
	return NULL;
    cp->state = state;
    cp->acceptors = 0;
    cp->fd = fd;
    cp->listen_fd = INVALID_FD;
    cp->proxy = NULL;
    cp->opaque = NULL;
    cp->ssl_want = 0;
    cp->eof = 0;
    cp->bp = 0;
    cp->clean = 0;		/* XXX Used? */
    cp->close = 0;
    cp->origin = -1;
    cp->flags = NULL;
    cp->logfp = NULL;
    cp->wq.size = 0;
    cp->wq.buf = NULL;
    cp->wq.len = 0;
    cp->wq.offset = 0;
    cp->next = connections;
    cp->errstr = NULL;
    connections = cp;
    return cp;
}


static void print_connections(void)
{
    if (debug) {
	Connection *cp = connections;
	DEBUGF(("CONNECTIONS:\n"));
	while (cp) {
	    if (cp->state == ESOCK_JOINED) {
		DEBUGF((" - %s [%8p] (origin = %s)\n"
			"       (fd = %d, eof = %d, wq = %d, bp = %d)\n"
			"       (proxyfd = %d, eof = %d, wq = %d, bp = %d)\n", 
		       connstr[cp->state], cp, originstr[cp->origin],
			cp->fd, cp->eof, cp->wq.len, cp->bp,
			cp->proxy->fd, cp->proxy->eof, cp->proxy->wq.len, 
			cp->proxy->bp));
	    } else if (cp->state == ESOCK_ACTIVE_LISTENING) {
		DEBUGF((" - %s [%8p] (fd = %d, acceptors = %d)\n", 
		       connstr[cp->state], cp, cp->fd, cp->acceptors));
	    } else {
 		DEBUGF((" - %s [%8p] (fd = %d)\n", connstr[cp->state], cp, 
		       cp->fd));
	    }
	    cp= cp->next;
	}
    }
}

static void dump_connections(void)
{
    Connection *cp = connections;
    Proxy      *pp = proxies;
    time_t t = time(NULL);
    int length = 0;
    struct sockaddr_in iserv_addr;

    __debugprintf("CONNECTIONS %s", ctime(&t));
    while (cp) {
	if (cp->state == ESOCK_JOINED) {
	    __debugprintf(" - %s [%8p] (origin = %s)\n"
			  "       (fd = %d, eof = %d, wq = %d, bp = %d), close = %d\n"
			  "       (proxyfd = %d, eof = %d, wq = %d, bp = %d)\n", 
			  connstr[cp->state], cp, originstr[cp->origin],
			  cp->fd, cp->eof, cp->wq.len, cp->bp, cp->close,
			  cp->proxy->fd, cp->proxy->eof, cp->proxy->wq.len, 
			  cp->proxy->bp);
	} else if (cp->state == ESOCK_ACTIVE_LISTENING) {
	    __debugprintf(" - %s [%8p] (fd = %d, acceptors = %d)\n", 
			  connstr[cp->state], cp, cp->fd, cp->acceptors);
	} else {
	    __debugprintf(" - %s [%8p] (fd = %d)\n", connstr[cp->state], cp, 
			  cp->fd);
	}
	length = sizeof(iserv_addr);
	if ((cp->state == ESOCK_ACTIVE_LISTENING) ||
	    (cp->state == ESOCK_PASSIVE_LISTENING)) {
	    getsockname(cp->fd, (struct sockaddr *) &iserv_addr, &length);
	    __debugprintf("       (ip = %s, port = %d)\n",
			  inet_ntoa(iserv_addr.sin_addr),
			  ntohs(iserv_addr.sin_port));
	}
	else {
	    getsockname(cp->fd, (struct sockaddr *) &iserv_addr, &length);
	    __debugprintf("       (local_ip = %s, local_port = %d)\n",
			  inet_ntoa(iserv_addr.sin_addr),
			  ntohs(iserv_addr.sin_port));
	    length = sizeof(iserv_addr);
	    getpeername(cp->fd, (struct sockaddr *) &iserv_addr, &length);
	    __debugprintf("       (remote_ip = %s, remote_port = %d)\n",
			  inet_ntoa(iserv_addr.sin_addr),
			  ntohs(iserv_addr.sin_port));
	}
	cp=cp->next;
    }
  
    __debugprintf("PROXIES\n");
    while (pp) {
	__debugprintf(" - fd = %d [%8p] (external_fd = %d, peer_port = %d,"
		      " eof = %d)\n", pp->fd, pp, pp->conn->fd, pp->peer_port,
		      pp->eof);
    
	pp= pp->next;
    }
}

static Connection *get_connection(FD fd)
{
    Connection *cp = connections;
    
    while(cp) {
	if(cp->fd == fd)
	    return cp;
	cp = cp->next;
    }
    return NULL;
}

/* 
 * Remove a connection from the list of connection, close the proxy
 * socket and free all resources. The main socket (fd) is *not* 
 * closed here, because the closing of that socket has to be synchronized
 * with the Erlang process controlling this port program.
 */
static void remove_connection(Connection *conn)
{
    Connection **prev = &connections;
    Connection *cp = connections; 
    
    while (cp) {
	if(cp == conn) {
	    DEBUGF(("remove_connection: fd = %d\n", cp->fd));
	    esock_ssl_free(cp);	/* frees cp->opaque only */
	    esock_free(cp->flags);
	    closelog(cp->logfp); /* XXX num_sock_fds */
	    esock_free(cp->wq.buf);
	    if (cp->proxy) {
		safe_close(cp->proxy->fd);
		remove_proxy(cp->proxy);
	    }
	    *prev = cp->next;
	    esock_free(cp);
	    return;
	}
	prev = &cp->next;
	cp = cp->next;
    }
}

static Proxy *get_proxy_by_peerport(int port)
{
    Proxy *p = proxies;

    while(p) {
	if (p->peer_port == port)
	    return p;
	p = p->next;
    }
    return NULL;
}

static Proxy *new_proxy(FD fd)
{
    Proxy *p;

    if (!(p = esock_malloc(sizeof(Proxy))))
	return NULL;

    p->fd = fd;
    p->peer_port = -1;
    p->eof = 0;
    p->bp = 0;
    p->conn = NULL;
    p->wq.size = 0;
    p->wq.buf = NULL;
    p->wq.len = 0;
    p->wq.offset = 0;
    p->next = proxies;
    proxies = p;
    return p;
}

static void remove_proxy(Proxy *proxy)
{
    Proxy *p = proxies, **pp = &proxies;

    while(p) {
	if (p == proxy) {
	    DEBUGF(("remove_proxyfd = %d\n", p->fd));
	    esock_free(p->wq.buf);
	    *pp = p->next;
	    esock_free(p);
	    return;
	}
	pp = &p->next;
	p = p->next;
    }
}

static int check_num_sock_fds(FD fd) 
{
    num_sock_fds++;		/* fd is valid */
#ifdef USE_SELECT
    if (num_sock_fds > FD_SETSIZE) {
	num_sock_fds--;
	sock_set_errno(ERRNO_MFILE);
	safe_close(fd);
	return -1;
    }
#endif
    return 0;
}

static void safe_close(FD fd)
{
    int err;

    err = sock_errno();
    DEBUGF(("safe_close fd = %d\n", fd));
    if (sock_close(fd) < 0) {
	DEBUGF(("safe_close failed\n"));
    } else {
	num_sock_fds--;
    }
    sock_set_errno(err);
}

static void clean_up(void)
{
    Connection *cp, *cpnext;
    Proxy *pp, *ppnext;

    cp = connections;
    while (cp) {
	safe_close(cp->fd);
	cpnext = cp->next;
	remove_connection(cp);
	cp = cpnext;
    }
    
    pp = proxies;
    while (pp) {
	safe_close(pp->fd);
	ppnext = pp->next;
	remove_proxy(pp);
	pp = ppnext;
    }
}

static void ensure_write_queue(WriteQueue *wq, int size)
{
    if (wq->size < size) {
	wq->buf = esock_realloc(wq->buf, size);
	wq->size = size;
    }
}







