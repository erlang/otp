/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

/*
 * Purpose: Special purpouse Unix domain socket driver for distribution.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>

#define HAVE_UIO_H
#include "erl_driver.h"

#define DEBUG
/*#define HARDDEBUG 1*/
/*
** Some constants/macros
*/

#ifdef HARDDEBUG
#define DEBUGF(P) debugf P
#include <stdarg.h>
static void debugf(char *str, ...)
{
    va_list ap;
    va_start(ap,str);
    fprintf(stderr,"Uds_drv debug: ");
    vfprintf(stderr,str, ap);
    fprintf(stderr,"\r\n");
    va_end(ap);
}
#ifndef DEBUG
#define DEBUG 1
#endif
#else
#define DEBUGF(P)
#endif


#ifdef DEBUG
#define ASSERT(X) 							\
do {									\
    if (!(X)) {								\
	fprintf(stderr,"Assertion (%s) failed at line %d file %s\r\n", #X, \
		__LINE__, __FILE__); 					\
	exit(1);							\
    } 									\
} while(0)
#define ASSERT_NONBLOCK(FD) ASSERT(fcntl((FD), F_GETFL, 0) & O_NONBLOCK)
#else
#define ASSERT(X)
#define ASSERT_NONBLOCK(FD)
#endif

    

#define SET_NONBLOCKING(FD)			\
     fcntl((FD), F_SETFL, 			\
	   fcntl((FD), F_GETFL, 0) | O_NONBLOCK)

#define ALLOC(X) my_malloc(X)
#define REALLOC(P,X) my_realloc(P,X)
#define FREE(X) driver_free(X)

#define CHUNK_SIZE 256

#define DIST_MAGIC_RECV_TAG 100

/*
** The max length of an I/O vector seams to be impossible to find
** out (?), so this is just a value known to work on solaris.
*/ 
#define IO_VECTOR_MAX 16

#define SOCKET_PATH "/tmp/erlang"
#define LOCK_SUFFIX ".lock"

#define NORMAL_READ_FAILURE -1
#define SEVERE_READ_FAILURE -2
#define EOF_READ_FAILURE    -3

/*
** Internal structures
*/

#define HEADER_LENGTH 4

typedef enum { 
    portTypeUnknown,      /* An uninitialized port */
    portTypeListener,     /* A listening port/socket */
    portTypeAcceptor,     /* An intermediate stage when accepting
			     on a listen port */
    portTypeConnector,    /* An intermediate stage when connecting */
    portTypeCommand,      /* A connected open port in command mode */
    portTypeIntermediate, /* A connected open port in special half 
			     active mode */
    portTypeData          /* A connectec open port in data mode */ 
} PortType;

typedef unsigned char Byte;
typedef unsigned int Word;

typedef struct uds_data {
    int fd;                   /* File descriptor */
    ErlDrvPort port;          /* The port identifier */
    int lockfd;               /* The file descriptor for a lock file in 
				 case of listen sockets */
    Byte creation;            /* The creation serial derived from the 
				 lockfile */
    PortType type;            /* Type of port */
    char *name;               /* Short name of socket for unlink */
    Word sent;                /* Messages sent */
    Word received;            /* Messages received */
    struct uds_data *partner; /* The partner in an accept/listen pair */
    struct uds_data *next;    /* Next structure in list */

    /* The input buffer and it's data */
    int buffer_size;          /* The allocated size of the input buffer */
    int buffer_pos;           /* Current position in input buffer */
    int header_pos;           /* Where the current header is in the 
				 input buffer */
    Byte *buffer;            /* The actual input buffer */
} UdsData;

/*
** Interface routines
*/
static ErlDrvData uds_start(ErlDrvPort port, char *buff);
static void uds_stop(ErlDrvData handle);
static void uds_command(ErlDrvData handle, char *buff, int bufflen);
static void uds_input(ErlDrvData handle, ErlDrvEvent event);
static void uds_output(ErlDrvData handle, ErlDrvEvent event);
static void uds_finish(void);
static int uds_control(ErlDrvData handle, unsigned int command, 
		       char* buf, int count, char** res, int res_size);
static void uds_stop_select(ErlDrvEvent event, void*);

/* 
** Local helpers forward declarations
*/

static void uds_command_listen(UdsData *ud, char *buff, int bufflen);
static void uds_command_accept(UdsData *ud, char *buff, int bufflen); 
static void uds_command_connect(UdsData *ud, char *buff, int bufflen); 
 
static void do_stop(UdsData *ud, int shutting_down);
static void do_send(UdsData *ud, char *buff, int bufflen); 
static void do_recv(UdsData *ud);

static int report_control_error(char **buffer, int buff_len, 
				char *error_message);
static int  send_out_queue(UdsData *ud);
static int buffered_read_package(UdsData *ud, char **result);
static int read_at_least(UdsData *ud, int num);
static int get_packet_length(char *b);
static void put_packet_length(char *b, int len);
static void *my_malloc(size_t size);
static void *my_realloc(void *optr, size_t size);
static int try_lock(char *sockname, Byte *p_creation);
static int ensure_dir(char *path);
static void do_unlink(char *name);

/*
** Global data
*/

/* The driver entry */
ErlDrvEntry uds_driver_entry = {
    NULL,		   /* init, N/A */
    uds_start,             /* start, called when port is opened */
    uds_stop,              /* stop, called when port is closed */
    uds_command,           /* output, called when erlang has sent */
    uds_input,             /* ready_input, called when input descriptor 
			      ready */
    uds_output,            /* ready_output, called when output 
			      descriptor ready */
    "uds_drv",             /* char *driver_name, the argument to open_port */
    uds_finish,            /* finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    uds_control,           /* control, port_control callback */
    NULL,                  /* timeout, called on timeouts */
    NULL,                  /* outputv, vector output interface */
    NULL,                  /* ready_async */
    NULL,                  /* flush */
    NULL,                  /* call */
    NULL,                  /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,	/* ERL_DRV_FLAGs */
    NULL,
    NULL,                  /* process_exit */
    uds_stop_select
};

/* Beginning of linked list of ports */
static UdsData *first_data;

/*
**
** Driver interface routines
**
*/

/*
** Driver initialization routine
*/
DRIVER_INIT(uds_drv)
{
    first_data = NULL;
    return &uds_driver_entry;
}

/*
** A port is opened, we need no information whatsoever about the socket 
** at this stage.
*/
static ErlDrvData uds_start(ErlDrvPort port, char *buff)
{
    UdsData *ud;
    
    ud = ALLOC(sizeof(UdsData));
    ud->fd = -1;
    ud->lockfd = -1;
    ud->creation = 0;
    ud->port = port;
    ud->type = portTypeUnknown;
    ud->name = NULL;
    ud->buffer_size = 0;
    ud->buffer_pos = 0;
    ud->header_pos = 0;
    ud->buffer = NULL;
    ud->sent = 0;
    ud->received = 0;
    ud->partner = NULL;
    ud->next = first_data;
    first_data = ud;
    
    return((ErlDrvData) ud);
}

/*
** Close the socket/port and free up
*/
static void uds_stop(ErlDrvData handle) 
{
    do_stop((UdsData *) handle, 0);
}

/*
** Command interface, operates in two modes, Command mode and data mode.
** Mode is shifted with the port_control function.
** Command mode protocol:
** 'L'<socketname>: Lock and listen on socket.
** 'A'<listennumber as 32 bit bigendian>: Accept from the port referenced by the 
**                                        "listennumber"
** 'C'<socketname>: Connect to the socket named <socketname>
** 'S'<data>: Send the data <data> 
** 'R': Receive one packet of data 
** Data mode protocol:
** Send anything that arrives (no opcodes/skip opcodes).
*/

static void uds_command(ErlDrvData handle, char *buff, int bufflen)
{
    UdsData *ud = (UdsData *) handle;

    if (ud->type == portTypeData || ud->type == portTypeIntermediate) {
	DEBUGF(("Passive do_send %d",bufflen));
	do_send(ud, buff + 1, bufflen - 1); /* XXX */
	return;
    } 
    if (bufflen == 0) {
	return;
    }
    switch (*buff) {
    case 'L':
	if (ud->type != portTypeUnknown) {
	    driver_failure_posix(ud->port, ENOTSUP);
	    return;
	}
	uds_command_listen(ud,buff,bufflen);
	return;
    case 'A':
	if (ud->type != portTypeUnknown) {
	    driver_failure_posix(ud->port, ENOTSUP);
	    return;
	}
	uds_command_accept(ud,buff,bufflen);
	return;
    case 'C':
	if (ud->type != portTypeUnknown) {
	    driver_failure_posix(ud->port, ENOTSUP);
	    return;
	}
	uds_command_connect(ud,buff,bufflen);
	return;
    case 'S':
	if (ud->type != portTypeCommand) {
	    driver_failure_posix(ud->port, ENOTSUP);
	    return;
	}
	do_send(ud, buff + 1, bufflen - 1);
	return;
    case 'R':
	if (ud->type != portTypeCommand) {
	    driver_failure_posix(ud->port, ENOTSUP);
	    return;
	}
	do_recv(ud);
	return;
    default:
	ASSERT(0);
	return;
    }
}
	
static void uds_input(ErlDrvData handle, ErlDrvEvent event)
{
    UdsData *ud = (UdsData *) handle;

    DEBUGF(("In uds_input type = %d",ud->type));
    if (ud->type == portTypeListener) {
	UdsData *ad = ud->partner;
	struct sockaddr_un peer;
	int pl = sizeof(struct sockaddr_un);
	int fd;

	ASSERT(ad != NULL);
	if ((fd = accept(ud->fd, (struct sockaddr *) &peer, &pl)) < 0) {
	    if (errno != EWOULDBLOCK) {
		DEBUGF(("Accept failed."));
		driver_failure_posix(ud->port, errno);
		return;
	    }
	    DEBUGF(("Accept would block."));
	    return;
	}
	SET_NONBLOCKING(fd);
	ad->fd = fd;
	ad->partner = NULL;
	ad->type = portTypeCommand;
	ud->partner = NULL;
	DEBUGF(("Accept successful."));
	driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_READ, 0);
	driver_output(ad->port, "Aok",3);
	return;
    }
    /* OK, normal data or command port */
    ASSERT(ud->type >= portTypeCommand);
#ifdef HARDDEBUG
    if (ud->type == portTypeData)
	DEBUGF(("Passive do_recv"));
#endif
    do_recv(ud);
}

static void uds_output(ErlDrvData handle, ErlDrvEvent event)
{
   UdsData *ud = (UdsData *) handle;
   if (ud->type == portTypeConnector) {
       ud->type = portTypeCommand;
       driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_WRITE, 0);
       driver_output(ud->port, "Cok",3);
       return;
   }
   ASSERT(ud->type == portTypeCommand || ud->type == portTypeData);
   send_out_queue(ud);
}

static void uds_finish(void) 
{
    while (first_data != NULL) {
	do_stop(first_data, 1);
    }
}

/*
** Protocol to control:
** 'C': Set port in command mode.
** 'I': Set port in intermediate mode
** 'D': Set port in data mode
** 'N': Get identification number for listen port
** 'S': Get statistics
** 'T': Send a tick message
** 'R': Get creation number of listen socket
** Answer is one byte status (0 == ok, Other is followed by error as string) 
** followed by data if applicable
*/
static int uds_control(ErlDrvData handle, unsigned int command, 
		       char* buf, int count, char** res, int res_size)
{
/* Local macro to ensure large enough buffer. */
#define ENSURE(N) 				\
   do {						\
       if (res_size < N) {			\
	   *res = ALLOC(N);			\
       }					\
   } while(0)

   UdsData *ud = (UdsData *) handle;

   DEBUGF(("Control, type = %d, fd = %d, command = %c", ud->type, ud->fd, 
	   (char) command));
   switch (command) {
   case 'S':
       {
	   ENSURE(13);
	   **res = 0;
	   put_packet_length((*res) + 1, ud->received);
	   put_packet_length((*res) + 5, ud->sent);
	   put_packet_length((*res) + 9, driver_sizeq(ud->port));
	   return 13;
       }
   case 'C':
       if (ud->type < portTypeCommand) {
	   return report_control_error(res, res_size, "einval");
       }
       ud->type = portTypeCommand;
       driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_READ, 0);
       ENSURE(1);
       **res = 0;
       return 1;
   case 'I':
       if (ud->type < portTypeCommand) {
	   return report_control_error(res, res_size, "einval");
       }
       ud->type = portTypeIntermediate;
       driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_READ, 0);
       ENSURE(1);
       **res = 0;
       return 1;
   case 'D':
       if (ud->type < portTypeCommand) {
	   return report_control_error(res, res_size, "einval");
       }
       ud->type = portTypeData;
       do_recv(ud);
       ENSURE(1);
       **res = 0;
       return 1;
   case 'N':
       if (ud->type != portTypeListener) {
	   return report_control_error(res, res_size, "einval");
       }
       ENSURE(5);
       (*res)[0] = 0;
       put_packet_length((*res) + 1, ud->fd);
       return 5;
   case 'T': /* tick */
       if (ud->type != portTypeData) {
	   return report_control_error(res, res_size, "einval");
       }
       do_send(ud,"",0);
       ENSURE(1);
       **res = 0;
       return 1;
   case 'R':
       if (ud->type != portTypeListener) {
	   return report_control_error(res, res_size, "einval");
       }
       ENSURE(2);
       (*res)[0] = 0;
       (*res)[1] = ud->creation;
       return 2;
   default:
       return report_control_error(res, res_size, "einval");
   }
#undef ENSURE
}

static void uds_stop_select(ErlDrvEvent event, void* _)
{
    close((int)(long)event);
}

/*
**
** Local helpers
**
*/

/*
** Command implementations 
*/
static void uds_command_connect(UdsData *ud, char *buff, int bufflen) 
{
    char *str;
    int fd;
    struct sockaddr_un s_un;
    int length;
    int res;

    str = ALLOC(25);
    sprintf(str, "erl%d", (int) getpid()); /* A temporary sufficiently 
					      unique name */
    do_unlink(str);
    s_un.sun_family = AF_UNIX;
    strcpy(s_un.sun_path, SOCKET_PATH "/");
    strcat(s_un.sun_path, str);
    DEBUGF(("Connect own filename: %s", s_un.sun_path));
    length = sizeof(s_un.sun_family) + strlen(s_un.sun_path);
    ud->name = str;
    ud->type = portTypeCommand;
    if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
	DEBUGF(("socket call failed, errno = %d"));
	driver_failure_posix(ud->port, errno);
	return;
    }
    ud->fd = fd;
    if ((res = bind(fd, (struct sockaddr *) &s_un, length)) < 0) {
	DEBUGF(("bind call failed, errno = %d",errno));
	driver_failure_posix(ud->port, errno);
	return;
    }
    str = ALLOC(bufflen);
    memcpy(str, buff + 1, bufflen - 1);
    str[bufflen - 1] = '\0';
    strcpy(s_un.sun_path, SOCKET_PATH "/");
    strcat(s_un.sun_path, str);
    length = sizeof(s_un.sun_family) + strlen(s_un.sun_path);
    DEBUGF(("Connect peer filename: %s", s_un.sun_path));
    SET_NONBLOCKING(fd);
    if (connect(fd, (struct sockaddr *) &s_un, length) < 0) {
	if (errno != EINPROGRESS) {
	    driver_failure_posix(ud->port, errno);
	} else {
	    DEBUGF(("Connect pending"));
	    ud->type = portTypeConnector;
	    driver_select(ud->port, (ErlDrvEvent) ud->fd,
			  ERL_DRV_WRITE|ERL_DRV_USE, 1);
	} 
    } else {
	DEBUGF(("Connect done"));
	driver_output(ud->port, "Cok", 3);
    }
    FREE(str);
}

static void uds_command_accept(UdsData *ud, char *buff, int bufflen) 
{
    int listen_no;
    UdsData *lp;

    if (bufflen < 5) {
	driver_failure_posix(ud->port, EINVAL);
	return;
    }
    
    listen_no = get_packet_length(buff + 1); /* Same format as 
						packet headers */
    DEBUGF(("Accept listen_no = %d",listen_no));
    for (lp = first_data; lp != NULL && lp->fd != listen_no; lp = lp->next)
	;
    if (lp == NULL) {
	DEBUGF(("Could not find listen port"));
	driver_failure_posix(ud->port, EINVAL);
	return;
    }
    if (lp->partner != NULL) {
	DEBUGF(("Listen port busy"));
	driver_failure_posix(ud->port, EADDRINUSE);
	return;
    }
    lp->partner = ud;
    ud->partner = lp;
    ud->type = portTypeAcceptor;
    driver_select(lp->port,(ErlDrvEvent) lp->fd, ERL_DRV_READ|ERL_DRV_USE, 1);
    /* Silent, answer will be sent in input routine */
}

static void uds_command_listen(UdsData *ud, char *buff, int bufflen)
{
    char *str;
    int fd;
    struct sockaddr_un s_un;
    int length;
    int res;
    UdsData *tmp;
    Byte creation;

    str = ALLOC(bufflen);
    memcpy(str, buff + 1,bufflen - 1);
    str[bufflen - 1] = '\0';

    /*
    ** Before trying lockfiles etc, we need to assure that our own process is 
    ** not using the filename. Advisory locks can be recursive in one process.
    */
    for(tmp = first_data; tmp != NULL; tmp = tmp->next) {
	if (tmp->name != NULL && strcmp(str, tmp->name) == 0) {
	    driver_failure_posix(ud->port, EADDRINUSE);
	    FREE(str);
	    return;
	}
    }
	    
    if ((fd = try_lock(str, &creation)) < 0) {
	driver_failure_posix(ud->port, EADDRINUSE);
	FREE(str);
	return;
    }
    s_un.sun_family = AF_UNIX;
    strcpy(s_un.sun_path, SOCKET_PATH "/");
    strcat(s_un.sun_path, str);
    length = sizeof(s_un.sun_family) + strlen(s_un.sun_path);
    ud->name = str;
    ud->type = portTypeListener;
    ud->lockfd = fd;
    ud->creation = creation;
    if ((fd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
	DEBUGF(("socket call failed, errno = %d"));
	driver_failure_posix(ud->port, errno);
	return;
    }
    SET_NONBLOCKING(fd);
    ud->fd = fd;
    do_unlink(str);
    DEBUGF(("Listen filename: %s", s_un.sun_path));
    if ((res = bind(fd, (struct sockaddr *) &s_un, length)) < 0) {
	DEBUGF(("bind call failed, errno = %d",errno));
	driver_failure_posix(ud->port, errno);
	return;
    }

    if ((res = listen(fd, 5)) < 0) {
	DEBUGF(("listen call failed, errno = %d"));
	driver_failure_posix(ud->port, errno);
	return;
    }
    driver_output(ud->port, "Lok", 3);
}

/*
** Input/output/stop helpers
*/
static void do_stop(UdsData *ud, int shutting_down) 
{
    UdsData **tmp;

    DEBUGF(("Cleaning up, type = %d, fd = %d, lockfd = %d", ud->type, 
	    ud->fd, ud->lockfd));
    for (tmp = &first_data; *tmp != NULL && *tmp != ud; tmp = &((*tmp)->next))
	;
    ASSERT(*tmp != NULL);
    *tmp = (*tmp)->next;
    if (ud->buffer != NULL) {
	FREE(ud->buffer); 
    }
    if (ud->fd >= 0) {
	driver_select(ud->port, (ErlDrvEvent) ud->fd,
		      ERL_DRV_READ|ERL_DRV_WRITE|ERL_DRV_USE, 0);
    }
    if (ud->name) {
	do_unlink(ud->name);
	FREE(ud->name);
    }
    if (ud->lockfd >= 0) {
	ASSERT(ud->type == portTypeListener);
	close(ud->lockfd); /* the lock will be released */
	/* But leave the file there for the creation counter... */
    }
    if (!shutting_down) { /* Dont bother if the driver is shutting down. */
	if (ud->partner != NULL) {
	    if (ud->type == portTypeAcceptor) {
		UdsData *listener = ud->partner;
		listener->partner = NULL;
		driver_select(listener->port, (ErlDrvEvent) listener->fd,
			      ERL_DRV_READ, 0);
	    } else {
		UdsData *acceptor = ud->partner;
		ASSERT(ud->type == portTypeListener);
		acceptor->partner = NULL;
		driver_failure_eof(acceptor->port);
	    }
	}
    }
    FREE(ud);
}

/*
** Actually send the data
*/
static void do_send(UdsData *ud, char *buff, int bufflen) 
{
    char header[4];
    int written;
    SysIOVec iov[2];
    ErlIOVec eio;
    ErlDrvBinary *binv[] = {NULL,NULL};

    put_packet_length(header, bufflen);
    DEBUGF(("Write packet header %u,%u,%u,%u.", (Word) header[0],
	   (Word) header[1], (Word) header[2],(Word) header[3]));
    iov[0].iov_base = (char *) header;
    iov[0].iov_len = 4;
    iov[1].iov_base = buff;
    iov[1].iov_len = bufflen;
    eio.iov = iov;
    eio.binv = binv;
    eio.vsize = 2;
    eio.size = bufflen + 4;
    written = 0;
    if (driver_sizeq(ud->port) == 0) {
	if ((written = writev(ud->fd, iov, 2)) == eio.size) {
	    ud->sent += written;
	    if (ud->type == portTypeCommand) {
		driver_output(ud->port, "Sok", 3);
	    }
	    DEBUGF(("Wrote all %d bytes immediately.",written));
	    return;
	} else if (written < 0) {
	    if (errno != EWOULDBLOCK) {
		driver_failure_eof(ud->port);
		return;
	    } else {
		written = 0;
	    }
	} else {
	    ud->sent += written;
	}
	DEBUGF(("Wrote %d bytes immediately.",written));
	/* Enqueue remaining */
    }
    driver_enqv(ud->port, &eio, written);
    DEBUGF(("Sending output queue."));
    send_out_queue(ud);
}

static void do_recv(UdsData *ud)
{
    int res;
    char *ibuf;
    ASSERT_NONBLOCK(ud->fd);
    DEBUGF(("do_recv called, type = %d", ud->type));
    for(;;) {
	if ((res = buffered_read_package(ud,&ibuf)) < 0) {
	    if (res == NORMAL_READ_FAILURE) {
		DEBUGF(("do_recv normal read failed"));
		driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_READ|ERL_DRV_USE, 1);
	    } else {
		DEBUGF(("do_recv fatal read failed (%d) (%d)",errno, res));
		driver_failure_eof(ud->port);
	    }
	    return;
	}
	DEBUGF(("do_recv got package, port type = %d", ud->type));
	/* Got a package */
	if (ud->type == portTypeCommand) {
	    ibuf[-1] = 'R'; /* There is always room for a single byte opcode
			       before the actual buffer (where the packet
			       header was) */
	    driver_output(ud->port,ibuf - 1, res + 1);
	    driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_READ, 0);
	    return;
	} else {
	    ibuf[-1] = DIST_MAGIC_RECV_TAG; /* XXX */
	    driver_output(ud->port,ibuf - 1, res + 1);
	    driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_READ|ERL_DRV_USE, 1);
	}
    }
}


/*
** Report control error, helper for error messages from control
*/
static int report_control_error(char **buffer, int buff_len, 
				char *error_message)
{
    int elen = strlen(error_message);
    if (elen + 1 < buff_len) {
	*buffer = ALLOC(elen + 1);
    }
    **buffer = 1;
    memcpy((*buffer) + 1, error_message, elen);
    return elen + 1;
}

/*
** Lower level I/O helpers
*/
static int send_out_queue(UdsData *ud)
{
    ASSERT_NONBLOCK(ud->fd);
    for(;;) {
	int vlen;
	SysIOVec *tmp = driver_peekq(ud->port, &vlen);
	int wrote;
	if (tmp == NULL) {
	    DEBUGF(("Write queue empty."));
	    driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_WRITE, 0);
	    if (ud->type == portTypeCommand) {
		driver_output(ud->port, "Sok", 3);
	    }
	    return 0;
	}
	if (vlen > IO_VECTOR_MAX) {
	    vlen = IO_VECTOR_MAX;
	} 
	DEBUGF(("Trying to writev %d vectors", vlen));
#ifdef HARDDEBUG
	{
	    int i;
	    for (i = 0; i < vlen; ++i) {
		DEBUGF(("Buffer %d: length %d", i, tmp[i].iov_len));
	    }
	}
#endif
	if ((wrote = writev(ud->fd, tmp, vlen)) < 0) {
	    if (errno == EWOULDBLOCK) {
		DEBUGF(("Write failed normal."));
		driver_select(ud->port, (ErlDrvEvent) ud->fd, ERL_DRV_WRITE|ERL_DRV_USE, 1);
		return 0;
	    } else {
		DEBUGF(("Write failed fatal (%d).", errno));
		driver_failure_eof(ud->port);
		return -1;
	    }
	}
	driver_deq(ud->port, wrote);
	ud->sent += wrote;
	DEBUGF(("Wrote %d bytes of data.",wrote));
    }
}

static int buffered_read_package(UdsData *ud, char **result)
{
    int res;
    int data_size;

    if (ud->buffer_pos < ud->header_pos + HEADER_LENGTH) {
	/* The header is not read yet */
	DEBUGF(("Header not read yet"));
	if ((res = read_at_least(ud, ud->header_pos + HEADER_LENGTH - 
				 ud->buffer_pos)) < 0) {
	    DEBUGF(("Header read failed"));
	    return res;
	}
    } 
    DEBUGF(("Header is read"));
    /* We have at least the header read */
    data_size = get_packet_length((char *) ud->buffer + ud->header_pos);
    DEBUGF(("Input packet size = %d", data_size));
    if (ud->buffer_pos < ud->header_pos + HEADER_LENGTH + data_size) {
	/* We need to read more */
	DEBUGF(("Need to read more (bufferpos %d, want %d)", ud->buffer_pos,
		ud->header_pos + HEADER_LENGTH + data_size));
	if ((res = read_at_least(ud, 
				 ud->header_pos + HEADER_LENGTH + 
				 data_size - ud->buffer_pos)) < 0) {
	    DEBUGF(("Data read failed"));
	    return res;
	}
    }
    DEBUGF(("Data is completely read"));
    *result = (char *) ud->buffer + ud->header_pos + HEADER_LENGTH;
    ud->header_pos += HEADER_LENGTH + data_size;
    return data_size;
}

static int read_at_least(UdsData *ud, int num)
{
    int got;
    if (ud->buffer_pos + num > ud->buffer_size) {
	/* No place in the buffer, try to pack it */
	if (ud->header_pos > 0) {
	    int offset = ud->header_pos;
	    DEBUGF(("Packing buffer, buffer_pos was %d, buffer_size was %d "
		    "offset %d num %d header_pos %d.", 
		    ud->buffer_pos, ud->buffer_size,
		    offset, num, ud->header_pos));
	    memmove(ud->buffer, ud->buffer + ud->header_pos, 
		    ud->buffer_pos - ud->header_pos);
	    ud->buffer_pos -= offset;
	    ud->header_pos -= offset;
	}
	/* The buffer is packed, look for space again and reallocate if 
	   needed */
	if (ud->buffer_pos + num > ud->buffer_size) {
	    /* Let's grow in chunks of 256 */
	    ud->buffer_size = (((ud->buffer_pos + num) / 
				  CHUNK_SIZE) + 1) * CHUNK_SIZE;
	    DEBUGF(("New buffer size %d.",ud->buffer_size)); 
	    /* We will always keep one extra byte before the buffer to
	       allow insertion of an opcode */
	    if (!ud->buffer) {
		ud->buffer = ALLOC(ud->buffer_size);
	    } else {
		ud->buffer = REALLOC(ud->buffer, ud->buffer_size);
	    }
	}
    }
    /* OK, now we have a large enough buffer, try to read into it */
    if ((got = read(ud->fd, ud->buffer + ud->buffer_pos, 
		    ud->buffer_size - ud->buffer_pos)) < 0) {
	/* It failed, the question is why... */
	if (errno == EAGAIN) {
	    return NORMAL_READ_FAILURE;
	} 
	return SEVERE_READ_FAILURE;
    } else if (got == 0) {
	return EOF_READ_FAILURE;
    }
    DEBUGF(("Got %d bytes.", got));
    ud->received += got;
    ud->buffer_pos += got;
   /* So, we got some bytes, but enough ? */
    if (got < num) {
	return NORMAL_READ_FAILURE;
    }
    return 0;
}
	
static int get_packet_length(char *b)
{
    Byte *u = (Byte *) b;
    int x = (((Word) u[0]) << 24) | (((Word) u[1]) << 16) | 
	(((Word) u[2]) << 8) | ((Word) u[3]);
    DEBUGF(("Packet length %d.", x));
    return x;
}

static void put_packet_length(char *b, int len)
{	   
    Byte *p = (Byte *) b;
    Word n = (Word) len;
    p[0] = (n >> 24) & 0xFF;
    p[1] = (n >> 16) & 0xFF;
    p[2] = (n >> 8) & 0xFF;
    p[3] = n & 0xFF;
}

/*
** Malloc wrappers
*/
static void *my_malloc(size_t size) 
{
    void *ptr;

    if ((ptr = driver_alloc(size)) == NULL) {
	fprintf(stderr, "Could not allocate %lu bytes of memory",(unsigned long) size);
	abort();
    }
    return ptr;
}

static void *my_realloc(void *ptr, size_t size)
{
    void *nptr;
    if ((nptr = driver_realloc(ptr, size)) == NULL) {
	fprintf(stderr, "Could not reallocate %lu bytes of memory",(unsigned long) size);
	abort();
    }
    return nptr;
}
    

/*
** Socket file handling helpers
*/

/*
** Check that directory exists, create if not (only works for one level)
*/
static int ensure_dir(char *path)
{
    if (mkdir(path,0777) != 0 && errno != EEXIST) {
	return -1;
    }
    return 0;
}

/*
** Try to open a lock file and lock the first byte write-only (advisory)
** return the file descriptor if successful, otherwise -1 (<0).
*/ 
static int try_lock(char *sockname, Byte *p_creation)
{
    char *lockname;
    int lockfd;
    struct flock fl;
    Byte creation;

    lockname = ALLOC(strlen(SOCKET_PATH)+1+strlen(sockname)+
		     strlen(LOCK_SUFFIX)+1);
    sprintf(lockname,SOCKET_PATH "/%s" LOCK_SUFFIX, sockname);
    DEBUGF(("lockname = %s", lockname));
    if (ensure_dir(SOCKET_PATH) != 0) {
	DEBUGF(("ensure_dir failed, errno = %d", errno));
	FREE(lockname);
	return -1;
    }
    if ((lockfd = open(lockname, O_RDWR | O_CREAT, 0666)) < 0) {
	DEBUGF(("open failed, errno = %d", errno));
	FREE(lockname);
	return -1;
    }
    FREE(lockname);
    memset(&fl,0,sizeof(fl));
    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 1;
    if (fcntl(lockfd, F_SETLK, &fl) < 0) {
	DEBUGF(("fcntl failed, errno = %d", errno));
	close(lockfd);
	return -1;
    }
    /* OK, check for creation and update */
    if (read(lockfd, &creation, 1) < 1) {
	creation = 0;
    } else {
	creation = (creation + 1) % 4;
    }
    lseek(lockfd, 0, SEEK_SET);
    write(lockfd, &creation, 1);
    fsync(lockfd); /* This could be concidered dangerous (blocking) */
    *p_creation = creation;
    return lockfd;
}
    
static void do_unlink(char *name)
{
    char buff[100];
    char *str = buff;
    int len = strlen(SOCKET_PATH) + 1 + strlen(name) + 1;

    if (len > 100) {
	str = ALLOC(len);
    }
    sprintf(str,SOCKET_PATH "/%s",name);
    unlink(str);
    if (str != buff) {
	FREE(str);
    }
}

