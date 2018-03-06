/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
 * Erlang port program to do the name service lookup for the erlang 
 * distribution and inet part of the kernel.
 * A pool of subprocess is kept, to which a pair of pipes is connected.
 * The main process schedules requests among the different subprocesses 
 * (created with fork()), to be able to handle as many requests as possible
 * simultaneously. The controlling erlang machine may request a "cancel",
 * in which case the process may be killed and restarted when the need arises.
 * The single numeric parameter to this program is the maximum port pool size,
 * which is the size of the bookkeeping array.
 *
 * Windows:
 * There is instead of a pool of processes a pool of threads.
 * Communication is not done through pipes but via message queues between 
 * the threads. The only "pipes" involved are the ones used for communicating
 * with Erlang. 
 * Important note:
 * For unknown reasons, the combination of a thread doing blocking I/O on
 * a named pipe at the same time as another thread tries to resolve a hostname
 * may (with certain software configurations) block the gethostbyname call (!) 
 * For that reason, standard input (and standard output) should be opened
 * in asynchronous mode (FILE_FLAG_OVERLAPPED), which has to be done by Erlang.
 * A special flag to open_port is used to work around this behaviour in winsock
 * and the threads doing read and write handle asynchronous I/O.
 * The ReadFile and WriteFile calls try to cope with both types of I/O, why 
 * the code is not really as Microsoft describes "the right way to do it" in
 * their documentation. Important to note is that **there is no supported way
 * to retrieve the information if the HANDLE was opened with 
 * FILE_FLAG_OVERLAPPED from the HANDLE itself**. 
 *
 */ 

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_printf.h"

#ifdef WIN32

#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>

/* These are not used even if they would exist which they should not */
#undef HAVE_GETIPNODEBYNAME
#undef HAVE_GETHOSTBYNAME2
#undef HAVE_GETIPNODEBYADDR

#else /* Unix */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#include <sys/times.h>

#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif

/* To simplify #ifdef code further down - select only one to be defined...
** Use them in pairs - if one is broken do not trust its mate.
**/
#if defined(HAVE_GETADDRINFO) && defined(HAVE_GETNAMEINFO)
#undef HAVE_GETIPNODEBYNAME
#undef HAVE_GETIPNODEBYADDR
#undef HAVE_GETHOSTBYNAME2
#elif defined(HAVE_GETIPNODEBYNAME) && defined(HAVE_GETIPNODEBYADDR)
#undef HAVE_GETADDRINFO
#undef HAVE_GETNAMEINFO
#undef HAVE_GETHOSTBYNAME2
#else
#undef HAVE_GETIPNODEBYNAME
#undef HAVE_GETIPNODEBYADDR
#undef HAVE_GETADDRINFO
#undef HAVE_GETNAMEINFO
#endif

#endif /* !WIN32 */

#define PACKET_BYTES 4
#ifdef WIN32
#define READ_PACKET_BYTES(X,Y,Z) read_int32((X),(Y),(Z))
#else
#define READ_PACKET_BYTES(X,Y) read_int32((X),(Y))
#endif
#define PUT_PACKET_BYTES(X,Y) put_int32((X),(Y)) 
/* The serial numbers of the requests */
typedef int SerialType;

#define INVALID_SERIAL -1

/* The operations performed by this program */
typedef unsigned char OpType;

#define OP_GETHOSTBYNAME 1
#define OP_GETHOSTBYADDR 2
#define OP_CANCEL_REQUEST 3
#define OP_CONTROL 4

/* The protocol (IPV4/IPV6) */
typedef unsigned char ProtoType;

#define PROTO_IPV4 1
#define PROTO_IPV6 2

/* OP_CONTROL */
typedef unsigned char CtlType;
#define SETOPT_DEBUG_LEVEL 0

/* The unit of an IP address (0 == error, 4 == IPV4, 16 == IPV6) */
typedef unsigned char UnitType;

#define UNIT_ERROR 0
#define UNIT_IPV4  4
#define UNIT_IPV6 16

/* And the byte type */
typedef unsigned char AddrByte; /* Must be compatible with character 
				   datatype */

/* 
 * Marshalled format of request: 
 *{
 *  Serial: 32 bit big endian
 *  Op:8 bit  [1,2,3]
 *  If op == 1 {
 *    Proto:8 bit [1,2]
 *    Str: Null terminated array of characters
 *  } Else if op == 2 {
 *    Proto:8 bit [1,2]
 *    If proto == 1 {
 *      B0..B3: 4 bytes, most significant first
 *    } Else (proto == 2) {
 *      B0..B15: 16 bytes, most significant first
 *    }
 *  } 
 *  (No more if op == 3)
 *} 
 * The request arrives as a packet, with 4 packet size bytes.
 */

/* The main process unpackes the marshalled message and sends the data 
 * to a suitable port process or, in the case of a close request, kills the
 * suitable port process. There is also a que of requests linked together,
 * for when all subrocesses are busy.
 */

typedef struct QueItem {
    struct QueItem *next;
    int req_size;
    AddrByte request[1];
} QueItem; /* Variable size due to request's variable size */

QueItem *que_first;
QueItem *que_last;

#ifdef WIN32
typedef struct mesq {
    HANDLE data_present;
    CRITICAL_SECTION crit;
    int shutdown;
    QueItem *first;
    QueItem *last;
} MesQ;

MesQ *to_erlang;
MesQ *from_erlang;
#endif

/*
 * Marshalled format of reply:
 *{
 *  Serial: 32 bit big endian
 *  Unit: 8 bit, same as h_length or 0 for error
 *  if unit == 0 {
 *    Str: Null terminated character string explaining the error
 *  } else {
 *    Naddr: 32 bit big endian
 *    if unit = 4 {
 *      (B0..B3)0..(B0..B3)Naddr-1: Naddr*4 bytes most significant first
 *    } else if unit == 16 {
 *      (B0..B15)0..(B0..B15)Naddr-1: Naddr*16 bytes most significant first
 *    }
 *    Nnames: 32 bit big endian >= 1
 *    Name0: Null terminated string of characters
 *    Alias[0]..Alias[Nnames - 2]: Nnames - 1 Null terminated strings of chars
 *  }
 *}
 * Four packet size bytes prepended (big endian)
 */
/* Internal error codes */
#define ERRCODE_NOTSUP 1
#define ERRCODE_HOST_NOT_FOUND 2
#define ERRCODE_TRY_AGAIN 3
#define ERRCODE_NO_RECOVERY 4
#define ERRCODE_NO_DATA 5
#define ERRCODE_NETDB_INTERNAL 7

/* 
 * Each worker process is represented in the parent by the following struct
 */

typedef unsigned WorkerState;

#define WORKER_EMPTY   0 /* No process created       */
#define WORKER_FREE    1 /* Living waiting process   */
#define WORKER_BUSY    2 /* Living busy process      */
#define WORKER_STALLED 3 /* Living cancelled process */

/* The timeout when killing a child process in seconds*/
#define CHILDWAIT_TMO 1
/* The domainname size_limit */
#define DOMAINNAME_MAX 258 /* 255 + Opcode + Protocol + Null termination */

typedef struct {
    WorkerState state;
#ifdef WIN32
    DWORD pid; /* 0 if unused */
    MesQ *writeto; /* Message queues */
    MesQ *readfrom;
#else
    pid_t pid; /* -1 if unused */
    int writeto, readfrom; /* Pipes */
#endif
    SerialType serial;
    AddrByte domain[DOMAINNAME_MAX];
    QueItem *que_first;
    QueItem *que_last;
    int que_size;
} Worker;

int num_busy_workers;
int num_free_workers;
int num_stalled_workers;
int max_workers;
int greedy_threshold;
Worker *busy_workers;  /* Workers doing any job that someone really is 
			  interested in */
Worker *free_workers;  /* Really free workers */
Worker *stalled_workers; /* May still deliver answers which we will 
			    discard */
#define BEE_GREEDY() (num_busy_workers >= greedy_threshold)

static char *program_name;

static int debug_level;
#ifdef WIN32
static HANDLE debug_console_allocated = INVALID_HANDLE_VALUE;
#endif

#ifdef NODEBUG
#define DEBUGF(L,P) /* Nothing */
#else
#define DEBUGF(Level,Printf) do { if (debug_level >= (Level)) \
                                      debugf Printf;} while(0)  
#endif
#define ALLOC(Size) my_malloc(Size)
#define REALLOC(Old, Size) my_realloc((Old), (Size))
#define FREE(Ptr) free(Ptr)

#ifdef WIN32
#define WAKEUP_WINSOCK() do {			\
    char dummy_buff[100];			\
    gethostname(dummy_buff,99);			\
} while (0)
#endif

/* The internal prototypes */
static char *format_address(int siz, AddrByte *addr);
static void debugf(char *format, ...);
static void warning(char *format, ...);
static void fatal(char *format, ...);
static void *my_malloc(size_t size);
static void *my_realloc(void *old, size_t size);
static int get_int32(AddrByte *buff);
static void put_int32(AddrByte *buff, int value);
static int create_worker(Worker *pworker, int save_que);
static int map_netdb_error(int netdb_code);
#if defined(HAVE_GETADDRINFO) || defined(HAVE_GETNAMEINFO)
static int map_netdb_error_ai(int netdb_code);
#endif
static char *errcode_to_string(int errcode);
static size_t build_error_reply(SerialType serial, int errnum, 
				AddrByte **preply,
				size_t *preply_size);
#ifdef HAVE_GETADDRINFO
static size_t build_reply_ai(SerialType serial, int, struct addrinfo *,
                            AddrByte **preply, size_t *preply_size);
#endif
static size_t build_reply(SerialType serial, struct hostent *he,
                         AddrByte **preply, size_t *preply_size);
static int read_request(AddrByte **buff, size_t *buff_size);
static OpType get_op(AddrByte *buff);
static AddrByte *get_op_addr(AddrByte *buff);
static SerialType get_serial(AddrByte *buff);
static ProtoType get_proto(AddrByte *buff);
static CtlType get_ctl(AddrByte *buff);
static AddrByte *get_data(AddrByte *buff);
static int get_debug_level(AddrByte *buff);
static int relay_reply(Worker *pw);
static int ignore_reply(Worker *pw);
static void init_workers(int max);
static void kill_worker(Worker *pw);
static Worker *pick_worker(void);
static void kill_last_picked_worker(void);
static void stall_worker(SerialType serial);
static int handle_io_busy(int ndx);
static int handle_io_free(int ndx);
static int handle_io_stalled(int ndx);
static void check_que(void);
static void main_loop(void);
static void usage(char *unknown);
static void domaincopy(AddrByte *out,AddrByte *in);
static int domaineq(AddrByte *d1, AddrByte *d2);
static int get_domainname(AddrByte *inbuff, int insize, AddrByte *domainbuff);
static Worker *pick_worker_greedy(AddrByte *domainbuff);
static void restart_worker(Worker *w);
static void start_que_request(Worker *w) ;
#ifdef WIN32
static int read_int32(HANDLE fd, int *res, HANDLE ev);
static int read_exact(HANDLE fd, void *vbuff, DWORD nbytes, HANDLE ev);
static int write_exact(HANDLE fd, AddrByte *buff, DWORD len,HANDLE ev);
DWORD WINAPI worker_loop(void *v);
DWORD WINAPI reader(void *data);
DWORD WINAPI writer(void *data);
static int send_mes_to_worker(QueItem *m, Worker *pw);
BOOL create_mesq(MesQ **q);
BOOL enque_mesq(MesQ *q, QueItem *m);
BOOL deque_mesq(MesQ *q, QueItem **m);
BOOL close_mesq(MesQ *q);
HANDLE event_mesq(MesQ *q);
#else
static size_t read_int32(int fd, int *res);
static ssize_t read_exact(int fd, void *vbuff, size_t nbytes);
static int write_exact(int fd, AddrByte *buff, int len);
void reap_children(int ignored);
static void init_signals(void);
static void kill_all_workers(void);
static void close_all_worker_fds(void);
static int worker_loop(void);
static int fillin_reply(Worker *pw);
static int send_request_to_worker(AddrByte *pr, int rsize, Worker *pw);
#endif

#define ERL_DBG_LVL_ENV_VAR "ERL_INET_GETHOST_DEBUG"

static int
get_env_debug_level(void)
{
#ifdef __WIN32__
    char value[21]; /* Enough for any 64-bit values */
    DWORD sz = GetEnvironmentVariable((LPCTSTR) ERL_DBG_LVL_ENV_VAR,
				      (LPTSTR) value,
				      (DWORD) sizeof(value));
    if (sz == 0 || sz > sizeof(value))
	return 0;
#else
    char *value = getenv(ERL_DBG_LVL_ENV_VAR);
    if (!value)
	return 0;
#endif
    return atoi(value);
}

#ifdef WIN32
static void do_allocate_console(void)
{
    AllocConsole();
    debug_console_allocated = CreateFile ("CONOUT$", GENERIC_WRITE, 
					  FILE_SHARE_WRITE, NULL,
					  OPEN_EXISTING, 
					  FILE_ATTRIBUTE_NORMAL, NULL);
}
#ifdef HARDDEBUG
DWORD WINAPI pseudo_worker_loop(void *v);
static void poll_gethost(int row);
#endif
#endif

/*
 * Main
 */
int main(int argc, char **argv) 
{
    int num_workers = 1;
    char **ap = argv + 1;
    int x;
    int disable_greedy = 0;

    program_name = *argv;
    que_first = que_last = NULL;
    debug_level = get_env_debug_level();
    greedy_threshold = 0;

    while (*ap) {
	if (!strcmp(*ap, "-d")) {
	    ++debug_level;
	} else if(!strcmp(*ap, "-g") && *(ap + 1)) {
	    ++ap;
	    x = atoi(*ap);
	    if (!x) {
		usage(*ap);
	    } else {
		greedy_threshold = x;
	    }
	} else if(!strcmp(*ap, "-ng")) {
	    disable_greedy = 1;
	} else {
	    x = atoi(*ap);
	    if (!x) {
		usage(*ap);
	    } else {
		num_workers = x;
	    }
	}
	++ap;
    }

#ifdef WIN32
    if (num_workers > 60 || greedy_threshold > 60) {
	usage("More than 60 workers on windows impossible!");
	num_workers = 60;
	greedy_threshold = 0;
    }
#endif

    if(!greedy_threshold) {
	greedy_threshold = (3*num_workers)/4; /* 75% */
	if (!greedy_threshold) {
	    greedy_threshold = num_workers;
	}
    }

    if (disable_greedy) {
	greedy_threshold = num_workers + 1;
    }

#ifdef WIN32
    {
	WORD wr;
	WSADATA wsa_data;
	int wsa_error;
	wr = MAKEWORD(2,0);
	
	wsa_error = WSAStartup(wr,&wsa_data);
	if (wsa_error) {
	    fatal("Could not open usable winsock library.");
	}
	if (LOBYTE(wsa_data.wVersion) != 2 || HIBYTE(wsa_data.wVersion) != 0) {
	    fatal("Could not open recent enough winsock library.");
	}

	if (debug_level >= 1) {
	    do_allocate_console();

	    DEBUGF(1,("num_workers = %d, greedy_threshold = %d, "
		      "debug_level = %d.",
		      num_workers, greedy_threshold, debug_level));
	}
    }
    WAKEUP_WINSOCK(); /* Why on earth is this needed? */

#endif

    init_workers(num_workers);

    main_loop();
#ifndef WIN32
    kill_all_workers();
#endif
    return 0;
}

static void usage(char *unknown)
{
    fprintf(stderr,"%s: Unknown option \"%s\"\n"
	    "Usage: %s [-d [-d ...]] [-g <greedy threshold>] "
	    "[<number of workers>]\n",
	    program_name, unknown, program_name);
}

/*
 * Main process main loop
 */

static int handle_io_busy(int ndx)
{
    /* Probably an answer */
    int res;
    res = relay_reply(&busy_workers[ndx]);
    if (res < 0) {
	/* Bad worker */
	if (busy_workers[ndx].que_size) {
	    restart_worker(&busy_workers[ndx]);
	    start_que_request(&busy_workers[ndx]);
	    return 0;
	} else {
	    kill_worker(&busy_workers[ndx]);
	    --num_busy_workers;
	    busy_workers[ndx] = busy_workers[num_busy_workers];
	}
	return 1; 
    } else if (res == 0) {
	/* Erlang has closed */
	return -1;
    } else {
	if (busy_workers[ndx].que_size) {
	   start_que_request(&busy_workers[ndx]);
	   return 0;
	}
	/* The worker is no longer busy, it should be in the free list */
	free_workers[num_free_workers] = busy_workers[ndx];
	free_workers[num_free_workers].state = WORKER_FREE;
	++num_free_workers;
	--num_busy_workers;
	busy_workers[ndx] = busy_workers[num_busy_workers];
	return 1;
    }
}

static int handle_io_free(int ndx)
{
    /* IO from a free worker means "kill me" */
    DEBUGF(1,("Free worker[%ld] spontaneously died.",
	      (long) free_workers[ndx].pid));
    kill_worker(&free_workers[ndx]);
    --num_free_workers;
    free_workers[ndx] = free_workers[num_free_workers];
    return 1;
}

static int handle_io_stalled(int ndx)
{
    int res;
    res = ignore_reply(&stalled_workers[ndx]);
    if (res <= 0) {
	/* Bad worker */
	kill_worker(&stalled_workers[ndx]);
	--num_stalled_workers;
	stalled_workers[ndx] = stalled_workers[num_stalled_workers];
	return 1; 
    } else {
	DEBUGF(3,("Ignoring reply from stalled worker[%ld].",
		  (long) stalled_workers[ndx].pid)); 
	free_workers[num_free_workers] = stalled_workers[ndx];
	free_workers[num_free_workers].state = WORKER_FREE;
	++num_free_workers;
	--num_stalled_workers;
	stalled_workers[ndx] = stalled_workers[num_stalled_workers];
	return 1;
    }
}

static void check_que(void) 
{
    /* Check if anything in the que can be handled */
    Worker *cw;

    while (que_first) {
	QueItem *qi,*nxt;
	qi = que_first;
	nxt = qi->next; /* Need to save before it's getting put in another que 
			   in threaded solution */
	if ((cw = pick_worker()) == NULL) {
	    break;
	}
#ifdef WIN32
	{
	    SerialType save_serial = get_serial(que_first->request);
	    if (send_mes_to_worker(que_first, cw) != 0) {
		kill_last_picked_worker();
		continue;
	    }
	    cw->serial = save_serial;
	}
#else	    
	if (send_request_to_worker(que_first->request, 
				   que_first->req_size, cw) != 0) {
	    /* Couldn't send request, kill the worker and retry */
	    kill_last_picked_worker();
	    continue;
	}
	cw->serial = get_serial(que_first->request);
#endif
	/* Went well, lets deque */
	que_first = nxt;
	if (que_first == NULL) {
	    que_last = NULL;
	}
	DEBUGF(3,("Did deque serial %d, Que is %sempty",
		  get_serial(qi->request), (que_first) ? "not " : ""));
#ifndef WIN32
	FREE(qi);
#endif
    }
}

static int clean_que_of(SerialType s)
{
    QueItem **qi;
    int i;

    for(qi=&que_first;*qi != NULL && 
	    s != get_serial((*qi)->request); qi = &((*qi)->next))
	;
    if(*qi != NULL) {
	QueItem *r = *qi;
	*qi = (*qi)->next;
	FREE(r);
	if(que_last == r) {
	    /* Lost the "last" pointer, should be very uncommon
	       if the que is not empty, so we simply do a traversal
	       to reclaim it. */
	    if (que_first == NULL) {
		que_last = NULL;
	    } else {
		for (que_last=que_first;que_last->next != NULL;
		     que_last = que_last->next)
		    ;
	    }
	}
	DEBUGF(3,("Removing serial %d from global que on request, "
		  "que %sempty",s, (que_first) ? "not " : ""));
	return 1;
    }
    for (i = 0; i < num_busy_workers; ++i) {
	for(qi=&(busy_workers[i].que_first);*qi != NULL && 
		s != get_serial((*qi)->request); qi = &((*qi)->next))
	    ;
	if(*qi != NULL) {
	    QueItem *r = *qi;
	    *qi = (*qi)->next;
	    FREE(r);
	    if(busy_workers[i].que_last == r) {
		/* Lost the "last" pointer, should be very uncommon
		   if the que is not empty, so we simply do a traversal
		   to reclaim it. */
		if (busy_workers[i].que_first == NULL) {
		    busy_workers[i].que_last = NULL;
		    if (busy_workers[i].que_size != 1) {
			fatal("Worker que size counter incorrect, internal datastructure error.");
		    }
		} else {
		    for (busy_workers[i].que_last = busy_workers[i].que_first;
			 busy_workers[i].que_last->next != NULL;
			 busy_workers[i].que_last = busy_workers[i].que_last->next)
			;
		}
	    }
	    --(busy_workers[i].que_size);
	    DEBUGF(3,("Removing serial %d from worker[%ld] specific que "
		      "on request, que %sempty", 
		      s, (long) busy_workers[i].pid, 
		      (busy_workers[i].que_first) ? "not " : ""));
	    return 1;
	}
    }
    return 0;
}

static void main_loop(void)
{
    AddrByte *inbuff = NULL;
    int insize;
    int i,w;
#ifdef WIN32
    HANDLE handles[64];
    DWORD num_handles;
    DWORD index;
    QueItem *qi;
#else
    size_t inbuff_size = 0;
    fd_set fds;
    int max_fd;
#endif
    int new_data;
    int save_serial;
    /* It's important that the free workers list is handled first */
    Worker *workers[3] = {free_workers, busy_workers, stalled_workers};
    int *wsizes[3] = {&num_free_workers, &num_busy_workers,
		      &num_stalled_workers};
    int (*handlers[3])(int) = {&handle_io_free, &handle_io_busy, 
			       &handle_io_stalled};
    Worker *cw;
    AddrByte domainbuff[DOMAINNAME_MAX];

#ifdef WIN32

    {
	DWORD dummy;
	/* Create the reader and writer */
	if ((!create_mesq(&to_erlang)) || (!create_mesq(&from_erlang))) {
	    fatal("Could not create message que! errno = %d.",GetLastError());
	}
	if (((HANDLE) _beginthreadex(NULL,0,writer,to_erlang,0,&dummy))
	    == NULL) {
	    fatal("Could not create writer thread! errno = %d.",GetLastError());
	}
	if (((HANDLE) _beginthreadex(NULL,0,reader,from_erlang,0,&dummy)) 
	    == NULL) {
	    fatal("Could not create reader thread! errno = %d.",GetLastError());
	}
	DEBUGF(4,("Created reader and writer threads."));
#ifdef HARDDEBUG
	poll_gethost(__LINE__);
#endif
    }
#endif

    for(;;) {
#ifdef WIN32
	num_handles = 0;
	handles[num_handles++] = event_mesq(from_erlang);
	for (w = 0; w < 3; ++w) {
	    for (i = 0; i < *wsizes[w]; ++i) {
		handles[num_handles++] = event_mesq(workers[w][i].readfrom);
	    }
	}

	if ((index = WaitForMultipleObjects(num_handles, handles, FALSE, INFINITE))
	    == WAIT_FAILED) {
	    fatal("Could not WaitForMultpleObjects! errno = %d.",GetLastError());
	}
	w = 0;
	index -= WAIT_OBJECT_0;

	DEBUGF(4,("Got data on index %d.",index));
	if (index > 0) {
	    if (((int)index - 1) < *wsizes[0]) {
		(*handlers[0])(index - 1);
	    } else if (((int)index - 1) <  ((*wsizes[0]) + (*wsizes[1]))) {
		(*handlers[1])(index - 1 - (*wsizes[0]));
	    } else {
		(*handlers[2])(index - 1 - (*wsizes[0]) - (*wsizes[1]));
	    }
	}
	new_data = (index == 0);
#else
	max_fd = 0;
	FD_ZERO(&fds);
	FD_SET(0,&fds);
	for (w = 0; w < 3; ++w) {
	    for (i = 0; i < *wsizes[w]; ++i) {
		FD_SET(workers[w][i].readfrom,&fds);
		if (workers[w][i].readfrom > max_fd) {
		    max_fd =  workers[w][i].readfrom;
		}
	    }
	}
	for (;;) {
	    if (select(max_fd + 1,&fds,NULL,NULL,NULL) < 0) {
		if (errno == EINTR) {
		    continue;
		} else {
		    fatal("Select failed (invalid internal structures?), "
			  "errno = %d.",errno);
		}
	    }
	    break;
	}
	for (w = 0; w < 3; ++w) {
	    for (i = 0; i < *wsizes[w]; ++i) {
		if (FD_ISSET(workers[w][i].readfrom, &fds)) {
		    int hres = (*handlers[w])(i);
		    if (hres < 0) {
			return;
		    } else {
			i -= hres; /* We'll retry this position, if hres == 1. 
				      The position is usually
				      replaced with another worker, 
				      a worker with
				      I/O usually changes state as we 
				      use blocking file I/O */
		    }
		}
	    }
	}
	new_data = FD_ISSET(0,&fds);

#endif

	check_que();

	/* Now check for new requests... */
	if (new_data) { /* Erlang... */
	    OpType op;
#ifdef WIN32
	    if (!deque_mesq(from_erlang,&qi)) {
		DEBUGF(1,("Erlang has closed."));
		return;
	    }
	    insize = qi->req_size;
	    inbuff = qi->request;
	    DEBUGF(4,("Got data from erlang."));
	    DEBUGF(4,("OPeration == %d.",get_op(inbuff)));
#else
	    insize = read_request(&inbuff, &inbuff_size);
	    if (insize == 0) { /* Other errors taken care of in 
				    read_request */
		DEBUGF(1,("Erlang has closed."));
		return;
	    }
#endif
	    op = get_op(inbuff);
	    if (op == OP_CANCEL_REQUEST) {
		SerialType serial = get_serial(inbuff);
		if (!clean_que_of(serial)) {
		    for (i = 0; i <  num_busy_workers; ++i) {
			if (busy_workers[i].serial == serial) {		    
			    if (busy_workers[i].que_size) {
				restart_worker(&busy_workers[i]);
				start_que_request(&busy_workers[i]);
			    } else {
				stall_worker(i);
				check_que();
			    }
			    break;
			}
		    }
		}
#ifdef WIN32
		FREE(qi);
#endif
		continue; /* New select */
	    } else if (op == OP_CONTROL) {
		CtlType ctl;
		SerialType serial = get_serial(inbuff);
		if (serial != INVALID_SERIAL) {
		    fatal("Invalid serial: %d.", serial);
		}
		switch (ctl = get_ctl(inbuff)) {
		case SETOPT_DEBUG_LEVEL:
		    {
			int tmp_debug_level = get_debug_level(inbuff);
#ifdef WIN32
			if (debug_console_allocated == INVALID_HANDLE_VALUE && 
			    tmp_debug_level > 0) {
			    DWORD res;
			    do_allocate_console();
			    WriteFile(debug_console_allocated,
				      "Hej\n",4,&res,NULL);
			}
#endif
			debug_level = tmp_debug_level;
			DEBUGF(debug_level, ("debug_level = %d", debug_level));
			for (w = 0; w < 3; ++w) {
			    for (i = 0; i < *wsizes[w]; i++) {
				int res;
#ifdef WIN32
				QueItem *m;
#endif
				cw = &(workers[w][i]);
#ifdef WIN32
				m = ALLOC(sizeof(QueItem) - 1 + qi->req_size);
				memcpy(m->request, qi->request,
				       (m->req_size = qi->req_size));
				m->next = NULL;
				if ((res = send_mes_to_worker(m, cw)) != 0) {
				    FREE(m);
				}
#else
				res = send_request_to_worker(inbuff, insize, cw);
#endif
				if (res != 0) {
				    kill_worker(cw);
				    (*wsizes[w])--;
				    *cw = workers[w][*wsizes[w]];
				}
			    }
			}
		    }
	            break;
		default:
		    warning("Unknown control requested from erlang (%d), "
			    "message discarded.", (int) ctl);
		    break;
		} 
#ifdef WIN32
		FREE(qi);
#endif
		continue; /* New select */
	    } else {
		ProtoType proto;
		if (op != OP_GETHOSTBYNAME && op != OP_GETHOSTBYADDR) {
		    warning("Unknown operation requested from erlang (%d), "
			    "message discarded.", op);
#ifdef WIN32
		    FREE(qi);
#endif
		    continue;
		}
		if ((proto = get_proto(inbuff)) != PROTO_IPV4 &&
		    proto != PROTO_IPV6) {
		    warning("Unknown protocol requested from erlang (%d), "
			    "message discarded.", proto);
#ifdef WIN32
		    FREE(qi);
#endif
		    continue;
		}
		if (get_domainname(inbuff,insize,domainbuff) < 0) {
		    warning("Malformed message sent from erlang, no domain, "
			    "message discarded.", op);
#ifdef WIN32
		    FREE(qi);
#endif
		    continue;
		}
	    }   

	    if (BEE_GREEDY()) {
		DEBUGF(4,("Beeing greedy!"));
		if ((cw = pick_worker_greedy(domainbuff)) != NULL) {
		    /* Put it in the worker specific que if the 
		       domainname matches... */
#ifndef WIN32		    
		    QueItem *qi = ALLOC(sizeof(QueItem) - 1 +
					insize);
		    qi->req_size = insize;
		    memcpy(&(qi->request), inbuff, insize);
		    qi->next = NULL;
#endif
		    if (!cw->que_first) {
			cw->que_first = cw->que_last = qi;
		    } else {
			cw->que_last->next = qi;
			cw->que_last = qi;
		    }
		    ++(cw->que_size);
		    continue;
		}
		/* Otherwise busyness as usual */
	    }

	    save_serial = get_serial(inbuff);

	    while ((cw = pick_worker()) != NULL) {
		int res;
#ifdef WIN32
		res = send_mes_to_worker(qi,cw);
#else
		res = send_request_to_worker(inbuff, insize, cw);
#endif
		if (res == 0) {
		    break;
		} else {
		    kill_last_picked_worker();
		}
	    }

	    if (cw == NULL) {
		/* Insert into que */
#ifndef WIN32
		QueItem *qi = ALLOC(sizeof(QueItem) - 1 +
				    insize);
		qi->req_size = insize;
		memcpy(&(qi->request), inbuff, insize);
		qi->next = NULL;
#endif
		if (!que_first) {
		    que_first = que_last = qi;
		} else {
		    que_last->next = qi;
		    que_last = qi;
		}
	    } else {
		cw->serial = save_serial;
		domaincopy(cw->domain, domainbuff);
	    }
	}
    }
}

/*
 * Main process worker administration
 */

static void init_workers(int max)
{
    max_workers = max;
    num_busy_workers = 0;
    num_free_workers = 0;
    num_stalled_workers = 0;

    busy_workers = ALLOC(sizeof(Worker) * max_workers);
    free_workers = ALLOC(sizeof(Worker) * max_workers);
    stalled_workers = ALLOC(sizeof(Worker) * max_workers);
#ifndef WIN32
    init_signals();
#endif
}

#ifdef WIN32
static void kill_worker(Worker *pw)
{
    /* Cannot really kill a thread in win32, have to just leave it to die */
    close_mesq(pw->writeto);
    close_mesq(pw->readfrom);
    pw->state = WORKER_EMPTY;
}
#else
static void kill_worker(Worker *pw)
{
    fd_set fds;
    struct timeval tmo;
    int selret;
    static char buff[1024];

    DEBUGF(3,("Killing worker[%ld] with fd %d, serial %d", 
	      (long) pw->pid,
	      (int) pw->readfrom, 
	      (int) pw->serial));
    kill(pw->pid, SIGUSR1);
    /* This is all just to check that the child died, not 
       really necessary */
    for(;;) {
	FD_ZERO(&fds);
	FD_SET(pw->readfrom, &fds);
	tmo.tv_usec=0;
	tmo.tv_sec = CHILDWAIT_TMO;
	selret = select(pw->readfrom+1, &fds, NULL, NULL, &tmo);
	if (selret < 0) {
	    if (errno != EINTR) {
		warning("Unable to select on dying child file descriptor, "
			"errno = %d.",errno);
		break;
	    }
	} else if (selret == 0) {
	    warning("Timeout waiting for child process to die, "
		    "ignoring child (pid = %d).", pw->pid);
	    break;
	} else {
	    int ret;
	    if ((ret = read(pw->readfrom, buff, 1024)) < 0) {
		if (errno != EINTR) {
		    warning("Child file descriptor not closed properly, "
			    "errno = %d", errno);
		    break;
		}
	    } else if (ret == 0) {
		break;
	    }
	    /* continue */
	}
    }
    /* Waiting is done by signal handler... */
    close(pw->readfrom);
    close(pw->writeto);
    pw->state = WORKER_EMPTY;
    /* Leave rest as is... */
}

static void kill_all_workers(void) 
/* Emergency function, will not check that the children died... */
{
    int i;
    for (i = 0; i < num_busy_workers; ++i) {
	kill(busy_workers[i].pid, SIGUSR1);
    }
    for (i = 0; i < num_free_workers; ++i) {
	kill(free_workers[i].pid, SIGUSR1);
    }
    for (i = 0; i < num_stalled_workers; ++i) {
	kill(stalled_workers[i].pid, SIGUSR1);
    }
}
#endif /* !WIN32 */

static Worker *pick_worker(void)
{
    Worker tmp;
    if (num_free_workers > 0) {
	--num_free_workers;
	tmp = free_workers[num_free_workers];
    } else if (num_stalled_workers > 0) {
	/* "restart" the worker... */
	--num_stalled_workers;
	kill_worker(&(stalled_workers[num_stalled_workers]));
	if (create_worker(&tmp,0) < 0) {
	    warning("Unable to create worker process, insufficient "
		    "resources");
	    return NULL;
	}
    } else {
	if (num_busy_workers == max_workers) {
	    return NULL;
	} 
	if (create_worker(&tmp,0) < 0) {
	    warning("Unable to create worker process, insufficient "
		    "resources");
	    return NULL;
	}
    }
    /* tmp contains a worker now, make it busy and put it in the right 
       array */
    tmp.state = WORKER_BUSY;
    busy_workers[num_busy_workers] = tmp;
    ++num_busy_workers;
    return &(busy_workers[num_busy_workers-1]);
}

static Worker *pick_worker_greedy(AddrByte *domainbuff)
{
    int i;
    int found = -1;
    for (i=0; i < num_busy_workers; ++i) {
	if (domaineq(busy_workers[i].domain, domainbuff)) {
	    if ((found < 0) || (busy_workers[i].que_size < 
				busy_workers[found].que_size)) {
		found = i;
	    }
	}
    }
    if (found >= 0) {
	return &busy_workers[found];
    } 
    return NULL;
}

static void restart_worker(Worker *w)
{
    kill_worker(w);
    if (create_worker(w,1) < 0) {
	fatal("Unable to create worker process, insufficient resources");
    }
}

static void kill_last_picked_worker(void)
{
    kill_worker( &(busy_workers[num_busy_workers-1]));
    --num_busy_workers;
}

/*
 * Starts a request qued to a specific worker, check_que starts normally queued requests.
 * We expect a que here...
 */
static void start_que_request(Worker *w) 
{
    QueItem *qi;
    SerialType save_serial;
    if (!w->que_first || !w->que_size) {
	fatal("Expected que'd requests but found none, "
	      "internal datastructure corrupted!");
    }
    qi = w->que_first;
    w->que_first = w->que_first->next;
    if (!w->que_first) {
	w->que_last = NULL;
    }
    --(w->que_size);
    save_serial = get_serial(qi->request);
#ifdef WIN32
    while (send_mes_to_worker(qi, w) != 0) {
	restart_worker(w);
    }
#else
    while (send_request_to_worker(qi->request, 
				  qi->req_size, w) != 0) {
	restart_worker(w);
    }
#endif
    w->serial = save_serial;
    DEBUGF(3,("Did deque serial %d from worker[%ld] specific que, "
	      "Que is %sempty",
	      get_serial(qi->request), (long) w->pid, 
	      (w->que_first) ? "not " : ""));
#ifndef WIN32
    FREE(qi);
#endif
}

#ifndef WIN32
/* Signal utilities */
static RETSIGTYPE (*sys_sigset(int sig, RETSIGTYPE (*func)(int)))(int)
{
    struct sigaction act, oact;

    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = func;
    sigaction(sig, &act, &oact);
    return(oact.sa_handler);
}


static void sys_sigblock(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_BLOCK, &mask, (sigset_t *)NULL);
}

static void sys_sigrelease(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *)NULL);
}

/* Child signal handler */
void reap_children(int ignored)
{
    int res;
    sys_sigblock(SIGCHLD);
    for (;;) {
	while ((res = waitpid((pid_t)-1, NULL, WNOHANG)) > 0)
	    ;
	if (!(res < 0 && errno == EAGAIN)) {
	    DEBUGF(4,("reap_children: res = %d, errno = %d.",res,errno));
	    break;
	}
    }
    sys_sigrelease(SIGCHLD);
}

static void init_signals(void)
{
    sys_sigset(SIGCHLD,&reap_children); /* SIG_IGN would give same result 
				       on most (?) platforms. */
    sys_sigset(SIGPIPE, SIG_IGN);
}
#endif

static void stall_worker(int ndx)
{
    --num_busy_workers;
    stalled_workers[num_stalled_workers] = busy_workers[ndx];
    stalled_workers[num_stalled_workers].state = WORKER_STALLED;
    busy_workers[ndx] = busy_workers[num_busy_workers];
    DEBUGF(3, ("Stalled worker[%ld]",
	   (long) stalled_workers[num_stalled_workers].pid));
    ++num_stalled_workers;
}


/*
 * Main loop message passing
 */
#ifndef WIN32
static int read_request(AddrByte **buff, size_t *buff_size)
{
    int siz;
    int r;

    if ((r = READ_PACKET_BYTES(0,&siz)) != PACKET_BYTES) {
	if (r == 0) {
	    return 0;
	} else {
	    fatal("Unexpected end of file on main input, errno = %d",errno);
	}
    }

    if (siz > *buff_size) {
	if (*buff_size == 0) {
	    *buff = ALLOC((*buff_size = siz));
	} else {
	    *buff = REALLOC(*buff, (*buff_size = siz));
	}
    }
    if (read_exact(0,*buff, siz) != siz) {
	fatal("Unexpected end of file on main input, errno = %d",errno);
    }
    if (siz < 5) {
	fatal("Unexpected message on main input, message size %d less "
	      "than minimum.");
    }
    return siz;
}

#endif /* !WIN32 */

static OpType get_op(AddrByte *buff)
{
    return (OpType) buff[4];
}

static AddrByte *get_op_addr(AddrByte *buff)
{
    return  buff + 4;
}

static SerialType get_serial(AddrByte *buff)
{
    return get_int32(buff);
}

static ProtoType get_proto(AddrByte *buff)
{
    return (ProtoType) buff[5];
}

static CtlType get_ctl(AddrByte *buff)
{
    return (CtlType) buff[5];
}

static AddrByte *get_data(AddrByte *buff)
{
    return buff + 6;
}

static int get_debug_level(AddrByte *buff)
{
    return get_int32(buff + 6);
}

#ifdef WIN32
static int send_mes_to_worker(QueItem *m, Worker *pw) 
{
    if (!enque_mesq(pw->writeto, m)) {
	warning("Unable to send to child process.");
	return -1;
    }
    return 0;
}
#else
static int send_request_to_worker(AddrByte *pr, int rsize, Worker *pw)    
{
    AddrByte hdr[PACKET_BYTES];

    PUT_PACKET_BYTES(hdr, rsize);
    if (write_exact(pw->writeto, hdr, PACKET_BYTES) < 0) { 
	warning("Unable to write to child process.");
	return -1;
    }
    if (write_exact(pw->writeto, (AddrByte *) pr, rsize) < 0) { 
	warning("Unable to write to child process.");
	return -1;
    }
    return 0;
}
#endif /* !WIN32 */

#ifdef WIN32
static int relay_reply(Worker *pw)
{
    QueItem *m;
    if (!deque_mesq(pw->readfrom,&m)) {
	return 0;
    }
    if (!enque_mesq(to_erlang,m)) {
	FREE(m);
	return 0;
    }
    return 1;
}

static int ignore_reply(Worker *pw) {
    QueItem *m;
    if (!deque_mesq(pw->readfrom,&m)) {
	return 0;
    }
    FREE(m);
    return 1;
}

#else

/* Static buffers used by the next three functions */
static AddrByte *relay_buff = NULL; 
static int relay_buff_size = 0;

static int fillin_reply(Worker *pw)
{
    int length;

    if (READ_PACKET_BYTES(pw->readfrom, &length) != PACKET_BYTES) {
	warning("Malformed reply (header) from worker process %d.", 
		pw->pid);
	return -1;
    }
    
    if (relay_buff_size < (length + PACKET_BYTES)) { 
	if (!relay_buff_size) {
	    relay_buff = 
		ALLOC((relay_buff_size = (length + PACKET_BYTES)));
	} else {
	    relay_buff = 
		REALLOC(relay_buff,
			(relay_buff_size = (length + PACKET_BYTES)));
	}
    }
    PUT_PACKET_BYTES(relay_buff, length);
    if (read_exact(pw->readfrom, relay_buff + PACKET_BYTES, length) != 
	length) {
	warning("Malformed reply (data) from worker process %d.", pw->pid);
	return -1;
    }
    return length;
}

static int relay_reply(Worker *pw)
{
    int length = fillin_reply(pw); /* Filled into the "global" buffer */
    int res;

    if (length < 0) {
	return -1;
    }
    if ((res = write_exact(1, relay_buff, length + PACKET_BYTES)) < 0) {
	fatal("Cannot write reply to erlang process, errno = %d.", errno);
    } else if (res == 0) {
	DEBUGF(1,("Erlang has closed write pipe."));
	return 0;
    }
    return length;
}

static int ignore_reply(Worker *pw)
{
    return fillin_reply(pw);
}

#endif /* !WIN32 */

/*
 * Domain name "parsing" and worker specific queing
 */
static void domaincopy(AddrByte *out, AddrByte *in)
{
    AddrByte *ptr = out; 
    *ptr++ = *in++;
    *ptr++ = *in++;
    switch(*out) {
    case OP_GETHOSTBYNAME:
	while(*in != '\0' && *in != '.')
	    ++in;
	strncpy((char*)ptr, (char*)in, DOMAINNAME_MAX-2);
	ptr[DOMAINNAME_MAX-3] = '\0';
	DEBUGF(4,("Saved domainname %s.", ptr));
	return;
    case OP_GETHOSTBYADDR:
	memcpy(ptr,in, ((out[1] == PROTO_IPV4) ? UNIT_IPV4 : UNIT_IPV6) - 1);
	DEBUGF(4, ("Saved domain address: %s.", 
		   format_address(((out[1] == PROTO_IPV4) ? 
				   UNIT_IPV4 : UNIT_IPV6) - 1,ptr)));
	return;
    default:
	fatal("Trying to copy buffer not containing valid domain, [%d,%d].",
	      (int) out[0], (int) out[1]);
    }
}

static int domaineq(AddrByte *d1, AddrByte *d2)
{
    if (d1[0] != d2[0] || d1[1] != d2[1]) {
	return 0;
    }
    switch (d1[0]) {
    case OP_GETHOSTBYNAME:
	return !strcmp((char*)d1+2,(char*)d2+2);
    case OP_GETHOSTBYADDR:
	return !memcmp(d1+2,d2+2, ((d1[1] == PROTO_IPV4) 
				   ? UNIT_IPV4 : UNIT_IPV6) - 1);
    default:
	fatal("Trying to compare buffers not containing valid domain, "
	      "[%d,%d].",
	      (int) d1[0], (int) d1[1]);
	return -1; /* Lint... */
    }
}
	    
static int get_domainname(AddrByte *inbuff, int insize, AddrByte *domainbuff)
{
    OpType op = get_op(inbuff);
    ProtoType proto;
    int i;
    AddrByte *data;

    data = get_data(inbuff);
    switch (op) {
    case OP_GETHOSTBYNAME:
	data = get_data(inbuff);
	for (i = (data - inbuff); i < insize && inbuff[i] != '\0'; ++i)
	    ;
	if (i < insize) {
	    domaincopy(domainbuff, get_op_addr(inbuff)); 
	    return 0;
	}
	DEBUGF(3, ("Could not pick valid domainname in "
		   "gethostbyname operation")); 
	return -1;
    case OP_GETHOSTBYADDR:
	proto = get_proto(inbuff);
	i = insize - (data - inbuff); 
	if ((proto == PROTO_IPV4 && i == UNIT_IPV4) || 
	    (proto == PROTO_IPV6 && i == UNIT_IPV6)) { 
	    /* An address buffer */
	    domaincopy(domainbuff, get_op_addr(inbuff));
	    return 0;
	}
	DEBUGF(3, ("Could not pick valid domainname in gethostbyaddr "
		   "operation")); 
	return -1;
    default:
	DEBUGF(2, ("Could not pick valid domainname because of "
		   "invalid opcode %d.", (int) op)); 
	return -1;
    }
}

/*
 * Worker subprocesses with utilities
 */
#ifdef WIN32
static int create_worker(Worker *pworker, int save_que)
{
    MesQ **thread_data = ALLOC(2*sizeof(MesQ *));
    DWORD tid;


    if (!create_mesq(thread_data)) {
	fatal("Could not create, pipes for subprocess, errno = %d",
		GetLastError());
    }
    if (!create_mesq(thread_data + 1)) {
	fatal("Could not create, pipes for subprocess, errno = %d",
		GetLastError());
    }
    /* Save those before the thread starts */
    pworker->writeto = thread_data[0];
    pworker->readfrom = thread_data[1];

    if (((HANDLE) _beginthreadex(NULL, 0, worker_loop, thread_data, 0, &tid))
	== NULL) {
	fatal("Could not create thread errno = %d",
	      GetLastError());
    }
    pworker->pid = tid;
    pworker->state = WORKER_FREE;
    pworker->serial = INVALID_SERIAL;
    if (!save_que) {
	pworker->que_first = pworker->que_last = NULL;
	pworker->que_size = 0;
    }
    DEBUGF(3,("Created worker[%ld] with fd %d", 
	      (long) pworker->pid, (int) pworker->readfrom));
    return 0;
}

#else

static int create_worker(Worker *pworker, int save_que)
{
    int p0[2], p1[2];
    pid_t child;

    if (pipe(p0)) {
	warning("Could not create, pipes for subprocess, errno = %d",
		errno);
	return -1;
    }

    if (pipe(p1)) {
	warning("Could not create, pipes for subprocess, errno = %d",
		errno);
	close(p0[0]);
	close(p0[1]);
	return -1;
    }
    if ((child = fork()) < 0) { /* failure */
	warning("Could not fork(), errno = %d",
		errno);
	close(p0[0]);
	close(p0[1]);
	close(p1[0]);
	close(p1[1]);
	return -1;
    } else if (child > 0) { /* parent */
	close(p0[1]);
	close(p1[0]);
	pworker->writeto = p1[1];
	pworker->readfrom = p0[0];
	pworker->pid = child;
	pworker->state = WORKER_FREE;
	pworker->serial = INVALID_SERIAL;
	if (!save_que) {
	    pworker->que_first = pworker->que_last = NULL;
	    pworker->que_size = 0;
	}
	DEBUGF(3,("Created worker[%ld] with fd %d", 
		  (long) pworker->pid, (int) pworker->readfrom));
	return 0;
    } else { /* child */
	close(p1[1]);
	close(p0[0]);
	close_all_worker_fds();
	/* Make "fatal" not find any children */
	num_busy_workers = num_free_workers = num_stalled_workers = 0;
	if((dup2(p1[0],0) < 0) || (dup2(p0[1],1) < 0)) {
	    fatal("Worker could not dup2(), errno = %d",
		  errno);
	    return -1; /* lint... */
	}
	close(p1[0]);
	close(p0[1]);
	signal(SIGCHLD, SIG_IGN); 
	return worker_loop();
    }
}

static void close_all_worker_fds(void) 
{
    int w,i;
    Worker *workers[3] = {free_workers, busy_workers, stalled_workers};
    int wsizes[3] = {num_free_workers, num_busy_workers,
		      num_stalled_workers};
    for (w = 0; w < 3; ++w) {
	for (i = 0; i < wsizes[w]; ++i) {
	    if (workers[w][i].state != WORKER_EMPTY) {
		close(workers[w][i].readfrom);
		close(workers[w][i].writeto);
	    }
	}
    }
}

#endif /* !WIN32 */

#ifdef WIN32
DWORD WINAPI worker_loop(void *v)
#else
static int worker_loop(void)
#endif
{
    AddrByte *req = NULL;
    size_t req_size = 0;
    int this_size;
    AddrByte *reply = NULL;
    size_t reply_size = 0;
    size_t data_size;

#ifdef WIN32
    QueItem *m = NULL;
    MesQ *readfrom = ((MesQ **) v)[0];
    MesQ *writeto = ((MesQ **) v)[1];
    /* XXX:PaN */
    FREE(v);
#endif

    for(;;) {
#ifdef HAVE_GETADDRINFO
	struct addrinfo *ai = NULL;
#endif
	struct hostent *he = NULL;
#ifdef HAVE_GETNAMEINFO
	struct sockaddr *sa = NULL;
	char name[NI_MAXHOST];
#endif
#if defined(HAVE_GETIPNODEBYNAME) || defined(HAVE_GETIPNODEBYADDR)
	int free_he = 0;
#endif
	int error_num = 0;
	SerialType serial;
	OpType op;
	ProtoType proto;
	AddrByte *data;

#ifdef WIN32
	WaitForSingleObject(event_mesq(readfrom),INFINITE);
	DEBUGF(4,("Worker got data on message que."));

	if(!deque_mesq(readfrom,&m)) {
	    goto fail;
	}
	this_size = m->req_size;
	req = m->request;
#else	
	if (READ_PACKET_BYTES(0,&this_size) != PACKET_BYTES) {
	    DEBUGF(2,("Worker got error/EOF while reading size, exiting."));
	    exit(0);
	}
	if (this_size > req_size) {
	    if (req == NULL) {
		req = ALLOC((req_size = this_size));
	    } else {
		req = REALLOC(req, (req_size = this_size));
	    }
	}
	if (read_exact(0, req, (size_t) this_size) != this_size) {
	    DEBUGF(1,("Worker got EOF while reading data, exiting."));
	    exit(0);
	}
#endif
	/* Decode the request... */
	serial = get_serial(req);
	if (OP_CONTROL == (op = get_op(req))) {
	    CtlType ctl;
	    if (serial != INVALID_SERIAL) {
		DEBUGF(1, ("Worker got invalid serial: %d.", serial));
		exit(0);
	    }
	    switch (ctl = get_ctl(req)) {
	    case SETOPT_DEBUG_LEVEL:
		debug_level = get_debug_level(req);
		DEBUGF(debug_level, 
		       ("Worker debug_level = %d.", debug_level));
		break;
	    }
	    continue;
	}
	proto = get_proto(req);
	data = get_data(req);
	DEBUGF(4,("Worker got request, op = %d, proto = %d, data = %s.",
		  op,proto,data));
	/* Got a request, lets go... */
	switch (op) {
	case OP_GETHOSTBYNAME:
	    switch (proto) {
		
#ifdef HAVE_IN6
	    case PROTO_IPV6: { /* switch (proto) { */
#ifdef HAVE_GETADDRINFO
		struct addrinfo hints;
		
		memset(&hints, 0, sizeof(hints));
		hints.ai_flags = AI_CANONNAME;
		hints.ai_socktype = SOCK_STREAM;
		hints.ai_family = AF_INET6;
		DEBUGF(5, ("Starting getaddrinfo(%s, ...)", data));
		error_num = getaddrinfo((char *)data, NULL, &hints, &ai);
		DEBUGF(5,("getaddrinfo returned %d", error_num));
		if (error_num) {
		    error_num = map_netdb_error_ai(error_num);
		}
#elif defined(HAVE_GETIPNODEBYNAME) /*#ifdef HAVE_GETADDRINFO */
		DEBUGF(5,("Starting getipnodebyname(%s)",data));
		he = getipnodebyname(data, AF_INET6, 0, &error_num);
		if (he) {
		    free_he = 1;
		    error_num = 0;
		    DEBUGF(5,("getipnodebyname(,AF_INET6,,) OK"));
		} else {
		    DEBUGF(5,("getipnodebyname(,AF_INET6,,) error %d", error_num));
		    error_num = map_netdb_error(error_num);
		}
#elif defined(HAVE_GETHOSTBYNAME2) /*#ifdef HAVE_GETADDRINFO */
		DEBUGF(5,("Starting gethostbyname2(%s, AF_INET6)",data));
		he = gethostbyname2((char*)data, AF_INET6);
		if (he) {
		    error_num = 0;
		    DEBUGF(5,("gethostbyname2(, AF_INET6) OK"));
		} else {
		    error_num = map_netdb_error(h_errno);
		    DEBUGF(5,("gethostbyname2(, AF_INET6) error %d", h_errno));
		}
#else
		error_num = ERRCODE_NOTSUP;
#endif /*#ifdef HAVE_GETADDRINFO */
	    } break;
#endif /*ifdef HAVE_IN6 */
	    
	    case PROTO_IPV4: { /* switch (proto) { */
		DEBUGF(5,("Starting gethostbyname(%s)",data));
		he = gethostbyname((char*)data);
		if (he) {
		    error_num = 0;
		    DEBUGF(5,("gethostbyname OK"));
		} else {
		    error_num = map_netdb_error(h_errno);
		    DEBUGF(5,("gethostbyname error %d", h_errno));
		}
	    } break;
	    
	    default: /* switch (proto) { */
		/* Not supported... */
		error_num = ERRCODE_NOTSUP;
		break;
	    } /* switch (proto) { */
	    
	    if (he) {
		data_size = build_reply(serial, he, &reply, &reply_size);
#ifdef HAVE_GETIPNODEBYNAME
		if (free_he) {
		    freehostent(he);
		}
#endif
#ifdef HAVE_GETADDRINFO
	    } else if (ai) {
		data_size = build_reply_ai(serial, 16, ai,
					   &reply, &reply_size);
		freeaddrinfo(ai);
#endif
	    } else {
		data_size = build_error_reply(serial, error_num, 
					      &reply, &reply_size);
	    }
	    break; /* case OP_GETHOSTBYNAME: */

	case OP_GETHOSTBYADDR: /* switch (op) { */
	    switch (proto) {
#ifdef HAVE_IN6
	    case PROTO_IPV6: {
#ifdef HAVE_GETNAMEINFO
		struct sockaddr_in6 *sin;
		socklen_t salen = sizeof(*sin);
		
		sin = ALLOC(salen);
#ifndef NO_SA_LEN
		sin->sin6_len = salen;
#endif
		sin->sin6_family = AF_INET6;
		sin->sin6_port = 0;
		memcpy(&sin->sin6_addr, data, 16);
		sa = (struct sockaddr *)sin;
		DEBUGF(5,("Starting getnameinfo(,,%s,16,,,)",
			  format_address(16, data)));
		error_num = getnameinfo(sa, salen, name, sizeof(name),
					NULL, 0, NI_NAMEREQD);
		DEBUGF(5,("getnameinfo returned %d", error_num));
		if (error_num) {
		    error_num = map_netdb_error_ai(error_num);
		    sa = NULL;
		}
#elif defined(HAVE_GETIPNODEBYADDR) /*#ifdef HAVE_GETNAMEINFO*/
		struct in6_addr ia;
		memcpy(ia.s6_addr, data, 16);
		DEBUGF(5,("Starting getipnodebyaddr(%s,16,AF_INET6,)",
			  format_address(16, data)));
		he = getipnodebyaddr(&ia, 16, AF_INET6, &error_num);
		free_he = 1;
		if (! he) {
		    DEBUGF(5,("getipnodebyaddr error %d", error_num));
		    error_num = map_netdb_error(error_num);
		} else {
		    DEBUGF(5,("getipnodebyaddr OK"));
		}
#else /*#ifdef HAVE_GETNAMEINFO*/
		struct in6_addr ia;
		memcpy(ia.s6_addr, data, 16);
		DEBUGF(5,("Starting gethostbyaddr(%s,16,AF_INET6)",
			  format_address(16, data)));
		he = gethostbyaddr((const char *) &ia, 16, AF_INET6);
		if (! he) {
		    error_num = map_netdb_error(h_errno);
		    DEBUGF(5,("gethostbyaddr error %d", h_errno));
		} else {
		    DEBUGF(5,("gethostbyaddr OK"));
		}
#endif /* #ifdef HAVE_GETNAMEINFO */
	    } break; /* case PROTO_IPV6: { */
#endif /* #ifdef HAVE_IN6 */
			    
	    case PROTO_IPV4: { /* switch(proto) { */
		struct in_addr ia;
		memcpy(&ia.s_addr, data, 4); /* Alignment required... */
		DEBUGF(5,("Starting gethostbyaddr(%s,4,AF_INET)",
			  format_address(4, data)));
		he = gethostbyaddr((const char *) &ia, 4, AF_INET);
		if (! he) {
		    error_num = map_netdb_error(h_errno);
		    DEBUGF(5,("gethostbyaddr error %d", h_errno));
		} else {
		    DEBUGF(5,("gethostbyaddr OK"));
		}
	    } break;
	    
	    default:
		error_num = ERRCODE_NOTSUP;
	    } /* switch(proto) { */
	    
	    if (he) {
		data_size = build_reply(serial, he, &reply, &reply_size);
#ifdef HAVE_GETIPNODEBYADDR
		if (free_he) {
		    freehostent(he);
		}
#endif
#ifdef HAVE_GETNAMEINFO
	    } else if (sa) {
		struct addrinfo res;
		memset(&res, 0, sizeof(res));
		res.ai_canonname = name;
		res.ai_addr = sa;
		res.ai_next = NULL;
		data_size = build_reply_ai(serial, 16, &res,
					   &reply, &reply_size);
		free(sa);
#endif
	    } else {
		data_size = build_error_reply(serial, error_num, 
					      &reply, &reply_size);
	    }
	    break; /* case OP_GETHOSTBYADR: */

	default:
	    data_size = build_error_reply(serial, ERRCODE_NOTSUP, 
					  &reply, &reply_size);
	    break;
	} /* switch (op) { */

#ifdef WIN32
	m = REALLOC(m, sizeof(QueItem) - 1 + data_size - PACKET_BYTES);
	m->next = NULL;
	m->req_size = data_size - PACKET_BYTES;
	memcpy(m->request,reply + PACKET_BYTES,data_size - PACKET_BYTES);
	if (!enque_mesq(writeto,m)) {
	    goto fail;
	}
	m = NULL;
#else
	/* expect no signals */
	if (write(1, reply, data_size) < 0)
	    goto fail;
#endif
    } /* for (;;) */

 fail:
#ifdef WIN32
    if (m != NULL) {
	FREE(m);
    }
    close_mesq(readfrom);
    close_mesq(writeto);
    if (reply) {
	FREE(reply);
    }
#endif
    return 1;
}

static int map_netdb_error(int netdb_code)
{
    switch (netdb_code) {
#ifdef HOST_NOT_FOUND
    case HOST_NOT_FOUND:
	return ERRCODE_HOST_NOT_FOUND;
#endif
#ifdef TRY_AGAIN
    case TRY_AGAIN:
	return ERRCODE_TRY_AGAIN;
#endif
#ifdef NO_RECOVERY
    case NO_RECOVERY:
	return ERRCODE_NO_RECOVERY;
#endif
#if defined(NO_DATA) || defined(NO_ADDRESS)
#ifdef NO_DATA
    case NO_DATA:
#endif
#ifdef NO_ADDRESS
#if !defined(NO_DATA) || (NO_DATA != NO_ADDRESS)	
    case NO_ADDRESS:
#endif
#endif
	return ERRCODE_NO_DATA;
#endif
    default:
	return ERRCODE_NETDB_INTERNAL;
    }
}

#if defined(HAVE_GETADDRINFO) || defined(HAVE_GETNAMEINFO)
static int map_netdb_error_ai(int netdb_code)
{
    switch(netdb_code) {
#ifdef EAI_ADDRFAMILY
    case EAI_ADDRFAMILY:
	return ERRCODE_NETDB_INTERNAL;
#endif
    case EAI_AGAIN:
	return ERRCODE_TRY_AGAIN;
    case EAI_BADFLAGS:
	return ERRCODE_NETDB_INTERNAL;
    case EAI_FAIL:
	return ERRCODE_HOST_NOT_FOUND;
    case EAI_FAMILY:
	return ERRCODE_NETDB_INTERNAL;
    case EAI_MEMORY:
	return ERRCODE_NETDB_INTERNAL;
#if defined(EAI_NODATA) && EAI_NODATA != EAI_NONAME
    case EAI_NODATA:
	return ERRCODE_HOST_NOT_FOUND;
#endif
    case EAI_NONAME:
	return ERRCODE_HOST_NOT_FOUND;
    case EAI_SERVICE:
	return ERRCODE_NETDB_INTERNAL;
    case EAI_SOCKTYPE:
	return ERRCODE_NETDB_INTERNAL;
    default:
	return ERRCODE_NETDB_INTERNAL;
    }
}
#endif /* #if defined(HAVE_GETADDRINFO) || defined(HAVE_GETNAMEINFO) */


static char *errcode_to_string(int errcode)
{
    switch (errcode) {
    case ERRCODE_NOTSUP:
	return "enotsup";
    case ERRCODE_HOST_NOT_FOUND:
	/* 
	 * I would preffer 
	 * return "host_not_found";
	 * but have to keep compatibility with the old 
	 * inet_gethost's error codes... 
	 */
	return "notfound";
    case ERRCODE_TRY_AGAIN:
	return "try_again";
    case ERRCODE_NO_RECOVERY:
	return "no_recovery";
    case ERRCODE_NO_DATA:
	return "no_data";
    default:
	/*case ERRCODE_NETDB_INTERNAL:*/
	return "netdb_internal";
    }
}
	
static size_t build_error_reply(SerialType serial, int errnum, 
				AddrByte **preply,
				size_t *preply_size)
{
    char *errstring = errcode_to_string(errnum);
    int string_need = strlen(errstring) + 1; /* a '\0' too */
    unsigned need;
    AddrByte *ptr;

    need = PACKET_BYTES + 4 /* Serial */ + 1 /* Unit */ + string_need;
    if (*preply_size < need) {
	if (*preply_size == 0) {
	    *preply = ALLOC((*preply_size = need));
	} else {
	    *preply = REALLOC(*preply, 
			      (*preply_size = need));
	}
    }
    ptr = *preply;
    PUT_PACKET_BYTES(ptr,need - PACKET_BYTES);
    ptr += PACKET_BYTES;
    put_int32(ptr,serial);
    ptr +=4;
    *ptr++ = (AddrByte) 0; /* 4 or 16 */
    strcpy((char*)ptr, errstring);
    return need;
}
    


static size_t build_reply(SerialType serial, struct hostent *he, 
			  AddrByte **preply, size_t *preply_size)
{
    unsigned need;
    int strings_need;
    int num_strings;
    int num_addresses;
    int i;
    AddrByte *ptr;
    int unit = he->h_length;

    for (num_addresses = 0; he->h_addr_list[num_addresses] != NULL; 
	 ++num_addresses)
	;
    strings_need = strlen(he->h_name) + 1; /* 1 for null byte */
    num_strings = 1;
    if (he->h_aliases) {
	for(i=0; he->h_aliases[i] != NULL; ++i) {
	    strings_need += strlen(he->h_aliases[i]) + 1;
	    ++num_strings;
	}
    }
    
    need = PACKET_BYTES + 
	4 /* Serial */ + 1 /* Unit */ + 4 /* Naddr */ +
	(unit * num_addresses) /* Address bytes */ +
	4 /* Nnames */ + strings_need /* The name and alias strings */;

    if (*preply_size < need) {
	if (*preply_size == 0) {
	    *preply = ALLOC((*preply_size = need));
	} else {
	    *preply = REALLOC(*preply, 
			      (*preply_size = need));
	}
    }
    ptr = *preply;
    PUT_PACKET_BYTES(ptr,need - PACKET_BYTES);
    ptr += PACKET_BYTES;
    put_int32(ptr,serial);
    ptr +=4;
    *ptr++ = (AddrByte) unit; /* 4 or 16 */
    put_int32(ptr, num_addresses);
    ptr += 4;
    for (i = 0; i < num_addresses; ++i) {
	memcpy(ptr, he->h_addr_list[i], unit);
	ptr += unit;
    }
    put_int32(ptr, num_strings);
    ptr += 4;
    strcpy((char*)ptr, he->h_name);
    ptr += 1 + strlen(he->h_name);
    for (i = 0; i < (num_strings - 1); ++i) {
	strcpy((char*)ptr, he->h_aliases[i]);
	ptr += 1 + strlen(he->h_aliases[i]);
    }
    return need;
}

#if defined(HAVE_GETADDRINFO) || defined(HAVE_GETNAMEINFO)
static size_t build_reply_ai(SerialType serial, int addrlen,
			     struct addrinfo *res0,
			     AddrByte **preply, size_t *preply_size)
{
    struct addrinfo *res;
    int num_strings;
    int num_addresses;
    AddrByte *ptr;
    int need;

    num_addresses = 0;
    num_strings = 0;
    need = PACKET_BYTES +
	4 /* Serial */ + 1 /* addrlen */ +
	4 /* Naddr */ + 4 /* Nnames */;

    for (res = res0; res != NULL; res = res->ai_next) {
	if (res->ai_addr) {
	    num_addresses++;
	    need += addrlen;
	}
	if (res->ai_canonname) {
	    num_strings++;
	    need += strlen(res->ai_canonname) + 1;
	}
    }

    if (*preply_size < need) {
	if (*preply_size == 0) {
	    *preply = ALLOC((*preply_size = need));
	} else {
	    *preply = REALLOC(*preply,
			      (*preply_size = need));
	}
    }

    ptr = *preply;
    PUT_PACKET_BYTES(ptr,need - PACKET_BYTES);
    ptr += PACKET_BYTES;
    put_int32(ptr,serial);
    ptr +=4;
    *ptr++ = (AddrByte) addrlen; /* 4 or 16 */
    put_int32(ptr, num_addresses);
    ptr += 4;
    for (res = res0; res != NULL && num_addresses; res = res->ai_next) {
	if (res->ai_addr == NULL)
	    continue;
	if (addrlen == 4)
	    memcpy(ptr, &((struct sockaddr_in *)res->ai_addr)->sin_addr, addrlen);
#ifdef AF_INET6
	else if (addrlen == 16)
	    memcpy(ptr, &((struct sockaddr_in6 *)res->ai_addr)->sin6_addr, addrlen);
#endif
	else
	    memcpy(ptr, res->ai_addr->sa_data, addrlen);
	ptr += addrlen;
	num_addresses--;
    }
    put_int32(ptr, num_strings);
    ptr += 4;
    for (res = res0; res != NULL && num_strings; res = res->ai_next) {
	if (res->ai_canonname == NULL)
	    continue;
	strcpy((char *)ptr, res->ai_canonname);
	ptr += strlen(res->ai_canonname) + 1;
	num_strings--;
    }
    return need;
}

#endif /* #if defined(HAVE_GETADDRINFO) || defined(HAVE_GETNAMEINFO) */



/*
 * Encode/decode/read/write 
 */

static int get_int32(AddrByte *b) 
{
    int res;
    res = (unsigned) b[3];
    res |= ((unsigned) b[2]) << 8;
    res |= ((unsigned) b[1]) << 16;
    res |= ((unsigned) b[0]) << 24;
    return res;
}

static void put_int32(AddrByte *buff, int value)
{
    buff[0] = (((unsigned) value) >> 24) & 0xFF;
    buff[1] = (((unsigned) value) >> 16) & 0xFF;
    buff[2] = (((unsigned) value) >> 8) & 0xFF;
    buff[3] = ((unsigned) value) & 0xFF;
}
#ifdef WIN32

static int read_int32(HANDLE fd, int *res, HANDLE ev)
{
    AddrByte b[4];
    int r;
    if ((r = read_exact(fd,b,4,ev)) < 0) {
	return -1;
    } else if (r == 0) {
	return 0;
    } else {
	*res = (unsigned) b[3];
	*res |= ((unsigned) b[2]) << 8;
	*res |= ((unsigned) b[1]) << 16;
	*res |= ((unsigned) b[0]) << 24;
    }
    return 4;
}
/*
 * The standard input is expected to be opened with FILE_FLAG_OVERLAPPED
 * but this code should handle both cases (although winsock might not).
 */
static int read_exact(HANDLE fd, void *vbuff, DWORD nbytes, HANDLE ev)
{
    DWORD ret,got;
    BOOL stat;
    char *buff = vbuff;
    OVERLAPPED ov;
    DWORD err;

    
    got = 0;
    for(;;) {
	memset(&ov,0,sizeof(ov));
	ov.hEvent = ev;
	ResetEvent(ov.hEvent);
	stat = ReadFile(fd, buff, nbytes - got, &ret, &ov);
	if (!stat) {
	    if ((err = GetLastError()) == ERROR_IO_PENDING) {
		DEBUGF(4,("Overlapped read, waiting for completion..."));
		WaitForSingleObject(ov.hEvent,INFINITE);
		stat = GetOverlappedResult(fd,&ov,&ret,TRUE);
		DEBUGF(4,("Overlapped read, completed with status %d,"
			  " result %d",stat,ret));
	    } 
	    if (!stat) {
		if (GetLastError() == ERROR_BROKEN_PIPE) {
		    DEBUGF(1, ("End of file while reading from pipe."));
		    return 0;
		} else {
		    DEBUGF(1, ("Error while reading from pipe,"
			       " errno = %d",
			       GetLastError()));
		    return -1;
		}
	    }
	} else {
	    DEBUGF(4,("Read completed syncronously, result %d",ret));
	}	    
	if (ret == 0) {
	    DEBUGF(1, ("End of file detected as zero read from pipe."));
	    return 0;
	}
	if (ret < nbytes - got) {
	    DEBUGF(4,("Not all data read from pipe, still %d bytes to read.",
		      nbytes - (got + ret)));
	    got += ret;
	    buff += ret;
	} else {
	    return nbytes;
	}
    }
}
/*
 * Now, we actually expect a HANDLE opened with FILE_FLAG_OVERLAPPED,
 * but this code should handle both cases (although winsock
 * does not always..)
 */
static int write_exact(HANDLE fd, AddrByte *buff, DWORD len, HANDLE ev) 
{
    DWORD res,stat;
    DWORD x = len;
    OVERLAPPED ov;
    DWORD err;


    for(;;) {
	memset(&ov,0,sizeof(ov));
	ov.hEvent = ev;
	ResetEvent(ov.hEvent);
	stat = WriteFile(fd,buff,x,&res,&ov);
	if (!stat) {
	    if ((err = GetLastError()) == ERROR_IO_PENDING) {
		DEBUGF(4,("Overlapped write, waiting for competion..."));
		WaitForSingleObject(ov.hEvent,INFINITE);
		stat = GetOverlappedResult(fd,&ov,&res,TRUE);
		DEBUGF(4,("Overlapped write, completed with status %d,"
			  " result %d",stat,res));
	    }
	    if (!stat) {
		if (GetLastError() == ERROR_BROKEN_PIPE) {
		    return 0;
		} else {
		    return -1;
		}
	    }
	} else {
	    DEBUGF(4,("Write completed syncronously, result %d",res));
	}
	    
	if (res < x) {
	    /* Microsoft states this can happen as HANDLE is a pipe... */
	    DEBUGF(4,("Not all data written to pipe, still %d bytes to write.",
		      x - res));
	    x -= res;
	    buff += res;
	} else {
	    return len;
	}
    }
}

DWORD WINAPI reader(void *data) {
    MesQ *mq = (MesQ *) data;
    QueItem *m;
    int siz;
    int r;
    HANDLE inp;
    int x = 0;
    HANDLE ev = CreateEvent(NULL, TRUE, FALSE, NULL);

    inp = GetStdHandle(STD_INPUT_HANDLE);
    for (;;) {
	if ((r = READ_PACKET_BYTES(inp,&siz,ev)) != 4) {
	    DEBUGF(1,("Erlang has closed (reading)"));
	    exit(0);
	}
	DEBUGF(4,("Read packet of size %d from erlang",siz));
	m = ALLOC(sizeof(QueItem) - 1 + siz);
	if (read_exact(inp, m->request, siz,ev) != siz) {
	    fatal("Unexpected end of file on main input, errno = %d",errno);
	}
	if (siz < 5) {
	    fatal("Unexpected message on main input, message size %d less "
		  "than minimum.");
	}
	m->req_size = siz;
	m->next = NULL;
	if (!enque_mesq(mq, m)) {
	     fatal("Reader could not talk to main thread!");
	}
    }
}	

DWORD WINAPI writer(void *data) 
{
    MesQ *mq = (MesQ *) data;
    QueItem *m;
    HANDLE outp = GetStdHandle(STD_OUTPUT_HANDLE);
    AddrByte hdr[PACKET_BYTES];
    HANDLE ev = CreateEvent(NULL, TRUE, FALSE, NULL);


    for (;;) {
	WaitForSingleObject(event_mesq(mq),INFINITE);
	if (!deque_mesq(mq, &m)) {
	    fatal("Writer could not talk to main thread!");
	}
	PUT_PACKET_BYTES(hdr, m->req_size);
	if (write_exact(outp, hdr, 4, ev) != 4) {
	    DEBUGF(1,("Erlang has closed (writing)"));
	    exit(0);
	}
	if (write_exact(outp, m->request, m->req_size, ev) != m->req_size) {
	    DEBUGF(1,("Erlang has closed (writing)"));
	    exit(0);
	}
	FREE(m);
    }
}


#else

static size_t read_int32(int fd, int *res)
{
    AddrByte b[4];
    int r;
    if ((r = read_exact(fd,b,4)) < 0) {
	return -1;
    } else if (r == 0) {
	return 0;
    } else {
	*res = (unsigned) b[3];
	*res |= ((unsigned) b[2]) << 8;
	*res |= ((unsigned) b[1]) << 16;
	*res |= ((unsigned) b[0]) << 24;
    }
    return 4;
}

static ssize_t read_exact(int fd, void *vbuff, size_t nbytes)
{
    ssize_t ret, got;
    char *buff = vbuff;

    got = 0;
    for(;;) {
	ret = read(fd, buff, nbytes - got);
	if (ret < 0) {
	    if (errno == EINTR) {
		continue;
	    } else {
		DEBUGF(1, ("Error while reading from pipe,"
			   " errno = %d",
			   errno));
		return -1;
	    }
	} else if (ret == 0) {
	    DEBUGF(1, ("End of file while reading from pipe."));
	    if (got == 0) {
		return 0; /* "Normal" EOF */
	    } else {
		return -1;
	    }
	} else if (ret < nbytes - got) {
	    got += ret;
	    buff += ret;
	} else {
	    return nbytes;
	}
    }
}

static int write_exact(int fd, AddrByte *buff, int len) 
{
    int res;
    int x = len;
    for(;;) {
	if((res = write(fd, buff, x)) == x) {
	    break;
	}
	if (res < 0) {
	    if (errno == EINTR) {
		continue;
	    } else if (errno == EPIPE) {
		return 0;
	    }
#ifdef ENXIO
	    else if (errno == ENXIO) {
		return 0;
	    }
#endif
	    else {
		return -1;
	    }
	} else {
	    /* Hmmm, blocking write but not all written, could this happen
	       if the other end was closed during the operation? Well, 
	       it costs very little to handle anyway... */
	    x -= res;
	    buff += res;
	}
    }
    return len;
}

#endif /* !WIN32 */

/*
 * Debug and memory allocation
 */

static char *format_address(int siz, AddrByte *addr)
{
    static char buff[50];
    char tmp[10];
    if (siz > 16) {
	return "(unknown)";
    }
    *buff='\0';
    if (siz <= 4) {
	while(siz--) {
	    erts_snprintf(tmp, sizeof(tmp), "%d",(int) *addr++);
	    strcat(buff,tmp);
	    if(siz) {
		strcat(buff,".");
	    }
	}
	return buff;
    } 
    while(siz--) {
	erts_snprintf(tmp, sizeof(tmp), "%02x",(int) *addr++);
	strcat(buff,tmp);
	if(siz) {
	    strcat(buff,":");
	}
    }
    return buff;
}

static void debugf(char *format, ...)
{
    char buff[2048];
    char *ptr;
    va_list ap;

    va_start(ap,format);
#ifdef WIN32
    erts_snprintf(buff, sizeof(buff), "%s[%d] (DEBUG):",program_name,(int) GetCurrentThreadId());
#else
    erts_snprintf(buff, sizeof(buff), "%s[%d] (DEBUG):",program_name,(int) getpid());
#endif
    ptr = buff + strlen(buff);
    erts_vsnprintf(ptr,sizeof(buff)-strlen(buff)-2,format,ap);
    strcat(ptr,"\r\n");
#ifdef WIN32
    if (debug_console_allocated != INVALID_HANDLE_VALUE) {
	DWORD res;
	WriteFile(debug_console_allocated,buff,strlen(buff),&res,NULL);
    }
#else
    /* suppress warning with 'if' */
    if(write(2,buff,strlen(buff)))
	;
#endif
    va_end(ap);
}

static void warning(char *format, ...)
{
    char buff[2048];
    char *ptr;
    va_list ap;

    va_start(ap,format);
    erts_snprintf(buff, sizeof(buff), "%s[%d]: WARNING:",program_name, (int) getpid());
    ptr = buff + strlen(buff);
    erts_vsnprintf(ptr,sizeof(buff)-strlen(buff)-2,format,ap);
    strcat(ptr,"\r\n");
#ifdef WIN32
    {
	DWORD res;
	WriteFile(GetStdHandle(STD_ERROR_HANDLE),buff,strlen(buff),&res,NULL);
    }
#else
    /* suppress warning with 'if' */
    if(write(2,buff,strlen(buff)))
	;
#endif
    va_end(ap);
}

static void fatal(char *format, ...)
{
    char buff[2048];
    char *ptr;
    va_list ap;

    va_start(ap,format);
    erts_snprintf(buff, sizeof(buff), "%s[%d]: FATAL ERROR:",program_name, (int) getpid());
    ptr = buff + strlen(buff);
    erts_vsnprintf(ptr,sizeof(buff)-strlen(buff)-2,format,ap);
    strcat(ptr,"\r\n");
#ifdef WIN32
    {
	DWORD res;
	WriteFile(GetStdHandle(STD_ERROR_HANDLE),buff,strlen(buff),&res,NULL);
    }
#else
    /* suppress warning with 'if' */
    if(write(2,buff,strlen(buff)))
	;
#endif
    va_end(ap);
#ifndef WIN32
    kill_all_workers();
#endif
    exit(1);
}

static void *my_malloc(size_t size)
{ 
    void *ptr = malloc(size);
    if (!ptr) {
	fatal("Cannot allocate %u bytes of memory.", (unsigned) size);
	return NULL; /* lint... */
    }
    return ptr;
}

static void *my_realloc(void *old, size_t size)
{
    void *ptr = realloc(old, size);
    if (!ptr) {
	fatal("Cannot reallocate %u bytes of memory from %p.",
	      (unsigned) size, old);
	return NULL; /* lint... */
    }
    return ptr;
}

#ifdef WIN32

BOOL create_mesq(MesQ **q) 
{
    MesQ *tmp = ALLOC(sizeof(MesQ));
    tmp->data_present = CreateEvent(NULL, TRUE, FALSE,NULL);
    if (tmp->data_present == NULL) {
	free(tmp);
	return FALSE;
    }
    InitializeCriticalSection(&(tmp->crit)); /* Cannot fail */
    tmp->shutdown = 0;
    tmp->first = NULL;
    tmp->last = NULL;
    *q = tmp;
    return TRUE;
}

BOOL enque_mesq(MesQ *q, QueItem *m)
{
    EnterCriticalSection(&(q->crit));
    if (q->shutdown) {
	LeaveCriticalSection(&(q->crit));
	return FALSE;
    }
    if (q->last == NULL) {
	q->first = q->last = m;
    } else {
	q->last->next = m;
	q->last = m;
    }
    m->next = NULL;
    if (!SetEvent(q->data_present)) {
	fprintf(stderr,"Fatal: Unable to signal event in %s:%d, last error: %d\n",
		__FILE__,__LINE__,GetLastError());
	exit(1); /* Unable to continue at all */
    }
    LeaveCriticalSection(&(q->crit));
    return TRUE;
}

BOOL deque_mesq(MesQ *q, QueItem **m)
{
    EnterCriticalSection(&(q->crit));
    if (q->first == NULL) { /* Usually shutdown from other end */
	ResetEvent(q->data_present); 
	LeaveCriticalSection(&(q->crit));
	return FALSE;
    }
    *m = q->first;
    q->first = q->first->next;
    if (q->first == NULL) {
	q->last = NULL;
	ResetEvent(q->data_present);
    }
    (*m)->next = NULL;
    LeaveCriticalSection(&(q->crit));
    return TRUE;
}

BOOL close_mesq(MesQ *q)
{
    QueItem *tmp;
    EnterCriticalSection(&(q->crit));
    if (!q->shutdown) {
	q->shutdown = TRUE;
	if (!SetEvent(q->data_present)) {
	    fprintf(stderr,
		    "Fatal: Unable to signal event in %s:%d, last error: %d\n",
		    __FILE__,__LINE__,GetLastError());
	    exit(1); /* Unable to continue at all */
	}
	LeaveCriticalSection(&(q->crit));
	return FALSE;
    }
    /* No one else is supposed to use this object any more */
    LeaveCriticalSection(&(q->crit));
    DeleteCriticalSection(&(q->crit));
    CloseHandle(q->data_present);
    tmp = q->first;
    while(tmp) {
	q->first = q->first->next;
	free(tmp);
	tmp = q->first;
    }
    free(q);
    return TRUE;
}

HANDLE event_mesq(MesQ *q)
{
    return q->data_present;
}

#ifdef HARDDEBUG
DWORD WINAPI pseudo_worker_loop(void *v)
{
    HOSTENT *hep;
    
    DEBUGF(1,("gethostbyname(\"ftp.funet.fi\") starting"));
    hep = gethostbyname("ftp.funet.fi");

    DEBUGF(1,("gethostbyname(\"ftp.funet.fi\") -> %d OK",(int) hep));
    return 0;
}    

static void poll_gethost(int row) {
    HANDLE h;
    DWORD tid;
    h = (HANDLE) _beginthreadex(NULL, 0, pseudo_worker_loop, NULL, 0, &tid);
    if (h == NULL) {
	DEBUGF(1,("Failed to spawn pseudo worker (%d)...",row));
    } else {
	DEBUGF(1,("Waiting for pseudo worker (%d)", row));
	WaitForSingleObject(h,INFINITE);
	DEBUGF(1,("Done Waiting for pseudo worker (%d)", row));
    }
}
#endif
    
#endif /* WIN32 */
