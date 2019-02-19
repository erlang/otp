/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
/**
 *
 *  File:     heart.c
 *  Purpose:  Portprogram for supervision of the Erlang emulator.
 *
 *  Synopsis: heart
 *
 *  SPAWNING FROM ERLANG
 *
 *  This program is started from Erlang as follows,
 *
 *      Port = open_port({spawn, 'heart'}, [{packet, 2}]),
 *
 *  ROLE OF THIS PORT PROGRAM
 *
 *  This program is started by the Erlang emulator. It  communicates
 *  with the emulator through file descriptor 0 (standard input).
 *
 *  MESSAGE FORMAT
 *
 *  All messages have the following format (a value in parentheses
 *  indicate field length in bytes),
 *
 *      {Length(2), Operation(1)}
 *
 *  START ACK
 *
 *  When this program has started it sends an START ACK message to Erlang.
 *
 *  HEART_BEATING
 *
 *  This program expects a heart beat message. If it does not receive a
 *  heart beat message from Erlang within heart_beat_timeout seconds, it
 *  reboots the system. The system is rebooted by executing the command
 *  stored in the environment variable HEART_COMMAND.
 *
 *  BLOCKING DESCRIPTORS
 *
 *  All file descriptors in this program are blocking. This can lead
 *  to deadlocks. The emulator reads and writes are blocking.
 *
 *  STANDARD INPUT, OUTPUT AND ERROR
 *
 *  This program communicates with Erlang through the standard
 *  input and output file descriptors (0 and 1). These descriptors
 *  (and the standard error descriptor 2) must NOT be closed
 *  explicitely by this program at termination (in UNIX it is
 *  taken care of by the operating system itself).
 *
 *  END OF FILE
 *
 *  If a read from a file descriptor returns zero (0), it means
 *  that there is no process at the other end of the connection
 *  having the connection open for writing (end-of-file).
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef __WIN32__
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <process.h>
#endif

/*
 * Implement time correction using times() call even on Linuxes 
 * that can simulate gethrtime with clock_gettime, no use implementing
 * a phony gethrtime in this file as the time questions are so infrequent.
 */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

#include <stdarg.h>

#include <string.h>
#include <time.h>
#include <errno.h>

#if !defined(__WIN32__)
#  include <sys/types.h>
#  include <netinet/in.h>
#  include <sys/time.h>
#  include <unistd.h>
#  include <signal.h>
#  if defined(OS_MONOTONIC_TIME_USING_TIMES)
#    include <sys/times.h>
#    include <limits.h>
#  endif
#endif

#define HEART_COMMAND_ENV          "HEART_COMMAND"
#define ERL_CRASH_DUMP_SECONDS_ENV "ERL_CRASH_DUMP_SECONDS"
#define HEART_KILL_SIGNAL          "HEART_KILL_SIGNAL"
#define HEART_NO_KILL              "HEART_NO_KILL"


#define MSG_HDR_SIZE         (2)
#define MSG_HDR_PLUS_OP_SIZE (3)
#define MSG_BODY_SIZE        (2048)
#define MSG_TOTAL_SIZE       (2050)

unsigned char cmd[MSG_BODY_SIZE];

struct msg {
  unsigned short len;
  unsigned char op;
  unsigned char fill[MSG_BODY_SIZE]; /* one too many */
};

/* operations */
#define  HEART_ACK       (1)
#define  HEART_BEAT      (2)
#define  SHUT_DOWN       (3)
#define  SET_CMD         (4)
#define  CLEAR_CMD       (5)
#define  GET_CMD         (6)
#define  HEART_CMD       (7)
#define  PREPARING_CRASH (8)


/*  Maybe interesting to change */

/* Times in seconds */
#define  SELECT_TIMEOUT               5  /* Every 5 seconds we reset the
					    watchdog timer */

/* heart_beat_timeout is the maximum gap in seconds between two
   consecutive heart beat messages from Erlang. */

int heart_beat_timeout = 60;
/* All current platforms have a process identifier that
   fits in an unsigned long and where 0 is an impossible or invalid value */
unsigned long heart_beat_kill_pid = 0;

/* reasons for reboot */
#define  R_TIMEOUT          (1)
#define  R_CLOSED           (2)
#define  R_ERROR            (3)
#define  R_SHUT_DOWN        (4)
#define  R_CRASHING         (5) /* Doing a crash dump and we will wait for it */


/*  macros */

#define  NULLFDS  ((fd_set *) NULL)
#define  NULLTV   ((struct timeval *) NULL)

/*  prototypes */

static int message_loop(int, int);
static void do_terminate(int, int);
static int notify_ack(int);
static int heart_cmd_reply(int, char *);
static int write_message(int, struct msg *);
static int read_message(int, struct msg *);
static int read_skip(int, char *, int, int);
static int read_fill(int, char *, int);
static void print_error(const char *,...);
static void debugf(const char *,...);
static void init_timestamp(void);
static time_t timestamp(time_t *);
static int  wait_until_close_write_or_env_tmo(int);

#ifdef __WIN32__
static BOOL enable_privilege(void);
static BOOL do_shutdown(int);
static void print_last_error(void);
static HANDLE start_reader_thread(void);
static DWORD WINAPI reader(LPVOID);
#define read _read
#define write _write
#endif

/*  static variables */

static char program_name[256];
static int erlin_fd = 0, erlout_fd = 1; /* std in and out */
static int debug_on = 0;
#ifdef __WIN32__
static HANDLE hreader_thread;
static HANDLE hevent_dataready;
static struct msg m, *mp = &m;
static int   tlen;			/* total message length */
static FILE* conh;

#endif

static int
is_env_set(char *key)
{
#ifdef __WIN32__
    char buf[1];
    DWORD sz = (DWORD) sizeof(buf);
    SetLastError(0);
    sz = GetEnvironmentVariable((LPCTSTR) key, (LPTSTR) buf, sz);
    return sz || GetLastError() != ERROR_ENVVAR_NOT_FOUND; 
#else
    return getenv(key) != NULL;
#endif
}

static char *
get_env(char *key)
{
#ifdef __WIN32__
    DWORD size = 32;
    char  *value=NULL;
    wchar_t *wcvalue = NULL;
    wchar_t wckey[256];
    int len; 

    MultiByteToWideChar(CP_UTF8, 0, key, -1, wckey, 256);
    
    while (1) {
	DWORD nsz;
	if (wcvalue)
	    free(wcvalue);
	wcvalue = malloc(size*sizeof(wchar_t));
	if (!wcvalue) {
	    print_error("Failed to allocate memory. Terminating...");
	    exit(1);
	}
	SetLastError(0);
	nsz = GetEnvironmentVariableW(wckey, wcvalue, size);
	if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
	    free(wcvalue);
	    return NULL;
	}
	if (nsz <= size) {
	    len = WideCharToMultiByte(CP_UTF8, 0, wcvalue, -1, NULL, 0, NULL, NULL);
	    value = malloc(len*sizeof(char));
	    if (!value) {
		print_error("Failed to allocate memory. Terminating...");
		exit(1);
	    }
	    WideCharToMultiByte(CP_UTF8, 0, wcvalue, -1, value, len, NULL, NULL);
	    free(wcvalue);
	    return value;
	}
	size = nsz;
    }
#else
    return getenv(key);
#endif
}

static void
free_env_val(char *value)
{
#ifdef __WIN32__
    if (value)
	free(value);
#endif
}

/*
 *  main
 */
static void get_arguments(int argc, char** argv) {
    int i = 1;
    int h;
    unsigned long p;

    while (i < argc) {
	switch (argv[i][0]) {
	case '-':
	    switch (argv[i][1]) {
	    case 'h':
		if (strcmp(argv[i], "-ht") == 0) 
		    if (sscanf(argv[i+1],"%i",&h) ==1)
			if ((h > 10) && (h <= 65535)) {
			    heart_beat_timeout = h;
			    fprintf(stderr,"heart_beat_timeout = %d\n",h);
			    i++;
			}
		break;
	    case 'p':
		if (strcmp(argv[i], "-pid") == 0)
		    if (sscanf(argv[i+1],"%lu",&p) ==1){
			heart_beat_kill_pid = p;
			fprintf(stderr,"heart_beat_kill_pid = %lu\n",p);
			i++;
		    }
		break;
#ifdef __WIN32__
	    case 's':
		if (strcmp(argv[i], "-shutdown") == 0){
		    do_shutdown(1);
		    exit(0);
		}
		break;
#endif
	    default:
		;
	    }
	    break;
	default:
	    ;
	}
	i++;
    }
    debugf("arguments -ht %d -pid %lu\n",h,p);
}

int main(int argc, char **argv) {

    if (is_env_set("HEART_DEBUG")) {
	fprintf(stderr, "heart: debug is ON!\r\n");
	debug_on = 1;
    }

    get_arguments(argc,argv);
#ifdef __WIN32__
    if (debug_on) {
	if(!is_env_set("ERLSRV_SERVICE_NAME")) {
	    /* this redirects stderr to a separate console (for debugging purposes)*/
	    erlin_fd = _dup(0);
	    erlout_fd = _dup(1);
	    AllocConsole();
	    conh = freopen("CONOUT$","w",stderr);
	    if (conh != NULL)
		fprintf(conh,"console alloced\n");
	}
	debugf("stderr\n");
    }
    _setmode(erlin_fd,_O_BINARY);
    _setmode(erlout_fd,_O_BINARY);
#endif
    strncpy(program_name, argv[0], sizeof(program_name));
    program_name[sizeof(program_name)-1] = '\0';
    notify_ack(erlout_fd);
    cmd[0] = '\0';
    do_terminate(erlin_fd,message_loop(erlin_fd,erlout_fd));
    return 0;
}

/*
 * message loop
 */
static int
message_loop(erlin_fd, erlout_fd)
     int   erlin_fd, erlout_fd;
{
  int   i;
  time_t now, last_received;
#ifdef __WIN32__
  DWORD wresult;
#else
  fd_set read_fds;
  int   max_fd;
  struct timeval timeout;
  int   tlen;			/* total message length */
  struct msg m, *mp = &m;
#endif
  
  init_timestamp();
  timestamp(&now);
  last_received = now;
#ifdef __WIN32__
  hevent_dataready = CreateEvent(NULL,FALSE,FALSE,NULL);
  hreader_thread = start_reader_thread();
#else
  max_fd = erlin_fd;
#endif

  while (1) {
      /* REFACTOR: below to select/tmo function */
#ifdef __WIN32__
	wresult = WaitForSingleObject(hevent_dataready,SELECT_TIMEOUT*1000+ 2);
	if (wresult == WAIT_FAILED) {
		print_last_error();
		return R_ERROR;
	}

	if (wresult == WAIT_TIMEOUT) {
		debugf("wait timed out\n");
		i = 0;
	} else {
		debugf("wait ok\n");
		i = 1;
	}
#else
    FD_ZERO(&read_fds);         /* ZERO on each turn */
    FD_SET(erlin_fd, &read_fds);
    timeout.tv_sec = SELECT_TIMEOUT;  /* On Linux timeout is modified 
					 by select */
    timeout.tv_usec = 0;
    if ((i = select(max_fd + 1, &read_fds, NULLFDS, NULLFDS, &timeout)) < 0) {
      print_error("error in select.");
      return R_ERROR;
    }
#endif
    /*
     * Maybe heart beat time-out
     * If we havn't got anything in 60 seconds we reboot, even if we may
     * have got something in the last 5 seconds. We may end up here if
     * the system clock is adjusted with more than 55 seconds, but we
     * regard this as en error and reboot anyway.
     */
    timestamp(&now);
    if (now > last_received + heart_beat_timeout) {
	print_error("heart-beat time-out, no activity for %lu seconds", 
		    (unsigned long) (now - last_received));
		return R_TIMEOUT;
    }
    /*
     * Do not check fd-bits if select timeout
     */
    if (i == 0) {
      continue;
    }
    /*
     * Message from ERLANG
     */
#ifdef __WIN32__
	if (wresult == WAIT_OBJECT_0) {
		if (tlen < 0) {
#else
    if (FD_ISSET(erlin_fd, &read_fds)) {
		if ((tlen = read_message(erlin_fd, mp)) < 0) {
#endif
			print_error("error in read_message.");
			return R_ERROR;
		}
		if ((tlen > MSG_HDR_SIZE) && (tlen <= MSG_TOTAL_SIZE)) {
			switch (mp->op) {
			case HEART_BEAT:
				timestamp(&last_received);
				break;
			case SHUT_DOWN:
				return R_SHUT_DOWN;
			case SET_CMD:
				/* override the HEART_COMMAND_ENV command */
			        memcpy(&cmd, &(mp->fill[0]), 
				       tlen-MSG_HDR_PLUS_OP_SIZE);
			        cmd[tlen-MSG_HDR_PLUS_OP_SIZE] = '\0';
			        notify_ack(erlout_fd);
			        break;
			case CLEAR_CMD:
				/* use the HEART_COMMAND_ENV command */
				cmd[0] = '\0';
				notify_ack(erlout_fd);
				break;
			case GET_CMD:
				/* send back command string */
			        {
				    char *env = NULL;
				    char *command
					= (cmd[0]
					   ? (char *)cmd
					   : (env = get_env(HEART_COMMAND_ENV)));
				    /* Not set and not in env  return "" */
				    if (!command) command = "";
				    heart_cmd_reply(erlout_fd, command);
				    free_env_val(env);
				}
			        break;
			case PREPARING_CRASH:
				/* Erlang has reached a crushdump point (is crashing for sure) */
				print_error("Erlang is crashing .. (waiting for crash dump file)");
				return R_CRASHING;
			default:
				/* ignore all other messages */
				break;
			}
		} else if (tlen == 0) {
		/* Erlang has closed its end */
		print_error("Erlang has closed.");
		return R_CLOSED;
    }
		/* Junk erroneous messages */
    }
  }
}

#if defined(__WIN32__)
static void 
kill_old_erlang(int reason){
    HANDLE erlh;
    DWORD exit_code;
    char* envvar = NULL;

    envvar = get_env(HEART_NO_KILL);
    if (envvar && strcmp(envvar, "TRUE") == 0)
      return;

    if(heart_beat_kill_pid != 0){
	if((erlh = OpenProcess(PROCESS_TERMINATE | 
			       SYNCHRONIZE | 
			       PROCESS_QUERY_INFORMATION ,
			       FALSE,
			       (DWORD) heart_beat_kill_pid)) == NULL){
	    return;
	}
	if(!TerminateProcess(erlh, 1)){
	    CloseHandle(erlh);
	    return;
	}
	if(WaitForSingleObject(erlh,5000) != WAIT_OBJECT_0){
	    print_error("Old process did not die, "
			"WaitForSingleObject timed out.");
	    CloseHandle(erlh);
	    return;
	}
	if(!GetExitCodeProcess(erlh, &exit_code)){
	    print_error("Old process did not die, "
			"GetExitCodeProcess failed.");
	}
	CloseHandle(erlh);
    }
}
#else
static void 
kill_old_erlang(int reason)
{
    pid_t pid;
    int i, res;
    int sig = SIGKILL;
    char *envvar = NULL;

    envvar = get_env(HEART_NO_KILL);
    if (envvar && strcmp(envvar, "TRUE") == 0)
      return;

    if(heart_beat_kill_pid != 0){
        pid = (pid_t) heart_beat_kill_pid;
        if (reason == R_CLOSED) {
            print_error("Wait 5 seconds for Erlang to terminate nicely");
            for (i=0; i < 5; ++i) {
                res = kill(pid, 0); /* check if alive */
                if (res < 0 && errno == ESRCH)
                    return;
                sleep(1);
            }
            print_error("Erlang still alive, kill it");
        }

        envvar = get_env(HEART_KILL_SIGNAL);
        if (envvar && strcmp(envvar, "SIGABRT") == 0) {
            print_error("kill signal SIGABRT requested");
            sig = SIGABRT;
        }

	res = kill(pid,sig);
	for(i=0; i < 5 && res == 0; ++i){
	    sleep(1);
	    res = kill(pid,sig);
	}
	if(errno != ESRCH){
	    print_error("Unable to kill old process, "
			"kill failed (tried multiple times).");
	}
    }
}
#endif

#ifdef __WIN32__
void win_system(char *command)
{
    char *comspec;
    char * cmdbuff;
    char * extra = " /C ";
    wchar_t *wccmdbuff;
    char *env;
    STARTUPINFOW start;
    SECURITY_ATTRIBUTES attr;
    PROCESS_INFORMATION info;
    int len;

    if (!debug_on) {
	len = MultiByteToWideChar(CP_UTF8, 0, command, -1, NULL, 0);
	wccmdbuff = malloc(len*sizeof(wchar_t));
	if (!wccmdbuff) {
	    print_error("Failed to allocate memory. Terminating...");
	    exit(1);
	}
	MultiByteToWideChar(CP_UTF8, 0, command, -1, wccmdbuff, len);
	_wsystem(wccmdbuff);
	return;
    }
    comspec = env = get_env("COMSPEC");
    if (!comspec)
	comspec = "CMD.EXE";
    cmdbuff = malloc(strlen(command) + strlen(comspec) + strlen(extra) + 1);
    if (!cmdbuff) {
	print_error("Failed to allocate memory. Terminating...");
	exit(1);
    }
    strcpy(cmdbuff, comspec);
    strcat(cmdbuff, extra);
    strcat(cmdbuff, command);
    free_env_val(env);

    debugf("running \"%s\"\r\n", cmdbuff);

    memset (&start, 0, sizeof (start));
    start.cb = sizeof (start);
    start.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
    start.wShowWindow = SW_HIDE;
    start.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    start.hStdOutput = GetStdHandle(STD_ERROR_HANDLE);
    start.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    attr.nLength = sizeof(attr);
    attr.lpSecurityDescriptor = NULL;
    attr.bInheritHandle = TRUE;

    fflush(stderr);

    len = MultiByteToWideChar(CP_UTF8, 0, cmdbuff, -1, NULL, 0);
    wccmdbuff = malloc(len*sizeof(wchar_t));
    if (!wccmdbuff) {
	print_error("Failed to allocate memory. Terminating...");
	exit(1);
    }
    MultiByteToWideChar(CP_UTF8, 0, cmdbuff, -1, wccmdbuff, len);

    if (!CreateProcessW(NULL,
			wccmdbuff,
			&attr,
			NULL,
			TRUE,
			0,
			NULL,
			NULL,
			&start,
			&info)) {
	debugf("Could not create process for the command %s.\r\n", cmdbuff);
    }
    WaitForSingleObject(info.hProcess,INFINITE);
    free(cmdbuff);
    free(wccmdbuff);
}
#endif /* defined(__WIN32__) */

/*
 * do_terminate
 */
static void 
do_terminate(int erlin_fd, int reason) {
  int ret = 0, tmo=0;
  char *tmo_env;

  switch (reason) {
  case R_SHUT_DOWN:
    break;
  case R_CRASHING:
    if (is_env_set(ERL_CRASH_DUMP_SECONDS_ENV)) {
	tmo_env = get_env(ERL_CRASH_DUMP_SECONDS_ENV);
	tmo = atoi(tmo_env);
	print_error("Waiting for dump - timeout set to %d seconds.", tmo);
	wait_until_close_write_or_env_tmo(tmo);
	free_env_val(tmo_env);
    }
    /* fall through */
  case R_TIMEOUT:
  case R_CLOSED:
  case R_ERROR:
  default:
    {
#if defined(__WIN32__) /* Not VxWorks */
	if(!cmd[0]) {
	    char *command = get_env(HEART_COMMAND_ENV);
	    if(!command)
		print_error("Would reboot. Terminating.");
	    else {
		kill_old_erlang(reason);
		/* High prio combined with system() works badly indeed... */
		SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
		win_system(command);
		print_error("Executed \"%s\". Terminating.",command);
	    }
	    free_env_val(command);
	} else {
	    kill_old_erlang(reason);
	    /* High prio combined with system() works badly indeed... */
	    SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);
	    win_system(&cmd[0]);
	    print_error("Executed \"%s\". Terminating.",cmd);
	}
#else
	if(!cmd[0]) {
	    char *command = get_env(HEART_COMMAND_ENV);
	    if(!command)
		print_error("Would reboot. Terminating.");
	    else {
		kill_old_erlang(reason);
		ret = system(command);
		print_error("Executed \"%s\" -> %d. Terminating.",command, ret);
	    }
	    free_env_val(command);
	} else {
	    kill_old_erlang(reason);
	    ret = system((char*)&cmd[0]);
	    print_error("Executed \"%s\" -> %d. Terminating.",cmd, ret);
	}
#endif
    }
    break;
  } /* switch(reason) */
}


/* Waits until something happens on socket or handle
 *
 * Uses global variables erlin_fd or hevent_dataready
 */
int wait_until_close_write_or_env_tmo(int tmo) {
    int i = 0;

#ifdef __WIN32__
    DWORD wresult;
    DWORD wtmo = INFINITE;

    if (tmo >= 0) {
	wtmo = tmo*1000 + 2;
    }

    wresult = WaitForSingleObject(hevent_dataready, wtmo);
    if (wresult == WAIT_FAILED) {
	print_last_error();
	return -1;
    }

    if (wresult == WAIT_TIMEOUT) {
	debugf("wait timed out\n");
	i = 0;
    } else {
	debugf("wait ok\n");
	i = 1;
    }
#else
    fd_set read_fds;
    int   max_fd;
    struct timeval timeout;
    struct timeval *tptr = NULL;

    max_fd = erlin_fd; /* global */

    if (tmo >= 0) {
	timeout.tv_sec  = tmo;  /* On Linux timeout is modified by select */
	timeout.tv_usec = 0;
	tptr = &timeout;
    }

    FD_ZERO(&read_fds);
    FD_SET(erlin_fd, &read_fds);
    if ((i = select(max_fd + 1, &read_fds, NULLFDS, NULLFDS, tptr)) < 0) {
	print_error("error in select.");
	return -1;
    }
#endif
    return i;
}


/*
 * notify_ack
 *
 * Sends an HEART_ACK.
 */
static int
notify_ack(fd)
  int   fd;
{
  struct msg m;
  
  m.op = HEART_ACK;
  m.len = htons(1);
  return write_message(fd, &m);
}


/*
 * send back current command
 *
 * Sends an HEART_CMD.
 */
static int
heart_cmd_reply(int fd, char *s)
{
  struct msg m;
  int len = strlen(s);

  /* if s >= MSG_BODY_SIZE, return a write
   * failure immediately.
   */
  if (len >= sizeof(m.fill))
      return -1;

  m.op = HEART_CMD;
  m.len = htons(len + 1);	/* Include Op */
  strcpy((char*)m.fill, s);

  return write_message(fd, &m);
}


/*
 *  write_message
 *
 *  Writes a message to a blocking file descriptor. Returns the total
 *  size of the message written (always > 0), or -1 if error.
 *
 *  A message which is too short or too long, is not written. The return
 *  value is then MSG_HDR_SIZE (2), as if the message had been written.
 *  Is this really necessary? Can't we assume that the length is ok?
 *  FIXME.
 */
static int
write_message(fd, mp)
  int   fd;
  struct msg *mp;
{
  int len = ntohs(mp->len);

  if ((len == 0) || (len > MSG_BODY_SIZE)) {
    return MSG_HDR_SIZE;
  }				/* cc68k wants (char *) */
  if (write(fd, (char *) mp, len + MSG_HDR_SIZE) != len + MSG_HDR_SIZE) {
    return -1;
  }
  return len + MSG_HDR_SIZE;
}

/*
 *  read_message
 *
 *  Reads a message from a blocking file descriptor. Returns the total
 *  size of the message read (> 0), 0 if eof, and < 0 if error.
 *
 *  Note: The return value MSG_HDR_SIZE means a message of total size
 *  MSG_HDR_SIZE, i.e. without even an operation field.
 *
 *  If the size of the message is larger than MSG_TOTAL_SIZE, the total
 *  number of bytes read is returned, but the buffer contains a truncated
 *  message.
 */
static int
read_message(fd, mp)
  int   fd;
  struct msg *mp;
{
  int   rlen, i;
  unsigned char* tmp;

  if ((i = read_fill(fd, (char *) mp, MSG_HDR_SIZE)) != MSG_HDR_SIZE) {
    /* < 0 is an error; = 0 is eof */
    return i;
  }

  tmp = (unsigned char*) &(mp->len);
  rlen = (*tmp * 256) + *(tmp+1);
  if (rlen == 0) {
    return MSG_HDR_SIZE;
  }
  if (rlen > MSG_BODY_SIZE) {
    if ((i = read_skip(fd, (((char *) mp) + MSG_HDR_SIZE),
		       MSG_BODY_SIZE, rlen)) != rlen) {
      return i;
    } else {
      return rlen + MSG_HDR_SIZE;
    }
  }
  if ((i = read_fill(fd, ((char *) mp + MSG_HDR_SIZE), rlen)) != rlen) {
    return i;
  }
  return rlen + MSG_HDR_SIZE;
}

/*
 *  read_fill
 *
 *  Reads len bytes into buf from a blocking fd. Returns total number of
 *  bytes read (i.e. len) , 0 if eof, or < 0 if error. len must be > 0.
 */
static int
read_fill(fd, buf, len)
  int   fd, len;
  char *buf;
{
  int   i, got = 0;

  do {
    if ((i = read(fd, buf + got, len - got)) <= 0) {
      return i;
    }
    got += i;
  } while (got < len);
  return len;
}

/*
 *  read_skip
 *
 *  Reads len bytes into buf from a blocking fd, but puts not more than
 *  maxlen bytes in buf. Returns total number of bytes read ( > 0),
 *  0 if eof, or < 0 if error. len > maxlen > 0 must hold.
 */
static int
read_skip(fd, buf, maxlen, len)
  int   fd, maxlen, len;
  char *buf;
{
  int   i, got = 0;
  char  c;

  if ((i = read_fill(fd, buf, maxlen)) <= 0) {
    return i;
  }
  do {
    if ((i = read(fd, &c, 1)) <= 0) {
      return i;
    }
    got += i;
  } while (got < len - maxlen);
  return len;
}

/*
 *  print_error
 */
static void
print_error(const char *format,...)
{
  va_list args;
  time_t now;
  char *timestr;

  va_start(args, format);
  time(&now);
  timestr = ctime(&now);
  fprintf(stderr, "%s: %.*s: ", program_name, (int) strlen(timestr)-1, timestr);
  vfprintf(stderr, format, args);
  va_end(args);
  fprintf(stderr, "\r\n");
}

static void
debugf(const char *format,...)
{
  va_list args;

  if (debug_on) {
      va_start(args, format);
      fprintf(stderr, "Heart: ");
      vfprintf(stderr, format, args);
      va_end(args);
      fprintf(stderr, "\r\n");
  }
}

#ifdef __WIN32__
void print_last_error() {
	LPVOID lpMsgBuf;
	FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		NULL,
		GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
		(LPTSTR) &lpMsgBuf,
		0,
		NULL 
	);

	/* Display the string.*/
	fprintf(stderr,"GetLastError:%s\n",lpMsgBuf);

	/* Free the buffer. */
	LocalFree( lpMsgBuf );
}


static BOOL enable_privilege() {
	HANDLE ProcessHandle;
	DWORD DesiredAccess = TOKEN_ADJUST_PRIVILEGES;
	HANDLE TokenHandle;
	TOKEN_PRIVILEGES Tpriv;
	LUID luid;
	ProcessHandle = GetCurrentProcess();
	OpenProcessToken(ProcessHandle, DesiredAccess, &TokenHandle);
	LookupPrivilegeValue(0,SE_SHUTDOWN_NAME,&luid);
	Tpriv.PrivilegeCount = 1;
	Tpriv.Privileges[0].Luid = luid;
	Tpriv.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
	return AdjustTokenPrivileges(TokenHandle,FALSE,&Tpriv,0,0,0);
}

static BOOL do_shutdown(int really_shutdown) {
    enable_privilege();
    if (really_shutdown) {
	if (InitiateSystemShutdown(NULL,"shutdown by HEART",10,TRUE,TRUE))
	    return TRUE;
    } else if (InitiateSystemShutdown(NULL,
				      "shutdown by HEART\n"
				      "will be interrupted",
				      30,TRUE,TRUE)) {
	AbortSystemShutdown(NULL);
	return TRUE;
    }
    return FALSE;
}

DWORD WINAPI reader(LPVOID lpvParam) {

	while (1) {
		debugf("reader is reading\n");
		tlen = read_message(erlin_fd, mp);
		debugf("reader setting event\n");
		SetEvent(hevent_dataready);
		if(tlen == 0)
		    break;
	}
	return 0;
}

HANDLE start_reader_thread(void) {
	DWORD tid;
	HANDLE thandle;
	if ((thandle = (HANDLE) 
	     _beginthreadex(NULL,0,reader,NULL,0,&tid)) == NULL) {
		print_last_error();
		exit(1);
	}
	return thandle;
}
#endif

#if defined(__WIN32__)

#  define TICK_MASK 0x7FFFFFFFUL

void init_timestamp(void)
{
}

time_t timestamp(time_t *res)
{
    static time_t extra = 0;
    static unsigned last_ticks = 0;
    unsigned this_ticks;
    time_t r;

    this_ticks = GetTickCount() & TICK_MASK;

    if (this_ticks < last_ticks) {
	extra += (time_t) ((TICK_MASK + 1) / 1000);
    }

    last_ticks = this_ticks;

    r = ((time_t) (this_ticks / 1000)) + extra;

    if (res != NULL)
	*res = r;
    return r;
}

#elif defined(OS_MONOTONIC_TIME_USING_GETHRTIME) || defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)

#if defined(OS_MONOTONIC_TIME_USING_CLOCK_GETTIME)
typedef long long SysHrTime;

SysHrTime sys_gethrtime(void);

SysHrTime sys_gethrtime(void)
{
    struct timespec ts;
    long long result;
    if (clock_gettime(MONOTONIC_CLOCK_ID,&ts) != 0) {
	print_error("Fatal, could not get clock_monotonic value, terminating! "
		    "errno = %d\n", errno);
	exit(1);
    }
    result = ((long long) ts.tv_sec) * 1000000000LL + 
	((long long) ts.tv_nsec);
    return (SysHrTime) result;
}
#else
typedef hrtime_t SysHrTime;
#define sys_gethrtime() gethrtime()
#endif

void init_timestamp(void)
{
}

time_t timestamp(time_t *res)
{
    SysHrTime ht = sys_gethrtime();
    time_t r = (time_t) (ht / 1000000000);
    if (res != NULL)
	*res = r;
    return r;
}

#elif defined(OS_MONOTONIC_TIME_USING_TIMES)

#  ifdef NO_SYSCONF
#    include <sys/param.h>
#    define TICKS_PER_SEC()	HZ
#  else
#    define TICKS_PER_SEC()	sysconf(_SC_CLK_TCK)
#  endif

#  define TICK_MASK 0x7FFFFFFFUL

static unsigned tps;

void init_timestamp(void)
{
    tps = TICKS_PER_SEC();
}

time_t timestamp(time_t *res)
{
    static time_t extra = 0;
    static clock_t last_ticks = 0;
    clock_t this_ticks;
    struct tms dummy;
    time_t r;

    this_ticks = (times(&dummy) & TICK_MASK);

    if (this_ticks < last_ticks) {
	extra += (time_t) ((TICK_MASK + 1) / tps);
    }

    last_ticks = this_ticks;

    r = ((time_t) (this_ticks / tps)) + extra;

    if (res != NULL)
	*res = r;
    return r;
}

#else

void init_timestamp(void)
{
}

time_t timestamp(time_t *res)
{
    return time(res);
}

#endif
