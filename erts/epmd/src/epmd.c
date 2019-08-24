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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "epmd.h"     /* Renamed from 'epmd_r4.h' */
#include "epmd_int.h"
#include "erl_printf.h"

#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
#include <time.h>
/* forward declarations */

static void usage(EpmdVars *);
static void run_daemon(EpmdVars*);
static char* get_addresses(void);
static int get_port_no(void);
static int check_relaxed(void);
#ifdef __WIN32__
static int has_console(void);
#endif

#ifdef DONT_USE_MAIN

static int epmd_main(int, char **, int);

/* VxWorks fill 10 stack words with zero when a function is called
   from the shell. So it is safe to have argv and argc as parameters
   even if they are not given in the call. */

#define MAX_DEBUG 10

int epmd_dbg(int level,int port) /* Utility to debug epmd... */
{
  char* argv[MAX_DEBUG+4];
  char  ibuff[100];
  int   argc = 0;
  
  argv[argc++] = "epmd";
  if(level > MAX_DEBUG)
    level = MAX_DEBUG;
  for(;level;--level)
    argv[argc++] = "-d";
  if(port)
    {
      argv[argc++] = "-port";
      erts_snprintf(ibuff, sizeof(ibuff), "%d",port);
      argv[argc++] = ibuff;
    }
  argv[argc] = NULL;

  return epmd(argc,argv);

}

static char *mystrdup(char *s)
{
    char *r = malloc(strlen(s)+1);
    strcpy(r,s);
    return r;
}

#ifdef VXWORKS
int start_epmd(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *a0, *a1, *a2, *a3, *a4, *a5, *a6, *a7, *a8, *a9;     
{
  char*  argarr[] = {a0,a1,a2,a3,a4,a5,a6,a7,a8,a9};
  int    i;
  char** argv = malloc(sizeof(char *)*10);
  int    argvsiz = 10;
  int    argc = 1;
  char*  tmp = malloc(100);
  int    tmpsiz = 100;
  char*  pplast;
  char*  token;
  
  argv[0] = mystrdup("epmd");
  argv[1] = NULL;
  
  for(i=0;i<10;++i)
    {
      if(argarr[i] == NULL || *argarr[i] == '\0')
	continue;
      if(strlen(argarr[i]) >= tmpsiz)
	tmp = realloc(tmp, tmpsiz = (strlen(argarr[i])+1));
      strcpy(tmp,argarr[i]);
      for(token = strtok_r(tmp," ",&pplast);
	  token != NULL;
	  token = strtok_r(NULL," ",&pplast))
	{
	  if(argc >= argvsiz - 1)
	    argv = realloc(argv,sizeof(char *) * (argvsiz += 10));
	  argv[argc++] = mystrdup(token);
	  argv[argc] = NULL;
	}
    }
  free(tmp);
  return taskSpawn("epmd",100,VX_FP_TASK,20000,epmd_main,
		   argc,(int) argv,1,
		   0,0,0,0,0,0,0);
}
#endif    /* WxWorks */


int epmd(int argc, char **argv)
{
  return epmd_main(argc,argv,0);
}

static int epmd_main(int argc, char** argv, int free_argv)
#else
int main(int argc, char** argv)
#endif /* DONT_USE_MAIN */
{
    EpmdVars g_empd_vars;
    EpmdVars *g = &g_empd_vars;
    int i;
#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;
    int err;

    wVersionRequested = MAKEWORD(1, 1);

    err = WSAStartup(wVersionRequested, &wsaData);
    if (err != 0)
        epmd_cleanup_exit(g,1);

    if (LOBYTE(wsaData.wVersion) != 1 || HIBYTE(wsaData.wVersion ) != 1) {
        WSACleanup();
    	epmd_cleanup_exit(g,1);
    }
#endif
#ifdef DONT_USE_MAIN
    if(free_argv)
	g->argv = argv;
    else 
	g->argv = NULL;
#else
    g->argv = NULL;
#endif

    g->addresses      = get_addresses();
    g->port           = get_port_no();
    g->debug          = 0;

    g->silent         = 0; 
    g->is_daemon      = 0;
    g->brutal_kill    = check_relaxed();
    g->packet_timeout = CLOSE_TIMEOUT; /* Default timeout */
    g->delay_accept   = 0;
    g->delay_write    = 0;
    g->progname       = argv[0];
    g->conn           = NULL;
    g->nodes.reg = g->nodes.unreg = g->nodes.unreg_tail = NULL;
    g->nodes.unreg_count = 0;
    g->active_conn    = 0;
#ifdef HAVE_SYSTEMD_DAEMON
    g->is_systemd     = 0;
#endif /* HAVE_SYSTEMD_DAEMON */

    for (i = 0; i < MAX_LISTEN_SOCKETS; i++)
	g->listenfd[i] = -1;

    argc--;
    argv++;
    while (argc > 0) {
	if ((strcmp(argv[0], "-debug")==0) ||
	    (strcmp(argv[0], "-d")==0)) {
	    g->debug += 1;
	    argv++; argc--;
	} else if (strcmp(argv[0], "-packet_timeout") == 0) {
	    if ((argc == 1) ||
		((g->packet_timeout = atoi(argv[1])) == 0))
		usage(g);
	    argv += 2; argc -= 2;
	} else if (strcmp(argv[0], "-delay_accept") == 0) {
	    if ((argc == 1) ||
		((g->delay_accept = atoi(argv[1])) == 0))
		usage(g);
	    argv += 2; argc -= 2;
	} else if (strcmp(argv[0], "-delay_write") == 0) {
	    if ((argc == 1) ||
		((g->delay_write = atoi(argv[1])) == 0))
		usage(g);
	    argv += 2; argc -= 2;
	} else if (strcmp(argv[0], "-daemon") == 0) {
	    g->is_daemon = 1;
	    argv++; argc--;
	} else if (strcmp(argv[0], "-relaxed_command_check") == 0) {
	    g->brutal_kill = 1;
	    argv++; argc--;
	} else if (strcmp(argv[0], "-kill") == 0) {
	    if (argc == 1)
		kill_epmd(g);
	    else
		usage(g);
	    epmd_cleanup_exit(g,0);
	} else if (strcmp(argv[0], "-address") == 0) {
	    if (argc == 1)
	      usage(g);
	    g->addresses = argv[1];
	    argv += 2; argc -= 2;
	} else if (strcmp(argv[0], "-port") == 0) {
	    if ((argc == 1) ||
		((g->port = atoi(argv[1])) == 0))
	      usage(g);
	    argv += 2; argc -= 2;
	} else if (strcmp(argv[0], "-names") == 0) {
	    if (argc == 1)
		epmd_call(g, EPMD_NAMES_REQ);
	    else
		usage(g);
	    epmd_cleanup_exit(g,0);
	} else if (strcmp(argv[0], "-started") == 0) {
	    g->silent = 1;
	    if (argc == 1)
		epmd_call(g, EPMD_NAMES_REQ);
	    else
		usage(g);
	    epmd_cleanup_exit(g,0);
	} else if (strcmp(argv[0], "-dump") == 0) {
	    if (argc == 1)
		epmd_call(g, EPMD_DUMP_REQ);
	    else
		usage(g);
	    epmd_cleanup_exit(g,0);
	} else if (strcmp(argv[0], "-stop") == 0) {
	    if (argc == 2)
		stop_cli(g, argv[1]);
	    else
		usage(g);
	    epmd_cleanup_exit(g,0);
#ifdef HAVE_SYSTEMD_DAEMON
	} else if (strcmp(argv[0], "-systemd") == 0) {
            g->is_systemd = 1;
            argv++; argc--;
#endif /* HAVE_SYSTEMD_DAEMON */
	} else
	    usage(g);
    }
    dbg_printf(g,1,"epmd running - daemon = %d",g->is_daemon);

#ifndef NO_SYSCONF
    if ((g->max_conn = sysconf(_SC_OPEN_MAX)) <= 0)
#endif
      g->max_conn = MAX_FILES;
  
    /*
     * max_conn must not be greater than FD_SETSIZE.
     * (at least QNX crashes)
     */
  
    if (g->max_conn > FD_SETSIZE) {
      g->max_conn = FD_SETSIZE;
    }

    if (g->is_daemon)  {
	run_daemon(g);
    } else {
	run(g);
    }
    return 0;
}

#ifndef NO_DAEMON
static void run_daemon(EpmdVars *g)
{
    register int child_pid, fd;
    
    dbg_tty_printf(g,2,"fork a daemon");

    /* fork to make sure first child is not a process group leader */
    if (( child_pid = fork()) < 0)
      {
#ifdef HAVE_SYSLOG_H
	syslog(LOG_ERR,"erlang mapper daemon cant fork %m");
#endif
	epmd_cleanup_exit(g,1);
      }
    else if (child_pid > 0)
      {
	dbg_tty_printf(g,2,"daemon child is %d",child_pid);
	epmd_cleanup_exit(g,0);  /*parent */
      }
    
    if (setsid() < 0)
      {
	dbg_perror(g,"epmd: Cant setsid()");
	epmd_cleanup_exit(g,1);
      }

    /* ???? */


    signal(SIGHUP, SIG_IGN);

    /* We don't want to be session leader so we fork again */

    if ((child_pid = fork()) < 0)
      {
#ifdef HAVE_SYSLOG_H
	syslog(LOG_ERR,"erlang mapper daemon cant fork 2'nd time %m");
#endif
	epmd_cleanup_exit(g,1);
      }
    else if (child_pid > 0)
      {
	dbg_tty_printf(g,2,"daemon 2'nd child is %d",child_pid);
	epmd_cleanup_exit(g,0);  /*parent */
      }

    /* move cwd to root to make sure we are not on a mounted filesystem  */
    if (chdir("/") < 0)
      {
	dbg_perror(g,"epmd: chdir() failed");
	epmd_cleanup_exit(g,1);
      }
    
    umask(0);

    for (fd = 0; fd < g->max_conn ; fd++) /* close all files ... */
        close(fd);
    /* Syslog on linux will try to write to whatever if we dont
       inform it that the log is closed. */
    closelog();

    /* These shouldn't be needed but for safety... */

    open("/dev/null", O_RDONLY); /* Order is important! */
    open("/dev/null", O_WRONLY);
    open("/dev/null", O_WRONLY);

    errno = 0;  /* if set by open */

    run(g);
}

#endif /* NO_DAEMON */    

#ifdef __WIN32__
static int has_console(void)
{
    HANDLE handle = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE,
			       NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

    if (handle == INVALID_HANDLE_VALUE) {
	return 0;
    } else {
        CloseHandle(handle);
	return 1;
    }
}

static void run_daemon(EpmdVars *g)
{
    if (has_console()) {
	if (spawnvp(_P_DETACH, __argv[0], __argv) == -1) {
	    fprintf(stderr, "Failed to spawn detached epmd\n");
	    exit(1);
	}
	exit(0);
    }

    close(0);
    close(1);
    close(2);

    /* These shouldn't be needed but for safety... */

    open("nul", O_RDONLY);
    open("nul", O_WRONLY);
    open("nul", O_WRONLY);

    run(g);
}
#endif

#if defined(VXWORKS)
static void run_daemon(EpmdVars *g)
{
    run(g);
}
#endif


/***************************************************************************
 *  Misc support routines
 *
 */

static void usage(EpmdVars *g)
{
    fprintf(stderr, "usage: epmd [-d|-debug] [DbgExtra...] [-address List]\n");
    fprintf(stderr, "            [-port No] [-daemon] [-relaxed_command_check]\n");
    fprintf(stderr, "       epmd [-d|-debug] [-port No] [-names|-kill|-stop name]\n\n");
    fprintf(stderr, "See the Erlang epmd manual page for info about the usage.\n\n");
    fprintf(stderr, "Regular options\n");
    fprintf(stderr, "    -address List\n");
    fprintf(stderr, "        Let epmd listen only on the comma-separated list of IP\n");
    fprintf(stderr, "        addresses (and on the loopback interface).\n");
    fprintf(stderr, "    -port No\n");
    fprintf(stderr, "        Let epmd listen to another port than default %d\n",
	    EPMD_PORT_NO);
    fprintf(stderr, "    -d\n");
    fprintf(stderr, "    -debug\n");
    fprintf(stderr, "        Enable debugging. This will give a log to\n");
    fprintf(stderr, "        the standard error stream. It will shorten\n");
    fprintf(stderr, "        the number of saved used node names to 5.\n\n");
    fprintf(stderr, "        If you give more than one debug flag you may\n");
    fprintf(stderr, "        get more debugging information.\n");
    fprintf(stderr, "    -daemon\n");
    fprintf(stderr, "        Start epmd detached (as a daemon)\n");
    fprintf(stderr, "    -relaxed_command_check\n");
    fprintf(stderr, "        Allow this instance of epmd to be killed with\n");
    fprintf(stderr, "        epmd -kill even if there "
	    "are registered nodes.\n");
    fprintf(stderr, "        Also allows forced unregister (epmd -stop).\n");
#ifdef HAVE_SYSTEMD_DAEMON
    fprintf(stderr, "    -systemd\n");
    fprintf(stderr, "        Wait for socket from systemd. The option makes sense\n");
    fprintf(stderr, "        when started from .socket unit.\n");
#endif /* HAVE_SYSTEMD_DAEMON */
    fprintf(stderr, "\nDbgExtra options\n");
    fprintf(stderr, "    -packet_timeout Seconds\n");
    fprintf(stderr, "        Set the number of seconds a connection can be\n");
    fprintf(stderr, "        inactive before epmd times out and closes the\n");
    fprintf(stderr, "        connection (default 60).\n\n");
    fprintf(stderr, "    -delay_accept Seconds\n");
    fprintf(stderr, "        To simulate a busy server you can insert a\n");
    fprintf(stderr, "        delay between epmd gets notified about that\n");
    fprintf(stderr, "        a new connection is requested and when the\n");
    fprintf(stderr, "        connections gets accepted.\n\n");
    fprintf(stderr, "    -delay_write Seconds\n");
    fprintf(stderr, "        Also a simulation of a busy server. Inserts\n");
    fprintf(stderr, "        a delay before a reply is sent.\n");
    fprintf(stderr, "\nInteractive options\n");
    fprintf(stderr, "    -names\n");
    fprintf(stderr, "        List names registered with the currently "
	    "running epmd\n");
    fprintf(stderr, "    -kill\n");
    fprintf(stderr, "        Kill the currently running epmd\n");
    fprintf(stderr, "        (only allowed if -names show empty database or\n");
    fprintf(stderr, "        -relaxed_command_check was given when epmd was started).\n");
    fprintf(stderr, "    -stop Name\n");
    fprintf(stderr, "        Forcibly unregisters a name with epmd\n");
    fprintf(stderr, "        (only allowed if -relaxed_command_check was given when \n");
    fprintf(stderr, "        epmd was started).\n");
    epmd_cleanup_exit(g,1);
}

/***************************************************************************
 *  Error reporting - dbg_printf() & dbg_tty_printf & dbg_perror()
 *
 *  The first form will print out on tty or syslog depending on
 *  if it runs as deamon or not. The second form will never print
 *  out on syslog.
 *
 *  The arguments are
 *
 *      g            Epmd variables
 *      from_level   From what debug level we print. 0 means always.
 *                   (This argument is missing from dbg_perror() )
 *      format       Format string
 *      args...      Arguments to print out according to the format
 *      
 */
#define DEBUG_BUFFER_SIZE 2048
static void dbg_gen_printf(int onsyslog,int perr,int from_level,
			   EpmdVars *g,const char *format, va_list args)
{
  time_t now;
  char *timestr;
  char buf[DEBUG_BUFFER_SIZE];

  if (g->is_daemon)
    {
#ifdef HAVE_SYSLOG_H
      if (onsyslog)
	{
	  erts_vsnprintf(buf, DEBUG_BUFFER_SIZE, format, args);
	  syslog(LOG_ERR,"epmd: %s",buf);
	}
#endif
    }
  else
    {
      int len;

      time(&now);
      timestr = (char *)ctime(&now);
      erts_snprintf(buf, DEBUG_BUFFER_SIZE, "epmd: %.*s: ",
		    (int) strlen(timestr)-1, timestr);
      len = strlen(buf);
      erts_vsnprintf(buf + len, DEBUG_BUFFER_SIZE - len, format, args);
      if (perr != 0)
	fprintf(stderr,"%s: %s\r\n",buf,strerror(perr));
      else
	fprintf(stderr,"%s\r\n",buf);
    }
}


void dbg_perror(EpmdVars *g,const char *format,...)
{
  va_list args;
  va_start(args, format);
  dbg_gen_printf(1,errno,0,g,format,args);
  va_end(args);
}


void dbg_tty_printf(EpmdVars *g,int from_level,const char *format,...)
{
  if (g->debug >= from_level) {
    va_list args;
    va_start(args, format);
    dbg_gen_printf(0,0,from_level,g,format,args);
    va_end(args);
  }
}

void dbg_printf(EpmdVars *g,int from_level,const char *format,...)
{
  if (g->debug >= from_level) {
    va_list args;
    va_start(args, format);
    dbg_gen_printf(1,0,from_level,g,format,args);
    va_end(args);
  }
}


/***************************************************************************
 *
 * This function is to clean up all filedescriptors and free up memory on 
 * VxWorks.
 * This function exits, there is nothing else to do when all here is run.
 *
 */

static void free_all_nodes(EpmdVars *g)
{
    Node *tmp;
    for(tmp=g->nodes.reg; tmp != NULL; tmp = g->nodes.reg){
	g->nodes.reg = tmp->next;
	free(tmp);
    }
    for(tmp=g->nodes.unreg; tmp != NULL; tmp = g->nodes.unreg){
	g->nodes.unreg = tmp->next;
	free(tmp);
    }
}
void epmd_cleanup_exit(EpmdVars *g, int exitval)
{
  int i;

  if(g->conn){
      for (i = 0; i < g->max_conn; i++)
	  if (g->conn[i].open == EPMD_TRUE)
	      epmd_conn_close(g,&g->conn[i]);
      free(g->conn);
  }
  for(i=0; i < MAX_LISTEN_SOCKETS; i++)
      if(g->listenfd[i] >= 0)
          close(g->listenfd[i]);
  free_all_nodes(g);
  if(g->argv){
      for(i=0; g->argv[i] != NULL; ++i)
	  free(g->argv[i]);
      free(g->argv);
  }
#ifdef HAVE_SYSTEMD_DAEMON
  if (g->is_systemd){
    sd_notifyf(0, "STATUS=Exited.\n"
               "ERRNO=%i", exitval);
  }
#endif /* HAVE_SYSTEMD_DAEMON */
  exit(exitval);
}

static char* get_addresses(void)
{
    return getenv("ERL_EPMD_ADDRESS");
}
static int get_port_no(void)
{
    char* port_str = getenv("ERL_EPMD_PORT");
    return (port_str != NULL) ? atoi(port_str) : EPMD_PORT_NO;
}
static int check_relaxed(void)
{
    char* port_str = getenv("ERL_EPMD_RELAXED_COMMAND_CHECK");
    return (port_str != NULL) ? 1 : 0;
}

