/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
 *
 */

/* An exception from using eidef.h, use config.h directly */
#include "config.h"

#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <stdio.h>
#include <string.h>
#include <vxWorks.h>
#include <hostLib.h>
#include <selectLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <symLib.h>
#include <sysSymTbl.h>
#include <sysLib.h>
#include <tickLib.h>

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

#include <a_out.h>

/* #include "netdb.h" */
#else /* other unix */
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#endif

#include "ei.h"
#include "ei_resolve.h"
#include "erl_start.h"

/* FIXME is this a case a vfork can be used? */
#if !HAVE_WORKING_VFORK
# define vfork fork
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#ifndef RSH
#define RSH "/usr/bin/ssh"
#endif

#ifndef HAVE_SOCKLEN_T
typedef int SocklenType;
#else
typedef socklen_t SocklenType;
#endif

/* FIXME check errors from malloc */

static struct in_addr *get_addr(const char *hostname, struct in_addr *oaddr);

static int wait_for_erlang(int sockd, int magic, struct timeval *timeout);
#if defined(VXWORKS) || defined(__WIN32__)
static int unique_id(void);
static unsigned long spawn_erlang_epmd(ei_cnode *ec,
				       char *alive,
				       Erl_IpAddr adr,
				       int flags,
				       char *erl_or_epmd,
				       char *args[],
				       int port,
				       int is_erlang);
#else
static int exec_erlang(ei_cnode *ec, char *alive, Erl_IpAddr adr, int flags,
		       char *erl, char *args[],int port);
#endif
/* Start an Erlang node. return value 0 indicates that node was
 * started successfully, negative values indicate error. 
 * 
 * node -  the name of the remote node to start (alivename@hostname).
 * flags - turn on or off certain options. See erl_start.h for a list.
 * erl -  is the name of the erl script to call. If NULL, the default
 * name "erl" will be used.
 * args - a NULL-terminated list of strings containing
 * additional arguments to be sent to the remote Erlang node. These
 * strings are simply appended to the end of the command line, so any
 * quoting of special characters, etc must be done by the caller.
 * There may be some conflicts between some of these arguments and the
 * default arguments hard-coded into this function, so be careful. 
 */
int erl_start_sys(ei_cnode *ec, char *alive, Erl_IpAddr adr, int flags,
		  char *erl, char *args[])
{
  struct timeval timeout;
  struct sockaddr_in addr;
  SocklenType namelen;
  int port;
  int sockd = 0;
  int one = 1;
#if defined(VXWORKS) || defined(__WIN32__)
  unsigned long pid = 0;
#else
  int pid = 0;
#endif
  int r = 0;

  if (((sockd = socket(AF_INET, SOCK_STREAM, 0)) < 0) ||
      (setsockopt(sockd, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(one)) < 0)) {
    r = ERL_SYS_ERROR;
    goto done;
  }

  memset(&addr,0,sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = 0;

  if (bind(sockd,(struct sockaddr *)&addr,sizeof(addr))<0) {
      return ERL_SYS_ERROR;
  }
  namelen = sizeof(addr);
  if (getsockname(sockd,(struct sockaddr *)&addr,&namelen)<0) {
      return ERL_SYS_ERROR;
  }
  port = ntohs(addr.sin_port);

  listen(sockd,5);

#if defined(VXWORKS) || defined(__WIN32__)
  if((pid = spawn_erlang_epmd(ec,alive,adr,flags,erl,args,port,1))
      == 0)
     return ERL_SYS_ERROR;
  timeout.tv_usec = 0;
  timeout.tv_sec = 10; /* ignoring ERL_START_TIME */
  if((r = wait_for_erlang(sockd,unique_id(),&timeout))
     == ERL_TIMEOUT) {
#if defined(VXWORKS)
      taskDelete((int) pid);
      if(taskIdVerify((int) pid) != ERROR)
	  taskDeleteForce((int) pid);
#else /* Windows */
      /* Well, this is not a nice way to do it, and it does not 
	 always kill the emulator, but the alternatives are few.*/
      TerminateProcess((HANDLE) pid,1);
#endif /* defined(VXWORKS) */
  }
#else /* Unix */
  switch ((pid = fork())) {
  case -1:
    r = ERL_SYS_ERROR;
    break;

  case 0:
    /* child - start the erlang node */
    exec_erlang(ec, alive, adr, flags, erl, args, port);

    /* error if reached - parent reports back to caller after timeout
       so we just exit here */
    exit(1);
    break;

  default:

    /* parent - waits for response from Erlang node */
    /* child pid used here as magic number */
    timeout.tv_usec = 0;
    timeout.tv_sec = 10; /* ignoring ERL_START_TIME */
    if ((r = wait_for_erlang(sockd,pid,&timeout)) == ERL_TIMEOUT) {
      /* kill child if no response */
      kill(pid,SIGINT);
      sleep(1);
      if (waitpid(pid,NULL,WNOHANG) != pid) {
	/* no luck - try harder */
	kill(pid,SIGKILL);
	sleep(1);
	waitpid(pid,NULL,WNOHANG);
      }
    }

  }
#endif /* defined(VXWORKS) || defined(__WIN32__) */

done:
#if defined(__WIN32__)
  if (sockd) closesocket(sockd);
#else
  if (sockd) close(sockd);
#endif
  return r;
} /* erl_start_sys() */

#if defined(VXWORKS) || defined(__WIN32__)
#if defined(VXWORKS)
#define DEF_ERL_COMMAND ""
#define DEF_EPMD_COMMAND ""
#define ERLANG_SYM "start_erl"
#define EPMD_SYM "start_epmd"
#define ERL_REPLY_FMT   "-s erl_reply reply %s %d %d"
#else
#define DEF_ERL_COMMAND "erl"
#define DEF_EPMD_COMMAND "epmd"
#define ERL_REPLY_FMT   "-s erl_reply reply \"%s\" \"%d\" \"%d\""
#endif
#define ERL_NAME_FMT    "-noinput -name %s"
#define ERL_SNAME_FMT   "-noinput -sname %s"

#define IP_ADDR_CHARS 15
#define FORMATTED_INT_LEN 10

static int unique_id(void){
#if defined(VXWORKS)
    return taskIdSelf();
#else
    return (int) GetCurrentThreadId();
#endif
}

static int enquote_args(char **oargs, char ***qargs){
    char **args;
    int len;
    int i;
    int qwhole;
    int extra;
    char *ptr;
    char *ptr2;

    if(oargs == NULL){
	*qargs = malloc(sizeof(char *));
	**qargs = NULL;
	return 0;
    };

    for(len=0;oargs[len] != NULL; ++len)
	;
    args = malloc(sizeof(char *) * (len + 1));

    for(i = 0; i < len; ++i){
	qwhole = strchr(oargs[i],' ') != NULL;
	extra = qwhole * 2;
	for(ptr = oargs[i]; *ptr != '\0'; ++ptr)
	    extra += (*ptr == '"');
	args[i] = malloc(strlen(oargs[i]) +
			     extra +
			     1);
	ptr2 = args[i];
	if(qwhole)
	    *(ptr2++) = '"';
	for(ptr = oargs[i]; *ptr != '\0'; ++ptr){
	    if(*ptr == '"')
		*(ptr2++) = '\\';
	    *(ptr2++) = *ptr;
	}
	if(qwhole)
	    *(ptr2++) = '"';
	*ptr2 = '\0';
    }
    args[len] = NULL;
    *qargs = args;
    return len;
}

static void free_args(char **args){
    char **ptr = args;
    while(*ptr != NULL)
	free(*(ptr++));
    free(args);
}

#if defined(VXWORKS)
static  FUNCPTR lookup_function(char *symname){
    char *value;
    SYM_TYPE type;
    if(symFindByName(sysSymTbl,
		     symname,
		     &value,
		     &type) == ERROR /*|| type != N_TEXT*/)
	return NULL;
    return (FUNCPTR) value;
}
#endif /* defined(VXWORKS) */

/* In NT and VxWorks, we cannot fork(), Erlang and Epmd gets 
   spawned by this function instead. */

static unsigned long spawn_erlang_epmd(ei_cnode *ec,
				       char *alive,
				       Erl_IpAddr adr,
				       int flags,
				       char *erl_or_epmd,
				       char *args[],
				       int port,
				       int is_erlang)
{
#if defined(VXWORKS)
    FUNCPTR erlfunc;
#else /* Windows */
    STARTUPINFO sinfo;
    SECURITY_ATTRIBUTES sa;
    PROCESS_INFORMATION pinfo;
#endif
    char *cmdbuf;
    int cmdlen;
    char *ptr;
    int i;
    int num_args;
    char *name_format;
    struct in_addr myaddr;
    struct in_addr *hisaddr = (struct in_addr *)adr;
    char iaddrbuf[IP_ADDR_CHARS + 1];
    int ret;

    if(is_erlang){
	get_addr(ei_thishostname(ec), &myaddr);
#if defined(VXWORKS)
        inet_ntoa_b(myaddr, iaddrbuf);
#else /* Windows */
	if((ptr = inet_ntoa(myaddr)) == NULL)
	    return 0;
	else
	    strcpy(iaddrbuf,ptr);
#endif
    }
    if ((flags & ERL_START_REMOTE) ||
	(is_erlang && (hisaddr->s_addr != myaddr.s_addr))) {
	return 0;
    } else {
	num_args = enquote_args(args, &args);
	for(cmdlen = i = 0; args[i] != NULL; ++i)
	    cmdlen += strlen(args[i]) + 1;
#if !defined(VXWORKS)
	/* On VxWorks, we dont actually run a command,
	   we call start_erl() */
	if(!erl_or_epmd)
#endif
	    erl_or_epmd = (is_erlang) ? DEF_ERL_COMMAND :
	    DEF_EPMD_COMMAND;
	if(is_erlang){
	    name_format = (flags & ERL_START_LONG) ? ERL_NAME_FMT :
		ERL_SNAME_FMT;
	    cmdlen +=
		strlen(erl_or_epmd) + (*erl_or_epmd != '\0') +
		strlen(name_format) + 1 + strlen(alive) +
		strlen(ERL_REPLY_FMT) + 1 + strlen(iaddrbuf) + 2 * FORMATTED_INT_LEN + 1;
	    ptr = cmdbuf = malloc(cmdlen);
	    if(*erl_or_epmd != '\0')
		ptr += sprintf(ptr,"%s ",erl_or_epmd);
	    ptr += sprintf(ptr, name_format,
			   alive);
	    ptr += sprintf(ptr, " " ERL_REPLY_FMT,
		       iaddrbuf, port, unique_id());
	} else { /* epmd */
	    cmdlen += strlen(erl_or_epmd) + (*erl_or_epmd != '\0') + 1;
	    ptr = cmdbuf = malloc(cmdlen);
	    if(*erl_or_epmd != '\0')
		ptr += sprintf(ptr,"%s ",erl_or_epmd);
	    else
		*(ptr++) = '\0';
	}
	for(i= 0; args[i] != NULL; ++i){
	    *(ptr++) = ' ';
	    strcpy(ptr,args[i]);
	    ptr += strlen(args[i]);
	}
	free_args(args);
	if (flags & ERL_START_VERBOSE) {
	    fprintf(stderr,"erl_call: commands are %s\n",cmdbuf);
	}
	/* OK, one single command line... */
#if defined(VXWORKS)
	erlfunc = lookup_function((is_erlang) ? ERLANG_SYM :
				  EPMD_SYM);
	if(erlfunc == NULL){
	    if (flags & ERL_START_VERBOSE) {
		fprintf(stderr,"erl_call: failed to find symbol %s\n",
			(is_erlang) ? ERLANG_SYM : EPMD_SYM);
	    }
	    ret = 0;
	} else {
	/* Just call it, it spawns itself... */
	    ret = (unsigned long) 
		(*erlfunc)((int) cmdbuf,0,0,0,0,0,0,0,0,0);
	    if(ret == (unsigned long) ERROR)
		ret = 0;
	}
#else /* Windows */
	/* Hmmm, hidden or unhidden window??? */
	memset(&sinfo,0,sizeof(sinfo));
	sinfo.cb = sizeof(STARTUPINFO); 
	sinfo.dwFlags = STARTF_USESHOWWINDOW /*| 
	    STARTF_USESTDHANDLES*/;
	sinfo.wShowWindow = SW_HIDE; /* Hidden! */
	sinfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	sinfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	sinfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = /*TRUE*/ FALSE;
	if(!CreateProcess(
			  NULL,
			  cmdbuf,
			  &sa,
			  NULL,
			  /*TRUE*/ FALSE,
			  0 | CREATE_NEW_CONSOLE,
			  NULL,
			  NULL,
			  &sinfo,
			  &pinfo))
	    ret = 0;
	else
	    ret = (unsigned long) pinfo.hProcess;
#endif
	free(cmdbuf);
	return ret;
    }
    /* NOTREACHED */
}
#else /* Unix */

/* call this from the child process to start an erlang system. This
 * function just builds the erlang command line and then calls it.
 *
 * node - the nodename for the new node
 * flags - various options that can be set (see erl_start.h)
 * erl - name of the erlang executable, or NULL for default ("erl")
 * args - additional arguments to pass to erlang executable
 * port - the port number where we wait for acknowledgment from the enode
 *
 * we have a potential problem if args conflicts with any of the 
 * arguments we use here.
 */
static int exec_erlang(ei_cnode *ec,
		       char *alive,
		       Erl_IpAddr adr,
		       int flags,
		       char *erl,
		       char *args[],
		       int port)
{
#if !defined(__WIN32__) && !defined(VXWORKS) 
  int fd,len,l,i;
  char **s;
  char *argv[4];
  char argbuf[BUFSIZ];
  struct in_addr myaddr;
  struct in_addr *hisaddr = (struct in_addr *)adr;

  if (!get_addr(ei_thishostname(ec), &myaddr)) {
      fprintf(stderr,"erl_call: failed to find hostname\r\n");
      return ERL_SYS_ERROR;
  }

  /* on this host? */
  /* compare ip addresses, unless forced by flag setting to use rsh */
  if ((flags & ERL_START_REMOTE) || (hisaddr->s_addr != myaddr.s_addr)) {
    argv[0] = RSH;
    len = strlen(inet_ntoa(*hisaddr));
    argv[1] = malloc(len+1);
    strcpy(argv[1],inet_ntoa(*hisaddr));
  }
  else {
  /* Yes - use sh to start local Erlang */
    argv[0] = "sh";
    argv[1] = "-c";
  }
  argv[2] = argbuf;
  argv[3] = NULL;

  len = 0;
  *argbuf=(char)0;

  sprintf(argbuf,"exec %s ", (erl? erl: "erl"));
  len = strlen(argbuf);

  /* *must* be noinput or node (seems to) hang... */
  /* long or short names? */
  sprintf(&argbuf[len], "-noinput %s %s ",
	  ((flags & ERL_START_LONG) ? "-name" : "-sname"),
	  alive);
  len = strlen(argbuf);

  /* now make the new node report back when it's ready */
  /* add: myip, myport and replymsg */
  sprintf(&argbuf[len],
	  "-s erl_reply reply %s %d %d ",
	  inet_ntoa(myaddr),port,(int)getpid());
#ifdef DEBUG
  fprintf(stderr,"erl_call: debug %s\n",&argbuf[len]);
#endif
  len = strlen(argbuf);

  /* additional arguments to be passed to the other system */
  /* make sure that they will fit first */
  for (l=0, s = args; s && *s; s++) l+= strlen(*s) + 1;

  if (len + l + 1 > BUFSIZ) return ERL_BADARG;
  else {
    for (s = args; s && *s; s++) {
      strcat(argbuf," ");
      strcat(argbuf,*s);
    }
    len += l + 1;
  }

  if (flags & ERL_START_VERBOSE) {
    fprintf(stderr,"erl_call: %s %s %s\n",argv[0],argv[1],argv[2]);
  }

  /* close all descriptors in child */
  for (i=0; i<64; i++) close(i);

  /* debug output to file? */
  if (flags & ERL_START_DEBUG) {
    char debugfile[MAXPATHLEN+1];
    char *home=getenv("HOME");
    sprintf(debugfile,"%s/%s.%s",home,ERL_START_LOGFILE,alive);
    if ((fd=open(debugfile, O_WRONLY | O_CREAT | O_APPEND, 0644)) >= 0) {
      time_t t = time(NULL);
      dup2(fd,1);
      dup2(fd,2);
      fprintf(stderr,"\n\n===== Log started ======\n%s \n",ctime(&t));
      fprintf(stderr,"erl_call: %s %s %s\n",argv[0],argv[1],argv[2]);
    }
  }

  /* start the system */
  execvp(argv[0], argv);

  if (flags & ERL_START_DEBUG) {
    fprintf(stderr,"erl_call: exec failed: (%d) %s %s %s\n",
	    errno,argv[0],argv[1],argv[2]);
  }

#endif
  /* (hopefully) NOT REACHED */
  return ERL_SYS_ERROR;
} /* exec_erlang() */

#endif /* defined(VXWORKS) || defined(WINDOWS) */

#if defined(__WIN32__)
static void gettimeofday(struct timeval *now,void *dummy){
    SYSTEMTIME systime;
	FILETIME ft;
    DWORD x;
    GetSystemTime(&systime);
    SystemTimeToFileTime(&systime,&ft);
    x = ft.dwLowDateTime / 10;
    now->tv_sec = x / 1000000;
    now->tv_usec = x % 1000000;
}

#elif defined(VXWORKS)
static void gettimeofday(struct timeval *now, void *dummy){
    int rate = sysClkRateGet(); /* Ticks per second */
    unsigned long ctick = tickGet();
    now->tv_sec = ctick / rate; /* secs since reboot */
    now->tv_usec = ((ctick - (now->tv_sec * rate))*1000000)/rate;
}
#endif


/* wait for the remote system to reply */
/*
 * sockd - an open socket where we expect a connection from the e-node
 * magic - sign on message the e-node must provide for verification
 * timeout - how long to wait before returning failure
 *
 * OBS: the socket is blocking, and there is a potential deadlock if we
 * get an accept but the peer sends no data (and does not close).
 * in normal cases the timeout will work ok however, i.e. either we
 * never get any connection, or we get connection then close().
 */
static int wait_for_erlang(int sockd, int magic, struct timeval *timeout)
{
  struct timeval to;
  struct timeval stop_time;
  struct timeval now;
  fd_set rdset;
  int fd;
  int n,i;
  char buf[16];
  struct sockaddr_in peer;
  SocklenType len = (SocklenType) sizeof(peer);

  /* determine when we should exit this function */
  gettimeofday(&now,NULL);
  stop_time.tv_sec = now.tv_sec + timeout->tv_sec;
  stop_time.tv_usec = now.tv_usec + timeout->tv_usec;
  while (stop_time.tv_usec > 1000000) {
    stop_time.tv_sec++;
    stop_time.tv_usec -= 1000000;
  }

#ifdef DEBUG
  fprintf(stderr,"erl_call: debug time is %ld.%06ld, "
	  "will timeout at %ld.%06ld\n",
	  now.tv_sec,now.tv_usec,stop_time.tv_sec,stop_time.tv_usec);
#endif

  while (1) {
    FD_ZERO(&rdset);
    FD_SET(sockd,&rdset);

    /* adjust the timeout to (stoptime - now) */
    gettimeofday(&now,NULL);
    to.tv_sec = stop_time.tv_sec - now.tv_sec;
    to.tv_usec = stop_time.tv_usec - now.tv_usec;
    while ((to.tv_usec < 0) && (to.tv_sec > 0)) {
      to.tv_usec += 1000000;
      to.tv_sec--;
    }
    if (to.tv_sec < 0) return ERL_TIMEOUT;

#ifdef DEBUG
    fprintf(stderr,"erl_call: debug remaining to timeout: %ld.%06ld\n",
	    to.tv_sec,to.tv_usec);
#endif
    switch ((i = select(sockd+1,&rdset,NULL,NULL,&to))) {
    case -1:
      return ERL_SYS_ERROR;
      break;

    case 0: /* timeout */
#ifdef DEBUG
      gettimeofday(&now,NULL);
      fprintf(stderr,"erl_call: debug timed out at %ld.%06ld\n",
	      now.tv_sec,now.tv_usec);
#endif
      return ERL_TIMEOUT;
      break;

    default: /* ready descriptors */
#ifdef DEBUG
      gettimeofday(&now,NULL);
      fprintf(stderr,"erl_call: debug got select at %ld.%06ld\n",
	      now.tv_sec,now.tv_usec);
#endif
      if (FD_ISSET(sockd,&rdset)) {
	if ((fd = accept(sockd,(struct sockaddr *)&peer,&len)) < 0)
	  return ERL_SYS_ERROR;

	/* now get sign-on message and terminate it */
#if defined(__WIN32__)
	if ((n=recv(fd,buf,16,0)) >= 0) buf[n]=0x0;
	closesocket(fd);
#else
	if ((n=read(fd,buf,16)) >= 0) buf[n]=0x0;
	close(fd);
#endif
#ifdef DEBUG
	fprintf(stderr,"erl_call: debug got %d, expected %d\n",
		atoi(buf),magic);
#endif
	if (atoi(buf) == magic) return 0; /* success */
      } /* if FD_SET */
    } /* switch */
  } /* while */

  /* unreached? */
  return ERL_SYS_ERROR;
} /* wait_for_erlang() */


static struct in_addr *get_addr(const char *hostname, struct in_addr *oaddr)
{
  struct hostent *hp;

#if !defined (__WIN32__)
  char buf[1024];
  struct hostent host;
  int herror;

  hp = ei_gethostbyname_r(hostname,&host,buf,1024,&herror);
#else
  hp = ei_gethostbyname(hostname);
#endif

  if (hp) {
    memmove(oaddr,hp->h_addr_list[0],sizeof(*oaddr));
    return oaddr;
  }
  return NULL;
}
