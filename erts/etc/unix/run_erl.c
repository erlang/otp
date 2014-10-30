/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
/* 
 * Module: run_erl.c
 * 
 * This module implements a reader/writer process that opens two specified 
 * FIFOs, one for reading and one for writing; reads from the read FIFO
 * and writes to stdout and the write FIFO.
 *
  ________                            _________ 
 |        |--<-- pipe.r (fifo1) --<--|         |
 | to_erl |                          | run_erl | (parent)
 |________|-->-- pipe.w (fifo2) -->--|_________|
                                          ^ master pty
                                          |
                                          | slave pty
                                      ____V____ 
                                     |         |
                                     |  "erl"  | (child)
                                     |_________|
*/


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_WORKING_POSIX_OPENPT
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600 
#endif
#endif

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/select.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <dirent.h>
#include <termios.h>
#include <time.h>
#ifdef HAVE_SYSLOG_H
#  include <syslog.h>
#endif
#ifdef HAVE_PTY_H
#  include <pty.h>
#endif
#ifdef HAVE_UTMP_H
#  include <utmp.h>
#endif
#ifdef HAVE_UTIL_H
#  include <util.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
#endif
#if defined(__sun) && defined(__SVR4)
#  include <stropts.h>
#endif

#include "run_erl_common.h"
#include "safe_string.h"    /* sn_printf, strn_cpy, strn_cat, etc */

#define MAX(x,y)  ((x) > (y) ? (x) : (y))

/* prototypes */
static void usage(char *);
static int open_pty_master(char **name, int *sfd);
static int open_pty_slave(char *name);
static void pass_on(pid_t);
static void exec_shell(char **);
static void catch_sigchild(int);
static void daemon_init(void);
static void init_outbuf(void);
static int outbuf_size(void);
static void clear_outbuf(void);
static char* outbuf_first(void);
static void outbuf_delete(int bytes);
static void outbuf_append(const char* bytes, int n);

#ifdef DEBUG
static void show_terminal_settings(struct termios *t);
#endif

/* static data */
static char fifo1[FILENAME_BUFSIZ], fifo2[FILENAME_BUFSIZ];
static char pipename[FILENAME_BUFSIZ];
static FILE *stdstatus = NULL;
static int run_daemon = 0;
static char *program_name;
static int mfd; /* master pty fd */

/*
 * Output buffer.
 *
 * outbuf_base <= outbuf_out <= outbuf_in <= outbuf_base+outbuf_total
 */
static char* outbuf_base;
static int outbuf_total;
static char* outbuf_out;
static char* outbuf_in;

#if defined(NO_SYSCONF) || !defined(_SC_OPEN_MAX) 
#    if defined(OPEN_MAX)
#        define HIGHEST_FILENO() OPEN_MAX
#    else
#        define HIGHEST_FILENO() 64 /* arbitrary value */
#    endif
#else 
#    define HIGHEST_FILENO() sysconf(_SC_OPEN_MAX)
#endif


#ifndef HAVE_SYSLOG_H
#    define OPEN_SYSLOG() ((void) 0)
#    define LOG_ERR NULL
#else
#    define OPEN_SYSLOG() openlog(simple_basename(program_name),   \
                                  LOG_PID|LOG_CONS|LOG_NOWAIT,LOG_USER)
#endif

int main(int argc, char **argv)
{
  int childpid;
  int sfd = -1;
  char *ptyslave=NULL;
  int i = 1;
  int off_argv;

  program_name = argv[0];

  if(argc<4) {
    usage(argv[0]);
    exit(1);
  }

  init_outbuf();

  if (!strcmp(argv[1],"-daemon")) {
      daemon_init();
      ++i;
  }

  off_argv = i;
  strn_cpy(pipename, sizeof(pipename), argv[i++]);

  erts_run_erl_log_init(run_daemon,argv[i]);

#ifdef DEBUG
  erts_run_erl_log_status("%s: pid is : %d\n", argv[0], getpid());
#endif

  /* Open read and write fifo */
  if (erts_run_erl_open_fifo(pipename,fifo1,fifo2))
    exit(1);

  /*
   * Open master pseudo-terminal
   */

  if ((mfd = open_pty_master(&ptyslave, &sfd)) < 0) {
    ERRNO_ERR0(LOG_ERR,"Could not open pty master");
    exit(1);
  }

  /* 
   * Now create a child process
   */

  if ((childpid = fork()) < 0) {
    ERRNO_ERR0(LOG_ERR,"Cannot fork");
    exit(1);
  }
  if (childpid == 0) {
    /* Child */
    sf_close(mfd);
    /* disassociate from control terminal */
#ifdef USE_SETPGRP_NOARGS       /* SysV */
    setpgrp();
#elif defined(USE_SETPGRP)       /* BSD */
    setpgrp(0,getpid());
#else                           /* POSIX */
    setsid();
#endif
    /* Open the slave pty */
    if (sfd < 0) {
	/* not allocated by open_pty_master */
	if ((sfd = open_pty_slave(ptyslave)) < 0) {
	    ERRNO_ERR1(LOG_ERR,"Could not open pty slave '%s'", ptyslave);
	    exit(1);
	}
	/* But sfd may be one of the stdio fd's now, and we should be unmodern and not use dup2... */
	/* easiest to dup it up... */
	while (sfd < 3) {
	    sfd = dup(sfd);
	}
    }
#if defined(HAVE_OPENPTY) && defined(TIOCSCTTY)
    else {
	/* sfd is from open_pty_master 
	 * openpty -> fork -> login_tty (forkpty)
	 * 
	 * It would be preferable to implement a portable 
	 * forkpty instead of open_pty_master / open_pty_slave
	 */
	/* login_tty(sfd);  <- FAIL */
	ioctl(sfd, TIOCSCTTY, (char *)NULL);
    }
#endif

#ifdef HAVE_SYSLOG_H
    /* Before fiddling with file descriptors we make sure syslog is turned off
       or "closed". In the single case where we might want it again, 
       we will open it again instead. Would not want syslog to
       go to some other fd... */
    if (run_daemon) {
	closelog();
    }
#endif

    /* Close stdio */
    sf_close(0);
    sf_close(1);
    sf_close(2);

    if (dup(sfd) != 0 || dup(sfd) != 1 || dup(sfd) != 2) {
      erts_run_erl_log_status("Cannot dup\n");
    }
    sf_close(sfd);
    exec_shell(argv+off_argv); /* exec_shell expects argv[2] to be */
                        /* the command name, so we have to */
                        /* adjust. */
  } else {
    /* Parent */
    /* Ignore the SIGPIPE signal, write() will return errno=EPIPE */
    struct sigaction sig_act;
    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = 0;
    sig_act.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sig_act, (struct sigaction *)NULL);

    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = SA_NOCLDSTOP;
    sig_act.sa_handler = catch_sigchild;
    sigaction(SIGCHLD, &sig_act, (struct sigaction *)NULL);

    /*
     * read and write: enter the workloop
     */

    pass_on(childpid);
  }
  return 0;
} /* main() */

/* pass_on()
 * Is the work loop of the logger. Selects on the pipe to the to_erl
 * program erlang. If input arrives from to_erl it is passed on to
 * erlang.
 */
static void pass_on(pid_t childpid)
{
    int len;
    fd_set readfds;
    fd_set writefds;
    fd_set* writefds_ptr;
    struct timeval timeout;
    time_t last_activity;
    char buf[BUFSIZ];
    int rfd, wfd=0;
    int maxfd;
    int ready;
    int got_some = 0; /* from to_erl */
    
    /* Open the to_erl pipe for reading.
     * We can't open the writing side because nobody is reading and 
     * we'd either hang or get an error.
     */
    if ((rfd = sf_open(fifo2, O_RDONLY|DONT_BLOCK_PLEASE, 0)) < 0) {
	ERRNO_ERR1(LOG_ERR,"Could not open FIFO '%s' for reading.", fifo2);
	exit(1);
    }
    
#ifdef DEBUG
    erts_run_erl_log_status("run_erl: %s opened for reading\n", fifo2);
#endif
    
    /* Open the log file */
    
    erts_run_erl_log_open();
    
    /* Enter the work loop */
    
    while (1) {
	int exit_status;
	maxfd = MAX(rfd, mfd);
	maxfd = MAX(wfd, maxfd);
	FD_ZERO(&readfds);
	FD_SET(rfd, &readfds);
	FD_SET(mfd, &readfds);
	FD_ZERO(&writefds);
	if (outbuf_size() == 0) {
	    writefds_ptr = NULL;
	} else {
	    FD_SET(wfd, &writefds);
	    writefds_ptr = &writefds;
	}
	time(&last_activity);
	/* don't assume old BSD bug */
	timeout.tv_sec  = erts_run_erl_log_alive_minutes()*60;
	timeout.tv_usec = 0;
	ready = select(maxfd + 1, &readfds, writefds_ptr, NULL, &timeout);
	if (ready < 0) {
	    if (errno == EINTR) {
		if (waitpid(childpid, &exit_status, WNOHANG) == childpid) {
		    /*
		     * The Erlang emulator has terminated. Give us some more
		     * time to write out any pending data before we terminate too.
		     */
		    alarm(5);
		}
		FD_ZERO(&readfds);
		FD_ZERO(&writefds);
	    } else {
		/* Some error occured */
		ERRNO_ERR0(LOG_ERR,"Error in select.");
		exit(1);
	    }
	} else {
	    time_t now;

	    if (waitpid(childpid, &exit_status, WNOHANG) == childpid) {
		alarm(5);
		FD_ZERO(&readfds);
		FD_ZERO(&writefds);
	    }

	    /* Check how long time we've been inactive */
	    time(&now);
	    erts_run_erl_log_activity(!ready,now,last_activity);
	}

	/*
	 * Write any pending output first.
	 */
	if (FD_ISSET(wfd, &writefds)) {
	    int written;
	    char* buf = outbuf_first();

	    len = outbuf_size();
	    written = sf_write(wfd, buf, len);
	    if (written < 0 && errno == EAGAIN) {
		/*
		 * Nothing was written - this is really strange because
		 * select() told us we could write. Ignore.
		 */
	    } else if (written < 0) {
		/*
		 * A write error. Assume that to_erl has terminated.
		 */
		clear_outbuf();
		sf_close(wfd);
		wfd = 0;
	    } else {
		/* Delete the written part (or all) from the buffer. */
		outbuf_delete(written);
	    }
	}
	
	/*
	 * Read master pty and write to FIFO.
	 */
	if (FD_ISSET(mfd, &readfds)) {
#ifdef DEBUG
	    erts_run_erl_log_status("Pty master read; ");
#endif
	    if ((len = sf_read(mfd, buf, BUFSIZ)) <= 0) {
		sf_close(rfd);
		if(wfd) sf_close(wfd);
		sf_close(mfd);
		unlink(fifo1);
		unlink(fifo2);
		if (len < 0) {
		    if(errno == EIO)
			ERROR0(LOG_ERR,"Erlang closed the connection.");
		    else
			ERRNO_ERR0(LOG_ERR,"Error in reading from terminal");
		    exit(1);
		}
		exit(0);
	    }

	    erts_run_erl_log_write(buf, len);

	    /*
	     * Save in the output queue.
	     */

	    if (wfd) {
		outbuf_append(buf, len);
	    }
	}	    

	/*
	 * Read from FIFO, write to master pty
	 */
	if (FD_ISSET(rfd, &readfds)) {
#ifdef DEBUG
	    erts_run_erl_log_status("FIFO read; ");
#endif
	    if ((len = sf_read(rfd, buf, BUFSIZ)) < 0) {
		sf_close(rfd);
		if(wfd) sf_close(wfd);
		sf_close(mfd);
		unlink(fifo1);
		unlink(fifo2);
		ERRNO_ERR0(LOG_ERR,"Error in reading from FIFO.");
		exit(1);
	    }

	    if(!len) {
		/* to_erl closed its end of the pipe */
		sf_close(rfd);
		rfd = sf_open(fifo2, O_RDONLY|DONT_BLOCK_PLEASE, 0);
		if (rfd < 0) {
		    ERRNO_ERR1(LOG_ERR,"Could not open FIFO '%s' for reading.", fifo2);
		    exit(1);
		}
		got_some = 0; /* reset for next session */
	    }
	    else { 
		if(!wfd) {
		    /* Try to open the write pipe to to_erl. Now that we got some data
		     * from to_erl, to_erl should already be reading this pipe - open
		     * should succeed. But in case of error, we just ignore it.
		     */
		    if ((wfd = sf_open(fifo1, O_WRONLY|DONT_BLOCK_PLEASE, 0)) < 0) {
			erts_run_erl_log_status("Client expected on FIFO %s, but can't open (len=%d)\n",
			       fifo1, len);
			sf_close(rfd);
			rfd = sf_open(fifo2, O_RDONLY|DONT_BLOCK_PLEASE, 0);
			if (rfd < 0) {
			    ERRNO_ERR1(LOG_ERR,"Could not open FIFO '%s' for reading.", fifo2);
			    exit(1);
			}
			wfd = 0;
		    } 
		    else {
#ifdef DEBUG
			erts_run_erl_log_status("run_erl: %s opened for writing\n", fifo1);
#endif
		    }
		}

		if (!got_some && wfd && buf[0] == '\014') {
		    char wbuf[30];
		    int wlen = sn_printf(wbuf,sizeof(wbuf),"[run_erl v%u-%u]\n",
					 RUN_ERL_HI_VER, RUN_ERL_LO_VER);
		    outbuf_append(wbuf,wlen);
		}
		got_some = 1;


		/* Write the message */
#ifdef DEBUG
		erts_run_erl_log_status("Pty master write; ");
#endif
		len = erts_run_erl_extract_ctrl_seq(buf, len, mfd);

		if(len==1 && buf[0] == '\003') {
		    kill(childpid,SIGINT);
		}
		else if (len>0 && erts_run_erl_write_all(mfd, buf, len) != len)
		  {
		    ERRNO_ERR0(LOG_ERR,"Error in writing to terminal.");
		    sf_close(rfd);
		    if(wfd) sf_close(wfd);
		    sf_close(mfd);
		    exit(1);
		}
	    }
#ifdef DEBUG
	    erts_run_erl_log_status("OK\n");
#endif
	}
    }
} /* pass_on() */

static void catch_sigchild(int sig)
{
}


/* open_pty_master()
 * Find a master device, open and return fd and slave device name.
 */

static int open_pty_master(char **ptyslave, int *sfdp)
{
  int mfd;

/* Use the posix_openpt if working, as this guarantees creation of the 
   slave device properly. */
#if defined(HAVE_WORKING_POSIX_OPENPT) || (defined(__sun) && defined(__SVR4))
#  ifdef HAVE_WORKING_POSIX_OPENPT
  if ((mfd = posix_openpt(O_RDWR)) >= 0) {
#  elif defined(__sun) && defined(__SVR4)
  mfd = sf_open("/dev/ptmx", O_RDWR, 0);

  if (mfd >= 0) {
#  endif
      if ((*ptyslave = ptsname(mfd)) != NULL &&
	  grantpt(mfd) == 0 && 
	  unlockpt(mfd) == 0) {

	  return mfd;
      }
      sf_close(mfd);
  }
  /* fallback to openpty if it exist */
#endif

#if defined(HAVE_OPENPTY)
#  ifdef PATH_MAX
#    define SLAVE_SIZE PATH_MAX
#  else
#    define SLAVE_SIZE 1024
#  endif
  {
      static char slave[SLAVE_SIZE];
#  undef SLAVE_SIZE
      if (openpty(&mfd, sfdp, slave, NULL, NULL) == 0) {
	  *ptyslave = slave;
	  return mfd;
      }
  }

#elif !defined(HAVE_WORKING_POSIX_OPENPT)
  /*
   * The traditional way to find ptys. We only try it if neither
   * posix_openpt or openpty() are available.
   */
  char *major, *minor;

  static char majorchars[] = "pqrstuvwxyzabcdePQRSTUVWXYZABCDE";
  static char minorchars[] = "0123456789abcdefghijklmnopqrstuv"
			     "wxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_+";

  /* In the old time the names where /dex/ptyXY where */
  /* X is in "pqrs" and Y in "0123456789abcdef" but FreeBSD */
  /* and some Linux version has extended this. */

  /* This code could probebly be improved alot. For example look at */
  /* http://www.xcf.berkeley.edu/~ali/K0D/UNIX/PTY/code/pty.c.html */
  /* http://www.xcf.berkeley.edu/~ali/K0D/UNIX/PTY/code/upty.h.html */

  {
    /* New style devpts or devfs /dev/pty/{m,s}{0,1....} */

    static char ptyname[] = "/dev/pty/mX";

    for (minor = minorchars; *minor; minor++) {
      ptyname[10] = *minor;

      if ((mfd = sf_open(ptyname, O_RDWR, 0)) >= 0) {
	ptyname[9] = 's';
	*ptyslave = ptyname;
	return mfd;
      }
    }
  }

  {
    /* Unix98 style /dev/ptym/ptyXY and /dev/pty/ttyXY */

    static char ptyname[] = "/dev/ptym/ptyXY";
    static char ttyname[] = "/dev/pty/ttyXY";

    for (major = majorchars; *major; major++) {
      ptyname[13] = *major;
      for (minor = minorchars; *minor; minor++) {
	ptyname[14] = *minor;
	if ((mfd = sf_open(ptyname, O_RDWR, 0)) >= 0) {
	  ttyname[12] = *major;
	  ttyname[13] = *minor;
	  *ptyslave = ttyname;
	  return mfd;
	}
      }
    }
  }

  {
    /* Old style /dev/ptyXY */

    static char ptyname[] = "/dev/ptyXY";

    for (major = majorchars; *major; major++) {
      ptyname[8] = *major;
      for (minor = minorchars; *minor; minor++) {
	ptyname[9] = *minor;
	if ((mfd = sf_open(ptyname, O_RDWR, 0)) >= 0) {
	  ptyname[5] = 't';
	  *ptyslave = ptyname;
	  return mfd;
	}
      }
    }
  }
#endif /* !HAVE_OPENPTY */
  return -1;
}

static int open_pty_slave(char *name)
{
  int sfd;
  struct termios tty_rmode;

  if ((sfd = sf_open(name, O_RDWR, 0)) < 0) {
    return -1;
  }

#if defined(__sun) && defined(__SVR4)
  /* Load the necessary STREAMS modules for Solaris */
  if ((ioctl(sfd, I_FIND, "ldterm")) < 0) {
    ERROR0(LOG_ERR, "Failed to find ldterm STREAMS module");
    return -1;
  }
  if (ioctl(sfd, I_PUSH, "ptem") < 0) {
    ERROR0(LOG_ERR, "Failed to push ptem STREAMS module");
    return -1;
  }
  if (ioctl(sfd, I_PUSH, "ldterm") < 0) {
    ERROR0(LOG_ERR, "Failed to push ldterm STREAMS module");
    return -1;
  }
  if (ioctl(sfd, I_PUSH, "ttcompat") < 0) {
    ERROR0(LOG_ERR, "Failed to push ttcompat STREAMS module");
    return -1;
  }
#endif

  if (getenv("RUN_ERL_DISABLE_FLOWCNTRL")) {
    if (tcgetattr(sfd, &tty_rmode) < 0) {
      fprintf(stderr, "Cannot get terminal's current mode\n");
      exit(-1);
    }

    tty_rmode.c_iflag &= ~IXOFF;
    if (tcsetattr(sfd, TCSANOW, &tty_rmode) < 0) {
      fprintf(stderr, "Cannot disable terminal's flow control on input\n");
      exit(-1);
    }

    tty_rmode.c_iflag &= ~IXON;
    if (tcsetattr(sfd, TCSANOW, &tty_rmode) < 0) {
      fprintf(stderr, "Cannot disable terminal's flow control on output\n");
      exit(-1);
    }
  }

#ifdef DEBUG
  if (tcgetattr(sfd, &tty_rmode) < 0) {
    fprintf(stderr, "Cannot get terminals current mode\n");
    exit(-1);
  }
  show_terminal_settings(&tty_rmode);
#endif

  return sfd;
}

/* exec_shell()
 * Executes the named command (in argv format) in a /bin/sh. IO redirection
 * should already have been taken care of, and this process should be the
 * child of a fork.
 */
static void exec_shell(char **argv)
{
  char *sh, **vp;
  int i;

  sh = "/bin/sh";
  if ((argv[0] = strrchr(sh, '/')) != NULL)
    argv[0]++;
  else
    argv[0] = sh;
  argv[1] = "-c";
  erts_run_erl_log_status("Args before exec of shell:\n");
  for (vp = argv, i = 0; *vp; vp++, i++)
    erts_run_erl_log_status("argv[%d] = %s\n", i, *vp);
  if (stdstatus) {
      fclose(stdstatus);
  }
  execv(sh, argv);
  if (run_daemon) {
      OPEN_SYSLOG();
  }
  ERRNO_ERR0(LOG_ERR,"Could not execv");
}


static void daemon_init(void) 
     /* As R Stevens wants it, to a certain extent anyway... */ 
{
    pid_t pid;
    int i, maxfd = HIGHEST_FILENO(); 

    if ((pid = fork()) != 0)
	exit(0);
#if defined(USE_SETPGRP_NOARGS)
    setpgrp();
#elif defined(USE_SETPGRP)
    setpgrp(0,getpid());
#else                           
    setsid(); /* Seems to be the case on all current platforms */
#endif
    signal(SIGHUP, SIG_IGN);
    if ((pid = fork()) != 0)
	exit(0);

    /* Should change working directory to "/" and change umask now, but that 
       would be backward incompatible */

    for (i = 0; i < maxfd; ++i ) {
	sf_close(i);
    }

    /* Necessary on some platforms */

    open("/dev/null", O_RDONLY); /* Order is important! */
    open("/dev/null", O_WRONLY);
    open("/dev/null", O_WRONLY);

    errno = 0;  /* if set by open */

    OPEN_SYSLOG();
    run_daemon = 1;
}

static void usage(char *pname)
{
  fprintf(stderr, "Usage: ");
  fprintf(stderr, RUN_ERL_USAGE, pname);
}

static void init_outbuf(void)
{
    outbuf_total = 1;
    outbuf_base = malloc(BUFSIZ);
    clear_outbuf();
}

static void clear_outbuf(void)
{
    outbuf_in = outbuf_out = outbuf_base;
}

static int outbuf_size(void)
{
    return outbuf_in - outbuf_out;
}

static char* outbuf_first(void)
{
    return outbuf_out;
}

static void outbuf_delete(int bytes)
{
    outbuf_out += bytes;
    if (outbuf_out >= outbuf_in) {
	outbuf_in = outbuf_out = outbuf_base;
    }
}

static void outbuf_append(const char* buf, int n)
{
    if (outbuf_base+outbuf_total < outbuf_in+n) {
	/*
	 * The new data does not fit at the end of the buffer.
	 * Slide down the data to the beginning of the buffer.
	 */
	if (outbuf_out > outbuf_base) {
	    int size = outbuf_in - outbuf_out;
	    char* p;

	    outbuf_in -= outbuf_out - outbuf_base;
	    p = outbuf_base;
	    while (size-- > 0) {
		*p++ = *outbuf_out++;
	    }
	    outbuf_out = outbuf_base;
	}

	/*
	 * Allocate a larger buffer if we still cannot fit the data.
	 */
	if (outbuf_base+outbuf_total < outbuf_in+n) {
	    int size = outbuf_in - outbuf_out;
	    outbuf_total = size+n;
	    outbuf_base = realloc(outbuf_base, outbuf_total);
	    outbuf_out = outbuf_base;
	    outbuf_in = outbuf_base + size;
	}
    }

    /*
     * Copy data to the end of the buffer.
     */
    memcpy(outbuf_in, buf, n);
    outbuf_in += n;
}

#ifdef DEBUG

#define S(x)  ((x) > 0 ? 1 : 0)

static void show_terminal_settings(struct termios *t)
{
  printf("c_iflag:\n");
  printf("Signal interrupt on break:   BRKINT  %d\n", S(t->c_iflag & BRKINT));
  printf("Map CR to NL on input:       ICRNL   %d\n", S(t->c_iflag & ICRNL));
  printf("Ignore break condition:      IGNBRK  %d\n", S(t->c_iflag & IGNBRK));
  printf("Ignore CR:                   IGNCR   %d\n", S(t->c_iflag & IGNCR));
  printf("Ignore char with par. err's: IGNPAR  %d\n", S(t->c_iflag & IGNPAR));
  printf("Map NL to CR on input:       INLCR   %d\n", S(t->c_iflag & INLCR));
  printf("Enable input parity check:   INPCK   %d\n", S(t->c_iflag & INPCK));
  printf("Strip character              ISTRIP  %d\n", S(t->c_iflag & ISTRIP));
  printf("Enable start/stop input ctrl IXOFF   %d\n", S(t->c_iflag & IXOFF));
  printf("ditto output ctrl            IXON    %d\n", S(t->c_iflag & IXON));
  printf("Mark parity errors           PARMRK  %d\n", S(t->c_iflag & PARMRK));
  printf("\n");
  printf("c_oflag:\n");
  printf("Perform output processing    OPOST   %d\n", S(t->c_oflag & OPOST));
  printf("\n");
  printf("c_cflag:\n");
  printf("Ignore modem status lines    CLOCAL  %d\n", S(t->c_cflag & CLOCAL));
  printf("\n");
  printf("c_local:\n");
  printf("Enable echo                  ECHO    %d\n", S(t->c_lflag & ECHO));
  printf("\n");
  printf("c_cc:\n");
  printf("c_cc[VEOF]                           %d\n", t->c_cc[VEOF]);
}

#endif /* DEBUG */


