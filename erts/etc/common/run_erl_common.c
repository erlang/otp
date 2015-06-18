/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014. All Rights Reserved.
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

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#ifdef __ANDROID__
#  include <termios.h>
#endif

#ifdef HAVE_SYSLOG_H
#  include <syslog.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
#endif

#ifdef __OSE__
#  include "ramlog.h"
#endif

#include "run_erl_common.h"
#include "safe_string.h"

#define DEFAULT_LOG_GENERATIONS 5
#define LOG_MAX_GENERATIONS     1000      /* No more than 1000 log files */
#define LOG_MIN_GENERATIONS     2         /* At least two to switch between */
#define DEFAULT_LOG_MAXSIZE     100000
#define LOG_MIN_MAXSIZE         1000      /* Smallast value for changing log file */
#define LOG_STUBNAME            "erlang.log."
#define LOG_PERM                0664
#define DEFAULT_LOG_ACTIVITY_MINUTES    5
#define DEFAULT_LOG_ALIVE_MINUTES       15
#define DEFAULT_LOG_ALIVE_FORMAT        "%a %b %e %T %Z %Y"
#define ALIVE_BUFFSIZ                   1024

#define STATUSFILENAME  "/run_erl.log"

#define PIPE_STUBNAME   "erlang.pipe"
#define PIPE_STUBLEN    strlen(PIPE_STUBNAME)
#define PERM            (S_IWUSR | S_IRUSR | S_IWOTH | S_IROTH | S_IWGRP | S_IRGRP)

/* OSE has defined O_SYNC but it is not recognized by open */
#if !defined(O_SYNC) || defined(__OSE__)
#undef O_SYNC
#define O_SYNC 0
#define USE_FSYNC 1
#endif

/* Global variable definitions
 * We need this complex way of handling global variables because of how
 * OSE works here. We want to make it possible to run the shell command
 * run_erl multiple times with different global variables without them
 * effecting eachother.
 */

#define STATUSFILE           (RE_DATA->statusfile)
#define LOG_DIR              (RE_DATA->log_dir)
#define STDSTATUS            (RE_DATA->stdstatus)
#define LOG_GENERATIONS      (RE_DATA->log_generations)
#define LOG_MAXSIZE          (RE_DATA->log_maxsize)
#define LOG_ACTIVITY_MINUTES (RE_DATA->log_activity_minutes)
#define LOG_ALIVE_IN_GMT     (RE_DATA->log_alive_in_gmt)
#define LOG_ALIVE_FORMAT     (RE_DATA->log_alive_format)
#define RUN_DAEMON           (RE_DATA->run_daemon)
#define LOG_ALIVE_MINUTES    (RE_DATA->log_alive_minutes)
#define LOG_NUM              (RE_DATA->log_num)
#define LFD                  (RE_DATA->lfd)
#define PROTOCOL_VER         (RE_DATA->protocol_ver)

struct run_erl_ {
  /* constant config data */
  char statusfile[FILENAME_BUFSIZ];
  char log_dir[FILENAME_BUFSIZ];
  FILE *stdstatus;
  int log_generations;
  int log_maxsize;
  int log_activity_minutes;
  int log_alive_in_gmt;
  char log_alive_format[ALIVE_BUFFSIZ+1];
  int run_daemon;
  int log_alive_minutes;
  /* Current log number and log fd */
  int log_num;
  int lfd;
  unsigned protocol_ver;
};

typedef struct run_erl_ run_erl;

#ifdef __OSE__
static OSPPDKEY run_erl_pp_key;
#define RE_DATA (*(run_erl**)ose_get_ppdata(run_erl_pp_key))
#else
static run_erl re;
#define RE_DATA (&re)
#endif

/* prototypes */

static int next_log(int log_num);
static int prev_log(int log_num);
static int find_next_log_num(void);
static int open_log(int log_num, int flags);

/*
 * getenv_int:
 */
static char *getenv_int(const char *name) {
#ifdef __OSE__
   return get_env(get_bid(current_process()),name);
#else
   return getenv(name);
#endif
}

/*
 * next_log:
 * Returns the index number that follows the given index number.
 * (Wrapping after log_generations)
 */
static int next_log(int log_num) {
  return log_num>=LOG_GENERATIONS?1:log_num+1;
}

/*
 * prev_log:
 * Returns the index number that precedes the given index number.
 * (Wrapping after log_generations)
 */
static int prev_log(int log_num) {
  return log_num<=1?LOG_GENERATIONS:log_num-1;
}

/*
 * find_next_log_num()
 * Searches through the log directory to check which logs that already
 * exist. It finds the "hole" in the sequence, and returns the index
 * number for the last log in the log sequence. If there is no hole, index
 * 1 is returned.
 */
static int find_next_log_num(void) {
  int i, next_gen, log_gen;
  DIR *dirp;
  struct dirent *direntp;
  int log_exists[LOG_MAX_GENERATIONS+1];
  int stub_len = strlen(LOG_STUBNAME);

  /* Initialize exiting log table */

  for(i=LOG_GENERATIONS; i>=0; i--)
    log_exists[i] = 0;
  dirp = opendir(LOG_DIR);
  if(!dirp) {
    ERRNO_ERR1(LOG_ERR,"Can't access log directory '%s'", LOG_DIR);
    exit(1);
  }

  /* Check the directory for existing logs */

  while((direntp=readdir(dirp)) != NULL) {
    if(strncmp(direntp->d_name,LOG_STUBNAME,stub_len)==0) {
      int num = atoi(direntp->d_name+stub_len);
      if(num < 1 || num > LOG_GENERATIONS)
	continue;
      log_exists[num] = 1;
    }
  }
  closedir(dirp);

  /* Find out the next available log file number */

  next_gen = 0;
  for(i=LOG_GENERATIONS; i>=0; i--) {
    if(log_exists[i])
      if(next_gen)
	break;
      else
	;
    else
      next_gen = i;
  }

  /* Find out the current log file number */

  if(next_gen)
    log_gen = prev_log(next_gen);
  else
    log_gen = 1;

  return log_gen;
} /* find_next_log_num() */

static int open_log(int log_num, int flags)
{
  char buf[FILENAME_MAX];
  time_t now;
  struct tm *tmptr;
  char log_buffer[ALIVE_BUFFSIZ+1];

  /* Remove the next log (to keep a "hole" in the log sequence) */
  sn_printf(buf, sizeof(buf), "%s/%s%d",
	    LOG_DIR, LOG_STUBNAME, next_log(log_num));
  unlink(buf);

  /* Create or continue on the current log file */
  sn_printf(buf, sizeof(buf), "%s/%s%d", LOG_DIR, LOG_STUBNAME, log_num);

  LFD = sf_open(buf, flags, LOG_PERM);

  if(LFD <0){
      ERRNO_ERR1(LOG_ERR,"Can't open log file '%s'.", buf);
    exit(1);
  }

  /* Write a LOGGING STARTED and time stamp into the log file */
  time(&now);
  if (LOG_ALIVE_IN_GMT) {
      tmptr = gmtime(&now);
  } else {
      tmptr = localtime(&now);
  }
  if (!strftime(log_buffer, ALIVE_BUFFSIZ, LOG_ALIVE_FORMAT,
		tmptr)) {
      strn_cpy(log_buffer, sizeof(log_buffer),
	      "(could not format time in 256 positions "
	      "with current format string.)");
  }
  log_buffer[ALIVE_BUFFSIZ] = '\0';

  sn_printf(buf, sizeof(buf), "\n=====\n===== LOGGING STARTED %s\n=====\n",
	    log_buffer);
  if (erts_run_erl_write_all(LFD, buf, strlen(buf)) < 0)
      erts_run_erl_log_status("Error in writing to log.\n");

#if USE_FSYNC
  fsync(LFD);
#endif

  return LFD;
}

/* Instead of making sure basename exists, we do our own */
char *simple_basename(char *path)
{
    char *ptr;
    for (ptr = path; *ptr != '\0'; ++ptr) {
	if (*ptr == '/') {
	    path = ptr + 1;
	}
    }
    return path;
}

ssize_t sf_read(int fd, void *buffer, size_t len) {
    ssize_t n = 0;

    do { n = read(fd, buffer, len); } while (n < 0 && errno == EINTR);

    return n;
}

ssize_t sf_write(int fd, const void *buffer, size_t len) {
    ssize_t n = 0;

    do { n = write(fd, buffer, len); } while (n < 0 && errno == EINTR);

    return n;
}

int sf_open(const char *path, int type, mode_t mode) {
    int fd = 0;

    do { fd = open(path, type, mode); } while(fd < 0 && errno == EINTR);

    return fd;
}

int sf_close(int fd) {
    int res = 0;

    do { res = close(fd); } while(res < 0 && errno == EINTR);

    return res;
}

/* Call write() until entire buffer has been written or error.
 * Return len or -1.
 */
int erts_run_erl_write_all(int fd, const char* buf, int len)
{
    int left = len;
    int written;
    for (;;) {
        do {
	  written = write(fd,buf,left);
	} while (written < 0 && errno == EINTR);
	if (written == left) {
	    return len;
	}
	if (written < 0) {
	    return -1;
	}
	left -= written;
	buf += written;
    }
    return written;
}

/* erts_run_erl_log_status()
 * Prints the arguments to a status file
 * Works like printf (see vfrpintf)
 */
void erts_run_erl_log_status(const char *format,...)
{
  va_list args;
  time_t now;

  if (STDSTATUS == NULL)
    STDSTATUS = fopen(STATUSFILE, "w");
  if (STDSTATUS == NULL)
    return;
  now = time(NULL);
  fprintf(STDSTATUS, "run_erl [%d] %s",
#ifdef __OSE__
	  (int)current_process(),
#else
	  (int)getpid(),
#endif
	  ctime(&now));
  va_start(args, format);
  vfprintf(STDSTATUS, format, args);
  va_end(args);
  fflush(STDSTATUS);
  return;
}

/* Fetch the current log alive minutes */
int erts_run_erl_log_alive_minutes() {
  return LOG_ALIVE_MINUTES;
}

/* error_logf()
 * Prints the arguments to stderr or syslog
 * Works like printf (see vfprintf)
 */
void erts_run_erl_log_error(int priority, int line, const char *format, ...)
{
    va_list args;
    va_start(args, format);

#ifdef HAVE_SYSLOG_H
    if (RUN_DAEMON) {
	vsyslog(priority,format,args);
    }
    else
#endif
#ifdef __OSE__
    if (RUN_DAEMON) {
      char *buff = malloc(sizeof(char)*1024);
      vsnprintf(buff,1024,format, args);
      ramlog_printf(buff);
    }
    else
#endif
    {
	time_t now = time(NULL);
	fprintf(stderr, "run_erl:%d [%d] %s", line,
#ifdef __OSE__
		(int)current_process(),
#else
		(int)getpid(),
#endif
		ctime(&now));
	vfprintf(stderr, format, args);
    }
    va_end(args);
}

/* erts_run_erl_log_write()
 * Writes a message to lfd. If the current log file is full,
 * a new log file is opened.
 */
int erts_run_erl_log_write(char* buf, size_t len)
{
  int size;
  ssize_t res;
  /* Decide if new logfile needed, and open if so */

  size = lseek(LFD,0,SEEK_END);
  if(size+len > LOG_MAXSIZE) {
    int res;
    do {
      res = close(LFD);
    } while (res < 0 && errno == EINTR);
    LOG_NUM = next_log(LOG_NUM);
    LFD = open_log(LOG_NUM, O_RDWR|O_CREAT|O_TRUNC|O_SYNC);
  }

  /* Write to log file */

  if ((res = erts_run_erl_write_all(LFD, buf, len)) < 0) {
    erts_run_erl_log_status("Error in writing to log.\n");
  }

#if USE_FSYNC
  fsync(LFD);
#endif
  return res;
}

int erts_run_erl_log_activity(int timeout,time_t now,time_t last_activity) {
  char log_alive_buffer[ALIVE_BUFFSIZ+1];
  char buf[BUFSIZ];

  if (timeout || now - last_activity > LOG_ACTIVITY_MINUTES*60) {
    /* Either a time out: 15 minutes without action, */
    /* or something is coming in right now, but it's a long time */
    /* since last time, so let's write a time stamp this message */
    struct tm *tmptr;
    if (LOG_ALIVE_IN_GMT) {
      tmptr = gmtime(&now);
    } else {
      tmptr = localtime(&now);
    }
    if (!strftime(log_alive_buffer, ALIVE_BUFFSIZ, LOG_ALIVE_FORMAT,
		  tmptr)) {
      strn_cpy(log_alive_buffer, sizeof(log_alive_buffer),
	       "(could not format time in 256 positions "
	       "with current format string.)");
    }
    log_alive_buffer[ALIVE_BUFFSIZ] = '\0';

    sn_printf(buf, sizeof(buf), "\n===== %s%s\n",
	      timeout?"ALIVE ":"", log_alive_buffer);
    return erts_run_erl_log_write(buf, strlen(buf));
  }
  return 0;
}

int erts_run_erl_log_open() {

  LOG_NUM = find_next_log_num();
  LFD = open_log(LOG_NUM, O_RDWR|O_APPEND|O_CREAT|O_SYNC);
  return 0;
}

int erts_run_erl_log_init(int daemon, char* logdir) {
  char *p;

#ifdef __OSE__
  run_erl **re_pp;
  if (!run_erl_pp_key)
     ose_create_ppdata("run_erl_ppdata",&run_erl_pp_key);
  re_pp = (run_erl **)ose_get_ppdata(run_erl_pp_key);
  *re_pp = malloc(sizeof(run_erl));
#endif

  STDSTATUS = NULL;
  LOG_GENERATIONS = DEFAULT_LOG_GENERATIONS;
  LOG_MAXSIZE     = DEFAULT_LOG_MAXSIZE;
  LOG_ACTIVITY_MINUTES = DEFAULT_LOG_ACTIVITY_MINUTES;
  LOG_ALIVE_IN_GMT = 0;
  RUN_DAEMON = 0;
  LOG_ALIVE_MINUTES = DEFAULT_LOG_ALIVE_MINUTES;
  LFD = 0;
  PROTOCOL_VER = RUN_ERL_LO_VER; /* assume lowest to begin with */

  /* Get values for LOG file handling from the environment */
  if ((p = getenv_int("RUN_ERL_LOG_ALIVE_MINUTES"))) {
      LOG_ALIVE_MINUTES = atoi(p);
      if (!LOG_ALIVE_MINUTES) {
	  ERROR1(LOG_ERR,"Minimum value for RUN_ERL_LOG_ALIVE_MINUTES is 1 "
		 "(current value is %s)",p);
      }
      LOG_ACTIVITY_MINUTES = LOG_ALIVE_MINUTES / 3;
      if (!LOG_ACTIVITY_MINUTES) {
	  ++LOG_ACTIVITY_MINUTES;
      }
  }
  if ((p = getenv_int(
		   "RUN_ERL_LOG_ACTIVITY_MINUTES"))) {
     LOG_ACTIVITY_MINUTES = atoi(p);
      if (!LOG_ACTIVITY_MINUTES) {
	  ERROR1(LOG_ERR,"Minimum value for RUN_ERL_LOG_ACTIVITY_MINUTES is 1 "
		 "(current value is %s)",p);
      }
  }
  if ((p = getenv_int("RUN_ERL_LOG_ALIVE_FORMAT"))) {
      if (strlen(p) > ALIVE_BUFFSIZ) {
	  ERROR1(LOG_ERR, "RUN_ERL_LOG_ALIVE_FORMAT can contain a maximum of "
		 "%d characters", ALIVE_BUFFSIZ);
      }
      strn_cpy(LOG_ALIVE_FORMAT, sizeof(LOG_ALIVE_FORMAT), p);
  } else {
      strn_cpy(LOG_ALIVE_FORMAT, sizeof(LOG_ALIVE_FORMAT),
	       DEFAULT_LOG_ALIVE_FORMAT);
  }
  if ((p = getenv_int("RUN_ERL_LOG_ALIVE_IN_UTC"))
      && strcmp(p,"0")) {
      ++LOG_ALIVE_IN_GMT;
  }
  if ((p = getenv_int("RUN_ERL_LOG_GENERATIONS"))) {
    LOG_GENERATIONS = atoi(p);
    if (LOG_GENERATIONS < LOG_MIN_GENERATIONS)
      ERROR1(LOG_ERR,"Minimum RUN_ERL_LOG_GENERATIONS is %d",
	     LOG_MIN_GENERATIONS);
    if (LOG_GENERATIONS > LOG_MAX_GENERATIONS)
      ERROR1(LOG_ERR,"Maximum RUN_ERL_LOG_GENERATIONS is %d",
	     LOG_MAX_GENERATIONS);
  }

  if ((p = getenv_int("RUN_ERL_LOG_MAXSIZE"))) {
    LOG_MAXSIZE = atoi(p);
    if (LOG_MAXSIZE < LOG_MIN_MAXSIZE)
      ERROR1(LOG_ERR,"Minimum RUN_ERL_LOG_MAXSIZE is %d", LOG_MIN_MAXSIZE);
  }

  RUN_DAEMON = daemon;

  strn_cpy(LOG_DIR, sizeof(LOG_DIR), logdir);
  strn_cpy(STATUSFILE, sizeof(STATUSFILE), LOG_DIR);
  strn_cat(STATUSFILE, sizeof(STATUSFILE), STATUSFILENAME);

  return 0;
}

/* create_fifo()
 * Creates a new fifo with the given name and permission.
 */
static int create_fifo(char *name, int perm)
{
  if ((mkfifo(name, perm) < 0) && (errno != EEXIST))
    return -1;
  return 0;
}

/*
 * w- and r_pipename have to be pre-allocated of atleast FILENAME_MAX size
 */
int erts_run_erl_open_fifo(char *pipename,char *w_pipename,char *r_pipename) {
  int calculated_pipename = 0;
  int highest_pipe_num = 0;
  int fd;

  /*
   * Create FIFOs and open them
   */

  if(*pipename && pipename[strlen(pipename)-1] == '/') {
    /* The user wishes us to find a unique pipe name in the specified */
    /* directory */
    DIR *dirp;
    struct dirent *direntp;

    calculated_pipename = 1;
    dirp = opendir(pipename);
    if(!dirp) {
      ERRNO_ERR1(LOG_ERR,"Can't access pipe directory '%s'.", pipename);
      return 1;
    }

    /* Check the directory for existing pipes */

    while((direntp=readdir(dirp)) != NULL) {
      if(strncmp(direntp->d_name,PIPE_STUBNAME,PIPE_STUBLEN)==0) {
	int num = atoi(direntp->d_name+PIPE_STUBLEN+1);
	if(num > highest_pipe_num)
	  highest_pipe_num = num;
      }
    }
    closedir(dirp);
    strn_catf(pipename, BUFSIZ, "%s.%d",
	      PIPE_STUBNAME, highest_pipe_num+1);
  } /* if */

  for(;;) {
      /* write FIFO - is read FIFO for `to_erl' program */
      strn_cpy(w_pipename, BUFSIZ, pipename);
      strn_cat(w_pipename, BUFSIZ, ".r");
      if (create_fifo(w_pipename, PERM) < 0) {
	  ERRNO_ERR1(LOG_ERR,"Cannot create FIFO %s for writing.",
		     w_pipename);
	  return 1;
      }

      /* read FIFO - is write FIFO for `to_erl' program */
      strn_cpy(r_pipename, BUFSIZ, pipename);
      strn_cat(r_pipename, BUFSIZ, ".w");

      /* Check that nobody is running run_erl already */
      if ((fd = sf_open(r_pipename, O_WRONLY|DONT_BLOCK_PLEASE, 0)) >= 0) {
	  /* Open as client succeeded -- run_erl is already running! */
	  sf_close(fd);
	  if (calculated_pipename) {
	      ++highest_pipe_num;
	      strn_catf(pipename, BUFSIZ, "%s.%d",
			PIPE_STUBNAME, highest_pipe_num+1);
	      continue;
	  }
	  ERROR1(LOG_ERR, "Erlang already running on pipe %s.\n", pipename);
	  unlink(w_pipename);
	  return 1;
      }
      if (create_fifo(r_pipename, PERM) < 0) {
	  unlink(w_pipename);
	  ERRNO_ERR1(LOG_ERR,"Cannot create FIFO %s for reading.",
		     r_pipename);
	  return 1;
      }
      break;
  }
  return 0;
}

/* Extract any control sequences that are ment only for run_erl
 * and should not be forwarded to the pty.
 */
int erts_run_erl_extract_ctrl_seq(char* buf, int len, int mfd)
{
    static const char prefix[] = "\033_";
    static const char suffix[] = "\033\\";
    char* bufend = buf + len;
    char* start = buf;
    char* command;
    char* end;

    for (;;) {
	start = find_str(start, bufend-start, prefix);
	if (!start) break;

	command = start + strlen(prefix);
	end = find_str(command, bufend-command, suffix);
	if (end) {
	    unsigned col, row;
	    if (sscanf(command,"version=%u", &PROTOCOL_VER)==1) {
		/*fprintf(stderr,"to_erl v%u\n", protocol_ver);*/
	    }
	    else if (sscanf(command,"winsize=%u,%u", &col, &row)==2) {
#ifdef TIOCSWINSZ
	      struct winsize ws;
	      ws.ws_col = col;
	      ws.ws_row = row;
	      if (ioctl(mfd, TIOCSWINSZ, &ws) < 0) {
		ERRNO_ERR0(LOG_ERR,"Failed to set window size");
	      }
#endif
	    }
	    else {
		ERROR2(LOG_ERR, "Ignoring unknown ctrl command '%.*s'\n",
		       (int)(end-command), command);
	    }

	    /* Remove ctrl sequence from buf */
	    end += strlen(suffix);
	    memmove(start, end, bufend-end);
	    bufend -= end - start;
	}
	else {
	    ERROR2(LOG_ERR, "Missing suffix in ctrl sequence '%.*s'\n",
		   (int)(bufend-start), start);
	    break;
	}
    }
    return bufend - buf;
}
