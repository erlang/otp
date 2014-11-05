/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013. All Rights Reserved.
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
 * Functions that are common to both OSE and unix implementations of run_erl
 */
#ifndef ERL_RUN_ERL_LOG_H
#define ERL_RUN_ERL_LOG_H

#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include "run_erl_vsn.h"

/* Log handling */
int erts_run_erl_log_init(int run_daemon, char* logdir);
int erts_run_erl_log_open(void);
int erts_run_erl_log_close(void);
int erts_run_erl_log_write(char *buff, size_t len);
int erts_run_erl_log_activity(int timeout, time_t now, time_t last_activity);

void erts_run_erl_log_status(const char *format,...);
void erts_run_erl_log_error(int priority, int line, const char *format,...);

int erts_run_erl_open_fifo(char *pipename,char *w_pipename,char *r_pipename);
int erts_run_erl_log_alive_minutes(void);
int erts_run_erl_extract_ctrl_seq(char* buf, int len, int mfd);

/* File operations */
ssize_t sf_read(int fd, void *buffer, size_t len);
ssize_t sf_write(int fd, const void *buffer, size_t len);
int sf_open(const char *path, int type, mode_t mode);
int sf_close(int fd);
int erts_run_erl_write_all(int fd, const char* buf, int len);
char *simple_basename(char *path);

#ifndef LOG_ERR
#ifdef __OSE__
#define LOG_ERR 0
#else
#define LOG_ERR NULL
#endif
#endif

#define ERROR0(Prio,Format) erts_run_erl_log_error(Prio,__LINE__,Format"\n")
#define ERROR1(Prio,Format,A1) erts_run_erl_log_error(Prio,__LINE__,Format"\n",A1)
#define ERROR2(Prio,Format,A1,A2) erts_run_erl_log_error(Prio,__LINE__,Format"\n",A1,A2)

#ifdef HAVE_STRERROR
#    define ADD_ERRNO(Format) "errno=%d '%s'\n"Format"\n",errno,strerror(errno)
#else
#    define ADD_ERRNO(Format) "errno=%d\n"Format"\n",errno
#endif
#define ERRNO_ERR0(Prio,Format) erts_run_erl_log_error(Prio,__LINE__,ADD_ERRNO(Format))
#define ERRNO_ERR1(Prio,Format,A1) erts_run_erl_log_error(Prio,__LINE__,ADD_ERRNO(Format),A1)
#define ERRNO_ERR2(Prio,Format,A1,A2) erts_run_erl_log_error(Prio,__LINE__,ADD_ERRNO(Format),A1,A2)

#define RUN_ERL_USAGE \
  "%s (pipe_name|pipe_dir/) log_dir \"command [parameters ...]\"" \
  "\n\nDESCRIPTION:\n"							\
  "You may also set the environment variables RUN_ERL_LOG_GENERATIONS\n" \
  "and RUN_ERL_LOG_MAXSIZE to the number of log files to use and the\n"	\
  "size of the log file when to switch to the next log file\n"

#ifndef FILENAME_MAX
#define FILENAME_MAX 250
#endif

#define FILENAME_BUFSIZ FILENAME_MAX

#ifdef O_NONBLOCK
#  define DONT_BLOCK_PLEASE O_NONBLOCK
#else
#  define DONT_BLOCK_PLEASE O_NDELAY
#  ifndef EAGAIN
#    define EAGAIN -3898734
#  endif
#endif

#endif
