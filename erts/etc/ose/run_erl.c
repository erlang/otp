/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013. All Rights Reserved.
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
 * Module: run_erl.c
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

/* System includes */
#include <aio.h>
#include <errno.h>
#include <dirent.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

/* OSE includes */
#include "ose.h"
#include "ose_spi/ose_spi.h"
#include "efs.h"
#include "pm.h"
#include "ose_spi/fm.sig"

/* erts includes */
#include "run_erl.h"
#include "run_erl_common.h"
#include "safe_string.h"    /* sn_printf, strn_cpy, strn_cat, etc */

typedef struct RunErlSetup_ {
  SIGSELECT signo;
  int run_daemon;
  char *logdir;
  char *command;
  char *pipename;
  char *blockname;
} RunErlSetup;

typedef struct ProgramState_ {
  /* child process */
  int ifd, ofd;
  OSDOMAIN domain;
  PROCESS progpid, mainbid;
  struct PmProgramInfo *info;
  /* to_erl */
  char w_pipe[FILENAME_BUFSIZ],
       r_pipe[FILENAME_BUFSIZ];
} ProgramState;

union SIGNAL {
  SIGSELECT signo;
  RunErlSetup setup;
  struct FmReadPtr fm_read_ptr;
  struct FmWritePtr fm_write_ptr;
};

static OSBOOLEAN hunt_in_block(char *block_name,
			       char *process_name,
			       PROCESS *pid);
static int create_child_process(char *command_string, char *blockname,
				ProgramState *state);


static OSBOOLEAN hunt_in_block(char *block_name,
			       char *process_name,
			       PROCESS *pid) {
  struct OS_pid_list *list;
  PROCESS block_id = OSE_ILLEGAL_PROCESS;
  int i;
  char *name;

  *pid = OSE_ILLEGAL_PROCESS;

  list = get_bid_list(0);

  if (!list)
    return 0;

  for (i = 0; i < list->count; i++) {

    if (list->list[i] == get_bid(current_process()))
      continue;

    name = (char*)get_pid_info(list->list[i], OSE_PI_NAME);
    if (name) {
      if (strcmp(name,block_name) == 0) {
	block_id = list->list[i];
	free_buf((union SIGNAL**)&name);
	break;
      }
      free_buf((union SIGNAL**)&name);
    }
  }

  free_buf((union SIGNAL**)&list);

  if (block_id == OSE_ILLEGAL_PROCESS)
    return 0;

  list = get_pid_list(block_id);

  if (!list)
    return 0;

  for (i = 0; i < list->count; i++) {
    name = (char*)get_pid_info(list->list[i], OSE_PI_NAME);
    if (name) {
      if (strcmp(name,process_name) == 0) {
	*pid = list->list[i];
	free_buf((union SIGNAL**)&name);
	break;
      }
      free_buf((union SIGNAL**)&name);
    }
  }

  free_buf((union SIGNAL**)&list);

  if (*pid == OSE_ILLEGAL_PROCESS)
    return 0;

  return 1;

}


static int create_child_process(char *command_string,  char *blockname,
				ProgramState *state) {
  char *command = command_string;
  char *argv;
  int i = 0;
  int ret_status;
  PmStatus pm_status;
  int tmp_io[2];
  int fd_arr[3];
  int ifd[2], ofd[2];
  char *handle;
  struct PmLoadModuleInfoReply *mod_info;

  /* Parse out cmd and argv from the command string */
  while (1) {
    if (command[i] == ' ' || command[i] == '\0') {
      if (command[i] == '\0')
	argv = NULL;
      else {
	command[i] = '\0';
	argv = command_string + i + 1;
      }
      break;
    }
    i++;
  }

  if (blockname)
    handle = blockname;
  else
    handle = simple_basename(command);

  if (ose_pm_load_module_info(handle,&mod_info) == PM_SUCCESS) {
    /* Already installed */
    free_buf((union SIGNAL**)&mod_info);
  } else if ((pm_status = ose_pm_install_load_module(0,"ELF",command,handle,0,0,NULL))
	     != PM_SUCCESS) {
      ERROR1(LOG_ERR,"ose_pm_install_load_module failed - pmstatus: 0x%08x\n",
	     pm_status);
      return 0;
  }

  state->domain = PM_NEW_DOMAIN;

  pm_status = ose_pm_create_program(&state->domain, handle, 0, 0 , NULL,
				    &state->progpid, &state->mainbid);

  if (pm_status != PM_SUCCESS) {
    if (pm_status == PM_EINSTALL_HANDLE_IN_USE)
      ERROR1(LOG_ERR,"ose_pm_create_program failed - "
	     "install handle \"%s\" is in use. You can specify another "
	     "install handle by using the -block option to run_erl.\n",handle);
    else
      ERROR1(LOG_ERR,"ose_pm_create_program failed - pmstatus: 0x%08x\n",
	     pm_status);
    return 0;
  }

  pm_status = ose_pm_program_info(state->progpid, &state->info);
  /* FIXME don't forget to free this ((union SIGNAL **)&info) */
  if (pm_status != PM_SUCCESS) {
    ERROR1(LOG_ERR,"ose_pm_program_info failed - pmstatus: 0x%08x\n",
	   pm_status);
    return 0;
  }

  /* We only clone stdin+stdout, what about stderr? */

  /* create pipes */
  if (pipe(ifd) < 0) {
    if (errno == ENOENT)
      ERRNO_ERR0(LOG_ERR,"The /pipe file system is not available\n");
    else
      ERRNO_ERR0(LOG_ERR,"pipe ifd failed\n");
    return 0;
  }

  if (pipe(ofd) < 0) {
    ERRNO_ERR0(LOG_ERR,"pipe ofd failed\n");
    return 0;
  }

  /* FIXME Lock? */

  /* backup our stdin stdout */
  if ((tmp_io[0] = dup(0)) < 0) {
    ERRNO_ERR0(LOG_ERR,"dup 0 failed\n");
    return 0;
  }

  if ((tmp_io[1] = dup(1)) < 0) {
    ERRNO_ERR0(LOG_ERR,"dup 1 failed\n");
    return 0;
  }

  /* set new pipe to fd 0,1 */
  if (dup2(ifd[1], 1) < 0) {
    ERRNO_ERR0(LOG_ERR,"dup2 1 failed\n");
    return 0;
  }

  if (dup2(ofd[0], 0) < 0) {
    ERRNO_ERR0(LOG_ERR,"dup2 0 failed\n");
    return 0;
  }

  /* clone array to newly created */
  fd_arr[0] = 2; /* Number of fd's */
  fd_arr[1] = 0;
  fd_arr[2] = 1;

  if ((ret_status = efs_clone_array(state->info->main_process, fd_arr))
      != EFS_SUCCESS) {
    ERROR1(LOG_ERR,"efs_close_array filed, errcode: %d\n", ret_status);
    return 0;
  }

  if (dup2(tmp_io[1], 1) < 0) {
    ERRNO_ERR0(LOG_ERR,"restoring dup2 1 failed\n");
    return 0;
  }

  if (dup2(tmp_io[0], 0) < 0) {
    ERRNO_ERR0(LOG_ERR,"restoring dup2 1 failed\n");
    return 0;
  }

  /* close loose-ends */
  sf_close(tmp_io[0]);
  sf_close(tmp_io[1]);
  sf_close(ifd[1]);
  sf_close(ofd[0]);
  state->ifd = ifd[0];
  state->ofd = ofd[1];

  if (argv && set_env(state->progpid, "ARGV", argv)) {
    ERRNO_ERR0(LOG_ERR,"something went wrong with set_env\n");
  }

  /*
   * Start the program.
   */
  pm_status = ose_pm_start_program(state->progpid);
  if (pm_status != PM_SUCCESS) {
    ERROR1(LOG_ERR,"ose_pm_install_load_module failed - pmstatus: 0x%08x\n",
	   pm_status);
    return 0;
  }

  return 1;
}

#define SET_AIO(REQ,FD,SIZE,BUFF)					\
  /* Make sure to clean data structure of previous request */		\
  memset(&(REQ),0,sizeof(REQ));						\
  (REQ).aio_fildes = FD;						\
  (REQ).aio_offset = FM_POSITION_CURRENT;				\
  (REQ).aio_nbytes = SIZE;						\
  (REQ).aio_buf = BUFF;							\
  (REQ).aio_sigevent.sigev_notify = SIGEV_NONE

#define READ_AIO(REQ,FD,SIZE,BUFF) do {					\
  SET_AIO(REQ,FD,SIZE,BUFF);						\
  if (aio_read(&(REQ)) != 0)						\
    ERRNO_ERR1(LOG_ERR,"aio_read of child_read_req(%d) failed\n",FD);	\
  } while (0)

#define WRITE_AIO(FD,SIZE,BUFF) do {					\
    struct aiocb *write_req = malloc(sizeof(struct aiocb));		\
    char *write_buff = malloc(sizeof(char)*SIZE);			\
    memcpy(write_buff,BUFF,SIZE);					\
    SET_AIO(*write_req,FD,SIZE,write_buff);				\
    if (aio_write(write_req) != 0)					\
      ERRNO_ERR1(LOG_ERR,"aio_write of write_req(%d) failed\n",FD);	\
  } while(0)

int pass_on(ProgramState *state);
int pass_on(ProgramState *s) {
  SIGSELECT sigsel[] = {0,FM_READ_PTR_REPLY};
  union SIGNAL *sig;
  char child_read_buff[BUFSIZ], pipe_read_buff[BUFSIZ];
  struct aiocb child_read_req, pipe_read_req;
  int rfd, wfd = 0;
  FmHandle rfh, child_rfh;
  int outstanding_writes = 0, got_some = 0, child_done = 0;

  if ((rfd = sf_open(s->r_pipe, O_RDONLY, 0)) < 0) {
    ERRNO_ERR1(LOG_ERR,"Could not open FIFO '%s' for reading.\n", s->r_pipe);
    rfd = 0;
    return 1;
  }

  attach(NULL,s->progpid);

  /* Open the log file */
  erts_run_erl_log_open();

  efs_examine_fd(rfd,FLIB_FD_HANDLE,&rfh);
  efs_examine_fd(s->ifd,FLIB_FD_HANDLE,&child_rfh);

  READ_AIO(child_read_req,s->ifd,BUFSIZ,child_read_buff);
  READ_AIO(pipe_read_req,rfd,BUFSIZ,pipe_read_buff);

  while (1) {
    time_t now,last_activity;

    time(&last_activity);
    sig = receive_w_tmo(erts_run_erl_log_alive_minutes()*60000,sigsel);

    time(&now);

    if (sig) {
      erts_run_erl_log_activity(0,now,last_activity);
    } else {
      /* timeout */
      erts_run_erl_log_activity(1,now,last_activity);
      continue;
    }

    switch (sig->signo) {
    case OS_ATTACH_SIG: {
      if (rfd) { sf_close(rfd); rfd = 0; }
      free_buf(&sig);
      child_done = 1;
      /* Make sure to to let all outstanding write request finish */
      if (outstanding_writes)
	break;
      if (wfd) sf_close(wfd);
      return 0;
    }
    case FM_WRITE_PTR_REPLY: {
      if (sig->fm_write_ptr.status == EFS_SUCCESS) {
	if (sig->fm_write_ptr.actual < sig->fm_write_ptr.requested) {
	  WRITE_AIO(wfd, sig->fm_write_ptr.requested-sig->fm_write_ptr.actual,
		    sig->fm_write_ptr.buffer+sig->fm_write_ptr.actual);
	}
      } else {
	/* Assume to_erl has terminated. */
	sf_close(wfd);
	wfd = 0;
      }
      free((char*)sig->fm_write_ptr.buffer);
      aio_dispatch(sig);
      if ((--outstanding_writes == 0) && child_done) {
	if (wfd) sf_close(wfd);
	return 0;
      }
      break;
    }
    case FM_READ_PTR_REPLY: {
      /* Child fd */
      if (sig->fm_read_ptr.handle == child_rfh) {

	/* Child terminated */
	if (sig->fm_read_ptr.status != EFS_SUCCESS ||
	    sig->fm_read_ptr.actual == 0) {

	  if (rfd) { sf_close(rfd); rfd = 0; }

	  if (sig->fm_read_ptr.status != EFS_SUCCESS) {
	    ERROR0(LOG_ERR,"Erlang closed the connection.");
	    aio_dispatch(sig);
	    return 1;
	  }

	  /* child closed connection gracefully */
	  aio_dispatch(sig);
	  if (outstanding_writes) {
	    child_done = 1;
	    break;
	  }

	  if (wfd) sf_close(wfd);

	  return 0;
	} else {
	  erts_run_erl_log_write(sig->fm_read_ptr.buffer,
				 sig->fm_read_ptr.actual);
	  if (wfd) {
	    WRITE_AIO(wfd, sig->fm_read_ptr.actual, sig->fm_read_ptr.buffer);
	    outstanding_writes++;
	  }
	  aio_dispatch(sig);
	  READ_AIO(child_read_req, s->ifd,BUFSIZ, child_read_buff);
	}
      /* pipe fd */
      } else if (sig->fm_read_ptr.handle == rfh) {
	if (sig->fm_read_ptr.status != EFS_SUCCESS) {
	  if(rfd) sf_close(rfd);
	  if(wfd) sf_close(wfd);
	  aio_dispatch(sig);
	  ERRNO_ERR0(LOG_ERR,"Error in reading from FIFO.");
	  return 1;
	}
	if (sig->fm_read_ptr.actual == 0) {
	  /* to_erl closed its end of the pipe */
	  aio_dispatch(sig);
	  sf_close(rfd);
	  rfd = sf_open(s->r_pipe,O_RDONLY|DONT_BLOCK_PLEASE, 0);
	  if (rfd < 0) {
	    ERRNO_ERR1(LOG_ERR,"Could not open FIFO '%s' for reading.",
		       s->r_pipe);
	    rfd = 0;
	  } else {
	    READ_AIO(pipe_read_req,rfd,BUFSIZ,pipe_read_buff);
	  }
	  got_some = 0; /* reset for next session */
	} else {
	  int len = sig->fm_read_ptr.actual;
	  char *buffer = sig->fm_read_ptr.buffer;
	  if (!wfd) {
	    /* Try to open the write pipe to to_erl. Now that we got some data
	     * from to_erl, to_erl should already be reading this pipe - open
	     * should succeed. But in case of error, we just ignore it.
	     */
	    if ((wfd = sf_open(s->w_pipe, O_WRONLY|DONT_BLOCK_PLEASE, 0)) < 0) {
	      erts_run_erl_log_status("Client expected on FIFO %s, "
				      "but can't open (len=%d)\n",
				      s->w_pipe, sig->fm_read_ptr.actual);
	      sf_close(rfd);
	      rfd = sf_open(s->r_pipe, O_RDONLY|DONT_BLOCK_PLEASE, 0);
	      if (rfd < 0) {
		ERRNO_ERR1(LOG_ERR,"Could not open FIFO '%s' for reading.",
			   s->r_pipe);
		return 1;
	      }
	      wfd = 0;
	    } else {
#ifdef DEBUG
	      erts_run_erl_log_status("run_erl: %s opened for writing\n",
				      s->w_pipe);
#endif
	    }
	  }

	  if (!got_some && wfd && buffer[0] == '\014') {
	    char wbuf[30];
	    int wlen = sn_printf(wbuf,sizeof(wbuf),"[run_erl v%u-%u]\n",
				 RUN_ERL_HI_VER, RUN_ERL_LO_VER);
	    /* For some reason this, the first write aio seems to
	       not get an FM_WRITE_PTR_REPLY, so we do not do:
	       outstanding_writes++;
	    */
	    WRITE_AIO(wfd, wlen, wbuf);
	  }
	  got_some = 1;

	  /* Write the message */
#ifdef DEBUG
	  erts_run_erl_log_status("Pty master write; ");
#endif
	  len = erts_run_erl_extract_ctrl_seq(buffer,len, s->ofd);

	  if (len > 0) {
	    int wlen = erts_run_erl_write_all(s->ofd, buffer, len);
	    if (wlen != len) {
	      aio_dispatch(sig);
	      ERRNO_ERR0(LOG_ERR,"Error in writing to terminal.");
	      if(rfd) sf_close(rfd);
	      if(wfd) sf_close(wfd);
	      return 1;
	    }
	  }
#ifdef DEBUG
	  erts_run_erl_log_status("OK\n");
#endif
	  aio_dispatch(sig);
	  READ_AIO(pipe_read_req,rfd,BUFSIZ,pipe_read_buff);
	}
	}
      break;
    }
    default: {
      free_buf(&sig);
      break;
    }
    }
  }
}

OS_PROCESS(run_erl_process) {
  char *logdir, *command, *blockname;
  SIGSELECT sigsel[] = {1,ERTS_SIGNAL_RUN_ERL_SETUP};
  union SIGNAL *sig = receive(sigsel);
  ProgramState state;
  char pipename[FILENAME_BUFSIZ];

  state.info = NULL;

  logdir = strdup(sig->setup.logdir);
  command = strdup(sig->setup.command);
  strn_cpy(pipename,sizeof(pipename),sig->setup.pipename);

  if (sig->setup.blockname)
    blockname = strdup(sig->setup.blockname);
  else
    blockname = NULL;

  erts_run_erl_log_init(sig->setup.run_daemon, logdir);

  free_buf(&sig);

  if (erts_run_erl_open_fifo(pipename,state.w_pipe,state.r_pipe))
    kill_proc(current_process());

  if (create_child_process(command,blockname,&state))
    pass_on(&state);

  free(logdir);
  free(command);
  if (blockname)
    free(blockname);

  if (state.info)
    free_buf(((union SIGNAL**)&state.info));

  sf_close(state.ifd);
  sf_close(state.ofd);

  unlink(state.w_pipe);
  unlink(state.r_pipe);

  kill_proc(current_process());
}

int run_erl(int argc,char **argv) {
  char *pipename, *logdir, *command, *blockname = NULL;
  int pipename_len, logdir_len, command_len, blockname_len = 0;
  int i = 1, run_daemon = 0;
  PROCESS pid;
  SIGSELECT sigsel[] = {0};
  union SIGNAL *sig;

  if(argc < 4) {
    fprintf(stderr,RUN_ERL_USAGE,"run_erl");
    return 1;
  }

  while (1) {
    if (argv[i][0] != '-')
      break;
    if (!strcmp(argv[i],"-daemon")) {
      run_daemon = 1;
      i++;
      continue;
    }
    if (!strcmp(argv[i],"-block")) {
      blockname = argv[i+1];
      blockname_len = strlen(argv[i+1]) + 1;
      i+=2;
      continue;
    }
    fprintf(stderr,RUN_ERL_USAGE,"run_erl");
    return 1;
  }

  pipename = argv[i++];
  logdir = argv[i++];
  command = argv[i++];

  /* + 1 to include NULL at end */
  logdir_len = strlen(logdir) + 1;
  command_len = strlen(command) + 1;
  pipename_len = strlen(pipename) + 1;

  if (run_daemon) {
    /* We request that the run_erl_process should be started from the
       main process so that it does not die when the shell command
       returns */
    PROCESS main_pid;
    hunt_in_block("run_erl","main",&main_pid);
    sig = alloc(sizeof(*sig),ERTS_SIGNAL_RUN_ERL_DAEMON);
    send(&sig,main_pid);
    sig = receive(sigsel);
    pid = sender(&sig);
    free_buf(&sig);
  } else {
    pid = create_process(OS_BG_PROC,"run_erl_process",
			 run_erl_process, 0x800,
			 0, 0, 0, NULL, 0, 0);
  }

  sig = alloc(sizeof(RunErlSetup)+
	      logdir_len+command_len+pipename_len+blockname_len,
	      ERTS_SIGNAL_RUN_ERL_SETUP);
  sig->setup.run_daemon = run_daemon;
  sig->setup.logdir = ((char*)sig)+sizeof(RunErlSetup);
  sig->setup.command = ((char*)sig)+sizeof(RunErlSetup)+logdir_len;
  sig->setup.pipename = ((char*)sig)+sizeof(RunErlSetup)+logdir_len+command_len;
  if (blockname)
    sig->setup.blockname = ((char*)sig)+sizeof(RunErlSetup)+
      logdir_len+command_len+pipename_len;
  else
    sig->setup.blockname = NULL;

  strcpy(sig->setup.logdir,logdir);
  strcpy(sig->setup.command,command);
  strcpy(sig->setup.pipename,pipename);
  if (blockname) strcpy(sig->setup.blockname,blockname);

  send(&sig,pid);

  if (run_daemon) {
    /* We are a daemon, error msgs will be sent to ramlog */
    start(pid);
    return 1;
  }

  /* We are not daemon, error msgs will be sent to stderr and we block here */
  efs_clone(pid);
  start(pid);

  attach(NULL,pid);
  sig = receive(sigsel);

  return 1;
}
