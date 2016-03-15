/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
/* Used by test case otp_7461 to spawn a child process with a given
 * command line. Child process group is killed by order received on stdin.
 *
 * Author: Sverker Eriksson
 */

#if defined (__WIN32__) || defined(VXWORKS)
int main() {return 0;}

#else /* UNIX only */

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#define TRY(cmd) if ((cmd) < 0) bail_out(#cmd " failed")

static void bail_out(const char* msg)
{
  perror(msg);
  exit(-1);
}

static void alarm_handler(int signo)
{
  fprintf(stderr, __FILE__" self terminating after timeout\n");
  exit(1);
}

int main(int argc, char* argv[])
{
  pid_t child;
  int ret; 
  char cmd;
  int child_exit;

  if (argc < 2) {
    fprintf(stderr, "Must specify command to run in background\n");
    exit(-1);
  }
  TRY(child=fork());

  if (child == 0) { /* child */
    pid_t gchild;
    TRY(setpgid(getpid(), getpid())); /* create process group */
    
    TRY(gchild=fork());
    if (gchild == 0) { /* grandchild */
      TRY(execvp(argv[1],&argv[1]));      
    }
    exit(0);
  }
  /* parent */    

  signal(SIGALRM, alarm_handler);
  alarm(10*60); /* suicide in case nothing happens */

  TRY(wait(&child_exit));
  if (!WIFEXITED(child_exit) || WEXITSTATUS(child_exit)!=0) {
    fprintf(stderr, "child did not exit normally (status=%d)\n", child_exit);
    exit(-1);
  }

  for (;;)
    {
      TRY(ret=read(STDIN_FILENO, &cmd, 1));
      if (ret == 0) break; /* eof -> exit */
      switch (cmd)
	{
	case 'K':
	  ret = kill(-child, SIGINT); /* child process _group_ */
	  if (ret < 0 && errno != ESRCH) {
	    bail_out("kill failed");
	  }
 	  write(STDOUT_FILENO, &cmd, 1); /* echo ack */
 	  break;
 	case '\n':
 	  break;/* ignore (for interactive testing) */
 	default:
 	  fprintf(stderr, "Unknown command '%c'\n", cmd);
	  exit(-1);
 	}
     }  

  return 0;
}

#endif /* UNIX */
