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
 * Module: run_erl_main.c
 *
 * Container for load module that installs both run_erl and to_erl command.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stdio.h>

#include "ose.h"
#include "shell.h"

#include "run_erl_common.h"
#include "run_erl.h"
#include "to_erl_common.h"

union SIGNAL {
  SIGSELECT signo;
};

int main(int argc, char **argv)
{

  char run_erl_usage[320],
    to_erl_usage[120];

  (void)stdin;(void)stdout;(void)stderr;

  sprintf(run_erl_usage,RUN_ERL_USAGE,"run_erl [-daemon] [-block blockname]");
  sprintf(to_erl_usage,TO_ERL_USAGE,"pipename");

  shell_add_cmd_attrs(
    "run_erl",run_erl_usage,
    "Redirect Erlang input and output streams",
    run_erl,DEFAULT_PROC_TYPE,DEFAULT_PRIORITY,DEFAULT_STACK_SIZE);

  shell_add_cmd_attrs(
    "to_erl",to_erl_usage,
    "Attach to redirected Erlang input and output streams",
    to_erl,DEFAULT_PROC_TYPE,DEFAULT_PRIORITY,DEFAULT_STACK_SIZE);

  while (1) {
    static const SIGSELECT sigsel[] = {0};
    union SIGNAL *sig = receive(sigsel);

    if (sig->signo == ERTS_SIGNAL_RUN_ERL_DAEMON) {
      PROCESS pid = create_process(OS_BG_PROC,"run_erl_daemon",
				   run_erl_process, 0x800,
				   0, 0, 0, NULL, 0, 0);
      send_w_s(&sig,pid,sender(&sig));
    } else {
      printf("Got unexpected signal!");
      free_buf(&sig);
    }
  }

  return 1;
}
