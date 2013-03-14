/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"

#include "shell.h"
#include "ramlog.h"
#include "ose_err/ose_err.h"

static PROCESS mainPid;

#ifdef DEBUG
static OSADDRESS err_handler(OSBOOLEAN user_called, OSERRCODE ecode, OSERRCODE extra) {
  fprintf(stderr,"err_handler: %p %p\n",ecode,extra);
  return 1;
}
#endif

static int
cmd_ek(int argc, char **argv) {
   kill_proc(mainPid);
   return 0;
}

static int
cmd_erl_start(int argc, char **argv) {
   ramlog_printf("\n");
   ramlog_printf("================================================================\n");
   ramlog_printf("\n");
#ifdef DEBUG
   create_error_handler(get_bid(current_process()),err_handler,0x100);
#endif
   erl_start(argc, argv);
   return 0;
}

int
main(int argc, char **argv) {
   mainPid = current_process();

   shell_add_cmd_attrs("start_beam", "start_beam [params]", "Start the Erlang VM",
                       cmd_erl_start, OS_PRI_PROC, 20, 0xF000);

   shell_add_cmd_attrs("ek", "ek", "Kills the Erlang VM",
                       cmd_ek, OS_PRI_PROC, 20, 0x100);

   stop(current_process());

   return 0;
}
