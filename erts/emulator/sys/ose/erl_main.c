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
#include <stdlib.h>

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "ose.h"

int
main(int argc, char **argv) {

  (void)stdin;(void)stdout;(void)stderr;

  /* When starting using pm_create -c ARGV="-- -root ..", argv[0] is the first
     part of ARGV and not the name of the executable. So we shuffle some
     pointers here to make erl_start happy. */
  if (argv[0][0] == '-') {
    int i;
    char **tmp_argv = malloc(sizeof(char*)*(argc+1));
    for (i = 0; i < argc; i++)
      tmp_argv[i+1] = argv[i];
    tmp_argv[0] = "beam";
    erl_start(argc+1,tmp_argv);
    free(tmp_argv);
  } else {
   erl_start(argc,argv);
  }

   stop(current_process());

   return 0;
}
