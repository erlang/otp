/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
#include "global.h"

/******************* Routines for time measurement *********************/

int erts_ticks_per_sec = 0; /* Will be SYS_CLK_TCK in erl_unix_sys.h */

int sys_init_time(void)
{
  return SYS_CLOCK_RESOLUTION;
}

clock_t sys_times(SysTimes *now) {
  now->tms_utime = now->tms_stime = now->tms_cutime =  now->tms_cstime = 0;
  return 0;
}

static OSTICK last_tick_count = 0;
static SysHrTime wrap = 0;
static OSTICK us_per_tick;

void sys_init_hrtime() {
  us_per_tick = system_tick();
}

SysHrTime sys_gethrtime() {
  OSTICK ticks = get_ticks();
  if (ticks < (SysHrTime) last_tick_count) {
    wrap += 1ULL << 32;
  }
  last_tick_count = ticks;
  return ((((SysHrTime) ticks) + wrap) * 1000*us_per_tick);
}
