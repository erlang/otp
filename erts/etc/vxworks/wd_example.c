/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
 *  File:      frc5te_wd.c
 *  Purpose:   Watchdog NMI handling for FORCE 5TE
 *
 * Description:
 * The watchdog handler routines are system specific. A program that
 * wants to utilize a hardware watchdog should call wd_init and test
 * the return value. If wd_init returns true (!0); there is a hardware
 * watchdog, and that watchdog has been activated. If no watchdog exists,
 * wd_init returns false (0).
 * 
 * To keep the watchdog happy, call wd_reset at least every X seconds,
 * where X is the number of seconds specified in the call to wd_init.
 *
 * The watchdog can be disarmed by setting the variable wd_disarmed to 1,
 * and armed again by setting the same variable to 0. Watchdog status 
 * information can be retrieved using the function wd_status.
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <frc5e.h>
#include <logLib.h>
#include <taskLib.h>
#include <sysLib.h>
#include <stdio.h>
#include "hw_watchdog.h"

/* prototypes */
extern sysNMIConnect();
#ifdef __STDC__
void wd_keeper(int);
void wd_nmi_int(UINT8);
void wd_status(void);
#else
void wd_keeper();
void wd_nmi_int();
void wd_status();
#endif

#define WD_NMI_MIN_DELAY  0.830      /* Min time before watchdog NMI (in seconds) */
#define WD_RESET_FREQUENCY (WD_NMI_MIN_DELAY / 2) /* how often the timer is reset */

#define WD_KEEPER_STACK_SIZE 10000

/* global variables */
extern int spTaskOptions;
static volatile int wd_count_startval;   /* start value set by wd_init */
static volatile int wd_count;            /* counter for wd_keeper */
volatile int wd_disarmed = 0;            /* debug feature */

/* wd_init is executed to initialize the watchdog. It spawns the task   */
/* wd_keeper and returns true (non-zero) if a hardware watchdog exists, */
/* or returns false (zero) otherwise.                                   */
int wd_init(timeout, prio)
     int timeout, prio;
{
  taskSpawn("wd_keeper", prio, spTaskOptions, WD_KEEPER_STACK_SIZE, 
	    (FUNCPTR)wd_keeper, timeout,0,0,0,0,0,0,0,0,0);
  return !0;                          /* watchdog exists */
} 


/* wd_reset is called as an alive-signal from the supervisor process. */
/* If there is no call to this function within a certain time, the    */
/* watchdog will reboot the system.                                   */
void wd_reset()
{
  wd_count = wd_count_startval;
}


/* wd_keeper runs as a separate task and resets the watchdog timer     */
/* before an NMI is generated. This task uses the counter wd_count to  */
/* decide if it should exit or keep resetting the timer.               */
/* Note! This task must run with higher priority than the application! */
void wd_keeper(timeout)
     int timeout;
{
  int wd_delay = sysClkRateGet() * WD_RESET_FREQUENCY;
  wd_count_startval = (int)(timeout / WD_RESET_FREQUENCY);
  wd_count = wd_count_startval;

  /* Connect and enable level 15 interrupts */
  sysNMIConnect((VOIDFUNCPTR) wd_nmi_int, WD_NMI, WD_NMI);
  *(char *)FRC5CE_GEN_PURPOSE2_REG |= FRC5CE_NMI_ENABLE;

  while ((wd_count > 0) || wd_disarmed) {
      *(char *)FRC5CE_VME_A32MAP_REG |= FRC5CE_WATCHDOG_ENABLE;
      taskDelay(wd_delay);
      if (!wd_disarmed) wd_count--;
      else wd_count = wd_count_startval;
    }
  logMsg("Watchdog keeper exits. No alive signal from application in %d seconds.\n",wd_count_startval * WD_RESET_FREQUENCY,0,0,0,0,0);
}


/* wd_nmi_int is the function connected to the watchdog interrupt. */
/* It will report the failure to reset the watchdog timer.         */
void wd_nmi_int(type)
     UINT8 type;
{
  switch(type) {
  case WD_NMI:
    logMsg("Watchdog interrupt! System will reboot.\n",0,0,0,0,0,0);
    break;
  default:
    logMsg("Bad type (%d) in call to watchdog interrupt handler.\n",type,0,0,0,0,0);
    break;
  }    
}


/* wd_status displays the current value of the counter. */
void wd_status()
{
    fprintf(stderr, "Watchdog is %sarmed.\n", wd_disarmed ? "dis" : "");
    fprintf(stderr, "Counter value:  %d\n", wd_count);
    fprintf(stderr, "Start value is: %d (%d seconds)\n",
	    wd_count_startval, (int)(wd_count_startval * WD_RESET_FREQUENCY));
}
