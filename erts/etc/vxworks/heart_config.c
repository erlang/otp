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
 * A basic heart configure module for VxWorks.
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <vxWorks.h>
#include <stdio.h>
#include <stdlib.h>
#include <rebootLib.h>
#include <sysLib.h>

/* wd_init is executed to initialize a watchdog (if one is used). */
int wd_init(timeout, prio)
     int timeout, prio;
{

} 

/* wd_reset should be called every 5th second from heart */
void wd_reset()
{

}

/* This routine is called when erlang has closed */
void heart_reboot()
{
    if (getenv("HEART_DONT_REBOOT") != NULL) {
	fprintf(stderr, "heart_config: HEART_DONT_REBOOT set, no reboot ...\n");
    } else {
	fprintf(stderr, "heart_config: rebooting ...\n");
	taskDelay(sysClkRateGet() * 5);
	reboot(BOOT_CLEAR);
    }
}




