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
 * This is heart's watchdog interface for VxWorks.
 */

#ifndef _HW_WATCHDOG_H
#define _HW_WATCHDOG_H

extern void wd_init(int timeout, int prio);     /* wd_init initializes the 
						  watchdog, if one is used. */
extern void wd_reset(void);                     /* wd_reset is used by heart to kick
						  the watchdog, if one is used. */
extern void heart_reboot(void);                        /* reboot is called if heart discovers
						  that the Erlang task has stopped sending
						  heart beats. It can log system status
						  and should reboot VxWorks. */

#endif
