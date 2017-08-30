/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

/* Purpose: demonstrate how to include interupt handlers in erlang */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_driver.h"
#include <signal.h>
#include <stdio.h>

static ErlDrvData sig_start(ErlDrvPort, char*);
static int sig_init(void);
static void sig_stop(ErlDrvData), doio(ErlDrvData, ErlDrvEvent);

ErlDrvEntry sig_driver_entry = {
    sig_init,
    sig_start,
    sig_stop,
    NULL,
    doio,
    NULL,
    "sig_test"
};

static ErlDrvPort this_port;

static int sig_init(void)
{
    this_port = (ErlDrvPort)-1;
    return 0;
}

static sigc(int ino)
{
    driver_interrupt(this_port, ino);
}

static ErlDrvData sig_start(ErlDrvPort port, char* buf)
{
    if (this_port != (ErlDrvPort)-1)
	return ERL_DRV_ERROR_GENERAL;
    this_port = port;
    signal(SIGUSR1, sigc);
    return (ErlDrvData)port;
}

static void sig_stop(ErlDrvData port)
{
    this_port = (ErlDrvPort)-1;
    signal(SIGUSR1, SIG_DFL);
}

doio(ErlDrvData port, ErlDrvEvent ino)
{
    /* First go get the io, unless we already did that */
    /* In the sighandler */

    /* Then send it to erlang */

    driver_output(this_port, "y", 1);
}
