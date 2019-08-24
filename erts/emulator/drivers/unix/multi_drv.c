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

/* Purpose:    Multidriver interface 
   This is an example of a driver which allows multiple instances of itself.
   I.e have one erlang process execute open_port(multi......) and
   at the same time have another erlang process open another port
   running multi there as well.
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_driver.h"
#include "sys.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define MAXCHANNEL 20

static char buf[BUFSIZ];

static ErlDrvData multi_start(ErlDrvPort, char*);
static int multi_init(void);
static void multi_stop(ErlDrvData),multi_erlang_read(ErlDrvData, char*, int);

struct driver_entry multi_driver_entry = {
    multi_init,
    multi_start,
    multi_stop,
    multi_erlang_read,
    NULL,
    NULL,
    "multi"
};


struct channel {
    ErlDrvPort portno;
    int channel;
};

struct channel channels[MAXCHANNEL]; /* Max MAXCHANNEL instances */

static int multi_init(void)
{
    memzero(channels,MAXCHANNEL * sizeof(struct channel));
    return 0;
}

static ErlDrvData multi_start(ErlDrvPort port, char* buf) 
{
    int chan;
    chan = get_new_channel();
    channels[port].portno = port;
    channels[port].channel = chan;
    fprintf(stderr,"Opening channel %d port is %d\n",chan,port);
    return (ErlDrvData)port;
}


static int multi_stop(ErlDrvData port)
{
    fprintf(stderr,"Closing channel %d\n",channels[port].channel);
    remove_channel(channels[(int)port].channel);
}

 
static int multi_erlang_read(ErlDrvData port, char* buf, int count)
{
    fprintf(stderr,"Writing %d bytes to channel %d\n",
	    count,
	    channels[(int)port].channel);
}


/* These two funs are fake */

int get_new_channel() 
{
    static int ch = 1;
    return(ch++);
}

void remove_channel(int ch)
{
}
