/* ``Licensed under the Apache License, Version 2.0 (the "License");
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
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

static ErlDrvData monitor_drv_start(ErlDrvPort, char *);
static void monitor_drv_stop(ErlDrvData data);
static ErlDrvSSizeT monitor_drv_control(ErlDrvData, unsigned int,
					char *, ErlDrvSizeT, char **, ErlDrvSizeT);
static void handle_monitor(ErlDrvData drv_data, ErlDrvMonitor *monitor);

#define OP_I_AM_IPID 1
#define OP_MONITOR_ME 2
#define OP_DEMONITOR_ME 3
#define OP_MONITOR_ME_LATER 4
#define OP_DO_DELAYED_MONITOR 5

typedef struct one_monitor {
    ErlDrvTermData pid;
    int later_id;
    ErlDrvMonitor mon;
    struct one_monitor *next;
} OneMonitor;


typedef struct {
    ErlDrvPort port;
    ErlDrvTermData ipid;
    int later_counter;
    OneMonitor *first;
} MyDrvData;


static ErlDrvEntry monitor_drv_entry = { 
    NULL /* init */,
    monitor_drv_start,
    monitor_drv_stop,
    NULL /* output */,
    NULL /* ready_input */,
    NULL /* ready_output */,
    "monitor_drv",
    NULL /* finish */,
    NULL /* handle */,
    monitor_drv_control,
    NULL /* timeout */,
    NULL /* outputv */,
    NULL /* ready_async */,
    NULL /* flush */,
    NULL /* call */,
    NULL /* event */,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL, /* handle2 */
    handle_monitor
};

DRIVER_INIT(monitor_drv)
{
    return &monitor_drv_entry;
}

static ErlDrvData
monitor_drv_start(ErlDrvPort port, char *command) {
    MyDrvData *data = driver_alloc(sizeof(MyDrvData));
    data->port = port;
    data->ipid = driver_term_nil;
    data->first = NULL;
    data->later_counter = 0;
    return (ErlDrvData) data;
}

static void monitor_drv_stop(ErlDrvData data)
{
    driver_free((void *) data);
}

static void handle_monitor(ErlDrvData drv_data, ErlDrvMonitor *monitor)
{
    
    MyDrvData *data = (MyDrvData *) drv_data;
    OneMonitor *p,*o;
    for (p = data->first, o = NULL; 
	 p != NULL && driver_compare_monitors(&p->mon,monitor); 
	 o = p, p = p->next)
	;
    if (!p) {
	fprintf(stderr,"Spooky Monitor executed!\r\n");
    } else {
	 ErlDrvTermData spec[] = {
	      ERL_DRV_ATOM, driver_mk_atom("monitor_fired"),
	      ERL_DRV_PORT, driver_mk_port(data->port),
	      ERL_DRV_PID, p->pid,
	      ERL_DRV_TUPLE, TERM_DATA(3)
	 };
	if (!o) {
	    data->first = p->next;
	} else {
	    o->next = p->next;
	}
	driver_free(p);
	erl_drv_send_term(driver_mk_port(data->port), data->ipid, spec, sizeof(spec)/sizeof(ErlDrvTermData));
    }
    
    return;
}

static ErlDrvSSizeT
monitor_drv_control(ErlDrvData drv_data,
		     unsigned int command,
		     char *ibuf, ErlDrvSizeT ilen,
		     char **rbuf, ErlDrvSizeT rlen)
{
    MyDrvData *data = (MyDrvData *) drv_data;
    char *answer = NULL;
    char buff[64];
    ErlDrvSSizeT alen;

    switch (command) {
    case OP_I_AM_IPID:
	data->ipid = driver_caller(data->port);
	answer = "ok";
	break;
    case OP_MONITOR_ME:
	{
	    int res;
	    OneMonitor *om = driver_alloc(sizeof(OneMonitor));
	    om->pid = driver_caller(data->port);
	    om->later_id = 0;
	    res = driver_monitor_process(data->port,om->pid,&(om->mon));
	    if (res < 0) {
		answer = "error";
		driver_free(om);
	    } else if (res > 0) {
		answer = "noproc";
		driver_free(om);
	    } else {
		om->next = data->first;
		data->first = om;
		answer = "ok";
	    }
	    break;
	}
    case OP_DEMONITOR_ME:
	{
	    int res;
	    OneMonitor *p,*q = NULL;
	    int found = 0;
	    ErlDrvTermData pid = driver_caller(data->port);
	    for (p = data->first; p != NULL; p = p->next) {
		if (p->pid == pid) {
		    q = p;
		    ++found;
		}
	    }
	    if (q == NULL) {
		answer = "not_monitored";
	    } else {
		if (q->later_id > 0) {
		    if (found > 1) {
			answer = "delayd_but_more";
		    } else {
			answer = "delayed";
		    }
		} else {
		    res = driver_demonitor_process(data->port, &(q->mon));
		    if (res < 0) {
			answer = "error";
		    } else if (res > 0) {
			if (found > 1) {
			    answer = "gone_but_more";
			} else {
			    answer = "gone";
			}
		    } else {
			if (found > 1) {
			    answer = "ok_but_more";
			} else {
			    answer = "ok";
			}
		    }
		}
		if (data->first == q) {
		    data->first = q->next;
		} else {
		    for (p = data->first; p != NULL; p = p->next) {
			if (p->next == q) {
			    p->next = q->next;
			    break;
			}
		    }
		}
		driver_free(q);
	    }
	    break;
	}
    case OP_MONITOR_ME_LATER:
	{
	    int res;
	    OneMonitor *om = driver_alloc(sizeof(OneMonitor));
	    om->pid = driver_caller(data->port);
	    om->later_id = (++(data->later_counter));
	    om->next = data->first;
	    data->first = om;
	    sprintf(buff,"ok:%d",om->later_id);
	    answer = buff;
	    break;
	}
    case OP_DO_DELAYED_MONITOR:
	{
	    int id = 0, sign = 1, in_number = 0;
	    OneMonitor *p, *q;
	    char *bp;
	    for (bp = ibuf; bp < (ibuf + ilen); ++bp) {
		if (*bp <= '9' && *bp >= '0') {
		    int x = *bp - '0';
		    in_number++;
		    id *= 10;
		    id += x;
		} else if (*bp == '-') {
		    if (in_number) {
			break;
		    } 
		    sign = -1;
		    ++in_number;
		} else {
		    if (in_number) {
			break;
		    }
		}
	    }
	    id *= sign;
	    q = NULL;
	    for (p = data->first; p != NULL; q = p, p = p->next) {
		if (p->later_id != 0 && p->later_id == id) {
		    break;
		}
	    }
	    if (p == NULL) {
		answer = "not_found";
	    } else {
		int res = driver_monitor_process(data->port,p->pid,&(p->mon));
		if (res != 0) {
		    if (res < 0) {
			answer = "error";
		    } else {
			answer = "noproc";
		    }
		    if (q == NULL) {
			data->first = p->next;
		    } else {
			q->next = p->next;
		    }
		    driver_free(p);
		} else {
		    p->later_id = 0;
		    answer = "ok";
		}
	    }
	    break;
	}
    default:
	answer = "unknown_op";
    }
    if (answer == NULL) {
	answer = "internal_error";
    }
    alen = strlen(answer);
    if (alen >= rlen) {
	*rbuf = driver_alloc(alen+1);
    }
    strcpy(*rbuf,answer);
    return alen;
}


