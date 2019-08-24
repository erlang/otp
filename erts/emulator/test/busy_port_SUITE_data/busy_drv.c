/*
 * Purpose: Provides a driver whose busy state can be controlled from Erlang.
 * Author: Bjorn Gustavsson
 */

#include "erl_driver.h"
#include <stdio.h>
#include <string.h>

#define NO 0
#define YES 1

static ErlDrvData busy_start(ErlDrvPort, char*);
static void busy_stop(ErlDrvData),
    busy_from_erlang(ErlDrvData, char*, ErlDrvSizeT);

ErlDrvEntry busy_driver_entry =
{
    NULL,
    busy_start,
    busy_stop,
    busy_from_erlang,
    NULL,
    NULL,
    "busy_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

static ErlDrvPort master_port;
static ErlDrvPort slave_port;
static int next_slave_state;

DRIVER_INIT(busy_drv)
{
    master_port = (ErlDrvPort)-1;
    slave_port = (ErlDrvPort)-1;
    return &busy_driver_entry;
}

static ErlDrvData busy_start(ErlDrvPort port, char* buf)
{
    char *s;
    int slave = YES;

    s = strchr(buf, ' ');
    if (s && s[1] == 'm') {
	/* This is the master port */
	if (master_port != (ErlDrvPort)-1)
	    return ERL_DRV_ERROR_GENERAL;		/* Already open */
	if (slave_port != (ErlDrvPort)-1) {
	    return ERL_DRV_ERROR_GENERAL;
	}
	master_port = port;
	next_slave_state = 1;
    } else {
	if (slave_port != (ErlDrvPort)-1)
	    return ERL_DRV_ERROR_GENERAL;		/* Already open */
	if (master_port == (ErlDrvPort)-1) {
	    return ERL_DRV_ERROR_GENERAL;
	}
	slave_port = port;
    }
    return (ErlDrvData)port;
}

static void busy_stop(ErlDrvData port)
{
    if ((ErlDrvPort)port == master_port) {
	master_port = (ErlDrvPort)-1;
    } else if ((ErlDrvPort)port == slave_port) {
	slave_port = (ErlDrvPort)-1;
    }
}

static void
busy_from_erlang(ErlDrvData port, char* buf, ErlDrvSizeT count)
{
    if ((ErlDrvPort)port == slave_port) {
	set_busy_port(slave_port, next_slave_state);
	next_slave_state = 0;
	return;
    }

    if (slave_port == (ErlDrvPort)-1 || count < 1) {
	driver_failure((ErlDrvPort)port, -1);
	return;
    }
    
    switch (buf[0]) {
    case 'l':			/* Lock port (set to busy) */
	set_busy_port(slave_port, 1);
	break;
    case 'u':			/* Unlock port (not busy) */
	set_busy_port(slave_port, 0);
	break;
    }
}
