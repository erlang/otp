#include <stdio.h>
#include "erl_driver.h"

static ErlDrvPort erlang_port;
static ErlDrvData echo_start(ErlDrvPort, char *);
static void echo_stop(ErlDrvData), echo_read(ErlDrvData, char*, int);

static ErlDrvEntry echo_driver_entry = { 
    NULL,
    echo_start,
    echo_stop,
    echo_read,
    NULL,
    NULL,
    "echo_drv",
    NULL
};

DRIVER_INIT(echo_drv)
{
    erlang_port = (ErlDrvPort)-1;
    return &echo_driver_entry;
}

static ErlDrvData echo_start(ErlDrvPort port,char *buf)
{
    if (erlang_port != (ErlDrvPort)-1) {
	return ERL_DRV_ERROR_GENERAL;
    }
    erlang_port = port;
    return (ErlDrvData)port;
}

static void echo_read(ErlDrvData data, char *buf, int count)
{
    driver_output(erlang_port, buf, count);
}

static void echo_stop(ErlDrvData data)
{
    erlang_port = (ErlDrvPort)-1;
}

