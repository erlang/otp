#include <stdio.h>
#include "erl_driver.h"

#ifndef DRIVER_INIT
#  define DRIVER_INIT(x) driver_init
#endif

static ErlDrvPort erlang_port;
static ErlDrvData dummy_start(ErlDrvPort, char*);
static void dummy_read(ErlDrvData port, char *buf, int count);
static void dummy_stop(ErlDrvData), easy_read(ErlDrvData, char*, int);

static ErlDrvEntry dummy_driver_entry = { 
    NULL,
    dummy_start,
    dummy_stop,
    dummy_read,
    NULL,
    NULL,
    "dummy_drv",
    NULL
};

DRIVER_INIT(dummy_drv)
{
    erlang_port = (ErlDrvPort)-1;
    return &dummy_driver_entry;
}

static ErlDrvData dummy_start(ErlDrvPort port,char *buf)
{
    if (erlang_port != (ErlDrvPort)-1) {
	return ERL_DRV_ERROR_GENERAL;
    }
    
    erlang_port = port;
    return (ErlDrvData)port;
}

static void dummy_read(ErlDrvData port, char *buf, int count)
{
    driver_output(erlang_port, buf, count);
}

static void dummy_stop(ErlDrvData port)
{
    erlang_port = (ErlDrvPort)-1;
}

